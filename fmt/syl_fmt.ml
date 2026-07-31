open! Core
open! Syl
open Doc

module Config = struct
  type t =
    { margin : int
    ; indent : int
    }

  let default = { margin = 100; indent = 2 }
end

let fmt_comment _cfg (comment : Lex.Comment.t) =
  if String.is_empty comment.text
  then text "(* *)"
  else text "(* " ^^ text comment.text ^^ text " *)"
;;

let fmt_before cfg comments doc =
  match comments with
  | [] -> doc
  | _ ->
    let cs =
      List.map comments ~f:(fmt_comment cfg)
      |> List.reduce_exn ~f:(fun a b -> a ^^ hardline ^^ hardline ^^ b)
    in
    cs ^^ line ^^ doc
;;

let fmt_after cfg comments doc =
  List.fold comments ~init:doc ~f:(fun acc c -> acc ^^ text " " ^^ fmt_comment cfg c)
;;

let fmt_with_loc cfg (expr : _ Cst.With_loc.t) inner =
  fmt_before cfg expr.before inner |> fmt_after cfg expr.after
;;

let fmt_erased (erased : Modes.Erasure.t) =
  match erased with
  | Erased -> text " erased"
  | Unerased -> nil
;;

let fmt_eliminator (eliminator : Modes.Eliminator.t) =
  match eliminator with
  | Static -> text " static"
  | Erased -> text " erased"
  | Dynamic -> nil
;;

let fmt_modes (mode : Modes.Maybe.t) =
  if Modes.Maybe.is_none mode then nil else text (Modes.Maybe.print () mode) ^^ text " "
;;

let fmt_literal (literal : Cst.Literal.t) =
  match literal with
  | Unit -> text "()"
  | Bool b -> text (sprintf "%b" b)
  | Int i -> text (sprintf "%Ld" i)
;;

let fmt_ident (id : Ident.Raw.t) = text (Ident.Raw.print () id)
let fmt_label (label : Ident.Label.t) = text (Ident.Label.print () label)
let fmt_dot_label (label : Ident.Label.t) = text ("." ^ Ident.Label.print () label)

let rec fmt_pattern ?(prec = `Tuple) cfg (pat : Cst.Expr.pattern) : Doc.t =
  let inner =
    match pat.node with
    | Var { id } -> fmt_ident id
    | Literal { value } -> fmt_literal value
    | Constructor { label; payload } ->
      (match payload with
       | None -> fmt_dot_label label
       | Some payload ->
         let doc = fmt_pattern ~prec:`Tuple cfg payload in
         let doc =
           match payload.node with
           | Constructor { payload = Some _; _ } -> text "(" ^^ doc ^^ text ")"
           | _ -> doc
         in
         fmt_dot_label label ^^ text " " ^^ doc)
    | Tuple { elts } ->
      let elts = Nonempty_list.to_list elts in
      let sep = text "," ^^ line in
      let body = align (concat ~sep ~f:(fmt_pattern ~prec:`Tuple cfg) elts) in
      (match prec with
       | `Or -> group body
       | `Arm | `Tuple -> group (text "(" ^^ body ^^ text ")"))
    | Or { left; right } ->
      (match prec with
       | `Or ->
         group
           (fmt_pattern ~prec:`Or cfg left ^^ line ^^ text "| " ^^ fmt_pattern ~prec:`Or cfg right)
       | `Arm ->
         (* Arm grammar is an or-chain of atoms: keep the chain bare, but tuple or
            nested-or operands must parenthesize. *)
         group
           (fmt_pattern ~prec:`Arm cfg left
            ^^ line
            ^^ text "| "
            ^^ fmt_pattern ~prec:`Tuple cfg right)
       | `Tuple ->
         let body =
           fmt_pattern ~prec:`Or cfg left ^^ line ^^ text "| " ^^ fmt_pattern ~prec:`Or cfg right
         in
         group (text "(" ^^ align body ^^ text ")"))
  in
  fmt_with_loc cfg pat inner
;;

let rec fmt_expr ?(force_break_if = false) (cfg : Config.t) (expr : Cst.Expr.t) : Doc.t =
  let inner = fmt_node ~force_break_if cfg expr in
  fmt_with_loc cfg expr inner

and fmt_node ~force_break_if cfg (expr : Cst.Expr.t) =
  let ind = cfg.indent in
  match expr.node with
  | Match { cond; arms; eliminator; before_elimination } ->
    let fmt_arm ((pat : Cst.Expr.pattern), rhs) =
      let pat_doc = fmt_pattern ~prec:`Arm cfg pat in
      group (pat_doc ^^ text " ->" ^^ nest ind (line ^^ fmt_expr cfg rhs))
    in
    let sep = text "," ^^ line in
    let arms = Nonempty_list.to_list arms in
    let arms_doc = concat ~sep ~f:fmt_arm arms in
    (* Multi-arm matches always break; only a single arm may stay flat. *)
    let br =
      match arms with
      | [ _ ] -> line
      | _ -> hardline
    in
    group
      (group
         (text "match"
          |> fmt_after cfg before_elimination
          |> fun d -> d ^^ fmt_eliminator eliminator ^^ text " " ^^ fmt_expr cfg cond ^^ text " {")
       ^^ nest ind (br ^^ arms_doc ^^ if_flat ~flat:nil ~broken:(text ","))
       ^^ br
       ^^ text "}")
  | Literal { value } -> fmt_literal value
  | Var { id } -> fmt_ident id
  | Constructor { label } -> fmt_dot_label label
  | Select { expr = e; label } -> fmt_expr cfg e ^^ fmt_dot_label label
  | Variant { constructors } ->
    let constructors = Nonempty_list.to_list constructors in
    let sep = text "," ^^ line in
    (* Multi-case variants always break; only a single case may stay flat. *)
    let br =
      match constructors with
      | [ _ ] -> line
      | _ -> hardline
    in
    group
      (text "variant {"
       ^^ nest
            ind
            (br
             ^^ concat ~sep ~f:(fmt_constructor cfg) constructors
             ^^ if_flat ~flat:nil ~broken:(text ","))
       ^^ br
       ^^ text "}")
  | Unreachable -> text "unreachable"
  | Paren { expr = e } -> group (text "(" ^^ align (fmt_expr cfg e) ^^ text ")")
  | Unop { op; arg } -> text (Ident.Unop.print () op) ^^ fmt_expr cfg arg
  | Binop { op; lhs; rhs } ->
    group
      (fmt_expr cfg lhs ^^ line ^^ text (Ident.Binop.print () op) ^^ text " " ^^ fmt_expr cfg rhs)
  | Make_tuple { elts } ->
    let elts = Nonempty_list.to_list elts in
    let sep = text "," ^^ line in
    group (concat ~sep ~f:(fmt_expr cfg) elts)
  | Tuple { elts } ->
    let elts = Nonempty_list.to_list elts in
    let sep = line ^^ text "^ " in
    group (concat ~sep ~f:(fmt_expr cfg) elts)
  | Arrow _ -> fmt_arrow cfg expr
  | Apply _ -> fmt_apply cfg expr
  | Lambda { erased = e_; arg; body; before_erased; after_arg } ->
    group
      (group
         (text "fn"
          |> fmt_after cfg before_erased
          |> fun d ->
          d
          ^^ fmt_erased e_
          ^^ nest
               ind
               (line ^^ fmt_arg cfg arg |> fmt_after cfg after_arg |> fun d -> d ^^ text " ->"))
       ^^ nest ind (line ^^ fmt_expr cfg body))
  | Let { var; erased = e_; bind; rest; before_erased; after_erased; after_var } ->
    let name =
      text "let"
      |> fmt_after cfg before_erased
      |> fun d ->
      d ^^ fmt_erased e_ |> fmt_after cfg after_erased |> fun d -> d ^^ text " " ^^ fmt_ident var
    in
    let header = name |> fmt_after cfg after_var |> fun d -> d ^^ text " =" in
    let let_ = group (header ^^ nest ind (line ^^ fmt_expr cfg bind) ^^ line ^^ text "in") in
    group (let_ ^^ line ^^ fmt_expr cfg rest)
  | Fun { funs; rest } ->
    local_fun_defs cfg (Nonempty_list.to_list funs)
    ^^ hardline
    ^^ text "in"
    ^^ hardline
    ^^ fmt_expr cfg rest
  | If { cond; then_; else_; erased; before_erased } ->
    fmt_if cfg ~ind ~force_break:force_break_if ~before_erased erased cond then_ else_
  | Assert { cond; erased; before_erased } ->
    group
      (text "assert"
       |> fmt_after cfg before_erased
       |> fun d -> d ^^ fmt_erased erased ^^ nest ind (line ^^ fmt_expr cfg cond))
  | Type_annotation { expr = e; ty } ->
    group (fmt_expr cfg e ^^ nest ind (line ^^ text ": " ^^ fmt_expr cfg ty))
  | Mode_annotation { expr = e; mode } ->
    group (fmt_expr cfg e ^^ text " @ " ^^ text (Modes.Maybe.print () mode))

and fmt_constructor cfg (c : Cst.Expr.constructor) =
  let n = c.node in
  let label = fmt_label n.label |> fmt_after cfg n.after_label in
  let inner =
    match n.payload with
    | None -> label
    | Some payload -> group (label ^^ text " : " ^^ fmt_expr cfg payload)
  in
  fmt_with_loc cfg c inner

and fmt_apply (cfg : Config.t) (expr : Cst.Expr.t) =
  let ind = cfg.indent in
  let rec collect acc (expr : Cst.Expr.t) =
    match expr.node with
    | Apply { fn; arg } -> collect (arg :: acc) fn
    | _ -> expr, acc
  in
  let fn, args = collect [] expr in
  group
    (fmt_expr cfg fn
     ^^ nest ind (List.fold args ~init:nil ~f:(fun acc a -> acc ^^ line ^^ fmt_expr cfg a)))

and fmt_arrow (cfg : Config.t) (expr : Cst.Expr.t) =
  let ind = cfg.indent in
  let rec collect acc (expr : Cst.Expr.t) =
    match expr.node with
    | Arrow { arg; arg_id; arg_mode; ret; ret_mode }
      when List.is_empty expr.before && List.is_empty expr.after ->
      let binding =
        match arg_id with
        | Some id -> text " \\ " ^^ fmt_ident id
        | None -> nil
      in
      let segment = fmt_modes arg_mode ^^ fmt_expr cfg arg ^^ binding in
      let arrow = text "-> " ^^ fmt_modes ret_mode in
      collect ((segment, arrow) :: acc) ret
    | _ -> List.rev acc, expr
  in
  let segments, last = collect [] expr in
  let first_seg = fst (List.hd_exn segments) in
  let arrows = List.map segments ~f:snd in
  let rest_segs = (List.tl_exn segments |> List.map ~f:fst) @ [ fmt_expr cfg last ] in
  let continuation =
    List.fold2_exn arrows rest_segs ~init:nil ~f:(fun acc arrow seg -> acc ^^ line ^^ arrow ^^ seg)
  in
  group (first_seg ^^ nest ind continuation)

and is_if_expr (expr : Cst.Expr.t) =
  match expr.node with
  | If _ -> true
  | Paren { expr } -> is_if_expr expr
  | Type_annotation { expr; _ } -> is_if_expr expr
  | Mode_annotation { expr; _ } -> is_if_expr expr
  | _ -> false

and fmt_if cfg ~ind ?(force_break = false) ?(before_erased = []) erased cond then_ else_ =
  let nested = force_break || is_if_expr then_ || is_if_expr else_ in
  let sep = if nested then hardline else line in
  let then_doc =
    match then_.node with
    | If { cond; then_; else_; erased; before_erased } when nested ->
      text "then "
      ^^ align (fmt_if cfg ~ind ~force_break:true ~before_erased erased cond then_ else_)
    | _ -> group (text "then" ^^ nest ind (line ^^ fmt_expr ~force_break_if:nested cfg then_))
  in
  let else_doc =
    match else_.node with
    | If { cond; then_; else_; erased; before_erased } ->
      text "else "
      ^^ align (fmt_if cfg ~ind ~force_break:nested ~before_erased erased cond then_ else_)
    | _ -> group (text "else" ^^ nest ind (line ^^ fmt_expr ~force_break_if:nested cfg else_))
  in
  group
    (text "if"
     |> fmt_after cfg before_erased
     |> fun d ->
     d ^^ fmt_erased erased ^^ text " " ^^ fmt_expr cfg cond ^^ sep ^^ then_doc ^^ sep ^^ else_doc)

and fmt_arg cfg (a : Cst.Expr.arg) =
  let n = a.node in
  let cs comments = List.map comments ~f:(fmt_comment cfg) in
  let parts =
    cs n.after_open
    @ (if Modes.Maybe.is_none n.mode then [] else [ text (Modes.Maybe.print () n.mode) ])
    @ cs n.after_mode
    @ [ fmt_ident n.var ]
    @ cs n.after_var
  in
  let inner =
    text "("
    ^^ List.reduce_exn parts ~f:(fun a b -> a ^^ text " " ^^ b)
    ^^ text " : "
    ^^ fmt_expr cfg n.ty
    ^^ text ")"
  in
  fmt_with_loc cfg a inner

and fmt_fun_header (cfg : Config.t) ~keyword (fw : Cst.Expr.fun_) =
  let f = fw.node in
  let ind = cfg.indent in
  let ret_mode =
    if Modes.Maybe.is_none f.ret_mode
    then nil
    else text " " ^^ text (Modes.Maybe.print () f.ret_mode)
  in
  let name =
    text keyword
    |> fmt_after cfg fw.before
    |> fun d ->
    d ^^ fmt_erased f.erased
    |> fmt_after cfg f.after_erased
    |> fun d -> d ^^ text " " ^^ fmt_ident f.var
  in
  group
    (name
     ^^ nest
          ind
          (line ^^ fmt_arg cfg f.arg
           |> fmt_after cfg f.after_arg
           |> fun d -> d ^^ text " :" ^^ ret_mode ^^ text " " ^^ fmt_expr cfg f.ret_ty ^^ text " ="
          ))

and fmt_fun_def cfg ~keyword (fw : Cst.Expr.fun_) =
  fmt_fun_header cfg ~keyword fw ^^ nest cfg.indent (hardline ^^ fmt_expr cfg fw.node.body)

and local_fun_defs cfg = function
  | [] -> nil
  | [ f ] -> fmt_fun_def cfg ~keyword:"fun" f
  | f :: rest ->
    List.fold rest ~init:(fmt_fun_def cfg ~keyword:"fun" f) ~f:(fun acc f ->
      acc ^^ hardline ^^ fmt_fun_def cfg ~keyword:"and" f)
;;

let fmt_top_level (cfg : Config.t) (top_level : Cst.Top_level.t) : Doc.t =
  let ind = cfg.indent in
  let inner =
    match top_level.node with
    | Let { var; erased = e; bind; before_erased; after_erased; after_var } ->
      let name =
        text "let"
        |> fmt_after cfg before_erased
        |> fun d ->
        d ^^ fmt_erased e |> fmt_after cfg after_erased |> fun d -> d ^^ text " " ^^ fmt_ident var
      in
      let header = name |> fmt_after cfg after_var |> fun d -> d ^^ text " =" in
      group
        (header
         ^^ nest ind (line ^^ fmt_expr cfg bind)
         ^^ if_flat ~flat:(text ";;") ~broken:(hardline ^^ text ";;"))
    | Fun { funs } ->
      let docs =
        List.mapi (Nonempty_list.to_list funs) ~f:(fun i f ->
          fmt_fun_def cfg ~keyword:(if i = 0 then "fun" else "and") f)
      in
      let funs_doc = List.reduce_exn docs ~f:(fun a b -> a ^^ hardline ^^ b) in
      funs_doc ^^ hardline ^^ text ";;"
    | External { var; ty; symbol } ->
      group
        (text "external "
         ^^ fmt_ident var
         ^^ text " : "
         ^^ fmt_expr cfg ty
         ^^ text " = "
         ^^ text symbol
         ^^ text ";;")
    | Builtin { var; name } ->
      text "builtin " ^^ fmt_ident var ^^ text " = " ^^ text name ^^ text ";;"
  in
  fmt_with_loc cfg top_level inner
;;

let fmt_program ?(cfg = Config.default) (program : Cst.Program.t) : Doc.t =
  let items =
    match program.items with
    | [] -> nil
    | _ ->
      List.map program.items ~f:(fmt_top_level cfg)
      |> List.reduce_exn ~f:(fun a b -> a ^^ hardline ^^ hardline ^^ b)
  in
  let trailing =
    match program.after with
    | [] -> nil
    | cs -> List.map cs ~f:(fmt_comment cfg) |> List.reduce_exn ~f:(fun a b -> a ^^ hardline ^^ b)
  in
  match program.items, program.after with
  | [], [] -> nil
  | [], _ -> trailing ^^ hardline
  | _, [] -> items ^^ hardline
  | _, _ -> items ^^ hardline ^^ hardline ^^ trailing ^^ hardline
;;

let to_doc ?(cfg = Config.default) program = fmt_program ~cfg program

let to_string ?(cfg = Config.default) program =
  Doc.pretty ~width:cfg.margin (fmt_program ~cfg program)
;;
