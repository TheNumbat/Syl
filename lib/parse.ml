open! Core
open Lex
open Cst
module Label = Ident.Label
module Ident = Ident.Raw

type t =
  { tokens : Tokenizer.t
  ; no_commas : bool
  }

let without_commas t = { t with no_commas = true }
let with_commas t = { t with no_commas = false }

(* Comma-separated items before a closing brace, with an optional trailing comma. *)
let braced_items t ~f =
  let rec rest acc =
    match Tokenizer.peek t.tokens with
    | Op Comma ->
      Tokenizer.skip t.tokens;
      (match Tokenizer.peek t.tokens with
       | Rbrace -> Nonempty_list.reverse acc
       | _ -> rest (Nonempty_list.cons (f t) acc))
    | _ -> Nonempty_list.reverse acc
  in
  rest (Nonempty_list.singleton (f t))
;;

module Error = struct
  type t =
    | Unexpected of Token.t
    | Duplicate_mode of Modes.Axis.t
    | Unexpected_modes of Modes.Maybe.t
    | Fun_underscore
    | Unterminated_comment
  [@@deriving sexp]
end

exception Error of Error.t Loc.t [@@deriving sexp]

module Fail = struct
  let unexpected ~loc here tok = raise (Error { loc; here; reason = Unexpected tok })
  let duplicate_mode ~loc here mode = raise (Error { loc; here; reason = Duplicate_mode mode })
  let unexpected_modes ~loc here mode = raise (Error { loc; here; reason = Unexpected_modes mode })
  let fun_underscore ~loc here = raise (Error { loc; here; reason = Fun_underscore })
end

let expect (type a) t ~(kind : a Kind.t) : a =
  let loc = Tokenizer.loc t.tokens in
  let tok = Tokenizer.next t.tokens in
  match Token.get ~kind tok with
  | Some k -> k
  | None -> Fail.unexpected [%here] ~loc tok
;;

let expect_op t ~op =
  let loc = Tokenizer.loc t.tokens in
  let tok = Tokenizer.peek t.tokens in
  if Lex.Op.equal (expect t ~kind:Op) op then () else Fail.unexpected [%here] ~loc tok
;;

let op_to_ident : Lex.Op.t -> Ident.t option = function
  | Tilde_minus -> Some Ident.(unop Neg)
  | Not -> Some Ident.(unop Not)
  | Minus -> Some Ident.(binop Sub)
  | Plus -> Some Ident.(binop Add)
  | Star -> Some Ident.(binop Mul)
  | Slash -> Some Ident.(binop Div)
  | Percent -> Some Ident.(binop Mod)
  | And -> Some Ident.(binop And)
  | Or -> Some Ident.(binop Or)
  | Eq -> Some Ident.(binop Eq)
  | Neq -> Some Ident.(binop Neq)
  | Lt -> Some Ident.(binop Lt)
  | Lte -> Some Ident.(binop Lte)
  | Gt -> Some Ident.(binop Gt)
  | Gte -> Some Ident.(binop Gte)
  | Tilde | Backslash | Arrow | Caret | Comma | Amp -> None
;;

let expect_ident t =
  let loc = Tokenizer.loc t.tokens in
  match Tokenizer.next t.tokens with
  | Ident id -> Ident.id id
  | Lparen ->
    (match Tokenizer.next t.tokens with
     | Op op ->
       let id =
         match op_to_ident op with
         | Some id -> id
         | None -> Fail.unexpected [%here] ~loc (Op op)
       in
       expect t ~kind:Rparen;
       id
     | tok -> Fail.unexpected [%here] ~loc tok)
  | tok -> Fail.unexpected [%here] ~loc tok
;;

let maybe_erased t : Modes.Erasure.t =
  match Tokenizer.peek t.tokens with
  | Erased ->
    expect t ~kind:Erased;
    Erased
  | _ -> Unerased
;;

let maybe_eliminator t : Modes.Eliminator.t =
  match Tokenizer.peek t.tokens with
  | Static ->
    expect t ~kind:Static;
    Static
  | Erased ->
    expect t ~kind:Erased;
    Erased
  | _ -> Dynamic
;;

let maybe_modes ~required t : Modes.Maybe.t =
  let loc = Tokenizer.loc t.tokens in
  let rec aux
            ?(staticity : Modes.Staticity.t option)
            ?(erasure : Modes.Erasure.t option)
            (axes : Modes.Axis.t list)
    =
    match Tokenizer.peek t.tokens with
    | Static ->
      expect t ~kind:Static;
      aux ~staticity:Static ?erasure (Staticity :: axes)
    | Dynamic ->
      expect t ~kind:Dynamic;
      aux ~staticity:Dynamic ?erasure (Staticity :: axes)
    | Erased ->
      expect t ~kind:Erased;
      aux ?staticity ~erasure:Erased (Erasure :: axes)
    | Unerased ->
      expect t ~kind:Unerased;
      aux ?staticity ~erasure:Unerased (Erasure :: axes)
    | _ -> staticity, erasure, axes
  in
  let staticity, erasure, axes = aux [] in
  if required && List.length axes = 0 then Fail.unexpected [%here] ~loc (Tokenizer.peek t.tokens);
  (match List.find_a_dup axes ~compare:Modes.Axis.compare with
   | Some mode -> Fail.duplicate_mode [%here] ~loc mode
   | None -> ());
  { staticity; erasure }
;;

let with_loc ~loc ?(before = []) node : Expr.t = With_loc.create ~before ~loc node
let leading t = Tokenizer.comments t.tokens

let trailing t (expr : Expr.t) : Expr.t =
  let _, comments = Tokenizer.comments t.tokens in
  if List.is_empty comments then expr else { expr with after = expr.after @ comments }
;;

let rec expr t : Expr.t = expr_ty_annot t

and expr_ty_annot t : Expr.t =
  let expr = trailing t (expr_arrow t) in
  let loc = Tokenizer.loc t.tokens in
  match Tokenizer.peek t.tokens with
  | Colon ->
    Tokenizer.skip t.tokens;
    let ty = expr_arrow t in
    with_loc ~loc (Type_annotation { expr; ty })
  | _ -> expr

and expr_arrow t : Expr.t =
  let arg_mode = maybe_modes ~required:false t in
  let arg = trailing t (expr_comma t) in
  let arg_id =
    match Tokenizer.peek t.tokens with
    | Op Backslash ->
      Tokenizer.skip t.tokens;
      Some (expect_ident t)
    | _ -> None
  in
  let loc = Tokenizer.loc t.tokens in
  match Tokenizer.peek t.tokens with
  | Op Arrow ->
    Tokenizer.skip t.tokens;
    let ret, ret_mode =
      let modes = maybe_modes ~required:false t in
      match expr_arrow t with
      | { node = Arrow arrow; _ } ->
        with_loc ~loc:arg.loc (Arrow { arrow with arg_mode = modes }), Modes.Maybe.none
      | expr -> expr, modes
    in
    with_loc ~loc:arg.loc (Arrow { arg; arg_id; arg_mode; ret; ret_mode })
  | _ ->
    if Modes.Maybe.(equal arg_mode none) then arg else Fail.unexpected_modes [%here] ~loc arg_mode

and expr_comma t : Expr.t =
  let loc = Tokenizer.loc t.tokens in
  let first = expr_mode_annot t in
  let first = trailing t first in
  match Tokenizer.peek t.tokens with
  | Op Comma when not t.no_commas ->
    let rec aux acc =
      match Tokenizer.peek t.tokens with
      | Op Comma ->
        Tokenizer.skip t.tokens;
        let e = expr_mode_annot t in
        aux (Nonempty_list.cons (trailing t e) acc)
      | _ -> Nonempty_list.reverse acc
    in
    with_loc ~loc (Make_tuple { elts = aux (Nonempty_list.singleton first) })
  | _ -> first

(* Mode annotations bind looser than operators but tighter than commas, so
   tuple elements can be annotated individually. *)
and expr_mode_annot t : Expr.t =
  let expr = trailing t (expr_caret t) in
  match Tokenizer.peek t.tokens with
  | At ->
    let loc = Tokenizer.loc t.tokens in
    expect t ~kind:At;
    let mode = maybe_modes ~required:true t in
    with_loc ~loc (Mode_annotation { expr; mode })
  | _ -> expr

and expr_caret t : Expr.t =
  let loc = Tokenizer.loc t.tokens in
  let first = expr_lor t in
  let first = trailing t first in
  match Tokenizer.peek t.tokens with
  | Op Caret ->
    let rec aux acc =
      match Tokenizer.peek t.tokens with
      | Op Caret ->
        Tokenizer.skip t.tokens;
        let e = expr_lor t in
        aux (Nonempty_list.cons (trailing t e) acc)
      | _ -> Nonempty_list.reverse acc
    in
    with_loc ~loc (Tuple { elts = aux (Nonempty_list.singleton first) })
  | _ -> first

and expr_lor t : Expr.t =
  let first = expr_land t in
  let rec aux lhs =
    let lhs = trailing t lhs in
    let loc = Tokenizer.loc t.tokens in
    match Tokenizer.peek t.tokens with
    | Op Or ->
      Tokenizer.skip t.tokens;
      let rhs = expr_land t in
      aux (with_loc ~loc (Binop { op = Or; lhs; rhs }))
    | _ -> lhs
  in
  aux first

and expr_land t : Expr.t =
  let first = expr_eq_neq t in
  let rec aux lhs =
    let lhs = trailing t lhs in
    let loc = Tokenizer.loc t.tokens in
    match Tokenizer.peek t.tokens with
    | Op And ->
      Tokenizer.skip t.tokens;
      let rhs = expr_eq_neq t in
      aux (with_loc ~loc (Binop { op = And; lhs; rhs }))
    | _ -> lhs
  in
  aux first

and expr_eq_neq t : Expr.t =
  let first = expr_cmp t in
  let rec aux lhs =
    let lhs = trailing t lhs in
    let loc = Tokenizer.loc t.tokens in
    match Tokenizer.peek t.tokens with
    | Op Eq ->
      Tokenizer.skip t.tokens;
      let rhs = expr_cmp t in
      aux (with_loc ~loc (Binop { op = Eq; lhs; rhs }))
    | Op Neq ->
      Tokenizer.skip t.tokens;
      let rhs = expr_cmp t in
      aux (with_loc ~loc (Binop { op = Neq; lhs; rhs }))
    | _ -> lhs
  in
  aux first

and expr_cmp t : Expr.t =
  let first = expr_add_sub t in
  let rec aux lhs =
    let lhs = trailing t lhs in
    let loc = Tokenizer.loc t.tokens in
    match Tokenizer.peek t.tokens with
    | Op Lt ->
      Tokenizer.skip t.tokens;
      let rhs = expr_add_sub t in
      aux (with_loc ~loc (Binop { op = Lt; lhs; rhs }))
    | Op Gt ->
      Tokenizer.skip t.tokens;
      let rhs = expr_add_sub t in
      aux (with_loc ~loc (Binop { op = Gt; lhs; rhs }))
    | Op Lte ->
      Tokenizer.skip t.tokens;
      let rhs = expr_add_sub t in
      aux (with_loc ~loc (Binop { op = Lte; lhs; rhs }))
    | Op Gte ->
      Tokenizer.skip t.tokens;
      let rhs = expr_add_sub t in
      aux (with_loc ~loc (Binop { op = Gte; lhs; rhs }))
    | _ -> lhs
  in
  aux first

and expr_add_sub t : Expr.t =
  let first = expr_mul_div_mod t in
  let rec aux lhs =
    let lhs = trailing t lhs in
    let loc = Tokenizer.loc t.tokens in
    match Tokenizer.peek t.tokens with
    | Op Plus ->
      Tokenizer.skip t.tokens;
      let rhs = expr_mul_div_mod t in
      aux (with_loc ~loc (Binop { op = Add; lhs; rhs }))
    | Op Minus ->
      Tokenizer.skip t.tokens;
      let rhs = expr_mul_div_mod t in
      aux (with_loc ~loc (Binop { op = Sub; lhs; rhs }))
    | _ -> lhs
  in
  aux first

and expr_mul_div_mod t : Expr.t =
  let first = expr_neg t in
  let rec aux lhs =
    let lhs = trailing t lhs in
    let loc = Tokenizer.loc t.tokens in
    match Tokenizer.peek t.tokens with
    | Op Star ->
      Tokenizer.skip t.tokens;
      let rhs = expr_neg t in
      aux (with_loc ~loc (Binop { op = Mul; lhs; rhs }))
    | Op Slash ->
      Tokenizer.skip t.tokens;
      let rhs = expr_neg t in
      aux (with_loc ~loc (Binop { op = Div; lhs; rhs }))
    | Op Percent ->
      Tokenizer.skip t.tokens;
      let rhs = expr_neg t in
      aux (with_loc ~loc (Binop { op = Mod; lhs; rhs }))
    | _ -> lhs
  in
  aux first

and expr_neg t : Expr.t =
  let loc, comments = leading t in
  match Tokenizer.peek t.tokens with
  | Op Minus ->
    Tokenizer.skip t.tokens;
    let arg = expr_app t in
    with_loc ~loc ~before:comments (Unop { op = Neg; arg })
  | _ ->
    let expr = expr_app t in
    if List.is_empty comments then expr else { expr with before = comments @ expr.before }

and can_start_atom t =
  match Tokenizer.peek t.tokens with
  | Op Not
  | Op Amp
  | Assert
  | Box
  | Match
  | If
  | Fun
  | Fn
  | Let
  | Lparen
  | Unit
  | Bool _
  | Int _
  | Ident _
  | Unreachable -> true
  | _ -> false

and expr_app t : Expr.t =
  let first = expr_lnot t in
  let rec aux acc = if can_start_atom t then aux (trailing t (expr_lnot t) :: acc) else acc in
  let args = aux [] in
  List.fold_right args ~init:(trailing t first) ~f:(fun exp (acc : _ With_loc.t) ->
    with_loc ~loc:acc.loc (Apply { fn = acc; arg = exp }))

and expr_lnot t : Expr.t =
  let loc, comments = leading t in
  match Tokenizer.peek t.tokens with
  | Op Not ->
    Tokenizer.skip t.tokens;
    let arg = expr_select t in
    with_loc ~loc ~before:comments (Unop { op = Not; arg })
  | Op Amp ->
    Tokenizer.skip t.tokens;
    let arg = expr_select t in
    with_loc ~loc ~before:comments (Ref { arg })
  | _ ->
    let expr = expr_select t in
    if List.is_empty comments then expr else { expr with before = comments @ expr.before }

and expr_select t : Expr.t =
  let first = expr_primary t in
  let rec aux expr =
    match Tokenizer.peek t.tokens with
    | Label label ->
      Tokenizer.skip t.tokens;
      let label = Label.of_string label in
      aux (with_loc ~loc:(Cst.Expr.loc expr) (Expr.Select { expr; label }))
    | _ -> expr
  in
  aux first

and expr_primary t : Expr.t =
  let loc, comments = leading t in
  let node =
    match Tokenizer.next t.tokens with
    | Unreachable -> Expr.Unreachable
    | Assert ->
      let _, before_erased = leading t in
      let erased = maybe_erased t in
      let cond = expr_lnot t in
      Assert { cond; erased; before_erased }
    | Box ->
      let arg = expr_lnot t in
      Box { arg }
    | Match ->
      let _, before_elimination = leading t in
      let eliminator = maybe_eliminator t in
      let cond = expr (without_commas t) in
      expect t ~kind:Lbrace;
      let arms = match_arms t in
      expect t ~kind:Rbrace;
      Match { cond; arms; eliminator; before_elimination }
    | If ->
      let _, before_erased = leading t in
      let erased = maybe_erased t in
      let cond = expr t in
      expect t ~kind:Then;
      let then_ = expr t in
      expect t ~kind:Else;
      let else_ = expr t in
      If { cond; then_; else_; erased; before_erased }
    | Fun ->
      let funs = expr_funs ~loc t [] in
      let rest = expr t in
      Fun { funs; rest }
    | Fn ->
      let _, before_erased = leading t in
      let erased = maybe_erased t in
      let arg = parse_arg t in
      let _, after_arg = leading t in
      expect_op t ~op:Arrow;
      let body = expr t in
      Lambda { erased; arg; body; before_erased; after_arg }
    | Let ->
      let _, before_erased = leading t in
      let erased = maybe_erased t in
      let _, after_erased = leading t in
      let var = expect_ident t in
      let _, after_var = leading t in
      expect t ~kind:Asn;
      let bind = expr t in
      expect t ~kind:In;
      let rest = expr t in
      Let { var; erased; bind; rest; before_erased; after_erased; after_var }
    | Lparen ->
      (match Tokenizer.peek2 t.tokens with
       | Op op, Rparen ->
         (match op_to_ident op with
          | Some id ->
            Tokenizer.skip t.tokens;
            Tokenizer.skip t.tokens;
            Var { id }
          | None ->
            let exp = expr (with_commas t) in
            expect t ~kind:Rparen;
            Paren { expr = exp })
       | _ ->
         let exp = expr (with_commas t) in
         expect t ~kind:Rparen;
         Paren { expr = exp })
    | Unit -> Literal { value = Unit }
    | Bool value -> Literal { value = Bool value }
    | Int value -> Literal { value = Int value }
    | Ident id -> Var { id = Ident.id id }
    | Label name -> Constructor { label = Label.of_string name }
    | Variant ->
      expect t ~kind:Lbrace;
      let constructors = variant_constructors t in
      expect t ~kind:Rbrace;
      Variant { constructors }
    | tok -> Fail.unexpected [%here] ~loc tok
  in
  with_loc ~loc ~before:comments node

and variant_constructor t : Expr.constructor =
  let loc, before = leading t in
  let label = Label.of_string (expect t ~kind:Ident) in
  let _, after_label = leading t in
  let payload =
    match Tokenizer.peek t.tokens with
    | Colon ->
      Tokenizer.skip t.tokens;
      Some (trailing t (expr (without_commas t)))
    | _ -> None
  in
  With_loc.create ~before ~loc { Expr.label; payload; after_label }

and variant_constructors t : Expr.constructor Nonempty_list.t =
  braced_items t ~f:variant_constructor

and expr_fun t : Expr.fun_ =
  let loc, before = leading t in
  let erased = maybe_erased t in
  let _, after_erased = leading t in
  let var =
    let name = expect t ~kind:Ident in
    if String.equal name "_" then Fail.fun_underscore [%here] ~loc else Ident.id name
  in
  let arg = parse_arg t in
  let _, after_arg = leading t in
  expect t ~kind:Colon;
  let ret_mode = maybe_modes ~required:false t in
  let ret_ty = expr t in
  expect t ~kind:Asn;
  let body = expr t in
  With_loc.create
    ~before
    ~loc
    { Expr.var; erased; arg; ret_mode; ret_ty; body; after_erased; after_arg }

and expr_funs ~loc t fs : Expr.fun_ Nonempty_list.t =
  let f = expr_fun t in
  match Tokenizer.next t.tokens with
  | And -> expr_funs ~loc t (f :: fs)
  | In -> Nonempty_list.reverse (f :: fs)
  | tok -> Fail.unexpected [%here] ~loc tok

and parse_arg t : Expr.arg =
  let loc, before = leading t in
  expect t ~kind:Lparen;
  let _, after_open = leading t in
  let mode = maybe_modes ~required:false t in
  let _, after_mode = leading t in
  let var = expect_ident t in
  let _, after_var = leading t in
  expect t ~kind:Colon;
  let ty = expr (with_commas t) in
  expect t ~kind:Rparen;
  With_loc.create ~before ~loc Expr.{ var; mode; ty; after_open; after_mode; after_var }

and can_start_pattern_atom t =
  match Tokenizer.peek t.tokens with
  | Unit | Bool _ | Int _ | Ident _ | Label _ | Lparen | Op Amp -> true
  | _ -> false

and pattern_atom t : Expr.pattern =
  let loc, before = leading t in
  let node : Expr.pattern_node =
    match Tokenizer.next t.tokens with
    | Unit -> Literal { value = Unit }
    | Bool value -> Literal { value = Bool value }
    | Int value -> Literal { value = Int value }
    | Ident id -> Var { id = Ident.id id }
    | Label name ->
      let payload = if can_start_pattern_atom t then Some (pattern_atom t) else None in
      Constructor { label = Label.of_string name; payload }
    | Op Amp -> Ref { payload = pattern_atom t }
    | Lparen ->
      let inner = parse_pattern t in
      expect t ~kind:Rparen;
      inner.node
    | tok -> Fail.unexpected [%here] ~loc tok
  in
  With_loc.create ~before ~loc node

and pattern_tuple t : Expr.pattern =
  let first = pattern_atom t in
  match Tokenizer.peek t.tokens with
  | Op Comma ->
    let rec aux acc =
      match Tokenizer.peek t.tokens with
      | Op Comma ->
        Tokenizer.skip t.tokens;
        aux (Nonempty_list.cons (pattern_atom t) acc)
      | _ -> Nonempty_list.reverse acc
    in
    let elts = aux (Nonempty_list.singleton first) in
    let node : Expr.pattern_node = Tuple { elts } in
    With_loc.create ~loc:first.loc node
  | _ -> first

and parse_pattern t : Expr.pattern =
  let first = pattern_tuple t in
  let rec aux left =
    match Tokenizer.peek t.tokens with
    | Pipe ->
      Tokenizer.skip t.tokens;
      let right = pattern_tuple t in
      let node : Expr.pattern_node = Or { left; right } in
      aux (With_loc.create ~loc:first.loc node)
    | _ -> left
  in
  aux first

and arm_pattern t : Expr.pattern =
  let first = pattern_atom t in
  let rec aux left =
    match Tokenizer.peek t.tokens with
    | Pipe ->
      Tokenizer.skip t.tokens;
      let right = pattern_atom t in
      let node : Expr.pattern_node = Or { left; right } in
      aux (With_loc.create ~loc:first.loc node)
    | _ -> left
  in
  aux first

and match_arm t : Expr.pattern * Expr.t =
  let pat = arm_pattern t in
  let _, after = leading t in
  expect_op t ~op:Arrow;
  let rhs = expr (without_commas t) in
  { pat with after = pat.after @ after }, rhs

and match_arms t : (Expr.pattern * Expr.t) Nonempty_list.t = braced_items t ~f:match_arm

let rec top_level_funs ~loc t fs : Top_level.t =
  let f = expr_fun t in
  match Tokenizer.next t.tokens with
  | And -> top_level_funs ~loc t (f :: fs)
  | Double_semicolon ->
    With_loc.create ~loc (Top_level.Fun { funs = Nonempty_list.reverse (f :: fs) })
  | tok -> Fail.unexpected [%here] ~loc tok
;;

let top_level t : Top_level.t =
  let loc, comments = leading t in
  let tl =
    match Tokenizer.next t.tokens with
    | Fun -> top_level_funs ~loc t []
    | Let ->
      let _, before_erased = leading t in
      let erased = maybe_erased t in
      let _, after_erased = leading t in
      let var = expect_ident t in
      let _, after_var = leading t in
      expect t ~kind:Asn;
      let bind = expr t in
      expect t ~kind:Double_semicolon;
      With_loc.create
        ~loc
        (Top_level.Let { var; erased; bind; before_erased; after_erased; after_var })
    | External ->
      let var = expect_ident t in
      expect t ~kind:Colon;
      let ty = expr t in
      expect t ~kind:Asn;
      let symbol = expect t ~kind:Ident in
      expect t ~kind:Double_semicolon;
      With_loc.create ~loc (Top_level.External { var; symbol; ty })
    | Builtin ->
      let var = expect_ident t in
      expect t ~kind:Asn;
      let name = expect t ~kind:Ident in
      expect t ~kind:Double_semicolon;
      With_loc.create ~loc (Top_level.Builtin { var; name })
    | tok -> Fail.unexpected [%here] ~loc tok
  in
  if List.is_empty comments then tl else { tl with before = comments }
;;

let top_level_list t : Program.t =
  let rec aux acc =
    match Tokenizer.peek t.tokens with
    | Eof ->
      let _, after = Tokenizer.comments t.tokens in
      { Program.items = List.rev acc; after }
    | _ -> aux (top_level t :: acc)
  in
  aux []
;;

let parse_exn input : Program.t =
  let t = { tokens = Tokenizer.create input; no_commas = false } in
  top_level_list t
;;

let parse input =
  try Ok (parse_exn input) with
  | Error err -> Error err
  | Lex.Unterminated_comment loc -> Error { loc; here = [%here]; reason = Unterminated_comment }
;;
