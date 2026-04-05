open! Core
open Lex
open Cst
module Ident = Ident.Raw

type t = { tokens : Tokenizer.t }

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
  let unexpected ~loc tok = raise (Error { loc; reason = Unexpected tok })
  let duplicate_mode ~loc mode = raise (Error { loc; reason = Duplicate_mode mode })
  let unexpected_modes ~loc mode = raise (Error { loc; reason = Unexpected_modes mode })
  let fun_underscore ~loc = raise (Error { loc; reason = Fun_underscore })
end

let expect (type a) t ~(kind : a Kind.t) : a =
  let loc = Tokenizer.loc t.tokens in
  let tok = Tokenizer.next t.tokens in
  match Token.get ~kind tok with
  | Some k -> k
  | None -> Fail.unexpected ~loc tok
;;

let expect_op t ~op =
  let loc = Tokenizer.loc t.tokens in
  let tok = Tokenizer.peek t.tokens in
  if Lex.Op.equal (expect t ~kind:Op) op then () else Fail.unexpected ~loc tok
;;

let expect_ident t =
  let loc = Tokenizer.loc t.tokens in
  match Tokenizer.next t.tokens with
  | Ident id -> Ident.id id
  | Lparen ->
    (match Tokenizer.next t.tokens with
     | Op op ->
       let id =
         match op with
         | Tilde_minus -> Ident.(unop Neg)
         | Not -> Ident.(unop Not)
         | Minus -> Ident.(binop Sub)
         | Plus -> Ident.(binop Add)
         | Star -> Ident.(binop Mul)
         | Slash -> Ident.(binop Div)
         | Percent -> Ident.(binop Mod)
         | And -> Ident.(binop And)
         | Or -> Ident.(binop Or)
         | Eq -> Ident.(binop Eq)
         | Neq -> Ident.(binop Neq)
         | Lt -> Ident.(binop Lt)
         | Lte -> Ident.(binop Lte)
         | Gt -> Ident.(binop Gt)
         | Gte -> Ident.(binop Gte)
         | op -> Fail.unexpected ~loc (Op op)
       in
       expect t ~kind:Rparen;
       id
     | tok -> Fail.unexpected ~loc tok)
  | tok -> Fail.unexpected ~loc tok
;;

let maybe_erased t : Modes.Erasure.t =
  match Tokenizer.peek t.tokens with
  | Erased ->
    expect t ~kind:Erased;
    Erased
  | _ -> Unerased
;;

let maybe_static t : Modes.Staticity.t =
  match Tokenizer.peek t.tokens with
  | Static ->
    expect t ~kind:Static;
    Static
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
  if required && List.length axes = 0 then Fail.unexpected ~loc (Tokenizer.peek t.tokens);
  (match List.find_a_dup axes ~compare:Modes.Axis.compare with
   | Some mode -> Fail.duplicate_mode ~loc mode
   | None -> ());
  { staticity; erasure }
;;

let with_loc ~loc ?(before = []) node : Expr.t = With_loc.create ~before ~loc node
let leading t = Tokenizer.comments t.tokens

let trailing t (expr : Expr.t) : Expr.t =
  let _, comments = Tokenizer.comments t.tokens in
  if List.is_empty comments then expr else { expr with after = expr.after @ comments }
;;

let rec expr t : Expr.t = expr_mode_annot t

and expr_mode_annot t : Expr.t =
  let expr = trailing t (expr_ty_annot t) in
  match Tokenizer.peek t.tokens with
  | At ->
    let loc = Tokenizer.loc t.tokens in
    expect t ~kind:At;
    let mode = maybe_modes ~required:true t in
    with_loc ~loc (Mode_annotation { expr; mode })
  | _ -> expr

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
  | _ -> if Modes.Maybe.(equal arg_mode none) then arg else Fail.unexpected_modes ~loc arg_mode

and expr_comma t : Expr.t =
  let loc = Tokenizer.loc t.tokens in
  let first = expr_caret t in
  let first = trailing t first in
  match Tokenizer.peek t.tokens with
  | Op Comma ->
    let rec aux acc =
      match Tokenizer.peek t.tokens with
      | Op Comma ->
        Tokenizer.skip t.tokens;
        let e = expr_caret t in
        aux (trailing t e :: acc)
      | _ -> List.rev acc
    in
    with_loc ~loc (Make_tuple { elts = aux [ first ] })
  | _ -> first

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
        aux (trailing t e :: acc)
      | _ -> List.rev acc
    in
    with_loc ~loc (Tuple { elts = aux [ first ] })
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
  | Assert | Match | If | Fun | Fn | Let | Lparen | Unit | Bool _ | Int _ | Ident _ | Unreachable ->
    true
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
    let arg = expr_primary t in
    with_loc ~loc ~before:comments (Unop { op = Not; arg })
  | _ ->
    let expr = expr_primary t in
    if List.is_empty comments then expr else { expr with before = comments @ expr.before }

and parse_arg t : Expr.arg =
  let loc, before = leading t in
  expect t ~kind:Lparen;
  let _, after_open = leading t in
  let mode = maybe_modes ~required:false t in
  let _, after_mode = leading t in
  let var = expect_ident t in
  let _, after_var = leading t in
  expect t ~kind:Colon;
  let ty = expr t in
  expect t ~kind:Rparen;
  With_loc.create ~before ~loc Expr.{ var; mode; ty; after_open; after_mode; after_var }

and require_args t : Expr.arg Nonempty_list.t =
  let first = parse_arg t in
  let rest = parse_args t in
  Nonempty_list.create first rest

and parse_args t : Expr.arg list =
  let rec aux acc =
    match Tokenizer.peek t.tokens with
    | Lparen -> aux (parse_arg t :: acc)
    | _ -> List.rev acc
  in
  aux []

and parse_arm t : Expr.pattern * Expr.t =
  let loc, before = leading t in
  let id = expect_ident t in
  let _, after = leading t in
  expect_op t ~op:Arrow;
  let rhs = expr t in
  let pat : Expr.pattern_node = Var { id } in
  With_loc.create ~before ~after ~loc pat, rhs

and parse_arms t : (Expr.pattern * Expr.t) Nonempty_list.t =
  expect t ~kind:Pipe;
  let first = parse_arm t in
  let rec aux acc =
    match Tokenizer.peek t.tokens with
    | Pipe ->
      Tokenizer.skip t.tokens;
      aux (parse_arm t :: acc)
    | _ -> Nonempty_list.of_list_exn (List.rev acc)
  in
  aux [ first ]

and expr_primary t : Expr.t =
  let loc, comments = leading t in
  let node =
    match Tokenizer.next t.tokens with
    | Unreachable -> Expr.Unreachable
    | Assert ->
      let _, before_static = leading t in
      let static = maybe_static t in
      let cond = expr_lnot t in
      Assert { cond; static; before_static }
    | Match ->
      let _, before_static = leading t in
      let static = maybe_static t in
      let cond = expr t in
      expect t ~kind:With;
      let arms = parse_arms t in
      Match { cond; arms; static; before_static }
    | If ->
      let _, before_static = leading t in
      let static = maybe_static t in
      let cond = expr t in
      expect t ~kind:Then;
      let then_ = expr t in
      expect t ~kind:Else;
      let else_ = expr t in
      If { cond; then_; else_; static; before_static }
    | Fun ->
      let funs = expr_funs ~loc t [] in
      let rest = expr t in
      Fun { funs; rest }
    | Fn ->
      let _, before_erased = leading t in
      let erased = maybe_erased t in
      let args = require_args t in
      let _, after_args = leading t in
      expect_op t ~op:Arrow;
      let body = expr t in
      Lambda { erased; args; body; before_erased; after_args }
    | Let ->
      let _, before_erased = leading t in
      let erased = maybe_erased t in
      let _, after_erased = leading t in
      let var = expect_ident t in
      let args = parse_args t in
      let _, after_args = leading t in
      expect t ~kind:Asn;
      let bind = expr t in
      expect t ~kind:In;
      let rest = expr t in
      Let { var; erased; args; bind; rest; before_erased; after_erased; after_args }
    | Lparen ->
      let exp = expr t in
      expect t ~kind:Rparen;
      Paren { expr = exp }
    | Unit -> Literal { value = Unit }
    | Bool value -> Literal { value = Bool value }
    | Int value -> Literal { value = Int value }
    | Ident id -> Var { id = Ident.id id }
    | tok -> Fail.unexpected ~loc tok
  in
  with_loc ~loc ~before:comments node

and expr_fun t : Expr.fun_ =
  let loc, before = leading t in
  let erased = maybe_erased t in
  let _, after_erased = leading t in
  let var =
    let name = expect t ~kind:Ident in
    if String.equal name "_" then Fail.fun_underscore ~loc else Ident.id name
  in
  let args = require_args t in
  let _, after_args = leading t in
  expect t ~kind:Colon;
  let ret_mode = maybe_modes ~required:false t in
  let ret_ty = expr t in
  expect t ~kind:Asn;
  let body = expr t in
  With_loc.create ~before ~loc { Expr.var; erased; args; ret_mode; ret_ty; body; after_erased; after_args }

and expr_funs ~loc t fs : Expr.fun_ Nonempty_list.t =
  let f = expr_fun t in
  match Tokenizer.next t.tokens with
  | And -> expr_funs ~loc t (f :: fs)
  | In -> Nonempty_list.reverse (f :: fs)
  | tok -> Fail.unexpected ~loc tok
;;

let rec top_level_funs ~loc t fs : Top_level.t =
  let f = expr_fun t in
  match Tokenizer.next t.tokens with
  | And -> top_level_funs ~loc t (f :: fs)
  | Double_semicolon ->
    With_loc.create ~loc (Top_level.Fun { funs = Nonempty_list.reverse (f :: fs) })
  | tok -> Fail.unexpected ~loc tok
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
      let args = parse_args t in
      let _, after_args = leading t in
      expect t ~kind:Asn;
      let bind = expr t in
      expect t ~kind:Double_semicolon;
      With_loc.create ~loc (Top_level.Let { var; erased; args; bind; before_erased; after_erased; after_args })
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
    | tok -> Fail.unexpected ~loc tok
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
  let t = { tokens = Tokenizer.create input } in
  top_level_list t
;;

let parse input =
  try Ok (parse_exn input) with
  | Error err -> Error err
  | Lex.Unterminated_comment loc -> Error { loc; reason = Unterminated_comment }
;;
