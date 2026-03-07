open! Core
open Lex
open Cst

type t = { tokens : Tokenizer.t }

module Error = struct
  type t =
    | Unexpected of Token.t
    | Duplicate_mode of Modes.t
    | Unexpected_modes of Modes.Modes.Maybe.t
    | Fun_underscore
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
         | Tilde_minus -> Ident.(unop Unop.Neg)
         | Not -> Ident.(unop Unop.Not)
         | Minus -> Ident.(binop Binop.Sub)
         | Plus -> Ident.(binop Binop.Add)
         | Star -> Ident.(binop Binop.Mul)
         | Slash -> Ident.(binop Binop.Div)
         | Percent -> Ident.(binop Binop.Mod)
         | And -> Ident.(binop Binop.And)
         | Or -> Ident.(binop Binop.Or)
         | Eq -> Ident.(binop Binop.Eq)
         | Neq -> Ident.(binop Binop.Neq)
         | Lt -> Ident.(binop Binop.Lt)
         | Lte -> Ident.(binop Binop.Lte)
         | Gt -> Ident.(binop Binop.Gt)
         | Gte -> Ident.(binop Binop.Gte)
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

let maybe_modes ~required t : Modes.Modes.Maybe.t =
  let open Modes in
  let loc = Tokenizer.loc t.tokens in
  let rec aux ?staticity ?erasure axes =
    match Tokenizer.peek t.tokens with
    | Static ->
      expect t ~kind:Static;
      aux ~staticity:Staticity.Static ?erasure (Staticity :: axes)
    | Dynamic ->
      expect t ~kind:Dynamic;
      aux ~staticity:Dynamic ?erasure (Staticity :: axes)
    | Erased ->
      expect t ~kind:Erased;
      aux ?staticity ~erasure:Erasure.Erased (Erasure :: axes)
    | Unerased ->
      expect t ~kind:Unerased;
      aux ?staticity ~erasure:Erasure.Unerased (Erasure :: axes)
    | _ -> staticity, erasure, axes
  in
  let staticity, erasure, axes = aux [] in
  if required && List.length axes = 0 then Fail.unexpected ~loc (Tokenizer.peek t.tokens);
  (match List.find_a_dup axes ~compare with
   | Some mode -> Fail.duplicate_mode ~loc mode
   | None -> ());
  { staticity; erasure }
;;

let rec expr t : Expr.t = expr_mode_annot t

and expr_mode_annot t : Expr.t =
  let expr = expr_ty_annot t in
  match Tokenizer.peek t.tokens with
  | At ->
    let loc = Tokenizer.loc t.tokens in
    expect t ~kind:At;
    let mode = maybe_modes ~required:true t in
    Mode_annotation { expr; mode; loc }
  | _ -> expr

and expr_ty_annot t : Expr.t =
  let expr = expr_arrow t in
  let loc = Tokenizer.loc t.tokens in
  match Tokenizer.peek t.tokens with
  | Colon ->
    Tokenizer.skip t.tokens;
    let ty = expr_arrow t in
    Type_annotation { expr; ty; loc }
  | _ -> expr

and expr_arrow t : Expr.t =
  let arg_mode = maybe_modes ~required:false t in
  let arg = expr_lor t in
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
      | Arrow arrow -> Expr.Arrow { arrow with arg_mode = modes }, Modes.Modes.Maybe.none
      | expr -> expr, modes
    in
    Arrow { arg; arg_id; arg_mode; ret; ret_mode; loc }
  | _ ->
    if Modes.Modes.Maybe.(equal arg_mode none) then arg else Fail.unexpected_modes ~loc arg_mode

and expr_lor t : Expr.t =
  let first = expr_land t in
  let rec aux lhs =
    let loc = Tokenizer.loc t.tokens in
    match Tokenizer.peek t.tokens with
    | Op Or ->
      Tokenizer.skip t.tokens;
      let rhs = expr_land t in
      aux (Expr.Binop { op = Or; lhs; rhs; loc })
    | _ -> lhs
  in
  aux first

and expr_land t : Expr.t =
  let first = expr_eq_neq t in
  let rec aux lhs =
    let loc = Tokenizer.loc t.tokens in
    match Tokenizer.peek t.tokens with
    | Op And ->
      Tokenizer.skip t.tokens;
      let rhs = expr_eq_neq t in
      aux (Expr.Binop { op = And; lhs; rhs; loc })
    | _ -> lhs
  in
  aux first

and expr_eq_neq t : Expr.t =
  let first = expr_cmp t in
  let rec aux lhs =
    let loc = Tokenizer.loc t.tokens in
    match Tokenizer.peek t.tokens with
    | Op Eq ->
      Tokenizer.skip t.tokens;
      let rhs = expr_cmp t in
      aux (Expr.Binop { op = Eq; lhs; rhs; loc })
    | Op Neq ->
      Tokenizer.skip t.tokens;
      let rhs = expr_cmp t in
      aux (Expr.Binop { op = Neq; lhs; rhs; loc })
    | _ -> lhs
  in
  aux first

and expr_cmp t : Expr.t =
  let first = expr_add_sub t in
  let rec aux lhs =
    let loc = Tokenizer.loc t.tokens in
    match Tokenizer.peek t.tokens with
    | Op Lt ->
      Tokenizer.skip t.tokens;
      let rhs = expr_add_sub t in
      aux (Expr.Binop { op = Lt; lhs; rhs; loc })
    | Op Gt ->
      Tokenizer.skip t.tokens;
      let rhs = expr_add_sub t in
      aux (Expr.Binop { op = Gt; lhs; rhs; loc })
    | Op Lte ->
      Tokenizer.skip t.tokens;
      let rhs = expr_add_sub t in
      aux (Expr.Binop { op = Lte; lhs; rhs; loc })
    | Op Gte ->
      Tokenizer.skip t.tokens;
      let rhs = expr_add_sub t in
      aux (Expr.Binop { op = Gte; lhs; rhs; loc })
    | _ -> lhs
  in
  aux first

and expr_add_sub t : Expr.t =
  let first = expr_mul_div_mod t in
  let rec aux lhs =
    let loc = Tokenizer.loc t.tokens in
    match Tokenizer.peek t.tokens with
    | Op Plus ->
      Tokenizer.skip t.tokens;
      let rhs = expr_mul_div_mod t in
      aux (Expr.Binop { op = Add; lhs; rhs; loc })
    | Op Minus ->
      Tokenizer.skip t.tokens;
      let rhs = expr_mul_div_mod t in
      aux (Expr.Binop { op = Sub; lhs; rhs; loc })
    | _ -> lhs
  in
  aux first

and expr_mul_div_mod t : Expr.t =
  let first = expr_neg t in
  let rec aux lhs =
    let loc = Tokenizer.loc t.tokens in
    match Tokenizer.peek t.tokens with
    | Op Star ->
      Tokenizer.skip t.tokens;
      let rhs = expr_neg t in
      aux (Expr.Binop { op = Mul; lhs; rhs; loc })
    | Op Slash ->
      Tokenizer.skip t.tokens;
      let rhs = expr_neg t in
      aux (Expr.Binop { op = Div; lhs; rhs; loc })
    | Op Percent ->
      Tokenizer.skip t.tokens;
      let rhs = expr_neg t in
      aux (Expr.Binop { op = Mod; lhs; rhs; loc })
    | _ -> lhs
  in
  aux first

and expr_neg t : Expr.t =
  let loc = Tokenizer.loc t.tokens in
  match Tokenizer.peek t.tokens with
  | Op Minus ->
    Tokenizer.skip t.tokens;
    let arg = expr_app t in
    Unop { op = Neg; arg; loc }
  | _ -> expr_app t

and can_start_atom t =
  match Tokenizer.peek t.tokens with
  | Op Not | Assert | If | Fun | Fn | Let | Lparen | Unit | Bool _ | Int _ | Ident _ | Unreachable
    -> true
  | _ -> false

and expr_app t : Expr.t =
  let first = expr_lnot t in
  let rec aux acc = if can_start_atom t then aux (expr_lnot t :: acc) else acc in
  let args = aux [] in
  List.fold_right args ~init:first ~f:(fun exp acc ->
    Expr.Apply { fn = acc; arg = exp; loc = Expr.loc acc })

and expr_lnot t : Expr.t =
  let loc = Tokenizer.loc t.tokens in
  match Tokenizer.peek t.tokens with
  | Op Not ->
    Tokenizer.skip t.tokens;
    let arg = expr_primary t in
    Unop { op = Not; arg; loc }
  | _ -> expr_primary t

and expr_primary t : Expr.t =
  let loc = Tokenizer.loc t.tokens in
  match Tokenizer.next t.tokens with
  | Unreachable -> Unreachable { loc }
  | Assert ->
    let static = maybe_static t in
    let cond = expr_lnot t in
    Assert { cond; static; loc }
  | If ->
    let static = maybe_static t in
    let cond = expr t in
    expect t ~kind:Then;
    let then_ = expr t in
    expect t ~kind:Else;
    let else_ = expr t in
    If { cond; then_; else_; static; loc }
  | Fun ->
    let funs = expr_funs ~loc t [] in
    let rest = expr t in
    Fun { funs; rest; loc }
  | Fn ->
    let erased = maybe_erased t in
    expect t ~kind:Lparen;
    let arg_mode = maybe_modes ~required:false t in
    let arg = expect_ident t in
    expect t ~kind:Colon;
    let arg_ty = expr t in
    expect t ~kind:Rparen;
    expect_op t ~op:Arrow;
    let body = expr t in
    Lambda { arg; erased; arg_mode; arg_ty; body; loc }
  | Let ->
    let var = expect_ident t in
    expect t ~kind:Asn;
    let bind = expr t in
    expect t ~kind:In;
    let rest = expr t in
    Let { var; bind; rest; loc }
  | Lparen ->
    let exp = expr t in
    expect t ~kind:Rparen;
    Paren { expr = exp; loc }
  | Unit -> Literal { value = Unit; loc }
  | Bool value -> Literal { value = Bool value; loc }
  | Int value -> Literal { value = Int value; loc }
  | Ident id -> Var { id = Ident.id id; loc }
  | tok -> Fail.unexpected ~loc tok

and expr_fun t : Expr.fun_ =
  let loc = Tokenizer.loc t.tokens in
  let erased = maybe_erased t in
  let var =
    let name = expect t ~kind:Ident in
    if String.equal name "_" then Fail.fun_underscore ~loc else Ident.id name
  in
  expect t ~kind:Lparen;
  let arg_mode = maybe_modes ~required:false t in
  let arg = expect_ident t in
  expect t ~kind:Colon;
  let arg_ty = expr t in
  expect t ~kind:Rparen;
  expect t ~kind:Colon;
  let ret_mode = maybe_modes ~required:false t in
  let ret_ty = expr t in
  expect t ~kind:Asn;
  let body = expr t in
  { var; arg; erased; arg_mode; arg_ty; ret_mode; ret_ty; body; loc }

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
  | Double_semicolon -> Fun { funs = Nonempty_list.reverse (f :: fs); loc }
  | tok -> Fail.unexpected ~loc tok
;;

let top_level t : Top_level.t =
  let loc = Tokenizer.loc t.tokens in
  match Tokenizer.next t.tokens with
  | Fun -> top_level_funs ~loc t []
  | Let ->
    let var = expect_ident t in
    expect t ~kind:Asn;
    let bind = expr t in
    expect t ~kind:Double_semicolon;
    Let { var; bind; loc }
  | External ->
    let var = expect_ident t in
    expect t ~kind:Colon;
    let ty = expr t in
    expect t ~kind:Asn;
    let symbol = expect t ~kind:Ident in
    expect t ~kind:Double_semicolon;
    External { var; symbol; ty; loc }
  | Builtin ->
    let var = expect_ident t in
    expect t ~kind:Asn;
    let name = expect t ~kind:Ident in
    expect t ~kind:Double_semicolon;
    Builtin { var; name; loc }
  | tok -> Fail.unexpected ~loc tok
;;

let top_level_list t : Top_level.t list =
  let rec aux acc =
    match Tokenizer.peek t.tokens with
    | Eof -> List.rev acc
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
;;
