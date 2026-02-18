open! Core
open Modes

module Literal = struct
  type t =
    | Unit
    | Bool of bool
    | Int of int64
  [@@deriving sexp]

  let print () = function
    | Unit -> "()"
    | Bool b -> sprintf "%b" b
    | Int i -> sprintf "%Ld" i
  ;;
end

module Unop = struct
  type t =
    | Not
    | Neg
  [@@deriving sexp]

  let print () = function
    | Not -> "!"
    | Neg -> "-"
  ;;
end

module Binop = struct
  type t =
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | And
    | Or
    | Eq
    | Neq
    | Lt
    | Lte
    | Gt
    | Gte
  [@@deriving sexp]

  let print () = function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Mod -> "%"
    | And -> "&&"
    | Or -> "||"
    | Eq -> "=="
    | Neq -> "!="
    | Lt -> "<"
    | Lte -> "<="
    | Gt -> ">"
    | Gte -> ">="
  ;;
end

module Expr = struct
  type fun_ =
    { var : Ident.t
    ; arg : Ident.t
    ; erased : Erasure.t
    ; arg_mode : Modes.Maybe.t
    ; arg_ty : t
    ; ret_mode : Modes.Maybe.t
    ; ret_ty : t
    ; body : t
    ; loc : Lex.Location.t
    }

  and t =
    | If of
        { cond : t
        ; then_ : t
        ; else_ : t
        ; static : bool
        ; loc : Lex.Location.t
        }
    | Let of
        { var : Ident.t
        ; bind : t
        ; rest : t
        ; loc : Lex.Location.t
        }
    | Fun of
        { funs : fun_ Nonempty_list.t
        ; rest : t
        ; loc : Lex.Location.t
        }
    | Lambda of
        { arg : Ident.t
        ; erased : Erasure.t
        ; arg_mode : Modes.Maybe.t
        ; arg_ty : t
        ; body : t
        ; loc : Lex.Location.t
        }
    | Apply of
        { fn : t
        ; arg : t
        ; loc : Lex.Location.t
        }
    | Paren of
        { expr : t
        ; loc : Lex.Location.t
        }
    | Var of
        { id : Ident.t
        ; loc : Lex.Location.t
        }
    | Literal of
        { value : Literal.t
        ; loc : Lex.Location.t
        }
    | Unop of
        { op : Unop.t
        ; arg : t
        ; loc : Lex.Location.t
        }
    | Binop of
        { op : Binop.t
        ; lhs : t
        ; rhs : t
        ; loc : Lex.Location.t
        }
    | Arrow of
        { arg : t
        ; arg_id : Ident.t Option.t
        ; arg_mode : Modes.Maybe.t
        ; ret : t
        ; ret_mode : Modes.Maybe.t
        ; loc : Lex.Location.t
        }
    | Type_annotation of
        { expr : t
        ; ty : t
        ; loc : Lex.Location.t
        }
    | Mode_annotation of
        { expr : t
        ; mode : Modes.Maybe.t
        ; loc : Lex.Location.t
        }
  [@@deriving sexp]

  let rec free_vars (expr : t) : Ident.Set.t =
    match expr with
    | Paren { expr; _ } -> free_vars expr
    | Arrow { arg; ret; _ } -> Set.union (free_vars arg) (free_vars ret)
    | Var { id; _ } -> Ident.Set.singleton id
    | Mode_annotation { expr; _ } -> free_vars expr
    | Type_annotation { expr; ty; _ } -> Set.union (free_vars expr) (free_vars ty)
    | Literal _ -> Ident.Set.empty
    | Unop { arg; _ } -> free_vars arg
    | Binop { lhs; rhs; _ } -> Set.union (free_vars lhs) (free_vars rhs)
    | If { cond; then_; else_; _ } ->
      Ident.Set.union_list [ free_vars cond; free_vars then_; free_vars else_ ]
    | Let { var; bind; rest; _ } -> Set.union (free_vars bind) (Set.remove (free_vars rest) var)
    | Apply { fn; arg; _ } -> Set.union (free_vars fn) (free_vars arg)
    | Lambda { arg; arg_ty; body; _ } ->
      let fv_body = Set.remove (free_vars body) arg in
      Set.union fv_body (free_vars arg_ty)
    | Fun { funs; rest; _ } ->
      let bound_ids =
        Nonempty_list.map funs ~f:(fun f -> f.var) |> Nonempty_list.to_list |> Ident.Set.of_list
      in
      let fvs_in_funs =
        Nonempty_list.fold funs ~init:Ident.Set.empty ~f:(fun acc f ->
          let fv_arg_ty = free_vars f.arg_ty in
          let fv_ret_ty = free_vars f.ret_ty in
          let fv_body = Set.remove (free_vars f.body) f.arg in
          Ident.Set.union_list [ acc; fv_arg_ty; fv_ret_ty; fv_body ])
      in
      Set.diff (Set.union fvs_in_funs (free_vars rest)) bound_ids
  ;;

  let loc = function
    | If { loc; _ }
    | Fun { loc; _ }
    | Let { loc; _ }
    | Lambda { loc; _ }
    | Apply { loc; _ }
    | Paren { loc; _ }
    | Var { loc; _ }
    | Literal { loc; _ }
    | Unop { loc; _ }
    | Binop { loc; _ }
    | Arrow { loc; _ }
    | Type_annotation { loc; _ }
    | Mode_annotation { loc; _ } -> loc
  ;;

  let maybe_erased () : Erasure.t -> _ = function
    | Erased -> " erased"
    | Unerased -> ""
  ;;

  let rec print_fun is_and = function
    | { var; arg; erased; arg_mode; arg_ty; ret_mode; ret_ty; body; _ } ->
      sprintf
        "%s %a%a (%a %a : %a) : %a%a = %a"
        (if is_and then "and" else "fun")
        Ident.print
        var
        maybe_erased
        erased
        Modes.Maybe.print
        arg_mode
        Ident.print
        arg
        print
        arg_ty
        Modes.Maybe.print
        ret_mode
        print
        ret_ty
        print
        body

  and print () = function
    | If { cond; then_; else_; static; _ } ->
      let static = if static then " static" else "" in
      sprintf "if%s %a then %a else %a" static print cond print then_ print else_
    | Fun { funs; rest; _ } ->
      let funs = Nonempty_list.mapi funs ~f:(fun i f -> print_fun (i > 0) f) in
      let funs = String.concat ~sep:" " (Nonempty_list.to_list funs) in
      sprintf "%s; %a" funs print rest
    | Let { var; bind; rest; _ } -> sprintf "let %a = %a; %a" Ident.print var print bind print rest
    | Lambda { arg; erased; arg_mode; arg_ty; body; _ } ->
      sprintf
        "fn%a (%a%a : %a) -> %a"
        maybe_erased
        erased
        Modes.Maybe.print
        arg_mode
        Ident.print
        arg
        print
        arg_ty
        print
        body
    | Apply { fn; arg; _ } -> sprintf "%a %a" print fn print arg
    | Paren { expr; _ } -> sprintf "(%a)" print expr
    | Var { id; _ } -> Ident.print () id
    | Literal { value; _ } -> sprintf "%a" Literal.print value
    | Unop { op; arg; _ } -> sprintf "%a%a" Unop.print op print arg
    | Binop { op; lhs; rhs; _ } -> sprintf "%a %a %a" print lhs Binop.print op print rhs
    | Arrow { arg; arg_id; arg_mode; ret; ret_mode; _ } ->
      let arg_id =
        match arg_id with
        | Some id -> " \\ " ^ Ident.to_string id
        | None -> ""
      in
      sprintf
        "%a%a%s -> %a%a"
        Modes.Maybe.print
        arg_mode
        print
        arg
        arg_id
        Modes.Maybe.print
        ret_mode
        print
        ret
    | Mode_annotation { expr; mode; _ } -> sprintf "%a @ %a" print expr Modes.Maybe.print mode
    | Type_annotation { expr; ty; _ } -> sprintf "%a : %a" print expr print ty
  ;;
end

module Top_level = struct
  type t =
    | Let of
        { var : Ident.t
        ; bind : Expr.t
        ; loc : Lex.Location.t
        }
    | Fun of
        { funs : Expr.fun_ Nonempty_list.t
        ; loc : Lex.Location.t
        }
    | External of
        { var : Ident.t
        ; ty : Expr.t
        ; symbol : string
        ; loc : Lex.Location.t
        }
  [@@deriving sexp]

  let loc = function
    | Fun { loc; _ } | Let { loc; _ } | External { loc; _ } -> loc
  ;;

  let print_fun is_and = function
    | Expr.{ var; arg; arg_mode; arg_ty; ret_mode; ret_ty; body; _ } ->
      sprintf
        "%s %a (%a %a : %a) : %a%a = %a"
        (if is_and then "and" else "fun")
        Ident.print
        var
        Modes.Maybe.print
        arg_mode
        Ident.print
        arg
        Expr.print
        arg_ty
        Modes.Maybe.print
        ret_mode
        Expr.print
        ret_ty
        Expr.print
        body
  ;;

  let print () = function
    | Fun { funs; _ } ->
      let funs = Nonempty_list.mapi funs ~f:(fun i f -> print_fun (i > 0) f) in
      String.concat ~sep:"\n" (Nonempty_list.to_list funs) ^ ";;"
    | Let { var; bind; _ } -> sprintf "let %a = %a;;" Ident.print var Expr.print bind
    | External { var; ty; symbol; _ } ->
      sprintf "external %a : %a = %s;;" Ident.print var Expr.print ty symbol
  ;;
end

module Program = struct
  type t = Top_level.t list [@@deriving sexp]

  let print () t = List.map t ~f:(Top_level.print ()) |> String.concat ~sep:"\n"
end
