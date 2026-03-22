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

module Expr = struct
  type fun_ =
    { var : Ident.Raw.t
    ; arg : Ident.Raw.t
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
        ; static : Staticity.t
        ; loc : Lex.Location.t
        }
    | Let of
        { var : Ident.Raw.t
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
        { arg : Ident.Raw.t
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
        { id : Ident.Raw.t
        ; loc : Lex.Location.t
        }
    | Literal of
        { value : Literal.t
        ; loc : Lex.Location.t
        }
    | Unop of
        { op : Ident.Unop.t
        ; arg : t
        ; loc : Lex.Location.t
        }
    | Binop of
        { op : Ident.Binop.t
        ; lhs : t
        ; rhs : t
        ; loc : Lex.Location.t
        }
    | Nop of
        { op : Ident.Nop.t
        ; elts : t list
        ; loc : Lex.Location.t
        }
    | Arrow of
        { arg : t
        ; arg_id : Ident.Raw.t Option.t
        ; arg_mode : Modes.Maybe.t
        ; ret : t
        ; ret_mode : Modes.Maybe.t
        ; loc : Lex.Location.t
        }
    | Assert of
        { cond : t
        ; static : Staticity.t
        ; loc : Lex.Location.t
        }
    | Unreachable of { loc : Lex.Location.t }
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
    | Assert { loc; _ }
    | Unreachable { loc; _ }
    | Type_annotation { loc; _ }
    | Mode_annotation { loc; _ }
    | Nop { loc; _ } -> loc
  ;;

  let maybe_erased () : Erasure.t -> _ = function
    | Erased -> " erased"
    | Unerased -> ""
  ;;

  let maybe_static () : Staticity.t -> _ = function
    | Static -> " static"
    | Dynamic -> ""
  ;;

  let rec print_fun is_and = function
    | { var; arg; erased; arg_mode; arg_ty; ret_mode; ret_ty; body; _ } ->
      sprintf
        "%s %a%a (%a %a : %a) : %a%a = %a"
        (if is_and then "and" else "fun")
        Ident.Raw.print
        var
        maybe_erased
        erased
        Modes.Maybe.print
        arg_mode
        Ident.Raw.print
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
      sprintf "if%a %a then %a else %a" maybe_static static print cond print then_ print else_
    | Fun { funs; rest; _ } ->
      let funs = Nonempty_list.mapi funs ~f:(fun i f -> print_fun (i > 0) f) in
      let funs = String.concat ~sep:" " (Nonempty_list.to_list funs) in
      sprintf "%s; %a" funs print rest
    | Let { var; bind; rest; _ } ->
      sprintf "let %a = %a in %a" Ident.Raw.print var print bind print rest
    | Lambda { arg; erased; arg_mode; arg_ty; body; _ } ->
      sprintf
        "fn%a (%a%a : %a) -> %a"
        maybe_erased
        erased
        Modes.Maybe.print
        arg_mode
        Ident.Raw.print
        arg
        print
        arg_ty
        print
        body
    | Apply { fn; arg; _ } -> sprintf "%a %a" print fn print arg
    | Paren { expr; _ } -> sprintf "(%a)" print expr
    | Var { id; _ } -> Ident.Raw.print () id
    | Literal { value; _ } -> sprintf "%a" Literal.print value
    | Assert { cond; static; _ } -> sprintf "assert%a %a" maybe_static static print cond
    | Unreachable _ -> "unreachable"
    | Unop { op; arg; _ } -> sprintf "%a%a" Ident.Unop.print op print arg
    | Binop { op; lhs; rhs; _ } -> sprintf "%a %a %a" print lhs Ident.Binop.print op print rhs
    | Arrow { arg; arg_id; arg_mode; ret; ret_mode; _ } ->
      let arg_id =
        match arg_id with
        | Some id -> " \\ " ^ Ident.Raw.print () id
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
    | Nop { op; elts; _ } ->
      let elts = List.map elts ~f:(print ()) in
      let sep = Ident.Nop.sep op in
      String.concat ~sep elts
  ;;
end

module Top_level = struct
  type t =
    | Let of
        { var : Ident.Raw.t
        ; bind : Expr.t
        ; loc : Lex.Location.t
        }
    | Fun of
        { funs : Expr.fun_ Nonempty_list.t
        ; loc : Lex.Location.t
        }
    | External of
        { var : Ident.Raw.t
        ; ty : Expr.t
        ; symbol : string
        ; loc : Lex.Location.t
        }
    | Builtin of
        { var : Ident.Raw.t
        ; name : string
        ; loc : Lex.Location.t
        }
  [@@deriving sexp]

  let loc = function
    | Fun { loc; _ } | Let { loc; _ } | External { loc; _ } | Builtin { loc; _ } -> loc
  ;;

  let print_fun is_and = function
    | Expr.{ var; arg; arg_mode; arg_ty; ret_mode; ret_ty; body; _ } ->
      sprintf
        "%s %a (%a %a : %a) : %a%a = %a"
        (if is_and then "and" else "fun")
        Ident.Raw.print
        var
        Modes.Maybe.print
        arg_mode
        Ident.Raw.print
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
    | Let { var; bind; _ } -> sprintf "let %a = %a;;" Ident.Raw.print var Expr.print bind
    | External { var; ty; symbol; _ } ->
      sprintf "external %a : %a = %s;;" Ident.Raw.print var Expr.print ty symbol
    | Builtin { var; name; _ } -> sprintf "builtin %a = %s;;" Ident.Raw.print var name
  ;;
end

module Program = struct
  type t = Top_level.t list [@@deriving sexp]

  let print () t = List.map t ~f:(Top_level.print ()) |> String.concat ~sep:"\n"
end
