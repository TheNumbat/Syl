open! Core

module rec Ty : sig
  type t =
    | Unit
    | Bool
    | Int
    | Arrow of
        { arg_ty : t
        ; ret_ty : t
        }
    | Pack
  [@@deriving sexp]
end

and Closure : sig
  type t =
    { arg : Ident.t
    ; ty : Ty.t
    ; body : Expr.t
    }
  [@@deriving sexp]
end

and Pack : sig
  module Mono : sig
    type t =
      { arg : Tst.Value.Concrete.t
      ; ty : Ty.t
      ; body : Expr.t
      }
    [@@deriving sexp]
  end

  type t = Mono.t Vec.t [@@deriving sexp]
end

and Value : sig
  type t =
    | Unit
    | Bool of bool
    | Int of int64
    | Closure of Closure.t
    | Pack of Pack.t
    | External of
        { symbol : string
        ; ty : Ty.t
        }
  [@@deriving sexp]
end

and Expr : sig
  type fun_ =
    | Mono of
        { var : Ident.t
        ; arg : Ident.t
        ; body : t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Pack of
        { var : Ident.t
        ; pack : Pack.t
        ; loc : Lex.Location.t
        }
  [@@deriving sexp]

  and t =
    | Literal of
        { value : Value.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Fun of
        { funs : fun_ Nonempty_list.t
        ; rest : t
        ; loc : Lex.Location.t
        }
    | Lambda of
        { arg : Ident.t
        ; ty : Ty.t
        ; body : t
        ; loc : Lex.Location.t
        }
    | Apply of
        { fn : t
        ; arg : t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Let of
        { var : Ident.t
        ; bind : t
        ; rest : t
        ; loc : Lex.Location.t
        }
    | Unop of
        { op : Cst.Unop.t
        ; arg : t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Binop of
        { op : Cst.Binop.t
        ; lhs : t
        ; rhs : t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | If of
        { cond : t
        ; then_ : t
        ; else_ : t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Var of
        { id : Ident.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Pack of
        { pack : Pack.t
        ; loc : Lex.Location.t
        }
    | Symbol of
        { id : Ident.t
        ; arg : Tst.Value.Concrete.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
  [@@deriving sexp]
end

module Top_level : sig
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
        ; symbol : string
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
  [@@deriving sexp]
end

module Program : sig
  type t = Top_level.t list [@@deriving sexp]
end
