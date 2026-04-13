open! Core

module Ty : sig
  type t =
    | Unit
    | Bool
    | Int
    | Arrow of
        { arg_ty : t
        ; ret_ty : t
        }
    | Tuple of t Nonempty_list.t
    | Pack of (Tst.Value.Concrete.t, t) Hashtbl.t
  [@@deriving sexp]

  val arg : t -> t
  val ret : t -> t
  val find : t -> Tst.Value.Concrete.t -> t
end

module Scalar : sig
  type t =
    | Unit
    | Bool of bool
    | Int of int64
  [@@deriving sexp]
end

module Access : sig
  type t =
    | Base
    | Keys of t Tst.Value.Concrete.Map.t
  [@@deriving sexp]

  val union : t -> t -> t
end

module Expr : sig
  type pack = (Tst.Value.Concrete.t, t * Ty.t Ident.Map.t) Hashtbl.t [@@deriving sexp]

  and fun_ =
    | Mono of
        { var : Ident.t
        ; arg : Ident.t
        ; body : t
        ; fvs : Ty.t Ident.Map.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Pack of
        { var : Ident.t
        ; pack : pack
        ; fvs : Ty.t Ident.Map.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
  [@@deriving sexp]

  and t =
    | Scalar of
        { value : Scalar.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Fun of
        { funs : fun_ Nonempty_list.t
        ; rest : t
        ; fvs : Ty.t Ident.Map.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Lambda of
        { arg : Ident.t
        ; body : t
        ; fvs : Ty.t Ident.Map.t
        ; ty : Ty.t
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
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Tuple of
        { elts : t Nonempty_list.t
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
    | Sequence of
        { first : t
        ; second : t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Pack of
        { pack : pack
        ; fvs : Ty.t Ident.Map.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Symbol of
        { fn : t
        ; arg : Tst.Value.Concrete.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | External of
        { symbol : string
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Tuple_get of
        { tuple : t
        ; index : int
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
  [@@deriving sexp]

  val ty : t -> Ty.t
  val with_ty : t -> Ty.t -> t
  val loc : t -> Lex.Location.t
  val free_keys : t -> Access.t Ident.Map.t
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
        ; fvs : Ty.t Ident.Map.t
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
