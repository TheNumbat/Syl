open! Core
module Key := Hashcons.Tag

module Ty : sig
  type t =
    | Unit
    | Bool
    | Int
    | Type
    | Tuple of t Nonempty_list.t
    | Arrow of
        { arg_ty : t
        ; ret_ty : t
        }
    | Pi
    | Variant of t option Ident.Label.Map.t
    | Ref
  [@@deriving sexp_of]
end

module Scalar : sig
  type t =
    | Unit
    | Type
    | Bool of bool
    | Int of int64
  [@@deriving sexp_of]
end

module Expr : sig
  type target =
    | Family of Ids.Family.t
    | Prim of Builtin0.Prim.t
  [@@deriving sexp_of]

  type fun_ =
    | Lambda of
        { var : Ident.t
        ; arg : Ident.t
        ; body : t
        ; ty : Ty.t
        ; family : Ids.Family.t
        ; loc : Lex.Location.t
        }
    | Binder of
        { var : Ident.t
        ; arg : Ident.t
        ; body : t Key.Map.t
        ; ty : Ty.t
        ; family : Ids.Family.t
        ; loc : Lex.Location.t
        }
  [@@deriving sexp_of]

  and case =
    { body : t
    ; bindings : Ty.t Ident.Map.t
    }
  [@@deriving sexp_of]

  and tree =
    | Leaf of
        { case : int
        ; bindings : t Ident.Map.t
        }
    | Split of
        { cond : t
        ; then_ : tree
        ; else_ : tree
        }
  [@@deriving sexp_of]

  and t =
    | Scalar of
        { value : Scalar.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Var of
        { id : Ident.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Fun of
        { funs : fun_ Nonempty_list.t
        ; rest : t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Lambda of
        { arg : Ident.t
        ; body : t
        ; ty : Ty.t
        ; family : Ids.Family.t
        ; loc : Lex.Location.t
        }
    | Binder of
        { arg : Ident.t
        ; body : t Key.Map.t
        ; ty : Ty.t
        ; family : Ids.Family.t
        ; loc : Lex.Location.t
        }
    | Apply of
        { fn : t
        ; arg : t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Specialize of
        { fn : t
        ; arg : t
        ; target : target
        ; key : Key.t option
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
    | Tuple_get of
        { tuple : t
        ; index : int
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Constructor of
        { label : Ident.Label.t
        ; payload : t option
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Inject of
        { label : Ident.Label.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Payload_get of
        { variant : t
        ; label : Ident.Label.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Tag_test of
        { variant : t
        ; label : Ident.Label.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Make_ref of
        { payload : t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Ref_get of
        { ref : t
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
    | Match of
        { cases : case Nonempty_list.t
        ; tree : tree
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | External of
        { symbol : string
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Extcall of
        { symbol : string
        ; arg : t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
  [@@deriving sexp_of]

  val ty : t -> Ty.t
  val with_ty : t -> Ty.t -> t
  val loc : t -> Lex.Location.t
  val free_vars : t -> Ident.Set.t
  val free_vars_funs : fun_ Nonempty_list.t -> Ident.Set.t
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
  [@@deriving sexp_of]
end

module Program : sig
  type t = { top_levels : Top_level.t list } [@@deriving sexp_of]
end
