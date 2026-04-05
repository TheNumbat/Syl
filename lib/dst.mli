open! Core
module Literal = Cst.Literal

module Expr : sig
  type fun_ =
    { var : Ident.t
    ; arg : Ident.t
    ; erased : Modes.Erasure.t
    ; arg_mode : Modes.Maybe.t
    ; arg_ty : t
    ; ret_mode : Modes.Maybe.t
    ; ret_ty : t
    ; body : t
    ; loc : Lex.Location.t
    }

  and pattern =
    | Var of
        { id : Ident.t
        ; loc : Lex.Location.t
        }

  and t =
    | If of
        { cond : t
        ; then_ : t
        ; else_ : t
        ; static : Modes.Staticity.t
        ; loc : Lex.Location.t
        }
    | Match of
        { cond : t
        ; arms : (pattern * t) Nonempty_list.t
        ; static : Modes.Staticity.t
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
    | Var of
        { id : Ident.t
        ; loc : Lex.Location.t
        }
    | Literal of
        { value : Literal.t
        ; loc : Lex.Location.t
        }
    | Arrow of
        { arg : t
        ; arg_id : Ident.t
        ; arg_mode : Modes.Maybe.t
        ; ret : t
        ; ret_mode : Modes.Maybe.t
        ; loc : Lex.Location.t
        }
    | Tuple of
        { elts : t list
        ; loc : Lex.Location.t
        }
    | Make_tuple of
        { elts : t list
        ; loc : Lex.Location.t
        }
    | Builtin of
        { builtin : Builtin0.t
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

  val free_vars : t -> Ident.Set.t
  val free_vars_pattern : pattern -> Ident.Set.t
  val loc : t -> Lex.Location.t
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
        ; ty : Expr.t
        ; symbol : string
        ; loc : Lex.Location.t
        }
    | Builtin of
        { var : Ident.t
        ; name : string
        ; loc : Lex.Location.t
        }
  [@@deriving sexp]

  val loc : t -> Lex.Location.t
end

module Program : sig
  type t = Top_level.t list [@@deriving sexp]
end
