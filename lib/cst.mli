open! Core
open Modes

module Literal : sig
  type t =
    | Unit
    | Bool of bool
    | Int of int64
  [@@deriving sexp]

  val print : unit -> t -> string
end

module Expr : sig
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
        ; static : Staticity.t
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
    | Arrow of
        { arg : t
        ; arg_id : Ident.t Option.t
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
    | Unreachable of
        { ty : t
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

  val free_vars : t -> Ident.Set.t
  val loc : t -> Lex.Location.t
  val print : unit -> t -> string
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
  val print : unit -> t -> string
end

module Program : sig
  type t = Top_level.t list [@@deriving sexp]

  (* Round trips. *)
  val print : unit -> t -> string
end
