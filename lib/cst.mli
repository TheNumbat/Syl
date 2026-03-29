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

module With_loc : sig
  type 'a t =
    { node : 'a
    ; loc : Lex.Location.t
    ; before : Lex.Comment.t list
    ; after : Lex.Comment.t list
    }
  [@@deriving sexp]

  val create
    :  ?before:Lex.Comment.t list
    -> ?after:Lex.Comment.t list
    -> loc:Lex.Location.t
    -> 'a
    -> 'a t
end

module Expr : sig
  type arg =
    { var : Ident.Raw.t
    ; mode : Modes.Maybe.t
    ; ty : t
    ; loc : Lex.Location.t
    }

  and fun_ =
    { var : Ident.Raw.t
    ; erased : Erasure.t
    ; args : arg Nonempty_list.t
    ; ret_mode : Modes.Maybe.t
    ; ret_ty : t
    ; body : t
    ; loc : Lex.Location.t
    }

  and node =
    | If of
        { cond : t
        ; then_ : t
        ; else_ : t
        ; static : Staticity.t
        }
    | Let of
        { var : Ident.Raw.t
        ; erased : Erasure.t
        ; args : arg list
        ; bind : t
        ; rest : t
        }
    | Fun of
        { funs : fun_ Nonempty_list.t
        ; rest : t
        }
    | Lambda of
        { erased : Erasure.t
        ; args : arg Nonempty_list.t
        ; body : t
        }
    | Apply of
        { fn : t
        ; arg : t
        }
    | Paren of { expr : t }
    | Var of { id : Ident.Raw.t }
    | Literal of { value : Literal.t }
    | Unop of
        { op : Ident.Unop.t
        ; arg : t
        }
    | Binop of
        { op : Ident.Binop.t
        ; lhs : t
        ; rhs : t
        }
    | Nop of
        { op : Ident.Nop.t
        ; elts : t list
        }
    | Arrow of
        { arg : t
        ; arg_id : Ident.Raw.t Option.t
        ; arg_mode : Modes.Maybe.t
        ; ret : t
        ; ret_mode : Modes.Maybe.t
        }
    | Assert of
        { cond : t
        ; static : Staticity.t
        }
    | Unreachable
    | Type_annotation of
        { expr : t
        ; ty : t
        }
    | Mode_annotation of
        { expr : t
        ; mode : Modes.Maybe.t
        }

  and t = node With_loc.t [@@deriving sexp]

  val strip : t -> t
  val loc : t -> Lex.Location.t
end

module Top_level : sig
  type node =
    | Let of
        { var : Ident.Raw.t
        ; erased : Erasure.t
        ; args : Expr.arg list
        ; bind : Expr.t
        }
    | Fun of { funs : Expr.fun_ Nonempty_list.t }
    | External of
        { var : Ident.Raw.t
        ; ty : Expr.t
        ; symbol : string
        }
    | Builtin of
        { var : Ident.Raw.t
        ; name : string
        }
  [@@deriving sexp]

  type t = node With_loc.t [@@deriving sexp]

  val strip : t -> t
  val loc : t -> Lex.Location.t
end

module Program : sig
  type t =
    { items : Top_level.t list
    ; after : Lex.Comment.t list
    }
  [@@deriving sexp]

  val strip : t -> t
end
