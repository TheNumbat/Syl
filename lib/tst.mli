open! Core
open Modes

module rec Ty : sig
  type t =
    | Unit
    | Bool
    | Int
    | Type
    | Arrow of
        { arg_ty : Value.t
        ; arg_mode : Modes.t
        ; ret_ty : Value.t
        ; ret_mode : Modes.t
        }
    | Pi of
        { arg_ty : Value.t
        ; arg_mode : Modes.t
        ; ret_ty : Dependent.t
        ; ret_mode : Modes.t
        }
  [@@deriving sexp]

  val of_literal : Cst.Literal.t -> t
end

and Dependent : sig
  type t =
    | T of Value.t
    | Meet of t * t
    | Join of t * t
    | Reduce of
        { env : Env.t
        ; arg : Ident.t
        ; arg_ty : Value.t
        ; arg_mode : Modes.t
        ; memo : (Value.Concrete.t, Value.t) Hashtbl.t
        ; ret_ty : Cst.Expr.t
        }
    | Typecheck of
        { env : Env.t
        ; arg : Ident.t
        ; arg_ty : Value.t
        ; arg_mode : Modes.t
        ; memo : (Value.Concrete.t, Value.t) Hashtbl.t
        ; body : Cst.Expr.t
        }
  [@@deriving sexp]

  (* Smart constructors fold concrete types *)
  val meet : t -> t -> t
  val join : t -> t -> t

  val reduce
    :  Value.t
    -> env:Env.t
    -> arg:Ident.t
    -> arg_ty:Value.t
    -> arg_mode:Modes.t
    -> ret_ty:Cst.Expr.t
    -> t

  val typecheck
    :  Value.t
    -> env:Env.t
    -> arg:Ident.t
    -> arg_ty:Value.t
    -> arg_mode:Modes.t
    -> body:Cst.Expr.t
    -> t
end

and Bool : sig
  type t =
    | T of bool
    | And of Value.t * Value.t
    | Or of Value.t * Value.t
    | Eq of Value.t * Value.t
    | Neq of Value.t * Value.t
    | Lt of Value.t * Value.t
    | Lte of Value.t * Value.t
    | Gt of Value.t * Value.t
    | Gte of Value.t * Value.t
    | Not of Value.t
  [@@deriving sexp]

  val reduce : t -> Value.t
end

and Int : sig
  type t =
    | T of int64
    | Add of Value.t * Value.t
    | Sub of Value.t * Value.t
    | Mul of Value.t * Value.t
    | Div of Value.t * Value.t
    | Mod of Value.t * Value.t
    | Neg of Value.t
  [@@deriving sexp]

  val reduce : t -> Value.t
end

and Closure : sig
  type t =
    { arg : Ident.t
    ; ty : Value.t
    ; body : Expr.t
    ; env : Env.t
    }
  [@@deriving sexp]
end

and Binder : sig
  module Mono : sig
    type t =
      { arg : Ident.t
      ; arg_mode : Modes.t
      ; arg_desc : Desc.t
      ; body : Expr.t
      ; body_desc : Desc.t
      }
    [@@deriving sexp]
  end

  type t =
    { arg : Ident.t
    ; ty : Value.t
    ; body : Cst.Expr.t
    ; mono : (Value.Concrete.t, Mono.t) Hashtbl.t
    ; env : Env.t
    }
  [@@deriving sexp]
end

and Value : sig
  module Concrete : sig
    type t =
      | Unit
      | Bool of bool
      | Int of int64
      | Closure of int
      | UnitT
      | BoolT
      | IntT
      | TypeT
      | ArrowT of
          { arg : t
          ; arg_mode : Modes.t
          ; ret : t
          ; ret_mode : Modes.t
          }
    [@@deriving sexp, hash, compare, equal]

    include Comparable.S with type t := t
    include Hashable.S with type t := t
  end

  type t =
    | Unit
    | Bool of Bool.t
    | Int of Int.t
    | Type of Ty.t
    | Closure of Closure.t
    | Binder of Binder.t
    | Var of Ident.t
    | If of
        { cond : t
        ; then_ : t
        ; else_ : t
        }
    | Apply of
        { fn : t
        ; arg : t
        }
    | External of
        { symbol : string
        ; ty : t
        }
  [@@deriving sexp]

  val of_literal : Cst.Literal.t -> t
  val is_true : t -> bool
end

and Desc : sig
  type t =
    { ty : Value.t
    ; mode : Modes.t
    ; static : Value.t Lazy.t
    }
  [@@deriving sexp]

  val of_type : Ty.t -> t
end

and Env : sig
  type t = Desc.t Ident.Map.t [@@deriving sexp]

  val initial : t
  val bind : t -> Ident.t -> Desc.t -> t
  val find : t -> Ident.t -> Desc.t Option.t
  val find_exn : t -> Ident.t -> Desc.t
end

and Expr : sig
  type fun_ =
    | Lambda of
        { var : Ident.t
        ; arg : Ident.t
        ; body : t
        ; ty : Value.t
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | Binder of
        { var : Ident.t
        ; arg : Ident.t
        ; body : Cst.Expr.t
        ; mono : (Value.Concrete.t, Binder.Mono.t) Hashtbl.t
        ; ty : Value.t
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
  [@@deriving sexp]

  and t =
    | Literal of
        { value : Value.t
        ; ty : Value.t
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | Fun of
        { funs : fun_ Nonempty_list.t
        ; rest : t
        ; ty : Value.t
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | Lambda of
        { arg : Ident.t
        ; body : t
        ; ty : Value.t
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | Binder of
        { arg : Ident.t
        ; body : Cst.Expr.t
        ; mono : (Value.Concrete.t, Binder.Mono.t) Hashtbl.t
        ; ty : Value.t
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | Apply of
        { fn : t
        ; arg : t
        ; ty : Value.t
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | Let of
        { var : Ident.t
        ; bind : t
        ; rest : t
        ; ty : Value.t
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | Unop of
        { op : Ident.Unop.t
        ; arg : t
        ; ty : Value.t
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | Binop of
        { op : Ident.Binop.t
        ; lhs : t
        ; rhs : t
        ; ty : Value.t
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | If of
        { cond : t
        ; then_ : t
        ; else_ : t
        ; ty : Value.t
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | Var of
        { id : Ident.t
        ; ty : Value.t
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | Symbol of
        { fn : t
        ; key : Value.Concrete.t
        ; ty : Value.t
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | Erased of
        { ty : Value.t
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
  [@@deriving sexp]

  val free_vars : t -> Ident.Set.t
  val ty : t -> Value.t
  val mode : t -> Modes.t
  val desc : t -> Value.t Lazy.t -> Desc.t
  val with_ : t -> ty:Value.t -> mode:Modes.t -> t
  val with_ty : t -> Value.t -> t
  val with_mode : t -> Modes.t -> t
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
        ; ty : Value.t
        ; loc : Lex.Location.t
        }
    | Builtin of
        { var : Ident.t
        ; builtin : Builtin0.t
        ; ty : Value.t
        ; loc : Lex.Location.t
        }
  [@@deriving sexp]
end

module Program : sig
  type t = Top_level.t list [@@deriving sexp]
end
