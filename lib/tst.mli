open! Core

module Step : sig
  type t =
    | Index of int
    | Payload of Ident.Label.t
  [@@deriving sexp]
end

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
    | Tuple of Value.t Nonempty_list.t
    | Variant of Value.t option Ident.Label.Map.t
  [@@deriving sexp]

  val arg : Value.t -> Value.t
  val arg_mode : Value.t -> Modes.t
  val ret : Value.t -> Value.t
  val ret_mode : Value.t -> Modes.t
  val of_literal : Dst.Literal.t -> t

  val unify_constructors
    :  f:(Value.t -> Value.t -> Value.t option)
    -> Value.t option Ident.Label.Map.t
    -> Value.t option Ident.Label.Map.t
    -> t option
end

and Dependent : sig
  type t = private
    | T of
        { ty : Value.t
        ; memo : (Value.Concrete.t, Value.t) Hashtbl.t
        }
    | Meet of t * t
    | Join of t * t
    | Reduce of
        { env : Env.t
        ; arg : Ident.t
        ; arg_ty : Value.t
        ; arg_mode : Modes.t
        ; memo : (Value.Concrete.t, Value.t) Hashtbl.t
        ; ret_ty : Dst.Expr.t
        }
    | Typecheck of
        { env : Env.t
        ; arg : Ident.t
        ; arg_ty : Value.t
        ; arg_mode : Modes.t
        ; memo : (Value.Concrete.t, Value.t) Hashtbl.t
        ; body : Dst.Expr.t
        }
  [@@deriving sexp]

  val mono : Value.t -> t
  val meet : t -> t -> t
  val join : t -> t -> t

  val reduce
    :  Value.t
    -> env:Env.t
    -> arg:Ident.t
    -> arg_ty:Value.t
    -> arg_mode:Modes.t
    -> ret_ty:Dst.Expr.t
    -> t

  val typecheck
    :  Value.t
    -> env:Env.t
    -> arg:Ident.t
    -> arg_ty:Value.t
    -> arg_mode:Modes.t
    -> body:Dst.Expr.t
    -> t
end

and Bool : sig
  type t = private
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

  val const : bool -> Value.t
  val and_ : Value.t -> Value.t -> Value.t
  val or_ : Value.t -> Value.t -> Value.t
  val eq : Value.t -> Value.t -> Value.t
  val neq : Value.t -> Value.t -> Value.t
  val lt : Value.t -> Value.t -> Value.t
  val lte : Value.t -> Value.t -> Value.t
  val gt : Value.t -> Value.t -> Value.t
  val gte : Value.t -> Value.t -> Value.t
  val not_ : Value.t -> Value.t
end

and Int : sig
  type t = private
    | T of int64
    | Add of Value.t * Value.t
    | Sub of Value.t * Value.t
    | Mul of Value.t * Value.t
    | Div of Value.t * Value.t
    | Mod of Value.t * Value.t
    | Neg of Value.t
  [@@deriving sexp]

  exception Divide_by_zero of t
  exception Negative_modulus of t

  val const : int64 -> Value.t
  val add : Value.t -> Value.t -> Value.t
  val sub : Value.t -> Value.t -> Value.t
  val mul : Value.t -> Value.t -> Value.t
  val div : Value.t -> Value.t -> Value.t
  val mod_ : Value.t -> Value.t -> Value.t
  val neg : Value.t -> Value.t
end

and Closure : sig
  type t =
    { arg : Ident.t
    ; ty : Value.t
    ; body : Expr.t
    ; body_dst : Dst.Expr.t
    ; env : Env.t
    ; family : int
    ; hash : int
    }
  [@@deriving sexp, compare, hash]
end

and Binder : sig
  type t =
    { arg : Ident.t
    ; ty : Value.t
    ; body_dst : Dst.Expr.t
    ; env : Env.t
    ; family : int
    ; hash : int
    }
  [@@deriving sexp, compare, hash]
end

and Value : sig
  module Concrete : sig
    type t =
      | Unit
      | Bool of bool
      | Int of int64
      | Tuple of t Nonempty_list.t
      | Closure of int
      | Prim of Builtin0.t
      | External of string
      | Arrow of
          { arg : t
          ; arg_mode : Modes.t
          ; ret : t
          ; ret_mode : Modes.t
          }
      | Tuple_t of t Nonempty_list.t
      | Variant_t of t option Ident.Label.Map.t
      | Inject of
          { label : Ident.Label.t
          ; ty : t
          }
      | Constructor of
          { label : Ident.Label.t
          ; payload : t option
          }
    [@@deriving sexp, compare, hash]

    include Comparable.S with type t := t
    include Hashable.S with type t := t
  end

  type t = private
    | Bottom
    | Unit
    | Bool of Bool.t
    | Int of Int.t
    | Type of Ty.t
    | Closure of Closure.t
    | Binder of Binder.t
    | Var of Ident.t
    | Tuple of t Nonempty_list.t
    | Inject of
        { label : Ident.Label.t
        ; ty : t
        }
    | Constructor of
        { label : Ident.Label.t
        ; payload : t option
        }
    | Apply of
        { fn : t
        ; arg : t
        }
    | Proj of
        { tuple : t
        ; index : int
        }
    | Payload of
        { variant : t
        ; label : Ident.Label.t
        }
    | Match of
        { scrutinee : t
        ; arms : (Dst.Expr.pattern * t) Nonempty_list.t
        }
    | External of
        { symbol : string
        ; ty : t
        }
    | Prim of Builtin0.Prim.t
  [@@deriving sexp]

  val ty : t -> Ty.t
  val bottom : t
  val unit : t
  val type_ : Ty.t -> t
  val closure : Closure.t -> t
  val binder : Binder.t -> t
  val var : Ident.t -> t
  val prim : Builtin0.Prim.t -> t
  val external_ : symbol:string -> ty:t -> t
  val of_literal : Dst.Literal.t -> t
  val tuple : t Nonempty_list.t -> t
  val proj : t -> int -> t
  val payload : t -> label:Ident.Label.t -> t
  val inject : ty:t -> label:Ident.Label.t -> t
  val constructor : label:Ident.Label.t -> payload:t option -> t
  val apply : fn:t -> arg:t -> t
  val match_ : scrutinee:t -> arms:(Dst.Expr.pattern * t) Nonempty_list.t -> t
  val if_ : loc:Lex.Location.t -> cond:t -> then_:t -> else_:t -> t

  (* Patterns must agree structurally, except the last arm, which
     exhaustiveness makes unconditional. *)
  val arms_unify
    :  (Dst.Expr.pattern * t) Nonempty_list.t
    -> (Dst.Expr.pattern * t) Nonempty_list.t
    -> bool

  (* Combine two arm lists positionally, keeping the first list's patterns.
     [None] if the arms don't unify (per [arms_unify]) or [f] fails on a pair
     of leaves. *)
  val merge_arms
    :  (Dst.Expr.pattern * t) Nonempty_list.t
    -> (Dst.Expr.pattern * t) Nonempty_list.t
    -> f:(t -> t -> t option)
    -> (Dst.Expr.pattern * t) Nonempty_list.t option

  module Matched : sig
    type t =
      | Match of (Ident.t * Step.t list) list
      | No_match
      | Unknown
  end

  val matches_pattern : t -> Dst.Expr.pattern -> Matched.t
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
        ; family : int
        ; loc : Lex.Location.t
        }
    | Binder of
        { var : Ident.t
        ; arg : Ident.t
        ; body : t Value.Concrete.Map.t
        ; ty : Value.t
        ; mode : Modes.t
        ; family : int
        ; loc : Lex.Location.t
        }
  [@@deriving sexp]

  and case =
    { bindings : Value.t Ident.Map.t
    ; body : t
    }
  [@@deriving sexp]

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
  [@@deriving sexp]

  and target =
    | Family of int
    | Prim of Builtin0.Prim.t
  [@@deriving sexp]

  and t =
    | Erased of
        { ty : Value.t
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
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
        ; family : int
        ; loc : Lex.Location.t
        }
    | Binder of
        { arg : Ident.t
        ; body : t Value.Concrete.Map.t
        ; ty : Value.t
        ; mode : Modes.t
        ; family : int
        ; loc : Lex.Location.t
        }
    | Apply of
        { fn : t
        ; arg : t
        ; ty : Value.t
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | Specialize of
        { fn : t
        ; arg : t
        ; target : target
        ; key : Value.Concrete.t option
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
    | Tuple of
        { elts : t Nonempty_list.t
        ; ty : Value.t
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | Tuple_get of
        { tuple : t
        ; index : int
        ; ty : Value.t
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | Payload_get of
        { variant : t
        ; label : Ident.Label.t
        ; ty : Value.t
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | Tag_test of
        { variant : t
        ; label : Ident.Label.t
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
    | Match of
        { cases : case Nonempty_list.t
        ; tree : tree
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
    | Builtin of
        { builtin : Builtin0.t
        ; ty : Value.t
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | Extcall of
        { symbol : string
        ; arg : t
        ; ty : Value.t
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
  [@@deriving sexp]

  (* Combinators *)

  val literal : loc:Lex.Location.t -> Dst.Literal.t -> t
  val rebind : t -> stamp:int -> f:(t -> t) -> t
  val tuple : loc:Lex.Location.t -> (t * Desc.t) Nonempty_list.t -> t * Desc.t

  (* Project a runtime component out of the scrutinee, tracking its type and
     static value. *)
  val project : loc:Lex.Location.t -> t -> Desc.t -> Step.t list -> t * Desc.t

  (* Util *)

  (* [monos] overrides what a [Binder] node binds: pre-reify nodes carry
     empty bodies, so supply the family's monos from the store. *)
  val free_vars : ?monos:(int -> t Value.Concrete.Map.t) -> t -> Ident.Set.t
  val ty : t -> Value.t
  val mode : t -> Modes.t
  val loc : t -> Lex.Location.t
  val desc : t -> Value.t Lazy.t -> Desc.t
  val with_ : t -> ty:Value.t -> mode:Modes.t -> t
  val with_ty : t -> Value.t -> t
  val with_mode : t -> Modes.t -> t
end

module Top_level : sig
  type t =
    | Erased of { loc : Lex.Location.t }
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
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | Builtin of
        { var : Ident.t
        ; builtin : Builtin0.t
        ; ty : Value.t
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
  [@@deriving sexp]
end

module Program : sig
  (* Fully reified: [Specialize] targets are resolved and binder bodies carry
     their monomorphizations. *)
  type t =
    { top_levels : Top_level.t list
    ; stamp : int
    }
  [@@deriving sexp]
end
