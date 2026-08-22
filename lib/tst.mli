open! Core

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
    | Ref of Value.t
  [@@deriving sexp_of, hash, equal]

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
        ; memo : (Value.t, Value.t) Hashtbl.t
        }
    | Reduce of
        { env : Env.t
        ; arg : Ident.t
        ; arg_ty : Value.t
        ; arg_mode : Modes.t
        ; ret_ty : Dst.Expr.t
        ; memo : (Value.t, Value.t) Hashtbl.t
        ; uid : int
        }
    | Typecheck of
        { env : Env.t
        ; arg : Ident.t
        ; arg_ty : Value.t
        ; arg_mode : Modes.t
        ; body : Dst.Expr.t
        ; memo : (Value.t, Value.t) Hashtbl.t
        ; uid : int
        }
  [@@deriving sexp_of, hash, equal]

  val mono : Value.t -> t

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

  val is_concrete : Value.t -> bool
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
  [@@deriving sexp_of, hash, equal]

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
  [@@deriving sexp_of, hash, equal]

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
  type t = private
    { arg : Ident.t
    ; ty : Value.t
    ; body : Expr.t Lazy.t
    ; body_dst : Dst.Expr.t
    ; env : Env.t
    ; family : int
    ; uid : int
    }
  [@@deriving sexp_of, hash, equal]

  val const
    :  arg:Ident.t
    -> ty:Value.t
    -> body:Expr.t Lazy.t
    -> body_dst:Dst.Expr.t
    -> env:Env.t
    -> family:int
    -> t
end

and Binder : sig
  type t = private
    { arg : Ident.t
    ; ty : Value.t
    ; body_dst : Dst.Expr.t
    ; env : Env.t
    ; family : int
    ; uid : int
    }
  [@@deriving sexp_of, hash, equal]

  val const : arg:Ident.t -> ty:Value.t -> body_dst:Dst.Expr.t -> env:Env.t -> family:int -> t
end

and Value : sig
  type t = node Hashcons.t [@@deriving sexp_of, equal, hash]

  and node = private
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
        ; arms : (Pattern.Canon.t * t) Nonempty_list.t
        }
    | External of
        { symbol : string
        ; ty : t
        }
    | Box of t
    | Deref of t
    | Prim of Builtin0.Prim.t
  [@@deriving sexp_of, equal, hash]

  include Comparable.S_plain with type t := t

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
  val box : t -> t
  val deref : t -> t
  val inject : ty:t -> label:Ident.Label.t -> t
  val constructor : label:Ident.Label.t -> payload:t option -> t
  val apply : fn:t -> arg:t -> t
  val if_ : cond:t -> then_:t -> else_:t -> t
  val match_ : scrutinee:t -> arms:(Pattern.Canon.t * t) Nonempty_list.t -> t

  module Eliminator : sig
    (* A stuck eliminator awaiting its subject *)
    type nonrec t =
      | Apply of t
      | Proj of int
      | Payload of Ident.Label.t
      | Deref

    (* [peel] strips a value's eliminator spine, [unpeel] rebuilds it. *)
    val peel : Value.t -> t list -> Value.t * t list
    val unpeel : Value.t -> t list -> Value.t
  end

  val is_fn_type : t -> bool

  (* Assuming [t] is [Type ty], returns [ty]. *)
  val ty_exn : t -> Ty.t

  (* Rewrite occurrences of [target] to [replacement], renormalizing through
     the smart constructors. *)
  val rewrite : t -> target:t -> replacement:t -> t
end

and Desc : sig
  type t =
    { ty : Value.t
    ; mode : Modes.t
    ; static : Value.t Lazy.t
    }
  [@@deriving sexp_of]

  val of_type : Ty.t -> t
end

and Env : sig
  module Kind : sig
    type t =
      | Abstract
      | Speculative
      | Reducing
      | Instancing
    [@@deriving sexp_of, compare]
  end

  type t [@@deriving sexp_of]

  val initial : t
  val enter : t -> Kind.t -> t
  val abstract : t -> bool
  val reducing : t -> bool
  val instancing : t -> bool

  (* Semantically rewrite every existing binding's type and static value with
  [target -> replacement]. The rewrites are applied lazily at [find]. *)
  val learn : t -> target:Value.t -> replacement:Value.t -> t
  val bind : t -> Ident.t -> Desc.t -> t
  val find : t -> Ident.t -> Desc.t Option.t
  val find_exn : t -> Ident.t -> Desc.t
  val merge : t -> t -> t
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
        ; body : t Core.Int.Map.t
        ; ty : Value.t
        ; mode : Modes.t
        ; family : int
        ; loc : Lex.Location.t
        }
  [@@deriving sexp_of]

  and case =
    { bindings : Value.t Ident.Map.t
    ; body : t
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

  and target =
    | Family of int
    | Prim of Builtin0.Prim.t
  [@@deriving sexp_of]

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
        ; body : t Core.Int.Map.t
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
        ; key : Value.t option
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
    | Make_ref of
        { payload : t
        ; ty : Value.t
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | Ref_get of
        { ref : t
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
  [@@deriving sexp_of]

  (* Combinators *)

  val literal : loc:Lex.Location.t -> Dst.Literal.t -> t
  val rebind : t -> id:Ident.t -> f:(t -> t) -> t
  val tuple : loc:Lex.Location.t -> (t * Desc.t) Nonempty_list.t -> t * Desc.t

  (* Util *)

  (* [monos] overrides what a [Binder] node binds: pre-reify nodes carry
     empty bodies, so supply the family's monos from the store. *)
  val free_vars : ?monos:(int -> t Core.Int.Map.t) -> t -> Ident.Set.t
  val ty : t -> Value.t
  val mode : t -> Modes.t
  val loc : t -> Lex.Location.t
  val desc : t -> Value.t Lazy.t -> Desc.t
  val with_ : t -> ty:Value.t -> mode:Modes.t -> t
  val with_ty : t -> Value.t -> t
  val with_mode : t -> Modes.t -> t
end

and Pattern : sig
  module Canon : sig
    type t =
      | Wild
      | Literal of Dst.Literal.t
      | Tuple of t Nonempty_list.t
      | Constructor of
          { label : Ident.Label.t
          ; payload : t option
          }
      | Ref of t
      | Or of t * t
    [@@deriving sexp_of, equal, hash]

    val of_pattern : Dst.Expr.pattern -> t
  end

  module Step : sig
    type t =
      | Index of int
      | Payload of Ident.Label.t
      | Deref
    [@@deriving sexp_of]
  end

  module Matched : sig
    type t =
      | Match
      | No_match
      | Unknown
    [@@deriving sexp_of]
  end

  module World : sig
    type 'a t =
      { pattern : Dst.Expr.pattern
      ; positive : Dst.Expr.pattern
      ; speculative : bool
      ; body : 'a
      }
  end

  (* Specialize [scrutinee] assuming it matches the pattern. *)
  val specialize : Canon.t -> scrutinee:Value.t -> Value.t

  (* All worlds covered by the patterns. Finite-domain wildcards become multiple worlds. *)
  val worlds
    :  unfold:(Value.t -> Value.t)
    -> ty:Value.t
    -> scrutinee:Value.t
    -> (Dst.Expr.pattern * 'a) Nonempty_list.t
    -> 'a World.t Nonempty_list.t

  (* Whether a value definitely matches a canonical pattern. *)
  val matches : Value.t -> Canon.t -> Matched.t

  (* Whether a value definitely selects a particular pattern from an exhaustive list. *)
  val selects
    :  Value.t
    -> Dst.Expr.pattern Nonempty_list.t
    -> (int * (Ident.t * Step.t list) list) Or_unknown.t

  (* Patterns must agree structurally, except the last arm, which exhaustiveness
     makes unconditional. *)
  val arms_agree
    :  (Canon.t * Value.t) Nonempty_list.t
    -> (Canon.t * Value.t) Nonempty_list.t
    -> bool

  (* Combine two arm lists positionally, keeping the first list's patterns.
     [None] if the arms don't agree or [f] fails on a pair of leaves. *)
  val map2_arms
    :  (Canon.t * Value.t) Nonempty_list.t
    -> (Canon.t * Value.t) Nonempty_list.t
    -> f:(Value.t -> Value.t -> Value.t option)
    -> (Canon.t * Value.t) Nonempty_list.t option
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
  [@@deriving sexp_of]
end

module Program : sig
  (* Fully reified: [Specialize] targets are resolved and binder bodies carry
     their monomorphizations. *)
  type t =
    { top_levels : Top_level.t list
    ; stamp : int
    }
  [@@deriving sexp_of]
end
