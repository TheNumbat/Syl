open! Core
module Key := Core.Int

module Path : sig
  module Entry : sig
    type t =
      | Id of Ident.t
      | Key of Key.t
    [@@deriving sexp_of, compare, equal, hash]
  end

  type t = Entry.t list [@@deriving sexp_of, compare, equal, hash]

  val empty : t
  val id : Ident.t -> t
  val with_ : ?id:Ident.t -> ?key:Key.t -> t -> t
  val with_id : t -> Ident.t -> t
  val with_key : t -> Key.t -> t

  include Hashable.S_plain with type t := t
  include Comparable.S_plain with type t := t
end

module Ty : sig
  type t =
    | Unit
    | Bool
    | Int
    | Type
    | Tuple of t Nonempty_list.t
    | Variant of t option Ident.Label.Map.t
    | Ref
    | Env
    | Closure of
        { arg_ty : t
        ; ret_ty : t
        }
  [@@deriving sexp_of]

  (* TODO these will change once we switch to an llvm backend. *)

  (* [size_in_mem] is C++ [sizeof] of the printed type; zero-size types are
     printed as [void] and never materialized. *)
  val size_in_mem : t -> int
  val align_in_mem : t -> int
  val payload_size_in_mem : t option Ident.Label.Map.t -> int
  val payload_align_in_mem : t option Ident.Label.Map.t -> int
  val align_to : int -> align:int -> int
  val is_zero_size : t -> bool
end

module Env : sig
  type entry =
    { path : Path.t
    ; ty : Ty.t
    ; offset : int
    }
  [@@deriving sexp_of]

  type t =
    { entries : entry array
    ; length : int
    }
  [@@deriving sexp_of]

  val empty : t
end

module Expr : sig
  type nonrec t =
    | Scalar of
        { value : Sst.Scalar.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Make_env of
        { env : Env.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Make_env_rec of
        { length : int
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Fill_env_rec of
        { path : Path.t
        ; env : Env.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Make_closure of
        { body : Path.t
        ; env : Path.t option
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Make_tuple of
        { elts : (Path.t * Ty.t) array
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Make_variant of
        { label : Ident.Label.t
        ; payload : (Path.t * Ty.t) option
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Make_ref of
        { payload : Path.t
        ; payload_ty : Ty.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Ref_get of
        { ref : Path.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Apply_closure of
        { fn : Path.t
        ; arg : Path.t
        ; arg_ty : Ty.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Apply_thunk of
        { fn : Path.t
        ; env : Path.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Apply_proc of
        { fn : Path.t
        ; arg : Path.t
        ; arg_ty : Ty.t
        ; env : Path.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Extcall of
        { symbol : string
        ; arg : Path.t
        ; arg_ty : Ty.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Ident of
        { path : Path.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Tuple_get of
        { tuple : Path.t
        ; index : int
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Payload_get of
        { variant : Path.t
        ; label : Ident.Label.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Tag_test of
        { variant : Path.t
        ; variant_ty : Ty.t
        ; label : Ident.Label.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
  [@@deriving sexp_of]

  val ty : t -> Ty.t
  val loc : t -> Lex.Location.t
  val with_ty : t -> Ty.t -> t
end

module Block : sig
  type case =
    { bindings : (Path.t * Ty.t) array
    ; block : t
    }
  [@@deriving sexp_of]

  and tree =
    | Leaf of
        { case : int
        ; bindings : (Path.t * t) array
        }
    | Split of
        { cond : t
        ; then_ : tree
        ; else_ : tree
        }
  [@@deriving sexp_of]

  and t =
    | Block of
        { bindings : (Path.t * t) array
        ; return : Expr.t
        }
    | If of
        { cond : Expr.t
        ; then_ : t
        ; else_ : t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Match of
        { tree : tree
        ; cases : case array
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
  [@@deriving sexp_of]

  val ty : t -> Ty.t
  val loc : t -> Lex.Location.t
end

module Proc : sig
  type t =
    | Closure of
        { path : Path.t
        ; arg : Path.t
        ; arg_ty : Ty.t
        ; env : Env.t
        ; body : Block.t
        ; loc : Lex.Location.t
        }
    | Thunk of
        { path : Path.t
        ; env : Env.t
        ; body : Block.t
        ; loc : Lex.Location.t
        }
    | External of
        { path : Path.t
        ; symbol : string
        ; arg_ty : Ty.t
        ; ret_ty : Ty.t
        ; loc : Lex.Location.t
        }
  [@@deriving sexp_of]
end

module Program : sig
  type t =
    { procs : Proc.t array
    ; bindings : (Path.t * Block.t) array
    }
  [@@deriving sexp_of]
end
