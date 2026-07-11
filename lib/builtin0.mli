open! Core

module Type : sig
  type t =
    | Unit
    | Bool
    | Int
    | Type
  [@@deriving sexp, compare, equal, hash]
end

module Prim : sig
  module Int : sig
    type t =
      | Add
      | Sub
      | Mul
      | Div
      | Mod
      | Neg
      | Eq
      | Neq
      | Lt
      | Lte
      | Gt
      | Gte
    [@@deriving sexp, compare, equal, hash]
  end

  module Bool : sig
    type t =
      | And
      | Or
      | Not
    [@@deriving sexp, compare, equal, hash]
  end

  module Type : sig
    type t =
      | Is_unit
      | Is_bool
      | Is_int
      | Is_type
      | Is_tuple
      | Is_arrow
      | Is_pi
      | Tuple_get
      | Tuple_length
      | Arrow_arg
      | Arrow_ret
      | Pi_arg
    [@@deriving sexp, compare, equal, hash]
  end

  module Unerase : sig
    type t =
      | Unit
      | Bool
      | Int
    [@@deriving sexp, compare, equal, hash]
  end

  type t =
    | Assert
    | Assert_erased
    | Int of Int.t
    | Bool of Bool.t
    | Type of Type.t
    | Unerase of Unerase.t
  [@@deriving sexp, compare, equal, hash]

  val symbol : t -> string
end

type t =
  | Type of Type.t
  | Prim of Prim.t
[@@deriving sexp, compare, equal, hash]

val find : string -> t option

include Hashable.S with type t := t
include Comparable.S with type t := t
