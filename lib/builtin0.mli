open! Core

type t =
  | UnitT
  | BoolT
  | IntT
  | TypeT
  | Assert
[@@deriving sexp, compare, equal, hash]

val find : string -> t option

include Hashable.S with type t := t
include Comparable.S with type t := t
