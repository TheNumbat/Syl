open! Core

type 'a t =
  | Known of 'a
  | Unknown
[@@deriving sexp, compare, equal, hash]

include Monad.S with type 'a t := 'a t

val is_true : bool t -> bool
val is_false : bool t -> bool
val is_unknown : _ t -> bool
