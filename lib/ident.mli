open! Core

type t [@@deriving sexp, equal]

val anon : t
val is_anon : t -> bool
val append : t -> t -> t
val print : unit -> t -> string

include Stringable.S with type t := t
include Hashable.S with type t := t
include Comparable.S with type t := t
