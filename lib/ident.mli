open! Core

type t [@@deriving sexp, equal]

val anon : t
val is_anon : t -> bool
val print : unit -> t -> string

include Stringable.S with type t := t
include Hashable.S with type t := t
include Comparable.S with type t := t

module Path : sig
  type id := t
  type t [@@deriving sexp, hash, compare]

  val empty : t
  val append : t -> id -> t
  val fresh : t -> string -> t
  val join : t -> id
end
