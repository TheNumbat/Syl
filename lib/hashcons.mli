open! Core

module Tag : sig
  type t [@@deriving sexp_of, compare, equal, hash]

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t

  val to_int : t -> int
end

type 'a t = private
  { node : 'a
  ; hash : int
  ; tag : Tag.t
  }
[@@deriving sexp_of]

val tag : 'a t -> Tag.t
val equal : 'a t -> 'a t -> bool
val hash : 'a t -> int
val hash_fold_t : 'a t Hash.folder
val compare : 'a t -> 'a t -> int

type 'a node := 'a t

module Table (T : sig
    type t [@@deriving sexp_of, hash, equal]
  end) : sig
  type t

  val create : unit -> t
  val intern : t -> T.t -> T.t node
end
