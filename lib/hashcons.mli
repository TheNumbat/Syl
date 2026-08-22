open! Core

type 'a t = private
  { node : 'a
  ; hash : int
  ; tag : int
  }
[@@deriving sexp_of]

val tag : 'a t -> int
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
