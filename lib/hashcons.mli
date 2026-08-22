open! Core

type 'a t = private
  { node : 'a
  ; hash : int
  }
[@@deriving sexp_of]

val equal : 'a t -> 'a t -> bool
val hash : 'a t -> int
val hash_fold_t : 'a t Hash.folder

type 'a node := 'a t

module Table (T : sig
    type t [@@deriving sexp_of, hash, equal]
  end) : sig
  type t

  val create : unit -> t
  val intern : t -> T.t -> T.t node
end
