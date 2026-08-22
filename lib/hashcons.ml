open! Core

type 'a t =
  { node : 'a
  ; hash : int
  ; tag : int
  }

let sexp_of_t sexp_of_a t = sexp_of_a t.node
let equal = phys_equal
let hash t = t.hash
let hash_fold_t s t = hash_fold_int s t.hash
let tag t = t.tag
let compare l r = Int.compare l.tag r.tag
let counter = ref 0

module Table (T : sig
    type t [@@deriving sexp_of, hash, equal]
  end) =
struct
  module WeakSet = Weak.Make (struct
      type nonrec t = T.t t

      let equal l r = T.equal l.node r.node
      let hash { hash; _ } = hash
    end)

  type t = WeakSet.t

  let create () = WeakSet.create 4096

  let intern t node =
    Int.incr counter;
    WeakSet.merge t { node; hash = T.hash node; tag = !counter }
  ;;
end
