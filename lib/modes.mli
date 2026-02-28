open! Core

type t =
  | Erasure
  | Staticity
[@@deriving sexp, compare, equal]

module Erasure : sig
  type t =
    | Erased
    | Unerased
  [@@deriving sexp, compare, equal, hash]

  val top : t
  val bottom : t
  val default : t
  val meet : t -> t -> t
  val join : t -> t -> t
  val leq : t -> t -> bool
  val geq : t -> t -> bool
  val print : t -> string
end

module Staticity : sig
  type t =
    | Dynamic
    | Static
  [@@deriving sexp, compare, equal, hash]

  val top : t
  val bottom : t
  val default : t
  val meet : t -> t -> t
  val join : t -> t -> t
  val leq : t -> t -> bool
  val geq : t -> t -> bool
  val print : t -> string
end

module Modes : sig
  module Maybe : sig
    type t =
      { staticity : Staticity.t option
      ; erasure : Erasure.t option
      }
    [@@deriving sexp, compare, equal, hash]

    val none : t
    val is_none : t -> bool
    val print : unit -> t -> string
  end

  type t =
    { staticity : Staticity.t
    ; erasure : Erasure.t
    }
  [@@deriving sexp, compare, equal, hash]

  val create : staticity:Staticity.t -> erasure:Erasure.t -> t
  val top : ?staticity:Staticity.t -> ?erasure:Erasure.t -> unit -> t
  val bottom : ?staticity:Staticity.t -> ?erasure:Erasure.t -> unit -> t
  val default : ?staticity:Staticity.t -> ?erasure:Erasure.t -> unit -> t
  val annotate : t -> Maybe.t -> t

  (* Transforms *)
  val return : t -> ret:t -> t
  val cond : cond:t -> t -> t -> t

  (* Logic *)
  val join : t -> t -> t
  val meet : t -> t -> t
  val leq : t -> t -> bool
  val geq : t -> t -> bool
  val is_erased : t -> bool
  val is_static : t -> bool
  val print : unit -> t -> string
end
