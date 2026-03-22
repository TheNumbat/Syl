open! Core

module Unop : sig
  type t =
    | Not
    | Neg
  [@@deriving sexp, compare, equal, hash]

  val print : unit -> t -> string
end

module Binop : sig
  type t =
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | And
    | Or
    | Eq
    | Neq
    | Lt
    | Lte
    | Gt
    | Gte
  [@@deriving sexp, compare, equal, hash]

  val print : unit -> t -> string
end

module Nop : sig
  type t =
    | Comma
    | Caret
  [@@deriving sexp, compare, equal, hash]

  val print : unit -> t -> string
  val sep : t -> string
end

module Raw : sig
  type t [@@deriving sexp, compare, equal, hash]

  val anon : t
  val id : string -> t
  val unop : Unop.t -> t
  val binop : Binop.t -> t
  val nop : Nop.t -> t
  val print : unit -> t -> string

  include Hashable.S with type t := t
  include Comparable.S with type t := t
end

type t [@@deriving sexp, compare, equal, hash]

val create : Raw.t -> stamp:int -> t
val is_anon : t -> bool
val print : unit -> t -> string

include Hashable.S with type t := t
include Comparable.S with type t := t
