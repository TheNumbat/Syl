open! Core

type t [@@deriving sexp_of]

val nil : t
val text : string -> t
val ( ^^ ) : t -> t -> t
val nest : int -> t -> t
val align : t -> t
val line : t
val softline : t
val hardline : t
val group : t -> t
val if_flat : flat:t -> broken:t -> t
val space : t
val concat : sep:t -> f:('a -> t) -> 'a list -> t
val pretty : width:int -> t -> string
