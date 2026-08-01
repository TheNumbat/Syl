open! Core

type 'a t =
  | Known of 'a
  | Unknown
[@@deriving sexp, compare, equal, hash]
