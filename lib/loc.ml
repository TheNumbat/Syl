open! Core

type 'a t =
  { loc : Lex.Location.t
  ; reason : 'a
  }
[@@deriving sexp]
