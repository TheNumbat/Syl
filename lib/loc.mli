open! Core

type 'a t =
  { loc : Lex.Location.t
  ; here : Source_code_position.t
  ; reason : 'a
  }
[@@deriving sexp]
