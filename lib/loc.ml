open! Core

type 'a t =
  { loc : Lex.Location.t
  ; here : (Source_code_position.t[@sexp.opaque])
  ; reason : 'a
  }
[@@deriving sexp]
