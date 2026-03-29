open! Core

module Error : sig
  type t =
    | Unexpected of Lex.Token.t
    | Duplicate_mode of Modes.Axis.t
    | Unexpected_modes of Modes.Maybe.t
    | Fun_underscore
    | Unterminated_comment
  [@@deriving sexp]
end

val parse : string -> (Cst.Program.t, Error.t Loc.t) Result.t
val parse_exn : string -> Cst.Program.t
