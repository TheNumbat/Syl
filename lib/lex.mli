open! Core

module Op : sig
  type t =
    | Minus
    | Plus
    | Star
    | Slash
    | Backslash
    | Percent
    | And
    | Or
    | Not
    | Eq
    | Neq
    | Lt
    | Lte
    | Gt
    | Gte
    | Arrow
  [@@deriving sexp, equal]
end

module Kind : sig
  type 'a t =
    | Eof : unit t
    | If : unit t
    | Then : unit t
    | Else : unit t
    | Fn : unit t
    | Let : unit t
    | Fun : unit t
    | And : unit t
    | Static : unit t
    | Dynamic : unit t
    | Erased : unit t
    | Unerased : unit t
    | Asn : unit t
    | At : unit t
    | Lparen : unit t
    | Rparen : unit t
    | Colon : unit t
    | Semicolon : unit t
    | Double_semicolon : unit t
    | External : unit t
    | Op : Op.t t
    | Unit : unit t
    | Bool : bool t
    | Int : int64 t
    | Ident : string t
    | Unknown : string t
end

module Token : sig
  type t =
    | Eof
    | If
    | Then
    | Else
    | Fn
    | Let
    | Fun
    | And
    | Static
    | Dynamic
    | Erased
    | Unerased
    | Asn
    | At
    | Lparen
    | Rparen
    | Colon
    | Semicolon
    | Double_semicolon
    | External
    | Op of Op.t
    | Unit
    | Bool of bool
    | Int of int64
    | Ident of string
    | Unknown of string
  [@@deriving sexp]

  val get : t -> kind:'a Kind.t -> 'a option
  val str : t -> string
  val space_between : t -> t -> bool
end

module Location : sig
  type t =
    { line : int
    ; column : int
    }
  [@@deriving sexp]

  val empty : t
end

module Tokenizer : sig
  type t
  type state

  val create : string -> t
  val loc : t -> Location.t
  val save : t -> state
  val restore : t -> state -> unit
  val next : t -> Token.t
  val peek : t -> Token.t
  val skip : t -> unit
end
