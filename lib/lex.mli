open! Core

module Op : sig
  type t =
    | Tilde
    | Tilde_minus
    | Minus
    | Plus
    | Star
    | Slash
    | Backslash
    | Percent
    | And
    | Amp
    | Or
    | Not
    | Eq
    | Neq
    | Lt
    | Lte
    | Gt
    | Gte
    | Arrow
    | Caret
    | Comma
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
    | In : unit t
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
    | Builtin : unit t
    | Match : unit t
    | Pipe : unit t
    | Assert : unit t
    | Unreachable : unit t
    | Variant : unit t
    | Box : unit t
    | Lbrace : unit t
    | Rbrace : unit t
    | Op : Op.t t
    | Unit : unit t
    | Bool : bool t
    | Int : int64 t
    | Ident : string t
    | Label : string t
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
    | In
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
    | Builtin
    | Match
    | Pipe
    | Assert
    | Unreachable
    | Variant
    | Box
    | Lbrace
    | Rbrace
    | Op of Op.t
    | Unit
    | Bool of bool
    | Int of int64
    | Ident of string
    | Label of string
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
  [@@deriving sexp, compare, equal, hash]

  val empty : t
end

module Comment : sig
  type t =
    { text : string
    ; loc : Location.t
    }
  [@@deriving sexp]
end

exception Unterminated_comment of Location.t [@@deriving sexp]

module Tokenizer : sig
  type t

  val create : string -> t
  val loc : t -> Location.t
  val next : t -> Token.t
  val peek : t -> Token.t
  val peek2 : t -> Token.t * Token.t
  val skip : t -> unit
  val comments : t -> Location.t * Comment.t list
end
