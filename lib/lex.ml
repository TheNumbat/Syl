open! Core

module Char = struct
  include Char

  let whitespace = function
    | ' ' | '\n' | '\t' | '\r' | '\012' -> true
    | _ -> false
  ;;

  let starts_identifier = function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
    | _ -> false
  ;;

  let in_identifier = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false
  ;;

  let is_int = function
    | '0' .. '9' -> true
    | _ -> false
  ;;
end

module Op = struct
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

module Kind = struct
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
    | Assert : unit t
    | Op : Op.t t
    | Unit : unit t
    | Bool : bool t
    | Int : int64 t
    | Ident : string t
    | Unknown : string t
end

module Token = struct
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
    | Assert
    | Op of Op.t
    | Unit
    | Bool of bool
    | Int of int64
    | Ident of string
    | Unknown of string
  [@@deriving sexp]

  let get (type a) t ~(kind : a Kind.t) : a option =
    match t, kind with
    | If, If -> Some ()
    | Then, Then -> Some ()
    | Else, Else -> Some ()
    | Fn, Fn -> Some ()
    | Let, Let -> Some ()
    | In, In -> Some ()
    | Fun, Fun -> Some ()
    | And, And -> Some ()
    | Static, Static -> Some ()
    | Dynamic, Dynamic -> Some ()
    | Erased, Erased -> Some ()
    | Unerased, Unerased -> Some ()
    | Asn, Asn -> Some ()
    | At, At -> Some ()
    | Lparen, Lparen -> Some ()
    | Rparen, Rparen -> Some ()
    | Colon, Colon -> Some ()
    | Semicolon, Semicolon -> Some ()
    | Double_semicolon, Double_semicolon -> Some ()
    | External, External -> Some ()
    | Builtin, Builtin -> Some ()
    | Assert, Assert -> Some ()
    | Op op, Op -> Some op
    | Unit, Unit -> Some ()
    | Bool const, Bool -> Some const
    | Int const, Int -> Some const
    | Ident id, Ident -> Some id
    | Unknown slice, Unknown -> Some slice
    | ( ( Eof
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
        | Assert
        | Op _
        | Unit
        | Bool _
        | Int _
        | Ident _
        | Unknown _ )
      , _ ) -> None
  ;;

  let str = function
    | Eof -> "\000"
    | If -> "if"
    | Then -> "then"
    | Else -> "else"
    | Fn -> "fn"
    | Let -> "let"
    | In -> "in"
    | Fun -> "fun"
    | And -> "and"
    | Static -> "static"
    | Dynamic -> "dynamic"
    | Erased -> "erased"
    | Unerased -> "unerased"
    | Asn -> "="
    | At -> "@"
    | Lparen -> "("
    | Rparen -> ")"
    | Colon -> ":"
    | Semicolon -> ";"
    | Double_semicolon -> ";;"
    | External -> "external"
    | Builtin -> "builtin"
    | Assert -> "assert"
    | Op Arrow -> "->"
    | Op Minus -> "-"
    | Op Tilde -> "~"
    | Op Tilde_minus -> "~-"
    | Op Plus -> "+"
    | Op Star -> "*"
    | Op Slash -> "/"
    | Op Backslash -> "\\"
    | Op Percent -> "%"
    | Op And -> "&&"
    | Op Or -> "||"
    | Op Not -> "!"
    | Op Eq -> "=="
    | Op Neq -> "!="
    | Op Lt -> "<"
    | Op Lte -> "<="
    | Op Gt -> ">"
    | Op Gte -> ">="
    | Unit -> "()"
    | Bool true -> "true"
    | Bool false -> "false"
    | Int const -> Int64.to_string const
    | Ident slice -> slice
    | Unknown slice -> slice
  ;;

  let space_between prev next =
    match prev, next with
    | Eof, _ | _, Eof | _, Rparen | Lparen, _ -> false
    | Rparen, _
    | If, _
    | Then, _
    | Else, _
    | Fn, _
    | Let, _
    | In, _
    | Fun, _
    | And, _
    | Static, _
    | Dynamic, _
    | Erased, _
    | Unerased, _
    | Colon, _
    | Semicolon, _
    | Double_semicolon, _
    | External, _
    | Builtin, _
    | Assert, _
    | Op _, _
    | Unit, _
    | Bool _, _
    | Int _, _
    | Ident _, _
    | Unknown _, _
    | Asn, _
    | At, _ -> true
  ;;
end

module Location = struct
  type t =
    { line : int
    ; column : int
    }
  [@@deriving sexp]

  let empty = { line = 1; column = 0 }
end

module Tokenizer = struct
  open Token

  type state =
    { mutable idx : int
    ; mutable line : int
    ; mutable column : int
    }

  type t =
    { input : string
    ; state : state
    }

  let create input = { input; state = { idx = 0; column = 0; line = 1 } }
  let save t = { idx = t.state.idx; column = t.state.column; line = t.state.line }

  let restore t state =
    t.state.idx <- state.idx;
    t.state.column <- state.column;
    t.state.line <- state.line
  ;;

  let advance t =
    if Char.(String.get t.input t.state.idx = '\n')
    then (
      t.state.line <- t.state.line + 1;
      t.state.column <- 0)
    else t.state.column <- t.state.column + 1;
    t.state.idx <- t.state.idx + 1
  ;;

  let skip_while t ~f =
    while t.state.idx < String.length t.input && f (String.get t.input t.state.idx) do
      advance t
    done
  ;;

  let take_while t ~f =
    let pos = t.state.idx in
    skip_while t ~f;
    let len = t.state.idx - pos in
    String.sub t.input ~pos ~len
  ;;

  let loc t =
    skip_while t ~f:Char.whitespace;
    Location.{ line = t.state.line; column = t.state.column }
  ;;

  let single t tok =
    advance t;
    tok
  ;;

  let eof t = t.state.idx >= String.length t.input
  let current t = if eof t then None else Some (String.get t.input t.state.idx)

  let ident_or_keyword t =
    let tok = take_while t ~f:Char.in_identifier in
    match tok with
    | "if" -> If
    | "then" -> Then
    | "else" -> Else
    | "fn" -> Fn
    | "let" -> Let
    | "in" -> In
    | "fun" -> Fun
    | "and" -> And
    | "static" -> Static
    | "dynamic" -> Dynamic
    | "erased" -> Erased
    | "unerased" -> Unerased
    | "external" -> External
    | "builtin" -> Builtin
    | "assert" -> Assert
    | "true" -> Bool true
    | "false" -> Bool false
    | _ -> Ident tok
  ;;

  let op_minus t =
    match current t with
    | Some '>' -> single t (Op Arrow)
    | Some _ -> Op Minus
    | None -> Eof
  ;;

  let op_semicolon t =
    match current t with
    | Some ';' -> single t Double_semicolon
    | Some _ -> Semicolon
    | None -> Eof
  ;;

  let rec comment t =
    match current t with
    | Some '*' ->
      advance t;
      (match current t with
       | Some ')' -> advance t
       | _ ->
         advance t;
         comment t)
    | _ ->
      advance t;
      comment t
  ;;

  let op_lparen t =
    match current t with
    | Some '*' ->
      advance t;
      comment t;
      None
    | Some ')' -> Some (single t Unit)
    | Some _ -> Some Lparen
    | None -> Some Eof
  ;;

  let op_amp t =
    match current t with
    | Some '&' -> single t (Op And)
    | Some _ -> Unknown "&"
    | None -> Eof
  ;;

  let op_pipe t =
    match current t with
    | Some '|' -> single t (Op Or)
    | Some _ -> Unknown "|"
    | None -> Eof
  ;;

  let op_eq t =
    match current t with
    | Some '=' -> single t (Op Eq)
    | Some _ -> Asn
    | None -> Eof
  ;;

  let op_bang t =
    match current t with
    | Some '=' -> single t (Op Neq)
    | Some _ -> Op Not
    | None -> Eof
  ;;

  let op_lt t =
    match current t with
    | Some '=' -> single t (Op Lte)
    | Some _ -> Op Lt
    | None -> Eof
  ;;

  let op_gt t =
    match current t with
    | Some '=' -> single t (Op Gte)
    | Some _ -> Op Gt
    | None -> Eof
  ;;

  let op_tilde t =
    match current t with
    | Some '-' -> single t (Op Tilde_minus)
    | Some _ -> Op Tilde
    | None -> Eof
  ;;

  let rec next' t =
    match current t with
    | Some '(' ->
      advance t;
      (match op_lparen t with
       | Some tok -> tok
       | None -> next t)
    | Some ')' -> single t Rparen
    | Some '=' ->
      advance t;
      op_eq t
    | Some ':' -> single t Colon
    | Some ';' ->
      advance t;
      op_semicolon t
    | Some '-' ->
      advance t;
      op_minus t
    | Some '@' -> single t At
    | Some '+' -> single t (Op Plus)
    | Some '*' -> single t (Op Star)
    | Some '/' -> single t (Op Slash)
    | Some '\\' -> single t (Op Backslash)
    | Some '%' -> single t (Op Percent)
    | Some '~' ->
      advance t;
      op_tilde t
    | Some '&' ->
      advance t;
      op_amp t
    | Some '|' ->
      advance t;
      op_pipe t
    | Some '!' ->
      advance t;
      op_bang t
    | Some '<' ->
      advance t;
      op_lt t
    | Some '>' ->
      advance t;
      op_gt t
    | Some c when Char.starts_identifier c -> ident_or_keyword t
    | Some c when Char.is_int c ->
      let tok = take_while t ~f:Char.is_int in
      Int (Int64.of_string tok)
    | Some c -> single t (Unknown (String.of_char c))
    | None -> Eof

  and next t =
    skip_while t ~f:Char.whitespace;
    next' t
  ;;

  let peek t =
    let state = save t in
    let tok = next t in
    restore t state;
    tok
  ;;

  let skip t = ignore (next t)
end
