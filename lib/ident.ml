open! Core

module Unop = struct
  type t =
    | Not
    | Neg
  [@@deriving sexp, compare, equal, hash]

  let print () = function
    | Not -> "!"
    | Neg -> "-"
  ;;
end

module Binop = struct
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

  let print () = function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Mod -> "%"
    | And -> "&&"
    | Or -> "||"
    | Eq -> "=="
    | Neq -> "!="
    | Lt -> "<"
    | Lte -> "<="
    | Gt -> ">"
    | Gte -> ">="
  ;;
end

module Nop = struct
  type t =
    | Comma
    | Caret
  [@@deriving sexp, compare, equal, hash]

  let print () = function
    | Comma -> ","
    | Caret -> "^"
  ;;

  let sep = function
    | Comma -> ", "
    | Caret -> " ^ "
  ;;
end

module Raw = struct
  module T = struct
    type t =
      | Anon
      | Unop of Unop.t
      | Binop of Binop.t
      | Nop of Nop.t
      | Id of string
    [@@deriving sexp, compare, equal, hash]
  end

  include T
  include Hashable.Make (T)
  include Comparable.Make (T)

  let anon = Anon

  let id id =
    match id with
    | "_" -> Anon
    | _ -> Id id
  ;;

  let unop op = Unop op
  let binop op = Binop op
  let nop op = Nop op

  let print () = function
    | Anon -> "_"
    | Unop op -> sprintf "(%a)" Unop.print op
    | Binop op -> sprintf "(%a)" Binop.print op
    | Nop op -> sprintf "(%a)" Nop.print op
    | Id id -> id
  ;;
end

module T = struct
  type t = Raw.t * (int[@sexp.opaque]) [@@deriving sexp, compare, equal, hash]
end

include T
include Hashable.Make (T)
include Comparable.Make (T)

let create raw ~stamp = raw, stamp

let is_anon : t -> bool = function
  | Anon, _ -> true
  | _ -> false
;;

let print () (raw, stamp) =
  match stamp with
  | 0 -> Raw.print () raw
  | _ -> Raw.print () raw ^ "ˢ" ^ Int.to_string stamp
;;

let name () ((raw, stamp) : t) =
  let name =
    match raw with
    | Anon -> "_"
    | Unop Neg -> "neg"
    | Unop Not -> "not"
    | Binop Add -> "add"
    | Binop Sub -> "sub"
    | Binop Mul -> "mul"
    | Binop Div -> "div"
    | Binop Mod -> "mod"
    | Binop And -> "and"
    | Binop Or -> "or"
    | Binop Eq -> "eq"
    | Binop Neq -> "neq"
    | Binop Lt -> "lt"
    | Binop Lte -> "lte"
    | Binop Gt -> "gt"
    | Binop Gte -> "gte"
    | Nop Comma -> "comma"
    | Nop Caret -> "caret"
    | Id id -> id
  in
  match stamp with
  | 0 -> name
  | _ -> name ^ "ˢ" ^ Int.to_string stamp
;;
