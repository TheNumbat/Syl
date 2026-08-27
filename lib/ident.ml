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

module Raw = struct
  module T = struct
    type t =
      | Anon
      | Unop of Unop.t
      | Binop of Binop.t
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

  let print () = function
    | Anon -> "_"
    | Unop op -> sprintf "(%a)" Unop.print op
    | Binop op -> sprintf "(%a)" Binop.print op
    | Id id -> id
  ;;
end

module Label = struct
  module T = struct
    type t = string [@@deriving sexp, compare, equal, hash]
  end

  include T
  include Hashable.Make (T)
  include Comparable.Make (T)

  let of_string t = t
  let print () t = t
end

module T = struct
  type t = Raw.t * (Ids.Stamp.t[@sexp.opaque]) [@@deriving sexp, compare, equal, hash]
end

include T
include Hashable.Make (T)
include Comparable.Make (T)

let create raw ~stamp = raw, stamp
let fresh raw = raw, Ids.Stamp.create ()
let unbound raw = raw, Ids.Stamp.of_int_exn (-1)

let is_anon : t -> bool = function
  | Anon, _ -> true
  | _ -> false
;;

let print () (raw, stamp) =
  match Ids.Stamp.to_int_exn stamp with
  | 0 -> Raw.print () raw
  | stamp -> Raw.print () raw ^ "ˢ" ^ Int.to_string stamp
;;

let name () ((raw, stamp) : t) =
  let name =
    match raw with
    | Anon -> "_"
    | Unop Neg -> "opˢneg"
    | Unop Not -> "opˢnot"
    | Binop Add -> "opˢadd"
    | Binop Sub -> "opˢsub"
    | Binop Mul -> "opˢmul"
    | Binop Div -> "opˢdiv"
    | Binop Mod -> "opˢmod"
    | Binop And -> "opˢand"
    | Binop Or -> "opˢor"
    | Binop Eq -> "opˢeq"
    | Binop Neq -> "opˢneq"
    | Binop Lt -> "opˢlt"
    | Binop Lte -> "opˢlte"
    | Binop Gt -> "opˢgt"
    | Binop Gte -> "opˢgte"
    | Id id -> id
  in
  match Ids.Stamp.to_int_exn stamp with
  | 0 -> name
  | stamp -> name ^ "ˢ" ^ Int.to_string stamp
;;
