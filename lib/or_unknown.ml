open! Core

module T = struct
  type 'a t =
    | Known of 'a
    | Unknown
  [@@deriving sexp, compare, equal, hash]

  let return x = Known x

  let bind t ~f =
    match t with
    | Known x -> f x
    | Unknown -> Unknown
  ;;

  let map = `Define_using_bind
end

include T
include Monad.Make (T)

let is_true = function
  | Known true -> true
  | Known false | Unknown -> false
;;

let is_false = function
  | Known false -> true
  | Known true | Unknown -> false
;;

let is_unknown = function
  | Unknown -> true
  | Known _ -> false
;;
