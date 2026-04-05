open! Core

module Type = struct
  type t =
    | Unit
    | Bool
    | Int
    | Type
  [@@deriving sexp, compare, equal, hash]
end

module Prim = struct
  module Int = struct
    type t =
      | Add
      | Sub
      | Mul
      | Div
      | Mod
      | Neg
      | Eq
      | Neq
      | Lt
      | Lte
      | Gt
      | Gte
    [@@deriving sexp, compare, equal, hash]
  end

  module Bool = struct
    type t =
      | And
      | Or
      | Not
    [@@deriving sexp, compare, equal, hash]
  end

  module Type = struct
    type t =
      | Is_unit
      | Is_bool
      | Is_int
      | Is_type
      | Is_tuple
      | Is_arrow
      | Is_pi
      | Tuple_get
      | Tuple_length
      | Arrow_arg
      | Arrow_ret
      | Pi_arg
    [@@deriving sexp, compare, equal, hash]
  end

  type t =
    | Assert
    | Assert_static
    | Int of Int.t
    | Bool of Bool.t
    | Type of Type.t
  [@@deriving sexp, compare, equal, hash]

  let symbol = function
    | Assert -> "syl_assert"
    | Assert_static -> "syl_assert_static"
    | Int Add -> "syl_int_add"
    | Int Sub -> "syl_int_sub"
    | Int Mul -> "syl_int_mul"
    | Int Div -> "syl_int_div"
    | Int Mod -> "syl_int_mod"
    | Int Neg -> "syl_int_neg"
    | Int Eq -> "syl_int_eq"
    | Int Neq -> "syl_int_neq"
    | Int Lt -> "syl_int_lt"
    | Int Lte -> "syl_int_lte"
    | Int Gt -> "syl_int_gt"
    | Int Gte -> "syl_int_gte"
    | Bool And -> "syl_bool_and"
    | Bool Or -> "syl_bool_or"
    | Bool Not -> "syl_bool_not"
    | Type Is_unit -> "syl_type_is_unit"
    | Type Is_bool -> "syl_type_is_bool"
    | Type Is_int -> "syl_type_is_int"
    | Type Is_type -> "syl_type_is_type"
    | Type Is_tuple -> "syl_type_is_tuple"
    | Type Is_arrow -> "syl_type_is_arrow"
    | Type Is_pi -> "syl_type_is_pi"
    | Type Tuple_get -> "syl_type_tuple_get"
    | Type Tuple_length -> "syl_type_tuple_length"
    | Type Arrow_arg -> "syl_type_arrow_arg"
    | Type Arrow_ret -> "syl_type_arrow_ret"
    | Type Pi_arg -> "syl_type_pi_arg"
  ;;
end

module T = struct
  type t =
    | Type of Type.t
    | Prim of Prim.t
  [@@deriving sexp, compare, equal, hash]
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

let builtins =
  let prims =
    List.map
      ~f:(fun p -> Prim.symbol p, Prim p)
      Prim.
        [ Assert
        ; Assert_static
        ; Int Add
        ; Int Sub
        ; Int Mul
        ; Int Div
        ; Int Mod
        ; Int Neg
        ; Int Eq
        ; Int Neq
        ; Int Lt
        ; Int Lte
        ; Int Gt
        ; Int Gte
        ; Bool And
        ; Bool Or
        ; Bool Not
        ; Type Is_unit
        ; Type Is_bool
        ; Type Is_int
        ; Type Is_type
        ; Type Is_tuple
        ; Type Is_arrow
        ; Type Is_pi
        ; Type Tuple_get
        ; Type Tuple_length
        ; Type Arrow_arg
        ; Type Arrow_ret
        ; Type Pi_arg
        ]
  in
  Hashtbl.of_alist_exn
    (module String)
    ([ "syl_unit_t", Type Unit
     ; "syl_bool_t", Type Bool
     ; "syl_int_t", Type Int
     ; "syl_type_t", Type Type
     ]
     @ prims)
;;

let find name = Hashtbl.find builtins name

let prelude =
  {|
#include <assert.h>

static syl_unit syl_assert(syl_bool cond) {
  assert(cond);
}
static syl_int syl_int_add(syl_tuple<syl_int,syl_int> x) {
  return x.first + x.rest.first;
}
static syl_int syl_int_sub(syl_tuple<syl_int,syl_int> x) {
  return x.first - x.rest.first;
}
static syl_int syl_int_mul(syl_tuple<syl_int,syl_int> x) {
  return x.first * x.rest.first;
}
static syl_int syl_int_div(syl_tuple<syl_int,syl_int> x) {
  return x.first / x.rest.first;
}
static syl_int syl_int_mod(syl_tuple<syl_int,syl_int> x) {
  return x.first % x.rest.first;
}
static syl_int syl_int_neg(syl_int x) {
  return -x;
}
static syl_bool syl_int_eq(syl_tuple<syl_int,syl_int> x) {
  return x.first == x.rest.first;
}
static syl_bool syl_int_neq(syl_tuple<syl_int,syl_int> x) {
  return x.first != x.rest.first;
}
static syl_bool syl_int_lt(syl_tuple<syl_int,syl_int> x) {
  return x.first < x.rest.first;
}
static syl_bool syl_int_lte(syl_tuple<syl_int,syl_int> x) {
  return x.first <= x.rest.first;
}
static syl_bool syl_int_gt(syl_tuple<syl_int,syl_int> x) {
  return x.first > x.rest.first;
}
static syl_bool syl_int_gte(syl_tuple<syl_int,syl_int> x) {
  return x.first >= x.rest.first;
}
static syl_bool syl_bool_and(syl_tuple<syl_bool,syl_bool> x) {
  return x.first && x.rest.first;
}
static syl_bool syl_bool_or(syl_tuple<syl_bool,syl_bool> x) {
  return x.first || x.rest.first;
}
static syl_bool syl_bool_not(syl_bool x) {
  return !x;
}
|}
;;
