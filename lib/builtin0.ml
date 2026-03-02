open! Core

module T = struct
  type t =
    | UnitT
    | BoolT
    | IntT
    | TypeT
    | Assert
  [@@deriving sexp, compare, equal, hash]
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

let builtins =
  Hashtbl.of_alist_exn
    (module String)
    [ "syl_unit_t", UnitT
    ; "syl_bool_t", BoolT
    ; "syl_int_t", IntT
    ; "syl_type_t", TypeT
    ; "syl_assert", Assert
    ]
;;

let find name = Hashtbl.find builtins name
