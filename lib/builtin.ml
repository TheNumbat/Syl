open! Core
open Tst
include Builtin0

let static_erased = Modes.Modes.create ~staticity:Static ~erasure:Erased
let dynamic_unerased = Modes.Modes.create ~staticity:Dynamic ~erasure:Unerased

let desc : t -> Desc.t = function
  | UnitT -> { ty = Type Type; mode = static_erased; static = Lazy.from_val (Value.Type Unit) }
  | BoolT -> { ty = Type Type; mode = static_erased; static = Lazy.from_val (Value.Type Bool) }
  | IntT -> { ty = Type Type; mode = static_erased; static = Lazy.from_val (Value.Type Int) }
  | TypeT -> { ty = Type Type; mode = static_erased; static = Lazy.from_val (Value.Type Type) }
  | Assert ->
    let ty =
      Value.Type
        (Arrow
           { arg_ty = Type Bool
           ; arg_mode = dynamic_unerased
           ; ret_ty = Type Unit
           ; ret_mode = dynamic_unerased
           })
    in
    { ty
    ; mode = static_erased
    ; static = Lazy.from_val (Value.External { symbol = "syl_assert"; ty })
    }
;;

let is_erased = function
  | UnitT | BoolT | IntT | TypeT -> true
  | Assert -> false
;;
