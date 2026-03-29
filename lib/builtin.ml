open! Core
open Tst
include Builtin0

let static_erased = Modes.create ~staticity:Static ~erasure:Erased

let desc : t -> Desc.t = function
  | UnitT -> { ty = Type Type; mode = static_erased; static = Lazy.from_val (Value.Type Unit) }
  | BoolT -> { ty = Type Type; mode = static_erased; static = Lazy.from_val (Value.Type Bool) }
  | IntT -> { ty = Type Type; mode = static_erased; static = Lazy.from_val (Value.Type Int) }
  | TypeT -> { ty = Type Type; mode = static_erased; static = Lazy.from_val (Value.Type Type) }
;;

let is_erased = function
  | UnitT | BoolT | IntT | TypeT -> true
;;
