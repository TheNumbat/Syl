open! Core
open Tst

include sig
  include module type of Builtin0
end

exception Divide_by_zero of Int.t

val desc : t -> Desc.t
val eval : Prim.t -> Value.t -> Value.t
