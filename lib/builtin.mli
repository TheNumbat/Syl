open! Core
open Tst

include sig
  include module type of Builtin0
end

val desc : t -> Desc.t
val is_erased : t -> bool
