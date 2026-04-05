open! Core
open Tst

include sig
  include module type of Builtin0
end

module Error : sig
  type t =
    | Divide_by_zero of Int.t
    | Assert_failed of Value.t
    | Expected_tuple of Value.t
    | Expected_arrow of Value.t
    | Expected_pi of Value.t
    | Out_of_bounds of
        { idx : int64
        ; len : int64
        }
  [@@deriving sexp]
end

exception Error of Error.t

val desc : t -> Desc.t
val eval : Prim.t -> Value.t -> Value.t
