open! Core
open Tst

module Error : sig
  type t =
    | Unbound_ident of Ident.t
    | Mode_mismatch of
        { got : Modes.t
        ; need : Modes.t
        }
    | Type_mismatch of
        { got : Value.t
        ; need : Value.t
        }
    | Cannot_unify of
        { lhs : Value.t
        ; rhs : Value.t
        }
    | Inline_self of Ident.t Nonempty_list.t
    | Static_external of Ident.t * string
    | Unknown_builtin of Ident.t * string
    | Recursion_limit of int
    | Static_assert of Value.t
    | Divide_by_zero of Int.t
    | Unreachable_reached
    | Dynamic_erased (* Can get rid of this once we have mode polymorphism *)
  [@@deriving sexp]
end

val typecheck : Dst.Program.t -> (Tst.Program.t, Error.t Loc.t) Result.t
val typecheck_exn : Dst.Program.t -> Tst.Program.t
