open! Core
open Tst

module Error : sig
  type t =
    | Mode_mismatch of
        { got : Modes.Modes.t
        ; need : Modes.Modes.t
        }
    | Type_mismatch of
        { got : Value.t
        ; need : Value.t
        }
    | Cannot_unify of
        { lhs : Value.t
        ; rhs : Value.t
        }
    | Unbound_ident of Ident.t
    | Recursion_limit of int
    | Dynamic_erased
    | Inline_self
    | Bad_external
  [@@deriving sexp]
end

val typecheck : Cst.Program.t -> (Tst.Program.t, Error.t Loc.t) Result.t
val typecheck_exn : Cst.Program.t -> Tst.Program.t
