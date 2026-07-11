open! Core
open Tst

module Error : sig
  module Match : sig
    type t =
      | Multiple_bindings of Ident.t
      | Expected_tuple of Value.t
      | Redundant of Dst.Expr.pattern Nonempty_list.t
      | Non_exhaustive of Match.Result.Missing.t Nonempty_list.t
      | Or_unbound of Ident.t Nonempty_list.t
    [@@deriving sexp]
  end

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
    | Expected_function of
        { fn : Value.t
        ; arg : Value.t
        }
    | Cannot_unify of
        { lhs : Value.t
        ; rhs : Value.t
        }
    | Match of Match.t
    | Unknown_builtin of Ident.t * string
    | Static_external of Ident.t * string
    | Static_failure of Builtin.Error.t
    | Static_cycle
    | Erased_application of
        { fn : Value.t
        ; result : Modes.t
        }
    | Recursion_limit of int
    | Unreachable_reached
  [@@deriving sexp]
end

val typecheck : Dst.Program.t -> (Tst.Program.t, Error.t Loc.t) Result.t
val typecheck_exn : Dst.Program.t -> Tst.Program.t
