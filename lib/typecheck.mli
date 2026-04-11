open! Core
open Tst

module Error : sig
  type t =
    | Unbound_ident of Ident.t
    | Unknown_builtin of Ident.t * string
    | Mode_mismatch of
        { got : Modes.t
        ; need : Modes.t (* TODO why *)
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
    | Redundant_patterns of Dst.Expr.pattern Nonempty_list.t
    | Static_external of Ident.t * string
    | Static_failure of Builtin.Error.t
    | Inline_self of Ident.t Nonempty_list.t
    | Recursion_limit of int
    | Dynamic_erased
    | Unreachable_reached
  [@@deriving sexp]
end

val typecheck : Dst.Program.t -> (Tst.Program.t, Error.t Loc.t) Result.t
val typecheck_exn : Dst.Program.t -> Tst.Program.t
