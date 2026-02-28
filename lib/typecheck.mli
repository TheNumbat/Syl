open! Core
open Tst

module Error : sig
  type t =
    | Unbound_ident of Ident.t
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
    | Inline_self of Ident.t Nonempty_list.t
    | Inline_dynamic of Ident.t
    | Static_external of Ident.t
    | Recursion_limit of int
    | Dynamic_erased (* Can get rid of this once we have mode polymorphism *)
  [@@deriving sexp]
end

val typecheck : Cst.Program.t -> (Tst.Program.t, Error.t Loc.t) Result.t
val typecheck_exn : Cst.Program.t -> Tst.Program.t
