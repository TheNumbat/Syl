open! Core
open Tst

module Error : sig
  module Branch : sig
    type t =
      | Then
      | Else
      | Arm of Dst.Expr.pattern
    [@@deriving sexp]
  end

  module Misplaced : sig
    type t =
      | Not_under_static_branch
      | Not_in_tail_position
      | Not_in_head_position
      | All_paths_unreachable
    [@@deriving sexp]
  end

  module Match : sig
    type t =
      | Multiple_bindings of Ident.t
      | Expected_tuple of Value.t
      | Redundant of Dst.Expr.pattern Nonempty_list.t
      | Non_exhaustive of Match.Result.Missing.t Nonempty_list.t
      | Or_unbound of Ident.t Nonempty_list.t
      | Payload_mismatch of
          { label : Ident.Label.t
          ; required : bool
          }
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
    | Duplicate_label of Ident.Label.t
    | Unknown_label of
        { from : Tst.Value.t
        ; label : Ident.Label.t
        }
    | Expected_variant of
        { got : Tst.Value.t
        ; label : Ident.Label.t
        }
    | Unknown_builtin of Ident.t * string
    | Static_external of Ident.t * string
    | Static_failure of Builtin.Error.t
    | Static_cycle
    | Erased_application of
        { fn : Value.t
        ; result : Modes.t
        }
    | Recursion_limit of int
    | Dead_branch of
        { branch : Branch.t
        ; value : Value.t
        }
    | Unreachable_reached
    | Misplaced_unreachable of Misplaced.t
  [@@deriving sexp]
end

val typecheck : Dst.Program.t -> (Tst.Program.t, Error.t Loc.t) Result.t
val typecheck_exn : Dst.Program.t -> Tst.Program.t

module For_testing : sig
  type state

  val create_state : unit -> state
  val leq_value : state -> Value.t -> Value.t -> bool
  val join_value : state -> Value.t -> Value.t -> Value.t option
  val meet_value : state -> Value.t -> Value.t -> Value.t option
end
