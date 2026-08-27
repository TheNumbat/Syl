open! Core
open Tst

module Error : sig
  module Branch : sig
    type t =
      | Then
      | Else
      | Arm of Dst.Expr.pattern
    [@@deriving sexp_of]
  end

  module Misplaced : sig
    type t =
      | Not_under_static_branch
      | Not_in_tail_position
      | Not_in_head_position
      | All_paths_unreachable
    [@@deriving sexp_of]
  end

  module Match : sig
    type t =
      | Multiple_bindings of Ident.t
      | Expected_tuple of Value.t
      | Expected_ref of Value.t
      | Redundant of Dst.Expr.pattern Nonempty_list.t
      | Non_exhaustive of Match.Result.Missing.t Nonempty_list.t
      | Or_unbound of Ident.t Nonempty_list.t
      | Payload_mismatch of
          { label : Ident.Label.t
          ; required : bool
          }
    [@@deriving sexp_of]
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
    | Erased_external of Ident.t * string
    | Erased_dynamic_argument of Modes.t
    | Static_failure of Builtin.Error.t
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
    | Infinite_size of Tst.Value.t
    | Misplaced_unreachable of Misplaced.t
    | Gave_up of t
  [@@deriving sexp_of]
end

val typecheck : Dst.Program.t -> (Tst.Program.t, Error.t Loc.t) Result.t
val typecheck_exn : Dst.Program.t -> Tst.Program.t

module For_testing : sig
  type state

  exception Gave_up

  val create_state : unit -> state
  val register_group : state -> (Ident.t * Ids.Fn.t * Ids.Family.t * Tst.Desc.t) list -> unit
  val settle_group : state -> (Ids.Fn.t * Value.t) list -> unit
  val wait : state -> Ids.Fn.t -> (unit -> unit) -> unit
  val leq_value : state -> Value.t -> Value.t -> bool
  val join_value : state -> Value.t -> Value.t -> Value.t option
  val meet_value : state -> Value.t -> Value.t -> Value.t option
  val unfold : state -> Value.t -> Value.t
end
