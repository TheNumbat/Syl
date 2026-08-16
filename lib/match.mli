open! Core
module Literal = Cst.Literal

module Pattern : sig
  type t = Dst.Expr.pattern =
    | Var of
        { id : Ident.t
        ; loc : Lex.Location.t
        }
    | Literal of
        { value : Literal.t
        ; loc : Lex.Location.t
        }
    | Constructor of
        { label : Ident.Label.t
        ; payload : t option
        ; loc : Lex.Location.t
        }
    | Tuple of
        { elts : t Nonempty_list.t
        ; loc : Lex.Location.t
        }
    | Or of
        { left : t
        ; right : t
        ; loc : Lex.Location.t
        }
  [@@deriving sexp]
end

module Tree : sig
  module Occurrence : sig
    type t =
      { path : Tst.Pattern.Step.t Vec.t
      ; ty : Tst.Value.t
      }
    [@@deriving sexp]
  end

  module Head : sig
    type t =
      | Literal of Literal.t
      | Tuple of int
      | Constructor of
          { label : Ident.Label.t
          ; payload : bool
          }
    [@@deriving sexp]
  end

  type t =
    | Fail
    | Leaf of
        { case : int
        ; bindings : Occurrence.t Ident.Map.t
        }
    | Switch of
        { occurrence : Occurrence.t
        ; cases : (Head.t * t) array
        ; default : t option
        }
  [@@deriving sexp]
end

module Result : sig
  module Missing : sig
    type t =
      | All_except of Tst.Pattern.Excluded.t list
      | Literal of Literal.t
      | Tuple of t list
      | Constructor of
          { label : Ident.Label.t
          ; payload : t option
          }
      | Or of t list
    [@@deriving sexp]
  end

  type t =
    { tree : Tree.t
    ; redundant : Pattern.t list
    ; missing : Missing.t list
    }
  [@@deriving sexp]
end

(** [unfold] head-normalizes a type for syntactic inspection; it must be the identity
    where no reduction applies. *)
val compile
  :  scrutinee:Tst.Value.t
  -> ty:Tst.Value.t
  -> unfold:(Tst.Value.t -> Tst.Value.t)
  -> Pattern.t Nonempty_list.t
  -> Result.t
