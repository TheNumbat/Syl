open! Core

module Path : sig
  module Entry : sig
    type t =
      | Id of Ident.t
      | Key of Tst.Value.Concrete.t
    [@@deriving sexp, compare, equal, hash]
  end

  type t = Entry.t list [@@deriving sexp, compare, equal, hash]

  val empty : t
  val id : Ident.t -> t
  val with_id : t -> Ident.t -> t
  val with_key : t -> Tst.Value.Concrete.t -> t

  include Comparable.S with type t := t
end

module Ty : sig
  type t =
    | Unit
    | Bool
    | Int
    | Env
    | Closure of
        { arg_ty : t
        ; ret_ty : t
        }
    | Thunk of t
    | Tuple of t list
    | Pack of (Tst.Value.Concrete.t, t) Hashtbl.t
  [@@deriving sexp]

  val size_in_mem : t -> int
  val align_in_mem : t -> int
  val is_zero_size : t -> bool
  val align_to : int -> align:int -> int
end

module Env : sig
  type entry =
    { path : Path.t
    ; ty : Ty.t
    ; offset_in_bytes : int
    }
  [@@deriving sexp]

  type t =
    { entries : entry array
    ; size_in_bytes : int
    }
  [@@deriving sexp]
end

module Expr : sig
  type t =
    | Scalar of
        { value : Sst.Scalar.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Make_env of
        { captures : Env.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Make_closure of
        { body : Path.t
        ; env : Path.t option
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Make_tuple of
        { elts : (Path.t * Ty.t) array
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Apply_closure of
        { fn : Path.t
        ; arg : Path.t
        ; arg_ty : Ty.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Apply_thunk of
        { fn : Path.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Unop of
        { op : Ident.Unop.t
        ; arg : Path.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Binop of
        { op : Ident.Binop.t
        ; lhs : Path.t
        ; rhs : Path.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Ident of
        { path : Path.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Builtin of
        { builtin : Builtin.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
  [@@deriving sexp]

  val ty : t -> Ty.t
  val with_ty : t -> Ty.t -> t
end

module Stmt : sig
  type values =
    { exprs : (Path.t * Expr.t) array
    ; bind : t array
    ; loc : Lex.Location.t
    }
  [@@deriving sexp]

  and functions =
    { paths : (Path.t * Ty.t * Path.t) array
    ; captures : Env.t
    ; loc : Lex.Location.t
    }
  [@@deriving sexp]

  and t =
    | Values of values
    | Functions of functions
    | If of
        { path : Path.t
        ; cond : Expr.t
        ; then_bind : t array
        ; then_ : Expr.t
        ; else_bind : t array
        ; else_ : Expr.t
        ; loc : Lex.Location.t
        }
  [@@deriving sexp]
end

module Decl : sig
  type t =
    | Values of Stmt.values
    | Functions of Stmt.functions
    | Closure_body of
        { path : Path.t
        ; arg : Path.t
        ; arg_ty : Ty.t
        ; captures : Env.entry array
        ; bind : Stmt.t array
        ; return : Expr.t
        ; loc : Lex.Location.t
        }
    | Thunk_body of
        { path : Path.t
        ; captures : Env.entry array
        ; bind : Stmt.t array
        ; return : Expr.t
        ; loc : Lex.Location.t
        }
    | External of
        { path : Path.t
        ; symbol : string
        ; arg_ty : Ty.t
        ; ret_ty : Ty.t
        ; loc : Lex.Location.t
        }
  [@@deriving sexp]
end

module Program : sig
  type t = Decl.t array [@@deriving sexp]
end
