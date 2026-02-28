open! Core

module Path : sig
  module Entry : sig
    type t =
      | Id of Ident.t
      | Shadow of int
      | Key of Tst.Value.Concrete.t
    [@@deriving sexp, compare, equal, hash]
  end

  type t = Entry.t list [@@deriving sexp, compare, equal, hash]

  val empty : t
  val with_id : t -> Ident.t -> t
  val with_key : t -> Tst.Value.Concrete.t -> t
  val with_shadow : t -> int -> t

  include Comparable.S with type t := t
end

module Ty : sig
  type t =
    | Unit
    | Bool
    | Int
    | Env
    | Closure
    | Thunk
    | Pack of (Tst.Value.Concrete.t, t) Hashtbl.t
  [@@deriving sexp]
end

module Env : sig
  type t = (Path.t * Ty.t) array [@@deriving sexp]

  module Sub : sig
    type t = (Path.t * Ty.t * int) array [@@deriving sexp]
  end
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
    | Make_thunk of
        { body : Path.t
        ; env : Path.t option
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Apply_closure of
        { fn : Path.t
        ; arg : Path.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Apply_thunk of
        { fn : Path.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Unop of
        { op : Cst.Unop.t
        ; arg : Path.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Binop of
        { op : Cst.Binop.t
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
  [@@deriving sexp]

  val ty : t -> Ty.t
end

module Stmt : sig
  type values =
    { exprs : (Path.t * Expr.t) array
    ; bind : t array
    ; loc : Lex.Location.t
    }
  [@@deriving sexp]

  and functions =
    { closures : (Path.t * Path.t) array
    ; thunks : (Path.t * Path.t) array
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
        ; captures : Env.Sub.t
        ; bind : Stmt.t array
        ; return : Expr.t
        ; loc : Lex.Location.t
        }
    | Thunk_body of
        { path : Path.t
        ; captures : Env.Sub.t
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
