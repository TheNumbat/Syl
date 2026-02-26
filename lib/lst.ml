open! Core

module Path = struct
  module Entry = struct
    type t =
      | Id of Ident.t
      | Shadow of int
      | Key of Tst.Value.Concrete.t
    [@@deriving sexp, compare, equal]
  end

  module T = struct
    type t = Entry.t list [@@deriving sexp, compare, equal]
  end

  open Entry
  include T
  include Comparable.Make (T)

  let empty = []
  let with_id t id = Id id :: t
  let with_key t key = Key key :: t
  let with_shadow t n = Shadow n :: t
end

module Ty = struct
  type t =
    | Unit
    | Bool
    | Int
    | Env
    | Closure
    | Thunk
    | Pack of ((Tst.Value.Concrete.t, t) Hashtbl.t[@sexp.opaque])
  [@@deriving sexp]
end

module Expr = struct
  type t =
    | Scalar of
        { value : Sst.Scalar.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Make_env of
        { captures : (Path.t * Ty.t) array
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

  let ty = function
    | Make_env { ty; _ }
    | Make_closure { ty; _ }
    | Make_thunk { ty; _ }
    | Apply_closure { ty; _ }
    | Apply_thunk { ty; _ }
    | Scalar { ty; _ }
    | Unop { ty; _ }
    | Binop { ty; _ }
    | Ident { ty; _ } -> ty
  ;;
end

module Stmt = struct
  type values =
    { exprs : (Path.t * Expr.t) array
    ; bind : t array
    ; loc : Lex.Location.t
    }
  [@@deriving sexp]

  and functions =
    { closures : (Path.t * Path.t) array
    ; thunks : (Path.t * Path.t) array
    ; captures : (Path.t * Ty.t) array
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

module Decl = struct
  type t =
    | Values of Stmt.values
    | Functions of Stmt.functions
    | Closure_body of
        { path : Path.t
        ; arg : Path.t
        ; arg_ty : Ty.t
        ; captures : (Path.t * Ty.t) array
        ; bind : Stmt.t array
        ; return : Expr.t
        ; loc : Lex.Location.t
        }
    | Thunk_body of
        { path : Path.t
        ; captures : (Path.t * Ty.t) array
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

module Program = struct
  type t = Decl.t array [@@deriving sexp]
end
