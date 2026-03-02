open! Core

module Path = struct
  module Entry = struct
    type t =
      | Id of Ident.t
      | Shadow of int
      | Key of Tst.Value.Concrete.t
    [@@deriving sexp, compare, hash, equal]
  end

  module T = struct
    type t = Entry.t list [@@deriving sexp, compare, equal, hash]
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
    | Closure of
        { arg_ty : t
        ; ret_ty : t
        }
    | Thunk of t
    | Pack of ((Tst.Value.Concrete.t, t) Hashtbl.t[@sexp.opaque])
  [@@deriving sexp]

  let size_in_bytes = function
    | Unit -> 0
    | Bool -> 1
    | Int -> 8
    | Env -> 8
    | Closure _ -> 16
    | Thunk _ -> 16
    | Pack _ -> raise_s [%message "Unexpected pack"]
  ;;

  let is_zero_size t = size_in_bytes t = 0
end

module Env = struct
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

module Expr = struct
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

  let ty = function
    | Make_env { ty; _ }
    | Make_closure { ty; _ }
    | Apply_closure { ty; _ }
    | Apply_thunk { ty; _ }
    | Scalar { ty; _ }
    | Unop { ty; _ }
    | Binop { ty; _ }
    | Ident { ty; _ }
    | Builtin { ty; _ } -> ty
  ;;

  let with_ty t ty =
    match t with
    | Make_env expr -> Make_env { expr with ty }
    | Make_closure expr -> Make_closure { expr with ty }
    | Apply_closure expr -> Apply_closure { expr with ty }
    | Apply_thunk expr -> Apply_thunk { expr with ty }
    | Scalar expr -> Scalar { expr with ty }
    | Unop expr -> Unop { expr with ty }
    | Binop expr -> Binop { expr with ty }
    | Ident expr -> Ident { expr with ty }
    | Builtin expr -> Builtin { expr with ty }
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

module Decl = struct
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

module Program = struct
  type t = Decl.t array [@@deriving sexp]
end
