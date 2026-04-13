open! Core

module Path = struct
  module Entry = struct
    type t =
      | Id of Ident.t
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
  let id id = [ Id id ]
  let with_id t id = Id id :: t
  let with_key t key = Key key :: t
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
    | Tuple of t Nonempty_list.t
    | Pack of ((Tst.Value.Concrete.t, t) Hashtbl.t[@sexp.opaque])
  [@@deriving sexp]

  let align_to x ~align =
    assert (Int.is_pow2 align);
    (x + align - 1) land lnot (align - 1)
  ;;

  let rec size_in_mem = function
    | Unit -> 0
    | Bool -> 1
    | Int -> 8
    | Env -> 8
    | Closure _ -> 16
    | Thunk _ -> 16
    | Tuple elts ->
      Nonempty_list.fold elts ~init:0 ~f:(fun acc elt ->
        match size_in_mem elt with
        | 0 -> acc
        | size ->
          let acc = align_to acc ~align:(align_in_mem elt) in
          acc + size)
    | Pack _ -> raise_s [%message "Unexpected pack"]

  and align_in_mem = function
    | Bool -> 1
    | Int -> 8
    | Env -> 8
    | Closure _ -> 8
    | Thunk _ -> 8
    | Tuple elts ->
      Nonempty_list.fold elts ~init:1 ~f:(fun acc elt ->
        match size_in_mem elt with
        | 0 -> acc
        | _ -> Int.max acc (align_in_mem elt))
    | Pack _ -> raise_s [%message "Unexpected pack"]
    | Unit -> raise_s [%message "Unit has undefined alignment"]
  ;;

  let is_zero_size t = size_in_mem t = 0
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
    | Ident of
        { path : Path.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Tuple_get of
        { tuple : Path.t
        ; index : int
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
    | Make_tuple { ty; _ }
    | Ident { ty; _ }
    | Tuple_get { ty; _ } -> ty
  ;;

  let with_ty t ty =
    match t with
    | Make_env expr -> Make_env { expr with ty }
    | Make_closure expr -> Make_closure { expr with ty }
    | Apply_closure expr -> Apply_closure { expr with ty }
    | Apply_thunk expr -> Apply_thunk { expr with ty }
    | Scalar expr -> Scalar { expr with ty }
    | Make_tuple expr -> Make_tuple { expr with ty }
    | Ident expr -> Ident { expr with ty }
    | Tuple_get expr -> Tuple_get { expr with ty }
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
