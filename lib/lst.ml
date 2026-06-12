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
  include Hashable.Make (T)
  include Comparable.Make (T)

  let empty = []
  let id id = [ Id id ]
  let with_id t id = Id id :: t
  let with_key t key = Key key :: t

  let with_ ?id ?key t =
    let t = Option.value_map id ~default:t ~f:(with_id t) in
    Option.value_map key ~default:t ~f:(with_key t)
  ;;
end

module Ty = struct
  type t =
    | Unit
    | Bool
    | Int
    | Type
    | Tuple of t Nonempty_list.t
    | Env
    | Closure of
        { arg_ty : t
        ; ret_ty : t
        }
  [@@deriving sexp]

  let align_to x ~align =
    assert (Int.is_pow2 align);
    (x + align - 1) land lnot (align - 1)
  ;;

  let rec size_in_mem = function
    | Unit | Type -> 0
    | Bool -> 1
    | Int -> 8
    | Env -> 8
    | Closure _ -> 16
    | Tuple elts ->
      Nonempty_list.fold elts ~init:0 ~f:(fun acc elt ->
        match size_in_mem elt with
        | 0 -> acc
        | size ->
          let acc = align_to acc ~align:(align_in_mem elt) in
          acc + size)

  and align_in_mem = function
    | Bool -> 1
    | Int -> 8
    | Env -> 8
    | Closure _ -> 8
    | Tuple elts ->
      Nonempty_list.fold elts ~init:1 ~f:(fun acc elt ->
        match size_in_mem elt with
        | 0 -> acc
        | _ -> Int.max acc (align_in_mem elt))
    | Unit | Type -> raise_s [%message "Bug: undefined alignment"]
  ;;

  let is_zero_size t = size_in_mem t = 0
end

module Env = struct
  type entry =
    { path : Path.t
    ; ty : Ty.t
    ; offset : int
    }
  [@@deriving sexp]

  type t =
    { entries : entry array
    ; length : int
    }
  [@@deriving sexp]

  let empty = { entries = [||]; length = 0 }
end

module Expr = struct
  type nonrec t =
    | Scalar of
        { value : Sst.Scalar.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Make_env of
        { env : Env.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Make_env_rec of
        { length : int
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Fill_env_rec of
        { path : Path.t
        ; env : Env.t
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
        ; env : Path.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Apply_proc of
        { fn : Path.t
        ; arg : Path.t
        ; arg_ty : Ty.t
        ; env : Path.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Extcall of
        { symbol : string
        ; arg : Path.t
        ; arg_ty : Ty.t
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
    | Make_env_rec { ty; _ }
    | Fill_env_rec { ty; _ }
    | Make_closure { ty; _ }
    | Apply_closure { ty; _ }
    | Apply_thunk { ty; _ }
    | Apply_proc { ty; _ }
    | Scalar { ty; _ }
    | Make_tuple { ty; _ }
    | Ident { ty; _ }
    | Tuple_get { ty; _ } -> ty
    | Extcall { ty; _ } -> ty
  ;;

  let loc = function
    | Make_env { loc; _ }
    | Make_env_rec { loc; _ }
    | Fill_env_rec { loc; _ }
    | Make_closure { loc; _ }
    | Apply_closure { loc; _ }
    | Apply_thunk { loc; _ }
    | Apply_proc { loc; _ }
    | Scalar { loc; _ }
    | Make_tuple { loc; _ }
    | Ident { loc; _ }
    | Tuple_get { loc; _ } -> loc
    | Extcall { loc; _ } -> loc
  ;;

  let with_ty t ty =
    match t with
    | Make_env expr -> Make_env { expr with ty }
    | Make_env_rec expr -> Make_env_rec { expr with ty }
    | Fill_env_rec expr -> Fill_env_rec { expr with ty }
    | Make_closure expr -> Make_closure { expr with ty }
    | Apply_closure expr -> Apply_closure { expr with ty }
    | Apply_thunk expr -> Apply_thunk { expr with ty }
    | Apply_proc expr -> Apply_proc { expr with ty }
    | Scalar expr -> Scalar { expr with ty }
    | Make_tuple expr -> Make_tuple { expr with ty }
    | Ident expr -> Ident { expr with ty }
    | Tuple_get expr -> Tuple_get { expr with ty }
    | Extcall expr -> Extcall { expr with ty }
  ;;
end

module Block = struct
  type case =
    { bindings : (Path.t * Ty.t) array
    ; block : t
    }
  [@@deriving sexp]

  and tree =
    | Leaf of
        { case : int
        ; bindings : (Path.t * Expr.t) array
        }
    | Split of
        { cond : Expr.t
        ; then_ : tree
        ; else_ : tree
        }
  [@@deriving sexp]

  and t =
    | Block of
        { bindings : (Path.t * t) array
        ; return : Expr.t
        }
    | If of
        { cond : Expr.t
        ; then_ : t
        ; else_ : t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Match of
        { tree : tree
        ; cases : case array
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
  [@@deriving sexp]

  let ty = function
    | Block { return; _ } -> Expr.ty return
    | If { ty; _ } | Match { ty; _ } -> ty
  ;;

  let loc = function
    | Block { return; _ } -> Expr.loc return
    | If { loc; _ } | Match { loc; _ } -> loc
  ;;
end

module Proc = struct
  type t =
    | Closure of
        { path : Path.t
        ; arg : Path.t
        ; arg_ty : Ty.t
        ; env : Env.t
        ; body : Block.t
        ; loc : Lex.Location.t
        }
    | Thunk of
        { path : Path.t
        ; env : Env.t
        ; body : Block.t
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
  type t =
    { procs : Proc.t array
    ; bindings : (Path.t * Block.t) array
    }
  [@@deriving sexp]
end
