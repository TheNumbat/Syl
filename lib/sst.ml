open! Core

module Ty = struct
  type t =
    | Unit
    | Bool
    | Int
    | Arrow of
        { arg_ty : t
        ; ret_ty : t
        }
    | Tuple of t Nonempty_list.t
    | Pack of ((Tst.Value.Concrete.t, t) Hashtbl.t[@sexp.opaque])
  [@@deriving sexp]

  let arg = function
    | Arrow { arg_ty; _ } -> arg_ty
    | ty -> raise_s [%message "Expected arrow" (ty : t)]
  ;;

  let ret = function
    | Arrow { ret_ty; _ } -> ret_ty
    | ty -> raise_s [%message "Expected arrow" (ty : t)]
  ;;

  let find t key =
    match t with
    | Pack pack | Arrow { arg_ty = Unit; ret_ty = Pack pack } -> Hashtbl.find_exn pack key
    | ty -> raise_s [%message "Expected pack" (ty : t)]
  ;;
end

module Scalar = struct
  type t =
    | Unit
    | Bool of bool
    | Int of int64
  [@@deriving sexp]
end

module Access = struct
  type t =
    | Base
    | Keys of t Tst.Value.Concrete.Map.t
  [@@deriving sexp]

  let rec union (a : t) (b : t) : t =
    match a, b with
    | Base, _ | _, Base -> Base
    | Keys ma, Keys mb -> Keys (Map.merge_skewed ma mb ~combine:(fun ~key:_ -> union))
  ;;
end

module Expr = struct
  type pack = ((Tst.Value.Concrete.t, t * Ty.t Ident.Map.t) Hashtbl.t[@sexp.opaque])

  and fun_ =
    | Mono of
        { var : Ident.t
        ; arg : Ident.t
        ; body : t
        ; fvs : Ty.t Ident.Map.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Pack of
        { var : Ident.t
        ; pack : pack
        ; fvs : Ty.t Ident.Map.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
  [@@deriving sexp]

  and t =
    | Scalar of
        { value : Scalar.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Fun of
        { funs : fun_ Nonempty_list.t
        ; rest : t
        ; fvs : Ty.t Ident.Map.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Lambda of
        { arg : Ident.t
        ; body : t
        ; fvs : Ty.t Ident.Map.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Apply of
        { fn : t
        ; arg : t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Let of
        { var : Ident.t
        ; bind : t
        ; rest : t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Tuple of
        { elts : t Nonempty_list.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | If of
        { cond : t
        ; then_ : t
        ; else_ : t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Var of
        { id : Ident.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Sequence of
        { first : t
        ; second : t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Pack of
        { pack : pack
        ; fvs : Ty.t Ident.Map.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Symbol of
        { fn : t
        ; arg : Tst.Value.Concrete.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | External of
        { symbol : string
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Tuple_get of
        { tuple : t
        ; index : int
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
  [@@deriving sexp]

  let ty : t -> Ty.t = function
    | Scalar { ty; _ }
    | Fun { ty; _ }
    | Lambda { ty; _ }
    | Apply { ty; _ }
    | Let { ty; _ }
    | Tuple { ty; _ }
    | If { ty; _ }
    | Var { ty; _ }
    | Sequence { ty; _ }
    | Pack { ty; _ }
    | Symbol { ty; _ }
    | External { ty; _ }
    | Tuple_get { ty; _ } -> ty
  ;;

  let with_ty t ty =
    match t with
    | Scalar expr -> Scalar { expr with ty }
    | Fun expr -> Fun { expr with ty }
    | Lambda expr -> Lambda { expr with ty }
    | Apply expr -> Apply { expr with ty }
    | Let expr -> Let { expr with ty }
    | Tuple expr -> Tuple { expr with ty }
    | If expr -> If { expr with ty }
    | Var expr -> Var { expr with ty }
    | Sequence expr -> Sequence { expr with ty }
    | Pack expr -> Pack { expr with ty }
    | Symbol expr -> Symbol { expr with ty }
    | External expr -> External { expr with ty }
    | Tuple_get expr -> Tuple_get { expr with ty }
  ;;

  let loc : t -> Lex.Location.t = function
    | Scalar { loc; _ }
    | Fun { loc; _ }
    | Lambda { loc; _ }
    | Apply { loc; _ }
    | Let { loc; _ }
    | Tuple { loc; _ }
    | If { loc; _ }
    | Var { loc; _ }
    | Sequence { loc; _ }
    | Pack { loc; _ }
    | Symbol { loc; _ }
    | External { loc; _ }
    | Tuple_get { loc; _ } -> loc
  ;;

  let merge = Map.merge_skewed ~combine:(fun ~key:_ -> Access.union)

  let rec free_keys_at (acc : Access.t) : t -> Access.t Ident.Map.t = function
    | Scalar _ | External _ -> Ident.Map.empty
    | Var { id; _ } -> Ident.Map.singleton id acc
    | Symbol { fn; arg; _ } -> free_keys_at (Keys (Tst.Value.Concrete.Map.singleton arg acc)) fn
    | Tuple { elts; _ } ->
      Nonempty_list.fold elts ~init:Ident.Map.empty ~f:(fun acc elt -> merge acc (free_keys elt))
    | Apply { fn; arg; _ } -> merge (free_keys fn) (free_keys arg)
    | Tuple_get { tuple; _ } -> free_keys tuple
    | If { cond; then_; else_; _ } ->
      merge (free_keys then_) (free_keys else_) |> merge (free_keys cond)
    | Lambda { arg; body; _ } -> Map.remove (free_keys body) arg
    | Let { var; bind; rest; _ } -> merge (free_keys bind) (Map.remove (free_keys rest) var)
    | Sequence { first; second; _ } -> merge (free_keys first) (free_keys second)
    | Pack { pack; _ } ->
      Hashtbl.data pack
      |> List.map ~f:(fun (expr, _) -> free_keys expr)
      |> List.fold ~init:Ident.Map.empty ~f:merge
    | Fun { funs; rest; _ } ->
      let bound_ids =
        Nonempty_list.map funs ~f:(function
          | Mono { var; _ } -> var
          | Pack { var; _ } -> var)
      in
      let fvs_in_funs =
        Nonempty_list.fold funs ~init:Ident.Map.empty ~f:(fun acc -> function
          | Mono { arg; body; _ } -> merge acc (Map.remove (free_keys body) arg)
          | Pack { pack; _ } ->
            let fvs =
              Hashtbl.data pack
              |> List.map ~f:(fun (expr, _) -> free_keys expr)
              |> List.fold ~init:Ident.Map.empty ~f:merge
            in
            merge acc fvs)
      in
      let fvs = merge fvs_in_funs (free_keys rest) in
      Nonempty_list.fold bound_ids ~init:fvs ~f:Map.remove

  and free_keys expr = free_keys_at Base expr
end

module Top_level = struct
  type t =
    | Let of
        { var : Ident.t
        ; bind : Expr.t
        ; loc : Lex.Location.t
        }
    | Fun of
        { funs : Expr.fun_ Nonempty_list.t
        ; fvs : Ty.t Ident.Map.t
        ; loc : Lex.Location.t
        }
    | External of
        { var : Ident.t
        ; symbol : string
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
  [@@deriving sexp]
end

module Program = struct
  type t = Top_level.t list [@@deriving sexp]
end
