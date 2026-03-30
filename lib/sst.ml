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
    | Tuple of t list
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
        { elts : t list
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
    | Pack { ty; _ }
    | Symbol { ty; _ }
    | External { ty; _ } -> ty
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
    | Pack expr -> Pack { expr with ty }
    | Symbol expr -> Symbol { expr with ty }
    | External expr -> External { expr with ty }
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
    | Pack { loc; _ }
    | Symbol { loc; _ }
    | External { loc; _ } -> loc
  ;;

  let rec free_keys : t -> Tst.Value.Concrete.Set.t Ident.Map.t = function
    | Scalar _ | External _ -> Ident.Map.empty
    | Var { id; _ } -> Ident.Map.singleton id Tst.Value.Concrete.Set.empty
    | Symbol { fn; arg; _ } -> Ident.Map.map (free_keys fn) ~f:(fun keys -> Set.add keys arg)
    | Tuple { elts; _ } ->
      List.map elts ~f:free_keys
      |> List.fold ~init:Ident.Map.empty ~f:(Map.merge_skewed ~combine:(fun ~key:_ -> Set.union))
    | Apply { fn; arg; _ } ->
      Map.merge_skewed (free_keys fn) (free_keys arg) ~combine:(fun ~key:_ -> Set.union)
    | If { cond; then_; else_; _ } ->
      Map.merge_skewed (free_keys then_) (free_keys else_) ~combine:(fun ~key:_ -> Set.union)
      |> Map.merge_skewed (free_keys cond) ~combine:(fun ~key:_ -> Set.union)
    | Lambda { arg; body; _ } -> Map.remove (free_keys body) arg
    | Let { var; bind; rest; _ } ->
      Map.merge_skewed
        (free_keys bind)
        (Map.remove (free_keys rest) var)
        ~combine:(fun ~key:_ -> Set.union)
    | Pack { pack; _ } ->
      Hashtbl.data pack
      |> List.map ~f:(fun (expr, _) -> free_keys expr)
      |> List.fold ~init:Ident.Map.empty ~f:(Map.merge_skewed ~combine:(fun ~key:_ -> Set.union))
    | Fun { funs; rest; _ } ->
      let bound_ids =
        Nonempty_list.map funs ~f:(function
          | Mono { var; _ } -> var
          | Pack { var; _ } -> var)
      in
      let fvs_in_funs =
        Nonempty_list.fold funs ~init:Ident.Map.empty ~f:(fun acc -> function
          | Mono { arg; body; _ } ->
            Map.merge_skewed
              acc
              (Map.remove (free_keys body) arg)
              ~combine:(fun ~key:_ -> Set.union)
          | Pack { pack; _ } ->
            let fvs =
              Hashtbl.data pack
              |> List.map ~f:(fun (expr, _) -> free_keys expr)
              |> List.fold
                   ~init:Ident.Map.empty
                   ~f:(Map.merge_skewed ~combine:(fun ~key:_ -> Set.union))
            in
            Map.merge_skewed acc fvs ~combine:(fun ~key:_ -> Set.union))
      in
      let fvs = Map.merge_skewed fvs_in_funs (free_keys rest) ~combine:(fun ~key:_ -> Set.union) in
      Nonempty_list.fold bound_ids ~init:fvs ~f:Map.remove
  ;;
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
