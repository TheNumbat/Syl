open! Core
module Key = Tst.Value.Concrete

module Ty = struct
  type t =
    | Unit
    | Bool
    | Int
    | Type
    | Tuple of t Nonempty_list.t
    | Arrow of
        { arg_ty : t
        ; ret_ty : t
        }
    | Pi of
        { arg_ty : t
        ; ret_ty : t Key.Map.t
        }
    | Variant of t option Ident.Label.Map.t
  [@@deriving sexp_of]
end

module Scalar = struct
  type t =
    | Unit
    | Type
    | Bool of bool
    | Int of int64
  [@@deriving sexp_of]
end

module Expr = struct
  type target =
    | Family of (int[@sexp.opaque])
    | Prim of Builtin0.Prim.t
  [@@deriving sexp_of]

  type fun_ =
    | Lambda of
        { var : Ident.t
        ; arg : Ident.t
        ; body : t
        ; ty : Ty.t
        ; family : (int[@sexp.opaque])
        ; loc : Lex.Location.t
        }
    | Binder of
        { var : Ident.t
        ; arg : Ident.t
        ; body : t Key.Map.t
        ; ty : Ty.t
        ; family : (int[@sexp.opaque])
        ; loc : Lex.Location.t
        }
  [@@deriving sexp_of]

  and case =
    { body : t
    ; bindings : Ty.t Ident.Map.t
    }
  [@@deriving sexp_of]

  and tree =
    | Leaf of
        { case : int
        ; bindings : t Ident.Map.t
        }
    | Split of
        { cond : t
        ; then_ : tree
        ; else_ : tree
        }
  [@@deriving sexp_of]

  and t =
    | Scalar of
        { value : Scalar.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Var of
        { id : Ident.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Fun of
        { funs : fun_ Nonempty_list.t
        ; rest : t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Lambda of
        { arg : Ident.t
        ; body : t
        ; ty : Ty.t
        ; family : (int[@sexp.opaque])
        ; loc : Lex.Location.t
        }
    | Binder of
        { arg : Ident.t
        ; body : t Key.Map.t
        ; ty : Ty.t
        ; family : (int[@sexp.opaque])
        ; loc : Lex.Location.t
        }
    | Apply of
        { fn : t
        ; arg : t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Specialize of
        { fn : t
        ; arg : t
        ; target : target
        ; key : Key.t option
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
    | Tuple_get of
        { tuple : t
        ; index : int
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Constructor of
        { label : Ident.Label.t
        ; payload : t option
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Inject of
        { label : Ident.Label.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Payload_get of
        { variant : t
        ; label : Ident.Label.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Tag_test of
        { variant : t
        ; label : Ident.Label.t
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
    | Match of
        { cases : case Nonempty_list.t
        ; tree : tree
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | External of
        { symbol : string
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Extcall of
        { symbol : string
        ; arg : t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
  [@@deriving sexp_of]

  let ty : t -> Ty.t = function
    | Scalar { ty; _ }
    | Fun { ty; _ }
    | Lambda { ty; _ }
    | Apply { ty; _ }
    | Let { ty; _ }
    | Tuple { ty; _ }
    | If { ty; _ }
    | Match { ty; _ }
    | Var { ty; _ }
    | Binder { ty; _ }
    | Specialize { ty; _ }
    | External { ty; _ }
    | Tuple_get { ty; _ }
    | Constructor { ty; _ }
    | Inject { ty; _ }
    | Payload_get { ty; _ }
    | Tag_test { ty; _ }
    | Extcall { ty; _ } -> ty
  ;;

  let loc : t -> Lex.Location.t = function
    | Scalar { loc; _ }
    | Fun { loc; _ }
    | Lambda { loc; _ }
    | Apply { loc; _ }
    | Let { loc; _ }
    | Tuple { loc; _ }
    | If { loc; _ }
    | Match { loc; _ }
    | Var { loc; _ }
    | Binder { loc; _ }
    | Specialize { loc; _ }
    | External { loc; _ }
    | Tuple_get { loc; _ }
    | Constructor { loc; _ }
    | Inject { loc; _ }
    | Payload_get { loc; _ }
    | Tag_test { loc; _ }
    | Extcall { loc; _ } -> loc
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
    | Match expr -> Match { expr with ty }
    | Var expr -> Var { expr with ty }
    | Binder expr -> Binder { expr with ty }
    | Specialize expr -> Specialize { expr with ty }
    | External expr -> External { expr with ty }
    | Tuple_get expr -> Tuple_get { expr with ty }
    | Constructor expr -> Constructor { expr with ty }
    | Inject expr -> Inject { expr with ty }
    | Payload_get expr -> Payload_get { expr with ty }
    | Tag_test expr -> Tag_test { expr with ty }
    | Extcall expr -> Extcall { expr with ty }
  ;;

  let rec free_vars = function
    | Scalar _ | External _ | Inject _ -> Ident.Set.empty
    | Extcall { arg; _ } -> free_vars arg
    | Constructor { payload; _ } -> Option.value_map payload ~default:Ident.Set.empty ~f:free_vars
    | Var { id; _ } -> Ident.Set.singleton id
    | Tuple { elts; _ } ->
      Nonempty_list.fold elts ~init:Ident.Set.empty ~f:(fun acc elt ->
        Set.union acc (free_vars elt))
    | Apply { fn; arg; _ } | Specialize { fn; arg; _ } -> Set.union (free_vars fn) (free_vars arg)
    | Tuple_get { tuple; _ } -> free_vars tuple
    | Payload_get { variant; _ } | Tag_test { variant; _ } -> free_vars variant
    | If { cond; then_; else_; _ } ->
      Ident.Set.union_list [ free_vars cond; free_vars then_; free_vars else_ ]
    | Match { cases; tree; _ } ->
      let cases =
        Array.of_list_map (Nonempty_list.to_list cases) ~f:(fun { bindings; body } ->
          Map.fold bindings ~init:(free_vars body) ~f:(fun ~key ~data:_ acc -> Set.remove acc key))
      in
      free_vars_switch tree cases
    | Lambda { arg; body; _ } -> Set.remove (free_vars body) arg
    | Let { var; bind; rest; _ } -> Set.union (free_vars bind) (Set.remove (free_vars rest) var)
    | Binder { arg; body; _ } ->
      Map.fold body ~init:Ident.Set.empty ~f:(fun ~key:_ ~data:body acc ->
        Set.union acc (Set.remove (free_vars body) arg))
    | Fun { funs; rest; _ } ->
      let fvs = Set.union (free_vars_funs funs) (free_vars rest) in
      Nonempty_list.fold funs ~init:fvs ~f:(fun acc fun_ ->
        match fun_ with
        | Lambda { var; _ } | Binder { var; _ } -> Set.remove acc var)

  and free_vars_funs funs =
    Nonempty_list.fold funs ~init:Ident.Set.empty ~f:(fun acc -> function
      | Lambda { arg; body; _ } -> Set.union acc (Set.remove (free_vars body) arg)
      | Binder { arg; body; _ } ->
        let fvs =
          Map.fold body ~init:Ident.Set.empty ~f:(fun ~key:_ ~data:body acc ->
            Set.union acc (Set.remove (free_vars body) arg))
        in
        Set.union acc fvs)

  and free_vars_switch tree cases =
    match tree with
    | Leaf { case; bindings } ->
      Map.fold bindings ~init:cases.(case) ~f:(fun ~key:_ ~data:body acc ->
        Set.union acc (free_vars body))
    | Split { cond; then_; else_ } ->
      Ident.Set.union_list
        [ free_vars cond; free_vars_switch then_ cases; free_vars_switch else_ cases ]
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
        ; loc : Lex.Location.t
        }
    | External of
        { var : Ident.t
        ; symbol : string
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
  [@@deriving sexp_of]
end

module Program = struct
  type t =
    { top_levels : Top_level.t list
    ; stamp : int
    }
  [@@deriving sexp_of]
end
