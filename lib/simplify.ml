open! Core
open Sst

module Env = struct
  type t = Ty.t Ident.Map.t

  let initial = Ident.Map.empty
  let bind t id ty = Map.set t ~key:id ~data:ty
  let find t id = Map.find_exn t id
end

let erased expr = Modes.is_erased (Tst.Expr.mode expr)

let rec simplify_ty ~loc (ty : Tst.Value.t) : Ty.t =
  match ty with
  | Type Unit -> Unit
  | Type Bool -> Bool
  | Type Int -> Int
  | Type Type -> Unit
  | Type (Arrow { arg_ty; ret_ty; _ }) ->
    Arrow { arg_ty = simplify_ty ~loc arg_ty; ret_ty = simplify_ty ~loc ret_ty }
  | Type (Tuple elts) -> Tuple (List.map elts ~f:(simplify_ty ~loc))
  | Bottom
  | Unit
  | Bool _
  | Int _
  | Closure _
  | Binder _
  | Var _
  | If _
  | Apply _
  | External _
  | Prim _
  | Tuple _
  | Type (Pi _) ->
    raise_s [%message "Bug: cannot simplify type" (ty : Tst.Value.t) (loc : Lex.Location.t)]
;;

let simplify_arrow ~loc (ty : Tst.Value.t) : Ty.t * Ty.t =
  match ty with
  | Type (Arrow { arg_ty; ret_ty; _ }) -> simplify_ty ~loc arg_ty, simplify_ty ~loc ret_ty
  | _ -> raise_s [%message "Bug: expected arrow" (ty : Tst.Value.t) (loc : Lex.Location.t)]
;;

let simplify_bool ~loc (bool : Tst.Bool.t) : bool =
  match bool with
  | T bool -> bool
  | _ -> raise_s [%message "Bug: cannot simplify bool" (bool : Tst.Bool.t) (loc : Lex.Location.t)]
;;

let simplify_int ~loc (int : Tst.Int.t) : int64 =
  match int with
  | T int -> int
  | _ -> raise_s [%message "Bug: cannot simplify int" (int : Tst.Int.t) (loc : Lex.Location.t)]
;;

let rec union_ty (ty1 : Ty.t) (ty2 : Ty.t) : Ty.t =
  match ty1, ty2 with
  | Unit, Unit -> Unit
  | Bool, Bool -> Bool
  | Int, Int -> Int
  | Arrow a, Arrow b ->
    Arrow { arg_ty = union_ty a.arg_ty b.arg_ty; ret_ty = union_ty a.ret_ty b.ret_ty }
  | Pack a, Pack b ->
    let merge = Hashtbl.copy a in
    Hashtbl.iteri b ~f:(fun ~key ~data:ty1 ->
      Hashtbl.update merge key ~f:(function
        | Some ty2 -> union_ty ty1 ty2
        | None -> ty1));
    Pack merge
  | Tuple a, Tuple b -> Tuple (List.map2_exn a b ~f:union_ty)
  | (Unit | Bool | Int | Arrow _ | Pack _ | Tuple _), _ ->
    raise_s [%message "Bug: cannot merge types" (ty1 : Ty.t) (ty2 : Ty.t)]
;;

let collect_free_keys env free_keys =
  Map.mapi free_keys ~f:(fun ~key:id ~data:keys ->
    match Env.find env id with
    | Ty.Pack pack ->
      let narrow = Hashtbl.create (module Tst.Value.Concrete) in
      Set.iter keys ~f:(fun key -> Hashtbl.add_exn narrow ~key ~data:(Hashtbl.find_exn pack key));
      Ty.Pack narrow
    | ty -> ty)
;;

let free_vars env arg body =
  let free_keys = Map.remove (Expr.free_keys body) arg in
  collect_free_keys env free_keys
;;

let rec simplify_value ~loc env (value : Tst.Value.t) : Expr.t =
  match value with
  | Unit -> Scalar { value = Unit; ty = Unit; loc }
  | Bool b -> Scalar { value = Bool (simplify_bool ~loc b); ty = Bool; loc }
  | Int i -> Scalar { value = Int (simplify_int ~loc i); ty = Int; loc }
  | Closure { arg; body; ty; _ } ->
    let arg_ty, ret_ty = simplify_arrow ~loc ty in
    let body = simplify (Env.bind env arg arg_ty) body in
    let fvs = free_vars env arg body in
    Lambda { arg; body; fvs; ty = Arrow { arg_ty; ret_ty }; loc }
  | Binder b ->
    let pack, fvs, ty = simplify_mono ~loc env b.mono in
    Pack { pack; fvs; ty; loc }
  | External { symbol; ty; _ } -> External { symbol; ty = simplify_ty ~loc ty; loc }
  | Prim prim ->
    let desc = Builtin.desc (Prim prim) in
    External { symbol = Builtin.Prim.symbol prim; ty = simplify_ty ~loc desc.ty; loc }
  | Tuple elts ->
    let elts = List.map elts ~f:(simplify_value ~loc env) in
    Tuple { elts; ty = Tuple (List.map elts ~f:Expr.ty); loc }
  | Bottom | Type _ | Var _ | If _ | Apply _ ->
    raise_s [%message "Bug: cannot simplify literal" (value : Tst.Value.t) (loc : Lex.Location.t)]

and simplify_mono ~loc env mono =
  let pack =
    Hashtbl.map mono ~f:(fun { Tst.Binder.Mono.arg; arg_mode; arg_desc; body; _ } ->
      let body =
        if Modes.is_erased arg_mode
        then simplify env body
        else (
          let arg_val = simplify_value env ~loc (Lazy.force arg_desc.static) in
          let body = simplify (Env.bind env arg (Expr.ty arg_val)) body in
          Expr.Let { var = arg; bind = arg_val; rest = body; ty = Expr.ty body; loc })
      in
      let fvs = free_vars env arg body in
      body, fvs)
  in
  let free_keys =
    Hashtbl.fold pack ~init:Ident.Map.empty ~f:(fun ~key:_ ~data:(body, _) acc ->
      Map.merge_skewed acc (Expr.free_keys body) ~combine:(fun ~key:_ -> Set.union))
  in
  let ty = Hashtbl.map pack ~f:(fun (expr, _) -> Expr.ty expr) in
  pack, collect_free_keys env free_keys, Ty.Pack ty

and simplify env (expr : Tst.Expr.t) : Expr.t =
  match expr with
  | Literal { value; loc; _ } -> simplify_value env ~loc value
  | Fun { funs; rest; loc; _ } ->
    let funs, fvs = simplify_funs env funs in
    (match Nonempty_list.of_list funs with
     | Some funs ->
       let env =
         Nonempty_list.fold funs ~init:env ~f:(fun env (fun_ : Expr.fun_) ->
           match fun_ with
           | Mono { var; ty; _ } | Pack { var; ty; _ } -> Env.bind env var ty)
       in
       let rest = simplify env rest in
       Fun { funs; fvs; rest; ty = Expr.ty rest; loc }
     | None -> simplify env rest)
  | Lambda { arg; body; ty; loc; _ } ->
    let arg_ty, ret_ty = simplify_arrow ~loc ty in
    let body = simplify (Env.bind env arg arg_ty) body in
    let fvs = free_vars env arg body in
    Lambda { arg; fvs; ty = Arrow { arg_ty; ret_ty }; body; loc }
  | Apply { fn; arg; loc; _ } ->
    let arg = simplify env arg in
    (match simplify env fn with
     | Lambda { arg = var; body; _ } -> Let { var; bind = arg; rest = body; ty = Expr.ty body; loc }
     | fn -> Apply { fn; arg; ty = Ty.ret (Expr.ty fn); loc })
  | Symbol { fn; key; loc; _ } ->
    (match simplify env fn with
     | Pack { pack; _ } -> fst (Hashtbl.find_exn pack key)
     | fn ->
       let ty = Ty.find (Expr.ty fn) key in
       let fn =
         Expr.with_ty fn (Pack (Hashtbl.of_alist_exn (module Tst.Value.Concrete) [ key, ty ]))
       in
       Symbol { fn; arg = key; ty; loc })
  | Let { var; bind; rest; loc; _ } ->
    if erased bind
    then simplify env rest
    else (
      let bind = simplify env bind in
      let rest = simplify (Env.bind env var (Expr.ty bind)) rest in
      Let { var; bind; rest; ty = Expr.ty rest; loc })
  | Tuple { elts; ty; loc; _ } ->
    let elts = List.map elts ~f:(simplify env) in
    Tuple { elts; ty = simplify_ty ~loc ty; loc }
  | If { cond; then_; else_; loc; _ } ->
    let cond = simplify env cond in
    (match cond with
     | Scalar { value = Bool b; _ } -> if b then simplify env then_ else simplify env else_
     | _ ->
       let then_ = simplify env then_ in
       let else_ = simplify env else_ in
       If { cond; then_; else_; ty = Expr.ty then_; loc })
  | Var { id; loc; _ } -> Var { id; ty = Env.find env id; loc }
  | Binder { mono; loc; _ } ->
    let pack, fvs, ty = simplify_mono ~loc env mono in
    Pack { pack; fvs; ty; loc }
  | Builtin { builtin; loc; ty; _ } ->
    (match builtin with
     | Type _ -> raise_s [%message "Bug: builtin type"]
     | Prim prim ->
       let ty = simplify_ty ~loc ty in
       External { symbol = Builtin.Prim.symbol prim; ty; loc })

and simplify_funs env (funs : Tst.Expr.fun_ Nonempty_list.t) =
  let rec mono_ty ~loc { Tst.Binder.Mono.body_desc; _ } =
    match body_desc.ty with
    | Type (Pi _) ->
      (match Lazy.force body_desc.static with
       | Binder { mono; _ } ->
         let ty = Hashtbl.map mono ~f:(mono_ty ~loc) in
         Ty.Pack ty
       | value ->
         raise_s [%message "Bug: expected binder" (value : Tst.Value.t) (loc : Lex.Location.t)])
    | _ -> simplify_ty ~loc body_desc.ty
  in
  let env =
    Nonempty_list.fold funs ~init:env ~f:(fun env (fun_ : Tst.Expr.fun_) ->
      match fun_ with
      | Lambda { var; ty; mode; loc; _ } ->
        if Modes.is_erased mode then env else Env.bind env var (simplify_ty ~loc ty)
      | Binder { var; mono; mode; loc; _ } ->
        if Modes.is_erased mode
        then env
        else (
          let ty = Hashtbl.map mono ~f:(mono_ty ~loc) in
          Env.bind env var (Pack ty)))
  in
  let funs =
    Nonempty_list.filter_map funs ~f:(function
      | Lambda { var; arg; body; ty; mode; loc } ->
        if Modes.is_erased mode
        then None
        else (
          let arg_ty, ret_ty = simplify_arrow ~loc ty in
          let body = simplify (Env.bind env arg arg_ty) body in
          let fvs = free_vars env arg body in
          Some (Expr.Mono { var; arg; fvs; body; ty = Arrow { arg_ty; ret_ty }; loc }))
      | Binder { var; mono; mode; loc; _ } ->
        if Modes.is_erased mode
        then None
        else (
          let pack, fvs, ty = simplify_mono ~loc env mono in
          Some (Pack { var; pack; fvs; ty; loc })))
  in
  let fvs =
    List.fold funs ~init:Ident.Map.empty ~f:(fun acc -> function
      | Expr.Mono { fvs; _ } | Pack { fvs; _ } ->
        Map.merge_skewed acc fvs ~combine:(fun ~key:_ -> union_ty))
  in
  funs, fvs
;;

let simplify_top_level env (tst : Tst.Top_level.t) : Env.t * Top_level.t Option.t =
  match tst with
  | Let { var; bind; loc } ->
    if erased bind
    then env, None
    else (
      let bind = simplify env bind in
      Env.bind env var (Expr.ty bind), Some (Let { var; bind; loc }))
  | Fun { funs; loc } ->
    let funs, fvs = simplify_funs env funs in
    (match Nonempty_list.of_list funs with
     | Some funs ->
       let env =
         Nonempty_list.fold funs ~init:env ~f:(fun env (fun_ : Expr.fun_) ->
           match fun_ with
           | Mono { var; ty; _ } | Pack { var; ty; _ } -> Env.bind env var ty)
       in
       env, Some (Fun { funs; fvs; loc })
     | None -> env, None)
  | External { var; symbol; ty; loc } ->
    let ty = simplify_ty ~loc ty in
    Env.bind env var ty, Some (External { var; symbol; ty; loc })
  | Builtin { var; builtin; ty; loc } ->
    (match builtin with
     | Type _ -> env, None
     | Prim prim ->
       let ty = simplify_ty ~loc ty in
       Env.bind env var ty, Some (External { var; symbol = Builtin.Prim.symbol prim; ty; loc }))
;;

let simplify (tst : Tst.Program.t) =
  let _, top_levels =
    List.fold tst ~init:(Env.initial, []) ~f:(fun (env, acc) top_level ->
      match simplify_top_level env top_level with
      | env, Some top_level -> env, top_level :: acc
      | env, None -> env, acc)
  in
  List.rev top_levels
;;
