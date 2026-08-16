open! Core
open Sst
module Key = Tst.Value.Concrete

let rec merge_ty (ty1 : Ty.t) (ty2 : Ty.t) : Ty.t =
  match ty1, ty2 with
  | Unit, Unit -> Unit
  | Bool, Bool -> Bool
  | Int, Int -> Int
  | Type, Type -> Type
  | Arrow a, Arrow b ->
    Arrow { arg_ty = merge_ty a.arg_ty b.arg_ty; ret_ty = merge_ty a.ret_ty b.ret_ty }
  | Pi a, Pi b -> Pi { arg_ty = merge_ty a.arg_ty b.arg_ty; ret_ty = merge_map a.ret_ty b.ret_ty }
  | Arrow a, Pi b ->
    Pi
      { arg_ty = merge_ty a.arg_ty b.arg_ty
      ; ret_ty = Map.map b.ret_ty ~f:(fun ty -> merge_ty a.ret_ty ty)
      }
  | Pi a, Arrow b ->
    Pi
      { arg_ty = merge_ty a.arg_ty b.arg_ty
      ; ret_ty = Map.map a.ret_ty ~f:(fun ty -> merge_ty b.ret_ty ty)
      }
  | Tuple a, Tuple b -> Tuple (Nonempty_list.map2_exn a b ~f:merge_ty)
  | Variant a_ctors, Variant b_ctors ->
    Variant
      (Map.merge a_ctors b_ctors ~f:(fun ~key:_ -> function
         | `Both (None, None) -> Some None
         | `Both (Some a, Some b) -> Some (Some (merge_ty a b))
         | `Both (Some _, None) | `Both (None, Some _) | `Left _ | `Right _ ->
           raise_s [%message "Bug: cannot merge types" (ty1 : Ty.t) (ty2 : Ty.t)]))
  | (Unit | Bool | Int | Type | Arrow _ | Pi _ | Tuple _ | Variant _), _ ->
    raise_s [%message "Bug: cannot merge types" (ty1 : Ty.t) (ty2 : Ty.t)]

and merge_map = Map.merge_skewed ~combine:(fun ~key:_ -> merge_ty)

let rec simplify_ty (ty : Tst.Value.t) : Ty.t =
  match ty with
  | Type Unit -> Unit
  | Type Bool -> Bool
  | Type Int -> Int
  | Type Type -> Type
  | Type (Arrow { arg_ty; ret_ty; _ }) ->
    Arrow { arg_ty = simplify_ty arg_ty; ret_ty = simplify_ty ret_ty }
  | Type (Pi { arg_ty; ret_ty; _ }) ->
    Pi { arg_ty = simplify_ty arg_ty; ret_ty = simplify_dependent ret_ty }
  | Type (Tuple elts) -> Tuple (Nonempty_list.map elts ~f:simplify_ty)
  | Type (Variant constructors) -> Variant (Map.map constructors ~f:(Option.map ~f:simplify_ty))
  | Unit
  | Bool _
  | Int _
  | Closure _
  | Binder _
  | Var _
  | Apply _
  | Proj _
  | Payload _
  | Match _
  | Refine _
  | External _
  | Prim _
  | Tuple _
  | Inject _
  | Constructor _
  | Bottom -> raise_s [%message "Bug: expected resolved type" (ty : Tst.Value.t)]

and simplify_dependent (ty : Tst.Dependent.t) =
  match ty with
  | T { memo; _ } | Typecheck { memo; _ } | Reduce { memo; _ } ->
    Key.Map.of_hashtbl_exn memo |> Map.map ~f:simplify_ty
  | Meet (a, b) | Join (a, b) ->
    let a = simplify_dependent a in
    let b = simplify_dependent b in
    merge_map a b
;;

let simplify_arrow (ty : Tst.Value.t) : Ty.t * Ty.t =
  match ty with
  | Type (Arrow { arg_ty; ret_ty; _ }) -> simplify_ty arg_ty, simplify_ty ret_ty
  | _ -> raise_s [%message "Bug: expected arrow" (ty : Tst.Value.t)]
;;

let simplify_pi (ty : Tst.Value.t) : Ty.t * _ =
  match ty with
  | Type (Pi { arg_ty; ret_ty; _ }) -> simplify_ty arg_ty, simplify_dependent ret_ty
  | _ -> raise_s [%message "Bug: expected arrow" (ty : Tst.Value.t)]
;;

let simplify_bool ~loc (bool : Tst.Bool.t) : bool =
  match bool with
  | T bool -> bool
  | _ -> raise_s [%message "Bug: expected concrete bool" (bool : Tst.Bool.t) (loc : Lex.Location.t)]
;;

let simplify_int ~loc (int : Tst.Int.t) : int64 =
  match int with
  | T int -> int
  | _ -> raise_s [%message "Bug: expected concrete int" (int : Tst.Int.t) (loc : Lex.Location.t)]
;;

(* Pure expressions may be dropped (an inlined application no longer evaluates them) or
   duplicated across a fold. Closure creation allocates but the allocation is unobservable. *)
let rec is_pure (expr : Expr.t) : bool =
  match expr with
  | Scalar _ | Var _ | External _ | Inject _ | Lambda _ | Binder _ -> true
  | Tuple { elts; _ } -> Nonempty_list.for_all elts ~f:is_pure
  | Constructor { payload; _ } -> Option.for_all payload ~f:is_pure
  | Tuple_get { tuple; _ } -> is_pure tuple
  | Payload_get { variant; _ } | Tag_test { variant; _ } -> is_pure variant
  | Fun _ | Apply _ | Specialize _ | Let _ | If _ | Match _ | Extcall _ -> false
;;

let rec to_value (expr : Expr.t) : Tst.Value.t option =
  match expr with
  | Scalar { value = Unit; _ } -> Some Tst.Value.unit
  | Scalar { value = Bool b; _ } -> Some (Tst.Bool.const b)
  | Scalar { value = Int i; _ } -> Some (Tst.Int.const i)
  | Tuple { elts; _ } ->
    Nonempty_list.to_list elts
    |> List.map ~f:to_value
    |> Option.all
    |> Option.map ~f:(fun elts -> Tst.Value.tuple (Nonempty_list.of_list_exn elts))
  | _ -> None
;;

let of_value ~loc (value : Tst.Value.t) : Expr.t option =
  match value with
  | Unit -> Some (Scalar { value = Unit; ty = Unit; loc })
  | Bool (T b) -> Some (Scalar { value = Bool b; ty = Bool; loc })
  | Int (T i) -> Some (Scalar { value = Int i; ty = Int; loc })
  | _ -> None
;;

(* A failing prim (division by zero, false assert) must keep aborting at runtime, so on
   [Builtin.Error] the application is left in place. *)
let fold_prim ~loc (prim : Builtin.Prim.t) (arg : Expr.t) : Expr.t option =
  match to_value arg with
  | None -> None
  | Some arg ->
    (match Builtin.eval prim arg with
     | value -> of_value ~loc value
     | exception Builtin.Error _ -> None)
;;

(* Bindings with statically known contents: primitive operators (bound by the prelude's
   [builtin] declarations, so applications see them through a [Var]) and scalar constants,
   which propagate into the folds above. *)
module Env = struct
  type entry =
    | Const of Scalar.t * Ty.t
    | Prim of Builtin.Prim.t

  type t = entry Ident.Map.t

  let empty : t = Ident.Map.empty

  let entry (env : t) (expr : Expr.t) : entry option =
    match expr with
    | Scalar { value; ty; _ } -> Some (Const (value, ty))
    | External { symbol; _ } ->
      (match Builtin0.find symbol with
       | Some (Prim prim) -> Some (Prim prim)
       | Some (Type _) | None -> None)
    | Var { id; _ } -> Map.find env id
    | _ -> None
  ;;

  let bind (env : t) var expr =
    match entry env expr with
    | Some entry -> Map.set env ~key:var ~data:entry
    | None -> env
  ;;

  let prim (env : t) (fn : Expr.t) : Builtin.Prim.t option =
    match entry env fn with
    | Some (Prim prim) -> Some prim
    | Some (Const _) | None -> None
  ;;
end

(* Folding can leave a binding without uses; drop it if evaluating it is unobservable. *)
let make_let ~loc var ~bind ~rest : Expr.t =
  if is_pure bind && not (Set.mem (Expr.free_vars rest) var)
  then rest
  else Let { var; bind; rest; ty = Expr.ty rest; loc }
;;

let rec simplify_value ~loc ~(ty : Tst.Value.t) (value : Tst.Value.t) : Expr.t =
  match value with
  | Unit -> Scalar { value = Unit; ty = Unit; loc }
  | Bool b -> Scalar { value = Bool (simplify_bool ~loc b); ty = Bool; loc }
  | Int i -> Scalar { value = Int (simplify_int ~loc i); ty = Int; loc }
  | Type _ -> Scalar { value = Type; ty = Type; loc }
  | Closure _ | Binder _ ->
    raise_s [%message "Bug: unreified function value" (value : Tst.Value.t) (loc : Lex.Location.t)]
  | Prim prim ->
    let desc = Builtin.desc (Prim prim) in
    External { symbol = Builtin.Prim.symbol prim; ty = simplify_ty desc.ty; loc }
  | External { symbol; ty; _ } -> External { symbol; ty = simplify_ty ty; loc }
  | Tuple elts ->
    (match ty with
     | Type (Tuple elt_tys) when Nonempty_list.length elt_tys = Nonempty_list.length elts ->
       let elts =
         Nonempty_list.map2_exn elts elt_tys ~f:(fun elt ty -> simplify_value ~loc ~ty elt)
       in
       Tuple { elts; ty = simplify_ty ty; loc }
     | _ -> raise_s [%message "Bug: expected tuple type" (ty : Tst.Value.t) (loc : Lex.Location.t)])
  | Inject { label; ty = _ } -> Inject { label; ty = simplify_ty ty; loc }
  | Constructor { label; payload } ->
    (match ty with
     | Type (Variant constructors) ->
       let payload =
         match payload, Map.find constructors label with
         | None, Some None -> None
         | Some payload, Some (Some payload_ty) -> Some (simplify_value ~loc ~ty:payload_ty payload)
         | _ ->
           raise_s
             [%message
               "Bug: constructor does not match its variant type"
                 (value : Tst.Value.t)
                 (ty : Tst.Value.t)
                 (loc : Lex.Location.t)]
       in
       Constructor { label; payload; ty = simplify_ty ty; loc }
     | _ ->
       raise_s [%message "Bug: expected variant type" (ty : Tst.Value.t) (loc : Lex.Location.t)])
  | Bottom -> raise_s [%message "Bug: emitted unreachable branch" (loc : Lex.Location.t)]
  | Var _ | Apply _ | Proj _ | Payload _ | Match _ | Refine _ ->
    raise_s [%message "Bug: expected runtime value" (value : Tst.Value.t) (loc : Lex.Location.t)]

and simplify_expr env (expr : Tst.Expr.t) : Expr.t =
  match expr with
  | Erased { loc; _ } -> Scalar { value = Unit; ty = Unit; loc }
  | Literal { value; ty; loc; _ } -> simplify_value ~loc ~ty value
  | Var { id; ty; loc; _ } ->
    (match Map.find env id with
     | Some (Env.Const (value, ty)) -> Scalar { value; ty; loc }
     | Some (Prim _) | None -> Var { id; ty = simplify_ty ty; loc })
  | Let { var; bind; rest; loc; _ } ->
    if Modes.is_erased (Tst.Expr.mode bind)
    then simplify_expr env rest
    else (
      let bind = simplify_expr env bind in
      let rest = simplify_expr (Env.bind env var bind) rest in
      make_let ~loc var ~bind ~rest)
  | Tuple { elts; ty; loc; _ } ->
    let elts = Nonempty_list.map elts ~f:(simplify_expr env) in
    Tuple { elts; ty = simplify_ty ty; loc }
  | Tuple_get { tuple; index; ty; loc; _ } ->
    let tuple = simplify_expr env tuple in
    (match tuple with
     | Tuple { elts; _ } when Nonempty_list.for_all elts ~f:is_pure ->
       Nonempty_list.nth_exn elts index
     | _ -> Tuple_get { tuple; index; ty = simplify_ty ty; loc })
  | Payload_get { variant; label; ty; loc; _ } ->
    let variant = simplify_expr env variant in
    (match variant with
     | Constructor { label = actual; payload = Some payload; _ } when Ident.Label.equal actual label
       -> payload
     | _ -> Payload_get { variant; label; ty = simplify_ty ty; loc })
  | Tag_test { variant; label; ty; loc; _ } ->
    let variant = simplify_expr env variant in
    (match variant with
     | Constructor { label = actual; payload; _ } when Option.for_all payload ~f:is_pure ->
       Scalar { value = Bool (Ident.Label.equal actual label); ty = Bool; loc }
     | _ -> Tag_test { variant; label; ty = simplify_ty ty; loc })
  | Builtin { builtin; loc; ty; _ } ->
    (match builtin with
     | Prim prim -> External { symbol = Builtin.Prim.symbol prim; ty = simplify_ty ty; loc }
     | Type ty -> raise_s [%message "Bug: unerased type" (ty : Builtin0.Type.t)])
  | Extcall { symbol; arg; ty; loc; _ } ->
    Extcall { symbol; arg = simplify_expr env arg; ty = simplify_ty ty; loc }
  | Apply { fn = Lambda { arg = var; body; _ }; arg; loc; _ } ->
    let bind = simplify_expr env arg in
    let rest = simplify_expr (Env.bind env var bind) body in
    make_let ~loc var ~bind ~rest
  | Apply { fn; arg; ty; loc; _ } ->
    let fn = simplify_expr env fn in
    let arg = simplify_expr env arg in
    let folded =
      match Env.prim env fn with
      | Some prim -> fold_prim ~loc prim arg
      | None -> None
    in
    (match fn, folded with
     | _, Some folded -> folded
     | Inject { label; _ }, None ->
       Constructor { label; payload = Some arg; ty = simplify_ty ty; loc }
     | Lambda { arg = var; body; _ }, None -> make_let ~loc var ~bind:arg ~rest:body
     | _, None -> Apply { fn; arg; ty = simplify_ty ty; loc })
  | Lambda { arg; body; ty; family; loc; _ } ->
    let arg_ty, ret_ty = simplify_arrow ty in
    let body = simplify_expr env body in
    Lambda { arg; body; ty = Arrow { arg_ty; ret_ty }; family; loc }
  | Fun { funs; rest; loc; _ } ->
    let funs = simplify_funs env funs in
    (match Nonempty_list.of_list funs with
     | Some funs ->
       let rest = simplify_expr env rest in
       Fun { funs; rest; ty = Expr.ty rest; loc }
     | None -> simplify_expr env rest)
  | If { cond; then_; else_; ty; loc; _ } ->
    let cond = simplify_expr env cond in
    (match cond with
     | Scalar { value = Bool true; _ } -> simplify_expr env then_
     | Scalar { value = Bool false; _ } -> simplify_expr env else_
     | _ ->
       let then_ = simplify_expr env then_ in
       let else_ = simplify_expr env else_ in
       If { cond; then_; else_; ty = simplify_ty ty; loc })
  | Match { cases; tree; ty; loc; _ } ->
    let cases =
      Nonempty_list.map cases ~f:(fun { body; bindings } ->
        let body = simplify_expr env body in
        let bindings = Map.map bindings ~f:simplify_ty in
        { Expr.body; bindings })
    in
    let tree = simplify_switch env (Nonempty_list.to_array cases) tree in
    Match { cases; tree; ty = simplify_ty ty; loc }
  | Binder { arg; ty; family; body; loc; _ } ->
    let arg_ty, ret_ty = simplify_pi ty in
    let body = simplify_monos env body in
    Binder { arg; body; ty = Pi { arg_ty; ret_ty }; family; loc }
  | Specialize { fn = Lambda { arg = var; body; _ }; arg; key = None; loc; _ } ->
    let bind = simplify_expr env arg in
    let rest = simplify_expr (Env.bind env var bind) body in
    make_let ~loc var ~bind ~rest
  | Specialize { fn; arg; target; key; ty; loc; _ } ->
    let arg = simplify_expr env arg in
    let mono =
      match fn, key with
      | Binder { body; _ }, Some key when is_pure arg -> Map.find body key
      | _ -> None
    in
    (match mono with
     | Some body -> simplify_expr env body
     | None ->
       let fn = simplify_expr env fn in
       let target : Expr.target =
         match target with
         | Family family -> Family family
         | Prim prim -> Prim prim
       in
       let specialize () : Expr.t = Specialize { fn; arg; key; ty = simplify_ty ty; target; loc } in
       (match target, key, fn with
        | Prim prim, _, fn when is_pure fn ->
          (match fold_prim ~loc prim arg with
           | Some folded -> folded
           | None -> specialize ())
        | Family _, None, Lambda { arg = var; body; _ } -> make_let ~loc var ~bind:arg ~rest:body
        | Family _, Some key, Binder { body; _ } when is_pure arg ->
          (match Map.find body key with
           | Some body -> body
           | None -> specialize ())
        | _ -> specialize ()))

and simplify_monos env body = Map.map body ~f:(simplify_expr env)

and simplify_switch env cases (tree : Tst.Expr.tree) : Expr.tree =
  match tree with
  | Leaf { case; bindings } ->
    let bindings = Map.map bindings ~f:(simplify_expr env) in
    Leaf { case; bindings }
  | Split { cond; then_; else_ } ->
    let cond = simplify_expr env cond in
    (match cond with
     | Scalar { value = Bool true; _ } -> simplify_switch env cases then_
     | Scalar { value = Bool false; _ } -> simplify_switch env cases else_
     | _ ->
       let then_ = simplify_switch env cases then_ in
       let else_ = simplify_switch env cases else_ in
       Split { cond; then_; else_ })

and simplify_funs env (funs : Tst.Expr.fun_ Nonempty_list.t) : Expr.fun_ list =
  Nonempty_list.filter_map funs ~f:(fun (fun_ : Tst.Expr.fun_) : Expr.fun_ option ->
    match fun_ with
    | Lambda { var; arg; body; ty; mode; family; loc; _ } ->
      if Modes.is_erased mode
      then None
      else (
        let arg_ty, ret_ty = simplify_arrow ty in
        let body = simplify_expr env body in
        Some (Expr.Lambda { var; arg; body; ty = Arrow { arg_ty; ret_ty }; family; loc }))
    | Binder { var; arg; ty; mode; family; body; loc; _ } ->
      if Modes.is_erased mode
      then None
      else (
        let arg_ty, ret_ty = simplify_pi ty in
        let body = simplify_monos env body in
        Some (Expr.Binder { var; arg; body; ty = Pi { arg_ty; ret_ty }; family; loc })))
;;

let simplify_top_level env (tst : Tst.Top_level.t) : Env.t * Top_level.t Option.t =
  match tst with
  | Erased _ -> env, None
  | Let { var; bind; loc } ->
    if Modes.is_erased (Tst.Expr.mode bind)
    then env, None
    else (
      let bind = simplify_expr env bind in
      Env.bind env var bind, Some (Let { var; bind; loc }))
  | Fun { funs; loc } ->
    let funs = simplify_funs env funs in
    (match Nonempty_list.of_list funs with
     | Some funs -> env, Some (Fun { funs; loc })
     | None -> env, None)
  | External { var; symbol; ty; mode; loc } ->
    if Modes.is_erased mode
    then env, None
    else env, Some (External { var; symbol; ty = simplify_ty ty; loc })
  | Builtin { var; builtin; ty; mode; loc } ->
    if Modes.is_erased mode
    then env, None
    else (
      match builtin with
      | Prim prim ->
        ( Map.set env ~key:var ~data:(Env.Prim prim)
        , Some (External { var; symbol = Builtin.Prim.symbol prim; ty = simplify_ty ty; loc }) )
      | Type ty -> raise_s [%message "Bug: unerased type" (ty : Builtin0.Type.t)])
;;

let simplify (tst : Tst.Program.t) : Program.t =
  let _, top_levels = List.fold_map tst.top_levels ~init:Env.empty ~f:simplify_top_level in
  { top_levels = List.filter_opt top_levels; stamp = tst.stamp }
;;
