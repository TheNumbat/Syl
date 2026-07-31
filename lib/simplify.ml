open! Core
open Sst
module Key = Tst.Value.Concrete

(* TODO constant folding *)

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
  | Bottom | Var _ | Apply _ | Proj _ | Payload _ | Match _ ->
    raise_s [%message "Bug: expected runtime value" (value : Tst.Value.t) (loc : Lex.Location.t)]

and simplify_expr (expr : Tst.Expr.t) : Expr.t =
  match expr with
  | Erased { loc; _ } -> Scalar { value = Unit; ty = Unit; loc }
  | Literal { value; ty; loc; _ } -> simplify_value ~loc ~ty value
  | Var { id; ty; loc; _ } -> Var { id; ty = simplify_ty ty; loc }
  | Let { var; bind; rest; loc; _ } ->
    if Modes.is_erased (Tst.Expr.mode bind)
    then simplify_expr rest
    else (
      let bind = simplify_expr bind in
      let rest = simplify_expr rest in
      Let { var; bind; rest; ty = Expr.ty rest; loc })
  | Tuple { elts; ty; loc; _ } ->
    let elts = Nonempty_list.map elts ~f:simplify_expr in
    Tuple { elts; ty = simplify_ty ty; loc }
  | Tuple_get { tuple; index; ty; loc; _ } ->
    Tuple_get { tuple = simplify_expr tuple; index; ty = simplify_ty ty; loc }
  | Payload_get { variant; label; ty; loc; _ } ->
    Payload_get { variant = simplify_expr variant; label; ty = simplify_ty ty; loc }
  | Tag_test { variant; label; ty; loc; _ } ->
    Tag_test { variant = simplify_expr variant; label; ty = simplify_ty ty; loc }
  | Builtin { builtin; loc; ty; _ } ->
    (match builtin with
     | Prim prim -> External { symbol = Builtin.Prim.symbol prim; ty = simplify_ty ty; loc }
     | Type ty -> raise_s [%message "Bug: unerased type" (ty : Builtin0.Type.t)])
  | Extcall { symbol; arg; ty; loc; _ } ->
    Extcall { symbol; arg = simplify_expr arg; ty = simplify_ty ty; loc }
  | Apply { fn; arg; ty; loc; _ } ->
    (* TODO inline if value *)
    let fn = simplify_expr fn in
    let arg = simplify_expr arg in
    (match fn with
     | Inject { label; _ } -> Constructor { label; payload = Some arg; ty = simplify_ty ty; loc }
     | _ -> Apply { fn; arg; ty = simplify_ty ty; loc })
  | Lambda { arg; body; ty; family; loc; _ } ->
    let arg_ty, ret_ty = simplify_arrow ty in
    let body = simplify_expr body in
    Lambda { arg; body; ty = Arrow { arg_ty; ret_ty }; family; loc }
  | Fun { funs; rest; loc; _ } ->
    let funs = simplify_funs funs in
    (match Nonempty_list.of_list funs with
     | Some funs ->
       let rest = simplify_expr rest in
       Fun { funs; rest; ty = Expr.ty rest; loc }
     | None -> simplify_expr rest)
  | If { cond; then_; else_; ty; loc; _ } ->
    let cond = simplify_expr cond in
    let then_ = simplify_expr then_ in
    let else_ = simplify_expr else_ in
    If { cond; then_; else_; ty = simplify_ty ty; loc }
  | Match { cases; tree; ty; loc; _ } ->
    let cases =
      Nonempty_list.map cases ~f:(fun { body; bindings } ->
        let body = simplify_expr body in
        let bindings = Map.map bindings ~f:simplify_ty in
        { Expr.body; bindings })
    in
    let tree = simplify_switch (Nonempty_list.to_array cases) tree in
    Match { cases; tree; ty = simplify_ty ty; loc }
  | Binder { arg; ty; family; body; loc; _ } ->
    let arg_ty, ret_ty = simplify_pi ty in
    let body = simplify_monos body in
    Binder { arg; body; ty = Pi { arg_ty; ret_ty }; family; loc }
  | Specialize { fn; arg; target; key; ty; loc; _ } ->
    (* TODO inline if value *)
    let fn = simplify_expr fn in
    let arg = simplify_expr arg in
    let target : Expr.target =
      match target with
      | Family family -> Family family
      | Prim prim -> Prim prim
    in
    Specialize { fn; arg; key; ty = simplify_ty ty; target; loc }

and simplify_monos body = Map.map body ~f:simplify_expr

and simplify_switch cases (tree : Tst.Expr.tree) : Expr.tree =
  match tree with
  | Leaf { case; bindings } ->
    let bindings = Map.map bindings ~f:simplify_expr in
    Leaf { case; bindings }
  | Split { cond; then_; else_ } ->
    let cond = simplify_expr cond in
    let then_ = simplify_switch cases then_ in
    let else_ = simplify_switch cases else_ in
    Split { cond; then_; else_ }

and simplify_funs (funs : Tst.Expr.fun_ Nonempty_list.t) : Expr.fun_ list =
  Nonempty_list.filter_map funs ~f:(fun (fun_ : Tst.Expr.fun_) : Expr.fun_ option ->
    match fun_ with
    | Lambda { var; arg; body; ty; mode; family; loc; _ } ->
      if Modes.is_erased mode
      then None
      else (
        let arg_ty, ret_ty = simplify_arrow ty in
        let body = simplify_expr body in
        Some (Expr.Lambda { var; arg; body; ty = Arrow { arg_ty; ret_ty }; family; loc }))
    | Binder { var; arg; ty; mode; family; body; loc; _ } ->
      if Modes.is_erased mode
      then None
      else (
        let arg_ty, ret_ty = simplify_pi ty in
        let body = simplify_monos body in
        Some (Expr.Binder { var; arg; body; ty = Pi { arg_ty; ret_ty }; family; loc })))
;;

let simplify_top_level (tst : Tst.Top_level.t) : Top_level.t Option.t =
  match tst with
  | Erased _ -> None
  | Let { var; bind; loc } ->
    if Modes.is_erased (Tst.Expr.mode bind)
    then None
    else Some (Let { var; bind = simplify_expr bind; loc })
  | Fun { funs; loc } ->
    let funs = simplify_funs funs in
    (match Nonempty_list.of_list funs with
     | Some funs -> Some (Fun { funs; loc })
     | None -> None)
  | External { var; symbol; ty; mode; loc } ->
    if Modes.is_erased mode then None else Some (External { var; symbol; ty = simplify_ty ty; loc })
  | Builtin { var; builtin; ty; mode; loc } ->
    if Modes.is_erased mode
    then None
    else (
      match builtin with
      | Prim prim ->
        Some (External { var; symbol = Builtin.Prim.symbol prim; ty = simplify_ty ty; loc })
      | Type ty -> raise_s [%message "Bug: unerased type" (ty : Builtin0.Type.t)])
;;

let simplify (tst : Tst.Program.t) : Program.t =
  { top_levels = List.filter_map tst.top_levels ~f:simplify_top_level; stamp = tst.stamp }
;;
