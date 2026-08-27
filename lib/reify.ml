open! Core
open Tst

type t =
  { monos : Expr.t Hashcons.Tag.Map.t Ids.Family.Map.t
  ; fun_bindings : Ident.t Ids.Fn.Map.t
  ; resolve : loc:Lex.Location.t -> Value.t -> Value.t
  }

let family_monos t family = Map.find t.monos family |> Option.value ~default:Hashcons.Tag.Map.empty
let free_vars t expr = Expr.free_vars ~monos:(family_monos t) expr

let tuple_elt_tys ~loc (ty : Value.t) elts =
  match ty.node with
  | Value.Type (Tuple elt_tys) when Nonempty_list.length elt_tys = Nonempty_list.length elts ->
    elt_tys
  | _ ->
    raise_s
      [%message
        "Bug: quoted tuple's type is not a matching tuple type"
          (ty : Value.t)
          (loc : Lex.Location.t)]
;;

let payload_ty ~loc (desc : Tst.Desc.t) label =
  match desc.ty.node with
  | Type (Variant constructors) ->
    (match Map.find constructors label with
     | Some (Some payload_ty) -> payload_ty
     | Some None | None ->
       raise_s
         [%message "Bug: expected label with payload" (desc.ty : Value.t) (loc : Lex.Location.t)])
  | _ -> raise_s [%message "Bug: expected varaint" (desc.ty : Value.t) (loc : Lex.Location.t)]
;;

module Func = struct
  type t =
    | Closure of Closure.t
    | Binder of Binder.t

  let of_val ~resolve (desc : Desc.t) =
    match (resolve (Lazy.force desc.static) : Value.t).node with
    | Closure closure -> Some (Closure closure)
    | Binder binder -> Some (Binder binder)
    | _ -> None
  ;;

  let env = function
    | Closure closure -> closure.env
    | Binder binder -> binder.env
  ;;

  let uid = function
    | Closure closure -> closure.uid
    | Binder binder -> binder.uid
  ;;

  let is_bound bound id fn =
    match Map.find bound (uid fn) with
    | Some bound_id -> Ident.equal bound_id id
    | None -> false
  ;;
end

let rec value_needs_reify (value : Value.t) =
  match value.node with
  | Closure _ | Binder _ | Box _ -> true
  | Tuple elts -> Nonempty_list.exists elts ~f:value_needs_reify
  | Constructor { payload; _ } -> Option.exists payload ~f:value_needs_reify
  | _ -> false
;;

let ref_content_ty t ~loc (ty : Value.t) =
  match ty.node with
  | Value.Type (Ref p) -> t.resolve ~loc p
  | _ ->
    raise_s [%message "Bug: boxed value at a non-ref type" (ty : Value.t) (loc : Lex.Location.t)]
;;

let rec reify_expr t bound (expr : Expr.t) : Expr.t =
  if Modes.is_erased (Expr.mode expr)
  then expr
  else (
    let expr = reify_expr' t bound expr in
    let ty = Expr.ty expr in
    let resolved = t.resolve ~loc:(Expr.loc expr) ty in
    if phys_equal resolved ty then expr else Expr.with_ty expr resolved)

and reify_expr' t bound (expr : Expr.t) : Expr.t =
  match expr with
  | Erased _ | Builtin _ | Var _ -> expr
  | Literal { value; ty; mode; loc } ->
    let value = t.resolve ~loc value in
    if value_needs_reify value
    then quote_value t bound ~loc { ty; mode; static = Lazy.from_val value } value
    else Literal { value; ty; mode; loc }
  | Extcall x -> Extcall { x with arg = reify_expr t bound x.arg }
  | Apply x -> Apply { x with fn = reify_expr t bound x.fn; arg = reify_expr t bound x.arg }
  | Specialize x ->
    Specialize { x with fn = reify_expr t bound x.fn; arg = reify_expr t bound x.arg }
  | Tuple_get x -> Tuple_get { x with tuple = reify_expr t bound x.tuple }
  | Payload_get x -> Payload_get { x with variant = reify_expr t bound x.variant }
  | Tag_test x -> Tag_test { x with variant = reify_expr t bound x.variant }
  | Make_ref x -> Make_ref { x with payload = reify_expr t bound x.payload }
  | Ref_get x -> Ref_get { x with ref = reify_expr t bound x.ref }
  | Tuple x -> Tuple { x with elts = Nonempty_list.map x.elts ~f:(reify_expr t bound) }
  | If x ->
    If
      { x with
        cond = reify_expr t bound x.cond
      ; then_ = reify_expr t bound x.then_
      ; else_ = reify_expr t bound x.else_
      }
  | Let x -> Let { x with bind = reify_expr t bound x.bind; rest = reify_expr t bound x.rest }
  | Lambda x -> Lambda { x with body = reify_expr t bound x.body }
  | Binder x -> Binder { x with body = reify_monos t bound (family_monos t x.family) }
  | Fun x ->
    Fun
      { x with
        funs = Nonempty_list.map x.funs ~f:(reify_fun t bound)
      ; rest = reify_expr t bound x.rest
      }
  | Match x ->
    let cases =
      Nonempty_list.map x.cases ~f:(fun ({ Expr.bindings; body } : Expr.case) ->
        let bindings = Map.map bindings ~f:(t.resolve ~loc:x.loc) in
        { Expr.bindings; body = reify_expr t bound body })
    in
    Match { x with cases; tree = reify_tree t bound x.tree }

and reify_tree t bound (tree : Expr.tree) : Expr.tree =
  match tree with
  | Leaf { case; bindings } -> Leaf { case; bindings = Map.map bindings ~f:(reify_expr t bound) }
  | Split { cond; then_; else_ } ->
    Split
      { cond = reify_expr t bound cond
      ; then_ = reify_tree t bound then_
      ; else_ = reify_tree t bound else_
      }

and reify_fun t bound (fun_ : Expr.fun_) : Expr.fun_ =
  match fun_ with
  | Lambda x ->
    Lambda { x with body = reify_expr t bound x.body; ty = t.resolve ~loc:(Expr.loc x.body) x.ty }
  | Binder x -> Binder { x with body = reify_monos t bound (family_monos t x.family) }

and reify_monos t bound monos = Map.map monos ~f:(reify_expr t bound)

and quote_value t bound ~loc (desc : Desc.t) (value : Value.t) : Expr.t =
  let value = t.resolve ~loc value in
  match value.node with
  | Bottom
  | Unit
  | Bool _
  | Int _
  | Type _
  | External _
  | Prim _
  | Var _
  | Proj _
  | Payload _
  | Inject _
  | Apply _
  | Match _
  | Deref _
  | Rec _ -> Literal { value; ty = desc.ty; mode = desc.mode; loc }
  | Box payload ->
    let content_ty = ref_content_ty t ~loc desc.ty in
    let payload =
      quote_value
        t
        bound
        ~loc
        { ty = content_ty; mode = desc.mode; static = Lazy.from_val payload }
        payload
    in
    Make_ref { payload; ty = desc.ty; mode = desc.mode; loc }
  | Constructor { payload = None; _ } -> Literal { value; ty = desc.ty; mode = desc.mode; loc }
  | Constructor { payload = Some payload; _ } when not (value_needs_reify payload) ->
    Literal { value; ty = desc.ty; mode = desc.mode; loc }
  | Constructor { label; payload = Some payload } ->
    (* Rebuild the constructor as an injection, since we do not have a constructor value node. *)
    let payload_ty = payload_ty ~loc desc label in
    let payload =
      quote_value
        t
        bound
        ~loc
        { ty = payload_ty; mode = desc.mode; static = Lazy.from_val payload }
        payload
    in
    let fn_ty =
      Value.type_
        (Arrow
           { arg_ty = payload_ty
           ; arg_mode = Modes.default ()
           ; ret_ty = desc.ty
           ; ret_mode = Modes.create ~staticity:Static ~erasure:Unerased
           })
    in
    let fn =
      Expr.Literal { value = Value.inject ~ty:desc.ty ~label; ty = fn_ty; mode = desc.mode; loc }
    in
    Apply { fn; arg = payload; ty = desc.ty; mode = desc.mode; loc }
  | Tuple elts ->
    let elt_tys = tuple_elt_tys ~loc desc.ty elts in
    let elts =
      Nonempty_list.map2_exn elts elt_tys ~f:(fun value ty ->
        quote_value t bound ~loc { ty; mode = desc.mode; static = Lazy.from_val value } value)
    in
    Tuple { elts; ty = desc.ty; mode = desc.mode; loc }
  | Closure closure ->
    quote_function t bound ~loc desc ~uid:closure.uid ~env:closure.env ~quote:(fun bound ->
      quote_closure t bound ~loc ~mode:desc.mode closure)
  | Binder binder ->
    quote_function t bound ~loc desc ~uid:binder.uid ~env:binder.env ~quote:(fun bound ->
      quote_binder t bound ~loc ~mode:desc.mode binder)

and quote_function t bound ~loc (desc : Desc.t) ~uid ~env ~quote =
  match Map.find bound uid with
  | Some id -> Var { id; ty = desc.ty; mode = desc.mode; loc }
  | None ->
    (match Map.find t.fun_bindings uid with
     | Some id -> quote_fun_group t bound env ~loc id
     | None -> quote bound)

and quote_closure t bound ~loc ~mode (closure : Closure.t) : Expr.t =
  let body = reify_expr t bound (Lazy.force closure.body) in
  let expr : Expr.t =
    Lambda { arg = closure.arg; body; ty = closure.ty; mode; family = closure.family; loc }
  in
  close_missing t bound closure.env expr

and quote_binder t bound ~loc ~mode (binder : Binder.t) : Expr.t =
  let body = reify_monos t bound (family_monos t binder.family) in
  let expr : Expr.t =
    Binder { arg = binder.arg; body; ty = binder.ty; mode; family = binder.family; loc }
  in
  close_missing t bound binder.env expr

and quote_fun_group t bound env ~loc root : Expr.t =
  let resolve = t.resolve ~loc in
  let members = collect_fun_group t bound env ~loc root in
  let env =
    Map.fold members ~init:env ~f:(fun ~key:_ ~data:desc env ->
      match Func.of_val ~resolve desc with
      | Some fn -> Env.merge env (Func.env fn)
      | None -> env)
  in
  let bound =
    Map.fold members ~init:bound ~f:(fun ~key:id ~data:desc bound ->
      match Func.of_val ~resolve desc with
      | Some fn -> Map.set bound ~key:(Func.uid fn) ~data:id
      | None -> bound)
  in
  let funs =
    Map.to_alist members
    |> List.filter_map ~f:(fun (var, desc) ->
      match Func.of_val ~resolve desc with
      | Some (Closure closure) ->
        Some
          (Lambda
             { var
             ; arg = closure.arg
             ; body = reify_expr t bound (Lazy.force closure.body)
             ; ty = closure.ty
             ; mode = desc.mode
             ; family = closure.family
             ; loc
             }
           : Expr.fun_)
      | Some (Binder binder) ->
        Some
          (Binder
             { var
             ; arg = binder.arg
             ; body = reify_monos t bound (family_monos t binder.family)
             ; ty = binder.ty
             ; mode = desc.mode
             ; family = binder.family
             ; loc
             }
           : Expr.fun_)
      | None -> None)
  in
  match Nonempty_list.of_list funs with
  | None -> raise_s [%message "Bug: function group with no members" (root : Ident.t)]
  | Some funs ->
    let desc = Env.find_exn env root in
    let expr : Expr.t =
      Fun
        { funs
        ; rest = Var { id = root; ty = desc.ty; mode = desc.mode; loc }
        ; ty = desc.ty
        ; mode = desc.mode
        ; loc
        }
    in
    close_missing t bound env expr

and close_missing t bound env expr =
  let loc = Expr.loc expr in
  let ty = Expr.ty expr in
  let mode = Expr.mode expr in
  Set.fold (Expr.free_vars expr) ~init:expr ~f:(fun rest id ->
    match Env.find env id with
    | None -> raise_s [%message "Bug: free var not bound" (id : Ident.t) (loc : Lex.Location.t)]
    | Some desc when Modes.is_erased desc.Desc.mode -> rest
    | Some desc ->
      (match Func.of_val ~resolve:(t.resolve ~loc) desc with
       | Some fn when Func.is_bound bound id fn -> rest
       | Some _ | None ->
         Let
           { var = id
           ; bind = quote_value t bound ~loc desc (Lazy.force desc.static)
           ; rest
           ; ty
           ; mode
           ; loc
           }))

and collect_fun_group t bound env ~loc root =
  let rec loop members work =
    match work with
    | [] -> members
    | (id, _) :: work when Map.mem members id -> loop members work
    | (id, source_env) :: work ->
      (match Option.first_some (Env.find env id) (Env.find source_env id) with
       | None ->
         raise_s
           [%message
             "Bug: function group dependency is not bound in any environment" (id : Ident.t)]
       | Some desc ->
         (match Func.of_val ~resolve:(t.resolve ~loc) desc with
          | None -> loop members work
          | Some fn when Func.is_bound bound id fn -> loop members work
          | Some fn ->
            let members = Map.set members ~key:id ~data:desc in
            let source_env = Func.env fn in
            let deps = function_deps t fn in
            let work =
              Set.fold deps ~init:work ~f:(fun work dep ->
                if Map.mem members dep then work else (dep, source_env) :: work)
            in
            loop members work))
  in
  loop Ident.Map.empty [ root, env ]

and function_deps t = function
  | Closure (closure : Closure.t) -> Set.remove (free_vars t (Lazy.force closure.body)) closure.arg
  | Binder (binder : Binder.t) ->
    Map.fold (family_monos t binder.family) ~init:Ident.Set.empty ~f:(fun ~key:_ ~data:body acc ->
      Set.union acc (free_vars t body))
    |> Fn.flip Set.remove binder.arg
;;

let rec find_targets acc (expr : Expr.t) =
  if Modes.is_erased (Expr.mode expr) then acc else find_targets' acc expr

and find_targets' acc (expr : Expr.t) =
  match expr with
  | Erased _ | Literal _ | Builtin _ | Var _ -> acc
  | Extcall { arg; _ } -> find_targets acc arg
  | Tuple_get { tuple; _ } -> find_targets acc tuple
  | Payload_get { variant; _ } | Tag_test { variant; _ } -> find_targets acc variant
  | Make_ref { payload; _ } -> find_targets acc payload
  | Ref_get { ref; _ } -> find_targets acc ref
  | Tuple { elts; _ } -> Nonempty_list.fold elts ~init:acc ~f:find_targets
  | Apply { fn; arg; _ } -> find_targets (find_targets acc fn) arg
  | Specialize { fn; arg; target; key; loc; _ } ->
    let acc =
      match target, key with
      | Family family, Some key -> (family, Hashcons.tag key, loc) :: acc
      | Family _, None | Prim _, _ -> acc
    in
    find_targets (find_targets acc fn) arg
  | If { cond; then_; else_; _ } -> find_targets (find_targets (find_targets acc cond) then_) else_
  | Let { bind; rest; _ } -> find_targets (find_targets acc bind) rest
  | Lambda { body; _ } -> find_targets acc body
  | Binder { body; _ } -> Map.fold body ~init:acc ~f:(fun ~key:_ ~data acc -> find_targets acc data)
  | Fun { funs; rest; _ } ->
    let acc = Nonempty_list.fold funs ~init:acc ~f:find_targets_fun in
    find_targets acc rest
  | Match { cases; tree; _ } ->
    let acc =
      Nonempty_list.fold cases ~init:acc ~f:(fun acc { Expr.bindings = _; body } ->
        find_targets acc body)
    in
    find_targets_tree acc tree

and find_targets_fun acc : Expr.fun_ -> _ = function
  | Lambda { body; _ } -> find_targets acc body
  | Binder { body; _ } -> Map.fold body ~init:acc ~f:(fun ~key:_ ~data acc -> find_targets acc data)

and find_targets_tree acc (tree : Expr.tree) =
  match tree with
  | Leaf { bindings; _ } ->
    Map.fold bindings ~init:acc ~f:(fun ~key:_ ~data acc -> find_targets acc data)
  | Split { cond; then_; else_ } ->
    find_targets_tree (find_targets_tree (find_targets acc cond) then_) else_
;;

let find_targets_top acc (tl : Top_level.t) =
  match tl with
  | Erased _ | External _ | Builtin _ -> acc
  | Let { bind; _ } -> find_targets acc bind
  | Fun { funs; _ } -> Nonempty_list.fold funs ~init:acc ~f:find_targets_fun
;;

let reachable_monos ~monomorphized top_levels =
  let seeds = List.fold top_levels ~init:[] ~f:find_targets_top in
  let rec visit kept ~depth frontier =
    match frontier with
    | [] -> kept
    | _ :: _ ->
      let kept, next =
        List.fold frontier ~init:(kept, []) ~f:(fun (kept, next) (family, key, _) ->
          let kept_already =
            Map.find kept family
            |> Option.value_map ~default:false ~f:(fun monos -> Map.mem monos key)
          in
          if kept_already
          then kept, next
          else (
            match monomorphized ~family ~key ~depth with
            | None -> kept, next
            | Some body ->
              let kept =
                Map.update kept family ~f:(fun monos ->
                  Map.set (Option.value monos ~default:Hashcons.Tag.Map.empty) ~key ~data:body)
              in
              kept, find_targets next body))
      in
      visit kept ~depth:(depth + 1) (List.rev next)
  in
  visit Ids.Family.Map.empty ~depth:0 seeds
;;

let top_level t (top_level : Top_level.t) : Top_level.t =
  let bound = Ids.Fn.Map.empty in
  match top_level with
  | Erased _ | Builtin _ -> top_level
  | External x -> External { x with ty = t.resolve ~loc:x.loc x.ty }
  | Let x -> Let { x with bind = reify_expr t bound x.bind }
  | Fun x -> Fun { x with funs = Nonempty_list.map x.funs ~f:(reify_fun t bound) }
;;

(* Every specialized call should have a monomorphized target *)
let check_targets t top_levels =
  List.fold top_levels ~init:[] ~f:find_targets_top
  |> List.iter ~f:(fun (family, key, loc) ->
    if not (Map.mem (family_monos t family) key)
    then
      raise_s
        [%message
          "Bug: emitted dispatch target lacks a mono" (family : Ids.Family.t) (loc : Lex.Location.t)])
;;

let program ~monomorphized ~fun_bindings ~resolve top_levels =
  let t = { monos = reachable_monos ~monomorphized top_levels; fun_bindings; resolve } in
  let top_levels = List.map top_levels ~f:(top_level t) in
  check_targets t top_levels;
  top_levels
;;
