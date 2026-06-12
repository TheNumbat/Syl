open! Core
open Tst
module Key = Value.Concrete

(* Mostly vibe coded. Needs more review. *)

type t =
  { monos : Expr.t Key.Map.t Core.Int.Map.t (* family -> key -> body; union across the family *)
  ; groups : Ident.t Core.Int.Map.t (* fun value -> its binding *)
  }

let family_monos t family = Map.find t.monos family |> Option.value ~default:Key.Map.empty
let free_vars t expr = Expr.free_vars ~monos:(family_monos t) expr

let tuple_elt_tys ~loc ty elts =
  match ty with
  | Value.Type (Tuple elt_tys) when Nonempty_list.length elt_tys = Nonempty_list.length elts ->
    elt_tys
  | _ ->
    raise_s
      [%message
        "Bug: quoted tuple's type is not a matching tuple type"
          (ty : Value.t)
          (loc : Lex.Location.t)]
;;

(* Typechecking has finished, so no static is mid-computation: forcing
   cannot raise [Lazy.Undefined]. *)
let force_static desc = Lazy.force desc.Desc.static

let as_function desc =
  match force_static desc with
  | Closure closure -> Some (`Closure closure)
  | Binder binder -> Some (`Binder binder)
  | _ -> None
;;

let function_env = function
  | `Closure (closure : Closure.t) -> closure.env
  | `Binder (binder : Binder.t) -> binder.env
;;

let function_hash = function
  | `Closure (closure : Closure.t) -> closure.hash
  | `Binder (binder : Binder.t) -> binder.hash
;;

let bound_to bound id fn =
  match Map.find bound (function_hash fn) with
  | Some bound_id -> Ident.equal bound_id id
  | None -> false
;;

(* Preferring the existing binding is sound because every env consulted
   during a group quote agrees on the idents actually looked up: members and
   their deps come from one instantiation's lexical scope, so a member cannot
   reference another instance's binding. *)
let merge_envs = Map.merge_skewed ~combine:(fun ~key:_ existing _from_function_env -> existing)

let rec value_needs_reification value = reduced_value_needs_reification (Value.reduce value)

and reduced_value_needs_reification (value : Value.t) =
  match value with
  | Closure _ | Binder _ -> true
  | Tuple elts -> Nonempty_list.exists elts ~f:value_needs_reification
  | If { cond; then_; else_ } ->
    value_needs_reification cond || value_needs_reification then_ || value_needs_reification else_
  | Apply { fn; arg } -> value_needs_reification fn || value_needs_reification arg
  | Proj { tuple; _ } -> value_needs_reification tuple
  | Bottom | Unit | Bool _ | Int _ | Type _ | Var _ | External _ | Prim _ -> false
;;

let rec reify_expr t bound (expr : Expr.t) : Expr.t =
  let recur = reify_expr t bound in
  match expr with
  | Erased _ | Builtin _ | Var _ -> expr
  | Literal { value; ty; mode; loc } when Modes.is_static mode ->
    let value = Value.reduce value in
    if reduced_value_needs_reification value
    then quote_reduced_value t bound ~loc { ty; mode; static = Lazy.from_val value } value
    else expr
  | Literal _ -> expr
  | Extcall x -> Extcall { x with arg = recur x.arg }
  | Apply x -> Apply { x with fn = recur x.fn; arg = recur x.arg }
  | Specialize x -> Specialize { x with fn = recur x.fn; arg = recur x.arg }
  | Tuple_get x -> Tuple_get { x with tuple = recur x.tuple }
  | Tuple x -> Tuple { x with elts = Nonempty_list.map x.elts ~f:recur }
  | If x -> If { x with cond = recur x.cond; then_ = recur x.then_; else_ = recur x.else_ }
  | Let x -> Let { x with bind = recur x.bind; rest = recur x.rest }
  | Lambda x -> Lambda { x with body = recur x.body }
  | Binder x -> Binder { x with body = reify_mono_table t bound (family_monos t x.family) }
  | Fun x ->
    Fun { x with funs = Nonempty_list.map x.funs ~f:(reify_fun t bound); rest = recur x.rest }
  | Match x ->
    let cases =
      Nonempty_list.map x.cases ~f:(fun ({ Expr.bindings; body } : Expr.case) ->
        { Expr.bindings; body = recur body })
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
  | Lambda x -> Lambda { x with body = reify_expr t bound x.body }
  | Binder x -> Binder { x with body = reify_mono_table t bound (family_monos t x.family) }

and reify_mono_table t bound monos = Map.map monos ~f:(reify_expr t bound)

and quote_value t bound ~loc (desc : Desc.t) (value : Value.t) : Expr.t =
  quote_reduced_value t bound ~loc desc (Value.reduce value)

and quote_reduced_value t bound ~loc (desc : Desc.t) (value : Value.t) : Expr.t =
  match value with
  | Bottom | Unit | Bool _ | Int _ | Type _ | External _ | Prim _ | Var _ | Proj _ ->
    Literal { value; ty = desc.ty; mode = desc.mode; loc }
  | (If _ | Apply _) as value ->
    if reduced_value_needs_reification value
    then raise_s [%message "Bug: stuck value cannot be reified" (value : Value.t)]
    else Literal { value; ty = desc.ty; mode = desc.mode; loc }
  | Tuple elts ->
    let elt_tys = tuple_elt_tys ~loc desc.ty elts in
    let elts =
      Nonempty_list.map2_exn elts elt_tys ~f:(fun value ty ->
        quote_value t bound ~loc { ty; mode = desc.mode; static = Lazy.from_val value } value)
    in
    Tuple { elts; ty = desc.ty; mode = desc.mode; loc }
  | Closure closure ->
    quote_function t bound ~loc desc ~hash:closure.hash ~env:closure.env ~quote:(fun bound ->
      quote_closure t bound ~loc ~mode:desc.mode closure)
  | Binder binder ->
    quote_function t bound ~loc desc ~hash:binder.hash ~env:binder.env ~quote:(fun bound ->
      quote_binder t bound ~loc ~mode:desc.mode binder)

and quote_function t bound ~loc (desc : Desc.t) ~hash ~env ~quote =
  match Map.find bound hash with
  | Some id -> Var { id; ty = desc.ty; mode = desc.mode; loc }
  | None ->
    (match Map.find t.groups hash with
     | Some id ->
       (* A fun-group member: its name is bound in its own captured env (via
          env_rec), so quote the whole binding group rooted there. *)
       quote_fun_group t bound env ~loc id
     | None -> quote bound)

and quote_closure t bound ~loc ~mode (closure : Closure.t) : Expr.t =
  let body = reify_expr t bound closure.body in
  let expr : Expr.t =
    Lambda { arg = closure.arg; body; ty = closure.ty; mode; family = closure.family; loc }
  in
  close_missing t bound closure.env expr

and quote_binder t bound ~loc ~mode (binder : Binder.t) : Expr.t =
  let body = reify_mono_table t bound (family_monos t binder.family) in
  let expr : Expr.t =
    Binder { arg = binder.arg; body; ty = binder.ty; mode; family = binder.family; loc }
  in
  close_missing t bound binder.env expr

and quote_fun_group t bound env ~loc root : Expr.t =
  let members = collect_fun_group t bound env root in
  let env =
    Map.fold members ~init:env ~f:(fun ~key:_ ~data:desc env ->
      match as_function desc with
      | Some fn -> merge_envs env (function_env fn)
      | None -> env)
  in
  let bound =
    Map.fold members ~init:bound ~f:(fun ~key:id ~data:desc bound ->
      match as_function desc with
      | Some fn -> Map.set bound ~key:(function_hash fn) ~data:id
      | None -> bound)
  in
  let funs =
    Map.to_alist members
    |> List.filter_map ~f:(fun (var, desc) ->
      match as_function desc with
      | Some (`Closure closure) ->
        Some
          (Lambda
             { var
             ; arg = closure.arg
             ; body = reify_expr t bound closure.body
             ; ty = closure.ty
             ; mode = desc.mode
             ; family = closure.family
             ; loc
             }
           : Expr.fun_)
      | Some (`Binder binder) ->
        Some
          (Binder
             { var
             ; arg = binder.arg
             ; body = reify_mono_table t bound (family_monos t binder.family)
             ; ty = binder.ty
             ; mode = desc.mode
             ; family = binder.family
             ; loc
             }
           : Expr.fun_)
      | None -> None)
  in
  match Nonempty_list.of_list funs with
  | None -> raise_s [%message "Bug: function group with no function members" (root : Ident.t)]
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
    | None ->
      raise_s
        [%message
          "Bug: free variable of a quoted value is not bound in its environment"
            (id : Ident.t)
            (loc : Lex.Location.t)]
    | Some desc when Modes.is_erased desc.Desc.mode -> rest
    | Some desc ->
      (match as_function desc with
       | Some fn when bound_to bound id fn -> rest
       | Some _ | None ->
         Let
           { var = id
           ; bind = quote_value t bound ~loc desc (force_static desc)
           ; rest
           ; ty
           ; mode
           ; loc
           }))

and collect_fun_group t bound env root =
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
         (match as_function desc with
          | None -> loop members work
          | Some fn when bound_to bound id fn -> loop members work
          | Some fn ->
            (* Function deps join the group even when they are not [t.groups]
               members: a dep's mono table can reference group members (e.g. a
               binder specialized at a member's value), so it must be bound
               inside the group's recursive env. *)
            let members = Map.set members ~key:id ~data:desc in
            let source_env = function_env fn in
            let deps = function_deps t fn in
            let work =
              Set.fold deps ~init:work ~f:(fun work dep ->
                if Map.mem members dep then work else (dep, source_env) :: work)
            in
            loop members work))
  in
  loop Ident.Map.empty [ root, env ]

and function_deps t = function
  | `Closure (closure : Closure.t) -> Set.remove (free_vars t closure.body) closure.arg
  | `Binder (binder : Binder.t) ->
    Map.fold (family_monos t binder.family) ~init:Ident.Set.empty ~f:(fun ~key:_ ~data:body acc ->
      Set.union acc (free_vars t body))
    |> Fn.flip Set.remove binder.arg
;;

let rec find_targets acc (expr : Expr.t) =
  match expr with
  | Erased _ | Literal _ | Builtin _ | Var _ -> acc
  | Extcall { arg; _ } -> find_targets acc arg
  | Tuple_get { tuple; _ } -> find_targets acc tuple
  | Tuple { elts; _ } -> Nonempty_list.fold elts ~init:acc ~f:find_targets
  | Apply { fn; arg; _ } -> find_targets (find_targets acc fn) arg
  | Specialize { fn; arg; target; key; _ } ->
    let acc =
      match target, key with
      | Family family, Some key -> (family, key) :: acc
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

let reachable_monos monos top_levels =
  let seeds =
    List.fold top_levels ~init:[] ~f:(fun acc (tl : Top_level.t) ->
      match tl with
      | Erased _ | External _ | Builtin _ -> acc
      | Let { bind; _ } -> find_targets acc bind
      | Fun { funs; _ } -> Nonempty_list.fold funs ~init:acc ~f:find_targets_fun)
  in
  let rec visit kept = function
    | [] -> kept
    | (family, key) :: frontier ->
      let kept_already =
        Map.find kept family |> Option.value_map ~default:false ~f:(fun m -> Map.mem m key)
      in
      if kept_already
      then visit kept frontier
      else (
        match Map.find monos family |> Option.bind ~f:(fun m -> Map.find m key) with
        | None -> visit kept frontier
        | Some body ->
          let kept =
            Map.update kept family ~f:(fun m ->
              Map.set (Option.value m ~default:Key.Map.empty) ~key ~data:body)
          in
          visit kept (find_targets frontier body))
  in
  visit Core.Int.Map.empty seeds
;;

let top_level t (top_level : Top_level.t) : Top_level.t =
  let bound = Core.Int.Map.empty in
  match top_level with
  | Erased _ | External _ | Builtin _ -> top_level
  | Let x -> Let { x with bind = reify_expr t bound x.bind }
  | Fun x -> Fun { x with funs = Nonempty_list.map x.funs ~f:(reify_fun t bound) }
;;

let program ~monos ~groups top_levels =
  let t = { monos = reachable_monos monos top_levels; groups } in
  List.map top_levels ~f:(top_level t)
;;
