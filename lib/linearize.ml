open! Core
open Lst
module Key = Tst.Value.Concrete

module State = struct
  type t =
    { mutable path : Path.t
    ; mutable stamp : int
    ; mutable bindings : (Path.t * Block.t) Vec.t
    ; procs : Proc.t Vec.t
    ; monomorphs : (int, Path.t) Hashtbl.t
    ; externals : (string, Path.t) Hashtbl.t
    }

  let create ~stamp =
    { path = Path.empty
    ; stamp
    ; bindings = Vec.of_array [||]
    ; procs = Vec.create ()
    ; monomorphs = Hashtbl.create (module Int)
    ; externals = Hashtbl.create (module String)
    }
  ;;

  let proc t proc = Vec.push_back t.procs proc
  let block t path block = Vec.push_back t.bindings (path, block)
  let monomorph t family id = Hashtbl.set t.monomorphs ~key:family ~data:(Path.with_id t.path id)
  let external_ t prim path = Hashtbl.set t.externals ~key:prim ~data:path

  let bind t ?id ?key return =
    let path = Path.with_ ?id ?key t.path in
    block t path (Block { bindings = [||]; return });
    path
  ;;

  let scope t ?id ?key f =
    let old_path = t.path in
    let old_bindings = t.bindings in
    let new_path = Path.with_ ?id ?key old_path in
    t.path <- new_path;
    t.bindings <- Vec.create ();
    let block = Block.Block { bindings = Vec.to_array t.bindings; return = f () } in
    t.path <- old_path;
    t.bindings <- old_bindings;
    new_path, block
  ;;

  module Id = struct
    let fresh t sym =
      let stamp = t.stamp in
      t.stamp <- stamp + 1;
      Ident.create (Ident.Raw.id sym) ~stamp
    ;;

    let refresh t id = fresh t (Ident.name () id)
    let inject t label = fresh t (Ident.Label.print () label)
    let if_ t = fresh t "if"
    let match_ t = fresh t "match"
    let temp t = fresh t "$"
    let lambda t = fresh t "λ"
    let env t = fresh t "𝒰"
  end
end

module Env = struct
  type t = (Path.t * Ty.t) Ident.Map.t [@@deriving sexp_of]

  let empty = Ident.Map.empty
  let bind t id path ty = Map.set t ~key:id ~data:(path, ty)
  let path t id = fst (Map.find_exn t id)

  let restrict t { Lst.Env.entries; length } free_vars : Lst.Env.t =
    let free_paths = Path.Set.map free_vars ~f:(path t) in
    let entries = Array.filter entries ~f:(fun { path; _ } -> Set.mem free_paths path) in
    { entries; length }
  ;;

  let rebind t free_vars =
    Set.fold free_vars ~init:t ~f:(fun vars id ->
      Map.update vars id ~f:(fun data -> Path.id id, snd (Option.value_exn data)))
  ;;

  let build t free_vars : Lst.Env.t * Lst.Env.t =
    let outer_entries = Vec.create () in
    let inner_entries = Vec.create () in
    let offset = ref 0 in
    let aux (ty : Ty.t) outer inner =
      let size =
        match Ty.size_in_mem ty with
        | 0 -> 0
        | size ->
          offset := Ty.align_to !offset ~align:(Ty.align_in_mem ty);
          size
      in
      let idx = !offset in
      offset := !offset + size;
      Vec.push_back inner_entries { Lst.Env.path = Path.id inner; ty; offset = idx };
      Vec.push_back outer_entries { Lst.Env.path = outer; ty; offset = idx }
    in
    Set.iter free_vars ~f:(fun id ->
      let outer, ty = Map.find_exn t id in
      aux ty outer id);
    ( { entries = Vec.to_array outer_entries; length = !offset }
    , { entries = Vec.to_array inner_entries; length = !offset } )
  ;;
end

let rec linearize_ty (ty : Sst.Ty.t) : Ty.t =
  match ty with
  | Unit -> Unit
  | Bool -> Bool
  | Int -> Int
  | Type -> Type
  | Pi _ -> Env
  | Arrow { arg_ty; ret_ty } ->
    Closure { arg_ty = linearize_ty arg_ty; ret_ty = linearize_ty ret_ty }
  | Tuple elts -> Tuple (Nonempty_list.map elts ~f:linearize_ty)
  | Variant constructors -> Variant (Map.map constructors ~f:(Option.map ~f:linearize_ty))
;;

let linearize_arrow (ty : Sst.Ty.t) : Ty.t * Ty.t =
  match ty with
  | Arrow { arg_ty; ret_ty } -> linearize_ty arg_ty, linearize_ty ret_ty
  | ty -> raise_s [%message "Bug: expected arrow" (ty : Sst.Ty.t)]
;;

let linearize_external state symbol ty loc : Expr.t =
  let path = Path.id (State.Id.fresh state symbol) in
  State.external_ state symbol path;
  let arg_ty, ret_ty = linearize_arrow ty in
  State.proc state (External { path; arg_ty; ret_ty; symbol; loc });
  Make_closure { body = path; env = None; ty = Closure { arg_ty; ret_ty }; loc }
;;

let linearize_inject state label ty loc : Expr.t =
  let arg_ty, ret_ty = linearize_arrow ty in
  let arg_path = Path.id (State.Id.temp state) in
  let body_path, body =
    State.scope state ~id:(State.Id.inject state label) (fun () ->
      Expr.Make_variant { label; payload = Some (arg_path, arg_ty); ty = ret_ty; loc })
  in
  State.proc
    state
    (Closure { path = body_path; arg = arg_path; arg_ty; env = Lst.Env.empty; body; loc });
  Make_closure { body = body_path; env = None; ty = Closure { arg_ty; ret_ty }; loc }
;;

let rec linearize_expr state env (sst : Sst.Expr.t) : Expr.t =
  match sst with
  | External { symbol; ty; loc } -> linearize_external state symbol ty loc
  | Extcall { symbol; arg; ty; loc } ->
    let arg_ty = linearize_ty (Sst.Expr.ty arg) in
    let arg = linearize_path state env arg in
    Extcall { symbol; arg; arg_ty; ty = linearize_ty ty; loc }
  | Scalar { value; ty; loc } -> Scalar { value; ty = linearize_ty ty; loc }
  | Var { id; ty; loc } -> Ident { path = Env.path env id; ty = linearize_ty ty; loc }
  | Let { var; bind; rest; _ } ->
    let path, block = State.scope state ~id:var (fun () -> linearize_expr state env bind) in
    State.block state path block;
    let env = Env.bind env var path (Block.ty block) in
    linearize_expr state env rest
  | Tuple { elts; ty; loc } ->
    let elts =
      Nonempty_list.to_list elts
      |> Array.of_list_map ~f:(fun elt ->
        linearize_path state env elt, linearize_ty (Sst.Expr.ty elt))
    in
    Make_tuple { elts; ty = linearize_ty ty; loc }
  | Tuple_get { tuple; index; ty; loc } ->
    let tuple = linearize_path state env tuple in
    Tuple_get { tuple; index; ty = linearize_ty ty; loc }
  | Constructor { label; payload; ty; loc } ->
    let payload =
      Option.map payload ~f:(fun payload ->
        let payload_ty = linearize_ty (Sst.Expr.ty payload) in
        linearize_path state env payload, payload_ty)
    in
    Make_variant { label; payload; ty = linearize_ty ty; loc }
  | Inject { label; ty; loc } -> linearize_inject state label ty loc
  | Payload_get { variant; label; ty; loc } ->
    let variant = linearize_path state env variant in
    Payload_get { variant; label; ty = linearize_ty ty; loc }
  | Tag_test { variant; label; ty; loc } ->
    let variant_ty = linearize_ty (Sst.Expr.ty variant) in
    let variant = linearize_path state env variant in
    Tag_test { variant; variant_ty; label; ty = linearize_ty ty; loc }
  | If { cond; then_; else_; ty; loc } ->
    let ty = linearize_ty ty in
    let id = State.Id.if_ state in
    let path = Path.with_id state.path id in
    let cond = linearize_expr state env cond in
    let _, then_ = State.scope state ~id (fun () -> linearize_expr state env then_) in
    let _, else_ = State.scope state ~id (fun () -> linearize_expr state env else_) in
    State.block state path (If { cond; then_; else_; ty; loc });
    Ident { path; ty; loc }
  | Match { cases; tree; ty; loc } ->
    let ty = linearize_ty ty in
    let id = State.Id.match_ state in
    let path = Path.with_id state.path id in
    let cases =
      Nonempty_list.to_list cases
      |> Array.of_list_map ~f:(fun { body; bindings } : Block.case ->
        let env =
          Map.fold bindings ~init:env ~f:(fun ~key ~data env ->
            Env.bind env key (Path.id key) (linearize_ty data))
        in
        let bindings =
          Map.to_sequence bindings
          |> Sequence.map ~f:(fun (id, ty) -> Path.id id, linearize_ty ty)
          |> Sequence.to_array
        in
        let _, block = State.scope state ~id (fun () -> linearize_expr state env body) in
        { bindings; block })
    in
    let tree = linearize_switch state env tree in
    State.block state path (Match { tree; cases; ty; loc });
    Ident { path; ty; loc }
  | Fun { funs; rest; loc; _ } ->
    let env = linearize_funs ~loc state env funs in
    linearize_expr state env rest
  | Lambda { arg; ty; body; family; loc; _ } ->
    let arg_ty, ret_ty = linearize_arrow ty in
    let arg_path = Path.id arg in
    let free_vars = Set.remove (Sst.Expr.free_vars body) arg in
    let outer_env, inner_env = Env.build env free_vars in
    let env = Env.rebind env free_vars in
    let env_path =
      State.bind state ~id:(State.Id.env state) (Make_env { env = outer_env; ty = Env; loc })
    in
    let lambda = State.Id.lambda state in
    State.monomorph state family lambda;
    let body_path, body =
      let env = Env.bind env arg arg_path arg_ty in
      State.scope state ~id:lambda (fun () -> linearize_expr state env body)
    in
    State.proc
      state
      (Closure { path = body_path; arg = arg_path; arg_ty; env = inner_env; body; loc });
    Make_closure { body = body_path; env = Some env_path; ty = Closure { arg_ty; ret_ty }; loc }
  | Apply { fn; arg; ty; loc } ->
    let arg_ty = linearize_ty (Sst.Expr.ty arg) in
    let fn = linearize_path state env fn in
    let arg = linearize_path state env arg in
    Apply_closure { fn; arg; arg_ty; ty = linearize_ty ty; loc }
  | Binder { body; family; loc; _ } ->
    let free_vars =
      Map.fold body ~init:Ident.Set.empty ~f:(fun ~key:_ ~data:body acc ->
        Set.union acc (Sst.Expr.free_vars body))
    in
    let lambda = State.Id.lambda state in
    State.monomorph state family lambda;
    (* print_s [%message (free_vars : Ident.Set.t) (body : Sst.Expr.t Key.Map.t)]; *)
    let outer_env, inner_env = Env.build env free_vars in
    Map.iteri body ~f:(fun ~key ~data:body ->
      let free_vars = Sst.Expr.free_vars body in
      let env = Env.rebind env free_vars in
      let inner_env = Env.restrict env inner_env free_vars in
      let body_path, body =
        State.scope state ~id:lambda ~key (fun () -> linearize_expr state env body)
      in
      State.proc state (Thunk { path = body_path; env = inner_env; body; loc }));
    Make_env { env = outer_env; ty = Env; loc }
  | Specialize { fn; arg; target; key; ty; loc } ->
    let ty = linearize_ty ty in
    let arg_ty = linearize_ty (Sst.Expr.ty arg) in
    let env_path = linearize_path state env fn in
    let fn_path =
      match target with
      | Family family -> Hashtbl.find_exn state.monomorphs family
      | Prim prim -> Hashtbl.find_exn state.externals (Builtin0.Prim.symbol prim)
    in
    let arg_path = linearize_path state env arg in
    (match key with
     | Some key -> Apply_thunk { fn = Path.with_key fn_path key; env = env_path; ty; loc }
     | None -> Apply_proc { fn = fn_path; arg = arg_path; arg_ty; env = env_path; ty; loc })

and linearize_switch state env (tree : Sst.Expr.tree) : Block.tree =
  match tree with
  | Leaf { case; bindings } ->
    let bindings =
      Map.to_sequence bindings
      |> Sequence.map ~f:(fun (id, binding) -> Path.id id, linearize_expr state env binding)
      |> Sequence.to_array
    in
    Leaf { case; bindings }
  | Split { cond; then_; else_ } ->
    let cond = linearize_expr state env cond in
    let then_ = linearize_switch state env then_ in
    let else_ = linearize_switch state env else_ in
    Split { cond; then_; else_ }

and linearize_path state env (sst : Sst.Expr.t) : Path.t =
  match sst with
  | Var { id; _ } -> Env.path env id
  | _ ->
    let id = State.Id.temp state in
    let path, block = State.scope state ~id (fun () -> linearize_expr state env sst) in
    State.block state path block;
    path

and linearize_funs ~loc state env (funs : Sst.Expr.fun_ Nonempty_list.t) =
  let env =
    Nonempty_list.fold funs ~init:env ~f:(fun acc -> function
      | Lambda { var; ty; _ } | Binder { var; ty; _ } ->
        Env.bind acc var (Path.id var) (linearize_ty ty))
  in
  let free_vars = Sst.Expr.free_vars_funs funs in
  let outer_env, inner_env = Env.build env free_vars in
  let env_path =
    State.bind
      state
      ~id:(State.Id.env state)
      (Make_env_rec { length = outer_env.length; ty = Env; loc })
  in
  let env =
    Nonempty_list.map funs ~f:(function
        | (Lambda { var; family; _ } | Binder { var; family; _ }) as fun_ ->
        let lambda = State.Id.refresh state var in
        State.monomorph state family lambda;
        lambda, fun_)
    |> Nonempty_list.fold ~init:env ~f:(fun acc -> function
      | lambda, Lambda { var; arg; body; ty; _ } ->
        let arg_ty, ret_ty = linearize_arrow ty in
        let arg_path = Path.id arg in
        let free_vars = Set.remove (Sst.Expr.free_vars body) arg in
        let env = Env.rebind env free_vars in
        let inner_env = Env.restrict env inner_env free_vars in
        let body_path, body =
          let env = Env.bind env arg arg_path arg_ty in
          State.scope state ~id:lambda (fun () -> linearize_expr state env body)
        in
        State.proc
          state
          (Closure { path = body_path; arg = arg_path; arg_ty; env = inner_env; body; loc });
        let ty = Ty.Closure { arg_ty; ret_ty } in
        let closure = Expr.Make_closure { body = body_path; env = Some env_path; ty; loc } in
        let path = State.bind state ~id:var closure in
        Env.bind acc var path ty
      | lambda, Binder { var; body; _ } ->
        Map.iteri body ~f:(fun ~key ~data:body ->
          let free_vars = Sst.Expr.free_vars body in
          let env = Env.rebind env free_vars in
          let inner_env = Env.restrict env inner_env free_vars in
          let body_path, body =
            State.scope state ~id:lambda ~key (fun () -> linearize_expr state env body)
          in
          State.proc state (Thunk { path = body_path; env = inner_env; body; loc }));
        let path = State.bind state ~id:var (Ident { path = env_path; ty = Env; loc }) in
        Env.bind acc var path Env)
  in
  let _ =
    let outer_env, _ = Env.build env free_vars in
    State.bind
      state
      ~id:(State.Id.temp state)
      (Fill_env_rec { path = env_path; env = outer_env; ty = Unit; loc })
  in
  env
;;

let linearize_top_level state env (sst : Sst.Top_level.t) : Env.t =
  match sst with
  | Let { var; bind; _ } ->
    let path, block = State.scope state ~id:var (fun () -> linearize_expr state env bind) in
    State.block state path block;
    Env.bind env var path (Block.ty block)
  | Fun { funs; loc; _ } -> linearize_funs ~loc state env funs
  | External { var; symbol; ty; loc } ->
    let path = State.bind state ~id:var (linearize_external state symbol ty loc) in
    Env.bind env var path (linearize_ty ty)
;;

let linearize (sst : Sst.Program.t) : Program.t =
  let state = State.create ~stamp:sst.stamp in
  let _ =
    List.fold sst.top_levels ~init:Env.empty ~f:(fun env top_level ->
      linearize_top_level state env top_level)
  in
  { procs = Vec.to_array state.procs; bindings = Vec.to_array state.bindings }
;;
