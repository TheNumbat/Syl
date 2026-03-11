open! Core
open Lst
module Key = Tst.Value.Concrete

module Id = struct
  let if_ = Ident.id "if"
  let then_ = Ident.id "then"
  let else_ = Ident.id "else"
  let temp = Ident.id "$"
  let lambda = Ident.id "λ"
  let env = Ident.id "env"
end

module State = struct
  type scope =
    { vars : ((Ident.t, int) Hashtbl.t[@sexp.opaque])
    ; stmts : Stmt.t Vec.t
    }
  [@@deriving sexp]

  type t =
    { mutable path : Path.t
    ; decls : Decl.t Vec.t
    ; scopes : scope Vec.t
    }
  [@@deriving sexp]

  let create () =
    { path = Path.empty
    ; decls = Vec.create ()
    ; scopes = Vec.of_array [| { vars = Hashtbl.create (module Ident); stmts = Vec.create () } |]
    }
  ;;

  let stmt t stmt = Vec.push_back (Vec.peek_back_exn t.scopes).stmts stmt
  let decl t decl = Vec.push_back t.decls decl
  let bind_one t path expr loc = stmt t (Values { exprs = [| path, expr |]; bind = [||]; loc })

  let unique { scopes; _ } path id =
    let path = Path.with_id path id in
    let top = Vec.peek_back_exn scopes in
    let n =
      Hashtbl.update_and_return top.vars id ~f:(function
        | Some n -> n + 1
        | None -> 0)
    in
    if n = 0 then path else Path.with_shadow path n
  ;;

  let global t id = unique t t.path id
  let local t id = unique t Path.empty id

  let scope t path ~f =
    let cur_path = t.path in
    t.path <- path;
    Vec.push_back t.scopes { vars = Hashtbl.create (module Ident); stmts = Vec.create () };
    let res = f () in
    t.path <- cur_path;
    res, Vec.to_array (Vec.pop_back_exn t.scopes).stmts
  ;;

  let is_top_level t = Vec.length t.scopes = 1
end

module Env = struct
  type t = Path.t Ident.Map.t [@@deriving sexp]

  let empty = Ident.Map.empty
  let bind t id path = Map.set t ~key:id ~data:path
  let find t id = Map.find_exn t id
end

let rec linearize_ty (ty : Sst.Ty.t) : Ty.t =
  match ty with
  | Unit -> Unit
  | Bool -> Bool
  | Int -> Int
  | Arrow { arg_ty; ret_ty } ->
    Closure { arg_ty = linearize_ty arg_ty; ret_ty = linearize_ty ret_ty }
  | Pack pack -> Pack (Hashtbl.map pack ~f:linearize_ty)
  | Tuple elts -> Tuple (List.map elts ~f:linearize_ty)
;;

let linearize_arrow ~loc (ty : Sst.Ty.t) : Ty.t * Ty.t =
  match ty with
  | Arrow { arg_ty; ret_ty } -> linearize_ty arg_ty, linearize_ty ret_ty
  | ty -> raise_s [%message "Expected arrow" (ty : Sst.Ty.t) (loc : Lex.Location.t)]
;;

let expand_pack path (expr : Expr.t) =
  let exprs = Vec.create () in
  let thunk ~f expr =
    let expr = f expr in
    Expr.with_ty expr (Thunk (Expr.ty expr))
  in
  let rec aux dst (expr : Expr.t) ~f =
    match expr with
    | Make_closure { body; env; ty = Pack pack; loc } ->
      Hashtbl.iteri pack ~f:(fun ~key ~data ->
        let body = Path.with_key body key in
        aux (Path.with_key dst key) (Expr.Make_closure { body; env; ty = data; loc }) ~f:(thunk ~f))
    | Apply_thunk { fn; ty = Pack pack; loc } ->
      Hashtbl.iteri pack ~f:(fun ~key ~data ->
        let fn = Path.with_key fn key in
        aux (Path.with_key dst key) (Expr.Apply_thunk { fn; ty = data; loc }) ~f:(thunk ~f))
    | Ident { path; ty = Pack pack; loc } ->
      Hashtbl.iteri pack ~f:(fun ~key ~data ->
        let path = Path.with_key path key in
        aux (Path.with_key dst key) (Expr.Ident { path; ty = data; loc }) ~f:(thunk ~f))
    | Builtin { ty; _ } (* TODO poly builtins? *)
    | Make_env { ty; _ }
    | Make_closure { ty; _ }
    | Apply_closure { ty; _ }
    | Apply_thunk { ty; _ }
    | Scalar { ty; _ }
    | Unop { ty; _ }
    | Binop { ty; _ }
    | Make_tuple { ty; _ }
    | Ident { ty; _ } ->
      (match ty with
       | Pack _ -> raise_s [%message "Unexpected pack" (expr : Expr.t)]
       | _ -> ());
      Vec.push_back exprs (dst, f expr)
  in
  aux path expr ~f:Fn.id;
  exprs
;;

let unarize_values state path bind expr loc =
  let exprs = Vec.to_array (expand_pack path expr) in
  if State.is_top_level state
  then State.decl state (Values { exprs; bind; loc })
  else State.stmt state (Values { exprs; bind; loc })
;;

let unarize_thunks state path captures bind expr loc =
  let exprs = expand_pack path expr in
  Vec.iter exprs ~f:(fun (path, return) ->
    State.decl state (Decl.Thunk_body { path; captures; bind; return; loc }))
;;

let unarize_env env (fvs : Sst.Ty.t Ident.Map.t) =
  let path_to_bind = Hashtbl.create (module Path) in
  let offset = ref 0 in
  let rec unarize outer_path inner_path (ty : Ty.t) ~f =
    match ty with
    | Pack pack ->
      Hashtbl.to_alist pack
      |> List.concat_map ~f:(fun (key, ty) ->
        unarize (Path.with_key outer_path key) (Path.with_key inner_path key) ty ~f:(fun ty ->
          Ty.Thunk (f ty)))
    | _ ->
      let ty = f ty in
      let size =
        match Ty.size_in_mem ty with
        | 0 -> 0
        | size ->
          offset := Ty.align_to !offset ~align:(Ty.align_in_mem ty);
          size
      in
      let idx = !offset in
      offset := !offset + size;
      Hashtbl.set
        path_to_bind
        ~key:inner_path
        ~data:{ Lst.Env.path = inner_path; ty; offset_in_bytes = idx };
      [ { Lst.Env.path = outer_path; ty; offset_in_bytes = idx } ]
  in
  let entries =
    Map.to_sequence fvs
    |> Sequence.concat_map ~f:(fun (id, ty) ->
      let ty = linearize_ty ty in
      let outer_path = Env.find env id in
      (* We know all free ids are unique *)
      let inner_path = Path.with_id Path.empty id in
      Sequence.of_list (unarize outer_path inner_path ty ~f:Fn.id))
    |> Sequence.to_array
  in
  { Lst.Env.entries; size_in_bytes = !offset }, path_to_bind
;;

let bind_env state env (fvs : Sst.Ty.t Ident.Map.t) path_to_bind =
  let rec unarize path (ty : Ty.t) =
    match ty with
    | Pack pack ->
      Hashtbl.to_alist pack
      |> List.concat_map ~f:(fun (key, ty) -> unarize (Path.with_key path key) ty)
    | _ -> [ path ]
  in
  let env, binds =
    Map.fold fvs ~init:(env, []) ~f:(fun ~key:id ~data:ty (env, binds) ->
      let ty = linearize_ty ty in
      let env = Env.bind env id (State.local state id) in
      let bind =
        (* We know all free ids are unique *)
        unarize (Path.with_id Path.empty id) ty
        |> Array.of_list_map ~f:(Hashtbl.find_exn path_to_bind)
      in
      env, bind :: binds)
  in
  env, Array.concat binds
;;

let linearize_external state symbol ty loc : Expr.t =
  let path = Path.with_id (State.global state (Ident.id symbol)) Id.lambda in
  let arg_ty, ret_ty = linearize_arrow ~loc ty in
  let decl = Decl.External { path; arg_ty; ret_ty; symbol; loc } in
  State.decl state decl;
  Make_closure { body = path; env = None; ty = Closure { arg_ty; ret_ty }; loc }
;;

let rec linearize_expr state env (sst : Sst.Expr.t) : Expr.t =
  match sst with
  | Builtin { builtin; ty; loc } -> Builtin { builtin; ty = linearize_ty ty; loc }
  | External { symbol; ty; loc } -> linearize_external state symbol ty loc
  | Scalar { value; ty; loc } -> Scalar { value; ty = linearize_ty ty; loc }
  | Var { id; ty; loc } -> Ident { path = Env.find env id; ty = linearize_ty ty; loc }
  | Unop { op; arg; ty; loc } ->
    Unop { op; arg = linearize_path state env arg; ty = linearize_ty ty; loc }
  | Binop { op; lhs; rhs; ty; loc } ->
    let lhs = linearize_path state env lhs in
    let rhs = linearize_path state env rhs in
    Binop { op; lhs; rhs; ty = linearize_ty ty; loc }
  | Tuple { elts; ty; loc } ->
    Make_tuple
      { elts =
          Array.of_list_map elts ~f:(fun elt ->
            linearize_path state env elt, linearize_ty (Sst.Expr.ty elt))
      ; ty = linearize_ty ty
      ; loc
      }
  | If { cond; then_; else_; ty; loc } ->
    let path = State.global state Id.if_ in
    let cond = linearize_expr state env cond in
    let then_, then_bind =
      State.scope state (Path.with_id path Id.then_) ~f:(fun () -> linearize_expr state env then_)
    in
    let else_, else_bind =
      State.scope state (Path.with_id path Id.else_) ~f:(fun () -> linearize_expr state env else_)
    in
    let stmt = Stmt.If { path; cond; then_bind; then_; else_bind; else_; loc } in
    State.stmt state stmt;
    Ident { path; ty = linearize_ty ty; loc }
  | Let { var; bind; rest; loc; _ } ->
    let path = State.global state var in
    let expr, bind = State.scope state path ~f:(fun () -> linearize_expr state env bind) in
    unarize_values state path bind expr loc;
    linearize_expr state (Env.bind env var path) rest
  | Lambda { arg; fvs; ty; body; loc } ->
    let arg_ty, ret_ty = linearize_arrow ~loc ty in
    let env_path = State.global state Id.env in
    let outer_captures, path_to_bind = unarize_env env fvs in
    State.bind_one state env_path (Make_env { captures = outer_captures; ty = Env; loc }) loc;
    let body_path = State.global state Id.lambda in
    let (return, arg_path, inner_captures), bind =
      State.scope state body_path ~f:(fun () ->
        let arg_path = State.local state arg in
        let env, inner_captures = bind_env state env fvs path_to_bind in
        let env = Env.bind env arg arg_path in
        linearize_expr state env body, arg_path, inner_captures)
    in
    State.decl
      state
      (Closure_body
         { path = body_path; arg = arg_path; arg_ty; captures = inner_captures; bind; return; loc });
    Make_closure { body = body_path; env = Some env_path; ty = Closure { arg_ty; ret_ty }; loc }
  | Apply { fn; arg; ty; loc } ->
    let arg_ty = linearize_ty (Sst.Expr.ty arg) in
    let fn = linearize_path state env fn in
    let arg = linearize_path state env arg in
    Apply_closure { fn; arg; arg_ty; ty = linearize_ty ty; loc }
  | Symbol { fn; arg; ty; loc } ->
    let fn = linearize_path state env fn in
    Apply_thunk { fn = Path.with_key fn arg; ty = linearize_ty ty; loc }
  | Fun { funs; fvs; rest; loc; _ } ->
    let bind = linearize_funs ~loc state env fvs funs in
    let env = Nonempty_list.fold bind ~init:env ~f:(fun env (var, path) -> Env.bind env var path) in
    linearize_expr state env rest
  | Pack { pack; fvs; ty; loc } ->
    let env_path = State.global state Id.env in
    let outer_captures, path_to_bind = unarize_env env fvs in
    State.bind_one state env_path (Make_env { captures = outer_captures; ty = Env; loc }) loc;
    let body_path = State.global state Id.lambda in
    linearize_pack ~loc state env body_path pack path_to_bind;
    Make_closure { body = body_path; env = Some env_path; ty = linearize_ty ty; loc }

and linearize_pack ~loc state env path pack path_to_bind =
  Hashtbl.iteri pack ~f:(fun ~key ~data:(expr, fvs) ->
    let path = Path.with_key path key in
    let (return, inner_captures), bind =
      State.scope state path ~f:(fun () ->
        let env, inner_captures = bind_env state env fvs path_to_bind in
        linearize_expr state env expr, inner_captures)
    in
    unarize_thunks state path inner_captures bind return loc)

and linearize_funs ~loc state env fvs (funs : Sst.Expr.fun_ Nonempty_list.t) =
  let binds =
    Nonempty_list.map funs ~f:(function Mono { var; _ } | Pack { var; _ } ->
        var, State.global state var)
  in
  let env = Nonempty_list.fold binds ~init:env ~f:(fun env (var, path) -> Env.bind env var path) in
  let outer_captures, path_to_bind = unarize_env env fvs in
  let paths =
    let paths = Vec.create () in
    List.iter2_exn
      (Nonempty_list.to_list funs)
      (Nonempty_list.to_list binds)
      ~f:(fun fun_ (_, path) ->
        let body_path = Path.with_id path Id.lambda in
        match fun_ with
        | Mono { arg; body; fvs; ty; loc; _ } ->
          let arg_ty, ret_ty = linearize_arrow ~loc ty in
          let (return, arg_path, inner_captures), bind =
            State.scope state body_path ~f:(fun () ->
              let arg_path = State.local state arg in
              let env, inner_captures = bind_env state env fvs path_to_bind in
              let env = Env.bind env arg arg_path in
              linearize_expr state env body, arg_path, inner_captures)
          in
          State.decl
            state
            (Closure_body
               { path = body_path
               ; arg = arg_path
               ; captures = inner_captures
               ; arg_ty
               ; bind
               ; return
               ; loc
               });
          Vec.push_back paths (path, Ty.Closure { arg_ty; ret_ty }, body_path)
        | Pack { pack; ty; loc; _ } ->
          linearize_pack ~loc state env body_path pack path_to_bind;
          let rec unarize_thunks path body_path (ty : Ty.t) ~f =
            match ty with
            | Pack pack ->
              Hashtbl.iteri pack ~f:(fun ~key ~data ->
                unarize_thunks
                  (Path.with_key path key)
                  (Path.with_key body_path key)
                  data
                  ~f:(fun ty -> Ty.Thunk (f ty)))
            | _ -> Vec.push_back paths (path, f ty, body_path)
          in
          unarize_thunks path body_path (linearize_ty ty) ~f:Fn.id);
    Vec.to_array paths
  in
  if State.is_top_level state
  then State.decl state (Functions { paths; captures = outer_captures; loc })
  else State.stmt state (Functions { paths; captures = outer_captures; loc });
  binds

and linearize_path state env (sst : Sst.Expr.t) : Path.t =
  let expr = linearize_expr state env sst in
  match expr with
  | Ident { path; _ } -> path
  | expr ->
    let path = State.local state Id.temp in
    let loc = Sst.Expr.loc sst in
    unarize_values state path [||] expr loc;
    path
;;

let linearize_top_level state env (sst : Sst.Top_level.t) : Env.t =
  match sst with
  | Let { var; bind; loc } ->
    let path = State.global state var in
    let expr, bind = State.scope state path ~f:(fun () -> linearize_expr state env bind) in
    unarize_values state path bind expr loc;
    Env.bind env var path
  | Fun { funs; fvs; loc; _ } ->
    let bind = linearize_funs ~loc state env fvs funs in
    Nonempty_list.fold bind ~init:env ~f:(fun env (var, path) -> Env.bind env var path)
  | External { var; symbol; ty; loc } ->
    let path = State.global state var in
    let expr = linearize_external state symbol ty loc in
    unarize_values state path [||] expr loc;
    Env.bind env var path
  | Builtin { var; builtin; ty; loc } ->
    let path = State.global state var in
    let expr = Expr.Builtin { builtin; ty = linearize_ty ty; loc } in
    unarize_values state path [||] expr loc;
    Env.bind env var path
;;

let linearize (sst : Sst.Program.t) : Program.t =
  let state = State.create () in
  let env = Env.empty in
  let _ =
    List.fold sst ~init:env ~f:(fun env top_level -> linearize_top_level state env top_level)
  in
  if Vec.length state.scopes <> 1
  then (
    let top = Vec.peek_back_exn state.scopes in
    raise_s [%message "Unused scope" (top : State.scope)]);
  if Vec.length (Vec.peek_back_exn state.scopes).stmts <> 0
  then (
    let top = Vec.peek_back_exn state.scopes in
    raise_s [%message "Unused statements" (top : State.scope)]);
  Vec.to_array state.decls
;;
