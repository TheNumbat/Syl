open! Core

module Env = struct
  type t =
    { vars : int Ident.Raw.Map.t
    ; stamp : int ref
    }

  let create () = { vars = Ident.Raw.Map.empty; stamp = ref 0 }

  let next t =
    let stamp = !(t.stamp) in
    t.stamp := stamp + 1;
    stamp
  ;;

  let bind t raw =
    let stamp = next t in
    { t with vars = Map.set t.vars ~key:raw ~data:stamp }, Ident.create raw ~stamp
  ;;

  let find t raw =
    let stamp = Map.find t.vars raw |> Option.value ~default:(-1) in
    Ident.create raw ~stamp
  ;;
end

let arg_is_static (arg_mode : Modes.Maybe.t) =
  match arg_mode.staticity with
  | Some Static -> true
  | Some (Parametric | Dynamic) -> false
  | None -> Modes.Maybe.is_erased arg_mode
;;

let maybe_erased (erased : Modes.Erasure.t) (expr : Dst.Expr.t) loc : Dst.Expr.t =
  match erased with
  | Erased -> Mode_annotation { expr; mode = { staticity = None; erasure = Some Erased }; loc }
  | Unerased -> expr
;;

let rec desugar_pattern env seen (pattern : Cst.Expr.pattern)
  : Env.t * Ident.Raw.Set.t * Dst.Expr.pattern
  =
  let loc = pattern.loc in
  match pattern.node with
  | Var { id } ->
    if Set.mem seen id
    then (
      let id = Env.find env id in
      env, seen, Var { id; loc })
    else (
      let seen = Set.add seen id in
      let env, id = Env.bind env id in
      env, seen, Var { id; loc })
  | Literal { value } -> env, seen, Literal { value; loc }
  | Tuple { elts } ->
    let (env, seen), elts =
      Nonempty_list.fold_map elts ~init:(env, seen) ~f:(fun (env, seen) pat ->
        let env, seen, pat = desugar_pattern env seen pat in
        (env, seen), pat)
    in
    env, seen, Tuple { elts; loc }
  | Or { left; right } ->
    let env, seen, left = desugar_pattern env seen left in
    let env, seen, right = desugar_pattern env seen right in
    env, seen, Or { left; right; loc }

and desugar_expr env (expr : Cst.Expr.t) : Dst.Expr.t =
  let loc = expr.loc in
  match expr.node with
  | If { cond; then_; else_; static; _ } ->
    If
      { cond = desugar_expr env cond
      ; then_ = desugar_expr env then_
      ; else_ = desugar_expr env else_
      ; static
      ; loc
      }
  | Match { cond; arms; static; _ } ->
    let cond = desugar_expr env cond in
    let arms =
      Nonempty_list.map arms ~f:(fun (pat, rhs) ->
        let env, _, pat = desugar_pattern env Ident.Raw.Set.empty pat in
        pat, desugar_expr env rhs)
    in
    Match { cond; arms; static; loc }
  | Let { var; erased; bind; rest; _ } ->
    let bind = maybe_erased erased (desugar_expr env bind) loc in
    let env, var = Env.bind env var in
    Let { var; bind; rest = desugar_expr env rest; loc }
  | Fun { funs; rest } ->
    let env, rev_pairs =
      Nonempty_list.fold funs ~init:(env, []) ~f:(fun (env, acc) f ->
        let env, var = Env.bind env f.node.var in
        env, (f, var) :: acc)
    in
    let pairs = List.rev rev_pairs in
    let funs =
      List.map pairs ~f:(fun (f, var) -> desugar_fun env f var) |> Nonempty_list.of_list_exn
    in
    Fun { funs; rest = desugar_expr env rest; loc }
  | Lambda { erased; arg; body; _ } ->
    let arg_ty = desugar_expr env arg.node.ty in
    let env, arg_id = Env.bind env arg.node.var in
    let body = desugar_expr env body in
    let lambda : Dst.Expr.t =
      Lambda { arg = arg_id; arg_mode = arg.node.mode; arg_ty; body; loc }
    in
    maybe_erased erased lambda loc
  | Apply { fn; arg } -> Apply { fn = desugar_expr env fn; arg = desugar_expr env arg; loc }
  | Paren { expr } -> desugar_expr env expr
  | Var { id } -> Var { id = Env.find env id; loc }
  | Literal { value } -> Literal { value; loc }
  | Unop { op; arg } ->
    Apply
      { fn = Var { id = Env.find env (Ident.Raw.unop op); loc }; arg = desugar_expr env arg; loc }
  | Binop { op; lhs; rhs } ->
    let lhs = desugar_expr env lhs in
    let rhs = desugar_expr env rhs in
    Apply
      { fn = Var { id = Env.find env (Ident.Raw.binop op); loc }
      ; arg = Make_tuple { elts = [ lhs; rhs ]; loc }
      ; loc
      }
  | Make_tuple { elts } ->
    let elts = Nonempty_list.map elts ~f:(desugar_expr env) in
    Make_tuple { elts; loc }
  | Arrow { arg; arg_id; arg_mode; ret; ret_mode } ->
    let arg = desugar_expr env arg in
    let arg_id = Option.value_or_thunk arg_id ~default:(fun () -> Ident.Raw.anon) in
    let env, arg_id = Env.bind env arg_id in
    Arrow { arg; arg_id; arg_mode; ret = desugar_expr env ret; ret_mode; loc }
  | Tuple { elts } -> Tuple { elts = Nonempty_list.map elts ~f:(desugar_expr env); loc }
  | Assert { cond; erased; _ } ->
    let prim =
      match erased with
      | Erased -> Dst.Expr.Builtin { builtin = Prim Assert_erased; loc }
      | Unerased -> Dst.Expr.Builtin { builtin = Prim Assert; loc }
    in
    Apply { fn = prim; arg = desugar_expr env cond; loc }
  | Unreachable -> Unreachable { loc }
  | Type_annotation { expr; ty } ->
    Type_annotation { expr = desugar_expr env expr; ty = desugar_expr env ty; loc }
  | Mode_annotation { expr; mode } -> Mode_annotation { expr = desugar_expr env expr; mode; loc }

and desugar_fun env ({ node = f; loc; _ } : Cst.Expr.fun_) (var : Ident.t) : Dst.Expr.fun_ =
  let arg_node = f.arg.node in
  let arg_ty = desugar_expr env arg_node.ty in
  let env', arg = Env.bind env arg_node.var in
  let env_ret = if arg_is_static arg_node.mode then env' else env in
  let ret_ty = desugar_expr env_ret f.ret_ty in
  let body = desugar_expr env' f.body in
  { var
  ; arg
  ; erased = f.erased
  ; arg_mode = arg_node.mode
  ; arg_ty
  ; ret_mode = f.ret_mode
  ; ret_ty
  ; body
  ; loc
  }
;;

let desugar_top_level env (t : Cst.Top_level.t) : Env.t * Dst.Top_level.t =
  let loc = t.loc in
  match t.node with
  | Let { var; erased; bind; _ } ->
    let bind = maybe_erased erased (desugar_expr env bind) loc in
    let env, var = Env.bind env var in
    env, Let { var; bind; loc }
  | Fun { funs } ->
    let env, rev_pairs =
      Nonempty_list.fold funs ~init:(env, []) ~f:(fun (env, acc) f ->
        let env, var = Env.bind env f.node.var in
        env, (f, var) :: acc)
    in
    let pairs = List.rev rev_pairs in
    let funs =
      List.map pairs ~f:(fun (f, var) -> desugar_fun env f var) |> Nonempty_list.of_list_exn
    in
    env, Fun { funs; loc }
  | External { var; ty; symbol } ->
    let ty = desugar_expr env ty in
    let env, var = Env.bind env var in
    env, External { var; ty; symbol; loc }
  | Builtin { var; name } ->
    let env, var = Env.bind env var in
    env, Builtin { var; name; loc }
;;

let fold_top_levels env items tls =
  List.fold items ~init:(env, tls) ~f:(fun (env, acc) top_level ->
    let env, tl = desugar_top_level env top_level in
    env, tl :: acc)
;;

let stdlib = (Parse.parse_exn Syl_std.source).items

let desugar (cst : Cst.Program.t) : Dst.Program.t =
  let env = Env.create () in
  let env, program = fold_top_levels env stdlib [] in
  let _, program = fold_top_levels env cst.items program in
  { top_levels = List.rev program; stamp = !(env.stamp) }
;;
