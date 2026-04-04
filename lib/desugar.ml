open! Core

module Env = struct
  type t =
    { vars : int Ident.Raw.Map.t
    ; mutable stamp : int
    }

  let create () = { vars = Ident.Raw.Map.empty; stamp = 0 }

  let bind t raw =
    let stamp = t.stamp in
    t.stamp <- stamp + 1;
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
  | _ -> false
;;

let maybe_erased (erased : Modes.Erasure.t) (expr : Dst.Expr.t) loc : Dst.Expr.t =
  match erased with
  | Erased -> Mode_annotation { expr; mode = { staticity = None; erasure = Some Erased }; loc }
  | Unerased -> expr
;;

let rec desugar_expr (env : Env.t) (expr : Cst.Expr.t) : Dst.Expr.t =
  let loc = expr.loc in
  match expr.node with
  | If { cond; then_; else_; static } ->
    If
      { cond = desugar_expr env cond
      ; then_ = desugar_expr env then_
      ; else_ = desugar_expr env else_
      ; static
      ; loc
      }
  | Let { var; erased; args; bind; rest } ->
    let bind =
      let bind =
        match args with
        | [] -> desugar_expr env bind
        | arg :: rest ->
          desugar_expr
            env
            (Cst.With_loc.create
               ~loc:arg.loc
               (Cst.Expr.Lambda { erased; args = Nonempty_list.create arg rest; body = bind }))
      in
      maybe_erased erased bind loc
    in
    let env, var = Env.bind env var in
    Let { var; bind; rest = desugar_expr env rest; loc }
  | Fun { funs; rest } ->
    let env, rev_pairs =
      Nonempty_list.fold funs ~init:(env, []) ~f:(fun (env, acc) f ->
        let env, var = Env.bind env f.var in
        env, (f, var) :: acc)
    in
    let pairs = List.rev rev_pairs in
    let funs =
      List.map pairs ~f:(fun (f, var) -> desugar_fun env f var) |> Nonempty_list.of_list_exn
    in
    Fun { funs; rest = desugar_expr env rest; loc }
  | Lambda { erased; args; body } ->
    let first = Nonempty_list.hd args in
    let rest = Nonempty_list.tl args in
    let arg_ty = desugar_expr env first.ty in
    let env, arg = Env.bind env first.var in
    let body =
      match rest with
      | [] -> desugar_expr env body
      | arg :: rest ->
        desugar_expr
          env
          (Cst.With_loc.create
             ~loc:arg.loc
             (Cst.Expr.Lambda { erased; args = Nonempty_list.create arg rest; body }))
    in
    let lambda : Dst.Expr.t = Lambda { arg; arg_mode = first.mode; arg_ty; body; loc } in
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
      ; arg = Nop { op = Comma; elts = [ lhs; rhs ]; loc }
      ; loc
      }
  | Nop { op; elts } -> Nop { op; elts = List.map elts ~f:(desugar_expr env); loc }
  | Arrow { arg; arg_id; arg_mode; ret; ret_mode } ->
    let arg = desugar_expr env arg in
    let arg_id = Option.value_or_thunk arg_id ~default:(fun () -> Ident.Raw.anon) in
    let env, arg_id = Env.bind env arg_id in
    Arrow { arg; arg_id; arg_mode; ret = desugar_expr env ret; ret_mode; loc }
  | Assert { cond; static } -> Assert { cond = desugar_expr env cond; static; loc }
  | Unreachable -> Unreachable { loc }
  | Type_annotation { expr; ty } ->
    Type_annotation { expr = desugar_expr env expr; ty = desugar_expr env ty; loc }
  | Mode_annotation { expr; mode } -> Mode_annotation { expr = desugar_expr env expr; mode; loc }

and desugar_fun (env : Env.t) (f : Cst.Expr.fun_) (var : Ident.t) : Dst.Expr.fun_ =
  let first = Nonempty_list.hd f.args in
  let rest = Nonempty_list.tl f.args in
  let arg_ty = desugar_expr env first.ty in
  let env', arg = Env.bind env first.var in
  let anon_ret_mode =
    match f.erased with
    | Erased -> { Modes.Maybe.staticity = Some Static; erasure = Some Erased }
    | Unerased -> { Modes.Maybe.staticity = Some Static; erasure = None }
  in
  let ret_ty, ret_mode =
    let env_ret = if arg_is_static first.mode then env' else env in
    let ret_ty, ret_mode =
      List.fold_right
        rest
        ~init:(f.ret_ty, f.ret_mode)
        ~f:(fun (arg : Cst.Expr.arg) (ret, ret_mode) ->
          ( Cst.With_loc.create
              ~loc:arg.loc
              (Cst.Expr.Arrow
                 { arg = arg.ty; arg_id = Some arg.var; arg_mode = arg.mode; ret; ret_mode })
          , anon_ret_mode ))
    in
    desugar_expr env_ret ret_ty, ret_mode
  in
  let body =
    (match rest with
     | [] -> f.body
     | arg :: rest ->
       Cst.With_loc.create
         ~loc:arg.loc
         (Cst.Expr.Lambda { erased = f.erased; args = Nonempty_list.create arg rest; body = f.body }))
    |> desugar_expr env'
  in
  { var
  ; arg
  ; erased = f.erased
  ; arg_mode = first.mode
  ; arg_ty
  ; ret_mode
  ; ret_ty
  ; body
  ; loc = f.loc
  }
;;

let desugar_top_level (env : Env.t) (t : Cst.Top_level.t) : Env.t * Dst.Top_level.t =
  let loc = t.loc in
  match t.node with
  | Let { var; erased; args; bind } ->
    let bind =
      let bind =
        match args with
        | [] -> desugar_expr env bind
        | a :: rest_args ->
          desugar_expr
            env
            (Cst.With_loc.create
               ~loc:a.loc
               (Cst.Expr.Lambda { erased; args = Nonempty_list.create a rest_args; body = bind }))
      in
      maybe_erased erased bind loc
    in
    let env, var = Env.bind env var in
    env, Let { var; bind; loc }
  | Fun { funs } ->
    let env, rev_pairs =
      Nonempty_list.fold funs ~init:(env, []) ~f:(fun (env, acc) f ->
        let env, var = Env.bind env f.var in
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
  List.rev program
;;
