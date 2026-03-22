open! Core
open Modes

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

let rec desugar_expr (env : Env.t) (e : Cst.Expr.t) : Dst.Expr.t =
  match e with
  | If { cond; then_; else_; static; loc } ->
    If
      { cond = desugar_expr env cond
      ; then_ = desugar_expr env then_
      ; else_ = desugar_expr env else_
      ; static
      ; loc
      }
  | Let { var; bind; rest; loc } ->
    let bind = desugar_expr env bind in
    let env, var = Env.bind env var in
    Let { var; bind; rest = desugar_expr env rest; loc }
  | Fun { funs; rest; loc } ->
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
  | Lambda { arg; erased; arg_mode; arg_ty; body; loc } ->
    let arg_ty = desugar_expr env arg_ty in
    let env, arg = Env.bind env arg in
    Lambda { arg; erased; arg_mode; arg_ty; body = desugar_expr env body; loc }
  | Apply { fn; arg; loc } -> Apply { fn = desugar_expr env fn; arg = desugar_expr env arg; loc }
  | Paren { expr; _ } -> desugar_expr env expr
  | Var { id; loc } -> Var { id = Env.find env id; loc }
  | Literal { value; loc } -> Literal { value; loc }
  | Unop { op; arg; loc } -> Unop { op; arg = desugar_expr env arg; loc }
  | Binop { op; lhs; rhs; loc } ->
    Binop { op; lhs = desugar_expr env lhs; rhs = desugar_expr env rhs; loc }
  | Nop { op; elts; loc } -> Nop { op; elts = List.map elts ~f:(desugar_expr env); loc }
  | Arrow { arg; arg_id; arg_mode; ret; ret_mode; loc } ->
    let arg = desugar_expr env arg in
    let arg_id = Option.value_or_thunk arg_id ~default:(fun () -> Ident.Raw.anon) in
    let env, arg_id = Env.bind env arg_id in
    Arrow { arg; arg_id; arg_mode; ret = desugar_expr env ret; ret_mode; loc }
  | Assert { cond; static; loc } -> Assert { cond = desugar_expr env cond; static; loc }
  | Unreachable { loc } -> Unreachable { loc }
  | Type_annotation { expr; ty; loc } ->
    Type_annotation { expr = desugar_expr env expr; ty = desugar_expr env ty; loc }
  | Mode_annotation { expr; mode; loc } ->
    Mode_annotation { expr = desugar_expr env expr; mode; loc }

and desugar_fun (env : Env.t) (f : Cst.Expr.fun_) (var : Ident.t) : Dst.Expr.fun_ =
  let arg_ty = desugar_expr env f.arg_ty in
  let env', arg = Env.bind env f.arg in
  let ret_ty =
    if arg_is_static f.arg_mode then desugar_expr env' f.ret_ty else desugar_expr env f.ret_ty
  in
  { var
  ; arg
  ; erased = f.erased
  ; arg_mode = f.arg_mode
  ; arg_ty
  ; ret_mode = f.ret_mode
  ; ret_ty
  ; body = desugar_expr env' f.body
  ; loc = f.loc
  }
;;

let desugar_top_level (env : Env.t) (t : Cst.Top_level.t) : Env.t * Dst.Top_level.t =
  match t with
  | Let { var; bind; loc } ->
    let bind = desugar_expr env bind in
    let env, var = Env.bind env var in
    env, Let { var; bind; loc }
  | Fun { funs; loc } ->
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
  | External { var; ty; symbol; loc } ->
    let ty = desugar_expr env ty in
    let env, var = Env.bind env var in
    env, External { var; ty; symbol; loc }
  | Builtin { var; name; loc } ->
    let env, var = Env.bind env var in
    env, Builtin { var; name; loc }
;;

let fold_top_levels env cst tls =
  List.fold cst ~init:(env, tls) ~f:(fun (env, acc) top_level ->
    let env, tl = desugar_top_level env top_level in
    env, tl :: acc)
;;

let stdlib = Parse.parse_exn Syl_stdlib.source

let desugar (cst : Cst.Program.t) : Dst.Program.t =
  let env = Env.create () in
  let env, program = fold_top_levels env stdlib [] in
  let _, program = fold_top_levels env cst program in
  List.rev program
;;
