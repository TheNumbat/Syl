open! Core
open Modes
open Ir

let erased expr = Modes.is_erased (Tst.Expr.mode expr)

let rec simplify_ty ~loc (ty : Tst.Value.t) : Ty.t =
  match ty with
  | Type Unit -> Unit
  | Type Bool -> Bool
  | Type Int -> Int
  | Type Type -> Unit
  | Type (Arrow { arg_ty; ret_ty; _ }) ->
    Arrow { arg_ty = simplify_ty ~loc arg_ty; ret_ty = simplify_ty ~loc ret_ty }
  | Type (Pi _) -> Pack
  | _ -> raise_s [%message "Cannot simplify type" (ty : Tst.Value.t) (loc : Lex.Location.t)]

and simplify_bool ~loc (bool : Tst.Bool.t) : bool =
  match bool with
  | T bool -> bool
  | _ -> raise_s [%message "Cannot simplify bool" (bool : Tst.Bool.t) (loc : Lex.Location.t)]

and simplify_int ~loc (int : Tst.Int.t) : int64 =
  match int with
  | T int -> int
  | _ -> raise_s [%message "Cannot simplify int" (int : Tst.Int.t) (loc : Lex.Location.t)]

and simplify_closure ~loc ({ arg; ty; body; _ } : Tst.Closure.t) : Closure.t =
  { arg; ty = simplify_ty ~loc ty; body = simplify body }

and simplify_literal ~loc (value : Tst.Value.t) : Value.t =
  match value with
  | Unit -> Unit
  | Bool value -> Bool (simplify_bool ~loc value)
  | Int value -> Int (simplify_int ~loc value)
  | Type _ -> Unit
  | Closure value -> Closure (simplify_closure ~loc value)
  | Binder binder -> Pack (simplify_mono ~loc binder.mono)
  | External { symbol; ty } -> External { symbol; ty = simplify_ty ~loc ty }
  | Var _ | If _ | Apply _ ->
    raise_s [%message "Cannot simplify literal" (value : Tst.Value.t) (loc : Lex.Location.t)]

and simplify_mono ~loc mono =
  let pack = Vec.create ~initial_capacity:(Hashtbl.length mono) () in
  Hashtbl.iteri
    mono
    ~f:(fun ~key ~data:{ Tst.Binder.Mono.arg; arg_val; arg_ty; arg_mode; body; body_desc } ->
      let body =
        if Modes.is_erased arg_mode
        then simplify body
        else
          Expr.Let
            { var = arg
            ; bind =
                Literal { value = simplify_literal ~loc arg_val; ty = simplify_ty ~loc arg_ty; loc }
            ; rest = simplify body
            ; loc
            }
      in
      Vec.push_back pack { Pack.Mono.arg = key; ty = simplify_ty ~loc body_desc.ty; body });
  pack

and simplify (expr : Tst.Expr.t) : Expr.t =
  match expr with
  | Literal { value; ty; mode = _; loc } ->
    (* Ok for inlined literals to be erased *)
    Literal { value = simplify_literal ~loc value; ty = simplify_ty ~loc ty; loc }
  | Fun { funs; rest; loc } ->
    let rest = simplify rest in
    let funs = simplify_funs funs in
    (match Nonempty_list.of_list funs with
     | Some funs -> Fun { funs; rest; loc }
     | None -> rest)
  | Lambda { arg; ty; body; mode; loc } ->
    assert (not (Modes.is_erased mode));
    Lambda { arg; ty = simplify_ty ~loc ty; body = simplify body; loc }
  | Apply { fn; arg; ty; mode; loc } ->
    assert (not (Modes.is_erased mode));
    let arg = simplify arg in
    (match simplify fn with
     | Lambda { arg = var; body; _ } -> Let { var; bind = arg; rest = body; loc }
     | fn -> Apply { fn; arg; ty = simplify_ty ~loc ty; loc })
  | Symbol { id; arg; ty; mode; loc } ->
    assert (not (Modes.is_erased mode));
    Symbol { id; arg; ty = simplify_ty ~loc ty; loc }
  | Let { var; bind; rest; loc; _ } ->
    let rest = simplify rest in
    if erased bind then rest else Let { var; bind = simplify bind; rest; loc }
  | Unop { op; arg; ty; mode; loc } ->
    assert (not (Modes.is_erased mode));
    let arg = simplify arg in
    (match op, arg with
     | Neg, Literal { value = Int i; loc; _ } ->
       Literal { value = Int (Int64.neg i); ty = Int; loc }
     | Not, Literal { value = Bool b; loc; _ } -> Literal { value = Bool (not b); ty = Bool; loc }
     | _ -> Unop { op; arg; ty = simplify_ty ~loc ty; loc })
  | Binop { op; lhs; rhs; ty; mode; loc } ->
    assert (not (Modes.is_erased mode));
    let lhs = simplify lhs in
    let rhs = simplify rhs in
    (match op, lhs, rhs with
     | Add, Literal { value = Int l; loc; _ }, Literal { value = Int r; _ } ->
       Literal { value = Int (Int64.( + ) l r); ty = Int; loc }
     | Sub, Literal { value = Int l; loc; _ }, Literal { value = Int r; _ } ->
       Literal { value = Int (Int64.( - ) l r); ty = Int; loc }
     | Mul, Literal { value = Int l; loc; _ }, Literal { value = Int r; _ } ->
       Literal { value = Int (Int64.( * ) l r); ty = Int; loc }
     | Div, Literal { value = Int l; loc; _ }, Literal { value = Int r; _ } ->
       Literal { value = Int (Int64.( / ) l r); ty = Int; loc }
     | Mod, Literal { value = Int l; loc; _ }, Literal { value = Int r; _ } ->
       Literal { value = Int (Int64.( % ) l r); ty = Int; loc }
     | And, Literal { value = Bool l; loc; _ }, Literal { value = Bool r; _ } ->
       Literal { value = Bool (l && r); ty = Bool; loc }
     | Or, Literal { value = Bool l; loc; _ }, Literal { value = Bool r; _ } ->
       Literal { value = Bool (l || r); ty = Bool; loc }
     | Eq, Literal { value = Int l; loc; _ }, Literal { value = Int r; _ } ->
       Literal { value = Bool (Int64.( = ) l r); ty = Bool; loc }
     | Neq, Literal { value = Int l; loc; _ }, Literal { value = Int r; _ } ->
       Literal { value = Bool (Int64.( <> ) l r); ty = Bool; loc }
     | Lt, Literal { value = Int l; loc; _ }, Literal { value = Int r; _ } ->
       Literal { value = Bool (Int64.( < ) l r); ty = Bool; loc }
     | Lte, Literal { value = Int l; loc; _ }, Literal { value = Int r; _ } ->
       Literal { value = Bool (Int64.( <= ) l r); ty = Bool; loc }
     | Gt, Literal { value = Int l; loc; _ }, Literal { value = Int r; _ } ->
       Literal { value = Bool (Int64.( > ) l r); ty = Bool; loc }
     | Gte, Literal { value = Int l; loc; _ }, Literal { value = Int r; _ } ->
       Literal { value = Bool (Int64.( <= ) l r); ty = Bool; loc }
     | _ -> Binop { op; lhs; rhs; ty = simplify_ty ~loc ty; loc })
  | If { cond; then_; else_; ty; mode; loc } ->
    assert (not (Modes.is_erased mode));
    let cond = simplify cond in
    (match cond with
     | Literal { value = Bool b; _ } -> if b then simplify then_ else simplify else_
     | _ ->
       If { cond; then_ = simplify then_; else_ = simplify else_; ty = simplify_ty ~loc ty; loc })
  | Var { id; ty; mode; loc } ->
    assert (not (Modes.is_erased mode));
    Var { id; ty = simplify_ty ~loc ty; loc }
  | Binder { mono; mode; loc; _ } ->
    assert (not (Modes.is_erased mode));
    Pack { pack = simplify_mono ~loc mono; loc }
  | Erased _ -> assert false

and simplify_funs (funs : Tst.Expr.fun_ Nonempty_list.t) =
  Nonempty_list.filter_map funs ~f:(function
    | Lambda { var; arg; body; ty; mode; loc } ->
      if Modes.is_erased mode
      then None
      else Some (Expr.Mono { var; arg; body = simplify body; ty = simplify_ty ~loc ty; loc })
    | Binder { var; mono; mode; loc; _ } ->
      if Modes.is_erased mode
      then None
      else Some (Expr.Pack { var; pack = simplify_mono ~loc mono; loc }))
;;

let simplify_top_level (tst : Tst.Top_level.t) : Top_level.t Option.t =
  match tst with
  | Let { var; bind; loc } ->
    if erased bind then None else Some (Let { var; bind = simplify bind; loc })
  | Fun { funs; loc } ->
    let funs = simplify_funs funs in
    (match Nonempty_list.of_list funs with
     | Some funs -> Some (Fun { funs; loc })
     | None -> None)
  | External { var; symbol; ty; loc } ->
    Some (External { var; symbol; ty = simplify_ty ~loc ty; loc })
;;

let simplify (tst : Tst.Program.t) =
  let top_levels =
    List.fold tst ~init:[] ~f:(fun acc top_level ->
      match simplify_top_level top_level with
      | Some top_level -> top_level :: acc
      | None -> acc)
  in
  List.rev top_levels
;;
