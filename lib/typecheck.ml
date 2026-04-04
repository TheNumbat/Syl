open! Core
open Tst
open Option.Let_syntax

module Error = struct
  type t =
    | Unbound_ident of Ident.t
    | Mode_mismatch of
        { got : Modes.t
        ; need : Modes.t (* TODO why *)
        }
    | Type_mismatch of
        { got : Value.t
        ; need : Value.t
        }
    | Cannot_unify of
        { lhs : Value.t
        ; rhs : Value.t
        }
    | Inline_self of Ident.t Nonempty_list.t
    | Static_external of Ident.t * string
    | Unknown_builtin of Ident.t * string
    | Recursion_limit of int
    | Static_assert of Value.t
    | Divide_by_zero of Int.t
    | Unreachable_reached
  [@@deriving sexp]
end

exception Error of Error.t Loc.t [@@deriving sexp]

module Fail = struct
  let mode_mismatch ~loc got need = raise (Error { loc; reason = Mode_mismatch { got; need } })
  let type_mismatch ~loc got need = raise (Error { loc; reason = Type_mismatch { got; need } })
  let cannot_unify ~loc lhs rhs = raise (Error { loc; reason = Cannot_unify { lhs; rhs } })
  let unbound_ident ~loc id = raise (Error { loc; reason = Unbound_ident id })
  let recursion_limit ~loc limit = raise (Error { loc; reason = Recursion_limit limit })
  let inline_self ~loc id = raise (Error { loc; reason = Inline_self id })
  let static_external ~loc id sym = raise (Error { loc; reason = Static_external (id, sym) })
  let unknown_builtin ~loc id name = raise (Error { loc; reason = Unknown_builtin (id, name) })
  let static_assert ~loc expr = raise (Error { loc; reason = Static_assert expr })
  let divide_by_zero ~loc expr = raise (Error { loc; reason = Divide_by_zero expr })
  let unreachable_reached ~loc = raise (Error { loc; reason = Unreachable_reached })

  let unreachable here ~loc =
    Lazy.from_fun (fun () ->
      raise_s
        [%message "Bug: forced dynamic" (loc : Lex.Location.t) (here : Source_code_position.t)])
  ;;
end

module State = struct
  type t =
    { mutable next_id : int
    ; mutable depth : int
    ; mutable app_depth : int
    ; mutable abs_depth : int
    }

  let recursion_limit = 1000
  let create () = { next_id = 0; depth = 0; app_depth = 0; abs_depth = 0 }
  let concrete t = t.abs_depth = 0
  let monomorphizing t = t.app_depth > 0

  let recur ~loc t ~f =
    if t.depth > recursion_limit
    then Fail.recursion_limit ~loc recursion_limit
    else (
      t.depth <- t.depth + 1;
      Exn.protect ~f ~finally:(fun () -> t.depth <- t.depth - 1))
  ;;

  let with_app t ~f =
    t.app_depth <- t.app_depth + 1;
    Exn.protect ~f ~finally:(fun () -> t.app_depth <- t.app_depth - 1)
  ;;

  let with_abs t ~f =
    t.abs_depth <- t.abs_depth + 1;
    Exn.protect ~f ~finally:(fun () -> t.abs_depth <- t.abs_depth - 1)
  ;;

  let fresh_id t =
    let res = t.next_id in
    t.next_id <- t.next_id + 1;
    res
  ;;

  let fresh_var t = Value.Var (Ident.create Ident.Raw.anon ~stamp:(fresh_id t))
end

let rec concrete state (v : Value.t) : Value.Concrete.t option =
  match v with
  | Unit -> Some Unit
  | Bool (T b) -> Some (Bool b)
  | Int (T i) -> Some (Int i)
  | Tuple elts ->
    let elts = List.map elts ~f:(concrete state) in
    Option.map (Option.all elts) ~f:(fun elts -> Value.Concrete.Tuple elts)
  | Closure _ | Binder _ | Prim _ -> Some (Closure (State.fresh_id state))
  | Type Unit -> Some (Scalar Unit)
  | Type Bool -> Some (Scalar Bool)
  | Type Int -> Some (Scalar Int)
  | Type Type -> Some (Scalar Type)
  | Type (Arrow { arg_ty; arg_mode; ret_ty; ret_mode })
  | Type (Pi { arg_ty; arg_mode; ret_ty = T ret_ty; ret_mode }) ->
    let%map arg = concrete state arg_ty
    and ret = concrete state ret_ty in
    Value.Concrete.Arrow { arg; arg_mode; ret; ret_mode }
  | Type (Tuple elts) ->
    let%map elts = Option.all (List.map elts ~f:(concrete state)) in
    Value.Concrete.Tuple elts
  | Bottom | Bool _ | Int _ | Var _ | If _ | Apply _ | External _ | Type (Pi _) -> None
;;

let weaken ~loc expr { Desc.ty = src_ty; mode = src_mode; static } ~ty:dst_ty ~mode:dst_mode =
  let expr : Expr.t =
    if Modes.is_unerased src_mode && Modes.is_erased dst_mode
    then (
      let loc = Expr.loc expr in
      if Modes.is_static dst_mode
      then Literal { value = Lazy.force static; ty = src_ty; mode = src_mode; loc }
      else Erased { ty = src_ty; mode = src_mode; loc })
    else expr
  in
  let static =
    if Modes.is_static src_mode && Modes.is_dynamic dst_mode
    then Fail.unreachable [%here] ~loc
    else static
  in
  Expr.with_ expr ~ty:dst_ty ~mode:dst_mode, { Desc.ty = dst_ty; mode = dst_mode; static }
;;

let require_mode ~loc src dst = if not (Modes.leq src dst) then Fail.mode_mismatch ~loc src dst

let require_dynamic_arrow ~loc var sym (ty : Value.t) =
  match ty with
  | Type (Arrow { arg_mode; ret_mode; _ } | Pi { arg_mode; ret_mode; _ }) ->
    if Modes.is_static arg_mode || Modes.is_static ret_mode then Fail.static_external ~loc var sym
  | _ -> Fail.static_external ~loc var sym
;;

let require_static ~loc (desc : Desc.t) =
  require_mode ~loc desc.mode (Modes.top ~staticity:Static ())
;;

let require_unerased ~loc (desc : Desc.t) =
  require_mode ~loc desc.mode (Modes.top ~erasure:Unerased ())
;;

let require_var ~loc env id =
  match Env.find env id with
  | Some value -> value
  | None -> Fail.unbound_ident ~loc id
;;

let inline ~loc ~(env : Env.t) ~arg_id ~arg ~arg_mode body =
  let ty = Expr.ty body in
  let mode = Expr.mode body in
  let fvs = Set.remove (Expr.free_vars body) arg_id in
  let body =
    if Modes.is_erased arg_mode
    then body
    else Expr.Let { var = arg_id; bind = arg; rest = body; ty; mode; loc }
  in
  Set.fold fvs ~init:body ~f:(fun acc id ->
    match Map.find env id with
    | Some (desc : Desc.t) when not (Modes.is_erased desc.mode) ->
      (* TODO allow inlining with dynamics if they're in scope at the call site *)
      require_static ~loc desc;
      (try
         let value = Lazy.force desc.static in
         let bind = Expr.Literal { value; ty = desc.ty; mode = desc.mode; loc } in
         Expr.Let { var = id; bind; rest = acc; ty; mode; loc }
       with
       | Lazy.Undefined -> acc)
    | _ -> acc)
;;

let rebind_if_var env (expr : Expr.t) value =
  (* TODO do something more expressive here *)
  match expr with
  | Var { id; ty; mode; _ } -> Env.bind env id { Desc.ty; mode; static = Lazy.from_val value }
  | _ -> env
;;

let rec require_leq state ~loc src dst =
  if not (leq_value state src dst) then Fail.type_mismatch ~loc src dst

and require_join state ~loc ty1 ty2 =
  match join_value state ty1 ty2 with
  | Some ty -> ty
  | None -> Fail.cannot_unify ~loc ty1 ty2

and require_static_type ~loc state (desc : Desc.t) =
  require_static ~loc desc;
  require_leq state ~loc desc.ty (Type Type);
  Lazy.force desc.static

and eval state (dep : Dependent.t) (arg_val : Value.t) : Value.t =
  (* We've already typechecked forall args, so these can't fail. *)
  match dep with
  | T a -> a
  | Meet (a, b) -> Option.value_exn (meet_value state (eval state a arg_val) (eval state b arg_val))
  | Join (a, b) -> Option.value_exn (join_value state (eval state a arg_val) (eval state b arg_val))
  | Reduce { env; arg; arg_ty; arg_mode; memo; ret_ty } ->
    let reduce () =
      let env = Env.bind env arg { ty = arg_ty; mode = arg_mode; static = Lazy.from_val arg_val } in
      let ret_ty_desc = reduce state env ret_ty in
      Lazy.force ret_ty_desc.static
    in
    (match concrete state arg_val with
     | Some arg_concrete ->
       Hashtbl.update_and_return memo arg_concrete ~f:(function
         | None -> State.with_app state ~f:reduce
         | Some ty -> ty)
     | None -> reduce ())
  | Typecheck { env; arg; arg_ty; arg_mode; memo; body } ->
    let reduce () =
      let env = Env.bind env arg { ty = arg_ty; mode = arg_mode; static = Lazy.from_val arg_val } in
      let body_desc = reduce state env body in
      body_desc.ty
    in
    (match concrete state arg_val with
     | Some arg_concrete ->
       Hashtbl.update_and_return memo arg_concrete ~f:(function
         | None -> State.with_app state ~f:reduce
         | Some ty -> ty)
     | None -> reduce ())

and leq_value state (a : Value.t) (b : Value.t) =
  match a, b with
  | Bottom, _ -> true
  | _, Bottom -> false
  | Unit, Unit -> true
  | Bool a, Bool b -> leq_bool state a b
  | Int a, Int b -> leq_int state a b
  | Type a, Type b -> leq_ty state a b
  | Closure a, Closure b -> leq_closure a b
  | Binder a, Binder b -> leq_binder a b
  | Var a, Var b -> Ident.equal a b
  | Prim a, Prim b -> Builtin.Prim.equal a b
  | Tuple a_elts, Tuple b_elts ->
    (match List.for_all2 a_elts b_elts ~f:(leq_value state) with
     | Ok leq -> leq
     | Unequal_lengths -> false)
  | External a, External b -> String.equal a.symbol b.symbol
  | ( If { cond = a_cond; then_ = a_then; else_ = a_else }
    , If { cond = b_cond; then_ = b_then; else_ = b_else } ) ->
    leq_value state a_cond b_cond && leq_value state a_then b_then && leq_value state a_else b_else
  | If { then_; else_; _ }, b -> leq_value state then_ b && leq_value state else_ b
  | a, If { then_; else_; _ } -> leq_value state a then_ && leq_value state a else_
  | Apply { fn = a_fn; arg = a_arg }, Apply { fn = b_fn; arg = b_arg } ->
    leq_value state a_fn b_fn && leq_value state a_arg b_arg
  | ( ( Unit
      | Bool _
      | Int _
      | Type _
      | Closure _
      | Binder _
      | Var _
      | Apply _
      | External _
      | Tuple _
      | Prim _ )
    , _ ) -> false

and geq_value state (a : Value.t) (b : Value.t) =
  match a, b with
  | _, Bottom -> true
  | Bottom, _ -> false
  | Unit, Unit -> true
  | Bool a, Bool b -> geq_bool state a b
  | Int a, Int b -> geq_int state a b
  | Type a, Type b -> geq_ty state a b
  | Closure a, Closure b -> geq_closure a b
  | Binder a, Binder b -> geq_binder a b
  | Var a, Var b -> Ident.equal a b
  | Prim a, Prim b -> Builtin.Prim.equal a b
  | Tuple a_elts, Tuple b_elts ->
    (match List.for_all2 a_elts b_elts ~f:(geq_value state) with
     | Ok leq -> leq
     | Unequal_lengths -> false)
  | External a, External b -> String.equal a.symbol b.symbol
  | ( If { cond = a_cond; then_ = a_then; else_ = a_else }
    , If { cond = b_cond; then_ = b_then; else_ = b_else } ) ->
    geq_value state a_cond b_cond && geq_value state a_then b_then && geq_value state a_else b_else
  | If { then_; else_; _ }, b -> geq_value state then_ b && geq_value state else_ b
  | a, If { then_; else_; _ } -> geq_value state a then_ && geq_value state a else_
  | Apply { fn = a_fn; arg = a_arg }, Apply { fn = b_fn; arg = b_arg } ->
    geq_value state a_fn b_fn && geq_value state a_arg b_arg
  | ( ( Unit
      | Bool _
      | Int _
      | Type _
      | Closure _
      | Binder _
      | Var _
      | Apply _
      | External _
      | Tuple _
      | Prim _ )
    , _ ) -> false

and join_value state (a : Value.t) (b : Value.t) : Value.t Option.t =
  match a, b with
  | a, Bottom -> Some a
  | Bottom, b -> Some b
  | Unit, Unit -> Some Unit
  | Bool a, Bool b -> Option.map (join_bool state a b) ~f:(fun b : Value.t -> Bool b)
  | Int a, Int b -> Option.map (join_int state a b) ~f:(fun i : Value.t -> Int i)
  | Type a, Type b -> Option.map (join_ty state a b) ~f:(fun ty : Value.t -> Type ty)
  | Closure a, Closure b -> Option.map (join_closure a b) ~f:(fun c : Value.t -> Closure c)
  | Binder a, Binder b -> Option.map (join_binder a b) ~f:(fun p : Value.t -> Binder p)
  | Var a, Var b when Ident.equal a b -> Some (Var a)
  | Prim a, Prim b when Builtin.Prim.equal a b -> Some (Prim a)
  | Tuple a_elts, Tuple b_elts ->
    (match List.map2 a_elts b_elts ~f:(join_value state) with
     | Ok elts -> Option.map (Option.all elts) ~f:(fun elts -> Value.Tuple elts)
     | Unequal_lengths -> None)
  | External a, External b when String.equal a.symbol b.symbol -> Some (External a)
  | ( If { cond = a_cond; then_ = a_then; else_ = a_else }
    , If { cond = b_cond; then_ = b_then; else_ = b_else } ) ->
    let%map cond = join_value state a_cond b_cond
    and then_ = join_value state a_then b_then
    and else_ = join_value state a_else b_else in
    Value.If { cond; then_; else_ }
  | If { then_; else_; _ }, b ->
    let%bind then_ = join_value state then_ b
    and else_ = join_value state else_ b in
    join_value state then_ else_
  | a, If { then_; else_; _ } ->
    let%bind then_ = join_value state a then_
    and else_ = join_value state a else_ in
    join_value state then_ else_
  | Apply { fn = a_fn; arg = a_arg }, Apply { fn = b_fn; arg = b_arg } ->
    let%map fn = join_value state a_fn b_fn
    and arg = join_value state a_arg b_arg in
    Value.Apply { fn; arg }
  | ( ( Unit
      | Bool _
      | Int _
      | Type _
      | Closure _
      | Binder _
      | Var _
      | Apply _
      | External _
      | Tuple _
      | Prim _ )
    , _ ) -> None

and meet_value state (a : Value.t) (b : Value.t) : Value.t Option.t =
  match a, b with
  | Bottom, _ | _, Bottom -> Some Bottom
  | Unit, Unit -> Some Unit
  | Bool a, Bool b -> Option.map (meet_bool state a b) ~f:(fun b : Value.t -> Bool b)
  | Int a, Int b -> Option.map (meet_int state a b) ~f:(fun i : Value.t -> Int i)
  | Type a, Type b -> Option.map (meet_ty state a b) ~f:(fun ty : Value.t -> Type ty)
  | Closure a, Closure b -> Option.map (meet_closure a b) ~f:(fun c : Value.t -> Closure c)
  | Binder a, Binder b -> Option.map (meet_binder a b) ~f:(fun p : Value.t -> Binder p)
  | Var a, Var b when Ident.equal a b -> Some (Var a)
  | Prim a, Prim b when Builtin.Prim.equal a b -> Some (Prim a)
  | Tuple a_elts, Tuple b_elts ->
    (match List.map2 a_elts b_elts ~f:(meet_value state) with
     | Ok elts -> Option.map (Option.all elts) ~f:(fun elts -> Value.Tuple elts)
     | Unequal_lengths -> None)
  | External a, External b when String.equal a.symbol b.symbol -> Some (External a)
  | ( If { cond = a_cond; then_ = a_then; else_ = a_else }
    , If { cond = b_cond; then_ = b_then; else_ = b_else } ) ->
    let%map cond = meet_value state a_cond b_cond
    and then_ = meet_value state a_then b_then
    and else_ = meet_value state a_else b_else in
    Value.If { cond; then_; else_ }
  | If { then_; else_; _ }, b ->
    let%bind then_ = meet_value state then_ b
    and else_ = meet_value state else_ b in
    meet_value state then_ else_
  | a, If { then_; else_; _ } ->
    let%bind then_ = meet_value state a then_
    and else_ = meet_value state a else_ in
    meet_value state then_ else_
  | Apply { fn = a_fn; arg = a_arg }, Apply { fn = b_fn; arg = b_arg } ->
    let%map fn = meet_value state a_fn b_fn
    and arg = meet_value state a_arg b_arg in
    Value.Apply { fn; arg }
  | ( ( Unit
      | Bool _
      | Int _
      | Type _
      | Closure _
      | Binder _
      | Var _
      | Apply _
      | External _
      | Tuple _
      | Prim _ )
    , _ ) -> None

and leq_closure _ _ = false
and geq_closure _ _ = false
and join_closure _ _ = None
and meet_closure _ _ = None
and leq_binder _ _ = false
and geq_binder _ _ = false
and join_binder _ _ = None
and meet_binder _ _ = None

(* Compares structurally. Could use a fancier solver. *)
and leq_bool state a b =
  match a, b with
  | T a, T b -> Core.Bool.equal a b
  | Not a, Not b -> leq_value state a b
  | And (a0, a1), And (b0, b1) | Or (a0, a1), Or (b0, b1) ->
    leq_value state a0 b0 && leq_value state a1 b1
  | Eq (a0, a1), Eq (b0, b1)
  | Neq (a0, a1), Neq (b0, b1)
  | Lt (a0, a1), Lt (b0, b1)
  | Lte (a0, a1), Lte (b0, b1)
  | Gt (a0, a1), Gt (b0, b1)
  | Gte (a0, a1), Gte (b0, b1) -> leq_value state a0 b0 && leq_value state a1 b1
  | _ -> false

and geq_bool state a b = leq_bool state a b

(* Compares structurally. Could use a fancier solver. *)
and join_bool state a b =
  match a, b with
  | T a, T b -> if Core.Bool.equal a b then Some (T a) else None
  | Not a, Not b -> Option.map (join_value state a b) ~f:(fun b : Bool.t -> Not b)
  | And (a0, a1), And (b0, b1) ->
    Option.map2 (join_value state a0 b0) (join_value state a1 b1) ~f:(fun a b : Bool.t ->
      And (a, b))
  | Or (a0, a1), Or (b0, b1) ->
    Option.map2 (join_value state a0 b0) (join_value state a1 b1) ~f:(fun a b : Bool.t -> Or (a, b))
  | Eq (a0, a1), Eq (b0, b1) ->
    Option.map2 (join_value state a0 b0) (join_value state a1 b1) ~f:(fun a b : Bool.t -> Eq (a, b))
  | Neq (a0, a1), Neq (b0, b1) ->
    Option.map2 (join_value state a0 b0) (join_value state a1 b1) ~f:(fun a b : Bool.t ->
      Neq (a, b))
  | Lt (a0, a1), Lt (b0, b1) ->
    Option.map2 (join_value state a0 b0) (join_value state a1 b1) ~f:(fun a b : Bool.t -> Lt (a, b))
  | Lte (a0, a1), Lte (b0, b1) ->
    Option.map2 (join_value state a0 b0) (join_value state a1 b1) ~f:(fun a b : Bool.t ->
      Lte (a, b))
  | Gt (a0, a1), Gt (b0, b1) ->
    Option.map2 (join_value state a0 b0) (join_value state a1 b1) ~f:(fun a b : Bool.t -> Gt (a, b))
  | Gte (a0, a1), Gte (b0, b1) ->
    Option.map2 (join_value state a0 b0) (join_value state a1 b1) ~f:(fun a b : Bool.t ->
      Gte (a, b))
  | _ -> None

and meet_bool state a b = join_bool state a b

(* Compares structurally. Could use a fancier solver. *)
and leq_int state a b =
  match a, b with
  | T a, T b -> Int64.equal a b
  | Neg a, Neg b -> leq_value state a b
  | Add (a0, a1), Add (b0, b1)
  | Sub (a0, a1), Sub (b0, b1)
  | Mul (a0, a1), Mul (b0, b1)
  | Div (a0, a1), Div (b0, b1)
  | Mod (a0, a1), Mod (b0, b1) -> leq_value state a0 b0 && leq_value state a1 b1
  | _ -> false

and geq_int state a b = leq_int state a b

(* Compares structurally. Could use a fancier solver. *)
and join_int state a b =
  match a, b with
  | T a, T b -> if Int64.equal a b then Some (T a) else None
  | Neg a, Neg b -> Option.map (join_value state a b) ~f:(fun i : Int.t -> Neg i)
  | Add (a0, a1), Add (b0, b1) ->
    Option.map2 (join_value state a0 b0) (join_value state a1 b1) ~f:(fun a b : Int.t -> Add (a, b))
  | Sub (a0, a1), Sub (b0, b1) ->
    Option.map2 (join_value state a0 b0) (join_value state a1 b1) ~f:(fun a b : Int.t -> Sub (a, b))
  | Mul (a0, a1), Mul (b0, b1) ->
    Option.map2 (join_value state a0 b0) (join_value state a1 b1) ~f:(fun a b : Int.t -> Mul (a, b))
  | Div (a0, a1), Div (b0, b1) ->
    Option.map2 (join_value state a0 b0) (join_value state a1 b1) ~f:(fun a b : Int.t -> Div (a, b))
  | Mod (a0, a1), Mod (b0, b1) ->
    Option.map2 (join_value state a0 b0) (join_value state a1 b1) ~f:(fun a b : Int.t -> Mod (a, b))
  | _ -> None

and meet_int state a b = join_int state a b

and leq_ty state a b =
  match a, b with
  | Unit, Unit | Bool, Bool | Int, Int | Type, Type -> true
  | Tuple a_elts, Tuple b_elts ->
    (match List.for_all2 a_elts b_elts ~f:(leq_value state) with
     | Ok leq -> leq
     | Unequal_lengths -> false)
  | ( Arrow { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Arrow { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } )
    ->
    Modes.geq a_arg_mode b_arg_mode
    && Modes.leq a_ret_mode b_ret_mode
    && geq_value state a_arg_ty b_arg_ty
    && leq_value state a_ret_ty b_ret_ty
  | ( Pi { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Pi { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } ) ->
    Modes.geq a_arg_mode b_arg_mode
    && Modes.leq a_ret_mode b_ret_mode
    && geq_value state a_arg_ty b_arg_ty
    &&
    let var = State.fresh_var state in
    leq_value state (eval state a_ret_ty var) (eval state b_ret_ty var)
  | ( Arrow { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Pi { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } ) ->
    Modes.geq a_arg_mode b_arg_mode
    && Modes.leq a_ret_mode b_ret_mode
    && geq_value state a_arg_ty b_arg_ty
    && leq_value state a_ret_ty (eval state b_ret_ty (State.fresh_var state))
  | ( Pi { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Arrow { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } )
    ->
    Modes.geq a_arg_mode b_arg_mode
    && Modes.leq a_ret_mode b_ret_mode
    && geq_value state a_arg_ty b_arg_ty
    && leq_value state (eval state a_ret_ty (State.fresh_var state)) b_ret_ty
  | (Unit | Bool | Int | Type | Arrow _ | Pi _ | Tuple _), _ -> false

and geq_ty state a b =
  match a, b with
  | Unit, Unit | Bool, Bool | Int, Int | Type, Type -> true
  | Tuple a_elts, Tuple b_elts ->
    (match List.for_all2 a_elts b_elts ~f:(geq_value state) with
     | Ok leq -> leq
     | Unequal_lengths -> false)
  | ( Arrow { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Arrow { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } )
    ->
    Modes.leq a_arg_mode b_arg_mode
    && Modes.geq a_ret_mode b_ret_mode
    && leq_value state a_arg_ty b_arg_ty
    && geq_value state a_ret_ty b_ret_ty
  | ( Pi { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Pi { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } ) ->
    Modes.leq a_arg_mode b_arg_mode
    && Modes.geq a_ret_mode b_ret_mode
    && leq_value state a_arg_ty b_arg_ty
    &&
    let var = State.fresh_var state in
    geq_value state (eval state a_ret_ty var) (eval state b_ret_ty var)
  | ( Arrow { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Pi { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } ) ->
    Modes.leq a_arg_mode b_arg_mode
    && Modes.geq a_ret_mode b_ret_mode
    && leq_value state a_arg_ty b_arg_ty
    && geq_value state a_ret_ty (eval state b_ret_ty (State.fresh_var state))
  | ( Pi { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Arrow { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } )
    ->
    Modes.leq a_arg_mode b_arg_mode
    && Modes.geq a_ret_mode b_ret_mode
    && leq_value state a_arg_ty b_arg_ty
    && geq_value state (eval state a_ret_ty (State.fresh_var state)) b_ret_ty
  | (Unit | Bool | Int | Type | Arrow _ | Pi _ | Tuple _), _ -> false

and join_ty state a b =
  match a, b with
  | Unit, Unit -> Some Unit
  | Bool, Bool -> Some Bool
  | Int, Int -> Some Int
  | Type, Type -> Some Type
  | Tuple a_elts, Tuple b_elts ->
    (match List.map2 a_elts b_elts ~f:(join_value state) with
     | Ok elts -> Option.map (Option.all elts) ~f:(fun elts -> Ty.Tuple elts)
     | Unequal_lengths -> None)
  | ( Arrow { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Arrow { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } )
    ->
    let%map arg_ty = meet_value state a_arg_ty b_arg_ty
    and ret_ty = join_value state a_ret_ty b_ret_ty in
    Ty.Arrow
      { arg_ty
      ; arg_mode = Modes.meet a_arg_mode b_arg_mode
      ; ret_ty
      ; ret_mode = Modes.join a_ret_mode b_ret_mode
      }
  | ( Pi { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Pi { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } ) ->
    let%bind arg_ty = meet_value state a_arg_ty b_arg_ty in
    let var = State.fresh_var state in
    let%map _ret_ty = join_value state (eval state a_ret_ty var) (eval state b_ret_ty var) in
    Ty.Pi
      { arg_ty
      ; arg_mode = Modes.meet a_arg_mode b_arg_mode
      ; ret_ty = Dependent.join a_ret_ty b_ret_ty
      ; ret_mode = Modes.join a_ret_mode b_ret_mode
      }
  | ( Arrow { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Pi { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } ) ->
    let%bind arg_ty = meet_value state a_arg_ty b_arg_ty in
    let%map _ret_ty = join_value state a_ret_ty (eval state b_ret_ty (State.fresh_var state)) in
    Ty.Pi
      { arg_ty
      ; arg_mode = Modes.meet a_arg_mode b_arg_mode
      ; ret_ty = Dependent.join (T a_ret_ty) b_ret_ty
      ; ret_mode = Modes.join a_ret_mode b_ret_mode
      }
  | ( Pi { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Arrow { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } )
    ->
    let%bind arg_ty = meet_value state a_arg_ty b_arg_ty in
    let%map _ret_ty = join_value state (eval state a_ret_ty (State.fresh_var state)) b_ret_ty in
    Ty.Pi
      { arg_ty
      ; arg_mode = Modes.meet a_arg_mode b_arg_mode
      ; ret_ty = Dependent.join a_ret_ty (T b_ret_ty)
      ; ret_mode = Modes.join a_ret_mode b_ret_mode
      }
  | (Unit | Bool | Int | Type | Arrow _ | Pi _ | Tuple _), _ -> None

and meet_ty state a b =
  match a, b with
  | Unit, Unit -> Some Unit
  | Bool, Bool -> Some Bool
  | Int, Int -> Some Int
  | Type, Type -> Some Type
  | Tuple a_elts, Tuple b_elts ->
    (match List.map2 a_elts b_elts ~f:(meet_value state) with
     | Ok elts -> Option.map (Option.all elts) ~f:(fun elts -> Ty.Tuple elts)
     | Unequal_lengths -> None)
  | ( Arrow { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Arrow { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } )
    ->
    let%map arg_ty = join_value state a_arg_ty b_arg_ty
    and ret_ty = meet_value state a_ret_ty b_ret_ty in
    Ty.Arrow
      { arg_ty
      ; arg_mode = Modes.join a_arg_mode b_arg_mode
      ; ret_ty
      ; ret_mode = Modes.meet a_ret_mode b_ret_mode
      }
  | ( Pi { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Pi { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } ) ->
    let%bind arg_ty = join_value state a_arg_ty b_arg_ty in
    let var = State.fresh_var state in
    let%map _ret_ty = meet_value state (eval state a_ret_ty var) (eval state b_ret_ty var) in
    Ty.Pi
      { arg_ty
      ; arg_mode = Modes.join a_arg_mode b_arg_mode
      ; ret_ty = Dependent.meet a_ret_ty b_ret_ty
      ; ret_mode = Modes.meet a_ret_mode b_ret_mode
      }
  | ( Arrow { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Pi { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } ) ->
    let%bind arg_ty = join_value state a_arg_ty b_arg_ty in
    let%map ret_ty = meet_value state a_ret_ty (eval state b_ret_ty (State.fresh_var state)) in
    Ty.Arrow
      { arg_ty
      ; arg_mode = Modes.join a_arg_mode b_arg_mode
      ; ret_ty
      ; ret_mode = Modes.meet a_ret_mode b_ret_mode
      }
  | ( Pi { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Arrow { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } )
    ->
    let%bind arg_ty = join_value state a_arg_ty b_arg_ty in
    let%map ret_ty = meet_value state (eval state a_ret_ty (State.fresh_var state)) b_ret_ty in
    Ty.Arrow
      { arg_ty
      ; arg_mode = Modes.join a_arg_mode b_arg_mode
      ; ret_ty
      ; ret_mode = Modes.meet a_ret_mode b_ret_mode
      }
  | (Unit | Bool | Int | Type | Arrow _ | Pi _ | Tuple _), _ -> None

and reduce state env (expr : Dst.Expr.t) : Desc.t =
  let _expr, expr_desc = typecheck state env expr in
  expr_desc

and typecheck state env expr =
  State.recur state ~loc:(Dst.Expr.loc expr) ~f:(fun () -> typecheck' state env expr)

and typecheck' state env (expr : Dst.Expr.t) : Expr.t * Desc.t =
  let open Lazy.Let_syntax in
  match expr with
  | Unreachable { loc } ->
    if State.concrete state
    then Fail.unreachable_reached ~loc
    else (
      let mode = Modes.bottom () in
      let ty = Value.Bottom in
      let value = Value.Bottom in
      Literal { value; ty; mode; loc }, { Desc.ty; mode; static = Lazy.from_val value })
  | Assert { cond; static; loc } ->
    let cond, cond_desc = typecheck state env cond in
    require_leq state ~loc cond_desc.ty (Type Bool);
    (match static with
     | Static ->
       require_static ~loc cond_desc;
       (match Lazy.force cond_desc.static with
        | Bool (T true) -> typecheck state env (Literal { value = Unit; loc })
        | expr -> Fail.static_assert ~loc expr)
     | Dynamic | Parametric ->
       require_unerased ~loc cond_desc;
       (* TODO don't force here *)
       if Modes.is_static cond_desc.mode && Value.is_true (Lazy.force cond_desc.static)
       then typecheck state env (Literal { value = Unit; loc })
       else (
         let mode = Modes.create ~staticity:Static ~erasure:Unerased in
         let ty =
           (* TODO swap to builtin *)
           let mode = Modes.create ~staticity:Dynamic ~erasure:Erased in
           Value.Type
             (Arrow { arg_ty = Type Bool; arg_mode = mode; ret_ty = Type Unit; ret_mode = mode })
         in
         let value = Value.External { symbol = "syl_assert"; ty } in
         let fn = Expr.Literal { value; ty; mode; loc } in
         ( Apply
             { fn
             ; arg = cond
             ; ty = Type Unit
             ; mode = Modes.create ~staticity:Static ~erasure:Unerased
             ; loc
             }
         , { Desc.ty; mode; static = Lazy.from_val value } )))
  | Literal { value; loc } ->
    let ty = Value.Type (Ty.of_literal value) in
    let value = Value.of_literal value in
    let mode = Modes.create ~staticity:Static ~erasure:Unerased in
    Literal { value; ty; mode; loc }, { ty; mode; static = Lazy.from_val value }
  | Var { id; loc } ->
    let desc = require_var ~loc env id in
    (match desc.mode.staticity, desc.mode.erasure with
     | Static, Erased ->
       Literal { value = Lazy.force desc.static; ty = desc.ty; mode = desc.mode; loc }, desc
     | (Dynamic | Parametric), Erased -> Erased { ty = desc.ty; mode = desc.mode; loc }, desc
     | _ -> Var { id; ty = desc.ty; mode = desc.mode; loc }, desc)
  | Nop { op; elts; loc } ->
    (* TODO replace with apply *)
    (match op with
     | Caret ->
       let ty = Value.Type Type in
       let mode = Modes.create ~staticity:Static ~erasure:Erased in
       let elts =
         List.map elts ~f:(fun elt ->
           let elt_desc = reduce state env elt in
           require_static_type state ~loc elt_desc)
       in
       let value = Value.Type (Tuple elts) in
       Literal { value; ty; mode; loc }, { ty; mode; static = Lazy.from_val value }
     | Comma ->
       let elts, descs = List.map elts ~f:(fun elt -> typecheck state env elt) |> List.unzip in
       let mode =
         List.fold descs ~init:(Modes.bottom ()) ~f:(fun acc desc -> Modes.join acc desc.mode)
       in
       let ty = Value.Type (Tuple (List.map descs ~f:(fun desc -> desc.ty))) in
       let static =
         List.map descs ~f:(fun desc -> desc.static)
         |> Lazy.all
         |> Lazy.map ~f:(fun elts -> Value.Tuple elts)
       in
       Tuple { elts; ty; mode; loc }, { ty; mode; static })
  | If { cond; then_; else_; static; loc } ->
    let cond, cond_desc = typecheck state env cond in
    require_leq state ~loc cond_desc.ty (Type Bool);
    (match static with
     | Static ->
       require_static ~loc cond_desc;
       (match Lazy.force cond_desc.static with
        | Bool (T true) when State.monomorphizing state -> typecheck state env then_
        | Bool (T false) when State.monomorphizing state -> typecheck state env else_
        | Bool (T true) ->
          let _else_desc = State.with_abs state ~f:(fun () -> reduce state env else_) in
          typecheck state env then_
        | Bool (T false) ->
          let _then_desc = State.with_abs state ~f:(fun () -> reduce state env then_) in
          typecheck state env else_
        | value ->
          let then_, then_desc =
            let env = rebind_if_var env cond (Value.Bool (T true)) in
            State.with_abs state ~f:(fun () -> typecheck state env then_)
          in
          let else_, else_desc =
            let env = rebind_if_var env cond (Value.Bool (T false)) in
            State.with_abs state ~f:(fun () -> typecheck state env else_)
          in
          let mode = Modes.cond ~cond:cond_desc.mode then_desc.mode else_desc.mode in
          let static =
            if Modes.is_static mode
            then (
              let%map then_ = then_desc.static
              and else_ = else_desc.static in
              Value.reduce (If { cond = value; then_; else_ }))
            else Fail.unreachable [%here] ~loc
          in
          let ty = Value.reduce (If { cond = value; then_ = then_desc.ty; else_ = else_desc.ty }) in
          If { cond; then_; else_; ty; mode; loc }, { ty; mode; static })
     | Dynamic | Parametric ->
       require_unerased ~loc cond_desc;
       let then_, then_desc = typecheck state env then_ in
       let else_, else_desc = typecheck state env else_ in
       let mode = Modes.cond ~cond:cond_desc.mode then_desc.mode else_desc.mode in
       let static =
         if Modes.is_static mode
         then
           Lazy.bind cond_desc.static ~f:(function
             | Bool (T true) -> then_desc.static
             | Bool (T false) -> else_desc.static
             | cond ->
               let%map then_ = then_desc.static
               and else_ = else_desc.static in
               Value.reduce (If { cond; then_; else_ }))
         else Fail.unreachable [%here] ~loc
       in
       let ty = require_join state ~loc then_desc.ty else_desc.ty in
       If { cond; then_; else_; ty; mode; loc }, { ty; mode; static })
  | Let { var; bind; rest; loc } ->
    let bind, bind_desc = typecheck state env bind in
    let env = Env.bind env var bind_desc in
    let rest, rest_desc = typecheck state env rest in
    Let { var; bind; rest; ty = rest_desc.ty; mode = rest_desc.mode; loc }, rest_desc
  | Lambda { arg; arg_mode; arg_ty; body = body_cst; loc } ->
    let arg_ty_desc = reduce state env arg_ty in
    let arg_ty = require_static_type state ~loc arg_ty_desc in
    let arg_static = arg_mode.staticity in
    let arg_mode = Modes.annotate (Modes.default ()) arg_mode in
    (match arg_static with
     | Some (Dynamic | Parametric) | None ->
       let is_unannotated = Option.is_none arg_static in
       let arg_mode =
         if is_unannotated then { arg_mode with staticity = Parametric } else arg_mode
       in
       let body, body_desc =
         let env =
           Env.bind env arg { ty = arg_ty; mode = arg_mode; static = Fail.unreachable [%here] ~loc }
         in
         typecheck state env body_cst
       in
       let arg_mode = { arg_mode with staticity = Dynamic } in
       let ret_mode =
         if is_unannotated
         then { body_desc.mode with staticity = Modes.Staticity.resolve body_desc.mode.staticity }
         else body_desc.mode
       in
       let ty = Value.Type (Arrow { arg_ty; arg_mode; ret_ty = body_desc.ty; ret_mode }) in
       let mode =
         Set.remove (Expr.free_vars body) arg
         |> Set.fold ~init:(Modes.bottom ()) ~f:(fun acc id ->
           match Env.find env id with
           | Some desc -> Modes.capture acc ~fv:desc.mode
           | None -> acc)
         |> Modes.return ~ret:body_desc.mode
       in
       let static = Lazy.from_val (Value.Closure { arg; ty; body; body_cst; env }) in
       Expr.Lambda { arg; ty; body; mode; loc }, { Desc.ty; mode; static }
     | Some Static ->
       let body_desc =
         let env =
           Env.bind
             env
             arg
             { ty = arg_ty; mode = arg_mode; static = Lazy.from_val (State.fresh_var state) }
         in
         reduce state env body_cst
       in
       let ret_ty = Dependent.typecheck body_desc.ty ~env ~arg ~arg_ty ~arg_mode ~body:body_cst in
       let ret_mode =
         { body_desc.mode with staticity = Modes.Staticity.resolve body_desc.mode.staticity }
       in
       let ty = Value.Type (Pi { arg_ty; arg_mode; ret_ty; ret_mode }) in
       let mode = Modes.return (Modes.bottom ()) ~ret:body_desc.mode in
       let mono = Hashtbl.create (module Value.Concrete) in
       let static = Lazy.from_val (Value.Binder { arg; ty; body = body_cst; mono; env }) in
       Binder { arg; ty; body = body_cst; mono; mode; loc }, { ty; mode; static })
  | Apply { fn; arg; loc } ->
    let fn, fn_desc = typecheck state env fn in
    let arg, arg_desc = typecheck state env arg in
    (match fn_desc.ty with
     | Type (Pi { arg_ty; arg_mode; ret_ty; ret_mode }) ->
       require_static ~loc fn_desc;
       require_mode ~loc arg_desc.mode arg_mode;
       require_leq state ~loc arg_desc.ty arg_ty;
       let arg_val = Lazy.force arg_desc.static in
       let ret_ty = eval state ret_ty arg_val in
       (match Lazy.force fn_desc.static with
        | Binder binder ->
          (match concrete state arg_val with
           | Some arg_concrete ->
             let { Binder.Mono.body; body_desc; _ } =
               Hashtbl.update_and_return binder.mono arg_concrete ~f:(function
                 | None ->
                   let env =
                     Env.bind
                       binder.env
                       binder.arg
                       { ty = arg_ty; mode = arg_mode; static = Lazy.from_val arg_val }
                   in
                   let body, body_desc =
                     State.with_app state ~f:(fun () -> typecheck state env binder.body)
                   in
                   let body, body_desc = weaken ~loc body body_desc ~ty:ret_ty ~mode:ret_mode in
                   { arg = binder.arg; arg_mode; arg_desc; body; body_desc }
                 | Some x -> x)
             in
             if Modes.is_erased fn_desc.mode
             then (
               let body = inline ~loc ~env:binder.env ~arg_id:binder.arg ~arg ~arg_mode body in
               body, body_desc)
             else
               ( Symbol { fn; key = arg_concrete; ty = body_desc.ty; mode = body_desc.mode; loc }
               , body_desc )
           | None ->
             ( Apply { fn; arg; ty = ret_ty; mode = ret_mode; loc }
             , { ty = ret_ty
               ; mode = ret_mode
               ; static = Lazy.from_val (Value.Apply { fn = Binder binder; arg = arg_val })
               } ))
        | Closure closure ->
          let static =
            if Modes.is_static ret_mode
            then (
              let env = Env.bind closure.env closure.arg arg_desc in
              let desc = reduce state env closure.body_cst in
              desc.static)
            else Fail.unreachable [%here] ~loc
          in
          if Modes.is_erased fn_desc.mode
          then (
            let body, body_desc =
              weaken
                ~loc
                closure.body
                { ty = Expr.ty closure.body; mode = Expr.mode closure.body; static }
                ~ty:ret_ty
                ~mode:ret_mode
            in
            let body = inline ~loc ~env:closure.env ~arg_id:closure.arg ~arg ~arg_mode body in
            body, body_desc)
          else
            ( Apply { fn; arg; ty = ret_ty; mode = ret_mode; loc }
            , { ty = ret_ty; mode = ret_mode; static } )
        | _ ->
          (* If we have monomorphized prims, they need to be implemented here. *)
          let static =
            if Modes.is_static ret_mode
            then (
              let%map fn = fn_desc.static
              and arg = arg_desc.static in
              Value.reduce (Apply { fn; arg }))
            else Fail.unreachable [%here] ~loc
          in
          ( Apply { fn; arg; ty = ret_ty; mode = ret_mode; loc }
          , { ty = ret_ty; mode = ret_mode; static } ))
     | Type (Arrow { arg_ty; arg_mode; ret_ty; ret_mode }) ->
       if Modes.is_dynamic fn_desc.mode then require_unerased ~loc fn_desc;
       if Modes.is_dynamic arg_desc.mode then require_unerased ~loc arg_desc;
       require_mode ~loc arg_desc.mode arg_mode;
       require_leq state ~loc arg_desc.ty arg_ty;
       let ret_mode =
         let staticity = Modes.Staticity.join fn_desc.mode.staticity arg_desc.mode.staticity in
         { ret_mode with staticity = Modes.Staticity.join staticity ret_mode.staticity }
       in
       let static =
         if Modes.is_static ret_mode
         then (
           let%bind fn = fn_desc.static in
           match fn with
           | Closure closure ->
             let env = Env.bind closure.env closure.arg arg_desc in
             let desc = reduce state env closure.body_cst in
             desc.static
           | Prim prim ->
             Lazy.map arg_desc.static ~f:(fun arg ->
               try Builtin.eval prim arg with
               | Builtin.Divide_by_zero expr -> Fail.divide_by_zero ~loc expr)
           | _ -> Lazy.map arg_desc.static ~f:(fun arg -> Value.reduce (Apply { fn; arg })))
         else Fail.unreachable [%here] ~loc
       in
       if Modes.is_erased fn_desc.mode
       then (
         require_static ~loc fn_desc;
         match Lazy.force fn_desc.static with
         | Closure closure ->
           let body, body_desc =
             weaken
               ~loc
               closure.body
               { ty = ret_ty; mode = ret_mode; static }
               ~ty:ret_ty
               ~mode:ret_mode
           in
           let body = inline ~loc ~env:closure.env ~arg_id:closure.arg ~arg ~arg_mode body in
           body, { body_desc with mode = ret_mode }
         | _ ->
           (* If we have inlined prims, they need to be implemented here. *)
           ( Apply { fn; arg; ty = ret_ty; mode = ret_mode; loc }
           , { ty = ret_ty; mode = ret_mode; static } ))
       else (
         let desc = { Desc.ty = ret_ty; mode = ret_mode; static } in
         Apply { fn; arg; ty = ret_ty; mode = ret_mode; loc }, desc)
     | ty -> raise_s [%message "Expected function type" (loc : Lex.Location.t) (ty : Value.t)])
  | Mode_annotation { expr; mode; loc } ->
    let expr, desc = typecheck state env expr in
    let mode = Modes.annotate desc.mode mode in
    require_mode ~loc desc.mode mode;
    weaken ~loc expr desc ~ty:desc.ty ~mode
  | Type_annotation { expr; ty; loc } ->
    let expr, desc = typecheck state env expr in
    let ty_desc = reduce state env ty in
    let ty = require_static_type state ~loc ty_desc in
    require_leq state ~loc desc.ty ty;
    weaken ~loc expr desc ~ty ~mode:desc.mode
  | Arrow { arg; arg_mode; arg_id; ret; ret_mode; loc } ->
    (* TODO should bind types instead of forcing *)
    let ty = Value.Type Type in
    let mode = Modes.create ~staticity:Static ~erasure:Erased in
    let value =
      typecheck_arrow state ~loc env ~arg_id ~arg_ty:arg ~arg_mode ~ret_ty:ret ~ret_mode
    in
    Literal { value; ty; mode; loc }, { ty; mode; static = Lazy.from_val value }
  | Fun { funs; rest; loc } ->
    let funs, env = typecheck_funs state env funs in
    let rest, rest_desc = typecheck state env rest in
    Fun { funs; rest; ty = rest_desc.ty; mode = rest_desc.mode; loc }, rest_desc

and typecheck_arrow state ~loc env ~arg_id ~arg_ty ~arg_mode ~ret_ty ~ret_mode : Value.t =
  let arg_desc = reduce state env arg_ty in
  let arg_mode = Modes.annotate (Modes.default ()) arg_mode in
  let ret_mode = Modes.annotate (Modes.default ()) ret_mode in
  let arg_ty = require_static_type state ~loc arg_desc in
  match arg_mode.staticity with
  | Dynamic | Parametric ->
    let ret_desc = reduce state env ret_ty in
    let ret_ty = require_static_type state ~loc ret_desc in
    Value.Type (Arrow { arg_ty; arg_mode; ret_ty; ret_mode })
  | Static ->
    let env =
      Env.bind
        env
        arg_id
        { ty = arg_ty; mode = arg_mode; static = Lazy.from_val (State.fresh_var state) }
    in
    let ret_desc = reduce state env ret_ty in
    let ty = require_static_type state ~loc ret_desc in
    let ret_ty = Dependent.reduce ty ~env ~arg:arg_id ~arg_ty ~arg_mode ~ret_ty in
    Value.Type (Pi { arg_ty; arg_mode; ret_ty; ret_mode })

and typecheck_funs state env (funs : Dst.Expr.fun_ Nonempty_list.t) =
  let env_rec = ref env in
  let env =
    Nonempty_list.fold
      funs
      ~init:env
      ~f:
        (fun
          acc
          { Dst.Expr.var; arg; erased; arg_ty; arg_mode; ret_mode; ret_ty; body = body_cst; loc }
        ->
        let ty = typecheck_arrow state ~loc env ~arg_id:arg ~arg_ty ~arg_mode ~ret_ty ~ret_mode in
        let desc : Desc.t =
          match ty with
          | Type (Pi { ret_mode; _ }) ->
            let mode = Modes.return (Modes.bottom ~erasure:erased ()) ~ret:ret_mode in
            let static =
              Lazy.from_fun (fun () ->
                Value.Binder
                  { arg
                  ; ty
                  ; body = body_cst
                  ; mono = Hashtbl.create (module Value.Concrete)
                  ; env = !env_rec
                  })
            in
            { ty; mode; static }
          | Type (Arrow { arg_ty; ret_ty; ret_mode; _ }) ->
            let is_unannotated = Option.is_none arg_mode.staticity in
            let arg_mode = Modes.annotate (Modes.default ()) arg_mode in
            let arg_mode =
              if is_unannotated then { arg_mode with staticity = Parametric } else arg_mode
            in
            let mode = Modes.return (Modes.bottom ~erasure:erased ()) ~ret:ret_mode in
            let static =
              Lazy.from_fun (fun () ->
                let body, body_desc =
                  let env =
                    Env.bind
                      !env_rec
                      arg
                      { ty = arg_ty; mode = arg_mode; static = Fail.unreachable [%here] ~loc }
                  in
                  typecheck state env body_cst
                in
                let body_mode =
                  if is_unannotated
                  then
                    { body_desc.mode with
                      staticity = Modes.Staticity.resolve body_desc.mode.staticity
                    }
                  else body_desc.mode
                in
                require_mode ~loc body_mode ret_mode;
                require_leq state ~loc body_desc.ty ret_ty;
                Value.Closure { arg; ty; body; body_cst; env = !env_rec })
            in
            let arg_mode = { arg_mode with staticity = Dynamic } in
            { Desc.ty = Type (Arrow { arg_ty; arg_mode; ret_ty; ret_mode }); mode; static }
          | _ -> assert false
        in
        Env.bind acc var desc)
  in
  env_rec := env;
  let funs =
    Nonempty_list.map funs ~f:(fun { var; loc; _ } : Expr.fun_ ->
      let { Desc.ty; mode; static } = require_var ~loc env var in
      try
        match ty, Lazy.force static with
        | Type (Arrow _), Closure { arg; ty; body; _ } -> Lambda { var; arg; body; ty; mode; loc }
        | Type (Pi { arg_ty; arg_mode; ret_mode; ret_ty }), Binder { arg; ty; body; mono; _ } ->
          let arg_val = State.fresh_var state in
          let env =
            Env.bind env arg { ty = arg_ty; mode = arg_mode; static = Lazy.from_val arg_val }
          in
          let body_desc = reduce state env body in
          require_mode ~loc body_desc.mode ret_mode;
          require_leq state ~loc body_desc.ty (eval state ret_ty arg_val);
          Binder { var; arg; body; mono; ty; mode; loc }
        | _ -> assert false
      with
      | Lazy.Undefined -> Fail.inline_self ~loc (Nonempty_list.map funs ~f:(fun { var; _ } -> var)))
  in
  funs, env
;;

let typecheck_top_level state env (dst : Dst.Top_level.t) : Top_level.t * Env.t =
  match dst with
  | Let { var; bind; loc } ->
    let bind, bind_desc = typecheck state env bind in
    let env = Env.bind env var bind_desc in
    Let { var; bind; loc }, env
  | Fun { funs; loc } ->
    let funs, env = typecheck_funs state env funs in
    Fun { funs; loc }, env
  | External { var; ty; symbol; loc } ->
    let ty_desc = reduce state env ty in
    let ty = require_static_type ~loc state ty_desc in
    require_dynamic_arrow ~loc var symbol ty;
    let mode = Modes.bottom () in
    let static = Lazy.from_val (Value.External { symbol; ty }) in
    External { var; symbol; ty; loc }, Env.bind env var { Desc.ty; mode; static }
  | Builtin { var; name; loc } ->
    let builtin =
      match Builtin.find name with
      | Some builtin -> builtin
      | None -> Fail.unknown_builtin ~loc var name
    in
    let desc = Builtin.desc builtin in
    Builtin { var; builtin; ty = desc.ty; loc }, Env.bind env var desc
;;

let fold_top_levels state env dst tls =
  List.fold dst ~init:(tls, env) ~f:(fun (acc, env) top_level ->
    let tl, env = typecheck_top_level state env top_level in
    tl :: acc, env)
;;

let typecheck_exn (dst : Dst.Program.t) =
  let state = State.create () in
  let env = Env.initial in
  let program, _ = fold_top_levels state env dst [] in
  List.rev program
;;

let typecheck tst =
  try Ok (typecheck_exn tst) with
  | Error err -> Error err
;;
