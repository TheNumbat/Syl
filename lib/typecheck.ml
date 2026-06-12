open! Core
open Tst
open Option.Let_syntax

module Error = struct
  module Match = struct
    type t =
      | Multiple_bindings of Ident.t
      | Expected_tuple of Value.t
      | Redundant of Dst.Expr.pattern Nonempty_list.t
      | Non_exhaustive of Match.Result.Missing.t Nonempty_list.t
      | Or_unbound of Ident.t Nonempty_list.t
    [@@deriving sexp]
  end

  type t =
    | Unbound_ident of Ident.t
    | Mode_mismatch of
        { got : Modes.t
        ; need : Modes.t
        }
    | Type_mismatch of
        { got : Value.t
        ; need : Value.t
        }
    | Expected_function of
        { fn : Value.t
        ; arg : Value.t
        }
    | Cannot_unify of
        { lhs : Value.t
        ; rhs : Value.t
        }
    | Match of Match.t
    | Unknown_builtin of Ident.t * string
    | Static_external of Ident.t * string
    | Static_failure of Builtin.Error.t
    | Erased_application of
        { fn : Value.t
        ; result : Modes.t
        }
    | Recursion_limit of int
    | Unreachable_reached
  [@@deriving sexp]
end

exception Error of Error.t Loc.t [@@deriving sexp]

module Fail = struct
  module Match = struct
    let multiple_bindings ~loc here id =
      raise (Error { loc; here; reason = Match (Multiple_bindings id) })
    ;;

    let redundant ~loc here patterns =
      raise (Error { loc; here; reason = Match (Redundant patterns) })
    ;;

    let expected_tuple ~loc here got =
      raise (Error { loc; here; reason = Match (Expected_tuple got) })
    ;;

    let non_exhaustive ~loc here missing =
      raise (Error { loc; here; reason = Match (Non_exhaustive missing) })
    ;;

    let or_unbound ~loc here ids = raise (Error { loc; here; reason = Match (Or_unbound ids) })
  end

  let mode_mismatch ~loc here got need =
    raise (Error { loc; here; reason = Mode_mismatch { got; need } })
  ;;

  let type_mismatch ~loc here got need =
    raise (Error { loc; here; reason = Type_mismatch { got; need } })
  ;;

  let cannot_unify ~loc here lhs rhs =
    raise (Error { loc; here; reason = Cannot_unify { lhs; rhs } })
  ;;

  let unbound_ident ~loc here id = raise (Error { loc; here; reason = Unbound_ident id })
  let recursion_limit ~loc here limit = raise (Error { loc; here; reason = Recursion_limit limit })

  let static_external ~loc here id name =
    raise (Error { loc; here; reason = Static_external (id, name) })
  ;;

  let unknown_builtin ~loc here id name =
    raise (Error { loc; here; reason = Unknown_builtin (id, name) })
  ;;

  let static_failure ~loc here err = raise (Error { loc; here; reason = Static_failure err })
  let unreachable_reached ~loc here = raise (Error { loc; here; reason = Unreachable_reached })

  let erased_application ~loc here fn result =
    raise (Error { loc; here; reason = Erased_application { fn; result } })
  ;;

  let expected_function ~loc here fn arg =
    raise (Error { loc; here; reason = Expected_function { fn; arg } })
  ;;

  let unreachable here ~loc =
    Lazy.from_fun (fun () ->
      raise_s
        [%message "Bug: forced dynamic" (loc : Lex.Location.t) (here : Source_code_position.t)])
  ;;
end

module State = struct
  module Spec = struct
    type t =
      { key : Value.Concrete.t
      ; family : int
      }
    [@@deriving sexp, compare, hash]
  end

  module Mono = struct
    type t =
      { family : int
      ; body : Expr.t
      ; desc : Desc.t
      }
  end

  module Instance = struct
    type t =
      { hash : int
      ; key : Value.Concrete.t
      }
    [@@deriving sexp, compare, hash]
  end

  module Family = struct
    type t =
      { path : Spec.t list
      ; loc : Lex.Location.t
      }
    [@@deriving sexp, compare, hash]
  end

  type t =
    { mutable stamp : int
    ; mutable depth : int
    ; mutable app_depth : int
    ; mutable abs_depth : int
    ; mutable path : Spec.t list
    ; families : (Family.t, int) Hashtbl.t
    ; specializations : (Instance.t, Mono.t) Hashtbl.t
    ; groups : (int, Ident.t) Hashtbl.t
    }

  let recursion_limit = 1000

  let create ~stamp =
    { stamp
    ; depth = 0
    ; app_depth = 0
    ; abs_depth = 0
    ; path = []
    ; families = Hashtbl.create (module Family)
    ; specializations = Hashtbl.create (module Instance)
    ; groups = Hashtbl.create (module Core.Int)
    }
  ;;

  let concrete t = t.abs_depth = 0
  let monomorphizing t = t.app_depth > 0

  let recur ~loc t ~f =
    if t.depth > recursion_limit
    then Fail.recursion_limit [%here] ~loc recursion_limit
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

  let with_spec t spec ~f =
    t.path <- spec :: t.path;
    Exn.protect ~f ~finally:(fun () -> t.path <- List.tl_exn t.path)
  ;;

  let fresh_id t =
    let res = t.stamp in
    t.stamp <- t.stamp + 1;
    res
  ;;

  let fresh_var t = Value.Var (Ident.create Ident.Raw.anon ~stamp:(fresh_id t))

  let family t ~loc =
    let key = { Family.loc; path = List.rev t.path } in
    Hashtbl.find_or_add t.families key ~default:(fun () -> fresh_id t)
  ;;

  let specialize t instance ~f =
    match Hashtbl.find t.specializations instance with
    | Some mono -> mono
    | None ->
      let mono = f () in
      Hashtbl.set t.specializations ~key:instance ~data:mono;
      mono
  ;;

  let record_group t var ({ mode; static; _ } : Desc.t) =
    if Lazy.is_val static && Modes.is_static mode && Modes.is_unerased mode
    then (
      match Lazy.force static with
      | Value.Closure { hash; _ } | Binder { hash; _ } ->
        Hashtbl.add_exn t.groups ~key:hash ~data:var
      | _ -> ())
  ;;

  let collect_monos t =
    Hashtbl.fold
      t.specializations
      ~init:Core.Int.Map.empty
      ~f:(fun ~key:{ hash = _; key } ~data:{ Mono.family; body; desc = _ } acc ->
        Map.update acc family ~f:(fun monos ->
          let monos = Option.value monos ~default:Value.Concrete.Map.empty in
          Map.update monos key ~f:(function
            | None -> body
            | Some existing -> existing)))
  ;;
end

let rec concrete (v : Value.t) : Value.Concrete.t option =
  match v with
  | Unit -> Some Unit
  | Bool (T b) -> Some (Bool b)
  | Int (T i) -> Some (Int i)
  | Tuple elts ->
    Nonempty_list.map elts ~f:concrete
    |> Nonempty_list.to_list
    |> Option.all
    |> Option.map ~f:(fun elts -> Value.Concrete.Tuple (Nonempty_list.of_list_exn elts))
  | Closure closure -> Some (Closure closure.hash)
  | Binder binder -> Some (Closure binder.hash)
  | Prim prim -> Some (Prim (Prim prim))
  | Type Unit -> Some (Prim (Type Unit))
  | Type Bool -> Some (Prim (Type Bool))
  | Type Int -> Some (Prim (Type Int))
  | Type Type -> Some (Prim (Type Type))
  | Type (Arrow { arg_ty; arg_mode; ret_ty; ret_mode })
  | Type (Pi { arg_ty; arg_mode; ret_ty = T { ty = ret_ty; _ }; ret_mode }) ->
    let%map arg = concrete arg_ty
    and ret = concrete ret_ty in
    Value.Concrete.Arrow { arg; arg_mode; ret; ret_mode }
  | Type (Tuple elts) ->
    let%map elts = Nonempty_list.map elts ~f:concrete |> Nonempty_list.to_list |> Option.all in
    Value.Concrete.Tuple_t (Nonempty_list.of_list_exn elts)
  | External { symbol; _ } -> Some (External symbol)
  | Bottom | Bool _ | Int _ | Var _ | If _ | Apply _ | Proj _ | Type (Pi _) -> None
;;

(* A static forced during its own computation becomes abstract. *)
let force_or_var state static =
  try Lazy.force static with
  | Lazy.Undefined -> State.fresh_var state
;;

let weaken ~loc expr { Desc.ty = src_ty; mode = src_mode; static } ~ty:dst_ty ~mode:dst_mode =
  let expr : Expr.t =
    if (not (Modes.is_erased src_mode)) && Modes.is_erased dst_mode
    then Erased { ty = src_ty; mode = src_mode; loc = Expr.loc expr }
    else expr
  in
  let static =
    if (not (Modes.is_dynamic src_mode)) && Modes.is_dynamic dst_mode
    then Fail.unreachable [%here] ~loc
    else static
  in
  Expr.with_ expr ~ty:dst_ty ~mode:dst_mode, { Desc.ty = dst_ty; mode = dst_mode; static }
;;

let resolve_body_mode arg_mode (body_desc : Desc.t) =
  let mode =
    if not (Modes.is_dynamic arg_mode)
    then { body_desc.mode with staticity = Modes.Staticity.resolve body_desc.mode.staticity }
    else body_desc.mode
  in
  { body_desc with mode }
;;

let resolve_app_mode ~loc (fn_desc : Desc.t) (ret_mode : Modes.t) : Modes.t =
  if Modes.is_erased fn_desc.mode
  then (
    if not (Modes.is_static ret_mode) then Fail.erased_application [%here] ~loc fn_desc.ty ret_mode;
    { ret_mode with erasure = Erased })
  else ret_mode
;;

let require_mode ~loc src dst =
  if not (Modes.leq src dst) then Fail.mode_mismatch [%here] ~loc src dst
;;

let require_mode_annotation src annot =
  let annot =
    if Modes.Maybe.is_erased annot && Option.is_none annot.staticity
    then { annot with staticity = Some Static }
    else annot
  in
  let annot =
    if Modes.Maybe.is_dynamic annot && Option.is_none annot.erasure
    then { annot with erasure = Some Unerased }
    else annot
  in
  Modes.annotate src annot
;;

let require_static ~loc (desc : Desc.t) =
  require_mode ~loc desc.mode (Modes.top ~staticity:Static ())
;;

let require_unerased ~loc (desc : Desc.t) =
  require_mode ~loc desc.mode (Modes.top ~erasure:Unerased ())
;;

let require_dynamic_arrow ~loc var sym (ty : Value.t) =
  match ty with
  | Type (Arrow { arg_mode; ret_mode; _ } | Pi { arg_mode; ret_mode; _ }) ->
    if Modes.is_static arg_mode || Modes.is_static ret_mode
    then Fail.static_external [%here] ~loc var sym
  | _ -> Fail.static_external [%here] ~loc var sym
;;

let require_var ~loc env id =
  match Env.find env id with
  | Some value -> value
  | None -> Fail.unbound_ident [%here] ~loc id
;;

let rebind_if_var env (expr : Expr.t) value =
  (* TODO do something more expressive here *)
  match expr with
  | Var { id; ty; mode; _ } -> Env.bind env id { Desc.ty; mode; static = Lazy.from_val value }
  | _ -> env
;;

let rec require_leq state ~loc src dst =
  if not (leq_value state src dst) then Fail.type_mismatch [%here] ~loc src dst

and require_join state ~loc ty1 ty2 =
  match join_value state ty1 ty2 with
  | Some ty -> ty
  | None -> Fail.cannot_unify [%here] ~loc ty1 ty2

and require_join_desc state ~loc (desc1 : Desc.t) (desc2 : Desc.t) =
  let open Lazy.Let_syntax in
  let ty = require_join state ~loc desc1.ty desc2.ty in
  let mode = Modes.join desc1.mode desc2.mode in
  (* TODO do we need to join the static values? *)
  let static =
    let%map static1 = desc1.static
    and static2 = desc2.static in
    require_join state ~loc static1 static2
  in
  { Desc.ty; mode; static }

and require_static_type ~loc state (desc : Desc.t) =
  require_static ~loc desc;
  require_leq state ~loc desc.ty (Type Type);
  force_or_var state desc.static

and eval state (dep : Dependent.t) (arg_val : Value.t) : Value.t =
  (* We've already typechecked forall args, so these can't fail. *)
  match dep with
  | T { ty; memo } ->
    (match concrete arg_val with
     | Some arg_concrete -> Hashtbl.set memo ~key:arg_concrete ~data:ty
     | None -> ());
    ty
  | Meet (a, b) -> Option.value_exn (meet_value state (eval state a arg_val) (eval state b arg_val))
  | Join (a, b) -> Option.value_exn (join_value state (eval state a arg_val) (eval state b arg_val))
  | Reduce { env; arg; arg_ty; arg_mode; memo; ret_ty } ->
    let reduce () =
      let env = Env.bind env arg { ty = arg_ty; mode = arg_mode; static = Lazy.from_val arg_val } in
      let ret_ty_desc = reduce state env ret_ty in
      force_or_var state ret_ty_desc.static
    in
    (match concrete arg_val with
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
    (match concrete arg_val with
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
    (match Nonempty_list.zip a_elts b_elts with
     | Ok zip -> Nonempty_list.for_all zip ~f:(fun (a, b) -> leq_value state a b)
     | Unequal_lengths -> false)
  | External a, External b -> String.equal a.symbol b.symbol
  | ( If { cond = a_cond; then_ = a_then; else_ = a_else }
    , If { cond = b_cond; then_ = b_then; else_ = b_else } )
    when leq_value state a_cond b_cond
         && leq_value state a_then b_then
         && leq_value state a_else b_else -> true
  | Apply { fn = a_fn; arg = a_arg }, Apply { fn = b_fn; arg = b_arg }
    when leq_value state a_fn b_fn && leq_value state a_arg b_arg -> true
  | a, Apply { fn = Binder binder; arg } -> unfold_apply state binder arg ~f:(leq_value state a)
  | Apply { fn = Binder binder; arg }, b ->
    unfold_apply state binder arg ~f:(fun a -> leq_value state a b)
  | If { then_; else_; _ }, b -> leq_value state then_ b && leq_value state else_ b
  | a, If { then_; else_; _ } -> leq_value state a then_ && leq_value state a else_
  | Proj a, Proj b -> a.index = b.index && leq_value state a.tuple b.tuple
  | ( ( Unit
      | Bool _
      | Int _
      | Type _
      | Closure _
      | Binder _
      | Var _
      | Apply _
      | Proj _
      | External _
      | Tuple _
      | Prim _ )
    , _ ) -> false

and geq_value state (a : Value.t) (b : Value.t) = leq_value state b a

and unfold_apply : 'a. State.t -> Binder.t -> Value.t -> f:(Value.t -> 'a) -> 'a =
  fun state binder arg_val ~f ->
  let loc = Dst.Expr.loc binder.body_dst in
  State.recur state ~loc ~f:(fun () ->
    let arg_ty = Ty.arg binder.ty in
    let arg_mode = Ty.arg_mode binder.ty in
    let env =
      Env.bind
        binder.env
        binder.arg
        { ty = arg_ty; mode = arg_mode; static = Lazy.from_val arg_val }
    in
    let desc = reduce state env binder.body_dst in
    f (force_or_var state desc.static))

and unfold_value state (a : Value.t) (b : Value.t) ~f =
  match a, b with
  | a, Apply { fn = Binder binder; arg } -> unfold_apply state binder arg ~f:(fun b -> f a b)
  | Apply { fn = Binder binder; arg }, b -> unfold_apply state binder arg ~f:(fun a -> f a b)
  | _, _ -> None

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
    (match Nonempty_list.map2 a_elts b_elts ~f:(join_value state) with
     | Ok elts ->
       Nonempty_list.to_list elts
       |> Option.all
       |> Option.map ~f:(fun elts -> Value.Tuple (Nonempty_list.of_list_exn elts))
     | Unequal_lengths -> None)
  | External a, External b when String.equal a.symbol b.symbol -> Some (External a)
  | ( If { cond = a_cond; then_ = a_then; else_ = a_else }
    , (If { cond = b_cond; then_ = b_then; else_ = b_else } as b) ) ->
    (match
       let%map cond = join_value state a_cond b_cond
       and then_ = join_value state a_then b_then
       and else_ = join_value state a_else b_else in
       Value.If { cond; then_; else_ }
     with
     | Some join -> Some join
     | None ->
       let%bind then_ = join_value state a_then b
       and else_ = join_value state a_else b in
       join_value state then_ else_)
  | If { then_; else_; _ }, b ->
    let%bind then_ = join_value state then_ b
    and else_ = join_value state else_ b in
    join_value state then_ else_
  | a, If { then_; else_; _ } ->
    let%bind then_ = join_value state a then_
    and else_ = join_value state a else_ in
    join_value state then_ else_
  | (Apply { fn = a_fn; arg = a_arg } as a), (Apply { fn = b_fn; arg = b_arg } as b) ->
    (match
       let%map fn = join_value state a_fn b_fn
       and arg = join_value state a_arg b_arg in
       Value.Apply { fn; arg }
     with
     | Some join -> Some join
     | None -> unfold_value state a b ~f:(join_value state))
  | a, (Apply { fn = Binder _; _ } as b) | (Apply { fn = Binder _; _ } as a), b ->
    unfold_value state a b ~f:(join_value state)
  | Proj a, Proj b when a.index = b.index ->
    let%map tuple = join_value state a.tuple b.tuple in
    Value.Proj { tuple; index = a.index }
  | ( ( Unit
      | Bool _
      | Int _
      | Type _
      | Closure _
      | Binder _
      | Var _
      | Apply _
      | Proj _
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
    (match Nonempty_list.map2 a_elts b_elts ~f:(meet_value state) with
     | Ok elts ->
       Nonempty_list.to_list elts
       |> Option.all
       |> Option.map ~f:(fun elts -> Value.Tuple (Nonempty_list.of_list_exn elts))
     | Unequal_lengths -> None)
  | External a, External b when String.equal a.symbol b.symbol -> Some (External a)
  | ( If { cond = a_cond; then_ = a_then; else_ = a_else }
    , (If { cond = b_cond; then_ = b_then; else_ = b_else } as b) ) ->
    (match
       let%map cond = meet_value state a_cond b_cond
       and then_ = meet_value state a_then b_then
       and else_ = meet_value state a_else b_else in
       Value.If { cond; then_; else_ }
     with
     | Some meet -> Some meet
     | None ->
       let%bind then_ = meet_value state a_then b
       and else_ = meet_value state a_else b in
       meet_value state then_ else_)
  | If { then_; else_; _ }, b ->
    let%bind then_ = meet_value state then_ b
    and else_ = meet_value state else_ b in
    meet_value state then_ else_
  | a, If { then_; else_; _ } ->
    let%bind then_ = meet_value state a then_
    and else_ = meet_value state a else_ in
    meet_value state then_ else_
  | (Apply { fn = a_fn; arg = a_arg } as a), (Apply { fn = b_fn; arg = b_arg } as b) ->
    (match
       let%map fn = meet_value state a_fn b_fn
       and arg = meet_value state a_arg b_arg in
       Value.Apply { fn; arg }
     with
     | Some meet -> Some meet
     | None -> unfold_value state a b ~f:(meet_value state))
  | a, (Apply { fn = Binder _; _ } as b) | (Apply { fn = Binder _; _ } as a), b ->
    unfold_value state a b ~f:(meet_value state)
  | Proj a, Proj b when a.index = b.index ->
    let%map tuple = meet_value state a.tuple b.tuple in
    Value.Proj { tuple; index = a.index }
  | ( ( Unit
      | Bool _
      | Int _
      | Type _
      | Closure _
      | Binder _
      | Var _
      | Apply _
      | Proj _
      | External _
      | Tuple _
      | Prim _ )
    , _ ) -> None

and leq_closure (a : Closure.t) (b : Closure.t) = a.hash = b.hash
and join_closure (a : Closure.t) (b : Closure.t) = if a.hash = b.hash then Some a else None
and meet_closure (a : Closure.t) (b : Closure.t) = if a.hash = b.hash then Some a else None
and leq_binder (a : Binder.t) (b : Binder.t) = a.hash = b.hash
and join_binder (a : Binder.t) (b : Binder.t) = if a.hash = b.hash then Some a else None
and meet_binder (a : Binder.t) (b : Binder.t) = if a.hash = b.hash then Some a else None

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

(* Compares structurally. Could use a fancier solver. *)
and unify_bool ~f (a : Bool.t) (b : Bool.t) : Bool.t option =
  match a, b with
  | T a, T b -> if Core.Bool.equal a b then Some (T a) else None
  | Not a, Not b -> Option.map (f a b) ~f:(fun b : Bool.t -> Not b)
  | And (a0, a1), And (b0, b1) ->
    Option.map2 (f a0 b0) (f a1 b1) ~f:(fun a b : Bool.t -> And (a, b))
  | Or (a0, a1), Or (b0, b1) -> Option.map2 (f a0 b0) (f a1 b1) ~f:(fun a b : Bool.t -> Or (a, b))
  | Eq (a0, a1), Eq (b0, b1) -> Option.map2 (f a0 b0) (f a1 b1) ~f:(fun a b : Bool.t -> Eq (a, b))
  | Neq (a0, a1), Neq (b0, b1) ->
    Option.map2 (f a0 b0) (f a1 b1) ~f:(fun a b : Bool.t -> Neq (a, b))
  | Lt (a0, a1), Lt (b0, b1) -> Option.map2 (f a0 b0) (f a1 b1) ~f:(fun a b : Bool.t -> Lt (a, b))
  | Lte (a0, a1), Lte (b0, b1) ->
    Option.map2 (f a0 b0) (f a1 b1) ~f:(fun a b : Bool.t -> Lte (a, b))
  | Gt (a0, a1), Gt (b0, b1) -> Option.map2 (f a0 b0) (f a1 b1) ~f:(fun a b : Bool.t -> Gt (a, b))
  | Gte (a0, a1), Gte (b0, b1) ->
    Option.map2 (f a0 b0) (f a1 b1) ~f:(fun a b : Bool.t -> Gte (a, b))
  | _ -> None

and join_bool state a b = unify_bool ~f:(join_value state) a b
and meet_bool state a b = unify_bool ~f:(meet_value state) a b

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

(* Compares structurally. Could use a fancier solver. *)
and unify_int ~f (a : Int.t) (b : Int.t) : Int.t option =
  match a, b with
  | T a, T b -> if Int64.equal a b then Some (T a) else None
  | Neg a, Neg b -> Option.map (f a b) ~f:(fun i : Int.t -> Neg i)
  | Add (a0, a1), Add (b0, b1) -> Option.map2 (f a0 b0) (f a1 b1) ~f:(fun a b : Int.t -> Add (a, b))
  | Sub (a0, a1), Sub (b0, b1) -> Option.map2 (f a0 b0) (f a1 b1) ~f:(fun a b : Int.t -> Sub (a, b))
  | Mul (a0, a1), Mul (b0, b1) -> Option.map2 (f a0 b0) (f a1 b1) ~f:(fun a b : Int.t -> Mul (a, b))
  | Div (a0, a1), Div (b0, b1) -> Option.map2 (f a0 b0) (f a1 b1) ~f:(fun a b : Int.t -> Div (a, b))
  | Mod (a0, a1), Mod (b0, b1) -> Option.map2 (f a0 b0) (f a1 b1) ~f:(fun a b : Int.t -> Mod (a, b))
  | _ -> None

and join_int state a b = unify_int ~f:(join_value state) a b
and meet_int state a b = unify_int ~f:(meet_value state) a b

and leq_ty state a b =
  match a, b with
  | Unit, Unit | Bool, Bool | Int, Int | Type, Type -> true
  | Tuple a_elts, Tuple b_elts ->
    (match Nonempty_list.zip a_elts b_elts with
     | Ok zip -> Nonempty_list.for_all zip ~f:(fun (a, b) -> leq_value state a b)
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

and join_ty state a b =
  match a, b with
  | Unit, Unit -> Some Unit
  | Bool, Bool -> Some Bool
  | Int, Int -> Some Int
  | Type, Type -> Some Type
  | Tuple a_elts, Tuple b_elts ->
    (match Nonempty_list.map2 a_elts b_elts ~f:(join_value state) with
     | Ok elts ->
       Nonempty_list.to_list elts
       |> Option.all
       |> Option.map ~f:(fun elts -> Ty.Tuple (Nonempty_list.of_list_exn elts))
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
      ; ret_ty = Dependent.join (Dependent.mono a_ret_ty) b_ret_ty
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
      ; ret_ty = Dependent.join a_ret_ty (Dependent.mono b_ret_ty)
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
    (match Nonempty_list.map2 a_elts b_elts ~f:(meet_value state) with
     | Ok elts ->
       Nonempty_list.to_list elts
       |> Option.all
       |> Option.map ~f:(fun elts -> Ty.Tuple (Nonempty_list.of_list_exn elts))
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
  match expr with
  | If { cond; then_; else_; static; loc } ->
    typecheck_if state env ~cond ~then_ ~else_ ~static ~loc
  | Match { cond; arms; static; loc } ->
    typecheck_match state env ~scrutinee:cond ~arms ~static ~loc
  | Lambda { arg; arg_mode; arg_ty; body = body_dst; loc } ->
    typecheck_lambda state env ~arg ~arg_mode ~arg_ty ~body_dst ~loc
  | Apply { fn; arg; loc } -> typecheck_apply state env ~fn ~arg ~loc
  | Literal { value; loc } ->
    let ty = Value.Type (Ty.of_literal value) in
    let value = Value.of_literal value in
    let mode = Modes.create ~staticity:Static ~erasure:Unerased in
    Literal { value; ty; mode; loc }, { ty; mode; static = Lazy.from_val value }
  | Builtin { builtin; loc } ->
    let desc = Builtin.desc builtin in
    Builtin { builtin; ty = desc.ty; mode = desc.mode; loc }, desc
  | Var { id; loc } ->
    let desc = require_var ~loc env id in
    (match desc.mode.erasure with
     | Erased -> Erased { ty = desc.ty; mode = desc.mode; loc }, desc
     | Unerased -> Var { id; ty = desc.ty; mode = desc.mode; loc }, desc)
  | Let { var; bind; rest; loc } ->
    let bind, bind_desc = typecheck state env bind in
    let env = Env.bind env var bind_desc in
    let rest, rest_desc = typecheck state env rest in
    Let { var; bind; rest; ty = rest_desc.ty; mode = rest_desc.mode; loc }, rest_desc
  | Fun { funs; rest; loc } ->
    let funs, env = typecheck_funs state env funs in
    let rest, rest_desc = typecheck state env rest in
    (match Nonempty_list.of_list funs with
     | Some funs -> Fun { funs; rest; ty = rest_desc.ty; mode = rest_desc.mode; loc }, rest_desc
     | None -> rest, rest_desc)
  | Arrow { arg; arg_mode; arg_id; ret; ret_mode; loc } ->
    let ty = Value.Type Type in
    let mode = Modes.create ~staticity:Static ~erasure:Erased in
    let value =
      typecheck_arrow state ~loc env ~arg_id ~arg_ty:arg ~arg_mode ~ret_ty:ret ~ret_mode
    in
    Literal { value; ty; mode; loc }, { ty; mode; static = Lazy.from_val value }
  | Tuple { elts; loc } ->
    let ty = Value.Type Type in
    let mode = Modes.create ~staticity:Static ~erasure:Erased in
    let elts =
      Nonempty_list.map elts ~f:(fun elt ->
        let elt_desc = reduce state env elt in
        require_static_type state ~loc elt_desc)
    in
    let value = Value.Type (Tuple elts) in
    Literal { value; ty; mode; loc }, { ty; mode; static = Lazy.from_val value }
  | Mode_annotation { expr; mode; loc } ->
    let expr, desc = typecheck state env expr in
    let mode = require_mode_annotation desc.mode mode in
    require_mode ~loc desc.mode mode;
    weaken ~loc expr desc ~ty:desc.ty ~mode
  | Type_annotation { expr; ty; loc } ->
    let expr, desc = typecheck state env expr in
    let ty_desc = reduce state env ty in
    let ty = require_static_type state ~loc ty_desc in
    require_leq state ~loc desc.ty ty;
    weaken ~loc expr desc ~ty ~mode:desc.mode
  | Unreachable { loc } ->
    if State.concrete state
    then Fail.unreachable_reached [%here] ~loc
    else (
      let mode = Modes.bottom () in
      let ty = Value.Bottom in
      let value = Value.Bottom in
      Literal { value; ty; mode; loc }, { Desc.ty; mode; static = Lazy.from_val value })
  | Make_tuple { elts; loc } ->
    (* TODO could maybe be a primitive *)
    let elts = Nonempty_list.map elts ~f:(fun elt -> typecheck state env elt) in
    Expr.tuple ~loc elts

and typecheck_arrow state ~loc env ~arg_id ~arg_ty ~arg_mode ~ret_ty ~ret_mode : Value.t =
  let arg_desc = reduce state env arg_ty in
  let arg_mode = require_mode_annotation (Modes.default ()) arg_mode in
  (* TODO default to static? *)
  let ret_mode = require_mode_annotation (Modes.default ()) ret_mode in
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
  let fun_names =
    Nonempty_list.map funs ~f:(fun f -> f.var) |> Nonempty_list.to_list |> Ident.Set.of_list
  in
  let fun_mode =
    Nonempty_list.fold funs ~init:(Modes.bottom ()) ~f:(fun acc f ->
      Set.diff (Set.remove (Dst.Expr.free_vars f.body) f.arg) fun_names
      |> Set.fold ~init:acc ~f:(fun acc id ->
        match Env.find env id with
        | Some desc -> Modes.capture acc ~fv:desc.mode
        | None -> acc))
  in
  let env_rec = ref env in
  let env =
    Nonempty_list.fold
      funs
      ~init:env
      ~f:
        (fun
          acc
          { Dst.Expr.var; arg; erased; arg_ty; arg_mode; ret_mode; ret_ty; body = body_dst; loc }
        ->
        let family = State.family state ~loc in
        let ty = typecheck_arrow state ~loc env ~arg_id:arg ~arg_ty ~arg_mode ~ret_ty ~ret_mode in
        let fun_mode = Modes.join fun_mode (Modes.bottom ~erasure:erased ()) in
        let desc : Desc.t =
          match ty with
          | Type (Pi { ret_mode; _ }) ->
            let fun_mode = Modes.return fun_mode ~ret:ret_mode in
            let static =
              Lazy.from_fun (fun () ->
                Value.Binder
                  { arg; ty; body_dst; env = !env_rec; family; hash = State.fresh_id state })
            in
            { ty; mode = fun_mode; static }
          | Type (Arrow { arg_ty; ret_ty; ret_mode; _ }) ->
            let arg_mode =
              require_mode_annotation (Modes.default ~staticity:Parametric ()) arg_mode
            in
            let fun_mode = Modes.return fun_mode ~ret:ret_mode in
            let static =
              Lazy.from_fun (fun () ->
                let body, body_desc =
                  let env =
                    Env.bind
                      !env_rec
                      arg
                      { ty = arg_ty; mode = arg_mode; static = Fail.unreachable [%here] ~loc }
                  in
                  typecheck state env body_dst
                in
                if Modes.is_erased ret_mode then require_static ~loc body_desc;
                let body_desc = resolve_body_mode arg_mode body_desc in
                require_mode ~loc body_desc.mode ret_mode;
                require_leq state ~loc body_desc.ty ret_ty;
                Value.Closure
                  { arg; ty; body; body_dst; env = !env_rec; family; hash = State.fresh_id state })
            in
            let arg_mode = { arg_mode with staticity = Dynamic } in
            { Desc.ty = Type (Arrow { arg_ty; arg_mode; ret_ty; ret_mode })
            ; mode = fun_mode
            ; static
            }
          | _ -> raise_s [%message "Bug: expected function type"]
        in
        Env.bind acc var desc)
  in
  env_rec := env;
  let funs =
    Nonempty_list.filter_map funs ~f:(fun { var; loc; _ } : Expr.fun_ option ->
      let { Desc.ty; mode; static } = require_var ~loc env var in
      let value = Lazy.force static in
      State.record_group state var { ty; mode; static };
      match ty, value with
      | Type (Arrow _), Closure { arg; ty; body; family; _ } ->
        if Modes.is_unerased mode
        then Some (Lambda { var; arg; body; ty; mode; family; loc })
        else None
      | Type (Pi { arg_ty; arg_mode; ret_mode; ret_ty }), Binder { arg; ty; body_dst; family; _ } ->
        let arg_val = State.fresh_var state in
        let env =
          Env.bind env arg { ty = arg_ty; mode = arg_mode; static = Lazy.from_val arg_val }
        in
        let body_desc = reduce state env body_dst in
        if Modes.is_erased ret_mode then require_static ~loc body_desc;
        let body_desc = resolve_body_mode arg_mode body_desc in
        require_mode ~loc body_desc.mode ret_mode;
        require_leq state ~loc body_desc.ty (eval state ret_ty arg_val);
        if Modes.is_unerased mode
        then Some (Binder { var; arg; body = Value.Concrete.Map.empty; ty; mode; family; loc })
        else None
      | _ -> raise_s [%message "Bug: expected function type"])
  in
  funs, env

and typecheck_lambda state env ~arg ~arg_mode ~arg_ty ~body_dst ~loc =
  let family = State.family state ~loc in
  let arg_ty_desc = reduce state env arg_ty in
  let arg_ty = require_static_type state ~loc arg_ty_desc in
  let arg_mode = require_mode_annotation (Modes.default ~staticity:Parametric ()) arg_mode in
  let fn_mode =
    Set.remove (Dst.Expr.free_vars body_dst) arg
    |> Set.fold ~init:(Modes.bottom ()) ~f:(fun acc id ->
      match Env.find env id with
      | Some desc -> Modes.capture acc ~fv:desc.mode
      | None -> acc)
  in
  match arg_mode.staticity with
  | Dynamic | Parametric ->
    let body, body_desc =
      let env =
        Env.bind env arg { ty = arg_ty; mode = arg_mode; static = Fail.unreachable [%here] ~loc }
      in
      typecheck state env body_dst
    in
    let body_desc = resolve_body_mode arg_mode body_desc in
    let arg_mode = { arg_mode with staticity = Dynamic } in
    let ty =
      Value.Type (Arrow { arg_ty; arg_mode; ret_ty = body_desc.ty; ret_mode = body_desc.mode })
    in
    let fn_mode = Modes.return fn_mode ~ret:body_desc.mode in
    let static =
      Lazy.from_val
        (Value.Closure { arg; ty; body; body_dst; env; family; hash = State.fresh_id state })
    in
    let expr : Expr.t =
      if Modes.is_erased fn_mode
      then Erased { ty; mode = fn_mode; loc }
      else Lambda { arg; ty; body; mode = fn_mode; family; loc }
    in
    expr, { Desc.ty; mode = fn_mode; static }
  | Static ->
    let body_desc =
      let env =
        Env.bind
          env
          arg
          { ty = arg_ty; mode = arg_mode; static = Lazy.from_val (State.fresh_var state) }
      in
      reduce state env body_dst
    in
    let body_desc = resolve_body_mode arg_mode body_desc in
    let ty =
      let ret_ty = Dependent.typecheck body_desc.ty ~env ~arg ~arg_ty ~arg_mode ~body:body_dst in
      Value.Type (Pi { arg_ty; arg_mode; ret_ty; ret_mode = body_desc.mode })
    in
    let fn_mode = Modes.return fn_mode ~ret:body_desc.mode in
    let static =
      Lazy.from_val (Value.Binder { arg; ty; body_dst; env; family; hash = State.fresh_id state })
    in
    let expr : Expr.t =
      if Modes.is_erased fn_mode
      then Erased { ty; mode = fn_mode; loc }
      else Binder { arg; ty; body = Value.Concrete.Map.empty; mode = fn_mode; family; loc }
    in
    expr, { ty; mode = fn_mode; static }

and specialize state ~loc (binder : Binder.t) ~arg_ty ~arg_mode ~ret_ty ~ret_mode ~arg_val key =
  State.specialize state { hash = binder.hash; key } ~f:(fun () ->
    let env =
      Env.bind
        binder.env
        binder.arg
        { ty = arg_ty; mode = arg_mode; static = Lazy.from_val arg_val }
    in
    let body, body_desc =
      State.with_spec state { family = binder.family; key } ~f:(fun () ->
        State.with_app state ~f:(fun () -> typecheck state env binder.body_dst))
    in
    let body =
      if Modes.is_erased arg_mode
      then body
      else
        Expr.Let
          { var = binder.arg
          ; bind = Literal { value = arg_val; ty = arg_ty; mode = arg_mode; loc }
          ; rest = body
          ; ty = body_desc.ty
          ; mode = body_desc.mode
          ; loc
          }
    in
    let body, desc = weaken ~loc body body_desc ~ty:ret_ty ~mode:ret_mode in
    { State.Mono.family = binder.family; body; desc })

and typecheck_apply state env ~fn ~arg ~loc =
  let open Lazy.Let_syntax in
  let fn, fn_desc = typecheck state env fn in
  let arg, arg_desc = typecheck state env arg in
  match fn_desc.ty with
  | Type (Pi { arg_ty; arg_mode; ret_ty; ret_mode }) ->
    require_static ~loc fn_desc;
    require_mode ~loc arg_desc.mode arg_mode;
    require_leq state ~loc arg_desc.ty arg_ty;
    let mode = resolve_app_mode ~loc fn_desc ret_mode in
    let fn_val = force_or_var state fn_desc.static in
    let arg_val = force_or_var state arg_desc.static in
    let key = concrete arg_val in
    let ty =
      match fn_val, key with
      | Binder binder, Some key ->
        State.with_spec state { family = binder.family; key } ~f:(fun () ->
          eval state ret_ty arg_val)
      | _ -> eval state ret_ty arg_val
    in
    (match fn_val, key with
     | Value.Binder binder, Some key ->
       let mono =
         specialize state ~loc binder ~arg_ty ~arg_mode ~ret_ty:ty ~ret_mode ~arg_val key
       in
       let desc = { Desc.ty; mode; static = mono.desc.static } in
       if Modes.is_erased mode
       then Erased { ty; mode; loc }, desc
       else
         Specialize { fn; arg; target = Family binder.family; key = Some key; ty; mode; loc }, desc
     | Binder binder, None ->
       let static =
         if Modes.is_static ret_mode
         then Lazy.from_val (Value.reduce (Apply { fn = Binder binder; arg = arg_val }))
         else Fail.unreachable [%here] ~loc
       in
       let desc = { Desc.ty; mode; static } in
       if Modes.is_erased mode
       then Erased { ty; mode; loc }, desc
       else Apply { fn; arg; ty; mode; loc }, desc
     | Closure closure, _ ->
       let static =
         if Modes.is_static ret_mode
         then (
           let env =
             Env.bind
               closure.env
               closure.arg
               { ty = arg_ty; mode = arg_mode; static = Lazy.from_val arg_val }
           in
           (reduce state env closure.body_dst).static)
         else Fail.unreachable [%here] ~loc
       in
       let desc = { Desc.ty; mode; static } in
       if Modes.is_erased mode
       then Erased { ty; mode; loc }, desc
       else Specialize { fn; arg; ty; target = Family closure.family; key = None; mode; loc }, desc
     | Prim prim, _ ->
       let static =
         if Modes.is_static ret_mode
         then
           Lazy.from_fun (fun () ->
             try Builtin.eval prim arg_val with
             | Builtin.Error err -> Fail.static_failure [%here] ~loc err)
         else Fail.unreachable [%here] ~loc
       in
       let desc = { Desc.ty; mode; static } in
       if Modes.is_erased mode
       then (
         (* Static computation stays lazy except for asserts, which exist to
            be checked: an unused [assert erased] must still fire. *)
         (match prim with
          | Assert_erased -> ignore (Lazy.force static : Value.t)
          | _ -> ());
         Erased { ty; mode; loc }, desc)
       else Specialize { fn; arg; ty; target = Prim prim; key = None; mode; loc }, desc
     | fn_val, _ ->
       let static =
         if Modes.is_static ret_mode
         then Lazy.from_val (Value.reduce (Apply { fn = fn_val; arg = arg_val }))
         else Fail.unreachable [%here] ~loc
       in
       let desc = { Desc.ty; mode; static } in
       if Modes.is_erased mode
       then Erased { ty; mode; loc }, desc
       else Apply { fn; arg; ty; mode; loc }, desc)
  | Type (Arrow { arg_ty; arg_mode; ret_ty; ret_mode }) ->
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
          let desc = reduce state env closure.body_dst in
          desc.static
        | Prim prim ->
          Lazy.map arg_desc.static ~f:(fun arg ->
            try Builtin.eval prim arg with
            | Builtin.Error err -> Fail.static_failure [%here] ~loc err)
        | _ -> Lazy.map arg_desc.static ~f:(fun arg -> Value.reduce (Apply { fn; arg })))
      else Fail.unreachable [%here] ~loc
    in
    let mode = resolve_app_mode ~loc fn_desc ret_mode in
    let desc = { Desc.ty = ret_ty; mode; static } in
    if Modes.is_erased mode
    then Erased { ty = ret_ty; mode; loc }, desc
    else Apply { fn; arg; ty = ret_ty; mode; loc }, desc
  | _ -> Fail.expected_function [%here] ~loc fn_desc.ty arg_desc.ty

and typecheck_if state env ~cond ~then_ ~else_ ~static ~loc =
  let open Lazy.Let_syntax in
  let cond, cond_desc = typecheck state env cond in
  require_leq state ~loc cond_desc.ty (Type Bool);
  match static with
  | Static ->
    require_static ~loc cond_desc;
    (match force_or_var state cond_desc.static with
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
       let mode = Modes.cond ~cond:cond_desc.mode [ then_desc.mode; else_desc.mode ] in
       let static =
         if Modes.is_dynamic mode
         then Fail.unreachable [%here] ~loc
         else (
           let%map then_ = then_desc.static
           and else_ = else_desc.static in
           Value.reduce (If { cond = value; then_; else_ }))
       in
       (* Resolve the [if]-value here if possible. *)
       let ty =
         match join_value state then_desc.ty else_desc.ty with
         | Some ty -> ty
         | None -> Value.reduce (If { cond = value; then_ = then_desc.ty; else_ = else_desc.ty })
       in
       If { cond; then_; else_; ty; mode; loc }, { ty; mode; static })
  | Dynamic | Parametric ->
    require_unerased ~loc cond_desc;
    let then_, then_desc = typecheck state env then_ in
    let else_, else_desc = typecheck state env else_ in
    let mode = Modes.cond ~cond:cond_desc.mode [ then_desc.mode; else_desc.mode ] in
    let static =
      if Modes.is_dynamic mode
      then Fail.unreachable [%here] ~loc
      else
        Lazy.bind cond_desc.static ~f:(function
          | Bool (T true) -> then_desc.static
          | Bool (T false) -> else_desc.static
          | cond ->
            let%map then_ = then_desc.static
            and else_ = else_desc.static in
            Value.reduce (If { cond; then_; else_ }))
    in
    let ty = require_join state ~loc then_desc.ty else_desc.ty in
    If { cond; then_; else_; ty; mode; loc }, { ty; mode; static }

and pattern_bindings state ~desc (pattern : Dst.Expr.pattern) : Desc.t Ident.Map.t =
  (* TODO dependent match *)
  match pattern with
  | Var { id; _ } -> if Ident.is_anon id then Ident.Map.empty else Ident.Map.singleton id desc
  | Literal { value; loc } ->
    require_leq ~loc state desc.ty (Value.Type (Ty.of_literal value));
    Ident.Map.empty
  | Tuple { elts; loc } ->
    (match desc.ty with
     | Type (Tuple elt_tys) when Nonempty_list.length elt_tys = Nonempty_list.length elts ->
       Nonempty_list.zip_exn elts elt_tys
       |> Nonempty_list.mapi ~f:(fun index (elt, ty) ->
         let static =
           Lazy.map desc.static ~f:(fun value ->
             match Value.reduce value with
             | Tuple elts -> Nonempty_list.nth_exn elts index
             | (Var _ | If _ | Apply _ | Proj _) as tuple -> Value.reduce (Proj { tuple; index })
             | value ->
               raise_s [%message "Bug: expected tuple" (value : Value.t) (loc : Lex.Location.t)])
         in
         elt, { Desc.ty; mode = desc.mode; static })
       |> Nonempty_list.fold ~init:Ident.Map.empty ~f:(fun acc (elt, elt_desc) ->
         pattern_bindings state ~desc:elt_desc elt
         |> Map.merge_skewed acc ~combine:(fun ~key _ _ ->
           Fail.Match.multiple_bindings [%here] ~loc key))
     | _ -> Fail.Match.expected_tuple [%here] ~loc desc.ty)
  | Or { left; right; loc } ->
    let lhs = pattern_bindings state ~desc left in
    let rhs = pattern_bindings state ~desc right in
    let diff =
      Set.symmetric_diff (Map.key_set lhs) (Map.key_set rhs)
      |> Sequence.map ~f:Either.value
      |> Sequence.to_list
    in
    (match diff with
     | [] -> ()
     | id :: rest -> Fail.Match.or_unbound [%here] ~loc (Nonempty_list.create id rest));
    Map.merge_skewed lhs rhs ~combine:(fun ~key:_ lhs rhs -> require_join_desc state ~loc lhs rhs)

and build_condition ~loc ~scrutinee ~scrutinee_desc (literal : Dst.Literal.t) : Expr.t =
  match literal with
  | Unit -> Expr.literal ~loc (Bool true)
  | Bool true -> scrutinee
  | Bool false -> Builtin.apply ~loc (Bool Not) scrutinee scrutinee_desc
  | Int i ->
    let arg, arg_desc =
      let lit = Expr.literal ~loc (Int i) in
      let lit_desc = Expr.desc lit (Lazy.from_val (Value.Int (T i))) in
      Expr.tuple ~loc (Nonempty_list.create (scrutinee, scrutinee_desc) [ lit, lit_desc ])
    in
    Builtin.apply ~loc (Int Eq) arg arg_desc

and typecheck_match state env ~scrutinee ~arms ~static:_ ~loc =
  (* TODO dependent match *)
  let scrutinee, scrutinee_desc = typecheck state env scrutinee in
  require_unerased ~loc scrutinee_desc;
  let patterns, (cases, descs) =
    let patterns, cases =
      Nonempty_list.map arms ~f:(fun (pattern, body) ->
        let bindings = pattern_bindings state ~desc:scrutinee_desc pattern in
        let binding_tys = Map.map bindings ~f:(fun (desc : Desc.t) -> desc.ty) in
        let env = Map.fold bindings ~init:env ~f:(fun ~key ~data env -> Env.bind env key data) in
        let body, body_desc = typecheck state env body in
        pattern, ({ Expr.body; bindings = binding_tys }, body_desc))
      |> Nonempty_list.unzip
    in
    patterns, Nonempty_list.unzip cases
  in
  let compiled =
    let compiled = Match.compile ~ty:scrutinee_desc.ty patterns in
    (match compiled.redundant with
     | [] -> ()
     | first :: rest -> Fail.Match.redundant [%here] ~loc (Nonempty_list.create first rest));
    (match compiled.missing with
     | [] -> ()
     | first :: rest -> Fail.Match.non_exhaustive [%here] ~loc (Nonempty_list.create first rest));
    compiled
  in
  let ty, mode =
    let ty, mode =
      let ({ ty; mode; _ } :: rest) = descs in
      List.fold rest ~init:(ty, mode) ~f:(fun (ty, mode) desc ->
        require_join state ~loc ty desc.ty, Modes.join mode desc.mode)
    in
    ty, Modes.cond ~cond:scrutinee_desc.mode [ mode ]
  in
  let expr =
    Expr.rebind scrutinee ~stamp:(State.fresh_id state) ~f:(fun scrutinee ->
      let project path =
        let rec aux path expr (desc : Desc.t) =
          match path with
          | [] -> expr, desc
          | index :: rest ->
            (match desc.ty with
             | Type (Tuple elt_tys) ->
               let ty = Nonempty_list.nth_exn elt_tys index in
               let get = Expr.Tuple_get { tuple = expr; index; ty; mode = desc.mode; loc } in
               let static =
                 Lazy.map desc.static ~f:(fun (v : Value.t) ->
                   match Value.reduce v with
                   | Tuple elts -> Nonempty_list.nth_exn elts index
                   | (Var _ | If _ | Apply _ | Proj _) as tuple ->
                     Value.reduce (Proj { tuple; index })
                   | v ->
                     raise_s [%message "Bug: expected tuple" (v : Value.t) (loc : Lex.Location.t)])
               in
               aux rest get { Desc.ty; mode = desc.mode; static }
             | ty -> raise_s [%message "Bug: expected tuple" (ty : Value.t) (loc : Lex.Location.t)])
        in
        aux (Vec.to_list path) scrutinee scrutinee_desc
      in
      let rec build_switch (tree : Match.Tree.t) : Expr.tree =
        match tree with
        | Leaf { case; bindings } ->
          let bindings =
            Map.map bindings ~f:(fun (occurrence : Match.Tree.Occurrence.t) ->
              let expr, _ = project occurrence.path in
              expr)
          in
          Leaf { case; bindings }
        | Switch { occurrence; cases; default } ->
          let scrutinee, scrutinee_desc = project occurrence.path in
          let build_case ((constructor, body) : Match.Tree.Constructor.t * _) acc =
            match constructor with
            | Literal value ->
              let cond = build_condition ~loc ~scrutinee ~scrutinee_desc value in
              let then_ = build_switch body in
              Expr.Split { cond; then_; else_ = acc }
            | Tuple _ -> build_switch body
          in
          Array.fold_right
            cases
            ~init:(Option.map default ~f:build_switch)
            ~f:(fun (constructor, tree) acc ->
              match acc with
              | None -> Some (build_switch tree)
              | Some acc -> Some (build_case (constructor, tree) acc))
          |> Option.value_exn
        | Fail -> raise_s [%message "Bug: nonexhaustive match" (loc : Lex.Location.t)]
      in
      Expr.Match { cases; tree = build_switch compiled.tree; ty; mode; loc })
  in
  expr, { Desc.ty; mode; static = Fail.unreachable [%here] ~loc }
;;

let typecheck_top_level state env (dst : Dst.Top_level.t) : Top_level.t * Env.t =
  match dst with
  | Let { var; bind; loc } ->
    let bind, bind_desc = typecheck state env bind in
    let env = Env.bind env var bind_desc in
    Let { var; bind; loc }, env
  | Fun { funs; loc } ->
    let funs, env = typecheck_funs state env funs in
    (match Nonempty_list.of_list funs with
     | Some funs -> Fun { funs; loc }, env
     | None -> Erased { loc }, env)
  | External { var; ty; symbol; loc } ->
    let ty_desc = reduce state env ty in
    let ty = require_static_type ~loc state ty_desc in
    require_dynamic_arrow ~loc var symbol ty;
    let mode = Modes.bottom () in
    let static = Lazy.from_val (Value.External { symbol; ty }) in
    External { var; symbol; ty; mode; loc }, Env.bind env var { Desc.ty; mode; static }
  | Builtin { var; name; loc } ->
    let builtin =
      match Builtin.find name with
      | Some builtin -> builtin
      | None -> Fail.unknown_builtin [%here] ~loc var name
    in
    let desc = Builtin.desc builtin in
    Builtin { var; builtin; ty = desc.ty; mode = desc.mode; loc }, Env.bind env var desc
;;

let fold_top_levels state env dst tls =
  List.fold dst ~init:(tls, env) ~f:(fun (acc, env) top_level ->
    let tl, env = typecheck_top_level state env top_level in
    tl :: acc, env)
;;

let typecheck_exn (dst : Dst.Program.t) : Program.t =
  let state = State.create ~stamp:dst.stamp in
  let env = Env.initial in
  let program = List.rev (fst (fold_top_levels state env dst.top_levels [])) in
  let groups = Core.Int.Map.of_hashtbl_exn state.groups in
  (* Check reify doesn't expand the monomorphization store. *)
  let specialization_count = Hashtbl.length state.specializations in
  let top_levels = Reify.program ~monos:(State.collect_monos state) ~groups program in
  if Hashtbl.length state.specializations <> specialization_count
  then raise_s [%message "Bug: specializations created during reification"];
  { top_levels; stamp = state.stamp }
;;

let typecheck tst =
  try Ok (typecheck_exn tst) with
  | Error err -> Error err
;;
