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
      | Payload_mismatch of
          { label : Ident.Label.t
          ; required : bool
          }
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
    | Duplicate_label of Ident.Label.t
    | Unknown_label of
        { from : Value.t
        ; label : Ident.Label.t
        }
    | Expected_variant of
        { got : Value.t
        ; label : Ident.Label.t
        }
    | Unknown_builtin of Ident.t * string
    | Static_external of Ident.t * string
    | Static_failure of Builtin.Error.t
    | Static_cycle
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

    let payload_mismatch ~loc here label ~required =
      raise (Error { loc; here; reason = Match (Payload_mismatch { label; required }) })
    ;;
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
  let static_cycle ~loc here = raise (Error { loc; here; reason = Static_cycle })

  let erased_application ~loc here fn result =
    raise (Error { loc; here; reason = Erased_application { fn; result } })
  ;;

  let expected_function ~loc here fn arg =
    raise (Error { loc; here; reason = Expected_function { fn; arg } })
  ;;

  let duplicate_label ~loc here label = raise (Error { loc; here; reason = Duplicate_label label })

  let unknown_label ~loc here from label =
    raise (Error { loc; here; reason = Unknown_label { from; label } })
  ;;

  let expected_variant ~loc here got label =
    raise (Error { loc; here; reason = Expected_variant { got; label } })
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

  let fresh_var t = Value.var (Ident.create Ident.Raw.anon ~stamp:(fresh_id t))

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
    let%bind arg = concrete arg_ty in
    let%map ret = concrete ret_ty in
    Value.Concrete.Arrow { arg; arg_mode; ret; ret_mode }
  | Type (Tuple elts) ->
    let%map elts = Nonempty_list.map elts ~f:concrete |> Nonempty_list.to_list |> Option.all in
    Value.Concrete.Tuple_t (Nonempty_list.of_list_exn elts)
  | Type (Variant constructors) ->
    with_return (fun { return } ->
      Some
        (Value.Concrete.Variant_t
           (Map.map constructors ~f:(fun payload ->
              Option.map payload ~f:(fun payload ->
                match concrete payload with
                | Some payload -> payload
                | None -> return None)))))
  | External { symbol; _ } -> Some (External symbol)
  | Inject { label; ty } ->
    let%map ty = concrete ty in
    Value.Concrete.Inject { label; ty }
  | Constructor { label; payload = None } ->
    Some (Value.Concrete.Constructor { label; payload = None })
  | Constructor { label; payload = Some payload } ->
    let%map payload = concrete payload in
    Value.Concrete.Constructor { label; payload = Some payload }
  | Bottom | Bool _ | Int _ | Var _ | Apply _ | Proj _ | Payload _ | Match _ | Type (Pi _) -> None
;;

(* A static forced during its own computation becomes abstract. *)
let force_or_var state static =
  try Lazy.force static with
  | Lazy.Undefined -> State.fresh_var state
;;

(* A cyclic condition can never be resolved, so fail eagerly. *)
let force_or_cyclic ~loc static =
  try Lazy.force static with
  | Lazy.Undefined -> Fail.static_cycle [%here] ~loc
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

let rebind_condition env (expr : Expr.t) value =
  match expr with
  | Var { id; ty; mode; _ } -> Env.bind env id { Desc.ty; mode; static = Lazy.from_val value }
  (* TODO non-variable scrutinees need refinement keyed on values (equality types) *)
  | _ -> env
;;

let payload_or_fresh state value ~label =
  match (value : Value.t) with
  | Constructor { label = got; _ } when not (Ident.Label.equal got label) -> State.fresh_var state
  | value -> Value.payload value ~label
;;

let rec rebind_scrutinee state env (scrutinee : Dst.Expr.t) (pattern : Dst.Expr.pattern) value =
  let rec specialize_matched (pattern : Dst.Expr.pattern) (ty : Value.t) default =
    match pattern with
    | Var _ -> default
    | Or _ ->
      (* TODO or-patterns would need refinement to some kind of or-type *)
      default
    | Literal { value; _ } -> Value.of_literal value
    | Constructor { label; payload; _ } ->
      (match ty with
       | Type (Variant constructors) ->
         (match payload, Map.find constructors label with
          | None, Some None -> Value.constructor ~label ~payload:None
          | Some payload, Some (Some payload_ty) ->
            let payload =
              specialize_matched payload payload_ty (payload_or_fresh state default ~label)
            in
            Value.constructor ~label ~payload:(Some payload)
          | (None | Some _), _ -> default)
       | _ -> default)
    | Tuple { elts; _ } ->
      (match ty with
       | Type (Tuple elt_tys) when Nonempty_list.length elt_tys = Nonempty_list.length elts ->
         Value.tuple
           (Nonempty_list.zip_exn elts elt_tys
            |> Nonempty_list.mapi ~f:(fun index (elt, elt_ty) ->
              specialize_matched elt elt_ty (Value.proj default index)))
       | _ -> default)
  in
  match scrutinee, pattern with
  | Var { id; _ }, _ ->
    (match Env.find env id with
     | Some desc ->
       Env.bind
         env
         id
         { desc with static = Lazy.from_val (specialize_matched pattern desc.ty value) }
     | None -> env)
  | (Type_annotation { expr; _ } | Mode_annotation { expr; _ }), _ ->
    rebind_scrutinee state env expr pattern value
  | Make_tuple { elts; _ }, Tuple { elts = pats; _ } ->
    (match Nonempty_list.zip elts pats with
     | Ok pairs ->
       Nonempty_list.to_list pairs
       |> List.foldi ~init:env ~f:(fun index env (elt, pat) ->
         rebind_scrutinee state env elt pat (Value.proj value index))
     | Unequal_lengths -> env)
  (* TODO non-variable scrutinees need refinement keyed on values (equality types) *)
  | _ -> env
;;

let try_select_arm (scrutinee : Value.t) patterns =
  let rec aux index = function
    | [] -> None
    | pattern :: rest ->
      (match Value.matches_pattern scrutinee pattern with
       | Match bindings -> Some (index, bindings)
       | No_match -> aux (index + 1) rest
       | Unknown -> None)
  in
  match scrutinee with
  | Bottom -> None
  | _ -> aux 0 (Nonempty_list.to_list patterns)
;;

let compile_match ~loc ~ty patterns =
  let compiled = Match.compile ~ty patterns in
  (match compiled.redundant with
   | [] -> ()
   | first :: rest -> Fail.Match.redundant [%here] ~loc (Nonempty_list.create first rest));
  (match compiled.missing with
   | [] -> ()
   | first :: rest -> Fail.Match.non_exhaustive [%here] ~loc (Nonempty_list.create first rest));
  compiled
;;

let rec require_leq state ~loc src dst =
  if not (leq_value state src dst) then Fail.type_mismatch [%here] ~loc src dst

and require_join state ~loc ty1 ty2 =
  match join_value state ty1 ty2 with
  | Some ty -> ty
  | None -> Fail.cannot_unify [%here] ~loc ty1 ty2

and require_static_type ~loc state (desc : Desc.t) =
  require_static ~loc desc;
  require_leq state ~loc desc.ty (Value.type_ Type);
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
  let leq_stuck_match state (a : Value.t) (b : Value.t) =
    match a, b with
    | Match { arms; _ }, _ ->
      Nonempty_list.for_all arms ~f:(fun (_, leaf) -> leq_value state leaf b)
    | _, Match { arms; _ } ->
      Nonempty_list.for_all arms ~f:(fun (_, leaf) -> leq_value state a leaf)
    | _, _ -> false
  in
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
  | ( Match { scrutinee = a_scrutinee; arms = a_arms }
    , Match { scrutinee = b_scrutinee; arms = b_arms } )
    when Value.arms_unify a_arms b_arms
         && leq_value state a_scrutinee b_scrutinee
         && Nonempty_list.for_all
              (Nonempty_list.zip_exn a_arms b_arms)
              ~f:(fun ((_, a_leaf), (_, b_leaf)) -> leq_value state a_leaf b_leaf) -> true
  | Apply { fn = a_fn; arg = a_arg }, Apply { fn = b_fn; arg = b_arg }
    when leq_value state a_fn b_fn && leq_value state a_arg b_arg -> true
  | Proj a, Proj b -> a.index = b.index && leq_value state a.tuple b.tuple
  | Payload a, Payload b -> Ident.Label.equal a.label b.label && leq_value state a.variant b.variant
  | Inject a, Inject b -> Ident.Label.equal a.label b.label && leq_value state a.ty b.ty
  | Constructor a, Constructor b ->
    Ident.Label.equal a.label b.label
    &&
      (match a.payload, b.payload with
      | None, None -> true
      | Some a, Some b -> leq_value state a b
      | None, Some _ | Some _, None -> false)
  | a, (Apply { fn = Binder _ | Apply _; _ } as b) | (Apply { fn = Binder _ | Apply _; _ } as a), b
    ->
    unfold_value state a b ~f:(fun a b -> Some (leq_value state a b))
    |> Option.value_or_thunk ~default:(fun () -> leq_stuck_match state a b)
  | Match _, _ | _, Match _ -> leq_stuck_match state a b
  | ( ( Unit
      | Bool _
      | Int _
      | Type _
      | Closure _
      | Binder _
      | Var _
      | Apply _
      | Proj _
      | Payload _
      | External _
      | Tuple _
      | Inject _
      | Constructor _
      | Prim _ )
    , _ ) -> false

and geq_value state (a : Value.t) (b : Value.t) = leq_value state b a

and unfold_spine : 'r. State.t -> Value.t -> f:(Value.t -> 'r) -> 'r option =
  fun state value ~f ->
  let rec peel (value : Value.t) args =
    match value with
    | Apply { fn; arg } -> peel fn (arg :: args)
    | head -> head, args
  in
  let rec apply (value : Value.t) args =
    match value, args with
    | value, [] -> Some (f value)
    | ( (Binder { arg; ty; env; body_dst; _ } | Closure { arg; ty; env; body_dst; _ })
      , arg_val :: args ) ->
      let loc = Dst.Expr.loc body_dst in
      State.recur state ~loc ~f:(fun () ->
        let env =
          Env.bind env arg { ty = Ty.arg ty; mode = Ty.arg_mode ty; static = Lazy.from_val arg_val }
        in
        apply (force_or_var state (reduce state env body_dst).static) args)
    | (Apply _ as value), args ->
      let head, args = peel value args in
      apply head args
    | _, _ :: _ -> None
  in
  match peel value [] with
  | _, [] -> None
  | head, args -> apply head args

and unfold_value
  : 'r. State.t -> Value.t -> Value.t -> f:(Value.t -> Value.t -> 'r option) -> 'r option
  =
  fun state a b ~f ->
  unfold_spine state b ~f:(fun b -> f a b)
  |> Option.value_or_thunk ~default:(fun () ->
    unfold_spine state a ~f:(fun a -> f a b) |> Option.value ~default:None)

and join_value state (a : Value.t) (b : Value.t) : Value.t Option.t =
  match a, b with
  | a, Bottom -> Some a
  | Bottom, b -> Some b
  | Unit, Unit -> Some Value.unit
  | Bool a, Bool b -> join_bool state a b
  | Int a, Int b -> join_int state a b
  | Type a, Type b -> Option.map (join_ty state a b) ~f:Value.type_
  | Closure a, Closure b -> Option.map (join_closure a b) ~f:Value.closure
  | Binder a, Binder b -> Option.map (join_binder a b) ~f:Value.binder
  | Var a, Var b when Ident.equal a b -> Some (Value.var a)
  | Prim a, Prim b when Builtin.Prim.equal a b -> Some (Value.prim a)
  | Tuple a_elts, Tuple b_elts ->
    (match Nonempty_list.map2 a_elts b_elts ~f:(join_value state) with
     | Ok elts ->
       Nonempty_list.to_list elts
       |> Option.all
       |> Option.map ~f:(fun elts -> Value.tuple (Nonempty_list.of_list_exn elts))
     | Unequal_lengths -> None)
  | External a, External b when String.equal a.symbol b.symbol ->
    Some (Value.external_ ~symbol:a.symbol ~ty:a.ty)
  | ( Match { scrutinee = a_scrutinee; arms = a_arms }
    , (Match { scrutinee = b_scrutinee; arms = b_arms } as b) ) ->
    (match
       let%bind scrutinee = join_value state a_scrutinee b_scrutinee in
       let%map arms = Value.merge_arms a_arms b_arms ~f:(join_value state) in
       Value.match_ ~scrutinee ~arms
     with
     | Some join -> Some join
     | None -> join_all_arms state a_arms ~f:(fun leaf -> join_value state leaf b))
  | (Apply { fn = a_fn; arg = a_arg } as a), (Apply { fn = b_fn; arg = b_arg } as b) ->
    (match
       let%bind fn = join_value state a_fn b_fn in
       let%map arg = join_value state a_arg b_arg in
       Value.apply ~fn ~arg
     with
     | Some join -> Some join
     | None -> unfold_value state a b ~f:(join_value state))
  | Proj a, Proj b when a.index = b.index ->
    let%map tuple = join_value state a.tuple b.tuple in
    Value.proj tuple a.index
  | Payload a, Payload b when Ident.Label.equal a.label b.label ->
    let%map variant = join_value state a.variant b.variant in
    Value.payload variant ~label:a.label
  | Inject a, Inject b when Ident.Label.equal a.label b.label ->
    let%map ty = join_value state a.ty b.ty in
    Value.inject ~ty ~label:a.label
  | Constructor a, Constructor b when Ident.Label.equal a.label b.label ->
    (match a.payload, b.payload with
     | None, None -> Some (Value.constructor ~label:a.label ~payload:None)
     | Some a_payload, Some b_payload ->
       let%map payload = join_value state a_payload b_payload in
       Value.constructor ~label:a.label ~payload:(Some payload)
     | None, Some _ | Some _, None -> None)
  | a, (Apply { fn = Binder _ | Apply _; _ } as b) | (Apply { fn = Binder _ | Apply _; _ } as a), b
    -> unfold_value state a b ~f:(join_value state)
  | Match { arms; _ }, b -> join_all_arms state arms ~f:(fun leaf -> join_value state leaf b)
  | a, Match { arms; _ } -> join_all_arms state arms ~f:(fun leaf -> join_value state a leaf)
  | ( ( Unit
      | Bool _
      | Int _
      | Type _
      | Closure _
      | Binder _
      | Var _
      | Apply _
      | Proj _
      | Payload _
      | External _
      | Tuple _
      | Inject _
      | Constructor _
      | Prim _ )
    , _ ) -> None

and meet_value state (a : Value.t) (b : Value.t) : Value.t Option.t =
  match a, b with
  | Bottom, _ | _, Bottom -> Some Value.bottom
  | Unit, Unit -> Some Value.unit
  | Bool a, Bool b -> meet_bool state a b
  | Int a, Int b -> meet_int state a b
  | Type a, Type b -> Option.map (meet_ty state a b) ~f:Value.type_
  | Closure a, Closure b -> Option.map (meet_closure a b) ~f:Value.closure
  | Binder a, Binder b -> Option.map (meet_binder a b) ~f:Value.binder
  | Var a, Var b when Ident.equal a b -> Some (Value.var a)
  | Prim a, Prim b when Builtin.Prim.equal a b -> Some (Value.prim a)
  | Tuple a_elts, Tuple b_elts ->
    (match Nonempty_list.map2 a_elts b_elts ~f:(meet_value state) with
     | Ok elts ->
       Nonempty_list.to_list elts
       |> Option.all
       |> Option.map ~f:(fun elts -> Value.tuple (Nonempty_list.of_list_exn elts))
     | Unequal_lengths -> None)
  | External a, External b when String.equal a.symbol b.symbol ->
    Some (Value.external_ ~symbol:a.symbol ~ty:a.ty)
  | ( Match { scrutinee = a_scrutinee; arms = a_arms }
    , (Match { scrutinee = b_scrutinee; arms = b_arms } as b) ) ->
    (match
       let%bind scrutinee = meet_value state a_scrutinee b_scrutinee in
       let%map arms = Value.merge_arms a_arms b_arms ~f:(meet_value state) in
       Value.match_ ~scrutinee ~arms
     with
     | Some meet -> Some meet
     | None -> meet_all_arms state a_arms ~f:(fun leaf -> meet_value state leaf b))
  | (Apply { fn = a_fn; arg = a_arg } as a), (Apply { fn = b_fn; arg = b_arg } as b) ->
    (match
       let%bind fn = meet_value state a_fn b_fn in
       let%map arg = meet_value state a_arg b_arg in
       Value.apply ~fn ~arg
     with
     | Some meet -> Some meet
     | None -> unfold_value state a b ~f:(meet_value state))
  | Proj a, Proj b when a.index = b.index ->
    let%map tuple = meet_value state a.tuple b.tuple in
    Value.proj tuple a.index
  | Payload a, Payload b when Ident.Label.equal a.label b.label ->
    let%map variant = meet_value state a.variant b.variant in
    Value.payload variant ~label:a.label
  | Inject a, Inject b when Ident.Label.equal a.label b.label ->
    let%map ty = meet_value state a.ty b.ty in
    Value.inject ~ty ~label:a.label
  | Constructor a, Constructor b when Ident.Label.equal a.label b.label ->
    (match a.payload, b.payload with
     | None, None -> Some (Value.constructor ~label:a.label ~payload:None)
     | Some a_payload, Some b_payload ->
       let%map payload = meet_value state a_payload b_payload in
       Value.constructor ~label:a.label ~payload:(Some payload)
     | None, Some _ | Some _, None -> None)
  | a, (Apply { fn = Binder _ | Apply _; _ } as b) | (Apply { fn = Binder _ | Apply _; _ } as a), b
    -> unfold_value state a b ~f:(meet_value state)
  | Match { arms; _ }, b -> meet_all_arms state arms ~f:(fun leaf -> meet_value state leaf b)
  | a, Match { arms; _ } -> meet_all_arms state arms ~f:(fun leaf -> meet_value state a leaf)
  | ( ( Unit
      | Bool _
      | Int _
      | Type _
      | Closure _
      | Binder _
      | Var _
      | Apply _
      | Proj _
      | Payload _
      | External _
      | Tuple _
      | Inject _
      | Constructor _
      | Prim _ )
    , _ ) -> None

and join_all_arms state arms ~f =
  let (first :: rest) = Nonempty_list.map arms ~f:(fun (_, leaf) -> f leaf) in
  List.fold rest ~init:first ~f:(fun acc leaf ->
    let%bind acc
    and leaf in
    join_value state acc leaf)

and meet_all_arms state arms ~f =
  let (first :: rest) = Nonempty_list.map arms ~f:(fun (_, leaf) -> f leaf) in
  List.fold rest ~init:first ~f:(fun acc leaf ->
    let%bind acc
    and leaf in
    meet_value state acc leaf)

and leq_closure (a : Closure.t) (b : Closure.t) = a.hash = b.hash
and join_closure (a : Closure.t) (b : Closure.t) = if a.hash = b.hash then Some a else None
and meet_closure (a : Closure.t) (b : Closure.t) = if a.hash = b.hash then Some a else None
and leq_binder (a : Binder.t) (b : Binder.t) = a.hash = b.hash
and join_binder (a : Binder.t) (b : Binder.t) = if a.hash = b.hash then Some a else None
and meet_binder (a : Binder.t) (b : Binder.t) = if a.hash = b.hash then Some a else None

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

and unify_bool ~f (a : Bool.t) (b : Bool.t) : Value.t option =
  match a, b with
  | T a, T b -> if Core.Bool.equal a b then Some (Bool.const a) else None
  | Not a, Not b -> Option.map (f a b) ~f:Bool.not_
  | And (a0, a1), And (b0, b1) -> Option.map2 (f a0 b0) (f a1 b1) ~f:Bool.and_
  | Or (a0, a1), Or (b0, b1) -> Option.map2 (f a0 b0) (f a1 b1) ~f:Bool.or_
  | Eq (a0, a1), Eq (b0, b1) -> Option.map2 (f a0 b0) (f a1 b1) ~f:Bool.eq
  | Neq (a0, a1), Neq (b0, b1) -> Option.map2 (f a0 b0) (f a1 b1) ~f:Bool.neq
  | Lt (a0, a1), Lt (b0, b1) -> Option.map2 (f a0 b0) (f a1 b1) ~f:Bool.lt
  | Lte (a0, a1), Lte (b0, b1) -> Option.map2 (f a0 b0) (f a1 b1) ~f:Bool.lte
  | Gt (a0, a1), Gt (b0, b1) -> Option.map2 (f a0 b0) (f a1 b1) ~f:Bool.gt
  | Gte (a0, a1), Gte (b0, b1) -> Option.map2 (f a0 b0) (f a1 b1) ~f:Bool.gte
  | _ -> None

and join_bool state a b = unify_bool ~f:(join_value state) a b
and meet_bool state a b = unify_bool ~f:(meet_value state) a b

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

and unify_int ~f (a : Int.t) (b : Int.t) : Value.t option =
  match a, b with
  | T a, T b -> if Int64.equal a b then Some (Int.const a) else None
  | Neg a, Neg b -> Option.map (f a b) ~f:Int.neg
  | Add (a0, a1), Add (b0, b1) -> Option.map2 (f a0 b0) (f a1 b1) ~f:Int.add
  | Sub (a0, a1), Sub (b0, b1) -> Option.map2 (f a0 b0) (f a1 b1) ~f:Int.sub
  | Mul (a0, a1), Mul (b0, b1) -> Option.map2 (f a0 b0) (f a1 b1) ~f:Int.mul
  | Div (a0, a1), Div (b0, b1) -> Option.map2 (f a0 b0) (f a1 b1) ~f:Int.div
  | Mod (a0, a1), Mod (b0, b1) -> Option.map2 (f a0 b0) (f a1 b1) ~f:Int.mod_
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
  | Variant a_ctors, Variant b_ctors ->
    Map.length a_ctors = Map.length b_ctors
    && Map.for_alli a_ctors ~f:(fun ~key ~data:a_payload ->
      match Map.find b_ctors key, a_payload with
      | Some None, None -> true
      | Some (Some b), Some a -> leq_value state a b
      | Some None, Some _ | Some (Some _), None | None, _ -> false)
  | (Unit | Bool | Int | Type | Arrow _ | Pi _ | Tuple _ | Variant _), _ -> false

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
    let%bind arg_ty = meet_value state a_arg_ty b_arg_ty in
    let%map ret_ty = join_value state a_ret_ty b_ret_ty in
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
  | Variant a_ctors, Variant b_ctors -> Ty.unify_constructors ~f:(join_value state) a_ctors b_ctors
  | (Unit | Bool | Int | Type | Arrow _ | Pi _ | Tuple _ | Variant _), _ -> None

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
    let%bind arg_ty = join_value state a_arg_ty b_arg_ty in
    let%map ret_ty = meet_value state a_ret_ty b_ret_ty in
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
  | Variant a_ctors, Variant b_ctors -> Ty.unify_constructors ~f:(meet_value state) a_ctors b_ctors
  | (Unit | Bool | Int | Type | Arrow _ | Pi _ | Tuple _ | Variant _), _ -> None

and reduce state env (expr : Dst.Expr.t) : Desc.t =
  let _expr, expr_desc = typecheck state env expr in
  expr_desc

and typecheck state env expr =
  State.recur state ~loc:(Dst.Expr.loc expr) ~f:(fun () -> typecheck' state env expr)

and typecheck' state env (expr : Dst.Expr.t) : Expr.t * Desc.t =
  match expr with
  | If { cond; then_; else_; erased; loc } ->
    typecheck_if state env ~cond ~then_ ~else_ ~erased ~loc
  | Match { cond; arms; eliminator; loc } ->
    typecheck_match state env ~scrutinee:cond ~arms ~eliminator ~loc
  | Lambda { arg; arg_mode; arg_ty; body = body_dst; loc } ->
    typecheck_lambda state env ~arg ~arg_mode ~arg_ty ~body_dst ~loc
  | Apply { fn; arg; loc } -> typecheck_apply state env ~fn ~arg ~loc
  | Literal { value; loc } ->
    let ty = Value.type_ (Ty.of_literal value) in
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
    let ty = Value.type_ Type in
    let mode = Modes.create ~staticity:Static ~erasure:Erased in
    let value =
      typecheck_arrow state ~loc env ~arg_id ~arg_ty:arg ~arg_mode ~ret_ty:ret ~ret_mode
    in
    Literal { value; ty; mode; loc }, { ty; mode; static = Lazy.from_val value }
  | Tuple { elts; loc } ->
    let ty = Value.type_ Type in
    let mode = Modes.create ~staticity:Static ~erasure:Erased in
    let elts =
      Nonempty_list.map elts ~f:(fun elt ->
        let elt_desc = reduce state env elt in
        require_static_type state ~loc elt_desc)
    in
    let value = Value.type_ (Tuple elts) in
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
      let ty = Value.bottom in
      let value = Value.bottom in
      Literal { value; ty; mode; loc }, { Desc.ty; mode; static = Lazy.from_val value })
  | Make_tuple { elts; loc } ->
    (* TODO could maybe be a primitive *)
    let elts = Nonempty_list.map elts ~f:(fun elt -> typecheck state env elt) in
    Expr.tuple ~loc elts
  | Constructor { loc; _ } ->
    raise_s [%message "Unimplemented: variant constructors" (loc : Lex.Location.t)]
  | Select { expr; label; loc } ->
    (* TODO record projection *)
    let _expr, expr_desc = typecheck state env expr in
    let from = require_static_type ~loc state expr_desc in
    let rec select (from : Value.t) : Expr.t * Desc.t =
      match from with
      | Type (Variant constructors) as variant_ty ->
        (match Map.find constructors label with
         | None -> Fail.unknown_label [%here] ~loc from label
         | Some None ->
           let value = Value.constructor ~label ~payload:None in
           let mode = Modes.create ~staticity:Static ~erasure:Unerased in
           ( Literal { value; ty = variant_ty; mode; loc }
           , { ty = variant_ty; mode; static = Lazy.from_val value } )
         | Some (Some payload_ty) ->
           (* An injection function whose result is as static as its argument. *)
           let ty =
             Value.type_
               (Arrow
                  { arg_ty = payload_ty
                  ; arg_mode = Modes.default ()
                  ; ret_ty = variant_ty
                  ; ret_mode = Modes.create ~staticity:Static ~erasure:Unerased
                  })
           in
           let value = Value.inject ~ty:variant_ty ~label in
           let mode = Modes.create ~staticity:Static ~erasure:Unerased in
           Literal { value; ty; mode; loc }, { ty; mode; static = Lazy.from_val value })
      | Apply _ as got ->
        unfold_spine state got ~f:select
        |> Option.value_or_thunk ~default:(fun () -> Fail.expected_variant [%here] ~loc got label)
      | got -> Fail.expected_variant [%here] ~loc got label
    in
    select from
  | Variant { constructors; loc } ->
    let ty = Value.type_ Type in
    let mode = Modes.create ~staticity:Static ~erasure:Erased in
    let constructors =
      Nonempty_list.map constructors ~f:(fun { Dst.Expr.label; payload } ->
        let payload =
          Option.map payload ~f:(fun payload ->
            let payload_desc = reduce state env payload in
            require_static_type state ~loc payload_desc)
        in
        label, payload)
    in
    let constructors =
      match Ident.Label.Map.of_alist (Nonempty_list.to_list constructors) with
      | `Ok constructors -> constructors
      | `Duplicate_key label -> Fail.duplicate_label [%here] ~loc label
    in
    let value = Value.type_ (Variant constructors) in
    Literal { value; ty; mode; loc }, { ty; mode; static = Lazy.from_val value }

and typecheck_arrow state ~loc env ~arg_id ~arg_ty ~arg_mode ~ret_ty ~ret_mode : Value.t =
  let arg_desc = reduce state env arg_ty in
  let arg_mode = require_mode_annotation (Modes.default ()) arg_mode in
  let ret_mode = require_mode_annotation (Modes.default ~staticity:Static ()) ret_mode in
  let arg_ty = require_static_type state ~loc arg_desc in
  match arg_mode.staticity with
  | Dynamic | Parametric ->
    let ret_desc = reduce state env ret_ty in
    let ret_ty = require_static_type state ~loc ret_desc in
    Value.type_ (Arrow { arg_ty; arg_mode; ret_ty; ret_mode })
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
    Value.type_ (Pi { arg_ty; arg_mode; ret_ty; ret_mode })

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
                Value.binder
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
                Value.closure
                  { arg; ty; body; body_dst; env = !env_rec; family; hash = State.fresh_id state })
            in
            let arg_mode = { arg_mode with staticity = Dynamic } in
            { Desc.ty = Value.type_ (Arrow { arg_ty; arg_mode; ret_ty; ret_mode })
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
      Value.type_ (Arrow { arg_ty; arg_mode; ret_ty = body_desc.ty; ret_mode = body_desc.mode })
    in
    let fn_mode = Modes.return fn_mode ~ret:body_desc.mode in
    let static =
      Lazy.from_val
        (Value.closure { arg; ty; body; body_dst; env; family; hash = State.fresh_id state })
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
      Value.type_ (Pi { arg_ty; arg_mode; ret_ty; ret_mode = body_desc.mode })
    in
    let fn_mode = Modes.return fn_mode ~ret:body_desc.mode in
    let static =
      Lazy.from_val (Value.binder { arg; ty; body_dst; env; family; hash = State.fresh_id state })
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
         then Lazy.from_val (Value.apply ~fn:(Value.binder binder) ~arg:arg_val)
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
         match prim with
         | Assert_erased ->
           (* Assert_erased forces the assert now *)
           ignore (Lazy.force static : Value.t);
           Erased { ty; mode; loc }, desc
         | Unerase _ ->
           (* Unerase reifies the value *)
           let mode = { mode with erasure = Unerased } in
           Literal { value = Lazy.force static; ty; mode; loc }, { desc with mode }
         | _ -> Erased { ty; mode; loc }, desc)
       else Specialize { fn; arg; ty; target = Prim prim; key = None; mode; loc }, desc
     | fn_val, _ ->
       let static =
         if Modes.is_static ret_mode
         then Lazy.from_val (Value.apply ~fn:fn_val ~arg:arg_val)
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
        | _ -> Lazy.map arg_desc.static ~f:(fun arg -> Value.apply ~fn ~arg))
      else Fail.unreachable [%here] ~loc
    in
    let mode = resolve_app_mode ~loc fn_desc ret_mode in
    let desc = { Desc.ty = ret_ty; mode; static } in
    if Modes.is_erased mode
    then Erased { ty = ret_ty; mode; loc }, desc
    else Apply { fn; arg; ty = ret_ty; mode; loc }, desc
  | _ -> Fail.expected_function [%here] ~loc fn_desc.ty arg_desc.ty

and typecheck_if state env ~cond ~then_ ~else_ ~erased ~loc =
  let open Lazy.Let_syntax in
  let cond, cond_desc = typecheck state env cond in
  require_leq state ~loc cond_desc.ty (Value.type_ Bool);
  match (erased : Modes.Erasure.t) with
  | Erased ->
    require_static ~loc cond_desc;
    (match force_or_cyclic ~loc cond_desc.static with
     | Bool (T true) when State.monomorphizing state -> typecheck state env then_
     | Bool (T false) when State.monomorphizing state -> typecheck state env else_
     | Bool (T true) ->
       let _else_desc =
         let env = rebind_condition env cond (Value.of_literal (Bool false)) in
         State.with_abs state ~f:(fun () -> reduce state env else_)
       in
       typecheck state env then_
     | Bool (T false) ->
       let _then_desc =
         let env = rebind_condition env cond (Value.of_literal (Bool true)) in
         State.with_abs state ~f:(fun () -> reduce state env then_)
       in
       typecheck state env else_
     | value ->
       let then_, then_desc =
         let env = rebind_condition env cond (Value.of_literal (Bool true)) in
         State.with_abs state ~f:(fun () -> typecheck state env then_)
       in
       let else_, else_desc =
         let env = rebind_condition env cond (Value.of_literal (Bool false)) in
         State.with_abs state ~f:(fun () -> typecheck state env else_)
       in
       let mode = Modes.cond ~cond:cond_desc.mode [ then_desc.mode; else_desc.mode ] in
       let static =
         if Modes.is_dynamic mode
         then Fail.unreachable [%here] ~loc
         else (
           let%map then_ = then_desc.static
           and else_ = else_desc.static in
           Value.if_ ~loc ~cond:value ~then_ ~else_)
       in
       (* Resolve the [if]-value here if possible. *)
       let ty =
         match join_value state then_desc.ty else_desc.ty with
         | Some ty -> ty
         | None -> Value.if_ ~loc ~cond:value ~then_:then_desc.ty ~else_:else_desc.ty
       in
       If { cond; then_; else_; ty; mode; loc }, { ty; mode; static })
  | Unerased ->
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
            Value.if_ ~loc ~cond ~then_ ~else_)
    in
    let ty = require_join state ~loc then_desc.ty else_desc.ty in
    If { cond; then_; else_; ty; mode; loc }, { ty; mode; static }

and pattern_bindings state ~desc (pattern : Dst.Expr.pattern) : Desc.t Ident.Map.t =
  match pattern with
  | Var { id; _ } -> if Ident.is_anon id then Ident.Map.empty else Ident.Map.singleton id desc
  | Constructor { label; payload; loc } ->
    (match desc.ty with
     | Type (Variant constructors) ->
       (match payload, Map.find constructors label with
        | _, None -> Fail.unknown_label [%here] ~loc desc.ty label
        | None, Some None -> Ident.Map.empty
        | Some payload, Some (Some payload_ty) ->
          let static = Lazy.map desc.static ~f:(payload_or_fresh state ~label) in
          pattern_bindings state ~desc:{ Desc.ty = payload_ty; mode = desc.mode; static } payload
        | None, Some (Some _) -> Fail.Match.payload_mismatch [%here] ~loc label ~required:true
        | Some _, Some None -> Fail.Match.payload_mismatch [%here] ~loc label ~required:false)
     | _ -> Fail.expected_variant [%here] ~loc desc.ty label)
  | Literal { value; loc } ->
    require_leq ~loc state desc.ty (Value.type_ (Ty.of_literal value));
    Ident.Map.empty
  | Tuple { elts; loc } ->
    (match desc.ty with
     | Type (Tuple elt_tys) when Nonempty_list.length elt_tys = Nonempty_list.length elts ->
       Nonempty_list.zip_exn elts elt_tys
       |> Nonempty_list.mapi ~f:(fun index (elt, ty) ->
         let static = Lazy.map desc.static ~f:(fun tuple -> Value.proj tuple index) in
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
    let match_ l r =
      Value.match_
        ~scrutinee:(force_or_var state desc.static)
        ~arms:(Nonempty_list.create (left, l) [ right, r ])
    in
    Map.merge_skewed lhs rhs ~combine:(fun ~key:_ (l : Desc.t) (r : Desc.t) ->
      let ty =
        match join_value state l.ty r.ty with
        | Some ty -> ty
        | None ->
          if Modes.is_static desc.mode
          then match_ l.ty r.ty
          else
            (* Attempted to bind incompatible types in a dynamic pattern *)
            Fail.cannot_unify [%here] ~loc l.ty r.ty
      in
      let mode = Modes.join l.mode r.mode in
      let static =
        Lazy.from_fun (fun () ->
          let lv = Lazy.force l.static in
          let rv = Lazy.force r.static in
          match join_value state lv rv with
          | Some value -> value
          | None -> match_ lv rv)
      in
      { Desc.ty; mode; static })

and build_condition ~loc ~scrutinee ~scrutinee_desc (literal : Dst.Literal.t) : Expr.t =
  match literal with
  | Unit -> Expr.literal ~loc (Bool true)
  | Bool true -> scrutinee
  | Bool false -> Builtin.apply ~loc (Bool Not) scrutinee scrutinee_desc
  | Int i ->
    let arg, arg_desc =
      let lit = Expr.literal ~loc (Int i) in
      let lit_desc = Expr.desc lit (Lazy.from_val (Value.of_literal (Int i))) in
      Expr.tuple ~loc (Nonempty_list.create (scrutinee, scrutinee_desc) [ lit, lit_desc ])
    in
    Builtin.apply ~loc (Int Eq) arg arg_desc

and typecheck_match state env ~scrutinee:scrutinee_dst ~arms ~eliminator ~loc =
  let scrutinee, scrutinee_desc = typecheck state env scrutinee_dst in
  let static_match (erasure : Modes.Erasure.t) =
    require_static ~loc scrutinee_desc;
    typecheck_match_static state env ~scrutinee ~scrutinee_desc ~scrutinee_dst ~arms ~erasure ~loc
  in
  match eliminator with
  | Dynamic -> typecheck_match_dynamic state env ~scrutinee ~scrutinee_desc ~arms ~loc
  | Static ->
    require_unerased ~loc scrutinee_desc;
    static_match Unerased
  | Erased -> static_match Erased

and typecheck_match_dynamic state env ~scrutinee ~scrutinee_desc ~arms ~loc =
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
  let compiled = compile_match ~loc ~ty:scrutinee_desc.ty patterns in
  let ty, mode =
    let ty, mode =
      let ({ ty; mode; _ } :: rest) = descs in
      List.fold rest ~init:(ty, mode) ~f:(fun (ty, mode) desc ->
        require_join state ~loc ty desc.ty, Modes.join mode desc.mode)
    in
    ty, Modes.cond ~cond:scrutinee_desc.mode [ mode ]
  in
  let static =
    if Modes.is_dynamic mode
    then Fail.unreachable [%here] ~loc
    else
      Lazy.from_fun (fun () ->
        let scrutinee = force_or_var state scrutinee_desc.static in
        let statics = Nonempty_list.map descs ~f:(fun (desc : Desc.t) -> desc.static) in
        match try_select_arm scrutinee patterns with
        | Some (index, _) -> Lazy.force (Nonempty_list.nth_exn statics index)
        | None ->
          Value.match_
            ~scrutinee
            ~arms:
              (Nonempty_list.zip_exn patterns statics
               |> Nonempty_list.map ~f:(fun (pattern, static) -> pattern, Lazy.force static)))
  in
  let expr = build_match state ~loc ~scrutinee ~scrutinee_desc ~cases ~compiled ~ty ~mode in
  expr, { Desc.ty; mode; static }

and typecheck_match_static state env ~scrutinee_dst ~scrutinee ~scrutinee_desc ~arms ~erasure ~loc =
  let project_value value path =
    List.fold path ~init:value ~f:(fun value (step : Step.t) ->
      match step with
      | Index index -> Value.proj value index
      | Payload label -> Value.payload value ~label)
  in
  let rec project_ty ~loc (ty : Value.t) (path : Step.t list) =
    match path with
    | [] -> ty
    | Index index :: path ->
      (match ty with
       | Value.Type (Tuple elt_tys) -> project_ty ~loc (Nonempty_list.nth_exn elt_tys index) path
       | ty -> raise_s [%message "Bug: expected tuple type" (ty : Value.t) (loc : Lex.Location.t)])
    | Payload label :: path ->
      (match ty with
       | Value.Type (Variant constructors) ->
         (match Map.find constructors label with
          | Some (Some payload_ty) -> project_ty ~loc payload_ty path
          | Some None | None ->
            raise_s
              [%message "Bug: expected payload" (label : Ident.Label.t) (loc : Lex.Location.t)])
       | ty -> raise_s [%message "Bug: expected variant type" (ty : Value.t) (loc : Lex.Location.t)])
  in
  let typecheck_arm ~desc ((pattern, body) : Dst.Expr.pattern * Dst.Expr.t) =
    let bindings =
      pattern_bindings state ~desc pattern
      |> Map.map ~f:(fun (desc : Desc.t) -> { desc with mode = { desc.mode with erasure } })
    in
    let env = Map.fold bindings ~init:env ~f:(fun ~key ~data env -> Env.bind env key data) in
    let env = rebind_scrutinee state env scrutinee_dst pattern (force_or_var state desc.static) in
    let body, body_desc = State.with_abs state ~f:(fun () -> typecheck state env body) in
    bindings, body, body_desc
  in
  let scrutinee_val = force_or_cyclic ~loc scrutinee_desc.static in
  (* TODO duplicated work *)
  Nonempty_list.iter arms ~f:(fun (pattern, _) ->
    ignore (pattern_bindings state ~desc:scrutinee_desc pattern));
  let compiled = compile_match ~loc ~ty:scrutinee_desc.ty (Nonempty_list.map arms ~f:fst) in
  match try_select_arm scrutinee_val (Nonempty_list.map arms ~f:fst) with
  | Some (index, bindings) ->
    let _, body_dst = Nonempty_list.nth_exn arms index in
    if not (State.monomorphizing state)
    then
      Nonempty_list.iteri arms ~f:(fun i arm ->
        if i <> index then ignore (typecheck_arm ~desc:scrutinee_desc arm));
    let bindings =
      List.map bindings ~f:(fun (id, path) ->
        let ty = project_ty ~loc scrutinee_desc.ty path in
        let mode = { scrutinee_desc.mode with erasure } in
        let static = Lazy.from_val (project_value scrutinee_val path) in
        id, path, { Desc.ty; mode; static })
    in
    let env = List.fold bindings ~init:env ~f:(fun env (id, _, desc) -> Env.bind env id desc) in
    let body, body_desc = typecheck state env body_dst in
    let expr =
      match (erasure : Modes.Erasure.t), bindings with
      | Erased, _ | Unerased, [] -> body
      | Unerased, _ :: _ ->
        Expr.rebind scrutinee ~stamp:(State.fresh_id state) ~f:(fun scrutinee ->
          List.fold_right bindings ~init:body ~f:(fun (id, path, _) rest ->
            let bind, _ = Expr.project ~loc scrutinee scrutinee_desc path in
            Expr.Let { var = id; bind; rest; ty = body_desc.ty; mode = body_desc.mode; loc }))
    in
    expr, body_desc
  | None ->
    let arms =
      Nonempty_list.map arms ~f:(fun ((pattern, _) as arm) ->
        let bindings, body, body_desc = typecheck_arm ~desc:scrutinee_desc arm in
        pattern, bindings, body, body_desc)
    in
    let mode =
      Modes.cond
        ~cond:scrutinee_desc.mode
        (Nonempty_list.map arms ~f:(fun (_, _, _, (desc : Desc.t)) -> desc.mode)
         |> Nonempty_list.to_list)
    in
    let case ~leaf =
      Value.match_
        ~scrutinee:scrutinee_val
        ~arms:(Nonempty_list.map arms ~f:(fun ((pattern, _, _, _) as arm) -> pattern, leaf arm))
    in
    let ty =
      match scrutinee_val with
      | Bottom ->
        (* The match is dead code, whatever the arms agree on. *)
        Value.bottom
      | _ ->
        let (ty :: tys) = Nonempty_list.map arms ~f:(fun (_, _, _, (desc : Desc.t)) -> desc.ty) in
        (match
           List.fold tys ~init:(Some ty) ~f:(fun acc ty ->
             Option.bind acc ~f:(fun acc -> join_value state acc ty))
         with
         | Some ty -> ty
         | None -> case ~leaf:(fun (_, _, _, (desc : Desc.t)) -> desc.ty))
    in
    let static =
      if Modes.is_dynamic mode
      then Fail.unreachable [%here] ~loc
      else
        Lazy.from_fun (fun () ->
          case ~leaf:(fun (_, _, _, (desc : Desc.t)) -> Lazy.force desc.static))
    in
    let cases =
      Nonempty_list.map arms ~f:(fun (_, bindings, body, _) ->
        { Expr.bindings = Map.map bindings ~f:(fun (desc : Desc.t) -> desc.ty); body })
    in
    let expr = build_match state ~loc ~scrutinee ~scrutinee_desc ~cases ~compiled ~ty ~mode in
    expr, { Desc.ty; mode; static }

and build_match state ~loc ~scrutinee ~scrutinee_desc ~cases ~(compiled : Match.Result.t) ~ty ~mode =
  Expr.rebind scrutinee ~stamp:(State.fresh_id state) ~f:(fun scrutinee ->
    let project path = Expr.project ~loc scrutinee scrutinee_desc (Vec.to_list path) in
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
        let build_case ((head, body) : Match.Tree.Head.t * _) acc =
          match head with
          | Literal value ->
            let cond = build_condition ~loc ~scrutinee ~scrutinee_desc value in
            let then_ = build_switch body in
            Expr.Split { cond; then_; else_ = acc }
          | Tuple _ -> build_switch body
          | Constructor { label; payload = _ } ->
            let cond =
              Expr.Tag_test
                { variant = scrutinee
                ; label
                ; ty = Value.type_ Bool
                ; mode = scrutinee_desc.mode
                ; loc
                }
            in
            let then_ = build_switch body in
            Expr.Split { cond; then_; else_ = acc }
        in
        Array.fold_right
          cases
          ~init:(Option.map default ~f:build_switch)
          ~f:(fun (head, tree) acc ->
            match acc with
            | None -> Some (build_switch tree)
            | Some acc -> Some (build_case (head, tree) acc))
        |> Option.value_exn
      | Fail -> raise_s [%message "Bug: nonexhaustive match" (loc : Lex.Location.t)]
    in
    Expr.Match { cases; tree = build_switch compiled.tree; ty; mode; loc })
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
    let static = Lazy.from_val (Value.external_ ~symbol ~ty) in
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
