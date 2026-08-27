open! Core
open Tst
open Option.Let_syntax

module Error = struct
  module Branch = struct
    type t =
      | Then
      | Else
      | Arm of Dst.Expr.pattern
    [@@deriving sexp]
  end

  module Misplaced = struct
    type t =
      | Not_under_static_branch
      | Not_in_tail_position
      | Not_in_head_position
      | All_paths_unreachable
    [@@deriving sexp]
  end

  module Match = struct
    type t =
      | Multiple_bindings of Ident.t
      | Expected_tuple of Value.t
      | Expected_ref of Value.t
      | Redundant of Dst.Expr.pattern Nonempty_list.t
      | Non_exhaustive of Match.Result.Missing.t Nonempty_list.t
      | Or_unbound of Ident.t Nonempty_list.t
      | Payload_mismatch of
          { label : Ident.Label.t
          ; required : bool
          }
    [@@deriving sexp_of]
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
    | Erased_external of Ident.t * string
    | Erased_dynamic_argument of Modes.t
    | Static_failure of Builtin.Error.t
    | Erased_application of
        { fn : Value.t
        ; result : Modes.t
        }
    | Recursion_limit of int
    | Dead_branch of
        { branch : Branch.t
        ; value : Value.t
        }
    | Unreachable_reached
    | Infinite_size of Value.t
    | Misplaced_unreachable of Misplaced.t
    | Gave_up of t
  [@@deriving sexp_of]
end

exception Error of Error.t Loc.t [@@deriving sexp_of]
exception Gave_up

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

    let expected_ref ~loc here got = raise (Error { loc; here; reason = Match (Expected_ref got) })

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

  let giveup_type_mismatch ~loc here got need =
    raise (Error { loc; here; reason = Gave_up (Type_mismatch { got; need }) })
  ;;

  let cannot_unify ~loc here lhs rhs =
    raise (Error { loc; here; reason = Cannot_unify { lhs; rhs } })
  ;;

  let giveup_cannot_unify ~loc here lhs rhs =
    raise (Error { loc; here; reason = Gave_up (Cannot_unify { lhs; rhs }) })
  ;;

  let unbound_ident ~loc here id = raise (Error { loc; here; reason = Unbound_ident id })
  let recursion_limit ~loc here limit = raise (Error { loc; here; reason = Recursion_limit limit })

  let static_external ~loc here id name =
    raise (Error { loc; here; reason = Static_external (id, name) })
  ;;

  let erased_external ~loc here id name =
    raise (Error { loc; here; reason = Erased_external (id, name) })
  ;;

  let erased_dynamic_argument ~loc here mode =
    raise (Error { loc; here; reason = Erased_dynamic_argument mode })
  ;;

  let unknown_builtin ~loc here id name =
    raise (Error { loc; here; reason = Unknown_builtin (id, name) })
  ;;

  let static_failure ~loc here err = raise (Error { loc; here; reason = Static_failure err })

  let dead_branch ~loc here branch value =
    raise (Error { loc; here; reason = Dead_branch { branch; value } })
  ;;

  let unreachable_reached ~loc here = raise (Error { loc; here; reason = Unreachable_reached })

  let misplaced_unreachable ~loc here why =
    raise (Error { loc; here; reason = Misplaced_unreachable why })
  ;;

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

  let infinite_size here ~loc ty = raise (Error { loc; here; reason = Infinite_size ty })

  let unreachable here ~loc =
    Lazy.from_fun (fun () ->
      raise_s
        [%message "Bug: forced dynamic" (loc : Lex.Location.t) (here : Source_code_position.t)])
  ;;
end

module State = struct
  module Pair = struct
    type t = Value.t * Value.t [@@deriving sexp_of, compare, hash]
  end

  (* One step of specialization context: [family] applied to [key] *)
  module Frame = struct
    type t =
      { family : Ids.Family.t
      ; key : Hashcons.Tag.t
      }
    [@@deriving sexp_of, compare, hash]
  end

  (* One monomorphization: the function [uid] applied to [key] *)
  module Instance = struct
    type t =
      { uid : Ids.Fn.t
      ; key : Value.t
      }
    [@@deriving sexp_of, compare, hash]
  end

  (* One binder: source location + specialization context *)
  module Family = struct
    type t =
      { path : Frame.t list
      ; loc : Lex.Location.t
      }
    [@@deriving sexp_of, compare, hash]
  end

  module Mono = struct
    type t =
      { family : Ids.Family.t
      ; body : Expr.t
      ; desc : Desc.t
      }
  end

  module Name = struct
    type t =
      { var : Ident.t option
      ; family : Ids.Family.t
      ; mutable value : Value.t option
      ; mutable waiters : (unit -> unit) list
      }
  end

  module Status = struct
    type t =
      | Computed of Instance.t
      | Deferred of Lex.Location.t * (unit -> Mono.t)
  end

  type t =
    { mutable depth : int
    ; mutable judgment_fuel : int
    ; mutable context : Frame.t list
    ; mutable unsettled : Ids.Fn.Set.t
    ; mutable unfolding : Ids.Fn.Set.t
    ; mutable demand_depth : int
    ; leq : (Pair.t, bool) Hashtbl.t
    ; bisim : (Pair.t, unit) Hashtbl.t
    ; whnf : (Value.t, Value.t option) Hashtbl.t
    ; families : (Family.t, Ids.Family.t) Hashtbl.t
    ; instances : (Instance.t, Mono.t) Hashtbl.t
    ; monos : (Frame.t, Status.t) Hashtbl.t
    ; names : (Ids.Fn.t, Name.t) Hashtbl.t
    }

  let recursion_limit = 1000
  let judgment_limit = 1000
  let bisim_limit = 16

  let create () =
    { depth = 0
    ; judgment_fuel = -1
    ; context = []
    ; unsettled = Ids.Fn.Set.empty
    ; unfolding = Ids.Fn.Set.empty
    ; demand_depth = 0
    ; leq = Hashtbl.create (module Pair)
    ; bisim = Hashtbl.create (module Pair)
    ; whnf = Hashtbl.create (module Value)
    ; families = Hashtbl.create (module Family)
    ; instances = Hashtbl.create (module Instance)
    ; monos = Hashtbl.create (module Frame)
    ; names = Hashtbl.create (module Ids.Fn)
    }
  ;;

  let recur ~loc t ~f =
    if t.depth > recursion_limit
    then Fail.recursion_limit [%here] ~loc recursion_limit
    else (
      t.depth <- t.depth + 1;
      Exn.protect ~f ~finally:(fun () -> t.depth <- t.depth - 1))
  ;;

  let with_judgement t ~f =
    if t.judgment_fuel >= 0
    then f ()
    else (
      t.judgment_fuel <- judgment_limit;
      Exn.protect ~f ~finally:(fun () -> t.judgment_fuel <- -1))
  ;;

  let judge t ~f =
    if t.judgment_fuel = 0
    then raise Gave_up
    else (
      t.judgment_fuel <- t.judgment_fuel - 1;
      f ())
  ;;

  let assuming t pair ~f =
    Hashtbl.mem t.bisim pair
    ||
    (if Hashtbl.length t.bisim >= bisim_limit then raise Gave_up;
     Hashtbl.set t.bisim ~key:pair ~data:();
     Exn.protect ~f ~finally:(fun () -> Hashtbl.remove t.bisim pair))
  ;;

  let with_frame t frame ~f =
    t.context <- frame :: t.context;
    Exn.protect ~f ~finally:(fun () -> t.context <- List.tl_exn t.context)
  ;;

  let settled_value t uid =
    let value = (Hashtbl.find_exn t.names uid).Name.value in
    if Option.is_none value then t.unsettled <- Set.add t.unsettled uid;
    value
  ;;

  let observing t ~f =
    let unsettled = t.unsettled in
    t.unsettled <- Ids.Fn.Set.empty;
    match f () with
    | result ->
      let missed = t.unsettled in
      t.unsettled <- Set.union unsettled missed;
      result, missed
    | exception exn ->
      t.unsettled <- Set.union unsettled t.unsettled;
      raise exn
  ;;

  let stable t ~f =
    let result, missed = observing t ~f in
    result, Set.is_empty missed
  ;;

  let with_unfolding t uid ~f =
    let unfolding = t.unfolding in
    t.unfolding <- Set.add unfolding uid;
    Exn.protect ~f ~finally:(fun () -> t.unfolding <- unfolding)
  ;;

  let unfolding t uid = Set.mem t.unfolding uid

  let wait t uid f =
    let name = Hashtbl.find_exn t.names uid in
    match name.Name.value with
    | Some _ -> f ()
    | None -> name.Name.waiters <- f :: name.Name.waiters
  ;;

  let settle_group t entries =
    let woken =
      List.concat_map entries ~f:(fun (uid, value) ->
        let name = Hashtbl.find_exn t.names uid in
        name.Name.value <- Some value;
        let waiters = List.rev name.Name.waiters in
        name.Name.waiters <- [];
        waiters)
    in
    List.iter woken ~f:(fun f -> f ())
  ;;

  let register_group t entries =
    List.iter entries ~f:(fun (var, uid, family, ({ mode; _ } : Desc.t)) ->
      let var = if Modes.is_static mode && Modes.is_unerased mode then Some var else None in
      Hashtbl.set t.names ~key:uid ~data:{ Name.var; family; value = None; waiters = [] })
  ;;

  let fn_family t uid = (Hashtbl.find_exn t.names uid).Name.family

  let family t ~loc =
    let key = { Family.loc; path = List.rev t.context } in
    Hashtbl.find_or_add t.families key ~default:(fun () -> Ids.Family.create ())
  ;;

  let specialize t instance ~loc ~f =
    match Hashtbl.find t.instances instance with
    | Some mono -> mono
    | None ->
      if t.demand_depth > recursion_limit then Fail.recursion_limit [%here] ~loc recursion_limit;
      let mono =
        let depth = t.depth in
        t.demand_depth <- t.demand_depth + 1;
        t.depth <- 0;
        Exn.protect ~f ~finally:(fun () ->
          t.depth <- depth;
          t.demand_depth <- t.demand_depth - 1)
      in
      Hashtbl.set t.instances ~key:instance ~data:mono;
      Hashtbl.update
        t.monos
        { Frame.family = mono.family; key = Hashcons.tag instance.key }
        ~f:(function
          | Some (Status.Computed _ as computed) -> computed
          | Some (Deferred _) | None -> Computed instance);
      mono
  ;;

  let defer t frame ~loc ~f =
    let context = t.context in
    let f () =
      let saved = t.context in
      t.context <- context;
      Exn.protect ~f ~finally:(fun () -> t.context <- saved)
    in
    ignore (Hashtbl.add t.monos ~key:frame ~data:(Status.Deferred (loc, f)))
  ;;

  let demand_mono t ~family ~key ~depth =
    match Hashtbl.find t.monos { Frame.family; key } with
    | None -> None
    | Some (Computed instance) -> Some (Hashtbl.find_exn t.instances instance).Mono.body
    | Some (Deferred (loc, compute)) ->
      if depth > recursion_limit then Fail.recursion_limit [%here] ~loc recursion_limit;
      Some (compute ()).Mono.body
  ;;
end

let fresh_var () = Value.var (Ident.fresh Ident.Raw.anon)

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

let require_reachable (expr : Dst.Expr.t) =
  if Dst.Expr.is_unreachable expr then Fail.unreachable_reached [%here] ~loc:(Dst.Expr.loc expr)
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

let require_not_dynamic_erased ~loc (mode : Modes.t) =
  match mode with
  | { staticity = Dynamic; erasure = Erased } -> Fail.erased_dynamic_argument [%here] ~loc mode
  | _ -> ()
;;

let require_static ~loc (desc : Desc.t) =
  require_mode ~loc desc.mode (Modes.top ~staticity:Static ())
;;

let require_unerased ~loc (desc : Desc.t) =
  require_mode ~loc desc.mode (Modes.top ~erasure:Unerased ())
;;

let require_dynamic_arrow ~loc var sym (ty : Value.t) =
  match ty.node with
  | Type (Arrow { arg_mode; ret_mode; _ } | Pi { arg_mode; ret_mode; _ }) ->
    if Modes.is_static arg_mode || Modes.is_static ret_mode
    then Fail.static_external [%here] ~loc var sym;
    if Modes.is_erased arg_mode || Modes.is_erased ret_mode
    then Fail.erased_external [%here] ~loc var sym
  | _ -> Fail.static_external [%here] ~loc var sym
;;

let require_var ~loc env id =
  match Env.find env id with
  | Some value -> value
  | None -> Fail.unbound_ident [%here] ~loc id
;;

let build_condition ~loc ~scrutinee ~scrutinee_desc (literal : Dst.Literal.t) : Expr.t =
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
;;

let rec require_leq state ~loc src dst =
  match leq_value state src dst with
  | true -> ()
  | false -> Fail.type_mismatch [%here] ~loc src dst
  | exception Gave_up -> Fail.giveup_type_mismatch [%here] ~loc src dst

and require_join state ~loc ty1 ty2 =
  match join_value state ty1 ty2 with
  | Some ty -> ty
  | None -> Fail.cannot_unify [%here] ~loc ty1 ty2
  | exception Gave_up -> Fail.giveup_cannot_unify [%here] ~loc ty1 ty2

and require_static_type ~loc state (desc : Desc.t) =
  require_static ~loc desc;
  require_leq state ~loc (unfold state desc.ty) (Value.type_ Type);
  Lazy.force desc.static

and eval state (dep : Dependent.t) (arg_val : Value.t) : Value.t =
  match dep with
  | T { ty; memo } ->
    Hashtbl.set memo ~key:arg_val ~data:ty;
    ty
  | Reduce { env; arg; arg_ty; arg_mode; memo; ret_ty; uid = _ } ->
    let reduce kind =
      let env = Env.demanded (Env.enter env kind) in
      let env = Env.bind env arg { ty = arg_ty; mode = arg_mode; static = Lazy.from_val arg_val } in
      let ret_ty_desc = reduce state env ret_ty in
      Lazy.force ret_ty_desc.static
    in
    Hashtbl.update_and_return memo arg_val ~f:(function
      | Some ty -> ty
      | None -> reduce (if Dependent.is_concrete arg_val then Instancing else Reducing))
  | Typecheck { env; arg; arg_ty; arg_mode; memo; body; uid = _ } ->
    let reduce kind =
      let env = Env.demanded (Env.enter env kind) in
      let env = Env.bind env arg { ty = arg_ty; mode = arg_mode; static = Lazy.from_val arg_val } in
      let body_desc = reduce state env body in
      body_desc.ty
    in
    Hashtbl.update_and_return memo arg_val ~f:(function
      | Some ty -> ty
      | None -> reduce (if Dependent.is_concrete arg_val then Instancing else Reducing))

and whnf (state : State.t) (value : Value.t) : Value.t option =
  match Hashtbl.find state.whnf value with
  | Some result -> result
  | None ->
    let rec go ~progress value frames =
      let head, frames = Value.Eliminator.peel value frames in
      match head.node, frames with
      | Match { scrutinee; arms }, frames ->
        (match whnf state scrutinee with
         | Some scrutinee -> go ~progress:true (Value.match_ ~scrutinee ~arms) frames
         | None ->
           (match frames with
            | [] -> if progress then Some head else None
            | _ :: _ ->
              let arms =
                Nonempty_list.map arms ~f:(fun (pattern, leaf) ->
                  pattern, Value.Eliminator.unpeel leaf frames)
              in
              Some (Value.match_ ~scrutinee ~arms)))
      | Rec uid, frames ->
        (match State.settled_value state uid with
         | Some value -> go ~progress:true value frames
         | None ->
           (match frames with
            | [] -> if progress then Some head else None
            | _ :: _ -> if progress then Some (Value.Eliminator.unpeel head frames) else None))
      | _, [] -> if progress then Some head else None
      | ( (Binder { arg; ty; env; body_dst; uid; _ } | Closure { arg; ty; env; body_dst; uid; _ })
        , Apply arg_val :: frames ) ->
        let loc = Dst.Expr.loc body_dst in
        let kind : Env.Kind.t = if Dependent.is_concrete arg_val then Instancing else Reducing in
        State.recur state ~loc ~f:(fun () ->
          State.with_unfolding state uid ~f:(fun () ->
            let env = Env.demanded (Env.enter env kind) in
            let mode = { (Ty.arg_mode ty) with staticity = Static } in
            let env = Env.bind env arg { ty = Ty.arg ty; mode; static = Lazy.from_val arg_val } in
            go ~progress:true (Lazy.force (reduce state env body_dst).static) frames))
      | Tuple elts, Proj index :: frames ->
        go ~progress:true (Nonempty_list.nth_exn elts index) frames
      | Box payload, Deref :: frames -> go ~progress:true payload frames
      | Constructor { label = got; payload = Some payload }, Payload label :: frames
        when Ident.Label.equal got label -> go ~progress:true payload frames
      | ( ( Bottom
          | Unit
          | Bool _
          | Int _
          | Type _
          | Closure _
          | Binder _
          | Var _
          | Tuple _
          | Inject _
          | Constructor _
          | Apply _
          | Proj _
          | Payload _
          | External _
          | Box _
          | Deref _
          | Prim _ )
        , _ :: _ ) ->
        (* Surface the partial reduction *)
        if progress then Some (Value.Eliminator.unpeel head frames) else None
    in
    let result, stable = State.stable state ~f:(fun () -> go ~progress:false value []) in
    if stable then Hashtbl.set state.whnf ~key:value ~data:result;
    result

and unfold state (v : Value.t) : Value.t =
  match v.node with
  | Apply _ | Proj _ | Payload _ | Match _ | Deref _ | Rec _ ->
    let next = Option.value (whnf state v) ~default:v in
    if phys_equal next v then v else unfold state next
  | _ -> v

and suspension_head (v : Value.t) =
  match v.node with
  | Apply { fn; _ } -> suspension_head fn
  | Binder { uid; _ } | Closure { uid; _ } | Rec uid -> Some uid
  | _ -> None

and is_suspension (v : Value.t) = Option.is_some (suspension_head v)

and unfolding_name state (v : Value.t) =
  match v.node with
  | Apply _ ->
    (match suspension_head v with
     | Some uid -> State.unfolding state uid
     | None -> false)
  | _ -> false

and demand state (lazy_v : Value.t Lazy.t) : Value.t =
  let value, missed = State.observing state ~f:(fun () -> unfold state (Lazy.force lazy_v)) in
  (match Set.min_elt missed with
   | Some uid ->
     State.wait state uid (fun () -> ignore (demand state (Lazy.from_val value) : Value.t))
   | None -> ());
  value

(* Primitives evaluate on values, so their arguments unfold through tuples. *)
and unfold_args state (v : Value.t) : Value.t =
  let v = unfold state v in
  match v.node with
  | Tuple elts -> Value.tuple (Nonempty_list.map elts ~f:(unfold_args state))
  | _ -> v

and resolve state ~resolved ~loc (v : Value.t) : Value.t =
  let visiting = Hashtbl.create (module Value) in
  let rec go (v : Value.t) : Value.t =
    match Hashtbl.find resolved v with
    | Some r -> r
    | None ->
      if Hashtbl.mem visiting v then Fail.infinite_size [%here] ~loc v;
      Hashtbl.set visiting ~key:v ~data:();
      let r = State.recur state ~loc ~f:(fun () -> descend (unfold state v)) in
      Hashtbl.set resolved ~key:v ~data:r;
      r
  and descend (v : Value.t) : Value.t =
    match v.node with
    | Type (Tuple elts) -> Value.type_ (Tuple (Nonempty_list.map elts ~f:go))
    | Type (Variant constructors) ->
      Value.type_ (Variant (Map.map constructors ~f:(Option.map ~f:go)))
    | Type (Arrow arrow) ->
      Value.type_ (Arrow { arrow with arg_ty = go arrow.arg_ty; ret_ty = go arrow.ret_ty })
    | Tuple elts -> Value.tuple (Nonempty_list.map elts ~f:go)
    | Constructor { label; payload = Some payload } ->
      Value.constructor ~label ~payload:(Some (go payload))
    | Type (Unit | Bool | Int | Type | Ref _) | _ -> v
  in
  go v

and leq_value state (a : Value.t) (b : Value.t) =
  State.with_judgement state ~f:(fun () ->
    match Hashtbl.find state.leq (a, b) with
    | Some verdict -> verdict
    | None ->
      let verdict, stable = State.stable state ~f:(fun () -> leq_value' state a b) in
      if stable && Hashtbl.length state.bisim = 0
      then Hashtbl.set state.leq ~key:(a, b) ~data:verdict;
      verdict)

and leq_value' state (a : Value.t) (b : Value.t) =
  let resolved (v : Value.t) =
    match v.node with
    | Rec uid -> Option.value (State.settled_value state uid) ~default:v
    | _ -> v
  in
  let a = resolved a
  and b = resolved b in
  match a.node, b.node with
  | Bottom, _ -> true
  | _, Bottom -> false
  | Unit, Unit -> true
  | Bool a, Bool b -> leq_bool state a b
  | Int a, Int b -> leq_int state a b
  | Type a, Type b -> leq_ty state a b
  | Closure p, Closure q -> Closure.equal p q || bisim_binder state a b
  | Binder p, Binder q -> Binder.equal p q || bisim_binder state a b
  | Var a, Var b -> Ident.equal a b
  | Prim a, Prim b -> Builtin.Prim.equal a b
  | Tuple a_elts, Tuple b_elts ->
    (match Nonempty_list.zip a_elts b_elts with
     | Ok zip -> Nonempty_list.for_all zip ~f:(fun (a, b) -> leq_value state a b)
     | Unequal_lengths -> false)
  | External a, External b -> String.equal a.symbol b.symbol
  | ( Match { scrutinee = a_scrutinee; arms = a_arms }
    , Match { scrutinee = b_scrutinee; arms = b_arms } )
    when Pattern.arms_agree a_arms b_arms
         && equal_value state a_scrutinee b_scrutinee
         && Nonempty_list.for_all
              (Nonempty_list.zip_exn a_arms b_arms)
              ~f:(fun ((_, a_leaf), (_, b_leaf)) -> leq_value state a_leaf b_leaf) -> true
  | Apply { fn = a_fn; arg = a_arg }, Apply { fn = b_fn; arg = b_arg }
    when equal_value state a_fn b_fn && equal_value state a_arg b_arg -> true
  | Proj a, Proj b when a.index = b.index && leq_value state a.tuple b.tuple -> true
  | Payload a, Payload b
    when Ident.Label.equal a.label b.label && leq_value state a.variant b.variant -> true
  | Box a, Box b -> leq_value state a b
  | Deref a, Deref b when leq_value state a b -> true
  | Rec a, Rec b when Ids.Fn.equal a b -> true
  | _, (Apply _ | Proj _ | Payload _ | Match _ | Deref _ | Rec _)
  | (Apply _ | Proj _ | Payload _ | Match _ | Deref _ | Rec _), _ ->
    (match State.judge state ~f:(fun () -> whnf state a) with
     | Some a -> leq_value state a b
     | None -> false)
    || (match State.judge state ~f:(fun () -> whnf state b) with
        | Some b -> leq_value state a b
        | None -> false)
    || leq_arms state a b
  | Inject a, Inject b -> Ident.Label.equal a.label b.label && leq_value state a.ty b.ty
  | Constructor a, Constructor b ->
    Ident.Label.equal a.label b.label
    &&
      (match a.payload, b.payload with
      | None, None -> true
      | Some a, Some b -> leq_value state a b
      | None, Some _ | Some _, None -> false)
  | ( ( Unit
      | Bool _
      | Int _
      | Type _
      | Closure _
      | Binder _
      | Var _
      | External _
      | Tuple _
      | Inject _
      | Constructor _
      | Box _
      | Prim _ )
    , _ ) -> false

and geq_value state (a : Value.t) (b : Value.t) = leq_value state b a
and equal_value state (a : Value.t) (b : Value.t) = leq_value state a b && leq_value state b a

and leq_arms state (a : Value.t) (b : Value.t) =
  let decompose scrutinee arms ~obligation =
    State.judge state ~f:(fun () ->
      Nonempty_list.for_all arms ~f:(fun (pattern, leaf) ->
        let fact value =
          Value.rewrite value ~target:scrutinee ~replacement:(Pattern.specialize pattern ~scrutinee)
        in
        obligation ~fact leaf))
  in
  (match a.node with
   | Match { scrutinee; arms } ->
     decompose scrutinee arms ~obligation:(fun ~fact leaf -> leq_value state (fact leaf) (fact b))
   | _ -> false)
  ||
  match b.node with
  | Match { scrutinee; arms } ->
    decompose scrutinee arms ~obligation:(fun ~fact leaf -> leq_value state (fact a) (fact leaf))
  | _ -> false

and join_value state (a : Value.t) (b : Value.t) : Value.t Option.t =
  State.with_judgement state ~f:(fun () -> join_value' state a b)

and join_value' state (a : Value.t) (b : Value.t) : Value.t Option.t =
  if leq_value state a b
  then Some b
  else if leq_value state b a
  then Some a
  else (
    let rep scrutinee arms ~join =
      Nonempty_list.map arms ~f:(fun (pattern, leaf) ->
        Option.map (join leaf) ~f:(fun joined -> pattern, joined))
      |> Nonempty_list.to_list
      |> Option.all
      |> Option.map ~f:(fun arms -> Value.match_ ~scrutinee ~arms:(Nonempty_list.of_list_exn arms))
    in
    match a.node, b.node with
    | Bool a, Bool b -> join_bool state a b
    | Int a, Int b -> join_int state a b
    | Type a, Type b -> Option.map (join_ty state a b) ~f:Value.type_
    | Tuple a_elts, Tuple b_elts ->
      (match Nonempty_list.map2 a_elts b_elts ~f:(join_value state) with
       | Ok elts ->
         Nonempty_list.to_list elts
         |> Option.all
         |> Option.map ~f:(fun elts -> Value.tuple (Nonempty_list.of_list_exn elts))
       | Unequal_lengths -> None)
    | ( Match { scrutinee = a_scrutinee; arms = a_arms }
      , Match { scrutinee = b_scrutinee; arms = b_arms } ) ->
      let aligned =
        if equal_value state a_scrutinee b_scrutinee
        then
          Option.map
            (Pattern.map2_arms a_arms b_arms ~f:(join_value state))
            ~f:(fun arms -> Value.match_ ~scrutinee:a_scrutinee ~arms)
        else None
      in
      (match aligned with
       | Some _ as joined -> joined
       | None ->
         (match rep a_scrutinee a_arms ~join:(fun leaf -> join_value state leaf b) with
          | Some _ as joined -> joined
          | None -> rep b_scrutinee b_arms ~join:(fun leaf -> join_value state a leaf)))
    | Match { scrutinee; arms }, _ -> rep scrutinee arms ~join:(fun leaf -> join_value state leaf b)
    | _, Match { scrutinee; arms } -> rep scrutinee arms ~join:(fun leaf -> join_value state a leaf)
    | Proj a_proj, Proj b_proj when a_proj.index = b_proj.index ->
      let%map tuple = join_value state a_proj.tuple b_proj.tuple in
      Value.proj tuple a_proj.index
    | Payload a_payload, Payload b_payload when Ident.Label.equal a_payload.label b_payload.label ->
      let%map variant = join_value state a_payload.variant b_payload.variant in
      Value.payload variant ~label:a_payload.label
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
    | Box a, Box b ->
      let%map payload = join_value state a b in
      Value.box payload
    | Deref a, Deref b ->
      let%map ref = join_value state a b in
      Value.deref ref
    | ( ( Bottom
        | Unit
        | Bool _
        | Int _
        | Type _
        | Closure _
        | Binder _
        | Var _
        | External _
        | Tuple _
        | Inject _
        | Constructor _
        | Apply _
        | Proj _
        | Payload _
        | Box _
        | Deref _
        | Prim _
        | Rec _ )
      , _ ) -> None)

and meet_value state (a : Value.t) (b : Value.t) : Value.t Option.t =
  State.with_judgement state ~f:(fun () -> meet_value' state a b)

and meet_value' state (a : Value.t) (b : Value.t) : Value.t Option.t =
  if leq_value state a b
  then Some a
  else if leq_value state b a
  then Some b
  else (
    let rep scrutinee arms ~meet =
      Nonempty_list.map arms ~f:(fun (pattern, leaf) ->
        Option.map (meet leaf) ~f:(fun met -> pattern, met))
      |> Nonempty_list.to_list
      |> Option.all
      |> Option.map ~f:(fun arms -> Value.match_ ~scrutinee ~arms:(Nonempty_list.of_list_exn arms))
    in
    match a.node, b.node with
    | Bool a, Bool b -> meet_bool state a b
    | Int a, Int b -> meet_int state a b
    | Type a, Type b -> Option.map (meet_ty state a b) ~f:Value.type_
    | Tuple a_elts, Tuple b_elts ->
      (match Nonempty_list.map2 a_elts b_elts ~f:(meet_value state) with
       | Ok elts ->
         Nonempty_list.to_list elts
         |> Option.all
         |> Option.map ~f:(fun elts -> Value.tuple (Nonempty_list.of_list_exn elts))
       | Unequal_lengths -> None)
    | ( Match { scrutinee = a_scrutinee; arms = a_arms }
      , Match { scrutinee = b_scrutinee; arms = b_arms } ) ->
      let aligned =
        if equal_value state a_scrutinee b_scrutinee
        then
          Option.map
            (Pattern.map2_arms a_arms b_arms ~f:(meet_value state))
            ~f:(fun arms -> Value.match_ ~scrutinee:a_scrutinee ~arms)
        else None
      in
      (match aligned with
       | Some _ as met -> met
       | None ->
         (match rep a_scrutinee a_arms ~meet:(fun leaf -> meet_value state leaf b) with
          | Some _ as met -> met
          | None -> rep b_scrutinee b_arms ~meet:(fun leaf -> meet_value state a leaf)))
    | Match { scrutinee; arms }, _ -> rep scrutinee arms ~meet:(fun leaf -> meet_value state leaf b)
    | _, Match { scrutinee; arms } -> rep scrutinee arms ~meet:(fun leaf -> meet_value state a leaf)
    | Proj a_proj, Proj b_proj when a_proj.index = b_proj.index ->
      let%map tuple = meet_value state a_proj.tuple b_proj.tuple in
      Value.proj tuple a_proj.index
    | Payload a_payload, Payload b_payload when Ident.Label.equal a_payload.label b_payload.label ->
      let%map variant = meet_value state a_payload.variant b_payload.variant in
      Value.payload variant ~label:a_payload.label
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
    | Box a, Box b ->
      let%map payload = meet_value state a b in
      Value.box payload
    | Deref a, Deref b ->
      let%map ref = meet_value state a b in
      Value.deref ref
    | ( ( Bottom
        | Unit
        | Bool _
        | Int _
        | Type _
        | Closure _
        | Binder _
        | Var _
        | External _
        | Tuple _
        | Inject _
        | Constructor _
        | Apply _
        | Proj _
        | Payload _
        | Box _
        | Deref _
        | Prim _
        | Rec _ )
      , _ ) -> None)

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

and ref_content state (payload : Value.t) : Value.t option =
  if is_suspension payload then whnf state payload else Some payload

and ref_content_exn state (payload : Value.t) : Value.t =
  match ref_content state payload with
  | Some content -> content
  | None -> raise_s [%message "Bug: ref name did not unfold" (payload : Value.t)]

(* Refs are invariant *)
and eq_ref state (a : Value.t) (b : Value.t) =
  let equiv a b = leq_value state a b && leq_value state b a in
  match is_suspension a, is_suspension b with
  | true, true ->
    (* Same-spine names short-circuit; distinct names may still be the same
       type, so fall back to bisimulation. *)
    let rec spine (a : Value.t) (b : Value.t) =
      match a.node, b.node with
      | Apply a, Apply b -> spine a.fn b.fn && equiv a.arg b.arg
      | _ -> equiv a b
    in
    spine a b || bisim_ref state a b
  | false, false -> equiv a b
  | true, false | false, true ->
    (match
       ( State.judge state ~f:(fun () -> ref_content state a)
       , State.judge state ~f:(fun () -> ref_content state b) )
     with
     | Some a, Some b -> equiv a b
     | None, _ | _, None -> false)

and bisim_ref state (a : Value.t) (b : Value.t) =
  State.assuming state (a, b) ~f:(fun () ->
    match
      ( State.judge state ~f:(fun () -> ref_content state a)
      , State.judge state ~f:(fun () -> ref_content state b) )
    with
    | Some a, Some b -> leq_value state a b && leq_value state b a
    | None, _ | _, None -> false)

and bisim_binder state (a : Value.t) (b : Value.t) =
  let candidate (v : Value.t) =
    match v.node with
    | (Binder { arg; ty; body_dst; env; _ } | Closure { arg; ty; body_dst; env; _ })
      when Modes.is_static (Ty.ret_mode ty) -> Some (arg, ty, body_dst, env)
    | _ -> None
  in
  match candidate a, candidate b with
  | Some ((_, p_ty, _, _) as p), Some ((_, q_ty, _, _) as q) ->
    Modes.equal (Ty.arg_mode p_ty) (Ty.arg_mode q_ty)
    && equal_value state (Ty.arg p_ty) (Ty.arg q_ty)
    && State.assuming state (a, b) ~f:(fun () ->
      State.judge state ~f:(fun () ->
        let arg_val = fresh_var () in
        let reduce_at (arg, ty, body_dst, env) =
          State.recur state ~loc:(Dst.Expr.loc body_dst) ~f:(fun () ->
            let env = Env.demanded (Env.enter env Reducing) in
            let mode = { (Ty.arg_mode ty) with staticity = Static } in
            let env = Env.bind env arg { ty = Ty.arg ty; mode; static = Lazy.from_val arg_val } in
            Lazy.force (reduce state env body_dst).static)
        in
        equal_value state (reduce_at p) (reduce_at q)))
  | _, _ -> false

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
    let var = fresh_var () in
    leq_value state (eval state a_ret_ty var) (eval state b_ret_ty var)
  | ( Arrow { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Pi { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } ) ->
    Modes.geq a_arg_mode b_arg_mode
    && Modes.leq a_ret_mode b_ret_mode
    && geq_value state a_arg_ty b_arg_ty
    && leq_value state a_ret_ty (eval state b_ret_ty (fresh_var ()))
  | ( Pi { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Arrow { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } )
    ->
    Modes.geq a_arg_mode b_arg_mode
    && Modes.leq a_ret_mode b_ret_mode
    && geq_value state a_arg_ty b_arg_ty
    && leq_value state (eval state a_ret_ty (fresh_var ())) b_ret_ty
  | Variant a_ctors, Variant b_ctors ->
    Map.length a_ctors = Map.length b_ctors
    && Map.for_alli a_ctors ~f:(fun ~key ~data:a_payload ->
      match Map.find b_ctors key, a_payload with
      | Some None, None -> true
      | Some (Some b), Some a -> leq_value state a b
      | Some None, Some _ | Some (Some _), None | None, _ -> false)
  | Ref a, Ref b -> eq_ref state a b
  | (Unit | Bool | Int | Type | Arrow _ | Pi _ | Tuple _ | Variant _ | Ref _), _ -> false

and join_dependent state (a : Dependent.t) (b : Dependent.t) : Dependent.t option =
  match a, b with
  | T { ty = a; _ }, T { ty = b; _ } -> Option.map (join_value state a b) ~f:Dependent.mono
  | _ -> None

and meet_dependent state (a : Dependent.t) (b : Dependent.t) : Dependent.t option =
  match a, b with
  | T { ty = a; _ }, T { ty = b; _ } -> Option.map (meet_value state a b) ~f:Dependent.mono
  | _ -> None

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
    let%map ret_ty = join_dependent state a_ret_ty b_ret_ty in
    Ty.Pi
      { arg_ty
      ; arg_mode = Modes.meet a_arg_mode b_arg_mode
      ; ret_ty
      ; ret_mode = Modes.join a_ret_mode b_ret_mode
      }
  | ( Arrow { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Pi { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } ) ->
    let%bind arg_ty = meet_value state a_arg_ty b_arg_ty in
    let%map ret_ty = join_dependent state (Dependent.mono a_ret_ty) b_ret_ty in
    Ty.Pi
      { arg_ty
      ; arg_mode = Modes.meet a_arg_mode b_arg_mode
      ; ret_ty
      ; ret_mode = Modes.join a_ret_mode b_ret_mode
      }
  | ( Pi { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Arrow { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } )
    ->
    let%bind arg_ty = meet_value state a_arg_ty b_arg_ty in
    let%map ret_ty = join_dependent state a_ret_ty (Dependent.mono b_ret_ty) in
    Ty.Pi
      { arg_ty
      ; arg_mode = Modes.meet a_arg_mode b_arg_mode
      ; ret_ty
      ; ret_mode = Modes.join a_ret_mode b_ret_mode
      }
  | Variant a_ctors, Variant b_ctors -> Ty.unify_constructors ~f:(join_value state) a_ctors b_ctors
  | Ref a, Ref b ->
    if eq_ref state a b then Some (Ty.Ref (if is_suspension a then a else b)) else None
  | (Unit | Bool | Int | Type | Arrow _ | Pi _ | Tuple _ | Variant _ | Ref _), _ -> None

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
    let%map ret_ty = meet_dependent state a_ret_ty b_ret_ty in
    Ty.Pi
      { arg_ty
      ; arg_mode = Modes.join a_arg_mode b_arg_mode
      ; ret_ty
      ; ret_mode = Modes.meet a_ret_mode b_ret_mode
      }
  | ( Arrow { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Pi { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } ) ->
    let%bind arg_ty = join_value state a_arg_ty b_arg_ty in
    let%map ret_ty = meet_value state a_ret_ty (eval state b_ret_ty (fresh_var ())) in
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
    let%map ret_ty = meet_value state (eval state a_ret_ty (fresh_var ())) b_ret_ty in
    Ty.Arrow
      { arg_ty
      ; arg_mode = Modes.join a_arg_mode b_arg_mode
      ; ret_ty
      ; ret_mode = Modes.meet a_ret_mode b_ret_mode
      }
  | Variant a_ctors, Variant b_ctors -> Ty.unify_constructors ~f:(meet_value state) a_ctors b_ctors
  | Ref a, Ref b ->
    if eq_ref state a b then Some (Ty.Ref (if is_suspension a then a else b)) else None
  | (Unit | Bool | Int | Type | Arrow _ | Pi _ | Tuple _ | Variant _ | Ref _), _ -> None

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
  | Select { expr; label; loc } -> typecheck_select state env ~expr ~label ~loc
  | Variant { constructors; loc } -> typecheck_variant state env ~constructors ~loc
  | Ref { arg; loc } -> typecheck_ref state env ~arg ~loc
  | Box { arg; loc } -> typecheck_box state env ~arg ~loc
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
    let mode = Modes.bottom () in
    let ty = Value.bottom in
    let value = Value.bottom in
    Literal { value; ty; mode; loc }, { Desc.ty; mode; static = Lazy.from_val value }
  | Make_tuple { elts; loc } ->
    (* TODO could maybe be a primitive *)
    let elts = Nonempty_list.map elts ~f:(fun elt -> typecheck state env elt) in
    Expr.tuple ~loc elts
  | Constructor { loc; _ } ->
    raise_s [%message "Unimplemented: variant constructors" (loc : Lex.Location.t)]

and typecheck_select state env ~expr ~label ~loc =
  (* TODO record projection *)
  let _expr, expr_desc = typecheck state env expr in
  let var_ty = unfold state (require_static_type ~loc state expr_desc) in
  match var_ty.node with
  | Type (Variant constructors) ->
    (match Map.find constructors label with
     | None -> Fail.unknown_label [%here] ~loc var_ty label
     | Some None ->
       let value = Value.constructor ~label ~payload:None in
       let mode = Modes.create ~staticity:Static ~erasure:Unerased in
       ( Literal { value; ty = var_ty; mode; loc }
       , { ty = var_ty; mode; static = Lazy.from_val value } )
     | Some (Some payload_ty) ->
       (* An injection function whose result is as static as its argument. *)
       let ty =
         Value.type_
           (Arrow
              { arg_ty = payload_ty
              ; arg_mode = Modes.default ()
              ; ret_ty = var_ty
              ; ret_mode = Modes.create ~staticity:Static ~erasure:Unerased
              })
       in
       let value = Value.inject ~ty:var_ty ~label in
       let mode = Modes.create ~staticity:Static ~erasure:Unerased in
       Literal { value; ty; mode; loc }, { ty; mode; static = Lazy.from_val value })
  | _ -> Fail.expected_variant [%here] ~loc var_ty label

and typecheck_variant state env ~constructors ~loc =
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

and typecheck_ref state env ~arg ~loc =
  let _arg_expr, arg_desc = typecheck state env arg in
  let payload = require_static_type ~loc state arg_desc in
  let ty = Value.type_ Type in
  let mode = Modes.create ~staticity:Static ~erasure:Erased in
  let value = Value.type_ (Ref payload) in
  Literal { value; ty; mode; loc }, { ty; mode; static = Lazy.from_val value }

and typecheck_box state env ~arg ~loc =
  let arg_expr, arg_desc = typecheck state env arg in
  let ty = Value.type_ (Ref arg_desc.ty) in
  let mode = arg_desc.mode in
  let static = Lazy.map arg_desc.static ~f:Value.box in
  if Modes.is_erased mode
  then Erased { ty; mode; loc }, { Desc.ty; mode; static }
  else Make_ref { payload = arg_expr; ty; mode; loc }, { Desc.ty; mode; static }

and typecheck_arrow state ~loc env ~arg_id ~arg_ty ~arg_mode ~ret_ty ~ret_mode =
  let arg_desc = reduce state env arg_ty in
  let arg_mode = require_mode_annotation (Modes.default ()) arg_mode in
  let ret_mode = require_mode_annotation (Modes.default ~staticity:Static ()) ret_mode in
  require_not_dynamic_erased ~loc arg_mode;
  let arg_ty = require_static_type state ~loc arg_desc in
  match arg_mode.staticity with
  | Dynamic | Parametric ->
    let ret_desc = reduce state env ret_ty in
    let ret_ty = require_static_type state ~loc ret_desc in
    Value.type_ (Arrow { arg_ty; arg_mode; ret_ty; ret_mode })
  | Static ->
    let env =
      Env.bind env arg_id { ty = arg_ty; mode = arg_mode; static = Lazy.from_val (fresh_var ()) }
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
  let members =
    Nonempty_list.map
      funs
      ~f:
        (fun
          { Dst.Expr.var; arg; erased; arg_ty; arg_mode; ret_mode; ret_ty; body = body_dst; loc } ->
        let uid = Ids.Fn.create () in
        let family = State.family state ~loc in
        let ty = typecheck_arrow state ~loc env ~arg_id:arg ~arg_ty ~arg_mode ~ret_ty ~ret_mode in
        let fun_mode = Modes.join fun_mode (Modes.bottom ~erasure:erased ()) in
        let desc : Desc.t =
          match ty.node with
          | Type (Pi { ret_mode; _ }) ->
            let fun_mode = Modes.return fun_mode ~ret:ret_mode in
            let static =
              Lazy.from_fun (fun () ->
                Value.binder (Binder.const ~uid ~arg ~ty ~body_dst ~env:!env_rec ~family ()))
            in
            { ty; mode = fun_mode; static }
          | Type (Arrow { arg_ty; ret_ty; ret_mode; _ }) ->
            let arg_mode =
              require_mode_annotation (Modes.default ~staticity:Parametric ()) arg_mode
            in
            let fun_mode = Modes.return fun_mode ~ret:ret_mode in
            let static =
              Lazy.from_fun (fun () ->
                let body =
                  Lazy.from_fun (fun () ->
                    let body, body_desc =
                      let env =
                        Env.bind
                          (Env.enter_body !env_rec)
                          arg
                          { ty = arg_ty; mode = arg_mode; static = Fail.unreachable [%here] ~loc }
                      in
                      typecheck state env body_dst
                    in
                    if Modes.is_erased ret_mode then require_static ~loc body_desc;
                    let body_desc = resolve_body_mode arg_mode body_desc in
                    require_mode ~loc body_desc.mode ret_mode;
                    require_leq state ~loc body_desc.ty ret_ty;
                    body)
                in
                Value.closure (Closure.const ~uid ~arg ~ty ~body ~body_dst ~env:!env_rec ~family ()))
            in
            let arg_mode = { arg_mode with staticity = Dynamic } in
            { Desc.ty = Value.type_ (Arrow { arg_ty; arg_mode; ret_ty; ret_mode })
            ; mode = fun_mode
            ; static
            }
          | _ -> raise_s [%message "Bug: expected function type"]
        in
        var, uid, family, desc)
  in
  let env =
    Nonempty_list.fold members ~init:env ~f:(fun acc (var, _, _, desc) -> Env.bind acc var desc)
  in
  env_rec
  := Nonempty_list.fold members ~init:env ~f:(fun acc (var, uid, _, desc) ->
       Env.bind acc var { desc with static = Lazy.from_val (Value.rec_ uid) });
  State.register_group state (Nonempty_list.to_list members);
  let funs =
    Nonempty_list.filter_map funs ~f:(fun { var; loc; _ } : Expr.fun_ option ->
      let { Desc.ty; mode; static } = require_var ~loc env var in
      let value = Lazy.force static in
      match ty.node, value.node with
      | Type (Arrow _), Closure { arg; ty; body; family; _ } ->
        let body = Lazy.force body in
        if Modes.is_unerased mode
        then Some (Lambda { var; arg; body; ty; mode; family; loc })
        else None
      | Type (Pi { arg_ty; arg_mode; ret_mode; ret_ty }), Binder { arg; ty; body_dst; family; _ } ->
        let arg_val = fresh_var () in
        let env =
          Env.bind
            (Env.enter_body !env_rec)
            arg
            { ty = arg_ty; mode = arg_mode; static = Lazy.from_val arg_val }
        in
        let body_desc = reduce state env body_dst in
        if Modes.is_erased ret_mode then require_static ~loc body_desc;
        let body_desc = resolve_body_mode arg_mode body_desc in
        require_mode ~loc body_desc.mode ret_mode;
        require_leq state ~loc body_desc.ty (eval state ret_ty arg_val);
        if Modes.is_unerased mode
        then Some (Binder { var; arg; body = Hashcons.Tag.Map.empty; ty; mode; family; loc })
        else None
      | _ -> raise_s [%message "Bug: expected function type"])
  in
  State.settle_group
    state
    (Nonempty_list.to_list members
     |> List.map ~f:(fun (_, uid, _, (desc : Desc.t)) -> uid, Lazy.force desc.static));
  funs, env

and typecheck_lambda state env ~arg ~arg_mode ~arg_ty ~body_dst ~loc =
  let family = State.family state ~loc in
  let arg_ty_desc = reduce state env arg_ty in
  let arg_ty = require_static_type state ~loc arg_ty_desc in
  let arg_mode = require_mode_annotation (Modes.default ~staticity:Parametric ()) arg_mode in
  require_not_dynamic_erased ~loc arg_mode;
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
        Env.bind
          (Env.enter_body env)
          arg
          { ty = arg_ty; mode = arg_mode; static = Fail.unreachable [%here] ~loc }
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
        (Value.closure
           (Closure.const ~arg ~ty ~body:(Lazy.from_val body) ~body_dst ~env ~family ()))
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
          (Env.enter_body env)
          arg
          { ty = arg_ty; mode = arg_mode; static = Lazy.from_val (fresh_var ()) }
      in
      reduce state env body_dst
    in
    let body_desc = resolve_body_mode arg_mode body_desc in
    let ty =
      let ret_ty = Dependent.typecheck body_desc.ty ~env ~arg ~arg_ty ~arg_mode ~body:body_dst in
      Value.type_ (Pi { arg_ty; arg_mode; ret_ty; ret_mode = body_desc.mode })
    in
    let fn_mode = Modes.return fn_mode ~ret:body_desc.mode in
    let static = Lazy.from_val (Value.binder (Binder.const ~arg ~ty ~body_dst ~env ~family ())) in
    let expr : Expr.t =
      if Modes.is_erased fn_mode
      then Erased { ty; mode = fn_mode; loc }
      else Binder { arg; ty; body = Hashcons.Tag.Map.empty; mode = fn_mode; family; loc }
    in
    expr, { ty; mode = fn_mode; static }

and specialize state ~loc (binder : Binder.t) ~arg_ty ~arg_mode ~ret_ty ~ret_mode ~arg_val =
  State.specialize state { uid = binder.uid; key = arg_val } ~loc ~f:(fun () ->
    let env =
      Env.bind
        binder.env
        binder.arg
        { ty = arg_ty; mode = arg_mode; static = Lazy.from_val arg_val }
    in
    let body, body_desc =
      State.with_frame
        state
        { family = binder.family; key = Hashcons.tag arg_val }
        ~f:(fun () -> typecheck state (Env.enter (Env.demanded env) Instancing) binder.body_dst)
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

and demand_name state ~loc (v : Value.t) : Value.t =
  match v.node with
  | Apply { fn; arg } when Dependent.is_concrete arg ->
    let resolved =
      let fn = demand_name state ~loc fn in
      match fn.node with
      | Rec uid when not (State.unfolding state uid) -> State.settled_value state uid
      | _ -> Some fn
    in
    (match resolved with
     | Some { node = Binder binder; _ } ->
       (match binder.ty.node with
        | Type (Pi { arg_ty; arg_mode; ret_ty; ret_mode }) ->
          let ty =
            State.with_frame
              state
              { family = binder.family; key = Hashcons.tag arg }
              ~f:(fun () -> eval state ret_ty arg)
          in
          let mono =
            specialize state ~loc binder ~arg_ty ~arg_mode ~ret_ty:ty ~ret_mode ~arg_val:arg
          in
          Lazy.force mono.desc.static
        | _ -> v)
     | Some _ | None -> v)
  | _ -> v

and typecheck_apply state env ~fn ~arg ~loc =
  let open Lazy.Let_syntax in
  let fn, fn_desc = typecheck state env fn in
  let arg, arg_desc = typecheck state env arg in
  let fn_desc = { fn_desc with ty = unfold state fn_desc.ty } in
  match fn_desc.ty.node with
  | Type (Pi { arg_ty; arg_mode; ret_ty; ret_mode }) ->
    require_static ~loc fn_desc;
    require_mode ~loc arg_desc.mode arg_mode;
    require_leq state ~loc arg_desc.ty arg_ty;
    let mode = resolve_app_mode ~loc fn_desc ret_mode in
    let arg_val =
      let v = Lazy.force arg_desc.static in
      match v.node with
      | Rec _ -> v
      | _ -> if unfolding_name state v then v else unfold state v
    in
    let keyable = Dependent.is_concrete arg_val in
    let fn_val =
      let v = Lazy.force fn_desc.static in
      if keyable && Modes.is_unerased mode then demand_name state ~loc v else v
    in
    let ty =
      match fn_val.node with
      | Binder { family; _ } when keyable ->
        State.with_frame
          state
          { family; key = Hashcons.tag arg_val }
          ~f:(fun () -> eval state ret_ty arg_val)
      | Rec uid when keyable ->
        State.with_frame
          state
          { family = State.fn_family state uid; key = Hashcons.tag arg_val }
          ~f:(fun () -> eval state ret_ty arg_val)
      | _ -> eval state ret_ty arg_val
    in
    let stuck () =
      if Modes.is_static ret_mode
      then Lazy.from_val (Value.apply ~fn:fn_val ~arg:arg_val)
      else Fail.unreachable [%here] ~loc
    in
    let arg =
      if Modes.is_erased arg_mode
      then Expr.Erased { ty = arg_desc.ty; mode = { arg_desc.mode with erasure = Erased }; loc }
      else arg
    in
    let emit static expr =
      let desc = { Desc.ty; mode; static } in
      if Modes.is_erased mode then Expr.Erased { ty; mode; loc }, desc else expr, desc
    in
    (match fn_val.node with
     | Binder binder ->
       if not keyable
       then emit (stuck ()) (Apply { fn; arg; ty; mode; loc })
       else if Env.live env
       then (
         let mono = specialize state ~loc binder ~arg_ty ~arg_mode ~ret_ty:ty ~ret_mode ~arg_val in
         emit
           mono.desc.static
           (Specialize { fn; arg; target = Family binder.family; key = Some arg_val; ty; mode; loc }))
       else (
         State.defer
           state
           { family = binder.family; key = Hashcons.tag arg_val }
           ~loc
           ~f:(fun () ->
             specialize state ~loc binder ~arg_ty ~arg_mode ~ret_ty:ty ~ret_mode ~arg_val);
         emit
           (stuck ())
           (Specialize { fn; arg; target = Family binder.family; key = Some arg_val; ty; mode; loc }))
     | Rec uid ->
       if Modes.is_erased mode || not keyable
       then emit (stuck ()) (Apply { fn; arg; ty; mode; loc })
       else (
         let family = State.fn_family state uid in
         let resolve_binder () =
           match State.settled_value state uid with
           | Some { node = Binder binder; _ } -> Some binder
           | Some _ | None -> None
         in
         match resolve_binder () with
         | Some binder when Env.live env ->
           let mono =
             specialize state ~loc binder ~arg_ty ~arg_mode ~ret_ty:ty ~ret_mode ~arg_val
           in
           emit
             mono.desc.static
             (Specialize { fn; arg; target = Family family; key = Some arg_val; ty; mode; loc })
         | Some _ | None ->
           State.defer
             state
             { family; key = Hashcons.tag arg_val }
             ~loc
             ~f:(fun () ->
               match resolve_binder () with
               | Some binder ->
                 specialize state ~loc binder ~arg_ty ~arg_mode ~ret_ty:ty ~ret_mode ~arg_val
               | None ->
                 raise_s [%message "Bug: demanded an unsettled recursive name" (uid : Ids.Fn.t)]);
           if
             Option.is_none (State.settled_value state uid)
             && (not (Value.is_fn_type ty))
             && Modes.is_static ret_mode
             && Modes.is_unerased mode
           then ignore (demand state (stuck ()) : Value.t);
           emit
             (stuck ())
             (Specialize { fn; arg; target = Family family; key = Some arg_val; ty; mode; loc }))
     | Closure closure ->
       let static =
         if Modes.is_static ret_mode
         then (
           let env =
             Env.bind
               (Env.demanded closure.env)
               closure.arg
               { ty = arg_ty; mode = arg_mode; static = Lazy.from_val arg_val }
           in
           (reduce state env closure.body_dst).static)
         else Fail.unreachable [%here] ~loc
       in
       emit
         static
         (Specialize { fn; arg; ty; target = Family closure.family; key = None; mode; loc })
     | Prim prim ->
       let static =
         if Modes.is_static ret_mode
         then
           Lazy.from_fun (fun () ->
             try Builtin.eval prim (unfold_args state arg_val) with
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
           let mode = { mode with erasure = Unerased } in
           Literal { value = Lazy.force static; ty; mode; loc }, { desc with mode }
         | _ -> Erased { ty; mode; loc }, desc)
       else Specialize { fn; arg; ty; target = Prim prim; key = None; mode; loc }, desc
     | _ -> emit (stuck ()) (Apply { fn; arg; ty; mode; loc }))
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
        match fn.node with
        | Closure closure ->
          let env = Env.bind (Env.demanded closure.env) closure.arg arg_desc in
          let desc = reduce state env closure.body_dst in
          desc.static
        | Prim prim ->
          Lazy.map arg_desc.static ~f:(fun arg ->
            try Builtin.eval prim (unfold_args state arg) with
            | Builtin.Error err -> Fail.static_failure [%here] ~loc err)
        | Rec uid ->
          Lazy.map arg_desc.static ~f:(fun arg ->
            let stuck = Value.apply ~fn ~arg in
            if Option.is_none (State.settled_value state uid)
            then ignore (demand state (Lazy.from_val stuck) : Value.t);
            stuck)
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
    let cond_val = demand state cond_desc.static in
    (match cond_val.node with
     | Bool (T true) when not (Env.abstract env) ->
       if Env.reducing env then require_reachable then_;
       typecheck state env then_
     | Bool (T false) when not (Env.abstract env) ->
       if Env.reducing env then require_reachable else_;
       typecheck state env else_
     | Bool (T true) -> Fail.dead_branch [%here] ~loc Else cond_val
     | Bool (T false) -> Fail.dead_branch [%here] ~loc Then cond_val
     | _ ->
       let learn b = Env.learn env ~target:cond_val ~replacement:(Value.of_literal (Bool b)) in
       let branch b body = typecheck state (Env.enter_body (learn b)) body in
       let then_, then_desc = branch true then_ in
       let else_, else_desc = branch false else_ in
       let mode = Modes.cond ~cond:cond_desc.mode [ then_desc.mode; else_desc.mode ] in
       let static =
         if Modes.is_dynamic mode
         then Fail.unreachable [%here] ~loc
         else (
           let%map then_ = then_desc.static
           and else_ = else_desc.static in
           Value.if_ ~cond:cond_val ~then_ ~else_)
       in
       let ty =
         let conditional () = Value.if_ ~cond:cond_val ~then_:then_desc.ty ~else_:else_desc.ty in
         match then_desc.ty.node, else_desc.ty.node with
         | Bottom, _ | _, Bottom -> conditional ()
         | _ ->
           (match join_value state then_desc.ty else_desc.ty with
            | Some ty -> ty
            | None | (exception Gave_up) -> conditional ())
       in
       If { cond; then_; else_; ty; mode; loc }, { ty; mode; static })
  | Unerased ->
    require_unerased ~loc cond_desc;
    if Env.instancing env
    then (
      require_reachable then_;
      require_reachable else_);
    let then_, then_desc = typecheck state env then_ in
    let else_, else_desc = typecheck state env else_ in
    let mode = Modes.cond ~cond:cond_desc.mode [ then_desc.mode; else_desc.mode ] in
    let static =
      if Modes.is_dynamic mode
      then Fail.unreachable [%here] ~loc
      else
        Lazy.bind cond_desc.static ~f:(fun cond ->
          match cond.node with
          | Bool (T true) -> then_desc.static
          | Bool (T false) -> else_desc.static
          | _ ->
            let%map then_ = then_desc.static
            and else_ = else_desc.static in
            Value.if_ ~cond ~then_ ~else_)
    in
    let ty = require_join state ~loc then_desc.ty else_desc.ty in
    If { cond; then_; else_; ty; mode; loc }, { ty; mode; static }

and typecheck_match state env ~scrutinee:scrutinee_dst ~arms ~eliminator ~loc =
  let scrutinee, scrutinee_desc = typecheck state env scrutinee_dst in
  let static_match (erasure : Modes.Erasure.t) =
    require_static ~loc scrutinee_desc;
    typecheck_match_static state env ~scrutinee ~scrutinee_desc ~arms ~erasure ~loc
  in
  match eliminator with
  | Dynamic -> typecheck_match_dynamic state env ~scrutinee ~scrutinee_desc ~arms ~loc
  | Static ->
    require_unerased ~loc scrutinee_desc;
    static_match Unerased
  | Erased -> static_match Erased

and typecheck_match_dynamic state env ~scrutinee ~scrutinee_desc ~arms ~loc =
  require_unerased ~loc scrutinee_desc;
  if Env.instancing env then Nonempty_list.iter arms ~f:(fun (_, body) -> require_reachable body);
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
  let compiled = compile_match state ~loc ~ty:scrutinee_desc.ty patterns in
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
        let scrutinee = demand state scrutinee_desc.static in
        let statics = Nonempty_list.map descs ~f:(fun (desc : Desc.t) -> desc.static) in
        match Pattern.selects scrutinee patterns with
        | Known (index, _) -> Lazy.force (Nonempty_list.nth_exn statics index)
        | Unknown ->
          Value.match_
            ~scrutinee
            ~arms:
              (Nonempty_list.zip_exn patterns statics
               |> Nonempty_list.map ~f:(fun (pattern, static) ->
                 Pattern.Canon.of_pattern pattern, Lazy.force static)))
  in
  let expr =
    build_match state ~loc ~static:false ~scrutinee ~scrutinee_desc ~cases ~compiled ~ty ~mode
  in
  expr, { Desc.ty; mode; static }

and typecheck_match_static state env ~scrutinee ~scrutinee_desc ~arms ~erasure ~loc =
  let scrutinee_val = demand state scrutinee_desc.static in
  let typecheck_arm ~(desc : Desc.t) ({ pattern; positive; speculative; body } : _ Pattern.World.t) =
    let bindings =
      pattern_bindings state ~desc pattern
      |> Map.map ~f:(fun (desc : Desc.t) -> { desc with mode = { desc.mode with erasure } })
    in
    let env = if speculative then Env.enter_body (Env.enter env Speculative) else env in
    let env =
      Env.learn
        env
        ~target:scrutinee_val
        ~replacement:
          (Pattern.specialize
             (Pattern.Canon.of_pattern positive)
             ~scrutinee:(Lazy.force desc.static))
    in
    let env = Map.fold bindings ~init:env ~f:(fun ~key ~data env -> Env.bind env key data) in
    let body, body_desc = typecheck state env body in
    bindings, body, body_desc
  in
  if not (Env.reducing env)
  then (* Validate patterns *)
    Nonempty_list.iter arms ~f:(fun (pattern, _) ->
      ignore (pattern_bindings state ~desc:scrutinee_desc pattern));
  let compiled =
    compile_match
      state
      ~loc
      ~ty:scrutinee_desc.ty
      ~scrutinee:scrutinee_val
      (Nonempty_list.map arms ~f:fst)
  in
  match Pattern.selects scrutinee_val (Nonempty_list.map arms ~f:fst) with
  | Known (index, bindings) ->
    let _, body_dst = Nonempty_list.nth_exn arms index in
    if Env.abstract env
    then
      Nonempty_list.iteri arms ~f:(fun i (pattern, _) ->
        if i <> index
        then
          Fail.dead_branch [%here] ~loc:(Dst.Expr.pattern_loc pattern) (Arm pattern) scrutinee_val);
    let bindings =
      let root = { scrutinee_desc with mode = { scrutinee_desc.mode with erasure } } in
      List.map bindings ~f:(fun (id, path) ->
        id, path, List.fold path ~init:root ~f:(project_desc state ~loc))
    in
    let env = List.fold bindings ~init:env ~f:(fun env (id, _, desc) -> Env.bind env id desc) in
    if Env.reducing env then require_reachable body_dst;
    let body, body_desc = typecheck state env body_dst in
    let expr =
      match (erasure : Modes.Erasure.t), bindings with
      | Erased, _ | Unerased, [] -> body
      | Unerased, _ :: _ ->
        Expr.rebind scrutinee ~id:(Ident.fresh Ident.Raw.anon) ~f:(fun scrutinee ->
          List.fold_right bindings ~init:body ~f:(fun (id, path, _) rest ->
            let bind, _ = project_scrut ~loc state scrutinee scrutinee_desc path in
            Expr.Let { var = id; bind; rest; ty = body_desc.ty; mode = body_desc.mode; loc }))
    in
    expr, body_desc
  | Unknown ->
    let live =
      Nonempty_list.to_list arms
      |> List.filter_map ~f:(fun ((pattern, _) as arm) ->
        let witness =
          match scrutinee_val.node with
          | Bottom -> None
          | _ ->
            (match Pattern.matches scrutinee_val (Pattern.Canon.of_pattern pattern) with
             | No_match -> Some scrutinee_val
             | Match | Unknown -> None)
        in
        (match witness with
         | Some witness when Env.abstract env ->
           Fail.dead_branch [%here] ~loc:(Dst.Expr.pattern_loc pattern) (Arm pattern) witness
         | _ -> ());
        Option.some_if (Option.is_none witness) arm)
      |> Nonempty_list.of_list_exn
    in
    let split =
      Pattern.worlds ~unfold:(unfold state) ~ty:scrutinee_desc.ty ~scrutinee:scrutinee_val live
    in
    let compiled =
      let patterns = Nonempty_list.map split ~f:(fun arm -> arm.positive) in
      if Nonempty_list.equal phys_equal patterns (Nonempty_list.map arms ~f:fst)
      then compiled
      else compile_match state ~loc ~ty:scrutinee_desc.ty ~scrutinee:scrutinee_val patterns
    in
    let arms =
      Nonempty_list.map split ~f:(fun arm ->
        let bindings, body, body_desc = typecheck_arm ~desc:scrutinee_desc arm in
        arm.positive, bindings, body, body_desc)
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
        ~arms:
          (Nonempty_list.map arms ~f:(fun ((pattern, _, _, _) as arm) ->
             Pattern.Canon.of_pattern pattern, leaf arm))
    in
    let ty =
      match scrutinee_val.node with
      | Bottom ->
        (* The match is dead code, whatever the arms agree on. *)
        Value.bottom
      | _ ->
        let (ty :: tys) = Nonempty_list.map arms ~f:(fun (_, _, _, (desc : Desc.t)) -> desc.ty) in
        let dead_arm =
          List.exists (ty :: tys) ~f:(fun (ty : Value.t) ->
            match ty.node with
            | Bottom -> true
            | _ -> false)
        in
        (match
           if dead_arm
           then None
           else
             List.fold tys ~init:(Some ty) ~f:(fun acc ty ->
               Option.bind acc ~f:(fun acc -> join_value state acc ty))
         with
         | Some ty -> ty
         | None | (exception Gave_up) -> case ~leaf:(fun (_, _, _, (desc : Desc.t)) -> desc.ty))
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
    let expr =
      build_match state ~loc ~static:true ~scrutinee ~scrutinee_desc ~cases ~compiled ~ty ~mode
    in
    expr, { Desc.ty; mode; static }

and pattern_bindings state ~(desc : Desc.t) (pattern : Dst.Expr.pattern) : Desc.t Ident.Map.t =
  match pattern with
  | Var { id; _ } -> if Ident.is_anon id then Ident.Map.empty else Ident.Map.singleton id desc
  | Constructor { label; payload; loc } ->
    let desc = { desc with ty = unfold state desc.ty } in
    (match desc.ty.node with
     | Type (Variant constructors) ->
       (match payload, Map.find constructors label with
        | _, None -> Fail.unknown_label [%here] ~loc desc.ty label
        | None, Some None -> Ident.Map.empty
        | Some payload, Some (Some payload_ty) ->
          let static = Lazy.map desc.static ~f:(Value.payload ~label) in
          pattern_bindings state ~desc:{ Desc.ty = payload_ty; mode = desc.mode; static } payload
        | None, Some (Some _) -> Fail.Match.payload_mismatch [%here] ~loc label ~required:true
        | Some _, Some None -> Fail.Match.payload_mismatch [%here] ~loc label ~required:false)
     | _ -> Fail.expected_variant [%here] ~loc desc.ty label)
  | Literal { value; loc } ->
    require_leq ~loc state (unfold state desc.ty) (Value.type_ (Ty.of_literal value));
    Ident.Map.empty
  | Tuple { elts; loc } ->
    let desc = { desc with ty = unfold state desc.ty } in
    (match desc.ty.node with
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
  | Ref { payload; loc } ->
    let desc = { desc with ty = unfold state desc.ty } in
    (match desc.ty.node with
     | Type (Ref p) ->
       let content = ref_content_exn state p in
       pattern_bindings
         state
         ~desc:{ Desc.ty = content; mode = desc.mode; static = Lazy.map desc.static ~f:Value.deref }
         payload
     | _ -> Fail.Match.expected_ref [%here] ~loc desc.ty)
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
      (* Note [Value.match_] treats [right] as a catch-all, which is true if the
         or pattern matched and [left] did not. *)
      Value.match_
        ~scrutinee:(Lazy.force desc.static)
        ~arms:
          (Nonempty_list.create
             (Pattern.Canon.of_pattern left, l)
             [ Pattern.Canon.of_pattern right, r ])
    in
    Map.merge_skewed lhs rhs ~combine:(fun ~key:_ (l : Desc.t) (r : Desc.t) ->
      let ty =
        let unjoined fail =
          if Modes.is_static desc.mode then match_ l.ty r.ty else fail l.ty r.ty
        in
        match join_value state l.ty r.ty with
        | Some ty -> ty
        | None -> unjoined (Fail.cannot_unify [%here] ~loc)
        | exception Gave_up -> unjoined (Fail.giveup_cannot_unify [%here] ~loc)
      in
      let mode = Modes.join l.mode r.mode in
      let static =
        Lazy.from_fun (fun () ->
          let lv = Lazy.force l.static in
          let rv = Lazy.force r.static in
          match join_value state lv rv with
          | Some value -> value
          | None | (exception Gave_up) -> match_ lv rv)
      in
      { Desc.ty; mode; static })

and project_desc state ~loc (desc : Desc.t) (step : Pattern.Step.t) : Desc.t =
  let ty = unfold state desc.ty in
  match step, ty.node with
  | Index index, Type (Tuple elt_tys) ->
    { Desc.ty = Nonempty_list.nth_exn elt_tys index
    ; mode = desc.mode
    ; static = Lazy.map desc.static ~f:(fun tuple -> Value.proj tuple index)
    }
  | Payload label, Type (Variant constructors) ->
    (match Map.find constructors label with
     | Some (Some payload_ty) ->
       { Desc.ty = payload_ty
       ; mode = desc.mode
       ; static = Lazy.map desc.static ~f:(Value.payload ~label)
       }
     | Some None | None ->
       raise_s [%message "Bug: expected payload" (label : Ident.Label.t) (loc : Lex.Location.t)])
  | Deref, Type (Ref p) ->
    { Desc.ty = ref_content_exn state p
    ; mode = desc.mode
    ; static = Lazy.map desc.static ~f:Value.deref
    }
  | Index _, _ ->
    raise_s [%message "Bug: expected tuple type" (ty : Value.t) (loc : Lex.Location.t)]
  | Payload _, _ ->
    raise_s [%message "Bug: expected variant type" (ty : Value.t) (loc : Lex.Location.t)]
  | Deref, _ -> raise_s [%message "Bug: expected ref type" (ty : Value.t) (loc : Lex.Location.t)]

and project_scrut ~loc (state : State.t) (scrutinee : Expr.t) (scrutinee_desc : Desc.t) path =
  List.fold path ~init:(scrutinee, scrutinee_desc) ~f:(fun (expr, desc) (step : Pattern.Step.t) ->
    let projected = project_desc state ~loc desc step in
    let expr =
      match step with
      | Index index ->
        Expr.Tuple_get { tuple = expr; index; ty = projected.ty; mode = desc.mode; loc }
      | Payload label ->
        Expr.Payload_get { variant = expr; label; ty = projected.ty; mode = desc.mode; loc }
      | Deref -> Expr.Ref_get { ref = expr; ty = projected.ty; mode = desc.mode; loc }
    in
    expr, projected)

and compile_match state ~loc ~ty ?scrutinee patterns =
  let compiled = Match.compile ~ty ~unfold:(unfold state) patterns in
  (match compiled.redundant with
   | [] -> ()
   | first :: rest -> Fail.Match.redundant [%here] ~loc (Nonempty_list.create first rest));
  let missing =
    match scrutinee with
    | None -> compiled.missing
    | Some scrutinee ->
      List.filter compiled.missing ~f:(fun missing ->
        not (Match.Result.Missing.refuted_by missing ~scrutinee ~unfold:(unfold state)))
  in
  (match missing with
   | [] -> ()
   | first :: rest -> Fail.Match.non_exhaustive [%here] ~loc (Nonempty_list.create first rest));
  { compiled with missing }

and build_match state ~loc ~static ~scrutinee ~scrutinee_desc ~cases ~compiled ~ty ~mode =
  Expr.rebind scrutinee ~id:(Ident.fresh Ident.Raw.anon) ~f:(fun scrutinee ->
    let project path = project_scrut ~loc state scrutinee scrutinee_desc (Vec.to_list path) in
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
        let default =
          Option.bind default ~f:(function
            | Fail when static -> None (* Missing cases were proven unreachable. *)
            | Fail ->
              raise_s [%message "Bug: dynamic match is non-exhaustive" (loc : Lex.Location.t)]
            | tree -> Some tree)
        in
        let scrutinee, scrutinee_desc = project occurrence.path in
        let build_case ((head, body) : Match.Tree.Head.t * _) acc =
          match head with
          | Literal value ->
            let cond = build_condition ~loc ~scrutinee ~scrutinee_desc value in
            let then_ = build_switch body in
            Expr.Split { cond; then_; else_ = acc }
          | Tuple _ -> build_switch body
          | Ref -> build_switch body
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
      | Fail -> raise_s [%message "Bug: dynamic match is non-exhaustive" (loc : Lex.Location.t)]
    in
    Expr.Match { cases; tree = build_switch compiled.tree; ty; mode; loc })
;;

(* [unreachable] may only appear as the entire body of a static branch, since any other code in the
   branch is necessarily dead. At least one branch of a conditional must not be unreachable. *)
let rec check_unreachable ~position ~in_branch (expr : Dst.Expr.t) =
  let check_consumed expr = check_unreachable ~position:`Consumed ~in_branch expr in
  if Dst.Expr.is_unreachable expr
  then (
    match position with
    | `Tail -> ()
    | `Continuation ->
      Fail.misplaced_unreachable [%here] ~loc:(Dst.Expr.loc expr) Not_in_head_position
    | `Consumed -> Fail.misplaced_unreachable [%here] ~loc:(Dst.Expr.loc expr) Not_in_tail_position);
  match expr with
  | Unreachable { loc } ->
    if not in_branch then Fail.misplaced_unreachable [%here] ~loc Not_under_static_branch
  | Type_annotation { expr; ty; loc = _ } ->
    check_unreachable ~position ~in_branch expr;
    check_consumed ty
  | Mode_annotation { expr; mode = _; loc = _ } -> check_unreachable ~position ~in_branch expr
  | If { cond; then_; else_; erased; loc } ->
    if Dst.Expr.is_unreachable then_ && Dst.Expr.is_unreachable else_
    then Fail.misplaced_unreachable [%here] ~loc All_paths_unreachable;
    check_consumed cond;
    let in_branch =
      match (erased : Modes.Erasure.t) with
      | Erased -> true
      | Unerased -> in_branch
    in
    check_unreachable ~position:`Tail ~in_branch then_;
    check_unreachable ~position:`Tail ~in_branch else_
  | Match { cond; arms; eliminator; loc } ->
    if Nonempty_list.for_all arms ~f:(fun (_, body) -> Dst.Expr.is_unreachable body)
    then Fail.misplaced_unreachable [%here] ~loc All_paths_unreachable;
    check_consumed cond;
    let in_branch =
      match (eliminator : Modes.Eliminator.t) with
      | Static | Erased -> true
      | Dynamic -> in_branch
    in
    Nonempty_list.iter arms ~f:(fun (_, body) -> check_unreachable ~position:`Tail ~in_branch body)
  | Lambda { arg_ty; body; arg = _; arg_mode = _; loc = _ } ->
    check_consumed arg_ty;
    check_unreachable ~position:`Tail ~in_branch:false body
  | Fun { funs; rest; loc = _ } ->
    Nonempty_list.iter funs ~f:(fun { arg_ty; ret_ty; body; _ } ->
      check_consumed arg_ty;
      check_consumed ret_ty;
      check_unreachable ~position:`Tail ~in_branch:false body);
    check_unreachable ~position:`Continuation ~in_branch rest
  | Let { bind; rest; var = _; loc = _ } ->
    check_consumed bind;
    check_unreachable ~position:`Continuation ~in_branch rest
  | Apply { fn; arg; loc = _ } ->
    check_consumed fn;
    check_consumed arg
  | Tuple { elts; loc = _ } | Make_tuple { elts; loc = _ } ->
    Nonempty_list.iter elts ~f:check_consumed
  | Arrow { arg; ret; arg_id = _; arg_mode = _; ret_mode = _; loc = _ } ->
    check_consumed arg;
    check_consumed ret
  | Select { expr; label = _; loc = _ } -> check_consumed expr
  | Variant { constructors; loc = _ } ->
    Nonempty_list.iter constructors ~f:(fun { payload; _ } -> Option.iter payload ~f:check_consumed)
  | Ref { arg; loc = _ } | Box { arg; loc = _ } -> check_consumed arg
  | Var _ | Literal _ | Constructor _ | Builtin _ -> ()
;;

let check_unreachable_top_level (dst : Dst.Top_level.t) =
  let check ~position expr = check_unreachable ~position ~in_branch:false expr in
  match dst with
  | Let { bind; _ } -> check ~position:`Consumed bind
  | Fun { funs; _ } ->
    Nonempty_list.iter funs ~f:(fun { Dst.Expr.arg_ty; ret_ty; body; _ } ->
      check ~position:`Consumed arg_ty;
      check ~position:`Consumed ret_ty;
      check ~position:`Tail body)
  | External { ty; _ } -> check ~position:`Consumed ty
  | Builtin _ -> ()
;;

let typecheck_top_level state env (dst : Dst.Top_level.t) : Top_level.t * Env.t =
  check_unreachable_top_level dst;
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
  let state = State.create () in
  let env = Env.initial in
  let program = List.rev (fst (fold_top_levels state env dst.top_levels [])) in
  let top_levels =
    let fun_bindings =
      Ids.Fn.Map.of_hashtbl_exn state.names |> Map.filter_map ~f:(fun { State.Name.var; _ } -> var)
    in
    let resolve = resolve state ~resolved:(Hashtbl.create (module Value)) in
    Reify.program ~monomorphized:(State.demand_mono state) ~fun_bindings ~resolve program
  in
  { top_levels }
;;

let typecheck tst =
  try Ok (typecheck_exn tst) with
  | Error err -> Error err
;;

module For_testing = struct
  type state = State.t

  exception Gave_up = Gave_up

  let create_state () = State.create ()
  let register_group = State.register_group
  let settle_group = State.settle_group
  let wait = State.wait
  let leq_value = leq_value
  let join_value = join_value
  let meet_value = meet_value
  let unfold = unfold
end
