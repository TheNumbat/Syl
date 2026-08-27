open! Core
open! Syl
open Tst

(* These are soundness obligations: reflexivity, transitivity, agreement
   across orientation, and every exhibited bound being leq-verifiable. *)

(* Fixture stamps live far above anything minted during the tests. *)

let fresh_var () = Value.var (Ident.fresh Ident.Raw.anon)
let int_vars = Array.init 3 ~f:(fun _ -> fresh_var ())
let bool_vars = Array.init 2 ~f:(fun _ -> fresh_var ())
let spine_vars = Array.init 2 ~f:(fun _ -> fresh_var ())
let label_a = Ident.Label.of_string "a"
let label_b = Ident.Label.of_string "b"

let modes =
  [ Modes.create ~staticity:Static ~erasure:Unerased
  ; Modes.create ~staticity:Parametric ~erasure:Unerased
  ; Modes.create ~staticity:Dynamic ~erasure:Unerased
  ; Modes.create ~staticity:Static ~erasure:Erased
  ]
;;

(* Families for suspensions and reducible spines: real binders whose bodies
   whnf evaluates — identity on its type argument, and a wrapping variant.
   Distinct uids make same-argument applications generative. *)
let binder_pool =
  let loc = Lex.Location.empty in
  let type_arrow =
    Value.type_
      (Ty.Arrow
         { arg_ty = Value.type_ Type
         ; arg_mode = Modes.create ~staticity:Static ~erasure:Erased
         ; ret_ty = Value.type_ Type
         ; ret_mode = Modes.create ~staticity:Static ~erasure:Erased
         })
  in
  let mk ~body =
    let arg = Ident.fresh (Ident.Raw.id "t") in
    Value.binder
      (Binder.const
         ~arg
         ~ty:type_arrow
         ~body_dst:(body arg)
         ~env:Env.initial
         ~family:(Ids.Family.create ())
         ())
  in
  let identity = mk ~body:(fun arg : Dst.Expr.t -> Var { id = arg; loc }) in
  let wrap =
    mk ~body:(fun arg : Dst.Expr.t ->
      Variant
        { constructors =
            Nonempty_list.singleton
              { Dst.Expr.label = label_a; payload = Some (Var { id = arg; loc }) }
        ; loc
        })
  in
  [ identity; wrap ]
;;

open Quickcheck.Generator
open Quickcheck.Generator.Let_syntax

let int_atom =
  union
    [ of_list (Array.to_list int_vars)
    ; (let%map i = Int64.gen_incl (-4L) 4L in
       Int.const i)
    ]
;;

let bool_atom =
  union
    [ of_list (Array.to_list bool_vars)
    ; (let%map b = bool in
       Bool.const b)
    ]
;;

let rec int_value depth =
  if depth = 0
  then int_atom
  else (
    let sub = int_value (depth - 1) in
    union
      [ int_atom
      ; (let%map a = sub
         and b = sub in
         Int.add a b)
      ; (let%map a = sub
         and b = sub in
         Int.sub a b)
      ; (let%map a = sub
         and b = sub in
         Int.mul a b)
      ; (let%map a = sub in
         Int.neg a)
      ])

and bool_value depth =
  if depth = 0
  then bool_atom
  else (
    let int_sub = int_value (depth - 1) in
    let sub = bool_value (depth - 1) in
    union
      [ bool_atom
      ; (let%map a = int_sub
         and b = int_sub in
         Bool.eq a b)
      ; (let%map a = int_sub
         and b = int_sub in
         Bool.lt a b)
      ; (let%map a = sub
         and b = sub in
         Bool.and_ a b)
      ; (let%map a = sub
         and b = sub in
         Bool.or_ a b)
      ; (let%map a = sub in
         Bool.not_ a)
      ])
;;

let rec ty_value depth =
  let atom = of_list [ Value.type_ Unit; Value.type_ Bool; Value.type_ Int; Value.type_ Type ] in
  if depth = 0
  then atom
  else (
    let sub = ty_value (depth - 1) in
    union
      [ atom
      ; (let%map arg_ty = sub
         and ret_ty = sub
         and arg_mode = of_list modes
         and ret_mode = of_list modes in
         Value.type_ (Ty.Arrow { arg_ty; arg_mode; ret_ty; ret_mode }))
      ; (let%map elts = list_with_length 2 sub in
         Value.type_ (Ty.Tuple (Nonempty_list.of_list_exn elts)))
      ; (let%map payload = sub in
         Value.type_
           (Ty.Variant (Ident.Label.Map.of_alist_exn [ label_a, Some payload; label_b, None ])))
        (* Ref types: invariant pointees, double indirection, and suspended
           names over the binder pool — the barrier compares those by spine,
           and mixed name/literal pairs exercise the roll by unfolding the
           pool binder for real. *)
      ; (let%map payload = sub in
         Value.type_ (Ty.Ref payload))
      ; (let%map payload = atom in
         Value.type_ (Ty.Ref (Value.type_ (Ty.Ref payload))))
      ; (let%map fn = of_list binder_pool
         and arg = sub in
         Value.type_ (Ty.Ref (Value.apply ~fn ~arg)))
      ])
;;

let rec value depth =
  let leaf =
    union
      [ return Value.unit
      ; return Value.bottom
      ; int_value 1
      ; bool_value 1
      ; ty_value 1
      ; of_list (Array.to_list spine_vars)
      ]
  in
  if depth = 0
  then leaf
  else (
    let sub = value (depth - 1) in
    union
      [ leaf
      ; (let%map elts = list_with_length 2 sub in
         Value.tuple (Nonempty_list.of_list_exn elts))
      ; (let%map payload = sub in
         Value.constructor ~label:label_a ~payload:(Some payload))
      ; return (Value.constructor ~label:label_b ~payload:None)
      ; (let%map fn = of_list (Array.to_list spine_vars)
         and arg = sub in
         Value.apply ~fn ~arg)
      ; (let%map head = of_list (Array.to_list spine_vars)
         and index = of_list [ 0; 1 ] in
         Value.proj head index)
      ; (let%map head = of_list (Array.to_list spine_vars) in
         Value.payload head ~label:label_a)
      ; (let%map payload = sub in
         Value.box payload)
      ; (let%map head = of_list (Array.to_list spine_vars) in
         Value.deref head)
        (* A genuinely reducible spine: whnf evaluates the pool binder. *)
      ; (let%map fn = of_list binder_pool
         and arg = ty_value 1 in
         Value.apply ~fn ~arg)
      ; (let%map cond = bool_value 1
         and then_ = sub
         and else_ = sub in
         Value.if_ ~cond ~then_ ~else_)
        (* Int-scrutineed conditionals, arms possibly or-patterned and leaves
           possibly the scrutinee itself: the facts machinery's home turf. *)
      ; (let%bind scrutinee = of_list (Array.to_list int_vars) in
         let leaf = union [ sub; return scrutinee ] in
         let head =
           union
             [ (let%map i = Int64.gen_incl (-2L) 2L in
                Pattern.Canon.Literal (Int i))
             ; (let%map i = Int64.gen_incl (-2L) 2L
                and j = Int64.gen_incl (-2L) 2L in
                Pattern.Canon.Or (Literal (Int i), Literal (Int j)))
             ]
         in
         let%map head = head
         and a = leaf
         and b = leaf in
         Value.match_ ~scrutinee ~arms:(Nonempty_list.create (head, a) [ Pattern.Canon.Wild, b ]))
        (* Reducible when the conditional resolves: whnf distributes the
           projection into the arms, so folded and forced spellings coexist. *)
      ; (let%map cond = bool_value 1
         and x = sub
         and y = sub
         and index = of_list [ 0; 1 ] in
         let tuple a b = Value.tuple (Nonempty_list.of_list_exn [ a; b ]) in
         Value.proj (Value.if_ ~cond ~then_:(tuple x y) ~else_:(tuple y x)) index)
      ])
;;

let pair = tuple2 (value 3) (value 3)
let triple = tuple3 (value 3) (value 3) (value 3)

(* Fuel exhaustion ([Typecheck.Gave_up]) is a permitted non-answer: the
   obligations below bind only judgments that complete. *)
let permitted f =
  try f () with
  | Typecheck.For_testing.Gave_up -> ()
;;

let%test_unit "leq is reflexive" =
  Quickcheck.test (value 3) ~sexp_of:[%sexp_of: Value.t] ~f:(fun a ->
    permitted
    @@ fun () ->
    let state = Typecheck.For_testing.create_state () in
    if not (Typecheck.For_testing.leq_value state a a)
    then raise_s [%message "leq not reflexive" (a : Value.t)])
;;

let%test_unit "leq is transitive" =
  Quickcheck.test triple ~sexp_of:[%sexp_of: Value.t * Value.t * Value.t] ~f:(fun (a, b, c) ->
    permitted
    @@ fun () ->
    let state = Typecheck.For_testing.create_state () in
    let leq = Typecheck.For_testing.leq_value state in
    if leq a b && leq b c && not (leq a c)
    then raise_s [%message "leq not transitive" (a : Value.t) (b : Value.t) (c : Value.t)])
;;

let%test_unit "join exhibits leq-verifiable upper bounds" =
  Quickcheck.test pair ~sexp_of:[%sexp_of: Value.t * Value.t] ~f:(fun (a, b) ->
    permitted
    @@ fun () ->
    let state = Typecheck.For_testing.create_state () in
    let leq = Typecheck.For_testing.leq_value state in
    match Typecheck.For_testing.join_value state a b with
    | None -> ()
    | Some j ->
      if not (leq a j && leq b j)
      then raise_s [%message "join not an upper bound" (a : Value.t) (b : Value.t) (j : Value.t)])
;;

let%test_unit "meet exhibits leq-verifiable lower bounds" =
  Quickcheck.test pair ~sexp_of:[%sexp_of: Value.t * Value.t] ~f:(fun (a, b) ->
    permitted
    @@ fun () ->
    let state = Typecheck.For_testing.create_state () in
    let leq = Typecheck.For_testing.leq_value state in
    match Typecheck.For_testing.meet_value state a b with
    | None -> ()
    | Some m ->
      if not (leq m a && leq m b)
      then raise_s [%message "meet not a lower bound" (a : Value.t) (b : Value.t) (m : Value.t)])
;;

let%test_unit "join agrees across orientation" =
  Quickcheck.test pair ~sexp_of:[%sexp_of: Value.t * Value.t] ~f:(fun (a, b) ->
    permitted
    @@ fun () ->
    let state = Typecheck.For_testing.create_state () in
    let leq = Typecheck.For_testing.leq_value state in
    match
      Typecheck.For_testing.join_value state a b, Typecheck.For_testing.join_value state b a
    with
    | None, None -> ()
    | Some j, Some j' ->
      if not (leq j j' && leq j' j)
      then
        raise_s
          [%message
            "join orientations disagree" (a : Value.t) (b : Value.t) (j : Value.t) (j' : Value.t)]
    | Some j, None | None, Some j ->
      raise_s
        [%message "join defined in one orientation only" (a : Value.t) (b : Value.t) (j : Value.t)])
;;

let%test_unit "meet agrees across orientation" =
  Quickcheck.test pair ~sexp_of:[%sexp_of: Value.t * Value.t] ~f:(fun (a, b) ->
    permitted
    @@ fun () ->
    let state = Typecheck.For_testing.create_state () in
    let leq = Typecheck.For_testing.leq_value state in
    match
      Typecheck.For_testing.meet_value state a b, Typecheck.For_testing.meet_value state b a
    with
    | None, None -> ()
    | Some m, Some m' ->
      if not (leq m m' && leq m' m)
      then
        raise_s
          [%message
            "meet orientations disagree" (a : Value.t) (b : Value.t) (m : Value.t) (m' : Value.t)]
    | Some m, None | None, Some m ->
      raise_s
        [%message "meet defined in one orientation only" (a : Value.t) (b : Value.t) (m : Value.t)])
;;

let%test_unit "forcing does not move a value in the order" =
  Quickcheck.test (value 3) ~sexp_of:[%sexp_of: Value.t] ~f:(fun a ->
    permitted
    @@ fun () ->
    let state = Typecheck.For_testing.create_state () in
    let leq = Typecheck.For_testing.leq_value state in
    let forced = Typecheck.For_testing.unfold state a in
    if not (leq a forced && leq forced a)
    then raise_s [%message "forced spelling not equivalent" (a : Value.t) (forced : Value.t)])
;;

let%test_unit "a comparable pair's bounds are its sides" =
  Quickcheck.test pair ~sexp_of:[%sexp_of: Value.t * Value.t] ~f:(fun (a, b) ->
    permitted
    @@ fun () ->
    let state = Typecheck.For_testing.create_state () in
    let leq = Typecheck.For_testing.leq_value state in
    if leq a b
    then (
      (match Typecheck.For_testing.join_value state a b with
       | Some j when leq j b && leq b j -> ()
       | j ->
         raise_s
           [%message "join of a comparable pair" (a : Value.t) (b : Value.t) (j : Value.t option)]);
      match Typecheck.For_testing.meet_value state a b with
      | Some m when leq m a && leq a m -> ()
      | m ->
        raise_s
          [%message "meet of a comparable pair" (a : Value.t) (b : Value.t) (m : Value.t option)]))
;;

let%test_unit "join and meet are idempotent" =
  Quickcheck.test (value 3) ~sexp_of:[%sexp_of: Value.t] ~f:(fun a ->
    permitted
    @@ fun () ->
    let state = Typecheck.For_testing.create_state () in
    let leq = Typecheck.For_testing.leq_value state in
    let check name = function
      | Some j when leq a j && leq j a -> ()
      | result ->
        raise_s [%message "not idempotent" (name : string) (a : Value.t) (result : Value.t option)]
    in
    check "join" (Typecheck.For_testing.join_value state a a);
    check "meet" (Typecheck.For_testing.meet_value state a a))
;;

(* Patterns a value definitely matches, by generalizing its own structure. *)
let rec generalize (v : Value.t) : Pattern.Canon.t Quickcheck.Generator.t =
  let wild = return Pattern.Canon.Wild in
  let base =
    match v.node with
    | Unit -> union [ wild; return (Pattern.Canon.Literal Unit) ]
    | Bool (T b) -> union [ wild; return (Pattern.Canon.Literal (Bool b)) ]
    | Int (T i) -> union [ wild; return (Pattern.Canon.Literal (Int i)) ]
    | Tuple elts ->
      union
        [ wild
        ; Nonempty_list.to_list elts
          |> List.map ~f:generalize
          |> all
          |> map ~f:(fun ps -> Pattern.Canon.Tuple (Nonempty_list.of_list_exn ps))
        ]
    | Constructor { label; payload = None } ->
      union [ wild; return (Pattern.Canon.Constructor { label; payload = None }) ]
    | Constructor { label; payload = Some payload } ->
      union
        [ wild
        ; map (generalize payload) ~f:(fun payload ->
            Pattern.Canon.Constructor { label; payload = Some payload })
        ]
    | Box payload -> union [ wild; map (generalize payload) ~f:(fun p -> Pattern.Canon.Ref p) ]
    | _ -> wild
  in
  union [ base; map base ~f:(fun p -> Pattern.Canon.Or (p, Wild)) ]
;;

(* The law behind positive facts: where the pattern definitely matches, the
   structure it implies at the scrutinee is the scrutinee. *)
let%test_unit "specialize at a definite match is the value" =
  Quickcheck.test
    (bind (value 3) ~f:(fun v -> map (generalize v) ~f:(fun p -> v, p)))
    ~sexp_of:[%sexp_of: Value.t * Pattern.Canon.t]
    ~f:(fun (v, p) ->
      permitted
      @@ fun () ->
      match Pattern.matches v p with
      | Unknown -> ()
      | No_match -> raise_s [%message "generalization refuted" (v : Value.t) (p : Pattern.Canon.t)]
      | Match ->
        let state = Typecheck.For_testing.create_state () in
        let leq = Typecheck.For_testing.leq_value state in
        let specialized = Pattern.specialize p ~scrutinee:v in
        if not (leq specialized v && leq v specialized)
        then
          raise_s
            [%message
              "specialize moved the value"
                (v : Value.t)
                (p : Pattern.Canon.t)
                (specialized : Value.t)])
;;

(* ---- Deterministic stage-4 pins ---- *)

let arrow ~arg_mode ~ret_mode : Value.t =
  Value.type_ (Ty.Arrow { arg_ty = Value.type_ Int; arg_mode; ret_ty = Value.type_ Int; ret_mode })
;;

let%test_unit "an ordered pair's join and meet are its sides" =
  let state = Typecheck.For_testing.create_state () in
  let x = fresh_var () in
  (match Typecheck.For_testing.join_value state Value.bottom x with
   | Some j when phys_equal j x -> ()
   | j -> raise_s [%message "join with bottom is not the other side" (j : Value.t option)]);
  match Typecheck.For_testing.meet_value state Value.bottom x with
  | Some m when phys_equal m Value.bottom -> ()
  | m -> raise_s [%message "meet with bottom is not bottom" (m : Value.t option)]
;;

(* Mode antichains make the pair genuinely unordered, so neither the ordered
   pick nor the aligned structural row applies: only the conditional
   representative can exhibit the bound, and [leq]'s arm rules must verify it.
   The two arms are chosen so their per-arm bounds against [arr_c] differ —
   otherwise the representative collapses to a bare arrow and the arm rules
   go unexercised. *)
let%test_unit "a match-involved pair takes the conditional representative" =
  let state = Typecheck.For_testing.create_state () in
  let leq = Typecheck.For_testing.leq_value state in
  let static_erased = Modes.create ~staticity:Static ~erasure:Erased in
  let param_unerased = Modes.create ~staticity:Parametric ~erasure:Unerased in
  let dynamic_unerased = Modes.create ~staticity:Dynamic ~erasure:Unerased in
  let ret = Modes.create ~staticity:Static ~erasure:Unerased in
  let arr_a = arrow ~arg_mode:static_erased ~ret_mode:ret in
  let arr_b = arrow ~arg_mode:param_unerased ~ret_mode:ret in
  let arr_c = arrow ~arg_mode:dynamic_unerased ~ret_mode:ret in
  let m = Value.if_ ~cond:(fresh_var ()) ~then_:arr_a ~else_:arr_b in
  assert (not (leq m arr_c));
  assert (not (leq arr_c m));
  (match Typecheck.For_testing.join_value state m arr_c with
   | Some j when leq m j && leq arr_c j -> ()
   | j -> raise_s [%message "no verifiable conditional join" (j : Value.t option)]);
  match Typecheck.For_testing.meet_value state m arr_c with
  | Some w when leq w m && leq w arr_c -> ()
  | w -> raise_s [%message "no verifiable conditional meet" (w : Value.t option)]
;;

(* The verdict memo: 1100 identical component pairs, each charging one unfold
   in the mixed leq row. Completing inside the 1000-charge episode budget
   requires the repeats to hit the cache rather than re-derive. *)
let%test_unit "repeated judgment pairs hit the leq memo" =
  let state = Typecheck.For_testing.create_state () in
  let v = fresh_var () in
  let arr m = arrow ~arg_mode:m ~ret_mode:(Modes.create ~staticity:Static ~erasure:Unerased) in
  let arr1 = arr (Modes.create ~staticity:Dynamic ~erasure:Erased) in
  let arr2 = arr (Modes.create ~staticity:Dynamic ~erasure:Unerased) in
  let arr3 = arr (Modes.create ~staticity:Parametric ~erasure:Unerased) in
  let arr4 = arr (Modes.create ~staticity:Static ~erasure:Unerased) in
  let pair a b = Value.tuple (Nonempty_list.of_list_exn [ a; b ]) in
  let x = Value.proj (Value.if_ ~cond:v ~then_:(pair arr1 arr2) ~else_:(pair arr2 arr1)) 0 in
  let y = Value.if_ ~cond:v ~then_:arr3 ~else_:arr4 in
  let wide value = Value.tuple (Nonempty_list.of_list_exn (List.init 1100 ~f:(fun _ -> value))) in
  if not (Typecheck.For_testing.leq_value state (wide x) (wide y))
  then raise_s [%message "repeated pairs did not complete"]
;;

(* Positive facts in arm decomposition: inside an arm's worlds the scrutinee
   is what the pattern implies, so a leaf or bound spelling the scrutinee
   compares as that world's value. *)
let%test_unit "arm decomposition assumes the arm's pattern" =
  let state = Typecheck.For_testing.create_state () in
  let leq = Typecheck.For_testing.leq_value state in
  let c = fresh_var () in
  let conditional = Value.if_ ~cond:c ~then_:c ~else_:(Bool.const false) in
  if not (leq conditional c) then raise_s [%message "a-side fact missing" (conditional : Value.t)];
  let conditional = Value.if_ ~cond:c ~then_:(Bool.const true) ~else_:c in
  if not (leq c conditional) then raise_s [%message "b-side fact missing" (conditional : Value.t)]
;;
