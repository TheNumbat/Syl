open! Core
open! Syl
open Tst

(* Property tests for the stuck-value lattice; the obligations are recorded in
   notes/stuck-values.md. Values are built through the smart constructors only, over a
   small pool of free vars, so every generated value is a well-formed normal form. *)

let loc = Lex.Location.empty
let vars = Array.init 3 ~f:(fun stamp -> Value.var (Ident.create Ident.Raw.anon ~stamp))
let label_a = Ident.Label.of_string "a"
let label_b = Ident.Label.of_string "b"

(* Pattern binders are anonymous — they bind nothing — so or-patterns need no
   binding-consistency care and wildcard arms are pure defaults. *)
let pattern_binders = Array.init 3 ~f:(fun i -> Ident.create Ident.Raw.anon ~stamp:(10 + i))
let var_gen = Quickcheck.Generator.of_list (Array.to_list vars)

let int_value_gen =
  let open Quickcheck.Generator.Let_syntax in
  let%map n = Quickcheck.Generator.of_list [ 0L; 1L; 2L ] in
  Value.of_literal (Int n)
;;

(* Values and patterns share one fixed variant schema — [.a] always carries an int
   payload, [.b] never does — because [matches_pattern] and [Value.payload] raise on
   values that disagree with their pattern or frame about payload-ness. [Payload] frames
   only ever extract [.a]; a [.b] constructor under one stays a stuck dead extraction. *)
let constructor_value_gen =
  let open Quickcheck.Generator in
  let open Quickcheck.Generator.Let_syntax in
  union
    [ (let%map payload = int_value_gen in
       Value.constructor ~label:label_a ~payload:(Some payload))
    ; return (Value.constructor ~label:label_b ~payload:None)
    ]
;;

let wildcard_gen =
  let open Quickcheck.Generator.Let_syntax in
  let%map id = Quickcheck.Generator.of_list (Array.to_list pattern_binders) in
  (Var { id; loc } : Dst.Expr.pattern)
;;

let int_pattern_gen =
  let open Quickcheck.Generator.Let_syntax in
  let%map n = Quickcheck.Generator.of_list [ 0L; 1L ] in
  (Literal { value = Int n; loc } : Dst.Expr.pattern)
;;

let pat_a payload_gen =
  let open Quickcheck.Generator.Let_syntax in
  let%map payload = payload_gen in
  (Constructor { label = label_a; payload = Some payload; loc } : Dst.Expr.pattern)
;;

let pat_b =
  Quickcheck.Generator.return
    (Constructor { label = label_b; payload = None; loc } : Dst.Expr.pattern)
;;

let pat_a_or_b =
  let open Quickcheck.Generator.Let_syntax in
  let%map left = pat_a int_pattern_gen
  and right = pat_b in
  (Or { left; right; loc } : Dst.Expr.pattern)
;;

let modes_pool = [ Modes.default (); Modes.top (); Modes.bottom () ]

let int_arrow_ty =
  Value.type_
    (Ty.Arrow
       { arg_ty = Value.type_ Ty.Int
       ; arg_mode = Modes.default ()
       ; ret_ty = Value.type_ Ty.Int
       ; ret_mode = Modes.default ()
       })
;;

(* A small pool of binders (pre-typecheck lambdas over int): identity and const-zero.
   Unfolding an application of one reduces its [body_dst] for real, so comparisons run
   the reducer. Hash identity comes from the pool — equal hashes mean the same binder.
   Binder args must be *named* idents: [Env.bind] deliberately ignores anonymous ones
   (they are wildcards), which would leave the identity body unbound. *)
let binder_pool =
  let identity =
    let arg = Ident.create (Ident.Raw.id "x") ~stamp:20 in
    Value.binder
      { Binder.arg
      ; ty = int_arrow_ty
      ; body_dst = Var { id = arg; loc }
      ; env = Env.initial
      ; family = 9001
      ; hash = 9001
      }
  in
  let const_zero =
    Value.binder
      { Binder.arg = Ident.create (Ident.Raw.id "x") ~stamp:21
      ; ty = int_arrow_ty
      ; body_dst = Literal { value = Int 0L; loc }
      ; env = Env.initial
      ; family = 9002
      ; hash = 9002
      }
  in
  [ identity; const_zero ]
;;

(* Stuck primitive-operator values, via the normalizing smart constructors. Operands are
   type-consistent — ints under arithmetic and comparisons, bools under logic — and
   division is omitted (concrete zero divisors raise at construction). *)
let stuck_int_gen =
  let open Quickcheck.Generator in
  let open Quickcheck.Generator.Let_syntax in
  let level operand =
    union
      [ (let%map a = operand
         and b = operand in
         Int.add a b)
      ; (let%map a = operand
         and b = operand in
         Int.mul a b)
      ; (let%map a = operand in
         Int.neg a)
      ]
  in
  let operand = union [ var_gen; int_value_gen ] in
  level (union [ operand; level operand ])
;;

let stuck_bool_gen =
  let open Quickcheck.Generator in
  let open Quickcheck.Generator.Let_syntax in
  let int_operand = union [ var_gen; int_value_gen; stuck_int_gen ] in
  let bool_literal =
    let%map b = of_list [ true; false ] in
    Value.of_literal (Bool b)
  in
  let comparison =
    union
      [ (let%map a = int_operand
         and b = int_operand in
         Bool.eq a b)
      ; (let%map a = int_operand
         and b = int_operand in
         Bool.lt a b)
      ]
  in
  let bool_operand = union [ var_gen; bool_literal; comparison ] in
  union
    [ comparison
    ; (let%map a = bool_operand
       and b = bool_operand in
       Bool.and_ a b)
    ; (let%map a = bool_operand
       and b = bool_operand in
       Bool.or_ a b)
    ; (let%map a = bool_operand in
       Bool.not_ a)
    ]
;;

(* Type-valued generator: primitive types, stuck type-level computation (vars and
   conditionals over types), and the structured formers — arrows and pis with modes
   drawn from the pool (exercising arg-mode contravariance), tuple types, and
   schema-variant types. Pi returns are constant [Dependent.mono] closures, so
   comparisons evaluate them without capture concerns. *)
let ty_value_gen =
  let open Quickcheck.Generator in
  let open Quickcheck.Generator.Let_syntax in
  let base =
    union
      [ (let%map ty = of_list [ Ty.Unit; Ty.Bool; Ty.Int; Ty.Type ] in
         Value.type_ ty)
      ; var_gen
      ]
  in
  let mode = of_list modes_pool in
  let component =
    union
      [ base
      ; (let%map cond = var_gen
         and then_ = base
         and else_ = base in
         Value.if_ ~loc ~cond ~then_ ~else_)
      ]
  in
  union
    [ base
    ; (let%map arg_ty = component
       and arg_mode = mode
       and ret_ty = component
       and ret_mode = mode in
       Value.type_ (Ty.Arrow { arg_ty; arg_mode; ret_ty; ret_mode }))
    ; (let%map arg_ty = component
       and arg_mode = mode
       and ret_ty = component
       and ret_mode = mode in
       Value.type_ (Ty.Pi { arg_ty; arg_mode; ret_ty = Dependent.mono ret_ty; ret_mode }))
    ; (let%map a = component
       and b = component in
       Value.type_ (Ty.Tuple (Nonempty_list.create a [ b ])))
    ; (let%map payload = component in
       Value.type_
         (Ty.Variant (Ident.Label.Map.of_alist_exn [ label_a, Some payload; label_b, None ])))
    ]
;;

(* Injections stay on the schema: only [.a] (payload-carrying) is generated, because an
   applied injection builds a constructor with a payload, and a [.b] constructor with a
   payload would violate the payload-ness invariant the match machinery raises on. *)
let inject_a_gen =
  let open Quickcheck.Generator.Let_syntax in
  let%map ty = Quickcheck.Generator.union [ var_gen; ty_value_gen ] in
  Value.inject ~ty ~label:label_a
;;

let tuple_pattern_gen elts =
  let open Quickcheck.Generator.Let_syntax in
  let%map elts = Quickcheck.Generator.all elts in
  (Tuple { elts = Nonempty_list.of_list_exn elts; loc } : Dst.Expr.pattern)
;;

(* Arm templates are exhaustive over the schema and non-redundant — [Value.match_]
   assumes exhaustiveness when it collapses a sole surviving arm, and the typechecker
   rejects redundant arms — while deliberately spanning the risky regimes: wildcard
   defaults (positive-fact refinement cannot decide them), overlapping first-match
   literal arms, and or-patterns. Scrutinees are weighted toward vars so most matches
   are stuck; a shared var pool correlates them across values. *)
let match_gen leaf_gen =
  let open Quickcheck.Generator in
  let open Quickcheck.Generator.Let_syntax in
  let template patterns =
    List.map patterns ~f:(fun pattern_gen ->
      let%map pattern = pattern_gen
      and leaf = leaf_gen in
      pattern, leaf)
    |> all
  in
  let%map scrutinee, arms =
    union
      [ (let%map scrutinee = weighted_union [ 3., var_gen; 1., constructor_value_gen ]
         and arms =
           union
             [ template [ pat_a wildcard_gen; pat_b ]
             ; template [ pat_a wildcard_gen; wildcard_gen ]
             ; template [ pat_a int_pattern_gen; wildcard_gen ]
             ; template [ pat_b; wildcard_gen ]
             ; template [ pat_a int_pattern_gen; pat_a wildcard_gen; pat_b ]
             ; template [ pat_a_or_b; wildcard_gen ]
             ]
         in
         scrutinee, arms)
      ; (let%map scrutinee = weighted_union [ 3., var_gen; 1., int_value_gen; 1., stuck_int_gen ]
         and arms = template [ int_pattern_gen; wildcard_gen ] in
         scrutinee, arms)
      ; (let%map scrutinee =
           weighted_union
             [ 3., var_gen
             ; ( 1.
               , let%map a = int_value_gen
                 and b = int_value_gen in
                 Value.tuple (Nonempty_list.create a [ b ]) )
             ]
         and arms =
           union
             [ template [ tuple_pattern_gen [ int_pattern_gen; wildcard_gen ]; wildcard_gen ]
             ; template
                 [ tuple_pattern_gen [ int_pattern_gen; int_pattern_gen ]
                 ; tuple_pattern_gen [ wildcard_gen; wildcard_gen ]
                 ]
             ]
         in
         scrutinee, arms)
      ]
  in
  Value.match_ ~scrutinee ~arms:(Nonempty_list.of_list_exn arms)
;;

let value_gen =
  let open Quickcheck.Generator in
  let open Quickcheck.Generator.Let_syntax in
  (* Generated values must be well-formed, as if produced by a typechecked program:
     projection subjects stay 2-tuple-shaped so [Value.proj] cannot index out of bounds,
     and variant values and patterns follow the fixed schema above. *)
  let leaf =
    union
      [ return Value.unit
      ; return Value.bottom
      ; (let%map b = of_list [ true; false ] in
         Value.of_literal (Bool b))
      ; int_value_gen
      ; (let%map ty = of_list [ Ty.Unit; Ty.Bool; Ty.Int ] in
         Value.type_ ty)
      ; var_gen
      ; constructor_value_gen
      ; of_list binder_pool
      ; (let%map symbol = of_list [ "ext_f"; "ext_g" ] in
         Value.external_ ~symbol ~ty:int_arrow_ty)
      ; (let%map prim =
           of_list [ Builtin.Prim.Int Add; Builtin.Prim.Int Lt; Builtin.Prim.Bool Not; Assert ]
         in
         Value.prim prim)
      ; inject_a_gen
      ]
  in
  (* Conditional scrutinees include stuck comparisons, matching how [if erased] guards
     look in real programs. *)
  let cond_gen = weighted_union [ 3., var_gen; 1., stuck_bool_gen ] in
  recursive_union [ leaf ] ~f:(fun self ->
    let pair =
      let%map a = self
      and b = self in
      Value.tuple (Nonempty_list.create a [ b ])
    in
    let if_ gen =
      let%map cond = cond_gen
      and then_ = gen
      and else_ = gen in
      Value.if_ ~loc ~cond ~then_ ~else_
    in
    [ pair
    ; if_ self
      (* Twice: nested conditionals are where the correlation machinery lives, and the
           flat-form branches below would otherwise dilute them. *)
    ; match_gen self
    ; match_gen self
    ; union [ stuck_int_gen; stuck_bool_gen; ty_value_gen ]
    ; (let%map fn = union [ var_gen; if_ var_gen; of_list binder_pool; inject_a_gen ]
       and arg = self in
       Value.apply ~fn ~arg)
    ; (let%map tuple = union [ var_gen; if_ pair ]
       and index = of_list [ 0; 1 ] in
       Value.proj tuple index)
    ; (let%map variant =
         union [ var_gen; if_ constructor_value_gen; match_gen constructor_value_gen ]
       in
       Value.payload variant ~label:label_a)
    ])
;;

let check_coherence state a b =
  let leq = Typecheck.For_testing.leq_value state in
  let require cond message =
    if not cond then raise_s [%message message (a : Value.t) (b : Value.t)]
  in
  require (leq a a) "leq is not reflexive on lhs";
  require (leq b b) "leq is not reflexive on rhs";
  let a_le_b = leq a b
  and b_le_a = leq b a in
  (* Both orientations: the join/meet procedures are asymmetric in code (which side
     decomposes or unfolds first), so definedness and the result must be checked
     order-independent up to mutual leq. *)
  (match Typecheck.For_testing.join_value state a b, Typecheck.For_testing.join_value state b a with
   | (Some _, None | None, Some _) as joins ->
     raise_s
       [%message
         "join definedness is orientation-dependent"
           (a : Value.t)
           (b : Value.t)
           (joins : Value.t option * Value.t option)]
   | None, None ->
     require ((not a_le_b) && not b_le_a) "leq exhibits an upper bound but join is undefined"
   | Some join, Some swapped ->
     let require cond message =
       if not cond then raise_s [%message message (a : Value.t) (b : Value.t) (join : Value.t)]
     in
     require (leq join join) "leq is not reflexive on a join output";
     require (leq a join) "join is not an upper bound of lhs";
     require (leq b join) "join is not an upper bound of rhs";
     (* Leastness is decidable at comparable pairs: the join is the larger side. *)
     if a_le_b then require (leq join b) "join of a comparable pair exceeds its larger side";
     if b_le_a then require (leq join a) "join of a comparable pair exceeds its larger side";
     if not (leq join swapped && leq swapped join)
     then
       raise_s
         [%message
           "join is orientation-dependent"
             (a : Value.t)
             (b : Value.t)
             (join : Value.t)
             (swapped : Value.t)]);
  match Typecheck.For_testing.meet_value state a b, Typecheck.For_testing.meet_value state b a with
  | (Some _, None | None, Some _) as meets ->
    raise_s
      [%message
        "meet definedness is orientation-dependent"
          (a : Value.t)
          (b : Value.t)
          (meets : Value.t option * Value.t option)]
  | None, None -> require ((not a_le_b) && not b_le_a) "leq holds but meet is undefined"
  | Some meet, Some swapped ->
    let require cond message =
      if not cond then raise_s [%message message (a : Value.t) (b : Value.t) (meet : Value.t)]
    in
    require (leq meet meet) "leq is not reflexive on a meet output";
    require (leq meet a) "meet is not a lower bound of lhs";
    require (leq meet b) "meet is not a lower bound of rhs";
    (* Greatestness is decidable at comparable pairs: the meet is the smaller side. *)
    if a_le_b then require (leq a meet) "meet of a comparable pair misses its smaller side";
    if b_le_a then require (leq b meet) "meet of a comparable pair misses its smaller side";
    if not (leq meet swapped && leq swapped meet)
    then
      raise_s
        [%message
          "meet is orientation-dependent"
            (a : Value.t)
            (b : Value.t)
            (meet : Value.t)
            (swapped : Value.t)]
;;

(* The default size ramp (0..29) draws values with hundreds of match nodes; the
   judgments are superlinear on nested stuck matches, so capping generator size keeps
   the suite fast in watch mode. Every counterexample found so far fits comfortably
   under this cap. *)
let sizes = Sequence.cycle_list_exn (List.range 0 13)

let%test_unit "leq/join/meet cohere on stuck values" =
  Quickcheck.test
    ~sizes
    (Quickcheck.Generator.tuple2 value_gen value_gen)
    ~sexp_of:[%sexp_of: Value.t * Value.t]
    ~f:(fun (a, b) ->
      let state = Typecheck.For_testing.create_state () in
      let leq = Typecheck.For_testing.leq_value state in
      let require cond message =
        if not cond then raise_s [%message message (a : Value.t) (b : Value.t)]
      in
      check_coherence state a b;
      (* Independent draws almost never produce the correlated shapes where leq holds
         through the stuck-match rules, so also relate each value to a conditional
         embedding of itself. These pin completeness, not just coherence: [refine]
         restores the arm-wise alignment, so a conditional with equal arms is equivalent
         to its leaf, and tuple formation commutes with a conditional. *)
      let embed v = Value.if_ ~loc ~cond:vars.(0) ~then_:v ~else_:v in
      require (leq (embed a) a) "a conditional with equal arms is not leq its leaf";
      require (leq a (embed a)) "a leaf is not leq the conditional embedding it";
      check_coherence state (embed a) a;
      check_coherence state a (embed a);
      check_coherence state (embed b) b;
      check_coherence state b (embed b);
      let pair x y = Value.tuple (Nonempty_list.create x [ y ]) in
      let inside = pair (Value.if_ ~loc ~cond:vars.(0) ~then_:a ~else_:b) a in
      let outside = Value.if_ ~loc ~cond:vars.(0) ~then_:(pair a a) ~else_:(pair b a) in
      let require cond message =
        if not cond then raise_s [%message message (inside : Value.t) (outside : Value.t)]
      in
      require (leq inside outside) "tuple formation does not commute into a conditional";
      require (leq outside inside) "tuple formation does not commute out of a conditional";
      check_coherence state inside outside)
;;

(* A correlated pair the generator cannot draw: a wildcard-arm match nested, under its
   own default arm, inside another match on the same scrutinee. [refine] orders the pair
   ([a ≤ b] holds arm-by-arm), the arm-wise join disagrees, and the disagreement
   conditional would have a wildcard arm [refine] could not re-select — so join must
   answer through leq's ordering ([join_stuck_match]'s fast path) rather than emit a
   bound leq cannot verify, and [arms_self_selecting] withholds the conditional. *)
let%test_unit "wildcard-arm conditionals stay coherent" =
  let state = Typecheck.For_testing.create_state () in
  let pat_a : Dst.Expr.pattern = Constructor { label = label_a; payload = None; loc } in
  let wildcard stamp : Dst.Expr.pattern = Var { id = Ident.create Ident.Raw.anon ~stamp; loc } in
  let match_a ~on_a ~otherwise ~stamp =
    Value.match_
      ~scrutinee:vars.(1)
      ~arms:(Nonempty_list.create (pat_a, on_a) [ wildcard stamp, otherwise ])
  in
  let int_ n = Value.of_literal (Int n) in
  let b = match_a ~on_a:(int_ 0L) ~otherwise:(int_ 1L) ~stamp:10 in
  let a = match_a ~on_a:(int_ 0L) ~otherwise:b ~stamp:11 in
  check_coherence state a b;
  check_coherence state b a
;;

(* [pattern_implies] is the soundness core of [refine]: a wrong [Some true] silently
   selects the wrong arm. Oracle: on a concrete value of the patterns' shared type,
   [matches_pattern] is decided, and any implication or exclusion [pattern_implies]
   claims must agree with it. *)
let%test_unit "pattern_implies agrees with matches_pattern on concrete values" =
  let open Quickcheck.Generator in
  let open Quickcheck.Generator.Let_syntax in
  let or_of gen =
    let%map left = gen
    and right = gen in
    (Or { left; right; loc } : Dst.Expr.pattern)
  in
  let full gen = union [ gen; wildcard_gen; or_of gen ] in
  let bool_pattern =
    let%map b = of_list [ true; false ] in
    (Literal { value = Bool b; loc } : Dst.Expr.pattern)
  in
  let bool_value =
    let%map b = of_list [ true; false ] in
    Value.of_literal (Bool b)
  in
  let variant_pattern = union [ pat_a (full int_pattern_gen); pat_b; pat_a_or_b ] in
  let tuple_pattern =
    let%map fst = full int_pattern_gen
    and snd = full variant_pattern in
    (Tuple { elts = Nonempty_list.create fst [ snd ]; loc } : Dst.Expr.pattern)
  in
  let tuple_value =
    let%map fst = int_value_gen
    and snd = constructor_value_gen in
    Value.tuple (Nonempty_list.create fst [ snd ])
  in
  let typed_case (pattern, value) = tuple3 (full pattern) (full pattern) value in
  let triple_gen =
    union
      (List.map
         ~f:typed_case
         [ int_pattern_gen, int_value_gen
         ; bool_pattern, bool_value
         ; variant_pattern, constructor_value_gen
         ; tuple_pattern, tuple_value
         ])
  in
  Quickcheck.test
    triple_gen
    ~sexp_of:[%sexp_of: Dst.Expr.pattern * Dst.Expr.pattern * Value.t]
    ~f:(fun (fact, pattern, value) ->
      let matches pattern =
        match Pattern.matches_pattern value pattern with
        | Match _ -> true
        | No_match -> false
        | Unknown ->
          raise_s
            [%message
              "matches_pattern is undecided on a concrete value"
                (value : Value.t)
                (pattern : Dst.Expr.pattern)]
      in
      if matches fact
      then (
        let require cond message =
          if not cond
          then
            raise_s
              [%message
                message (fact : Dst.Expr.pattern) (pattern : Dst.Expr.pattern) (value : Value.t)]
        in
        match Pattern.pattern_implies ~fact pattern with
        | Known true -> require (matches pattern) "claimed implication has a concrete refutation"
        | Known false -> require (not (matches pattern)) "claimed exclusion has a concrete witness"
        | Unknown -> ()))
;;

(* Distributing a stuck eliminator frame into a match's arms is claimed to be a semantic
   equivalence, not an approximation (notes/stuck-values.md): the framed and distributed
   forms must be mutually leq and cohere as a pair. This exercises
   admission/reducibility parity directly — the judgment only relates the two forms if
   its dispatch admits the shape [unfold_value] can reduce — and would have caught the
   fresh-var reflexivity bug: dead payload extractions must rewrite deterministically. *)
let%test_unit "frame distribution over match arms is a semantic equivalence" =
  let open Quickcheck.Generator in
  let open Quickcheck.Generator.Let_syntax in
  let pair_gen =
    let%map a = value_gen
    and b = value_gen in
    Value.tuple (Nonempty_list.create a [ b ])
  in
  let framed_gen =
    union
      [ (let%map m = match_gen pair_gen
         and index = of_list [ 0; 1 ] in
         m, `Proj index)
      ; (let%map m = match_gen constructor_value_gen in
         m, `Payload)
      ; (let%map m = match_gen var_gen
         and arg = value_gen in
         m, `Apply arg)
      ]
  in
  Quickcheck.test
    ~sizes
    framed_gen
    ~sexp_of:[%sexp_of: Value.t * [ `Proj of int | `Payload | `Apply of Value.t ]]
    ~f:(fun (m, frame) ->
      let state = Typecheck.For_testing.create_state () in
      let frame value =
        match frame with
        | `Proj index -> Value.proj value index
        | `Payload -> Value.payload value ~label:label_a
        | `Apply arg -> Value.apply ~fn:value ~arg
      in
      let framed = frame m in
      let distributed =
        (* [m] may have collapsed at construction (concrete scrutinee); then there is
           nothing to distribute into and the forms coincide. *)
        match m with
        | Match { scrutinee; arms } ->
          Value.match_
            ~scrutinee
            ~arms:(Nonempty_list.map arms ~f:(fun (pattern, leaf) -> pattern, frame leaf))
        | m -> frame m
      in
      let leq = Typecheck.For_testing.leq_value state in
      let require cond message =
        if not cond then raise_s [%message message (framed : Value.t) (distributed : Value.t)]
      in
      require (leq framed distributed) "framed form is not leq its distribution";
      require (leq distributed framed) "distributed form is not leq its framed form";
      check_coherence state framed distributed)
;;
