open! Core
open! Syl
open Tst

(* Property tests for the stuck-value lattice. Values are built through the smart
   constructors only, over a small pool of free vars, so every generated value is
   a well-formed normal form. *)

let loc = Lex.Location.empty
let label_a = Ident.Label.of_string "a"
let label_b = Ident.Label.of_string "b"
let excl list = Pattern.Excluded.Set.of_list list

(* Typed var pools: every position draws vars of its own type, as a typechecked program
   would — the machinery's shape demands (projection subjects, function positions,
   pattern/value agreement) are honest invariants over well-typed values, and
   [specialize_arm] preserves types, so even contradictory worlds stay shape-legal.
   Sharing within a pool keeps values correlated across positions. Tuple vars denote
   int pairs, matching the tuple patterns below; fn vars are opaque functions. *)
let typed_vars offset count =
  Array.init count ~f:(fun i -> Value.var (Ident.create Ident.Raw.anon ~stamp:(offset + i)))
;;

let int_vars = typed_vars 0 3
let bool_vars = typed_vars 30 2
let variant_vars = typed_vars 40 2
let tuple_vars = typed_vars 50 2
let fn_vars = typed_vars 60 2
let ty_vars = typed_vars 70 2
let pool vars = Quickcheck.Generator.of_list (Array.to_list vars)
let int_var_gen = pool int_vars
let bool_var_gen = pool bool_vars
let variant_var_gen = pool variant_vars
let tuple_var_gen = pool tuple_vars
let fn_var_gen = pool fn_vars
let ty_var_gen = pool ty_vars

(* Pattern binders are anonymous — they bind nothing — so or-patterns need no
   binding-consistency care and wildcard arms are pure defaults. *)
let pattern_binders = Array.init 3 ~f:(fun i -> Ident.create Ident.Raw.anon ~stamp:(10 + i))

let int_value_gen =
  let open Quickcheck.Generator.Let_syntax in
  let%map n = Quickcheck.Generator.of_list [ 0L; 1L; 2L ] in
  Value.of_literal (Int n)
;;

(* Values and patterns share one fixed variant schema — [.a] always carries an int
   payload, [.b] never does — because [matches] and [Value.payload] raise on
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

(* Refinements pair exclusions with type-consistent stuck bases, mirroring the
   producers-to-be: int literals over int-shaped bases (vars, projections, binder
   applications — some of which unfold, so weakening meets reduction), schema tags over
   variant-shaped ones. Overlapping and nested exclusion sets are drawn so the subset
   order is exercised, and the shared var pool correlates refinements with the values
   they appear beside — including as operands of the stuck operators and as match
   scrutinees below, where the consumers ([matches], [Bool.eq]) fire. *)
let int_refined_gen =
  let open Quickcheck.Generator in
  let open Quickcheck.Generator.Let_syntax in
  let int_excluded n : Pattern.Excluded.t = Literal (Int n) in
  let%map base =
    weighted_union
      [ 3., int_var_gen
      ; ( 1.
        , let%map tuple = tuple_var_gen
          and index = of_list [ 0; 1 ] in
          Value.proj tuple index )
      ; ( 1.
        , let%map fn = of_list binder_pool
          and arg = int_var_gen in
          Value.apply ~fn ~arg )
      ]
  and excluded =
    (* Includes non-nested overlapping sets ([0;1] vs [0;2]): their intersection is
       nonempty while neither side covers the other, which is the only regime where the
       join candidate must carry facts. *)
    of_list
      [ excl [ int_excluded 0L ]
      ; excl [ int_excluded 1L ]
      ; excl [ int_excluded 0L; int_excluded 1L ]
      ; excl [ int_excluded 0L; int_excluded 2L ]
      ]
  in
  Value.refine base ~excluded
;;

let tag_refined_gen =
  let open Quickcheck.Generator.Let_syntax in
  let%map base = variant_var_gen
  and excluded =
    Quickcheck.Generator.of_list
      [ excl [ Pattern.Excluded.Constructor { label = label_a; payload = None } ]
      ; excl [ Pattern.Excluded.Constructor { label = label_b; payload = None } ]
      ]
  in
  Value.refine base ~excluded
;;

let refine_value_gen = Quickcheck.Generator.union [ int_refined_gen; tag_refined_gen ]

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
  let operand = union [ int_var_gen; int_value_gen; int_refined_gen ] in
  level (union [ operand; level operand ])
;;

let stuck_bool_gen =
  let open Quickcheck.Generator in
  let open Quickcheck.Generator.Let_syntax in
  let int_operand = union [ int_var_gen; int_value_gen; stuck_int_gen; int_refined_gen ] in
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
         Bool.neq a b)
      ; (let%map a = int_operand
         and b = int_operand in
         Bool.lt a b)
      ]
  in
  let bool_operand = union [ bool_var_gen; bool_literal; comparison ] in
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
      ; ty_var_gen
      ]
  in
  let mode = of_list modes_pool in
  let component =
    union
      [ base
      ; (let%map cond = bool_var_gen
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
  let%map ty = Quickcheck.Generator.union [ ty_var_gen; ty_value_gen ] in
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
      [ (let%map scrutinee =
           weighted_union [ 3., variant_var_gen; 1., constructor_value_gen; 1., tag_refined_gen ]
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
      ; (let%map scrutinee =
           weighted_union
             [ 3., int_var_gen; 1., int_value_gen; 1., stuck_int_gen; 1., int_refined_gen ]
         and arms = template [ int_pattern_gen; wildcard_gen ] in
         scrutinee, arms)
      ; (let%map scrutinee =
           weighted_union
             [ 3., tuple_var_gen
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
      ; union [ int_var_gen; bool_var_gen; variant_var_gen; tuple_var_gen; fn_var_gen; ty_var_gen ]
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
  let cond_gen = weighted_union [ 3., bool_var_gen; 1., stuck_bool_gen ] in
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
    ; refine_value_gen
      (* Refinements over stuck-match bases: [rebind_scrutinee] builds these for any
         scrutinee whose static is a stuck conditional, and they exercise the
         stuck-match fallbacks in the [Refine] lattice rules. A collapsed match drops
         the facts at construction. *)
    ; union
        [ (let%map m = match_gen constructor_value_gen
           and excluded =
             of_list
               [ excl [ Pattern.Excluded.Constructor { label = label_a; payload = None } ]
               ; excl [ Pattern.Excluded.Constructor { label = label_b; payload = None } ]
               ]
           in
           Value.refine m ~excluded)
        ; (let%map m = match_gen int_value_gen
           and excluded =
             of_list [ excl [ (Literal (Int 0L) : Pattern.Excluded.t) ]; excl [ Literal (Int 1L) ] ]
           in
           Value.refine m ~excluded)
        ]
    ; union [ stuck_int_gen; stuck_bool_gen; ty_value_gen ]
      (* Opaque functions take any argument; the binder pool and injections are
         int-domained, so their arguments stay ints. *)
    ; (let%map fn = union [ fn_var_gen; if_ fn_var_gen ]
       and arg = self in
       Value.apply ~fn ~arg)
    ; (let%map fn = union [ of_list binder_pool; inject_a_gen ]
       and arg = union [ int_var_gen; int_value_gen; stuck_int_gen; int_refined_gen ] in
       Value.apply ~fn ~arg)
    ; (let%map tuple = union [ tuple_var_gen; if_ pair ]
       and index = of_list [ 0; 1 ] in
       Value.proj tuple index)
    ; (let%map variant =
         union [ variant_var_gen; if_ constructor_value_gen; match_gen constructor_value_gen ]
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
    ~trials:100_000
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
      let embed v = Value.if_ ~loc ~cond:bool_vars.(0) ~then_:v ~else_:v in
      require (leq (embed a) a) "a conditional with equal arms is not leq its leaf";
      require (leq a (embed a)) "a leaf is not leq the conditional embedding it";
      check_coherence state (embed a) a;
      check_coherence state a (embed a);
      check_coherence state (embed b) b;
      check_coherence state b (embed b);
      let pair x y = Value.tuple (Nonempty_list.create x [ y ]) in
      let inside = pair (Value.if_ ~loc ~cond:bool_vars.(0) ~then_:a ~else_:b) a in
      let outside = Value.if_ ~loc ~cond:bool_vars.(0) ~then_:(pair a a) ~else_:(pair b a) in
      let require cond message =
        if not cond then raise_s [%message message (inside : Value.t) (outside : Value.t)]
      in
      require (leq inside outside) "tuple formation does not commute into a conditional";
      require (leq outside inside) "tuple formation does not commute out of a conditional";
      check_coherence state inside outside)
;;

(* A correlated pair the generator cannot draw: a wildcard-arm match nested, under its
   own default arm, inside another match on the same scrutinee. [refine] orders the pair
   ([a ≤ b] holds arm-by-arm), so join must answer through leq's ordering
   ([join_stuck_match]'s fast path) before the arm-wise route can emit a disagreement
   conditional whose wildcard arm re-selects only through the negation judgment. *)
let%test_unit "wildcard-arm conditionals stay coherent" =
  let state = Typecheck.For_testing.create_state () in
  let pat_a : Dst.Expr.pattern = Constructor { label = label_a; payload = None; loc } in
  let wildcard stamp : Dst.Expr.pattern = Var { id = Ident.create Ident.Raw.anon ~stamp; loc } in
  let match_a ~on_a ~otherwise ~stamp =
    Value.match_
      ~scrutinee:variant_vars.(0)
      ~arms:(Nonempty_list.create (pat_a, on_a) [ wildcard stamp, otherwise ])
  in
  let int_ n = Value.of_literal (Int n) in
  let b = match_a ~on_a:(int_ 0L) ~otherwise:(int_ 1L) ~stamp:10 in
  let a = match_a ~on_a:(int_ 0L) ~otherwise:b ~stamp:11 in
  check_coherence state a b;
  check_coherence state b a
;;

(* The weakening rule, pinned directly: forgetting facts goes up, gaining them never
   holds, and the exclusion-set order is the subset order over a shared base — with
   join intersecting the sets and meet unioning them. *)
let%test_unit "refine weakening orders exclusion sets" =
  let state = Typecheck.For_testing.create_state () in
  let leq = Typecheck.For_testing.leq_value state in
  let show value = [%sexp (value : Value.t option)] in
  let base = int_vars.(0) in
  let refined excluded =
    Value.refine
      base
      ~excluded:(excl (List.map excluded ~f:(fun n : Pattern.Excluded.t -> Literal (Int n))))
  in
  let refined_0 = refined [ 0L ]
  and refined_1 = refined [ 1L ]
  and refined_01 = refined [ 0L; 1L ] in
  let require cond message =
    if not cond then raise_s [%message message (refined_0 : Value.t) (refined_01 : Value.t)]
  in
  require (leq refined_0 base) "forgetting all facts is not leq";
  require (leq refined_01 refined_0) "forgetting some facts is not leq";
  require (not (leq base refined_0)) "leq invents facts";
  require (not (leq refined_0 refined_01)) "leq invents additional facts";
  require (not (leq refined_0 refined_1)) "leq relates disjoint fact sets";
  [%test_eq: Sexp.t]
    (show (Typecheck.For_testing.join_value state refined_0 refined_1))
    (show (Some base));
  [%test_eq: Sexp.t]
    (show (Typecheck.For_testing.meet_value state refined_0 refined_1))
    (show (Some refined_01));
  check_coherence state refined_0 base;
  check_coherence state refined_01 refined_0;
  check_coherence state refined_0 refined_1
;;

(* A refinement over a defined-leaf conditional: the arm decomposition rewrites both
   sides under each arm's fact, and [Value.refine] collapses the leaf that violates the
   exclusion to [Bottom] rather than forget it. The pair is therefore strictly ordered —
   the refined side lacks the excluded-leaf world the bare side keeps — so join returns
   the bare side and meet the refined side in either orientation, and no lattice result
   assumes the exclusion in the bare side's worlds. *)
let%test_unit "refined conditionals do not launder exclusions" =
  let state = Typecheck.For_testing.create_state () in
  let leq = Typecheck.For_testing.leq_value state in
  let show value = [%sexp (value : Value.t option)] in
  let int_ n = Value.of_literal (Int n) in
  let m = Value.if_ ~loc ~cond:bool_vars.(0) ~then_:(int_ 0L) ~else_:(int_ 1L) in
  let r = Value.refine m ~excluded:(excl [ Literal (Int 0L) ]) in
  let require cond message =
    if not cond then raise_s [%message message (m : Value.t) (r : Value.t)]
  in
  require (leq r m) "forgetting the exclusion is not leq";
  require (not (leq m r)) "the excluded-leaf world laundered through the decomposition";
  List.iter
    [ m, r; r, m ]
    ~f:(fun (a, b) ->
      [%test_eq: Sexp.t] (show (Typecheck.For_testing.join_value state a b)) (show (Some m));
      [%test_eq: Sexp.t] (show (Typecheck.For_testing.meet_value state a b)) (show (Some r));
      check_coherence state a b)
;;

(* Refutation soundness oracle for the consumers: any verdict reached through a
   [Refine] must agree with the verdict at every resolution of its base that respects
   the exclusions — [matches] refutations may not have consistent witnesses,
   and decided [Bool.eq]/[neq] comparisons must equal their concrete counterparts.
   Refinement must also never lose a decision the bare base already had. *)
let%test_unit "refined verdicts agree with every consistent resolution" =
  let open Quickcheck.Generator in
  let open Quickcheck.Generator.Let_syntax in
  let or_of gen =
    let%map left = gen
    and right = gen in
    (Or { left; right; loc } : Dst.Expr.pattern)
  in
  let full gen = union [ gen; wildcard_gen; or_of gen ] in
  let int_case =
    let%map excluded = of_list [ [ 0L ]; [ 1L ]; [ 0L; 1L ] ]
    and pattern = full int_pattern_gen in
    let consistent =
      List.filter [ 0L; 1L; 2L ] ~f:(fun n -> not (List.mem excluded n ~equal:Int64.equal))
      |> List.map ~f:(fun n -> Value.of_literal (Int n))
    in
    ( int_vars.(0)
    , excl (List.map excluded ~f:(fun n : Pattern.Excluded.t -> Literal (Int n)))
    , consistent
    , pattern )
  in
  let variant_case =
    let%map excluded_label = of_list [ label_a; label_b ]
    and pattern = full (union [ pat_a (full int_pattern_gen); pat_b; pat_a_or_b ]) in
    let consistent =
      if Ident.Label.equal excluded_label label_a
      then [ Value.constructor ~label:label_b ~payload:None ]
      else
        List.map [ 0L; 1L ] ~f:(fun n ->
          Value.constructor ~label:label_a ~payload:(Some (Value.of_literal (Int n))))
    in
    ( variant_vars.(0)
    , excl [ Pattern.Excluded.Constructor { label = excluded_label; payload = None } ]
    , consistent
    , pattern )
  in
  Quickcheck.test
    (union [ int_case; variant_case ])
    ~sexp_of:[%sexp_of: Value.t * Set.M(Pattern.Excluded).t * Value.t list * Dst.Expr.pattern]
    ~f:(fun (base, excluded, consistent, pattern) ->
      let refined = Value.refine base ~excluded in
      let matches value =
        match Pattern.matches value pattern with
        | Match _ -> Some true
        | No_match -> Some false
        | Unknown -> None
      in
      let require cond message =
        if not cond
        then
          raise_s
            [%message
              message (refined : Value.t) (pattern : Dst.Expr.pattern) (consistent : Value.t list)]
      in
      (match matches refined with
       | Some claimed ->
         List.iter consistent ~f:(fun value ->
           match matches value with
           | Some got ->
             require (Core.Bool.equal got claimed) "refined match verdict has a refutation"
           | None -> require false "matches is undecided on a concrete value")
       | None -> ());
      (match matches base, matches refined with
       | Some _, None -> require false "refinement lost a decided match verdict"
       | (Some _ | None), _ -> ());
      (* The comparison consumers, on the int-shaped cases. *)
      List.iter consistent ~f:(fun value ->
        match value with
        | Int _ ->
          List.iter [ 0L; 1L; 2L ] ~f:(fun n ->
            let literal = Value.of_literal (Int n) in
            List.iter
              [ Bool.eq refined literal, Bool.eq value literal
              ; Bool.eq literal refined, Bool.eq literal value
              ; Bool.neq refined literal, Bool.neq value literal
              ; Bool.neq literal refined, Bool.neq literal value
              ]
              ~f:(fun (refined_verdict, concrete_verdict) ->
                match refined_verdict, concrete_verdict with
                | Bool (T claimed), Bool (T got) ->
                  require (Core.Bool.equal claimed got) "refined comparison has a refutation"
                | Bool (T _), _ -> require false "concrete comparison did not decide"
                | _, _ -> ()))
        | _ -> ()))
;;

(* Refutation at construction and the judgments must stay coherent: a stuck match over
   a refined scrutinee sits below its unrefined counterpart, and where refutation
   leaves a sole survivor the match collapses to it outright. *)
let%test_unit "refuted arms cohere across refinement" =
  let state = Typecheck.For_testing.create_state () in
  let int_ n = Value.of_literal (Int n) in
  let zero_pat : Dst.Expr.pattern = Literal { value = Int 0L; loc } in
  let one_pat : Dst.Expr.pattern = Literal { value = Int 1L; loc } in
  let wild stamp : Dst.Expr.pattern = Var { id = Ident.create Ident.Raw.anon ~stamp; loc } in
  let arms : _ Nonempty_list.t = [ zero_pat, int_ 10L; wild 10, int_ 20L ] in
  let unrefined = Value.match_ ~scrutinee:int_vars.(0) ~arms in
  let collapsed =
    Value.match_ ~scrutinee:(Value.refine int_vars.(0) ~excluded:(excl [ Literal (Int 0L) ])) ~arms
  in
  [%test_eq: Sexp.t] [%sexp (collapsed : Value.t)] [%sexp (int_ 20L : Value.t)];
  let stuck =
    Value.match_ ~scrutinee:(Value.refine int_vars.(0) ~excluded:(excl [ Literal (Int 1L) ])) ~arms
  in
  let leq = Typecheck.For_testing.leq_value state in
  if not (leq stuck unrefined)
  then raise_s [%message "refined match is not leq its unrefined counterpart"];
  check_coherence state stuck unrefined;
  check_coherence state unrefined stuck;
  (* A dead arm behind an undecided one: the match stays stuck with its arms intact,
     and still coheres with the unrefined original and its own embedding. *)
  let arms3 = [ zero_pat, int_ 10L; one_pat, int_ 11L; wild 11, int_ 20L ] in
  let unrefined3 = Value.match_ ~scrutinee:int_vars.(0) ~arms:(Nonempty_list.of_list_exn arms3) in
  let stuck3 =
    Value.match_
      ~scrutinee:(Value.refine int_vars.(0) ~excluded:(excl [ Literal (Int 1L) ]))
      ~arms:(Nonempty_list.of_list_exn arms3)
  in
  check_coherence state stuck3 unrefined3;
  check_coherence state stuck3 (int_ 20L)
;;

(* A conditional that collapses at construction (here: a dead else-arm leaves a sole
   survivor) must return its arm's *unspecialized* leaf. The specialized leaf would bake
   the arm's fact ([c = true]) into the result while sibling values beside the collapse
   were never rewritten with it, breaking the correlation laws the coherence suite
   checks — this is the minimal witness. *)
let%test_unit "a collapsed conditional does not leak its arm's facts" =
  let state = Typecheck.For_testing.create_state () in
  let leq = Typecheck.For_testing.leq_value state in
  let c = bool_vars.(0) in
  let int_ n = Value.of_literal (Int n) in
  let a = Value.if_ ~loc ~cond:c ~then_:(int_ 1L) ~else_:(int_ 2L) in
  let collapsed = Value.if_ ~loc ~cond:c ~then_:a ~else_:Value.bottom in
  [%test_eq: Sexp.t] [%sexp (collapsed : Value.t)] [%sexp (a : Value.t)];
  let pair x y = Value.tuple (Nonempty_list.create x [ y ]) in
  let inside = pair collapsed a in
  let outside = Value.if_ ~loc ~cond:c ~then_:(pair a a) ~else_:Value.bottom in
  let require cond message =
    if not cond then raise_s [%message message (inside : Value.t) (outside : Value.t)]
  in
  require (leq inside outside) "tuple formation does not commute into the collapse";
  require (leq outside inside) "tuple formation does not commute out of the collapse";
  check_coherence state inside outside
;;

(* Regression, found by the referee: a stuck match that is order-equal to a concrete
   literal (all arms [0], undecidable scrutinee), refined, against a refined binder
   application unfolding to the same literal. Construction now decides exclusions
   through the unanimous arms — excluding [0] contradicts the match outright, other
   literals discharge — while facts on the application base (opaque until unfolding)
   stay attached, so only the application side still wraps. The base meet/join may
   surface that base in its concrete form, discharging the facts — the lattice ops
   prefer an ordered base pair's original (wrappable) base as the representative. *)
let%test_unit "order-equal concrete bases keep refinement coherent" =
  let state = Typecheck.For_testing.create_state () in
  let tuple_pat : Dst.Expr.pattern =
    Tuple
      { elts =
          [ Literal { value = Int 1L; loc }
          ; Var { id = Ident.create Ident.Raw.anon ~stamp:90; loc }
          ]
      ; loc
      }
  in
  let wild : Dst.Expr.pattern = Var { id = Ident.create Ident.Raw.anon ~stamp:91; loc } in
  let zero = Value.of_literal (Int 0L) in
  let stuck_zero = Value.match_ ~scrutinee:tuple_vars.(0) ~arms:[ tuple_pat, zero; wild, zero ] in
  let unfolds_to_zero = Value.apply ~fn:(List.nth_exn binder_pool 1) ~arg:int_vars.(0) in
  let refined base excluded =
    Value.refine
      base
      ~excluded:(excl (List.map excluded ~f:(fun n : Pattern.Excluded.t -> Literal (Int n))))
  in
  [%test_eq: Sexp.t] [%sexp (refined stuck_zero [ 0L ] : Value.t)] [%sexp (Value.bottom : Value.t)];
  [%test_eq: Sexp.t] [%sexp (refined stuck_zero [ 1L ] : Value.t)] [%sexp (stuck_zero : Value.t)];
  check_coherence state (refined stuck_zero [ 0L ]) (refined unfolds_to_zero [ 1L ]);
  check_coherence state (refined unfolds_to_zero [ 1L ]) (refined stuck_zero [ 0L ]);
  check_coherence state stuck_zero (refined unfolds_to_zero [ 1L ]);
  check_coherence state (refined unfolds_to_zero [ 1L ]) stuck_zero;
  (* The join analogue needs non-nested overlapping sets: the pair is incomparable
     (neither exclusion set covers the other) while the intersection is nonempty, so
     the join candidate must carry it. *)
  check_coherence state (refined stuck_zero [ 0L; 1L ]) (refined unfolds_to_zero [ 0L; 2L ]);
  check_coherence state (refined unfolds_to_zero [ 0L; 2L ]) (refined stuck_zero [ 0L; 1L ]);
  check_coherence state (refined unfolds_to_zero [ 0L; 1L ]) (refined unfolds_to_zero [ 0L; 2L ]);
  check_coherence state (refined unfolds_to_zero [ 0L; 2L ]) (refined unfolds_to_zero [ 0L; 1L ])
;;

(* Regression: a refinement's facts must be definitely *absent* from the left side for
   leq to hold — a refactor inverted the check to definitely *present*, accepting exactly
   the excluded value. The base is a binder application, so construction cannot decide
   the exclusion (opaque until unfolding): [const_zero x ≠ 1] admits 0 through
   unfolding, while [const_zero x ≠ 0] is contradictory and admits nothing. *)
let%test_unit "exclusions on the right reject the excluded value" =
  let state = Typecheck.For_testing.create_state () in
  let leq = Typecheck.For_testing.leq_value state in
  let unfolds_to_zero = Value.apply ~fn:(List.nth_exn binder_pool 1) ~arg:int_vars.(0) in
  let refined excluded =
    Value.refine
      unfolds_to_zero
      ~excluded:(excl (List.map excluded ~f:(fun n : Pattern.Excluded.t -> Literal (Int n))))
  in
  let zero = Value.of_literal (Int 0L) in
  let require cond message =
    if not cond then raise_s [%message message (unfolds_to_zero : Value.t)]
  in
  require (leq zero (refined [ 1L ])) "a consistent refinement rejects its own base value";
  require (not (leq zero (refined [ 0L ]))) "a refinement admits the value it excludes";
  check_coherence state zero (refined [ 1L ]);
  check_coherence state zero (refined [ 0L ])
;;

(* The eager concrete lattice ([Dependent.join]/[meet] on [T] pairs) must agree with
   the value lattice that evaluates the deferred symbolic node: Bottom is join's
   identity and meet's absorbing element, and Refine follows the shared-facts /
   imposed-facts policy. A concrete [None] only defers, so divergence there is mere
   incompleteness — but an eager merge that inverted Bottom or dropped facts would
   stand uncorrected. *)
let%test_unit "the concrete dependent lattice agrees with the value lattice" =
  let state = Typecheck.For_testing.create_state () in
  let show value = [%sexp (value : Value.t option)] in
  let eager f a b : Value.t option =
    match (f (Dependent.mono a) (Dependent.mono b) : Dependent.t) with
    | T { ty; _ } -> Some ty
    | Meet _ | Join _ | Reduce _ | Typecheck _ -> None
  in
  let x = int_vars.(0) in
  let refined excluded =
    Value.refine
      x
      ~excluded:(excl (List.map excluded ~f:(fun n : Pattern.Excluded.t -> Literal (Int n))))
  in
  let r0 = refined [ 0L ]
  and r1 = refined [ 1L ]
  and r01 = refined [ 0L; 1L ] in
  List.iter
    [ Value.bottom, x; x, Value.bottom; r0, r1; r0, r01; r0, x; x, r0 ]
    ~f:(fun (a, b) ->
      [%test_eq: Sexp.t]
        (show (eager Dependent.join a b))
        (show (Typecheck.For_testing.join_value state a b));
      [%test_eq: Sexp.t]
        (show (eager Dependent.meet a b))
        (show (Typecheck.For_testing.meet_value state a b)))
;;

(* [implies] is the soundness core of [refine]: a wrong [Some true] silently
   selects the wrong arm. Oracle: on a concrete value of the patterns' shared type,
   [matches] is decided, and any implication or exclusion [implies]
   claims must agree with it. *)
let%test_unit "implies agrees with matches on concrete values" =
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
        match Pattern.matches value pattern with
        | Match _ -> true
        | No_match -> false
        | Unknown ->
          raise_s
            [%message
              "matches is undecided on a concrete value"
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
        match Pattern.implies fact pattern with
        | Known true -> require (matches pattern) "claimed implication has a concrete refutation"
        | Known false -> require (not (matches pattern)) "claimed exclusion has a concrete witness"
        | Unknown -> ()))
;;

(* [collapse_arms] rebuilds undecided per-arm results into a match unconditionally,
   which is justified by every constructible pattern implying itself — pin that,
   including through nested or-patterns, where the verdict threads through both
   branches of the fact side. *)
let%test_unit "self-implication always decides" =
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
  let variant_pattern = union [ pat_a (full int_pattern_gen); pat_b; pat_a_or_b ] in
  let tuple_pattern =
    let%map fst = full int_pattern_gen
    and snd = full variant_pattern in
    (Tuple { elts = Nonempty_list.create fst [ snd ]; loc } : Dst.Expr.pattern)
  in
  let pattern_gen =
    union
      (or_of (or_of (full int_pattern_gen))
       :: List.map ~f:full [ int_pattern_gen; bool_pattern; variant_pattern; tuple_pattern ])
  in
  Quickcheck.test pattern_gen ~sexp_of:[%sexp_of: Dst.Expr.pattern] ~f:(fun pattern ->
    match Pattern.implies pattern pattern with
    | Known true -> ()
    | verdict ->
      raise_s
        [%message
          "a pattern does not imply itself"
            (pattern : Dst.Expr.pattern)
            (verdict : bool Or_unknown.t)])
;;

(* The comparison rules decide syntactic equality with [identical]: on compound
   operands it is the only rule that can decide [Bool.eq], so the two must agree in
   both directions — [eq] folds exactly the identical pairs. *)
let%test_unit "reduce's syntactic comparison equality is identical" =
  let compound : Value.t -> bool = function
    | Int (Add _ | Sub _ | Mul _ | Div _ | Mod _ | Neg _) -> true
    | _ -> false
  in
  Quickcheck.test
    (Quickcheck.Generator.tuple2 stuck_int_gen stuck_int_gen)
    ~sexp_of:[%sexp_of: Value.t * Value.t]
    ~f:(fun (a, b) ->
      if compound a && compound b
      then (
        match Bool.eq a b, Value.identical a b with
        | Bool (T true), false ->
          raise_s [%message "eq decided a pair identical does not" (a : Value.t) (b : Value.t)]
        | (Bool (Eq _) | Bool (T false)), true ->
          raise_s [%message "eq left an identical pair stuck" (a : Value.t) (b : Value.t)]
        | _ -> ()));
  (* Separately-built refined operands compare identical, so [eq] decides them. *)
  let one = Value.of_literal (Int 1L) in
  let refined () =
    Value.refine int_vars.(0) ~excluded:(excl [ (Literal (Int 0L) : Pattern.Excluded.t) ])
  in
  let a = Int.add (refined ()) one
  and b = Int.add (refined ()) one in
  match Bool.eq a b with
  | Bool (T true) -> ()
  | verdict ->
    raise_s [%message "eq does not decide structurally equal refinements" (verdict : Value.t)]
;;

(* Distributing a stuck eliminator frame into a match's arms is claimed to be a semantic
   equivalence, not an approximation (notes/stuck-values.md): the framed and distributed
   forms must be mutually leq and cohere as a pair. This exercises
   admission/reducibility parity directly — the judgment only relates the two forms if
   its dispatch admits the shape [try_unfold] can reduce — and would have caught the
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
      ; (let%map m =
           match_gen
             (Quickcheck.Generator.union [ fn_var_gen; Quickcheck.Generator.of_list binder_pool ])
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

(* The lifted arm rules assume the full first-match fact ⟨p_i, ¬p_1..p_{i-1}⟩: in the
   default arm's world, ¬[.a] kills the nested copy's [.a] arm outright — its leaf (77)
   agrees with nothing and never blocks — and exhaustiveness selects the survivor. Under
   the lone positive fact this pair was unrelatable in either direction. *)
let%test_unit "refuted arms are dead in nested decompositions" =
  let state = Typecheck.For_testing.create_state () in
  let leq = Typecheck.For_testing.leq_value state in
  let pat_a : Dst.Expr.pattern = Constructor { label = label_a; payload = None; loc } in
  let wildcard stamp : Dst.Expr.pattern = Var { id = Ident.create Ident.Raw.anon ~stamp; loc } in
  let int_ n = Value.of_literal (Int n) in
  let match_a ~on_a ~otherwise ~stamp =
    Value.match_
      ~scrutinee:variant_vars.(0)
      ~arms:(Nonempty_list.create (pat_a, on_a) [ wildcard stamp, otherwise ])
  in
  let nested =
    match_a
      ~on_a:(int_ 5L)
      ~otherwise:(match_a ~on_a:(int_ 77L) ~otherwise:(int_ 9L) ~stamp:12)
      ~stamp:13
  in
  let flat = match_a ~on_a:(int_ 5L) ~otherwise:(int_ 9L) ~stamp:14 in
  let require cond message =
    if not cond then raise_s [%message message (nested : Value.t) (flat : Value.t)]
  in
  require (leq nested flat) "the dead arm blocks the nested decomposition";
  require (leq flat nested) "the dead arm blocks the reverse decomposition";
  check_coherence state nested flat;
  check_coherence state flat nested
;;

(* The judgment-time twin of leaf specialization: comparing a conditional against a
   plain value substitutes each arm's implied value for the scrutinee inside the plain
   side, so the eta law holds — a match returning its own scrutinee's arm-implied
   values is the scrutinee. *)
let%test_unit "conditionals over their own scrutinee satisfy eta" =
  let state = Typecheck.For_testing.create_state () in
  let leq = Typecheck.For_testing.leq_value state in
  let x = int_vars.(0) in
  let zero_pat : Dst.Expr.pattern = Literal { value = Int 0L; loc } in
  let wild : Dst.Expr.pattern = Var { id = Ident.create Ident.Raw.anon ~stamp:80; loc } in
  let eta = Value.match_ ~scrutinee:x ~arms:[ zero_pat, Value.of_literal (Int 0L); wild, x ] in
  let require cond message = if not cond then raise_s [%message message (eta : Value.t)] in
  require (leq eta x) "eta expansion is not leq its scrutinee";
  require (leq x eta) "a scrutinee is not leq its eta expansion";
  check_coherence state eta x;
  check_coherence state x eta
;;

(* Pointwise soundness, made executable: a resolution assigns every pool var a concrete
   value of its type, applied through [Value.specialize] so reduction renormalizes.
   Granted symbolic verdicts must survive resolution — the resolved pair is (near-)
   concrete, so a lost verdict witnesses an unsound symbolic one, not incompleteness. *)
let%test_unit "verdicts survive resolution" =
  let open Quickcheck.Generator in
  let open Quickcheck.Generator.Let_syntax in
  let assignment_gen =
    let per pool value_gen =
      Array.to_list pool
      |> List.map ~f:(fun var -> Quickcheck.Generator.map value_gen ~f:(fun value -> var, value))
      |> all
    in
    let int_value = map (of_list [ 0L; 1L; 2L ]) ~f:(fun n -> Value.of_literal (Int n)) in
    let bool_value = map (of_list [ true; false ]) ~f:(fun b -> Value.of_literal (Bool b)) in
    let tuple_value =
      let%map a = int_value
      and b = int_value in
      Value.tuple (Nonempty_list.create a [ b ])
    in
    let ty_value = map (of_list [ Ty.Int; Ty.Bool; Ty.Unit ]) ~f:Value.type_ in
    let%map assignments =
      all
        [ per int_vars int_value
        ; per bool_vars bool_value
        ; per variant_vars constructor_value_gen
        ; per tuple_vars tuple_value
        ; per fn_vars (of_list binder_pool)
        ; per ty_vars ty_value
        ]
    in
    List.concat assignments
  in
  let resolve assignments value =
    List.fold assignments ~init:value ~f:(fun value (var, replacement) ->
      Value.rewrite value ~target:var ~replacement)
  in
  (* Pointwise soundness quantifies over resolutions that respect the values' facts: an
     assignment putting a [Refine]'s base on an excluded shape describes a world the
     producers guarantee cannot occur, so verdicts owe it nothing. Collecting from
     match arms is over-conservative (selection would guard those facts), costing only
     trials. *)
  let rec refine_constraints (value : Value.t) =
    let list values = List.concat_map values ~f:refine_constraints in
    match value with
    | Bottom | Unit | Var _ | Closure _ | Binder _ | External _ | Prim _ -> []
    | Refine { value = base; excluded } -> (base, excluded) :: refine_constraints base
    | Bool (T _) | Int (T _) -> []
    | Bool
        ( And (a, b)
        | Or (a, b)
        | Eq (a, b)
        | Neq (a, b)
        | Lt (a, b)
        | Lte (a, b)
        | Gt (a, b)
        | Gte (a, b) ) -> list [ a; b ]
    | Bool (Not a) | Int (Neg a) -> refine_constraints a
    | Int (Add (a, b) | Sub (a, b) | Mul (a, b) | Div (a, b) | Mod (a, b)) -> list [ a; b ]
    | Type (Unit | Bool | Int | Type) -> []
    | Type (Arrow { arg_ty; ret_ty; _ }) -> list [ arg_ty; ret_ty ]
    | Type (Pi { arg_ty; _ }) -> refine_constraints arg_ty
    | Type (Tuple elts) -> list (Nonempty_list.to_list elts)
    | Type (Variant constructors) -> list (Map.data constructors |> List.filter_opt)
    | Tuple elts -> list (Nonempty_list.to_list elts)
    | Inject { ty; _ } -> refine_constraints ty
    | Constructor { payload; _ } -> list (Option.to_list payload)
    | Apply { fn; arg } -> list [ fn; arg ]
    | Proj { tuple; _ } -> refine_constraints tuple
    | Payload { variant; _ } -> refine_constraints variant
    | Match { scrutinee; arms } ->
      refine_constraints scrutinee @ list (Nonempty_list.to_list arms |> List.map ~f:snd)
  in
  (* A base that only *unfolds* to an excluded shape (a binder application, a decided
     match) still violates its facts, so consistency is judged with leq — mutual leq
     with the excluded literal decides through unfolding. This does lean on the
     machinery under test, but only to skip vacuous worlds; the obligations checked
     below are unchanged. *)
  let consistent state assignments value =
    let leq = Typecheck.For_testing.leq_value state in
    List.for_all (refine_constraints value) ~f:(fun (base, excluded) ->
      let resolved = resolve assignments base in
      Set.for_all excluded ~f:(fun excluded ->
        match (excluded : Pattern.Excluded.t) with
        | Literal literal ->
          let literal = Value.of_literal literal in
          not (leq resolved literal && leq literal resolved)
        | Constructor { label; payload = None } ->
          (match resolved with
           | Constructor { label = got; _ } -> not (Ident.Label.equal got label)
           | _ -> true)
        (* The generators only exclude whole tags. *)
        | Constructor { payload = Some _; _ } -> true))
  in
  Quickcheck.test
    ~sizes
    (tuple3 value_gen value_gen assignment_gen)
    ~sexp_of:[%sexp_of: Value.t * Value.t * (Value.t * Value.t) list]
    ~f:(fun (a, b, assignments) ->
      let state = Typecheck.For_testing.create_state () in
      let leq = Typecheck.For_testing.leq_value state in
      if consistent state assignments a && consistent state assignments b
      then (
        let resolve = resolve assignments in
        let resolved_a = resolve a
        and resolved_b = resolve b in
        let require cond message =
          if not cond
          then
            raise_s
              [%message
                message
                  (a : Value.t)
                  (b : Value.t)
                  (resolved_a : Value.t)
                  (resolved_b : Value.t)
                  (assignments : (Value.t * Value.t) list)]
        in
        if leq a b
        then require (leq resolved_a resolved_b) "a leq verdict was refuted by a resolution";
        (match Typecheck.For_testing.join_value state a b with
         | None -> ()
         | Some join ->
           let resolved_join = resolve join in
           require
             (leq resolved_a resolved_join && leq resolved_b resolved_join)
             "a join bound was refuted by a resolution");
        match Typecheck.For_testing.meet_value state a b with
        | None -> ()
        | Some meet ->
          let resolved_meet = resolve meet in
          require
            (leq resolved_meet resolved_a && leq resolved_meet resolved_b)
            "a meet bound was refuted by a resolution"))
;;

(* Normal forms are fixpoints: rebuilding a stuck match from its own scrutinee and arms
   must reproduce it — leaf specialization and selection already happened, so a second
   pass changing anything would let verdicts depend on how often a value was rebuilt. *)
let%test_unit "stuck matches rebuild to themselves" =
  Quickcheck.test ~sizes value_gen ~sexp_of:[%sexp_of: Value.t] ~f:(fun value ->
    match value with
    | Match { scrutinee; arms } ->
      let rebuilt = Value.match_ ~scrutinee ~arms in
      [%test_eq: Sexp.t] [%sexp (value : Value.t)] [%sexp (rebuilt : Value.t)]
    | _ -> ())
;;
