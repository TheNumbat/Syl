open! Core
open! Syl
open Tst

(* Values reduce on construction; these tests pin down how the smart
   constructors propagate [Bottom]: an operation on an unreachable operand is
   itself unreachable. *)

let show value = print_s [%sexp (value : Value.t)]
let int i = Int.const (Int64.of_int i)
let excl list = Pattern.Excluded.Set.of_list list

let%expect_test "int ops propagate an unreachable operand" =
  show (Int.add Value.bottom (int 1));
  [%expect {| Bottom |}];
  show (Int.sub (int 1) Value.bottom);
  [%expect {| Bottom |}];
  show (Int.mul Value.bottom (int 0));
  [%expect {| Bottom |}];
  show (Int.neg Value.bottom);
  [%expect {| Bottom |}]
;;

let%expect_test "an unreachable division never reports" =
  show (Int.div Value.bottom (int 0));
  [%expect {| Bottom |}];
  show (Int.div (int 1) Value.bottom);
  [%expect {| Bottom |}];
  show (Int.mod_ Value.bottom (int 0));
  [%expect {| Bottom |}];
  show (Int.mod_ Value.bottom (int (-1)));
  [%expect {| Bottom |}]
;;

let%expect_test "live divisor checks still report" =
  (try show (Int.div (int 1) (int 0)) with
   | Int.Divide_by_zero i -> print_s [%sexp (i : Int.t)]);
  [%expect {| (Div (Int (T 1)) (Int (T 0))) |}];
  (try show (Int.mod_ (int 1) (int (-1))) with
   | Int.Negative_modulus i -> print_s [%sexp (i : Int.t)]);
  [%expect {| (Mod (Int (T 1)) (Int (T -1))) |}]
;;

(* [Bottom] wins over the short-circuit rewrites: the operation is dead code,
   not false/true. *)
let%expect_test "bool ops propagate an unreachable operand" =
  show (Bool.and_ Value.bottom (Bool.const false));
  [%expect {| Bottom |}];
  show (Bool.or_ (Bool.const true) Value.bottom);
  [%expect {| Bottom |}];
  show (Bool.eq Value.bottom (int 0));
  [%expect {| Bottom |}];
  show (Bool.not_ Value.bottom);
  [%expect {| Bottom |}]
;;

let%expect_test "composite values with an unreachable part are unreachable" =
  show (Value.tuple [ int 1; Value.bottom ]);
  [%expect {| Bottom |}];
  show (Value.proj Value.bottom 0);
  [%expect {| Bottom |}];
  show (Value.apply ~fn:Value.bottom ~arg:(int 1));
  [%expect {| Bottom |}];
  show (Value.apply ~fn:(int 1) ~arg:Value.bottom);
  [%expect {| Bottom |}]
;;

let%expect_test "an unreachable assertion never reports" =
  show (Builtin.eval Builtin.Prim.Assert Value.bottom);
  [%expect {| Bottom |}];
  show (Builtin.eval Builtin.Prim.Assert_erased Value.bottom);
  [%expect {| Bottom |}]
;;

(* [Value.refine] canonicalizes: exclusion sets are nonempty, sorted, deduped, and
   nested refinements flatten, so structurally equal facts are structurally equal
   values. *)
let%expect_test "refine canonicalizes its exclusion set" =
  let x = Value.var (Ident.create Ident.Raw.anon ~stamp:0) in
  let lit n : Pattern.Excluded.t = Literal (Int (Int64.of_int n)) in
  show (Value.refine x ~excluded:(excl []));
  [%expect {| (Var (Anon <opaque>)) |}];
  show (Value.refine x ~excluded:(excl [ lit 1; lit 0; lit 1 ]));
  [%expect
    {|
    (Refine (value (Var (Anon <opaque>)))
     (excluded ((Literal (Int 0)) (Literal (Int 1)))))
    |}];
  show (Value.refine (Value.refine x ~excluded:(excl [ lit 0 ])) ~excluded:(excl [ lit 1; lit 0 ]));
  [%expect
    {|
    (Refine (value (Var (Anon <opaque>)))
     (excluded ((Literal (Int 0)) (Literal (Int 1)))))
    |}]
;;

let loc = Lex.Location.empty
let anon stamp = Ident.create Ident.Raw.anon ~stamp
let wild stamp : Dst.Expr.pattern = Var { id = anon stamp; loc }
let int_pat n : Dst.Expr.pattern = Literal { value = Int (Int64.of_int n); loc }
let excluded_int n : Pattern.Excluded.t = Literal (Int (Int64.of_int n))
let label_a = Ident.Label.of_string "a"
let label_b = Ident.Label.of_string "b"

(* Refinement never forgets a fact: a decidably-present exclusion contradicts the value
   (the world constructing it is dead), a decidably-absent one is discharged, and
   undecided ones stay attached whatever the base's head. *)
let%expect_test "refine decides facts against the base" =
  show (Value.refine Value.bottom ~excluded:(excl [ excluded_int 0 ]));
  [%expect {| Bottom |}];
  show (Value.refine (int 0) ~excluded:(excl [ excluded_int 0 ]));
  [%expect {| Bottom |}];
  show (Value.refine (int 0) ~excluded:(excl [ excluded_int 1 ]));
  [%expect {| (Int (T 0)) |}];
  show (Value.refine Value.unit ~excluded:(excl [ Literal Unit ]));
  [%expect {| Bottom |}];
  let a = Value.constructor ~label:label_a ~payload:None in
  show (Value.refine a ~excluded:(excl [ Constructor { label = label_a; payload = None } ]));
  [%expect {| Bottom |}];
  show
    (Value.refine
       a
       ~excluded:(excl [ Constructor { label = label_b; payload = None }; excluded_int 7 ]));
  [%expect {| (Constructor (label a) (payload ())) |}];
  (* A symbolic operator head keeps the fact: nothing decides it. *)
  show (Value.refine (Int.add (Value.var (anon 0)) (int 1)) ~excluded:(excl [ excluded_int 5 ]));
  [%expect
    {|
    (Refine (value (Int (Add (Var (Anon <opaque>)) (Int (T 1)))))
     (excluded ((Literal (Int 5)))))
    |}];
  (* A stuck match decides through its arms: a unanimously-present exclusion collapses
     the refinement, a unanimously-absent one discharges, and a split verdict keeps
     the fact attached. *)
  let unanimous = Value.if_ ~loc ~cond:(Value.var (anon 0)) ~then_:(int 4) ~else_:(int 4) in
  show (Value.refine unanimous ~excluded:(excl [ excluded_int 4 ]));
  [%expect {| Bottom |}];
  [%test_eq: Sexp.t]
    [%sexp (Value.refine unanimous ~excluded:(excl [ excluded_int 5 ]) : Value.t)]
    [%sexp (unanimous : Value.t)];
  let split = Value.if_ ~loc ~cond:(Value.var (anon 0)) ~then_:(int 4) ~else_:(int 5) in
  (match Value.refine split ~excluded:(excl [ excluded_int 4; excluded_int 9 ]) with
   | Refine { value; excluded } ->
     [%test_eq: Sexp.t] [%sexp (value : Value.t)] [%sexp (split : Value.t)];
     print_s [%sexp (excluded : Set.M(Pattern.Excluded).t)]
   | refined -> print_s [%message "expected a kept fact" (refined : Value.t)]);
  [%expect {| ((Literal (Int 4))) |}]
;;

(* Consumers: [matches_pattern] refutes a [Refine]'s excluded shapes, so arm selection
   skips dead arms, and exhaustiveness collapses a sole survivor. *)
let%expect_test "refuted literal arms are skipped" =
  let refined = Value.refine (Value.var (anon 0)) ~excluded:(excl [ excluded_int 0 ]) in
  show (Value.match_ ~scrutinee:refined ~arms:[ int_pat 0, int 10; wild 1, int 20 ]);
  [%expect {| (Int (T 20)) |}];
  (* An unexcluded literal still stalls selection, arms intact. *)
  show (Value.match_ ~scrutinee:refined ~arms:[ int_pat 1, int 10; wild 1, int 20 ]);
  [%expect
    {|
    (Match
     (scrutinee
      (Refine (value (Var (Anon <opaque>))) (excluded ((Literal (Int 0))))))
     (arms
      (((Literal (value (Int 1)) (loc ((line 1) (column 0)))) (Int (T 10)))
       ((Var (id (Anon <opaque>)) (loc ((line 1) (column 0)))) (Int (T 20))))))
    |}];
  (* An or-pattern is dead only when every alternative is refuted. *)
  let refined_01 =
    Value.refine (Value.var (anon 0)) ~excluded:(excl [ excluded_int 0; excluded_int 1 ])
  in
  let zero_or_one : Dst.Expr.pattern = Or { left = int_pat 0; right = int_pat 1; loc } in
  show (Value.match_ ~scrutinee:refined_01 ~arms:[ zero_or_one, int 10; wild 1, int 20 ]);
  [%expect {| (Int (T 20)) |}]
;;

let%expect_test "refuted tags leave a sole surviving arm" =
  let refined =
    Value.refine
      (Value.var (anon 0))
      ~excluded:(excl [ Constructor { label = label_a; payload = None } ])
  in
  let pat_a : Dst.Expr.pattern = Constructor { label = label_a; payload = Some (wild 1); loc } in
  let pat_b : Dst.Expr.pattern = Constructor { label = label_b; payload = None; loc } in
  (* The tag exclusion kills the [.a] arm regardless of its payload pattern;
     exhaustiveness makes [.b] unconditional. *)
  show (Value.match_ ~scrutinee:refined ~arms:[ pat_a, int 10; pat_b, int 20 ]);
  [%expect {| (Int (T 20)) |}];
  (* The unexcluded tag is not thereby confirmed: a wildcard-refuted [.b] still cannot
     select [.a] positively (its payload bindings would need a real constructor). *)
  let refined_b =
    Value.refine
      (Value.var (anon 0))
      ~excluded:(excl [ Constructor { label = label_b; payload = None } ])
  in
  show (Value.match_ ~scrutinee:refined_b ~arms:[ pat_b, int 20; pat_a, int 10; wild 2, int 30 ]);
  [%expect
    {|
    (Match
     (scrutinee
      (Refine (value (Var (Anon <opaque>)))
       (excluded ((Constructor (label b) (payload ()))))))
     (arms
      (((Constructor (label b) (payload ()) (loc ((line 1) (column 0))))
        (Int (T 20)))
       ((Constructor (label a)
         (payload ((Var (id (Anon <opaque>)) (loc ((line 1) (column 0))))))
         (loc ((line 1) (column 0))))
        (Int (T 10)))
       ((Var (id (Anon <opaque>)) (loc ((line 1) (column 0)))) (Int (T 30))))))
    |}]
;;

let%expect_test "a refined condition resolves erased ifs" =
  let cond value = Value.refine (Value.var (anon 0)) ~excluded:(excl [ Literal (Bool value) ]) in
  (* Bool exclusions collapse to the remaining literal, so either polarity decides. *)
  show (Value.if_ ~loc ~cond:(cond true) ~then_:(int 1) ~else_:(int 2));
  [%expect {| (Int (T 2)) |}];
  show (Value.if_ ~loc ~cond:(cond false) ~then_:(int 1) ~else_:(int 2));
  [%expect {| (Int (T 1)) |}]
;;

(* Bool exclusions collapse to the positive remainder: the domain is known from the
   exclusion shape alone, so every construction site agrees on the one normal form.
   Variant exclusions stay negative facts verbatim — that collapse would need the
   domain's type, which only producers have, so the same fact would take two normal
   forms depending on who built it. *)
let%expect_test "only bool exclusions collapse to the positive remainder" =
  let x = Value.var (anon 0) in
  show (Value.refine x ~excluded:(excl [ Literal (Bool true) ]));
  [%expect {| (Bool (T false)) |}];
  show (Value.refine x ~excluded:(excl [ Literal (Bool false) ]));
  [%expect {| (Bool (T true)) |}];
  show (Value.refine x ~excluded:(excl [ Constructor { label = label_a; payload = None } ]));
  [%expect
    {|
    (Refine (value (Var (Anon <opaque>)))
     (excluded ((Constructor (label a) (payload ())))))
    |}]
;;

(* A refined tuple component refutes through projection. *)
let%expect_test "refutation reaches tuple components" =
  let refined = Value.refine (Value.var (anon 0)) ~excluded:(excl [ excluded_int 0 ]) in
  let scrutinee = Value.tuple [ refined; int 5 ] in
  let pair fst snd : Dst.Expr.pattern = Tuple { elts = [ fst; snd ]; loc } in
  show
    (Value.match_
       ~scrutinee
       ~arms:[ pair (int_pat 0) (wild 1), int 10; pair (wild 2) (wild 3), int 20 ]);
  [%expect {| (Int (T 20)) |}]
;;

let%expect_test "refined operands decide excluded comparisons" =
  let x = Value.var (anon 0) in
  let refined = Value.refine x ~excluded:(excl [ excluded_int 0 ]) in
  show (Bool.eq refined (int 0));
  [%expect {| (Bool (T false)) |}];
  show (Bool.eq (int 0) refined);
  [%expect {| (Bool (T false)) |}];
  show (Bool.neq refined (int 0));
  [%expect {| (Bool (T true)) |}];
  show (Bool.neq (int 0) refined);
  [%expect {| (Bool (T true)) |}];
  (* An unexcluded literal stays stuck, keeping the refinement. *)
  show (Bool.eq refined (int 1));
  [%expect
    {|
    (Bool
     (Eq (Refine (value (Var (Anon <opaque>))) (excluded ((Literal (Int 0)))))
      (Int (T 1))))
    |}]
;;

(* Identity sees through refinement: both sides denote the base's runtime value, no
   matter their exclusion sets. *)
let%expect_test "identity comparisons see through refinement" =
  let x = Value.var (anon 0) in
  let refined_0 = Value.refine x ~excluded:(excl [ excluded_int 0 ]) in
  let refined_1 = Value.refine x ~excluded:(excl [ excluded_int 1 ]) in
  show (Bool.eq refined_0 refined_1);
  [%expect {| (Bool (T true)) |}];
  show (Bool.eq refined_0 x);
  [%expect {| (Bool (T true)) |}];
  show (Bool.eq x refined_0);
  [%expect {| (Bool (T true)) |}];
  show (Bool.neq refined_0 refined_1);
  [%expect {| (Bool (T false)) |}];
  (* Distinct vars stay stuck even when both are refined. *)
  show (Bool.eq refined_0 (Value.refine (Value.var (anon 1)) ~excluded:(excl [ excluded_int 0 ])));
  [%expect
    {|
    (Bool
     (Eq (Refine (value (Var (Anon <opaque>))) (excluded ((Literal (Int 0)))))
      (Refine (value (Var (Anon <opaque>))) (excluded ((Literal (Int 0)))))))
    |}]
;;

(* A stuck match specializes each arm's leaf to its own world: occurrences of the
   scrutinee become the arm-implied value, and the consumers fire inside the leaves.
   The specialized leaves belong to the stuck form only — a collapse escapes the
   conditional's guard, so it yields the arm's original leaf. *)
let%expect_test "stuck arms specialize their leaves" =
  let x = Value.var (anon 0) in
  show (Value.match_ ~scrutinee:x ~arms:[ int_pat 0, Int.add x (int 1); wild 1, Int.add x (int 1) ]);
  [%expect
    {|
    (Match (scrutinee (Var (Anon <opaque>)))
     (arms
      (((Literal (value (Int 0)) (loc ((line 1) (column 0)))) (Int (T 1)))
       ((Var (id (Anon <opaque>)) (loc ((line 1) (column 0))))
        (Int
         (Add
          (Refine (value (Var (Anon <opaque>))) (excluded ((Literal (Int 0)))))
          (Int (T 1))))))))
    |}];
  (* The comparison consumer decides inside the wildcard arm's world. *)
  show (Value.match_ ~scrutinee:x ~arms:[ int_pat 0, Bool.const true; wild 1, Bool.eq x (int 0) ]);
  [%expect
    {|
    (Match (scrutinee (Var (Anon <opaque>)))
     (arms
      (((Literal (value (Int 0)) (loc ((line 1) (column 0)))) (Bool (T true)))
       ((Var (id (Anon <opaque>)) (loc ((line 1) (column 0)))) (Bool (T false))))))
    |}];
  (* Collapse yields the original leaf: the refuted arm dies, and the survivor escapes
     unspecialized. *)
  let refined = Value.refine x ~excluded:(excl [ excluded_int 0 ]) in
  show (Value.match_ ~scrutinee:refined ~arms:[ int_pat 0, int 9; wild 1, Int.add x (int 1) ]);
  [%expect {| (Int (Add (Var (Anon <opaque>)) (Int (T 1)))) |}]
;;
