open! Core
open! Syl

let go = Common.typecheck

let%expect_test "simple var binding" =
  go {| let _ = match true { x -> x };; |};
  [%expect {| |}]
;;

let%expect_test "wildcard exhaustive" =
  go {| let _ = match true { _ -> 0 };; |};
  [%expect {| |}]
;;

let%expect_test "exhaustive bool" =
  go
    {|
let _ =
  match true {
    true -> 0,
    false -> 1,
  }
;;
|};
  [%expect {| |}]
;;

let%expect_test "int literal with wildcard exhaustive" =
  go
    {|
let _ =
  match 0 {
    0 -> true,
    _ -> false,
  }
;;
|};
  [%expect {| |}]
;;

let%expect_test "constructor then var exhaustive" =
  go
    {|
let _ =
  match true {
    true -> 0,
    x -> 1,
  }
;;
|};
  [%expect {| |}]
;;

let%expect_test "duplicate binding in tuple" =
  go {| let _ = match (true, false) { (x, x) -> x };; |};
  [%expect
    {|
    ((loc ((line 1) (column 31)))
     (reason (Match (Multiple_bindings ((Id x) <opaque>)))))
    |}]
;;

let%expect_test "or-pattern then duplicate in tuple" =
  go {| let _ = match (true, true) { ((x | x), x) -> x };; |};
  [%expect
    {|
    ((loc ((line 1) (column 30)))
     (reason (Match (Multiple_bindings ((Id x) <opaque>)))))
    |}]
;;

let%expect_test "or-pattern different names then duplicate left" =
  go {| let _ = match (true, true) { ((x | y), x) -> x };; |};
  [%expect
    {|
    ((loc ((line 1) (column 31)))
     (reason (Match (Or_unbound (((Id x) <opaque>) ((Id y) <opaque>))))))
    |}]
;;

let%expect_test "or-pattern different names then duplicate right" =
  go {| let _ = match (true, true) { ((x | y), y) -> y };; |};
  [%expect
    {|
    ((loc ((line 1) (column 31)))
     (reason (Match (Or_unbound (((Id x) <opaque>) ((Id y) <opaque>))))))
    |}]
;;

let%expect_test "or-pattern different names under constructor" =
  go
    {|
let _ =
  match (true, 0) {
    (x, 0) | (y, 1) -> true,
    _ -> false,
  }
;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 4)))
     (reason (Match (Or_unbound (((Id x) <opaque>) ((Id y) <opaque>))))))
    |}]
;;

let%expect_test "or-pattern binds only on left" =
  go
    {|
let _ =
  match (true, 0) {
    (x, 0) | (z, 1) -> true,
    _ -> false,
  }
;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 4)))
     (reason (Match (Or_unbound (((Id x) <opaque>) ((Id z) <opaque>))))))
    |}]
;;

let%expect_test "or-pattern var vs wildcard is asymmetric" =
  go
    {|
let _ =
  match (true, 0) {
    (x, 0) | (_, 1) -> true,
    _ -> false,
  }
;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 4)))
     (reason (Match (Or_unbound (((Id x) <opaque>))))))
    |}]
;;

let%expect_test "or-pattern wildcards on both sides is allowed" =
  go
    {|
let _ =
  match (true, 0) {
    (_, 0) | (_, 1) -> true,
    _ -> false,
  }
;;
|};
  [%expect {| |}]
;;

let%expect_test "nested tuple duplicate" =
  go {| let _ = match ((true, false), true) { ((x, y), x) -> x };; |};
  [%expect
    {|
    ((loc ((line 1) (column 39)))
     (reason (Match (Multiple_bindings ((Id x) <opaque>)))))
    |}]
;;

let%expect_test "or-pattern same name both branches" =
  go {| let _ = match true { x | x -> x };; |};
  [%expect
    {|
    ((loc ((line 1) (column 9)))
     (reason
      (Match
       (Redundant ((Var (id ((Id x) <opaque>)) (loc ((line 1) (column 26)))))))))
    |}]
;;

let%expect_test "or-pattern same name no conflict with tuple" =
  go {| let _ = match (true, true) { ((x | x), y) -> y };; |};
  [%expect
    {|
    ((loc ((line 1) (column 9)))
     (reason
      (Match
       (Redundant
        ((Tuple
          (elts
           ((Var (id ((Id x) <opaque>)) (loc ((line 1) (column 36))))
            (Var (id ((Id y) <opaque>)) (loc ((line 1) (column 40))))))
          (loc ((line 1) (column 30)))))))))
    |}]
;;

let%expect_test "match on abstract type accepted" =
  go
    {|
let _ = fn (static erased t : type) -> fn (x : t) -> match x { y -> y };;
|};
  [%expect {| |}]
;;

let%expect_test "match on abstract type rejected" =
  go
    {|
let _ = fn (static erased t : type) -> fn (x : t) -> match x { 0 -> 1 };;
|};
  [%expect
    {|
    ((loc ((line 2) (column 63)))
     (reason (Type_mismatch (got (Var (Anon <opaque>))) (need (Type Int)))))
    |}]
;;

let%expect_test "match tuple pattern on abstract type rejected" =
  go
    {|
let _ = fn (static erased t : type) -> fn (x : t) -> match x { (a, b) -> a };;
|};
  [%expect
    {|
    ((loc ((line 2) (column 63)))
     (reason (Match (Expected_tuple (Var (Anon <opaque>))))))
    |}]
;;

let%expect_test "non-exhaustive bool" =
  go {| let _ = match true { true -> 0 };; |};
  [%expect
    {|
    ((loc ((line 1) (column 9)))
     (reason (Match (Non_exhaustive ((Literal (Bool false)))))))
    |}]
;;

let%expect_test "int literal non-exhaustive" =
  go {| let _ = match 0 { 0 -> true };; |};
  [%expect
    {|
    ((loc ((line 1) (column 9)))
     (reason (Match (Non_exhaustive ((Excluding ((Int 0))))))))
    |}]
;;

let%expect_test "redundant case" =
  go
    {|
let _ =
  match true {
    true -> 0,
    false -> 1,
    true -> 2,
  }
;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 2)))
     (reason
      (Match
       (Redundant ((Literal (value (Bool true)) (loc ((line 6) (column 4)))))))))
    |}]
;;

let%expect_test "wildcard after full coverage redundant" =
  go
    {|
let _ =
  match true {
    true -> 0,
    false -> 1,
    _ -> 2,
  }
;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 2)))
     (reason
      (Match
       (Redundant ((Var (id (Anon <opaque>)) (loc ((line 6) (column 4)))))))))
    |}]
;;

let%expect_test "or-pattern makes next case redundant" =
  go
    {|
let _ =
  match true {
    true | false -> 0,
    true -> 1,
  }
;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 2)))
     (reason
      (Match
       (Redundant ((Literal (value (Bool true)) (loc ((line 5) (column 4)))))))))
    |}]
;;

(* Static and erased matches *)

(* A concrete scrutinee decides the match generically, so the non-selected arms
   are dead code in the source. *)
let%expect_test "match static with a concrete scrutinee has dead arms" =
  go
    {|
let _ =
  match static 0 {
    0 -> 1,
    _ -> true,
  }
;;
|};
  [%expect
    {|
    ((loc ((line 5) (column 4)))
     (reason
      (Dead_branch
       (branch (Arm (Var (id (Anon <opaque>)) (loc ((line 5) (column 4))))))
       (value (Int (T 0))))))
    |}]
;;

let%expect_test "match static scrutinee must be static" =
  go
    {|
fun f (x : int) : int =
  match static x {
    0 -> 1,
    _ -> 2,
  }
;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 2)))
     (reason
      (Mode_mismatch (got ((staticity Parametric) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "match static scrutinee must be unerased" =
  go
    {|
let t = (1, 2) @ erased;;

let _ = match static t { (a, _) -> a };;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "match erased scrutinee may be erased" =
  go
    {|
let t = (int, bool) @ erased;;

let x = match erased t { (a, _) -> a };;

let _ = (0 : x);;
|};
  [%expect {| |}]
;;

let%expect_test "match erased bindings are ghosts" =
  go
    {|
let p = (1, 2) @ static;;

let _ = match erased p { (x, _) -> x + 1 };;
|};
  [%expect
    {|
    ((loc ((line 4) (column 37)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "match erased bindings usable at the type level" =
  go
    {|
let pick = fn (static erased p : type ^ type) -> match erased p { (t, _) -> t };;

let _ = (0 : pick ((int, bool) @ erased));;
|};
  [%expect {| |}]
;;

let%expect_test "match static non-exhaustive" =
  go {| let f = fn (static x : int) -> match static x { 0 -> 1 };; |};
  [%expect
    {|
    ((loc ((line 1) (column 32)))
     (reason (Match (Non_exhaustive ((Excluding ((Int 0))))))))
    |}]
;;

(* Exhaustiveness sees the scrutinee: cases its static refutes are not
   required, so deleting a dead arm is always legal. *)
let%expect_test "match static exhaustive over the live remainder" =
  go {| let _ = match static 0 { 0 -> 1 };; |};
  [%expect {| |}]
;;

(* The concrete component refutes the first arm outright even though the match
   stays undecided: the arm is dead in every instance. *)
let%expect_test "undecided match's refuted arm is dead" =
  go
    {|
fun f (static n : int) : int =
  match static (n, 1) {
    (0, 0) -> 0,
    (0, 1) -> 1,
    _ -> n,
  }
;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 4)))
     (reason
      (Dead_branch
       (branch
        (Arm
         (Tuple
          (elts
           ((Literal (value (Int 0)) (loc ((line 4) (column 5))))
            (Literal (value (Int 0)) (loc ((line 4) (column 8))))))
          (loc ((line 4) (column 4))))))
       (value (Tuple ((Var (Anon <opaque>)) (Int (T 1))))))))
    |}]
;;

(* Partially applying [h] refutes the [(0, y)] arm in the inner instance:
   deadness owned by the instance, not the source, so the arm is skipped — its
   body is never checked in the instance's world, where [g a 5] would not
   type. *)
let%expect_test "a partial instance skips a refuted arm" =
  go
    {|
let g = fn (static m : int) -> fn (x : if m == 0 then int else bool) -> x;;

let h =
  fn (static a : int) ->
    fn (static b : int) ->
      match static (a, b) {
        (0, y) -> g a 5,
        (x, 0) -> x,
        _ -> 7,
      }
;;

let _ = h 3 0;;
|};
  [%expect {| |}]
;;

(* The concrete component refutes the missing cases of an undecided match, and
   no runtime condition guards the pruned space. *)
let%expect_test "statically refuted missing cases are not required" =
  go
    {|
fun f (static n : int) : int =
  match static (n, 1) {
    (0, m) -> m,
    (x, 1) -> x,
  }
;;

let _ = f 0;;
let _ = f 3;;
|};
  [%expect {| |}]
;;

(* Refuting the residue witness must not excuse the missing cases hiding
   under the covered heads: they get their own witnesses. *)
let%expect_test "a refuted residue does not excuse covered-head cases" =
  go
    {|
fun f (static n : int) : static int = match static (0, n) { (0, 1) -> 7 };;
|};
  [%expect
    {|
    ((loc ((line 2) (column 38)))
     (reason
      (Match
       (Non_exhaustive ((Tuple ((Literal (Int 0)) (Excluding ((Int 1))))))))))
    |}]
;;

let%expect_test "match static redundant arm" =
  go
    {|
let _ =
  match static 0 {
    0 -> 1,
    0 -> 2,
    _ -> 3,
  }
;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 2)))
     (reason
      (Match (Redundant ((Literal (value (Int 0)) (loc ((line 5) (column 4)))))))))
    |}]
;;

let%expect_test "match static duplicate binding in selected arm" =
  go
    {|
let p = (1, 2) @ static;;

let _ = match static p { (x, x) -> x };;
|};
  [%expect
    {|
    ((loc ((line 4) (column 25)))
     (reason (Match (Multiple_bindings ((Id x) <opaque>)))))
    |}]
;;

let%expect_test "match static or-pattern binding mismatch in selected arm" =
  go
    {|
let p = (1, 0) @ static;;

let _ =
  match static p {
    (x, 0) | (0, y) -> 0,
    _ -> 1,
  }
;;
|};
  [%expect
    {|
    ((loc ((line 6) (column 4)))
     (reason (Match (Or_unbound (((Id x) <opaque>) ((Id y) <opaque>))))))
    |}]
;;

(* The dead arm's ill-typed body is never policed: the dead-arm error preempts. *)
let%expect_test "match static non-selected arms are dead" =
  go
    {|
let _ =
  match static 0 {
    0 -> 1,
    _ -> true + 1,
  }
;;
|};
  [%expect
    {|
    ((loc ((line 5) (column 4)))
     (reason
      (Dead_branch
       (branch (Arm (Var (id (Anon <opaque>)) (loc ((line 5) (column 4))))))
       (value (Int (T 0))))))
    |}]
;;

let%expect_test "match static unreachable in selected arm" =
  go
    {|
let _ =
  match static (0 @ static) {
    _ -> unreachable,
  }
;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 2)))
     (reason (Misplaced_unreachable All_paths_unreachable)))
    |}]
;;

(* A scrutinee with symbolic components can still decide the match: the second
   arm matches in every instance, so its neighbors are dead. *)
let%expect_test "match static partially known scrutinee decides the match" =
  go
    {|
fun f (static n : int) : int =
  match static (n, 0) {
    (_, 1) -> 0,
    (x, 0) -> x,
    _ -> 2,
  }
;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 4)))
     (reason
      (Dead_branch
       (branch
        (Arm
         (Tuple
          (elts
           ((Var (id (Anon <opaque>)) (loc ((line 4) (column 5))))
            (Literal (value (Int 1)) (loc ((line 4) (column 8))))))
          (loc ((line 4) (column 4))))))
       (value (Tuple ((Var (Anon <opaque>)) (Int (T 0))))))))
    |}]
;;

(* Inside the [0] arm the scrutinee is rebound to [0], so a nested match on it
   is decided: the nested wildcard arm is dead in every instance reaching the
   outer arm. *)
let%expect_test "arm refinement decides a nested match on the same scrutinee" =
  go
    {|
fun f (static n : int) : int =
  match static n {
    0 -> (match static n { 0 -> 10, _ -> unreachable }),
    _ -> n,
  }
;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 36)))
     (reason
      (Dead_branch
       (branch (Arm (Var (id (Anon <opaque>)) (loc ((line 4) (column 36))))))
       (value (Int (T 0))))))
    |}]
;;

(* In the wildcard arm the scrutinee excludes [0], so a nested match on it is
   decided the other way: the nested [0] arm is refuted. *)
let%expect_test "learned exclusions decide a nested match on the same scrutinee" =
  go
    {|
fun f (static n : int) : int =
  match static n {
    0 -> 1,
    _ -> match static n { 0 -> 99, _ -> n },
  }
;;
|};
  [%expect {| |}]
;;

(* An or-pattern predecessor excludes both alternatives, so the nested match is
   decided past both of its literal arms. *)
let%expect_test "or-pattern exclusions union into the arm's world" =
  go
    {|
fun f (static n : int) : int =
  match static n {
    0 | 1 -> n,
    _ -> match static n { 0 -> 9, 1 -> 8, _ -> n },
  }
;;
|};
  [%expect {| |}]
;;

(* Refinements flatten across nesting levels: the innermost world excludes both
   literals. *)
let%expect_test "nested wildcard refinements merge their exclusions" =
  go
    {|
fun f (static n : int) : int =
  match static n {
    0 -> 0,
    _ ->
      (match static n {
         1 -> 1,
         _ -> match static n { 0 -> 90, 1 -> 91, _ -> n },
       }),
  }
;;
|};
  [%expect {| |}]
;;

let%expect_test "match static or-pattern binds the matched alternative" =
  go
    {|
fun f (static p : int ^ int) : int =
  match static p {
    (0, y) | (y, 0) -> y,
    _ -> 99,
  }
;;

let _ = (f ((0, 7) @ static)) : int;;
|};
  [%expect {| |}]
;;

let%expect_test "static recursion through match static" =
  go
    {|
fun fact (static n : int) : int =
  match static n {
    0 -> 1,
    _ -> n * fact (n - 1),
  }
;;

let _ = fact 5;;
|};
  [%expect {| |}]
;;

let%expect_test "dynamic match on static scrutinee has a static value" =
  go
    {|
let t =
  match true {
    true -> int,
    false -> bool,
  }
;;

let _ = (0 : t);;
|};
  [%expect {| |}]
;;

let%expect_test "match erased on a self-referential static is an error" =
  go
    {|
fun f (x : int) : int =
  let e = (f 1) @ erased in
  match erased e {
    0 -> x,
    _ -> 0 - x,
  }
;;
|};
  [%expect {| ((loc ((line 3) (column 11))) (reason (Recursion_limit 1000))) |}]
;;

let%expect_test "if erased on a self-referential static is an error" =
  go {| fun f (x : int) : bool = let e = (f 1) @ erased in if erased e then x == 1 else x != 1;; |};
  [%expect {| ((loc ((line 1) (column 35))) (reason (Recursion_limit 1000))) |}]
;;

let%expect_test "match static pattern arity mismatch is a clean error" =
  go
    {|
let p = (1, 2) @ static;;

let _ =
  match static p {
    (a, b, c) -> a,
    _ -> 0,
  }
;;
|};
  [%expect
    {|
    ((loc ((line 6) (column 4)))
     (reason (Match (Expected_tuple (Type (Tuple ((Type Int) (Type Int))))))))
    |}]
;;

let%expect_test "match static on a stuck dependent scrutinee type is a clean error" =
  go
    {|
let f =
  fn (static b : bool) ->
    fn (static p : if b then int ^ int else int) ->
      match static p {
        (x, y) -> x,
        _ -> 0,
      }
;;
|};
  [%expect
    {|
    ((loc ((line 6) (column 8)))
     (reason
      (Match
       (Expected_tuple
        (Match (scrutinee (Var (Anon <opaque>)))
         (arms
          (((Literal (Bool true)) (Type (Tuple ((Type Int) (Type Int)))))
           ((Literal (Bool false)) (Type Int)))))))))
    |}]
;;

let%expect_test "or-pattern bindings take the matched alternative's static" =
  go
    {|
let v =
  match (1, 2) {
    (x, 2) | (2, x) -> x,
    _ -> 0,
  }
;;

let _ = assert erased (v == 1);;
|};
  [%expect {| |}]
;;

let%expect_test "match static or-pattern with per-alternative binding types" =
  go
    {|
let f =
  fn (static p : int ^ bool) ->
    match static p {
      (0, y) | (y, true) -> y,
      _ -> false,
    }
;;

let _ = (f ((0, true) @ static)) : bool;;
|};
  [%expect {| |}]
;;

let%expect_test "unit pattern is irrefutable on an abstract scrutinee" =
  go {| let f = fn (static u : unit) -> assert erased (match static u { () -> true });; |};
  [%expect {| |}]
;;

let%expect_test "or-pattern with unjoinable types on a parametric scrutinee" =
  go
    {|
fun d (x : int) : dynamic int =
  x + 0
;;

fun f (p : int ^ bool) : dynamic int =
  match p {
    (0, y) | (y, true) -> 0,
    _ -> 1,
  }
;;

let _ = f ((d 5, true));;
|};
  [%expect
    {|
    ((loc ((line 8) (column 4)))
     (reason (Cannot_unify (lhs (Type Bool)) (rhs (Type Int)))))
    |}]
;;

(* A dead value cannot be built into a scrutinee: the tuple component is not a
   branch tail. *)
let%expect_test "unreachable cannot be built into a scrutinee" =
  go
    {|
let f =
  fn (static b : bool) ->
    if erased b
    then 0
    else
      ((match static ((unreachable : bool), 0) {
          (true, 0) -> 1,
          _ -> true,
        })
         : int)
;;
|};
  [%expect
    {|
    ((loc ((line 7) (column 35)))
     (reason (Misplaced_unreachable Not_in_tail_position)))
    |}]
;;

(* An all-dead conditional is a spelling of [unreachable]: outside a branch
   body it is rejected at the composite. *)
let%expect_test "an all-dead match cannot be a scrutinee" =
  go
    {|
let f =
  fn (static c : bool) ->
    match static (match static c { true -> unreachable, false -> unreachable }) {
      0 -> 1,
      _ -> 2,
    }
;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 18)))
     (reason (Misplaced_unreachable All_paths_unreachable)))
    |}]
;;

(* A non-static function's body is emitted wherever the function survives, so
   a dead body has no demand point to verify at: the dead function is spelled
   [(unreachable : ty)] instead. *)
let%expect_test "a dead closure body is rejected" =
  go
    {|
fun f (static c : bool) : int =
  let h = fn (x : int) -> (match static c { true -> unreachable, false -> unreachable }) in
  match static (h 0) {
    0 -> 1,
    _ -> 2,
  }
;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 27)))
     (reason (Misplaced_unreachable All_paths_unreachable)))
    |}]
;;

(* Same-scrutinee spelling: the outer arm decides the inner match, whose
   non-selected arm is dead — the honest spelling is a bare [unreachable]. *)
let%expect_test "an all-dead nested match is dead code" =
  go
    {|
let opt = variant { a, b };;
fun f (static o : opt) : int =
  match static o {
    .a -> (match static o { .a -> unreachable, .b -> unreachable }),
    .b -> 0,
  }
;;
|};
  [%expect
    {|
    ((loc ((line 5) (column 11)))
     (reason (Misplaced_unreachable All_paths_unreachable)))
    |}]
;;

(* The argument position rejects the composite just like a bare
   [unreachable]. *)
let%expect_test "an all-dead match cannot be a function argument" =
  go
    {|
let g = fn (y : int) -> y;;

let f =
  fn (static c : bool) ->
    if erased c then 0 else g (match static c { true -> unreachable, false -> unreachable })
;;
|};
  [%expect
    {|
    ((loc ((line 6) (column 31)))
     (reason (Misplaced_unreachable All_paths_unreachable)))
    |}]
;;

(* The scrutinee position is not a branch tail, even under an irrefutable
   arm. *)
let%expect_test "unreachable cannot be a scrutinee" =
  go
    {|
let f =
  fn (static b : bool) ->
    if erased b then 0 else ((match static (unreachable : int) { _ -> true }) : int)
;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 56)))
     (reason (Misplaced_unreachable Not_in_tail_position)))
    |}]
;;

(* Code before an [unreachable] is dead with it. *)
let%expect_test "code before unreachable is dead" =
  go
    {|
let f =
  fn (static b : bool) ->
    if erased b then 0 else (let x = 1 in unreachable)
;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 42)))
     (reason (Misplaced_unreachable Not_in_head_position)))
    |}]
;;

(* Exhaustiveness soundness: a concrete value no pattern matches must be
   covered by some missing witness — otherwise refuting the witnesses could
   excuse a reachable case. *)
let%test_unit "missing witnesses cover every unmatched value" =
  let open Tst in
  let open Quickcheck.Generator in
  let open Quickcheck.Generator.Let_syntax in
  let rec covers (value : Value.t) (missing : Match.Result.Missing.t) =
    match missing, value.node with
    | Wildcard, _ -> true
    | Literal Unit, _ -> true
    | Literal (Bool want), Bool (T got) -> Core.Bool.equal want got
    | Literal (Int want), Int (T got) -> Int64.equal want got
    | Literal _, _ -> false
    | Tuple ms, Tuple vs ->
      (match List.zip ms (Nonempty_list.to_list vs) with
       | Ok zip -> List.for_all zip ~f:(fun (m, v) -> covers v m)
       | Unequal_lengths -> false)
    | Tuple _, _ -> false
    | Constructor { label; payload }, Constructor { label = got; payload = got_payload } ->
      Ident.Label.equal label got
      &&
        (match payload, got_payload with
        | None, None -> true
        | Some m, Some v -> covers v m
        | None, Some _ | Some _, None -> false)
    | Constructor _, _ -> false
    | Or ms, _ -> Nonempty_list.exists ms ~f:(covers value)
    | Excluding literals, Bool (T got) ->
      not (Nonempty_list.exists literals ~f:(Dst.Literal.equal (Bool got)))
    | Excluding literals, Int (T got) ->
      not (Nonempty_list.exists literals ~f:(Dst.Literal.equal (Int got)))
    | Excluding _, _ -> false
  in
  let loc = Lex.Location.empty in
  let ty =
    Value.type_ (Ty.Tuple (Nonempty_list.of_list_exn [ Value.type_ Bool; Value.type_ Int ]))
  in
  let wild : Dst.Expr.pattern = Var { id = Ident.create Ident.Raw.anon ~stamp:0; loc } in
  let bool_pat =
    union
      [ return wild
      ; (let%map b = bool in
         (Literal { value = Bool b; loc } : Dst.Expr.pattern))
      ]
  in
  let int_pat =
    union
      [ return wild
      ; (let%map i = Int64.gen_incl (-1L) 2L in
         (Literal { value = Int i; loc } : Dst.Expr.pattern))
      ]
  in
  let row =
    let%map b = bool_pat
    and i = int_pat in
    (Tuple { elts = Nonempty_list.of_list_exn [ b; i ]; loc } : Dst.Expr.pattern)
  in
  let pattern =
    union
      [ row
      ; return wild
      ; (let%map left = row
         and right = row in
         (Or { left; right; loc } : Dst.Expr.pattern))
      ]
  in
  let patterns =
    let%bind n = of_list [ 1; 2; 3 ] in
    list_with_length n pattern >>| Nonempty_list.of_list_exn
  in
  let concrete =
    let%map b = bool
    and i = Int64.gen_incl (-2L) 3L in
    Value.tuple (Nonempty_list.of_list_exn [ Bool.const b; Int.const i ])
  in
  Quickcheck.test
    (tuple2 patterns concrete)
    ~sexp_of:[%sexp_of: Dst.Expr.pattern Nonempty_list.t * Value.t]
    ~f:(fun (patterns, value) ->
      let compiled = Match.compile ~ty ~unfold:Fn.id patterns in
      let matched =
        Nonempty_list.exists patterns ~f:(fun pattern ->
          match Pattern.matches value (Pattern.Canon.of_pattern pattern) with
          | Match -> true
          | No_match -> false
          | Unknown -> raise_s [%message "undecided concrete match" (value : Value.t)])
      in
      if (not matched) && not (List.exists compiled.missing ~f:(covers value))
      then
        raise_s
          [%message
            "unmatched value not covered by any missing witness"
              (value : Value.t)
              (compiled.missing : Match.Result.Missing.t list)])
;;
