open! Core
open! Syl

let go = Common.typecheck

(* Bool wildcard splitting: the wildcard arm is checked once per remaining
   literal, so a nested match on the same scrutinee is exhaustive without the
   excluded case. *)
let%expect_test "bool wildcard worlds refute nested missing cases" =
  go
    {|
fun f (static c : bool) : static int =
  match static c { true -> 0, _ -> match static c { false -> 5 } };;
let _ = assert erased (f false == 5);;
|};
  [%expect {| |}]
;;

(* A conditional resolved inside one world of a multi-world split takes its
   live branch: the other branch is dead only world-relatively, so it is
   neither reported nor checked there. The nested match resolves per world
   (concrete constructor heads), so each world's condition is a constant. *)
let%expect_test "resolved conditionals inside split worlds take their branch" =
  go
    {|
let opt = variant { none, some : int };;
fun f (static o : opt) : static int =
  match static o {
    .some 3 -> 0,
    _ -> if erased (match static o { .none -> true, _ -> false }) then 1 else 2,
  };;
|};
  [%expect {| |}]
;;

(* Refutation head-normalizes at every step: the projected component is stuck
   as spelled (a projection out of a conditional), but its whnf distributes
   and collapses to a constant that refutes the missing bool case. *)
let%expect_test "refutation unfolds projected components" =
  go
    {|
fun f (static c : bool) : static int =
  let p = if erased c then (true, 2) else (true, 3) in
  match static p { (true, x) -> x };;
|};
  [%expect {| |}]
;;

(* Selecting [unreachable] inside a speculative world is not yet an error —
   the world quantifies over a subset of instances, so the failure is not
   value-independent. The instance that actually selects it reports. *)
let%expect_test "speculative worlds defer unreachable enforcement" =
  go
    {|
let opt = variant { none, some : int };;
fun f (static o : opt) : static int =
  match static o {
    .some 3 -> 0,
    _ -> if erased (match static o { .none -> true, _ -> false }) then unreachable else 2,
  };;
let _ = assert erased (f (opt.some 5) == 2);;
|};
  [%expect {| |}]
;;

let%expect_test "the selecting instance still reports unreachable" =
  go
    {|
let opt = variant { none, some : int };;
fun f (static o : opt) : static int =
  match static o {
    .some 3 -> 0,
    _ -> if erased (match static o { .none -> true, _ -> false }) then unreachable else 2,
  };;
let _ = assert erased (f (opt.none) == 2);;
|};
  [%expect {| ((loc ((line 6) (column 71))) (reason Unreachable_reached)) |}]
;;

(* Emission enforcement: a runtime match emits every arm, so the instance
   whose world keeps a bare [unreachable] arm reports it. *)
let%expect_test "runtime match arms are emission-checked at the instance" =
  go
    {|
fun f (static b : bool) : bool -> dynamic int =
  fn (x : bool) ->
    match static b {
      true -> match x { true -> unreachable, false -> 2 },
      false -> match x { true -> 1, false -> 2 },
    };;
let _ = f true;;
|};
  [%expect {| ((loc ((line 5) (column 32))) (reason Unreachable_reached)) |}]
;;

(* The live-arm discipline: an instance checks only the arm its scrutinee
   selects, so the pruned arm's unreachable is not emitted or reported. *)
let%expect_test "pruned arms are not emission-checked" =
  go
    {|
fun f (static b : bool) : bool -> dynamic int =
  fn (x : bool) ->
    match static b {
      true -> match x { true -> unreachable, false -> 2 },
      false -> match x { true -> 1, false -> 2 },
    };;
let _ = f false;;
|};
  [%expect {| |}]
;;

(* Runtime if branches are emitted like match arms. *)
let%expect_test "runtime if branches are emission-checked at the instance" =
  go
    {|
fun h (static b : bool) : bool -> dynamic int =
  fn (x : bool) ->
    match static b {
      true -> if x then unreachable else 2,
      false -> if x then 1 else 2,
    };;
let _ = h true;;
|};
  [%expect {| ((loc ((line 5) (column 24))) (reason Unreachable_reached)) |}]
;;

(* A resolved selection skips the other arms, but their patterns are still
   validated in the abstract pass. *)
let%expect_test "a dead arm's pattern is still validated" =
  go
    {|
let _ = match static 1 { 1 -> 0, x | 0 -> x };;
|};
  [%expect
    {|
    ((loc ((line 2) (column 33)))
     (reason (Match (Or_unbound (((Id x) <opaque>))))))
    |}]
;;

(* The selection half of the unreachable contract: resolving onto an
   [unreachable] arm at a real world reports. *)
let%expect_test "the selecting instance reports an unreachable arm" =
  go
    {|
fun f (static n : int) : static int = match static n { 0 -> unreachable, _ -> 1 };;
let _ = assert erased (f 0 == 1);;
|};
  [%expect {| ((loc ((line 2) (column 60))) (reason Unreachable_reached)) |}]
;;

(* As with conditionals, a known selection inside a speculative world defers
   the unreachable claim to the instances that actually reach it. *)
let%expect_test "known selection under a speculative world defers unreachable" =
  go
    {|
let opt = variant { none, some : int };;
fun f (static o : opt) : static int =
  match static o {
    .some 3 -> 0,
    _ -> match static o { .none -> unreachable, _ -> 2 },
  };;
let _ = assert erased (f (opt.some 5) == 2);;
|};
  [%expect {| |}]
;;
