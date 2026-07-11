open! Core
open! Syl

let go = Common.typecheck

let%expect_test "simple var binding" =
  go {| let _ = match true with | x -> x;; |};
  [%expect {| |}]
;;

let%expect_test "wildcard exhaustive" =
  go {| let _ = match true with | _ -> 0;; |};
  [%expect {| |}]
;;

let%expect_test "exhaustive bool" =
  go {| let _ = match true with | true -> 0 | false -> 1;; |};
  [%expect {| |}]
;;

let%expect_test "int literal with wildcard exhaustive" =
  go {| let _ = match 0 with | 0 -> true | _ -> false;; |};
  [%expect {| |}]
;;

let%expect_test "constructor then var exhaustive" =
  go {| let _ = match true with | true -> 0 | x -> 1;; |};
  [%expect {| |}]
;;

let%expect_test "duplicate binding in tuple" =
  go {| let _ = match (true, false) with | (x, x) -> x;; |};
  [%expect
    {|
    ((loc ((line 1) (column 36)))
     (reason (Match (Multiple_bindings ((Id x) <opaque>)))))
    |}]
;;

let%expect_test "or-pattern then duplicate in tuple" =
  go {| let _ = match (true, true) with | ((x | x), x) -> x;; |};
  [%expect
    {|
    ((loc ((line 1) (column 35)))
     (reason (Match (Multiple_bindings ((Id x) <opaque>)))))
    |}]
;;

let%expect_test "or-pattern different names then duplicate left" =
  go {| let _ = match (true, true) with | ((x | y), x) -> x;; |};
  [%expect
    {|
    ((loc ((line 1) (column 36)))
     (reason (Match (Or_unbound (((Id x) <opaque>) ((Id y) <opaque>))))))
    |}]
;;

let%expect_test "or-pattern different names then duplicate right" =
  go {| let _ = match (true, true) with | ((x | y), y) -> y;; |};
  [%expect
    {|
    ((loc ((line 1) (column 36)))
     (reason (Match (Or_unbound (((Id x) <opaque>) ((Id y) <opaque>))))))
    |}]
;;

let%expect_test "or-pattern different names under constructor" =
  go {| let _ = match (true, 0) with | ((x, 0) | (y, 1)) -> true | _ -> false;; |};
  [%expect
    {|
    ((loc ((line 1) (column 32)))
     (reason (Match (Or_unbound (((Id x) <opaque>) ((Id y) <opaque>))))))
    |}]
;;

let%expect_test "or-pattern binds only on left" =
  go {| let _ = match (true, 0) with | ((x, 0) | (z, 1)) -> true | _ -> false;; |};
  [%expect
    {|
    ((loc ((line 1) (column 32)))
     (reason (Match (Or_unbound (((Id x) <opaque>) ((Id z) <opaque>))))))
    |}]
;;

let%expect_test "or-pattern var vs wildcard is asymmetric" =
  go {| let _ = match (true, 0) with | ((x, 0) | (_, 1)) -> true | _ -> false;; |};
  [%expect
    {|
    ((loc ((line 1) (column 32)))
     (reason (Match (Or_unbound (((Id x) <opaque>))))))
    |}]
;;

let%expect_test "or-pattern wildcards on both sides is allowed" =
  go {| let _ = match (true, 0) with | ((_, 0) | (_, 1)) -> true | _ -> false;; |};
  [%expect {| |}]
;;

let%expect_test "nested tuple duplicate" =
  go {| let _ = match ((true, false), true) with | ((x, y), x) -> x;; |};
  [%expect
    {|
    ((loc ((line 1) (column 44)))
     (reason (Match (Multiple_bindings ((Id x) <opaque>)))))
    |}]
;;

let%expect_test "or-pattern same name both branches" =
  go {| let _ = match true with | (x | x) -> x;; |};
  [%expect
    {|
    ((loc ((line 1) (column 9)))
     (reason
      (Match
       (Redundant ((Var (id ((Id x) <opaque>)) (loc ((line 1) (column 32)))))))))
    |}]
;;

let%expect_test "or-pattern same name no conflict with tuple" =
  go {| let _ = match (true, true) with | ((x | x), y) -> y;; |};
  [%expect
    {|
    ((loc ((line 1) (column 9)))
     (reason
      (Match
       (Redundant
        ((Tuple
          (elts
           ((Var (id ((Id x) <opaque>)) (loc ((line 1) (column 41))))
            (Var (id ((Id y) <opaque>)) (loc ((line 1) (column 45))))))
          (loc ((line 1) (column 35)))))))))
    |}]
;;

let%expect_test "match on abstract type accepted" =
  go
    {|
let _ = fn (static erased t : type) -> fn (x : t) -> match x with | y -> y;;
|};
  [%expect {| |}]
;;

let%expect_test "match on abstract type rejected" =
  go
    {|
let _ = fn (static erased t : type) -> fn (x : t) -> match x with | 0 -> 1;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 68)))
     (reason (Type_mismatch (got (Var (Anon <opaque>))) (need (Type Int)))))
    |}]
;;

let%expect_test "match tuple pattern on abstract type rejected" =
  go
    {|
let _ = fn (static erased t : type) -> fn (x : t) -> match x with | (a, b) -> a;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 68)))
     (reason (Match (Expected_tuple (Var (Anon <opaque>))))))
    |}]
;;

let%expect_test "non-exhaustive bool" =
  go {| let _ = match true with | true -> 0;; |};
  [%expect
    {|
    ((loc ((line 1) (column 9)))
     (reason (Match (Non_exhaustive ((Literal (Bool false)))))))
    |}]
;;

let%expect_test "int literal non-exhaustive" =
  go {| let _ = match 0 with | 0 -> true;; |};
  [%expect {| ((loc ((line 1) (column 9))) (reason (Match (Non_exhaustive (Wildcard))))) |}]
;;

let%expect_test "redundant case" =
  go {| let _ = match true with | true -> 0 | false -> 1 | true -> 2;; |};
  [%expect
    {|
    ((loc ((line 1) (column 9)))
     (reason
      (Match
       (Redundant ((Literal (value (Bool true)) (loc ((line 1) (column 52)))))))))
    |}]
;;

let%expect_test "wildcard after full coverage redundant" =
  go {| let _ = match true with | true -> 0 | false -> 1 | _ -> 2;; |};
  [%expect
    {|
    ((loc ((line 1) (column 9)))
     (reason
      (Match
       (Redundant ((Var (id (Anon <opaque>)) (loc ((line 1) (column 52)))))))))
    |}]
;;

let%expect_test "or-pattern makes next case redundant" =
  go {| let _ = match true with | (true | false) -> 0 | true -> 1;; |};
  [%expect
    {|
    ((loc ((line 1) (column 9)))
     (reason
      (Match
       (Redundant ((Literal (value (Bool true)) (loc ((line 1) (column 49)))))))))
    |}]
;;

(* Static and erased matches *)

let%expect_test "match static selects arm with concrete scrutinee" =
  go {| let _ = match static 0 with | 0 -> 1 | _ -> true;; |};
  [%expect {| |}]
;;

let%expect_test "match static scrutinee must be static" =
  go {| fun f (x : int) : int = match static x with | 0 -> 1 | _ -> 2;; |};
  [%expect
    {|
    ((loc ((line 1) (column 25)))
     (reason
      (Mode_mismatch (got ((staticity Parametric) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "match static scrutinee must be unerased" =
  go {| let t = (1, 2) @ erased;; let _ = match static t with | (a, _) -> a;; |};
  [%expect
    {|
    ((loc ((line 1) (column 35)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "match erased scrutinee may be erased" =
  go
    {| let t = (int, bool) @ erased;;
let x = match erased t with | (a, _) -> a;;
let _ = (0 : x);; |};
  [%expect {| |}]
;;

let%expect_test "match erased bindings are ghosts" =
  go {| let p = (1, 2) @ static;; let _ = match erased p with | (x, _) -> x + 1;; |};
  [%expect
    {|
    ((loc ((line 1) (column 69)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "match erased bindings usable at the type level" =
  go
    {| let pick = fn (static erased p : type ^ type) -> match erased p with | (t, _) -> t;;
let _ = (0 : pick ((int, bool) @ erased));; |};
  [%expect {| |}]
;;

let%expect_test "match static non-exhaustive" =
  go {| let _ = match static 0 with | 0 -> 1;; |};
  [%expect {| ((loc ((line 1) (column 9))) (reason (Match (Non_exhaustive (Wildcard))))) |}]
;;

let%expect_test "match static redundant arm" =
  go {| let _ = match static 0 with | 0 -> 1 | 0 -> 2 | _ -> 3;; |};
  [%expect
    {|
    ((loc ((line 1) (column 9)))
     (reason
      (Match
       (Redundant ((Literal (value (Int 0)) (loc ((line 1) (column 40)))))))))
    |}]
;;

let%expect_test "match static duplicate binding in selected arm" =
  go {| let p = (1, 2) @ static;; let _ = match static p with | (x, x) -> x;; |};
  [%expect
    {|
    ((loc ((line 1) (column 57)))
     (reason (Match (Multiple_bindings ((Id x) <opaque>)))))
    |}]
;;

let%expect_test "match static or-pattern binding mismatch in selected arm" =
  go
    {| let p = (1, 0) @ static;;
let _ = match static p with | ((x, 0) | (0, y)) -> 0 | _ -> 1;; |};
  [%expect
    {|
    ((loc ((line 2) (column 30)))
     (reason (Match (Or_unbound (((Id x) <opaque>) ((Id y) <opaque>))))))
    |}]
;;

let%expect_test "match static non-selected arms still typecheck" =
  go {| let _ = match static 0 with | 0 -> 1 | _ -> true + 1;; |};
  [%expect
    {|
    ((loc ((line 1) (column 50)))
     (reason
      (Type_mismatch (got (Type (Tuple ((Type Bool) (Type Int)))))
       (need (Type (Tuple ((Type Int) (Type Int))))))))
    |}]
;;

let%expect_test "match static unreachable in selected arm" =
  go {| let _ = match static (0 @ static) with | 0 -> unreachable | _ -> 1;; |};
  [%expect {| ((loc ((line 1) (column 47))) (reason Unreachable_reached)) |}]
;;

let%expect_test "match static or-pattern binds the matched alternative" =
  go
    {| let p = (0, 7) @ static;;
let _ = (match static p with | (0, y) | (y, 0) -> y | _ -> 99) : int;; |};
  [%expect {| |}]
;;

let%expect_test "static recursion through match static" =
  go
    {| fun fact (static n : int) : int = match static n with | 0 -> 1 | _ -> n * fact (n - 1);;
let _ = fact 5;; |};
  [%expect {| |}]
;;

let%expect_test "dynamic match on static scrutinee has a static value" =
  go
    {| let t = match true with | true -> int | false -> bool;;
let _ = (0 : t);; |};
  [%expect {| |}]
;;

let%expect_test "match erased on a self-referential static is an error" =
  go
    {| fun f (x : int) : int = let e = (f 1) @ erased in match erased e with | 0 -> x | _ -> 0 - x;; |};
  [%expect {| ((loc ((line 1) (column 51))) (reason Static_cycle)) |}]
;;

let%expect_test "if erased on a self-referential static is an error" =
  go {| fun f (x : int) : bool = let e = (f 1) @ erased in if erased e then x == 1 else x != 1;; |};
  [%expect {| ((loc ((line 1) (column 52))) (reason Static_cycle)) |}]
;;

let%expect_test "match static pattern arity mismatch is a clean error" =
  go {| let p = (1, 2) @ static;; let _ = match static p with | (a, b, c) -> a | _ -> 0;; |};
  [%expect
    {|
    ((loc ((line 1) (column 57)))
     (reason (Match (Expected_tuple (Type (Tuple ((Type Int) (Type Int))))))))
    |}]
;;

let%expect_test "match static on a stuck dependent scrutinee type is a clean error" =
  go
    {| let f = fn (static b : bool) -> fn (static p : if b then int ^ int else int) ->
  match static p with | (x, y) -> x | _ -> 0;; |};
  [%expect
    {|
    ((loc ((line 2) (column 24)))
     (reason
      (Match
       (Expected_tuple
        (Match (scrutinee (Var (Anon <opaque>)))
         (arms
          (((Literal (value (Bool true)) (loc ((line 1) (column 48))))
            (Type (Tuple ((Type Int) (Type Int)))))
           ((Literal (value (Bool false)) (loc ((line 1) (column 48))))
            (Type Int)))))))))
    |}]
;;

let%expect_test "or-pattern bindings take the matched alternative's static" =
  go
    {| let v = match (1, 2) with | (x, 2) | (2, x) -> x | _ -> 0;;
let _ = assert erased (v == 1);; |};
  [%expect {| |}]
;;

let%expect_test "match static or-pattern with per-alternative binding types" =
  go
    {| let p = (0, true) @ static;;
let _ = (match static p with | (0, y) | (y, true) -> y | _ -> false) : bool;; |};
  [%expect {| |}]
;;

let%expect_test "unit pattern is irrefutable on an abstract scrutinee" =
  go {| let f = fn (static u : unit) -> assert erased (match static u with | () -> true);; |};
  [%expect {| |}]
;;

let%expect_test "or-pattern with unjoinable types on a parametric scrutinee" =
  go
    {| fun d (x : int) : dynamic int = x + 0;;
fun f (p : int ^ bool) : dynamic int = match p with | (0, y) | (y, true) -> 0 | _ -> 1;;
let _ = f ((d 5, true));; |};
  [%expect
    {|
    ((loc ((line 2) (column 54)))
     (reason (Cannot_unify (lhs (Type Bool)) (rhs (Type Int)))))
    |}]
;;

let%expect_test "bottom scrutinee components collapse stuck matches" =
  (* The scrutinee contains an unreachable component, so it reduces to
     [Bottom] and the whole match collapses to [Bottom], which is below
     [int]. *)
  go
    {| let f = fn (static b : bool) ->
  if erased b
  then 0
  else ((match static ((unreachable : bool), 0) with | (true, 0) -> 1 | _ -> true) : int);; |};
  [%expect {| |}]
;;

let%expect_test "bottom scrutinee is not selected by an irrefutable arm" =
  (* An irrefutable pattern matches even a [Bottom] scrutinee, but selecting
     the arm would type this dead match as [bool]; it must collapse to
     [Bottom] instead. *)
  go
    {| let f = fn (static b : bool) ->
  if erased b
  then 0
  else ((match static (unreachable : int) with | _ -> true) : int);; |};
  [%expect {| |}]
;;
