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
