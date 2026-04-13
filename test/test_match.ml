open! Core
open! Syl

let go input =
  let cst = Parse.parse_exn input in
  let dst = Desugar.desugar cst in
  match Typecheck.typecheck dst with
  | Ok _ -> ()
  | Error { loc; reason; _ } ->
    print_s [%message (loc : Lex.Location.t) (reason : Typecheck.Error.t)]
;;

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
    ((loc ((line 1) (column 35)))
     (reason (Match (Multiple_bindings ((Id x) <opaque>)))))
    |}]
;;

let%expect_test "or-pattern different names then duplicate right" =
  go {| let _ = match (true, true) with | ((x | y), y) -> y;; |};
  [%expect
    {|
    ((loc ((line 1) (column 35)))
     (reason (Match (Multiple_bindings ((Id y) <opaque>)))))
    |}]
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
