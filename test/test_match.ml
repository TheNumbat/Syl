open! Core
open! Syl

let go ?(print = false) input =
  let cst = Parse.parse_exn input in
  let dst = Desugar.desugar cst in
  match Typecheck.typecheck dst with
  | Ok tst -> if print then print_s [%message (tst : Tst.Program.t)]
  | Error { loc; reason } -> print_s [%message (loc : Lex.Location.t) (reason : Typecheck.Error.t)]
;;

let%expect_test "var" =
  go
    {|
let _ =
  match static 0 @ dynamic with
  | x -> 1
;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 2)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "var" =
  go
    {|
let _ =
  match 0 @ erased with
  | x -> 1
;;
|};
  [%expect
    {| |}]
;;

let%expect_test "var" =
  go
    {|
let x = 0 @ dynamic;;
let _ =
  match x @ erased with
  | x -> 1
;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 2)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "var" =
  go
    {|
let _ =
  match 0 with
  | x -> 1
;;
|};
  [%expect {| |}]
;;

let%expect_test "var" =
  go
    {|
let x = true;;
let _ =
  match x with
  | x -> assert static x
;;
|};
  [%expect {| |}]
;;

let%expect_test "var" =
  go
    {|
let x = true @ dynamic;;
let _ =
  match x with
  | x -> assert x
;;
|};
  [%expect {| |}]
;;

let%expect_test "var" =
  go
    {|
let x = true @ dynamic;;
let _ =
  match x with
  | x -> assert static x
;;
|};
  [%expect
    {|
    ((loc ((line 5) (column 9)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "redundant" =
  go
    {|
let _ =
  match 0 with
  | x -> 1
  | y -> 2
;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 2)))
     (reason
      (Redundant_patterns
       ((Var (id ((Id x) <opaque>)) (loc ((line 4) (column 4))))
        (Var (id ((Id y) <opaque>)) (loc ((line 5) (column 4))))))))
    |}]
;;
