open! Core
open! Syl

let go ?(print = false) input =
  let cst = Parse.parse_exn input in
  let dst = Desugar.desugar cst in
  match Typecheck.typecheck dst with
  | Ok tst -> if print then print_s [%message (tst : Tst.Program.t)]
  | Error { loc; reason } -> print_s [%message (loc : Lex.Location.t) (reason : Typecheck.Error.t)]
;;

let%expect_test "tuple type basics" =
  go
    {|
let t1 = int ^ int;;
let t2 = int ^ (bool ^ unit);;
let t3 = (int ^ bool) ^ unit;;
let t4 = int ^ bool ^ unit;;
|};
  [%expect {| |}]
;;

let%expect_test "tuple type used as annotation" =
  go
    {|
let t = (1, 2) : int ^ int;;
|};
  [%expect {| |}]
;;

let%expect_test "tuple type annotation nested" =
  go
    {|
let t = (1, (true, ())) : int ^ (bool ^ unit);;
|};
  [%expect {| |}]
;;

let%expect_test "tuple type annotation mismatch" =
  go
    {|
let t = (1, 2) : int ^ bool;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 15)))
     (reason
      (Type_mismatch (got (Tuple ((Type Int) (Type Int))))
       (need (Tuple ((Type Int) (Type Bool)))))))
    |}]
;;

let%expect_test "tuple type annotation wrong arity" =
  go
    {|
let t = (1, 2, 3) : int ^ int;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 18)))
     (reason
      (Type_mismatch (got (Tuple ((Type Int) (Type Int) (Type Int))))
       (need (Tuple ((Type Int) (Type Int)))))))
    |}]
;;

let%expect_test "tuple type in function argument" =
  go
    {|
let f = fn (t : int ^ int) -> t;;
let _ = f (1, 2);;
|};
  [%expect {| |}]
;;

let%expect_test "tuple type in function argument mismatch" =
  go
    {|
let f = fn (t : int ^ bool) -> t;;
let _ = f (1, 2);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Type_mismatch (got (Tuple ((Type Int) (Type Int))))
       (need (Tuple ((Type Int) (Type Bool)))))))
    |}]
;;

let%expect_test "tuple type in arrow return" =
  go
    {|
let f = (fn (x : int) -> (x, x)) : int -> int ^ int;;
|};
  [%expect {| |}]
;;

let%expect_test "tuple type computed statically" =
  go
    {|
let T = int ^ bool;;
let t = (1, true) : T;;
|};
  [%expect {| |}]
;;

let%expect_test "tuple type with non-type element rejected" =
  go
    {|
let t = 1 ^ 2;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 8)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Type)))))
    |}]
;;

let%expect_test "tuple values" =
  go
    {|
let t1 = 1, 2;;
let t2 = 1, (true, ());;
let t3 = (1, true), ();;
let t4 = 1, true, ();;
|};
  [%expect {| |}]
;;

let%expect_test "tuple with dynamic elements" =
  go
    {|
let x = 1 @ dynamic;;
let t1 = x, 2;;
let t2 = 1, x;;
let t3 = x, x;;
|};
  [%expect {| |}]
;;

let%expect_test "tuple mode propagation - dynamic taints tuple" =
  go
    {|
let x = 1 @ dynamic;;
let t = x, 2;;
let _ = t @ static;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 10)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "tuple as function return" =
  go
    {|
let f = fn (x : int) -> (x, x);;
let _ = f 1;;
|};
  [%expect {| |}]
;;

let%expect_test "nested tuple" =
  go
    {|
let t = (1, (2, 3)), (4, 5);;
|};
  [%expect {| |}]
;;

let%expect_test "tuple with arithmetic" =
  go
    {|
let t = 1 + 2, 3 * 4;;
|};
  [%expect {| |}]
;;

let%expect_test "tuple in let binding used later" =
  go
    {|
let t = 1, 2;;
let _ = t;;
|};
  [%expect {| |}]
;;

let%expect_test "tuple with if expression elements" =
  go
    {|
let t = (if true then 1 else 2), (if false then 3 else 4);;
|};
  [%expect {| |}]
;;

let%expect_test "tuple of functions" =
  go
    {|
let t = (fn (x : int) -> x), (fn (y : bool) -> y);;
|};
  [%expect {| |}]
;;

let%expect_test "large tuple" =
  go
    {|
let t = 1, 2, 3, 4, 5;;
|};
  [%expect {| |}]
;;

let%expect_test "tuple with erased elements" =
  go
    {|
let x = 1 @ erased;;
let t = x, 2;;
|};
  [%expect {| |}]
;;

let%expect_test "tuple mixing types and values" =
  go
    {|
let t = int, 1;;
|};
  [%expect {| |}]
;;
