open! Core
open! Syl

let go ?print ?(check = `Run) input = Common.codegen ?print ~check input

let%expect_test "assert" =
  go
    {|
let _ = assert true;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert" =
  go
    {|
let x = true @ dynamic;;
let _ = assert x;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert dynamic literal" =
  go
    {|
let _ = assert true;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert dynamic variable" =
  go
    {|
let x = true @ dynamic;;
let _ = assert x;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert dynamic result is unit" =
  go
    {|
fun f (x : bool) : unit = assert x;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert dynamic result is static" =
  go
    {|
let x = true;;
let _ = (assert x) @ static;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert dynamic result is static" =
  go
    {|
let x = true;;
let _ = (assert x) @ static erased;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert dynamic in function body" =
  go
    {|
fun check (x : int) : unit =
  assert (x > 0);;
  |};
  [%expect {| |}]
;;

let%expect_test "assert dynamic in let body" =
  go
    {|
fun f (x : int) : int =
  let _ = assert (x >= 0) in
  x + 1;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert dynamic in if branch" =
  go
    {|
fun f (x : int) : int =
  if x > 0 then
    let _ = assert (x != 0) in
    x
  else
    0 - x;;
  |};
  [%expect {| |}]
;;

let%expect_test "fuzz: runtime assert true" =
  go
    {|
let _ = assert (true @ dynamic);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: static assert" =
  go
    {|
let _ = assert erased true;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: assert with comparison" =
  go
    {|
let _ = assert (1 < 2);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: assert erased with gte (value-level correct)" =
  go
    {|
let _ = assert erased (5 >= 3);;
|};
  [%expect {| |}]
;;
