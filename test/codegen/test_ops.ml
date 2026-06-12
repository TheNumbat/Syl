open! Core
open! Syl

let go ?print ?(check = `Run) input = Common.codegen ?print ~check input

let%expect_test "Unop static" =
  go
    {|
let _ =
  !true
;;|};
  [%expect {| |}]
;;

let%expect_test "Unop dynamic" =
  go
    {|
let _ =
  !(true @ dynamic)
;;|};
  [%expect {| |}]
;;

let%expect_test "Unop var static" =
  go
    {|
let dyn = true;;
let _ =
  !dyn
;;|};
  [%expect {| |}]
;;

let%expect_test "Unop var dynamic" =
  go
    {|
let dyn = true @ dynamic;;
let _ =
  !dyn
;;|};
  [%expect {| |}]
;;

let%expect_test "Binop static + static" =
  go
    {|
let _ =
  1 + 2
;;|};
  [%expect {| |}]
;;

let%expect_test "Binop static + dynamic" =
  go
    {|
let dyn = 2 @ dynamic;;
let _ =
  1 + dyn
;;|};
  [%expect {| |}]
;;

let%expect_test "Binop dynamic + static" =
  go
    {|
let dyn = 1 @ dynamic;;
let _ =
  dyn + 2
;;|};
  [%expect {| |}]
;;

let%expect_test "Binop dynamic + dynamic" =
  go
    {|
let dyn1 = 1 @ dynamic;;
let dyn2 = 2 @ dynamic;;
let _ =
  dyn1 + dyn2
;;|};
  [%expect {| |}]
;;

let%expect_test "fuzz: gte 5 >= 3 gives wrong constant" =
  go
    {|
let _ = assert erased (5 >= 3);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: gte 3 >= 5 gives wrong constant" =
  go
    {|
let _ = assert erased !(3 >= 5);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: gte equal values" =
  go
    {|
let _ = assert erased (3 >= 3);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: all comparison operators" =
  go
    {|
let _ = assert erased (1 < 2);;
let _ = assert erased !(2 < 1);;
let _ = assert erased (1 <= 2);;
let _ = assert erased !(2 <= 1);;
let _ = assert erased !(1 > 2);;
let _ = assert erased (2 > 1);;
let _ = assert erased (1 == 1);;
let _ = assert erased (1 != 2);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: gte 5 >= 3 gives wrong constant" =
  go
    {|
let _ = 5 >= 3;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: gte 3 >= 5 gives wrong constant" =
  go
    {|
let _ = 3 >= 5;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: gte equal values" =
  go
    {|
let _ = 3 >= 3;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: all comparison operators" =
  go
    {|
let _ = 1 < 2;;
let _ = 2 < 1;;
let _ = 1 <= 2;;
let _ = 2 <= 1;;
let _ = 1 > 2;;
let _ = 2 > 1;;
let _ = 1 == 1;;
let _ = 1 != 2;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: gte result used in dynamic if takes wrong branch" =
  go
    {|
let _ = if (5 >= 3) then 1 else 2;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: gte in boolean expression" =
  go
    {|
let _ = (5 >= 3) && (10 >= 1);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: division by constant" =
  go
    {|
let _ = 10 / 2;;
let _ = 10 % 3;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: nested arithmetic" =
  go
    {|
let _ = (1 + 2) * (3 - 4) / (5 + 1);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: negation of negation" =
  go
    {|
let _ = - (- 5);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: boolean double negation" =
  go
    {|
let _ = !(!true);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: zero arithmetic" =
  go
    {|
let _ = 0 + 0;;
let _ = 0 * 100;;
let _ = 0 - 0;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: large integers" =
  go
    {|
let _ = 999999999;;
let _ = 999999999 + 1;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: mixed dynamic arithmetic" =
  go
    {|
let x = 10 @ dynamic;;
let y = 20 @ dynamic;;
let _ = x + y * 2 - 1;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: boolean operators" =
  go
    {|
let _ = true || false;;
let _ = false && true;;
let _ = true && true;;
let _ = false || false;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: dynamic boolean operations" =
  go
    {|
let a = true @ dynamic;;
let b = false @ dynamic;;
let _ = a && b;;
let _ = a || b;;
let _ = !a;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: comparison chain" =
  go
    {|
let x = 5 @ dynamic;;
let _ = (x > 0) && (x < 10);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: gte with dynamic operands (not constant-folded)" =
  go
    {|
let x = 10 @ dynamic;;
let y = 5 @ dynamic;;
let _ = if x >= y then 1 else 2;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: negative number operations" =
  go
    {|
let _ = -1 + -2;;
let _ = -(-3);;
let _ = 0 - 1;;
|};
  [%expect {| |}]
;;
