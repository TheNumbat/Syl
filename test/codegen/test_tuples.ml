open! Core
open! Syl

let go ?print ?(check = `Run) input = Common.codegen ?print ~check input

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

let%expect_test "tuple type in function argument" =
  go
    {|
let f = fn (t : int ^ int) -> t;;
let _ = f (1, 2);;
|};
  [%expect {| |}]
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
let t = 1, 2, (), 4, 5;;
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

let%expect_test "fuzz: all-unit tuple" =
  go
    {|
let t = (), ();;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: triple unit tuple" =
  go
    {|
let t = (), (), ();;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: unit tuple in function return" =
  go
    {|
let f = fn (_ : unit) -> ((), ());;
let _ = f ();;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: unit and int tuple" =
  go
    {|
let t = (), 42;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: deeply nested tuples" =
  go
    {|
let t = (1, (2, (3, (4, 5))));;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: tuple with dynamic elements" =
  go
    {|
let x = 1 @ dynamic;;
let y = 2 @ dynamic;;
let t = (x, y, 3);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: tuple with bool and int" =
  go
    {|
let t = (true, 42, false, 0);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: tuple of closures" =
  go
    {|
let f = fn (x : int) -> x + 1;;
let g = fn (x : int) -> x * 2;;
let t = (f, g);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: tuple with mixed modes" =
  go
    {|
let x = 1 @ erased;;
let y = 2;;
let z = 3 @ dynamic;;
let t = (x, y, z);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: tuple returned from closure" =
  go
    {|
let f = fn (x : int) -> (x, x + 1, x + 2);;
let _ = f 10;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: tuple of tuples" =
  go
    {|
let a = (1, 2);;
let b = (3, 4);;
let c = (a, b);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: tuple in function argument" =
  go
    {|
let f = fn (t : int ^ int) -> t;;
let _ = f (1, 2);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: if returning tuple" =
  go
    {|
let b = true @ dynamic;;
let _ = if b then (1, 2) else (3, 4);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: function taking and returning tuple" =
  go
    {|
let swap = fn (t : int ^ bool) -> t;;
let _ = swap (1, true);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: tuple type annotation" =
  go
    {|
let _ = ((1, true) : int ^ bool);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: let-in in tuple elements" =
  go
    {|
let t = (let x = 1 in x), (let y = 2 in y);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: monomorphize with tuple type arg" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let t = id (int ^ bool) (1, true);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: closure capturing tuple" =
  go
    {|
let t = (1, 2) @ dynamic;;
let f = fn (_ : unit) -> t;;
let _ = f ();;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: tuple type as value" =
  go
    {|
let T = int ^ bool;;
let t = (42, true) : T;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: recursive with tuple return" =
  go
    {|
fun f (x : int) : int ^ int =
  if x <= 0 then (0, 0) else (x, x + 1);;
let _ = f 5;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: nested tuples with closures" =
  go
    {|
let f = fn (x : int) -> x;;
let g = fn (x : bool) -> x;;
let t = ((f, 1), (g, true));;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: tuple mixed modes" =
  go
    {|
let d = 1 @ dynamic;;
let s = 2;;
let e = 3 @ erased;;
let t = (d, s, e, true, ());;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: tuple with closure capturing tuple" =
  go
    {|
let pair = (1, 2) @ dynamic;;
let f = fn (_ : unit) -> pair;;
let t = (f, 3);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: tuple round-trip through function" =
  go
    {|
let roundtrip = fn (t : int ^ bool ^ unit) -> t;;
let _ = roundtrip (1, true, ());;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: function applications as tuple elements" =
  go
    {|
let inc = fn (x : int) -> x + 1;;
let dec = fn (x : int) -> x - 1;;
let t = (inc 5, dec 5);;
|};
  [%expect {| |}]
;;
