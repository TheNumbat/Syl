open! Core
open! Syl

let go ?print ?(check = `Run) input = Common.codegen ?print ~check input

let%expect_test "Apply fn dynamic arg" =
  go
    {|
let dyn = 1 @ dynamic;;
let _ =
  (fn (x : int) -> x) dyn
;;|};
  [%expect {| |}]
;;

let%expect_test "Apply dynamic fn dynamic arg" =
  go
    {|
let dyn_fn = (fn (x : int) -> x) @ dynamic;;
let dyn_arg = 1 @ dynamic;;
let _ =
  dyn_fn dyn_arg
;;|};
  [%expect {| |}]
;;

let%expect_test "binder taking closure, applied erased inside" =
  go
    {|
let apply = fn (static f : static int -> erased int) -> (f @ erased) 0;;
let g = fn (x : int) -> 1;;
let _ = apply g;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: immediate closure application" =
  go
    {|
let _ = (fn (x : int) -> fn (y : int) -> x + y) 10 20;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: higher-order: apply twice" =
  go
    {|
let apply_twice = fn (f : int -> int) -> fn (x : int) -> f (f x);;
let add3 = fn (x : int) -> x + 3;;
let _ = apply_twice add3 0;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: deeply nested closure application" =
  go
    {|
let f = fn (a : int) -> fn (b : int) -> fn (c : int) -> fn (d : int) -> a + b + c + d;;
let _ = f 1 2 3 4;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: immediately applied lambda" =
  go
    {|
let _ = (fn (x : int) -> x) 42;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: nested immediate application" =
  go
    {|
let _ = (fn (x : int) -> (fn (y : int) -> x + y) 20) 10;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: closure capturing result of closure application" =
  go
    {|
let f = fn (x : int) -> x + 1;;
let y = f (10 @ dynamic);;
let g = fn (_ : unit) -> y;;
let _ = g ();;
|};
  [%expect {| |}]
;;
