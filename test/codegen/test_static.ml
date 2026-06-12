open! Core
open! Syl

let go ?print ?(check = `Run) input = Common.codegen ?print ~check input

let%expect_test "Apply fn static arg" =
  go
    {|
let _ =
  (fn (x : int) -> x) 1
;;|};
  [%expect {| |}]
;;

let%expect_test "Apply dynamic fn static arg" =
  go
    {|
let dyn_fn = (fn (x : int) -> x) @ dynamic;;
let _ =
  dyn_fn 1
;;|};
  [%expect {| |}]
;;

let%expect_test "Lambda static arg" =
  go
    {|
let _ =
  fn (static x : int) -> 1
;;|};
  [%expect {| |}]
;;

let%expect_test "static fun" =
  go
    {|
fun f (static x : int) : unit = print_int x;;
let _ = f 42;;
let _ = f 69;;
|};
  [%expect
    {|
    42
    69
    |}]
;;

let%expect_test "static lambda" =
  go
    {|
let f = fn (static x : int) -> print_int x;;
let _ = f 42;;
let _ = f 69;;
|};
  [%expect
    {|
    42
    69
    |}]
;;

let%expect_test "static lambda with arithmetic on static arg" =
  go
    {|
let f = fn (static x : int) -> x + 1;;
let _ = f 10;;
|};
  [%expect {| |}]
;;

let%expect_test "static lambda with capture" =
  go
    {|
let y = 1;;
let f = fn (static x : int) -> x + y;;
let _ = f 10;;
|};
  [%expect {| |}]
;;

let%expect_test "static lambda with capture" =
  go
    {|
let _ =
let y = 1 in
let f = fn (static x : int) -> x + y in
f 10;;
|};
  [%expect {| |}]
;;

let%expect_test "static lambda with boolean op on static arg" =
  go
    {|
let f = fn (static x : bool) -> x && true;;
let _ = f false;;
|};
  [%expect {| |}]
;;

let%expect_test "nested static lambdas" =
  go
    {|
let f = fn (static x : int) -> fn (static y : int) -> x + y;;
let _ = f 1 2;;
let _ = f 1 3;;
|};
  [%expect {| |}]
;;

let%expect_test "static lambda returning static lambda" =
  go
    {|
let f = fn (static x : int) -> fn (static y : int) -> x;;
let g = f 1;;
let _ = g 2;;
|};
  [%expect {| |}]
;;

let%expect_test "static lambda mixed with dynamic lambda" =
  go
    {|
let f = fn (static x : int) -> fn (y : int) -> y;;
let g = f 1;;
let _ = g 2;;
|};
  [%expect {| |}]
;;

let%expect_test "dynamic lambda inside static lambda uses static arg as type" =
  go
    {|
let f = fn (static erased t : type) -> fn (x : t) -> x;;
let g = f int;;
let _ = g 42;;
let h = f bool;;
let _ = h true;;
|};
  [%expect {| |}]
;;

let%expect_test "if static with correct type annotation using non-static if" =
  go
    {|
let f = fn (static x : int) -> (if static x == 0 then 1 else true) : (if x == 0 then int else bool);;
|};
  [%expect {| |}]
;;

let%expect_test "fun with static arg creates Pi type" =
  go
    {|
fun f (static x : int) : int = x;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "fun with static arg creates Pi type" =
  go
    {|
fun f (static x : int) : int = x + 1;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "fun with static arg creates Pi type" =
  go
    {|
let _ =
  fun f (static x : int) : int = x in
  let _ = f 0
in ();;
|};
  [%expect {| |}]
;;

let%expect_test "lift static value through Pi" =
  go
    {|
let f = fn (static x : int) -> x @ erased;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "static lambda unused arg" =
  go
    {|
let f = fn (static _ : int) -> 42;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "if static with bool static arg" =
  go
    {|
let f = fn (static b : bool) -> if static b then 1 else true;;
let _ = f true;;
let _ = f false;;
|};
  [%expect {| |}]
;;

let%expect_test "static arg used in arithmetic, result applied" =
  go
    {|
let double = fn (static x : int) -> x + x;;
let _ = double 5;;
|};
  [%expect {| |}]
;;

let%expect_test "symbolic arrow type as static arg" =
  go
    {|
let choose = fn (static f : static int \ x -> if x == 0 then int else bool) -> f 0;;
let _ = choose (fn (static x : int) -> if static x == 0 then 0 else true);;
|};
  [%expect {| |}]
;;

let%expect_test "static lambda body references outer let binding" =
  go
    {|
let n = 10;;
let f = fn (static x : int) -> x + n;;
let _ = f 5;;
|};
  [%expect {| |}]
;;

let%expect_test "static lambda with type annotation on body" =
  go
    {|
let f = fn (static x : int) -> (x : int);;
let _ = f 42;;
|};
  [%expect {| |}]
;;

let%expect_test "higher-order static: take a static function and apply it" =
  go
    {|
let apply = fn (static f : static int -> int) -> f 5;;
let _ = apply (fn (static x : int) -> x + 1);;
|};
  [%expect {| |}]
;;

let%expect_test "static lambda" =
  go
    {|
let _ = (fn (static x : int) -> x + 1) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: static if with different result types" =
  go
    {|
let f = fn (static b : bool) -> if static b then 42 else true;;
let _ = f true;;
let _ = f false;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: nested static if" =
  go
    {|
let f = fn (static x : int) ->
  if static x > 0 then
    if static x > 10 then 100 else x
  else 0;;
let _ = f 5;;
let _ = f 15;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: static lambda returning function" =
  go
    {|
let make_adder = fn (static n : int) -> fn (x : int) -> x + n;;
let add5 = make_adder 5;;
let add10 = make_adder 10;;
let _ = add5 1;;
let _ = add10 1;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: static value used as dynamic" =
  go
    {|
let x = 5;;
let y = x @ dynamic;;
let _ = y + 1;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: static if with unreachable else" =
  go
    {|
let _ = if static true then 42 else unreachable;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: static if with unreachable then" =
  go
    {|
let _ = if static false then unreachable else 42;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: static function returning closure over dynamic" =
  go
    {|
let make = fn (static n : int) -> fn (x : int) -> x + n;;
let f = make 5;;
let _ = f (10 @ dynamic);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: static computation fed to dynamic" =
  go
    {|
let s = 2 + 3;;
let d = s @ dynamic;;
let _ = d + 1;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: static lambda many instances" =
  go
    {|
let f = fn (static x : int) -> x * 2;;
let a = f 1;;
let b = f 2;;
let c = f 3;;
let _ = a + b + c;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: static lambda returning unit" =
  go
    {|
let f = fn (static erased t : type) -> fn (_ : t) -> ();;
let _ = f int 0;;
let _ = f bool true;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: gte in non-static if (exposes wrong branch)" =
  go
    {|
let _ = if (10 >= 5) then 1 else 2;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: multiple independent static lambdas" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let const = fn (static erased t : type) -> fn (static erased u : type) -> fn (x : t) -> fn (_ : u) -> x;;
let _ = id int 42;;
let _ = const int bool 42 true;;
|};
  [%expect {| |}]
;;
