open! Core
open! Syl

let go ?print ?(check = `Run) input = Common.codegen ?print ~check input

let%expect_test "primitive" =
  go
    {|
builtin add = syl_int_add;;
let _ = add (1, 2);;
|};
  [%expect {| |}]
;;

let%expect_test "literals" =
  go
    {|
let _ = ();;
let _ = true;;
let _ = 123;;
let _ = () @ erased;;
let _ = true @ erased;;
let _ = 123 @ erased;;
let _ = () @ dynamic;;
let _ = true @ dynamic;;
let _ = 123 @ dynamic;;|};
  [%expect {| |}]
;;

let%expect_test "if" =
  go
    {|
let _ = if true then 1, 2 else 3, 4;;
|};
  [%expect {| |}]
;;

let%expect_test "fun" =
  go
    {|
fun a (x : int) : int = x;;
let _ = print_int (a 1);;
|};
  [%expect {| 1 |}]
;;

let%expect_test "fun" =
  go
    {|
fun b (_ : unit) : unit -> unit = fn (_ : unit) -> ();;
let _ = print_unit (b () ());;
|};
  [%expect {| () |}]
;;

let%expect_test "join" =
  go
    {|
fun a (_ : unit) : unit = ();;
fun b (_ : unit) : unit -> unit = fn (_ : unit) -> ();;
let x = if erased false then a else b;;
let _ = x () ();;
|};
  [%expect {| |}]
;;

let%expect_test "three-level nested 2-arg static mono" =
  go
    {|
let min2 = fn (static x : int) -> fn (static y : int) -> x + y;;
let lo   = fn (static a : int) -> fn (static b : int) -> min2 a b;;
let wrap = fn (static a : int) -> fn (static b : int) -> lo a b;;
let _ = print_int (wrap 1 2);;
|};
  [%expect {| 3 |}]
;;

let%expect_test "static int" =
  go
    {|
let f = fn (static x : int) -> x;;
let g = f;;
let _ = print_int (g 2);;
|};
  [%expect {| 2 |}]
;;

let%expect_test "static int" =
  go
    {|
let f = fn (static x : int) -> let _ = print_int x in x;;
let x = f (f 1);;
let _ = print_int x;;
|};
  [%expect
    {|
    1
    1
    1
    |}]
;;

let%expect_test "static int" =
  go
    {|
let f = fn (static x : int) -> x + 1;;
let x = f (f 1);;
let _ = print_int x;;
|};
  [%expect {| 3 |}]
;;

let%expect_test "fuzz: capture bool then int (misaligned access)" =
  go
    {|
let b = true @ dynamic;;
let n = 42 @ dynamic;;
let f = fn (_ : unit) -> if b then n else 0;;
let _ = f ();;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: capture int then bool then int" =
  go
    {|
let a = 1 @ dynamic;;
let b = true @ dynamic;;
let c = 2 @ dynamic;;
let f = fn (_ : unit) -> if b then a else c;;
let _ = f ();;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: capture bool then int (misaligned access)" =
  go
    {|
let b = true @ dynamic;;
let n = 42 @ dynamic;;
let f = fn (_ : unit) -> if b then n else 0;;
let _ = f ();;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: capture int then bool then int" =
  go
    {|
let a = 1 @ dynamic;;
let b = true @ dynamic;;
let c = 2 @ dynamic;;
let f = fn (_ : unit) -> if b then a else c;;
let _ = f ();;
|};
  [%expect {| |}]
;;

let%expect_test "var" =
  go
    {|
let x = true @ dynamic;;
let _ =
  match !x {
   x -> assert !x

};;
|};
  [%expect {| |}]
;;

let%expect_test "erase with static captures" =
  go
    {|
let _ = fn (static x : int) ->
  let f = fn (y : int) -> y + x in
  let _ = f 10 in
  ()
;;|};
  [%expect {| |}]
;;

let%expect_test "worst-case-style tree with N columns" =
  go
    {|
fun classify (t : bool ^ bool ^ bool ^ bool ^ bool) : int =
  match t {
    (true, _, _, _, _) -> 1,
    (_, true, _, _, _) -> 2,
    (_, _, true, _, _) -> 3,
    (_, _, _, true, _) -> 4,
    (_, _, _, _, true) -> 5,
    _ -> 0,
  }
;;

let _ = assert (classify (true, false, false, false, false) == 1);;

let _ = assert (classify (false, true, false, false, false) == 2);;

let _ = assert (classify (false, false, true, false, false) == 3);;

let _ = assert (classify (false, false, false, true, false) == 4);;

let _ = assert (classify (false, false, false, false, true) == 5);;

let _ = assert (classify (false, false, false, false, false) == 0);;

let _ = assert (classify (true, true, true, true, true) == 1);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: capture bool then int (misaligned access)" =
  go
    {|
let b = true @ dynamic;;
let n = 42 @ dynamic;;
let f = fn (_ : unit) -> if b then n else 0;;
let _ = f ();;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: capture int then bool then int" =
  go
    {|
let a = 1 @ dynamic;;
let b = true @ dynamic;;
let c = 2 @ dynamic;;
let f = fn (_ : unit) -> if b then a else c;;
let _ = f ();;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: type annotations on everything" =
  go
    {|
let x = (42 : int);;
let b = (true : bool);;
let u = (() : unit);;
let f = (fn (x : int -> int) -> x : int -> int);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: many bindings" =
  go
    {|
let a = 1;;
let b = 2;;
let c = 3;;
let d = 4;;
let e = 5;;
let f = 6;;
let g = 7;;
let h = 8;;
let _ = a + b + c + d + e + f + g + h;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: type variable used multiple times" =
  go
    {|
let f = fn (static erased t : type) -> fn (x : t) -> fn (y : t) -> (x, y);;
let _ = f int 1 2;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: multiple unit bindings" =
  go
    {|
let _ = ();;
let _ = ();;
let _ = ();;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: multi-level capture" =
  go
    {|
let a = 1 @ dynamic;;
let f = fn (b : int) ->
  let c = b + 1 in
  fn (_ : unit) -> a + c;;
let g = f (2 @ dynamic);;
let _ = print_int (g ());;
|};
  [%expect {| 4 |}]
;;
