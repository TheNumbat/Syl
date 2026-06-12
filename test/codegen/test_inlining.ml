open! Core
open! Syl

let go ?print ?(check = `Run) input = Common.codegen ?print ~check input

let%expect_test "static closure nest" =
  go
    {|
let f1 = fn (erased x : int) -> 1;;
let g = fn (static f2 : static int -> int) -> f2 0;;
let _ = g f1;;
|};
  [%expect {| |}]
;;

let%expect_test "static closure nest" =
  go
    {|
let f1 = fn (erased x : int) -> 1;;
let g = fn (static f2 : erased int -> int) -> f2 0;;
let _ = g f1;;
|};
  [%expect {| |}]
;;

let%expect_test "static closure nest" =
  go
    {|
let f1 = fn (erased x : int) -> 1;;
let g = fn (static f2 : erased int -> int) -> f2 0;;
let _ = g f1;;
|};
  [%expect {| |}]
;;

let%expect_test "parametric erased closure" =
  go
    {|
let f = fn (erased x : int) -> 1;;
let _ = f 42;;
|};
  [%expect {| |}]
;;

let%expect_test "parametric erased closure multi-arg" =
  go
    {|
let f = fn (erased x : int) -> fn (y : int) -> y;;
let _ = f 10 20;;
|};
  [%expect {| |}]
;;

let%expect_test "parametric erased closure multiple erased params" =
  go
    {|
let f = fn (erased x : int) -> fn (erased y : int) -> 1;;
let _ = f 10 20;;
|};
  [%expect {| |}]
;;

let%expect_test "parametric erased closure in conditional" =
  go
    {|
let f = fn (erased x : int) -> if 0 == 0 then 1 else 2;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "parametric erased closure unused" =
  go
    {|
let f = fn (erased x : int) -> 99;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "parametric erased with type param" =
  go
    {|
let id = fn (static erased t : type) -> fn (erased x : int) -> fn (y : t) -> y;;
let _ = id int 0 42;;
|};
  [%expect {| |}]
;;

let%expect_test "parametric erased partial application" =
  go
    {|
let f = fn (erased x : int) -> fn (y : int) -> y + 1;;
let g = f 10;;
let _ = g 20;;
|};
  [%expect {| |}]
;;

let%expect_test "static closure param capturing static value" =
  go
    {|
let n = 10;;
let add_n = fn (x : int) -> x + n;;
let apply = fn (static f : int -> int) -> fn (x : int) -> f x;;
let _ = apply add_n 5;;
|};
  [%expect {| |}]
;;

let%expect_test "static closure param capturing multiple values" =
  go
    {|
let a = 10;;
let b = 20;;
let combine = fn (x : int) -> x + a + b;;
let apply = fn (static f : int -> int) -> fn (x : int) -> f x;;
let _ = apply combine 5;;
|};
  [%expect {| |}]
;;

let%expect_test "static closure param applied twice" =
  go
    {|
let n = 1;;
let inc = fn (x : int) -> x + n;;
let apply2 = fn (static f : int -> int) -> f (f 0);;
let _ = apply2 inc;;
|};
  [%expect {| |}]
;;

let%expect_test "static closure param with polymorphic caller" =
  go
    {|
let map =
  fn (static erased t : type) ->
    fn (static erased u : type) -> fn (static f : t -> u) -> fn (x : t) -> f x
;;
let base = 100;;
let _ = map int int (fn (x : int) -> x + base) 42;;
|};
  [%expect {| |}]
;;

let%expect_test "static closure param at multiple monomorphizations" =
  go
    {|
let apply = fn (static erased t : type) -> fn (static f : t -> t) -> fn (x : t) -> f x;;
let n = 1;;
let _ = apply int (fn (x : int) -> x + n) 42;;
let _ = apply bool (fn (x : bool) -> x) true;;
|};
  [%expect {| |}]
;;

let%expect_test "composed static closure params capturing ambient" =
  go
    {|
let a = 1;;
let b = 2;;
let compose = fn (static f : int -> int) -> fn (static g : int -> int) -> fn (x : int) -> f (g x);;
let _ = compose (fn (x : int) -> x + a) (fn (x : int) -> x * b) 5;;
|};
  [%expect {| |}]
;;

let%expect_test "static closure param from nested let" =
  go
    {|
let apply = fn (static f : int -> int) -> fn (x : int) -> f x;;
let _ =
  let offset = 100 in
  let add_offset = fn (x : int) -> x + offset in
  apply add_offset 42
;;
|};
  [%expect {| |}]
;;

let%expect_test "static fn with parametric captures" =
  go
    {|
let apply = fn (f : int -> int) -> fn (x : int) -> f x;;
let _ = fn (x : int) -> apply (fn (y : int) -> y + x) 10;;
|};
  [%expect {| |}]
;;

let%expect_test "static fn with parametric captures" =
  go
    {|
fun apply (f : int -> int) : int -> int =
  fun g (x : int) : int = f x in g;;
let _ = fn (x : int) -> apply (fn (y : int) -> y + x) 10;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: many static instantiations" =
  go
    {|
let f = fn (static x : int) -> x + 1;;
let _ = f 0;;
let _ = f 1;;
let _ = f 2;;
let _ = f 3;;
let _ = f 4;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: static erased lambda" =
  go
    {|
let _ = (fn (static erased t : type) -> fn (x : t) -> x) int 42;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: multiple monomorphizations" =
  go
    {|
let f = fn (static erased t : type) -> fn (x : t) -> x;;
let int_id = f int;;
let bool_id = f bool;;
let _ = int_id 42;;
let _ = bool_id true;;
|};
  [%expect {| |}]
;;

let%expect_test "static fn applied for effects" =
  go
    {|
let f = fn (x : int) -> print_int x;;
let _ = f 1;;
let _ = f 2;;
|};
  [%expect
    {|
    1
    2
    |}]
;;

let%expect_test "quoted closure rebinds capture of a later mono" =
  go
    {|
let f = let c = 5 in fn (u : unit) -> fn (static n : int) -> n + c;;
let g = f ();;
let _ = print_int (g 1);;
|};
  [%expect {| 6 |}]
;;

let%expect_test "quoted binder rebinds capture of a later mono" =
  go
    {|
let f = let c = 5 in fn (static u : unit) -> fn (static n : int) -> n + c;;
let g = f ();;
let _ = print_int (g 1);;
|};
  [%expect {| 6 |}]
;;
