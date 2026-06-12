open! Core
open! Syl

let go ?print ?(check = `Run) input = Common.codegen ?print ~check input

let%expect_test "dynamic static erased" =
  go
    {|
let _ =
  (true @ static erased)
;;|};
  [%expect {| |}]
;;

let%expect_test "erased closure" =
  go
    {|
let _ =
  (fn (x : int) -> x) @ erased
;;|};
  [%expect {| |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (erased x : int) -> x;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (erased x : int) -> 1;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (erased x : int) -> 1;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = (fn (erased x : int) -> 1) ;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = (fn (erased x : int) -> 1) ;;
let _ = f (0 @ erased);;
|};
  [%expect {| |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (erased x : int) -> 1;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let _ = (fn (erased x : int) -> 1) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let _ = (fn (erased x : int) -> 1) (0 @ erased);;
|};
  [%expect {| |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let _ = (fn (static x : int) -> 1) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let _ = (fn (static erased x : int) -> 1) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let _ = (fn (static erased x : int) -> 1) (0 @ erased);;
|};
  [%expect {| |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (static erased g : static int -> erased int) -> let _ = g 1 in 2;;
let _ = f (fn (x : int) -> 0 @ erased);;
|};
  [%expect {| |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (erased x : int -> int) -> 1;;
let _ = f ((fn (x : int) -> x + 1) @ erased);;
|};
  [%expect {| |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (erased x : int -> int) -> 1;;
let _ = f ((fn (x : int) -> x + 1) @ erased);;
|};
  [%expect {| |}]
;;

let%expect_test "static erased closure arg" =
  go
    {|
let _ =
  (fn (static erased x : int) -> x)
;;|};
  [%expect {| |}]
;;

let%expect_test "static erased arg" =
  go
    {|
let f = (fn (static erased x : int) -> 1);;
let _ = f 0;;
let _ = f 1;;
|};
  [%expect {| |}]
;;

let%expect_test "closure static erased" =
  go
    {|
let x = (fn (static erased x : int) -> 1) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "Apply static erased fn static arg" =
  go
    {|
let _ =
  (fn (static erased x : int) -> x) 1
;;|};
  [%expect {| |}]
;;

let%expect_test "Apply erased fn dynamic arg" =
  go
    {|
let dyn = 1;;
let y =
  (fn (erased x : int) -> 5) (dyn-1)
;;
let _ = y @ unerased;;|};
  [%expect {| |}]
;;

let%expect_test "Apply erased fn dynamic arg" =
  go
    {|
let f = fn (static x : int) -> x @ erased;;
let y =
  (fn (erased x : int) -> 5) (f 1)
;;
let _ = y @ unerased;;|};
  [%expect {| |}]
;;

let%expect_test "Apply erased fn stat arg" =
  go
    {|
let dyn = 1;;
let y =
  (fn (erased x : int) -> 5) (dyn-1)
;;
let _ = y @ unerased;;|};
  [%expect {| |}]
;;

let%expect_test "Apply erased fn stat arg" =
  go
    {|
let dyn = 1;;
let y =
  (fn (static x : int) -> 5) (dyn-1)
;;
let _ = y @ unerased;;|};
  [%expect {| |}]
;;

let%expect_test "Apply erased fn stat arg" =
  go
    {|
let dyn = 1;;
let y =
  (fn (static erased x : int) -> 5) (dyn-1)
;;
let _ = y @ unerased;;|};
  [%expect {| |}]
;;

let%expect_test "Lambda erased arg" =
  go
    {|
let _ =
  fn (erased x : int) -> x
;;|};
  [%expect {| |}]
;;

let%expect_test "static erased lambda" =
  go
    {|
let f = fn (static x : int) -> fn (_ : unit) -> x;;
let g = (f 1 ()) @ unerased;;
|};
  [%expect {| |}]
;;

let%expect_test "return erased" =
  go
    {|
let f = fn (x : int) -> 0 @ erased;;
let _ = f 1;;
|};
  [%expect {| |}]
;;

let%expect_test "Fun erased arg" =
  go
    {|
fun f (erased x : int) : erased int = x;;
|};
  [%expect {| |}]
;;

let%expect_test "Fun return erased" =
  go
    {|
fun f (x : int) : erased int = 1 @ erased;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken mode: static unerased -> static erased (literal substitution)" =
  go
    {|
let _ = 1 @ erased;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken if non-split: mode erasure on branch" =
  go
    {|
let _ = if true then 1 else 1 @ erased;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken binder apply: erasure on body" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else 1 @ erased;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken pi closure apply erased: erasure only" =
  go
    {|
let f = fn (x : int) -> x;;
let g = fn (static erased x : int) -> x;;
let _ = (if true then f else g) @ erased;;
|};
  [%expect {| |}]
;;

let%expect_test "closure to closure: arg erasure contravariant" =
  go
    {|
let apply = fn (static f : static int -> int) -> f 0;;
let g = fn (erased x : int) -> 1;;
let _ = apply g;;
|};
  [%expect {| |}]
;;

let%expect_test "closure to closure: ret erasure covariant" =
  go
    {|
let apply = fn (f : int -> erased int) -> f 0;;
let g = fn (x : int) -> x;;
let _ = apply g;;
|};
  [%expect {| |}]
;;

let%expect_test "binder to binder: arg erasure contravariant" =
  go
    {|
let apply = fn (static f : static int -> int) -> f 0;;
let g = fn (static erased x : int) -> 1;;
let _ = apply g;;
|};
  [%expect {| |}]
;;

let%expect_test "erased closure taking closure arg" =
  go
    {|
let apply = fn (f : int -> int) -> f 0;;
let _ = apply (fn (x : int) -> x);;
|};
  [%expect {| |}]
;;

let%expect_test "fun with static erased type arg, two sequential funs" =
  go
    {|
fun id1 (static erased t : type) : t -> t = fn (x : t) -> x;;
fun id2 (static erased t : type) : t -> t = id1 t;;
let _ = id2 int 0;;
|};
  [%expect {| |}]
;;

let%expect_test "static erased lambda captures no runtime value" =
  go
    {|
let f = fn (static erased x : int) -> 0;;
let _ = f 1;;
|};
  [%expect {| |}]
;;

let%expect_test "fun returning static erased type" =
  go
    {|
fun f (static _ : unit) : static erased type = int;;
let _ = 5 : f ();;
|};
  [%expect {| |}]
;;

let%expect_test "multiple static erased type args" =
  go
    {|
let f = fn (static erased t1 : type) -> fn (static erased t2 : type) -> fn (x : t1) -> fn (y : t2) -> x;;
let _ = f int bool 0 true;;
|};
  [%expect {| |}]
;;

let%expect_test "erased fn" =
  go
    {|
fun erased f (x : int) : erased int = 0;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "local erased fun preserves rest" =
  go
    {|
let _ =
  fun erased f (x : int) : erased int = 0 in
  print_int 1
;;
|};
  [%expect {| 1 |}]
;;

let%expect_test "erased fn" =
  go
    {|
let f = fn erased (x : int) -> 0 @ erased;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "erased fn" =
  go
    {|
fun erased f (erased x : int) : erased int = 0;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "erased fn" =
  go
    {|
let f = fn erased (erased x : int) -> 0 @ erased;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "erased fn" =
  go
    {|
fun erased f (x : int) : erased int = 0;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "erased fn" =
  go
    {|
let f = fn erased (x : int) -> 0 @ erased;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "static erased lambda" =
  go
    {|
fun apply (static f : int -> int) : int = f 0;;
let x = apply (fn (x : int) -> x + 1);;
|};
  [%expect {| |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (erased g : int -> erased int) -> let _ = g 1 in 2;;
let _ = f (fn (x : int) -> 0 @ erased);;
|};
  [%expect {| |}]
;;

let%expect_test "erased binder arg" =
  go
    {|
let f = fn (erased g : static int -> erased int) -> let _ = g 1 in 2;;
let _ = f (fn (static x : int) -> 0 @ erased);;
|};
  [%expect {| |}]
;;

let%expect_test "erased closure fun arg" =
  go
    {|
let f = fn (erased g : int -> erased int) -> let _ = g 1 in 2;;
fun g (x : int) : erased int = 0;;
let _ = f g;;
|};
  [%expect {| |}]
;;

let%expect_test "erased binder fun arg" =
  go
    {|
let f = fn (erased g : static int -> erased int) -> let _ = g 1 in 2;;
fun g (static x : int) : erased int = 0;;
let _ = f g;;
|};
  [%expect {| |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (erased g : int -> erased int) -> let _ = g 1 in 2;;
let _ = f (fn (x : int) -> 0);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: erased function" =
  go
    {|
fun erased f (static x : int) : erased int = x + 1;;
let _ = f 5;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: erased function applied multiple times" =
  go
    {|
fun erased inc (static x : int) : erased int = x + 1;;
let _ = inc 1;;
let _ = inc 2;;
let _ = inc 3;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: erased parameter" =
  go
    {|
let f = fn (erased x : int) -> 42;;
let _ = f 10;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: erased function with captures" =
  go
    {|
let x = 10;;
fun erased helper (static y : int) : erased int = x + y;;
let _ = helper 5;;
let _ = helper 20;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: function with erased return" =
  go
    {|
let f = fn (x : int) -> x;;
let g = (f : int -> erased int);;
let _ = g 5;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: erased function taking dynamic" =
  go
    {|
fun apply (f : int -> int) : int -> int = f;;
let inc = fn (x : int) -> x + 1;;
let _ = apply inc (5 @ dynamic);;
|};
  [%expect {| |}]
;;
