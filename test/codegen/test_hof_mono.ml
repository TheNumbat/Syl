open! Core
open! Syl

let go ?print ?(check = `Run) input = Common.codegen ?print ~check input

let%expect_test "basic: receiver applies static lambda once" =
  go
    {|
let apply = fn (static f : int -> dynamic unit) -> f 1;;
let _ = apply (fn (x : int) -> print_int x);;
|};
  [%expect {| 1 |}]
;;

let%expect_test "static erased lambda is inlined and effects still fire" =
  go
    {|
let apply = fn (static f : int -> dynamic unit) -> f 5;;
let _ = apply (fn (x : int) -> print_int x);;
|};
  [%expect {| 5 |}]
;;

let%expect_test "receiver applies lambda multiple times in one call" =
  go
    {|
let apply3 = fn (static f : int -> dynamic unit) ->
  let _ = f 1 in
  let _ = f 2 in
  f 3;;
let _ = apply3 (fn (x : int) -> print_int x);;
|};
  [%expect
    {|
    1
    2
    3
    |}]
;;

let%expect_test "receiver effects interleave with lambda body effects" =
  go
    {|
let triple = fn (static f : int -> dynamic unit) ->
  let _ = print_int 0 in
  let _ = f 1 in
  let _ = print_int 0 in
  let _ = f 2 in
  let _ = print_int 0 in
  f 3;;
let _ = triple (fn (x : int) -> print_int (10 + x));;
|};
  [%expect
    {|
    0
    11
    0
    12
    0
    13
    |}]
;;

let%expect_test "lambda body effect fires on each application" =
  go
    {|
let twice = fn (static f : int -> int) -> f 1 + f 2;;
let _ = print_int (twice (fn (x : int) -> let _ = print_int x in x * 10));;
|};
  [%expect
    {|
    1
    2
    30
    |}]
;;

let%expect_test "two distinct lambdas → two monomorphizations, distinct effects" =
  go
    {|
let apply = fn (static f : int -> dynamic unit) -> f 0;;
let _ = apply (fn (x : int) -> print_int (x + 10));;
let _ = apply (fn (x : int) -> print_int (x + 20));;
|};
  [%expect
    {|
    10
    20
    |}]
;;

let%expect_test "same named lambda at two call sites — body fires per call" =
  go
    {|
let apply = fn (static f : int -> dynamic unit) -> f 0;;
let g = fn (x : int) -> print_int (x + 100);;
let _ = apply g;;
let _ = apply g;;
|};
  [%expect
    {|
    100
    100
    |}]
;;

let%expect_test "receiver effects per call are not memoized across instantiations" =
  go
    {|
let runner = fn (static f : int -> int) -> let _ = print_int 99 in f 1;;
let g = fn (x : int) -> x;;
let _ = print_int (runner g);;
let _ = print_int (runner g);;
|};
  [%expect
    {|
    99
    1
    99
    1
    |}]
;;

let%expect_test "receiver does not apply lambda — body effects must not fire" =
  go
    {|
let unused = fn (static f : int -> int) -> 99;;
let _ = print_int (unused (fn (x : int) -> let _ = print_int 7 in x));;
|};
  [%expect {| 99 |}]
;;

let%expect_test "receiver applies lambda only on one branch — effect fires selectively" =
  go
    {|
let runner = fn (static f : int -> dynamic unit) ->
  fn (b : bool) -> if b then f 1 else ();;
let r = runner (fn (x : int) -> print_int (x + 100));;
let _ = r false;;
let _ = print_int 0;;
let _ = r true;;
|};
  [%expect
    {|
    0
    101
    |}]
;;

let%expect_test "lambda used inside match arms in receiver body" =
  go
    {|
let dispatch =
  fn (static f : int -> dynamic unit) ->
    fn (x : int) ->
      match x {
        0 -> f 100,
        1 -> f 200,
        _ -> f 300,
      }
;;

let go = dispatch (fn (k : int) -> print_int k);;

let _ = go 0;;

let _ = go 1;;

let _ = go 5;;
|};
  [%expect
    {|
    100
    200
    300
    |}]
;;

let%expect_test "receiver returns closure capturing the static lambda" =
  go
    {|
let make_iter = fn (static f : int -> dynamic unit) ->
  fn (n : int) ->
    let _ = f n in
    let _ = f (n + 1) in
    f (n + 2);;
let printer = make_iter (fn (x : int) -> print_int x);;
let _ = printer 10;;
let _ = printer 20;;
|};
  [%expect
    {|
    10
    11
    12
    20
    21
    22
    |}]
;;

let%expect_test "lambda capturing top-level static — capture preserved across instances" =
  go
    {|
let a = 1000;;
let b = 2000;;
let f1 = fn (k : int) -> print_int (k + a);;
let f2 = fn (k : int) -> print_int (k + b);;
let runner = fn (static f : int -> dynamic unit) -> f 5;;
let _ = runner f1;;
let _ = runner f2;;
|};
  [%expect
    {|
    1005
    2005
    |}]
;;

let%expect_test "lambda body composed with itself by receiver" =
  go
    {|
let twice = fn (static f : int -> int) -> fn (x : int) -> f (f x);;
let _ = print_int (twice (fn (x : int) -> x + 1) 5);;
let _ = print_int (twice (fn (x : int) -> x * 2) 5);;
|};
  [%expect
    {|
    7
    20
    |}]
;;

let%expect_test "receiver builds tuple of applications, drains in match" =
  go
    {|
let mk_pair = fn (static f : int -> int) -> (f 1, f 2);;

let _ =
  match mk_pair (fn (x : int) -> let _ = print_int x in x * 10) {
    (a, b) -> let _ = print_int a in print_int b,
  }
;;
|};
  [%expect
    {|
    1
    2
    10
    20
    |}]
;;

let%expect_test "two distinct static-lambda parameters used in alternation" =
  go
    {|
let zip = fn (static f : int -> int) -> fn (static g : int -> int) ->
  let _ = print_int (f 1) in
  let _ = print_int (g 2) in
  let _ = print_int (f 3) in
  print_int (g 4);;
let _ = zip (fn (x : int) -> x + 100) (fn (x : int) -> x * 10);;
|};
  [%expect
    {|
    101
    20
    103
    40
    |}]
;;

let%expect_test "lambda returns lambda; receiver drives both layers" =
  go
    {|
let twostage = fn (static mk : int -> int -> dynamic unit) ->
  let h = mk 100 in
  let _ = h 1 in
  let _ = h 2 in
  ();;
let _ = twostage (fn (a : int) -> let _ = print_int a in fn (b : int) -> print_int (a + b));;
|};
  [%expect
    {|
    100
    101
    102
    |}]
;;

let%expect_test "cascading mono: lambda arg is itself a binder over static int" =
  go
    {|
let invoke = fn (static f : static int -> dynamic unit) ->
  let _ = f 1 in
  let _ = f 2 in
  f 3;;
let _ = invoke (fn (static k : int) -> print_int (k + 100));;
|};
  [%expect
    {|
    101
    102
    103
    |}]
;;

let%expect_test "cascading mono: same key applied multiple times — effects per call" =
  go
    {|
let invoke = fn (static f : static int -> dynamic unit) ->
  let _ = f 5 in
  let _ = f 5 in
  f 5;;
let _ = invoke (fn (static k : int) -> let _ = print_int 99 in print_int k);;
|};
  [%expect
    {|
    99
    5
    99
    5
    99
    5
    |}]
;;

let%expect_test "lambda arg's body delegates to another binder" =
  go
    {|
let print_with = fn (static k : int) -> print_int (k + 1000);;
let invoke = fn (static f : static int -> dynamic unit) -> f 5;;
let _ = invoke (fn (static x : int) -> print_with x);;
let _ = invoke (fn (static x : int) -> print_with (x * 2));;
|};
  [%expect
    {|
    1005
    1010
    |}]
;;

let%expect_test "outer mono forwards its static lambda to an inner mono" =
  go
    {|
let inner = fn (static g : int -> dynamic unit) -> g 7;;
let outer = fn (static f : int -> dynamic unit) -> inner f;;
let _ = outer (fn (x : int) -> print_int (x + 1000));;
|};
  [%expect {| 1007 |}]
;;

let%expect_test "outer mono uses inner mono with both forwarded and fresh lambdas" =
  go
    {|
let inner = fn (static g : int -> dynamic unit) -> g 7;;
let outer = fn (static f : int -> dynamic unit) ->
  let _ = inner f in
  let _ = inner (fn (x : int) -> print_int (x * 2)) in
  f 100;;
let _ = outer (fn (x : int) -> print_int (x + 1000));;
|};
  [%expect
    {|
    1007
    14
    1100
    |}]
;;

let%expect_test "polymorphic identity binder passed as static arg" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let use_at_int = fn (static p : static erased type \ t -> t -> t) -> p int 42;;
let _ = print_int (use_at_int id);;
|};
  [%expect {| 42 |}]
;;

let%expect_test "polymorphic binder applied at multiple types in receiver" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
fun use_thrice (static p : static erased type \ t -> t -> t) : dynamic unit =
  let _ = print_int (p int 1) in
  let _ = print_bool (p bool true) in
  let _ = print_int (p int 2) in
  print_unit (p unit ());;
let _ = use_thrice id;;
|};
  [%expect
    {|
    1
    true
    2
    ()
    |}]
;;

let%expect_test "effect inside polymorphic binder body fires per type instantiation" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> let _ = print_int 99 in x;;
let use = fn (static p : static erased type \ t -> t -> t) ->
  let _ = print_int (p int 1) in
  print_bool (p bool false);;
let _ = use id;;
|};
  [%expect
    {|
    99
    1
    99
    false
    |}]
;;

let%expect_test "two distinct binders with same shape → distinct monomorphizations" =
  go
    {|
let succ = fn (static erased t : type) -> fn (x : t) -> x;;
let pred = fn (static erased t : type) -> fn (x : t) -> x;;
let use_at_int = fn (static p : static erased type \ t -> t -> t) ->
  let _ = print_int (p int 100) in
  print_int (p int 200);;
let _ = use_at_int succ;;
let _ = use_at_int pred;;
|};
  [%expect
    {|
    100
    200
    100
    200
    |}]
;;

let%expect_test "fun-form receiver with effects between applications" =
  go
    {|
fun runner (static f : int -> dynamic unit) : int -> dynamic unit =
  fn (x : int) -> let _ = f x in f (x + 1);;
let _ =
  let r = runner (fn (k : int) -> print_int k) in
  let _ = r 1 in
  r 5
;;
|};
  [%expect
    {|
    1
    2
    5
    6
    |}]
;;

let%expect_test "fun-form receiver: lambda passed at multiple call sites" =
  go
    {|
fun apply (static f : int -> dynamic unit) : dynamic unit =
  let _ = print_int 999 in f 7;;
let _ = apply (fn (x : int) -> print_int x);;
let _ = apply (fn (x : int) -> print_int (x + 1));;
|};
  [%expect
    {|
    999
    7
    999
    8
    |}]
;;

let%expect_test "construction-time effect at lambda definition site fires once" =
  go
    {|
let runner = fn (static f : int -> dynamic unit) -> fn (x : int) -> f x;;
let g_with_print =
  let _ = print_int 99 in
  fn (x : int) -> print_int x;;
let r = runner g_with_print;;
let _ = r 1;;
let _ = r 2;;
|};
  [%expect
    {|
    99
    1
    2
    |}]
;;

let%expect_test "same key invoked many times in body — effects each time" =
  go
    {|
let runner = fn (static f : int -> int) -> f 1 + f 1 + f 1;;
let g = fn (x : int) -> let _ = print_int 9 in x + 100;;
let _ = print_int (runner g);;
|};
  [%expect
    {|
    9
    9
    9
    303
    |}]
;;

let%expect_test "lambda captures top-level binding declared after receiver" =
  go
    {|
let make_apply = fn (static f : int -> int) -> fn (x : int) -> f x;;
let a = 100;;
let f1 = fn (x : int) -> x + a;;
let _ = print_int (make_apply f1 1);;
|};
  [%expect {| 101 |}]
;;

let%expect_test "recursive reify through tuple containing captured closure" =
  go
    {|
let make_apply = fn (static f : int -> int) -> fn (x : int) -> f x;;

let n = 100;;

let add_n = fn (x : int) -> x + n;;

let pair = (add_n, 0);;

let f = fn (x : int) -> match pair { (g, _) -> g x };;

let _ = print_int (make_apply f 1);;
|};
  [%expect {| 101 |}]
;;

let%expect_test "recursive reify through tuple containing captured binder" =
  go
    {|
let make_apply = fn (static f : int -> int) -> fn (x : int) -> f x;;

let n = 100;;

let mk = fn (static erased t : type) -> fn (x : int) -> x + n;;

let pair = (mk, 0);;

let f = fn (x : int) -> match pair { (p, _) -> p int x };;

let _ = print_int (make_apply f 1);;
|};
  [%expect {| 101 |}]
;;

let%expect_test "recursive reify through recursive captured function" =
  go
    {|
let make_apply = fn (static f : int -> int) -> fn (x : int) -> f x;;
let n = 100;;
fun add_n(x : int) : int = if x == 0 then n else add_n (x - 1);;
let f = fn (x : int) -> add_n x;;
let _ = print_int (make_apply f 2);;
|};
  [%expect {| 100 |}]
;;

let%expect_test "recursive reify through mutually recursive captured function" =
  go
    {|
let make_apply = fn (static f : int -> int) -> fn (x : int) -> f x;;
let n = 100;;
fun f (x : int) : int = if x == 0 then n else g (x - 1)
and g (x : int) : int = if x == 0 then n + 1 else f (x - 1);;
let h = fn (x : int) -> f x;;
let _ = print_int (make_apply h 1);;
|};
  [%expect {| 101 |}]
;;

let%expect_test "recursive reify through recursive captured function with helper" =
  go
    {|
let make_apply = fn (static f : int -> int) -> fn (x : int) -> f x;;
let n = 100;;
fun helper (x : int) : int = x + n;;
fun down (x : int) : int = if x == 0 then helper x else down (x - 1);;
let f = fn (x : int) -> down x;;
let _ = print_int (make_apply f 2);;
|};
  [%expect {| 100 |}]
;;

let%expect_test "recursive reify through tuple containing recursive captured function" =
  go
    {|
let make_apply = fn (static f : int -> int) -> fn (x : int) -> f x;;

let n = 100;;

fun down (x : int) : int =
  if x == 0 then n else down (x - 1)
;;

let pair = (down, 0);;

let f = fn (x : int) -> match pair { (g, _) -> g x };;

let _ = print_int (make_apply f 2);;
|};
  [%expect {| 100 |}]
;;

let%expect_test "recursive reify through three-function captured cycle" =
  go
    {|
let make_apply = fn (static f : int -> int) -> fn (x : int) -> f x;;
let n = 100;;
fun f1 (x : int) : int = if x == 0 then n else f2 (x - 1)
and f2 (x : int) : int = if x == 0 then n + 1 else f3 (x - 1)
and f3 (x : int) : int = if x == 0 then n + 2 else f1 (x - 1);;
let h = fn (x : int) -> f1 x;;
let _ = print_int (make_apply h 2);;
|};
  [%expect {| 102 |}]
;;

let%expect_test "recursive reify keeps one-way helper out of captured cycle" =
  go
    {|
let make_apply = fn (static f : int -> int) -> fn (x : int) -> f x;;
let n = 100;;
fun helper (x : int) : int = x + n;;
fun f1 (x : int) : int = if x == 0 then helper x else f2 (x - 1)
and f2 (x : int) : int = if x == 0 then n + 1 else f1 (x - 1);;
let h = fn (x : int) -> f1 x;;
let _ = print_int (make_apply h 2);;
|};
  [%expect {| 100 |}]
;;

let%expect_test "lambda captures local let binding outside receiver's scope" =
  go
    {|
let runner = fn (static f : int -> dynamic unit) ->
  fn (x : int) -> let _ = f x in f (x + 1);;
let _ =
  let n = 100 in
  let g = fn (k : int) -> print_int (k + n) in
  let r = runner g in
  let _ = r 1 in
  r 5
;;
|};
  [%expect
    {|
    101
    102
    105
    106
    |}]
;;

let%expect_test "fun-form receiver, lambda captures binding declared after fun" =
  go
    {|
fun runner (static f : int -> dynamic unit) : int -> dynamic unit =
  fn (x : int) -> let _ = f x in f (x + 1);;
let n = 100;;
let g = fn (k : int) -> print_int (k + n);;
let r = runner g;;
let _ = r 1;;
|};
  [%expect
    {|
    101
    102
    |}]
;;

let%expect_test "recursive reify through captured top-level closure" =
  go
    {|
let make_apply = fn (static f : int -> int) -> fn (x : int) -> f x;;
let n = 100;;
let add_n = fn (x : int) -> x + n;;
let f = fn (x : int) -> add_n x;;
let _ = print_int (make_apply f 1);;
|};
  [%expect {| 101 |}]
;;

let%expect_test "recursive reify through captured local closure" =
  go
    {|
let make_apply = fn (static f : int -> int) -> fn (x : int) -> f x;;
let _ =
  let n = 100 in
  let add_n = fn (x : int) -> x + n in
  let f = fn (x : int) -> add_n x in
  print_int (make_apply f 1)
;;
|};
  [%expect {| 101 |}]
;;

let%expect_test "tuple of closures as a specialization key" =
  go
    {|
let use = fn (static p : (int -> int) ^ (int -> int)) -> match p { (f, g) -> f 1 + g 10 };;

let _ = print_int (use ((fn (x : int) -> x + 1), (fn (y : int) -> y * 2)));;
|};
  [%expect {| 22 |}]
;;

let%expect_test "external as a specialization key" =
  go
    {|
let use = fn (static h : int -> dynamic unit) -> h 5;;
let _ = use print_int;;
|};
  [%expect {| 5 |}]
;;

let%expect_test "static tuple of closures quoted as a key" =
  go
    {|
let p = ((fn (x : int) -> x + 1), (fn (x : int) -> x * 2)) @ static;;

let use = fn (static q : (int -> int) ^ (int -> int)) -> match q { (f, g) -> f (g 5) };;

let _ = print_int (use p);;
|};
  [%expect {| 11 |}]
;;

let%expect_test "match-bound static closure keys a specialization" =
  go
    {|
let use = fn (static h : int -> int) -> h 2;;

let p = ((fn (x : int) -> x + 1), 5) @ static;;

let _ = match p { (f, k) -> print_int (use f + k) };;
|};
  [%expect {| 8 |}]
;;

let%expect_test "one closure key dispatched at two receiver families" =
  go
    {|
let use1 = fn (static h : int -> int) -> h 1;;
let use2 = fn (static h : int -> int) -> h 2;;
let f = fn (x : int) -> x * 10;;
let _ = print_int (use1 f + use2 f);;
|};
  [%expect {| 30 |}]
;;

let%expect_test "negative and large int keys" =
  go
    {|
let use = fn (static k : int) -> k + 1;;
let _ = print_int (use (0 - 5));;
let _ = print_int (use 4611686018427387904);;
|};
  [%expect
    {|
    -4
    4611686018427387905
    |}]
;;
