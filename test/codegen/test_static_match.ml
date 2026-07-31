open! Core
open! Syl

let go ?print ?(check = `Run) input = Common.codegen ?print ~check input

(* Count runtime conditionals in the generated program (prelude excluded). *)
let branches input =
  let dst = Common.desugar input in
  let tst = Typecheck.typecheck_exn dst in
  let sst = Simplify.simplify tst in
  let lst = Linearize.linearize sst in
  let cpp = Common.strip_prelude (Codegen.cpp lst) in
  let branches = String.substr_index_all cpp ~may_overlap:false ~pattern:"if(" |> List.length in
  print_s [%message (branches : int)]
;;

let%expect_test "match static selects the arm at compile time" =
  go
    {|
let x = 1 @ static;;

let _ =
  print_int
    (match static x {
       1 -> 42,
       _ -> 0,
     })
;;
|};
  [%expect {| 42 |}]
;;

let%expect_test "match static monomorphizes per key" =
  go
    {|
fun f (static n : int) : int =
  match static n {
    0 -> 100,
    _ -> 200,
  }
;;

let _ = print_int (f 0);;

let _ = print_int (f 5);;
|};
  [%expect
    {|
    100
    200
    |}]
;;

let%expect_test "dynamic match emits a conditional" =
  branches
    {|
let p = (1, 2);;

let _ =
  print_int
    (match p {
       (1, y) -> y,
       _ -> 0,
     })
;;
|};
  [%expect {| (branches 1) |}]
;;

let%expect_test "match static projects components without a conditional" =
  branches
    {|
let p = (1, 2) @ static;;

let _ =
  print_int
    (match static p {
       (1, y) -> y,
       _ -> 0,
     })
;;
|};
  [%expect {| (branches 0) |}]
;;

let%expect_test "match erased emits neither conditional nor projections" =
  branches
    {|
let p = (1, 2) @ static;;

let _ =
  print_int
    (match erased p {
       (1, y) -> 10,
       _ -> 20,
     })
;;
|};
  [%expect {| (branches 0) |}]
;;

let%expect_test "match static or pattern binds the matched alternative" =
  go
    {|
let p = (0, 7) @ static;;

let _ =
  print_int
    (match static p {
       (0, y) | (y, 0) -> y,
       _ -> 99,
     })
;;
|};
  [%expect {| 7 |}]
;;

let%expect_test "match static nested tuple projections" =
  go
    {|
let q = ((1, 2), 3) @ static;;

let _ = print_int (match static q { ((a, b), c) -> a + b + c });;
|};
  [%expect {| 6 |}]
;;

let%expect_test "static recursion through match static" =
  go
    {|
fun fact (static n : int) : int =
  match static n {
    0 -> 1,
    _ -> n * fact (n - 1),
  }
;;

let _ = print_int (fact 5);;
|};
  [%expect {| 120 |}]
;;

let%expect_test "unreachable in a dead arm is skipped per instance" =
  go
    {|
fun f (static n : int) : int =
  match static n {
    0 -> unreachable,
    _ -> n,
  }
;;

let _ = print_int (f 3);;
|};
  [%expect {| 3 |}]
;;

let%expect_test "match static under a dynamic closure" =
  go
    {|
let k = 3 @ static;;

let f =
  fn (x : int) ->
    match static k {
      3 -> x,
      _ -> 0,
    }
;;

let _ = print_int (f 9);;
|};
  [%expect {| 9 |}]
;;

let%expect_test "dependent match monomorphizes types per key" =
  go
    {|
let g = fn (static n : int) -> fn (x : if n == 0 then int else bool) -> x;;

let h =
  fn (static n : int) ->
    match static n {
      0 -> g n 5,
      1 -> g n true,
      _ -> 0,
    }
;;

let _ = print_int (h 0);;

let _ = print_bool (h 1);;

let _ = print_int (h 2);;
|};
  [%expect
    {|
    5
    true
    0
    |}]
;;

let%expect_test "match erased bindings feed types" =
  go
    {|
let pick = fn (static erased p : type ^ type) -> match erased p { (t, _) -> t };;

let _ = print_int (0 : pick ((int, bool) @ erased));;
|};
  [%expect {| 0 |}]
;;

let%expect_test "computed scrutinee resolves per instance" =
  go
    {|
fun f (static n : int) : int =
  match static (n % 3) {
    0 -> n / 3,
    1 -> n * 2,
    _ -> n + 1,
  }
;;

let _ = print_int (f 9);;

let _ = print_int (f 4);;

let _ = print_int (f 5);;
|};
  [%expect
    {|
    3
    8
    6
    |}]
;;

let%expect_test "double static recursion reuses instances" =
  go
    {|
fun fib (static n : int) : int =
  match static n {
    0 -> 0,
    1 -> 1,
    _ -> fib (n - 1) + fib (n - 2),
  }
;;

let _ = print_int (fib 10);;
|};
  [%expect {| 55 |}]
;;

let%expect_test "static mutual recursion through match static" =
  go
    {|
fun even (static n : int) : bool =
  match static n {
    0 -> true,
    _ -> odd (n - 1),
  }
and odd (static n : int) : bool =
  match static n {
    0 -> false,
    _ -> even (n - 1),
  }
;;

let _ = print_bool (even 10);;

let _ = print_bool (odd 7);;
|};
  [%expect
    {|
    true
    true
    |}]
;;

let%expect_test "match static on bool and unit scrutinees" =
  go
    {|
fun f (static b : bool) : int =
  match static b {
    true -> 1,
    false -> 2,
  }
;;

let _ = print_int (f true);;

let _ = print_int (f false);;

let u = () @ static;;

let _ = print_int (match static u { () -> 5 });;
|};
  [%expect
    {|
    1
    2
    5
    |}]
;;

let%expect_test "partially known scrutinee refutes arms at declaration" =
  go
    {|
fun f (static n : int) : int =
  match static (n, 0) {
    (_, 1) -> unreachable,
    (x, 0) -> x,
    _ -> unreachable,
  }
;;

let _ = print_int (f 42);;
|};
  [%expect {| 42 |}]
;;

let%expect_test "pattern binding shadows the scrutinee" =
  go
    {|
fun f (static p : int ^ int) : int =
  match static p { (p, _) -> p }
;;

let _ = print_int (f ((7, 8) @ static));;
|};
  [%expect {| 7 |}]
;;

let%expect_test "match static binding can be rescrutinized" =
  go
    {|
let p = (1, 2) @ static;;

let _ =
  print_int
    (match static p {
       (a, _) ->
         match static a {
           1 -> 10,
           _ -> 20,
         },
     })
;;
|};
  [%expect {| 10 |}]
;;

let%expect_test "or pattern selects a different alternative per instance" =
  go
    {|
fun f (static p : int ^ int) : int =
  match static p {
    (0, y) | (y, 0) -> y * 10 + 1,
    _ -> 99,
  }
;;

let _ = print_int (f ((0, 7) @ static));;

let _ = print_int (f ((7, 0) @ static));;

let _ = print_int (f ((0, 0) @ static));;

let _ = print_int (f ((1, 2) @ static));;
|};
  [%expect
    {|
    71
    71
    1
    99
    |}]
;;

let%expect_test "or pattern bindings with incompatible types resolve per instance" =
  go
    {|
let f =
  fn (static p : int ^ bool) ->
    match static p {
      (0, y) | (y, true) -> y,
      _ -> 0,
    }
;;

let _ = print_bool (f ((0, false) @ static));;

let _ = print_int (f ((5, true) @ static));;

let _ = print_int (f ((3, false) @ static));;
|};
  [%expect
    {|
    false
    5
    0
    |}]
;;

let%expect_test "or patterns bind types at different paths" =
  go
    {|
let pick =
  fn (static erased b : bool) ->
    fn (static erased p : type ^ type) ->
      match erased (b, p) { (true, (t, _)) | (false, (_, t)) -> t }
;;

let _ = print_int (0 : pick (true @ erased) ((int, bool) @ erased));;

let _ = print_bool (true : pick (false @ erased) ((int, bool) @ erased));;
|};
  [%expect
    {|
    0
    true
    |}]
;;

let%expect_test "stuck match is a monomorphization key" =
  go
    {|
fun g (static m : int) : int =
  m * 10
;;

fun f (static n : int) : int =
  g
    (match static n {
       0 -> 1,
       _ -> 2,
     })
  + n
;;

let _ = print_int (f 0);;

let _ = print_int (f 7);;
|};
  [%expect
    {|
    10
    27
    |}]
;;

let%expect_test "stuck match as the scrutinee of another static match" =
  go
    {|
fun f (static n : int) : int =
  match static (match static n {
                  0 -> 1,
                  _ -> 0,
                }) {
    0 -> 100,
    _ -> 200,
  }
;;

let _ = print_int (f 0);;

let _ = print_int (f 3);;
|};
  [%expect
    {|
    200
    100
    |}]
;;

let%expect_test "arm refinement resolves a nested match on the same scrutinee" =
  go
    {|
fun f (static n : int) : int =
  match static n {
    0 ->
      (match static n {
         0 -> 10,
         _ -> unreachable,
       }),
    _ -> n,
  }
;;

let _ = print_int (f 0);;

let _ = print_int (f 5);;
|};
  [%expect
    {|
    10
    5
    |}]
;;

let%expect_test "match static selects between closures" =
  go
    {|
fun pick (static n : int) : int -> dynamic int =
  match static n {
    0 -> fn (x : int) -> x + 1,
    _ -> fn (x : int) -> x * 2,
  }
;;

let _ = print_int (pick 0 10);;

let _ = print_int (pick 3 10);;
|};
  [%expect
    {|
    11
    20
    |}]
;;

let%expect_test "projected bindings are captured by a closure" =
  go
    {|
let p = (3, 4) @ static;;

let addp = match static p { (x, y) -> fn (z : int) -> x + y + z };;

let _ = print_int (addp 10);;
|};
  [%expect {| 17 |}]
;;

let%expect_test "static match around a dynamic match" =
  go
    {|
fun f (static n : int) : int -> dynamic int =
  fn (x : int) ->
    match static n {
      0 -> x,
      _ ->
        (match x {
           0 -> n,
           _ -> x * n,
         }),
    }
;;

let _ = print_int (f 0 5);;

let _ = print_int (f 3 0);;

let _ = print_int (f 3 2);;
|};
  [%expect
    {|
    5
    3
    6
    |}]
;;

let%expect_test "only the dynamic inner match emits a conditional" =
  branches
    {|
fun f (static n : int) : int -> dynamic int =
  fn (x : int) ->
    match static n {
      0 -> x,
      _ ->
        (match x {
           0 -> n,
           _ -> x * n,
         }),
    }
;;

let _ = print_int (f 0 5);;

let _ = print_int (f 3 0);;

let _ = print_int (f 3 2);;
|};
  [%expect {| (branches 1) |}]
;;

let%expect_test "match erased on an erased scrutinee yields runtime values" =
  go
    {|
let f =
  fn (static erased n : int) ->
    match erased n {
      0 -> 111,
      _ -> 222,
    }
;;

let _ = print_int (f 0);;

let _ = print_int (f 9);;
|};
  [%expect
    {|
    111
    222
    |}]
;;

let%expect_test "erased tuple bindings drive further erased matches" =
  go
    {|
let sel =
  fn (static erased m : int) ->
    match erased m {
      0 -> 1,
      _ -> 2,
    }
;;

let f = fn (static erased p : int ^ int) -> match erased p { (a, b) -> sel a * 10 + sel b };;

let _ = print_int (f ((0, 3) @ erased));;

let _ = print_int (f ((3, 0) @ erased));;
|};
  [%expect
    {|
    12
    21
    |}]
;;

let%expect_test "match-typed return shape varies per instance" =
  go
    {|
let shape =
  fn (static erased n : int) ->
    match erased n {
      0 -> int,
      _ -> int ^ int,
    }
;;

fun make (static n : int) : shape n =
  match static n {
    0 -> n + 7,
    _ -> (n, n + 1),
  }
;;

let _ = print_int (make 0);;

let _ = (match static (make 3) { (a, b) -> print_int (a + b) });;
|};
  [%expect
    {|
    7
    7
    |}]
;;

let%expect_test "assert erased sees the refined scrutinee" =
  go
    {|
fun f (static n : int) : int =
  match static n {
    0 -> let _ = assert erased (n == 0) in 0,
    _ -> n,
  }
;;

let _ = print_int (f 0);;

let _ = print_int (f 8);;
|};
  [%expect
    {|
    0
    8
    |}]
;;

let%expect_test "if erased skips monomorphizing the dead branch" =
  go
    {|
fun f (static n : int) : int =
  if erased n == 0
  then 1
  else
    match static n {
      0 -> unreachable,
      _ -> n,
    }
;;

let _ = print_int (f 0);;

let _ = print_int (f 4);;
|};
  [%expect
    {|
    1
    4
    |}]
;;

let%expect_test "match on a dead scrutinee is dead code" =
  go
    {|
fun f (static n : int) : int =
  match static n {
    0 ->
      (match static (unreachable : int) {
         0 -> 1,
         _ -> 2,
       }),
    _ -> n,
  }
;;

let _ = print_int (f 5);;
|};
  [%expect {| 5 |}]
;;

let%expect_test "dead arms keep the known scrutinee value" =
  go
    {|
let g = fn (static m : int) -> fn (x : if m == 3 then int else bool) -> x;;

let k = 3 @ static;;

fun f (static n : int) : int =
  match static (k, 0) {
    (m, 1) -> g m 5,
    (m, 0) -> m + n,
    _ -> 0,
  }
;;

let _ = print_int (f 4);;
|};
  [%expect {| 7 |}]
;;

let%expect_test "tuple scrutinee components are refined per arm" =
  go
    {|
let g = fn (static m : int) -> fn (x : if m == 0 then int else bool) -> x;;

let h =
  fn (static a : int) ->
    fn (static b : int) ->
      match static (a, b) {
        (0, y) -> g a 5,
        (x, 0) -> x,
        _ -> 7,
      }
;;

let _ = print_int (h 0 1);;

let _ = print_int (h 3 0);;

let _ = print_int (h 2 2);;
|};
  [%expect
    {|
    5
    3
    7
    |}]
;;

let%expect_test "nested tuple scrutinee refines inner components" =
  go
    {|
let g = fn (static m : int) -> fn (x : if m == 0 then int else bool) -> x;;

let h =
  fn (static a : int) ->
    fn (static c : int) ->
      match static ((a, 1), c) {
        ((0, y), 2) -> g a 5,
        _ -> 9,
      }
;;

let _ = print_int (h 0 2);;

let _ = print_int (h 0 3);;
|};
  [%expect
    {|
    5
    9
    |}]
;;

let%expect_test "mode-annotated scrutinee still refines" =
  go
    {|
let g = fn (static m : int) -> fn (x : if m == 0 then int else bool) -> x;;

let h =
  fn (static a : int) ->
    match static (a @ static) {
      0 -> g a 5,
      _ -> 9,
    }
;;

let _ = print_int (h 0);;

let _ = print_int (h 4);;
|};
  [%expect
    {|
    5
    9
    |}]
;;
