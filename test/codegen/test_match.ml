open! Core
open! Syl

let go ?print ?(check = `Run) input = Common.codegen ?print ~check input

let%expect_test "if static with mismatched branch types without annotation" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else true;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent arrow applied to matching function" =
  go
    {|
let apply_type = fn (static f : static erased type \ t -> t -> t) -> f int;;
let _ = apply_type (fn (static erased t : type) -> fn (x : t) -> x);;
|};
  [%expect {| |}]
;;

let%expect_test "match simple bool" =
  go
    {|
fun f (x : bool) : int =
  match x with
  | true -> 1
  | false -> 2
;;
let _ = assert (f true == 1);;
let _ = assert (f false == 2);;
|};
  [%expect {| |}]
;;

let%expect_test "match tuple binding" =
  go
    {|
let _ = match (1, true) with | (x, y) -> assert (x == 1);;
|};
  [%expect {| |}]
;;

let%expect_test "match tuple binding second element" =
  go
    {|
let _ = match (1, 2) with | (x, y) -> assert (y == 2);;
|};
  [%expect {| |}]
;;

let%expect_test "match 3-element tuple" =
  go
    {|
let _ = match (1, 2, 3) with | (a, b, c) -> assert (a + b + c == 6);;
|};
  [%expect {| |}]
;;

let%expect_test "match nested tuple" =
  go
    {|
let _ = match ((1, 2), (3, 4)) with | ((a, b), (c, d)) -> assert (a + b + c + d == 10);;
|};
  [%expect {| |}]
;;

let%expect_test "match tuple with literal patterns" =
  go
    {|
fun f (x : int ^ bool) : int =
  match x with
  | (0, true) -> 1
  | (0, false) -> 2
  | (n, true) -> n + 10
  | (n, false) -> n + 20
;;
let _ = assert (f (0, true) == 1);;
let _ = assert (f (0, false) == 2);;
let _ = assert (f (5, true) == 15);;
let _ = assert (f (5, false) == 25);;
|};
  [%expect {| |}]
;;

let%expect_test "match deeply nested tuple" =
  go
    {|
fun f (x : ((int ^ int) ^ int) ^ int) : int =
  match x with
  | (((a, b), c), d) -> a * 1000 + b * 100 + c * 10 + d
;;
let _ = assert (f (((1, 2), 3), 4) == 1234);;
|};
  [%expect {| |}]
;;

let%expect_test "match tuple with literal in nested position" =
  go
    {|
fun g (x : (int ^ bool) ^ int) : int =
  match x with
  | ((0, true), n) -> n
  | ((0, false), n) -> 0 - 1
  | ((x, b), n) -> x
;;
let _ = assert (g ((0, true), 42) == 42);;
let _ = assert (g ((0, false), 42) == 0 - 1);;
let _ = assert (g ((7, true), 42) == 7);;
|};
  [%expect {| |}]
;;

let%expect_test "match tuple compiles" =
  go
    {|
let x = match (1, true) with | (x, y) -> x;;
let _ = print_int x;;
|};
  [%expect {| 1 |}]
;;

let%expect_test "match in closure captures outer var" =
  go
    {|
builtin add = syl_int_add;;
fun outer (z : int) : int -> int =
  fn (x : int) ->
    match x with
    | 0 -> z
    | n -> add (n, z)
;;
let _ = assert (outer 42 0 == 42);;
let _ = assert (outer 42 5 == 47);;
|};
  [%expect {| |}]
;;

let%expect_test "match pattern binding shadows outer var" =
  go
    {|
builtin add = syl_int_add;;
fun outer (z : int) : int -> int =
  fn (x : int) ->
    match x with
    | 0 -> z
    | z -> add (z, z)
;;
let _ = assert (outer 100 0 == 100);;
let _ = assert (outer 100 7 == 14);;
|};
  [%expect {| |}]
;;

let%expect_test "or-pattern shared body captures outer var" =
  go
    {|
fun outer (z : int) : bool -> int =
  fn (b : bool) ->
    match b with
    | (true | false) -> z
;;
let _ = assert (outer 77 true == 77);;
let _ = assert (outer 77 false == 77);;
|};
  [%expect {| |}]
;;

let%expect_test "or-pattern same-named binding captures outer var" =
  go
    {|
builtin add = syl_int_add;;
fun outer (z : int) : int ^ int -> int =
  fn (p : int ^ int) ->
    match p with
    | ((x, 0) | (0, x)) -> add (x, z)
    | (a, b) -> add (z, add (a, b))
;;
let _ = assert (outer 10 (5, 0) == 15);;
let _ = assert (outer 10 (0, 7) == 17);;
let _ = assert (outer 10 (3, 4) == 17);;
|};
  [%expect {| |}]
;;

let%expect_test "match on complex scrutinee" =
  go
    {|
builtin add = syl_int_add;;
fun test (n : int) : int =
  match (add (n, n), add (n, 1)) with
  | (0, _) -> 100
  | (_, 0) -> 200
  | (a, b) -> add (a, b)
;;
let _ = assert (test 0 == 100);;
let _ = assert (test 5 == 16);;
let _ = assert (test (0 - 1) == 200);;
|};
  [%expect {| |}]
;;

let%expect_test "nested match" =
  go
    {|
fun classify (p : int ^ int) : int =
  match p with
  | (0, y) ->
    (match y with
     | 0 -> 0
     | _ -> 1)
  | (_, 0) -> 2
  | _ -> 3
;;
let _ = assert (classify (0, 0) == 0);;
let _ = assert (classify (0, 5) == 1);;
let _ = assert (classify (5, 0) == 2);;
let _ = assert (classify (5, 5) == 3);;
|};
  [%expect {| |}]
;;

let%expect_test "or-pattern binds same name in different positions" =
  go
    {|
builtin add = syl_int_add;;
fun swap_zero (p : int ^ int) : int =
  match p with
  | ((x, 0) | (0, x)) -> add (x, x)
  | (a, b) -> add (a, b)
;;
let _ = assert (swap_zero (7, 0) == 14);;
let _ = assert (swap_zero (0, 11) == 22);;
let _ = assert (swap_zero (3, 4) == 7);;
let _ = assert (swap_zero (0, 0) == 0);;
|};
  [%expect {| |}]
;;

let%expect_test "or-pattern with three branches" =
  go
    {|
builtin add = syl_int_add;;
fun sum_nonzero (t : int ^ int ^ int) : int =
  match t with
  | (0, x, y) -> add (x, y)
  | (x, 0, y) -> add (x, y)
  | (x, y, 0) -> add (x, y)
  | (a, b, c) -> add (a, add (b, c))
;;
let _ = assert (sum_nonzero (0, 3, 4) == 7);;
let _ = assert (sum_nonzero (5, 0, 6) == 11);;
let _ = assert (sum_nonzero (7, 8, 0) == 15);;
let _ = assert (sum_nonzero (1, 2, 3) == 6);;
|};
  [%expect {| |}]
;;

let%expect_test "pattern binding shadows outer" =
  go
    {|
builtin add = syl_int_add;;
fun make_adder (base : int) : int -> int =
  fn (x : int) ->
    match x with
    | 0 -> base
    | base -> add (base, base)
;;
let f = make_adder 100;;
let _ = assert (f 0 == 100);;
let _ = assert (f 7 == 14);;
|};
  [%expect {| |}]
;;

let%expect_test "match arm returns tuple" =
  go
    {|
fun swap (p : int ^ int) : int ^ int =
  match p with
  | (a, b) -> (b, a)
;;
let _ =
  match swap (10, 20) with
  | (x, y) -> assert (x == 20)
;;
let _ =
  match swap (10, 20) with
  | (x, y) -> assert (y == 10)
;;
|};
  [%expect {| |}]
;;

let%expect_test "match with many int literals plus wildcard" =
  go
    {|
fun day_code (d : int) : int =
  match d with
  | 0 -> 100 | 1 -> 101 | 2 -> 102 | 3 -> 103
  | 4 -> 104 | 5 -> 105 | 6 -> 106
  | _ -> 999
;;
let _ = assert (day_code 0 == 100);;
let _ = assert (day_code 3 == 103);;
let _ = assert (day_code 6 == 106);;
let _ = assert (day_code 42 == 999);;
let _ = assert (day_code (0 - 1) == 999);;
|};
  [%expect {| |}]
;;

let%expect_test "match on unit" =
  go
    {|
fun always42 (u : unit) : int =
  match u with | () -> 42
;;
let _ = assert (always42 () == 42);;
|};
  [%expect {| |}]
;;

let%expect_test "nested match as scrutinee" =
  go
    {|
builtin add = syl_int_add;;
fun weird (b : bool) : int =
  match (match b with | true -> (1, 2) | false -> (3, 4)) with
  | (x, y) -> add (x, y)
;;
let _ = assert (weird true == 3);;
let _ = assert (weird false == 7);;
|};
  [%expect {| |}]
;;

let%expect_test "recursive match" =
  go
    {|
builtin add = syl_int_add;;
builtin sub = syl_int_sub;;
fun sum_to (n : int) : int =
  match n with
  | 0 -> 0
  | _ -> add (n, sum_to (sub (n, 1)))
;;
let _ = assert (sum_to 0 == 0);;
let _ = assert (sum_to 10 == 55);;
let _ = assert (sum_to 100 == 5050);;
|};
  [%expect {| |}]
;;

let%expect_test "match in mutual recursion" =
  go
    {|
builtin sub = syl_int_sub;;
fun even (n : int) : bool =
  match n with | 0 -> true | _ -> odd (sub (n, 1))
and odd (n : int) : bool =
  match n with | 0 -> false | _ -> even (sub (n, 1))
;;
let _ = assert (even 0);;
let _ = assert (!(even 7));;
let _ = assert (even 100);;
let _ = assert (odd 3);;
let _ = assert (!(odd 4));;
|};
  [%expect {| |}]
;;

let%expect_test "match arm returns a closure" =
  go
    {|
builtin add = syl_int_add;;
builtin sub = syl_int_sub;;
fun make_op (op : int) : int -> int -> int =
  match op with
  | 0 -> fn (x : int) -> fn (y : int) -> add (x, y)
  | _ -> fn (x : int) -> fn (y : int) -> sub (x, y)
;;
let _ = assert (make_op 0 5 3 == 8);;
let _ = assert (make_op 1 5 3 == 2);;
let _ = assert (make_op 99 10 4 == 6);;
|};
  [%expect {| |}]
;;

let%expect_test "match full 4-way bool tuple" =
  go
    {|
fun classify (p : bool ^ bool) : int =
  match p with
  | (true, true) -> 11
  | (true, false) -> 10
  | (false, true) -> 1
  | (false, false) -> 0
;;
let _ = assert (classify (true, true) == 11);;
let _ = assert (classify (true, false) == 10);;
let _ = assert (classify (false, true) == 1);;
let _ = assert (classify (false, false) == 0);;
|};
  [%expect {| |}]
;;

let%expect_test "match arm priority" =
  go
    {|
fun first_match (p : int ^ int) : int =
  match p with
  | (0, y) -> 1
  | (x, 0) -> 2
  | (a, b) -> 3
;;
let _ = assert (first_match (0, 0) == 1);;
let _ = assert (first_match (0, 5) == 1);;
let _ = assert (first_match (5, 0) == 2);;
let _ = assert (first_match (5, 5) == 3);;
|};
  [%expect {| |}]
;;

let%expect_test "match with pattern binding used in collapsed body" =
  go
    {|
fun sum (t : int ^ int) : int =
  match t with
  | (a, b) -> a + b
;;
let _ = assert (sum (3, 4) == 7);;
|};
  [%expect {| |}]
;;

let%expect_test "deeply nested match on nested tuples" =
  go
    {|
let _ = match ((1, 2), (3, 4)) with
  | ((a, b), (c, d)) -> (fn (static erased t : type) -> ())
;;
|};
  [%expect {| |}]
;;

let%expect_test "nested match where outer stays as split" =
  go
    {|
let _ = (fn (x : bool) -> match x with
  | true -> (match true with
             | true -> 10
             | false -> 20)
  | false -> 30) true
;;
|};
  [%expect {| |}]
;;

let%expect_test "match scrutinee is itself a match" =
  go
    {|
let _ = match (match true with | true -> false | false -> true) with
  | true -> (fn (static erased t : type) -> ())
  | false -> (fn (static erased t : type) -> ())
;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: thunk pattern" =
  go
    {|
let f = fn (static x : int) -> x + 1;;
let _ = f 10;;
|};
  [%expect {| |}]
;;
