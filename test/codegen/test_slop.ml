open! Core
open! Syl

let go ?print ?(check = `Run) input = Common.codegen ?print ~check input

let%expect_test "match materializes zero-size tuple binding" =
  go
    {|
let _ = match ((), 1) { (u, n) -> n };;
|};
  [%expect {| |}]
;;

let%expect_test "match wildcard zero-size tuple slots still compile" =
  go
    {|
fun project (t : unit ^ int ^ unit ^ bool) : int =
  match t { (_, n, _, b) -> if b then n else 0 }
;;

let _ = assert (project ((), 42, (), true) == 42);;

let _ = assert (project ((), 42, (), false) == 0);;
|};
  [%expect {| |}]
;;

let%expect_test "static tuple specialization with unit key compiles" =
  go
    {|
let use_static_tuple =
  fn (static p : unit ^ int ^ bool) ->
    match p { (_, n, flag) -> if flag then print_int n else print_int 0 }
;;

let _ = use_static_tuple ((), 9, true);;

let _ = use_static_tuple ((), 9, false);;
|};
  [%expect
    {|
    9
    0
    |}]
;;

let%expect_test "nested match materializes zero-size tuple binding" =
  go
    {|
let _ = match (((), true), 7) { ((u, flag), n) -> if flag then n else 0 };;
|};
  [%expect {| |}]
;;

let%expect_test "all-unit tuple pattern binding is zero-size" =
  go
    {|
let _ = match ((), ()) { (left, right) -> () };;
|};
  [%expect {| |}]
;;

let%expect_test "negative static int specialization key reaches clang" =
  go
    {|
let print_static = fn (static x : int) -> print_int x;;
let _ = print_static (0 - 1);;
|};
  [%expect {| -1 |}]
;;

let%expect_test "negative int inside static tuple specialization key reaches clang" =
  go
    {|
let sum_static_pair = fn (static p : int ^ int) -> match p { (a, b) -> print_int (a + b) };;

let _ = sum_static_pair (0 - 1, 2);;
|};
  [%expect {| 1 |}]
;;

let%expect_test "type-level tuple get drives codegen at two instantiations" =
  go
    {|
builtin get = syl_type_tuple_get;;
fun id_at (static erased i : int) : get (int ^ bool, i) -> get (int ^ bool, i) =
  fn (x : get (int ^ bool, i)) -> x
;;
let _ = assert (id_at 0 12 == 12);;
let _ = assert (id_at 1 true);;
|};
  [%expect {| |}]
;;

let%expect_test "closure stored after zero-size tuple slot" =
  go
    {|
fun make (tag : int) : unit ^ (int -> int) =
  ((), fn (x : int) -> x + tag)
;;

let apply = fn (p : unit ^ (int -> int)) -> match p { (_, f) -> f 10 };;

let _ = assert (apply (make 5) == 15);;

let _ = assert (apply (make 0) == 10);;
|};
  [%expect {| |}]
;;

let%expect_test "match arm closure captures pattern bindings" =
  go
    {|
fun make_adder (p : int ^ int) : int -> int =
  match p { (a, b) -> fn (x : int) -> x + a + b }
;;

let add_pair = make_adder (3, 4);;

let _ = assert (add_pair 10 == 17);;
|};
  [%expect {| |}]
;;

let%expect_test "recursive function captures only zero-size outer value" =
  go
    {|
let marker = ();;

fun loop (n : int) : int =
  match (marker, n) {
    (_, 0) -> 0,
    (_, k) -> loop (k - 1),
  }
;;

let _ = assert (loop 3 == 0);;
|};
  [%expect {| |}]
;;

let%expect_test "nested tuple match skips zero-size fields" =
  go
    {|
fun classify (t : (unit ^ int) ^ (unit ^ bool)) : int =
  match t {
    ((_, n), (_, true)) -> n,
    ((_, n), (_, false)) -> 0 - n,
  }
;;

let _ = assert (classify (((), 8), ((), true)) == 8);;

let _ = assert (classify (((), 8), ((), false)) == 0 - 8);;
|};
  [%expect {| |}]
;;

(* ----------------------------------------------------------------------------
   Section 1: Integer literal edge cases
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: max int64 literal" =
  go
    {|
let _ = print_int 9223372036854775807;;
|};
  [%expect {| 9223372036854775807 |}]
;;

let%expect_test "fuzz: -max int64 literal" =
  go
    {|
let _ = print_int (-9223372036854775807);;
|};
  [%expect {| -9223372036854775807 |}]
;;

let%expect_test "fuzz: min int64 via -max - 1" =
  go
    {|
let _ = print_int (-9223372036854775807 - 1);;
|};
  [%expect {| -9223372036854775808 |}]
;;

let%expect_test "fuzz: zero literal" =
  go
    {|
let _ = print_int 0;;
let _ = print_int (-0);;
|};
  [%expect
    {|
    0
    0
    |}]
;;

let%expect_test "fuzz: integer overflow at compile time wraps" =
  go
    {|
let _ = print_int (9223372036854775807 + 1);;
|};
  [%expect {| -9223372036854775808 |}]
;;

let%expect_test "fuzz: chained negations" =
  go
    {|
let _ = print_int (- (- (- (- 5))));;
|};
  [%expect {| 5 |}]
;;

let%expect_test "fuzz: deeply nested parentheses" =
  go
    {|
let _ = print_int ((((((((((42))))))))));;
|};
  [%expect {| 42 |}]
;;

let%expect_test "fuzz: bool negation chain" =
  go
    {|
let _ = print_bool (! (! (! (! true))));;
|};
  [%expect {| true |}]
;;

let%expect_test "fuzz: mixed comparisons" =
  go
    {|
let _ = print_bool (1 < 2 && 2 <= 2 && 3 > 2 && 3 >= 3 && 1 == 1 && 1 != 2);;
|};
  [%expect {| true |}]
;;

let%expect_test "fuzz: division of negative by positive" =
  go
    {|
let _ = print_int (-7 / (2 @ dynamic));;
let _ = print_int (-7 % (2 @ dynamic));;
|};
  [%expect
    {|
    -3
    1
    |}]
;;

(* ----------------------------------------------------------------------------
   Section 2: BUG - Pattern binding for unit-typed tuple element fails
              in codegen (`print_expr_zero` doesn't handle Tuple_get).
   See lib/codegen.ml `print_expr_zero` and `emit_bind`.
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: BUG match tuple binds named unit element" =
  go
    {|
let _ = match (1, ()) { (a, b) -> a };;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: BUG match tuple binds named unit element (reverse)" =
  go
    {|
let _ = match ((), 1) { (a, b) -> b };;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: BUG match tuple of all units with named bindings" =
  go
    {|
let _ = match ((), ()) { (a, b) -> () };;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: BUG dependent tuple with unit element bound" =
  go
    {|
let f = fn (static x : unit ^ int) -> match x { (a, b) -> b };;

let _ = print_int (f ((), 4));;
|};
  [%expect {| 4 |}]
;;

(* Same bug also triggers when the tuple comes from an arrow argument. *)

let%expect_test "fuzz: BUG arrow with tuple containing unit binds unit element" =
  go
    {|
let f = fn (x : int ^ unit) -> match x { (a, b) -> b };;

let _ = f (1, ());;
|};
  [%expect {| |}]
;;

(* These work because the unit element is ignored with `_` (no binding). *)

let%expect_test "fuzz: match tuple ignores unit element with _" =
  go
    {|
let _ = match (1, ()) { (a, _) -> print_int a };;
|};
  [%expect {| 1 |}]
;;

let%expect_test "fuzz: match tuple matches unit literal" =
  go
    {|
let _ = match (1, ()) { (a, ()) -> print_int a };;
|};
  [%expect {| 1 |}]
;;

(* ----------------------------------------------------------------------------
   Section 3: BUG - Negative integer monomorphization keys produce
              invalid C++ symbols. The `-` ends up unescaped in the symbol
              name, e.g. `_fˢ27·λˢ49ₒ-1` is invalid C++.
   See lib/codegen.ml `print_key` for the `Int i -> Int64.to_string i` case.
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: BUG static int param negative" =
  go
    {|
let f = fn (static x : int) -> x;;
let _ = f (-1);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: BUG erased if with negative literal" =
  go
    {|
let f = fn (static x : int) -> if erased x == -5 then 1 else 2;;
let _ = print_int (f (-5));;
|};
  [%expect {| 1 |}]
;;

let%expect_test "fuzz: BUG static int through let" =
  go
    {|
let f = fn (static x : int) -> x;;
let x = -5;;
let _ = f x;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: BUG static tuple containing negative int" =
  go
    {|
let f = fn (static x : int ^ int) -> match x { (a, b) -> a + b };;

let _ = f (-1, 2);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: BUG two static negative args" =
  go
    {|
let f = fn (static x : int) -> fn (static y : int) -> x + y;;
let _ = print_int (f (-3) 4);;
|};
  [%expect {| 1 |}]
;;

(* These work because the keys don't include negative ints in the symbol path. *)

let%expect_test "fuzz: static int 0 works" =
  go
    {|
let f = fn (static x : int) -> x;;
let _ = print_int (f 0);;
|};
  [%expect {| 0 |}]
;;

let%expect_test "fuzz: poly id with negative arg works" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = print_int (id int (-5));;
|};
  [%expect {| -5 |}]
;;

(* ----------------------------------------------------------------------------
   Section 4: Tuple variations that should compile and run
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: nested tuple match" =
  go
    {|
let _ =
  match ((1, 2), (3, (4, (5, 6)))) {
    ((a, b), (c, (d, (e, f)))) -> assert (a + b + c + d + e + f == 21),
  }
;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: large tuple" =
  go
    {|
let _ =
  match (1, 2, 3, 4, 5, 6, 7, 8, 9, 10) {
    (a, b, c, d, e, f, g, h, i, j) -> assert (a + b + c + d + e + f + g + h + i + j == 55),
  }
;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: alternating bool/int tuple" =
  go
    {|
let _ = match (true, 1, false, 2, true, 3) { (a, b, c, d, e, f) -> assert (b + d + f == 6) };;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: tuple of closures" =
  go
    {|
let pair = ((fn (x : int) -> x + 1), (fn (x : int) -> x + 2));;

let _ = match pair { (f, g) -> print_int (f (g 10)) };;
|};
  [%expect {| 13 |}]
;;

let%expect_test "fuzz: tuple of partial applications" =
  go
    {|
let f = fn (x : int) -> fn (y : int) -> x + y;;

let g = (f 1, f 2);;

let _ = match g { (h, i) -> print_int (h 10 + i 20) };;
|};
  [%expect {| 33 |}]
;;

(* ----------------------------------------------------------------------------
   Section 5: Patterns
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: deep or-pattern alternatives" =
  go
    {|
fun f (x : int) : int =
  match x {
    1 | 2 | 3 | 4 | 5 -> 1,
    6 | 7 | 8 | 9 | 10 -> 2,
    _ -> 3,
  }
;;

let _ = assert (f 3 == 1);;

let _ = assert (f 8 == 2);;

let _ = assert (f 100 == 3);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: nested or-pattern with var binding" =
  go
    {|
fun f (p : int ^ int ^ int) : int =
  match p { (0, x, _) | (x, 0, _) | (_, _, x) -> x }
;;

let _ = assert (f (0, 5, 9) == 5);;

let _ = assert (f (7, 0, 9) == 7);;

let _ = assert (f (1, 2, 3) == 3);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: match with var arm shadows scrutinee" =
  go
    {|
let x = 5;;

let _ = match x { x -> assert (x == 5) };;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: match wide or-pattern" =
  go
    {|
let _ =
  match 5 {
    1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 -> assert true,
    _ -> assert false,
  }
;;
|};
  [%expect {| |}]
;;

(* ----------------------------------------------------------------------------
   Section 6: Closures with diverse captures
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: closure captures 8 dynamic ints" =
  go
    {|
let a = 1 @ dynamic;;
let b = 2 @ dynamic;;
let c = 3 @ dynamic;;
let d = 4 @ dynamic;;
let e = 5 @ dynamic;;
let f = 6 @ dynamic;;
let g = 7 @ dynamic;;
let h = 8 @ dynamic;;
let cl = fn (_ : unit) -> a + b + c + d + e + f + g + h;;
let _ = print_int (cl ());;
|};
  [%expect {| 36 |}]
;;

let%expect_test "fuzz: closure captures bool/int interleaved" =
  go
    {|
let a = true @ dynamic;;
let b = 42 @ dynamic;;
let c = false @ dynamic;;
let cl = fn (_ : unit) -> if a && !c then b else 0;;
let _ = print_int (cl ());;
|};
  [%expect {| 42 |}]
;;

let%expect_test "fuzz: closure of closure capture chain" =
  go
    {|
let a = 1 @ dynamic;;
let f = fn (_ : unit) ->
  let b = 2 @ dynamic in
  fn (_ : unit) -> a + b;;
let g = f ();;
let _ = print_int (g ());;
|};
  [%expect {| 3 |}]
;;

let%expect_test "fuzz: closure capturing tuple" =
  go
    {|
let t = (1, 2, 3) @ dynamic;;

let cl = fn (_ : unit) -> match t { (a, b, c) -> a + b + c };;

let _ = print_int (cl ());;
|};
  [%expect {| 6 |}]
;;

let%expect_test "fuzz: dynamic unit captured by closure" =
  go
    {|
let u = () @ dynamic;;
let f = fn (_ : unit) -> u;;
let _ = f ();;
|};
  [%expect {| |}]
;;

(* ----------------------------------------------------------------------------
   Section 7: Higher-order functions
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: HOF returning HOF — compose" =
  go
    {|
let compose = fn (static erased a : type) -> fn (static erased b : type) -> fn (static erased c : type) ->
  fn (g : b -> c) -> fn (f : a -> b) -> fn (x : a) -> g (f x);;
let inc = fn (x : int) -> x + 1;;
let _ = compose int int int inc inc 3;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: many-arg curried sum" =
  go
    {|
let f =
  fn (a : int) -> fn (b : int) -> fn (c : int) -> fn (d : int) -> fn (e : int) ->
    a + b + c + d + e;;
let _ = print_int (f 1 2 3 4 5);;
|};
  [%expect {| 15 |}]
;;

let%expect_test "fuzz: poly id on arrow type" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = print_int ((id (int -> int)) (fn (x : int) -> x + 1) 5);;
|};
  [%expect {| 6 |}]
;;

let%expect_test "fuzz: poly id on pi type" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let inner = fn (static erased u : type) -> fn (x : int) -> x + 1;;
let g = id (static erased type -> int -> int) inner;;
let _ = print_int (g int 5);;
|};
  [%expect {| 6 |}]
;;

(* ----------------------------------------------------------------------------
   Section 8: Recursion edge cases
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: recursion returning tuple" =
  go
    {|
fun pair (n : int) : int ^ int =
  if n <= 0 then (0, 0) else match pair (n - 1) { (a, b) -> (a + n, b + 2 * n) }
;;

let _ = match pair 5 { (x, _) -> assert (x == 15) };;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: deep recursion 1000 levels" =
  go
    {|
fun count (n : int) : int =
  if n == 0 then 0 else 1 + count (n - 1);;
let _ = print_int (count 1000);;
|};
  [%expect {| 1000 |}]
;;

let%expect_test "fuzz: 3-way mutual recursion" =
  go
    {|
fun a (n : int) : int =
  if n == 0 then 0 else b (n - 1) + 1
and b (n : int) : int =
  if n == 0 then 0 else c (n - 1) + 2
and c (n : int) : int =
  if n == 0 then 0 else a (n - 1) + 3;;
let _ = print_int (a 6);;
|};
  [%expect {| 12 |}]
;;

let%expect_test "fuzz: recursive function captures dyn" =
  go
    {|
let x = 1 @ dynamic;;
fun f (n : int) : dynamic int = if n == 0 then x else f (n - 1) + 1;;
let _ = print_int (f 5);;
|};
  [%expect {| 6 |}]
;;

(* ----------------------------------------------------------------------------
   Section 9: Static / dependent types
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: static recursion to 50" =
  go
    {|
fun f (static x : int) : static int = if erased x == 0 then 0 else f (x - 1) + 1;;
let _ = print_int (f 50);;
|};
  [%expect {| 50 |}]
;;

let%expect_test "fuzz: chained dependent application" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = print_int (id int (id int (id int (id int 7))));;
|};
  [%expect {| 7 |}]
;;

let%expect_test "fuzz: nested erased if returning function" =
  go
    {|
let f = fn (static x : int) ->
  if erased x == 0 then fn (y : int) -> y + 1
  else if erased x == 1 then fn (y : int) -> y + 2
  else fn (y : int) -> y + 100;;
let _ = print_int (f 0 10);;
let _ = print_int (f 1 10);;
let _ = print_int (f 5 10);;
|};
  [%expect
    {|
    11
    12
    110
    |}]
;;

let%expect_test "fuzz: type primitives tuple_get" =
  go
    {|
builtin tuple_get = syl_type_tuple_get;;
let _ = (0 : tuple_get (int ^ bool, 0));;
let _ = (true : tuple_get (int ^ bool, 1));;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: type primitives arrow_arg" =
  go
    {|
builtin arrow_arg = syl_type_arrow_arg;;
let _ = (0 : arrow_arg (int -> bool));;
|};
  [%expect {| |}]
;;

(* ----------------------------------------------------------------------------
   Section 10: Mode mixtures
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: erased lambda with erased capture" =
  go
    {|
let _ =
  let x = 10 @ erased in
  ((fn (_ : unit) -> x) @ erased) ()
;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: weakening static->dynamic int" =
  go
    {|
let _ = print_int ((1 + 2) @ dynamic);;
|};
  [%expect {| 3 |}]
;;

let%expect_test "fuzz: branch with mixed staticity" =
  go
    {|
let cond = true @ dynamic;;
let _ = if cond then 1 + 2 else 3;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: static through let" =
  go
    {|
let x = 1;;
let y = x @ dynamic;;
let _ = print_int y;;
|};
  [%expect {| 1 |}]
;;

(* ----------------------------------------------------------------------------
   Section 11: Match exhaustiveness corners
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: match unit" =
  go
    {|
let _ = match () { () -> assert true };;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: match deeply nested unit tuple" =
  go
    {|
let _ = match ((), ((), ())) { (_, (_, _)) -> () };;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: match all bool x bool combos" =
  go
    {|
fun classify (p : bool ^ bool) : int =
  match p {
    (true, true) -> 0,
    (true, false) -> 1,
    (false, true) -> 2,
    (false, false) -> 3,
  }
;;

let _ = assert (classify (true, true) == 0);;

let _ = assert (classify (false, false) == 3);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: match returning unit from arms" =
  go
    {|
let x = true @ dynamic;;

let _ =
  match x {
    true -> (),
    false -> (),
  }
;;
|};
  [%expect {| |}]
;;

(* ----------------------------------------------------------------------------
   Section 12: Unreachable / Bottom corners
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: unreachable inside erased if (true branch taken)" =
  go
    {|
let f = fn (static x : int) -> if erased x == 0 then 0 else unreachable;;
let _ = print_int (f 0);;
|};
  [%expect {| 0 |}]
;;

let%expect_test "fuzz: dynamic if with both branches reachable" =
  go
    {|
let f = fn (x : int) -> if x == 0 then 0 else x + 1;;
let _ = print_int (f 0);;
let _ = print_int (f 5);;
|};
  [%expect
    {|
    0
    6
    |}]
;;

(* ----------------------------------------------------------------------------
   Section 13: Misc syntax / structural fuzz
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: top-level shadow with self-reference" =
  go
    {|
let x = 1;;
let x = x + 1;;
let _ = print_int x;;
|};
  [%expect {| 2 |}]
;;

let%expect_test "fuzz: pattern bindings shadow scrutinee element" =
  go
    {|
let x = (1, 2);;

let _ = match x { (x, y) -> print_int (x + y) };;
|};
  [%expect {| 3 |}]
;;

let%expect_test "fuzz: shadowed fn arg" =
  go
    {|
let f = fn (x : int) -> fn (x : int) -> x + x;;
let _ = print_int (f 1 2);;
|};
  [%expect {| 4 |}]
;;

let%expect_test "fuzz: arrow type as parameter" =
  go
    {|
let f = fn (x : int -> int -> int) -> x 1 2;;
let _ = print_int (f (fn (a : int) -> fn (b : int) -> a + b));;
|};
  [%expect {| 3 |}]
;;

let%expect_test "fuzz: nested let-in closures" =
  go
    {|
let _ =
  let f = fn (x : int) -> x + 1 in
  let g = fn (x : int) -> f (f x) in
  print_int (g 5);;
|};
  [%expect {| 7 |}]
;;

let%expect_test "fuzz: many sequential top-level lets" =
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
let _ = print_int (a + b + c + d + e + f + g + h);;
|};
  [%expect {| 36 |}]
;;

let%expect_test "fuzz: operator as identifier" =
  go
    {|
let plus = (+);;
let _ = print_int (plus (1, 2));;
|};
  [%expect {| 3 |}]
;;

(* ----------------------------------------------------------------------------
   Section 14: Specialization with diverse keys
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: poly id on tuple" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;

let _ = match id (int ^ int) (1, 2) { (a, b) -> print_int (a + b) };;
|};
  [%expect {| 3 |}]
;;

let%expect_test "fuzz: poly id on closure type" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let inc = fn (x : int) -> x + 1;;
let f = id (int -> int) inc;;
let _ = print_int (f 10);;
|};
  [%expect {| 11 |}]
;;

let%expect_test "fuzz: poly id at base types" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = print_int (id int 42);;
let _ = print_bool (id bool true);;
let _ = id unit ();;
|};
  [%expect
    {|
    42
    true
    |}]
;;

let%expect_test "fuzz: distinct static int args produce distinct monomorphs" =
  go
    {|
let f = fn (static x : int) -> let _ = x in 0;;
let _ = print_int (f 1);;
let _ = print_int (f 2);;
let _ = print_int (f 3);;
let _ = print_int (f 1);;
|};
  [%expect
    {|
    0
    0
    0
    0
    |}]
;;

(* ----------------------------------------------------------------------------
   Section 15: Effects ordering and side-effects
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: ordering of let-bound effects" =
  go
    {|
let _ =
  let _ = print_int 1 in
  let _ = print_int 2 in
  let _ = print_int 3 in
  ()
;;
|};
  [%expect
    {|
    1
    2
    3
    |}]
;;

let%expect_test "fuzz: ordering inside lambda body" =
  go
    {|
let f = fn (_ : unit) ->
  let _ = print_int 1 in
  let _ = print_int 2 in
  print_int 3;;
let _ = f ();;
|};
  [%expect
    {|
    1
    2
    3
    |}]
;;

let%expect_test "fuzz: side-effect" =
  go
    {|
fun side (x : int) : int = let _ = print_int x in x;;
let _ = side 7;;
|};
  [%expect {| 7 |}]
;;

(* ----------------------------------------------------------------------------
   Section 16: Static/dynamic interactions in conditionals
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: if erased with both arms returning fns of different mode" =
  go
    {|
let f = fn (static x : int) ->
  if erased x == 0 then (fn (y : int) -> y) else (fn (y : int) -> y + 1);;
let _ = print_int (f 0 10);;
let _ = print_int (f 1 10);;
|};
  [%expect
    {|
    10
    11
    |}]
;;

let%expect_test "fuzz: dynamic if with both arms tuples" =
  go
    {|
let _ = match (if true then (1, 2) else (3, 4)) { (x, y) -> print_int (x + y) };;
|};
  [%expect {| 3 |}]
;;

let%expect_test "fuzz: deeply nested dyn-conditional with closures" =
  go
    {|
let cond = true @ dynamic;;
let f =
  if cond
  then (fn (x : int) -> x + 1)
  else (fn (x : int) -> x + 2);;
let _ = print_int (f 10);;
|};
  [%expect {| 11 |}]
;;

(* ----------------------------------------------------------------------------
   Section 17: Reuse / aliasing of compiler-generated names
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: many anonymous _ bindings" =
  go
    {|
let _ = ();;
let _ = ();;
let _ = ();;
let _ = print_int 7;;
|};
  [%expect {| 7 |}]
;;

let%expect_test "fuzz: collision with existing C identifiers (syl_int)" =
  go
    {|
let syl_int = 7;;
let _ = print_int syl_int;;
|};
  [%expect {| 7 |}]
;;

(* ----------------------------------------------------------------------------
   Section 18: Static binders with dependent return
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: nested dependent if-static" =
  go
    {|
let f = fn (static x : int) ->
  fn (y : if erased x == 0 then int else bool) ->
    if erased x == 0 then y else 1;;
let _ = print_int (f 0 5);;
let _ = print_int (f 1 true);;
|};
  [%expect
    {|
    5
    1
    |}]
;;

(* ----------------------------------------------------------------------------
   Section 19: Return-type weakening
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: arrow with explicit static return mode" =
  go
    {|
let f = (fn (x : int) -> x) : int -> static int;;
let _ = print_int (f 1);;
|};
  [%expect {| 1 |}]
;;

let%expect_test "fuzz: arrow with explicit erased return" =
  go
    {|
let f = (fn (x : int) -> x) : int -> erased int;;
let _ = f 1;;
|};
  [%expect {| |}]
;;

(* ----------------------------------------------------------------------------
   Section 20: Heavy nesting of function applications
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: 10-deep partial-application chain" =
  go
    {|
let mk = fn (a : int) ->
  fn (b : int) ->
    fn (c : int) ->
      fn (d : int) ->
        fn (e : int) ->
          fn (f : int) ->
            fn (g : int) ->
              fn (h : int) ->
                fn (i : int) ->
                  fn (j : int) ->
                    a + b + c + d + e + f + g + h + i + j;;
let _ = print_int (mk 1 2 3 4 5 6 7 8 9 10);;
|};
  [%expect {| 55 |}]
;;

(* ----------------------------------------------------------------------------
   Section 21: Various assertions
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: assert with side effect" =
  go
    {|
let x = 5 @ dynamic;;
let _ = assert (x > 0);;
let _ = assert (x < 100);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: assert erased with int comparison" =
  go
    {|
let _ = assert erased (1 + 1 == 2);;
let _ = assert erased (1 != 2);;
let _ = assert erased (5 * 5 == 25);;
|};
  [%expect {| |}]
;;

(* ----------------------------------------------------------------------------
   Section 22: Modifying environments / closures
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: closure capturing nested closure" =
  go
    {|
let mk = fn (x : int) ->
  let inner = fn (y : int) -> x + y in
  fn (z : int) -> inner z + x;;
let f = mk 10;;
let _ = print_int (f 5);;
|};
  [%expect {| 25 |}]
;;

let%expect_test "fuzz: 3-level capture" =
  go
    {|
let _ =
  fn (a : int) ->
    fn (b : int) ->
      fn (c : int) ->
        a + b + c
;;
|};
  [%expect {| |}]
;;

(* ----------------------------------------------------------------------------
   Section 23: Match with if/match in arm bodies
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: match arm contains if" =
  go
    {|
fun classify (n : int) : int =
  match n {
    0 -> if true then 100 else 200,
    _ -> if n > 0 then 1 else -1,
  }
;;

let _ = print_int (classify 0);;

let _ = print_int (classify 5);;

let _ = print_int (classify (-3));;
|};
  [%expect
    {|
    100
    1
    -1
    |}]
;;

let%expect_test "fuzz: nested match in arm body" =
  go
    {|
fun f (p : int ^ int) : int =
  match p {
    (a, b) ->
      match a {
        0 -> b,
        _ -> a,
      },
  }
;;

let _ = print_int (f (0, 7));;

let _ = print_int (f (5, 7));;
|};
  [%expect
    {|
    7
    5
    |}]
;;

(* ----------------------------------------------------------------------------
   Section 24: Larger programs
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: factorial dynamic" =
  go
    {|
fun fact (n : int) : int =
  if n <= 1 then 1 else n * fact (n - 1);;
let _ = print_int (fact 10);;
|};
  [%expect {| 3628800 |}]
;;

let%expect_test "fuzz: fib dynamic" =
  go
    {|
fun fib (n : int) : int =
  if n <= 1 then n else fib (n - 1) + fib (n - 2);;
let _ = print_int (fib 12);;
|};
  [%expect {| 144 |}]
;;

let%expect_test "fuzz: fib via mutual recursion" =
  go
    {|
fun fib_a (n : int) : int =
  if n <= 1 then n else fib_b (n - 1) + fib_a (n - 2)
and fib_b (n : int) : int =
  if n <= 1 then n else fib_a (n - 1) + fib_b (n - 2);;
let _ = print_int (fib_a 12);;
|};
  [%expect {| 144 |}]
;;

let%expect_test "fuzz: gcd via recursion" =
  go
    {|
fun gcd (p : int ^ int) : int =
  match p {
    (a, 0) -> a,
    (a, b) -> gcd (b, a % b),
  }
;;

let _ = print_int (gcd (48, 18));;
|};
  [%expect {| 6 |}]
;;

let%expect_test "fuzz: sum of integers from 1 to N" =
  go
    {|
fun sum (n : int) : int =
  if n == 0 then 0 else n + sum (n - 1);;
let _ = print_int (sum 100);;
|};
  [%expect {| 5050 |}]
;;

(* ----------------------------------------------------------------------------
   Section 25: Effects ordering across constructs
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: effect in match scrutinee runs first" =
  go
    {|
let _ =
  match (let _ = print_int 1 in 5) {
    5 -> print_int 2,
    _ -> (),
  }
;;
|};
  [%expect
    {|
    1
    2
    |}]
;;

let%expect_test "fuzz: effects across tuple elements" =
  go
    {|
let _ = match (let _ = print_int 1 in 1, let _ = print_int 2 in 2) { (a, b) -> print_int (a + b) };;
|};
  [%expect
    {|
    1
    2
    3
    |}]
;;

(* ----------------------------------------------------------------------------
   Section 26: Static dispatch (jump table style)
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: static dispatch via if chain" =
  go
    {|
let dispatch = fn (static op : int) ->
  if erased op == 0 then fn (x : int) -> fn (y : int) -> x + y
  else if erased op == 1 then fn (x : int) -> fn (y : int) -> x - y
  else if erased op == 2 then fn (x : int) -> fn (y : int) -> x * y
  else fn (x : int) -> fn (y : int) -> 0;;
let _ = print_int (dispatch 0 5 3);;
let _ = print_int (dispatch 1 5 3);;
let _ = print_int (dispatch 2 5 3);;
let _ = print_int (dispatch 99 5 3);;
|};
  [%expect
    {|
    8
    2
    15
    0
    |}]
;;

(* ----------------------------------------------------------------------------
   Section 27: Polymorphic specialization with closures as keys
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: specialize on distinct closure keys" =
  go
    {|
let apply = fn (static f : int -> int) -> f 10;;
let f1 = fn (x : int) -> x + 1;;
let f2 = fn (x : int) -> x * 2;;
let _ = print_int (apply f1);;
let _ = print_int (apply f2);;
|};
  [%expect
    {|
    11
    20
    |}]
;;

let%expect_test "fuzz: specialize on same closure twice" =
  go
    {|
let apply = fn (static f : int -> int) -> f 10;;
let f1 = fn (x : int) -> x + 1;;
let _ = print_int (apply f1);;
let _ = print_int (apply f1);;
|};
  [%expect
    {|
    11
    11
    |}]
;;

(* ----------------------------------------------------------------------------
   Section 28: Many sequential static specializations
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: ten distinct positive static int specializations" =
  go
    {|
let f = fn (static x : int) -> x;;
let _ = print_int (f 1);;
let _ = print_int (f 2);;
let _ = print_int (f 3);;
let _ = print_int (f 4);;
let _ = print_int (f 5);;
let _ = print_int (f 6);;
let _ = print_int (f 7);;
let _ = print_int (f 8);;
let _ = print_int (f 9);;
let _ = print_int (f 10);;
|};
  [%expect
    {|
    1
    2
    3
    4
    5
    6
    7
    8
    9
    10
    |}]
;;

let%expect_test "fuzz: chained static call through three layers" =
  go
    {|
let f = fn (static x : int) -> x + 1;;
let g = fn (static x : int) -> f x + 1;;
let h = fn (static x : int) -> g x + 1;;
let _ = print_int (h 5);;
|};
  [%expect {| 8 |}]
;;

(* ----------------------------------------------------------------------------
   Section 29: Match with negative scrutinee at runtime
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: match runtime negative int" =
  go
    {|
fun classify (n : int) : int =
  match n {
    0 -> 0,
    _ -> n,
  }
;;

let _ = print_int (classify (-5));;
|};
  [%expect {| -5 |}]
;;

(* ----------------------------------------------------------------------------
   Section 30: Local recursion capturing dynamics
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: local mutual recursion with distinct dynamics" =
  go
    {|
let _ =
  let a = 1 @ dynamic in
  let b = 2 @ dynamic in
  fun f (n : int) : dynamic int = if n == 0 then a else g (n - 1) + 1
  and g (n : int) : dynamic int = if n == 0 then b else f (n - 1) + 1 in
  print_int (f 4);;
|};
  [%expect {| 5 |}]
;;

let%expect_test "fuzz: local rec returning closure that captures" =
  go
    {|
let _ =
  let z = 100 @ dynamic in
  fun mk (n : int) : dynamic (int -> dynamic int) =
    if n == 0 then fn (x : int) -> x + z
    else fn (x : int) -> x + n in
  let f = mk 5 in
  print_int (f 1);;
|};
  [%expect {| 6 |}]
;;

(* ----------------------------------------------------------------------------
   Section 31: Various reasoning-heavy programs
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: collatz" =
  go
    {|
fun collatz (n : int) : int =
  if n == 1 then 0
  else if n % 2 == 0 then 1 + collatz (n / 2)
  else 1 + collatz (3 * n + 1);;
let _ = print_int (collatz 27);;
|};
  [%expect {| 111 |}]
;;

let%expect_test "fuzz: ackermann (small)" =
  go
    {|
fun ack (m : int) : int -> int =
  fn (n : int) ->
    if m == 0 then n + 1
    else if n == 0 then ack (m - 1) 1
    else ack (m - 1) (ack m (n - 1));;
let _ = print_int (ack 2 3);;
|};
  [%expect {| 9 |}]
;;

let%expect_test "fuzz: power" =
  go
    {|
fun pow (b : int) : int -> int =
  fn (e : int) ->
    if e == 0 then 1
    else b * pow b (e - 1);;
let _ = print_int (pow 2 10);;
let _ = print_int (pow 3 5);;
|};
  [%expect
    {|
    1024
    243
    |}]
;;

(* ----------------------------------------------------------------------------
   Section 32: Pi instantiated to pi
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: poly id at pi type" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let f = id (static erased type -> int -> int) (fn (static erased u : type) -> fn (x : int) -> x + 1);;
let _ = print_int (f int 5);;
|};
  [%expect {| 6 |}]
;;

(* ----------------------------------------------------------------------------
   Section 1: Min int as a static monomorphization key. Previously triggered a
              codegen bug (the `"N" ^ Int64.to_string (-i)` escape overflowed
              for min int and emitted a literal `-` into the symbol name).
              Fixed by dropping the leading `-` instead of negating.
              See lib/codegen.ml `print_key` Int case.
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: min int as static key" =
  go
    {|
let f = fn (static x : int) -> x;;
let _ = print_int (f (-9223372036854775807 - 1));;
|};
  [%expect {| -9223372036854775808 |}]
;;

let%expect_test "fuzz: negate min int at static (negation overflows back to min)" =
  go
    {|
let f = fn (static x : int) -> x;;
let _ = print_int (f (- (-9223372036854775807 - 1)));;
|};
  [%expect {| -9223372036854775808 |}]
;;

(* Static `min / -1` reduces to `min` in OCaml int64; the symbol-name fix
   makes that work as a monomorph key. We can't run the application though,
   because the runtime `syl_int_div` would re-execute `min / -1` and hit
   signed-integer-overflow UB (separate issue, not addressed here). *)
let%expect_test "fuzz: static (min / -1) reduces to min as a key" =
  go
    ~check:`Compile
    {|
let f = fn (static x : int) -> x;;
let _ = (f ((-9223372036854775807 - 1) / (-1))) @ erased;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: -max as static key (sanity)" =
  go
    {|
let f = fn (static x : int) -> x;;
let _ = print_int (f (-9223372036854775807));;
|};
  [%expect {| -9223372036854775807 |}]
;;

(* ----------------------------------------------------------------------------
   Section 2: `%` semantics. Compile-time uses OCaml's Euclidean modulo (result
              in [0, b) for positive b). Runtime `syl_int_mod` was previously
              C's truncated `%` (preserves dividend sign), so the two diverged
              for negative dividends. Fixed by making the runtime Euclidean too.
              See std/syl_std.ml `syl_int_mod`.
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: runtime % matches static % for negative dividend" =
  go
    {|
(* Both runtime and compile-time now Euclidean: (-7) mod 2 = 1. *)
let _ = print_int ((-7) % (2 @ dynamic));;
let _ = assert erased ((-7) % 2 == 1);;
|};
  [%expect {| 1 |}]
;;

let%expect_test "fuzz: runtime % Euclidean for various negative dividends" =
  go
    {|
let _ = print_int ((-1) % (3 @ dynamic));;
let _ = print_int ((-2) % (3 @ dynamic));;
let _ = print_int ((-3) % (3 @ dynamic));;
let _ = print_int ((-4) % (3 @ dynamic));;
|};
  [%expect
    {|
    2
    1
    0
    2
    |}]
;;

(* Negative divisor is reported as a Negative_modulus error during typecheck;
   see test/typecheck/test_fuzz.ml. *)

(* ----------------------------------------------------------------------------
   Section 3: BUG - Compile-time div of min by -1 returns min (well-defined
              OCaml int64 div), but the runtime version is undefined behavior
              and returns garbage. The two should agree.
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: static min/-1 returns min (OCaml semantics)" =
  go
    {|
let _ = assert erased ((-9223372036854775807 - 1) / (-1) == -9223372036854775807 - 1);;
|};
  [%expect {| |}]
;;

(* The runtime case can't be tested deterministically (UB returns garbage),
   but the divergence between compile and runtime semantics is itself a bug. *)

(* ----------------------------------------------------------------------------
   Section 4: Integer literal edge cases that work
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: max int64 literal" =
  go
    {|
let _ = print_int 9223372036854775807;;
|};
  [%expect {| 9223372036854775807 |}]
;;

let%expect_test "fuzz: min int via -max - 1" =
  go
    {|
let _ = print_int (-9223372036854775807 - 1);;
|};
  [%expect {| -9223372036854775808 |}]
;;

let%expect_test "fuzz: integer overflow at compile time wraps" =
  go
    {|
let _ = print_int (9223372036854775807 + 1);;
|};
  [%expect {| -9223372036854775808 |}]
;;

let%expect_test "fuzz: division of negative by positive is consistent" =
  go
    {|
let _ = print_int (-7 / (2 @ dynamic));;
let _ = assert erased (-7 / 2 == -3);;
|};
  [%expect {| -3 |}]
;;

(* ----------------------------------------------------------------------------
   Section 5: Subtle subtyping cases that should typecheck
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: arrow with static ret can be used where dynamic ret expected" =
  go
    {|
let f = (fn (x : int) -> x) : int -> static int;;
let apply = fn (g : int -> int) -> g 5;;
let _ = print_int (apply f);;
|};
  [%expect {| 5 |}]
;;

let%expect_test "fuzz: arrow used where pi- with -static-arg expected" =
  go
    {|
let f = fn (x : int) -> x;;
let pi_apply = fn (static g : static int -> int) -> g 5;;
let _ = print_int (pi_apply f);;
|};
  [%expect {| 5 |}]
;;

let%expect_test "fuzz: erased fn arg accepts literal (auto-weakening)" =
  go
    {|
let f = fn (erased x : int) -> 0;;
let _ = print_int (f 5);;
|};
  [%expect {| 0 |}]
;;

let%expect_test "fuzz: tuple subtype with mode mixing" =
  go
    {|
let _ = (1, 2 @ erased);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: join of arrow types with diff ret modes" =
  go
    {|
let f = fn (x : int) -> x;;
let g = (fn (x : int) -> x + 1) : int -> static int;;
let h = if true then f else g;;
let _ = print_int (h 5);;
|};
  [%expect {| 5 |}]
;;

let%expect_test "fuzz: join Arrow/Pi at if branch" =
  go
    {|
let arr_fn = fn (x : int) -> x + 1;;
let pi_fn = fn (static x : int) -> x + 2;;
let h = if true then arr_fn else pi_fn;;
let _ = print_int (h 5);;
|};
  [%expect {| 6 |}]
;;

(* ----------------------------------------------------------------------------
   Section 6: Static evaluation algebraic identities
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: static x + 0 == x" =
  go
    {|
let f = fn (static x : int) ->
  let _ = assert erased (x + 0 == x) in
  x;;
let _ = print_int (f 5);;
|};
  [%expect {| 5 |}]
;;

let%expect_test "fuzz: static x + (-x) == 0" =
  go
    {|
let f = fn (static x : int) ->
  let _ = assert erased (x + (-x) == 0) in
  x;;
let _ = print_int (f 5);;
|};
  [%expect {| 5 |}]
;;

let%expect_test "fuzz: static x * 0 == 0" =
  go
    {|
let f = fn (static x : int) ->
  let _ = assert erased (x * 0 == 0) in
  x;;
let _ = print_int (f 5);;
|};
  [%expect {| 5 |}]
;;

let%expect_test "fuzz: static x % x == 0" =
  go
    {|
let f = fn (static x : int) ->
  let _ = assert erased (x % x == 0) in
  x;;
let _ = print_int (f 5);;
|};
  [%expect {| 5 |}]
;;

(* ----------------------------------------------------------------------------
   Section 7: Recursive monomorphization scenarios
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: static factorial up to 20 (within depth limit)" =
  go
    {|
fun fact (static x : int) : static int =
  if erased x <= 1 then 1 else x * fact (x - 1);;
let _ = print_int (fact 20);;
|};
  [%expect {| 2432902008176640000 |}]
;;

let%expect_test "fuzz: static recursion within limit" =
  go
    {|
fun loop (static x : int) : static int =
  if erased x == 0 then 0 else loop (x - 1);;
let _ = print_int (loop 100);;
|};
  [%expect {| 0 |}]
;;

(* ----------------------------------------------------------------------------
   Section 8: Closure environments with mixed captures
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: closure captures bool then int (alignment)" =
  go
    {|
let b = true @ dynamic;;
let i = 100 @ dynamic;;
let cl = fn (_ : unit) -> if b then i else 0;;
let _ = print_int (cl ());;
|};
  [%expect {| 100 |}]
;;

let%expect_test "fuzz: closure captures int, bool, int interleaved" =
  go
    {|
let i1 = 10 @ dynamic;;
let b = true @ dynamic;;
let i2 = 20 @ dynamic;;
let cl = fn (_ : unit) -> if b then i1 + i2 else 0;;
let _ = print_int (cl ());;
|};
  [%expect {| 30 |}]
;;

let%expect_test "fuzz: closure captures dynamic unit + int" =
  go
    {|
let u = () @ dynamic;;
let i = 42 @ dynamic;;
let f = fn (_ : unit) -> let _ = u in i;;
let _ = print_int (f ());;
|};
  [%expect {| 42 |}]
;;

let%expect_test "fuzz: 4-deep nested closures sharing captures" =
  go
    {|
let outer = 100 @ dynamic;;
let f = fn (a : int) ->
  let g = fn (b : int) ->
    let h = fn (c : int) -> outer + a + b + c in
    h 1 in
  g 2;;
let _ = print_int (f 3);;
|};
  [%expect {| 106 |}]
;;

(* ----------------------------------------------------------------------------
   Section 9: HOF and polymorphic usage
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: poly id at arrow type" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let h = id (int -> int);;
let _ = print_int (h (fn (x : int) -> x + 100) 5);;
|};
  [%expect {| 105 |}]
;;

let%expect_test "fuzz: poly id at closure type, then apply" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let f = id (int -> int) (fn (x : int) -> x + 1);;
let _ = print_int (f 10);;
|};
  [%expect {| 11 |}]
;;

let%expect_test "fuzz: rank-2 polymorphism via static erased pi" =
  go
    {|
let apply_at_int =
  fn (static f : static erased type \ t -> t -> t) -> f int 5;;
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = print_int (apply_at_int id);;
|};
  [%expect {| 5 |}]
;;

let%expect_test "fuzz: rank-2 polymorphism applied at two types" =
  go
    {|
let apply_two =
  fn (static f : static erased type \ t -> t -> t) ->
    let _ = print_int (f int 5) in
    print_bool (f bool true);;
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = apply_two id;;
|};
  [%expect
    {|
    5
    true
    |}]
;;

(* ----------------------------------------------------------------------------
   Section 10: Match with closures and pattern bindings
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: match arm closures capturing scrutinee" =
  go
    {|
let make =
  fn (n : int) ->
    match n {
      0 -> (fn (x : int) -> x),
      _ -> (fn (x : int) -> x + n),
    }
;;

let _ = print_int (make 0 10);;

let _ = print_int (make 5 10);;
|};
  [%expect
    {|
    10
    15
    |}]
;;

let%expect_test "fuzz: match arm closures capturing pattern bindings" =
  go
    {|
let make =
  fn (p : int ^ int) ->
    match p {
      (a, 0) -> (fn (x : int) -> a + x),
      (a, b) -> (fn (x : int) -> a * b + x),
    }
;;

let _ = print_int (make (5, 0) 10);;

let _ = print_int (make (3, 4) 10);;
|};
  [%expect
    {|
    15
    22
    |}]
;;

let%expect_test "fuzz: match returns tuple of closures" =
  go
    {|
let f =
  match true {
    true -> ((fn (x : int) -> x + 1), (fn (x : int) -> x + 2)),
    false -> ((fn (x : int) -> x), (fn (x : int) -> x)),
  }
;;

let _ = match f { (g, h) -> print_int (g (h 10)) };;
|};
  [%expect {| 13 |}]
;;

(* ----------------------------------------------------------------------------
   Section 11: Static dispatch and varying closure shapes
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: static fn returns closure with varying captures" =
  go
    {|
let mk = fn (static x : int) ->
  let captured = x * 100 in
  fn (y : int) -> captured + y;;
let f = mk 5;;
let g = mk 10;;
let _ = print_int (f 1);;
let _ = print_int (g 1);;
let _ = print_int (f 2);;
|};
  [%expect
    {|
    501
    1001
    502
    |}]
;;

let%expect_test "fuzz: many distinct closure-key specializations" =
  go
    {|
let apply = fn (static f : int -> int) -> f 10;;
let _ = print_int (apply (fn (x : int) -> x + 1));;
let _ = print_int (apply (fn (x : int) -> x + 2));;
let _ = print_int (apply (fn (x : int) -> x + 3));;
let _ = print_int (apply (fn (x : int) -> x * 2));;
|};
  [%expect
    {|
    11
    12
    13
    20
    |}]
;;

let%expect_test "fuzz: 8-deep static curry" =
  go
    {|
let f = fn (static a : int) ->
  fn (static b : int) ->
    fn (static c : int) ->
      fn (static d : int) ->
        fn (static e : int) ->
          fn (static g : int) ->
            fn (static h : int) ->
              fn (static i : int) -> a + b + c + d + e + g + h + i;;
let _ = print_int (f 1 2 3 4 5 6 7 8);;
|};
  [%expect {| 36 |}]
;;

(* ----------------------------------------------------------------------------
   Section 12: Effects ordering
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: tuple element evaluation order" =
  go
    {|
let _ =
  match (let _ = print_int 1 in 100, let _ = print_int 2 in 200) { (a, b) -> print_int (a + b) }
;;
|};
  [%expect
    {|
    1
    2
    300
    |}]
;;

let%expect_test "fuzz: app argument evaluation order" =
  go
    {|
let f = fn (a : int) -> fn (b : int) -> a + b;;
let _ = print_int (f (let _ = print_int 11 in 100) (let _ = print_int 22 in 200));;
|};
  [%expect
    {|
    11
    22
    300
    |}]
;;

let%expect_test "fuzz: binder body re-emits effects per call" =
  go
    {|
let f = fn (static x : int) ->
  let _ = print_int x in
  x + 1;;
let _ = print_int (f 1);;
let _ = print_int (f 1);;
|};
  [%expect
    {|
    1
    2
    1
    2
    |}]
;;

(* ----------------------------------------------------------------------------
   Section 13: Match decision tree complexity
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: 8-arm match on bool^bool^bool tuple" =
  go
    {|
fun f (p : bool ^ bool ^ bool) : int =
  match p {
    (true, true, true) -> 7,
    (true, true, false) -> 6,
    (true, false, true) -> 5,
    (true, false, false) -> 4,
    (false, true, true) -> 3,
    (false, true, false) -> 2,
    (false, false, true) -> 1,
    (false, false, false) -> 0,
  }
;;

let _ = assert (f (true, true, true) == 7);;

let _ = assert (f (false, false, false) == 0);;

let _ = assert (f (true, false, true) == 5);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: 9-pattern match with overlapping wildcards" =
  go
    {|
fun f (p : int ^ int ^ int) : int =
  match p {
    (0, 0, 0) -> 1,
    (0, 0, _) -> 2,
    (0, _, 0) -> 3,
    (_, 0, 0) -> 4,
    (0, _, _) -> 5,
    (_, 0, _) -> 6,
    (_, _, 0) -> 7,
    _ -> 8,
  }
;;

let _ = print_int (f (0, 0, 0));;

let _ = print_int (f (1, 1, 1));;

let _ = print_int (f (1, 1, 0));;
|};
  [%expect
    {|
    1
    8
    7
    |}]
;;

(* ----------------------------------------------------------------------------
   Section 14: Pi types as static keys
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: pi with various mode annotations as static erased key" =
  go
    {|
let f = fn (static erased t : type) -> 5;;
let _ = print_int (f (static int -> int));;
let _ = print_int (f (erased int -> int));;
let _ = print_int (f (int -> static int));;
let _ = print_int (f (int -> erased int));;
|};
  [%expect
    {|
    5
    5
    5
    5
    |}]
;;

let%expect_test "fuzz: tuple of pi types as static key" =
  go
    {|
let f = fn (static erased t : type) -> 0;;
let _ = print_int (f ((static int -> int) ^ (static erased type -> int)));;
|};
  [%expect {| 0 |}]
;;

let%expect_test "fuzz: type as type as static key" =
  go
    {|
let f = fn (static erased t : type) -> 5;;
let _ = print_int (f type);;
|};
  [%expect {| 5 |}]
;;

(* ----------------------------------------------------------------------------
   Section 15: Larger / complex programs
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: collatz sequence length" =
  go
    {|
fun collatz (n : int) : int =
  if n == 1 then 0
  else if n % 2 == 0 then 1 + collatz (n / 2)
  else 1 + collatz (3 * n + 1);;
let _ = print_int (collatz 27);;
|};
  [%expect {| 111 |}]
;;

let%expect_test "fuzz: ackermann (2, 3)" =
  go
    {|
fun ack (m : int) : int -> int =
  fn (n : int) ->
    if m == 0 then n + 1
    else if n == 0 then ack (m - 1) 1
    else ack (m - 1) (ack m (n - 1));;
let _ = print_int (ack 2 3);;
|};
  [%expect {| 9 |}]
;;

let%expect_test "fuzz: gcd" =
  go
    {|
fun gcd (p : int ^ int) : int =
  match p {
    (a, 0) -> a,
    (a, b) -> gcd (b, a % b),
  }
;;

let _ = print_int (gcd (48, 18));;

let _ = print_int (gcd (100, 35));;
|};
  [%expect
    {|
    6
    5
    |}]
;;

let%expect_test "fuzz: deep dynamic recursion (10k)" =
  go
    {|
fun count (n : int) : int =
  if n == 0 then 0 else 1 + count (n - 1);;
let _ = print_int (count 10000);;
|};
  [%expect {| 10000 |}]
;;

(* ----------------------------------------------------------------------------
   Section 16: Mode interactions in lambda chains
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: pi taking pi arg, then specialized" =
  go
    {|
let apply = fn (static f : static int -> int) ->
  fn (static x : int) -> f x;;
let inc = fn (static x : int) -> x + 1;;
let inc_b = apply inc;;
let _ = print_int (inc_b 5);;
let _ = print_int (inc_b 10);;
|};
  [%expect
    {|
    6
    11
    |}]
;;

let%expect_test "fuzz: arrow used as pi value" =
  go
    {|
let arrow_fn = fn (x : int) -> x;;
let pi_apply = fn (static g : static int -> int) -> g 5;;
let _ = print_int (pi_apply arrow_fn);;
|};
  [%expect {| 5 |}]
;;

(* ----------------------------------------------------------------------------
   Section 17: Unreachable in static branches
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: unreachable in erased if (untaken)" =
  go
    {|
let f = fn (static x : int) -> if erased x == 0 then 0 else unreachable;;
let _ = print_int (f 0);;
|};
  [%expect {| 0 |}]
;;

let%expect_test "fuzz: chained erased-if unreachable in last branch" =
  go
    {|
let f = fn (static x : int) ->
  if erased x == 0 then 100
  else if erased x == 1 then 200
  else if erased x == 2 then 300
  else unreachable;;
let _ = print_int (f 0);;
let _ = print_int (f 2);;
|};
  [%expect
    {|
    100
    300
    |}]
;;

(* ----------------------------------------------------------------------------
   Section 18: Workaround for pi-capturing-dyn — see test/typecheck/test_fuzz.ml
              for the failing typecheck cases. The fix is to pass the dynamic
              value as another argument instead of capturing it.
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: workaround for pi-capturing-dyn — pass as extra arg" =
  go
    {|
let f = fn (static n : int) -> fn (outer : int) -> outer + n;;
let _ = print_int (f 5 1000);;
|};
  [%expect {| 1005 |}]
;;

(* ----------------------------------------------------------------------------
   Section 19: Effect-erasure interaction — when a value is weakened to
                erased (or the call result is erased), the body is not
                executed at runtime, even if it has side effects. This
                might be intentional (effect-tracking is on the TODO list).
   ---------------------------------------------------------------------------- *)

let%expect_test "fuzz: side effect lost when call result is erased" =
  go
    {|
let f = fn (x : int) -> let _ = print_int 999 in 0;;
let _ = (f 1) @ erased;;       (* call's result erased -> body dropped *)
let _ = print_int 100;;
|};
  [%expect {| 100 |}]
;;

let%expect_test "fuzz: side effect lost when bind is erased" =
  go
    {|
let _ =
  let x = (let _ = print_int 999 in 5) @ erased in
  print_int 100;;
|};
  [%expect {| 100 |}]
;;
