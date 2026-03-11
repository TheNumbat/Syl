open! Core
open! Syl

(* Tests written by Claude acting as a fuzzer. *)

let check_all = `No

let compile c =
  let tmp = Stdlib.Filename.temp_file "syl_fuzz" ".cpp" in
  Out_channel.write_all tmp ~data:c;
  let cmd = Printf.sprintf "clang++ -fsyntax-only -w %s 2>/dev/null" tmp in
  (match Core_unix.system cmd with
   | Ok () -> ()
   | Error (`Exit_non_zero exit_code) -> raise_s [%message "Clang failed" (exit_code : int)]
   | Error (`Signal signal) -> raise_s [%message "Clang killed" (signal : Signal.t)]);
  Core_unix.unlink tmp
;;

let compile_and_run c =
  let tmp_c = Stdlib.Filename.temp_file "syl_fuzz" ".cpp" in
  let tmp_exe = Stdlib.Filename.temp_file "syl_fuzz" ".exe" in
  Out_channel.write_all tmp_c ~data:c;
  let cmd =
    Printf.sprintf
      "clang++ -fsanitize=address -fsanitize=alignment -o %s -w %s 2>/dev/null"
      tmp_exe
      tmp_c
  in
  (match Core_unix.system cmd with
   | Ok () -> ()
   | Error (`Exit_non_zero exit_code) -> raise_s [%message "Clang failed" (exit_code : int)]
   | Error (`Signal signal) -> raise_s [%message "Clang killed" (signal : Signal.t)]);
  Core_unix.unlink tmp_c;
  (match Core_unix.system ("ASAN_OPTIONS=detect_leaks=0 " ^ tmp_exe) with
   | Ok () -> ()
   | Error (`Exit_non_zero exit_code) -> raise_s [%message "Program failed" (exit_code : int)]
   | Error (`Signal signal) -> raise_s [%message "Program killed" (signal : Signal.t)]);
  Core_unix.unlink tmp_exe
;;

let strip_prelude c =
  let c_preamble_end = "//SYL_STDLIB_END" in
  match String.substr_index c ~pattern:c_preamble_end with
  | None -> c
  | Some i ->
    let c = String.drop_prefix c (i + String.length c_preamble_end) in
    (match String.chop_prefix c ~prefix:"\n" with
     | Some c -> c
     | None -> c)
;;

let go ?print ?(check = check_all) input =
  let cst = Parse.parse_exn input in
  let tst = Typecheck.typecheck_exn cst in
  let sst = Simplify.simplify tst in
  let lst = Linearize.linearize sst in
  let c = Codegen.c lst in
  if Option.is_some print then print_string (strip_prelude c);
  match check with
  | `Compile -> compile c
  | `Run -> compile_and_run c
  | _ -> ()
;;

(* This should constant-fold to true but the bug makes it false *)
let%expect_test "fuzz: gte 5 >= 3 gives wrong constant" =
  go
    {|
let _ = 5 >= 3;;
|};
  [%expect {| |}]
;;

(* This should constant-fold to false but the bug makes it true *)
let%expect_test "fuzz: gte 3 >= 5 gives wrong constant" =
  go
    {|
let _ = 3 >= 5;;
|};
  [%expect {| |}]
;;

(* Equal values: 3 >= 3 should be true; the bug makes it true too
   (since 3 <= 3 is also true). No observable difference here. *)
let%expect_test "fuzz: gte equal values" =
  go
    {|
let _ = 3 >= 3;;
|};
  [%expect {| |}]
;;

(* Verify all other comparison operators are correct *)
let%expect_test "fuzz: all comparison operators" =
  go
    {|
let _ = 1 < 2;;
let _ = 2 < 1;;
let _ = 1 <= 2;;
let _ = 2 <= 1;;
let _ = 1 > 2;;
let _ = 2 > 1;;
let _ = 1 == 1;;
let _ = 1 != 2;;
|};
  [%expect {| |}]
;;

(* Use >= result in a dynamic if: the wrong branch is taken.
   With the bug, 5 >= 3 folds to false, so else is taken. *)
let%expect_test "fuzz: gte result used in dynamic if takes wrong branch" =
  go
    {|
let _ = if (5 >= 3) then 1 else 2;;
|};
  [%expect {| |}]
;;

(* Gte in nested boolean expression *)
let%expect_test "fuzz: gte in boolean expression" =
  go
    {|
let _ = (5 >= 3) && (10 >= 1);;
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

(* ============================================================
   ARITHMETIC EDGE CASES
   ============================================================ *)

let%expect_test "fuzz: division by constant" =
  go
    {|
let _ = 10 / 2;;
let _ = 10 % 3;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: nested arithmetic" =
  go
    {|
let _ = (1 + 2) * (3 - 4) / (5 + 1);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: negation of negation" =
  go
    {|
let _ = - (- 5);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: boolean double negation" =
  go
    {|
let _ = !(!true);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: zero arithmetic" =
  go
    {|
let _ = 0 + 0;;
let _ = 0 * 100;;
let _ = 0 - 0;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: large integers" =
  go
    {|
let _ = 999999999;;
let _ = 999999999 + 1;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: mixed dynamic arithmetic" =
  go
    {|
let x = 10 @ dynamic;;
let y = 20 @ dynamic;;
let _ = x + y * 2 - 1;;
|};
  [%expect {| |}]
;;

(* ============================================================
   CLOSURES AND CAPTURE
   ============================================================ *)

let%expect_test "fuzz: closure capturing closure" =
  go
    {|
let f = fn (x : int) -> fn (y : int) -> x + y;;
let g = f 10;;
let _ = g 20;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: triple-nested closure" =
  go
    {|
let f = fn (x : int) -> fn (y : int) -> fn (z : int) -> x + y + z;;
let g = f 1;;
let h = g 2;;
let _ = h 3;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: closure capturing multiple dynamic variables" =
  go
    {|
let a = 1 @ dynamic;;
let b = 2 @ dynamic;;
let c = 3 @ dynamic;;
let f = fn (_ : unit) -> a + b + c;;
let _ = f ();;
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

let%expect_test "fuzz: closure capturing a closure" =
  go
    {|
let f = fn (x : int) -> x + 1;;
let g = fn (_ : unit) -> f 10;;
let _ = g ();;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: closure capturing dynamic bool" =
  go
    {|
let b = true @ dynamic;;
let f = fn (_ : unit) -> if b then 1 else 0;;
let _ = f ();;
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

let%expect_test "fuzz: closure capturing unit" =
  go
    {|
let u = ();;
let f = fn (_ : unit) -> u;;
let _ = f ();;
|};
  [%expect {| |}]
;;

(* ============================================================
   RECURSIVE FUNCTIONS
   ============================================================ *)

let%expect_test "fuzz: simple recursion" =
  go
    {|
fun count (x : int) : int =
  if x == 0 then 0 else count (x - 1);;
let _ = count 5;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: mutual recursion" =
  go
    {|
fun even (x : int) : bool =
  if x == 0 then true else odd (x - 1)
and odd (x : int) : bool =
  if x == 0 then false else even (x - 1);;
let _ = even 4;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: mutual recursion accumulating values" =
  go
    {|
fun f (x : int) : int =
  if x <= 0 then 0 else g (x - 1) + 1
and g (x : int) : int =
  if x <= 0 then 100 else f (x - 1) + 2;;
let _ = f 5;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: static recursion fibonacci" =
  go
    {|
fun fib (static x : int) : static int =
  if static x <= 1 then x else fib (x - 1) + fib (x - 2);;
let _ = fib 10;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: recursion capturing outer variable" =
  go
    {|
let offset = 10 @ dynamic;;
fun add_offset (x : int) : int =
  if x == 0 then offset else add_offset (x - 1) + 1;;
let _ = add_offset 3;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: recursive function used as value" =
  go
    {|
fun id (x : int) : int = x;;
let f = id;;
let _ = f 42;;
|};
  [%expect {| |}]
;;

(* ============================================================
   DEPENDENT TYPES AND STATIC IF
   ============================================================ *)

let%expect_test "fuzz: polymorphic identity on all base types" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = id int 42;;
let _ = id bool true;;
let _ = id unit ();;
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

let%expect_test "fuzz: dependent type computation" =
  go
    {|
let choose_type = fn (static erased b : bool) ->
  if static b then int else bool;;
let _ = (42 : choose_type true);;
let _ = (true : choose_type false);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: polymorphic identity on arrow type" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let f = id (int -> int) (fn (x : int) -> x + 1);;
let _ = f 10;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: polymorphic pair constructor" =
  go
    {|
let pair_id = fn (static erased t : type) -> fn (x : t) -> fn (y : t) -> (x, y);;
let _ = pair_id int 1 2;;
let _ = pair_id bool true false;;
|};
  [%expect {| |}]
;;

(* ============================================================
   MODE SYSTEM EDGE CASES
   ============================================================ *)

let%expect_test "fuzz: erased value in computation" =
  go
    {|
let x = 5 @ erased;;
let y = x + 3;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: dynamic erased value" =
  go
    {|
let x = 5 @ dynamic erased;;
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

let%expect_test "fuzz: erased function" =
  go
    {|
fun erased f (x : int) : int = x + 1;;
let _ = f 5;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: erased function applied multiple times" =
  go
    {|
fun erased inc (x : int) : int = x + 1;;
let _ = inc 1;;
let _ = inc 2;;
let _ = inc 3;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: mode annotation on if result" =
  go
    {|
let _ = (if true then 1 else 2) @ dynamic;;
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

(* ============================================================
   TUPLES
   ============================================================ *)

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

(* ============================================================
   IF EXPRESSIONS
   ============================================================ *)

let%expect_test "fuzz: dynamic if with closures in branches" =
  go
    {|
let b = true @ dynamic;;
let f = if b then fn (x : int) -> x + 1 else fn (x : int) -> x * 2;;
let _ = f 5;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: nested dynamic ifs" =
  go
    {|
let a = true @ dynamic;;
let b = false @ dynamic;;
let _ = if a then (if b then 1 else 2) else (if b then 3 else 4);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: if returning unit" =
  go
    {|
let b = true @ dynamic;;
let _ = if b then () else ();;
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

(* ============================================================
   SCOPING AND SHADOWING
   ============================================================ *)

let%expect_test "fuzz: variable shadowing" =
  go
    {|
let x = 1;;
let x = 2;;
let x = 3;;
let _ = x;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: shadowing in nested let" =
  go
    {|
let x = 1;;
let _ = let x = 2 in let x = 3 in x;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: shadow in closure body" =
  go
    {|
let x = 10;;
let f = fn (x : int) -> x + 1;;
let _ = f 20;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: deep let nesting" =
  go
    {|
let _ = let a = 1 in let b = a + 1 in let c = b + 1 in let d = c + 1 in d;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: same name different scopes" =
  go
    {|
let f = fn (x : int) -> x;;
let g = fn (x : bool) -> x;;
let _ = f 1;;
let _ = g true;;
|};
  [%expect {| |}]
;;

(* ============================================================
   EXTERNAL FUNCTIONS
   ============================================================ *)

let%expect_test "fuzz: external print" =
  go
    {|
external print_int : int -> unit = sylstd_print_int;;
let _ = print_int 42;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: external in higher-order context" =
  go
    {|
external print_int : int -> unit = sylstd_print_int;;
let apply = fn (f : int -> unit) -> f 42;;
let _ = apply print_int;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: multiple externals" =
  go
    {|
external print_int : int -> unit = sylstd_print_int;;
external print_bool : bool -> unit = sylstd_print_bool;;
let _ = print_int 1;;
let _ = print_bool true;;
let _ = print_int 2;;
|};
  [%expect {| |}]
;;

(* ============================================================
   ASSERT
   ============================================================ *)

let%expect_test "fuzz: runtime assert true" =
  go
    {|
let _ = assert (true @ dynamic);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: static assert" =
  go
    {|
let _ = assert static true;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: assert with comparison" =
  go
    {|
let _ = assert (1 < 2);;
|};
  [%expect {| |}]
;;

(* assert static uses value-level evaluation (correct), not simplifier *)
let%expect_test "fuzz: assert static with gte (value-level correct)" =
  go
    {|
let _ = assert static (5 >= 3);;
|};
  [%expect {| |}]
;;

(* ============================================================
   COMPLEX INTERACTIONS
   ============================================================ *)

let%expect_test "fuzz: polymorphic identity on closure type" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let inc = fn (x : int) -> x + 1;;
let f = id (int -> int) inc;;
let _ = f 10;;
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

let%expect_test "fuzz: closure returning closure" =
  go
    {|
let f = fn (x : int) -> fn (y : int) -> fn (z : int) -> x + y + z;;
let _ = f 1 2 3;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: recursive function with closure" =
  go
    {|
let add = fn (x : int) -> fn (y : int) -> x + y;;
fun loop (n : int) : int =
  if n <= 0 then 0 else add n (loop (n - 1));;
let _ = loop 5;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: mutual recursion with closures" =
  go
    {|
let inc = fn (x : int) -> x + 1;;
fun f (x : int) : int =
  if x == 0 then 0 else inc (g (x - 1))
and g (x : int) : int =
  if x == 0 then 0 else inc (f (x - 1));;
let _ = f 4;;
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

let%expect_test "fuzz: let in function body" =
  go
    {|
let f = fn (x : int) -> let y = x + 1 in let z = y * 2 in z;;
let _ = f 5;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: static recursion with dependent type" =
  go
    {|
fun choose (static b : bool) : static erased type =
  if static b then int else bool;;
let _ = (42 : choose true);;
let _ = (false : choose false);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: polymorphic identity on tuple type" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = id (int ^ int) (1, 2);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: boolean operators" =
  go
    {|
let _ = true || false;;
let _ = false && true;;
let _ = true && true;;
let _ = false || false;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: dynamic boolean operations" =
  go
    {|
let a = true @ dynamic;;
let b = false @ dynamic;;
let _ = a && b;;
let _ = a || b;;
let _ = !a;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: comparison chain" =
  go
    {|
let x = 5 @ dynamic;;
let _ = (x > 0) && (x < 10);;
|};
  [%expect {| |}]
;;

(* ============================================================
   TYPE ANNOTATIONS
   ============================================================ *)

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

let%expect_test "fuzz: tuple type annotation" =
  go
    {|
let _ = ((1, true) : int ^ bool);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: arrow type annotation" =
  go
    {|
let f = (fn (x : int) -> fn (y : int) -> x + y) : int -> int -> int;;
|};
  [%expect {| |}]
;;

(* ============================================================
   STRESS TESTS
   ============================================================ *)

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

let%expect_test "fuzz: deeply nested closure application" =
  go
    {|
let f = fn (a : int) -> fn (b : int) -> fn (c : int) -> fn (d : int) -> a + b + c + d;;
let _ = f 1 2 3 4;;
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

let%expect_test "fuzz: polymorphic with many type instantiations" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = id int 1;;
let _ = id bool true;;
let _ = id unit ();;
let _ = id (int -> int) (fn (x : int) -> x);;
|};
  [%expect {| |}]
;;

(* ============================================================
   SIMPLIFICATION EDGE CASES
   ============================================================ *)

let%expect_test "fuzz: erased computation chains" =
  go
    {|
let a = 1 @ erased;;
let b = (a + 2) @ erased;;
let c = (b * 3) @ erased;;
let _ = c + 1;;
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

let%expect_test "fuzz: erased function with captures" =
  go
    {|
let x = 10;;
fun erased helper (y : int) : int = x + y;;
let _ = helper 5;;
let _ = helper 20;;
|};
  [%expect {| |}]
;;

(* ============================================================
   POTENTIAL CRASH SITES
   ============================================================ *)

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

let%expect_test "fuzz: static erased lambda inline" =
  go
    {|
let _ = (fn (static erased t : type) -> fn (x : t) -> x) int 42;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: unit returning function" =
  go
    {|
let f = fn (_ : int) -> ();;
let _ = f 42;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: function taking unit returning int" =
  go
    {|
let f = fn (_ : unit) -> 42;;
let _ = f ();;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: function taking and returning unit" =
  go
    {|
let f = fn (_ : unit) -> ();;
let _ = f ();;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: recursive function returning unit" =
  go
    {|
fun loop (n : int) : unit =
  if n <= 0 then () else loop (n - 1);;
let _ = loop 3;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: mutual recursion with unit returns" =
  go
    {|
fun f (n : int) : unit =
  if n <= 0 then () else g (n - 1)
and g (n : int) : unit =
  if n <= 0 then () else f (n - 1);;
let _ = f 4;;
|};
  [%expect {| |}]
;;

(* ============================================================
   STATIC RECURSION EDGE CASES
   ============================================================ *)

let%expect_test "fuzz: static factorial" =
  go
    {|
fun f (static x : int) : static int =
  if static x == 0 then 1 else x * f (x - 1);;
let _ = f 5;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: static recursion with boolean" =
  go
    {|
fun f (static b : bool) : static int =
  if static b then 1 else 0;;
let _ = f true;;
let _ = f false;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: static mutual recursion" =
  go
    {|
fun f (static x : int) : static int =
  if static x <= 0 then 0 else g (x - 1) + 1
and g (static x : int) : static int =
  if static x <= 0 then 0 else f (x - 1) + 2;;
let _ = f 4;;
|};
  [%expect {| |}]
;;

(* ============================================================
   SUBTYPING
   ============================================================ *)

let%expect_test "fuzz: subtype static to dynamic" =
  go
    {|
let f = fn (x : int) -> x;;
let g = (f : dynamic int -> int);;
let _ = g (1 @ dynamic);;
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

(* ============================================================
   LET-IN EXPRESSIONS IN VARIOUS POSITIONS
   ============================================================ *)

let%expect_test "fuzz: let-in as function argument" =
  go
    {|
let f = fn (x : int) -> x + 1;;
let _ = f (let y = 10 in y);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: let-in in if condition" =
  go
    {|
let _ = if (let x = true in x) then 1 else 2;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: let-in in if branches" =
  go
    {|
let b = true @ dynamic;;
let _ = if b then (let x = 1 in x) else (let y = 2 in y);;
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

let%expect_test "fuzz: let-in binding a function" =
  go
    {|
let _ = let f = fn (x : int) -> x + 1 in f 10;;
|};
  [%expect {| |}]
;;

(* ============================================================
   COMPLEX MONOMORPHIZATION
   ============================================================ *)

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

let%expect_test "fuzz: dependent return feeding another dependent" =
  go
    {|
let choose = fn (static erased b : bool) -> if static b then int else bool;;
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = id (choose true) 42;;
let _ = id (choose false) true;;
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

(* ============================================================
   DYNAMIC CLOSURES WITH COMPLEX CAPTURE
   ============================================================ *)

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

let%expect_test "fuzz: multiple closures sharing captured variable" =
  go
    {|
let x = 10 @ dynamic;;
let f = fn (_ : unit) -> x + 1;;
let g = fn (_ : unit) -> x + 2;;
let _ = f ();;
let _ = g ();;
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

(* ============================================================
   TYPE-LEVEL PROGRAMMING
   ============================================================ *)

let%expect_test "fuzz: type variable used multiple times" =
  go
    {|
let f = fn (static erased t : type) -> fn (x : t) -> fn (y : t) -> (x, y);;
let _ = f int 1 2;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: arrow type as value" =
  go
    {|
let T = int -> int;;
let f = (fn (x : int) -> x + 1) : T;;
let _ = f 5;;
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

(* ============================================================
   MINIMAL CONSTRUCTS
   ============================================================ *)

let%expect_test "fuzz: just unit" =
  go {|let _ = ();;|};
  [%expect {| |}]
;;

let%expect_test "fuzz: just a type" =
  go {|let _ = int;;|};
  [%expect {| |}]
;;

let%expect_test "fuzz: just type of types" =
  go {|let _ = type;;|};
  [%expect {| |}]
;;

let%expect_test "fuzz: identity function minimal" =
  go {|let _ = fn (x : int) -> x;;|};
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

(* ============================================================
   DYNAMIC RECURSIVE WITH COMPLEX ENVIRONMENTS
   ============================================================ *)

let%expect_test "fuzz: recursive capturing multiple dynamic vars" =
  go
    {|
let a = 1 @ dynamic;;
let b = 2 @ dynamic;;
fun f (x : int) : int =
  if x <= 0 then a + b else f (x - 1) + 1;;
let _ = f 3;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: mutual rec different captured scopes" =
  go
    {|
let a = 10 @ dynamic;;
let b = 20 @ dynamic;;
fun f (x : int) : int =
  if x <= 0 then a else g (x - 1)
and g (x : int) : int =
  if x <= 0 then b else f (x - 1);;
let _ = f 3;;
|};
  [%expect {| |}]
;;

(* ============================================================
   COMBINING FEATURES
   ============================================================ *)

let%expect_test "fuzz: polymorphic higher-order with recursion" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
fun loop (n : int) : int =
  if n <= 0 then 0 else id int n + loop (n - 1);;
let _ = loop 5;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: dependent type choosing function type" =
  go
    {|
let choose = fn (static erased b : bool) ->
  if static b then int -> int else bool -> bool;;
let f = (fn (x : int) -> x + 1) : choose true;;
let g = (fn (x : bool) -> !x) : choose false;;
let _ = f 10;;
let _ = g true;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: static lambda with dynamic body capture" =
  go
    {|
let x = 42 @ dynamic;;
let f = fn (static erased t : type) -> fn (_ : t) -> x;;
let _ = f int 0;;
let _ = f bool true;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: erased function taking dynamic" =
  go
    {|
fun erased apply (f : int -> int) : int -> int = f;;
let inc = fn (x : int) -> x + 1;;
let _ = apply inc (5 @ dynamic);;
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

let%expect_test "fuzz: passing recursive function as argument" =
  go
    {|
fun double (x : int) : int =
  if x <= 0 then 0 else double (x - 1) + 2;;
let apply = fn (f : int -> int) -> f 5;;
let _ = apply double;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: type computed from int" =
  go
    {|
fun type_for (static x : int) : static erased type =
  if static x > 0 then int else bool;;
let _ = (42 : type_for 1);;
let _ = (true : type_for 0);;
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

let%expect_test "fuzz: if with captures in both branches" =
  go
    {|
let threshold = 10 @ dynamic;;
let classify = fn (x : int) -> if x > threshold then true else false;;
let _ = classify (5 @ dynamic);;
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

let%expect_test "fuzz: chain polymorphic through higher-order" =
  go
    {|
let f = fn (static erased t : type) -> fn (x : t) -> x;;
let g = f int;;
let h = fn (func : int -> int) -> func 42;;
let _ = h g;;
|};
  [%expect {| |}]
;;

(* ============================================================
   TARGETING SPECIFIC CODEGEN PATTERNS
   ============================================================ *)

(* Closure with zero-size env *)
let%expect_test "fuzz: closure with no captures" =
  go
    {|
let f = fn (x : int) -> x + 1;;
let _ = f 10;;
|};
  [%expect {| |}]
;;

(* Thunk (zero-arg closure) *)
let%expect_test "fuzz: thunk pattern" =
  go
    {|
let f = fn (static x : int) -> x + 1;;
let _ = f 10;;
|};
  [%expect {| |}]
;;

(* Closure with large environment *)
let%expect_test "fuzz: closure with many captures" =
  go
    {|
let a = 1 @ dynamic;;
let b = 2 @ dynamic;;
let c = 3 @ dynamic;;
let d = 4 @ dynamic;;
let e = 5 @ dynamic;;
let f = fn (_ : unit) -> a + b + c + d + e;;
let _ = f ();;
|};
  [%expect {| |}]
;;

(* If expression where both branches produce closures *)
let%expect_test "fuzz: if branches both produce closures" =
  go
    {|
let x = 1 @ dynamic;;
let b = true @ dynamic;;
let f =
  if b then fn (y : int) -> x + y
  else fn (y : int) -> x - y;;
let _ = f 10;;
|};
  [%expect {| |}]
;;

(* Recursive function that returns a closure *)
let%expect_test "fuzz: recursive function returning closure" =
  go
    {|
fun make (n : int) : int -> int =
  if n <= 0 then fn (x : int) -> x
  else fn (x : int) -> x + n;;
let f = make 5;;
let _ = f 10;;
|};
  [%expect {| |}]
;;

(* Multiple recursive functions where one captures a closure from the other *)
let%expect_test "fuzz: mutual recursion returning closures" =
  go
    {|
fun f (x : int) : int -> int =
  if x <= 0 then fn (y : int) -> y
  else g (x - 1)
and g (x : int) : int -> int =
  if x <= 0 then fn (y : int) -> y + 1
  else f (x - 1);;
let h = f 3;;
let _ = h 10;;
|};
  [%expect {| |}]
;;

(* Static lambda that returns unit *)
let%expect_test "fuzz: static lambda returning unit" =
  go
    {|
let f = fn (static erased t : type) -> fn (_ : t) -> ();;
let _ = f int 0;;
let _ = f bool true;;
|};
  [%expect {| |}]
;;

(* Tuple containing a closure that captures a tuple *)
let%expect_test "fuzz: tuple with closure capturing tuple" =
  go
    {|
let pair = (1, 2) @ dynamic;;
let f = fn (_ : unit) -> pair;;
let t = (f, 3);;
|};
  [%expect {| |}]
;;

(* Deeply nested if-else chains *)
let%expect_test "fuzz: deep if-else chain" =
  go
    {|
let x = 5 @ dynamic;;
let _ =
  if x > 10 then 100
  else if x > 5 then 50
  else if x > 0 then 25
  else 0;;
|};
  [%expect {| |}]
;;

(* Gte used in non-static if where result matters *)
let%expect_test "fuzz: gte in non-static if (exposes wrong branch)" =
  go
    {|
let _ = if (10 >= 5) then 1 else 2;;
|};
  [%expect {| |}]
;;

(* Gte used in dynamic context *)
let%expect_test "fuzz: gte with dynamic operands (not constant-folded)" =
  go
    {|
let x = 10 @ dynamic;;
let y = 5 @ dynamic;;
let _ = if x >= y then 1 else 2;;
|};
  [%expect {| |}]
;;

(* Multiple static lambdas with different type params *)
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

(* Static lambda applied to unit type *)
let%expect_test "fuzz: polymorphic identity on unit" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = id unit ();;
|};
  [%expect {| |}]
;;

(* Function that takes a tuple and returns one element (not supported?) *)
(* Tuples can't be destructured, so just pass and return them *)
let%expect_test "fuzz: tuple round-trip through function" =
  go
    {|
let roundtrip = fn (t : int ^ bool ^ unit) -> t;;
let _ = roundtrip (1, true, ());;
|};
  [%expect {| |}]
;;

(* Erased let inside non-erased context *)
let%expect_test "fuzz: erased let in non-erased scope" =
  go
    {|
let T = int;;
let x = (42 : T);;
|};
  [%expect {| |}]
;;

(* Multiple shadowed variables captured in closure *)
let%expect_test "fuzz: capture shadowed variables" =
  go
    {|
let x = 1 @ dynamic;;
let y = 2 @ dynamic;;
let x = 3 @ dynamic;;
let f = fn (_ : unit) -> x + y;;
let _ = f ();;
|};
  [%expect {| |}]
;;

(* Recursive function with erased parameters *)
let%expect_test "fuzz: recursive with mixed mode params" =
  go
    {|
fun f (static n : int) : static (int -> int) =
  fn (x : int) -> x + n;;
let add3 = f 3;;
let add7 = f 7;;
let _ = add3 10;;
let _ = add7 10;;
|};
  [%expect {| |}]
;;

(* Closure over dynamic closure *)
let%expect_test "fuzz: closure over dynamic closure" =
  go
    {|
let f = (fn (x : int) -> x + 1) @ dynamic;;
let g = fn (_ : unit) -> f 10;;
let _ = g ();;
|};
  [%expect {| |}]
;;

(* Function application in tuple position *)
let%expect_test "fuzz: function applications as tuple elements" =
  go
    {|
let inc = fn (x : int) -> x + 1;;
let dec = fn (x : int) -> x - 1;;
let t = (inc 5, dec 5);;
|};
  [%expect {| |}]
;;

(* Nested closures where inner captures from multiple levels *)
let%expect_test "fuzz: multi-level capture" =
  go
    {|
let a = 1 @ dynamic;;
let f = fn (b : int) ->
  let c = b + 1 in
  fn (_ : unit) -> a + c;;
let g = f (2 @ dynamic);;
let _ = g ();;
|};
  [%expect {| |}]
;;

(* Negative numbers *)
let%expect_test "fuzz: negative number operations" =
  go
    {|
let _ = -1 + -2;;
let _ = -(-3);;
let _ = 0 - 1;;
|};
  [%expect {| |}]
;;

(* Type-level if used as function param type *)
let%expect_test "fuzz: dependent type in function param" =
  go
    {|
let choose = fn (static erased b : bool) -> if static b then int else bool;;
let f = fn (static erased b : bool) -> fn (x : choose b) -> x;;
let _ = f true 42;;
let _ = f false true;;
|};
  [%expect {| |}]
;;
