open! Core
open! Syl

let go ?print ?(check = `Run) input = Common.codegen ?print ~check input

let%expect_test "static closure" =
  go
    {|
let _ =
  (fn (x : int) -> x)
;;|};
  [%expect {| |}]
;;

let%expect_test "dynamic closure" =
  go
    {|
let y = 1 @ dynamic;;
let f =
  (fn (x : int) -> x + y)
;;
let _ = print_int (f 2);;
|};
  [%expect {| 3 |}]
;;

let%expect_test "closure erased" =
  go
    {|
let x = (fn (x : int) -> x) @ erased;;
|};
  [%expect {| |}]
;;

let%expect_test "closure erased" =
  go
    {|
let x = (fn (erased x : int) -> x) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "closure erased" =
  go
    {|
let x = ((fn (erased x : int) -> 1) @ erased) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "closure erased" =
  go
    {|
let x = ((fn (erased x : int) -> 1) @ erased) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "closure branches" =
  go
    {|
let h = if true @ dynamic
        then let _ = print_int 1 in (fn (erased x : int) -> 1)
        else let _ = print_int 2 in (fn (static x : int) -> 2);;
|};
  [%expect {| 1 |}]
;;

let%expect_test "closure branches" =
  go
    {|
let h = if true then (fn (erased x : int) -> 1) else (fn (static x : int) -> 2);;
let _ = h 1;;
|};
  [%expect {| |}]
;;

let%expect_test "closure branches" =
  go
    {|
let f = (fn (erased x : int) -> 1);;
let g = (fn (static x : int) -> 2);;
let h = if true then f else g;;
let _ = h 1;;
|};
  [%expect {| |}]
;;

let%expect_test "closure branches" =
  go
    {|
let f = (fn (erased x : int) -> 1);;
let g = (fn (static x : int) -> 2);;
let _ = print_int ((if erased true then f else g) 0);;
|};
  [%expect {| 1 |}]
;;

let%expect_test "closure branches" =
  go
    {|
let f = (fn (erased x : int) -> 1);;
let g = (fn (static x : int) -> 2);;
let _ = print_int ((if true then f else g) 0);;
|};
  [%expect {| 1 |}]
;;

let%expect_test "closure branches" =
  go
    {|
let f = (fn (static erased x : int) -> 1);;
let g = (fn (static x : int) -> 2);;
let h = if true then f else g;;
let _ = h 0;;
|};
  [%expect {| |}]
;;

let%expect_test "closure branches" =
  go
    {|
let f = (fn (static erased x : int) -> 1);;
let g = (fn (static erased x : int) -> 2);;
let _ = if true then f else g;;
|};
  [%expect {| |}]
;;

let%expect_test "closure erased" =
  go
    {|
let c = fn (_ : unit) -> let _ = print_unit () in false;;
let f = (fn (x : int) -> 1);;
let g = (fn (erased x : int) -> 2);;
let _ = (if c () then f else let _ = print_unit () in g) 0;;
|};
  [%expect
    {|
    ()
    ()
    |}]
;;

let%expect_test "primitive branch" =
  go
    {|
let _ = print_int ((if true then ( + ) else ( - )) (1, 1));;
let _ = print_int ((if false then ( + ) else ( - )) (1, 1));;
|};
  [%expect
    {|
    2
    0
    |}]
;;

let%expect_test "subtyped primitive" =
  go
    {|
let f = fn (static x : int ^ int) -> match x with | (a,b) -> a - b;;
let _ = print_int ((if true then (+) else f) (1, 1));;
let _ = print_int ((if false then (+) else f) (1, 1));;
|};
  [%expect
    {|
    2
    0
    |}]
;;

let%expect_test "closure erased" =
  go
    {|
let c = fn (_ : unit) -> let _ = print_unit () in true;;
let f = (fn (x : int) -> 1);;
let g = (fn (erased x : int) -> 2);;
let _ = (if c () then f else g) 0;;
let _ = (if !(c ()) then f else g) 1;;
|};
  [%expect
    {|
    ()
    ()
    |}]
;;

let%expect_test "closure erased" =
  go
    {|
let c = fn (_ : unit) -> let _ = print_unit () in true;;
let f = (fn (x : int) -> 1);;
let g = (fn (erased x : int) -> 2);;
let _ = (match c () with | true -> f | false -> g) 0;;
let _ = (match !(c ()) with | true -> f | false -> g) 0;;
|};
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  ("Bug: forced dynamic" (loc ((line 5) (column 9)))
    (here lib/typecheck.ml:1446:51))
  Raised at Base__Error.raise in file "src/error.ml" (inlined), line 9, characters 21-37
  Called from Base__Error.raise_s in file "src/error.ml", line 10, characters 26-47
  Called from CamlinternalLazy.do_force_block in file "camlinternalLazy.ml", line 49, characters 17-27
  Re-raised at CamlinternalLazy.do_force_block in file "camlinternalLazy.ml", line 56, characters 4-11
  Called from CamlinternalLazy.force_lazy_block in file "camlinternalLazy.ml" (inlined), line 78, characters 27-67
  Called from Syl__Typecheck.force_or_var in file "lib/typecheck.ml", line 270, characters 6-23
  Called from Syl__Typecheck.typecheck_apply in file "lib/typecheck.ml", line 1137, characters 17-50
  Called from Base__Exn.protectx in file "src/exn.ml", line 79, characters 8-11
  Re-raised at Base__Exn.raise_with_original_backtrace in file "src/exn.ml" (inlined), line 59, characters 2-50
  Called from Base__Exn.protectx in file "src/exn.ml", line 86, characters 13-49
  Called from Syl__Typecheck.typecheck_top_level in file "lib/typecheck.ml", line 1452, characters 26-50
  Called from Syl__Typecheck.fold_top_levels.(fun) in file "lib/typecheck.ml", line 1479, characters 18-57
  Called from Base__List0.fold in file "src/list0.ml", line 43, characters 27-37
  Called from Syl__Typecheck.typecheck_exn in file "lib/typecheck.ml", line 1486, characters 30-75
  Called from Common.codegen in file "test/common.ml", line 143, characters 12-39
  Called from Syl_test_codegen__Test_closures.(fun) in file "test/codegen/test_closures.ml", lines 189-196, characters 2-2
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 142, characters 10-28
  |}]
;;

let%expect_test "closure nest" =
  go
    {|
let f = fn (x : int) -> 1;;
let g = fn (f : int -> int) -> f 0;;
let _ = g f;;
|};
  [%expect {| |}]
;;

let%expect_test "closure nest" =
  go
    {|
let f = fn (x : int) -> 1;;
let g = fn (static f : int -> int) -> f 0;;
let _ = g f;;
|};
  [%expect {| |}]
;;

let%expect_test "closure nest" =
  go
    {|
let f = fn (erased x : int) -> 1;;
let g = fn (static f : erased int -> int) -> f 0;;
let _ = g f;;
|};
  [%expect {| |}]
;;

let%expect_test "closure nest" =
  go
    {|
let f1 = fn (x : int) -> 1;;
let g = fn (static f2 : int -> int) -> f2 0;;
let _ = g f1;;
|};
  [%expect {| |}]
;;

let%expect_test "closure static" =
  go
    {|
let x = (fn (static x : int) -> x) 0;;
|};
  [%expect {| |}]
;;

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

let%expect_test "fuzz: closure capturing unit" =
  go
    {|
let u = ();;
let f = fn (_ : unit) -> u;;
let _ = f ();;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: dynamic if with closures in branches" =
  go
    {|
let b = true @ dynamic;;
let f = if b then fn (x : int) -> x + 1 else fn (x : int) -> x * 2;;
let _ = f 5;;
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

let%expect_test "fuzz: closure returning closure" =
  go
    {|
let f = fn (x : int) -> fn (y : int) -> fn (z : int) -> x + y + z;;
let _ = f 1 2 3;;
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

let%expect_test "fuzz: closure with no captures" =
  go
    {|
let f = fn (x : int) -> x + 1;;
let _ = f 10;;
|};
  [%expect {| |}]
;;

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

let%expect_test "fuzz: closure over dynamic closure" =
  go
    {|
let f = (fn (x : int) -> x + 1) @ dynamic;;
let g = fn (_ : unit) -> f 10;;
let _ = g ();;
|};
  [%expect {| |}]
;;
