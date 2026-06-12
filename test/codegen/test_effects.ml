open! Core
open! Syl

let go ?print ?(check = `Run) input = Common.codegen ?print ~check input

let%expect_test "static lambda effects" =
  go
    {|
external print_int : int -> unit = syl_std_print_int;;
let print = fn (static x : int) -> print_int x;;
let _ = print 0;;
let _ = print 0;;
let _ = print 1;;
|};
  [%expect
    {|
    0
    0
    1
    |}]
;;

let%expect_test "static lambda effects" =
  go
    {|
external print_int : int -> unit = syl_std_print_int;;
let _ =
let print = fn (static x : int) -> print_int x in
let _ = print 0 in
let _ = print 0 in
let _ = print 1 in
();;
|};
  [%expect
    {|
    0
    0
    1
    |}]
;;

let%expect_test "static lambda effects" =
  go
    {|
external print_int : int -> unit = syl_std_print_int;;
let print = fn (static x : int) ->
  let _ = print_int x in
  fn (static y : int) -> print_int y;;
let _ = print 0 1;;
let _ = print 1 2;;
let _ = print 1 3;;
|};
  [%expect
    {|
    0
    1
    1
    2
    1
    3
    |}]
;;

let%expect_test "monomorphizing side effects" =
  go
    {|
external print_int : int -> unit = syl_std_print_int;;
fun print (static _ : unit) : unit = print_int 0;;
let _ = print ();;
|};
  [%expect {| 0 |}]
;;

let%expect_test "external" =
  go
    {|
external f : int -> unit = syl_std_print_int;;
let _ = f 0;;
|};
  [%expect {| 0 |}]
;;

let%expect_test "prim" =
  go
    {|
builtin f = syl_int_add;;
let a = f (0, 1);;
let b = f (2, 3);;
let _ = print_int a, print_int b;;
|};
  [%expect
    {|
    1
    5
    |}]
;;

let%expect_test "external" =
  go
    {|
external f : int -> unit = syl_std_print_int;;
let _ = f 0;;
let _ = f 1;;
|};
  [%expect
    {|
    0
    1
    |}]
;;

let%expect_test "external" =
  go
    {|
external f : int -> unit = syl_std_print_int;;
let g = f;;
let _ = g 0;;
let _ = g 1;;
|};
  [%expect
    {|
    0
    1
    |}]
;;

let%expect_test "external" =
  go
    {|
external print_int : int -> unit = syl_std_print_int;;
let apply = fn (static g : int -> unit) -> g 42;;
let _ = apply print_int;;
|};
  [%expect {| 42 |}]
;;

let%expect_test "static lambda effects" =
  go
    {|
external print_int : int -> unit = syl_std_print_int;;
let print = fn (static x : int) -> print_int x;;
let _ = print 0;;
let _ = print 1;;
|};
  [%expect
    {|
    0
    1
    |}]
;;

let%expect_test "static lambda effects" =
  go
    {|
fun mk_ident (static erased pick_t : static unit -> static erased type) : static (pick_t () -> pick_t ()) =
  fn (x : pick_t ()) -> let _ = print_int 1 in x
;;

let b = mk_ident (fn (static _ : unit) -> let _ = print_int 0 in if 1 + 1 == 2 then bool else unit) true;;
let _ = print_bool b;;
|};
  [%expect
    {|
    1
    true
    |}]
;;

let%expect_test "static lambda effects" =
  go
    {|
fun mk_ident (static pick_t : static unit -> static int) : static (let t = if pick_t () == 0 then int else bool in t -> t) =
  fn (x : if pick_t () == 0 then int else bool) -> let _ = print_int 1 in x
;;

let b = mk_ident (fn (static _ : unit) -> 1) true;;
let _ = print_bool b;;
|};
  [%expect
    {|
    1
    true
    |}]
;;

let%expect_test "static lambda effects are thunked" =
  go
    {|
external print_int : int -> unit = syl_std_print_int;;

fun mk_ident (static pick_t : static unit -> static int) : static (let t = if pick_t () == 0 then int else bool in t -> t) =
  let _ = pick_t () in
  let _ = pick_t () in
  fn (x : if pick_t () == 0 then int else bool) -> x
;;

let _ = mk_ident (fn (static _ : unit) -> let _ = print_int 10 in 1) true;;
|};
  [%expect
    {|
    10
    10
    |}]
;;

let%expect_test "external" =
  go
    {|
external print_int : int -> unit = syl_std_print_int;;
|};
  [%expect {| |}]
;;

let%expect_test "scoping" =
  go
    {|
let x = 1 @ dynamic;;
let _ = let _ = let _ = x + x in x + x in x + x;;
|};
  [%expect {| |}]
;;

let%expect_test "scoping" =
  go
    {|
let c = true @ dynamic;;
let _ = if c then 0 else if !c then 1 else 2;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: external print" =
  go
    {|
external print_int : int -> unit = syl_std_print_int;;
let _ = print_int 42;;
|};
  [%expect {| 42 |}]
;;

let%expect_test "fuzz: external in higher-order context" =
  go
    {|
external print_int : int -> unit = syl_std_print_int;;
let apply = fn (f : int -> unit) -> f 42;;
let _ = apply print_int;;
|};
  [%expect {| 42 |}]
;;

let%expect_test "fuzz: multiple externals" =
  go
    {|
external print_int : int -> unit = syl_std_print_int;;
external print_bool : bool -> unit = syl_std_print_bool;;
let _ = print_int 1;;
let _ = print_bool true;;
let _ = print_int 2;;
|};
  [%expect
    {|
    1
    true
    2
    |}]
;;

let%expect_test "apply fold: lambda returning non-pack preserves effects" =
  go
    {|
external print_int : int -> unit = syl_std_print_int;;
let _ = (fn (x : int) -> let _ = print_int x in x) 10;;
|};
  [%expect {| 10 |}]
;;

let%expect_test "apply fold: non-lambda returning non-pack preserves effects" =
  go
    {|
external print_int : int -> unit = syl_std_print_int;;
let f = fn (x : int) -> let _ = print_int x in x;;
let _ = f 20;;
|};
  [%expect {| 20 |}]
;;

let%expect_test "apply fold: lambda returning pack preserves effects" =
  go
    {|
external print_int : int -> unit = syl_std_print_int;;
let p =
  (fn (tag : int) ->
    let _ = print_int tag in
    fn (static k : int) -> print_int k)
    30
;;
let _ = p 31;;
|};
  [%expect
    {|
    30
    31
    |}]
;;

let%expect_test "apply fold: lambda returning pack captures argument" =
  go
    {|
external print_int : int -> unit = syl_std_print_int;;
let p =
  (fn (tag : int) ->
    fn (static k : int) -> print_int (tag + k))
    60
;;
let _ = p 7;;
|};
  [%expect {| 67 |}]
;;

let%expect_test "pack-returning thunk effects at each function level" =
  go
    {|
external print_int : int -> unit = syl_std_print_int;;
let make = fn (static i : int) ->
  let _ = print_int (100 + i) in
  fn (static j : int) ->
    let _ = print_int (200 + j) in
    fn (static k : int) ->
      let _ = print_int (300 + k) in
      print_int (i * 100 + j * 10 + k)
;;
let mid = make 1;;
let low = mid 2;;
let _ = low 4;;
let _ = mid 3 5;;
let _ = low 6;;
|};
  [%expect
    {|
    101
    202
    304
    124
    203
    305
    135
    306
    126
    |}]
;;

let%expect_test "symbol fold preserves effects" =
  go
    {|
external print_int : int -> unit = syl_std_print_int;;
let _ = (fn (static k : int) -> print_int k) 50;;
|};
  [%expect {| 50 |}]
;;

let%expect_test "symbol captures only the selected pack specialization" =
  go
    {|
external print_int : int -> unit = syl_std_print_int;;
let p = fn (static k : int) -> print_int k;;
let f = fn (_ : unit) -> p 1;;
let _ = p 2;;
let _ = f ();;
|};
  [%expect
    {|
    2
    1
    |}]
;;
