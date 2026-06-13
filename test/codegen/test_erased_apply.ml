open! Core
open! Syl

let go ?print ?(check = `Run) input = Common.codegen ?print ~check input

let%expect_test "type-level function drives codegen through erased application" =
  go
    {|
let pick = fn (static erased b : bool) -> if erased b then int else bool;;
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = print_int (id (pick true) 7);;
let _ = print_bool (id (pick false) true);;
|};
  [%expect
    {|
    7
    true
    |}]
;;

let%expect_test "erased recursion computes a type used at runtime" =
  go
    {|
fun erased pick (static n : int) : erased type =
  if erased n == 0 then int else pick (n - 1)
;;
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = print_int (id (pick 0) 1);;
let _ = print_int (id (pick 4) 2);;
|};
  [%expect
    {|
    1
    2
    |}]
;;

let%expect_test "erased application keys a monomorphization" =
  go
    {|
fun erased depth (static u : unit) : erased int = 3;;
let show = fn (static erased n : int) -> fn (u : unit) -> print_int 7;;
let _ = show (depth ()) ();;
|};
  [%expect {| 7 |}]
;;

let%expect_test "erased application discarded inside a runtime body" =
  go
    {|
fun erased ghost (static u : unit) : erased int = 9;;
let f = fn (x : int) -> let _ = ghost () in x + 1;;
let _ = print_int (f 1);;
|};
  [%expect {| 2 |}]
;;

let%expect_test "mono body containing an erased application is quoted" =
  go
    {|
fun erased ghost (static u : unit) : erased int = 9;;
let use = fn (static f : int -> int) -> f 2;;
let g = fn (x : int) -> let _ = ghost () in x * 2;;
let _ = print_int (use g);;
|};
  [%expect {| 4 |}]
;;

let%expect_test "fun group with an erased sibling is quoted" =
  go
    {|
let use = fn (static f : int -> int) -> f 2;;
fun f (x : int) : int = x + 1
and erased g (static u : unit) : erased int = 0 @ erased;;
let _ = print_int (use f);;
|};
  [%expect {| 3 |}]
;;

let%expect_test "erased twin of a quoted closure does not leak into its env" =
  go
    {|
let n = 5;;
let add_n = fn (x : int) -> x + n;;
let ghost = add_n @ erased;;
let use = fn (static f : int -> int) -> f 2;;
let _ = print_int (use add_n);;
|};
  [%expect {| 7 |}]
;;

let%expect_test "per-value result captures survive alongside erased consumers" =
  go
    {|
let make = fn (b : bool) -> fn (static erased t : type) -> fn (v : int) -> if b then v else 0 - v;;
let g1 = make true;;
let g2 = make false;;
let use = fn (static h : int -> int) -> h 7;;
let _ = print_int (use (g1 int));;
let _ = print_int (use (g2 int));;
let ghost = (g1 int) @ erased;;
|};
  [%expect
    {|
    7
    -7
    |}]
;;

let%expect_test "late mono of a quoted binder under an erased type key" =
  go
    {|
fun erased pick (static u : unit) : erased type = int;;
let use = fn (static p : static erased type \ s -> s -> s) -> p (pick ()) 1;;
let id_t = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = print_int (use id_t);;
let _ = print_int (id_t (pick ()) 5);;
|};
  [%expect
    {|
    1
    5
    |}]
;;

let%expect_test "ghost computation keys a runtime monomorphization" =
  go
    {|
let ghost_inc = (fn (static x : int) -> x + 1) @ erased;;
let show = fn (static erased n : int) -> fn (u : unit) -> print_int 7;;
let _ = show (ghost_inc 1) ();;
|};
  [%expect {| 7 |}]
;;

let%expect_test "ghost computation drives a dependent type" =
  go
    {|
let ghost_pos = (fn (static x : int) -> x > 0) @ erased;;
let pick = fn (static erased b : bool) -> if erased b then int else bool;;
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = print_int (id (pick (ghost_pos 3)) 9);;
let _ = print_bool (id (pick (ghost_pos 0)) true);;
|};
  [%expect
    {|
    9
    true
    |}]
;;

(* The mono of (lit, 7) exists only in the memo: the sole application is
   erased, so no dispatch names it and no thunk is compiled. *)
let%expect_test "ghost-only specialization is not compiled" =
  go
    {|
let lit = fn (static k : int) -> k + 0;;
let id2 = fn (erased z : int) -> 5;;
let _ = print_int (id2 ((lit 7) @ erased));;
|};
  [%expect {| 5 |}]
;;
