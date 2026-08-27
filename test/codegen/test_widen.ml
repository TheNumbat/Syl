open! Core
open! Syl

let go ?print ?(check = `Run) input = Common.codegen ?print ~check input

(* An arrow weakens to a pi (every call through the pi view is a
   specialization with a known target), and every function value shares one
   layout — a code pointer plus an environment — so the weakening changes
   nothing at runtime: merges, annotations, tuples, and variant payloads
   cross formers as plain copies, and call sites cast to the signature they
   know statically. *)

let%expect_test "an if joins an arrow with a pi at a value" =
  go
    {|
fun pick (dynamic b : bool) : dynamic int =
  let _ = print_int 9 in
  let h = if b then (fn (x : int) -> x + 1) else (fn (static x : int) -> x + 6) in
  0;;
let _ = print_int (pick (true @ dynamic));;
|};
  [%expect
    {|
    9
    0
    |}]
;;

let%expect_test "a higher-order if widens the pi side through a trampoline" =
  go
    {|
fun usea (k : int -> int) : dynamic int = k 3;;
fun useb (k : static int -> int) : dynamic int = 7;;
fun pick (dynamic b : bool) : dynamic int =
  let h = if b then usea else useb in
  h (fn (x : int) -> x + 1);;
let _ = print_int (pick (true @ dynamic));;
let _ = print_int (pick (false @ dynamic));;
|};
  [%expect
    {|
    4
    7
    |}]
;;

let%expect_test "a direct annotation widens without any join" =
  go
    {|
fun useb (k : static int -> int) : dynamic int = 7;;
fun pick (dynamic b : bool) : dynamic int =
  let u2 = useb : (int -> int) -> dynamic int in
  u2 (fn (x : int) -> x + 1);;
let _ = print_int (pick (true @ dynamic));;
|};
  [%expect {| 7 |}]
;;

let%expect_test "a tuple-nested crossing widens memberwise" =
  go
    {|
fun pick (dynamic b : bool) : dynamic int =
  let h = if b then ((fn (x : int) -> x + 1), 1) else ((fn (static x : int) -> x + 6), 2) in
  let p = box h in
  match p { &(_, n) -> n };;
let _ = print_int (pick (true @ dynamic));;
let _ = print_int (pick (false @ dynamic));;
|};
  [%expect
    {|
    1
    2
    |}]
;;

let%expect_test "a dynamic arrow annotated at a pi stays a working value" =
  go
    {|
fun w (f : int -> int) : dynamic int =
  let k = f : static int -> int in
  let p = box (k, 3) in
  match p { &(_, n) -> n };;
let _ = print_int (w (fn (x : int) -> x + 1));;
|};
  [%expect {| 3 |}]
;;

let%expect_test "a variant payload crossing is layout-invisible" =
  go
    {|
fun pick (dynamic b : bool) : dynamic int =
  let h =
    if b then ((variant { none, f : int -> int }).f (fn (x : int) -> x + 1))
    else ((variant { none, f : static int -> int }).f (fn (static x : int) -> x + 6)) in
  match h { .f _ -> 5, .none -> 0 };;
let _ = print_int (pick (true @ dynamic));;
let _ = print_int (pick (false @ dynamic));;
|};
  [%expect
    {|
    5
    5
    |}]
;;
