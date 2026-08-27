open! Core
open! Syl

let go ?print ?(check = `Run) input = Common.codegen ?print ~check input

(* The typecase resolves per instance: each mono splices the selected extern
   with no runtime dispatch. *)
let%expect_test "polymorphic print" =
  go
    {|
fun print (erased t : type) : t -> dynamic unit =
  match erased repr t {
    .unit -> print_unit,
    .bool -> print_bool,
    .int -> print_int,
    _ -> unreachable,
  }
;;

let _ = print int 42;;
let _ = print bool true;;
let _ = print unit ();;
|};
  [%expect
    {|
    42
    true
    ()
    |}]
;;

(* An erased index crosses back to runtime through a typecase-selected
   unerase. *)
let%expect_test "unerase through typecase" =
  go
    {|
fun erased unerase (erased t : type) : erased (erased t -> t) =
  match erased repr t {
    .unit -> unerase_unit,
    .bool -> unerase_bool,
    .int -> unerase_int,
    _ -> unreachable,
  }
;;

fun f (static erased n : int) : dynamic unit =
  print_int (unerase int n)
;;

let _ = f 7;;
let _ = f 8;;
|};
  [%expect
    {|
    7
    8
    |}]
;;
