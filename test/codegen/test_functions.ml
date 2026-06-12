open! Core
open! Syl

let go ?print ?(check = `Run) input = Common.codegen ?print ~check input

let%expect_test "Lambda dynamic arg" =
  go
    {|
let _ =
  fn (x : int) -> x
;;|};
  [%expect {| |}]
;;

let%expect_test "Lambda capturing dynamic var" =
  go
    {|
let x = 1 @ dynamic;;
let _ =
  fn (y : int) -> x
;;|};
  [%expect {| |}]
;;

let%expect_test "Lambda capturing static var" =
  go
    {|
let x = 1 @ static;;
let _ =
  fn (y : int) -> x
;;|};
  [%expect {| |}]
;;

let%expect_test "Lambda capturing erased var" =
  go
    {|
let x = 1 @ static erased;;
let _ =
  fn (y : int) -> x
;;|};
  [%expect {| |}]
;;

let%expect_test "Lambda capturing erased var" =
  go
    {|
let x = 1 @ static erased;;
let _ =
  (fn (y : int) -> x) 0
;;|};
  [%expect {| |}]
;;

let%expect_test "mono fn" =
  go
    {|
let x = fn (static erased x : type) -> x;;
let y = x int;;
|};
  [%expect {| |}]
;;

let%expect_test "mono fn" =
  go
    {|
let x = fn (static x : type) -> x;;
let y = x @ dynamic;;
|};
  [%expect {| |}]
;;

let%expect_test "mono fn" =
  go
    {|
let x = fn (static x : type) -> x;;
let y = x @ unerased;;
|};
  [%expect {| |}]
;;

let%expect_test "Fun return static" =
  go
    {|
fun f (x : int) : int = 1;;
|};
  [%expect {| |}]
;;

let%expect_test "mono fun" =
  go
    {|
fun x (static x : int) : int = x;;
let y = x 0;;
|};
  [%expect {| |}]
;;

let%expect_test "erased fun " =
  go
    {|
fun id (erased x : int) : erased int = x;;
let _ = id 0;;
|};
  [%expect {| |}]
;;

let%expect_test "return fn" =
  go
    {|
fun x (_ : unit) : unit -> unit = fn (_ : unit) -> ();;
let _ = x () ();;
|};
  [%expect {| |}]
;;

let%expect_test "arg fn" =
  go
    {|
fun x (f : unit -> int) : int = f ();;
let _ = x (fn (_ : unit) -> 1);;
|};
  [%expect {| |}]
;;

let%expect_test "fun env" =
  go
    {|
let a = 0 @ dynamic;;
fun f (x : int) : int = let _ = a in x;;
|};
  [%expect {| |}]
;;

let%expect_test "local fun inside top-level fun" =
  go
    {|
fun outer (x : int) : int =
  fun inner (y : int) : int = y + x in
  inner x;;
let _ = print_int (outer 5);;
|};
  [%expect {| 10 |}]
;;

let%expect_test "fuzz: let in function body" =
  go
    {|
let f = fn (x : int) -> let y = x + 1 in let z = y * 2 in z;;
let _ = f 5;;
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

let%expect_test "fuzz: let-in as function argument" =
  go
    {|
let f = fn (x : int) -> x + 1;;
let _ = f (let y = 10 in y);;
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

let%expect_test "fuzz: identity function minimal" =
  go {|let _ = fn (x : int) -> x;;|};
  [%expect {| |}]
;;
