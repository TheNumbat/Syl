open! Core
open! Syl

let go ?(print = false) input =
  let cst = Parse.parse_exn input in
  let dst = Desugar.desugar cst in
  match Typecheck.typecheck dst with
  | Ok tst -> if print then print_s [%message (tst : Tst.Program.t)]
  | Error { loc; reason } -> print_s [%message (loc : Lex.Location.t) (reason : Typecheck.Error.t)]
;;

let%expect_test "lambda static -> arg" =
  go
    {|
let f = fn (static x : int) -> x;;
let _ = assert static (f 0 == 0);;
|};
  [%expect {| |}]
;;

let%expect_test "lambda static -> static" =
  go
    {|
let f = fn (static x : int) -> 0;;
let _ = assert static (f 0 == 0);;
|};
  [%expect {| |}]
;;

let%expect_test "lambda static -> dynamic" =
  go
    {|
let f = fn (static x : int) -> 0 @ dynamic;;
let _ = assert static (f 0 == 0);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

(* TODO should be accepted *)
let%expect_test "lambda _ -> arg" =
  go
    {|
let f = fn (x : int) -> x;;
let _ = assert static (f 0 == 0);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "lambda _ -> static" =
  go
    {|
let f = fn (x : int) -> 0;;
let _ = assert static (f 0 == 0);;
|};
  [%expect {| |}]
;;

let%expect_test "lambda _ -> dynamic" =
  go
    {|
let f = fn (x : int) -> 0 @ dynamic;;
let _ = assert static (f 0 == 0);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "lambda dynamic -> arg" =
  go
    {|
let f = fn (dynamic x : int) -> x;;
let _ = assert static (f 0 == 0);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "lambda dynamic -> static" =
  go
    {|
let f = fn (dynamic x : int) -> 0;;
let _ = assert static (f 0 == 0);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "lambda dynamic -> dynamic" =
  go
    {|
let f = fn (dynamic x : int) -> 0 @ dynamic;;
let _ = assert static (f 0 == 0);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "fun static -> static" =
  go
    {|
fun f (static x : int) : static int = x;;
let _ = assert static (f 0 == 0);;
|};
  [%expect {| |}]
;;

(* TODO should be accepted *)
let%expect_test "fun static -> _" =
  go
    {|
fun f (static x : int) : int = x;;
let _ = assert static (f 0 == 0);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "fun static -> dynamic" =
  go
    {|
fun f (static x : int) : dynamic int = x;;
let _ = assert static (f 0 == 0);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "fun _ -> static" =
  go
    {|
fun f (x : int) : static int = 0;;
let _ = assert static (f 0 == 0);;
|};
  [%expect {| |}]
;;

(* TODO should be accepted *)
let%expect_test "fun _ -> static" =
  go
    {|
fun f (x : int) : static int = 0;;
let _ = assert static (f (0 @ dynamic) == 0);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

(* TODO should be accepted *)
let%expect_test "fun _ -> _" =
  go
    {|
fun f (x : int) : int = x;;
let _ = assert static (f 0 == 0);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "fun _ -> dynamic" =
  go
    {|
fun f (x : int) : dynamic int = x;;
let _ = assert static (f 0 == 0);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "fun dynamic -> static" =
  go
    {|
fun f (dynamic x : int) : static int = 0;;
let _ = assert static (f 0 == 0);;
|};
  [%expect {| |}]
;;

(* TODO should be accepted *)
let%expect_test "fun dynamic -> static" =
  go
    {|
fun f (dynamic x : int) : static int = 0;;
let _ = assert static (f (0 @ dynamic) == 0);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "fun dynamic -> _" =
  go
    {|
fun f (dynamic x : int) : int = x;;
let _ = assert static (f 0 == 0);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "fun dynamic -> dynamic" =
  go
    {|
fun f (dynamic x : int) : dynamic int = x;;
let _ = assert static (f 0 == 0);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;
