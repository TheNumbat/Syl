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
let bad = fn (z : int) ->
  assert static ((fn (x : int) -> z) 0 == 0)
;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 2)))
     (reason
      (Mode_mismatch (got ((staticity Parametric) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "lambda static -> arg" =
  go
    {|
let bad = fn (dynamic z : int) ->
  assert static ((fn (x : int) -> z) 0 == 0)
;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 2)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "lambda static -> arg" =
  go
    {|
let f = fn (x : int) (static y : int) -> x + y;;
let _ = (f 0) @ static;;
let _ = assert static (f 0 1 == 1);;
|};
  [%expect {| |}]
;;

let%expect_test "fun static -> arg" =
  go
    {|
fun f (x : int) (static y : int) : int = x + y;;
let _ = (f 0) @ static;;
let _ = assert static (f 0 1 == 1);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 14)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "lambda static -> arg" =
  go
    {|
let f = fn (x : int) (static y : int) -> x + y;;
let _ = f (0 @ dynamic) 1;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
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
    ~print:true
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

let%expect_test "lambda _ -> arg" =
  go
    {|
let f = fn (x : int) -> x;;
let _ = assert static (f 0 == 0);;
|};
  [%expect {| |}]
;;

let%expect_test "lambda _ -> dynamic" =
  go
    {|
let f = fn (x : int) -> x @ dynamic;;
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

let%expect_test "nested lambda _ -> arg" =
  go
    {|
let f = fn (x : int) (y : int) -> x + y;;
let _ = assert static (f 0 1 == 1);;
|};
  [%expect {| |}]
;;

let%expect_test "partial app -> dynamic" =
  go
    {|
let f = fn (x : int) (y : int) -> (x + y) @ dynamic;;
let _ = (f 0 @ static);;
|};
  [%expect {| |}]
;;

let%expect_test "nested lambda _ -> arg" =
  go
    {|
let f = fn (x : int) (y : int) -> (x @ dynamic) + y;;
let _ = assert static (f 0 1 == 1);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "nested lambda _ -> arg" =
  go
    {|
let f = fn (x : int) (y : int) -> x + (y @ dynamic);;
let _ = assert static (f 0 1 == 1);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "dyn lambda _ -> arg" =
  go
    {|
let f = fn (x : int) -> x;;
let _ = assert static (((f @ dynamic) 0) == 0);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "nested lambda _ -> arg" =
  go
    {|
let f = fn (x : int) (y : int) -> x + y;;
let _ = assert static (f (0 @ dynamic) 1 == 1);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "nested lambda _ -> arg" =
  go
    {|
let f = fn (x : int) (y : int) -> x + y;;
let _ = assert static (f 0 (1 @ dynamic) == 1);;
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
  [%expect {| |}]
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

let%expect_test "fun static -> _" =
  go
    {|
fun f (static x : int) : static int = x;;
let _ = assert static ((f @ dynamic) 0 == 0);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 23)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "fun phase -> static" =
  go
    {|
fun f (x : int) : static int = x;;
let _ = assert static (f 0 == 0);;
|};
  [%expect {| |}]
;;

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

let%expect_test "fun dynamic -> static" =
  go
    {|
fun f (dynamic x : int) : int = 0;;
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
fun f (dynamic x : int) : dynamic int = 0;;
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
fun f (static x : int) : dynamic int = 0;;
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
fun f (dynamic x : int) : int = 0;;
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

let%expect_test "fun captures dynamic" =
  go
    {|
let x = 0 @ dynamic;;
fun f (y : int) : int = x;;
let _ = f @ static;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 10)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "lambda captures dynamic" =
  go
    {|
let x = 0 @ dynamic;;
let f = fn (y : int) -> x;;
let _ = f @ static;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 10)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "mono lambda captures dynamic" =
  go
    {|
let x = 0 @ dynamic;;
let f = fn (static y : int) -> x;;
let _ = f @ static;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 10)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "fun captures static" =
  go
    {|
let x = 0;;
fun f (y : int) : int = x;;
let _ = f @ static;;
|};
  [%expect {| |}]
;;

let%expect_test "mutual recursion captures dynamic" =
  go
    {|
let x = 0 @ dynamic;;
fun f (a : int) : int = g a
and g (b : int) : int = x;;
let _ = f @ static;;
|};
  [%expect
    {|
    ((loc ((line 5) (column 10)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "mutual recursion captures static" =
  go
    {|
let x = 0;;
fun f (a : int) : int = g a
and g (b : int) : int = x;;
let _ = f @ static;;
let _ = g @ static;;
|};
  [%expect {| |}]
;;

(* ============================================================ *)
(* Arrow type annotations and mode interactions                  *)
(* ============================================================ *)

(* Arrow type annotations default both arg and ret to dynamic.
   Compile-time callability must be written explicitly as -> static t. *)

let%expect_test "arrow annotation: int -> int defaults both arg and ret to dynamic" =
  go
    {|
let f = fn (x : int) -> x;;
let _ = f : int -> int;;
let _ = f : int -> dynamic int;;
let _ = f : static int -> int;;
let _ = f : dynamic int -> int;;
let _ = f : static int -> dynamic int;;
let _ = f : dynamic int -> dynamic int;;
|};
  [%expect {| |}]
;;

let%expect_test "arrow annotation: static int -> int (pi type)" =
  go
    {|
let f = fn (static x : int) -> x;;
let _ = f : static int -> static int;;
let _ = f : static int -> int;;
let _ = f : static int -> dynamic int;;
|};
  [%expect {| |}]
;;

let%expect_test "arrow annotation: dynamic int -> int" =
  go
    {|
fun f (dynamic x : int) : dynamic int = 0 @ dynamic;;
let _ = f : dynamic int -> dynamic int;;
let _ = f : int -> dynamic int;;
let _ = f : static int -> dynamic int;;
|};
  [%expect {| |}]
;;

let%expect_test "arrow annotation: int -> static int" =
  go
    {|
fun f (x : int) : static int = 0;;
let _ = f : int -> static int;;
let _ = f : int -> int;;
let _ = f : int -> dynamic int;;
|};
  [%expect {| |}]
;;

let%expect_test "arrow annotation: int -> dynamic int" =
  go
    {|
fun f (x : int) : dynamic int = 0 @ dynamic;;
let _ = f : int -> dynamic int;;
let _ = f : dynamic int -> dynamic int;;
|};
  [%expect {| |}]
;;

let%expect_test "arrow annotation: static int -> static int" =
  go
    {|
fun f (static x : int) : static int = x;;
let _ = f : static int -> static int;;
|};
  [%expect {| |}]
;;

let%expect_test "arrow annotation: static int -> dynamic int" =
  go
    {|
fun f (static x : int) : dynamic int = x;;
let _ = f : static int -> dynamic int;;
|};
  [%expect {| |}]
;;

let%expect_test "arrow annotation: dynamic int -> static int" =
  go
    {|
fun f (dynamic x : int) : static int = 0;;
let _ = f : dynamic int -> static int;;
|};
  [%expect {| |}]
;;

let%expect_test "arrow annotation: dynamic int -> dynamic int" =
  go
    {|
fun f (dynamic x : int) : dynamic int = 0 @ dynamic;;
let _ = f : dynamic int -> dynamic int;;
|};
  [%expect {| |}]
;;

(* Annotating a lambda with a plain arrow type drops compile-time return information. *)

let%expect_test "arrow annotation: int -> int loses compile-time callability" =
  go
    {|
let f = (fn (x : int) -> x) : int -> int;;
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

let%expect_test "arrow annotation: static int -> int still has dynamic return" =
  go
    {|
let f = (fn (x : int) -> x) : static int -> int;;
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

(* ============================================================ *)
(* Phase resolution at application sites                         *)
(* ============================================================ *)

let%expect_test "phase fn applied to static arg -> static result" =
  go
    {|
let f = fn (x : int) -> x + 1;;
let _ = assert static (f 5 == 6);;
|};
  [%expect {| |}]
;;

let%expect_test "phase fn applied to dynamic arg -> dynamic result" =
  go
    {|
let f = fn (x : int) -> x + 1;;
let y = 5 @ dynamic;;
let _ = assert static (f y == 6);;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "static fn applied to static arg -> static result" =
  go
    {|
fun f (static x : int) : static int = x + 1;;
let _ = assert static (f 5 == 6);;
|};
  [%expect {| |}]
;;

let%expect_test "dynamic fn applied to static arg -> dynamic result" =
  go
    {|
let f = (fn (x : int) -> x + 1) @ dynamic;;
let _ = assert static (f 5 == 6);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "named fun with omitted return mode stays dynamic on static args" =
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

let%expect_test "dynamic ret in arrow, called with static -> stays dynamic" =
  go
    {|
fun f (x : int) : dynamic int = 0 @ dynamic;;
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

(* ============================================================ *)
(* Higher-order functions and explicit static returns            *)
(* ============================================================ *)

let%expect_test "higher-order: int -> int parameter is not compile-time callable" =
  go
    {|
let apply = fn (f : int -> int) (x : int) -> f x;;
let _ = assert static (apply (fn (x : int) -> x + 1) 5 == 6);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "higher-order: int -> int plus dynamic arg stays dynamic" =
  go
    {|
let apply = fn (f : int -> int) (x : int) -> f x;;
let y = 5 @ dynamic;;
let _ = assert static (apply (fn (x : int) -> x + 1) y == 6);;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "higher-order: dynamic fn passed to int -> int param stays dynamic" =
  go
    {|
let apply = fn (f : int -> int) (x : int) -> f x;;
let g = (fn (x : int) -> x) @ dynamic;;
let _ = assert static (apply g 0 == 0);;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "higher-order: return a compile-time-callable lambda" =
  go
    {|
let mk = fn (x : int) -> fn (y : int) -> x + y;;
let add5 = mk 5;;
let _ = assert static (add5 3 == 8);;
|};
  [%expect {| |}]
;;

let%expect_test "higher-order: returned lambda becomes dynamic on dynamic arg" =
  go
    {|
let mk = fn (x : int) -> fn (y : int) -> x + y;;
let add5 = mk 5;;
let y = 3 @ dynamic;;
let _ = assert static (add5 y == 8);;
|};
  [%expect
    {|
    ((loc ((line 5) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

(* ============================================================ *)
(* Phase with let bindings                                       *)
(* ============================================================ *)

let%expect_test "let binding preserves phase resolution" =
  go
    {|
let f = fn (x : int) -> let y = x + 1 in y;;
let _ = assert static (f 0 == 1);;
|};
  [%expect {| |}]
;;

let%expect_test "let binding with dynamic forces dynamic" =
  go
    {|
let f = fn (x : int) -> let y = x @ dynamic in y;;
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

(* ============================================================ *)
(* Phase with if expressions                                     *)
(* ============================================================ *)

let%expect_test "if with phase cond, phase branches" =
  go
    {|
let f = fn (x : int) -> if x == 0 then 1 else x;;
let _ = assert static (f 0 == 1);;
|};
  [%expect {| |}]
;;

let%expect_test "if with dynamic cond, phase branches" =
  go
    {|
let f = fn (x : int) ->
  let c = (x == 0) @ dynamic in
  if c then 1 else x;;
let _ = assert static (f 0 == 1);;
|};
  [%expect
    {|
    ((loc ((line 5) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "if with phase cond, one dynamic branch" =
  go
    {|
let f = fn (x : int) -> if x == 0 then 1 @ dynamic else x;;
let _ = assert static (f 0 == 1);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

(* ============================================================ *)
(* Phase with recursion                                          *)
(* ============================================================ *)

let%expect_test "recursive fun with omitted return mode is runtime-only" =
  go
    {|
fun f (x : int) : int = if x == 0 then 0 else f (x - 1);;
let _ = assert static (f 3 == 0);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "recursive fun with explicit dynamic return stays dynamic" =
  go
    {|
fun f (x : int) : dynamic int = if x == 0 then 0 @ dynamic else f (x - 1);;
let _ = assert static (f 3 == 0);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "recursive fun: dynamic arg captures dynamic" =
  go
    {|
let offset = 10 @ dynamic;;
fun f (x : int) : dynamic int = if x == 0 then offset else f (x - 1) + 1;;
let _ = f 3;;
|};
  [%expect {| |}]
;;

let%expect_test "mutual recursion without static returns is runtime-only" =
  go
    {|
fun f (x : int) : int = if x <= 0 then 0 else g (x - 1)
and g (x : int) : int = if x <= 0 then 1 else f (x - 1);;
let _ = assert static (f 3 == 1);;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

(* ============================================================ *)
(* Phase with erasure interactions                               *)
(* ============================================================ *)

let%expect_test "phase erased lambda" =
  go
    {|
let f = fn (erased x : int) -> 0;;
let _ = assert static (f 0 == 0);;
|};
  [%expect {| |}]
;;

let%expect_test "static erased arg (pi, dependent)" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = assert static (id int 5 == 5);;
|};
  [%expect {| |}]
;;

let%expect_test "erased fun with omitted return mode is runtime-only" =
  go
    {|
fun erased f (x : int) : int = x;;
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

(* ============================================================ *)
(* Function types in higher-order positions                      *)
(* ============================================================ *)

let%expect_test "arrow type: int -> int parameter returns dynamic results" =
  go
    {|
let f = fn (g : int -> int) (x : int) -> g x;;
let _ = assert static (f (fn (x : int) -> x + 1) 5 == 6);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "arrow type: pi-typed parameter must itself be static for compile-time use" =
  go
    {|
let f = fn (g : static int -> int) -> g 5;;
let _ = assert static (f (fn (static x : int) -> x + 1) == 6);;
|};
  [%expect
    {|
    ((loc ((line 2) (column 38)))
     (reason
      (Mode_mismatch (got ((staticity Parametric) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "arrow type: dynamic function value cannot satisfy compile-time pi use" =
  go
    {|
let f = fn (g : static int -> int) -> g 5;;
let _ = f ((fn (static x : int) -> x + 1) @ dynamic);;
|};
  [%expect
    {|
    ((loc ((line 2) (column 38)))
     (reason
      (Mode_mismatch (got ((staticity Parametric) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "arrow type: static parameter alone does not make the callee return static" =
  go
    {|
let f = fn (static g : static int -> int) -> g 5;;
let _ = assert static (f (fn (static x : int) -> x + 1) == 6);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "arrow type: dynamic arg keeps dynamic" =
  go
    {|
let f = fn (g : dynamic int -> int) (x : int) -> g x;;
let _ = f;;
|};
  [%expect {| |}]
;;

(* ============================================================ *)
(* Phase resolution through nested applications                  *)
(* ============================================================ *)

let%expect_test "nested phase application: both args static" =
  go
    {|
let f = fn (x : int) (y : int) -> x + y;;
let _ = assert static (f 1 2 == 3);;
|};
  [%expect {| |}]
;;

let%expect_test "nested phase application: first arg dynamic" =
  go
    {|
let f = fn (x : int) (y : int) -> x + y;;
let a = 1 @ dynamic;;
let _ = assert static (f a 2 == 3);;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "nested phase application: second arg dynamic" =
  go
    {|
let f = fn (x : int) (y : int) -> x + y;;
let b = 2 @ dynamic;;
let _ = assert static (f 1 b == 3);;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "partial application: phase fn, static arg -> static closure" =
  go
    {|
let f = fn (x : int) (y : int) -> x + y;;
let g = f 5;;
let _ = assert static (g 3 == 8);;
|};
  [%expect {| |}]
;;

let%expect_test "partial application: phase fn, dynamic arg -> dynamic closure" =
  go
    {|
let f = fn (x : int) (y : int) -> x + y;;
let a = 5 @ dynamic;;
let g = f a;;
let _ = assert static (g 3 == 8);;
|};
  [%expect
    {|
    ((loc ((line 5) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

(* ============================================================ *)
(* Named fun declarations require explicit static returns        *)
(* ============================================================ *)

(* For named funs, the body may typecheck against a more precise internal mode,
   but the exported function is only compile-time callable when the return is
   annotated static. *)

let%expect_test "fun with omitted return is runtime-only even when body uses arg" =
  go
    {|
fun f (x : int) : int = x + 1;;
let _ = assert static (f 5 == 6);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "fun with omitted return is runtime-only even with a static body" =
  go
    {|
fun f (x : int) : int = 0;;
let _ = assert static (f 5 == 0);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "fun with static return may read an unannotated arg in the body" =
  go
    {|
fun f (x : int) : static int = x;;
|};
  [%expect {| |}]
;;

let%expect_test "fun with static return and literal body is compile-time callable" =
  go
    {|
fun f (x : int) : static int = 0;;
let _ = assert static (f 5 == 0);;
|};
  [%expect {| |}]
;;

let%expect_test "fun with static arg but omitted return is still runtime-only" =
  go
    {|
fun f (static x : int) : int = x + 1;;
let _ = assert static (f 5 == 6);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "fun with static arg and static return is compile-time callable" =
  go
    {|
fun f (static x : int) : static int = x + 1;;
let _ = assert static (f 5 == 6);;
|};
  [%expect {| |}]
;;

let%expect_test "fun with dynamic arg may still typecheck with omitted return" =
  go
    {|
fun f (dynamic x : int) : int = x;;
|};
  [%expect {| |}]
;;

let%expect_test "fun with dynamic arg and dynamic return is allowed" =
  go
    {|
fun f (dynamic x : int) : dynamic int = x;;
|};
  [%expect {| |}]
;;

(* ============================================================ *)
(* Phase with static if                                          *)
(* ============================================================ *)

let%expect_test "static if with phase cond -> error (phase is not static)" =
  go
    {|
let f = fn (x : bool) -> if static x then 1 else 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 25)))
     (reason
      (Mode_mismatch (got ((staticity Parametric) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "static if with static cond -> ok" =
  go
    {|
let f = fn (static x : bool) -> if static x then 1 else 0;;
let _ = assert static (f true == 1);;
|};
  [%expect {| |}]
;;

(* ============================================================ *)
(* Phase with assert static                                      *)
(* ============================================================ *)

let%expect_test "assert static on phase expression -> error" =
  go
    {|
let f = fn (x : int) -> assert static (x == 0);;
|};
  [%expect
    {|
    ((loc ((line 2) (column 24)))
     (reason
      (Mode_mismatch (got ((staticity Parametric) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "assert static on static expression inside phase fn -> ok" =
  go
    {|
let f = fn (x : int) -> assert static (0 == 0);;
let _ = f 5;;
|};
  [%expect {| |}]
;;

(* ============================================================ *)
(* Phase with mode annotations on expressions                    *)
(* ============================================================ *)

let%expect_test "phase expr annotated static -> error" =
  go
    {|
let f = fn (x : int) -> x @ static;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 26)))
     (reason
      (Mode_mismatch (got ((staticity Parametric) (erasure Unerased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "phase expr annotated dynamic -> ok" =
  go
    {|
let f = fn (x : int) -> x @ dynamic;;
let _ = f 5;;
|};
  [%expect {| |}]
;;

let%expect_test "static expr annotated dynamic -> ok (weakening)" =
  go
    {|
let x = 5 @ dynamic;;
let _ = x;;
|};
  [%expect {| |}]
;;

(* ============================================================ *)
(* Phase with operators                                          *)
(* ============================================================ *)

let%expect_test "phase operands: both phase -> phase result" =
  go
    {|
let f = fn (x : int) (y : int) -> x + y;;
let _ = assert static (f 1 2 == 3);;
|};
  [%expect {| |}]
;;

let%expect_test "phase operands: one dynamic -> dynamic result" =
  go
    {|
let f = fn (x : int) -> x + (0 @ dynamic);;
let _ = assert static (f 1 == 1);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

(* ============================================================ *)
(* Phase with tuples                                             *)
(* ============================================================ *)

let%expect_test "phase tuple elements" =
  go
    {|
let f = fn (x : int) (y : int) -> (x, y);;
let _ = f 1 2;;
|};
  [%expect {| |}]
;;

(* ============================================================ *)
(* Phase with composition / chaining                             *)
(* ============================================================ *)

let%expect_test "compose two phase functions -> static when all static" =
  go
    {|
let f = fn (x : int) -> x + 1;;
let g = fn (x : int) -> x * 2;;
let _ = assert static (g (f 3) == 8);;
|};
  [%expect {| |}]
;;

let%expect_test "compose: inner dynamic -> outer dynamic" =
  go
    {|
let f = fn (x : int) -> (x + 1) @ dynamic;;
let g = fn (x : int) -> x * 2;;
let _ = assert static (g (f 3) == 8);;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

(* ============================================================ *)
(* Lambda vs fun defaults                                        *)
(* ============================================================ *)

let%expect_test "lambda is compile-time callable by default; fun needs static return" =
  go
    {|
let f1 = fn (x : int) -> x + 1;;
fun f2 (x : int) : int = x + 1;;
let _ = assert static (f1 5 == f2 5);;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

(* ============================================================ *)
(* Phase with multiple layers of function application            *)
(* ============================================================ *)

let%expect_test "triple nested phase: all static" =
  go
    {|
let f = fn (x : int) (y : int) (z : int) -> x + y + z;;
let _ = assert static (f 1 2 3 == 6);;
|};
  [%expect {| |}]
;;

let%expect_test "triple nested phase: middle arg dynamic" =
  go
    {|
let f = fn (x : int) (y : int) (z : int) -> x + y + z;;
let b = 2 @ dynamic;;
let _ = assert static (f 1 b 3 == 6);;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

(* ============================================================ *)
(* Named funs capturing dynamic values                           *)
(* ============================================================ *)

let%expect_test "fun capturing dynamic outer with dynamic return is allowed" =
  go
    {|
let d = 10 @ dynamic;;
fun f (x : int) : dynamic int = d + x;;
let _ = f 3;;
|};
  [%expect {| |}]
;;

let%expect_test "fun capturing dynamic outer with omitted return is also allowed" =
  go
    {|
let d = 10 @ dynamic;;
fun f (x : int) : int = d + x;;
|};
  [%expect {| |}]
;;

(* ============================================================ *)
(* Phase with erased functions (inlining)                        *)
(* ============================================================ *)

let%expect_test "erased phase fn: inlined at callsite, static" =
  go
    {|
let f = (fn (x : int) -> x + 1) @ erased;;
let _ = assert static (f 5 == 6);;
|};
  [%expect {| |}]
;;

(* ============================================================ *)
(* Phase subtyping in arrow positions                            *)
(* ============================================================ *)

let%expect_test "phase arrow subtype of dynamic arrow (covariant ret)" =
  go
    {|
let f = fn (x : int) -> x;;
let _ = f : int -> dynamic int;;
|};
  [%expect {| |}]
;;

let%expect_test "dynamic arrow not subtype of static arrow (ret)" =
  go
    {|
fun f (x : int) : dynamic int = 0 @ dynamic;;
let _ = f : int -> static int;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 10)))
     (reason
      (Type_mismatch
       (got
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret_ty (Type Int))
          (ret_mode ((staticity Dynamic) (erasure Unerased))))))
       (need
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret_ty (Type Int))
          (ret_mode ((staticity Static) (erasure Unerased)))))))))
    |}]
;;

(* ============================================================ *)
(* Phase with unreachable                                        *)
(* ============================================================ *)

let%expect_test "unreachable in phase context" =
  go
    {|
let f = fn (x : bool) -> if x then 1 else unreachable;;
let _ = assert static (f true == 1);;
|};
  [%expect {| ((loc ((line 2) (column 42))) (reason Unreachable_reached)) |}]
;;

let%expect_test "unreachable in static if -> ok" =
  go
    {|
let f = fn (static x : bool) -> if static x then 1 else unreachable;;
let _ = assert static (f true == 1);;
|};
  [%expect {| |}]
;;
