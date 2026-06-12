open! Core
open! Syl

let go = Common.typecheck

let%expect_test "lambda static -> arg" =
  go
    {|
let bad = fn (z : int) ->
  assert erased ((fn (x : int) -> z) 0 == 0)
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
  assert erased ((fn (x : int) -> z) 0 == 0)
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
let f = fn (x : int) -> fn (static y : int) -> x + y;;
let _ = (f 0) @ static;;
let _ = assert erased (f 0 1 == 1);;
|};
  [%expect {| |}]
;;

let%expect_test "fun static -> arg" =
  go
    {|
fun f (x : int) : static (static int -> int) = fn (static y : int) -> x + y;;
let _ = (f 0) @ static;;
let _ = assert erased (f 0 1 == 1);;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "lambda static -> arg" =
  go
    {|
let f = fn (x : int) -> fn (static y : int) -> x + y;;
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
let _ = assert erased (f 0 == 0);;
|};
  [%expect {| |}]
;;

let%expect_test "lambda static -> static" =
  go
    {|
let f = fn (static x : int) -> 0;;
let _ = assert erased (f 0 == 0);;
|};
  [%expect {| |}]
;;

let%expect_test "lambda static -> dynamic" =
  go
    {|
let f = fn (static x : int) -> 0 @ dynamic;;
let _ = assert erased (f 0 == 0);;
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
let _ = assert erased (f 0 == 0);;
|};
  [%expect {| |}]
;;

let%expect_test "lambda _ -> dynamic" =
  go
    {|
let f = fn (x : int) -> x @ dynamic;;
let _ = assert erased (f 0 == 0);;
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
let f = fn (x : int) -> fn (y : int) -> x + y;;
let _ = assert erased (f 0 1 == 1);;
|};
  [%expect {| |}]
;;

let%expect_test "partial app -> dynamic" =
  go
    {|
let f = fn (x : int) -> fn (y : int) -> (x + y) @ dynamic;;
let _ = (f 0 @ static);;
|};
  [%expect {| |}]
;;

let%expect_test "nested lambda _ -> arg" =
  go
    {|
let f = fn (x : int) -> fn (y : int) -> (x @ dynamic) + y;;
let _ = assert erased (f 0 1 == 1);;
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
let f = fn (x : int) -> fn (y : int) -> x + (y @ dynamic);;
let _ = assert erased (f 0 1 == 1);;
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
let _ = assert erased (((f @ dynamic) 0) == 0);;
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
let f = fn (x : int) -> fn (y : int) -> x + y;;
let _ = assert erased (f (0 @ dynamic) 1 == 1);;
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
let f = fn (x : int) -> fn (y : int) -> x + y;;
let _ = assert erased (f 0 (1 @ dynamic) == 1);;
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
let _ = assert erased (f 0 == 0);;
|};
  [%expect {| |}]
;;

let%expect_test "lambda _ -> dynamic" =
  go
    {|
let f = fn (x : int) -> 0 @ dynamic;;
let _ = assert erased (f 0 == 0);;
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
let _ = assert erased (f 0 == 0);;
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
let _ = assert erased (f 0 == 0);;
|};
  [%expect {| |}]
;;

let%expect_test "lambda dynamic -> dynamic" =
  go
    {|
let f = fn (dynamic x : int) -> 0 @ dynamic;;
let _ = assert erased (f 0 == 0);;
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
let _ = assert erased (f 0 == 0);;
|};
  [%expect {| |}]
;;

let%expect_test "fun static -> _" =
  go
    {|
fun f (static x : int) : static int = x;;
let _ = assert erased ((f @ dynamic) 0 == 0);;
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
let _ = assert erased (f 0 == 0);;
|};
  [%expect {| |}]
;;

let%expect_test "fun static -> _" =
  go
    {|
fun f (static x : int) : int = x;;
let _ = assert erased (f 0 == 0);;
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
let _ = assert erased (f 0 == 0);;
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
let _ = assert erased (f 0 == 0);;
|};
  [%expect {| |}]
;;

let%expect_test "fun _ -> static" =
  go
    {|
fun f (x : int) : static int = 0;;
let _ = assert erased (f (0 @ dynamic) == 0);;
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
let _ = assert erased (f 0 == 0);;
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
let _ = assert erased (f 0 == 0);;
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
let _ = assert erased (f 0 == 0);;
|};
  [%expect {| |}]
;;

let%expect_test "fun dynamic -> static" =
  go
    {|
fun f (dynamic x : int) : int = 0;;
let _ = assert erased (f 0 == 0);;
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
let _ = assert erased (f 0 == 0);;
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
let _ = assert erased (f 0 == 0);;
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
let _ = assert erased (f 0 == 0);;
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
let _ = assert erased (f (0 @ dynamic) == 0);;
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
let _ = assert erased (f 0 == 0);;
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
let _ = assert erased (f 0 == 0);;
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

let%expect_test "arrow annotation: int -> int loses compile-time callability" =
  go
    {|
let f = (fn (x : int) -> x) : int -> int;;
let _ = assert erased (f 0 == 0);;
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
let _ = assert erased (f 0 == 0);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "phase fn applied to static arg -> static result" =
  go
    {|
let f = fn (x : int) -> x + 1;;
let _ = assert erased (f 5 == 6);;
|};
  [%expect {| |}]
;;

let%expect_test "phase fn applied to dynamic arg -> dynamic result" =
  go
    {|
let f = fn (x : int) -> x + 1;;
let y = 5 @ dynamic;;
let _ = assert erased (f y == 6);;
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
let _ = assert erased (f 5 == 6);;
|};
  [%expect {| |}]
;;

let%expect_test "dynamic fn applied to static arg -> dynamic result" =
  go
    {|
let f = (fn (x : int) -> x + 1) @ dynamic;;
let _ = assert erased (f 5 == 6);;
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
let _ = assert erased (f 0 == 0);;
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
let _ = assert erased (f 0 == 0);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "higher-order: int -> int parameter is not compile-time callable" =
  go
    {|
let apply = fn (f : int -> int) -> fn (x : int) -> f x;;
let _ = assert erased (apply (fn (x : int) -> x + 1) 5 == 6);;
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
let apply = fn (f : int -> int) -> fn (x : int) -> f x;;
let y = 5 @ dynamic;;
let _ = assert erased (apply (fn (x : int) -> x + 1) y == 6);;
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
let apply = fn (f : int -> int) -> fn (x : int) -> f x;;
let g = (fn (x : int) -> x) @ dynamic;;
let _ = assert erased (apply g 0 == 0);;
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
let _ = assert erased (add5 3 == 8);;
|};
  [%expect {| |}]
;;

let%expect_test "higher-order: returned lambda becomes dynamic on dynamic arg" =
  go
    {|
let mk = fn (x : int) -> fn (y : int) -> x + y;;
let add5 = mk 5;;
let y = 3 @ dynamic;;
let _ = assert erased (add5 y == 8);;
|};
  [%expect
    {|
    ((loc ((line 5) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "let binding preserves phase resolution" =
  go
    {|
let f = fn (x : int) -> let y = x + 1 in y;;
let _ = assert erased (f 0 == 1);;
|};
  [%expect {| |}]
;;

let%expect_test "let binding with dynamic forces dynamic" =
  go
    {|
let f = fn (x : int) -> let y = x @ dynamic in y;;
let _ = assert erased (f 0 == 0);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "if with phase cond, phase branches" =
  go
    {|
let f = fn (x : int) -> if x == 0 then 1 else x;;
let _ = assert erased (f 0 == 1);;
|};
  [%expect {| |}]
;;

let%expect_test "if with dynamic cond, phase branches" =
  go
    {|
let f = fn (x : int) ->
  let c = (x == 0) @ dynamic in
  if c then 1 else x;;
let _ = assert erased (f 0 == 1);;
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
let _ = assert erased (f 0 == 1);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "recursive fun with omitted return mode is runtime-only" =
  go
    {|
fun f (x : int) : int = if x == 0 then 0 else f (x - 1);;
let _ = assert erased (f 3 == 0);;
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
let _ = assert erased (f 3 == 0);;
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
let _ = assert erased (f 3 == 1);;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "phase erased lambda" =
  go
    {|
let f = fn (erased x : int) -> 0;;
let _ = assert erased (f 0 == 0);;
|};
  [%expect {| |}]
;;

let%expect_test "static erased arg (pi, dependent)" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = assert erased (id int 5 == 5);;
|};
  [%expect {| |}]
;;

let%expect_test "erased fun with omitted return mode is runtime-only" =
  go
    {|
fun erased f (x : int) : int = x;;
let _ = assert erased (f 0 == 0);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 23)))
     (reason
      (Erased_application
       (fn
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret_ty (Type Int))
          (ret_mode ((staticity Dynamic) (erasure Unerased))))))
       (result ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "arrow type: int -> int parameter returns dynamic results" =
  go
    {|
let f = fn (g : int -> int) -> fn (x : int) -> g x;;
let _ = assert erased (f (fn (x : int) -> x + 1) 5 == 6);;
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
let _ = assert erased (f (fn (static x : int) -> x + 1) == 6);;
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
let _ = assert erased (f (fn (static x : int) -> x + 1) == 6);;
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
let f = fn (g : dynamic int -> int) -> fn (x : int) -> g x;;
let _ = f;;
|};
  [%expect {| |}]
;;

let%expect_test "nested phase application: both args static" =
  go
    {|
let f = fn (x : int) -> fn (y : int) -> x + y;;
let _ = assert erased (f 1 2 == 3);;
|};
  [%expect {| |}]
;;

let%expect_test "nested phase application: first arg dynamic" =
  go
    {|
let f = fn (x : int) -> fn (y : int) -> x + y;;
let a = 1 @ dynamic;;
let _ = assert erased (f a 2 == 3);;
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
let f = fn (x : int) -> fn (y : int) -> x + y;;
let b = 2 @ dynamic;;
let _ = assert erased (f 1 b == 3);;
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
let f = fn (x : int) -> fn (y : int) -> x + y;;
let g = f 5;;
let _ = assert erased (g 3 == 8);;
|};
  [%expect {| |}]
;;

let%expect_test "partial application: phase fn, dynamic arg -> dynamic closure" =
  go
    {|
let f = fn (x : int) -> fn (y : int) -> x + y;;
let a = 5 @ dynamic;;
let g = f a;;
let _ = assert erased (g 3 == 8);;
|};
  [%expect
    {|
    ((loc ((line 5) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "fun with omitted return is runtime-only even when body uses arg" =
  go
    {|
fun f (x : int) : int = x + 1;;
let _ = assert erased (f 5 == 6);;
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
let _ = assert erased (f 5 == 0);;
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
let _ = assert erased (f 5 == 0);;
|};
  [%expect {| |}]
;;

let%expect_test "fun with static arg but omitted return is still runtime-only" =
  go
    {|
fun f (static x : int) : int = x + 1;;
let _ = assert erased (f 5 == 6);;
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
let _ = assert erased (f 5 == 6);;
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
let _ = assert erased (f true == 1);;
|};
  [%expect {| |}]
;;

let%expect_test "assert erased on phase expression -> error" =
  go
    {|
let f = fn (x : int) -> assert erased (x == 0);;
|};
  [%expect
    {|
    ((loc ((line 2) (column 24)))
     (reason
      (Mode_mismatch (got ((staticity Parametric) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "assert erased on static expression inside phase fn -> ok" =
  go
    {|
let f = fn (x : int) -> assert erased (0 == 0);;
let _ = f 5;;
|};
  [%expect {| |}]
;;

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

let%expect_test "phase operands: both phase -> phase result" =
  go
    {|
let f = fn (x : int) -> fn (y : int) -> x + y;;
let _ = assert erased (f 1 2 == 3);;
|};
  [%expect {| |}]
;;

let%expect_test "phase operands: one dynamic -> dynamic result" =
  go
    {|
let f = fn (x : int) -> x + (0 @ dynamic);;
let _ = assert erased (f 1 == 1);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "phase tuple elements" =
  go
    {|
let f = fn (x : int) -> fn (y : int) -> (x, y);;
let _ = f 1 2;;
|};
  [%expect {| |}]
;;

let%expect_test "compose two phase functions -> static when all static" =
  go
    {|
let f = fn (x : int) -> x + 1;;
let g = fn (x : int) -> x * 2;;
let _ = assert erased (g (f 3) == 8);;
|};
  [%expect {| |}]
;;

let%expect_test "compose: inner dynamic -> outer dynamic" =
  go
    {|
let f = fn (x : int) -> (x + 1) @ dynamic;;
let g = fn (x : int) -> x * 2;;
let _ = assert erased (g (f 3) == 8);;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "lambda is compile-time callable by default; fun needs static return" =
  go
    {|
let f1 = fn (x : int) -> x + 1;;
fun f2 (x : int) : int = x + 1;;
let _ = assert erased (f1 5 == f2 5);;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "triple nested phase: all static" =
  go
    {|
let f = fn (x : int) -> fn (y : int) -> fn (z : int) -> x + y + z;;
let _ = assert erased (f 1 2 3 == 6);;
|};
  [%expect {| |}]
;;

let%expect_test "triple nested phase: middle arg dynamic" =
  go
    {|
let f = fn (x : int) -> fn (y : int) -> fn (z : int) -> x + y + z;;
let b = 2 @ dynamic;;
let _ = assert erased (f 1 b 3 == 6);;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

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

let%expect_test "static fn applied in static assert" =
  go
    {|
let f = fn (x : int) -> x + 1;;
let _ = assert erased (f 5 == 6);;
|};
  [%expect {| |}]
;;

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

let%expect_test "unreachable in phase context" =
  go
    {|
let f = fn (x : bool) -> if x then 1 else unreachable;;
let _ = assert erased (f true == 1);;
|};
  [%expect {| ((loc ((line 2) (column 42))) (reason Unreachable_reached)) |}]
;;

let%expect_test "unreachable in static if -> ok" =
  go
    {|
let f = fn (static x : bool) -> if static x then 1 else unreachable;;
let _ = assert erased (f true == 1);;
|};
  [%expect {| |}]
;;

let%expect_test "pi return static parametric" =
  go
    {|
 let g = fn (x : int) ->
    fun f (erased u : type) : static int = x in
    f int
  ;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 4)))
     (reason
      (Mode_mismatch (got ((staticity Parametric) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "pi return static parametric" =
  go
    {|
let _ = fn (b : bool) -> if b then (fn (static erased t : type) -> ()) else (fn (static erased t : type) -> ());;
|};
  [%expect {| |}]
;;

(* Quoting demands the captured static, so a failing computation that was
   never demanded during checking reports at its own location when its value
   is quoted into a specialization key. *)
let%expect_test "quoting forces a captured static and reports its failure" =
  go
    {|
let c = (1 / 0) @ static;;
let f = fn (x : int) -> x + c;;
let use = fn (static h : int -> int) -> h 1;;
let _ = use f;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 11)))
     (reason (Static_failure (Divide_by_zero (Div (Int (T 1)) (Int (T 0)))))))
    |}]
;;

(* Application modes come from the signature: the Pi's declared return mode
   (here inferred dynamic from the generic body's join) governs every
   application, even when the body at this particular key would be static.
   Modes never depend on per-key evaluation. *)
let%expect_test "reject: application mode is the declared one, static branch" =
  go
    {|
let f = fn (static b : bool) -> if static b then 0 else (0 @ dynamic);;
let _ = (f true) @ static;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 17)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "reject: application mode is the declared one, dynamic branch" =
  go
    {|
let f = fn (static b : bool) -> if static b then 0 else (0 @ dynamic);;
let _ = (f false) @ static;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 18)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "instantiation errors are local to the application" =
  go
    {|
fun pick (static b : bool) : int = if static b then 0 else unreachable;;
let _ = pick false;;
let _ = (1 + 2) : bool;;
|};
  [%expect {| ((loc ((line 2) (column 59))) (reason Unreachable_reached)) |}]
;;

let%expect_test "instantiation error reports at the application" =
  go
    {|
fun pick (static b : bool) : int = if static b then 0 else unreachable;;
let _ = pick false;;
|};
  [%expect {| ((loc ((line 2) (column 59))) (reason Unreachable_reached)) |}]
;;
