open! Core
open! Syl

let go ?(print = false) input =
  match Parse.parse input with
  | Ok cst -> if print then print_s [%message (cst : Cst.Program.t)]
  | Error { loc; reason } -> print_s [%message (loc : Lex.Location.t) (reason : Parse.Error.t)]
;;

let%expect_test "arrow" =
  go "let _ = t -> static t -> t;;";
  [%expect {| |}];
  go "let _ = t -> (static t -> t);;";
  [%expect {| |}];
  go "let _ = t -> static (t -> t);;";
  [%expect {| |}];
  go "let _ = t -> static (static t -> t);;";
  [%expect {| |}];
  go "let _ = t -> static static t -> t;;";
  [%expect {| ((loc ((line 1) (column 13))) (reason (Duplicate_mode Staticity))) |}];
  go "let _ = t -> static t;;";
  [%expect {| |}]
;;

let%expect_test "arrow" =
  go "let _ = (int : type) \\ name -> static (erased type -> static name);;";
  [%expect {| |}]
;;

let%expect_test "funs" =
  go "let _ = fun x (_ : unit) : unit = () in x;;";
  [%expect {| |}]
;;

let%expect_test "bad modes" =
  go "fun x (_ : unit @ ) : unit = ();;";
  [%expect {| ((loc ((line 1) (column 18))) (reason (Unexpected Rparen))) |}];
  go "fun x (_ : unit @ static static) : unit = ();;";
  [%expect {| ((loc ((line 1) (column 18))) (reason (Duplicate_mode Staticity))) |}];
  go "fun x (_ : unit @ erased static erased) : unit = ();;";
  [%expect {| ((loc ((line 1) (column 18))) (reason (Duplicate_mode Erasure))) |}]
;;

let%expect_test "arrow" =
  go "let _ = 1 + 2 -> 3 @ static;;";
  [%expect {| |}];
  go "let x = x : type -> x * x;;";
  [%expect {| |}];
  go "let x = (x : type) -> x * x;;";
  [%expect {| |}];
  go "let _ = x -> y;;";
  [%expect {| |}];
  go "let _ = erased static x -> y;;";
  [%expect {| |}];
  go "let _ = static x -> erased y;;";
  [%expect {| |}];
  go "let _ = static x -> (erased y -> z);;";
  [%expect {| |}];
  go "let _ = (static x -> erased y) -> z;;";
  [%expect {| |}];
  go "let _ = static x -> (y -> z);;";
  [%expect {| |}];
  go "let _ = static x -> erased y -> z;;";
  [%expect {| |}]
;;

let%expect_test "static" =
  go "let _ = fn (static x : int @ static) -> ();;";
  [%expect {| |}];
  go "let _ = fn (x : (int)) -> ();;";
  [%expect {| |}];
  go "let _ = fn (x : int @ static static) -> ();;";
  [%expect {| ((loc ((line 1) (column 22))) (reason (Duplicate_mode Staticity))) |}];
  go "let _ = fn (x : int @ (static)) -> ();;";
  [%expect {| ((loc ((line 1) (column 22))) (reason (Unexpected Lparen))) |}];
  go "let _ = fn (x : (int) @ static) -> ();;";
  [%expect {| |}];
  go "let _ = fn (x : (int @ static) @ static) -> ();;";
  [%expect {| |}];
  go "let _ = fn (x : (int @ static)) -> ();;";
  [%expect {| |}];
  go "let _ = fn (x : int @ static -> int @ static) -> ();;";
  [%expect {| ((loc ((line 1) (column 29))) (reason (Unexpected (Op Arrow)))) |}];
  go "let _ = fn (x : (int -> int) @ static ) -> ();;";
  [%expect {| |}];
  go "let _ = fn (x : (int @ static -> int @ static) @ static) -> ();;";
  [%expect {| ((loc ((line 1) (column 30))) (reason (Unexpected (Op Arrow)))) |}]
;;

let%expect_test _ =
  go "let _ = 2 - 1;;";
  [%expect {| |}]
;;

let%expect_test _ =
  go "let _ = a b c d;;";
  [%expect {| |}]
;;

let%expect_test _ =
  go "let _ = x == y == z;;";
  [%expect {| |}]
;;

let%expect_test _ =
  go "let _ = a a + b - c * d || e && f g == x;;";
  [%expect {| |}]
;;

let%expect_test _ =
  go "let _ = let x = false in x;;";
  [%expect {| |}]
;;

let%expect_test _ =
  go "let _ = fn (x : bool -> (bool -> bool)) -> true false;;";
  [%expect {| |}]
;;

let%expect_test _ =
  go "let _ = (true);;";
  [%expect {| |}]
;;

let%expect_test _ =
  go "let _ = if true then false else true;;";
  [%expect {| |}]
;;

let%expect_test _ =
  go "let _ = true false;;";
  [%expect {| |}]
;;

let%expect_test _ =
  go "let _ = (fn (x : bool) -> (fn (y : bool) -> x) false) true;;";
  [%expect {| |}]
;;

let%expect_test _ =
  go "let _ = (fn (x : bool) -> fn (y : bool) -> x) false true;;";
  [%expect {| |}]
;;

let%expect_test _ =
  go "let _ = (let x = fn (y : bool) -> fn (z : bool) -> y in x true) false;;";
  [%expect {| |}]
;;

let%expect_test _ =
  go "let _ = let a = (let x = fn (y : bool) -> fn (z : bool) -> y in x true) in a false;;";
  [%expect {| |}]
;;

let%expect_test _ =
  go "let _ = let x = fn (y : bool) -> y in x true;;";
  [%expect {| |}]
;;

let%expect_test _ =
  go "let _ = let x = fn (y : lol) -> y in x true;;";
  [%expect {| |}]
;;

let%expect_test _ =
  go "let _ = let x ? fn (y : lol) -> y in x true;;";
  [%expect {| ((loc ((line 1) (column 14))) (reason (Unexpected (Unknown ?)))) |}]
;;

let%expect_test _ =
  go
    {|
let a =
    let second = fn (y : bool) -> y in
    second (second (true))
;;|};
  [%expect {| |}]
;;

let%expect_test _ =
  go
    {|
let a = (
    let second = fn (y : bool) -> y in
    second (second (true))
);;|};
  [%expect {| |}]
;;

let%expect_test _ =
  go
    {|
fun first (x : bool -> bool) : bool = if x then first false else false;;
let _ = first true;;
|};
  [%expect {| |}]
;;

let%expect_test _ =
  go
    {|
fun first (x : bool) : bool -> (bool -> bool) = x;;
let _ = first;;
|};
  [%expect {| |}]
;;

let%expect_test _ =
  go
    {|
fun first (x : bool) : (bool -> bool) -> bool = x;;
let _ = first;;
|};
  [%expect {| |}]
;;

let%expect_test _ =
  go
    {|
fun first (x : bool) : bool -> bool -> bool = x;;
let _ = first;;
|};
  [%expect {| |}]
;;

let%expect_test _ =
  go
    {|
  let _ =
    fun aux (x : int) : int = (
      x
    ) in
    aux 1
  ;;
|};
  [%expect {| |}]
;;

let%expect_test _ =
  go
    {|
  let _ =
    let capture = 10 in
    fun aux (x : int) : int = (
      if x > 0
      then aux (x - 1)
      else capture
    ) in
    print_int (aux 5)
  ;;
|};
  [%expect {| |}]
;;

let%expect_test "mutual" =
  go
    {|
fun f (x : int) : int = g x
and g (x : int) : int = f x
;;
let _ = print_int (f 0);;
  |};
  [%expect {| |}]
;;

let%expect_test "mutual2" =
  go
    {|
fun f (x : int) : (unit -> int) =
  if x == 0 then (fn (_ : unit) -> 0)
  else (fn (_ : unit) -> 1 + g (x - 1) ())
and g (x : int) : (unit -> int) =
  if x == 0 then (fn (_ : unit) -> 0)
  else (fn (_ : unit) -> 1 + f (x - 1) ())
;;
let _ = print_int (f 10 ());;
  |};
  [%expect {| |}]
;;

let%expect_test "mutual inner" =
  go
    {|
let _ =
  fun f (x : int) : int = g x
  and g (x : int) : int = f x
  in print_int (f 0)
;;
  |};
  [%expect {| |}]
;;

let%expect_test "mutual2 inner" =
  go
    {|
let _ =
  fun f (x : int) : (unit -> int) =
    if x == 0 then (fn (_ : unit) -> 0)
    else (fn (_ : unit) -> 1 + g (x - 1) ())
  and g (x : int) : (unit -> int) =
    if x == 0 then (fn (_ : unit) -> 0)
    else (fn (_ : unit) -> 1 + f (x - 1) ())
  in print_int (f 10 ())
;;
  |};
  [%expect {| |}]
;;

let%expect_test "comment" =
  go
    {|
let _ = (* hello *) 0;;
  |};
  [%expect {| |}]
;;

let%expect_test "comment" =
  go
    {|
(* hello *)
  |};
  [%expect {| |}]
;;

let%expect_test "comment" =
  go
    {|
(* (* *)
  |};
  [%expect {| |}]
;;

let%expect_test "comment" =
  go
    {|
(* (* *) *)
  |};
  [%expect {| ((loc ((line 2) (column 9))) (reason (Unexpected (Op Star)))) |}]
;;

let%expect_test "comment" =
  go
    {|
(* *) fun (* *) erased (* *) f (* *) ((* *) static (* *) erased (* *) x (* *) : (* *) int (* *)) (* *) : (* *) static (* *) erased (* *) int (* *) = (* *) x (* *);;(* *)
  |};
  [%expect {| |}]
;;

let%expect_test "assert" =
  go
    {|
let _ = assert true;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert" =
  go
    {|
let _ = assert x && y;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert" =
  go
    {|
let _ = assert (x && y);;
  |};
  [%expect {| |}]
;;

let%expect_test "assert" =
  go
    {|
let _ = assert !x;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert" =
  go
    {|
let _ = assert assert x;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert" =
  go
    {|
let _ = assert (assert x);;
  |};
  [%expect {| |}]
;;

let%expect_test "assert" =
  go
    {|
let _ = assert f x;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert" =
  go
    {|
let _ = assert (f x);;
  |};
  [%expect {| |}]
;;

let%expect_test "assert" =
  go
    {|
let _ = assert static x;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert" =
  go
    {|
let _ = assert (static x);;
  |};
  [%expect
    {|
    ((loc ((line 2) (column 24)))
     (reason (Unexpected_modes ((staticity (Static)) (erasure ())))))
    |}]
;;

let%expect_test "assert" =
  go
    {|
let _ = assert static x @ static;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert" =
  go
    {|
let _ = assert static (x @ static);;
  |};
  [%expect {| |}]
;;
