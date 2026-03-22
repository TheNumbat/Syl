open! Core
open! Syl

let go ?(print = false) input =
  match Parse.parse input with
  | Ok cst ->
    print_endline (sprintf "%a" Cst.Program.print cst);
    if print then print_s [%message (cst : Cst.Program.t)]
  | Error { loc; reason } -> print_s [%message (loc : Lex.Location.t) (reason : Parse.Error.t)]
;;

let%expect_test "arrow" =
  go "let _ = t -> static t -> t;;";
  [%expect {| let _ = t -> statict -> t;; |}];
  go "let _ = t -> (static t -> t);;";
  [%expect {| let _ = t -> (statict -> t);; |}];
  go "let _ = t -> static (t -> t);;";
  [%expect {| let _ = t -> static(t -> t);; |}];
  go "let _ = t -> static (static t -> t);;";
  [%expect {| let _ = t -> static(statict -> t);; |}];
  go "let _ = t -> static static t -> t;;";
  [%expect {| ((loc ((line 1) (column 13))) (reason (Duplicate_mode Staticity))) |}];
  go "let _ = t -> static t;;";
  [%expect {| let _ = t -> statict;; |}]
;;

let%expect_test "arrow" =
  go "let _ = (int : type) \\ name -> static (erased type -> static name);;";
  [%expect {| let _ = (int : type) \ name -> static(erasedtype -> staticname);; |}]
;;

let%expect_test "funs" =
  go "let _ = fun x (_ : unit) : unit = () in x;;";
  [%expect {| let _ = fun x ( _ : unit) : unit = (); x;; |}]
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
  [%expect {| let _ = 1 + 2 -> 3 @ static;; |}];
  go "let x = x : type -> x * x;;";
  [%expect {| let x = x : type -> x * x;; |}];
  go "let x = (x : type) -> x * x;;";
  [%expect {| let x = (x : type) -> x * x;; |}];
  go "let _ = x -> y;;";
  [%expect {| let _ = x -> y;; |}];
  go "let _ = erased static x -> y;;";
  [%expect {| let _ = static erasedx -> y;; |}];
  go "let _ = static x -> erased y;;";
  [%expect {| let _ = staticx -> erasedy;; |}];
  go "let _ = static x -> (erased y -> z);;";
  [%expect {| let _ = staticx -> (erasedy -> z);; |}];
  go "let _ = (static x -> erased y) -> z;;";
  [%expect {| let _ = (staticx -> erasedy) -> z;; |}];
  go "let _ = static x -> (y -> z);;";
  [%expect {| let _ = staticx -> (y -> z);; |}];
  go "let _ = static x -> erased y -> z;;";
  [%expect {| let _ = staticx -> erasedy -> z;; |}]
;;

let%expect_test "static" =
  go "let _ = fn (static x : int @ static) -> ();;";
  [%expect {| let _ = fn (staticx : int @ static) -> ();; |}];
  go "let _ = fn (x : (int)) -> ();;";
  [%expect {| let _ = fn (x : (int)) -> ();; |}];
  go "let _ = fn (x : int @ static static) -> ();;";
  [%expect {| ((loc ((line 1) (column 22))) (reason (Duplicate_mode Staticity))) |}];
  go "let _ = fn (x : int @ (static)) -> ();;";
  [%expect {| ((loc ((line 1) (column 22))) (reason (Unexpected Lparen))) |}];
  go "let _ = fn (x : (int) @ static) -> ();;";
  [%expect {| let _ = fn (x : (int) @ static) -> ();; |}];
  go "let _ = fn (x : (int @ static) @ static) -> ();;";
  [%expect {| let _ = fn (x : (int @ static) @ static) -> ();; |}];
  go "let _ = fn (x : (int @ static)) -> ();;";
  [%expect {| let _ = fn (x : (int @ static)) -> ();; |}];
  go "let _ = fn (x : int @ static -> int @ static) -> ();;";
  [%expect {| ((loc ((line 1) (column 29))) (reason (Unexpected (Op Arrow)))) |}];
  go "let _ = fn (x : (int -> int) @ static ) -> ();;";
  [%expect {| let _ = fn (x : (int -> int) @ static) -> ();; |}];
  go "let _ = fn (x : (int @ static -> int @ static) @ static) -> ();;";
  [%expect {| ((loc ((line 1) (column 30))) (reason (Unexpected (Op Arrow)))) |}]
;;

let%expect_test _ =
  go "let _ = 2 - 1;;";
  [%expect {| let _ = 2 - 1;; |}]
;;

let%expect_test _ =
  go "let _ = a b c d;;";
  [%expect {| let _ = a b c d;; |}]
;;

let%expect_test _ =
  go "let _ = x == y == z;;";
  [%expect {| let _ = x == y == z;; |}]
;;

let%expect_test _ =
  go "let _ = a a + b - c * d || e && f g == x;;";
  [%expect {| let _ = a a + b - c * d || e && f g == x;; |}]
;;

let%expect_test _ =
  go "let _ = let x = false in x;;";
  [%expect {| let _ = let x = false in x;; |}]
;;

let%expect_test _ =
  go "let _ = fn (x : bool -> (bool -> bool)) -> true false;;";
  [%expect {| let _ = fn (x : bool -> (bool -> bool)) -> true false;; |}]
;;

let%expect_test _ =
  go "let _ = (true);;";
  [%expect {| let _ = (true);; |}]
;;

let%expect_test _ =
  go "let _ = if true then false else true;;";
  [%expect {| let _ = if true then false else true;; |}]
;;

let%expect_test _ =
  go "let _ = true false;;";
  [%expect {| let _ = true false;; |}]
;;

let%expect_test _ =
  go "let _ = (fn (x : bool) -> (fn (y : bool) -> x) false) true;;";
  [%expect {| let _ = (fn (x : bool) -> (fn (y : bool) -> x) false) true;; |}]
;;

let%expect_test _ =
  go "let _ = (fn (x : bool) -> fn (y : bool) -> x) false true;;";
  [%expect {| let _ = (fn (x : bool) -> fn (y : bool) -> x) false true;; |}]
;;

let%expect_test _ =
  go "let _ = (let x = fn (y : bool) -> fn (z : bool) -> y in x true) false;;";
  [%expect {| let _ = (let x = fn (y : bool) -> fn (z : bool) -> y in x true) false;; |}]
;;

let%expect_test _ =
  go "let _ = let a = (let x = fn (y : bool) -> fn (z : bool) -> y in x true) in a false;;";
  [%expect
    {| let _ = let a = (let x = fn (y : bool) -> fn (z : bool) -> y in x true) in a false;; |}]
;;

let%expect_test _ =
  go "let _ = let x = fn (y : bool) -> y in x true;;";
  [%expect {| let _ = let x = fn (y : bool) -> y in x true;; |}]
;;

let%expect_test _ =
  go "let _ = let x = fn (y : lol) -> y in x true;;";
  [%expect {| let _ = let x = fn (y : lol) -> y in x true;; |}]
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
  [%expect {| let a = let second = fn (y : bool) -> y in second (second (true));; |}]
;;

let%expect_test _ =
  go
    {|
let a = (
    let second = fn (y : bool) -> y in
    second (second (true))
);;|};
  [%expect {| let a = (let second = fn (y : bool) -> y in second (second (true)));; |}]
;;

let%expect_test _ =
  go
    {|
fun first (x : bool -> bool) : bool = if x then first false else false;;
let _ = first true;;
|};
  [%expect
    {|
    fun first ( x : bool -> bool) : bool = if x then first false else false;;
    let _ = first true;;
    |}]
;;

let%expect_test _ =
  go
    {|
fun first (x : bool) : bool -> (bool -> bool) = x;;
let _ = first;;
|};
  [%expect
    {|
    fun first ( x : bool) : bool -> (bool -> bool) = x;;
    let _ = first;;
    |}]
;;

let%expect_test _ =
  go
    {|
fun first (x : bool) : (bool -> bool) -> bool = x;;
let _ = first;;
|};
  [%expect
    {|
    fun first ( x : bool) : (bool -> bool) -> bool = x;;
    let _ = first;;
    |}]
;;

let%expect_test _ =
  go
    {|
fun first (x : bool) : bool -> bool -> bool = x;;
let _ = first;;
|};
  [%expect
    {|
    fun first ( x : bool) : bool -> bool -> bool = x;;
    let _ = first;;
    |}]
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
  [%expect {| let _ = fun aux ( x : int) : int = (x); aux 1;; |}]
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
  [%expect
    {| let _ = let capture = 10 in fun aux ( x : int) : int = (if x > 0 then aux (x - 1) else capture); print_int (aux 5);; |}]
;;

let%expect_test "mutual" =
  go
    {|
fun f (x : int) : int = g x
and g (x : int) : int = f x
;;
let _ = print_int (f 0);;
  |};
  [%expect
    {|
    fun f ( x : int) : int = g x
    and g ( x : int) : int = f x;;
    let _ = print_int (f 0);;
    |}]
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
  [%expect
    {|
    fun f ( x : int) : (unit -> int) = if x == 0 then (fn (_ : unit) -> 0) else (fn (_ : unit) -> 1 + g (x - 1) ())
    and g ( x : int) : (unit -> int) = if x == 0 then (fn (_ : unit) -> 0) else (fn (_ : unit) -> 1 + f (x - 1) ());;
    let _ = print_int (f 10 ());;
    |}]
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
  [%expect
    {| let _ = fun f ( x : int) : int = g x and g ( x : int) : int = f x; print_int (f 0);; |}]
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
  [%expect
    {| let _ = fun f ( x : int) : (unit -> int) = if x == 0 then (fn (_ : unit) -> 0) else (fn (_ : unit) -> 1 + g (x - 1) ()) and g ( x : int) : (unit -> int) = if x == 0 then (fn (_ : unit) -> 0) else (fn (_ : unit) -> 1 + f (x - 1) ()); print_int (f 10 ());; |}]
;;

let%expect_test "comment" =
  go
    {|
let _ = (* hello *) 0;;
  |};
  [%expect {| let _ = 0;; |}]
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
  [%expect {| fun f (static erased x : int) : static erasedint = x;; |}]
;;

let%expect_test "assert" =
  go
    {|
let _ = assert true;;
  |};
  [%expect {| let _ = assert true;; |}]
;;

let%expect_test "assert" =
  go
    {|
let _ = assert x && y;;
  |};
  [%expect {| let _ = assert x && y;; |}]
;;

let%expect_test "assert" =
  go
    {|
let _ = assert (x && y);;
  |};
  [%expect {| let _ = assert (x && y);; |}]
;;

let%expect_test "assert" =
  go
    {|
let _ = assert !x;;
  |};
  [%expect {| let _ = assert !x;; |}]
;;

let%expect_test "assert" =
  go
    {|
let _ = assert assert x;;
  |};
  [%expect {| let _ = assert assert x;; |}]
;;

let%expect_test "assert" =
  go
    {|
let _ = assert (assert x);;
  |};
  [%expect {| let _ = assert (assert x);; |}]
;;

let%expect_test "assert" =
  go
    {|
let _ = assert f x;;
  |};
  [%expect {| let _ = assert f x;; |}]
;;

let%expect_test "assert" =
  go
    {|
let _ = assert (f x);;
  |};
  [%expect {| let _ = assert (f x);; |}]
;;

let%expect_test "assert" =
  go
    {|
let _ = assert static x;;
  |};
  [%expect {| let _ = assert static x;; |}]
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
  [%expect {| let _ = assert static x @ static;; |}]
;;

let%expect_test "assert" =
  go
    {|
let _ = assert static (x @ static);;
  |};
  [%expect {| let _ = assert static (x @ static);; |}]
;;

let%expect_test "2 tuple" =
  go
    {|
let t = int ^ int;;
|};
  [%expect {| let t = int ^ int;; |}]
;;

let%expect_test "2 tuple" =
  go
    {|
let t = int ^ (int);;
|};
  [%expect {| let t = int ^ (int);; |}]
;;

let%expect_test "2, 1 tuple" =
  go
    {|
let t = (int ^ bool) ^ unit;;
|};
  [%expect {| let t = (int ^ bool) ^ unit;; |}]
;;

let%expect_test "1, 2 tuple" =
  go
    {|
let t = int ^ (bool ^ unit);;
|};
  [%expect {| let t = int ^ (bool ^ unit);; |}]
;;

let%expect_test "3 caret" =
  go "let t = int ^ bool ^ unit;;";
  [%expect {| let t = int ^ bool ^ unit;; |}]
;;

let%expect_test "comma" =
  go "let t = 1, 2;;";
  [%expect {| let t = 1, 2;; |}]
;;

let%expect_test "3 comma" =
  go "let t = 1, 2, 3;;";
  [%expect {| let t = 1, 2, 3;; |}]
;;

let%expect_test "comma and caret precedence" =
  go "let t = a, b ^ c, d;;";
  [%expect {| let t = a, b ^ c, d;; |}]
;;

let%expect_test "caret in arrow" =
  go "let t = int ^ bool -> unit;;";
  [%expect {| let t = int ^ bool -> unit;; |}]
;;

let%expect_test "comma in arrow" =
  go "let t = 1, 2 -> 3;;";
  [%expect {| let t = 1, 2 -> 3;; |}]
;;

let%expect_test "comma and caret parenthesized" =
  go "let t = (a, b) ^ (c, d);;";
  [%expect {| let t = (a, b) ^ (c, d);; |}]
;;

let%expect_test "caret in comma" =
  go "let t = a ^ b, c ^ d;;";
  [%expect {| let t = a ^ b, c ^ d;; |}]
;;

let%expect_test "let in tuple" =
  go "let _ = (let first = x in 1, let second = x in 2);;";
  [%expect {| let _ = (let first = x in 1, let second = x in 2);; |}]
;;

let%expect_test "let in tuple" =
  go "let _ = (let first = x in 1), (let second = x in 2);;";
  [%expect {| let _ = (let first = x in 1), (let second = x in 2);; |}]
;;
