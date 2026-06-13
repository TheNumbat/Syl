open! Core
open! Syl

let go ?print ?(check = `Run) input = Common.codegen ?print ~check input

let%expect_test "Fun recursive dynamic arg" =
  go
    {|
fun f (x : int) : int = f x;;
|};
  [%expect {| |}]
;;

let%expect_test "fun dynamic recursion is allowed" =
  go
    {|
fun f (x : int) : int = f x;;
|};
  [%expect {| |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = g x
and g (y : int) : int = let _ = if y < 0 then f y else 0 in 0;;
let _ = g 0;;
|};
  [%expect {| |}]
;;

let%expect_test "fun recurse" =
  go
    {|
let a = 0 @ dynamic;;
fun f (x : int) : int = let _ = a in f x;;
|};
  [%expect {| |}]
;;

let%expect_test "fun recurse" =
  go
    {|
let a = 0 @ dynamic;;
let _ =
fun f (x : int) : int = let _ = a in f x in
()
;;
|};
  [%expect {| |}]
;;

let%expect_test "recursive env" =
  go
    {|
let a = 0 @ dynamic;;
let b = 1 @ dynamic;;
let c = 2 @ dynamic;;
fun f (x : int) : int = let _ = a in let _ = b in g x
and g (y : int) : int = let _ = a in let _ = c in f y;;
|};
  [%expect {| |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = let _ = if x < 0 then g x else 0 in 0
and g (y : int) : int = f y;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = g x
and g (y : int) : int = if y == 0 then 0 else f (y-1);;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "mutual pi recursion" =
  go
    {|
fun f (static erased t : type) : t -> t = fn (x : t) -> x
and g (static erased t : type) : t -> t = f t;;
let _ = g int 0;;
|};
  [%expect {| |}]
;;

let%expect_test "static recursion with base case" =
  go
    {|
fun f (static x : int) : int = if erased x == 0 then x else f (x - 1);;
let _ = print_int (f 3);;
|};
  [%expect {| 0 |}]
;;

let%expect_test "static recursion with base case" =
  go
    {|
fun f (static x : int) : int = if erased x == 10 then x else f (x + 1);;
let _ = print_int (f 3);;
|};
  [%expect {| 10 |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (static x : int) : erased int = (if erased x == 0 then 42 else f (x - 1)) @ erased;;
let _ = f 3;;
|};
  [%expect {| |}]
;;

let%expect_test "arrow and pi mutual recursion with application" =
  go
    {|
fun double (x : int) : int = x + x
and apply_double (static erased t : type) : int -> int = fn (x : int) -> double x;;
let _ = apply_double int 5;;
|};
  [%expect {| |}]
;;

let%expect_test "mutually recursive fun with static arg" =
  go
    {|
fun id1 (static erased t : type) : t -> t = fn (x : t) -> x
and id2 (static erased t : type) : t -> t = id1 t;;
|};
  [%expect {| |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = g x
and g (y : int) : int = y;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = g x
and g (y : int) : int = y;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (static x : int) : erased int = g x
and g (static y : int) : erased int = y;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = g x
and g (y : int) : int = y;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = if x == 0 then 0 else g (x - 1)
and g (y : int) : int = if y == 0 then 0 else f (y - 1);;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = if x == 0 then 0 else g (x - 1)
and g (y : int) : int = if y == 0 then 0 else f (y - 1);;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = if x == 0 then 0 else g (x - 1)
and g (y : int) : int = if y == 0 then 0 else f (y - 1);;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "top-level mutual recursion with different bodies" =
  go
    {|
fun f (a : int) : int = g (a + 1)
and g (b : int) : int = b;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "top-level mutual static recursion" =
  go
    {|
fun f (static x : int) : int = if erased x == 0 then 0 else g (x - 1)
and g (static y : int) : int = if erased y == 0 then 1 else f (y - 1);;
let _ = f 2;;
|};
  [%expect {| |}]
;;

let%expect_test "static mutual recursion cross-monomorphization" =
  go
    {|
fun f (static erased t : type) : t -> t = g t
and g (static erased t : type) : t -> t = fn (x : t) -> x;;
let _ = f int 0;;
|};
  [%expect {| |}]
;;

let%expect_test "static mutual recursion cross-monomorphization" =
  go
    {|
fun f (static erased t : type) : t -> t = fn (x : t) -> x
and g (static erased t : type) : t -> t = f t;;
let _ = g int 0;;
|};
  [%expect {| |}]
;;

let%expect_test "mutually recursive local closures share environment" =
  go
    {|
fun outer (x : int) : int =
  fun f (a : int) : int = if a == 0 then 0 else g (a + x)
  and g (b : int) : int = if b == 0 then 0 else f (b + x) in
  f 0;;
let _ = outer 5;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: simple recursion" =
  go
    {|
fun count (x : int) : int =
  if x == 0 then 0 else count (x - 1);;
let _ = count 5;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: mutual recursion" =
  go
    {|
fun even (x : int) : bool =
  if x == 0 then true else odd (x - 1)
and odd (x : int) : bool =
  if x == 0 then false else even (x - 1);;
let _ = even 4;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: mutual recursion accumulating values" =
  go
    {|
fun f (x : int) : int =
  if x <= 0 then 0 else g (x - 1) + 1
and g (x : int) : int =
  if x <= 0 then 100 else f (x - 1) + 2;;
let _ = f 5;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: static recursion fibonacci" =
  go
    {|
fun fib (static x : int) : static int =
  if erased x <= 1 then x else fib (x - 1) + fib (x - 2);;
let _ = fib 10;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: recursion capturing outer variable" =
  go
    {|
let offset = 10 @ dynamic;;
fun add_offset (x : int) : dynamic int =
  if x == 0 then offset else add_offset (x - 1) + 1;;
let _ = add_offset 3;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: recursive function used as value" =
  go
    {|
fun id (x : int) : int = x;;
let f = id;;
let _ = f 42;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: recursive function with closure" =
  go
    {|
let add = fn (x : int) -> fn (y : int) -> x + y;;
fun loop (n : int) : int =
  if n <= 0 then 0 else add n (loop (n - 1));;
let _ = loop 5;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: mutual recursion with closures" =
  go
    {|
let inc = fn (x : int) -> x + 1;;
fun f (x : int) : int =
  if x == 0 then 0 else inc (g (x - 1))
and g (x : int) : int =
  if x == 0 then 0 else inc (f (x - 1));;
let _ = f 4;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: static recursion with dependent type" =
  go
    {|
fun choose (static b : bool) : static erased type =
  if erased b then int else bool;;
let _ = (42 : choose true);;
let _ = (false : choose false);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: recursive function returning unit" =
  go
    {|
fun loop (n : int) : unit =
  if n <= 0 then () else loop (n - 1);;
let _ = loop 3;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: mutual recursion with unit returns" =
  go
    {|
fun f (n : int) : unit =
  if n <= 0 then () else g (n - 1)
and g (n : int) : unit =
  if n <= 0 then () else f (n - 1);;
let _ = f 4;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: static factorial" =
  go
    {|
fun f (static x : int) : static int =
  if erased x == 0 then 1 else x * f (x - 1);;
let _ = f 5;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: static recursion with boolean" =
  go
    {|
fun f (static b : bool) : static int =
  if erased b then 1 else 0;;
let _ = f true;;
let _ = f false;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: static mutual recursion" =
  go
    {|
fun f (static x : int) : static int =
  if erased x <= 0 then 0 else g (x - 1) + 1
and g (static x : int) : static int =
  if erased x <= 0 then 0 else f (x - 1) + 2;;
let _ = f 4;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: recursive capturing multiple dynamic vars" =
  go
    {|
let a = 1 @ dynamic;;
let b = 2 @ dynamic;;
fun f (x : int) : dynamic int =
  if x <= 0 then a + b else f (x - 1) + 1;;
let _ = f 3;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: mutual rec different captured scopes" =
  go
    {|
let a = 10 @ dynamic;;
let b = 20 @ dynamic;;
fun f (x : int) : dynamic int =
  if x <= 0 then a else g (x - 1)
and g (x : int) : dynamic int =
  if x <= 0 then b else f (x - 1);;
let _ = f 3;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: polymorphic higher-order with recursion" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
fun loop (n : int) : int =
  if n <= 0 then 0 else id int n + loop (n - 1);;
let _ = loop 5;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: passing recursive function as argument" =
  go
    {|
fun double (x : int) : int =
  if x <= 0 then 0 else double (x - 1) + 2;;
let apply = fn (f : int -> int) -> f 5;;
let _ = apply double;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: recursive function returning closure" =
  go
    {|
fun make (n : int) : int -> int =
  if n <= 0 then fn (x : int) -> x
  else fn (x : int) -> x + n;;
let f = make 5;;
let _ = f 10;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: mutual recursion returning closures" =
  go
    {|
fun f (x : int) : int -> int =
  if x <= 0 then fn (y : int) -> y
  else g (x - 1)
and g (x : int) : int -> int =
  if x <= 0 then fn (y : int) -> y + 1
  else f (x - 1);;
let h = f 3;;
let _ = h 10;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: recursive with mixed mode params" =
  go
    {|
fun f (static n : int) : static (int -> int) =
  fn (x : int) -> x + n;;
let add3 = f 3;;
let add7 = f 7;;
let _ = add3 10;;
let _ = add7 10;;
|};
  [%expect {| |}]
;;

let%expect_test "runtime recursion over a static via an inner fun" =
  go
    {|
fun count (static lo : int) : static (int -> int) =
  fun go (x : int) : int = if x == 0 then lo else go (x - 1) in go
;;
let _ = print_int (count 7 3);;
|};
  [%expect {| 7 |}]
;;
