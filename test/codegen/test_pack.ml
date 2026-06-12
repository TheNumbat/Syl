open! Core
open! Syl

let go ?print ?(check = `Run) input = Common.codegen ?print ~check input

let%expect_test "fun with static erased type arg — polymorphic identity" =
  go
    {|
fun id (static erased t : type) : t -> t = fn (x : t) -> x;;
let _ = id int 0;;
let _ = id bool true;;
|};
  [%expect {| |}]
;;

let%expect_test "if arm returns pack (polymorphic binder)" =
  go
    {|
let x = false && true;;
let _ = if x then
   (fn (static erased t : type) -> ())
   else (fn (static erased t : type) -> ())
;;
|};
  [%expect {| |}]
;;

let%expect_test "match arm returns pack (polymorphic binder)" =
  go
    {|
let _ = match true with
  | true -> (fn (static erased t : type) -> ())
  | false -> (fn (static erased t : type) -> ())
;;
|};
  [%expect {| |}]
;;

let%expect_test "if returns polymorphic identity" =
  go
    {|
let _ = if true then
  (fn (static erased t : type) -> fn (x : t) -> x)
else
  (fn (static erased t : type) -> fn (x : t) -> x)
;;
|};
  [%expect {| |}]
;;

let%expect_test "match arm returns polymorphic identity" =
  go
    {|
let _ = match true with
  | true -> (fn (static erased t : type) -> fn (x : t) -> x)
  | false -> (fn (static erased t : type) -> fn (x : t) -> x)
;;
|};
  [%expect {| |}]
;;

let%expect_test "match arm returns polymorphic identity" =
  go
    {|
let _ = match true with
  | true -> (fn (static x : int) -> x)
  | false -> (fn (x : int) -> x)
;;
|};
  [%expect {| |}]
;;

let%expect_test "match arm returns polymorphic identity" =
  go
    {|
let _ = match true with
  | false -> (fn (x : int) -> x)
  | true -> (fn (static x : int) -> x)
;;
|};
  [%expect {| |}]
;;

let%expect_test "match arm returns polymorphic identity" =
  go
    {|
let _ = if (true @ dynamic) && true then
  (fn (static x : int) -> x)
else
  (fn (x : int) -> x)
;;
|};
  [%expect {| |}]
;;

let%expect_test "match arm returns polymorphic identity" =
  go
    {|
let _ = if true && true then
  (fn (static x : int) -> x)
else
  (fn (x : int) -> x)
;;
|};
  [%expect {| |}]
;;

let%expect_test "nested match with pack-typed inner body" =
  go
    {|
let _ = match true with
  | true -> (match false with
             | true -> (fn (static erased t : type) -> ())
             | false -> (fn (static erased t : type) -> ()))
  | false -> (fn (static erased t : type) -> ())
;;
|};
  [%expect {| |}]
;;

let%expect_test "match arm body is another match returning pack" =
  go
    {|
let _ = match true with
  | true -> (match false with
             | true -> (fn (static x : int) -> x)
             | false -> (fn (x : int) -> x))
  | false -> (fn (x : int) -> x)
;;
|};
  [%expect {| |}]
;;

let%expect_test "three-arm match on int with pack arms" =
  go
    {|
let _ = (fn (x : int) -> match x with
  | 0 -> (fn (static erased t : type) -> ())
  | 1 -> (fn (static erased t : type) -> ())
  | _ -> (fn (static erased t : type) -> ())) 5
;;
|};
  [%expect {| |}]
;;

let%expect_test "if with dynamic scrutinee and empty-pack arms inside lambda" =
  go
    {|
let f = (fn (b : bool) -> if b
  then (fn (static erased t : type) -> ())
  else (fn (static erased t : type) -> ())) true
;;
|};
  [%expect {| |}]
;;

let%expect_test "lambda returning pack" =
  go
    {|
let f = (fn (b : bool) -> (fn (static erased t : type) -> ())) true;;
let _ = f int;;
|};
  [%expect {| |}]
;;

let%expect_test "lambda returning pack" =
  go
    {|
let f = (fn (b : bool) -> (fn (static erased t : type) -> ()));;
|};
  [%expect {| |}]
;;

let%expect_test "lambda returning pack" =
  go
    {|
let _ = ((fn (b : bool) -> (fn (static erased t : type) -> ())) true) int;;
|};
  [%expect {| |}]
;;

let%expect_test "lambda returning pack" =
  go
    {|
let f = (fn (b : bool) -> let _ = print_int 0 in (fn (static erased t : type) -> ()));;
let g = f true;;
let _ = g int;;
|};
  [%expect {| 0 |}]
;;

let%expect_test "fun returning pack" =
  go
    {|
fun f (b : bool) : static (erased type -> unit) = (fn (static erased t : type) -> ());;
let _ = f true int;;
|};
  [%expect {| |}]
;;

let%expect_test "fun returning pack" =
  go
    {|
fun f (b : bool) : static (erased type -> unit) = (fn (static erased t : type) -> ());;
|};
  [%expect {| |}]
;;

let%expect_test "fun returning pack" =
  go
    {|
fun f (b : bool) : static (erased type -> unit) = (fn (static erased t : type) -> ());;
let g = f true;;
let _ = g int;;
|};
  [%expect {| |}]
;;

let%expect_test "if with dynamic scrutinee and empty-pack arms inside lambda" =
  go
    {|
let f = (fn (b : bool) -> if b
  then (fn (static erased t : type) -> ())
  else (fn (static erased t : type) -> ())) true
;;
let _ = f int;;
let _ = f bool;;
|};
  [%expect {| |}]
;;

let%expect_test "match with dynamic scrutinee and empty-pack arms inside lambda" =
  go
    {|
let _ = (fn (b : bool) -> match b with
  | true -> (fn (static erased t : type) -> ())
  | false -> (fn (static erased t : type) -> ())) true
;;
|};
  [%expect {| |}]
;;

let%expect_test "match with tuple pattern and pack body" =
  go
    {|
let _ = (fn (t : int ^ int) -> match t with
  | (a, b) -> (fn (static erased u : type) -> ())) (1, 2)
;;
|};
  [%expect {| |}]
;;

let%expect_test "if with non-empty pack arms (both arms monomorphized)" =
  go
    {|
let a = fn (static erased t : type) -> ();;
let b = fn (static erased t : type) -> ();;
let _ = a int;;
let _ = b int;;
let cond = true && true;;
let _ = if cond then a else b
;;
|};
  [%expect {| |}]
;;

let%expect_test "match with non-empty pack arms (both arms monomorphized)" =
  go
    {|
let a = fn (static erased t : type) -> ();;
let b = fn (static erased t : type) -> ();;
let _ = a int;;
let _ = b int;;
let cond = true && true;;
let _ = match cond with
  | true -> a
  | false -> b
;;
|};
  [%expect {| |}]
;;

let%expect_test "match leaves pack-typed unreachable case" =
  go
    {|
let x = 1 + 2;;
let _ = match x with
  | 0 -> (fn (static x : int) -> x)
  | 1 -> (fn (x : int) -> x)
  | _ -> (fn (x : int) -> x)
;;
|};
  [%expect {| |}]
;;

let%expect_test "three-arm match on int with pack arms" =
  go
    {|
let _ =
  fun f (x : int) : static (static int -> unit) =
  match x with
  | 0 -> (fn (static x : int) -> ())
  | 1 -> (fn (static x : int) -> ())
  | _ -> (fn (static x : int) -> ()) in
  f 5
;;
|};
  [%expect {| |}]
;;

let%expect_test "if on int with pack arms" =
  go
    {|
let _ =
  fun f (x : int) : static (static int -> unit) =
  if x == 0 then
    (fn (static x : int) -> ())
  else
    (fn (static x : int) -> ())
  in
  let _ = f 5 in
  let _ = f 6 in
  ()
;;
|};
  [%expect {| |}]
;;

let%expect_test "if on int with pack arms" =
  go
    {|
let _ =
  fun f (x : int) : static (static int \ x -> if x == 0 then unit else int) =
  if x == 0 then
    (fn (static x : int) -> if static x == 0 then () else x)
  else
    (fn (static x : int) -> if static x == 0 then () else x)
  in
  let _ = f 5 in
  let _ = f 6 in
  ()
;;
|};
  [%expect {| |}]
;;

let%expect_test "if on int with pack arms" =
  go
    {|
let _ =
  let f = fn (x : int) ->
  if x == 0 then
    (fn (static x : int) -> ())
  else
    (fn (static x : int) -> ())
  in
  let _ = f 5 in
  let _ = f 6 in
  ()
;;
|};
  [%expect {| |}]
;;

let%expect_test "if on int with pack arms" =
  go
    {|
let _ =
  let f = fn (x : int) ->
  if x == 0 then
    (fn (static x : int) -> if static x == 0 then () else x)
  else
    (fn (static x : int) -> if static x == 0 then () else x)
  in
  let _ = f 5 in
  let _ = f 6 in
  ()
;;
|};
  [%expect {| |}]
;;

let%expect_test "lambda with pack return" =
  go
    {|
let _ =
  let f = fn (x : int) ->
    (fn (static x : int) -> if static x == 0 then () else print_int x)
  in
  let f5 = f 5 in
  let f6 = f 6 in
  f6 2
;;
|};
  [%expect {| 2 |}]
;;

let%expect_test "fun with pack return" =
  go
    {|
let _ =
  fun f (x : int) : static (static int \ x -> if x == 0 then unit else int) =
    (fn (static x : int) -> if static x == 0 then () else x)
  in
  let _ = f 5 in
  let _ = f 6 in
  ()
;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: polymorphic identity on all base types" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = id int 42;;
let _ = id bool true;;
let _ = id unit ();;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: polymorphic identity on arrow type" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let f = id (int -> int) (fn (x : int) -> x + 1);;
let _ = f 10;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: polymorphic identity on closure type" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let inc = fn (x : int) -> x + 1;;
let f = id (int -> int) inc;;
let _ = f 10;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: polymorphic identity on tuple type" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = id (int ^ int) (1, 2);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: polymorphic identity on unit" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = id unit ();;
|};
  [%expect {| |}]
;;

let%expect_test "effects around lambda returning pack" =
  go
    {|
let _ =
  let make = fn (tag : int) ->
    let _ = print_int tag in
    (fn (static k : int) -> print_int k)
  in
  let zero = make 100 in
  let one = make 200 in
  let many = make 300 in
  let _ = one 1 in
  let _ = many 1 in
  let _ = many 2 in
  let _ = many 1 in
  ()
;;
|};
  [%expect
    {|
    100
    200
    300
    1
    1
    2
    1
    |}]
;;

let%expect_test "effects around fun returning pack" =
  go
    {|
let _ =
  fun make (tag : int) : static (static int -> unit) =
    let _ = print_int tag in
    (fn (static k : int) -> print_int k)
  in
  let zero = make 1000 in
  let one = make 2000 in
  let many = make 3000 in
  let _ = one 1 in
  let _ = many 1 in
  let _ = many 2 in
  let _ = many 1 in
  ()
;;
|};
  [%expect
    {|
    1000
    2000
    3000
    1
    1
    2
    1
    |}]
;;

let%expect_test "effects around if returning pack" =
  go
    {|
let _ =
  let make = fn (choose : bool) ->
    let _ = print_int 4000 in
    if choose
    then
      (fn (static k : int) -> print_int (k + 10))
    else
      (fn (static k : int) -> print_int (k + 20))
  in
  let zero = make true in
  let one = make false in
  let many = make true in
  let _ = one 1 in
  let _ = many 1 in
  let _ = many 2 in
  let _ = many 1 in
  ()
;;
|};
  [%expect
    {|
    4000
    4000
    4000
    21
    11
    12
    11
    |}]
;;
