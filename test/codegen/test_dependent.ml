open! Core
open! Syl

let go ?print ?(check = `Run) input = Common.codegen ?print ~check input

let%expect_test "dependent closure arg" =
  go
    {|
let _ =
  (fn (x : type) -> x)
;;|};
  [%expect {| |}]
;;

let%expect_test "dependent closure arg" =
  go
    {|
let f = if true then
  (fn (erased x : type) -> 0) else (fn (erased x : type) -> 1);;
let _ = f int;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent closure arg" =
  go
    {|
fun f (x : type) : type = x;;|};
  [%expect {| |}]
;;

let%expect_test "dependent closure arg" =
  go
    {|
fun f (static x : int) : static erased type = int;;|};
  [%expect {| |}]
;;

let%expect_test "dependent closure arg" =
  go
    {|
let _ =
  (fn (static x : type) -> x)
;;|};
  [%expect {| |}]
;;

let%expect_test "dependent closure arg" =
  go
    {|
let _ =
  (fn (erased x : type) -> x)
;;|};
  [%expect {| |}]
;;

let%expect_test "Dependent lambda" =
  go
    {|
let f = fn (static erased ty : type) -> fn (x : ty) -> x;;
|};
  [%expect {| |}]
;;

let%expect_test "Dependent lambda" =
  go
    {|
let f = fn (static erased ty : type) -> fn (x : ty) -> x;;
let g = f int;;
let _ = g 0;;
let g = f bool;;
let _ = g true;;
|};
  [%expect {| |}]
;;

let%expect_test "Dependent lambda" =
  go
    {|
let f = fn (static g : int -> int) -> g 0;;
let _ = f (fn (x : int) -> x + 1);;
|};
  [%expect {| |}]
;;

let%expect_test "dependent unit" =
  go
    {|
let f = fn (static x : unit) -> let x = 0 in ();;
let _ = f ();;
|};
  [%expect {| |}]
;;

let%expect_test "dependent bool" =
  go
    {|
let f = fn (static x : bool) -> !x;;
let _ = f true;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent int" =
  go
    {|
let f = fn (static x : int) -> -x;;
let _ = f 1;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent type" =
  go
    {|
let f = fn (static erased t : type) -> fn (x : t) -> if true then x else x;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent lambda" =
  go
    {|
let f = fn (static g : static erased type -> int -> int) -> g int;;
let _ = f (fn (static erased t : type) -> fn (x : int) -> x);;
|};
  [%expect {| |}]
;;

let%expect_test "dependent fn" =
  go
    {|
let id = fn (static erased t : type) -> (fn (x : t) -> x);;
let x = (id int) (0 @ dynamic);;
let y = (id bool) (true @ dynamic);;
|};
  [%expect {| |}]
;;

let%expect_test "dependent fn" =
  go
    {|
let id = fn (static erased t : type) -> (fn (x : t) -> x);;
let x = (id int) (0 @ dynamic);;
let y = (id bool) (true @ dynamic);;
|};
  [%expect {| |}]
;;

let%expect_test "dependent arrow" =
  go
    {|
let mk_int = fn (static x : int) -> int;;
let apply_int = fn (static f : static int \ x -> mk_int x) -> 2;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent arrow" =
  go
    {|
let mk_int = fn (static x : int) -> int;;
let apply_int = fn (static f : static int \ x -> unit -> mk_int x) -> f 2;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent arrow" =
  go
    {|
let apply_int = fn (static f : static erased type \ t -> t -> t) -> f int;;
let _ = apply_int (fn (static erased t : type) -> fn (x : t) -> x);;
|};
  [%expect {| |}]
;;

let%expect_test "dependent arrow" =
  go
    {|
let apply = fn (static f : static erased type \ t -> t -> t) -> fn (static erased t2 : type) -> f t2;;
let f = apply (fn (static erased t : type) -> fn (x : t) -> x);;
let g = f int;;
let h = f bool;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static x : int) -> if erased x == 0 then 1 else true;;
let _ = f 0;;
let _ = f 1;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent if unify" =
  go
    {|
let f = fn (static x : int) -> if erased x == 0 then 1 else true;;
let g = fn (static x : int) -> if erased x == 0 then true else 2;;
let _ = if true then f 0 else g 1;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent fun " =
  go
    {|
fun id (static erased t : type) : t -> t = fn (x : t) -> x;;
let i = id int;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent fun erased" =
  go
    {|
fun id (static erased t : type) : t -> t = fn (x : t) -> x;;
let x = (id int) (0 @ dynamic);;
|};
  [%expect {| |}]
;;

let%expect_test "dependent fun" =
  go
    {|
let ty = fn (static _ : unit) -> int -> int;;
fun id (_ : unit) : ty () = fn (x : int) -> x;;
let x = id () 0;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent fun" =
  go
    {|
fun id1 (static erased t : type) : t -> t = fn (x : t) -> x;;
fun id2 (static erased t : type) : t -> t = id1 t;;
let x = id2 int (0 @ dynamic);;
|};
  [%expect {| |}]
;;

let%expect_test "dependent if unify" =
  go
    {|
let f = fn (static x : int) -> if erased x == 0 then 1 else true;;
let g = fn (static x : int) -> if erased x == 0 then 0 else false;;
let h = if true then f else g;;
let _ = h 0;;
let _ = h 1;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static x : int) -> (if erased x == 0 then 1 else true) : (if x == 0 then int else bool);;
let _ = f 0;;
let _ = f 1;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static x : int) -> (if erased x == 1 + 1 then 1 else true) : (if x == 2 then int else bool);;
let _ = f 1;;
let _ = f 2;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static x : int) -> (if erased x == x + 1 then 1 else true) : (if x == x + 1 then int else bool);;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static x : int) -> (if erased x == (if x == 1 then x else 0) then 1 else true) : (if x == (if x == 1 then x else 0) then int else bool);;
let _ = f 0;;
let _ = f 1;;
let _ = f 2;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent abstraction" =
  go
    {|
let choose = fn (static f : static int \ x -> if x == 0 then int else bool) -> f 0;;
let _ = choose (fn (static x : int) -> if erased x == 0 then 0 else true);;
|};
  [%expect {| |}]
;;

let%expect_test "if erased with nested dependent types in branches" =
  go
    {|
let f = fn (static x : int) -> if erased x == 0 then fn (y : int) -> y else fn (y : bool) -> y;;
let g = f 0;;
let _ = g 42;;
let h = f 1;;
let _ = h true;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent arrow type with backslash binder" =
  go
    {|
let f = fn (static g : static int \ x -> int) -> g 0;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent arrow with return type depending on arg" =
  go
    {|
let mk_int = fn (static x : int) -> int;;
let f = fn (static g : static int \ x -> mk_int x) -> g 0;;
|};
  [%expect {| |}]
;;

let%expect_test "joining f 0 and g 1 resolves dependent types" =
  go
    {|
let f = fn (static x : int) -> if erased x == 0 then 1 else true;;
let g = fn (static x : int) -> if erased x == 0 then true else 2;;
let _ = if true then f 0 else g 1;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent type: apply polymorphic id to itself" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = (id (int -> int)) (fn (x : int) -> x + 1) 5;;
|};
  [%expect {| |}]
;;

let%expect_test "chaining dependent applications" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let f = id int;;
let g = id bool;;
let _ = f 0;;
let _ = g true;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: dependent type computation" =
  go
    {|
let choose_type = fn (static erased b : bool) ->
  if erased b then int else bool;;
let _ = (42 : choose_type true);;
let _ = (true : choose_type false);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: dependent return feeding another dependent" =
  go
    {|
let choose = fn (static erased b : bool) -> if erased b then int else bool;;
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = id (choose true) 42;;
let _ = id (choose false) true;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: dependent type choosing function type" =
  go
    {|
let choose = fn (static erased b : bool) ->
  if erased b then int -> int else bool -> bool;;
let f = (fn (x : int) -> x + 1) : choose true;;
let g = (fn (x : bool) -> !x) : choose false;;
let _ = f 10;;
let _ = g true;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: dependent type in function param" =
  go
    {|
let choose = fn (static erased b : bool) -> if erased b then int else bool;;
let f = fn (static erased b : bool) -> fn (x : choose b) -> x;;
let _ = f true 42;;
let _ = f false true;;
|};
  [%expect {| |}]
;;

(* Computed dependent types with recursive type-level functions: comparing a
   body's residual type against [ivec n] requires unfolding the application
   one step during leq, and identical residual applications compare by value
   identity. *)
let%expect_test "recursive computed type built generically" =
  go
    {|
fun ivec (static n : int) : erased type =
  if erased n == 1 then int else int ^ ivec (n - 1)
;;

fun replicate (static n : int) : int -> ivec n =
  fn (x : int) -> if erased n == 1 then x else (x, replicate (n - 1) x)
;;

let sum4 = fn (v : ivec 4) -> match v { (a, (b, (c, d))) -> a + b + c + d };;

let _ = print_int (sum4 (replicate 4 10));;

let _ = print_int (sum4 ((1, (2, (3, 4))) : ivec 4));;
|};
  [%expect
    {|
    40
    10
    |}]
;;

let%expect_test "instantiation check via erased if and unreachable" =
  go
    {|
let div_by =
  fn (static d : int) -> if erased d != 0 then (fn (x : int) -> x / d) else unreachable
;;
let _ = print_int (div_by 2 10);;
let _ = print_int (div_by 5 10);;
|};
  [%expect
    {|
    5
    2
    |}]
;;

(* One family whose monos have different result types. *)
let%expect_test "heterogeneous result types within a family" =
  go
    {|
let choose = fn (static b : bool) -> if erased b then 1 else false;;
let _ = print_int (choose true);;
let _ = print_bool (choose false);;
|};
  [%expect
    {|
    1
    false
    |}]
;;

let%expect_test "static tuple result carrying a closure" =
  go
    {|
fun mk (static n : int) : static ((int -> int) ^ int) =
  ((fn (x : int) -> x + n), n)
;;

let _ = match mk 4 { (f, k) -> print_int (f 10 + k) };;
|};
  [%expect {| 18 |}]
;;

(* Arrow types as keys carry their modes: three distinct keys. *)
let%expect_test "arrow keys distinguished by parameter modes" =
  go
    {|
let size = fn (static erased t : type) -> 1;;
let _ = print_int (size (static int -> int));;
let _ = print_int (size (int -> int));;
let _ = print_int (size ((int ^ bool) -> unit));;
|};
  [%expect
    {|
    1
    1
    1
    |}]
;;
