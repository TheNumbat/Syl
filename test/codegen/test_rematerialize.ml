open! Core
open! Syl

let go ?print ?(check = `Run) input = Common.codegen ?print ~check input

let%expect_test "recursive reify through recursive captured binder" =
  go
    {|
let make_apply = fn (static f : static int -> int) -> f 2;;
let n = 100;;
fun down(static x : int) : int = if erased x == 0 then n else down (x - 1);;
let _ = print_int (make_apply down);;
|};
  [%expect {| 100 |}]
;;

let%expect_test "recursive reify through tuple containing recursive captured closure" =
  go
    {|
let make_apply = fn (static f : int -> int) -> fn (x : int) -> f x;;
let n = 100;;
fun down (x : int) : int = if x == 0 then n else down (x - 1);;
let pair = (down, 0);;
let f = fn (x : int) -> match pair with | (g, _) -> g x;;
let _ = print_int (make_apply f 2);;
|};
  [%expect {| 100 |}]
;;

let%expect_test "reify rematerialized static closure ignores unrelated dynamic env entry" =
  go
    {|
let buf = 0 @ dynamic;;
let make = fn (static n : int) -> fn (x : int) -> x + n;;
let use = fn (static f : int -> int) -> f 2;;
let _ = print_int (use (make 40));;
|};
  [%expect {| 42 |}]
;;

let%expect_test "binder rematerialized by simplify" =
  go
    {|
let f = fn (static x : int) -> print_int x;;
let _ = f 0;;
let _ = f 10;;
let _ = f 20;;
let _ = f 30;;
let g = f;;
let h = f;;
let _ = h 1;;
let _ = h 2;;
let _ = g 3;;
let _ = f 4;;
|};
  [%expect
    {|
    0
    10
    20
    30
    1
    2
    3
    4
    |}]
;;

let%expect_test "binder rematerialized by simplify" =
  go
    {|
let make = fn (static x : int) -> fn (static y : int) -> x + y;;
let f = make 10;;
let g = make 20;;
let _ = f 0;;
let _ = g 0;;
|};
  [%expect {| |}]
;;

let%expect_test "binder rematerialized by simplify_value" =
  go
    {|
let f = fn (static x : int) -> x;;
let consume = fn (erased g : static int -> int) -> ();;
let _ = consume (f @ erased);;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "binder rematerialized by simplify_concrete" =
  go
    {|
let f = fn (static x : int) -> x;;
let use = fn (static g : static int -> int) -> g 0;;
let _ = use f;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "same-stamp binders rematerialized by simplify_concrete stay distinct" =
  go
    {|
let make = fn (static n : int) -> fn (static x : int) -> n + x;;
let use2 =
  fn (static f : static int -> int) ->
  fn (static g : static int -> int) ->
    f 0 + g 0;;
let f = make 10;;
let g = make 20;;
let _ = print_int (use2 f g);;
|};
  [%expect {| 30 |}]
;;

let%expect_test "same-stamp binders rematerialized by simplify_value stay distinct" =
  go
    {|
let make = fn (static n : int) -> fn (static x : int) -> n + x;;
let f = make 10;;
let g = make 20;;
let use2 =
  fn (static f : static int -> int) ->
  fn (static g : static int -> int) ->
    f 0 + g 0;;
let _ = print_int (use2 f g);;
|};
  [%expect {| 30 |}]
;;

let%expect_test "same-stamp closures rematerialized by simplify_concrete stay distinct" =
  go
    {|
let make = fn (static n : int) -> fn (x : int) -> n + x;;
let use2 =
  fn (static f : int -> int) ->
  fn (static g : int -> int) ->
    f 0 + g 0;;
let f = make 10;;
let g = make 20;;
let _ = print_int (use2 f g);;
|};
  [%expect {| 30 |}]
;;

let%expect_test "same-stamp closures rematerialized by simplify_value stay distinct" =
  go
    {|
let make = fn (static n : int) -> fn (x : int) -> n + x;;
let f = make 10;;
let g = make 20;;
let use2 =
  fn (static f : int -> int) ->
  fn (static g : int -> int) ->
    f 0 + g 0;;
let _ = print_int (use2 f g);;
|};
  [%expect {| 30 |}]
;;

let%expect_test "same-stamp closures rematerialized through static tuple stay distinct" =
  go
    {|
let make = fn (static n : int) -> fn (x : int) -> n + x;;
let f = make 10;;
let g = make 20;;
let use =
  fn (static p : (int -> int) ^ (int -> int)) ->
  fn (x : int) ->
    match p with
    | (a, b) -> a x + b x;;
let _ = print_int (use (f, g) 0);;
|};
  [%expect {| 30 |}]
;;

let%expect_test "binder rematerialized through returned factory pair" =
  go
    {|
let make =
  fn (static n : int) ->
    let apply = fn (static f : static int -> int) -> f 0 in
    let down = fn (static x : int) -> n in
    (apply, down)
;;
let _ =
  match make 100 with
  | (apply, down) -> print_int (apply down)
;;
|};
  [%expect {| 100 |}]
;;

let%expect_test "rematerialized tuple shares recursive captured closure" =
  go
    {|
let use_pair =
  fn (static pair : (int -> int) ^ (int -> int)) ->
  fn (x : int) ->
    match pair with
    | (f, g) -> f x + g x
;;
let n = 10;;
fun down (x : int) : int = if x == 0 then n else down (x - 1);;
let pair = (down, down);;
let _ = print_int (use_pair pair 2);;
|};
  [%expect {| 20 |}]
;;

let%expect_test "binder rematerialized before later specialization keeps captures" =
  go
    {|
let capture = fn (static f : static int -> int) -> f;;
let make = fn (static n : int) -> fn (static x : int) -> n + x;;
let g = capture (make 100);;
let _ = print_int (g 0);;
|};
  [%expect {| 100 |}]
;;

let%expect_test "nested binder rematerialized before inner specialization keeps captures" =
  go
    {|
let capture = fn (static f : static int -> static (static int -> int)) -> f;;
let make =
  fn (static n : int) ->
  fn (static x : int) ->
  fn (static y : int) ->
    n + x + y
;;
let use =
  fn (static f : static int -> static (static int -> int)) ->
    (f 1) 2
;;
let _ = print_int (use (capture (make 100)));;
|};
  [%expect {| 103 |}]
;;

let%expect_test "recursive binder group rematerialized before sibling captures are filled" =
  go
    {|
let capture = fn (static f : static int -> int) -> f;;
let n = 100;;
fun f (static x : int) : int = (capture g) x
and g (static y : int) : int = n;;
let use = fn (static h : static int -> int) -> h 0;;
let _ = print_int (use f);;
|};
  [%expect {| 100 |}]
;;

let%expect_test "static tuple of scalars rematerialized into binder body" =
  go
    {|
let use =
  fn (static t : int ^ bool ^ unit) ->
  fn (k : int) ->
    match t with
    | (i, b, _) -> if b then i + k else k - i;;
let _ = print_int (use (10, true, ()) 5);;
let _ = print_int (use (10, false, ()) 5);;
|};
  [%expect
    {|
    15
    -5
    |}]
;;

let%expect_test "static three-tuple of distinct closures rematerialized" =
  go
    {|
let n = 100;;
let m = 200;;
let a = fn (x : int) -> x + n;;
let b = fn (x : int) -> x + m;;
let c = fn (x : int) -> x + 1;;
let use =
  fn (static t : (int -> int) ^ (int -> int) ^ (int -> int)) ->
  fn (x : int) ->
    match t with
    | (p, q, r) -> p x + q x + r x;;
let _ = print_int (use (a, b, c) 1);;
|};
  [%expect {| 304 |}]
;;

let%expect_test "static tuple closure captures another tuple closure" =
  go
    {|
let inc = fn (x : int) -> x + 1;;
let then_dbl = fn (x : int) -> inc x * 2;;
let use_pair =
  fn (static p : (int -> int) ^ (int -> int)) ->
  fn (x : int) ->
    match p with
    | (a, b) -> a x + b x;;
let _ = print_int (use_pair (inc, then_dbl) 5);;
|};
  [%expect {| 18 |}]
;;

let%expect_test "static tuple of mutually-recursive funs rematerialized" =
  go
    {|
fun f (x : int) : int = if x == 0 then 1 else g (x - 1)
and g (x : int) : int = if x == 0 then 2 else f (x - 1);;
let use =
  fn (static p : (int -> int) ^ (int -> int)) ->
  fn (x : int) ->
    match p with
    | (a, b) -> a x + b x;;
let _ = print_int (use (f, g) 2);;
let _ = print_int (use (g, f) 2);;
|};
  [%expect
    {|
    3
    3
    |}]
;;

let%expect_test "static tuple of polymorphic binder specializations rematerialized" =
  go
    {|
let id =
  fn (static erased t : type) -> fn (x : t) -> x;;
let use =
  fn (static p : (int -> int) ^ (int -> int)) ->
  fn (x : int) ->
    match p with
    | (a, b) -> a x + b x;;
let _ = print_int (use (id int, id int) 5);;
|};
  [%expect {| 10 |}]
;;

let%expect_test "local closure capturing local let rematerialized into binder body" =
  go
    {|
let use = fn (static f : int -> int) -> f 10 + f 20;;
let _ =
  let n = 100 in
  let g = fn (x : int) -> x + n in
  print_int (use g)
;;
|};
  [%expect {| 230 |}]
;;

let%expect_test "static closure transitively capturing another static closure" =
  go
    {|
let n = 10;;
let a = fn (x : int) -> x + n;;
let b = fn (x : int) -> a x + n;;
let use = fn (static f : int -> int) -> fn (x : int) -> f x;;
let _ = print_int (use b 5);;
|};
  [%expect {| 25 |}]
;;

let%expect_test "rematerialized closure reifies erased helper with private capture" =
  go
    {|
let g = let n = 7 in fn (x : int) -> x + n;;
let f = fn (x : int) -> g x;;
let c = fn (z : int) -> f z;;
let use = fn (static h : int -> int) -> h 1;;
let _ = print_int (use c);;
|};
  [%expect {| 8 |}]
;;

let%expect_test "rematerialized closure captures sibling with env-private state" =
  go
    {|
let make = fn (static a : int) ->
  let inner =
    let private_x = a + 1 in
    fn (y : int) -> y + private_x
  in
  fn (z : int) -> inner z;;
let outer = make 10;;
let use = fn (static h : int -> int) -> h 5;;
let _ = print_int (use outer);;
|};
  [%expect {| 16 |}]
;;

let%expect_test "rematerialized closure captures sibling with multiple private bindings" =
  go
    {|
let make = fn (static a : int) ->
  let inner =
    let p = a + 1 in
    let q = a + 2 in
    fn (y : int) -> y + p + q
  in
  fn (z : int) -> inner z + z;;
let outer = make 10;;
let use = fn (static h : int -> int) -> h 5;;
let _ = print_int (use outer);;
|};
  [%expect {| 33 |}]
;;

let%expect_test "rematerialized closure captures two siblings each with disjoint private state" =
  go
    {|
let make = fn (static a : int) ->
  let inner_a =
    let pa = a + 1 in
    fn (y : int) -> y + pa
  in
  let inner_b =
    let pb = a + 2 in
    fn (y : int) -> y + pb
  in
  fn (z : int) -> inner_a z + inner_b z;;
let outer = make 10;;
let use = fn (static h : int -> int) -> h 5;;
let _ = print_int (use outer);;
|};
  [%expect {| 33 |}]
;;

let%expect_test "polymorphic binder specialization shared across call sites" =
  go
    {|
let id =
  fn (static erased t : type) -> fn (x : t) -> x;;
let _ = print_int (id int 7);;
let _ = print_bool (id bool true);;
let _ = print_int (id int 7);;
let _ = print_bool (id bool false);;
|};
  [%expect
    {|
    7
    true
    7
    false
    |}]
;;

let%expect_test "recursive binder specialization shared across applications" =
  go
    {|
let n = 100;;
fun down (static x : int) : int = if erased x == 0 then n else down (x - 1);;
let make_apply = fn (static f : static int -> int) -> f 2 + f 3;;
let _ = print_int (make_apply down);;
let _ = print_int (make_apply down);;
let _ = print_int (down 2);;
|};
  [%expect
    {|
    200
    200
    100
    |}]
;;

let%expect_test "recursive binder specialization shared via closure boundary" =
  go
    {|
let n = 100;;
fun down (static x : int) : int = if erased x == 0 then n else down (x - 1);;
let make_apply = fn (static f : static int -> int) -> f 2;;
let go = fn (k : int) -> make_apply down + k;;
let _ = print_int (go 1);;
let _ = print_int (go 2);;
|};
  [%expect
    {|
    101
    102
    |}]
;;

let%expect_test "reify rematerialized closure projected from static tuple" =
  go
    {|
let make = fn (static n : int) -> fn (x : int) -> x + n;;
let use =
  fn (static p : (int -> int) ^ int) ->
    match p with
    | (f, _) -> f 2;;
let _ = print_int (use (make 40, 0));;
|};
  [%expect {| 42 |}]
;;

let%expect_test "reify rematerialized specialization argument" =
  go
    {|
let static_id = fn (static erased t : type) -> fn (static x : t) -> x;;
let make = fn (static n : int) -> fn (x : int) -> x + n;;
let use = fn (static f : int -> int) -> f 2;;
let _ = print_int (use (static_id (int -> int) (make 40)));;
|};
  [%expect {| 42 |}]
;;

let%expect_test "reify rematerialized closure selected by static branch" =
  go
    {|
let choose =
  fn (static b : bool) ->
    if erased b then fn (x : int) -> x + 1 else fn (x : int) -> x + 2;;
let use = fn (static f : int -> int) -> f 10;;
let _ = print_int (use (choose true));;
let _ = print_int (use (choose false));;
|};
  [%expect
    {|
    11
    12
    |}]
;;

let%expect_test "binder reached through static application result" =
  go
    {|
let f = fn (x : int) -> fn (static erased t : type) -> fn (v : t) -> v;;
let id2 = f 0;;
let _ = print_int (id2 int 5);;
|};
  [%expect {| 5 |}]
;;

let%expect_test "specialization result of baked static function parameter" =
  go
    {|
let id_t = fn (static erased t : type) -> fn (x : t) -> x;;
let ret_p = fn (static p : static erased type \ s -> s -> s) -> p;;
let _ = print_int (ret_p id_t int 5);;
|};
  [%expect {| 5 |}]
;;

let%expect_test "chained specialization inside re-evaluated binder mono" =
  go
    {|
let id_t = fn (static erased t : type) -> fn (x : t) -> x;;
let ret_p = fn (static p : static erased type \ s -> s -> s) -> p;;
let f = fn (b : bool) -> fn (static erased u : type) -> fn (v : u) -> ret_p id_t u v;;
let g = f true;;
let _ = print_int (g int 5);;
|};
  [%expect {| 5 |}]
;;

let%expect_test "value both baked into a mono and specialized via its definition site" =
  go
    {|
let f = fn (x : int) -> fn (static erased t : type) -> fn (v : t) -> v;;
let g = f 0;;
let use = fn (static p : static erased type \ s -> s -> s) -> p int 1;;
let _ = print_int (use g);;
let _ = print_int (g int 5);;
|};
  [%expect
    {|
    1
    5
    |}]
;;

let%expect_test "quoted binder flows through a tuple result and a match pattern" =
  go
    {|
let ret_pair = fn (static p : static erased type \ s -> s -> s) -> (p, 0);;
let id_t = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = match ret_pair id_t with | (f, _) -> print_int (f int 5);;
|};
  [%expect {| 5 |}]
;;

let%expect_test "local binding reused instead of rematerializing into a mono" =
  go
    {|
let result =
  let local_id = fn (static erased t : type) -> fn (x : t) -> x in
  let use = fn (static p : static erased type \ s -> s -> s) -> p int 7 in
  use local_id;;
let _ = print_int result;;
|};
  [%expect {| 7 |}]
;;

let%expect_test "one value baked into two monos shares one materialization" =
  go
    {|
let use_int = fn (static p : static erased type \ s -> s -> s) -> p int 1;;
let use_bool = fn (static p : static erased type \ s -> s -> s) -> p bool true;;
let id_t = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = print_int (use_int id_t);;
let _ = print_bool (use_bool id_t);;
|};
  [%expect
    {|
    1
    true
    |}]
;;

(* [g1] and [g2] are two values of one family, specialized at the same key.
   Their mono bodies agree (b is env-provided), but each application's result
   embeds a closure created under that value's captured env (b = true vs
   false). Memoizing specializations per (family, key) instead of per value
   would hand [g2]'s caller a result capturing [g1]'s b and quote 7 for [c]. *)
let%expect_test "shared specialization key keeps per-value result captures" =
  go
    {|
let make = fn (b : bool) -> fn (static erased t : type) -> fn (v : int) -> if b then v else 0 - v;;
let g1 = make true;;
let g2 = make false;;
let use = fn (static h : int -> int) -> h 7;;
let a = use (g1 int);;
let c = use (g2 int);;
let _ = print_int a;;
let _ = print_int c;;
|};
  [%expect
    {|
    7
    -7
    |}]
;;

(* [capture]'s mono materializes [use]'s value before [add_n] is defined, and
   that materialization escapes as [g]; [mid]'s mono materializes the same
   value after [add_n] is in scope. The dispatch [g add_n] pairs the escaped
   environment with whichever registration of the family linearize saw last,
   so the two materializations must agree exactly. A scope-sensitive reify
   gave them different environment layouts and miscompiled [c] to 2. *)
let%expect_test "escaped quote dispatched after a later quote of the same value" =
  go
    {|
let capture = fn (static p : static (int -> int) -> int) -> p;;
let n = 5;;
let add_n = fn (x : int) -> x + n;;
let mid = fn (static u : static (int -> int) -> int) -> fn (static g : int -> int) -> u g;;
let use = fn (static f : int -> int) -> f 2;;
let a = use add_n;;
let g = capture use;;
let b = (mid use) add_n;;
let c = g add_n;;
let _ = print_int a;;
let _ = print_int b;;
let _ = print_int c;;
|};
  [%expect
    {|
    7
    7
    7
    |}]
;;

let%expect_test "specialization results chained across two helpers" =
  go
    {|
let ret_p = fn (static p : static erased type \ s -> s -> s) -> p;;
let ret_ret = fn (static q : static erased type \ s -> s -> s) -> ret_p q;;
let id_t = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = print_int (ret_ret id_t int 9);;
|};
  [%expect {| 9 |}]
;;

let%expect_test "definition-site dispatch before the value is baked into a mono" =
  go
    {|
let f = fn (x : int) -> fn (static erased t : type) -> fn (v : t) -> v;;
let g = f 0;;
let _ = print_int (g int 5);;
let use = fn (static p : static erased type \ s -> s -> s) -> p int 1;;
let _ = print_int (use g);;
|};
  [%expect
    {|
    5
    1
    |}]
;;

let%expect_test "two re-evaluated values of one family union their keys at the definition site" =
  go
    {|
let make = fn (b : bool) -> fn (static erased t : type) -> fn (v : t) -> v;;
let g1 = make true;;
let g2 = make false;;
let _ = print_int (g1 int 1);;
let _ = print_bool (g1 bool true);;
let _ = print_int (g2 int 2);;
|};
  [%expect
    {|
    1
    true
    2
    |}]
;;

let%expect_test "reify rematerialized binder projected from static tuple" =
  go
    {|
let id_t = fn (static erased t : type) -> fn (x : t) -> x;;
let pair = (id_t, 41);;
let use = fn (static p : (static erased type \ s -> s -> s) ^ int) -> match p with | (f, n) -> f int n;;
let _ = print_int (use pair);;
|};
  [%expect {| 41 |}]
;;

let%expect_test "two projections of one static tuple component both specialize" =
  go
    {|
let use =
  fn (static p : (static erased type \ s -> s -> s) ^ int) ->
    match p with
    | (f, _) -> (match p with | (g, n) -> f int (g int n));;
let id_t = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = print_int (use (id_t, 23));;
|};
  [%expect {| 23 |}]
;;

(* The generic pass of [outer] records a mono of [use] keyed by an
   abstract-context closure whose captured [c] is a function-containing
   residual. Such generic monos are excluded from the program store; only the
   per-instantiation mono (where [c] is resolved) is compiled. *)
let%expect_test "generic-pass mono with residual function capture is not compiled" =
  go
    {|
let use = fn (static h : int -> int) -> h 1;;
let outer = fn (static b : bool) ->
  let c = if erased b then (fn (x : int) -> x) else (fn (x : int) -> 0 - x) in
  use (fn (y : int) -> c y);;
let _ = print_int (outer true);;
let _ = print_int (outer false);;
|};
  [%expect
    {|
    1
    -1
    |}]
;;

(* A recursive mono chain rebinds one lexical [let f] per level, so quoting a
   chain closure nests same-ident lets. Each capture must quote against its
   own env: rooting a recursive group at the capture's ident conflated the
   levels and the quoted closure captured itself (stack overflow). *)
let%expect_test "quoted closure chain rebinds same-ident captures per level" =
  go
    {|
fun mk (static n : int) : static (int -> int) =
  if erased n == 0 then (fn (x : int) -> x) else (let f = mk (n - 1) in fn (x : int) -> f x + 1)
;;
let use = fn (static h : int -> int) -> h 0;;
let _ = print_int (mk 3 100);;
let _ = print_int (use (mk 2));;
|};
  [%expect
    {|
    103
    2
    |}]
;;

let%expect_test "two chain depths quoted as distinct keys" =
  go
    {|
fun mk (static n : int) : static (int -> int) =
  if erased n == 0 then (fn (x : int) -> x) else (let f = mk (n - 1) in fn (x : int) -> f x + 1)
;;
let use = fn (static h : int -> int) -> h 0;;
let _ = print_int (use (mk 2));;
let _ = print_int (use (mk 3));;
let _ = print_int (mk 3 0);;
|};
  [%expect
    {|
    2
    3
    3
    |}]
;;

let%expect_test "quoted capture of a fun-group member rebuilds the group" =
  go
    {|
fun even (x : int) : bool = if x == 0 then true else odd (x - 1)
and odd (x : int) : bool = if x == 0 then false else even (x - 1);;
let use = fn (static h : int -> bool) -> h 4;;
let _ = print_bool (use (fn (x : int) -> even x));;
|};
  [%expect {| true |}]
;;

let%expect_test "chain closure over a fun-group base" =
  go
    {|
fun double (x : int) : int = x + x;;
fun mk (static n : int) : static (int -> int) =
  if erased n == 0 then (fn (x : int) -> double x) else (let f = mk (n - 1) in fn (x : int) -> f x + 1)
;;
let use = fn (static h : int -> int) -> h 10;;
let _ = print_int (use (mk 2));;
|};
  [%expect {| 22 |}]
;;

(* The key closure's mono result is itself the key of another dispatch. *)
let%expect_test "mono result quoted as another mono's key" =
  go
    {|
let use = fn (static h : int -> int) -> h 1;;
let twice = fn (static g : int -> int) -> fn (x : int) -> g (g x);;
let _ = print_int (use (twice (fn (y : int) -> y + 3)));;
|};
  [%expect {| 7 |}]
;;

let%expect_test "quoted capture resolves the shadowing binding" =
  go
    {|
let n = 1;;
let f = (let n = 2 in fn (x : int) -> x + n) @ static;;
let use = fn (static h : int -> int) -> h 0;;
let _ = print_int (use f);;
|};
  [%expect {| 2 |}]
;;

(* Reachability through an argument-position closure: the seed walk finds the
   dispatch in the key closure's body, and that mono's body names the next. *)
let%expect_test "store reachability is transitive through key closures" =
  go
    {|
let lit = fn (static k : int) -> k;;
fun outer (static n : int) : int = lit n;;
let use = fn (static h : unit -> int) -> h ();;
let _ = print_int (use (fn (_ : unit) -> outer 9));;
|};
  [%expect {| 9 |}]
;;

let%expect_test "chain closure capturing its own fun" =
  go
    {|
fun f (static n : int) : static (int -> int) =
  if erased n == 0 then (fn (x : int) -> x) else (fn (x : int) -> (f (n - 1)) x + 1)
;;
let use = fn (static h : int -> int) -> h 0;;
let _ = print_int (use (f 2));;
|};
  [%expect {| 2 |}]
;;

let%expect_test "quote of a quote of a fun group" =
  go
    {|
fun even (x : int) : bool = if x == 0 then true else odd (x - 1)
and odd (x : int) : bool = if x == 0 then false else even (x - 1);;
let wrap = fn (x : int) -> even x;;
let wrap2 = fn (x : int) -> wrap x;;
let use = fn (static h : int -> bool) -> h 2;;
let _ = print_bool (use wrap2);;
|};
  [%expect {| true |}]
;;

let%expect_test "same-ident chain over a fun-group base" =
  go
    {|
fun even (x : int) : bool = if x == 0 then true else odd (x - 1)
and odd (x : int) : bool = if x == 0 then false else even (x - 1);;
fun mk (static n : int) : static (int -> bool) =
  if erased n == 0 then (fn (x : int) -> even x) else (let f = mk (n - 1) in fn (x : int) -> f (x + 1))
;;
let use = fn (static h : int -> bool) -> h 0;;
let _ = print_bool (use (mk 2));;
|};
  [%expect {| true |}]
;;

let%expect_test "quoted capture of a static tuple containing a closure" =
  go
    {|
let pair = ((fn (x : int) -> x * 3), 4) @ static;;
let f = fn (y : int) -> (match pair with | (g, k) -> g y + k);;
let use = fn (static h : int -> int) -> h 2;;
let _ = print_int (use f);;
|};
  [%expect {| 10 |}]
;;

let%expect_test "quoted capture of a shadowed top-level binding" =
  go
    {|
let f = (fn (x : int) -> x + 1) @ static;;
let f = (fn (x : int) -> f x * 2) @ static;;
let use = fn (static h : int -> int) -> h 3;;
let _ = print_int (use f);;
|};
  [%expect {| 8 |}]
;;

(* pick false is never instantiated, so (lit, 2) exists only in the memo;
   the store keeps reachable monos only. *)
let%expect_test "dispatches in uninstantiated branches are not compiled" =
  go
    {|
let lit = fn (static k : int) -> k;;
fun pick (static b : bool) : int = if erased b then lit 1 else lit 2;;
let _ = print_int (pick true);;
|};
  [%expect {| 1 |}]
;;

(* Two instantiations of one lexical expression-level fun, quoted side by side
   in a single key: each quote resolves against its own instance's env. *)
let%expect_test "two instances of one lexical fun quoted in one key" =
  go
    {|
fun outer (static k : int) : static (int -> int) =
  fun helper (x : int) : int = x + k in (fn (y : int) -> helper y)
;;
let use = fn (static p : (int -> int) ^ (int -> int)) -> match p with | (a, b) -> a 10 + b 10;;
let _ = print_int (use ((outer 1), (outer 2)));;
|};
  [%expect {| 23 |}]
;;
