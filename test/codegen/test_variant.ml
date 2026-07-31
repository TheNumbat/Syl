open! Core
open! Syl

let go ?print ?(check = `Run) input = Common.codegen ?print ~check input

(* Variant values compile to [syl_variant<Size,Align>]: a [syl_int] tag (the
   constructor's label-sorted rank) followed by payload storage for the largest
   payload. Matches split on the tag and project payloads back out with
   [syl_project]; everything runs under the address and alignment
   sanitizers. *)

let%expect_test "monomorphization keyed on a variant type" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : int) -> x;;
let _ = print_int (id (variant { none, some : int }) 1);;
let _ = print_int (id (variant { none, some : bool ^ int }) 2);;
|};
  [%expect
    {|
    1
    2
    |}]
;;

let%expect_test "static constructor value" =
  go
    {|
let x = (variant { none, some : int }).some 1;;
let _ = print_int 1;;
|};
  [%expect {| 1 |}]
;;

let%expect_test "nullary constructor value" =
  go
    {|
let x = (variant { none, some : int }).none;;
let _ = print_int 2;;
|};
  [%expect {| 2 |}]
;;

let%expect_test "unit payload" =
  go
    {|
let x = (variant { none, some : unit }).some ();;
let _ = print_int 3;;
|};
  [%expect {| 3 |}]
;;

let%expect_test "dynamic payload" =
  go
    {|
let d = 4 @ dynamic;;
let x = (variant { none, some : int }).some d;;
let _ = print_int d;;
|};
  [%expect {| 4 |}]
;;

let%expect_test "first-class constructor function" =
  go
    {|
let d = 5 @ dynamic;;
let some = (variant { none, some : int }).some;;
let x = some d;;
let _ = print_int 5;;
|};
  [%expect {| 5 |}]
;;

let%expect_test "bool payload" =
  go
    {|
let d = true @ dynamic;;
let x = (variant { none, some : bool }).some d;;
let _ = print_int 6;;
|};
  [%expect {| 6 |}]
;;

(* The payload's storage must hold a padded C++ value: [int ^ bool] is 9 bytes
   packed but 16 as a [syl_tuple]. *)
let%expect_test "tuple payload from a variant family" =
  go
    {|
let pair = fn (erased t : type) -> variant { none, pair : t ^ bool };;
let d = 7 @ dynamic;;
let x = (pair int).pair (d, true);;
let _ = print_int 7;;
|};
  [%expect {| 7 |}]
;;

let%expect_test "nested constructor payload" =
  go
    {|
let inner = variant { a, b };;
let outer = variant { wrap : inner };;
let x = outer.wrap (inner.a);;
let _ = print_int 8;;
|};
  [%expect {| 8 |}]
;;

let%expect_test "variant joined through an if" =
  go
    {|
let opt = variant { none, some : int };;
let d = 9 @ dynamic;;
let x = if d == 9 then opt.some d else opt.none;;
let _ = print_int 9;;
|};
  [%expect {| 9 |}]
;;

(* A captured variant round-trips through a closure environment buffer; the
   alignment sanitizer checks the layout on the way back out. *)
let%expect_test "captured variant value" =
  go
    {|
let opt = variant { none, some : int };;
let x = opt.some (10 @ dynamic);;
fun f (y : int) : dynamic opt = x;;
let z = f 0;;
let _ = print_int 10;;
|};
  [%expect {| 10 |}]
;;

let%expect_test "match on a dynamic variant" =
  go
    {|
let opt = variant { none, some : int };;
let d = 11 @ dynamic;;
let x = opt.some d;;
let r = match x { .none -> 0, .some v -> v };;
let _ = print_int r;;
|};
  [%expect {| 11 |}]
;;

let%expect_test "runtime tag dispatch reaches both arms" =
  go
    {|
let opt = variant { none, some : int };;
let d = 12 @ dynamic;;
let x = if d == 12 then opt.some d else opt.none;;
let _ = print_int (match x { .none -> 0, .some v -> v });;
let y = if d == 0 then opt.some d else opt.none;;
let _ = print_int (match y { .none -> 13, .some v -> v });;
|};
  [%expect
    {|
    12
    13
    |}]
;;

let%expect_test "static match payload projection" =
  go
    {|
let opt = variant { none, some : int };;
let x = opt.some 14;;
let y = match static x { .none -> 0, .some v -> v };;
let _ = print_int y;;
|};
  [%expect {| 14 |}]
;;

let%expect_test "three constructors chain tag tests" =
  go
    {|
let abc = variant { a : int, b : int, c };;
let d = 15 @ dynamic;;
let x = abc.b d;;
let _ = print_int (match x { .a v -> v, .b v -> v + 1, .c -> 0 });;
|};
  [%expect {| 16 |}]
;;

let%expect_test "nested tuple pattern" =
  go
    {|
let p = variant { empty, pair : int ^ bool };;
let d = 17 @ dynamic;;
let x = p.pair (d, true);;
let r = match x { .pair (n, true) -> n, .pair (n, false) -> 0 - n, .empty -> 0 };;
let _ = print_int r;;
|};
  [%expect {| 17 |}]
;;

let%expect_test "or pattern across constructors" =
  go
    {|
let opt = variant { none, some : int };;
let d = 1 @ dynamic;;
let x = opt.some d;;
let r = match x { .none | .some _ -> 18 };;
let _ = print_int r;;
|};
  [%expect {| 18 |}]
;;

let%expect_test "nested variant pattern" =
  go
    {|
let opt = variant { none, some : int };;
let w = variant { empty, wrap : opt };;
let d = 19 @ dynamic;;
let x = w.wrap (opt.some d);;
let r = match x { .wrap (.some v) -> v, .wrap (.none) -> 0, .empty -> 1 };;
let _ = print_int r;;
|};
  [%expect {| 19 |}]
;;

let%expect_test "match under a closure" =
  go
    {|
let opt = variant { none, some : int };;
let d = 20 @ dynamic;;
let x = opt.some d;;
let f = fn (o : opt) -> match o { .none -> 0, .some v -> v };;
let _ = print_int (f x);;
|};
  [%expect {| 20 |}]
;;

let%expect_test "zero-size payload pattern" =
  go
    {|
let u = variant { empty, mark : unit };;
let d = () @ dynamic;;
let x = u.mark d;;
let r = match x { .mark _ -> 21, .empty -> 0 };;
let _ = print_int r;;
|};
  [%expect {| 21 |}]
;;

(* Tags are label-sorted ranks, independent of source order. *)
let%expect_test "scrambled label order" =
  go
    {|
let t = variant { zebra, apple : int, mango : bool };;
let d = 24 @ dynamic;;
let f = fn (v : t) -> match v { .zebra -> 100, .mango b -> if b then 1 else 2, .apple n -> n };;
let _ = print_int (f (t.apple d));;
let _ = print_int (f (t.mango (true @ dynamic)));;
let _ = print_int (f (t.zebra));;
|};
  [%expect
    {|
    24
    1
    100
    |}]
;;

let%expect_test "literal payload patterns" =
  go
    {|
let t = variant { num : int, flag : bool };;
let f = fn (v : t) -> match v { .num 1 -> 10, .num n -> n, .flag true -> 20, .flag false -> 30 };;
let _ = print_int (f (t.num (5 @ dynamic)));;
let _ = print_int (f (t.num (1 @ dynamic)));;
let _ = print_int (f (t.flag (true @ dynamic)));;
let _ = print_int (f (t.flag (false @ dynamic)));;
|};
  [%expect
    {|
    5
    10
    20
    30
    |}]
;;

let%expect_test "tuple of variants splits on both columns" =
  go
    {|
let ab = variant { x, y };;
let mk = fn (b : bool) -> if b then ab.x else ab.y;;
let f = fn (p : ab ^ ab) -> match p { (.x, .x) -> 1, (.x, .y) -> 2, (.y, _) -> 3 };;
let _ = print_int (f (mk (true @ dynamic), mk (true @ dynamic)));;
let _ = print_int (f (mk (true @ dynamic), mk (false @ dynamic)));;
let _ = print_int (f (mk (false @ dynamic), mk (true @ dynamic)));;
|};
  [%expect
    {|
    1
    2
    3
    |}]
;;

let%expect_test "variant inside a tuple scrutinee" =
  go
    {|
let opt = variant { none, some : int };;
let d = 7 @ dynamic;;
let r = match (opt.some d, 2) { (.some v, n) -> v + n, (.none, n) -> n };;
let _ = print_int r;;
|};
  [%expect {| 9 |}]
;;

let%expect_test "or pattern binds the payload from either constructor" =
  go
    {|
let dir = variant { left : int, right : int };;
let f = fn (v : dir) -> match v { .left n | .right n -> n };;
let _ = print_int (f (dir.left (31 @ dynamic)));;
let _ = print_int (f (dir.right (32 @ dynamic)));;
|};
  [%expect
    {|
    31
    32
    |}]
;;

(* A closure lives in the payload buffer and is projected back out. *)
let%expect_test "dynamic closure payload" =
  go
    {|
let fv = variant { nothing, func : int -> dynamic int };;
let base = 1 @ dynamic;;
let g = fn (x : int) -> x + base;;
let v = fv.func g;;
let _ = print_int (match v { .func f -> f 41, .nothing -> 0 });;
|};
  [%expect {| 42 |}]
;;

(* A static closure payload is dispatched directly to its family's proc. *)
let%expect_test "static closure payload" =
  go
    {|
let fv = variant { nothing, func : int -> int };;
let v = fv.func (fn (x : int) -> x + 1);;
let _ = print_int (match v { .func f -> f 41, .nothing -> 0 });;
|};
  [%expect {| 42 |}]
;;

(* A polymorphic function stored in a payload: the payload is the binder's
   environment buffer, and applying the projection specializes it. *)
let%expect_test "polymorphic payload" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let pv = variant { nothing, poly : static erased type \ t -> t -> t };;
let v = pv.poly id;;
let _ = print_int (match v { .poly f -> f int (43 @ dynamic), .nothing -> 0 });;
|};
  [%expect {| 43 |}]
;;

(* All-nullary variants have no payload storage but still traverse tuples and
   environment buffers. *)
let%expect_test "nullary variant in a captured tuple" =
  go
    {|
let e = variant { lo, hi };;
let d = true @ dynamic;;
let x = if d then e.lo else e.hi;;
let p = (x, 5);;
let f = fn (u : unit) -> p;;
let r = match f () { (.lo, n) -> n, (.hi, n) -> n + 1 };;
let _ = print_int r;;
|};
  [%expect {| 5 |}]
;;

let%expect_test "variant captured by a recursive group" =
  go
    {|
let opt = variant { none, some : int };;
let x = opt.some (5 @ dynamic);;
fun bounce (n : int) : dynamic int = if n == 0 then finish 0 else bounce (n - 1)
and finish (u : int) : dynamic int = match x { .some v -> v + u, .none -> 0 };;
let _ = print_int (bounce 3);;
|};
  [%expect {| 5 |}]
;;

let%expect_test "generic function instantiated at a variant type" =
  go
    {|
let opt = variant { none, some : int };;
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = print_int (match id opt (opt.some (9 @ dynamic)) { .some v -> v, .none -> 0 });;
|};
  [%expect {| 9 |}]
;;

(* The instance rebinds its static variant argument as a literal, so the mono
   builds the constructor at runtime and projects from it. *)
let%expect_test "static variant argument end to end" =
  go
    {|
let opt = variant { none, some : int };;
fun get (static o : opt) : int = match static o { .none -> 0, .some v -> v };;
let _ = print_int (get (opt.some 3));;
let _ = print_int (get (opt.none));;
|};
  [%expect
    {|
    3
    0
    |}]
;;

let%expect_test "nested variant sharing a label" =
  go
    {|
let inner = variant { some : int, other };;
let outer = variant { some : inner, none };;
let d = 21 @ dynamic;;
let x = outer.some (inner.some d);;
let r = match x { .some (.some v) -> v, .some (.other) -> 0, .none -> 1 };;
let _ = print_int r;;
|};
  [%expect {| 21 |}]
;;

let%expect_test "static match refinement over tuples and rebinds" =
  go
    {|
let opt = variant { none, some : int };;
let a = opt.some 1;;
let b = opt.none;;
let r = match static (a, b) { (.some v, .none) -> v, (_, _) -> 0 };;
let _ = print_int r;;
fun probe (static o : opt) : int =
  match static o { .some v -> match static o { .some w -> w + v, .none -> 0 }, .none -> 0 }
;;
let _ = print_int (probe (opt.some 20));;
|};
  [%expect
    {|
    1
    40
    |}]
;;

(* [vec 0] is a single-constructor variant with no payload storage; its match
   needs no tag test. *)
let%expect_test "empty vector from the indexed family" =
  go
    {|
fun vec (static n : int) : erased type =
  match erased n { 0 -> variant { nil }, _ -> variant { cons : int ^ vec (n - 1) } }
;;
fun replicate (static n : int) : int -> vec n =
  fn (x : int) ->
    match erased n {
      0 -> (vec n).nil,
      _ -> (variant { cons : int ^ vec (n - 1) }).cons (x, replicate (n - 1) x),
    }
;;
let z = replicate 0 44;;
let _ = print_int (match z { .nil -> 99 });;
|};
  [%expect {| 99 |}]
;;

(* Mixed payload sizes round-trip through closure environments under the
   alignment sanitizer. *)
let%expect_test "mixed payload sizes through environments" =
  go
    {|
let inner = variant { deep : int, no };;
let big = variant { one : bool, three : int ^ bool ^ int, wrap : inner };;
let d = 3 @ dynamic;;
let x = big.three (d, true, d + 1);;
let y = big.wrap (inner.deep d);;
let fx = fn (u : unit) -> x;;
let fy = fn (u : unit) -> y;;
let get = fn (v : big) -> match v { .three (a, b, c) -> a + c, .one f -> 0, .wrap (.deep n) -> n, .wrap (.no) -> 100 };;
let _ = print_int (get (fx ()));;
let _ = print_int (get (fy ()));;
|};
  [%expect
    {|
    7
    3
    |}]
;;

let%expect_test "unit literal payload pattern" =
  go
    {|
let u = variant { mark : unit, num : int };;
let x = u.mark (() @ dynamic);;
let y = u.num (6 @ dynamic);;
let f = fn (v : u) -> match v { .mark () -> 1, .num n -> n };;
let _ = print_int (f x);;
let _ = print_int (f y);;
|};
  [%expect
    {|
    1
    6
    |}]
;;

let%expect_test "constructor function through a higher-order function" =
  go
    {|
let opt = variant { none, some : int };;
let apply = fn (g : int -> opt) -> fn (x : int) -> g x;;
let r = apply (opt.some) 7;;
let _ = print_int (match r { .some v -> v, .none -> 0 });;
|};
  [%expect {| 7 |}]
;;

(* Each expanded or-alternative materializes its own binding paths. Mode
   annotations bind tighter than commas, so tuple elements annotate in
   place. *)
let%expect_test "or pattern with swapped tuple bindings" =
  go
    {|
let t = variant { a : int ^ bool, b : bool ^ int };;
let f = fn (v : t) -> match v { .a (n, x) | .b (x, n) -> if x then n else n + 1 };;
let _ = print_int (f (t.a (7, true @ dynamic)));;
let _ = print_int (f (t.b (false @ dynamic, 9)));;
|};
  [%expect
    {|
    7
    10
    |}]
;;

(* The instance's key is a constructor holding a closure; reification rebuilds
   the injection around a quoted lambda. *)
let%expect_test "closure payload as a mono key" =
  go
    {|
let fopt = variant { none, some : int -> int };;
fun call (static o : fopt) : dynamic int = match static o { .none -> 0, .some f -> f 1 };;
let _ = print_int (call (fopt.some (fn (x : int) -> x + 41)));;
|};
  [%expect {| 42 |}]
;;

let%expect_test "curried family end to end" =
  go
    {|
let pair = fn (static erased a : type) -> fn (static erased b : type) -> variant { fst : a, snd : b };;
fun mk (static erased t : type) : t -> pair t bool = fn (x : t) -> (pair t bool).fst x;;
let _ = print_int (match mk int 5 { .fst n -> n, .snd _ -> 0 });;
|};
  [%expect {| 5 |}]
;;

let%expect_test "partially applied family alias end to end" =
  go
    {|
let pair = fn (static erased a : type) -> fn (static erased b : type) -> variant { fst : a, snd : b };;
let pair2 = fn (static erased a : type) -> pair a;;
fun mk (static erased t : type) : t -> pair2 t bool = fn (x : t) -> (pair2 t bool).fst x;;
let _ = print_int (match mk int 5 { .fst n -> n, .snd _ -> 0 });;
|};
  [%expect {| 5 |}]
;;

(* One branch types at the unfolded row, the other at the stuck application;
   the joined type drives both branches' code. *)
let%expect_test "curried family joined through an if" =
  go
    {|
let pair = fn (static erased a : type) -> fn (static erased b : type) -> variant { fst : a, snd : b };;
fun mk (static erased t : type) : t -> pair t bool = fn (x : t) -> (pair t bool).fst x;;
fun pick (static erased t : type) : bool -> t -> pair t bool =
  fn (c : bool) -> fn (x : t) -> if c then (pair t bool).fst x else mk t x
;;
let _ = print_int (match pick int (true @ dynamic) 7 { .fst n -> n, .snd _ -> 0 });;
let _ = print_int (match pick int (false @ dynamic) 8 { .fst n -> n, .snd _ -> 0 });;
|};
  [%expect
    {|
    7
    8
    |}]
;;

let%expect_test "closure inside a tuple payload" =
  go
    {|
let base = 2 @ dynamic;;
let t = variant { empty, call : (int -> dynamic int) ^ int };;
let g = fn (y : int) -> y * base;;
let x = t.call (g, 23);;
let r = match x { .call (f, n) -> f n, .empty -> 0 };;
let _ = print_int r;;
|};
  [%expect {| 46 |}]
;;

let%expect_test "match scrutinizing another match" =
  go
    {|
let opt = variant { none, some : int };;
let d = 8 @ dynamic;;
let first = match opt.some d { .some v -> opt.some (v + 1), .none -> opt.none };;
let _ = print_int (match first { .some v -> v, .none -> 0 });;
|};
  [%expect {| 9 |}]
;;

let%expect_test "indexed variant family end to end" =
  go
    {|
fun vec (static n : int) : erased type =
  match erased n { 0 -> variant { nil }, _ -> variant { cons : int ^ vec (n - 1) } }
;;
fun replicate (static n : int) : int -> vec n =
  fn (x : int) ->
    match erased n {
      0 -> (vec n).nil,
      _ -> (variant { cons : int ^ vec (n - 1) }).cons (x, replicate (n - 1) x),
    }
;;
let sum3 = fn (v : vec 3) -> match v { .cons (a, .cons (b, .cons (c, .nil))) -> a + b + c };;
let _ = print_int (sum3 (replicate 3 22));;
|};
  [%expect {| 66 |}]
;;
