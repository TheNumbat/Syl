open! Core
open! Syl

let go ?print ?(check = `Run) input = Common.codegen ?print ~check input

let go_killed input =
  try go input with
  | exn -> print_s (Exn.sexp_of_t exn)
;;

(* Print the simplified user top-levels (the prelude is all [External]s), without locs. *)
let go_sst input =
  let rec strip_locs : Sexp.t -> Sexp.t = function
    | Atom _ as atom -> atom
    | List list ->
      List
        (List.filter_map list ~f:(function
           | Sexp.List (Atom "loc" :: _) -> None
           | sexp -> Some (strip_locs sexp)))
  in
  let sst =
    input |> Parse.parse_exn |> Desugar.desugar |> Typecheck.typecheck_exn |> Simplify.simplify
  in
  List.iter sst.top_levels ~f:(function
    | Sst.Top_level.External _ -> ()
    | top -> print_s (strip_locs (Sst.Top_level.sexp_of_t top)))
;;

(* Simplify beta-inlines directly-applied lambdas, splices statically-keyed binder
   monomorphizations, propagates scalar constants and prim bindings through variables, and
   folds prim applications, conditionals, and projections on constants. These tests pin
   down that the transformations fire, that they preserve evaluation order and runtime
   failures, and that the runtime shims stay covered: [lit @ dynamic] used to force runtime
   evaluation, but the folds now see through it, so runtime paths are exercised by routing
   operands through an opaque [fun] instead. *)

(* -------------------------------------------------------------------------------------
   The transformations fire: the simplified tree holds the folded constant, not the
   application.
   ------------------------------------------------------------------------------------- *)

let%expect_test "beta-inline and folding collapse a lambda application to a constant" =
  go_sst
    {|
let _ = print_int ((fn (x : int) -> x + 1) 41);;
|};
  [%expect
    {|
    (Let (var (Anon <opaque>))
     (bind
      (Apply
       (fn
        (Var (id ((Id print_int) <opaque>))
         (ty (Arrow (arg_ty Int) (ret_ty Unit)))))
       (arg (Scalar (value (Int 42)) (ty Int))) (ty Unit))))
    |}]
;;

let%expect_test "a statically-keyed binder application splices the mono, no thunk" =
  go_sst
    {|
let _ = print_int ((fn (static n : int) -> n * 2) 5);;
|};
  [%expect
    {|
    (Let (var (Anon <opaque>))
     (bind
      (Apply
       (fn
        (Var (id ((Id print_int) <opaque>))
         (ty (Arrow (arg_ty Int) (ret_ty Unit)))))
       (arg (Scalar (value (Int 10)) (ty Int))) (ty Unit))))
    |}]
;;

let%expect_test "a constant conditional's dead branch disappears" =
  go_sst
    {|
let _ = if (true @ dynamic) then print_int 1 else print_int 2;;
|};
  [%expect
    {|
    (Let (var (Anon <opaque>))
     (bind
      (Apply
       (fn
        (Var (id ((Id print_int) <opaque>))
         (ty (Arrow (arg_ty Int) (ret_ty Unit)))))
       (arg (Scalar (value (Int 1)) (ty Int))) (ty Unit))))
    |}]
;;

(* -------------------------------------------------------------------------------------
   Inlining semantics.
   ------------------------------------------------------------------------------------- *)

let%expect_test "an effectful argument is evaluated once, before the body" =
  go
    {|
let _ = print_int ((fn (x : int) -> x + x) (let u = print_int 7 in 3));;
|};
  [%expect
    {|
    7
    6
    |}]
;;

let%expect_test "beta-inline of a lambda literal with dynamic captures" =
  go
    {|
fun opaque (x : int) : dynamic int = x;;
let c = opaque 10;;
let _ = print_int ((fn (x : int) -> x + c) 5);;
|};
  [%expect {| 15 |}]
;;

let%expect_test "binder literal applied to a closure argument" =
  go
    {|
let _ = print_int ((fn (static f : int -> int) -> f 3) (fn (x : int) -> x + 1));;
|};
  [%expect {| 4 |}]
;;

let%expect_test "spliced mono body keeps its runtime part" =
  go
    {|
fun opaque (x : int) : dynamic int = x;;
let _ = print_int ((fn (static n : int) -> n + opaque n) 5);;
|};
  [%expect {| 10 |}]
;;

let%expect_test "binder mono containing a lambda application folds through" =
  go
    {|
let _ = print_int ((fn (static n : int) -> (fn (x : int) -> x * n) 3) 4);;
|};
  [%expect {| 12 |}]
;;

let%expect_test "unused pure binding is dropped, effectful binding is kept" =
  go
    {|
let _ = let u = print_int 1 in 2;;
let _ = let unused = (3 @ dynamic) in print_int 4;;
|};
  [%expect
    {|
    1
    4
    |}]
;;

(* -------------------------------------------------------------------------------------
   Constant propagation and folding semantics.
   ------------------------------------------------------------------------------------- *)

let%expect_test "constants propagate through alias chains" =
  go
    {|
let a = (2 @ dynamic);;
let b = a;;
let c = b;;
let _ = print_int (c * 3);;
|};
  [%expect {| 6 |}]
;;

let%expect_test "an aliased operator folds on constants and runs on dynamic arguments" =
  go
    {|
fun opaque (x : int) : dynamic int = x;;
let mul = ( * );;
let _ = print_int (mul (6, 7 @ dynamic));;
let _ = print_int (mul (6, opaque 7));;
|};
  [%expect
    {|
    42
    42
    |}]
;;

let%expect_test "a prim as a static function argument still dispatches at runtime" =
  go
    {|
fun opaque (x : int) : dynamic int = x;;
let apply = fn (static f : (int ^ int) -> int) -> fn (a : int) -> f (a, 2);;
let _ = print_int (apply ( + ) (opaque 1));;
|};
  [%expect {| 3 |}]
;;

let%expect_test "a constant conditional selects its branch, dead effects don't run" =
  go
    {|
let cond = (true @ dynamic);;
let _ = if cond then print_int 1 else print_int 2;;
|};
  [%expect {| 1 |}]
;;

let%expect_test "match on a constant scrutinee" =
  go
    {|
let _ = print_int (match (2 @ dynamic) { 0 -> 10, _ -> 20 });;
let _ = print_int (match ((1 @ dynamic), 2) { (a, b) -> a + b });;
|};
  [%expect
    {|
    20
    3
    |}]
;;

let%expect_test "runtime variant dispatch survives when the tag is opaque" =
  go
    {|
fun opaque (x : int) : dynamic int = x;;
let opt = variant { none, some : int };;
let x = if opaque 0 == 0 then opt.some (opaque 5) else opt.none;;
let _ = print_int (match x { .none -> 13, .some v -> v });;
|};
  [%expect {| 5 |}]
;;

(* -------------------------------------------------------------------------------------
   Failing prims never fold: they abort at runtime, exactly as before.
   ------------------------------------------------------------------------------------- *)

let%expect_test "division by a folded zero still aborts at runtime" =
  go_killed
    {|
let _ = print_int ((1 @ dynamic) / 0);;
|};
  [%expect {| ("Program killed" (signal sigabrt)) |}]
;;

let%expect_test "modulo by a folded zero still aborts at runtime" =
  go_killed
    {|
let _ = print_int ((7 @ dynamic) % 0);;
|};
  [%expect {| ("Program killed" (signal sigabrt)) |}]
;;

let%expect_test "an assert whose condition folds to false still aborts at runtime" =
  go_killed
    {|
let _ = assert ((1 @ dynamic) == 2);;
|};
  [%expect {| ("Program killed" (signal sigabrt)) |}]
;;

let%expect_test "an assert whose condition folds to true is dropped" =
  go
    {|
let _ = assert ((1 @ dynamic) == 1);;
let _ = print_int 9;;
|};
  [%expect {| 9 |}]
;;

(* -------------------------------------------------------------------------------------
   Runtime shims match compile-time semantics. Operands go through [opaque] so the
   simplifier cannot fold; expectations mirror the static results.
   ------------------------------------------------------------------------------------- *)

let%expect_test "runtime % is Euclidean, matching static %" =
  go
    {|
fun opaque (x : int) : dynamic int = x;;
let _ = print_int (opaque (-7) % opaque 2);;
let _ = print_int (opaque (-1) % opaque 3);;
let _ = print_int (opaque (-4) % opaque 3);;
|};
  [%expect
    {|
    1
    2
    2
    |}]
;;

let%expect_test "runtime wrapping at int_min matches static wrapping" =
  go
    {|
fun opaque (x : int) : dynamic int = x;;
let min = 0 - 9223372036854775807 - 1;;
let _ = print_int (opaque min / opaque (-1));;
let _ = print_int (opaque min % opaque (-1));;
let _ = print_int (opaque min * opaque (-1));;
let _ = print_int (0 - opaque min);;
|};
  [%expect
    {|
    -9223372036854775808
    0
    -9223372036854775808
    -9223372036854775808
    |}]
;;

let%expect_test "runtime comparisons and bool ops via opaque operands" =
  go
    {|
fun opaque (x : int) : dynamic int = x;;
fun opaqueb (b : bool) : dynamic bool = b;;
let min = 0 - 9223372036854775807 - 1;;
let _ = print_bool (opaque min < opaque 0);;
let _ = print_bool (opaque 1 >= opaque 2);;
let _ = print_bool (opaqueb true && opaqueb false);;
let _ = print_bool (opaqueb false || opaqueb true);;
let _ = print_bool (! (opaqueb true));;
|};
  [%expect
    {|
    true
    false
    false
    true
    false
    |}]
;;

let%expect_test "the runtime assert shim still runs on opaque conditions" =
  go
    {|
fun opaqueb (b : bool) : dynamic bool = b;;
let _ = assert (opaqueb true);;
let _ = print_int 1;;
|};
  [%expect {| 1 |}]
;;

let%expect_test "the runtime assert shim still aborts on opaque false" =
  go_killed
    {|
fun opaqueb (b : bool) : dynamic bool = b;;
let _ = assert (opaqueb false);;
|};
  [%expect {| ("Program killed" (signal sigabrt)) |}]
;;
