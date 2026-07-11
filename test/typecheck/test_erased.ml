open! Core
open! Syl

let go ?print input = Common.typecheck ?print input

(* The erased-application rule: applying an erased function requires a static
   result and taints the result with erasure (application stays in the erased
   world). Applications with dynamic results are errors -- the body never
   runs, so the result must be computable at compile time. Arguments are not
   relaxed: an erased argument still needs a parameter that accepts erased.
   These pin the accept/reject boundary. *)

let%expect_test "accept: erased application with static result, result is erased" =
  go
    {|
let square = fn (x : int) -> x * x;;
let ghost = square @ erased;;
let _ = ghost 5;;
|};
  [%expect {| |}]
;;

let%expect_test "accept: prim computation through an erased binding" =
  go
    {|
let ghost_neg = (fn (x : int) -> 0 - x) @ erased;;
let _ = ghost_neg 1;;
|};
  [%expect {| |}]
;;

let%expect_test "reject: the tainted result cannot be consumed unerased" =
  go
    {|
let square = fn (x : int) -> x * x;;
let ghost = square @ erased;;
let _ = print_int (ghost 5);;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "reject: no argument relaxation, ghost results do not compose directly" =
  go
    {|
let square = fn (x : int) -> x * x;;
let ghost = square @ erased;;
let _ = ghost (ghost 2);;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "accept: tainted result feeds an erased parameter" =
  go
    {|
let square = fn (x : int) -> x * x;;
let ghost = square @ erased;;
let id_at = fn (static erased n : int) -> fn (x : int) -> x;;
let _ = id_at (ghost 4) 1;;
|};
  [%expect {| |}]
;;

let%expect_test "reject: erased application with a dynamic argument" =
  go
    {|
let square = fn (x : int) -> x * x;;
let ghost = square @ erased;;
let d = 2 @ dynamic;;
let _ = ghost d;;
|};
  [%expect
    {|
    ((loc ((line 5) (column 8)))
     (reason
      (Erased_application
       (fn
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret_ty (Type Int))
          (ret_mode ((staticity Static) (erasure Unerased))))))
       (result ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "reject: erased fun with dynamic result applied" =
  go
    {|
fun erased f (x : int) : dynamic int = x;;
let _ = f 0;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
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

let%expect_test "reject: external applied through an erased binding" =
  go
    {|
let ghost_print = print_int @ erased;;
let _ = ghost_print 0;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Erased_application
       (fn
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased)))
          (ret_ty (Type Unit))
          (ret_mode ((staticity Dynamic) (erasure Unerased))))))
       (result ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "reject: erased parameter with dynamic-result type applied" =
  go
    {|
let use = fn (static erased f : static int -> dynamic int) -> f 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 62)))
     (reason
      (Erased_application
       (fn
        (Type
         (Pi (arg_ty (Type Int))
          (arg_mode ((staticity Static) (erasure Unerased)))
          (ret_ty (T (ty (Type Int)) (memo <opaque>)))
          (ret_mode ((staticity Dynamic) (erasure Unerased))))))
       (result ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "accept: erased parameter with static-result type applied" =
  go
    {|
let use = fn (static erased f : static int -> int) -> f 0;;
let g = fn (static x : int) -> x;;
let _ = use g;;
|};
  [%expect {| |}]
;;

let%expect_test "reject: applying an erased result whose type has a dynamic return" =
  go
    {|
let cap = fn (static erased f : static int -> dynamic int) -> f;;
let g = fn (static x : int) -> x;;
let _ = (cap g) 5;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason
      (Erased_application
       (fn
        (Type
         (Pi (arg_ty (Type Int))
          (arg_mode ((staticity Static) (erasure Unerased)))
          (ret_ty (T (ty (Type Int)) (memo <opaque>)))
          (ret_mode ((staticity Dynamic) (erasure Unerased))))))
       (result ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "reject: dynamic data cannot be erased" =
  go
    {|
let d = 0 @ dynamic;;
let _ = d @ erased;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 10)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "reject: arrow type cannot promise an erased return for a dynamic body" =
  go
    {|
let f = fn (x : int) -> print_int x;;
let _ = f : int -> erased unit;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 10)))
     (reason
      (Type_mismatch
       (got
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased)))
          (ret_ty (Type Unit))
          (ret_mode ((staticity Dynamic) (erasure Unerased))))))
       (need
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased)))
          (ret_ty (Type Unit)) (ret_mode ((staticity Static) (erasure Erased)))))))))
    |}]
;;

let%expect_test "accept: erased application consumed as a type annotation" =
  go
    {|
let pick = fn (static erased b : bool) -> if erased b then int else bool;;
let _ = 0 : pick true;;
let _ = false : pick false;;
|};
  [%expect {| |}]
;;

let%expect_test "accept: erased result bound and consumed erased" =
  go
    {|
let cap = fn (static erased f : static int -> dynamic int) -> f;;
let g = fn (static x : int) -> x;;
let h = cap g;;
let _ = h @ static erased;;
|};
  [%expect {| |}]
;;

let%expect_test "accept: erased recursion with erased results" =
  go
    {|
fun erased count (static x : int) : erased int =
  if erased x == 0 then 0 @ erased else count (x - 1)
;;
let _ = count 3;;
|};
  [%expect {| |}]
;;

let%expect_test "accept: erased application as a static erased argument" =
  go
    {|
fun erased depth (static u : unit) : erased int = 2;;
let id_at = fn (static erased n : int) -> fn (x : int) -> x;;
let _ = id_at (depth ()) 5;;
|};
  [%expect {| |}]
;;

(* [assert erased] is the erased-world checkpoint: its argument mode is
   already erased, so ghost results can be asserted directly, and the assert
   forces even though its result is unused. *)
let%expect_test "accept: assert erased checks a ghost condition" =
  go
    {|
let ghost_pos = (fn (static x : int) -> x > 0) @ erased;;
let _ = assert erased (ghost_pos 3);;
|};
  [%expect {| |}]
;;

let%expect_test "reject: assert erased fires on a false ghost condition" =
  go
    {|
let ghost_pos = (fn (static x : int) -> x > 0) @ erased;;
let _ = assert erased (ghost_pos 0);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason (Static_failure (Assert_failed (Bool (T false))))))
    |}]
;;

let%expect_test "accept: prim applied through an erased binding stays in the erased world" =
  go
    {|
builtin myadd = syl_int_add;;
let ghost_add = myadd @ erased;;
let _ = ghost_add (1, 2);;
|};
  [%expect {| |}]
;;

(* An in-flight recursive static demanded as a type goes abstract and fails
   the annotation check instead of crashing. *)
let%expect_test "reject: recursive static demanded as a type" =
  go
    {|
fun f (u : unit) : static type = let _ = 0 : f () in int;;
let _ = f ();;
|};
  [%expect
    {|
    ((loc ((line 2) (column 43)))
     (reason (Type_mismatch (got (Type Int)) (need (Var (Anon <opaque>))))))
    |}]
;;

(* [erased] with unannotated staticity implies static, so the parameter binds
   in the return type just like [static erased]. *)
let%expect_test "accept: erased fun parameter scopes over the return type" =
  go
    {|
fun f (erased t : type) : t -> t = fn (v : t) -> v;;
let _ = f int 1;;
|};
  [%expect {| |}]
;;
