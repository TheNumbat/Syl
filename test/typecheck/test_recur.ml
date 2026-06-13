open! Core
open! Syl

let go = Common.typecheck

let%expect_test "pi function calling arrow function in same group" =
  go
    {|
fun inc (x : int) : int = x + 1
and f (static erased t : type) : t -> t = fn (x : t) -> x;;
let _ = f int 0;;
|};
  [%expect {| |}]
;;

let%expect_test "arrow function calling pi function in same group" =
  go
    {|
fun f (static erased t : type) : t -> t = fn (x : t) -> x
and g (x : int) : int = f int x;;
let _ = g 5;;
|};
  [%expect {| |}]
;;

let%expect_test "pi calling arrow from same group at application" =
  go
    {|
fun inc (x : int) : int = x + 1
and choose (static erased b : bool) : int -> int =
  if erased b then fn (x : int) -> inc x else fn (x : int) -> x;;
let _ = choose true 5;;
let _ = choose false 5;;
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
fun f (static x : int) : int = if erased x == 0 then 42 else f (x - 1);;
let _ = f 3;;
|};
  [%expect {| |}]
;;

let%expect_test "recursive erased application" =
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

let%expect_test "same-key static self-recursion hits the limit" =
  go
    {|
fun f (static x : int) : int = f x;;
let _ = f 0;;
|};
  [%expect {| ((loc ((line 2) (column 31))) (reason (Recursion_limit 1000))) |}]
;;

let%expect_test "same-key static self-recursion, erased result" =
  go
    {|
fun f (static x : int) : erased int = f x;;
let _ = f 0;;
|};
  [%expect {| ((loc ((line 2) (column 38))) (reason (Recursion_limit 1000))) |}]
;;

let%expect_test "recursive erased application" =
  go
    {|
fun f (x : int) : erased int = f x;;
let _ = f 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 31)))
     (reason
      (Erased_application
       (fn
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret_ty (Type Int))
          (ret_mode ((staticity Static) (erasure Erased))))))
       (result ((staticity Parametric) (erasure Erased))))))
    |}]
;;

let%expect_test "recursive erased application" =
  go
    {|
fun f (x : int) : int = g x
and erased g (y : int) : int = y;;
let _ = f 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 24)))
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

let%expect_test "recursive erased application" =
  go
    {|
fun f (x : int) : int = (g @ erased) x
and g (y : int) : int = y;;
let _ = f 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 24)))
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

let%expect_test "recursive erased application" =
  go
    {|
fun f (x : int) : erased int = g x
and g (y : int) : erased int = y;;
let _ = f 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 31)))
     (reason
      (Erased_application
       (fn
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret_ty (Type Int))
          (ret_mode ((staticity Static) (erasure Erased))))))
       (result ((staticity Parametric) (erasure Erased))))))
    |}]
;;

let%expect_test "recursive erased application" =
  go
    {|
fun f (static x : int) : erased int = g x
and g (static y : int) : erased int = y;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "recursive erased application" =
  go
    {|
fun f (static x : int) : erased int = g x
and g (y : int) : erased int = y;;
let _ = f 0;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 4)))
     (reason
      (Mode_mismatch (got ((staticity Parametric) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "recursive erased application" =
  go
    {|
fun erased f (x : int) : int = (g @ erased) x
and g (y : int) : int = y;;
let _ = f 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 31)))
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

let%expect_test "recursive erased application" =
  go
    {|
fun f (x : int) : int = g x
and g (y : int) : int = f y;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "recursive erased application" =
  go
    {|
fun erased f (x : int) : int = g x
and erased g (y : int) : int = f y;;
let _ = f 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 31)))
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

let%expect_test "recursive erased application" =
  go
    {|
fun f (x : int) : int = g x
and erased g (y : int) : int = f y;;
let _ = (f @ erased) 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 24)))
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

let%expect_test "recursive erased application" =
  go
    {|
fun f (x : int) : int = g x
and g (y : int) : int = (f @ erased) y;;
let _ = f 0;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 24)))
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

let%expect_test "recursive erased application" =
  go
    {|
fun f (x : int) : int = (g @ erased) x
and g (y : int) : int = (f @ erased) y;;
let _ = f 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 24)))
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

let%expect_test "recursive erased application" =
  go
    {|
fun f (x : int) : int = (g @ erased) x
and g (y : int) : int = (f @ erased) y;;
let _ = f 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 24)))
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

let%expect_test "recursive erased application" =
  go
    {|
fun f (x : int) : erased int = g x @ erased
and g (y : int) : erased int = (f @ erased) y;;
let _ = f 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 31)))
     (reason
      (Erased_application
       (fn
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret_ty (Type Int))
          (ret_mode ((staticity Static) (erasure Erased))))))
       (result ((staticity Parametric) (erasure Erased))))))
    |}]
;;

let%expect_test "recursive erased application" =
  go
    {|
fun f (x : int) : erased int = g x
and g (y : int) : int = let _ = f y in 0;;
let _ = g 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 4)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

(* [h]'s static argument demands the static value of [f 0] while [f] is still
   being defined; the in-flight static is treated as abstract, so the body's
   result is dynamic and fails [f]'s declared static return. *)
let%expect_test "recursive static demanded during its own definition" =
  go
    {|
fun f (x : int) : static int = h (f 0)
and h (static z : int) : int = z;;
let _ = f 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 4)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "erased fn" =
  go
    {|
fun erased f (x : int) : int = 0;;
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

let%expect_test "erased fn" =
  go
    {|
let f = fn erased (x : int) -> 0;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "erased fn" =
  go
    {|
let f = fn erased (x : int) -> x @ erased;;
let _ = f 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 33)))
     (reason
      (Mode_mismatch (got ((staticity Parametric) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

(* TODO do we want both if erased and if erased? *)
(* The in-flight cond goes abstract; both branch types are int, so the if
   resolves to a concrete type and the definition is accepted (calling it
   diverges at runtime, like any non-terminating recursion). *)
let%expect_test "erased-if on a recursive static defines" =
  go
    {|
fun f (x : int) : static int = if erased (f 0 == 0) then 0 else 1;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "cyclic type function in an annotation hits the limit" =
  go
    {|
fun t (static n : int) : erased type = t n;;
fun f (static n : int) : t n -> int = fn (v : int) -> v;;
|};
  [%expect {| ((loc ((line 2) (column 39))) (reason (Recursion_limit 1000))) |}]
;;

let%expect_test "cyclic type function forced by an annotation hits the limit" =
  go
    {|
fun t (static n : int) : erased type = t n;;
let _ = 0 : t 0;;
|};
  [%expect {| ((loc ((line 2) (column 39))) (reason (Recursion_limit 1000))) |}]
;;

let%expect_test "unbounded different-key static recursion hits the limit" =
  go
    {|
fun f (static x : int) : int = f (x + 1);;
let _ = f 0;;
|};
  [%expect {| ((loc ((line 2) (column 34))) (reason (Recursion_limit 1000))) |}]
;;
