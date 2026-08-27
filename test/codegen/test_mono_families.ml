open! Core
open! Syl

let go ?print ?(check = `Run) input = Common.codegen ?print ~check input

(* Adversarial pins on the mono-family invariant: reify unions monos across a
   family and keeps one body per key ([State.collect_monos]), so two instances
   that disagree on a body must never share a (family, key). Each test takes a
   route by which they could. *)

let%expect_test "same inner key under two outer keys: frames split the families" =
  (* The inner binder's body shape depends on the captured outer key, and both
     instances specialize it at the same keys. If the outer application did not
     push a frame, both would share one family and f2 would splice f1's bodies:
     10 20 10 20. *)
  go
    {|
let mk = fn (static n : int) -> fn (static b : bool) ->
  if erased n == 1 then (if erased b then 10 else 20) else (if erased b then 30 else 40);;
let f1 = mk 1;;
let f2 = mk 2;;
let _ = print_int (f1 true);;
let _ = print_int (f1 false);;
let _ = print_int (f2 true);;
let _ = print_int (f2 false);;
|};
  [%expect
    {|
    10
    20
    30
    40
    |}]
;;

let%expect_test "parametric capture cannot reach an erased decision" =
  (* The closure route: applying [mk] re-reduces its body per static argument
     without pushing a frame, so an inner binder would keep one family across
     arguments whose captures differ. The mode system seals the route: a
     parametric argument is not static, so no body shape can depend on it. *)
  Common.typecheck
    {|
let mk = fn (n : int) -> fn (static b : bool) -> if erased n == 1 then 10 else 20;;
let _ = print_int ((mk 1) true);;
|};
  [%expect
    {|
    ((loc ((line 2) (column 49)))
     (reason
      (Mode_mismatch (got ((staticity Parametric) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "stuck recursive type applications reduce unframed: colliding monos stay dead" =
  (* Inside [tup k] the recursive [tup (k - 1)] is erased and active, so it
     sticks; the annotation's leq later whnf-reduces it at top level with no
     frame, landing [elt]'s monos from different [n] in one ambient family
     (alongside the declaration pass's symbolic record). Instance memoization is
     per uid, so the value world stays correct; the colliding family must never
     reach reified code. *)
  go
    {|
fun tup (static n : int) : erased type =
  let elt = fn (static u : unit) -> if erased n == 1 then bool else int in
  if erased n == 0 then unit else elt () ^ tup (n - 1);;
let a = (true, ()) : tup 1;;
let b = (7, (true, ())) : tup 2;;
let c = (8, (9, (true, ()))) : tup 3;;
let _ = match b { (i, _) -> print_int i };;
let _ = match c { (i, (j, _)) -> let _ = print_int i in print_int j };;
|};
  [%expect
    {|
    7
    8
    9
    |}]
;;
