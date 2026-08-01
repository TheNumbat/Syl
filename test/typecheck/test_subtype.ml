open! Core
open! Syl

let go = Common.typecheck

let%expect_test "weaken mode: static unerased -> static erased (literal substitution)" =
  go
    {|
let _ = 1 @ erased;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken mode: dynamic unerased -> dynamic erased (erased marker)" =
  go
    {|
let x = 1 @ dynamic;;
let _ = x @ erased;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 10)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "weaken mode: static -> dynamic (staticity only)" =
  go
    {|
let _ = (fn (x : int) -> x) @ dynamic;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken type: arrow ret_mode covariant" =
  go
    {|
let f = fn (x : int) -> x;;
let _ = f : int -> erased int;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken type: arrow arg_mode contravariant" =
  go
    {|
let f = fn (erased x : int) -> 1;;
let _ = f : int -> int;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 10)))
     (reason
      (Type_mismatch
       (got
        (Type
         (Pi (arg_ty (Type Int)) (arg_mode ((staticity Static) (erasure Erased)))
          (ret_ty (T (ty (Type Int)) (memo <opaque>)))
          (ret_mode ((staticity Static) (erasure Unerased))))))
       (need
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret_ty (Type Int))
          (ret_mode ((staticity Static) (erasure Unerased)))))))))
    |}]
;;

let%expect_test "weaken if non-split: mode erasure on branch" =
  go
    {|
let _ = if true then 1 else 1 @ erased;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken if non-split: arrow type join" =
  go
    {|
let _ = if true then fn (erased x : int) -> 1 else fn (x : int) -> 1;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken if split: mode only" =
  go
    {|
let f = fn (static x : int) -> if erased x == 0 then 1 else 1 @ erased;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken binder apply: body weakened to ret_mode" =
  go
    {|
let f = fn (static x : int) -> x;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken arrow closure apply erased: body weakened" =
  go
    {|
let f = fn (x : int) -> x;;
let _ = (f @ erased) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken pi closure apply erased: both axes" =
  go
    {|
let f = fn (x : int) -> 1;;
let g = fn (static erased x : int) -> x;;
let _ = ((if true then f else g) @ erased) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken mode: both axes (static unerased -> dynamic erased)" =
  go
    {|
let _ = 1 @ dynamic erased;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken if non-split: staticity on branch" =
  go
    {|
let x = 1 @ dynamic;;
let _ = if true then 1 else x;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken if non-split: both axes on branch" =
  go
    {|
let x = 1 @ dynamic;;
let _ = if true then 1 else x @ erased;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 30)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "weaken if split: staticity on branch" =
  go
    {|
let f = fn (static b : bool) -> if erased b then 1 @ dynamic else 1;;
let _ = f false;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken if split: both axes on branch" =
  go
    {|
let f = fn (static b : bool) -> if erased b then 1 @ dynamic erased else 1;;
let _ = f false;;
let _ = f true;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Erased_application
       (fn
        (Type
         (Pi (arg_ty (Type Bool))
          (arg_mode ((staticity Static) (erasure Unerased)))
          (ret_ty (T (ty (Type Int)) (memo <opaque>)))
          (ret_mode ((staticity Dynamic) (erasure Erased))))))
       (result ((staticity Dynamic) (erasure Erased))))))
    |}]
;;

let%expect_test "weaken binder apply: erasure on body" =
  go
    {|
let f = fn (static x : int) -> if erased x == 0 then 1 else 1 @ erased;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken arrow closure apply erased: staticity on body" =
  go
    {|
let f = fn (x : int) -> 1;;
let _ = (f @ erased) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken pi closure apply erased: erasure only" =
  go
    {|
let f = fn (x : int) -> x;;
let g = fn (static erased x : int) -> x;;
let _ = ((if true then f else g) @ erased) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken pi closure apply erased: staticity only" =
  go
    {|
let f = fn (x : int) -> 1;;
let g = fn (static x : int) -> x;;
let _ = ((if true then f else g) @ erased) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "closure to closure: arg erasure contravariant" =
  go
    {|
let apply = fn (f : int -> int) -> f 0;;
let g = fn (erased x : int) -> 1;;
let _ = apply g;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason
      (Type_mismatch
       (got
        (Type
         (Pi (arg_ty (Type Int)) (arg_mode ((staticity Static) (erasure Erased)))
          (ret_ty (T (ty (Type Int)) (memo <opaque>)))
          (ret_mode ((staticity Static) (erasure Unerased))))))
       (need
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret_ty (Type Int))
          (ret_mode ((staticity Static) (erasure Unerased)))))))))
    |}]
;;

let%expect_test "closure to closure: ret erasure covariant" =
  go
    {|
let apply = fn (f : int -> erased int) -> f 0;;
let g = fn (x : int) -> x;;
let _ = apply g;;
|};
  [%expect {| |}]
;;

let%expect_test "closure to closure: both arg and ret subtyping" =
  go
    {|
let apply = fn (f : int -> erased int) -> f 0;;
let g = fn (erased x : int) -> 1;;
let _ = apply g;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason
      (Type_mismatch
       (got
        (Type
         (Pi (arg_ty (Type Int)) (arg_mode ((staticity Static) (erasure Erased)))
          (ret_ty (T (ty (Type Int)) (memo <opaque>)))
          (ret_mode ((staticity Static) (erasure Unerased))))))
       (need
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret_ty (Type Int))
          (ret_mode ((staticity Static) (erasure Erased)))))))))
    |}]
;;

let%expect_test "closure where binder expected: Arrow leq Pi" =
  go
    {|
let apply = fn (static f : static int -> int) -> f 0;;
let g = fn (x : int) -> x;;
let _ = apply g;;
|};
  [%expect {| |}]
;;

let%expect_test "binder to binder: arg erasure contravariant" =
  go
    {|
let apply = fn (static f : static int -> int) -> f 0;;
let g = fn (static erased x : int) -> 1;;
let _ = apply g;;
|};
  [%expect {| |}]
;;

let%expect_test "erased closure taking closure arg returning dynamic" =
  go
    {|
let apply = fn (f : int -> dynamic int) -> f 0;;
let _ = (apply @ erased) (fn (x : int) -> x);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Erased_application
       (fn
        (Type
         (Arrow
          (arg_ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret_ty (Type Int))
          (ret_mode ((staticity Dynamic) (erasure Unerased))))))
       (result ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "erased closure taking closure arg" =
  go
    {|
let apply = fn (f : int -> int) -> f 0;;
let _ = (apply @ erased) (fn (x : int) -> x);;
|};
  [%expect {| |}]
;;

let%expect_test "binder taking closure, applied erased inside" =
  go
    {|
let apply = fn (static f : static int -> erased int) -> (f @ erased) 0;;
let g = fn (x : int) -> 1;;
let _ = apply g;;
|};
  [%expect {| |}]
;;

let%expect_test "leq Pi/Pi: ret_mode covariant" =
  go
    {|
let f = fn (static x : int) -> 1;;
let _ = f : static int -> erased int;;
|};
  [%expect {| |}]
;;

let%expect_test "leq Pi/Pi: arg_mode contravariant" =
  go
    {|
let f = fn (static erased x : int) -> 1;;
let _ = f : static int -> int;;
|};
  [%expect {| |}]
;;

let%expect_test "leq Pi/Pi: fails when ret_mode wrong direction" =
  go
    {|
let f = fn (static x : int) -> 1 @ erased;;
let _ = f : static int -> int;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 10)))
     (reason
      (Type_mismatch
       (got
        (Type
         (Pi (arg_ty (Type Int))
          (arg_mode ((staticity Static) (erasure Unerased)))
          (ret_ty (T (ty (Type Int)) (memo <opaque>)))
          (ret_mode ((staticity Static) (erasure Erased))))))
       (need
        (Type
         (Pi (arg_ty (Type Int))
          (arg_mode ((staticity Static) (erasure Unerased)))
          (ret_ty (T (ty (Type Int)) (memo <opaque>)))
          (ret_mode ((staticity Static) (erasure Unerased)))))))))
    |}]
;;

let%expect_test "leq Arrow/Pi: closure where Pi expected with mode variance" =
  go
    {|
let apply = fn (static f : static int -> erased int) -> f 0;;
let g = fn (x : int) -> x;;
let _ = apply g;;
|};
  [%expect {| |}]
;;

let%expect_test "join Pi/Pi: different ret_mode" =
  go
    {|
let f = fn (static x : int) -> 1;;
let g = fn (static x : int) -> 1 @ erased;;
let _ = if true then f else g;;
|};
  [%expect {| |}]
;;

let%expect_test "join Pi/Pi: different arg_mode" =
  go
    {|
let f = fn (static x : int) -> 1;;
let g = fn (static erased x : int) -> 1;;
let _ = if true then f else g;;
|};
  [%expect {| |}]
;;

let%expect_test "join Arrow/Pi" =
  go
    {|
let f = fn (x : int) -> 1;;
let g = fn (static x : int) -> 1;;
let _ = if true then f else g;;
|};
  [%expect {| |}]
;;

let%expect_test "join Pi/Arrow" =
  go
    {|
let f = fn (static x : int) -> 1;;
let g = fn (x : int) -> 1;;
let _ = if true then f else g;;
|};
  [%expect {| |}]
;;

let%expect_test "meet Pi/Pi: via arg contravariance in join" =
  go
    {|
let f = fn (static g : static int -> int) -> g 0;;
let h = fn (static g : static erased int -> int) -> g 0;;
let _ = if true then f else h;;
|};
  [%expect {| |}]
;;

let%expect_test "apply joined Pi/Pi" =
  go
    {|
let f = fn (static x : int) -> 1;;
let g = fn (static x : int) -> 1 @ erased;;
let _ = (if true then f else g) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "leq Pi/Pi dependent return: ret_mode covariant" =
  go
    {|
let id = fn (static t : type) -> fn (x : t) -> x;;
let _ = id : static type \ t -> erased t -> t;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 11)))
     (reason
      (Type_mismatch
       (got
        (Type
         (Pi (arg_ty (Type Type))
          (arg_mode ((staticity Static) (erasure Unerased)))
          (ret_ty
           (Typecheck (env <opaque>) (arg ((Id t) <opaque>)) (arg_ty (Type Type))
            (arg_mode ((staticity Static) (erasure Unerased))) (memo <opaque>)
            (body
             (Lambda (arg ((Id x) <opaque>))
              (arg_mode ((staticity ()) (erasure ())))
              (arg_ty (Var (id ((Id t) <opaque>)) (loc ((line 2) (column 41)))))
              (body (Var (id ((Id x) <opaque>)) (loc ((line 2) (column 47)))))
              (loc ((line 2) (column 33)))))))
          (ret_mode ((staticity Static) (erasure Unerased))))))
       (need
        (Type
         (Pi (arg_ty (Type Type))
          (arg_mode ((staticity Static) (erasure Unerased)))
          (ret_ty
           (Reduce (env <opaque>) (arg ((Id t) <opaque>)) (arg_ty (Type Type))
            (arg_mode ((staticity Static) (erasure Unerased))) (memo <opaque>)
            (ret_ty
             (Arrow
              (arg (Var (id ((Id t) <opaque>)) (loc ((line 3) (column 39)))))
              (arg_id (Anon <opaque>))
              (arg_mode ((staticity ()) (erasure (Erased))))
              (ret (Var (id ((Id t) <opaque>)) (loc ((line 3) (column 44)))))
              (ret_mode ((staticity ()) (erasure ())))
              (loc ((line 3) (column 20)))))))
          (ret_mode ((staticity Static) (erasure Unerased)))))))))
    |}]
;;

let%expect_test "leq Pi/Pi dependent return: ret_mode covariant" =
  go
    {|
let id = fn (static t : type) -> fn (x : t) -> x;;
let _ = id : static type \ t -> erased (t -> t);;
|};
  [%expect {| |}]
;;

let%expect_test "join Pi/Pi dependent return: different ret_mode" =
  go
    {|
let f = fn (static t : type) -> fn (x : t) -> x;;
let g = fn (static t : type) -> (fn (x : t) -> x) @ erased;;
let _ = if true then f else g;;
|};
  [%expect {| |}]
;;

let%expect_test "join Pi/Pi dependent return: different arg_mode" =
  go
    {|
let f = fn (static t : type) -> fn (x : t) -> x;;
let g = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = if true then f else g;;
|};
  [%expect {| |}]
;;

let%expect_test "apply joined dependent Pi/Pi" =
  go
    {|
let f = fn (static erased t : type) -> fn (x : t) -> x;;
let g = fn (static erased t : type) -> (fn (x : t) -> x) @ erased;;
let _ = (if true then f else g) int;;
|};
  [%expect {| |}]
;;

let%expect_test "leq Pi/Pi function-type arg: arg contravariant" =
  go
    {|
let f = fn (static g : static int -> int) -> g 0;;
let _ = f : static (static erased int -> int) -> int;;
|};
  [%expect {| |}]
;;

let%expect_test "leq Pi/Pi function-type arg: arg contravariant" =
  go
    {|
let f = fn (static g : static type \ t -> t) -> ();;
let _ = f : static (static erased type \ t -> t) -> unit;;
|};
  [%expect {| |}]
;;

let%expect_test "leq Pi/Pi function-type ret" =
  go
    {|
let f = fn (static g : static type \ t -> erased t) -> ();;
let _ = f : static (static erased type \ t -> dynamic t) -> unit;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 10)))
     (reason
      (Type_mismatch
       (got
        (Type
         (Pi
          (arg_ty
           (Type
            (Pi (arg_ty (Type Type))
             (arg_mode ((staticity Static) (erasure Unerased)))
             (ret_ty
              (Reduce (env <opaque>) (arg ((Id t) <opaque>)) (arg_ty (Type Type))
               (arg_mode ((staticity Static) (erasure Unerased))) (memo <opaque>)
               (ret_ty (Var (id ((Id t) <opaque>)) (loc ((line 2) (column 49)))))))
             (ret_mode ((staticity Static) (erasure Erased))))))
          (arg_mode ((staticity Static) (erasure Unerased)))
          (ret_ty (T (ty (Type Unit)) (memo <opaque>)))
          (ret_mode ((staticity Static) (erasure Unerased))))))
       (need
        (Type
         (Pi
          (arg_ty
           (Type
            (Pi (arg_ty (Type Type))
             (arg_mode ((staticity Static) (erasure Erased)))
             (ret_ty
              (Reduce (env <opaque>) (arg ((Id t) <opaque>)) (arg_ty (Type Type))
               (arg_mode ((staticity Static) (erasure Erased))) (memo <opaque>)
               (ret_ty (Var (id ((Id t) <opaque>)) (loc ((line 3) (column 54)))))))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (arg_mode ((staticity Static) (erasure Unerased)))
          (ret_ty (T (ty (Type Unit)) (memo <opaque>)))
          (ret_mode ((staticity Static) (erasure Unerased)))))))))
    |}]
;;

let%expect_test "join Pi/Pi function-type arg: different inner arg_mode" =
  go
    {|
let f = fn (static g : static int -> int) -> g 0;;
let h = fn (static g : static erased int -> int) -> g 0;;
let _ = if true then f else h;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent return via function-type arg returning type" =
  go
    {|
let wrap = fn (static f : static int -> static type) -> fn (x : f 0) -> x;;
|};
  [%expect {| |}]
;;

let%expect_test "leq Pi/Pi function-type arg returning type" =
  go
    {|
let wrap = fn (static f : static int -> static type) -> fn (x : f 0) -> x;;
let _ = wrap : static (static int -> static type) \ f -> f 0 -> f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "meet Pi/Pi function-type arg: via arg contravariance in join" =
  go
    {|
let f = fn (static apply : static (static int -> int) -> int) -> apply (fn (static x : int) -> 0);;
let g = fn (static apply : static (static erased int -> int) -> int) -> apply (fn (static erased x : int) -> 0);;
let _ = if true then f else g;;
|};
  [%expect {| |}]
;;

let%expect_test "join Pi/Pi function-type arg returning type: fresh var issue" =
  go
    {|
let f = fn (static g : static int -> static type) -> fn (x : g 0) -> x;;
let h = fn (static g : static int -> static type) -> fn (x : g 0) -> x;;
let _ = if true then f else h;;
|};
  [%expect {| |}]
;;

let%expect_test "join unfolds an abstract type-function application" =
  go
    {|
fun id (static erased t : type) : erased type = t;;
fun f (static erased t : type) : t -> bool -> t =
  fn (x : id t) -> fn (b : bool) -> if b then x else (x : t);;
let _ = f int 5 true;;
|};
  [%expect {| |}]
;;

let%expect_test "meet unfolds an abstract type-function application (arrow arg in join)" =
  go
    {|
fun id (static erased t : type) : erased type = t;;
fun f (static erased t : type) : t -> t =
  fn (y : t) -> (if true then fn (v : id t) -> y else fn (v : t) -> y) y;;
let _ = f int 5;;
|};
  [%expect {| |}]
;;

let%expect_test "geq unfolds an abstract type-function application (arg contravariance)" =
  go
    {|
fun id (static erased t : type) : erased type = t;;
fun f (static erased t : type) : (id t -> int) -> t -> int =
  fn (g : id t -> int) -> (g : t -> int);;
let _ = f int (fn (x : int) -> x) 5;;
|};
  [%expect {| |}]
;;

let%expect_test "leq If/If with distinct conds falls through to branch comparison" =
  go
    {|
let f = fn (static a : bool) -> fn (static b : bool) ->
  fn (x : (if erased a then int else int)) -> (x : (if erased b then int else int));;
let _ = f true false 1;;
|};
  [%expect {| |}]
;;

let%expect_test "join If/If with distinct conds falls through to branch collapse" =
  go
    {|
let f = fn (static a : bool) -> fn (static b : bool) ->
  fn (x : (if erased a then int else int)) -> fn (y : (if erased b then int else int)) ->
    if true then x else y;;
let _ = f true false 1 2;;
|};
  [%expect {| |}]
;;

let%expect_test
    "meet If/If with distinct conds falls through to branch collapse (arrow arg in join)"
  =
  go
    {|
let f = fn (static a : bool) -> fn (static b : bool) ->
  fn (g : (if erased a then int else int) -> int) ->
    fn (h : (if erased b then int else int) -> int) ->
      if true then g else h;;
let _ = f true false (fn (v : int) -> v) (fn (v : int) -> v);;
|};
  [%expect {| |}]
;;

(* A two-argument spine [g 0 1] with a variable head cannot unfold; comparing it against a stuck
   [if] must fall back to arm-wise comparison, as it already does for the one-argument [g 0]. *)
let%expect_test "leq If vs stuck spine falls back to arm comparison when unfold fails" =
  go
    {|
let f = fn (static erased g : static int -> erased (static int -> erased type)) -> fn (static a : bool) ->
  fn (x : (if erased a then g 0 1 else g 0 1)) -> (x : g 0 1);;
let _ = f (fn (static u : int) -> fn (static v : int) -> int) true 1;;
|};
  [%expect {| |}]
;;

let%expect_test "join If vs stuck spine falls back to arm collapse when unfold fails" =
  go
    {|
let f = fn (static erased g : static int -> erased (static int -> erased type)) -> fn (static a : bool) ->
  fn (x : (if erased a then g 0 1 else g 0 1)) -> fn (y : g 0 1) ->
    if true then x else y;;
let _ = f (fn (static u : int) -> fn (static v : int) -> int) true 1 2;;
|};
  [%expect {| |}]
;;

let%expect_test
    "meet If vs stuck spine falls back to arm collapse when unfold fails (arrow arg in join)"
  =
  go
    {|
let f = fn (static erased g : static int -> erased (static int -> erased type)) -> fn (static a : bool) ->
  fn (h : (if erased a then g 0 1 else g 0 1) -> int) ->
    fn (k : g 0 1 -> int) ->
      if true then h else k;;
let _ = f (fn (static u : int) -> fn (static v : int) -> int) true (fn (v : int) -> v) (fn (v : int) -> v);;
|};
  [%expect {| |}]
;;

(* Applying a stuck [if] of type functions distributes the pending argument into the arms. *)
let%expect_test "leq distributes a pending application into stuck-match arms" =
  go
    {|
fun id (static erased t : type) : erased type = t;;
fun id2 (static erased t : type) : erased type = t;;
let f = fn (static a : bool) ->
  fn (x : (if erased a then id else id2) int) -> (x : int);;
let _ = f true 1;;
|};
  [%expect {| |}]
;;

let%expect_test "leq distributes a pending two-argument spine into stuck-match arms" =
  go
    {|
let k = fn (static erased t : type) -> fn (static erased u : type) -> u;;
let f = fn (static a : bool) ->
  fn (x : (if erased a then k else k) bool int) -> (x : int);;
let _ = f true 1;;
|};
  [%expect {| |}]
;;

let%expect_test "join distributes a pending application into stuck-match arms" =
  go
    {|
fun id (static erased t : type) : erased type = t;;
fun id2 (static erased t : type) : erased type = t;;
let f = fn (static a : bool) ->
  fn (x : (if erased a then id else id2) int) -> fn (y : int) ->
    if true then x else y;;
let _ = f true 1 2;;
|};
  [%expect {| |}]
;;

let%expect_test
    "meet distributes a pending application into stuck-match arms (arrow arg in join)"
  =
  go
    {|
fun id (static erased t : type) : erased type = t;;
fun id2 (static erased t : type) : erased type = t;;
let f = fn (static a : bool) ->
  fn (h : (if erased a then id else id2) int -> int) -> fn (k : int -> int) ->
    if true then h else k;;
let _ = f true (fn (v : int) -> v) (fn (v : int) -> v);;
|};
  [%expect {| |}]
;;

(* Applying a stuck [if] of closures distributes into [Apply { fn = Closure _; _ }] arms, which
   must unfold like binder-headed applications. *)
let%expect_test "leq unfolds a closure-headed application from stuck-match arms" =
  go
    {|
let c = fn (x : int) -> int;;
let c2 = fn (x : int) -> int;;
let f = fn (static a : bool) ->
  fn (x : (if erased a then c else c2) 5) -> (x : int);;
let _ = f true 1;;
|};
  [%expect {| |}]
;;

let%expect_test "join unfolds a closure-headed application from stuck-match arms" =
  go
    {|
let c = fn (x : int) -> int;;
let c2 = fn (x : int) -> int;;
let f = fn (static a : bool) ->
  fn (x : (if erased a then c else c2) 5) -> fn (y : int) ->
    if true then x else y;;
let _ = f true 1 2;;
|};
  [%expect {| |}]
;;

let%expect_test
    "meet unfolds a closure-headed application from stuck-match arms (arrow arg in join)"
  =
  go
    {|
let c = fn (x : int) -> int;;
let c2 = fn (x : int) -> int;;
let f = fn (static a : bool) ->
  fn (h : (if erased a then c else c2) 5 -> int) -> fn (k : int -> int) ->
    if true then h else k;;
let _ = f true (fn (v : int) -> v) (fn (v : int) -> v);;
|};
  [%expect {| |}]
;;

(* Projections and payload extractions are eliminators like applications: comparisons unfold
   their subjects and distribute them into stuck-match arms. *)
let%expect_test "leq unfolds a projection of a stuck application" =
  go
    {|
fun pair (static erased t : type) : erased (type ^ type) = (t, t);;
fun f (static erased t : type) : t -> t =
  match erased (pair t) { (u, v) -> fn (x : u) -> (x : t) };;
let _ = f int 5;;
|};
  [%expect {| |}]
;;

let%expect_test "leq distributes a projection into stuck-match arms" =
  go
    {|
let f = fn (static a : bool) ->
  match erased (if erased a then (int, bool) else (int, unit)) { (u, v) -> fn (x : u) -> (x : int) };;
let _ = f true 1;;
|};
  [%expect {| |}]
;;

let%expect_test "join distributes a projection into stuck-match arms" =
  go
    {|
let f = fn (static a : bool) ->
  match erased (if erased a then (int, bool) else (int, unit)) { (u, v) ->
    fn (x : u) -> fn (y : int) -> if true then x else y };;
let _ = f true 1 2;;
|};
  [%expect {| |}]
;;

let%expect_test "meet distributes a projection into stuck-match arms (arrow arg in join)" =
  go
    {|
let f = fn (static a : bool) ->
  match erased (if erased a then (int, bool) else (int, unit)) { (u, v) ->
    fn (h : u -> int) -> fn (k : int -> int) -> if true then h else k };;
let _ = f true (fn (w : int) -> w) (fn (w : int) -> w);;
|};
  [%expect {| |}]
;;

let%expect_test "leq unfolds an application headed by a projection" =
  go
    {|
let p = ((fn (static erased u : type) -> u), (fn (static erased u : type) -> u));;
let f = fn (static a : bool) ->
  match erased (if erased a then p else p) { (g, h) -> fn (x : g int) -> (x : int) };;
let _ = f true 1;;
|};
  [%expect {| |}]
;;

(* [refine]: decomposing a stuck match assumes the arm's pattern for both sides, so
   correlated conditionals compare arm-by-arm even when structurally misaligned. *)
let%expect_test "leq refines correlated conditionals across an arrow" =
  go
    {|
let f = fn (static s : bool) ->
  fn (x : (if erased s then int else bool) -> int) ->
    (x : if erased s then (int -> int) else (bool -> int));;
let _ = f true (fn (v : int) -> v);;
|};
  [%expect {| |}]
;;
