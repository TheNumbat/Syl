open! Core
open! Syl

let go ?(print = false) input =
  let cst = Parse.parse_exn input in
  match Typecheck.typecheck cst with
  | Ok tst -> if print then print_s [%message (tst : Tst.Program.t)]
  | Error { loc; reason } -> print_s [%message (loc : Lex.Location.t) (reason : Typecheck.Error.t)]
;;

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
  [%expect {| |}]
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
  [%expect {| |}]
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
let f = fn (static x : int) -> if static x == 0 then 1 else 1 @ erased;;
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
  [%expect {| |}]
;;

let%expect_test "weaken if split: staticity on branch" =
  go
    {|
let f = fn (static b : bool) -> if static b then 1 @ dynamic else 1;;
let _ = f false;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken if split: both axes on branch" =
  go
    {|
let f = fn (static b : bool) -> if static b then 1 @ dynamic erased else 1;;
let _ = f false;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken binder apply: erasure on body" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else 1 @ erased;;
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
  [%expect {| |}]
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
  [%expect {| |}]
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
          (ret_ty (T (Type Int)))
          (ret_mode ((staticity Static) (erasure Erased))))))
       (need
        (Type
         (Pi (arg_ty (Type Int))
          (arg_mode ((staticity Static) (erasure Unerased)))
          (ret_ty (T (Type Int)))
          (ret_mode ((staticity Dynamic) (erasure Unerased)))))))))
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
           (Typecheck (env <opaque>) (arg (Id t)) (arg_ty (Type Type))
            (arg_mode ((staticity Static) (erasure Unerased))) (memo <opaque>)
            (body
             (Lambda (arg (Id x)) (erased Unerased)
              (arg_mode ((staticity ()) (erasure ())))
              (arg_ty (Var (id (Id t)) (loc ((line 2) (column 41)))))
              (body (Var (id (Id x)) (loc ((line 2) (column 47)))))
              (loc ((line 2) (column 33)))))))
          (ret_mode ((staticity Static) (erasure Unerased))))))
       (need
        (Type
         (Pi (arg_ty (Type Type))
          (arg_mode ((staticity Static) (erasure Unerased)))
          (ret_ty
           (Reduce (env <opaque>) (arg (Id t)) (arg_ty (Type Type))
            (arg_mode ((staticity Static) (erasure Unerased))) (memo <opaque>)
            (ret_ty
             (Arrow (arg (Var (id (Id t)) (loc ((line 3) (column 39)))))
              (arg_id ()) (arg_mode ((staticity ()) (erasure (Erased))))
              (ret (Var (id (Id t)) (loc ((line 3) (column 44)))))
              (ret_mode ((staticity ()) (erasure ())))
              (loc ((line 3) (column 41)))))))
          (ret_mode ((staticity Dynamic) (erasure Unerased)))))))))
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
let _ = f : static (static erased type \ t -> t) -> unit;;
|};
  [%expect {| |}]
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
