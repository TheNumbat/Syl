open! Core
open! Syl

let go input =
  let cst = Parse.parse_exn input in
  match Typecheck.typecheck cst with
  | Ok tst -> print_s [%message (tst : Tst.Program.t)]
  | Error { loc; reason } -> print_s [%message (loc : Lex.Location.t) (reason : Typecheck.Error.t)]
;;

let%expect_test "weaken mode: static unerased -> static erased (literal substitution)" =
  go
    {|
let _ = 1 @ erased;;
|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Static) (erasure Erased)))
         (loc ((line 2) (column 10)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "weaken mode: dynamic unerased -> dynamic erased (erased marker)" =
  go
    {|
let x = 1 @ dynamic;;
let _ = x @ erased;;
|};
  [%expect
    {|
    (tst
     ((Let (var x)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Erased (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
         (loc ((line 3) (column 10)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "weaken mode: static -> dynamic (staticity only)" =
  go
    {|
let _ = (fn (x : int) -> x) @ dynamic;;
|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (Lambda (arg x)
         (body
          (Var (id x) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Unerased)))
           (loc ((line 2) (column 25)))))
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 2) (column 9)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "weaken type: arrow ret_mode covariant" =
  go
    {|
let f = fn (x : int) -> x;;
let _ = f : int -> erased int;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Lambda (arg x)
         (body
          (Var (id x) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Unerased)))
           (loc ((line 2) (column 24)))))
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Var (id f)
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Erased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "weaken type: arrow arg_mode contravariant" =
  go
    {|
let f = fn (erased x : int) -> 1;;
let _ = f : int -> int;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Lambda (arg x)
         (body
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 2) (column 31)))))
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Erased))) (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Var (id f)
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "weaken if non-split: mode erasure on branch" =
  go
    {|
let _ = if true then 1 else 1 @ erased;;
|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (If
         (cond
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 2) (column 11)))))
         (then_
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 2) (column 21)))))
         (else_
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Erased)))
           (loc ((line 2) (column 30)))))
         (ty (Type Int)) (mode ((staticity Static) (erasure Erased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "weaken if non-split: arrow type join" =
  go
    {|
let _ = if true then fn (erased x : int) -> 1 else fn (x : int) -> 1;;
|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (If
         (cond
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 2) (column 11)))))
         (then_
          (Lambda (arg x)
           (body
            (Literal (value (Int (T 1))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased)))
             (loc ((line 2) (column 44)))))
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Erased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 2) (column 21)))))
         (else_
          (Lambda (arg x)
           (body
            (Literal (value (Int (T 1))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased)))
             (loc ((line 2) (column 67)))))
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 2) (column 51)))))
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "weaken if split: mode only" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else 1 @ erased;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body
          (If
           (cond
            (Binop (op Eq) (lhs (Var (id x) (loc ((line 2) (column 41)))))
             (rhs (Literal (value (Int 0)) (loc ((line 2) (column 46)))))
             (loc ((line 2) (column 43)))))
           (then_ (Literal (value (Int 1)) (loc ((line 2) (column 53)))))
           (else_
            (Mode_annotation
             (expr (Literal (value (Int 1)) (loc ((line 2) (column 60)))))
             (mode ((staticity ()) (erasure (Erased))))
             (loc ((line 2) (column 62)))))
           (static true) (loc ((line 2) (column 31)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (Typecheck (env <opaque>) (arg x) (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased))) (memo <opaque>)
              (body
               (If
                (cond
                 (Binop (op Eq) (lhs (Var (id x) (loc ((line 2) (column 41)))))
                  (rhs (Literal (value (Int 0)) (loc ((line 2) (column 46)))))
                  (loc ((line 2) (column 43)))))
                (then_ (Literal (value (Int 1)) (loc ((line 2) (column 53)))))
                (else_
                 (Mode_annotation
                  (expr (Literal (value (Int 1)) (loc ((line 2) (column 60)))))
                  (mode ((staticity ()) (erasure (Erased))))
                  (loc ((line 2) (column 62)))))
                (static true) (loc ((line 2) (column 31)))))))
            (ret_mode ((staticity Static) (erasure Erased))))))
         (mode ((staticity Static) (erasure Erased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "weaken binder apply: body weakened to ret_mode" =
  go
    {|
let f = fn (static x : int) -> x;;
let _ = f 0;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x) (body (Var (id x) (loc ((line 2) (column 31)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn
          (Var (id f)
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Int)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 8)))))
         (key (Int 0)) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "weaken arrow closure apply erased: body weakened" =
  go
    {|
let f = fn (x : int) -> x;;
let _ = (f @ erased) 0;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Lambda (arg x)
         (body
          (Var (id x) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Unerased)))
           (loc ((line 2) (column 24)))))
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Let (var x)
         (bind
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 21)))))
         (rest
          (Var (id x) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Unerased)))
           (loc ((line 2) (column 24)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "weaken pi closure apply erased: both axes" =
  go
    {|
let f = fn (x : int) -> 1;;
let g = fn (static erased x : int) -> x;;
let _ = ((if true then f else g) @ erased) 0;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Lambda (arg x)
         (body
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 2) (column 24)))))
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Binder (arg x) (body (Var (id x) (loc ((line 3) (column 38)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Erased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Static) (erasure Erased))))))
         (mode ((staticity Static) (erasure Erased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Let (var x)
         (bind
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 43)))))
         (rest
          (Erased (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
           (loc ((line 4) (column 8)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "weaken mode: both axes (static unerased -> dynamic erased)" =
  go
    {|
let _ = 1 @ dynamic erased;;
|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (Erased (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
         (loc ((line 2) (column 10)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "weaken if non-split: staticity on branch" =
  go
    {|
let x = 1 @ dynamic;;
let _ = if true then 1 else x;;
|};
  [%expect
    {|
    (tst
     ((Let (var x)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (If
         (cond
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 11)))))
         (then_
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 21)))))
         (else_
          (Var (id x) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Unerased)))
           (loc ((line 3) (column 28)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "weaken if non-split: both axes on branch" =
  go
    {|
let x = 1 @ dynamic;;
let _ = if true then 1 else x @ erased;;
|};
  [%expect
    {|
    (tst
     ((Let (var x)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (If
         (cond
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 11)))))
         (then_
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 21)))))
         (else_
          (Erased (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
           (loc ((line 3) (column 30)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "weaken if split: staticity on branch" =
  go
    {|
let f = fn (static b : bool) -> if static b then 1 @ dynamic else 1;;
let _ = f false;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg b)
         (body
          (If (cond (Var (id b) (loc ((line 2) (column 42)))))
           (then_
            (Mode_annotation
             (expr (Literal (value (Int 1)) (loc ((line 2) (column 49)))))
             (mode ((staticity (Dynamic)) (erasure ())))
             (loc ((line 2) (column 51)))))
           (else_ (Literal (value (Int 1)) (loc ((line 2) (column 66)))))
           (static true) (loc ((line 2) (column 32)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Bool))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (Typecheck (env <opaque>) (arg b) (arg_ty (Type Bool))
              (arg_mode ((staticity Static) (erasure Unerased))) (memo <opaque>)
              (body
               (If (cond (Var (id b) (loc ((line 2) (column 42)))))
                (then_
                 (Mode_annotation
                  (expr (Literal (value (Int 1)) (loc ((line 2) (column 49)))))
                  (mode ((staticity (Dynamic)) (erasure ())))
                  (loc ((line 2) (column 51)))))
                (else_ (Literal (value (Int 1)) (loc ((line 2) (column 66)))))
                (static true) (loc ((line 2) (column 32)))))))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn
          (Var (id f)
           (ty
            (Type
             (Pi (arg_ty (Type Bool))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty
               (Typecheck (env <opaque>) (arg b) (arg_ty (Type Bool))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (memo <opaque>)
                (body
                 (If (cond (Var (id b) (loc ((line 2) (column 42)))))
                  (then_
                   (Mode_annotation
                    (expr (Literal (value (Int 1)) (loc ((line 2) (column 49)))))
                    (mode ((staticity (Dynamic)) (erasure ())))
                    (loc ((line 2) (column 51)))))
                  (else_ (Literal (value (Int 1)) (loc ((line 2) (column 66)))))
                  (static true) (loc ((line 2) (column 32)))))))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 8)))))
         (key (Bool false)) (ty (Type Int))
         (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "weaken if split: both axes on branch" =
  go
    {|
let f = fn (static b : bool) -> if static b then 1 @ dynamic erased else 1;;
let _ = f false;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg b)
         (body
          (If (cond (Var (id b) (loc ((line 2) (column 42)))))
           (then_
            (Mode_annotation
             (expr (Literal (value (Int 1)) (loc ((line 2) (column 49)))))
             (mode ((staticity (Dynamic)) (erasure (Erased))))
             (loc ((line 2) (column 51)))))
           (else_ (Literal (value (Int 1)) (loc ((line 2) (column 73)))))
           (static true) (loc ((line 2) (column 32)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Bool))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (Typecheck (env <opaque>) (arg b) (arg_ty (Type Bool))
              (arg_mode ((staticity Static) (erasure Unerased))) (memo <opaque>)
              (body
               (If (cond (Var (id b) (loc ((line 2) (column 42)))))
                (then_
                 (Mode_annotation
                  (expr (Literal (value (Int 1)) (loc ((line 2) (column 49)))))
                  (mode ((staticity (Dynamic)) (erasure (Erased))))
                  (loc ((line 2) (column 51)))))
                (else_ (Literal (value (Int 1)) (loc ((line 2) (column 73)))))
                (static true) (loc ((line 2) (column 32)))))))
            (ret_mode ((staticity Dynamic) (erasure Erased))))))
         (mode ((staticity Static) (erasure Erased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Let (var b)
         (bind
          (Literal (value (Bool (T false))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 10)))))
         (rest
          (Erased (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
           (loc ((line 3) (column 8)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "weaken binder apply: erasure on body" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else 1 @ erased;;
let _ = f 0;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body
          (If
           (cond
            (Binop (op Eq) (lhs (Var (id x) (loc ((line 2) (column 41)))))
             (rhs (Literal (value (Int 0)) (loc ((line 2) (column 46)))))
             (loc ((line 2) (column 43)))))
           (then_ (Literal (value (Int 1)) (loc ((line 2) (column 53)))))
           (else_
            (Mode_annotation
             (expr (Literal (value (Int 1)) (loc ((line 2) (column 60)))))
             (mode ((staticity ()) (erasure (Erased))))
             (loc ((line 2) (column 62)))))
           (static true) (loc ((line 2) (column 31)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (Typecheck (env <opaque>) (arg x) (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased))) (memo <opaque>)
              (body
               (If
                (cond
                 (Binop (op Eq) (lhs (Var (id x) (loc ((line 2) (column 41)))))
                  (rhs (Literal (value (Int 0)) (loc ((line 2) (column 46)))))
                  (loc ((line 2) (column 43)))))
                (then_ (Literal (value (Int 1)) (loc ((line 2) (column 53)))))
                (else_
                 (Mode_annotation
                  (expr (Literal (value (Int 1)) (loc ((line 2) (column 60)))))
                  (mode ((staticity ()) (erasure (Erased))))
                  (loc ((line 2) (column 62)))))
                (static true) (loc ((line 2) (column 31)))))))
            (ret_mode ((staticity Static) (erasure Erased))))))
         (mode ((staticity Static) (erasure Erased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Let (var x)
         (bind
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 10)))))
         (rest
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Erased)))
           (loc ((line 3) (column 8)))))
         (ty (Type Int)) (mode ((staticity Static) (erasure Erased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "weaken arrow closure apply erased: staticity on body" =
  go
    {|
let f = fn (x : int) -> 1;;
let _ = (f @ erased) 0;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Lambda (arg x)
         (body
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 2) (column 24)))))
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Let (var x)
         (bind
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 21)))))
         (rest
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Unerased)))
           (loc ((line 2) (column 24)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "weaken pi closure apply erased: erasure only" =
  go
    {|
let f = fn (x : int) -> x;;
let g = fn (static erased x : int) -> x;;
let _ = ((if true then f else g) @ erased) 0;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Lambda (arg x)
         (body
          (Var (id x) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Unerased)))
           (loc ((line 2) (column 24)))))
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Binder (arg x) (body (Var (id x) (loc ((line 3) (column 38)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Erased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Static) (erasure Erased))))))
         (mode ((staticity Static) (erasure Erased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Let (var x)
         (bind
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 43)))))
         (rest
          (Erased (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
           (loc ((line 4) (column 8)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "weaken pi closure apply erased: staticity only" =
  go
    {|
let f = fn (x : int) -> 1;;
let g = fn (static x : int) -> x;;
let _ = ((if true then f else g) @ erased) 0;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Lambda (arg x)
         (body
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 2) (column 24)))))
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Binder (arg x) (body (Var (id x) (loc ((line 3) (column 31)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Let (var x)
         (bind
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 43)))))
         (rest
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Unerased)))
           (loc ((line 2) (column 24)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
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
    (tst
     ((Let (var apply)
       (bind
        (Lambda (arg f)
         (body
          (Apply
           (fn
            (Var (id f)
             (ty
              (Type
               (Arrow (arg_ty (Type Int))
                (arg_mode ((staticity Dynamic) (erasure Unerased)))
                (ret_ty (Type Int))
                (ret_mode ((staticity Dynamic) (erasure Unerased))))))
             (mode ((staticity Dynamic) (erasure Unerased)))
             (loc ((line 2) (column 35)))))
           (arg
            (Literal (value (Int (T 0))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased)))
             (loc ((line 2) (column 37)))))
           (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
           (loc ((line 2) (column 35)))))
         (ty
          (Type
           (Arrow
            (arg_ty
             (Type
              (Arrow (arg_ty (Type Int))
               (arg_mode ((staticity Dynamic) (erasure Unerased)))
               (ret_ty (Type Int))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 12)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Lambda (arg x)
         (body
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 31)))))
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Erased))) (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id apply)
           (ty
            (Type
             (Arrow
              (arg_ty
               (Type
                (Arrow (arg_ty (Type Int))
                 (arg_mode ((staticity Dynamic) (erasure Unerased)))
                 (ret_ty (Type Int))
                 (ret_mode ((staticity Dynamic) (erasure Unerased))))))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 8)))))
         (arg
          (Var (id g)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Erased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 14)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "closure to closure: ret erasure covariant" =
  go
    {|
let apply = fn (f : int -> erased int) -> f 0;;
let g = fn (x : int) -> x;;
let _ = apply g;;
|};
  [%expect
    {|
    (tst
     ((Let (var apply)
       (bind
        (Lambda (arg f)
         (body
          (Apply
           (fn
            (Var (id f)
             (ty
              (Type
               (Arrow (arg_ty (Type Int))
                (arg_mode ((staticity Dynamic) (erasure Unerased)))
                (ret_ty (Type Int))
                (ret_mode ((staticity Dynamic) (erasure Erased))))))
             (mode ((staticity Dynamic) (erasure Unerased)))
             (loc ((line 2) (column 42)))))
           (arg
            (Literal (value (Int (T 0))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased)))
             (loc ((line 2) (column 44)))))
           (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
           (loc ((line 2) (column 42)))))
         (ty
          (Type
           (Arrow
            (arg_ty
             (Type
              (Arrow (arg_ty (Type Int))
               (arg_mode ((staticity Dynamic) (erasure Unerased)))
               (ret_ty (Type Int))
               (ret_mode ((staticity Dynamic) (erasure Erased))))))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Erased))))))
         (mode ((staticity Static) (erasure Erased)))
         (loc ((line 2) (column 12)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Lambda (arg x)
         (body
          (Var (id x) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Unerased)))
           (loc ((line 3) (column 24)))))
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Let (var f)
         (bind
          (Var (id g)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 14)))))
         (rest
          (Apply
           (fn
            (Var (id f)
             (ty
              (Type
               (Arrow (arg_ty (Type Int))
                (arg_mode ((staticity Dynamic) (erasure Unerased)))
                (ret_ty (Type Int))
                (ret_mode ((staticity Dynamic) (erasure Erased))))))
             (mode ((staticity Dynamic) (erasure Unerased)))
             (loc ((line 2) (column 42)))))
           (arg
            (Literal (value (Int (T 0))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased)))
             (loc ((line 2) (column 44)))))
           (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
           (loc ((line 2) (column 42)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
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
    (tst
     ((Let (var apply)
       (bind
        (Lambda (arg f)
         (body
          (Apply
           (fn
            (Var (id f)
             (ty
              (Type
               (Arrow (arg_ty (Type Int))
                (arg_mode ((staticity Dynamic) (erasure Unerased)))
                (ret_ty (Type Int))
                (ret_mode ((staticity Dynamic) (erasure Erased))))))
             (mode ((staticity Dynamic) (erasure Unerased)))
             (loc ((line 2) (column 42)))))
           (arg
            (Literal (value (Int (T 0))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased)))
             (loc ((line 2) (column 44)))))
           (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
           (loc ((line 2) (column 42)))))
         (ty
          (Type
           (Arrow
            (arg_ty
             (Type
              (Arrow (arg_ty (Type Int))
               (arg_mode ((staticity Dynamic) (erasure Unerased)))
               (ret_ty (Type Int))
               (ret_mode ((staticity Dynamic) (erasure Erased))))))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Erased))))))
         (mode ((staticity Static) (erasure Erased)))
         (loc ((line 2) (column 12)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Lambda (arg x)
         (body
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 31)))))
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Erased))) (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Let (var f)
         (bind
          (Var (id g)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Erased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 14)))))
         (rest
          (Apply
           (fn
            (Var (id f)
             (ty
              (Type
               (Arrow (arg_ty (Type Int))
                (arg_mode ((staticity Dynamic) (erasure Unerased)))
                (ret_ty (Type Int))
                (ret_mode ((staticity Dynamic) (erasure Erased))))))
             (mode ((staticity Dynamic) (erasure Unerased)))
             (loc ((line 2) (column 42)))))
           (arg
            (Literal (value (Int (T 0))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased)))
             (loc ((line 2) (column 44)))))
           (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
           (loc ((line 2) (column 42)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "closure where binder expected: Arrow leq Pi" =
  go
    {|
let apply = fn (static f : static int -> int) -> f 0;;
let g = fn (x : int) -> x;;
let _ = apply g;;
|};
  [%expect
    {|
    (tst
     ((Let (var apply)
       (bind
        (Binder (arg f)
         (body
          (Apply (fn (Var (id f) (loc ((line 2) (column 49)))))
           (arg (Literal (value (Int 0)) (loc ((line 2) (column 51)))))
           (loc ((line 2) (column 49)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Pi (arg_ty (Type Int))
               (arg_mode ((staticity Static) (erasure Unerased)))
               (ret_ty (T (Type Int)))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 12)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Lambda (arg x)
         (body
          (Var (id x) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Unerased)))
           (loc ((line 3) (column 24)))))
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn
          (Var (id apply)
           (ty
            (Type
             (Pi
              (arg_ty
               (Type
                (Pi (arg_ty (Type Int))
                 (arg_mode ((staticity Static) (erasure Unerased)))
                 (ret_ty (T (Type Int)))
                 (ret_mode ((staticity Dynamic) (erasure Unerased))))))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Int)))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 8)))))
         (key (Closure 3)) (ty (Type Int))
         (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "binder to binder: arg erasure contravariant" =
  go
    {|
let apply = fn (static f : static int -> int) -> f 0;;
let g = fn (static erased x : int) -> 1;;
let _ = apply g;;
|};
  [%expect
    {|
    (tst
     ((Let (var apply)
       (bind
        (Binder (arg f)
         (body
          (Apply (fn (Var (id f) (loc ((line 2) (column 49)))))
           (arg (Literal (value (Int 0)) (loc ((line 2) (column 51)))))
           (loc ((line 2) (column 49)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Pi (arg_ty (Type Int))
               (arg_mode ((staticity Static) (erasure Unerased)))
               (ret_ty (T (Type Int)))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 12)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Binder (arg x)
         (body (Literal (value (Int 1)) (loc ((line 3) (column 38)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Erased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn
          (Var (id apply)
           (ty
            (Type
             (Pi
              (arg_ty
               (Type
                (Pi (arg_ty (Type Int))
                 (arg_mode ((staticity Static) (erasure Unerased)))
                 (ret_ty (T (Type Int)))
                 (ret_mode ((staticity Dynamic) (erasure Unerased))))))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Int)))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 8)))))
         (key (Closure 4)) (ty (Type Int))
         (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "erased closure taking closure arg" =
  go
    {|
let apply = fn (f : int -> int) -> f 0;;
let _ = (apply @ erased) (fn (x : int) -> x);;
|};
  [%expect
    {|
    (tst
     ((Let (var apply)
       (bind
        (Lambda (arg f)
         (body
          (Apply
           (fn
            (Var (id f)
             (ty
              (Type
               (Arrow (arg_ty (Type Int))
                (arg_mode ((staticity Dynamic) (erasure Unerased)))
                (ret_ty (Type Int))
                (ret_mode ((staticity Dynamic) (erasure Unerased))))))
             (mode ((staticity Dynamic) (erasure Unerased)))
             (loc ((line 2) (column 35)))))
           (arg
            (Literal (value (Int (T 0))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased)))
             (loc ((line 2) (column 37)))))
           (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
           (loc ((line 2) (column 35)))))
         (ty
          (Type
           (Arrow
            (arg_ty
             (Type
              (Arrow (arg_ty (Type Int))
               (arg_mode ((staticity Dynamic) (erasure Unerased)))
               (ret_ty (Type Int))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 12)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Let (var f)
         (bind
          (Lambda (arg x)
           (body
            (Var (id x) (ty (Type Int))
             (mode ((staticity Dynamic) (erasure Unerased)))
             (loc ((line 3) (column 42)))))
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 26)))))
         (rest
          (Apply
           (fn
            (Var (id f)
             (ty
              (Type
               (Arrow (arg_ty (Type Int))
                (arg_mode ((staticity Dynamic) (erasure Unerased)))
                (ret_ty (Type Int))
                (ret_mode ((staticity Dynamic) (erasure Unerased))))))
             (mode ((staticity Dynamic) (erasure Unerased)))
             (loc ((line 2) (column 35)))))
           (arg
            (Literal (value (Int (T 0))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased)))
             (loc ((line 2) (column 37)))))
           (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
           (loc ((line 2) (column 35)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "binder taking closure, applied erased inside" =
  go
    {|
let apply = fn (static f : static int -> erased int) -> (f @ erased) 0;;
let g = fn (x : int) -> 1;;
let _ = apply g;;
|};
  [%expect
    {|
    (tst
     ((Let (var apply)
       (bind
        (Binder (arg f)
         (body
          (Apply
           (fn
            (Paren
             (expr
              (Mode_annotation (expr (Var (id f) (loc ((line 2) (column 57)))))
               (mode ((staticity ()) (erasure (Erased))))
               (loc ((line 2) (column 59)))))
             (loc ((line 2) (column 56)))))
           (arg (Literal (value (Int 0)) (loc ((line 2) (column 69)))))
           (loc ((line 2) (column 56)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Pi (arg_ty (Type Int))
               (arg_mode ((staticity Static) (erasure Unerased)))
               (ret_ty (T (Type Int)))
               (ret_mode ((staticity Dynamic) (erasure Erased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Erased))))))
         (mode ((staticity Static) (erasure Erased)))
         (loc ((line 2) (column 12)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Lambda (arg x)
         (body
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 24)))))
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Let (var f)
         (bind
          (Var (id g)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 14)))))
         (rest
          (Let (var x)
           (bind
            (Literal (value (Int (T 0))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased)))
             (loc ((line 2) (column 69)))))
           (rest
            (Erased (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
             (loc ((line 2) (column 56)))))
           (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
           (loc ((line 2) (column 56)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "leq Pi/Pi: ret_mode covariant" =
  go
    {|
let f = fn (static x : int) -> 1;;
let _ = f : static int -> erased int;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body (Literal (value (Int 1)) (loc ((line 2) (column 31)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Var (id f)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Erased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "leq Pi/Pi: arg_mode contravariant" =
  go
    {|
let f = fn (static erased x : int) -> 1;;
let _ = f : static int -> int;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body (Literal (value (Int 1)) (loc ((line 2) (column 38)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Erased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Var (id f)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
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
  [%expect
    {|
    (tst
     ((Let (var apply)
       (bind
        (Binder (arg f)
         (body
          (Apply (fn (Var (id f) (loc ((line 2) (column 56)))))
           (arg (Literal (value (Int 0)) (loc ((line 2) (column 58)))))
           (loc ((line 2) (column 56)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Pi (arg_ty (Type Int))
               (arg_mode ((staticity Static) (erasure Unerased)))
               (ret_ty (T (Type Int)))
               (ret_mode ((staticity Dynamic) (erasure Erased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Erased))))))
         (mode ((staticity Static) (erasure Erased)))
         (loc ((line 2) (column 12)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Lambda (arg x)
         (body
          (Var (id x) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Unerased)))
           (loc ((line 3) (column 24)))))
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Let (var f)
         (bind
          (Var (id g)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 14)))))
         (rest
          (Apply
           (fn
            (Var (id f)
             (ty
              (Type
               (Pi (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (ret_ty (T (Type Int)))
                (ret_mode ((staticity Dynamic) (erasure Erased))))))
             (mode ((staticity Static) (erasure Unerased)))
             (loc ((line 2) (column 56)))))
           (arg
            (Literal (value (Int (T 0))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased)))
             (loc ((line 2) (column 58)))))
           (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
           (loc ((line 2) (column 56)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "join Pi/Pi: different ret_mode" =
  go
    {|
let f = fn (static x : int) -> 1;;
let g = fn (static x : int) -> 1 @ erased;;
let _ = if true then f else g;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body (Literal (value (Int 1)) (loc ((line 2) (column 31)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Binder (arg x)
         (body
          (Mode_annotation
           (expr (Literal (value (Int 1)) (loc ((line 3) (column 31)))))
           (mode ((staticity ()) (erasure (Erased))))
           (loc ((line 3) (column 33)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Static) (erasure Erased))))))
         (mode ((staticity Static) (erasure Erased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (If
         (cond
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 11)))))
         (then_
          (Var (id f)
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Int)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 21)))))
         (else_
          (Literal
           (value
            (Binder
             ((arg x)
              (ty
               (Type
                (Pi (arg_ty (Type Int))
                 (arg_mode ((staticity Static) (erasure Unerased)))
                 (ret_ty (T (Type Int)))
                 (ret_mode ((staticity Static) (erasure Erased))))))
              (body
               (Mode_annotation
                (expr (Literal (value (Int 1)) (loc ((line 3) (column 31)))))
                (mode ((staticity ()) (erasure (Erased))))
                (loc ((line 3) (column 33)))))
              (mono <opaque>) (env <opaque>))))
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Int)))
              (ret_mode ((staticity Static) (erasure Erased))))))
           (mode ((staticity Static) (erasure Erased)))
           (loc ((line 4) (column 28)))))
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Static) (erasure Erased))))))
         (mode ((staticity Static) (erasure Erased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "join Pi/Pi: different arg_mode" =
  go
    {|
let f = fn (static x : int) -> 1;;
let g = fn (static erased x : int) -> 1;;
let _ = if true then f else g;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body (Literal (value (Int 1)) (loc ((line 2) (column 31)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Binder (arg x)
         (body (Literal (value (Int 1)) (loc ((line 3) (column 38)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Erased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (If
         (cond
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 11)))))
         (then_
          (Var (id f)
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Int)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 21)))))
         (else_
          (Var (id g)
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Erased)))
              (ret_ty (T (Type Int)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 28)))))
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "join Arrow/Pi" =
  go
    {|
let f = fn (x : int) -> 1;;
let g = fn (static x : int) -> 1;;
let _ = if true then f else g;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Lambda (arg x)
         (body
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 2) (column 24)))))
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Binder (arg x)
         (body (Literal (value (Int 1)) (loc ((line 3) (column 31)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (If
         (cond
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 11)))))
         (then_
          (Var (id f)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 21)))))
         (else_
          (Var (id g)
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Int)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 28)))))
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "join Pi/Arrow" =
  go
    {|
let f = fn (static x : int) -> 1;;
let g = fn (x : int) -> 1;;
let _ = if true then f else g;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body (Literal (value (Int 1)) (loc ((line 2) (column 31)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Lambda (arg x)
         (body
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 24)))))
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (If
         (cond
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 11)))))
         (then_
          (Var (id f)
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Int)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 21)))))
         (else_
          (Var (id g)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 28)))))
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "meet Pi/Pi: via arg contravariance in join" =
  go
    {|
let f = fn (static g : static int -> int) -> g 0;;
let h = fn (static g : static erased int -> int) -> g 0;;
let _ = if true then f else h;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg g)
         (body
          (Apply (fn (Var (id g) (loc ((line 2) (column 45)))))
           (arg (Literal (value (Int 0)) (loc ((line 2) (column 47)))))
           (loc ((line 2) (column 45)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Pi (arg_ty (Type Int))
               (arg_mode ((staticity Static) (erasure Unerased)))
               (ret_ty (T (Type Int)))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var h)
       (bind
        (Binder (arg g)
         (body
          (Apply (fn (Var (id g) (loc ((line 3) (column 52)))))
           (arg (Literal (value (Int 0)) (loc ((line 3) (column 54)))))
           (loc ((line 3) (column 52)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Pi (arg_ty (Type Int))
               (arg_mode ((staticity Static) (erasure Erased)))
               (ret_ty (T (Type Int)))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (If
         (cond
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 11)))))
         (then_
          (Var (id f)
           (ty
            (Type
             (Pi
              (arg_ty
               (Type
                (Pi (arg_ty (Type Int))
                 (arg_mode ((staticity Static) (erasure Unerased)))
                 (ret_ty (T (Type Int)))
                 (ret_mode ((staticity Dynamic) (erasure Unerased))))))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Int)))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 21)))))
         (else_
          (Var (id h)
           (ty
            (Type
             (Pi
              (arg_ty
               (Type
                (Pi (arg_ty (Type Int))
                 (arg_mode ((staticity Static) (erasure Erased)))
                 (ret_ty (T (Type Int)))
                 (ret_mode ((staticity Dynamic) (erasure Unerased))))))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Int)))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 28)))))
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Pi (arg_ty (Type Int))
               (arg_mode ((staticity Static) (erasure Erased)))
               (ret_ty (T (Type Int)))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "apply joined Pi/Pi" =
  go
    {|
let f = fn (static x : int) -> 1;;
let g = fn (static x : int) -> 1 @ erased;;
let _ = (if true then f else g) 0;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body (Literal (value (Int 1)) (loc ((line 2) (column 31)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Binder (arg x)
         (body
          (Mode_annotation
           (expr (Literal (value (Int 1)) (loc ((line 3) (column 31)))))
           (mode ((staticity ()) (erasure (Erased))))
           (loc ((line 3) (column 33)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Static) (erasure Erased))))))
         (mode ((staticity Static) (erasure Erased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Let (var x)
         (bind
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 32)))))
         (rest
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Erased)))
           (loc ((line 4) (column 8)))))
         (ty (Type Int)) (mode ((staticity Static) (erasure Erased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
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
           (Typecheck (env <opaque>) (arg t) (arg_ty (Type Type))
            (arg_mode ((staticity Static) (erasure Unerased))) (memo <opaque>)
            (body
             (Lambda (arg x) (erased Unerased)
              (arg_mode ((staticity ()) (erasure ())))
              (arg_ty (Var (id t) (loc ((line 2) (column 41)))))
              (body (Var (id x) (loc ((line 2) (column 47)))))
              (loc ((line 2) (column 33)))))))
          (ret_mode ((staticity Static) (erasure Unerased))))))
       (need
        (Type
         (Pi (arg_ty (Type Type))
          (arg_mode ((staticity Static) (erasure Unerased)))
          (ret_ty
           (Reduce (env <opaque>) (arg t) (arg_ty (Type Type))
            (arg_mode ((staticity Static) (erasure Unerased))) (memo <opaque>)
            (ret_ty
             (Arrow (arg (Var (id t) (loc ((line 3) (column 39))))) (arg_id ())
              (arg_mode ((staticity ()) (erasure (Erased))))
              (ret (Var (id t) (loc ((line 3) (column 44)))))
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
  [%expect
    {|
    (tst
     ((Let (var id)
       (bind
        (Binder (arg t)
         (body
          (Lambda (arg x) (erased Unerased)
           (arg_mode ((staticity ()) (erasure ())))
           (arg_ty (Var (id t) (loc ((line 2) (column 41)))))
           (body (Var (id x) (loc ((line 2) (column 47)))))
           (loc ((line 2) (column 33)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Type))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (Typecheck (env <opaque>) (arg t) (arg_ty (Type Type))
              (arg_mode ((staticity Static) (erasure Unerased))) (memo <opaque>)
              (body
               (Lambda (arg x) (erased Unerased)
                (arg_mode ((staticity ()) (erasure ())))
                (arg_ty (Var (id t) (loc ((line 2) (column 41)))))
                (body (Var (id x) (loc ((line 2) (column 47)))))
                (loc ((line 2) (column 33)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 9)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Var (id id)
         (ty
          (Type
           (Pi (arg_ty (Type Type))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (Reduce (env <opaque>) (arg t) (arg_ty (Type Type))
              (arg_mode ((staticity Static) (erasure Unerased))) (memo <opaque>)
              (ret_ty
               (Paren
                (expr
                 (Arrow (arg (Var (id t) (loc ((line 3) (column 40)))))
                  (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
                  (ret (Var (id t) (loc ((line 3) (column 45)))))
                  (ret_mode ((staticity ()) (erasure ())))
                  (loc ((line 3) (column 42)))))
                (loc ((line 3) (column 39)))))))
            (ret_mode ((staticity Dynamic) (erasure Erased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "join Pi/Pi dependent return: different ret_mode" =
  go
    {|
let f = fn (static t : type) -> fn (x : t) -> x;;
let g = fn (static t : type) -> (fn (x : t) -> x) @ erased;;
let _ = if true then f else g;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg t)
         (body
          (Lambda (arg x) (erased Unerased)
           (arg_mode ((staticity ()) (erasure ())))
           (arg_ty (Var (id t) (loc ((line 2) (column 40)))))
           (body (Var (id x) (loc ((line 2) (column 46)))))
           (loc ((line 2) (column 32)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Type))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (Typecheck (env <opaque>) (arg t) (arg_ty (Type Type))
              (arg_mode ((staticity Static) (erasure Unerased))) (memo <opaque>)
              (body
               (Lambda (arg x) (erased Unerased)
                (arg_mode ((staticity ()) (erasure ())))
                (arg_ty (Var (id t) (loc ((line 2) (column 40)))))
                (body (Var (id x) (loc ((line 2) (column 46)))))
                (loc ((line 2) (column 32)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Binder (arg t)
         (body
          (Mode_annotation
           (expr
            (Paren
             (expr
              (Lambda (arg x) (erased Unerased)
               (arg_mode ((staticity ()) (erasure ())))
               (arg_ty (Var (id t) (loc ((line 3) (column 41)))))
               (body (Var (id x) (loc ((line 3) (column 47)))))
               (loc ((line 3) (column 33)))))
             (loc ((line 3) (column 32)))))
           (mode ((staticity ()) (erasure (Erased))))
           (loc ((line 3) (column 50)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Type))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (Typecheck (env <opaque>) (arg t) (arg_ty (Type Type))
              (arg_mode ((staticity Static) (erasure Unerased))) (memo <opaque>)
              (body
               (Mode_annotation
                (expr
                 (Paren
                  (expr
                   (Lambda (arg x) (erased Unerased)
                    (arg_mode ((staticity ()) (erasure ())))
                    (arg_ty (Var (id t) (loc ((line 3) (column 41)))))
                    (body (Var (id x) (loc ((line 3) (column 47)))))
                    (loc ((line 3) (column 33)))))
                  (loc ((line 3) (column 32)))))
                (mode ((staticity ()) (erasure (Erased))))
                (loc ((line 3) (column 50)))))))
            (ret_mode ((staticity Static) (erasure Erased))))))
         (mode ((staticity Static) (erasure Erased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (If
         (cond
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 11)))))
         (then_
          (Var (id f)
           (ty
            (Type
             (Pi (arg_ty (Type Type))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty
               (Typecheck (env <opaque>) (arg t) (arg_ty (Type Type))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (memo <opaque>)
                (body
                 (Lambda (arg x) (erased Unerased)
                  (arg_mode ((staticity ()) (erasure ())))
                  (arg_ty (Var (id t) (loc ((line 2) (column 40)))))
                  (body (Var (id x) (loc ((line 2) (column 46)))))
                  (loc ((line 2) (column 32)))))))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 21)))))
         (else_
          (Literal
           (value
            (Binder
             ((arg t)
              (ty
               (Type
                (Pi (arg_ty (Type Type))
                 (arg_mode ((staticity Static) (erasure Unerased)))
                 (ret_ty
                  (Typecheck (env <opaque>) (arg t) (arg_ty (Type Type))
                   (arg_mode ((staticity Static) (erasure Unerased)))
                   (memo <opaque>)
                   (body
                    (Mode_annotation
                     (expr
                      (Paren
                       (expr
                        (Lambda (arg x) (erased Unerased)
                         (arg_mode ((staticity ()) (erasure ())))
                         (arg_ty (Var (id t) (loc ((line 3) (column 41)))))
                         (body (Var (id x) (loc ((line 3) (column 47)))))
                         (loc ((line 3) (column 33)))))
                       (loc ((line 3) (column 32)))))
                     (mode ((staticity ()) (erasure (Erased))))
                     (loc ((line 3) (column 50)))))))
                 (ret_mode ((staticity Static) (erasure Erased))))))
              (body
               (Mode_annotation
                (expr
                 (Paren
                  (expr
                   (Lambda (arg x) (erased Unerased)
                    (arg_mode ((staticity ()) (erasure ())))
                    (arg_ty (Var (id t) (loc ((line 3) (column 41)))))
                    (body (Var (id x) (loc ((line 3) (column 47)))))
                    (loc ((line 3) (column 33)))))
                  (loc ((line 3) (column 32)))))
                (mode ((staticity ()) (erasure (Erased))))
                (loc ((line 3) (column 50)))))
              (mono <opaque>) (env <opaque>))))
           (ty
            (Type
             (Pi (arg_ty (Type Type))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty
               (Typecheck (env <opaque>) (arg t) (arg_ty (Type Type))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (memo <opaque>)
                (body
                 (Mode_annotation
                  (expr
                   (Paren
                    (expr
                     (Lambda (arg x) (erased Unerased)
                      (arg_mode ((staticity ()) (erasure ())))
                      (arg_ty (Var (id t) (loc ((line 3) (column 41)))))
                      (body (Var (id x) (loc ((line 3) (column 47)))))
                      (loc ((line 3) (column 33)))))
                    (loc ((line 3) (column 32)))))
                  (mode ((staticity ()) (erasure (Erased))))
                  (loc ((line 3) (column 50)))))))
              (ret_mode ((staticity Static) (erasure Erased))))))
           (mode ((staticity Static) (erasure Erased)))
           (loc ((line 4) (column 28)))))
         (ty
          (Type
           (Pi (arg_ty (Type Type))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (Join
              (Typecheck (env <opaque>) (arg t) (arg_ty (Type Type))
               (arg_mode ((staticity Static) (erasure Unerased))) (memo <opaque>)
               (body
                (Lambda (arg x) (erased Unerased)
                 (arg_mode ((staticity ()) (erasure ())))
                 (arg_ty (Var (id t) (loc ((line 2) (column 40)))))
                 (body (Var (id x) (loc ((line 2) (column 46)))))
                 (loc ((line 2) (column 32))))))
              (Typecheck (env <opaque>) (arg t) (arg_ty (Type Type))
               (arg_mode ((staticity Static) (erasure Unerased))) (memo <opaque>)
               (body
                (Mode_annotation
                 (expr
                  (Paren
                   (expr
                    (Lambda (arg x) (erased Unerased)
                     (arg_mode ((staticity ()) (erasure ())))
                     (arg_ty (Var (id t) (loc ((line 3) (column 41)))))
                     (body (Var (id x) (loc ((line 3) (column 47)))))
                     (loc ((line 3) (column 33)))))
                   (loc ((line 3) (column 32)))))
                 (mode ((staticity ()) (erasure (Erased))))
                 (loc ((line 3) (column 50))))))))
            (ret_mode ((staticity Static) (erasure Erased))))))
         (mode ((staticity Static) (erasure Erased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "join Pi/Pi dependent return: different arg_mode" =
  go
    {|
let f = fn (static t : type) -> fn (x : t) -> x;;
let g = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = if true then f else g;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg t)
         (body
          (Lambda (arg x) (erased Unerased)
           (arg_mode ((staticity ()) (erasure ())))
           (arg_ty (Var (id t) (loc ((line 2) (column 40)))))
           (body (Var (id x) (loc ((line 2) (column 46)))))
           (loc ((line 2) (column 32)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Type))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (Typecheck (env <opaque>) (arg t) (arg_ty (Type Type))
              (arg_mode ((staticity Static) (erasure Unerased))) (memo <opaque>)
              (body
               (Lambda (arg x) (erased Unerased)
                (arg_mode ((staticity ()) (erasure ())))
                (arg_ty (Var (id t) (loc ((line 2) (column 40)))))
                (body (Var (id x) (loc ((line 2) (column 46)))))
                (loc ((line 2) (column 32)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Binder (arg t)
         (body
          (Lambda (arg x) (erased Unerased)
           (arg_mode ((staticity ()) (erasure ())))
           (arg_ty (Var (id t) (loc ((line 3) (column 47)))))
           (body (Var (id x) (loc ((line 3) (column 53)))))
           (loc ((line 3) (column 39)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Type))
            (arg_mode ((staticity Static) (erasure Erased)))
            (ret_ty
             (Typecheck (env <opaque>) (arg t) (arg_ty (Type Type))
              (arg_mode ((staticity Static) (erasure Erased))) (memo <opaque>)
              (body
               (Lambda (arg x) (erased Unerased)
                (arg_mode ((staticity ()) (erasure ())))
                (arg_ty (Var (id t) (loc ((line 3) (column 47)))))
                (body (Var (id x) (loc ((line 3) (column 53)))))
                (loc ((line 3) (column 39)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (If
         (cond
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 11)))))
         (then_
          (Var (id f)
           (ty
            (Type
             (Pi (arg_ty (Type Type))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty
               (Typecheck (env <opaque>) (arg t) (arg_ty (Type Type))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (memo <opaque>)
                (body
                 (Lambda (arg x) (erased Unerased)
                  (arg_mode ((staticity ()) (erasure ())))
                  (arg_ty (Var (id t) (loc ((line 2) (column 40)))))
                  (body (Var (id x) (loc ((line 2) (column 46)))))
                  (loc ((line 2) (column 32)))))))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 21)))))
         (else_
          (Var (id g)
           (ty
            (Type
             (Pi (arg_ty (Type Type))
              (arg_mode ((staticity Static) (erasure Erased)))
              (ret_ty
               (Typecheck (env <opaque>) (arg t) (arg_ty (Type Type))
                (arg_mode ((staticity Static) (erasure Erased))) (memo <opaque>)
                (body
                 (Lambda (arg x) (erased Unerased)
                  (arg_mode ((staticity ()) (erasure ())))
                  (arg_ty (Var (id t) (loc ((line 3) (column 47)))))
                  (body (Var (id x) (loc ((line 3) (column 53)))))
                  (loc ((line 3) (column 39)))))))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 28)))))
         (ty
          (Type
           (Pi (arg_ty (Type Type))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (Join
              (Typecheck (env <opaque>) (arg t) (arg_ty (Type Type))
               (arg_mode ((staticity Static) (erasure Unerased))) (memo <opaque>)
               (body
                (Lambda (arg x) (erased Unerased)
                 (arg_mode ((staticity ()) (erasure ())))
                 (arg_ty (Var (id t) (loc ((line 2) (column 40)))))
                 (body (Var (id x) (loc ((line 2) (column 46)))))
                 (loc ((line 2) (column 32))))))
              (Typecheck (env <opaque>) (arg t) (arg_ty (Type Type))
               (arg_mode ((staticity Static) (erasure Erased))) (memo <opaque>)
               (body
                (Lambda (arg x) (erased Unerased)
                 (arg_mode ((staticity ()) (erasure ())))
                 (arg_ty (Var (id t) (loc ((line 3) (column 47)))))
                 (body (Var (id x) (loc ((line 3) (column 53)))))
                 (loc ((line 3) (column 39))))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "apply joined dependent Pi/Pi" =
  go
    {|
let f = fn (static erased t : type) -> fn (x : t) -> x;;
let g = fn (static erased t : type) -> (fn (x : t) -> x) @ erased;;
let _ = (if true then f else g) int;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg t)
         (body
          (Lambda (arg x) (erased Unerased)
           (arg_mode ((staticity ()) (erasure ())))
           (arg_ty (Var (id t) (loc ((line 2) (column 47)))))
           (body (Var (id x) (loc ((line 2) (column 53)))))
           (loc ((line 2) (column 39)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Type))
            (arg_mode ((staticity Static) (erasure Erased)))
            (ret_ty
             (Typecheck (env <opaque>) (arg t) (arg_ty (Type Type))
              (arg_mode ((staticity Static) (erasure Erased))) (memo <opaque>)
              (body
               (Lambda (arg x) (erased Unerased)
                (arg_mode ((staticity ()) (erasure ())))
                (arg_ty (Var (id t) (loc ((line 2) (column 47)))))
                (body (Var (id x) (loc ((line 2) (column 53)))))
                (loc ((line 2) (column 39)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Binder (arg t)
         (body
          (Mode_annotation
           (expr
            (Paren
             (expr
              (Lambda (arg x) (erased Unerased)
               (arg_mode ((staticity ()) (erasure ())))
               (arg_ty (Var (id t) (loc ((line 3) (column 48)))))
               (body (Var (id x) (loc ((line 3) (column 54)))))
               (loc ((line 3) (column 40)))))
             (loc ((line 3) (column 39)))))
           (mode ((staticity ()) (erasure (Erased))))
           (loc ((line 3) (column 57)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Type))
            (arg_mode ((staticity Static) (erasure Erased)))
            (ret_ty
             (Typecheck (env <opaque>) (arg t) (arg_ty (Type Type))
              (arg_mode ((staticity Static) (erasure Erased))) (memo <opaque>)
              (body
               (Mode_annotation
                (expr
                 (Paren
                  (expr
                   (Lambda (arg x) (erased Unerased)
                    (arg_mode ((staticity ()) (erasure ())))
                    (arg_ty (Var (id t) (loc ((line 3) (column 48)))))
                    (body (Var (id x) (loc ((line 3) (column 54)))))
                    (loc ((line 3) (column 40)))))
                  (loc ((line 3) (column 39)))))
                (mode ((staticity ()) (erasure (Erased))))
                (loc ((line 3) (column 57)))))))
            (ret_mode ((staticity Static) (erasure Erased))))))
         (mode ((staticity Static) (erasure Erased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Literal
         (value
          (Closure
           ((arg x)
            (ty
             (Type
              (Arrow (arg_ty (Type Int))
               (arg_mode ((staticity Dynamic) (erasure Unerased)))
               (ret_ty (Type Int))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (body
             (Var (id x) (ty (Type Int))
              (mode ((staticity Dynamic) (erasure Unerased)))
              (loc ((line 2) (column 53)))))
            (env <opaque>))))
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Erased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "leq Pi/Pi function-type arg: arg contravariant" =
  go
    {|
let f = fn (static g : static int -> int) -> g 0;;
let _ = f : static (static erased int -> int) -> int;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg g)
         (body
          (Apply (fn (Var (id g) (loc ((line 2) (column 45)))))
           (arg (Literal (value (Int 0)) (loc ((line 2) (column 47)))))
           (loc ((line 2) (column 45)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Pi (arg_ty (Type Int))
               (arg_mode ((staticity Static) (erasure Unerased)))
               (ret_ty (T (Type Int)))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Var (id f)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Pi (arg_ty (Type Int))
               (arg_mode ((staticity Static) (erasure Erased)))
               (ret_ty (T (Type Int)))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "leq Pi/Pi function-type arg: arg contravariant" =
  go
    {|
let f = fn (static g : static type \ t -> t) -> ();;
let _ = f : static (static erased type \ t -> t) -> unit;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg g)
         (body (Literal (value Unit) (loc ((line 2) (column 48)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Pi (arg_ty (Type Type))
               (arg_mode ((staticity Static) (erasure Unerased)))
               (ret_ty
                (Reduce (env <opaque>) (arg t) (arg_ty (Type Type))
                 (arg_mode ((staticity Static) (erasure Unerased)))
                 (memo <opaque>)
                 (ret_ty (Var (id t) (loc ((line 2) (column 42)))))))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Unit)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Var (id f)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Pi (arg_ty (Type Type))
               (arg_mode ((staticity Static) (erasure Erased)))
               (ret_ty
                (Reduce (env <opaque>) (arg t) (arg_ty (Type Type))
                 (arg_mode ((staticity Static) (erasure Erased))) (memo <opaque>)
                 (ret_ty (Var (id t) (loc ((line 3) (column 46)))))))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Unit)))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "leq Pi/Pi function-type ret" =
  go
    {|
let f = fn (static g : static type \ t -> erased t) -> ();;
let _ = f : static (static erased type \ t -> t) -> unit;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg g)
         (body (Literal (value Unit) (loc ((line 2) (column 55)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Pi (arg_ty (Type Type))
               (arg_mode ((staticity Static) (erasure Unerased)))
               (ret_ty
                (Reduce (env <opaque>) (arg t) (arg_ty (Type Type))
                 (arg_mode ((staticity Static) (erasure Unerased)))
                 (memo <opaque>)
                 (ret_ty (Var (id t) (loc ((line 2) (column 49)))))))
               (ret_mode ((staticity Dynamic) (erasure Erased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Unit)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Var (id f)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Pi (arg_ty (Type Type))
               (arg_mode ((staticity Static) (erasure Erased)))
               (ret_ty
                (Reduce (env <opaque>) (arg t) (arg_ty (Type Type))
                 (arg_mode ((staticity Static) (erasure Erased))) (memo <opaque>)
                 (ret_ty (Var (id t) (loc ((line 3) (column 46)))))))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Unit)))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "join Pi/Pi function-type arg: different inner arg_mode" =
  go
    {|
let f = fn (static g : static int -> int) -> g 0;;
let h = fn (static g : static erased int -> int) -> g 0;;
let _ = if true then f else h;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg g)
         (body
          (Apply (fn (Var (id g) (loc ((line 2) (column 45)))))
           (arg (Literal (value (Int 0)) (loc ((line 2) (column 47)))))
           (loc ((line 2) (column 45)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Pi (arg_ty (Type Int))
               (arg_mode ((staticity Static) (erasure Unerased)))
               (ret_ty (T (Type Int)))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var h)
       (bind
        (Binder (arg g)
         (body
          (Apply (fn (Var (id g) (loc ((line 3) (column 52)))))
           (arg (Literal (value (Int 0)) (loc ((line 3) (column 54)))))
           (loc ((line 3) (column 52)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Pi (arg_ty (Type Int))
               (arg_mode ((staticity Static) (erasure Erased)))
               (ret_ty (T (Type Int)))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (If
         (cond
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 11)))))
         (then_
          (Var (id f)
           (ty
            (Type
             (Pi
              (arg_ty
               (Type
                (Pi (arg_ty (Type Int))
                 (arg_mode ((staticity Static) (erasure Unerased)))
                 (ret_ty (T (Type Int)))
                 (ret_mode ((staticity Dynamic) (erasure Unerased))))))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Int)))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 21)))))
         (else_
          (Var (id h)
           (ty
            (Type
             (Pi
              (arg_ty
               (Type
                (Pi (arg_ty (Type Int))
                 (arg_mode ((staticity Static) (erasure Erased)))
                 (ret_ty (T (Type Int)))
                 (ret_mode ((staticity Dynamic) (erasure Unerased))))))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Int)))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 28)))))
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Pi (arg_ty (Type Int))
               (arg_mode ((staticity Static) (erasure Erased)))
               (ret_ty (T (Type Int)))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "dependent return via function-type arg returning type" =
  go
    {|
let wrap = fn (static f : static int -> static type) -> fn (x : f 0) -> x;;
|};
  [%expect
    {|
    (tst
     ((Let (var wrap)
       (bind
        (Binder (arg f)
         (body
          (Lambda (arg x) (erased Unerased)
           (arg_mode ((staticity ()) (erasure ())))
           (arg_ty
            (Apply (fn (Var (id f) (loc ((line 2) (column 64)))))
             (arg (Literal (value (Int 0)) (loc ((line 2) (column 66)))))
             (loc ((line 2) (column 64)))))
           (body (Var (id x) (loc ((line 2) (column 72)))))
           (loc ((line 2) (column 56)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Pi (arg_ty (Type Int))
               (arg_mode ((staticity Static) (erasure Unerased)))
               (ret_ty (T (Type Type)))
               (ret_mode ((staticity Static) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (Typecheck (env <opaque>) (arg f)
              (arg_ty
               (Type
                (Pi (arg_ty (Type Int))
                 (arg_mode ((staticity Static) (erasure Unerased)))
                 (ret_ty (T (Type Type)))
                 (ret_mode ((staticity Static) (erasure Unerased))))))
              (arg_mode ((staticity Static) (erasure Unerased))) (memo <opaque>)
              (body
               (Lambda (arg x) (erased Unerased)
                (arg_mode ((staticity ()) (erasure ())))
                (arg_ty
                 (Apply (fn (Var (id f) (loc ((line 2) (column 64)))))
                  (arg (Literal (value (Int 0)) (loc ((line 2) (column 66)))))
                  (loc ((line 2) (column 64)))))
                (body (Var (id x) (loc ((line 2) (column 72)))))
                (loc ((line 2) (column 56)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 11)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "leq Pi/Pi function-type arg returning type" =
  go
    {|
let wrap = fn (static f : static int -> static type) -> fn (x : f 0) -> x;;
let _ = wrap : static (static int -> static type) \ f -> f 0 -> f 0;;
|};
  [%expect
    {|
    (tst
     ((Let (var wrap)
       (bind
        (Binder (arg f)
         (body
          (Lambda (arg x) (erased Unerased)
           (arg_mode ((staticity ()) (erasure ())))
           (arg_ty
            (Apply (fn (Var (id f) (loc ((line 2) (column 64)))))
             (arg (Literal (value (Int 0)) (loc ((line 2) (column 66)))))
             (loc ((line 2) (column 64)))))
           (body (Var (id x) (loc ((line 2) (column 72)))))
           (loc ((line 2) (column 56)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Pi (arg_ty (Type Int))
               (arg_mode ((staticity Static) (erasure Unerased)))
               (ret_ty (T (Type Type)))
               (ret_mode ((staticity Static) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (Typecheck (env <opaque>) (arg f)
              (arg_ty
               (Type
                (Pi (arg_ty (Type Int))
                 (arg_mode ((staticity Static) (erasure Unerased)))
                 (ret_ty (T (Type Type)))
                 (ret_mode ((staticity Static) (erasure Unerased))))))
              (arg_mode ((staticity Static) (erasure Unerased))) (memo <opaque>)
              (body
               (Lambda (arg x) (erased Unerased)
                (arg_mode ((staticity ()) (erasure ())))
                (arg_ty
                 (Apply (fn (Var (id f) (loc ((line 2) (column 64)))))
                  (arg (Literal (value (Int 0)) (loc ((line 2) (column 66)))))
                  (loc ((line 2) (column 64)))))
                (body (Var (id x) (loc ((line 2) (column 72)))))
                (loc ((line 2) (column 56)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 11)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Var (id wrap)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Pi (arg_ty (Type Int))
               (arg_mode ((staticity Static) (erasure Unerased)))
               (ret_ty (T (Type Type)))
               (ret_mode ((staticity Static) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (Reduce (env <opaque>) (arg f)
              (arg_ty
               (Type
                (Pi (arg_ty (Type Int))
                 (arg_mode ((staticity Static) (erasure Unerased)))
                 (ret_ty (T (Type Type)))
                 (ret_mode ((staticity Static) (erasure Unerased))))))
              (arg_mode ((staticity Static) (erasure Unerased))) (memo <opaque>)
              (ret_ty
               (Arrow
                (arg
                 (Apply (fn (Var (id f) (loc ((line 3) (column 57)))))
                  (arg (Literal (value (Int 0)) (loc ((line 3) (column 59)))))
                  (loc ((line 3) (column 57)))))
                (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
                (ret
                 (Apply (fn (Var (id f) (loc ((line 3) (column 64)))))
                  (arg (Literal (value (Int 0)) (loc ((line 3) (column 66)))))
                  (loc ((line 3) (column 64)))))
                (ret_mode ((staticity ()) (erasure ())))
                (loc ((line 3) (column 61)))))))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "meet Pi/Pi function-type arg: via arg contravariance in join" =
  go
    {|
let f = fn (static apply : static (static int -> int) -> int) -> apply (fn (static x : int) -> 0);;
let g = fn (static apply : static (static erased int -> int) -> int) -> apply (fn (static erased x : int) -> 0);;
let _ = if true then f else g;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg apply)
         (body
          (Apply (fn (Var (id apply) (loc ((line 2) (column 65)))))
           (arg
            (Paren
             (expr
              (Lambda (arg x) (erased Unerased)
               (arg_mode ((staticity (Static)) (erasure ())))
               (arg_ty (Var (id int) (loc ((line 2) (column 87)))))
               (body (Literal (value (Int 0)) (loc ((line 2) (column 95)))))
               (loc ((line 2) (column 72)))))
             (loc ((line 2) (column 71)))))
           (loc ((line 2) (column 65)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Pi
               (arg_ty
                (Type
                 (Pi (arg_ty (Type Int))
                  (arg_mode ((staticity Static) (erasure Unerased)))
                  (ret_ty (T (Type Int)))
                  (ret_mode ((staticity Dynamic) (erasure Unerased))))))
               (arg_mode ((staticity Static) (erasure Unerased)))
               (ret_ty (T (Type Int)))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Binder (arg apply)
         (body
          (Apply (fn (Var (id apply) (loc ((line 3) (column 72)))))
           (arg
            (Paren
             (expr
              (Lambda (arg x) (erased Unerased)
               (arg_mode ((staticity (Static)) (erasure (Erased))))
               (arg_ty (Var (id int) (loc ((line 3) (column 101)))))
               (body (Literal (value (Int 0)) (loc ((line 3) (column 109)))))
               (loc ((line 3) (column 79)))))
             (loc ((line 3) (column 78)))))
           (loc ((line 3) (column 72)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Pi
               (arg_ty
                (Type
                 (Pi (arg_ty (Type Int))
                  (arg_mode ((staticity Static) (erasure Erased)))
                  (ret_ty (T (Type Int)))
                  (ret_mode ((staticity Dynamic) (erasure Unerased))))))
               (arg_mode ((staticity Static) (erasure Unerased)))
               (ret_ty (T (Type Int)))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (If
         (cond
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 11)))))
         (then_
          (Var (id f)
           (ty
            (Type
             (Pi
              (arg_ty
               (Type
                (Pi
                 (arg_ty
                  (Type
                   (Pi (arg_ty (Type Int))
                    (arg_mode ((staticity Static) (erasure Unerased)))
                    (ret_ty (T (Type Int)))
                    (ret_mode ((staticity Dynamic) (erasure Unerased))))))
                 (arg_mode ((staticity Static) (erasure Unerased)))
                 (ret_ty (T (Type Int)))
                 (ret_mode ((staticity Dynamic) (erasure Unerased))))))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Int)))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 21)))))
         (else_
          (Var (id g)
           (ty
            (Type
             (Pi
              (arg_ty
               (Type
                (Pi
                 (arg_ty
                  (Type
                   (Pi (arg_ty (Type Int))
                    (arg_mode ((staticity Static) (erasure Erased)))
                    (ret_ty (T (Type Int)))
                    (ret_mode ((staticity Dynamic) (erasure Unerased))))))
                 (arg_mode ((staticity Static) (erasure Unerased)))
                 (ret_ty (T (Type Int)))
                 (ret_mode ((staticity Dynamic) (erasure Unerased))))))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Int)))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 28)))))
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Pi
               (arg_ty
                (Type
                 (Pi (arg_ty (Type Int))
                  (arg_mode ((staticity Static) (erasure Unerased)))
                  (ret_ty (T (Type Int)))
                  (ret_mode ((staticity Dynamic) (erasure Unerased))))))
               (arg_mode ((staticity Static) (erasure Unerased)))
               (ret_ty (T (Type Int)))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "join Pi/Pi function-type arg returning type: fresh var issue" =
  go
    {|
let f = fn (static g : static int -> static type) -> fn (x : g 0) -> x;;
let h = fn (static g : static int -> static type) -> fn (x : g 0) -> x;;
let _ = if true then f else h;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg g)
         (body
          (Lambda (arg x) (erased Unerased)
           (arg_mode ((staticity ()) (erasure ())))
           (arg_ty
            (Apply (fn (Var (id g) (loc ((line 2) (column 61)))))
             (arg (Literal (value (Int 0)) (loc ((line 2) (column 63)))))
             (loc ((line 2) (column 61)))))
           (body (Var (id x) (loc ((line 2) (column 69)))))
           (loc ((line 2) (column 53)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Pi (arg_ty (Type Int))
               (arg_mode ((staticity Static) (erasure Unerased)))
               (ret_ty (T (Type Type)))
               (ret_mode ((staticity Static) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (Typecheck (env <opaque>) (arg g)
              (arg_ty
               (Type
                (Pi (arg_ty (Type Int))
                 (arg_mode ((staticity Static) (erasure Unerased)))
                 (ret_ty (T (Type Type)))
                 (ret_mode ((staticity Static) (erasure Unerased))))))
              (arg_mode ((staticity Static) (erasure Unerased))) (memo <opaque>)
              (body
               (Lambda (arg x) (erased Unerased)
                (arg_mode ((staticity ()) (erasure ())))
                (arg_ty
                 (Apply (fn (Var (id g) (loc ((line 2) (column 61)))))
                  (arg (Literal (value (Int 0)) (loc ((line 2) (column 63)))))
                  (loc ((line 2) (column 61)))))
                (body (Var (id x) (loc ((line 2) (column 69)))))
                (loc ((line 2) (column 53)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var h)
       (bind
        (Binder (arg g)
         (body
          (Lambda (arg x) (erased Unerased)
           (arg_mode ((staticity ()) (erasure ())))
           (arg_ty
            (Apply (fn (Var (id g) (loc ((line 3) (column 61)))))
             (arg (Literal (value (Int 0)) (loc ((line 3) (column 63)))))
             (loc ((line 3) (column 61)))))
           (body (Var (id x) (loc ((line 3) (column 69)))))
           (loc ((line 3) (column 53)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Pi (arg_ty (Type Int))
               (arg_mode ((staticity Static) (erasure Unerased)))
               (ret_ty (T (Type Type)))
               (ret_mode ((staticity Static) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (Typecheck (env <opaque>) (arg g)
              (arg_ty
               (Type
                (Pi (arg_ty (Type Int))
                 (arg_mode ((staticity Static) (erasure Unerased)))
                 (ret_ty (T (Type Type)))
                 (ret_mode ((staticity Static) (erasure Unerased))))))
              (arg_mode ((staticity Static) (erasure Unerased))) (memo <opaque>)
              (body
               (Lambda (arg x) (erased Unerased)
                (arg_mode ((staticity ()) (erasure ())))
                (arg_ty
                 (Apply (fn (Var (id g) (loc ((line 3) (column 61)))))
                  (arg (Literal (value (Int 0)) (loc ((line 3) (column 63)))))
                  (loc ((line 3) (column 61)))))
                (body (Var (id x) (loc ((line 3) (column 69)))))
                (loc ((line 3) (column 53)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (If
         (cond
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 11)))))
         (then_
          (Var (id f)
           (ty
            (Type
             (Pi
              (arg_ty
               (Type
                (Pi (arg_ty (Type Int))
                 (arg_mode ((staticity Static) (erasure Unerased)))
                 (ret_ty (T (Type Type)))
                 (ret_mode ((staticity Static) (erasure Unerased))))))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty
               (Typecheck (env <opaque>) (arg g)
                (arg_ty
                 (Type
                  (Pi (arg_ty (Type Int))
                   (arg_mode ((staticity Static) (erasure Unerased)))
                   (ret_ty (T (Type Type)))
                   (ret_mode ((staticity Static) (erasure Unerased))))))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (memo <opaque>)
                (body
                 (Lambda (arg x) (erased Unerased)
                  (arg_mode ((staticity ()) (erasure ())))
                  (arg_ty
                   (Apply (fn (Var (id g) (loc ((line 2) (column 61)))))
                    (arg (Literal (value (Int 0)) (loc ((line 2) (column 63)))))
                    (loc ((line 2) (column 61)))))
                  (body (Var (id x) (loc ((line 2) (column 69)))))
                  (loc ((line 2) (column 53)))))))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 21)))))
         (else_
          (Var (id h)
           (ty
            (Type
             (Pi
              (arg_ty
               (Type
                (Pi (arg_ty (Type Int))
                 (arg_mode ((staticity Static) (erasure Unerased)))
                 (ret_ty (T (Type Type)))
                 (ret_mode ((staticity Static) (erasure Unerased))))))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty
               (Typecheck (env <opaque>) (arg g)
                (arg_ty
                 (Type
                  (Pi (arg_ty (Type Int))
                   (arg_mode ((staticity Static) (erasure Unerased)))
                   (ret_ty (T (Type Type)))
                   (ret_mode ((staticity Static) (erasure Unerased))))))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (memo <opaque>)
                (body
                 (Lambda (arg x) (erased Unerased)
                  (arg_mode ((staticity ()) (erasure ())))
                  (arg_ty
                   (Apply (fn (Var (id g) (loc ((line 3) (column 61)))))
                    (arg (Literal (value (Int 0)) (loc ((line 3) (column 63)))))
                    (loc ((line 3) (column 61)))))
                  (body (Var (id x) (loc ((line 3) (column 69)))))
                  (loc ((line 3) (column 53)))))))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 28)))))
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Pi (arg_ty (Type Int))
               (arg_mode ((staticity Static) (erasure Unerased)))
               (ret_ty (T (Type Type)))
               (ret_mode ((staticity Static) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (Join
              (Typecheck (env <opaque>) (arg g)
               (arg_ty
                (Type
                 (Pi (arg_ty (Type Int))
                  (arg_mode ((staticity Static) (erasure Unerased)))
                  (ret_ty (T (Type Type)))
                  (ret_mode ((staticity Static) (erasure Unerased))))))
               (arg_mode ((staticity Static) (erasure Unerased))) (memo <opaque>)
               (body
                (Lambda (arg x) (erased Unerased)
                 (arg_mode ((staticity ()) (erasure ())))
                 (arg_ty
                  (Apply (fn (Var (id g) (loc ((line 2) (column 61)))))
                   (arg (Literal (value (Int 0)) (loc ((line 2) (column 63)))))
                   (loc ((line 2) (column 61)))))
                 (body (Var (id x) (loc ((line 2) (column 69)))))
                 (loc ((line 2) (column 53))))))
              (Typecheck (env <opaque>) (arg g)
               (arg_ty
                (Type
                 (Pi (arg_ty (Type Int))
                  (arg_mode ((staticity Static) (erasure Unerased)))
                  (ret_ty (T (Type Type)))
                  (ret_mode ((staticity Static) (erasure Unerased))))))
               (arg_mode ((staticity Static) (erasure Unerased))) (memo <opaque>)
               (body
                (Lambda (arg x) (erased Unerased)
                 (arg_mode ((staticity ()) (erasure ())))
                 (arg_ty
                  (Apply (fn (Var (id g) (loc ((line 3) (column 61)))))
                   (arg (Literal (value (Int 0)) (loc ((line 3) (column 63)))))
                   (loc ((line 3) (column 61)))))
                 (body (Var (id x) (loc ((line 3) (column 69)))))
                 (loc ((line 3) (column 53))))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;
