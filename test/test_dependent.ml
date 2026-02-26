open! Core
open! Syl

let go input =
  let cst = Parse.parse_exn input in
  match Typecheck.typecheck cst with
  | Ok tst -> print_s [%message (tst : Tst.Program.t)]
  | Error { loc; reason } -> print_s [%message (loc : Lex.Location.t) (reason : Typecheck.Error.t)]
;;

let%expect_test "static lambda identity returns dependent type" =
  go
    {|
let f = fn (static x : int) -> x;;
let _ = f 42;;
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
         (key (Int 42)) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "static lambda with arithmetic on static arg" =
  go
    {|
let f = fn (static x : int) -> x + 1;;
let _ = f 10;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body
          (Binop (op Add) (lhs (Var (id x) (loc ((line 2) (column 31)))))
           (rhs (Literal (value (Int 1)) (loc ((line 2) (column 35)))))
           (loc ((line 2) (column 33)))))
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
         (key (Int 10)) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "static lambda with boolean op on static arg" =
  go
    {|
let f = fn (static x : bool) -> x && true;;
let _ = f false;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body
          (Binop (op And) (lhs (Var (id x) (loc ((line 2) (column 32)))))
           (rhs (Literal (value (Bool true)) (loc ((line 2) (column 37)))))
           (loc ((line 2) (column 34)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Bool))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Bool)))
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
             (Pi (arg_ty (Type Bool))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Bool)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 8)))))
         (key (Bool false)) (ty (Type Bool))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "nested static lambdas" =
  go
    {|
let f = fn (static x : int) -> fn (static y : int) -> x + y;;
let _ = f 1 2;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body
          (Lambda (arg y) (erased Unerased)
           (arg_mode ((staticity (Static)) (erasure ())))
           (arg_ty (Var (id int) (loc ((line 2) (column 46)))))
           (body
            (Binop (op Add) (lhs (Var (id x) (loc ((line 2) (column 54)))))
             (rhs (Var (id y) (loc ((line 2) (column 58)))))
             (loc ((line 2) (column 56)))))
           (loc ((line 2) (column 31)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (T
              (Type
               (Pi (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (ret_ty (T (Type Int)))
                (ret_mode ((staticity Static) (erasure Unerased)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn
          (Symbol
           (fn
            (Var (id f)
             (ty
              (Type
               (Pi (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (ret_ty
                 (T
                  (Type
                   (Pi (arg_ty (Type Int))
                    (arg_mode ((staticity Static) (erasure Unerased)))
                    (ret_ty (T (Type Int)))
                    (ret_mode ((staticity Static) (erasure Unerased)))))))
                (ret_mode ((staticity Static) (erasure Unerased))))))
             (mode ((staticity Static) (erasure Unerased)))
             (loc ((line 3) (column 8)))))
           (key (Int 1))
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Int)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 8)))))
         (key (Int 2)) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "static lambda returning static lambda" =
  go
    {|
let f = fn (static x : int) -> fn (static y : int) -> x;;
let g = f 1;;
let _ = g 2;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body
          (Lambda (arg y) (erased Unerased)
           (arg_mode ((staticity (Static)) (erasure ())))
           (arg_ty (Var (id int) (loc ((line 2) (column 46)))))
           (body (Var (id x) (loc ((line 2) (column 54)))))
           (loc ((line 2) (column 31)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (T
              (Type
               (Pi (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (ret_ty (T (Type Int)))
                (ret_mode ((staticity Static) (erasure Unerased)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Symbol
         (fn
          (Var (id f)
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty
               (T
                (Type
                 (Pi (arg_ty (Type Int))
                  (arg_mode ((staticity Static) (erasure Unerased)))
                  (ret_ty (T (Type Int)))
                  (ret_mode ((staticity Static) (erasure Unerased)))))))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 8)))))
         (key (Int 1))
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
        (Symbol
         (fn
          (Var (id g)
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Int)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 8)))))
         (key (Int 2)) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "static lambda mixed with dynamic lambda" =
  go
    {|
let f = fn (static x : int) -> fn (y : int) -> y;;
let g = f 1;;
let _ = g 2;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body
          (Lambda (arg y) (erased Unerased)
           (arg_mode ((staticity ()) (erasure ())))
           (arg_ty (Var (id int) (loc ((line 2) (column 39)))))
           (body (Var (id y) (loc ((line 2) (column 47)))))
           (loc ((line 2) (column 31)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (T
              (Type
               (Arrow (arg_ty (Type Int))
                (arg_mode ((staticity Dynamic) (erasure Unerased)))
                (ret_ty (Type Int))
                (ret_mode ((staticity Dynamic) (erasure Unerased)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Symbol
         (fn
          (Var (id f)
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty
               (T
                (Type
                 (Arrow (arg_ty (Type Int))
                  (arg_mode ((staticity Dynamic) (erasure Unerased)))
                  (ret_ty (Type Int))
                  (ret_mode ((staticity Dynamic) (erasure Unerased)))))))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 8)))))
         (key (Int 1))
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
        (Apply
         (fn
          (Var (id g)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 8)))))
         (arg
          (Literal (value (Int (T 2))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 10)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "dynamic lambda inside static lambda uses static arg as type" =
  go
    {|
let f = fn (static erased t : type) -> fn (x : t) -> x;;
let g = f int;;
let _ = g 42;;
let h = f bool;;
let _ = h true;;
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
        (Symbol
         (fn
          (Var (id f)
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
           (loc ((line 3) (column 8)))))
         (key IntT)
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
        (Apply
         (fn
          (Var (id g)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 8)))))
         (arg
          (Literal (value (Int (T 42))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 10)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))
      (Let (var h)
       (bind
        (Symbol
         (fn
          (Var (id f)
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
           (loc ((line 5) (column 8)))))
         (key BoolT)
         (ty
          (Type
           (Arrow (arg_ty (Type Bool))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Bool))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 5) (column 8)))))
       (loc ((line 5) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id h)
           (ty
            (Type
             (Arrow (arg_ty (Type Bool))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Bool))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 6) (column 8)))))
         (arg
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 6) (column 10)))))
         (ty (Type Bool)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 6) (column 8)))))
       (loc ((line 6) (column 0))))))
    |}]
;;

let%expect_test "passing dynamic value to static arg fails" =
  go
    {|
let f = fn (static x : int) -> x;;
let y = 1 @ dynamic;;
let _ = f y;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "static lambda arg type must be a type" =
  go
    {|
let f = fn (static x : 0) -> 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 8)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Type)))))
    |}]
;;

let%expect_test "erased (not static) arg cannot be used as type" =
  go
    {|
let f = fn (erased ty : type) -> fn (x : ty) -> x;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 33)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "if static with literal condition true" =
  go
    {|
let _ = if static true then 1 else true;;
|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 28)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "if static with literal condition false" =
  go
    {|
let _ = if static false then 1 else true;;
|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (Literal (value (Bool (T true))) (ty (Type Bool))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 36)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "if static with static variable condition" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else true;;
let a = f 0;;
let b = f 1;;
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
           (else_ (Literal (value (Bool true)) (loc ((line 2) (column 60)))))
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
                 (Literal (value (Bool true)) (loc ((line 2) (column 60)))))
                (static true) (loc ((line 2) (column 31)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var a)
       (bind
        (Symbol
         (fn
          (Var (id f)
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty
               (Typecheck (env <opaque>) (arg x) (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (memo <opaque>)
                (body
                 (If
                  (cond
                   (Binop (op Eq) (lhs (Var (id x) (loc ((line 2) (column 41)))))
                    (rhs (Literal (value (Int 0)) (loc ((line 2) (column 46)))))
                    (loc ((line 2) (column 43)))))
                  (then_ (Literal (value (Int 1)) (loc ((line 2) (column 53)))))
                  (else_
                   (Literal (value (Bool true)) (loc ((line 2) (column 60)))))
                  (static true) (loc ((line 2) (column 31)))))))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 8)))))
         (key (Int 0)) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var b)
       (bind
        (Symbol
         (fn
          (Var (id f)
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty
               (Typecheck (env <opaque>) (arg x) (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (memo <opaque>)
                (body
                 (If
                  (cond
                   (Binop (op Eq) (lhs (Var (id x) (loc ((line 2) (column 41)))))
                    (rhs (Literal (value (Int 0)) (loc ((line 2) (column 46)))))
                    (loc ((line 2) (column 43)))))
                  (then_ (Literal (value (Int 1)) (loc ((line 2) (column 53)))))
                  (else_
                   (Literal (value (Bool true)) (loc ((line 2) (column 60)))))
                  (static true) (loc ((line 2) (column 31)))))))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 8)))))
         (key (Int 1)) (ty (Type Bool))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "if static with mismatched branch types without annotation" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else true;;
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
           (else_ (Literal (value (Bool true)) (loc ((line 2) (column 60)))))
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
                 (Literal (value (Bool true)) (loc ((line 2) (column 60)))))
                (static true) (loc ((line 2) (column 31)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "if static with correct type annotation using non-static if" =
  go
    {|
let f = fn (static x : int) -> (if static x == 0 then 1 else true) : (if x == 0 then int else bool);;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body
          (Type_annotation
           (expr
            (Paren
             (expr
              (If
               (cond
                (Binop (op Eq) (lhs (Var (id x) (loc ((line 2) (column 42)))))
                 (rhs (Literal (value (Int 0)) (loc ((line 2) (column 47)))))
                 (loc ((line 2) (column 44)))))
               (then_ (Literal (value (Int 1)) (loc ((line 2) (column 54)))))
               (else_ (Literal (value (Bool true)) (loc ((line 2) (column 61)))))
               (static true) (loc ((line 2) (column 32)))))
             (loc ((line 2) (column 31)))))
           (ty
            (Paren
             (expr
              (If
               (cond
                (Binop (op Eq) (lhs (Var (id x) (loc ((line 2) (column 73)))))
                 (rhs (Literal (value (Int 0)) (loc ((line 2) (column 78)))))
                 (loc ((line 2) (column 75)))))
               (then_ (Var (id int) (loc ((line 2) (column 85)))))
               (else_ (Var (id bool) (loc ((line 2) (column 94)))))
               (static false) (loc ((line 2) (column 70)))))
             (loc ((line 2) (column 69)))))
           (loc ((line 2) (column 67)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (Typecheck (env <opaque>) (arg x) (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased))) (memo <opaque>)
              (body
               (Type_annotation
                (expr
                 (Paren
                  (expr
                   (If
                    (cond
                     (Binop (op Eq)
                      (lhs (Var (id x) (loc ((line 2) (column 42)))))
                      (rhs
                       (Literal (value (Int 0)) (loc ((line 2) (column 47)))))
                      (loc ((line 2) (column 44)))))
                    (then_
                     (Literal (value (Int 1)) (loc ((line 2) (column 54)))))
                    (else_
                     (Literal (value (Bool true)) (loc ((line 2) (column 61)))))
                    (static true) (loc ((line 2) (column 32)))))
                  (loc ((line 2) (column 31)))))
                (ty
                 (Paren
                  (expr
                   (If
                    (cond
                     (Binop (op Eq)
                      (lhs (Var (id x) (loc ((line 2) (column 73)))))
                      (rhs
                       (Literal (value (Int 0)) (loc ((line 2) (column 78)))))
                      (loc ((line 2) (column 75)))))
                    (then_ (Var (id int) (loc ((line 2) (column 85)))))
                    (else_ (Var (id bool) (loc ((line 2) (column 94)))))
                    (static false) (loc ((line 2) (column 70)))))
                  (loc ((line 2) (column 69)))))
                (loc ((line 2) (column 67)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "if (non-static) requires branches to unify" =
  go
    {|
let f = fn (static x : int) -> if x == 0 then 1 else true;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 31)))
     (reason (Cannot_unify (lhs (Type Int)) (rhs (Type Bool)))))
    |}]
;;

let%expect_test "if static wrong annotation type" =
  go
    {|
let f = fn (static x : int) -> (if static x == 0 then 1 else true) : int;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 67)))
     (reason
      (Type_mismatch
       (got
        (If (cond (Bool (Eq (Var $0) (Int (T 0))))) (then_ (Type Int))
         (else_ (Type Bool))))
       (need (Type Int)))))
    |}]
;;

let%expect_test
    "if static annotation uses static if — fails because type-level if static produces If of types"
  =
  go
    {|
let f = fn (static x : int) -> (if static x == 0 then 1 else true) : (if static x == 0 then int else bool);;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body
          (Type_annotation
           (expr
            (Paren
             (expr
              (If
               (cond
                (Binop (op Eq) (lhs (Var (id x) (loc ((line 2) (column 42)))))
                 (rhs (Literal (value (Int 0)) (loc ((line 2) (column 47)))))
                 (loc ((line 2) (column 44)))))
               (then_ (Literal (value (Int 1)) (loc ((line 2) (column 54)))))
               (else_ (Literal (value (Bool true)) (loc ((line 2) (column 61)))))
               (static true) (loc ((line 2) (column 32)))))
             (loc ((line 2) (column 31)))))
           (ty
            (Paren
             (expr
              (If
               (cond
                (Binop (op Eq) (lhs (Var (id x) (loc ((line 2) (column 80)))))
                 (rhs (Literal (value (Int 0)) (loc ((line 2) (column 85)))))
                 (loc ((line 2) (column 82)))))
               (then_ (Var (id int) (loc ((line 2) (column 92)))))
               (else_ (Var (id bool) (loc ((line 2) (column 101)))))
               (static true) (loc ((line 2) (column 70)))))
             (loc ((line 2) (column 69)))))
           (loc ((line 2) (column 67)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (Typecheck (env <opaque>) (arg x) (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased))) (memo <opaque>)
              (body
               (Type_annotation
                (expr
                 (Paren
                  (expr
                   (If
                    (cond
                     (Binop (op Eq)
                      (lhs (Var (id x) (loc ((line 2) (column 42)))))
                      (rhs
                       (Literal (value (Int 0)) (loc ((line 2) (column 47)))))
                      (loc ((line 2) (column 44)))))
                    (then_
                     (Literal (value (Int 1)) (loc ((line 2) (column 54)))))
                    (else_
                     (Literal (value (Bool true)) (loc ((line 2) (column 61)))))
                    (static true) (loc ((line 2) (column 32)))))
                  (loc ((line 2) (column 31)))))
                (ty
                 (Paren
                  (expr
                   (If
                    (cond
                     (Binop (op Eq)
                      (lhs (Var (id x) (loc ((line 2) (column 80)))))
                      (rhs
                       (Literal (value (Int 0)) (loc ((line 2) (column 85)))))
                      (loc ((line 2) (column 82)))))
                    (then_ (Var (id int) (loc ((line 2) (column 92)))))
                    (else_ (Var (id bool) (loc ((line 2) (column 101)))))
                    (static true) (loc ((line 2) (column 70)))))
                  (loc ((line 2) (column 69)))))
                (loc ((line 2) (column 67)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "if static with nested dependent types in branches" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then fn (y : int) -> y else fn (y : bool) -> y;;
let g = f 0;;
let _ = g 42;;
let h = f 1;;
let _ = h true;;
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
           (then_
            (Lambda (arg y) (erased Unerased)
             (arg_mode ((staticity ()) (erasure ())))
             (arg_ty (Var (id int) (loc ((line 2) (column 61)))))
             (body (Var (id y) (loc ((line 2) (column 69)))))
             (loc ((line 2) (column 53)))))
           (else_
            (Lambda (arg y) (erased Unerased)
             (arg_mode ((staticity ()) (erasure ())))
             (arg_ty (Var (id bool) (loc ((line 2) (column 84)))))
             (body (Var (id y) (loc ((line 2) (column 93)))))
             (loc ((line 2) (column 76)))))
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
                (then_
                 (Lambda (arg y) (erased Unerased)
                  (arg_mode ((staticity ()) (erasure ())))
                  (arg_ty (Var (id int) (loc ((line 2) (column 61)))))
                  (body (Var (id y) (loc ((line 2) (column 69)))))
                  (loc ((line 2) (column 53)))))
                (else_
                 (Lambda (arg y) (erased Unerased)
                  (arg_mode ((staticity ()) (erasure ())))
                  (arg_ty (Var (id bool) (loc ((line 2) (column 84)))))
                  (body (Var (id y) (loc ((line 2) (column 93)))))
                  (loc ((line 2) (column 76)))))
                (static true) (loc ((line 2) (column 31)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Symbol
         (fn
          (Var (id f)
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty
               (Typecheck (env <opaque>) (arg x) (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (memo <opaque>)
                (body
                 (If
                  (cond
                   (Binop (op Eq) (lhs (Var (id x) (loc ((line 2) (column 41)))))
                    (rhs (Literal (value (Int 0)) (loc ((line 2) (column 46)))))
                    (loc ((line 2) (column 43)))))
                  (then_
                   (Lambda (arg y) (erased Unerased)
                    (arg_mode ((staticity ()) (erasure ())))
                    (arg_ty (Var (id int) (loc ((line 2) (column 61)))))
                    (body (Var (id y) (loc ((line 2) (column 69)))))
                    (loc ((line 2) (column 53)))))
                  (else_
                   (Lambda (arg y) (erased Unerased)
                    (arg_mode ((staticity ()) (erasure ())))
                    (arg_ty (Var (id bool) (loc ((line 2) (column 84)))))
                    (body (Var (id y) (loc ((line 2) (column 93)))))
                    (loc ((line 2) (column 76)))))
                  (static true) (loc ((line 2) (column 31)))))))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 8)))))
         (key (Int 0))
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
        (Apply
         (fn
          (Var (id g)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 8)))))
         (arg
          (Literal (value (Int (T 42))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 10)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))
      (Let (var h)
       (bind
        (Symbol
         (fn
          (Var (id f)
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty
               (Typecheck (env <opaque>) (arg x) (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (memo <opaque>)
                (body
                 (If
                  (cond
                   (Binop (op Eq) (lhs (Var (id x) (loc ((line 2) (column 41)))))
                    (rhs (Literal (value (Int 0)) (loc ((line 2) (column 46)))))
                    (loc ((line 2) (column 43)))))
                  (then_
                   (Lambda (arg y) (erased Unerased)
                    (arg_mode ((staticity ()) (erasure ())))
                    (arg_ty (Var (id int) (loc ((line 2) (column 61)))))
                    (body (Var (id y) (loc ((line 2) (column 69)))))
                    (loc ((line 2) (column 53)))))
                  (else_
                   (Lambda (arg y) (erased Unerased)
                    (arg_mode ((staticity ()) (erasure ())))
                    (arg_ty (Var (id bool) (loc ((line 2) (column 84)))))
                    (body (Var (id y) (loc ((line 2) (column 93)))))
                    (loc ((line 2) (column 76)))))
                  (static true) (loc ((line 2) (column 31)))))))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 5) (column 8)))))
         (key (Int 1))
         (ty
          (Type
           (Arrow (arg_ty (Type Bool))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Bool))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 5) (column 8)))))
       (loc ((line 5) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id h)
           (ty
            (Type
             (Arrow (arg_ty (Type Bool))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Bool))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 6) (column 8)))))
         (arg
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 6) (column 10)))))
         (ty (Type Bool)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 6) (column 8)))))
       (loc ((line 6) (column 0))))))
    |}]
;;

let%expect_test "if static true selects then branch type" =
  go
    {|
let _ = (if static true then 1 else true) : (if true then int else bool);;
|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 29)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "if static false selects else branch type" =
  go
    {|
let _ = (if static false then 1 else true) : (if false then int else bool);;
|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (Literal (value (Bool (T true))) (ty (Type Bool))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 37)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "dependent arrow type with backslash binder" =
  go
    {|
let f = fn (static g : static int \ x -> int) -> g 0;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg g)
         (body
          (Apply (fn (Var (id g) (loc ((line 2) (column 49)))))
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
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "dependent arrow applied to matching function" =
  go
    {|
let apply_type = fn (static f : static erased type \ t -> t -> t) -> f int;;
let _ = apply_type (fn (static erased t : type) -> fn (x : t) -> x);;
|};
  [%expect
    {|
    (tst
     ((Let (var apply_type)
       (bind
        (Binder (arg f)
         (body
          (Apply (fn (Var (id f) (loc ((line 2) (column 69)))))
           (arg (Var (id int) (loc ((line 2) (column 71)))))
           (loc ((line 2) (column 69)))))
         (mono <opaque>)
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
                 (ret_ty
                  (Arrow (arg (Var (id t) (loc ((line 2) (column 58)))))
                   (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
                   (ret (Var (id t) (loc ((line 2) (column 63)))))
                   (ret_mode ((staticity ()) (erasure ())))
                   (loc ((line 2) (column 60)))))))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (T
              (Type
               (Arrow (arg_ty (Type Int))
                (arg_mode ((staticity Dynamic) (erasure Unerased)))
                (ret_ty (Type Int))
                (ret_mode ((staticity Dynamic) (erasure Unerased)))))))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 17)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn
          (Var (id apply_type)
           (ty
            (Type
             (Pi
              (arg_ty
               (Type
                (Pi (arg_ty (Type Type))
                 (arg_mode ((staticity Static) (erasure Erased)))
                 (ret_ty
                  (Reduce (env <opaque>) (arg t) (arg_ty (Type Type))
                   (arg_mode ((staticity Static) (erasure Erased)))
                   (memo <opaque>)
                   (ret_ty
                    (Arrow (arg (Var (id t) (loc ((line 2) (column 58)))))
                     (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
                     (ret (Var (id t) (loc ((line 2) (column 63)))))
                     (ret_mode ((staticity ()) (erasure ())))
                     (loc ((line 2) (column 60)))))))
                 (ret_mode ((staticity Dynamic) (erasure Unerased))))))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty
               (T
                (Type
                 (Arrow (arg_ty (Type Int))
                  (arg_mode ((staticity Dynamic) (erasure Unerased)))
                  (ret_ty (Type Int))
                  (ret_mode ((staticity Dynamic) (erasure Unerased)))))))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 8)))))
         (key (Closure 4))
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "dependent arrow with return type depending on arg" =
  go
    {|
let mk_int = fn (static x : int) -> int;;
let f = fn (static g : static int \ x -> mk_int x) -> g 0;;
|};
  [%expect
    {|
    (tst
     ((Let (var mk_int)
       (bind
        (Binder (arg x) (body (Var (id int) (loc ((line 2) (column 36)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Type)))
            (ret_mode ((staticity Static) (erasure Erased))))))
         (mode ((staticity Static) (erasure Erased)))
         (loc ((line 2) (column 13)))))
       (loc ((line 2) (column 0))))
      (Let (var f)
       (bind
        (Binder (arg g)
         (body
          (Apply (fn (Var (id g) (loc ((line 3) (column 54)))))
           (arg (Literal (value (Int 0)) (loc ((line 3) (column 56)))))
           (loc ((line 3) (column 54)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Pi (arg_ty (Type Int))
               (arg_mode ((staticity Static) (erasure Unerased)))
               (ret_ty
                (Reduce (env <opaque>) (arg x) (arg_ty (Type Int))
                 (arg_mode ((staticity Static) (erasure Unerased)))
                 (memo <opaque>)
                 (ret_ty
                  (Apply (fn (Var (id mk_int) (loc ((line 3) (column 41)))))
                   (arg (Var (id x) (loc ((line 3) (column 48)))))
                   (loc ((line 3) (column 41)))))))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "fun with static arg creates Pi type" =
  go
    {|
fun f (static x : int) : int = x;;
let _ = f 0;;
|};
  [%expect
    {|
    (tst
     ((Fun
       (funs
        ((Binder (var f) (arg x) (body (Var (id x) (loc ((line 2) (column 31)))))
          (mono <opaque>)
          (ty
           (Type
            (Pi (arg_ty (Type Int))
             (arg_mode ((staticity Static) (erasure Unerased)))
             (ret_ty (T (Type Int)))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Unerased)))
          (loc ((line 2) (column 4))))))
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
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 8)))))
         (key (Int 0)) (ty (Type Int))
         (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "fun with static erased type arg — polymorphic identity" =
  go
    {|
fun id (static erased t : type) : t -> t = fn (x : t) -> x;;
let _ = id int 0;;
let _ = id bool true;;
|};
  [%expect
    {|
    (tst
     ((Fun
       (funs
        ((Binder (var id) (arg t)
          (body
           (Lambda (arg x) (erased Unerased)
            (arg_mode ((staticity ()) (erasure ())))
            (arg_ty (Var (id t) (loc ((line 2) (column 51)))))
            (body (Var (id x) (loc ((line 2) (column 57)))))
            (loc ((line 2) (column 43)))))
          (mono <opaque>)
          (ty
           (Type
            (Pi (arg_ty (Type Type))
             (arg_mode ((staticity Static) (erasure Erased)))
             (ret_ty
              (Reduce (env <opaque>) (arg t) (arg_ty (Type Type))
               (arg_mode ((staticity Static) (erasure Erased))) (memo <opaque>)
               (ret_ty
                (Arrow (arg (Var (id t) (loc ((line 2) (column 34)))))
                 (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
                 (ret (Var (id t) (loc ((line 2) (column 39)))))
                 (ret_mode ((staticity ()) (erasure ())))
                 (loc ((line 2) (column 36)))))))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Unerased)))
          (loc ((line 2) (column 4))))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Symbol
           (fn
            (Var (id id)
             (ty
              (Type
               (Pi (arg_ty (Type Type))
                (arg_mode ((staticity Static) (erasure Erased)))
                (ret_ty
                 (Reduce (env <opaque>) (arg t) (arg_ty (Type Type))
                  (arg_mode ((staticity Static) (erasure Erased)))
                  (memo <opaque>)
                  (ret_ty
                   (Arrow (arg (Var (id t) (loc ((line 2) (column 34)))))
                    (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
                    (ret (Var (id t) (loc ((line 2) (column 39)))))
                    (ret_mode ((staticity ()) (erasure ())))
                    (loc ((line 2) (column 36)))))))
                (ret_mode ((staticity Dynamic) (erasure Unerased))))))
             (mode ((staticity Static) (erasure Unerased)))
             (loc ((line 3) (column 8)))))
           (key IntT)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Dynamic) (erasure Unerased)))
           (loc ((line 3) (column 8)))))
         (arg
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 15)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Symbol
           (fn
            (Var (id id)
             (ty
              (Type
               (Pi (arg_ty (Type Type))
                (arg_mode ((staticity Static) (erasure Erased)))
                (ret_ty
                 (Reduce (env <opaque>) (arg t) (arg_ty (Type Type))
                  (arg_mode ((staticity Static) (erasure Erased)))
                  (memo <opaque>)
                  (ret_ty
                   (Arrow (arg (Var (id t) (loc ((line 2) (column 34)))))
                    (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
                    (ret (Var (id t) (loc ((line 2) (column 39)))))
                    (ret_mode ((staticity ()) (erasure ())))
                    (loc ((line 2) (column 36)))))))
                (ret_mode ((staticity Dynamic) (erasure Unerased))))))
             (mode ((staticity Static) (erasure Unerased)))
             (loc ((line 4) (column 8)))))
           (key BoolT)
           (ty
            (Type
             (Arrow (arg_ty (Type Bool))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Bool))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Dynamic) (erasure Unerased)))
           (loc ((line 4) (column 8)))))
         (arg
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 16)))))
         (ty (Type Bool)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "fun with erased (non-static) type arg fails for dependent ret type" =
  go
    {|
fun id (erased t : type) : t -> t = fn (x : t) -> x;;
|};
  [%expect {| ((loc ((line 2) (column 27))) (reason (Unbound_ident t))) |}]
;;

let%expect_test "dynamic app" =
  go
    {|
fun f (x : int) : static int = 0;;
let _ = 0 : if f 0 == 0 then int else bool;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 4)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "fun dynamic recursion is allowed" =
  go
    {|
fun f (x : int) : int = f x;;
|};
  [%expect
    {|
    (tst
     ((Fun
       (funs
        ((Lambda (var f) (arg x)
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
              (mode ((staticity Static) (erasure Unerased)))
              (loc ((line 2) (column 24)))))
            (arg
             (Var (id x) (ty (Type Int))
              (mode ((staticity Dynamic) (erasure Unerased)))
              (loc ((line 2) (column 26)))))
            (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
            (loc ((line 2) (column 24)))))
          (ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Unerased)))
          (loc ((line 2) (column 4))))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "static cond" =
  go
    {|
let c = true @ dynamic;;
let _ = if static c then 0 else 1;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "fun with static arg, passing dynamic value fails" =
  go
    {|
fun f (static x : int) : int = x;;
let y = 0 @ dynamic;;
let _ = f y;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "fun with static erased type arg, two sequential funs" =
  go
    {|
fun id1 (static erased t : type) : t -> t = fn (x : t) -> x;;
fun id2 (static erased t : type) : t -> t = id1 t;;
let _ = id2 int 0;;
|};
  [%expect
    {|
    (tst
     ((Fun
       (funs
        ((Binder (var id1) (arg t)
          (body
           (Lambda (arg x) (erased Unerased)
            (arg_mode ((staticity ()) (erasure ())))
            (arg_ty (Var (id t) (loc ((line 2) (column 52)))))
            (body (Var (id x) (loc ((line 2) (column 58)))))
            (loc ((line 2) (column 44)))))
          (mono <opaque>)
          (ty
           (Type
            (Pi (arg_ty (Type Type))
             (arg_mode ((staticity Static) (erasure Erased)))
             (ret_ty
              (Reduce (env <opaque>) (arg t) (arg_ty (Type Type))
               (arg_mode ((staticity Static) (erasure Erased))) (memo <opaque>)
               (ret_ty
                (Arrow (arg (Var (id t) (loc ((line 2) (column 35)))))
                 (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
                 (ret (Var (id t) (loc ((line 2) (column 40)))))
                 (ret_mode ((staticity ()) (erasure ())))
                 (loc ((line 2) (column 37)))))))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Unerased)))
          (loc ((line 2) (column 4))))))
       (loc ((line 2) (column 0))))
      (Fun
       (funs
        ((Binder (var id2) (arg t)
          (body
           (Apply (fn (Var (id id1) (loc ((line 3) (column 44)))))
            (arg (Var (id t) (loc ((line 3) (column 48)))))
            (loc ((line 3) (column 44)))))
          (mono <opaque>)
          (ty
           (Type
            (Pi (arg_ty (Type Type))
             (arg_mode ((staticity Static) (erasure Erased)))
             (ret_ty
              (Reduce (env <opaque>) (arg t) (arg_ty (Type Type))
               (arg_mode ((staticity Static) (erasure Erased))) (memo <opaque>)
               (ret_ty
                (Arrow (arg (Var (id t) (loc ((line 3) (column 35)))))
                 (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
                 (ret (Var (id t) (loc ((line 3) (column 40)))))
                 (ret_mode ((staticity ()) (erasure ())))
                 (loc ((line 3) (column 37)))))))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Unerased)))
          (loc ((line 3) (column 4))))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Symbol
           (fn
            (Var (id id2)
             (ty
              (Type
               (Pi (arg_ty (Type Type))
                (arg_mode ((staticity Static) (erasure Erased)))
                (ret_ty
                 (Reduce (env <opaque>) (arg t) (arg_ty (Type Type))
                  (arg_mode ((staticity Static) (erasure Erased)))
                  (memo <opaque>)
                  (ret_ty
                   (Arrow (arg (Var (id t) (loc ((line 3) (column 35)))))
                    (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
                    (ret (Var (id t) (loc ((line 3) (column 40)))))
                    (ret_mode ((staticity ()) (erasure ())))
                    (loc ((line 3) (column 37)))))))
                (ret_mode ((staticity Dynamic) (erasure Unerased))))))
             (mode ((staticity Static) (erasure Unerased)))
             (loc ((line 4) (column 8)))))
           (key IntT)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Dynamic) (erasure Unerased)))
           (loc ((line 4) (column 8)))))
         (arg
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 16)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "static erased lambda captures no runtime value" =
  go
    {|
let f = fn (static erased x : int) -> 0;;
let _ = f 1;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body (Literal (value (Int 0)) (loc ((line 2) (column 38)))))
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
        (Symbol
         (fn
          (Var (id f)
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Erased)))
              (ret_ty (T (Type Int)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 8)))))
         (key (Int 1)) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "lift static value through Pi" =
  go
    {|
let f = fn (static x : int) -> x @ erased;;
let _ = f 0;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body
          (Mode_annotation (expr (Var (id x) (loc ((line 2) (column 31)))))
           (mode ((staticity ()) (erasure (Erased))))
           (loc ((line 2) (column 33)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
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
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Erased)))
           (loc ((line 2) (column 33)))))
         (ty (Type Int)) (mode ((staticity Static) (erasure Erased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "cannot return static from function with dynamic arg via fun" =
  go
    {|
fun f (x : int) : static int = x;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 4)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "fun returning static erased type" =
  go
    {|
fun f (static _ : unit) : static erased type = int;;
let _ = 5 : f ();;
|};
  [%expect
    {|
    (tst
     ((Fun
       (funs
        ((Binder (var f) (arg _)
          (body (Var (id int) (loc ((line 2) (column 47))))) (mono <opaque>)
          (ty
           (Type
            (Pi (arg_ty (Type Unit))
             (arg_mode ((staticity Static) (erasure Unerased)))
             (ret_ty (T (Type Type)))
             (ret_mode ((staticity Static) (erasure Erased))))))
          (mode ((staticity Static) (erasure Erased)))
          (loc ((line 2) (column 4))))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Literal (value (Int (T 5))) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "fun returning static erased type, used as annotation" =
  go
    {|
fun f (static _ : unit) : static erased type = int;;
let _ = true : f ();;
|};
  [%expect
    {|
    ((loc ((line 3) (column 13)))
     (reason (Type_mismatch (got (Type Bool)) (need (Type Int)))))
    |}]
;;

let%expect_test "pi and arrow join — if choosing between Pi and Arrow" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else true;;
let g = fn (static x : int) -> if static x == 0 then 1 else true;;
let _ = if true then f else g;;
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
           (else_ (Literal (value (Bool true)) (loc ((line 2) (column 60)))))
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
                 (Literal (value (Bool true)) (loc ((line 2) (column 60)))))
                (static true) (loc ((line 2) (column 31)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Binder (arg x)
         (body
          (If
           (cond
            (Binop (op Eq) (lhs (Var (id x) (loc ((line 3) (column 41)))))
             (rhs (Literal (value (Int 0)) (loc ((line 3) (column 46)))))
             (loc ((line 3) (column 43)))))
           (then_ (Literal (value (Int 1)) (loc ((line 3) (column 53)))))
           (else_ (Literal (value (Bool true)) (loc ((line 3) (column 60)))))
           (static true) (loc ((line 3) (column 31)))))
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
                 (Binop (op Eq) (lhs (Var (id x) (loc ((line 3) (column 41)))))
                  (rhs (Literal (value (Int 0)) (loc ((line 3) (column 46)))))
                  (loc ((line 3) (column 43)))))
                (then_ (Literal (value (Int 1)) (loc ((line 3) (column 53)))))
                (else_
                 (Literal (value (Bool true)) (loc ((line 3) (column 60)))))
                (static true) (loc ((line 3) (column 31)))))))
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
              (ret_ty
               (Typecheck (env <opaque>) (arg x) (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (memo <opaque>)
                (body
                 (If
                  (cond
                   (Binop (op Eq) (lhs (Var (id x) (loc ((line 2) (column 41)))))
                    (rhs (Literal (value (Int 0)) (loc ((line 2) (column 46)))))
                    (loc ((line 2) (column 43)))))
                  (then_ (Literal (value (Int 1)) (loc ((line 2) (column 53)))))
                  (else_
                   (Literal (value (Bool true)) (loc ((line 2) (column 60)))))
                  (static true) (loc ((line 2) (column 31)))))))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 21)))))
         (else_
          (Var (id g)
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty
               (Typecheck (env <opaque>) (arg x) (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (memo <opaque>)
                (body
                 (If
                  (cond
                   (Binop (op Eq) (lhs (Var (id x) (loc ((line 3) (column 41)))))
                    (rhs (Literal (value (Int 0)) (loc ((line 3) (column 46)))))
                    (loc ((line 3) (column 43)))))
                  (then_ (Literal (value (Int 1)) (loc ((line 3) (column 53)))))
                  (else_
                   (Literal (value (Bool true)) (loc ((line 3) (column 60)))))
                  (static true) (loc ((line 3) (column 31)))))))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 28)))))
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (Join
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
                  (Literal (value (Bool true)) (loc ((line 2) (column 60)))))
                 (static true) (loc ((line 2) (column 31))))))
              (Typecheck (env <opaque>) (arg x) (arg_ty (Type Int))
               (arg_mode ((staticity Static) (erasure Unerased))) (memo <opaque>)
               (body
                (If
                 (cond
                  (Binop (op Eq) (lhs (Var (id x) (loc ((line 3) (column 41)))))
                   (rhs (Literal (value (Int 0)) (loc ((line 3) (column 46)))))
                   (loc ((line 3) (column 43)))))
                 (then_ (Literal (value (Int 1)) (loc ((line 3) (column 53)))))
                 (else_
                  (Literal (value (Bool true)) (loc ((line 3) (column 60)))))
                 (static true) (loc ((line 3) (column 31))))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "pi and arrow cannot join if return types differ structurally" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else true;;
let g = fn (static x : int) -> if static x == 0 then true else 2;;
let _ = if true then f else g;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason
      (Cannot_unify
       (lhs
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
              (else_ (Literal (value (Bool true)) (loc ((line 2) (column 60)))))
              (static true) (loc ((line 2) (column 31)))))))
          (ret_mode ((staticity Static) (erasure Unerased))))))
       (rhs
        (Type
         (Pi (arg_ty (Type Int))
          (arg_mode ((staticity Static) (erasure Unerased)))
          (ret_ty
           (Typecheck (env <opaque>) (arg x) (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased))) (memo <opaque>)
            (body
             (If
              (cond
               (Binop (op Eq) (lhs (Var (id x) (loc ((line 3) (column 41)))))
                (rhs (Literal (value (Int 0)) (loc ((line 3) (column 46)))))
                (loc ((line 3) (column 43)))))
              (then_ (Literal (value (Bool true)) (loc ((line 3) (column 53)))))
              (else_ (Literal (value (Int 2)) (loc ((line 3) (column 63)))))
              (static true) (loc ((line 3) (column 31)))))))
          (ret_mode ((staticity Static) (erasure Unerased)))))))))
    |}]
;;

let%expect_test "joining f 0 and g 1 resolves dependent types" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else true;;
let g = fn (static x : int) -> if static x == 0 then true else 2;;
let _ = if true then f 0 else g 1;;
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
           (else_ (Literal (value (Bool true)) (loc ((line 2) (column 60)))))
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
                 (Literal (value (Bool true)) (loc ((line 2) (column 60)))))
                (static true) (loc ((line 2) (column 31)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Binder (arg x)
         (body
          (If
           (cond
            (Binop (op Eq) (lhs (Var (id x) (loc ((line 3) (column 41)))))
             (rhs (Literal (value (Int 0)) (loc ((line 3) (column 46)))))
             (loc ((line 3) (column 43)))))
           (then_ (Literal (value (Bool true)) (loc ((line 3) (column 53)))))
           (else_ (Literal (value (Int 2)) (loc ((line 3) (column 63)))))
           (static true) (loc ((line 3) (column 31)))))
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
                 (Binop (op Eq) (lhs (Var (id x) (loc ((line 3) (column 41)))))
                  (rhs (Literal (value (Int 0)) (loc ((line 3) (column 46)))))
                  (loc ((line 3) (column 43)))))
                (then_
                 (Literal (value (Bool true)) (loc ((line 3) (column 53)))))
                (else_ (Literal (value (Int 2)) (loc ((line 3) (column 63)))))
                (static true) (loc ((line 3) (column 31)))))))
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
          (Symbol
           (fn
            (Var (id f)
             (ty
              (Type
               (Pi (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (ret_ty
                 (Typecheck (env <opaque>) (arg x) (arg_ty (Type Int))
                  (arg_mode ((staticity Static) (erasure Unerased)))
                  (memo <opaque>)
                  (body
                   (If
                    (cond
                     (Binop (op Eq)
                      (lhs (Var (id x) (loc ((line 2) (column 41)))))
                      (rhs
                       (Literal (value (Int 0)) (loc ((line 2) (column 46)))))
                      (loc ((line 2) (column 43)))))
                    (then_
                     (Literal (value (Int 1)) (loc ((line 2) (column 53)))))
                    (else_
                     (Literal (value (Bool true)) (loc ((line 2) (column 60)))))
                    (static true) (loc ((line 2) (column 31)))))))
                (ret_mode ((staticity Static) (erasure Unerased))))))
             (mode ((staticity Static) (erasure Unerased)))
             (loc ((line 4) (column 21)))))
           (key (Int 0)) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 21)))))
         (else_
          (Symbol
           (fn
            (Var (id g)
             (ty
              (Type
               (Pi (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (ret_ty
                 (Typecheck (env <opaque>) (arg x) (arg_ty (Type Int))
                  (arg_mode ((staticity Static) (erasure Unerased)))
                  (memo <opaque>)
                  (body
                   (If
                    (cond
                     (Binop (op Eq)
                      (lhs (Var (id x) (loc ((line 3) (column 41)))))
                      (rhs
                       (Literal (value (Int 0)) (loc ((line 3) (column 46)))))
                      (loc ((line 3) (column 43)))))
                    (then_
                     (Literal (value (Bool true)) (loc ((line 3) (column 53)))))
                    (else_
                     (Literal (value (Int 2)) (loc ((line 3) (column 63)))))
                    (static true) (loc ((line 3) (column 31)))))))
                (ret_mode ((staticity Static) (erasure Unerased))))))
             (mode ((staticity Static) (erasure Unerased)))
             (loc ((line 4) (column 30)))))
           (key (Int 1)) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 30)))))
         (ty (Type Int)) (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "apply static lambda to wrong type" =
  go
    {|
let f = fn (static x : int) -> x;;
let _ = f true;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason (Type_mismatch (got (Type Bool)) (need (Type Int)))))
    |}]
;;

let%expect_test "nested if static with different types per level" =
  go
    {|
let f = fn (static x : int) -> fn (static y : int) ->
  if static x == 0 then
    (if static y == 0 then 1 else true)
  else
    (if static y == 0 then () else 2);;
let _ = f 0 0;;
let _ = f 0 1;;
let _ = f 1 0;;
let _ = f 1 1;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body
          (Lambda (arg y) (erased Unerased)
           (arg_mode ((staticity (Static)) (erasure ())))
           (arg_ty (Var (id int) (loc ((line 2) (column 46)))))
           (body
            (If
             (cond
              (Binop (op Eq) (lhs (Var (id x) (loc ((line 3) (column 12)))))
               (rhs (Literal (value (Int 0)) (loc ((line 3) (column 17)))))
               (loc ((line 3) (column 14)))))
             (then_
              (Paren
               (expr
                (If
                 (cond
                  (Binop (op Eq) (lhs (Var (id y) (loc ((line 4) (column 15)))))
                   (rhs (Literal (value (Int 0)) (loc ((line 4) (column 20)))))
                   (loc ((line 4) (column 17)))))
                 (then_ (Literal (value (Int 1)) (loc ((line 4) (column 27)))))
                 (else_
                  (Literal (value (Bool true)) (loc ((line 4) (column 34)))))
                 (static true) (loc ((line 4) (column 5)))))
               (loc ((line 4) (column 4)))))
             (else_
              (Paren
               (expr
                (If
                 (cond
                  (Binop (op Eq) (lhs (Var (id y) (loc ((line 6) (column 15)))))
                   (rhs (Literal (value (Int 0)) (loc ((line 6) (column 20)))))
                   (loc ((line 6) (column 17)))))
                 (then_ (Literal (value Unit) (loc ((line 6) (column 27)))))
                 (else_ (Literal (value (Int 2)) (loc ((line 6) (column 35)))))
                 (static true) (loc ((line 6) (column 5)))))
               (loc ((line 6) (column 4)))))
             (static true) (loc ((line 3) (column 2)))))
           (loc ((line 2) (column 31)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (Typecheck (env <opaque>) (arg x) (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased))) (memo <opaque>)
              (body
               (Lambda (arg y) (erased Unerased)
                (arg_mode ((staticity (Static)) (erasure ())))
                (arg_ty (Var (id int) (loc ((line 2) (column 46)))))
                (body
                 (If
                  (cond
                   (Binop (op Eq) (lhs (Var (id x) (loc ((line 3) (column 12)))))
                    (rhs (Literal (value (Int 0)) (loc ((line 3) (column 17)))))
                    (loc ((line 3) (column 14)))))
                  (then_
                   (Paren
                    (expr
                     (If
                      (cond
                       (Binop (op Eq)
                        (lhs (Var (id y) (loc ((line 4) (column 15)))))
                        (rhs
                         (Literal (value (Int 0)) (loc ((line 4) (column 20)))))
                        (loc ((line 4) (column 17)))))
                      (then_
                       (Literal (value (Int 1)) (loc ((line 4) (column 27)))))
                      (else_
                       (Literal (value (Bool true)) (loc ((line 4) (column 34)))))
                      (static true) (loc ((line 4) (column 5)))))
                    (loc ((line 4) (column 4)))))
                  (else_
                   (Paren
                    (expr
                     (If
                      (cond
                       (Binop (op Eq)
                        (lhs (Var (id y) (loc ((line 6) (column 15)))))
                        (rhs
                         (Literal (value (Int 0)) (loc ((line 6) (column 20)))))
                        (loc ((line 6) (column 17)))))
                      (then_ (Literal (value Unit) (loc ((line 6) (column 27)))))
                      (else_
                       (Literal (value (Int 2)) (loc ((line 6) (column 35)))))
                      (static true) (loc ((line 6) (column 5)))))
                    (loc ((line 6) (column 4)))))
                  (static true) (loc ((line 3) (column 2)))))
                (loc ((line 2) (column 31)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn
          (Symbol
           (fn
            (Var (id f)
             (ty
              (Type
               (Pi (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (ret_ty
                 (Typecheck (env <opaque>) (arg x) (arg_ty (Type Int))
                  (arg_mode ((staticity Static) (erasure Unerased)))
                  (memo <opaque>)
                  (body
                   (Lambda (arg y) (erased Unerased)
                    (arg_mode ((staticity (Static)) (erasure ())))
                    (arg_ty (Var (id int) (loc ((line 2) (column 46)))))
                    (body
                     (If
                      (cond
                       (Binop (op Eq)
                        (lhs (Var (id x) (loc ((line 3) (column 12)))))
                        (rhs
                         (Literal (value (Int 0)) (loc ((line 3) (column 17)))))
                        (loc ((line 3) (column 14)))))
                      (then_
                       (Paren
                        (expr
                         (If
                          (cond
                           (Binop (op Eq)
                            (lhs (Var (id y) (loc ((line 4) (column 15)))))
                            (rhs
                             (Literal (value (Int 0))
                              (loc ((line 4) (column 20)))))
                            (loc ((line 4) (column 17)))))
                          (then_
                           (Literal (value (Int 1)) (loc ((line 4) (column 27)))))
                          (else_
                           (Literal (value (Bool true))
                            (loc ((line 4) (column 34)))))
                          (static true) (loc ((line 4) (column 5)))))
                        (loc ((line 4) (column 4)))))
                      (else_
                       (Paren
                        (expr
                         (If
                          (cond
                           (Binop (op Eq)
                            (lhs (Var (id y) (loc ((line 6) (column 15)))))
                            (rhs
                             (Literal (value (Int 0))
                              (loc ((line 6) (column 20)))))
                            (loc ((line 6) (column 17)))))
                          (then_
                           (Literal (value Unit) (loc ((line 6) (column 27)))))
                          (else_
                           (Literal (value (Int 2)) (loc ((line 6) (column 35)))))
                          (static true) (loc ((line 6) (column 5)))))
                        (loc ((line 6) (column 4)))))
                      (static true) (loc ((line 3) (column 2)))))
                    (loc ((line 2) (column 31)))))))
                (ret_mode ((staticity Static) (erasure Unerased))))))
             (mode ((staticity Static) (erasure Unerased)))
             (loc ((line 7) (column 8)))))
           (key (Int 0))
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty
               (Typecheck (env <opaque>) (arg y) (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (memo <opaque>)
                (body
                 (If
                  (cond
                   (Binop (op Eq) (lhs (Var (id x) (loc ((line 3) (column 12)))))
                    (rhs (Literal (value (Int 0)) (loc ((line 3) (column 17)))))
                    (loc ((line 3) (column 14)))))
                  (then_
                   (Paren
                    (expr
                     (If
                      (cond
                       (Binop (op Eq)
                        (lhs (Var (id y) (loc ((line 4) (column 15)))))
                        (rhs
                         (Literal (value (Int 0)) (loc ((line 4) (column 20)))))
                        (loc ((line 4) (column 17)))))
                      (then_
                       (Literal (value (Int 1)) (loc ((line 4) (column 27)))))
                      (else_
                       (Literal (value (Bool true)) (loc ((line 4) (column 34)))))
                      (static true) (loc ((line 4) (column 5)))))
                    (loc ((line 4) (column 4)))))
                  (else_
                   (Paren
                    (expr
                     (If
                      (cond
                       (Binop (op Eq)
                        (lhs (Var (id y) (loc ((line 6) (column 15)))))
                        (rhs
                         (Literal (value (Int 0)) (loc ((line 6) (column 20)))))
                        (loc ((line 6) (column 17)))))
                      (then_ (Literal (value Unit) (loc ((line 6) (column 27)))))
                      (else_
                       (Literal (value (Int 2)) (loc ((line 6) (column 35)))))
                      (static true) (loc ((line 6) (column 5)))))
                    (loc ((line 6) (column 4)))))
                  (static true) (loc ((line 3) (column 2)))))))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 7) (column 8)))))
         (key (Int 0)) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 7) (column 8)))))
       (loc ((line 7) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn
          (Symbol
           (fn
            (Var (id f)
             (ty
              (Type
               (Pi (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (ret_ty
                 (Typecheck (env <opaque>) (arg x) (arg_ty (Type Int))
                  (arg_mode ((staticity Static) (erasure Unerased)))
                  (memo <opaque>)
                  (body
                   (Lambda (arg y) (erased Unerased)
                    (arg_mode ((staticity (Static)) (erasure ())))
                    (arg_ty (Var (id int) (loc ((line 2) (column 46)))))
                    (body
                     (If
                      (cond
                       (Binop (op Eq)
                        (lhs (Var (id x) (loc ((line 3) (column 12)))))
                        (rhs
                         (Literal (value (Int 0)) (loc ((line 3) (column 17)))))
                        (loc ((line 3) (column 14)))))
                      (then_
                       (Paren
                        (expr
                         (If
                          (cond
                           (Binop (op Eq)
                            (lhs (Var (id y) (loc ((line 4) (column 15)))))
                            (rhs
                             (Literal (value (Int 0))
                              (loc ((line 4) (column 20)))))
                            (loc ((line 4) (column 17)))))
                          (then_
                           (Literal (value (Int 1)) (loc ((line 4) (column 27)))))
                          (else_
                           (Literal (value (Bool true))
                            (loc ((line 4) (column 34)))))
                          (static true) (loc ((line 4) (column 5)))))
                        (loc ((line 4) (column 4)))))
                      (else_
                       (Paren
                        (expr
                         (If
                          (cond
                           (Binop (op Eq)
                            (lhs (Var (id y) (loc ((line 6) (column 15)))))
                            (rhs
                             (Literal (value (Int 0))
                              (loc ((line 6) (column 20)))))
                            (loc ((line 6) (column 17)))))
                          (then_
                           (Literal (value Unit) (loc ((line 6) (column 27)))))
                          (else_
                           (Literal (value (Int 2)) (loc ((line 6) (column 35)))))
                          (static true) (loc ((line 6) (column 5)))))
                        (loc ((line 6) (column 4)))))
                      (static true) (loc ((line 3) (column 2)))))
                    (loc ((line 2) (column 31)))))))
                (ret_mode ((staticity Static) (erasure Unerased))))))
             (mode ((staticity Static) (erasure Unerased)))
             (loc ((line 8) (column 8)))))
           (key (Int 0))
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty
               (Typecheck (env <opaque>) (arg y) (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (memo <opaque>)
                (body
                 (If
                  (cond
                   (Binop (op Eq) (lhs (Var (id x) (loc ((line 3) (column 12)))))
                    (rhs (Literal (value (Int 0)) (loc ((line 3) (column 17)))))
                    (loc ((line 3) (column 14)))))
                  (then_
                   (Paren
                    (expr
                     (If
                      (cond
                       (Binop (op Eq)
                        (lhs (Var (id y) (loc ((line 4) (column 15)))))
                        (rhs
                         (Literal (value (Int 0)) (loc ((line 4) (column 20)))))
                        (loc ((line 4) (column 17)))))
                      (then_
                       (Literal (value (Int 1)) (loc ((line 4) (column 27)))))
                      (else_
                       (Literal (value (Bool true)) (loc ((line 4) (column 34)))))
                      (static true) (loc ((line 4) (column 5)))))
                    (loc ((line 4) (column 4)))))
                  (else_
                   (Paren
                    (expr
                     (If
                      (cond
                       (Binop (op Eq)
                        (lhs (Var (id y) (loc ((line 6) (column 15)))))
                        (rhs
                         (Literal (value (Int 0)) (loc ((line 6) (column 20)))))
                        (loc ((line 6) (column 17)))))
                      (then_ (Literal (value Unit) (loc ((line 6) (column 27)))))
                      (else_
                       (Literal (value (Int 2)) (loc ((line 6) (column 35)))))
                      (static true) (loc ((line 6) (column 5)))))
                    (loc ((line 6) (column 4)))))
                  (static true) (loc ((line 3) (column 2)))))))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 8) (column 8)))))
         (key (Int 1)) (ty (Type Bool))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 8) (column 8)))))
       (loc ((line 8) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn
          (Symbol
           (fn
            (Var (id f)
             (ty
              (Type
               (Pi (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (ret_ty
                 (Typecheck (env <opaque>) (arg x) (arg_ty (Type Int))
                  (arg_mode ((staticity Static) (erasure Unerased)))
                  (memo <opaque>)
                  (body
                   (Lambda (arg y) (erased Unerased)
                    (arg_mode ((staticity (Static)) (erasure ())))
                    (arg_ty (Var (id int) (loc ((line 2) (column 46)))))
                    (body
                     (If
                      (cond
                       (Binop (op Eq)
                        (lhs (Var (id x) (loc ((line 3) (column 12)))))
                        (rhs
                         (Literal (value (Int 0)) (loc ((line 3) (column 17)))))
                        (loc ((line 3) (column 14)))))
                      (then_
                       (Paren
                        (expr
                         (If
                          (cond
                           (Binop (op Eq)
                            (lhs (Var (id y) (loc ((line 4) (column 15)))))
                            (rhs
                             (Literal (value (Int 0))
                              (loc ((line 4) (column 20)))))
                            (loc ((line 4) (column 17)))))
                          (then_
                           (Literal (value (Int 1)) (loc ((line 4) (column 27)))))
                          (else_
                           (Literal (value (Bool true))
                            (loc ((line 4) (column 34)))))
                          (static true) (loc ((line 4) (column 5)))))
                        (loc ((line 4) (column 4)))))
                      (else_
                       (Paren
                        (expr
                         (If
                          (cond
                           (Binop (op Eq)
                            (lhs (Var (id y) (loc ((line 6) (column 15)))))
                            (rhs
                             (Literal (value (Int 0))
                              (loc ((line 6) (column 20)))))
                            (loc ((line 6) (column 17)))))
                          (then_
                           (Literal (value Unit) (loc ((line 6) (column 27)))))
                          (else_
                           (Literal (value (Int 2)) (loc ((line 6) (column 35)))))
                          (static true) (loc ((line 6) (column 5)))))
                        (loc ((line 6) (column 4)))))
                      (static true) (loc ((line 3) (column 2)))))
                    (loc ((line 2) (column 31)))))))
                (ret_mode ((staticity Static) (erasure Unerased))))))
             (mode ((staticity Static) (erasure Unerased)))
             (loc ((line 9) (column 8)))))
           (key (Int 1))
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty
               (Typecheck (env <opaque>) (arg y) (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (memo <opaque>)
                (body
                 (If
                  (cond
                   (Binop (op Eq) (lhs (Var (id x) (loc ((line 3) (column 12)))))
                    (rhs (Literal (value (Int 0)) (loc ((line 3) (column 17)))))
                    (loc ((line 3) (column 14)))))
                  (then_
                   (Paren
                    (expr
                     (If
                      (cond
                       (Binop (op Eq)
                        (lhs (Var (id y) (loc ((line 4) (column 15)))))
                        (rhs
                         (Literal (value (Int 0)) (loc ((line 4) (column 20)))))
                        (loc ((line 4) (column 17)))))
                      (then_
                       (Literal (value (Int 1)) (loc ((line 4) (column 27)))))
                      (else_
                       (Literal (value (Bool true)) (loc ((line 4) (column 34)))))
                      (static true) (loc ((line 4) (column 5)))))
                    (loc ((line 4) (column 4)))))
                  (else_
                   (Paren
                    (expr
                     (If
                      (cond
                       (Binop (op Eq)
                        (lhs (Var (id y) (loc ((line 6) (column 15)))))
                        (rhs
                         (Literal (value (Int 0)) (loc ((line 6) (column 20)))))
                        (loc ((line 6) (column 17)))))
                      (then_ (Literal (value Unit) (loc ((line 6) (column 27)))))
                      (else_
                       (Literal (value (Int 2)) (loc ((line 6) (column 35)))))
                      (static true) (loc ((line 6) (column 5)))))
                    (loc ((line 6) (column 4)))))
                  (static true) (loc ((line 3) (column 2)))))))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 9) (column 8)))))
         (key (Int 0)) (ty (Type Unit))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 9) (column 8)))))
       (loc ((line 9) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn
          (Symbol
           (fn
            (Var (id f)
             (ty
              (Type
               (Pi (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (ret_ty
                 (Typecheck (env <opaque>) (arg x) (arg_ty (Type Int))
                  (arg_mode ((staticity Static) (erasure Unerased)))
                  (memo <opaque>)
                  (body
                   (Lambda (arg y) (erased Unerased)
                    (arg_mode ((staticity (Static)) (erasure ())))
                    (arg_ty (Var (id int) (loc ((line 2) (column 46)))))
                    (body
                     (If
                      (cond
                       (Binop (op Eq)
                        (lhs (Var (id x) (loc ((line 3) (column 12)))))
                        (rhs
                         (Literal (value (Int 0)) (loc ((line 3) (column 17)))))
                        (loc ((line 3) (column 14)))))
                      (then_
                       (Paren
                        (expr
                         (If
                          (cond
                           (Binop (op Eq)
                            (lhs (Var (id y) (loc ((line 4) (column 15)))))
                            (rhs
                             (Literal (value (Int 0))
                              (loc ((line 4) (column 20)))))
                            (loc ((line 4) (column 17)))))
                          (then_
                           (Literal (value (Int 1)) (loc ((line 4) (column 27)))))
                          (else_
                           (Literal (value (Bool true))
                            (loc ((line 4) (column 34)))))
                          (static true) (loc ((line 4) (column 5)))))
                        (loc ((line 4) (column 4)))))
                      (else_
                       (Paren
                        (expr
                         (If
                          (cond
                           (Binop (op Eq)
                            (lhs (Var (id y) (loc ((line 6) (column 15)))))
                            (rhs
                             (Literal (value (Int 0))
                              (loc ((line 6) (column 20)))))
                            (loc ((line 6) (column 17)))))
                          (then_
                           (Literal (value Unit) (loc ((line 6) (column 27)))))
                          (else_
                           (Literal (value (Int 2)) (loc ((line 6) (column 35)))))
                          (static true) (loc ((line 6) (column 5)))))
                        (loc ((line 6) (column 4)))))
                      (static true) (loc ((line 3) (column 2)))))
                    (loc ((line 2) (column 31)))))))
                (ret_mode ((staticity Static) (erasure Unerased))))))
             (mode ((staticity Static) (erasure Unerased)))
             (loc ((line 10) (column 8)))))
           (key (Int 1))
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty
               (Typecheck (env <opaque>) (arg y) (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (memo <opaque>)
                (body
                 (If
                  (cond
                   (Binop (op Eq) (lhs (Var (id x) (loc ((line 3) (column 12)))))
                    (rhs (Literal (value (Int 0)) (loc ((line 3) (column 17)))))
                    (loc ((line 3) (column 14)))))
                  (then_
                   (Paren
                    (expr
                     (If
                      (cond
                       (Binop (op Eq)
                        (lhs (Var (id y) (loc ((line 4) (column 15)))))
                        (rhs
                         (Literal (value (Int 0)) (loc ((line 4) (column 20)))))
                        (loc ((line 4) (column 17)))))
                      (then_
                       (Literal (value (Int 1)) (loc ((line 4) (column 27)))))
                      (else_
                       (Literal (value (Bool true)) (loc ((line 4) (column 34)))))
                      (static true) (loc ((line 4) (column 5)))))
                    (loc ((line 4) (column 4)))))
                  (else_
                   (Paren
                    (expr
                     (If
                      (cond
                       (Binop (op Eq)
                        (lhs (Var (id y) (loc ((line 6) (column 15)))))
                        (rhs
                         (Literal (value (Int 0)) (loc ((line 6) (column 20)))))
                        (loc ((line 6) (column 17)))))
                      (then_ (Literal (value Unit) (loc ((line 6) (column 27)))))
                      (else_
                       (Literal (value (Int 2)) (loc ((line 6) (column 35)))))
                      (static true) (loc ((line 6) (column 5)))))
                    (loc ((line 6) (column 4)))))
                  (static true) (loc ((line 3) (column 2)))))))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 10) (column 8)))))
         (key (Int 1)) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 10) (column 8)))))
       (loc ((line 10) (column 0))))))
    |}]
;;

let%expect_test "static lambda unused arg" =
  go
    {|
let f = fn (static _ : int) -> 42;;
let _ = f 0;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg _)
         (body (Literal (value (Int 42)) (loc ((line 2) (column 31)))))
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

let%expect_test "dependent type: apply polymorphic id to itself" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = (id (int -> int)) (fn (x : int) -> x) 5;;
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
           (arg_ty (Var (id t) (loc ((line 2) (column 48)))))
           (body (Var (id x) (loc ((line 2) (column 54)))))
           (loc ((line 2) (column 40)))))
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
                (arg_ty (Var (id t) (loc ((line 2) (column 48)))))
                (body (Var (id x) (loc ((line 2) (column 54)))))
                (loc ((line 2) (column 40)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 9)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Apply
           (fn
            (Symbol
             (fn
              (Var (id id)
               (ty
                (Type
                 (Pi (arg_ty (Type Type))
                  (arg_mode ((staticity Static) (erasure Erased)))
                  (ret_ty
                   (Typecheck (env <opaque>) (arg t) (arg_ty (Type Type))
                    (arg_mode ((staticity Static) (erasure Erased)))
                    (memo <opaque>)
                    (body
                     (Lambda (arg x) (erased Unerased)
                      (arg_mode ((staticity ()) (erasure ())))
                      (arg_ty (Var (id t) (loc ((line 2) (column 48)))))
                      (body (Var (id x) (loc ((line 2) (column 54)))))
                      (loc ((line 2) (column 40)))))))
                  (ret_mode ((staticity Static) (erasure Unerased))))))
               (mode ((staticity Static) (erasure Unerased)))
               (loc ((line 3) (column 9)))))
             (key
              (ArrowT (arg IntT)
               (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret IntT)
               (ret_mode ((staticity Dynamic) (erasure Unerased)))))
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
                (ret_ty
                 (Type
                  (Arrow (arg_ty (Type Int))
                   (arg_mode ((staticity Dynamic) (erasure Unerased)))
                   (ret_ty (Type Int))
                   (ret_mode ((staticity Dynamic) (erasure Unerased))))))
                (ret_mode ((staticity Dynamic) (erasure Unerased))))))
             (mode ((staticity Static) (erasure Unerased)))
             (loc ((line 3) (column 9)))))
           (arg
            (Lambda (arg x)
             (body
              (Var (id x) (ty (Type Int))
               (mode ((staticity Dynamic) (erasure Unerased)))
               (loc ((line 3) (column 43)))))
             (ty
              (Type
               (Arrow (arg_ty (Type Int))
                (arg_mode ((staticity Dynamic) (erasure Unerased)))
                (ret_ty (Type Int))
                (ret_mode ((staticity Dynamic) (erasure Unerased))))))
             (mode ((staticity Static) (erasure Unerased)))
             (loc ((line 3) (column 27)))))
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Dynamic) (erasure Unerased)))
           (loc ((line 3) (column 8)))))
         (arg
          (Literal (value (Int (T 5))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 46)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "if static with bool static arg" =
  go
    {|
let f = fn (static b : bool) -> if static b then 1 else true;;
let _ = f true;;
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
           (then_ (Literal (value (Int 1)) (loc ((line 2) (column 49)))))
           (else_ (Literal (value (Bool true)) (loc ((line 2) (column 56)))))
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
                (then_ (Literal (value (Int 1)) (loc ((line 2) (column 49)))))
                (else_
                 (Literal (value (Bool true)) (loc ((line 2) (column 56)))))
                (static true) (loc ((line 2) (column 32)))))))
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
             (Pi (arg_ty (Type Bool))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty
               (Typecheck (env <opaque>) (arg b) (arg_ty (Type Bool))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (memo <opaque>)
                (body
                 (If (cond (Var (id b) (loc ((line 2) (column 42)))))
                  (then_ (Literal (value (Int 1)) (loc ((line 2) (column 49)))))
                  (else_
                   (Literal (value (Bool true)) (loc ((line 2) (column 56)))))
                  (static true) (loc ((line 2) (column 32)))))))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 8)))))
         (key (Bool true)) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
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
                  (then_ (Literal (value (Int 1)) (loc ((line 2) (column 49)))))
                  (else_
                   (Literal (value (Bool true)) (loc ((line 2) (column 56)))))
                  (static true) (loc ((line 2) (column 32)))))))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 8)))))
         (key (Bool false)) (ty (Type Bool))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "static arg used in arithmetic, result applied" =
  go
    {|
let double = fn (static x : int) -> x + x;;
let _ = double 5;;
|};
  [%expect
    {|
    (tst
     ((Let (var double)
       (bind
        (Binder (arg x)
         (body
          (Binop (op Add) (lhs (Var (id x) (loc ((line 2) (column 36)))))
           (rhs (Var (id x) (loc ((line 2) (column 40)))))
           (loc ((line 2) (column 38)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 13)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn
          (Var (id double)
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Int)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 8)))))
         (key (Int 5)) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
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
  [%expect
    {|
    (tst
     ((Let (var id)
       (bind
        (Binder (arg t)
         (body
          (Lambda (arg x) (erased Unerased)
           (arg_mode ((staticity ()) (erasure ())))
           (arg_ty (Var (id t) (loc ((line 2) (column 48)))))
           (body (Var (id x) (loc ((line 2) (column 54)))))
           (loc ((line 2) (column 40)))))
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
                (arg_ty (Var (id t) (loc ((line 2) (column 48)))))
                (body (Var (id x) (loc ((line 2) (column 54)))))
                (loc ((line 2) (column 40)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 9)))))
       (loc ((line 2) (column 0))))
      (Let (var f)
       (bind
        (Symbol
         (fn
          (Var (id id)
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
                  (arg_ty (Var (id t) (loc ((line 2) (column 48)))))
                  (body (Var (id x) (loc ((line 2) (column 54)))))
                  (loc ((line 2) (column 40)))))))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 8)))))
         (key IntT)
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var g)
       (bind
        (Symbol
         (fn
          (Var (id id)
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
                  (arg_ty (Var (id t) (loc ((line 2) (column 48)))))
                  (body (Var (id x) (loc ((line 2) (column 54)))))
                  (loc ((line 2) (column 40)))))))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 8)))))
         (key BoolT)
         (ty
          (Type
           (Arrow (arg_ty (Type Bool))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Bool))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id f)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 5) (column 8)))))
         (arg
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 5) (column 10)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 5) (column 8)))))
       (loc ((line 5) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id g)
           (ty
            (Type
             (Arrow (arg_ty (Type Bool))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Bool))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 6) (column 8)))))
         (arg
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 6) (column 10)))))
         (ty (Type Bool)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 6) (column 8)))))
       (loc ((line 6) (column 0))))))
    |}]
;;

let%expect_test "dependent type mismatch: pass bool where int expected in body" =
  go
    {|
let f = fn (static erased t : type) -> fn (x : t) -> x;;
let g = f int;;
let _ = g true;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason (Type_mismatch (got (Type Bool)) (need (Type Int)))))
    |}]
;;

let%expect_test "symbolic arrow type as static arg" =
  go
    {|
let choose = fn (static f : static int \ x -> if x == 0 then int else bool) -> f 0;;
let _ = choose (fn (static x : int) -> if static x == 0 then 0 else true);;
|};
  [%expect
    {|
    (tst
     ((Let (var choose)
       (bind
        (Binder (arg f)
         (body
          (Apply (fn (Var (id f) (loc ((line 2) (column 79)))))
           (arg (Literal (value (Int 0)) (loc ((line 2) (column 81)))))
           (loc ((line 2) (column 79)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Pi (arg_ty (Type Int))
               (arg_mode ((staticity Static) (erasure Unerased)))
               (ret_ty
                (Reduce (env <opaque>) (arg x) (arg_ty (Type Int))
                 (arg_mode ((staticity Static) (erasure Unerased)))
                 (memo <opaque>)
                 (ret_ty
                  (If
                   (cond
                    (Binop (op Eq)
                     (lhs (Var (id x) (loc ((line 2) (column 49)))))
                     (rhs (Literal (value (Int 0)) (loc ((line 2) (column 54)))))
                     (loc ((line 2) (column 51)))))
                   (then_ (Var (id int) (loc ((line 2) (column 61)))))
                   (else_ (Var (id bool) (loc ((line 2) (column 70)))))
                   (static false) (loc ((line 2) (column 46)))))))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 13)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn
          (Var (id choose)
           (ty
            (Type
             (Pi
              (arg_ty
               (Type
                (Pi (arg_ty (Type Int))
                 (arg_mode ((staticity Static) (erasure Unerased)))
                 (ret_ty
                  (Reduce (env <opaque>) (arg x) (arg_ty (Type Int))
                   (arg_mode ((staticity Static) (erasure Unerased)))
                   (memo <opaque>)
                   (ret_ty
                    (If
                     (cond
                      (Binop (op Eq)
                       (lhs (Var (id x) (loc ((line 2) (column 49)))))
                       (rhs
                        (Literal (value (Int 0)) (loc ((line 2) (column 54)))))
                       (loc ((line 2) (column 51)))))
                     (then_ (Var (id int) (loc ((line 2) (column 61)))))
                     (else_ (Var (id bool) (loc ((line 2) (column 70)))))
                     (static false) (loc ((line 2) (column 46)))))))
                 (ret_mode ((staticity Dynamic) (erasure Unerased))))))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Int)))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 8)))))
         (key (Closure 4)) (ty (Type Int))
         (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "static lambda body references outer let binding" =
  go
    {|
let n = 10;;
let f = fn (static x : int) -> x + n;;
let _ = f 5;;
|};
  [%expect
    {|
    (tst
     ((Let (var n)
       (bind
        (Literal (value (Int (T 10))) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var f)
       (bind
        (Binder (arg x)
         (body
          (Binop (op Add) (lhs (Var (id x) (loc ((line 3) (column 31)))))
           (rhs (Var (id n) (loc ((line 3) (column 35)))))
           (loc ((line 3) (column 33)))))
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
           (loc ((line 4) (column 8)))))
         (key (Int 5)) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "static lambda with type annotation on body" =
  go
    {|
let f = fn (static x : int) -> (x : int);;
let _ = f 42;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body
          (Paren
           (expr
            (Type_annotation (expr (Var (id x) (loc ((line 2) (column 32)))))
             (ty (Var (id int) (loc ((line 2) (column 36)))))
             (loc ((line 2) (column 34)))))
           (loc ((line 2) (column 31)))))
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
         (key (Int 42)) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "if static in type annotation position" =
  go
    {|
let f = fn (static b : bool) -> (if static b then 0 else true) : (if b then int else bool);;
let _ = f true;;
let _ = f false;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg b)
         (body
          (Type_annotation
           (expr
            (Paren
             (expr
              (If (cond (Var (id b) (loc ((line 2) (column 43)))))
               (then_ (Literal (value (Int 0)) (loc ((line 2) (column 50)))))
               (else_ (Literal (value (Bool true)) (loc ((line 2) (column 57)))))
               (static true) (loc ((line 2) (column 33)))))
             (loc ((line 2) (column 32)))))
           (ty
            (Paren
             (expr
              (If (cond (Var (id b) (loc ((line 2) (column 69)))))
               (then_ (Var (id int) (loc ((line 2) (column 76)))))
               (else_ (Var (id bool) (loc ((line 2) (column 85)))))
               (static false) (loc ((line 2) (column 66)))))
             (loc ((line 2) (column 65)))))
           (loc ((line 2) (column 63)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Bool))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (Typecheck (env <opaque>) (arg b) (arg_ty (Type Bool))
              (arg_mode ((staticity Static) (erasure Unerased))) (memo <opaque>)
              (body
               (Type_annotation
                (expr
                 (Paren
                  (expr
                   (If (cond (Var (id b) (loc ((line 2) (column 43)))))
                    (then_
                     (Literal (value (Int 0)) (loc ((line 2) (column 50)))))
                    (else_
                     (Literal (value (Bool true)) (loc ((line 2) (column 57)))))
                    (static true) (loc ((line 2) (column 33)))))
                  (loc ((line 2) (column 32)))))
                (ty
                 (Paren
                  (expr
                   (If (cond (Var (id b) (loc ((line 2) (column 69)))))
                    (then_ (Var (id int) (loc ((line 2) (column 76)))))
                    (else_ (Var (id bool) (loc ((line 2) (column 85)))))
                    (static false) (loc ((line 2) (column 66)))))
                  (loc ((line 2) (column 65)))))
                (loc ((line 2) (column 63)))))))
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
             (Pi (arg_ty (Type Bool))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty
               (Typecheck (env <opaque>) (arg b) (arg_ty (Type Bool))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (memo <opaque>)
                (body
                 (Type_annotation
                  (expr
                   (Paren
                    (expr
                     (If (cond (Var (id b) (loc ((line 2) (column 43)))))
                      (then_
                       (Literal (value (Int 0)) (loc ((line 2) (column 50)))))
                      (else_
                       (Literal (value (Bool true)) (loc ((line 2) (column 57)))))
                      (static true) (loc ((line 2) (column 33)))))
                    (loc ((line 2) (column 32)))))
                  (ty
                   (Paren
                    (expr
                     (If (cond (Var (id b) (loc ((line 2) (column 69)))))
                      (then_ (Var (id int) (loc ((line 2) (column 76)))))
                      (else_ (Var (id bool) (loc ((line 2) (column 85)))))
                      (static false) (loc ((line 2) (column 66)))))
                    (loc ((line 2) (column 65)))))
                  (loc ((line 2) (column 63)))))))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 8)))))
         (key (Bool true)) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
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
                 (Type_annotation
                  (expr
                   (Paren
                    (expr
                     (If (cond (Var (id b) (loc ((line 2) (column 43)))))
                      (then_
                       (Literal (value (Int 0)) (loc ((line 2) (column 50)))))
                      (else_
                       (Literal (value (Bool true)) (loc ((line 2) (column 57)))))
                      (static true) (loc ((line 2) (column 33)))))
                    (loc ((line 2) (column 32)))))
                  (ty
                   (Paren
                    (expr
                     (If (cond (Var (id b) (loc ((line 2) (column 69)))))
                      (then_ (Var (id int) (loc ((line 2) (column 76)))))
                      (else_ (Var (id bool) (loc ((line 2) (column 85)))))
                      (static false) (loc ((line 2) (column 66)))))
                    (loc ((line 2) (column 65)))))
                  (loc ((line 2) (column 63)))))))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 8)))))
         (key (Bool false)) (ty (Type Bool))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "higher-order static: take a static function and apply it" =
  go
    {|
let apply = fn (static f : static int -> int) -> f 5;;
let _ = apply (fn (static x : int) -> x + 1);;
|};
  [%expect
    {|
    (tst
     ((Let (var apply)
       (bind
        (Binder (arg f)
         (body
          (Apply (fn (Var (id f) (loc ((line 2) (column 49)))))
           (arg (Literal (value (Int 5)) (loc ((line 2) (column 51)))))
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
           (loc ((line 3) (column 8)))))
         (key (Closure 4)) (ty (Type Int))
         (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "static erased type arg with wrong application type" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = id 0;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Type)))))
    |}]
;;

let%expect_test "multiple static erased type args" =
  go
    {|
let f = fn (static erased t1 : type) -> fn (static erased t2 : type) -> fn (x : t1) -> fn (y : t2) -> x;;
let _ = f int bool 0 true;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg t1)
         (body
          (Lambda (arg t2) (erased Unerased)
           (arg_mode ((staticity (Static)) (erasure (Erased))))
           (arg_ty (Var (id type) (loc ((line 2) (column 63)))))
           (body
            (Lambda (arg x) (erased Unerased)
             (arg_mode ((staticity ()) (erasure ())))
             (arg_ty (Var (id t1) (loc ((line 2) (column 80)))))
             (body
              (Lambda (arg y) (erased Unerased)
               (arg_mode ((staticity ()) (erasure ())))
               (arg_ty (Var (id t2) (loc ((line 2) (column 95)))))
               (body (Var (id x) (loc ((line 2) (column 102)))))
               (loc ((line 2) (column 87)))))
             (loc ((line 2) (column 72)))))
           (loc ((line 2) (column 40)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Type))
            (arg_mode ((staticity Static) (erasure Erased)))
            (ret_ty
             (Typecheck (env <opaque>) (arg t1) (arg_ty (Type Type))
              (arg_mode ((staticity Static) (erasure Erased))) (memo <opaque>)
              (body
               (Lambda (arg t2) (erased Unerased)
                (arg_mode ((staticity (Static)) (erasure (Erased))))
                (arg_ty (Var (id type) (loc ((line 2) (column 63)))))
                (body
                 (Lambda (arg x) (erased Unerased)
                  (arg_mode ((staticity ()) (erasure ())))
                  (arg_ty (Var (id t1) (loc ((line 2) (column 80)))))
                  (body
                   (Lambda (arg y) (erased Unerased)
                    (arg_mode ((staticity ()) (erasure ())))
                    (arg_ty (Var (id t2) (loc ((line 2) (column 95)))))
                    (body (Var (id x) (loc ((line 2) (column 102)))))
                    (loc ((line 2) (column 87)))))
                  (loc ((line 2) (column 72)))))
                (loc ((line 2) (column 40)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Apply
           (fn
            (Symbol
             (fn
              (Symbol
               (fn
                (Var (id f)
                 (ty
                  (Type
                   (Pi (arg_ty (Type Type))
                    (arg_mode ((staticity Static) (erasure Erased)))
                    (ret_ty
                     (Typecheck (env <opaque>) (arg t1) (arg_ty (Type Type))
                      (arg_mode ((staticity Static) (erasure Erased)))
                      (memo <opaque>)
                      (body
                       (Lambda (arg t2) (erased Unerased)
                        (arg_mode ((staticity (Static)) (erasure (Erased))))
                        (arg_ty (Var (id type) (loc ((line 2) (column 63)))))
                        (body
                         (Lambda (arg x) (erased Unerased)
                          (arg_mode ((staticity ()) (erasure ())))
                          (arg_ty (Var (id t1) (loc ((line 2) (column 80)))))
                          (body
                           (Lambda (arg y) (erased Unerased)
                            (arg_mode ((staticity ()) (erasure ())))
                            (arg_ty (Var (id t2) (loc ((line 2) (column 95)))))
                            (body (Var (id x) (loc ((line 2) (column 102)))))
                            (loc ((line 2) (column 87)))))
                          (loc ((line 2) (column 72)))))
                        (loc ((line 2) (column 40)))))))
                    (ret_mode ((staticity Static) (erasure Unerased))))))
                 (mode ((staticity Static) (erasure Unerased)))
                 (loc ((line 3) (column 8)))))
               (key IntT)
               (ty
                (Type
                 (Pi (arg_ty (Type Type))
                  (arg_mode ((staticity Static) (erasure Erased)))
                  (ret_ty
                   (Typecheck (env <opaque>) (arg t2) (arg_ty (Type Type))
                    (arg_mode ((staticity Static) (erasure Erased)))
                    (memo <opaque>)
                    (body
                     (Lambda (arg x) (erased Unerased)
                      (arg_mode ((staticity ()) (erasure ())))
                      (arg_ty (Var (id t1) (loc ((line 2) (column 80)))))
                      (body
                       (Lambda (arg y) (erased Unerased)
                        (arg_mode ((staticity ()) (erasure ())))
                        (arg_ty (Var (id t2) (loc ((line 2) (column 95)))))
                        (body (Var (id x) (loc ((line 2) (column 102)))))
                        (loc ((line 2) (column 87)))))
                      (loc ((line 2) (column 72)))))))
                  (ret_mode ((staticity Static) (erasure Unerased))))))
               (mode ((staticity Static) (erasure Unerased)))
               (loc ((line 3) (column 8)))))
             (key BoolT)
             (ty
              (Type
               (Arrow (arg_ty (Type Int))
                (arg_mode ((staticity Dynamic) (erasure Unerased)))
                (ret_ty
                 (Type
                  (Arrow (arg_ty (Type Bool))
                   (arg_mode ((staticity Dynamic) (erasure Unerased)))
                   (ret_ty (Type Int))
                   (ret_mode ((staticity Dynamic) (erasure Unerased))))))
                (ret_mode ((staticity Dynamic) (erasure Unerased))))))
             (mode ((staticity Static) (erasure Unerased)))
             (loc ((line 3) (column 8)))))
           (arg
            (Literal (value (Int (T 0))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased)))
             (loc ((line 3) (column 19)))))
           (ty
            (Type
             (Arrow (arg_ty (Type Bool))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Dynamic) (erasure Unerased)))
           (loc ((line 3) (column 8)))))
         (arg
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 21)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "if static nested in let expression" =
  go
    {|
let f = fn (static x : int) ->
  let y = if static x == 0 then 1 else true in
  y;;
let _ = f 0;;
let _ = f 1;;
|};
  [%expect {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body
          (Let (var y)
           (bind
            (If
             (cond
              (Binop (op Eq) (lhs (Var (id x) (loc ((line 3) (column 20)))))
               (rhs (Literal (value (Int 0)) (loc ((line 3) (column 25)))))
               (loc ((line 3) (column 22)))))
             (then_ (Literal (value (Int 1)) (loc ((line 3) (column 32)))))
             (else_ (Literal (value (Bool true)) (loc ((line 3) (column 39)))))
             (static true) (loc ((line 3) (column 10)))))
           (rest (Var (id y) (loc ((line 4) (column 2)))))
           (loc ((line 3) (column 2)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (Typecheck (env <opaque>) (arg x) (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased))) (memo <opaque>)
              (body
               (Let (var y)
                (bind
                 (If
                  (cond
                   (Binop (op Eq) (lhs (Var (id x) (loc ((line 3) (column 20)))))
                    (rhs (Literal (value (Int 0)) (loc ((line 3) (column 25)))))
                    (loc ((line 3) (column 22)))))
                  (then_ (Literal (value (Int 1)) (loc ((line 3) (column 32)))))
                  (else_
                   (Literal (value (Bool true)) (loc ((line 3) (column 39)))))
                  (static true) (loc ((line 3) (column 10)))))
                (rest (Var (id y) (loc ((line 4) (column 2)))))
                (loc ((line 3) (column 2)))))))
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
              (ret_ty
               (Typecheck (env <opaque>) (arg x) (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (memo <opaque>)
                (body
                 (Let (var y)
                  (bind
                   (If
                    (cond
                     (Binop (op Eq)
                      (lhs (Var (id x) (loc ((line 3) (column 20)))))
                      (rhs
                       (Literal (value (Int 0)) (loc ((line 3) (column 25)))))
                      (loc ((line 3) (column 22)))))
                    (then_
                     (Literal (value (Int 1)) (loc ((line 3) (column 32)))))
                    (else_
                     (Literal (value (Bool true)) (loc ((line 3) (column 39)))))
                    (static true) (loc ((line 3) (column 10)))))
                  (rest (Var (id y) (loc ((line 4) (column 2)))))
                  (loc ((line 3) (column 2)))))))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 5) (column 8)))))
         (key (Int 0)) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 5) (column 8)))))
       (loc ((line 5) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn
          (Var (id f)
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty
               (Typecheck (env <opaque>) (arg x) (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (memo <opaque>)
                (body
                 (Let (var y)
                  (bind
                   (If
                    (cond
                     (Binop (op Eq)
                      (lhs (Var (id x) (loc ((line 3) (column 20)))))
                      (rhs
                       (Literal (value (Int 0)) (loc ((line 3) (column 25)))))
                      (loc ((line 3) (column 22)))))
                    (then_
                     (Literal (value (Int 1)) (loc ((line 3) (column 32)))))
                    (else_
                     (Literal (value (Bool true)) (loc ((line 3) (column 39)))))
                    (static true) (loc ((line 3) (column 10)))))
                  (rest (Var (id y) (loc ((line 4) (column 2)))))
                  (loc ((line 3) (column 2)))))))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 6) (column 8)))))
         (key (Int 1)) (ty (Type Bool))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 6) (column 8)))))
       (loc ((line 6) (column 0))))))
    |}]
;;

let%expect_test "cannot use dynamic erased as condition" =
  go
    {|
let x = true @ dynamic erased;;
let _ = if x then 1 else 2;;
|};
  [%expect {| ((loc ((line 3) (column 8))) (reason Dynamic_erased)) |}]
;;

let%expect_test "inline case 1: binder with captured static var" =
  go
    {|
let f = fn (static x : int) -> fn (static y : int) -> x + y;;
let _ = f 10 20;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body
          (Lambda (arg y) (erased Unerased)
           (arg_mode ((staticity (Static)) (erasure ())))
           (arg_ty (Var (id int) (loc ((line 2) (column 46)))))
           (body
            (Binop (op Add) (lhs (Var (id x) (loc ((line 2) (column 54)))))
             (rhs (Var (id y) (loc ((line 2) (column 58)))))
             (loc ((line 2) (column 56)))))
           (loc ((line 2) (column 31)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (T
              (Type
               (Pi (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (ret_ty (T (Type Int)))
                (ret_mode ((staticity Static) (erasure Unerased)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn
          (Symbol
           (fn
            (Var (id f)
             (ty
              (Type
               (Pi (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (ret_ty
                 (T
                  (Type
                   (Pi (arg_ty (Type Int))
                    (arg_mode ((staticity Static) (erasure Unerased)))
                    (ret_ty (T (Type Int)))
                    (ret_mode ((staticity Static) (erasure Unerased)))))))
                (ret_mode ((staticity Static) (erasure Unerased))))))
             (mode ((staticity Static) (erasure Unerased)))
             (loc ((line 3) (column 8)))))
           (key (Int 10))
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Int)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 8)))))
         (key (Int 20)) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "inline case 1: triple nested binder" =
  go
    {|
let f = fn (static x : int) -> fn (static y : int) -> fn (static z : int) -> x + y + z;;
let _ = f 1 2 3;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body
          (Lambda (arg y) (erased Unerased)
           (arg_mode ((staticity (Static)) (erasure ())))
           (arg_ty (Var (id int) (loc ((line 2) (column 46)))))
           (body
            (Lambda (arg z) (erased Unerased)
             (arg_mode ((staticity (Static)) (erasure ())))
             (arg_ty (Var (id int) (loc ((line 2) (column 69)))))
             (body
              (Binop (op Add)
               (lhs
                (Binop (op Add) (lhs (Var (id x) (loc ((line 2) (column 77)))))
                 (rhs (Var (id y) (loc ((line 2) (column 81)))))
                 (loc ((line 2) (column 79)))))
               (rhs (Var (id z) (loc ((line 2) (column 85)))))
               (loc ((line 2) (column 83)))))
             (loc ((line 2) (column 54)))))
           (loc ((line 2) (column 31)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (T
              (Type
               (Pi (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (ret_ty
                 (T
                  (Type
                   (Pi (arg_ty (Type Int))
                    (arg_mode ((staticity Static) (erasure Unerased)))
                    (ret_ty (T (Type Int)))
                    (ret_mode ((staticity Static) (erasure Unerased)))))))
                (ret_mode ((staticity Static) (erasure Unerased)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn
          (Symbol
           (fn
            (Symbol
             (fn
              (Var (id f)
               (ty
                (Type
                 (Pi (arg_ty (Type Int))
                  (arg_mode ((staticity Static) (erasure Unerased)))
                  (ret_ty
                   (T
                    (Type
                     (Pi (arg_ty (Type Int))
                      (arg_mode ((staticity Static) (erasure Unerased)))
                      (ret_ty
                       (T
                        (Type
                         (Pi (arg_ty (Type Int))
                          (arg_mode ((staticity Static) (erasure Unerased)))
                          (ret_ty (T (Type Int)))
                          (ret_mode ((staticity Static) (erasure Unerased)))))))
                      (ret_mode ((staticity Static) (erasure Unerased)))))))
                  (ret_mode ((staticity Static) (erasure Unerased))))))
               (mode ((staticity Static) (erasure Unerased)))
               (loc ((line 3) (column 8)))))
             (key (Int 1))
             (ty
              (Type
               (Pi (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (ret_ty
                 (T
                  (Type
                   (Pi (arg_ty (Type Int))
                    (arg_mode ((staticity Static) (erasure Unerased)))
                    (ret_ty (T (Type Int)))
                    (ret_mode ((staticity Static) (erasure Unerased)))))))
                (ret_mode ((staticity Static) (erasure Unerased))))))
             (mode ((staticity Static) (erasure Unerased)))
             (loc ((line 3) (column 8)))))
           (key (Int 2))
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Int)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 8)))))
         (key (Int 3)) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "inline case 3: erased closure capturing static var" =
  go
    {|
let mk = fn (static a : int) -> (fn (y : int) -> a + y) @ erased;;
let f = mk 10;;
let _ = f 5;;
|};
  [%expect
    {|
    (tst
     ((Let (var mk)
       (bind
        (Binder (arg a)
         (body
          (Mode_annotation
           (expr
            (Paren
             (expr
              (Lambda (arg y) (erased Unerased)
               (arg_mode ((staticity ()) (erasure ())))
               (arg_ty (Var (id int) (loc ((line 2) (column 41)))))
               (body
                (Binop (op Add) (lhs (Var (id a) (loc ((line 2) (column 49)))))
                 (rhs (Var (id y) (loc ((line 2) (column 53)))))
                 (loc ((line 2) (column 51)))))
               (loc ((line 2) (column 33)))))
             (loc ((line 2) (column 32)))))
           (mode ((staticity ()) (erasure (Erased))))
           (loc ((line 2) (column 56)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (T
              (Type
               (Arrow (arg_ty (Type Int))
                (arg_mode ((staticity Dynamic) (erasure Unerased)))
                (ret_ty (Type Int))
                (ret_mode ((staticity Dynamic) (erasure Unerased)))))))
            (ret_mode ((staticity Static) (erasure Erased))))))
         (mode ((staticity Static) (erasure Erased)))
         (loc ((line 2) (column 9)))))
       (loc ((line 2) (column 0))))
      (Let (var f)
       (bind
        (Let (var a)
         (bind
          (Literal (value (Int (T 10))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 11)))))
         (rest
          (Literal
           (value
            (Closure
             ((arg y)
              (ty
               (Type
                (Arrow (arg_ty (Type Int))
                 (arg_mode ((staticity Dynamic) (erasure Unerased)))
                 (ret_ty (Type Int))
                 (ret_mode ((staticity Dynamic) (erasure Unerased))))))
              (body
               (Binop (op Add)
                (lhs
                 (Var (id a) (ty (Type Int))
                  (mode ((staticity Static) (erasure Unerased)))
                  (loc ((line 2) (column 49)))))
                (rhs
                 (Var (id y) (ty (Type Int))
                  (mode ((staticity Dynamic) (erasure Unerased)))
                  (loc ((line 2) (column 53)))))
                (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
                (loc ((line 2) (column 51)))))
              (env <opaque>))))
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Erased)))
           (loc ((line 2) (column 56)))))
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Erased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Let (var a)
         (bind
          (Literal (value (Int (T 10))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 8)))))
         (rest
          (Let (var y)
           (bind
            (Literal (value (Int (T 5))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased)))
             (loc ((line 4) (column 10)))))
           (rest
            (Binop (op Add)
             (lhs
              (Var (id a) (ty (Type Int))
               (mode ((staticity Static) (erasure Unerased)))
               (loc ((line 2) (column 49)))))
             (rhs
              (Var (id y) (ty (Type Int))
               (mode ((staticity Dynamic) (erasure Unerased)))
               (loc ((line 2) (column 53)))))
             (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
             (loc ((line 2) (column 51)))))
           (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
           (loc ((line 4) (column 8)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "inline case 3: erased closure capturing two static vars" =
  go
    {|
let mk = fn (static a : int) -> fn (static b : int) -> (fn (y : int) -> a + b + y) @ erased;;
let f = mk 10 20;;
let _ = f 5;;
|};
  [%expect
    {|
    (tst
     ((Let (var mk)
       (bind
        (Binder (arg a)
         (body
          (Lambda (arg b) (erased Unerased)
           (arg_mode ((staticity (Static)) (erasure ())))
           (arg_ty (Var (id int) (loc ((line 2) (column 47)))))
           (body
            (Mode_annotation
             (expr
              (Paren
               (expr
                (Lambda (arg y) (erased Unerased)
                 (arg_mode ((staticity ()) (erasure ())))
                 (arg_ty (Var (id int) (loc ((line 2) (column 64)))))
                 (body
                  (Binop (op Add)
                   (lhs
                    (Binop (op Add)
                     (lhs (Var (id a) (loc ((line 2) (column 72)))))
                     (rhs (Var (id b) (loc ((line 2) (column 76)))))
                     (loc ((line 2) (column 74)))))
                   (rhs (Var (id y) (loc ((line 2) (column 80)))))
                   (loc ((line 2) (column 78)))))
                 (loc ((line 2) (column 56)))))
               (loc ((line 2) (column 55)))))
             (mode ((staticity ()) (erasure (Erased))))
             (loc ((line 2) (column 83)))))
           (loc ((line 2) (column 32)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (T
              (Type
               (Pi (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (ret_ty
                 (T
                  (Type
                   (Arrow (arg_ty (Type Int))
                    (arg_mode ((staticity Dynamic) (erasure Unerased)))
                    (ret_ty (Type Int))
                    (ret_mode ((staticity Dynamic) (erasure Unerased)))))))
                (ret_mode ((staticity Static) (erasure Erased)))))))
            (ret_mode ((staticity Static) (erasure Erased))))))
         (mode ((staticity Static) (erasure Erased)))
         (loc ((line 2) (column 9)))))
       (loc ((line 2) (column 0))))
      (Let (var f)
       (bind
        (Let (var b)
         (bind
          (Literal (value (Int (T 20))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 14)))))
         (rest
          (Literal
           (value
            (Closure
             ((arg y)
              (ty
               (Type
                (Arrow (arg_ty (Type Int))
                 (arg_mode ((staticity Dynamic) (erasure Unerased)))
                 (ret_ty (Type Int))
                 (ret_mode ((staticity Dynamic) (erasure Unerased))))))
              (body
               (Binop (op Add)
                (lhs
                 (Binop (op Add)
                  (lhs
                   (Var (id a) (ty (Type Int))
                    (mode ((staticity Static) (erasure Unerased)))
                    (loc ((line 2) (column 72)))))
                  (rhs
                   (Var (id b) (ty (Type Int))
                    (mode ((staticity Static) (erasure Unerased)))
                    (loc ((line 2) (column 76)))))
                  (ty (Type Int)) (mode ((staticity Static) (erasure Unerased)))
                  (loc ((line 2) (column 74)))))
                (rhs
                 (Var (id y) (ty (Type Int))
                  (mode ((staticity Dynamic) (erasure Unerased)))
                  (loc ((line 2) (column 80)))))
                (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
                (loc ((line 2) (column 78)))))
              (env <opaque>))))
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Erased)))
           (loc ((line 2) (column 83)))))
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Erased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Let (var b)
         (bind
          (Literal (value (Int (T 20))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 8)))))
         (rest
          (Let (var a)
           (bind
            (Literal (value (Int (T 10))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased)))
             (loc ((line 4) (column 8)))))
           (rest
            (Let (var y)
             (bind
              (Literal (value (Int (T 5))) (ty (Type Int))
               (mode ((staticity Static) (erasure Unerased)))
               (loc ((line 4) (column 10)))))
             (rest
              (Binop (op Add)
               (lhs
                (Binop (op Add)
                 (lhs
                  (Var (id a) (ty (Type Int))
                   (mode ((staticity Static) (erasure Unerased)))
                   (loc ((line 2) (column 72)))))
                 (rhs
                  (Var (id b) (ty (Type Int))
                   (mode ((staticity Static) (erasure Unerased)))
                   (loc ((line 2) (column 76)))))
                 (ty (Type Int)) (mode ((staticity Static) (erasure Unerased)))
                 (loc ((line 2) (column 74)))))
               (rhs
                (Var (id y) (ty (Type Int))
                 (mode ((staticity Dynamic) (erasure Unerased)))
                 (loc ((line 2) (column 80)))))
               (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
               (loc ((line 2) (column 78)))))
             (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
             (loc ((line 4) (column 8)))))
           (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
           (loc ((line 4) (column 8)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "inline case 1: binder with erased captured var" =
  go
    {|
let f = fn (static erased x : int) -> fn (static y : int) -> y;;
let _ = f 10 20;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body
          (Lambda (arg y) (erased Unerased)
           (arg_mode ((staticity (Static)) (erasure ())))
           (arg_ty (Var (id int) (loc ((line 2) (column 53)))))
           (body (Var (id y) (loc ((line 2) (column 61)))))
           (loc ((line 2) (column 38)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Erased)))
            (ret_ty
             (T
              (Type
               (Pi (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (ret_ty (T (Type Int)))
                (ret_mode ((staticity Static) (erasure Unerased)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn
          (Symbol
           (fn
            (Var (id f)
             (ty
              (Type
               (Pi (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Erased)))
                (ret_ty
                 (T
                  (Type
                   (Pi (arg_ty (Type Int))
                    (arg_mode ((staticity Static) (erasure Unerased)))
                    (ret_ty (T (Type Int)))
                    (ret_mode ((staticity Static) (erasure Unerased)))))))
                (ret_mode ((staticity Static) (erasure Unerased))))))
             (mode ((staticity Static) (erasure Unerased)))
             (loc ((line 3) (column 8)))))
           (key (Int 10))
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Int)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 8)))))
         (key (Int 20)) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "inline rebinds captured var shadowed at call site" =
  go
    {|
let x = 1;;
let f = fn (static a : int) -> x + a;;
let x = 2;;
let _ = f 10;;
|};
  [%expect
    {|
    (tst
     ((Let (var x)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var f)
       (bind
        (Binder (arg a)
         (body
          (Binop (op Add) (lhs (Var (id x) (loc ((line 3) (column 31)))))
           (rhs (Var (id a) (loc ((line 3) (column 35)))))
           (loc ((line 3) (column 33)))))
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
      (Let (var x)
       (bind
        (Literal (value (Int (T 2))) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))
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
           (loc ((line 5) (column 8)))))
         (key (Int 10)) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 5) (column 8)))))
       (loc ((line 5) (column 0))))))
    |}]
;;

let%expect_test "smart constructors" =
  go
    {|
let f = fn (static x : int) -> fn (_ : int) -> true;;
let g = fn (static x : int) -> fn (_ : int) -> true;;
let _ = if true then f else g;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body
          (Lambda (arg _) (erased Unerased)
           (arg_mode ((staticity ()) (erasure ())))
           (arg_ty (Var (id int) (loc ((line 2) (column 39)))))
           (body (Literal (value (Bool true)) (loc ((line 2) (column 47)))))
           (loc ((line 2) (column 31)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (T
              (Type
               (Arrow (arg_ty (Type Int))
                (arg_mode ((staticity Dynamic) (erasure Unerased)))
                (ret_ty (Type Bool))
                (ret_mode ((staticity Dynamic) (erasure Unerased)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Binder (arg x)
         (body
          (Lambda (arg _) (erased Unerased)
           (arg_mode ((staticity ()) (erasure ())))
           (arg_ty (Var (id int) (loc ((line 3) (column 39)))))
           (body (Literal (value (Bool true)) (loc ((line 3) (column 47)))))
           (loc ((line 3) (column 31)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (T
              (Type
               (Arrow (arg_ty (Type Int))
                (arg_mode ((staticity Dynamic) (erasure Unerased)))
                (ret_ty (Type Bool))
                (ret_mode ((staticity Dynamic) (erasure Unerased)))))))
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
              (ret_ty
               (T
                (Type
                 (Arrow (arg_ty (Type Int))
                  (arg_mode ((staticity Dynamic) (erasure Unerased)))
                  (ret_ty (Type Bool))
                  (ret_mode ((staticity Dynamic) (erasure Unerased)))))))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 21)))))
         (else_
          (Var (id g)
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty
               (T
                (Type
                 (Arrow (arg_ty (Type Int))
                  (arg_mode ((staticity Dynamic) (erasure Unerased)))
                  (ret_ty (Type Bool))
                  (ret_mode ((staticity Dynamic) (erasure Unerased)))))))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 28)))))
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (T
              (Type
               (Arrow (arg_ty (Type Int))
                (arg_mode ((staticity Dynamic) (erasure Unerased)))
                (ret_ty (Type Bool))
                (ret_mode ((staticity Dynamic) (erasure Unerased)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "smart constructors" =
  go
    {|
let f = fn (static x : int) -> fn (_ : int) -> true;;
let g = fn (static x : int) -> fn (static _ : int) -> true;;
let _ = if true then f else g;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body
          (Lambda (arg _) (erased Unerased)
           (arg_mode ((staticity ()) (erasure ())))
           (arg_ty (Var (id int) (loc ((line 2) (column 39)))))
           (body (Literal (value (Bool true)) (loc ((line 2) (column 47)))))
           (loc ((line 2) (column 31)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (T
              (Type
               (Arrow (arg_ty (Type Int))
                (arg_mode ((staticity Dynamic) (erasure Unerased)))
                (ret_ty (Type Bool))
                (ret_mode ((staticity Dynamic) (erasure Unerased)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Binder (arg x)
         (body
          (Lambda (arg _) (erased Unerased)
           (arg_mode ((staticity (Static)) (erasure ())))
           (arg_ty (Var (id int) (loc ((line 3) (column 46)))))
           (body (Literal (value (Bool true)) (loc ((line 3) (column 54)))))
           (loc ((line 3) (column 31)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (T
              (Type
               (Pi (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (ret_ty (T (Type Bool)))
                (ret_mode ((staticity Static) (erasure Unerased)))))))
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
              (ret_ty
               (T
                (Type
                 (Arrow (arg_ty (Type Int))
                  (arg_mode ((staticity Dynamic) (erasure Unerased)))
                  (ret_ty (Type Bool))
                  (ret_mode ((staticity Dynamic) (erasure Unerased)))))))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 21)))))
         (else_
          (Var (id g)
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty
               (T
                (Type
                 (Pi (arg_ty (Type Int))
                  (arg_mode ((staticity Static) (erasure Unerased)))
                  (ret_ty (T (Type Bool)))
                  (ret_mode ((staticity Static) (erasure Unerased)))))))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 28)))))
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (T
              (Type
               (Pi (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (ret_ty (T (Type Bool)))
                (ret_mode ((staticity Dynamic) (erasure Unerased)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "smart constructors" =
  go
    {|
let f = fn (static x : int) -> fn (static _ : int) -> true;;
let g = fn (static x : int) -> fn (_ : int) -> true;;
let _ = if true then f else g;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body
          (Lambda (arg _) (erased Unerased)
           (arg_mode ((staticity (Static)) (erasure ())))
           (arg_ty (Var (id int) (loc ((line 2) (column 46)))))
           (body (Literal (value (Bool true)) (loc ((line 2) (column 54)))))
           (loc ((line 2) (column 31)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (T
              (Type
               (Pi (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (ret_ty (T (Type Bool)))
                (ret_mode ((staticity Static) (erasure Unerased)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Binder (arg x)
         (body
          (Lambda (arg _) (erased Unerased)
           (arg_mode ((staticity ()) (erasure ())))
           (arg_ty (Var (id int) (loc ((line 3) (column 39)))))
           (body (Literal (value (Bool true)) (loc ((line 3) (column 47)))))
           (loc ((line 3) (column 31)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (T
              (Type
               (Arrow (arg_ty (Type Int))
                (arg_mode ((staticity Dynamic) (erasure Unerased)))
                (ret_ty (Type Bool))
                (ret_mode ((staticity Dynamic) (erasure Unerased)))))))
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
              (ret_ty
               (T
                (Type
                 (Pi (arg_ty (Type Int))
                  (arg_mode ((staticity Static) (erasure Unerased)))
                  (ret_ty (T (Type Bool)))
                  (ret_mode ((staticity Static) (erasure Unerased)))))))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 21)))))
         (else_
          (Var (id g)
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty
               (T
                (Type
                 (Arrow (arg_ty (Type Int))
                  (arg_mode ((staticity Dynamic) (erasure Unerased)))
                  (ret_ty (Type Bool))
                  (ret_mode ((staticity Dynamic) (erasure Unerased)))))))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 28)))))
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (T
              (Type
               (Pi (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (ret_ty (T (Type Bool)))
                (ret_mode ((staticity Dynamic) (erasure Unerased)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "smart constructors" =
  go
    {|
let f = fn (static x : int) -> fn (static _ : int) -> true;;
let g = fn (static x : int) -> fn (static _ : int) -> true;;
let _ = if true then f else g;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body
          (Lambda (arg _) (erased Unerased)
           (arg_mode ((staticity (Static)) (erasure ())))
           (arg_ty (Var (id int) (loc ((line 2) (column 46)))))
           (body (Literal (value (Bool true)) (loc ((line 2) (column 54)))))
           (loc ((line 2) (column 31)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (T
              (Type
               (Pi (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (ret_ty (T (Type Bool)))
                (ret_mode ((staticity Static) (erasure Unerased)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Binder (arg x)
         (body
          (Lambda (arg _) (erased Unerased)
           (arg_mode ((staticity (Static)) (erasure ())))
           (arg_ty (Var (id int) (loc ((line 3) (column 46)))))
           (body (Literal (value (Bool true)) (loc ((line 3) (column 54)))))
           (loc ((line 3) (column 31)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (T
              (Type
               (Pi (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (ret_ty (T (Type Bool)))
                (ret_mode ((staticity Static) (erasure Unerased)))))))
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
              (ret_ty
               (T
                (Type
                 (Pi (arg_ty (Type Int))
                  (arg_mode ((staticity Static) (erasure Unerased)))
                  (ret_ty (T (Type Bool)))
                  (ret_mode ((staticity Static) (erasure Unerased)))))))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 21)))))
         (else_
          (Var (id g)
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty
               (T
                (Type
                 (Pi (arg_ty (Type Int))
                  (arg_mode ((staticity Static) (erasure Unerased)))
                  (ret_ty (T (Type Bool)))
                  (ret_mode ((staticity Static) (erasure Unerased)))))))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 28)))))
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (T
              (Type
               (Pi (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (ret_ty (T (Type Bool)))
                (ret_mode ((staticity Static) (erasure Unerased)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "smart constructors" =
  go
    {|
let f = fn (static x : int -> int) -> ();;
let g = fn (static x : int -> int) -> ();;
let _ = if true then f else g;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body (Literal (value Unit) (loc ((line 2) (column 38)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Arrow (arg_ty (Type Int))
               (arg_mode ((staticity Dynamic) (erasure Unerased)))
               (ret_ty (Type Int))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Unit)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Binder (arg x)
         (body (Literal (value Unit) (loc ((line 3) (column 38)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Arrow (arg_ty (Type Int))
               (arg_mode ((staticity Dynamic) (erasure Unerased)))
               (ret_ty (Type Int))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Unit)))
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
                (Arrow (arg_ty (Type Int))
                 (arg_mode ((staticity Dynamic) (erasure Unerased)))
                 (ret_ty (Type Int))
                 (ret_mode ((staticity Dynamic) (erasure Unerased))))))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Unit)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 21)))))
         (else_
          (Var (id g)
           (ty
            (Type
             (Pi
              (arg_ty
               (Type
                (Arrow (arg_ty (Type Int))
                 (arg_mode ((staticity Dynamic) (erasure Unerased)))
                 (ret_ty (Type Int))
                 (ret_mode ((staticity Dynamic) (erasure Unerased))))))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Unit)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 28)))))
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Arrow (arg_ty (Type Int))
               (arg_mode ((staticity Dynamic) (erasure Unerased)))
               (ret_ty (Type Int))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Unit)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "smart constructors" =
  go
    {|
let f = fn (static x : static int -> int) -> ();;
let g = fn (static x : int -> int) -> ();;
let _ = if true then f else g;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body (Literal (value Unit) (loc ((line 2) (column 45)))))
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
            (ret_ty (T (Type Unit)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Binder (arg x)
         (body (Literal (value Unit) (loc ((line 3) (column 38)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Arrow (arg_ty (Type Int))
               (arg_mode ((staticity Dynamic) (erasure Unerased)))
               (ret_ty (Type Int))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Unit)))
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
                 (ret_ty (T (Type Int)))
                 (ret_mode ((staticity Dynamic) (erasure Unerased))))))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Unit)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 21)))))
         (else_
          (Var (id g)
           (ty
            (Type
             (Pi
              (arg_ty
               (Type
                (Arrow (arg_ty (Type Int))
                 (arg_mode ((staticity Dynamic) (erasure Unerased)))
                 (ret_ty (Type Int))
                 (ret_mode ((staticity Dynamic) (erasure Unerased))))))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Unit)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 28)))))
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Arrow (arg_ty (Type Int))
               (arg_mode ((staticity Dynamic) (erasure Unerased)))
               (ret_ty (Type Int))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Unit)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "smart constructors" =
  go
    {|
let f = fn (static x : int -> int) -> ();;
let g = fn (static x : static int -> int) -> ();;
let _ = if true then f else g;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body (Literal (value Unit) (loc ((line 2) (column 38)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Arrow (arg_ty (Type Int))
               (arg_mode ((staticity Dynamic) (erasure Unerased)))
               (ret_ty (Type Int))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Unit)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Binder (arg x)
         (body (Literal (value Unit) (loc ((line 3) (column 45)))))
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
            (ret_ty (T (Type Unit)))
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
                (Arrow (arg_ty (Type Int))
                 (arg_mode ((staticity Dynamic) (erasure Unerased)))
                 (ret_ty (Type Int))
                 (ret_mode ((staticity Dynamic) (erasure Unerased))))))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Unit)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 21)))))
         (else_
          (Var (id g)
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
              (ret_ty (T (Type Unit)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 28)))))
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Arrow (arg_ty (Type Int))
               (arg_mode ((staticity Dynamic) (erasure Unerased)))
               (ret_ty (Type Int))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Unit)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "smart constructors" =
  go
    {|
let f = fn (static x : static int -> int) -> ();;
let g = fn (static x : static int -> int) -> ();;
let _ = if true then f else g;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body (Literal (value Unit) (loc ((line 2) (column 45)))))
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
            (ret_ty (T (Type Unit)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Binder (arg x)
         (body (Literal (value Unit) (loc ((line 3) (column 45)))))
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
            (ret_ty (T (Type Unit)))
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
                 (ret_ty (T (Type Int)))
                 (ret_mode ((staticity Dynamic) (erasure Unerased))))))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Unit)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 21)))))
         (else_
          (Var (id g)
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
              (ret_ty (T (Type Unit)))
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
               (ret_ty (T (Type Int)))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Unit)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "smart constructors" =
  go
    {|
let f = fn (static x : int -> static int) -> ();;
let g = fn (static x : int -> int) -> ();;
let _ = if true then f else g;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body (Literal (value Unit) (loc ((line 2) (column 45)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Arrow (arg_ty (Type Int))
               (arg_mode ((staticity Dynamic) (erasure Unerased)))
               (ret_ty (Type Int))
               (ret_mode ((staticity Static) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Unit)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Binder (arg x)
         (body (Literal (value Unit) (loc ((line 3) (column 38)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Arrow (arg_ty (Type Int))
               (arg_mode ((staticity Dynamic) (erasure Unerased)))
               (ret_ty (Type Int))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Unit)))
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
                (Arrow (arg_ty (Type Int))
                 (arg_mode ((staticity Dynamic) (erasure Unerased)))
                 (ret_ty (Type Int))
                 (ret_mode ((staticity Static) (erasure Unerased))))))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Unit)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 21)))))
         (else_
          (Var (id g)
           (ty
            (Type
             (Pi
              (arg_ty
               (Type
                (Arrow (arg_ty (Type Int))
                 (arg_mode ((staticity Dynamic) (erasure Unerased)))
                 (ret_ty (Type Int))
                 (ret_mode ((staticity Dynamic) (erasure Unerased))))))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Unit)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 28)))))
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Arrow (arg_ty (Type Int))
               (arg_mode ((staticity Dynamic) (erasure Unerased)))
               (ret_ty (Type Int))
               (ret_mode ((staticity Static) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Unit)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "smart constructors" =
  go
    {|
let f = fn (static x : int -> int) -> ();;
let g = fn (static x : int -> static int) -> ();;
let _ = if true then f else g;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body (Literal (value Unit) (loc ((line 2) (column 38)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Arrow (arg_ty (Type Int))
               (arg_mode ((staticity Dynamic) (erasure Unerased)))
               (ret_ty (Type Int))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Unit)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Binder (arg x)
         (body (Literal (value Unit) (loc ((line 3) (column 45)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Arrow (arg_ty (Type Int))
               (arg_mode ((staticity Dynamic) (erasure Unerased)))
               (ret_ty (Type Int))
               (ret_mode ((staticity Static) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Unit)))
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
                (Arrow (arg_ty (Type Int))
                 (arg_mode ((staticity Dynamic) (erasure Unerased)))
                 (ret_ty (Type Int))
                 (ret_mode ((staticity Dynamic) (erasure Unerased))))))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Unit)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 21)))))
         (else_
          (Var (id g)
           (ty
            (Type
             (Pi
              (arg_ty
               (Type
                (Arrow (arg_ty (Type Int))
                 (arg_mode ((staticity Dynamic) (erasure Unerased)))
                 (ret_ty (Type Int))
                 (ret_mode ((staticity Static) (erasure Unerased))))))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Unit)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 28)))))
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Arrow (arg_ty (Type Int))
               (arg_mode ((staticity Dynamic) (erasure Unerased)))
               (ret_ty (Type Int))
               (ret_mode ((staticity Static) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Unit)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "smart constructors" =
  go
    {|
let f = fn (static x : int -> static int) -> ();;
let g = fn (static x : int -> static int) -> ();;
let _ = if true then f else g;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body (Literal (value Unit) (loc ((line 2) (column 45)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Arrow (arg_ty (Type Int))
               (arg_mode ((staticity Dynamic) (erasure Unerased)))
               (ret_ty (Type Int))
               (ret_mode ((staticity Static) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Unit)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Binder (arg x)
         (body (Literal (value Unit) (loc ((line 3) (column 45)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Arrow (arg_ty (Type Int))
               (arg_mode ((staticity Dynamic) (erasure Unerased)))
               (ret_ty (Type Int))
               (ret_mode ((staticity Static) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Unit)))
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
                (Arrow (arg_ty (Type Int))
                 (arg_mode ((staticity Dynamic) (erasure Unerased)))
                 (ret_ty (Type Int))
                 (ret_mode ((staticity Static) (erasure Unerased))))))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Unit)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 21)))))
         (else_
          (Var (id g)
           (ty
            (Type
             (Pi
              (arg_ty
               (Type
                (Arrow (arg_ty (Type Int))
                 (arg_mode ((staticity Dynamic) (erasure Unerased)))
                 (ret_ty (Type Int))
                 (ret_mode ((staticity Static) (erasure Unerased))))))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Unit)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 28)))))
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Arrow (arg_ty (Type Int))
               (arg_mode ((staticity Dynamic) (erasure Unerased)))
               (ret_ty (Type Int))
               (ret_mode ((staticity Static) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Unit)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "smart constructors" =
  go
    {|
let f = fn (static x : static int -> int) -> ();;
let g = fn (static x : int -> static int) -> ();;
let _ = if true then f else g;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (body (Literal (value Unit) (loc ((line 2) (column 45)))))
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
            (ret_ty (T (Type Unit)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Binder (arg x)
         (body (Literal (value Unit) (loc ((line 3) (column 45)))))
         (mono <opaque>)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Arrow (arg_ty (Type Int))
               (arg_mode ((staticity Dynamic) (erasure Unerased)))
               (ret_ty (Type Int))
               (ret_mode ((staticity Static) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Unit)))
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
                 (ret_ty (T (Type Int)))
                 (ret_mode ((staticity Dynamic) (erasure Unerased))))))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Unit)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 21)))))
         (else_
          (Var (id g)
           (ty
            (Type
             (Pi
              (arg_ty
               (Type
                (Arrow (arg_ty (Type Int))
                 (arg_mode ((staticity Dynamic) (erasure Unerased)))
                 (ret_ty (Type Int))
                 (ret_mode ((staticity Static) (erasure Unerased))))))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Unit)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 28)))))
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Arrow (arg_ty (Type Int))
               (arg_mode ((staticity Dynamic) (erasure Unerased)))
               (ret_ty (Type Int))
               (ret_mode ((staticity Static) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Unit)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;
