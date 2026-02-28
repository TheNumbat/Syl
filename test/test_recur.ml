open! Core
open! Syl

let go input =
  let cst = Parse.parse_exn input in
  match Typecheck.typecheck cst with
  | Ok tst -> print_s [%message (tst : Tst.Program.t)]
  | Error { loc; reason } -> print_s [%message (loc : Lex.Location.t) (reason : Typecheck.Error.t)]
;;

(* ===== Static recursion ===== *)

let%expect_test "pi function calling arrow function in same group" =
  go
    {|
fun inc (x : int) : int = x + 1
and f (static erased t : type) : t -> t = fn (x : t) -> x;;
let _ = f int 0;;
|};
  [%expect
    {|
    (tst
     ((Fun
       (funs
        ((Lambda (var inc) (arg x)
          (body
           (Binop (op Add)
            (lhs
             (Var (id x) (ty (Type Int))
              (mode ((staticity Dynamic) (erasure Unerased)))
              (loc ((line 2) (column 26)))))
            (rhs
             (Literal (value (Int (T 1))) (ty (Type Int))
              (mode ((staticity Static) (erasure Unerased)))
              (loc ((line 2) (column 30)))))
            (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
            (loc ((line 2) (column 28)))))
          (ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Unerased)))
          (loc ((line 2) (column 4))))
         (Binder (var f) (arg t)
          (body
           (Lambda (arg x) (erased Unerased)
            (arg_mode ((staticity ()) (erasure ())))
            (arg_ty (Var (id t) (loc ((line 3) (column 50)))))
            (body (Var (id x) (loc ((line 3) (column 56)))))
            (loc ((line 3) (column 42)))))
          (mono <opaque>)
          (ty
           (Type
            (Pi (arg_ty (Type Type))
             (arg_mode ((staticity Static) (erasure Erased)))
             (ret_ty
              (Reduce (env <opaque>) (arg t) (arg_ty (Type Type))
               (arg_mode ((staticity Static) (erasure Erased))) (memo <opaque>)
               (ret_ty
                (Arrow (arg (Var (id t) (loc ((line 3) (column 33)))))
                 (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
                 (ret (Var (id t) (loc ((line 3) (column 38)))))
                 (ret_mode ((staticity ()) (erasure ())))
                 (loc ((line 3) (column 35)))))))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Unerased)))
          (loc ((line 3) (column 4))))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Symbol
           (fn
            (Var (id f)
             (ty
              (Type
               (Pi (arg_ty (Type Type))
                (arg_mode ((staticity Static) (erasure Erased)))
                (ret_ty
                 (Reduce (env <opaque>) (arg t) (arg_ty (Type Type))
                  (arg_mode ((staticity Static) (erasure Erased)))
                  (memo <opaque>)
                  (ret_ty
                   (Arrow (arg (Var (id t) (loc ((line 3) (column 33)))))
                    (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
                    (ret (Var (id t) (loc ((line 3) (column 38)))))
                    (ret_mode ((staticity ()) (erasure ())))
                    (loc ((line 3) (column 35)))))))
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
           (loc ((line 4) (column 14)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "arrow function calling pi function in same group" =
  go
    {|
fun f (static erased t : type) : t -> t = fn (x : t) -> x
and g (x : int) : int = f int x;;
let _ = g 5;;
|};
  [%expect
    {|
    (tst
     ((Fun
       (funs
        ((Binder (var f) (arg t)
          (body
           (Lambda (arg x) (erased Unerased)
            (arg_mode ((staticity ()) (erasure ())))
            (arg_ty (Var (id t) (loc ((line 2) (column 50)))))
            (body (Var (id x) (loc ((line 2) (column 56)))))
            (loc ((line 2) (column 42)))))
          (mono <opaque>)
          (ty
           (Type
            (Pi (arg_ty (Type Type))
             (arg_mode ((staticity Static) (erasure Erased)))
             (ret_ty
              (Reduce (env <opaque>) (arg t) (arg_ty (Type Type))
               (arg_mode ((staticity Static) (erasure Erased))) (memo <opaque>)
               (ret_ty
                (Arrow (arg (Var (id t) (loc ((line 2) (column 33)))))
                 (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
                 (ret (Var (id t) (loc ((line 2) (column 38)))))
                 (ret_mode ((staticity ()) (erasure ())))
                 (loc ((line 2) (column 35)))))))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Unerased)))
          (loc ((line 2) (column 4))))
         (Lambda (var g) (arg x)
          (body
           (Apply
            (fn
             (Symbol
              (fn
               (Var (id f)
                (ty
                 (Type
                  (Pi (arg_ty (Type Type))
                   (arg_mode ((staticity Static) (erasure Erased)))
                   (ret_ty
                    (Reduce (env <opaque>) (arg t) (arg_ty (Type Type))
                     (arg_mode ((staticity Static) (erasure Erased)))
                     (memo <opaque>)
                     (ret_ty
                      (Arrow (arg (Var (id t) (loc ((line 2) (column 33)))))
                       (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
                       (ret (Var (id t) (loc ((line 2) (column 38)))))
                       (ret_mode ((staticity ()) (erasure ())))
                       (loc ((line 2) (column 35)))))))
                   (ret_mode ((staticity Dynamic) (erasure Unerased))))))
                (mode ((staticity Static) (erasure Unerased)))
                (loc ((line 3) (column 24)))))
              (key IntT)
              (ty
               (Type
                (Arrow (arg_ty (Type Int))
                 (arg_mode ((staticity Dynamic) (erasure Unerased)))
                 (ret_ty (Type Int))
                 (ret_mode ((staticity Dynamic) (erasure Unerased))))))
              (mode ((staticity Dynamic) (erasure Unerased)))
              (loc ((line 3) (column 24)))))
            (arg
             (Var (id x) (ty (Type Int))
              (mode ((staticity Dynamic) (erasure Unerased)))
              (loc ((line 3) (column 30)))))
            (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
            (loc ((line 3) (column 24)))))
          (ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Unerased)))
          (loc ((line 3) (column 4))))))
       (loc ((line 2) (column 0))))
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
          (Literal (value (Int (T 5))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 10)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "pi calling arrow from same group at application" =
  go
    {|
fun inc (x : int) : int = x + 1
and choose (static erased b : bool) : int -> int =
  if static b then fn (x : int) -> inc x else fn (x : int) -> x;;
let _ = choose true 5;;
let _ = choose false 5;;
|};
  [%expect
    {|
    (tst
     ((Fun
       (funs
        ((Lambda (var inc) (arg x)
          (body
           (Binop (op Add)
            (lhs
             (Var (id x) (ty (Type Int))
              (mode ((staticity Dynamic) (erasure Unerased)))
              (loc ((line 2) (column 26)))))
            (rhs
             (Literal (value (Int (T 1))) (ty (Type Int))
              (mode ((staticity Static) (erasure Unerased)))
              (loc ((line 2) (column 30)))))
            (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
            (loc ((line 2) (column 28)))))
          (ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Unerased)))
          (loc ((line 2) (column 4))))
         (Binder (var choose) (arg b)
          (body
           (If (cond (Var (id b) (loc ((line 4) (column 12)))))
            (then_
             (Lambda (arg x) (erased Unerased)
              (arg_mode ((staticity ()) (erasure ())))
              (arg_ty (Var (id int) (loc ((line 4) (column 27)))))
              (body
               (Apply (fn (Var (id inc) (loc ((line 4) (column 35)))))
                (arg (Var (id x) (loc ((line 4) (column 39)))))
                (loc ((line 4) (column 35)))))
              (loc ((line 4) (column 19)))))
            (else_
             (Lambda (arg x) (erased Unerased)
              (arg_mode ((staticity ()) (erasure ())))
              (arg_ty (Var (id int) (loc ((line 4) (column 54)))))
              (body (Var (id x) (loc ((line 4) (column 62)))))
              (loc ((line 4) (column 46)))))
            (static true) (loc ((line 4) (column 2)))))
          (mono <opaque>)
          (ty
           (Type
            (Pi (arg_ty (Type Bool))
             (arg_mode ((staticity Static) (erasure Erased)))
             (ret_ty
              (T
               (Type
                (Arrow (arg_ty (Type Int))
                 (arg_mode ((staticity Dynamic) (erasure Unerased)))
                 (ret_ty (Type Int))
                 (ret_mode ((staticity Dynamic) (erasure Unerased)))))))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Unerased)))
          (loc ((line 3) (column 4))))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Symbol
           (fn
            (Var (id choose)
             (ty
              (Type
               (Pi (arg_ty (Type Bool))
                (arg_mode ((staticity Static) (erasure Erased)))
                (ret_ty
                 (T
                  (Type
                   (Arrow (arg_ty (Type Int))
                    (arg_mode ((staticity Dynamic) (erasure Unerased)))
                    (ret_ty (Type Int))
                    (ret_mode ((staticity Dynamic) (erasure Unerased)))))))
                (ret_mode ((staticity Dynamic) (erasure Unerased))))))
             (mode ((staticity Static) (erasure Unerased)))
             (loc ((line 5) (column 8)))))
           (key (Bool true))
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Dynamic) (erasure Unerased)))
           (loc ((line 5) (column 8)))))
         (arg
          (Literal (value (Int (T 5))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 5) (column 20)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 5) (column 8)))))
       (loc ((line 5) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Symbol
           (fn
            (Var (id choose)
             (ty
              (Type
               (Pi (arg_ty (Type Bool))
                (arg_mode ((staticity Static) (erasure Erased)))
                (ret_ty
                 (T
                  (Type
                   (Arrow (arg_ty (Type Int))
                    (arg_mode ((staticity Dynamic) (erasure Unerased)))
                    (ret_ty (Type Int))
                    (ret_mode ((staticity Dynamic) (erasure Unerased)))))))
                (ret_mode ((staticity Dynamic) (erasure Unerased))))))
             (mode ((staticity Static) (erasure Unerased)))
             (loc ((line 6) (column 8)))))
           (key (Bool false))
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Dynamic) (erasure Unerased)))
           (loc ((line 6) (column 8)))))
         (arg
          (Literal (value (Int (T 5))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 6) (column 21)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 6) (column 8)))))
       (loc ((line 6) (column 0))))))
    |}]
;;

let%expect_test "mutual pi recursion" =
  go
    {|
fun f (static erased t : type) : t -> t = fn (x : t) -> x
and g (static erased t : type) : t -> t = f t;;
let _ = g int 0;;
|};
  [%expect
    {|
    (tst
     ((Fun
       (funs
        ((Binder (var f) (arg t)
          (body
           (Lambda (arg x) (erased Unerased)
            (arg_mode ((staticity ()) (erasure ())))
            (arg_ty (Var (id t) (loc ((line 2) (column 50)))))
            (body (Var (id x) (loc ((line 2) (column 56)))))
            (loc ((line 2) (column 42)))))
          (mono <opaque>)
          (ty
           (Type
            (Pi (arg_ty (Type Type))
             (arg_mode ((staticity Static) (erasure Erased)))
             (ret_ty
              (Reduce (env <opaque>) (arg t) (arg_ty (Type Type))
               (arg_mode ((staticity Static) (erasure Erased))) (memo <opaque>)
               (ret_ty
                (Arrow (arg (Var (id t) (loc ((line 2) (column 33)))))
                 (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
                 (ret (Var (id t) (loc ((line 2) (column 38)))))
                 (ret_mode ((staticity ()) (erasure ())))
                 (loc ((line 2) (column 35)))))))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Unerased)))
          (loc ((line 2) (column 4))))
         (Binder (var g) (arg t)
          (body
           (Apply (fn (Var (id f) (loc ((line 3) (column 42)))))
            (arg (Var (id t) (loc ((line 3) (column 44)))))
            (loc ((line 3) (column 42)))))
          (mono <opaque>)
          (ty
           (Type
            (Pi (arg_ty (Type Type))
             (arg_mode ((staticity Static) (erasure Erased)))
             (ret_ty
              (Reduce (env <opaque>) (arg t) (arg_ty (Type Type))
               (arg_mode ((staticity Static) (erasure Erased))) (memo <opaque>)
               (ret_ty
                (Arrow (arg (Var (id t) (loc ((line 3) (column 33)))))
                 (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
                 (ret (Var (id t) (loc ((line 3) (column 38)))))
                 (ret_mode ((staticity ()) (erasure ())))
                 (loc ((line 3) (column 35)))))))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Unerased)))
          (loc ((line 3) (column 4))))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Symbol
           (fn
            (Var (id g)
             (ty
              (Type
               (Pi (arg_ty (Type Type))
                (arg_mode ((staticity Static) (erasure Erased)))
                (ret_ty
                 (Reduce (env <opaque>) (arg t) (arg_ty (Type Type))
                  (arg_mode ((staticity Static) (erasure Erased)))
                  (memo <opaque>)
                  (ret_ty
                   (Arrow (arg (Var (id t) (loc ((line 3) (column 33)))))
                    (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
                    (ret (Var (id t) (loc ((line 3) (column 38)))))
                    (ret_mode ((staticity ()) (erasure ())))
                    (loc ((line 3) (column 35)))))))
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
           (loc ((line 4) (column 14)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "static recursion with base case" =
  go
    {|
fun f (static x : int) : int = if static x == 0 then 42 else f (x - 1);;
let _ = f 3;;
|};
  [%expect
    {|
    (tst
     ((Fun
       (funs
        ((Binder (var f) (arg x)
          (body
           (If
            (cond
             (Binop (op Eq) (lhs (Var (id x) (loc ((line 2) (column 41)))))
              (rhs (Literal (value (Int 0)) (loc ((line 2) (column 46)))))
              (loc ((line 2) (column 43)))))
            (then_ (Literal (value (Int 42)) (loc ((line 2) (column 53)))))
            (else_
             (Apply (fn (Var (id f) (loc ((line 2) (column 61)))))
              (arg
               (Paren
                (expr
                 (Binop (op Sub) (lhs (Var (id x) (loc ((line 2) (column 64)))))
                  (rhs (Literal (value (Int 1)) (loc ((line 2) (column 68)))))
                  (loc ((line 2) (column 66)))))
                (loc ((line 2) (column 63)))))
              (loc ((line 2) (column 61)))))
            (static true) (loc ((line 2) (column 31)))))
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
         (key (Int 3)) (ty (Type Int))
         (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (static x : int) : erased int = (if static x == 0 then 42 else f (x - 1)) @ erased;;
let _ = f 3;;
|};
  [%expect
    {|
    (tst
     ((Fun
       (funs
        ((Binder (var f) (arg x)
          (body
           (Mode_annotation
            (expr
             (Paren
              (expr
               (If
                (cond
                 (Binop (op Eq) (lhs (Var (id x) (loc ((line 2) (column 49)))))
                  (rhs (Literal (value (Int 0)) (loc ((line 2) (column 54)))))
                  (loc ((line 2) (column 51)))))
                (then_ (Literal (value (Int 42)) (loc ((line 2) (column 61)))))
                (else_
                 (Apply (fn (Var (id f) (loc ((line 2) (column 69)))))
                  (arg
                   (Paren
                    (expr
                     (Binop (op Sub)
                      (lhs (Var (id x) (loc ((line 2) (column 72)))))
                      (rhs
                       (Literal (value (Int 1)) (loc ((line 2) (column 76)))))
                      (loc ((line 2) (column 74)))))
                    (loc ((line 2) (column 71)))))
                  (loc ((line 2) (column 69)))))
                (static true) (loc ((line 2) (column 39)))))
              (loc ((line 2) (column 38)))))
            (mode ((staticity ()) (erasure (Erased))))
            (loc ((line 2) (column 80)))))
          (mono <opaque>)
          (ty
           (Type
            (Pi (arg_ty (Type Int))
             (arg_mode ((staticity Static) (erasure Unerased)))
             (ret_ty (T (Type Int)))
             (ret_mode ((staticity Dynamic) (erasure Erased))))))
          (mode ((staticity Static) (erasure Erased)))
          (loc ((line 2) (column 4))))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Let (var x)
         (bind
          (Literal (value (Int (T 3))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 3) (column 10)))))
         (rest
          (Let (var x)
           (bind
            (Binop (op Sub)
             (lhs
              (Var (id x) (ty (Type Int))
               (mode ((staticity Static) (erasure Unerased)))
               (loc ((line 2) (column 72)))))
             (rhs
              (Literal (value (Int (T 1))) (ty (Type Int))
               (mode ((staticity Static) (erasure Unerased)))
               (loc ((line 2) (column 76)))))
             (ty (Type Int)) (mode ((staticity Static) (erasure Unerased)))
             (loc ((line 2) (column 74)))))
           (rest
            (Let (var x)
             (bind
              (Binop (op Sub)
               (lhs
                (Var (id x) (ty (Type Int))
                 (mode ((staticity Static) (erasure Unerased)))
                 (loc ((line 2) (column 72)))))
               (rhs
                (Literal (value (Int (T 1))) (ty (Type Int))
                 (mode ((staticity Static) (erasure Unerased)))
                 (loc ((line 2) (column 76)))))
               (ty (Type Int)) (mode ((staticity Static) (erasure Unerased)))
               (loc ((line 2) (column 74)))))
             (rest
              (Let (var x)
               (bind
                (Binop (op Sub)
                 (lhs
                  (Var (id x) (ty (Type Int))
                   (mode ((staticity Static) (erasure Unerased)))
                   (loc ((line 2) (column 72)))))
                 (rhs
                  (Literal (value (Int (T 1))) (ty (Type Int))
                   (mode ((staticity Static) (erasure Unerased)))
                   (loc ((line 2) (column 76)))))
                 (ty (Type Int)) (mode ((staticity Static) (erasure Unerased)))
                 (loc ((line 2) (column 74)))))
               (rest
                (Literal (value (Int (T 42))) (ty (Type Int))
                 (mode ((staticity Dynamic) (erasure Erased)))
                 (loc ((line 2) (column 80)))))
               (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
               (loc ((line 2) (column 69)))))
             (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
             (loc ((line 2) (column 69)))))
           (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
           (loc ((line 2) (column 69)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "arrow and pi mutual recursion with application" =
  go
    {|
fun double (x : int) : int = x + x
and apply_double (static erased t : type) : int -> int = fn (x : int) -> double x;;
let _ = apply_double int 5;;
|};
  [%expect
    {|
    (tst
     ((Fun
       (funs
        ((Lambda (var double) (arg x)
          (body
           (Binop (op Add)
            (lhs
             (Var (id x) (ty (Type Int))
              (mode ((staticity Dynamic) (erasure Unerased)))
              (loc ((line 2) (column 29)))))
            (rhs
             (Var (id x) (ty (Type Int))
              (mode ((staticity Dynamic) (erasure Unerased)))
              (loc ((line 2) (column 33)))))
            (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
            (loc ((line 2) (column 31)))))
          (ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Unerased)))
          (loc ((line 2) (column 4))))
         (Binder (var apply_double) (arg t)
          (body
           (Lambda (arg x) (erased Unerased)
            (arg_mode ((staticity ()) (erasure ())))
            (arg_ty (Var (id int) (loc ((line 3) (column 65)))))
            (body
             (Apply (fn (Var (id double) (loc ((line 3) (column 73)))))
              (arg (Var (id x) (loc ((line 3) (column 80)))))
              (loc ((line 3) (column 73)))))
            (loc ((line 3) (column 57)))))
          (mono <opaque>)
          (ty
           (Type
            (Pi (arg_ty (Type Type))
             (arg_mode ((staticity Static) (erasure Erased)))
             (ret_ty
              (T
               (Type
                (Arrow (arg_ty (Type Int))
                 (arg_mode ((staticity Dynamic) (erasure Unerased)))
                 (ret_ty (Type Int))
                 (ret_mode ((staticity Dynamic) (erasure Unerased)))))))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Unerased)))
          (loc ((line 3) (column 4))))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Symbol
           (fn
            (Var (id apply_double)
             (ty
              (Type
               (Pi (arg_ty (Type Type))
                (arg_mode ((staticity Static) (erasure Erased)))
                (ret_ty
                 (T
                  (Type
                   (Arrow (arg_ty (Type Int))
                    (arg_mode ((staticity Dynamic) (erasure Unerased)))
                    (ret_ty (Type Int))
                    (ret_mode ((staticity Dynamic) (erasure Unerased)))))))
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
          (Literal (value (Int (T 5))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 25)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "mutually recursive fun with static arg" =
  go
    {|
fun id1 (static erased t : type) : t -> t = fn (x : t) -> x
and id2 (static erased t : type) : t -> t = id1 t;;
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
          (loc ((line 2) (column 4))))
         (Binder (var id2) (arg t)
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
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "infinite static recursion" =
  go
    {|
fun f (static x : int) : int = f x;;
let _ = f 0;;
|};
  [%expect {| ((loc ((line 2) (column 31))) (reason (Recursion_limit 1000))) |}]
;;

let%expect_test "infinite static recursion" =
  go
    {|
fun f (static x : int) : erased int = f x;;
let _ = f 0;;
|};
  [%expect {| ((loc ((line 2) (column 38))) (reason (Recursion_limit 1000))) |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : erased int = f x;;
let _ = f 0;;
|};
  [%expect {| ((loc ((line 2) (column 4))) (reason (Inline_self (f)))) |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = g x
and erased g (y : int) : int = y;;
let _ = f 0;;
|};
  [%expect
    {|
    (tst
     ((Fun
       (funs
        ((Lambda (var f) (arg x)
          (body
           (Let (var y)
            (bind
             (Var (id x) (ty (Type Int))
              (mode ((staticity Dynamic) (erasure Unerased)))
              (loc ((line 2) (column 26)))))
            (rest
             (Var (id y) (ty (Type Int))
              (mode ((staticity Dynamic) (erasure Unerased)))
              (loc ((line 3) (column 31)))))
            (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
            (loc ((line 2) (column 24)))))
          (ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Unerased)))
          (loc ((line 2) (column 4))))
         (Lambda (var g) (arg y)
          (body
           (Var (id y) (ty (Type Int))
            (mode ((staticity Dynamic) (erasure Unerased)))
            (loc ((line 3) (column 31)))))
          (ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Erased)))
          (loc ((line 3) (column 4))))))
       (loc ((line 2) (column 0))))
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
           (loc ((line 4) (column 8)))))
         (arg
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 10)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = (g @ erased) x
and g (y : int) : int = y;;
let _ = f 0;;
|};
  [%expect
    {|
    (tst
     ((Fun
       (funs
        ((Lambda (var f) (arg x)
          (body
           (Let (var y)
            (bind
             (Var (id x) (ty (Type Int))
              (mode ((staticity Dynamic) (erasure Unerased)))
              (loc ((line 2) (column 37)))))
            (rest
             (Var (id y) (ty (Type Int))
              (mode ((staticity Dynamic) (erasure Unerased)))
              (loc ((line 3) (column 24)))))
            (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
            (loc ((line 2) (column 24)))))
          (ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Unerased)))
          (loc ((line 2) (column 4))))
         (Lambda (var g) (arg y)
          (body
           (Var (id y) (ty (Type Int))
            (mode ((staticity Dynamic) (erasure Unerased)))
            (loc ((line 3) (column 24)))))
          (ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Unerased)))
          (loc ((line 3) (column 4))))))
       (loc ((line 2) (column 0))))
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
           (loc ((line 4) (column 8)))))
         (arg
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 10)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : erased int = g x
and g (y : int) : erased int = y;;
let _ = f 0;;
|};
  [%expect
    {|
    (tst
     ((Fun
       (funs
        ((Lambda (var f) (arg x)
          (body
           (Let (var y)
            (bind
             (Var (id x) (ty (Type Int))
              (mode ((staticity Dynamic) (erasure Unerased)))
              (loc ((line 2) (column 33)))))
            (rest
             (Erased (ty (Type Int))
              (mode ((staticity Dynamic) (erasure Erased)))
              (loc ((line 2) (column 31)))))
            (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
            (loc ((line 2) (column 31)))))
          (ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Erased))))))
          (mode ((staticity Static) (erasure Erased)))
          (loc ((line 2) (column 4))))
         (Lambda (var g) (arg y)
          (body
           (Var (id y) (ty (Type Int))
            (mode ((staticity Dynamic) (erasure Unerased)))
            (loc ((line 3) (column 31)))))
          (ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Erased))))))
          (mode ((staticity Static) (erasure Erased)))
          (loc ((line 3) (column 4))))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Let (var x)
         (bind
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 10)))))
         (rest
          (Let (var y)
           (bind
            (Var (id x) (ty (Type Int))
             (mode ((staticity Dynamic) (erasure Unerased)))
             (loc ((line 2) (column 33)))))
           (rest
            (Erased (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
             (loc ((line 2) (column 31)))))
           (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
           (loc ((line 2) (column 31)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun erased f (x : int) : int = (g @ erased) x
and g (y : int) : int = y;;
let _ = f 0;;
|};
  [%expect
    {|
    (tst
     ((Fun
       (funs
        ((Lambda (var f) (arg x)
          (body
           (Let (var y)
            (bind
             (Var (id x) (ty (Type Int))
              (mode ((staticity Dynamic) (erasure Unerased)))
              (loc ((line 2) (column 44)))))
            (rest
             (Var (id y) (ty (Type Int))
              (mode ((staticity Dynamic) (erasure Unerased)))
              (loc ((line 3) (column 24)))))
            (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
            (loc ((line 2) (column 31)))))
          (ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Erased)))
          (loc ((line 2) (column 4))))
         (Lambda (var g) (arg y)
          (body
           (Var (id y) (ty (Type Int))
            (mode ((staticity Dynamic) (erasure Unerased)))
            (loc ((line 3) (column 24)))))
          (ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Unerased)))
          (loc ((line 3) (column 4))))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Let (var x)
         (bind
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 10)))))
         (rest
          (Let (var y)
           (bind
            (Var (id x) (ty (Type Int))
             (mode ((staticity Dynamic) (erasure Unerased)))
             (loc ((line 2) (column 44)))))
           (rest
            (Var (id y) (ty (Type Int))
             (mode ((staticity Dynamic) (erasure Unerased)))
             (loc ((line 3) (column 24)))))
           (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
           (loc ((line 2) (column 31)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = g x
and g (y : int) : int = f y;;
let _ = f 0;;
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
             (Var (id g)
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
          (loc ((line 2) (column 4))))
         (Lambda (var g) (arg y)
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
              (loc ((line 3) (column 24)))))
            (arg
             (Var (id y) (ty (Type Int))
              (mode ((staticity Dynamic) (erasure Unerased)))
              (loc ((line 3) (column 26)))))
            (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
            (loc ((line 3) (column 24)))))
          (ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Unerased)))
          (loc ((line 3) (column 4))))))
       (loc ((line 2) (column 0))))
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
           (loc ((line 4) (column 8)))))
         (arg
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 10)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun erased f (x : int) : int = g x
and erased g (y : int) : int = f y;;
let _ = f 0;;
|};
  [%expect {| ((loc ((line 2) (column 4))) (reason (Inline_self (f g)))) |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = g x
and erased g (y : int) : int = f y;;
let _ = (f @ erased) 0;;
|};
  [%expect
    {|
    (tst
     ((Fun
       (funs
        ((Lambda (var f) (arg x)
          (body
           (Let (var y)
            (bind
             (Var (id x) (ty (Type Int))
              (mode ((staticity Dynamic) (erasure Unerased)))
              (loc ((line 2) (column 26)))))
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
                (mode ((staticity Static) (erasure Unerased)))
                (loc ((line 3) (column 31)))))
              (arg
               (Var (id y) (ty (Type Int))
                (mode ((staticity Dynamic) (erasure Unerased)))
                (loc ((line 3) (column 33)))))
              (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
              (loc ((line 3) (column 31)))))
            (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
            (loc ((line 2) (column 24)))))
          (ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Unerased)))
          (loc ((line 2) (column 4))))
         (Lambda (var g) (arg y)
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
              (loc ((line 3) (column 31)))))
            (arg
             (Var (id y) (ty (Type Int))
              (mode ((staticity Dynamic) (erasure Unerased)))
              (loc ((line 3) (column 33)))))
            (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
            (loc ((line 3) (column 31)))))
          (ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Erased)))
          (loc ((line 3) (column 4))))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Let (var x)
         (bind
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 21)))))
         (rest
          (Let (var y)
           (bind
            (Var (id x) (ty (Type Int))
             (mode ((staticity Dynamic) (erasure Unerased)))
             (loc ((line 2) (column 26)))))
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
               (mode ((staticity Static) (erasure Unerased)))
               (loc ((line 3) (column 31)))))
             (arg
              (Var (id y) (ty (Type Int))
               (mode ((staticity Dynamic) (erasure Unerased)))
               (loc ((line 3) (column 33)))))
             (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
             (loc ((line 3) (column 31)))))
           (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
           (loc ((line 2) (column 24)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = g x
and g (y : int) : int = (f @ erased) y;;
let _ = f 0;;
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
             (Var (id g)
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
          (loc ((line 2) (column 4))))
         (Lambda (var g) (arg y)
          (body
           (Let (var x)
            (bind
             (Var (id y) (ty (Type Int))
              (mode ((staticity Dynamic) (erasure Unerased)))
              (loc ((line 3) (column 37)))))
            (rest
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
                (loc ((line 2) (column 24)))))
              (arg
               (Var (id x) (ty (Type Int))
                (mode ((staticity Dynamic) (erasure Unerased)))
                (loc ((line 2) (column 26)))))
              (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
              (loc ((line 2) (column 24)))))
            (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
            (loc ((line 3) (column 24)))))
          (ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Unerased)))
          (loc ((line 3) (column 4))))))
       (loc ((line 2) (column 0))))
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
           (loc ((line 4) (column 8)))))
         (arg
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 10)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = (g @ erased) x
and g (y : int) : int = (f @ erased) y;;
let _ = f 0;;
|};
  [%expect {| ((loc ((line 2) (column 4))) (reason (Inline_self (f g)))) |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = (g @ erased) x
and g (y : int) : int = (f @ erased) y;;
let _ = f 0;;
|};
  [%expect {| ((loc ((line 2) (column 4))) (reason (Inline_self (f g)))) |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : erased int = g x @ erased
and g (y : int) : erased int = (f @ erased) y;;
let _ = f 0;;
|};
  [%expect {| ((loc ((line 2) (column 4))) (reason (Inline_self (f g)))) |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : erased int = g x
and g (y : int) : int = let _ = f y in 0;;
let _ = g 0;;
|};
  [%expect {|
    (tst
     ((Fun
       (funs
        ((Lambda (var f) (arg x)
          (body
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
              (loc ((line 2) (column 31)))))
            (arg
             (Var (id x) (ty (Type Int))
              (mode ((staticity Dynamic) (erasure Unerased)))
              (loc ((line 2) (column 33)))))
            (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
            (loc ((line 2) (column 31)))))
          (ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Erased))))))
          (mode ((staticity Static) (erasure Erased)))
          (loc ((line 2) (column 4))))
         (Lambda (var g) (arg y)
          (body
           (Let (var _)
            (bind
             (Let (var x)
              (bind
               (Var (id y) (ty (Type Int))
                (mode ((staticity Dynamic) (erasure Unerased)))
                (loc ((line 3) (column 34)))))
              (rest
               (Erased (ty (Type Int))
                (mode ((staticity Dynamic) (erasure Erased)))
                (loc ((line 3) (column 32)))))
              (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
              (loc ((line 3) (column 32)))))
            (rest
             (Literal (value (Int (T 0))) (ty (Type Int))
              (mode ((staticity Static) (erasure Unerased)))
              (loc ((line 3) (column 39)))))
            (ty (Type Int)) (mode ((staticity Static) (erasure Unerased)))
            (loc ((line 3) (column 24)))))
          (ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Unerased)))
          (loc ((line 3) (column 4))))))
       (loc ((line 2) (column 0))))
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
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 4) (column 10)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
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
    (tst
     ((Fun
       (funs
        ((Lambda (var f) (arg x)
          (body
           (Literal (value (Int (T 0))) (ty (Type Int))
            (mode ((staticity Static) (erasure Unerased)))
            (loc ((line 2) (column 31)))))
          (ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Erased)))
          (loc ((line 2) (column 4))))))
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
           (mode ((staticity Dynamic) (erasure Unerased)))
           (loc ((line 2) (column 31)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "erased fn" =
  go
    {|
let f = fn erased (x : int) -> 0;;
let _ = f 0;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Lambda (arg x)
         (body
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased)))
           (loc ((line 2) (column 31)))))
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
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
           (mode ((staticity Dynamic) (erasure Unerased)))
           (loc ((line 2) (column 31)))))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;
