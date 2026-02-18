open! Core
open! Syl

let go input =
  let cst = Parse.parse_exn input in
  match Typecheck.typecheck cst with
  | Ok tst -> print_s [%message (tst : Tst.Program.t)]
  | Error { loc; reason } -> print_s [%message (loc : Lex.Location.t) (reason : Typecheck.Error.t)]
;;

let%expect_test "literals" =
  go
    {|
let _ = ();;
let _ = true;;
let _ = 123;;
let _ = () @ erased;;
let _ = true @ erased;;
let _ = 123 @ erased;;
let _ = () @ dynamic;;
let _ = true @ dynamic;;
let _ = 123 @ dynamic;;|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (Literal (value Unit) (ty (Type Unit))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Literal (value (Bool (T true))) (ty (Type Bool))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Literal (value (Int (T 123))) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Literal (value Unit) (ty (Type Unit))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Literal (value (Bool (T true))) (ty (Type Bool))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Literal (value (Int (T 123))) (ty (Type Int))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Literal (value Unit) (ty (Type Unit))
         (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Literal (value (Bool (T true))) (ty (Type Bool))
         (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Literal (value (Int (T 123))) (ty (Type Int))
         (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Mode annotation valid static" =
  go
    {|
let _ =
  1 @ static
;;|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Mode annotation valid dynamic" =
  go
    {|
let _ =
  1 @ dynamic
;;|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Mode annotation invalid" =
  go
    {|
let dyn = 1 @ dynamic;;
let _ =
  dyn @ static
;;|};
  [%expect
    {|
    ((loc ((line 4) (column 6)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "Mode annotation valid" =
  go
    {|
let dyn = 1;;
let _ =
  dyn @ dynamic erased
;;|};
  [%expect
    {|
    (tst
     ((Let (var dyn)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Erased (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Mode annotation valid" =
  go
    {|
let dyn = 1 @ erased;;
let _ =
  dyn @ dynamic erased
;;|};
  [%expect
    {|
    (tst
     ((Let (var dyn)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Dynamic) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Mode annotation valid" =
  go
    {|
let dyn = 1;;
let _ =
  dyn @ dynamic
;;|};
  [%expect
    {|
    (tst
     ((Let (var dyn)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Var (id dyn) (ty (Type Int))
         (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Mode annotation valid" =
  go
    {|
let dyn = 1 @ dynamic;;
let _ =
  dyn @ dynamic erased
;;|};
  [%expect
    {|
    (tst
     ((Let (var dyn)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Erased (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Lambda return dynamic erased" =
  go
    {|
let x =
  (fn (x : int) -> 1 @ dynamic erased) 0
;;
let y = x @ static unerased;;
|};
  [%expect
    {|
    ((loc ((line 5) (column 10)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "Lambda return dynamic erased" =
  go
    {|
let x =
  ((fn (x : int) -> 1 @ dynamic erased) @ dynamic) 0
;;
|};
  [%expect {| ((loc ((line 3) (column 2))) (reason Dynamic_erased)) |}]
;;

let%expect_test "Lambda return dynamic unerased" =
  go
    {|
let x =
  (fn (x : int) -> x) 0
;;
let y = x @ static unerased;;
|};
  [%expect
    {|
    ((loc ((line 5) (column 10)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "Lambda capture static erased" =
  go
    {|
let x =
  (fn (x : int) -> int) 0
;;
let y = x @ unerased;;
|};
  [%expect
    {|
    ((loc ((line 5) (column 10)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "Unop static" =
  go
    {|
let _ =
  !true
;;|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (Unop (op Not)
         (arg
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Bool)) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Unop dynamic" =
  go
    {|
let _ =
  !(true @ dynamic)
;;|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (Unop (op Not)
         (arg
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Bool)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "dynamic static erased" =
  go
    {|
let _ =
  (true @ static erased)
;;|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (Literal (value (Bool (T true))) (ty (Type Bool))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "dynamic erased" =
  go
    {|
let _ =
  (true @ dynamic erased)
;;|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (Erased (ty (Type Bool)) (mode ((staticity Dynamic) (erasure Erased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "erased dynamic" =
  go
    {|
let _ =
  ((true @ erased) @ dynamic)
;;|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (Literal (value (Bool (T true))) (ty (Type Bool))
         (mode ((staticity Dynamic) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Unop var static" =
  go
    {|
let dyn = true;;
let _ =
  !dyn
;;|};
  [%expect
    {|
    (tst
     ((Let (var dyn)
       (bind
        (Literal (value (Bool (T true))) (ty (Type Bool))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Unop (op Not)
         (arg
          (Var (id dyn) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Bool)) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Unop var erased" =
  go
    {|
let dyn = true @ erased;;
let _ =
  !dyn
;;|};
  [%expect
    {|
    (tst
     ((Let (var dyn)
       (bind
        (Literal (value (Bool (T true))) (ty (Type Bool))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Unop (op Not)
         (arg
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (ty (Type Bool)) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Unop var erased" =
  go
    {|
let dyn = true @ erased;;
let _ =
  !(!dyn @ erased)
;;|};
  [%expect
    {|
    (tst
     ((Let (var dyn)
       (bind
        (Literal (value (Bool (T true))) (ty (Type Bool))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Unop (op Not)
         (arg
          (Literal (value (Bool (T false))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (ty (Type Bool)) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Unop var dynamic" =
  go
    {|
let dyn = true @ dynamic;;
let _ =
  !dyn
;;|};
  [%expect
    {|
    (tst
     ((Let (var dyn)
       (bind
        (Literal (value (Bool (T true))) (ty (Type Bool))
         (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Unop (op Not)
         (arg
          (Var (id dyn) (ty (Type Bool))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Bool)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Unop var dynamic erased" =
  go
    {|
let dyn = true @ erased dynamic;;
let _ = !dyn;;|};
  [%expect {| ((loc ((line 3) (column 8))) (reason Dynamic_erased)) |}]
;;

let%expect_test "Unop var dynamic" =
  go
    {|
let dyn = true @ dynamic;;
let x = !dyn @ erased;;|};
  [%expect
    {|
    (tst
     ((Let (var dyn)
       (bind
        (Literal (value (Bool (T true))) (ty (Type Bool))
         (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var x)
       (bind
        (Erased (ty (Type Bool)) (mode ((staticity Dynamic) (erasure Erased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Binop static + static" =
  go
    {|
let _ =
  1 + 2
;;|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (Binop (op Add)
         (lhs
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (rhs
          (Literal (value (Int (T 2))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Binop static + static erased" =
  go
    {|
let _ =
  1 + (2 @ erased)
;;|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (Binop (op Add)
         (lhs
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (rhs
          (Literal (value (Int (T 2))) (ty (Type Int))
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Unop erased dynamic" =
  go
    {|
let _ =
  !(true @ erased dynamic)
;;|};
  [%expect {| ((loc ((line 3) (column 2))) (reason Dynamic_erased)) |}]
;;

let%expect_test "Binop erased dynamic" =
  go
    {|
let _ =
  1 + (2 + 3 @ erased dynamic)
;;|};
  [%expect {| ((loc ((line 3) (column 4))) (reason Dynamic_erased)) |}]
;;

let%expect_test "Binop erased dynamic" =
  go
    {|
let _ =
  1 + (2 @ dynamic) + (3 @ erased)
;;|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (Binop (op Add)
         (lhs
          (Binop (op Add)
           (lhs
            (Literal (value (Int (T 1))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
           (rhs
            (Literal (value (Int (T 2))) (ty (Type Int))
             (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
           (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
           (loc <opaque>)))
         (rhs
          (Literal (value (Int (T 3))) (ty (Type Int))
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Binop erased static" =
  go
    {|
let _ =
  1 + ((2 + 3) @ erased)
;;|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (Binop (op Add)
         (lhs
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (rhs
          (Literal (value (Int (T 5))) (ty (Type Int))
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Binop static + dynamic" =
  go
    {|
let dyn = 2 @ dynamic;;
let _ =
  1 + dyn
;;|};
  [%expect
    {|
    (tst
     ((Let (var dyn)
       (bind
        (Literal (value (Int (T 2))) (ty (Type Int))
         (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Binop (op Add)
         (lhs
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (rhs
          (Var (id dyn) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Binop dynamic + static" =
  go
    {|
let dyn = 1 @ dynamic;;
let _ =
  dyn + 2
;;|};
  [%expect
    {|
    (tst
     ((Let (var dyn)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Binop (op Add)
         (lhs
          (Var (id dyn) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (rhs
          (Literal (value (Int (T 2))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Binop dynamic + dynamic" =
  go
    {|
let dyn1 = 1 @ dynamic;;
let dyn2 = 2 @ dynamic;;
let _ =
  dyn1 + dyn2
;;|};
  [%expect
    {|
    (tst
     ((Let (var dyn1)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var dyn2)
       (bind
        (Literal (value (Int (T 2))) (ty (Type Int))
         (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Binop (op Add)
         (lhs
          (Var (id dyn1) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (rhs
          (Var (id dyn2) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Binop erased + dynamic" =
  go
    {|
let dyn1 = 1 @ erased;;
let dyn2 = 2 @ dynamic;;
let _ =
  dyn1 + dyn2
;;|};
  [%expect
    {|
    (tst
     ((Let (var dyn1)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var dyn2)
       (bind
        (Literal (value (Int (T 2))) (ty (Type Int))
         (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Binop (op Add)
         (lhs
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (rhs
          (Var (id dyn2) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Binop erased + erased" =
  go
    {|
let dyn1 = 1 @ erased;;
let dyn2 = 2 @ erased;;
let _ =
  dyn1 + dyn2
;;|};
  [%expect
    {|
    (tst
     ((Let (var dyn1)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var dyn2)
       (bind
        (Literal (value (Int (T 2))) (ty (Type Int))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Binop (op Add)
         (lhs
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (rhs
          (Literal (value (Int (T 2))) (ty (Type Int))
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "If static cond static branches" =
  go
    {|
let _ =
  if true then 1 else 2
;;|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (If
         (cond
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (then_
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (else_
          (Literal (value (Int (T 2))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "If erased" =
  go
    {|
let _ =
  (if true then int else int) @ erased
;;|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (If
         (cond
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (then_
          (Literal (value (Type Int)) (ty (Type Type))
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (else_
          (Literal (value (Type Int)) (ty (Type Type))
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (ty (Type Type)) (mode ((staticity Static) (erasure Erased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "If erased cond" =
  go
    {|
let x = true || false @ erased;;
let _ =
  if x then 1 else 2
;;|};
  [%expect
    {|
    (tst
     ((Let (var x)
       (bind
        (Literal (value (Bool (T true))) (ty (Type Bool))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (If
         (cond
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (then_
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (else_
          (Literal (value (Int (T 2))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "If dynamic erased cond" =
  go
    {|
let x = true @ dynamic erased;;
let _ =
  if x then 1 else 2
;;|};
  [%expect {| ((loc ((line 4) (column 2))) (reason Dynamic_erased)) |}]
;;

let%expect_test "If static cond dynamic branches" =
  go
    {|
let dyn = 1 @ dynamic;;
let _ =
  if true then dyn else 2
;;|};
  [%expect
    {|
    (tst
     ((Let (var dyn)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (If
         (cond
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (then_
          (Var (id dyn) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (else_
          (Literal (value (Int (T 2))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "If dynamic cond" =
  go
    {|
let dyn = true @ dynamic;;
let _ =
  if dyn then 1 else 2
;;|};
  [%expect
    {|
    (tst
     ((Let (var dyn)
       (bind
        (Literal (value (Bool (T true))) (ty (Type Bool))
         (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (If
         (cond
          (Var (id dyn) (ty (Type Bool))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (then_
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (else_
          (Literal (value (Int (T 2))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "erased if expr" =
  go
    {|
let x = true;;
let _ =
  0 + ((if x then 1 else 2) @ erased)
;;|};
  [%expect
    {|
    (tst
     ((Let (var x)
       (bind
        (Literal (value (Bool (T true))) (ty (Type Bool))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Binop (op Add)
         (lhs
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (rhs
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "if branch checks" =
  go
    {|
let _ = if true then 0 else true;;|};
  [%expect
    {|
    ((loc ((line 2) (column 8)))
     (reason (Cannot_unify (lhs (Type Int)) (rhs (Type Bool)))))
    |}]
;;

let%expect_test "if branch checks" =
  go
    {|
let _ = if 0 then 0 else 0;;|};
  [%expect
    {|
    ((loc ((line 2) (column 8)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Bool)))))
    |}]
;;

let%expect_test "if branch checks" =
  go
    {|
let _ = if true then 1+1 else 1+false;;|};
  [%expect
    {|
    ((loc ((line 2) (column 31)))
     (reason (Type_mismatch (got (Type Bool)) (need (Type Int)))))
    |}]
;;

let%expect_test "if erased branch" =
  go
    {|
let _ = if true then 0 else (true @ erased);;|};
  [%expect
    {|
    ((loc ((line 2) (column 8)))
     (reason (Cannot_unify (lhs (Type Int)) (rhs (Type Bool)))))
    |}]
;;

let%expect_test "if erased branch" =
  go
    {|
let _ = if true then 0 else int;;|};
  [%expect
    {|
    ((loc ((line 2) (column 8)))
     (reason (Cannot_unify (lhs (Type Int)) (rhs (Type Type)))))
    |}]
;;

let%expect_test "if erased branch" =
  go
    {|
let _ = if 1==2 then unit else int;;|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (If
         (cond
          (Binop (op Eq)
           (lhs
            (Literal (value (Int (T 1))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
           (rhs
            (Literal (value (Int (T 2))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
           (ty (Type Bool)) (mode ((staticity Static) (erasure Unerased)))
           (loc <opaque>)))
         (then_
          (Literal (value (Type Unit)) (ty (Type Type))
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (else_
          (Literal (value (Type Int)) (ty (Type Type))
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (ty (Type Type)) (mode ((staticity Static) (erasure Erased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "if erased branch" =
  go
    {|
let cond = true @ dynamic;;
let t = if cond then unit else int;;|};
  [%expect
    {|
    (tst
     ((Let (var cond)
       (bind
        (Literal (value (Bool (T true))) (ty (Type Bool))
         (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var t)
       (bind
        (If
         (cond
          (Var (id cond) (ty (Type Bool))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (then_
          (Literal (value (Type Unit)) (ty (Type Type))
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (else_
          (Literal (value (Type Int)) (ty (Type Type))
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (ty (Type Type)) (mode ((staticity Dynamic) (erasure Erased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "if erased branch" =
  go
    {|
let cond = true @ dynamic;;
let t = (if cond then false else cond) @ erased;;|};
  [%expect
    {|
    (tst
     ((Let (var cond)
       (bind
        (Literal (value (Bool (T true))) (ty (Type Bool))
         (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var t)
       (bind
        (Erased (ty (Type Bool)) (mode ((staticity Dynamic) (erasure Erased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Let static" =
  go
    {|
let _ =
  let x = 1;
  x
;;|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (Let (var x)
         (bind
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (rest
          (Var (id x) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Let dynamic" =
  go
    {|
let dyn = 1 @ dynamic;;
let _ =
  let x = dyn;
  x
;;|};
  [%expect
    {|
    (tst
     ((Let (var dyn)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Let (var x)
         (bind
          (Var (id dyn) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (rest
          (Var (id x) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Let dynamic" =
  go
    {|
let dyn = 1 @ erased;;
let _ =
  let x = dyn + 1 @ dynamic;
  x
;;|};
  [%expect
    {|
    (tst
     ((Let (var dyn)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Let (var x)
         (bind
          (Binop (op Add)
           (lhs
            (Literal (value (Int (T 1))) (ty (Type Int))
             (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
           (rhs
            (Literal (value (Int (T 1))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
           (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
           (loc <opaque>)))
         (rest
          (Var (id x) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Let erased" =
  go
    {|
let dyn = 1 @ erased;;
let _ =
  let x = dyn;
  x
;;|};
  [%expect
    {|
    (tst
     ((Let (var dyn)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Let (var x)
         (bind
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (rest
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Static) (erasure Erased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Let erased" =
  go
    {|
let _ =
  let x = 1 @ erased;
  x + 1
;;|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (Let (var x)
         (bind
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (rest
          (Binop (op Add)
           (lhs
            (Literal (value (Int (T 1))) (ty (Type Int))
             (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
           (rhs
            (Literal (value (Int (T 1))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
           (ty (Type Int)) (mode ((staticity Static) (erasure Unerased)))
           (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Let erased" =
  go
    {|
let _ =
  let x = 1 @ erased;
  let y = 1 @ dynamic;
  x + y
;;|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (Let (var x)
         (bind
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (rest
          (Let (var y)
           (bind
            (Literal (value (Int (T 1))) (ty (Type Int))
             (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
           (rest
            (Binop (op Add)
             (lhs
              (Literal (value (Int (T 1))) (ty (Type Int))
               (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
             (rhs
              (Var (id y) (ty (Type Int))
               (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
             (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
             (loc <opaque>)))
           (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
           (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Let erased" =
  go
    {|
let _ =
  let x = 1 @ erased;
  let y = 1 @ erased;
  0 + (x + y)
;;|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (Let (var x)
         (bind
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (rest
          (Let (var y)
           (bind
            (Literal (value (Int (T 1))) (ty (Type Int))
             (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
           (rest
            (Binop (op Add)
             (lhs
              (Literal (value (Int (T 0))) (ty (Type Int))
               (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
             (rhs
              (Binop (op Add)
               (lhs
                (Literal (value (Int (T 1))) (ty (Type Int))
                 (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
               (rhs
                (Literal (value (Int (T 1))) (ty (Type Int))
                 (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
               (ty (Type Int)) (mode ((staticity Static) (erasure Unerased)))
               (loc <opaque>)))
             (ty (Type Int)) (mode ((staticity Static) (erasure Unerased)))
             (loc <opaque>)))
           (ty (Type Int)) (mode ((staticity Static) (erasure Unerased)))
           (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Let erased" =
  go
    {|
let _ =
  let x = 1;
  let y = 1;
  0 + ((x + y) @ erased)
;;|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (Let (var x)
         (bind
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (rest
          (Let (var y)
           (bind
            (Literal (value (Int (T 1))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
           (rest
            (Binop (op Add)
             (lhs
              (Literal (value (Int (T 0))) (ty (Type Int))
               (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
             (rhs
              (Literal (value (Int (T 2))) (ty (Type Int))
               (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
             (ty (Type Int)) (mode ((staticity Static) (erasure Unerased)))
             (loc <opaque>)))
           (ty (Type Int)) (mode ((staticity Static) (erasure Unerased)))
           (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "static closure" =
  go
    {|
let _ =
  (fn (x : int) -> x)
;;|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (Lambda (arg x)
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Var (id x) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "erased closure" =
  go
    {|
let _ =
  (fn (x : int) -> x) @ erased
;;|};
  [%expect
    {|
    (tst
     ((Let (var _)
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
              (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
            (env <opaque>))))
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "closure return type" =
  go
    {|
let _ =
  (fn (x : int) -> int)
;;|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (Lambda (arg x)
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Type))
            (ret_mode ((staticity Dynamic) (erasure Erased))))))
         (body
          (Literal (value (Type Int)) (ty (Type Type))
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "dynamic closure" =
  go
    {|
let y = 1 @ dynamic;;
let _ =
  (fn (x : int) -> x + y)
;;|};
  [%expect
    {|
    (tst
     ((Let (var y)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Lambda (arg x)
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Binop (op Add)
           (lhs
            (Var (id x) (ty (Type Int))
             (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
           (rhs
            (Var (id y) (ty (Type Int))
             (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
           (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
           (loc <opaque>)))
         (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "static closure arg" =
  go
    {|
let f =
  (fn (static x : int) -> x + 1) @ dynamic;;
let x = f 1;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (erased x : int) -> x;;
let _ = f 0;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Lambda (arg x)
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Erased))) (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Erased))))))
         (body
          (Erased (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
           (loc <opaque>)))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Erased (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (erased x : int) -> 1;;
let _ = f 0;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Lambda (arg x)
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Erased))) (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id f)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Erased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (arg
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (erased x : int) -> 1;;
let _ = f 0;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Lambda (arg x)
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Erased))) (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id f)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Erased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (arg
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (g : int -> erased int) -> let _ = g 1; 2;;
let _ = f (fn (x : int) -> 0 @ erased);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (erased x : int -> int) -> 1;;
let _ = f ((fn (x : int) -> x + 1) @ erased);;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Lambda (arg x)
         (ty
          (Type
           (Arrow
            (arg_ty
             (Type
              (Arrow (arg_ty (Type Int))
               (arg_mode ((staticity Dynamic) (erasure Unerased)))
               (ret_ty (Type Int))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Dynamic) (erasure Erased))) (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id f)
           (ty
            (Type
             (Arrow
              (arg_ty
               (Type
                (Arrow (arg_ty (Type Int))
                 (arg_mode ((staticity Dynamic) (erasure Unerased)))
                 (ret_ty (Type Int))
                 (ret_mode ((staticity Dynamic) (erasure Unerased))))))
              (arg_mode ((staticity Dynamic) (erasure Erased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (arg
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
               (Binop (op Add)
                (lhs
                 (Var (id x) (ty (Type Int))
                  (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
                (rhs
                 (Literal (value (Int (T 1))) (ty (Type Int))
                  (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
                (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
                (loc <opaque>)))
              (env <opaque>))))
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (x : int) -> x;;
let g = fn (erased x : int) -> f x;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 31)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "static erased closure arg" =
  go
    {|
let _ =
  (fn (static erased x : int) -> x)
;;|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (Binder (arg x)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Erased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Static) (erasure Erased))))))
         (body (Var (id x) (loc ((line 3) (column 33))))) (mono <opaque>)
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "dependent closure arg" =
  go
    {|
let _ =
  (fn (x : type) -> x)
;;|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (Lambda (arg x)
         (ty
          (Type
           (Arrow (arg_ty (Type Type))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Type))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Var (id x) (ty (Type Type))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "closure erased" =
  go
    {|
let x = (fn (x : int) -> x) @ erased;;
|};
  [%expect
    {|
    (tst
     ((Let (var x)
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
              (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
            (env <opaque>))))
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "closure erased" =
  go
    {|
let x = (fn (erased x : int) -> x) 0;;
|};
  [%expect
    {|
    (tst
     ((Let (var x)
       (bind
        (Erased (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "closure erased" =
  go
    {|
let x = (fn (erased x : int) -> 1) 0;;
|};
  [%expect
    {|
    (tst
     ((Let (var x)
       (bind
        (Apply
         (fn
          (Lambda (arg x)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Erased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (body
            (Literal (value (Int (T 1))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (arg
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "closure erased" =
  go
    {|
let f = (fn (erased x : int) -> 1);;
let g = (fn (erased x : int) -> 2);;
let _ = (if true @ dynamic then f else g) 0;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Lambda (arg x)
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Erased))) (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var g)
       (bind
        (Lambda (arg x)
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Erased))) (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Literal (value (Int (T 2))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Apply
         (fn
          (If
           (cond
            (Literal (value (Bool (T true))) (ty (Type Bool))
             (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
           (then_
            (Var (id f)
             (ty
              (Type
               (Arrow (arg_ty (Type Int))
                (arg_mode ((staticity Dynamic) (erasure Erased)))
                (ret_ty (Type Int))
                (ret_mode ((staticity Dynamic) (erasure Unerased))))))
             (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
           (else_
            (Var (id g)
             (ty
              (Type
               (Arrow (arg_ty (Type Int))
                (arg_mode ((staticity Dynamic) (erasure Erased)))
                (ret_ty (Type Int))
                (ret_mode ((staticity Dynamic) (erasure Unerased))))))
             (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Erased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (arg
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "closure branches" =
  go
    {|
let f = (fn (erased x : int) -> 1);;
let g = (fn (static x : int) -> 2);;
let _ = if true then f else g;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Lambda (arg x)
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Erased))) (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var g)
       (bind
        (Binder (arg x)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (body (Literal (value (Int 2)) (loc ((line 3) (column 32)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (If
         (cond
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (then_
          (Var (id f)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Erased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (else_
          (Var (id g)
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Int)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "closure branches" =
  go
    {|
let f = (fn (static erased x : int) -> 1);;
let g = (fn (static x : int) -> 2);;
let _ = if true then f else g;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Erased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (body (Literal (value (Int 1)) (loc ((line 2) (column 39)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var g)
       (bind
        (Binder (arg x)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (body (Literal (value (Int 2)) (loc ((line 3) (column 32)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (If
         (cond
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (then_
          (Var (id f)
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Erased)))
              (ret_ty (T (Type Int)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (else_
          (Var (id g)
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Int)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "closure branches" =
  go
    {|
let f = (fn (static erased x : int) -> 1);;
let g = (fn (static erased x : int) -> 2);;
let _ = if true then f else g;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Erased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (body (Literal (value (Int 1)) (loc ((line 2) (column 39)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var g)
       (bind
        (Binder (arg x)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Erased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (body (Literal (value (Int 2)) (loc ((line 3) (column 39)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (If
         (cond
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (then_
          (Var (id f)
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Erased)))
              (ret_ty (T (Type Int)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (else_
          (Var (id g)
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Erased)))
              (ret_ty (T (Type Int)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Erased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "closure erased" =
  go
    {|
let c = fn (_ : unit) -> true;;
let f = (fn (erased x : int) -> 1);;
let g = (fn (erased x : int) -> 2);;
let _ = (if c () then f else g) 0;;
|};
  [%expect
    {|
    (tst
     ((Let (var c)
       (bind
        (Lambda (arg _)
         (ty
          (Type
           (Arrow (arg_ty (Type Unit))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Bool))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var f)
       (bind
        (Lambda (arg x)
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Erased))) (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var g)
       (bind
        (Lambda (arg x)
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Erased))) (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Literal (value (Int (T 2))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Apply
         (fn
          (If
           (cond
            (Apply
             (fn
              (Var (id c)
               (ty
                (Type
                 (Arrow (arg_ty (Type Unit))
                  (arg_mode ((staticity Dynamic) (erasure Unerased)))
                  (ret_ty (Type Bool))
                  (ret_mode ((staticity Dynamic) (erasure Unerased))))))
               (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
             (arg
              (Literal (value Unit) (ty (Type Unit))
               (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
             (ty (Type Bool)) (mode ((staticity Dynamic) (erasure Unerased)))
             (loc <opaque>)))
           (then_
            (Var (id f)
             (ty
              (Type
               (Arrow (arg_ty (Type Int))
                (arg_mode ((staticity Dynamic) (erasure Erased)))
                (ret_ty (Type Int))
                (ret_mode ((staticity Dynamic) (erasure Unerased))))))
             (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
           (else_
            (Var (id g)
             (ty
              (Type
               (Arrow (arg_ty (Type Int))
                (arg_mode ((staticity Dynamic) (erasure Erased)))
                (ret_ty (Type Int))
                (ret_mode ((staticity Dynamic) (erasure Unerased))))))
             (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Erased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (arg
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "closure erased" =
  go
    {|
let c = fn (_ : unit) -> true;;
let f = (fn (x : int) -> 1);;
let g = (fn (erased x : int) -> 2);;
let _ = (if c () then f else g) 0;;
|};
  [%expect
    {|
    (tst
     ((Let (var c)
       (bind
        (Lambda (arg _)
         (ty
          (Type
           (Arrow (arg_ty (Type Unit))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Bool))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var f)
       (bind
        (Lambda (arg x)
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var g)
       (bind
        (Lambda (arg x)
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Erased))) (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Literal (value (Int (T 2))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Apply
         (fn
          (If
           (cond
            (Apply
             (fn
              (Var (id c)
               (ty
                (Type
                 (Arrow (arg_ty (Type Unit))
                  (arg_mode ((staticity Dynamic) (erasure Unerased)))
                  (ret_ty (Type Bool))
                  (ret_mode ((staticity Dynamic) (erasure Unerased))))))
               (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
             (arg
              (Literal (value Unit) (ty (Type Unit))
               (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
             (ty (Type Bool)) (mode ((staticity Dynamic) (erasure Unerased)))
             (loc <opaque>)))
           (then_
            (Var (id f)
             (ty
              (Type
               (Arrow (arg_ty (Type Int))
                (arg_mode ((staticity Dynamic) (erasure Unerased)))
                (ret_ty (Type Int))
                (ret_mode ((staticity Dynamic) (erasure Unerased))))))
             (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
           (else_
            (Var (id g)
             (ty
              (Type
               (Arrow (arg_ty (Type Int))
                (arg_mode ((staticity Dynamic) (erasure Erased)))
                (ret_ty (Type Int))
                (ret_mode ((staticity Dynamic) (erasure Unerased))))))
             (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (arg
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "closure nest" =
  go
    {|
let f = fn (x : int) -> 1;;
let g = fn (f : int -> int) -> f 0;;
let _ = g f;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Lambda (arg x)
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var g)
       (bind
        (Lambda (arg f)
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
             (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
           (arg
            (Literal (value (Int (T 0))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
           (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
           (loc <opaque>)))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id g)
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
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (arg
          (Var (id f)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "closure nest" =
  go
    {|
let f = fn (x : int) -> 1;;
let g = fn (static f : int -> int) -> f 0;;
let _ = g f;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Lambda (arg x)
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var g)
       (bind
        (Binder (arg f)
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
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Apply (fn (Var (id f) (loc ((line 3) (column 38)))))
           (arg (Literal (value (Int 0)) (loc ((line 3) (column 40)))))
           (loc ((line 3) (column 38)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Symbol (id g) (arg (Closure 1))
         (mode ((staticity Dynamic) (erasure Unerased))) (ty (Type Int))
         (loc ((line 4) (column 8)))))
       (loc <opaque>))))
    |}]
;;

let%expect_test "closure nest" =
  go
    {|
let f = fn (x : int) -> 1;;
let g = fn (erased f : int -> int) -> f 0;;
let _ = g f;;
|};
  [%expect {| ((loc ((line 3) (column 38))) (reason Dynamic_erased)) |}]
;;

let%expect_test "closure nest" =
  go
    {|
let f1 = (fn (x : int) -> 1) @ erased;;
let g = fn (f2 : int -> int) -> f2 0;;
let _ = g f1;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "closure nest" =
  go
    {|
let f1 = (fn (x : int) -> 1) @ erased;;
let g = fn (erased f2 : int -> int) -> f2 0;;
let _ = g f1;;
|};
  [%expect {| ((loc ((line 3) (column 39))) (reason Dynamic_erased)) |}]
;;

let%expect_test "closure nest" =
  go
    {|
let f1 = (fn (x : int) -> 1) @ erased;;
let g = fn (static erased f2 : int -> int) -> f2 0;;
let _ = g f1;;
|};
  [%expect
    {|
    (tst
     ((Let (var f1)
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
             (Literal (value (Int (T 1))) (ty (Type Int))
              (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
            (env <opaque>))))
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var g)
       (bind
        (Binder (arg f2)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Arrow (arg_ty (Type Int))
               (arg_mode ((staticity Dynamic) (erasure Unerased)))
               (ret_ty (Type Int))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Erased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Apply (fn (Var (id f2) (loc ((line 3) (column 46)))))
           (arg (Literal (value (Int 0)) (loc ((line 3) (column 49)))))
           (loc ((line 3) (column 46)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Symbol (id g) (arg (Closure 1))
         (mode ((staticity Dynamic) (erasure Unerased))) (ty (Type Int))
         (loc ((line 4) (column 8)))))
       (loc <opaque>))))
    |}]
;;

let%expect_test "inlined closure nest" =
  go
    {|
let f1 = fn (erased x : int) -> 1;;
let g = fn (f2 : int -> int) -> f2 0;;
let _ = g f1;;
|};
  [%expect
    {|
    (tst
     ((Let (var f1)
       (bind
        (Lambda (arg x)
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Erased))) (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var g)
       (bind
        (Lambda (arg f2)
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
         (body
          (Apply
           (fn
            (Var (id f2)
             (ty
              (Type
               (Arrow (arg_ty (Type Int))
                (arg_mode ((staticity Dynamic) (erasure Unerased)))
                (ret_ty (Type Int))
                (ret_mode ((staticity Dynamic) (erasure Unerased))))))
             (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
           (arg
            (Literal (value (Int (T 0))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
           (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
           (loc <opaque>)))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id g)
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
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (arg
          (Var (id f1)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Erased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "inlined closure nest" =
  go
    {|
let f1 = fn (erased x : int) -> 1;;
let g = fn (f2 : erased int -> int) -> f2 0;;
let _ = g f1;;
|};
  [%expect
    {|
    (tst
     ((Let (var f1)
       (bind
        (Lambda (arg x)
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Erased))) (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var g)
       (bind
        (Lambda (arg f2)
         (ty
          (Type
           (Arrow
            (arg_ty
             (Type
              (Arrow (arg_ty (Type Int))
               (arg_mode ((staticity Dynamic) (erasure Erased)))
               (ret_ty (Type Int))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Apply
           (fn
            (Var (id f2)
             (ty
              (Type
               (Arrow (arg_ty (Type Int))
                (arg_mode ((staticity Dynamic) (erasure Erased)))
                (ret_ty (Type Int))
                (ret_mode ((staticity Dynamic) (erasure Unerased))))))
             (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
           (arg
            (Literal (value (Int (T 0))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
           (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
           (loc <opaque>)))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id g)
           (ty
            (Type
             (Arrow
              (arg_ty
               (Type
                (Arrow (arg_ty (Type Int))
                 (arg_mode ((staticity Dynamic) (erasure Erased)))
                 (ret_ty (Type Int))
                 (ret_mode ((staticity Dynamic) (erasure Unerased))))))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (arg
          (Var (id f1)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Erased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "inlined closure nest" =
  go
    {|
let f1 = fn (erased x : int) -> 1;;
let g = fn (static f2 : erased int -> int) -> f2 0;;
let _ = g f1;;
|};
  [%expect
    {|
    (tst
     ((Let (var f1)
       (bind
        (Lambda (arg x)
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Erased))) (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var g)
       (bind
        (Binder (arg f2)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Arrow (arg_ty (Type Int))
               (arg_mode ((staticity Dynamic) (erasure Erased)))
               (ret_ty (Type Int))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Apply (fn (Var (id f2) (loc ((line 3) (column 46)))))
           (arg (Literal (value (Int 0)) (loc ((line 3) (column 49)))))
           (loc ((line 3) (column 46)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Symbol (id g) (arg (Closure 1))
         (mode ((staticity Dynamic) (erasure Unerased))) (ty (Type Int))
         (loc ((line 4) (column 8)))))
       (loc <opaque>))))
    |}]
;;

let%expect_test "inlined closure nest" =
  go
    {|
let f1 = fn (erased x : int) -> 1;;
let g = fn (static erased f2 : erased int -> int) -> f2 0;;
let _ = g f1;;
|};
  [%expect
    {|
    (tst
     ((Let (var f1)
       (bind
        (Lambda (arg x)
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Erased))) (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var g)
       (bind
        (Binder (arg f2)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Arrow (arg_ty (Type Int))
               (arg_mode ((staticity Dynamic) (erasure Erased)))
               (ret_ty (Type Int))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Erased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Apply (fn (Var (id f2) (loc ((line 3) (column 53)))))
           (arg (Literal (value (Int 0)) (loc ((line 3) (column 56)))))
           (loc ((line 3) (column 53)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Symbol (id g) (arg (Closure 1))
         (mode ((staticity Dynamic) (erasure Unerased))) (ty (Type Int))
         (loc ((line 4) (column 8)))))
       (loc <opaque>))))
    |}]
;;

let%expect_test "closure static" =
  go
    {|
let x = (fn (static x : int) -> x) 0;;
|};
  [%expect
    {|
    (tst
     ((Let (var x)
       (bind
        (Let (var x)
         (bind
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (rest
          (Var (id x) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "closure dependent" =
  go
    {|
let x = (fn (static x : type) -> 0 : x);;
|};
  [%expect
    {|
    ((loc ((line 2) (column 35)))
     (reason (Type_mismatch (got (Type Int)) (need (Var $0)))))
    |}]
;;

let%expect_test "closure dependent" =
  go
    {|
let x = (fn (static erased x : type) -> 0 : x);;
let _ = x int;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 42)))
     (reason (Type_mismatch (got (Type Int)) (need (Var $0)))))
    |}]
;;

let%expect_test "closure dependent" =
  go
    {|
let x = (fn (static erased x : type) -> 0 : x);;
|};
  [%expect
    {|
    ((loc ((line 2) (column 42)))
     (reason (Type_mismatch (got (Type Int)) (need (Var $0)))))
    |}]
;;

let%expect_test "closure static erased" =
  go
    {|
let x = (fn (static erased x : int) -> 1) 0;;
|};
  [%expect
    {|
    (tst
     ((Let (var x)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "closure return dynamic type" =
  go
    {|
let t = (fn (x : int) -> int) 0;;
let _ = 0 : t;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 10)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "closure return static type" =
  go
    {|
let t = (fn (static x : int) -> int) 0;;
let _ = 0 : t;;
|};
  [%expect
    {|
    (tst
     ((Let (var t)
       (bind
        (Let (var x)
         (bind
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (rest
          (Literal (value (Type Int)) (ty (Type Type))
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (ty (Type Type)) (mode ((staticity Static) (erasure Erased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Literal (value (Int (T 0))) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "closure return static type" =
  go
    {|
let t = (fn (static x : int) -> bool) 0;;
let _ = 0 : t;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 10)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Bool)))))
    |}]
;;

let%expect_test "closure return static type" =
  go
    {|
let t = (fn (static erased x : int) -> int) 0;;
let _ = 0 : t;;
|};
  [%expect
    {|
    (tst
     ((Let (var t)
       (bind
        (Literal (value (Type Int)) (ty (Type Type))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Literal (value (Int (T 0))) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Apply fn static arg" =
  go
    {|
let _ =
  (fn (x : int) -> x) 1
;;|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (Apply
         (fn
          (Lambda (arg x)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (body
            (Var (id x) (ty (Type Int))
             (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (arg
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Apply static erased fn static arg" =
  go
    {|
let _ =
  (fn (static erased x : int) -> x) 1
;;|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Apply fn dynamic arg" =
  go
    {|
let dyn = 1 @ dynamic;;
let _ =
  (fn (x : int) -> x) dyn
;;|};
  [%expect
    {|
    (tst
     ((Let (var dyn)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Lambda (arg x)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (body
            (Var (id x) (ty (Type Int))
             (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (arg
          (Var (id dyn) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Apply erased fn dynamic arg" =
  go
    {|
let dyn = 1 @ dynamic;;
let y =
  (fn (erased x : int) -> x) dyn
;;
let _ = y @ unerased;;|};
  [%expect
    {|
    ((loc ((line 6) (column 10)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "Apply erased fn dynamic arg" =
  go
    {|
let dyn = 1 @ dynamic;;
let y =
  (fn (erased x : int) -> 5) (dyn-1)
;;
let _ = y @ unerased;;|};
  [%expect
    {|
    (tst
     ((Let (var dyn)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var y)
       (bind
        (Apply
         (fn
          (Lambda (arg x)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Erased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (body
            (Literal (value (Int (T 5))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (arg
          (Binop (op Sub)
           (lhs
            (Var (id dyn) (ty (Type Int))
             (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
           (rhs
            (Literal (value (Int (T 1))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
           (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
           (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Var (id y) (ty (Type Int))
         (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Apply erased fn dynamic arg" =
  go
    {|
let dyn = 1 @ dynamic erased;;
let y =
  (fn (erased x : int) -> 5) (dyn-1)
;;
let _ = y @ unerased;;|};
  [%expect {| ((loc ((line 4) (column 33))) (reason Dynamic_erased)) |}]
;;

let%expect_test "Apply erased fn dynamic arg" =
  go
    {|
let f = fn (static x : int) -> x @ erased;;
let y =
  (fn (erased x : int) -> 5) (f 1)
;;
let _ = y @ unerased;;|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Static) (erasure Erased))))))
         (body
          (Mode_annotation (expr (Var (id x) (loc ((line 2) (column 31)))))
           (mode ((staticity ()) (erasure (Erased))))
           (loc ((line 2) (column 33)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Erased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var y)
       (bind
        (Apply
         (fn
          (Lambda (arg x)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Erased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (body
            (Literal (value (Int (T 5))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (arg
          (Let (var x)
           (bind
            (Literal (value (Int (T 1))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
           (rest
            (Literal (value (Int (T 1))) (ty (Type Int))
             (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
           (ty (Type Int)) (mode ((staticity Static) (erasure Erased)))
           (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Var (id y) (ty (Type Int))
         (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Apply erased fn stat arg" =
  go
    {|
let dyn = 1;;
let y =
  (fn (erased x : int) -> 5) (dyn-1)
;;
let _ = y @ unerased;;|};
  [%expect
    {|
    (tst
     ((Let (var dyn)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var y)
       (bind
        (Apply
         (fn
          (Lambda (arg x)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Erased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (body
            (Literal (value (Int (T 5))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (arg
          (Binop (op Sub)
           (lhs
            (Var (id dyn) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
           (rhs
            (Literal (value (Int (T 1))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
           (ty (Type Int)) (mode ((staticity Static) (erasure Unerased)))
           (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Var (id y) (ty (Type Int))
         (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Apply dynamic fn static arg" =
  go
    {|
let dyn_fn = (fn (x : int) -> x) @ dynamic;;
let _ =
  dyn_fn 1
;;|};
  [%expect
    {|
    (tst
     ((Let (var dyn_fn)
       (bind
        (Lambda (arg x)
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Var (id x) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id dyn_fn)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (arg
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Apply dynamic fn erased arg" =
  go
    {|
let dyn_fn = (fn (erased x : int) -> x) @ dynamic;;
let _ =
  dyn_fn 1
;;|};
  [%expect {| ((loc ((line 4) (column 2))) (reason Dynamic_erased)) |}]
;;

let%expect_test "Apply dynamic fn erased arg" =
  go
    {|
let dyn_fn = (fn (erased x : int) -> 0) @ dynamic;;
let _ =
  dyn_fn (1 @ dynamic)
;;|};
  [%expect
    {|
    (tst
     ((Let (var dyn_fn)
       (bind
        (Lambda (arg x)
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Erased))) (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id dyn_fn)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Erased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (arg
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Apply dynamic fn dynamic arg" =
  go
    {|
let dyn_fn = (fn (x : int) -> x) @ dynamic;;
let dyn_arg = 1 @ dynamic;;
let _ =
  dyn_fn dyn_arg
;;|};
  [%expect
    {|
    (tst
     ((Let (var dyn_fn)
       (bind
        (Lambda (arg x)
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Var (id x) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var dyn_arg)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id dyn_fn)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (arg
          (Var (id dyn_arg) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Apply erased fn dynamic arg" =
  go
    {|
let dyn_fn = (fn (erased x : int) -> x) @ dynamic;;
let dyn_arg = 1 @ dynamic;;
let _ =
  dyn_fn dyn_arg
;;|};
  [%expect {| ((loc ((line 5) (column 2))) (reason Dynamic_erased)) |}]
;;

let%expect_test "Apply erased fn dynamic arg" =
  go
    {|
let dyn_fn = (fn (erased x : int) -> x) @ erased;;
let dyn_arg = 1 @ dynamic;;
let _ =
  dyn_fn dyn_arg
;;|};
  [%expect
    {|
    (tst
     ((Let (var dyn_fn)
       (bind
        (Lambda (arg x)
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Erased))) (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Erased))))))
         (body
          (Erased (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
           (loc <opaque>)))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var dyn_arg)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Erased (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Lambda dynamic arg" =
  go
    {|
let _ =
  fn (x : int) -> x
;;|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (Lambda (arg x)
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Var (id x) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Lambda static arg" =
  go
    {|
let _ =
  fn (static x : int) -> 1
;;|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (Binder (arg x)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (body (Literal (value (Int 1)) (loc ((line 3) (column 25)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Lambda erased arg" =
  go
    {|
let _ =
  fn (erased x : int) -> x
;;|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (Lambda (arg x)
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Erased))) (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Erased))))))
         (body
          (Erased (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
           (loc <opaque>)))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Lambda capturing dynamic var" =
  go
    {|
let x = 1 @ dynamic;;
let _ =
  fn (y : int) -> x
;;|};
  [%expect
    {|
    (tst
     ((Let (var x)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Lambda (arg y)
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Var (id x) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Lambda capturing static var" =
  go
    {|
let x = 1 @ static;;
let _ =
  fn (y : int) -> x
;;|};
  [%expect
    {|
    (tst
     ((Let (var x)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Lambda (arg y)
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Var (id x) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Lambda capturing erased var" =
  go
    {|
let x = 1 @ static erased;;
let _ =
  fn (y : int) -> x
;;|};
  [%expect
    {|
    (tst
     ((Let (var x)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Lambda (arg y)
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Erased))))))
         (body
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Lambda capturing erased var" =
  go
    {|
let x = 1 @ static erased;;
let _ =
  (fn (y : int) -> x) 0
;;|};
  [%expect
    {|
    (tst
     ((Let (var x)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Let (var y)
         (bind
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (rest
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Erased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Lambda capturing type" =
  go
    {|
let f = fn (_ : unit) -> int;;
let g = fn (x : f ()) -> x + 1;;
let _ = g 0;;|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "Lambda capturing type" =
  go
    {|
let f = fn (static _ : unit) -> int;;
let g = fn (x : f ()) -> x + 1;;|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg _)
         (ty
          (Type
           (Pi (arg_ty (Type Unit))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Type)))
            (ret_mode ((staticity Static) (erasure Erased))))))
         (body (Var (id int) (loc ((line 2) (column 32))))) (mono <opaque>)
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var g)
       (bind
        (Lambda (arg x)
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Binop (op Add)
           (lhs
            (Var (id x) (ty (Type Int))
             (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
           (rhs
            (Literal (value (Int (T 1))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
           (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
           (loc <opaque>)))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Lambda take type" =
  go
    {|
let f = fn (ty : type) -> ty;;
let _ = f int;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "Lambda take type" =
  go
    {|
let f = fn (erased ty : type) -> ty;;
let _ = f int;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Lambda (arg ty)
         (ty
          (Type
           (Arrow (arg_ty (Type Type))
            (arg_mode ((staticity Dynamic) (erasure Erased)))
            (ret_ty (Type Type))
            (ret_mode ((staticity Dynamic) (erasure Erased))))))
         (body
          (Erased (ty (Type Type)) (mode ((staticity Dynamic) (erasure Erased)))
           (loc <opaque>)))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Erased (ty (Type Type)) (mode ((staticity Dynamic) (erasure Erased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Lambda take type" =
  go
    {|
let f = fn (static erased ty : type) -> ty;;
let _ = 0 : f int;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg ty)
         (ty
          (Type
           (Pi (arg_ty (Type Type))
            (arg_mode ((staticity Static) (erasure Erased)))
            (ret_ty (T (Type Type)))
            (ret_mode ((staticity Static) (erasure Erased))))))
         (body (Var (id ty) (loc ((line 2) (column 40))))) (mono <opaque>)
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Literal (value (Int (T 0))) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Lambda take type" =
  go
    {|
let f = fn (erased ty : type) -> ty;;
let g = fn (static f : type -> type) -> f int;;
let _ = g f;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 40)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "Lambda take type" =
  go
    {|
let f = fn (static erased ty : type) -> ty;;
let _ = 0 : f bool;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 10)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Bool)))))
    |}]
;;

let%expect_test "mono fn" =
  go
    {|
let x = fn (static erased x : type) -> x;;
let y = x int;;
|};
  [%expect
    {|
    (tst
     ((Let (var x)
       (bind
        (Binder (arg x)
         (ty
          (Type
           (Pi (arg_ty (Type Type))
            (arg_mode ((staticity Static) (erasure Erased)))
            (ret_ty (T (Type Type)))
            (ret_mode ((staticity Static) (erasure Erased))))))
         (body (Var (id x) (loc ((line 2) (column 39))))) (mono <opaque>)
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var y)
       (bind
        (Literal (value (Type Int)) (ty (Type Type))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "mono fn" =
  go
    {|
let x = fn (static x : type) -> x;;
let y = x @ dynamic;;
|};
  [%expect
    {|
    (tst
     ((Let (var x)
       (bind
        (Binder (arg x)
         (ty
          (Type
           (Pi (arg_ty (Type Type))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Type)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (body (Var (id x) (loc ((line 2) (column 32))))) (mono <opaque>)
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var y)
       (bind
        (Var (id x)
         (ty
          (Type
           (Pi (arg_ty (Type Type))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Type)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "mono fn" =
  go
    {|
let x = fn (erased x : type) -> x;;
let y = x @ dynamic;;
|};
  [%expect
    {|
    (tst
     ((Let (var x)
       (bind
        (Lambda (arg x)
         (ty
          (Type
           (Arrow (arg_ty (Type Type))
            (arg_mode ((staticity Dynamic) (erasure Erased)))
            (ret_ty (Type Type))
            (ret_mode ((staticity Dynamic) (erasure Erased))))))
         (body
          (Erased (ty (Type Type)) (mode ((staticity Dynamic) (erasure Erased)))
           (loc <opaque>)))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var y)
       (bind
        (Literal
         (value
          (Closure
           ((arg x)
            (ty
             (Type
              (Arrow (arg_ty (Type Type))
               (arg_mode ((staticity Dynamic) (erasure Erased)))
               (ret_ty (Type Type))
               (ret_mode ((staticity Dynamic) (erasure Erased))))))
            (body
             (Erased (ty (Type Type))
              (mode ((staticity Dynamic) (erasure Erased))) (loc <opaque>)))
            (env <opaque>))))
         (ty
          (Type
           (Arrow (arg_ty (Type Type))
            (arg_mode ((staticity Dynamic) (erasure Erased)))
            (ret_ty (Type Type))
            (ret_mode ((staticity Dynamic) (erasure Erased))))))
         (mode ((staticity Dynamic) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "mono fn" =
  go
    {|
let x = fn (static x : type) -> x;;
let y = x @ unerased;;
|};
  [%expect
    {|
    (tst
     ((Let (var x)
       (bind
        (Binder (arg x)
         (ty
          (Type
           (Pi (arg_ty (Type Type))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Type)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (body (Var (id x) (loc ((line 2) (column 32))))) (mono <opaque>)
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var y)
       (bind
        (Var (id x)
         (ty
          (Type
           (Pi (arg_ty (Type Type))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Type)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "mono fn" =
  go
    {|
let x = fn (static x : type) -> x;;
let y = x (0 @ dynamic);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "mono fn" =
  go
    {|
let x = fn (erased x : type) -> x;;
let y = x (0 @ dynamic);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Type)))))
    |}]
;;

let%expect_test "mono fn" =
  go
    {|
let x = fn (static erased x : type) -> x;;
let y = (x int) @ dynamic;;
|};
  [%expect
    {|
    (tst
     ((Let (var x)
       (bind
        (Binder (arg x)
         (ty
          (Type
           (Pi (arg_ty (Type Type))
            (arg_mode ((staticity Static) (erasure Erased)))
            (ret_ty (T (Type Type)))
            (ret_mode ((staticity Static) (erasure Erased))))))
         (body (Var (id x) (loc ((line 2) (column 39))))) (mono <opaque>)
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var y)
       (bind
        (Literal (value (Type Int)) (ty (Type Type))
         (mode ((staticity Dynamic) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Dependent lambda" =
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

let%expect_test "Dependent lambda" =
  go
    {|
let f = fn (static erased ty : type) -> fn (x : ty) -> x;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg ty)
         (ty
          (Type
           (Pi (arg_ty (Type Type))
            (arg_mode ((staticity Static) (erasure Erased)))
            (ret_ty
             (Typecheck (env <opaque>) (arg ty) (arg_ty (Type Type))
              (arg_mode ((staticity Static) (erasure Erased))) (memo <opaque>)
              (body
               (Lambda (arg x) (erased Unerased)
                (arg_mode ((staticity ()) (erasure ())))
                (arg_ty (Var (id ty) (loc ((line 2) (column 48)))))
                (body (Var (id x) (loc ((line 2) (column 55)))))
                (loc ((line 2) (column 40)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (body
          (Lambda (arg x) (erased Unerased)
           (arg_mode ((staticity ()) (erasure ())))
           (arg_ty (Var (id ty) (loc ((line 2) (column 48)))))
           (body (Var (id x) (loc ((line 2) (column 55)))))
           (loc ((line 2) (column 40)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "static erased lambda" =
  go
    {|
let f = fn (static x : int) -> fn (_ : unit) -> x;;
let g = (f 1 ()) @ unerased;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (T
              (Type
               (Arrow (arg_ty (Type Unit))
                (arg_mode ((staticity Dynamic) (erasure Unerased)))
                (ret_ty (Type Int))
                (ret_mode ((staticity Dynamic) (erasure Unerased)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (body
          (Lambda (arg _) (erased Unerased)
           (arg_mode ((staticity ()) (erasure ())))
           (arg_ty (Var (id unit) (loc ((line 2) (column 39)))))
           (body (Var (id x) (loc ((line 2) (column 48)))))
           (loc ((line 2) (column 31)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var g)
       (bind
        (Apply
         (fn
          (Symbol (id f) (arg (Int 1))
           (mode ((staticity Static) (erasure Unerased)))
           (ty
            (Type
             (Arrow (arg_ty (Type Unit))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (loc ((line 3) (column 9)))))
         (arg
          (Literal (value Unit) (ty (Type Unit))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "lift universal type" =
  go
    {|
let f = fn (static ty : type) -> ty @ erased;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg ty)
         (ty
          (Type
           (Pi (arg_ty (Type Type))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Type)))
            (ret_mode ((staticity Static) (erasure Erased))))))
         (body
          (Mode_annotation (expr (Var (id ty) (loc ((line 2) (column 33)))))
           (mode ((staticity ()) (erasure (Erased))))
           (loc ((line 2) (column 36)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Erased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "lift universal int" =
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
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Static) (erasure Erased))))))
         (body
          (Mode_annotation (expr (Var (id x) (loc ((line 2) (column 31)))))
           (mode ((staticity ()) (erasure (Erased))))
           (loc ((line 2) (column 33)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Erased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Let (var x)
         (bind
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (rest
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Static) (erasure Erased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Dependent lambda" =
  go
    {|
let f = fn (static erased ty : type) -> fn (x : ty) -> x;;
let g = f int;;
let _ = g 0;;
let g = f bool;;
let _ = g true;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg ty)
         (ty
          (Type
           (Pi (arg_ty (Type Type))
            (arg_mode ((staticity Static) (erasure Erased)))
            (ret_ty
             (Typecheck (env <opaque>) (arg ty) (arg_ty (Type Type))
              (arg_mode ((staticity Static) (erasure Erased))) (memo <opaque>)
              (body
               (Lambda (arg x) (erased Unerased)
                (arg_mode ((staticity ()) (erasure ())))
                (arg_ty (Var (id ty) (loc ((line 2) (column 48)))))
                (body (Var (id x) (loc ((line 2) (column 55)))))
                (loc ((line 2) (column 40)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (body
          (Lambda (arg x) (erased Unerased)
           (arg_mode ((staticity ()) (erasure ())))
           (arg_ty (Var (id ty) (loc ((line 2) (column 48)))))
           (body (Var (id x) (loc ((line 2) (column 55)))))
           (loc ((line 2) (column 40)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var g)
       (bind
        (Symbol (id f) (arg IntT) (mode ((staticity Static) (erasure Unerased)))
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (loc ((line 3) (column 8)))))
       (loc <opaque>))
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
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (arg
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var g)
       (bind
        (Symbol (id f) (arg BoolT) (mode ((staticity Static) (erasure Unerased)))
         (ty
          (Type
           (Arrow (arg_ty (Type Bool))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Bool))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (loc ((line 5) (column 8)))))
       (loc <opaque>))
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
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (arg
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Bool)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Dependent lambda" =
  go
    {|
let f = fn (static g : int -> int) -> g 0;;
let _ = f (fn (x : int) -> x + 1);;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg g)
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
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Apply (fn (Var (id g) (loc ((line 2) (column 38)))))
           (arg (Literal (value (Int 0)) (loc ((line 2) (column 40)))))
           (loc ((line 2) (column 38)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Symbol (id f) (arg (Closure 1))
         (mode ((staticity Dynamic) (erasure Unerased))) (ty (Type Int))
         (loc ((line 3) (column 8)))))
       (loc <opaque>))))
    |}]
;;

let%expect_test "dependent unit" =
  go
    {|
let f = fn (static x : unit) -> ();;
let _ = f ();;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (ty
          (Type
           (Pi (arg_ty (Type Unit))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Unit)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (body (Literal (value Unit) (loc ((line 2) (column 32)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Symbol (id f) (arg Unit) (mode ((staticity Static) (erasure Unerased)))
         (ty (Type Unit)) (loc ((line 3) (column 8)))))
       (loc <opaque>))))
    |}]
;;

let%expect_test "dependent bool" =
  go
    {|
let f = fn (static x : bool) -> !x;;
let _ = f true;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (ty
          (Type
           (Pi (arg_ty (Type Bool))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Bool)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (body
          (Unop (op Not) (arg (Var (id x) (loc ((line 2) (column 33)))))
           (loc ((line 2) (column 32)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Symbol (id f) (arg (Bool true))
         (mode ((staticity Static) (erasure Unerased))) (ty (Type Bool))
         (loc ((line 3) (column 8)))))
       (loc <opaque>))))
    |}]
;;

let%expect_test "dependent int" =
  go
    {|
let f = fn (static x : int) -> -x;;
let _ = f 1;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (body
          (Unop (op Neg) (arg (Var (id x) (loc ((line 2) (column 32)))))
           (loc ((line 2) (column 31)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Symbol (id f) (arg (Int 1))
         (mode ((staticity Static) (erasure Unerased))) (ty (Type Int))
         (loc ((line 3) (column 8)))))
       (loc <opaque>))))
    |}]
;;

let%expect_test "dependent type" =
  go
    {|
let f = fn (static erased t : type) -> fn (x : t) -> -x;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 53)))
     (reason (Type_mismatch (got (Var $0)) (need (Type Int)))))
    |}]
;;

let%expect_test "dependent type" =
  go
    {|
let f = fn (static erased t : type) -> fn (x : t) -> if true then x else x;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg t)
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
                (body
                 (If
                  (cond
                   (Literal (value (Bool true)) (loc ((line 2) (column 56)))))
                  (then_ (Var (id x) (loc ((line 2) (column 66)))))
                  (else_ (Var (id x) (loc ((line 2) (column 73)))))
                  (static false) (loc ((line 2) (column 53)))))
                (loc ((line 2) (column 39)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (body
          (Lambda (arg x) (erased Unerased)
           (arg_mode ((staticity ()) (erasure ())))
           (arg_ty (Var (id t) (loc ((line 2) (column 47)))))
           (body
            (If (cond (Literal (value (Bool true)) (loc ((line 2) (column 56)))))
             (then_ (Var (id x) (loc ((line 2) (column 66)))))
             (else_ (Var (id x) (loc ((line 2) (column 73))))) (static false)
             (loc ((line 2) (column 53)))))
           (loc ((line 2) (column 39)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "dependent type" =
  go
    {|
let f = fn (static erased t1 : type) -> fn (static erased t2 : type) -> fn (x : t1) -> fn (y : t2) -> if true then x else y;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 102)))
     (reason (Cannot_unify (lhs (Var $0)) (rhs (Var $1)))))
    |}]
;;

let%expect_test "arrow typechecking" =
  go
    {|
let f = fn (g : int -> int) -> g 0;;
let _ = f (fn (x : int) -> 0);;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Lambda (arg g)
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
             (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
           (arg
            (Literal (value (Int (T 0))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
           (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
           (loc <opaque>)))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id f)
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
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (arg
          (Lambda (arg x)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (body
            (Literal (value (Int (T 0))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "arrow typechecking" =
  go
    {|
let f = fn (g : static int -> int) -> g 0;;
let _ = f (fn (static x : int) -> 0);;
|};
  [%expect
    {|
    ((loc ((line 2) (column 38)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "arrow typechecking" =
  go
    {|
let f = fn (_ : int) -> 0 @ erased;;
let _ = (f @ dynamic) 0;;
|};
  [%expect {| ((loc ((line 3) (column 8))) (reason Dynamic_erased)) |}]
;;

let%expect_test "arrow typechecking" =
  go
    {|
let f = fn (g : static int -> int) -> g 0;;
let _ = f (fn (x : int) -> 0);;
|};
  [%expect
    {|
    ((loc ((line 2) (column 38)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "arrow typechecking" =
  go
    {|
let f = fn (static g : static int -> int) -> g 0;;
let _ = f (fn (static x : int) -> 0);;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg g)
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
         (body
          (Apply (fn (Var (id g) (loc ((line 2) (column 45)))))
           (arg (Literal (value (Int 0)) (loc ((line 2) (column 47)))))
           (loc ((line 2) (column 45)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Symbol (id f) (arg (Closure 4))
         (mode ((staticity Dynamic) (erasure Unerased))) (ty (Type Int))
         (loc ((line 3) (column 8)))))
       (loc <opaque>))))
    |}]
;;

let%expect_test "arrow typechecking" =
  go
    {|
let f = fn (static g : static int -> int) -> g 0;;
let _ = f (fn (x : int) -> 0);;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg g)
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
         (body
          (Apply (fn (Var (id g) (loc ((line 2) (column 45)))))
           (arg (Literal (value (Int 0)) (loc ((line 2) (column 47)))))
           (loc ((line 2) (column 45)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Symbol (id f) (arg (Closure 3))
         (mode ((staticity Dynamic) (erasure Unerased))) (ty (Type Int))
         (loc ((line 3) (column 8)))))
       (loc <opaque>))))
    |}]
;;

let%expect_test "arrow typechecking" =
  go
    {|
let f = fn (erased g : erased int -> int) -> g 0;;
let _ = f (fn (x : int) -> 0);;
|};
  [%expect {| ((loc ((line 2) (column 45))) (reason Dynamic_erased)) |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (static x : int) -> x else fn (x : int) -> x;;
|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (If
         (cond
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (then_
          (Binder (arg x)
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Int)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (body (Var (id x) (loc ((line 2) (column 44))))) (mono <opaque>)
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (else_
          (Lambda (arg x)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (body
            (Var (id x) (ty (Type Int))
             (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (static x : int) -> x else fn (erased x : int) -> x;;
|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (If
         (cond
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (then_
          (Binder (arg x)
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Int)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (body (Var (id x) (loc ((line 2) (column 44))))) (mono <opaque>)
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (else_
          (Lambda (arg x)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Erased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Erased))))))
           (body
            (Erased (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
             (loc <opaque>)))
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Erased))))))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (static erased x : int) -> x else fn (x : int) -> x;;
|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (If
         (cond
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (then_
          (Binder (arg x)
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Erased)))
              (ret_ty (T (Type Int)))
              (ret_mode ((staticity Static) (erasure Erased))))))
           (body (Var (id x) (loc ((line 2) (column 51))))) (mono <opaque>)
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (else_
          (Lambda (arg x)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (body
            (Var (id x) (ty (Type Int))
             (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Erased))))))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (erased x : int) -> x else fn (x : int) -> x;;
|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (If
         (cond
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (then_
          (Lambda (arg x)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Erased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Erased))))))
           (body
            (Erased (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
             (loc <opaque>)))
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (else_
          (Lambda (arg x)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (body
            (Var (id x) (ty (Type Int))
             (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Erased))))))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (x : int) -> x else fn (static x : int) -> x;;
|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (If
         (cond
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (then_
          (Lambda (arg x)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (body
            (Var (id x) (ty (Type Int))
             (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (else_
          (Binder (arg x)
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Int)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (body (Var (id x) (loc ((line 2) (column 67))))) (mono <opaque>)
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (x : int) -> x else fn (erased x : int) -> x;;
|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (If
         (cond
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (then_
          (Lambda (arg x)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (body
            (Var (id x) (ty (Type Int))
             (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (else_
          (Lambda (arg x)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Erased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Erased))))))
           (body
            (Erased (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
             (loc <opaque>)))
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Erased))))))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (erased x : int) -> x else fn (static x : int) -> x;;
|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (If
         (cond
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (then_
          (Lambda (arg x)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Erased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Erased))))))
           (body
            (Erased (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
             (loc <opaque>)))
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (else_
          (Binder (arg x)
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Unerased)))
              (ret_ty (T (Type Int)))
              (ret_mode ((staticity Static) (erasure Unerased))))))
           (body (Var (id x) (loc ((line 2) (column 74))))) (mono <opaque>)
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Erased))))))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (x : int) -> x else fn (static erased x : int) -> x;;
|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (If
         (cond
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (then_
          (Lambda (arg x)
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (body
            (Var (id x) (ty (Type Int))
             (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (else_
          (Binder (arg x)
           (ty
            (Type
             (Pi (arg_ty (Type Int))
              (arg_mode ((staticity Static) (erasure Erased)))
              (ret_ty (T (Type Int)))
              (ret_mode ((staticity Static) (erasure Erased))))))
           (body (Var (id x) (loc ((line 2) (column 74))))) (mono <opaque>)
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Erased))))))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "return erased" =
  go
    {|
let f = fn (x : int) -> int;;
let _ = 0 : f 1;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 10)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "return erased" =
  go
    {|
let f = fn (x : int) -> 0 @ erased;;
let _ = f 1;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Lambda (arg x)
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Erased))))))
         (body
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Let (var x)
         (bind
          (Literal (value (Int (T 1))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (rest
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Erased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "arrow typechecking" =
  go
    {|
  let f = fn (g : erased int -> int) -> g 0;;
  |};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Lambda (arg g)
         (ty
          (Type
           (Arrow
            (arg_ty
             (Type
              (Arrow (arg_ty (Type Int))
               (arg_mode ((staticity Dynamic) (erasure Erased)))
               (ret_ty (Type Int))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Apply
           (fn
            (Var (id g)
             (ty
              (Type
               (Arrow (arg_ty (Type Int))
                (arg_mode ((staticity Dynamic) (erasure Erased)))
                (ret_ty (Type Int))
                (ret_mode ((staticity Dynamic) (erasure Unerased))))))
             (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
           (arg
            (Literal (value (Int (T 0))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
           (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
           (loc <opaque>)))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "arrow typechecking" =
  go
    {|
  let f = fn (g : static int -> int) -> g 0;;
  |};
  [%expect
    {|
    ((loc ((line 2) (column 40)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "pi typechecking" =
  go
    {|
  let f = fn (static g : erased int -> int) -> g 0;;
  let _ = f (fn (erased x : int) -> 0);;
  |};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg g)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Arrow (arg_ty (Type Int))
               (arg_mode ((staticity Dynamic) (erasure Erased)))
               (ret_ty (Type Int))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Apply (fn (Var (id g) (loc ((line 2) (column 47)))))
           (arg (Literal (value (Int 0)) (loc ((line 2) (column 49)))))
           (loc ((line 2) (column 47)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Symbol (id f) (arg (Closure 1))
         (mode ((staticity Dynamic) (erasure Unerased))) (ty (Type Int))
         (loc ((line 3) (column 10)))))
       (loc <opaque>))))
    |}]
;;

let%expect_test "arrow-pi typechecking" =
  go
    {|
let f = fn (x : int) -> 0 @ dynamic;;
let s = fn (static x : int) -> ();;
let _ = s (f 0);;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "arrow-pi typechecking" =
  go
    {|
let f = fn (x : int) -> 0;;
let s = fn (static x : int) -> ();;
let _ = s (f 0);;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "arrow-pi typechecking" =
  go
    {|
let f = fn (static g : static int -> int) -> g 1;;
let _ = f (fn (x : int) -> 0);;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg g)
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
         (body
          (Apply (fn (Var (id g) (loc ((line 2) (column 45)))))
           (arg (Literal (value (Int 1)) (loc ((line 2) (column 47)))))
           (loc ((line 2) (column 45)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Symbol (id f) (arg (Closure 3))
         (mode ((staticity Dynamic) (erasure Unerased))) (ty (Type Int))
         (loc ((line 3) (column 8)))))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Pi typechecking" =
  go
    {|
let f = fn (static erased g : static int -> int) -> g 0;;
let _ = f (fn (static x : int) -> x + 1);;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg g)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Pi (arg_ty (Type Int))
               (arg_mode ((staticity Static) (erasure Unerased)))
               (ret_ty (T (Type Int)))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Erased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Apply (fn (Var (id g) (loc ((line 2) (column 52)))))
           (arg (Literal (value (Int 0)) (loc ((line 2) (column 54)))))
           (loc ((line 2) (column 52)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Symbol (id f) (arg (Closure 4))
         (mode ((staticity Dynamic) (erasure Unerased))) (ty (Type Int))
         (loc ((line 3) (column 8)))))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Pi typechecking" =
  go
    {|
let f = fn (static erased g : static erased type -> int) -> g unit;;
let _ = f (fn (static erased t : type) -> () : t);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 45)))
     (reason (Type_mismatch (got (Type Unit)) (need (Var $2)))))
    |}]
;;

let%expect_test "Dependent lambda" =
  go
    {|
let f = fn (static erased g : static erased type -> unit) -> g unit;;
let _ = f (fn (static erased t : type) -> () : t);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 45)))
     (reason (Type_mismatch (got (Type Unit)) (need (Var $2)))))
    |}]
;;

let%expect_test "Dependent lambda" =
  go
    {|
let f = fn (static erased g : static erased type -> unit) -> g int;;
let _ = f (fn (static erased t : type) -> () : t);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 45)))
     (reason (Type_mismatch (got (Type Unit)) (need (Var $2)))))
    |}]
;;

let%expect_test "dependent lambda" =
  go
    {|
let f = fn (static g : static erased type -> int -> int) -> g int;;
let _ = f (fn (static erased t : type) -> fn (x : int) -> x);;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg g)
         (ty
          (Type
           (Pi
            (arg_ty
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
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (T
              (Type
               (Arrow (arg_ty (Type Int))
                (arg_mode ((staticity Dynamic) (erasure Unerased)))
                (ret_ty (Type Int))
                (ret_mode ((staticity Dynamic) (erasure Unerased)))))))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Apply (fn (Var (id g) (loc ((line 2) (column 60)))))
           (arg (Var (id int) (loc ((line 2) (column 62)))))
           (loc ((line 2) (column 60)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Symbol (id f) (arg (Closure 4))
         (mode ((staticity Dynamic) (erasure Unerased)))
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (loc ((line 3) (column 8)))))
       (loc <opaque>))))
    |}]
;;

let%expect_test "dependent fn" =
  go
    {|
let id = fn (static erased t : type) -> (fn (x : t) -> x);;
let x = (id int) (0 @ dynamic);;
let y = (id bool) (true @ dynamic);;
|};
  [%expect
    {|
    (tst
     ((Let (var id)
       (bind
        (Binder (arg t)
         (ty
          (Type
           (Pi (arg_ty (Type Type))
            (arg_mode ((staticity Static) (erasure Erased)))
            (ret_ty
             (Typecheck (env <opaque>) (arg t) (arg_ty (Type Type))
              (arg_mode ((staticity Static) (erasure Erased))) (memo <opaque>)
              (body
               (Paren
                (expr
                 (Lambda (arg x) (erased Unerased)
                  (arg_mode ((staticity ()) (erasure ())))
                  (arg_ty (Var (id t) (loc ((line 2) (column 49)))))
                  (body (Var (id x) (loc ((line 2) (column 55)))))
                  (loc ((line 2) (column 41)))))
                (loc ((line 2) (column 40)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (body
          (Paren
           (expr
            (Lambda (arg x) (erased Unerased)
             (arg_mode ((staticity ()) (erasure ())))
             (arg_ty (Var (id t) (loc ((line 2) (column 49)))))
             (body (Var (id x) (loc ((line 2) (column 55)))))
             (loc ((line 2) (column 41)))))
           (loc ((line 2) (column 40)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var x)
       (bind
        (Apply
         (fn
          (Symbol (id id) (arg IntT)
           (mode ((staticity Static) (erasure Unerased)))
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (loc ((line 3) (column 9)))))
         (arg
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var y)
       (bind
        (Apply
         (fn
          (Symbol (id id) (arg BoolT)
           (mode ((staticity Static) (erasure Unerased)))
           (ty
            (Type
             (Arrow (arg_ty (Type Bool))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Bool))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (loc ((line 4) (column 9)))))
         (arg
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Bool)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "dependent fn" =
  go
    {|
let id = fn (static erased t : type) -> (fn (x : t) -> x) @ erased;;
let x = (id int) (0 @ dynamic);;
let y = (id bool) (true @ dynamic);;
|};
  [%expect
    {|
    (tst
     ((Let (var id)
       (bind
        (Binder (arg t)
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
                    (arg_ty (Var (id t) (loc ((line 2) (column 49)))))
                    (body (Var (id x) (loc ((line 2) (column 55)))))
                    (loc ((line 2) (column 41)))))
                  (loc ((line 2) (column 40)))))
                (mode ((staticity ()) (erasure (Erased))))
                (loc ((line 2) (column 58)))))))
            (ret_mode ((staticity Static) (erasure Erased))))))
         (body
          (Mode_annotation
           (expr
            (Paren
             (expr
              (Lambda (arg x) (erased Unerased)
               (arg_mode ((staticity ()) (erasure ())))
               (arg_ty (Var (id t) (loc ((line 2) (column 49)))))
               (body (Var (id x) (loc ((line 2) (column 55)))))
               (loc ((line 2) (column 41)))))
             (loc ((line 2) (column 40)))))
           (mode ((staticity ()) (erasure (Erased))))
           (loc ((line 2) (column 58)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Erased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var x)
       (bind
        (Let (var x)
         (bind
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (rest
          (Var (id x) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var y)
       (bind
        (Let (var x)
         (bind
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (rest
          (Var (id x) (ty (Type Bool))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Bool)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "dependent arrow" =
  go
    {|
let mk_int = fn (static x : int) -> int;;
let apply_int = fn (static f : static int \ x -> mk_int x) -> 2;;
|};
  [%expect
    {|
    (tst
     ((Let (var mk_int)
       (bind
        (Binder (arg x)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Type)))
            (ret_mode ((staticity Static) (erasure Erased))))))
         (body (Var (id int) (loc ((line 2) (column 36))))) (mono <opaque>)
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var apply_int)
       (bind
        (Binder (arg f)
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
                  (Apply (fn (Var (id mk_int) (loc ((line 3) (column 49)))))
                   (arg (Var (id x) (loc ((line 3) (column 56)))))
                   (loc ((line 3) (column 49)))))))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (body (Literal (value (Int 2)) (loc ((line 3) (column 62)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "dependent arrow" =
  go
    {|
let mk_int = fn (static x : int) -> int;;
let apply_int = fn (static f : static int \ x -> unit -> mk_int x) -> f 2;;
|};
  [%expect
    {|
    (tst
     ((Let (var mk_int)
       (bind
        (Binder (arg x)
         (ty
          (Type
           (Pi (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Type)))
            (ret_mode ((staticity Static) (erasure Erased))))))
         (body (Var (id int) (loc ((line 2) (column 36))))) (mono <opaque>)
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var apply_int)
       (bind
        (Binder (arg f)
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
                  (Arrow (arg (Var (id unit) (loc ((line 3) (column 49)))))
                   (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
                   (ret
                    (Apply (fn (Var (id mk_int) (loc ((line 3) (column 57)))))
                     (arg (Var (id x) (loc ((line 3) (column 64)))))
                     (loc ((line 3) (column 57)))))
                   (ret_mode ((staticity ()) (erasure ())))
                   (loc ((line 3) (column 54)))))))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (T
              (Type
               (Arrow (arg_ty (Type Unit))
                (arg_mode ((staticity Dynamic) (erasure Unerased)))
                (ret_ty (Type Int))
                (ret_mode ((staticity Dynamic) (erasure Unerased)))))))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (body
          (Apply (fn (Var (id f) (loc ((line 3) (column 70)))))
           (arg (Literal (value (Int 2)) (loc ((line 3) (column 72)))))
           (loc ((line 3) (column 70)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "dependent arrow" =
  go
    {|
let apply_int = fn (static f : static erased type \ t -> t -> t) -> f int;;
let _ = apply_int (fn (static erased t : type) -> fn (x : t) -> x);;
|};
  [%expect
    {|
    (tst
     ((Let (var apply_int)
       (bind
        (Binder (arg f)
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
                  (Arrow (arg (Var (id t) (loc ((line 2) (column 57)))))
                   (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
                   (ret (Var (id t) (loc ((line 2) (column 62)))))
                   (ret_mode ((staticity ()) (erasure ())))
                   (loc ((line 2) (column 59)))))))
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
         (body
          (Apply (fn (Var (id f) (loc ((line 2) (column 68)))))
           (arg (Var (id int) (loc ((line 2) (column 70)))))
           (loc ((line 2) (column 68)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Symbol (id apply_int) (arg (Closure 4))
         (mode ((staticity Dynamic) (erasure Unerased)))
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (loc ((line 3) (column 8)))))
       (loc <opaque>))))
    |}]
;;

let%expect_test "dependent arrow" =
  go
    {|
let apply = fn (static f : static erased type \ t -> t -> t) -> fn (static erased t2 : type) -> f t2;;
let f = apply (fn (static erased t : type) -> fn (x : t) -> x);;
let g = f int;;
let h = f bool;;
|};
  [%expect
    {|
    (tst
     ((Let (var apply)
       (bind
        (Binder (arg f)
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
                  (Arrow (arg (Var (id t) (loc ((line 2) (column 53)))))
                   (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
                   (ret (Var (id t) (loc ((line 2) (column 58)))))
                   (ret_mode ((staticity ()) (erasure ())))
                   (loc ((line 2) (column 55)))))))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (Typecheck (env <opaque>) (arg f)
              (arg_ty
               (Type
                (Pi (arg_ty (Type Type))
                 (arg_mode ((staticity Static) (erasure Erased)))
                 (ret_ty
                  (Reduce (env <opaque>) (arg t) (arg_ty (Type Type))
                   (arg_mode ((staticity Static) (erasure Erased)))
                   (memo <opaque>)
                   (ret_ty
                    (Arrow (arg (Var (id t) (loc ((line 2) (column 53)))))
                     (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
                     (ret (Var (id t) (loc ((line 2) (column 58)))))
                     (ret_mode ((staticity ()) (erasure ())))
                     (loc ((line 2) (column 55)))))))
                 (ret_mode ((staticity Dynamic) (erasure Unerased))))))
              (arg_mode ((staticity Static) (erasure Unerased))) (memo <opaque>)
              (body
               (Lambda (arg t2) (erased Unerased)
                (arg_mode ((staticity (Static)) (erasure (Erased))))
                (arg_ty (Var (id type) (loc ((line 2) (column 87)))))
                (body
                 (Apply (fn (Var (id f) (loc ((line 2) (column 96)))))
                  (arg (Var (id t2) (loc ((line 2) (column 98)))))
                  (loc ((line 2) (column 96)))))
                (loc ((line 2) (column 64)))))))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (body
          (Lambda (arg t2) (erased Unerased)
           (arg_mode ((staticity (Static)) (erasure (Erased))))
           (arg_ty (Var (id type) (loc ((line 2) (column 87)))))
           (body
            (Apply (fn (Var (id f) (loc ((line 2) (column 96)))))
             (arg (Var (id t2) (loc ((line 2) (column 98)))))
             (loc ((line 2) (column 96)))))
           (loc ((line 2) (column 64)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var f)
       (bind
        (Symbol (id apply) (arg (Closure 7))
         (mode ((staticity Static) (erasure Unerased)))
         (ty
          (Type
           (Pi (arg_ty (Type Type))
            (arg_mode ((staticity Static) (erasure Erased)))
            (ret_ty
             (Typecheck (env <opaque>) (arg t2) (arg_ty (Type Type))
              (arg_mode ((staticity Static) (erasure Erased))) (memo <opaque>)
              (body
               (Apply (fn (Var (id f) (loc ((line 2) (column 96)))))
                (arg (Var (id t2) (loc ((line 2) (column 98)))))
                (loc ((line 2) (column 96)))))))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (loc ((line 3) (column 8)))))
       (loc <opaque>))
      (Let (var g)
       (bind
        (Symbol (id f) (arg IntT) (mode ((staticity Dynamic) (erasure Unerased)))
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (loc ((line 4) (column 8)))))
       (loc <opaque>))
      (Let (var h)
       (bind
        (Symbol (id f) (arg BoolT)
         (mode ((staticity Dynamic) (erasure Unerased)))
         (ty
          (Type
           (Arrow (arg_ty (Type Bool))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Bool))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (loc ((line 5) (column 8)))))
       (loc <opaque>))))
    |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else true;;
let _ = f 0;;
let _ = f 1;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg x)
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
         (body
          (If
           (cond
            (Binop (op Eq) (lhs (Var (id x) (loc ((line 2) (column 41)))))
             (rhs (Literal (value (Int 0)) (loc ((line 2) (column 46)))))
             (loc ((line 2) (column 43)))))
           (then_ (Literal (value (Int 1)) (loc ((line 2) (column 53)))))
           (else_ (Literal (value (Bool true)) (loc ((line 2) (column 60)))))
           (static true) (loc ((line 2) (column 31)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Symbol (id f) (arg (Int 0))
         (mode ((staticity Static) (erasure Unerased))) (ty (Type Int))
         (loc ((line 3) (column 8)))))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Symbol (id f) (arg (Int 1))
         (mode ((staticity Static) (erasure Unerased))) (ty (Type Bool))
         (loc ((line 4) (column 8)))))
       (loc <opaque>))))
    |}]
;;

let%expect_test "dependent if" =
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

let%expect_test "dependent if" =
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

let%expect_test "dependent if" =
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
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "dependent if" =
  go
    {|
let _ = (if static true then 1 else true) : (if static true then int else bool);;
|};
  [%expect
    {|
    (tst
     ((Let (var _)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "dependent if" =
  go
    {|
let _ = (if static true then 1 else true + 1) : int;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 41)))
     (reason (Type_mismatch (got (Type Bool)) (need (Type Int)))))
    |}]
;;

let%expect_test "dependent if" =
  go
    {|
let _ = fn (static x : int) -> (if static x==0 then 1 else true + 1) : int;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 64)))
     (reason (Type_mismatch (got (Type Bool)) (need (Type Int)))))
    |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static _ : unit) -> int;;
let _ = (if static true then 1 else (0 : f ())) : int;;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg _)
         (ty
          (Type
           (Pi (arg_ty (Type Unit))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Type)))
            (ret_mode ((staticity Static) (erasure Erased))))))
         (body (Var (id int) (loc ((line 2) (column 32))))) (mono <opaque>)
         (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Literal (value (Int (T 1))) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "dependent if" =
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
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "dependent if" =
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
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "bad annotation" =
  go
    {|
let f = fn (static x : int) -> (if static x == 0 then 1 else true) : (if static x == 0 then 0 else bool);;
|};
  [%expect
    {|
    ((loc ((line 2) (column 67)))
     (reason
      (Type_mismatch
       (got
        (If (cond (Bool (Eq (Var $0) (Int (T 0))))) (then_ (Type Int))
         (else_ (Type Type))))
       (need (Type Type)))))
    |}]
;;

let%expect_test "bad annotation" =
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

let%expect_test "bad annotation" =
  go
    {|
let f = 0 : 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 10)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Type)))))
    |}]
;;

let%expect_test "bad annotation" =
  go
    {|
fun f (x : 0) : int = 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 4)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Type)))))
    |}]
;;

let%expect_test "bad annotation" =
  go
    {|
fun f (x : int) : 0 = 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 4)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Type)))))
    |}]
;;

let%expect_test "dependent if unify" =
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
         (body
          (If
           (cond
            (Binop (op Eq) (lhs (Var (id x) (loc ((line 2) (column 41)))))
             (rhs (Literal (value (Int 0)) (loc ((line 2) (column 46)))))
             (loc ((line 2) (column 43)))))
           (then_ (Literal (value (Int 1)) (loc ((line 2) (column 53)))))
           (else_ (Literal (value (Bool true)) (loc ((line 2) (column 60)))))
           (static true) (loc ((line 2) (column 31)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var g)
       (bind
        (Binder (arg x)
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
         (body
          (If
           (cond
            (Binop (op Eq) (lhs (Var (id x) (loc ((line 3) (column 41)))))
             (rhs (Literal (value (Int 0)) (loc ((line 3) (column 46)))))
             (loc ((line 3) (column 43)))))
           (then_ (Literal (value (Int 1)) (loc ((line 3) (column 53)))))
           (else_ (Literal (value (Bool true)) (loc ((line 3) (column 60)))))
           (static true) (loc ((line 3) (column 31)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (If
         (cond
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
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
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
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
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
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
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "dependent if unify" =
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
         (body
          (If
           (cond
            (Binop (op Eq) (lhs (Var (id x) (loc ((line 2) (column 41)))))
             (rhs (Literal (value (Int 0)) (loc ((line 2) (column 46)))))
             (loc ((line 2) (column 43)))))
           (then_ (Literal (value (Int 1)) (loc ((line 2) (column 53)))))
           (else_ (Literal (value (Bool true)) (loc ((line 2) (column 60)))))
           (static true) (loc ((line 2) (column 31)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var g)
       (bind
        (Binder (arg x)
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
         (body
          (If
           (cond
            (Binop (op Eq) (lhs (Var (id x) (loc ((line 3) (column 41)))))
             (rhs (Literal (value (Int 0)) (loc ((line 3) (column 46)))))
             (loc ((line 3) (column 43)))))
           (then_ (Literal (value (Bool true)) (loc ((line 3) (column 53)))))
           (else_ (Literal (value (Int 2)) (loc ((line 3) (column 63)))))
           (static true) (loc ((line 3) (column 31)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (If
         (cond
          (Literal (value (Bool (T true))) (ty (Type Bool))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (then_
          (Symbol (id f) (arg (Int 0))
           (mode ((staticity Static) (erasure Unerased))) (ty (Type Int))
           (loc ((line 4) (column 21)))))
         (else_
          (Symbol (id g) (arg (Int 1))
           (mode ((staticity Static) (erasure Unerased))) (ty (Type Int))
           (loc ((line 4) (column 30)))))
         (ty (Type Int)) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "symbolic arrow type" =
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
         (body
          (Apply (fn (Var (id f) (loc ((line 2) (column 79)))))
           (arg (Literal (value (Int 0)) (loc ((line 2) (column 81)))))
           (loc ((line 2) (column 79)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Symbol (id choose) (arg (Closure 4))
         (mode ((staticity Dynamic) (erasure Unerased))) (ty (Type Int))
         (loc ((line 3) (column 8)))))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Fun recursive dynamic arg" =
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
              (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
            (arg
             (Var (id x) (ty (Type Int))
              (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
            (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
            (loc <opaque>)))
          (ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Unerased))) (loc <opaque>))))
       (loc <opaque>))))
    |}]
;;

let%expect_test "fun recursive erased" =
  go
    {|
fun f (erased x : int) : int = f x;;
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
                 (arg_mode ((staticity Dynamic) (erasure Erased)))
                 (ret_ty (Type Int))
                 (ret_mode ((staticity Dynamic) (erasure Unerased))))))
              (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
            (arg
             (Erased (ty (Type Int))
              (mode ((staticity Dynamic) (erasure Erased))) (loc <opaque>)))
            (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
            (loc <opaque>)))
          (ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Erased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Unerased))) (loc <opaque>))))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Fun erased arg" =
  go
    {|
fun f (erased x : int) : erased int = x;;
|};
  [%expect
    {|
    (tst
     ((Fun
       (funs
        ((Lambda (var f) (arg x)
          (body
           (Erased (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
            (loc <opaque>)))
          (ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Erased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Erased))))))
          (mode ((staticity Static) (erasure Erased))) (loc <opaque>))))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Fun erased ret" =
  go
    {|
fun f (erased x : int) : int = x;;
let y = (f 0) @ unerased;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 4)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "Fun return static" =
  go
    {|
fun f (x : int) : int = 1;;
|};
  [%expect
    {|
    (tst
     ((Fun
       (funs
        ((Lambda (var f) (arg x)
          (body
           (Literal (value (Int (T 1))) (ty (Type Int))
            (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
          (ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Unerased))) (loc <opaque>))))
       (loc <opaque>))))
    |}]
;;

let%expect_test "Fun return erased" =
  go
    {|
fun f (x : int) : erased int = 1 @ erased;;
|};
  [%expect
    {|
    (tst
     ((Fun
       (funs
        ((Lambda (var f) (arg x)
          (body
           (Literal (value (Int (T 1))) (ty (Type Int))
            (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
          (ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Erased))))))
          (mode ((staticity Static) (erasure Erased))) (loc <opaque>))))
       (loc <opaque>))))
    |}]
;;

let%expect_test "mono fun" =
  go
    {|
fun x (static x : int) : int = x;;
let y = x (0 @ dynamic);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "mono fun" =
  go
    {|
fun x (static x : int) : int = x;;
let y = x 0;;
|};
  [%expect
    {|
    (tst
     ((Fun
       (funs
        ((Binder (var x) (arg x) (body (Var (id x) (loc ((line 2) (column 31)))))
          (mono <opaque>)
          (ty
           (Type
            (Pi (arg_ty (Type Int))
             (arg_mode ((staticity Static) (erasure Unerased)))
             (ret_ty (T (Type Int)))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Unerased))) (loc <opaque>))))
       (loc <opaque>))
      (Let (var y)
       (bind
        (Symbol (id x) (arg (Int 0))
         (mode ((staticity Dynamic) (erasure Unerased))) (ty (Type Int))
         (loc ((line 3) (column 8)))))
       (loc <opaque>))))
    |}]
;;

let%expect_test "static type" =
  go
    {|
fun f (static _ : unit) : static type = int;;
let y = 0 : f ();;
|};
  [%expect
    {|
    ((loc ((line 2) (column 4)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "static type" =
  go
    {|
fun f (static _ : unit) : static erased type = int;;
let y = 0 : f ();;
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
          (mode ((staticity Static) (erasure Erased))) (loc <opaque>))))
       (loc <opaque>))
      (Let (var y)
       (bind
        (Literal (value (Int (T 0))) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "types are erased" =
  go
    {|
fun f (static _ : unit) : static erased type = int;;
let y = (f () @ dynamic);;
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
          (mode ((staticity Static) (erasure Erased))) (loc <opaque>))))
       (loc <opaque>))
      (Let (var y)
       (bind
        (Let (var _)
         (bind
          (Literal (value Unit) (ty (Type Unit))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (rest
          (Literal (value (Type Int)) (ty (Type Type))
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (ty (Type Type)) (mode ((staticity Dynamic) (erasure Erased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "types are erased" =
  go
    {|
fun f (static _ : unit) : static erased type = int;;
let y = 5 : f ();;
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
          (mode ((staticity Static) (erasure Erased))) (loc <opaque>))))
       (loc <opaque>))
      (Let (var y)
       (bind
        (Literal (value (Int (T 5))) (ty (Type Int))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "types are erased" =
  go
    {|
fun f (static _ : unit) : static erased type = int;;
let y = f @ unerased;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 10)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "dependent fun " =
  go
    {|
fun id (erased t : type) : t -> t = fn (x : t) -> x;;
|};
  [%expect {| ((loc ((line 2) (column 27))) (reason (Unbound_ident t))) |}]
;;

let%expect_test "dependent fun " =
  go
    {|
fun id (static erased t : type) : t -> t = fn (x : t) -> x;;
let i = id int;;
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
          (mode ((staticity Static) (erasure Unerased))) (loc <opaque>))))
       (loc <opaque>))
      (Let (var i)
       (bind
        (Symbol (id id) (arg IntT)
         (mode ((staticity Dynamic) (erasure Unerased)))
         (ty
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (loc ((line 3) (column 8)))))
       (loc <opaque>))))
    |}]
;;

let%expect_test "erased fun " =
  go
    {|
fun id (erased x : int) : erased int = x;;
let _ = id 0;;
|};
  [%expect
    {|
    (tst
     ((Fun
       (funs
        ((Lambda (var id) (arg x)
          (body
           (Erased (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
            (loc <opaque>)))
          (ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Erased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Erased))))))
          (mode ((staticity Static) (erasure Erased))) (loc <opaque>))))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Erased (ty (Type Int)) (mode ((staticity Dynamic) (erasure Erased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "erased fun " =
  go
    {|
fun id (erased x : int) : erased int = x;;
let _ = (id @ dynamic) 0;;
|};
  [%expect {| ((loc ((line 3) (column 8))) (reason Dynamic_erased)) |}]
;;

let%expect_test "dependent fun erased" =
  go
    {|
fun id (static erased t : type) : t -> t = fn (x : t) -> x;;
let x = (id int) (0 @ dynamic);;
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
          (mode ((staticity Static) (erasure Unerased))) (loc <opaque>))))
       (loc <opaque>))
      (Let (var x)
       (bind
        (Apply
         (fn
          (Symbol (id id) (arg IntT)
           (mode ((staticity Dynamic) (erasure Unerased)))
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (loc ((line 3) (column 9)))))
         (arg
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "dependent fun" =
  go
    {|
let ty = fn (static _ : unit) -> int -> int;;
fun id (_ : unit) : ty () = fn (x : int) -> x;;
let x = id () 0;;
|};
  [%expect
    {|
    (tst
     ((Let (var ty)
       (bind
        (Binder (arg _)
         (ty
          (Type
           (Pi (arg_ty (Type Unit))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty (T (Type Type)))
            (ret_mode ((staticity Static) (erasure Erased))))))
         (body
          (Arrow (arg (Var (id int) (loc ((line 2) (column 33))))) (arg_id ())
           (arg_mode ((staticity ()) (erasure ())))
           (ret (Var (id int) (loc ((line 2) (column 40)))))
           (ret_mode ((staticity ()) (erasure ()))) (loc ((line 2) (column 37)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Erased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Fun
       (funs
        ((Lambda (var id) (arg _)
          (body
           (Lambda (arg x)
            (ty
             (Type
              (Arrow (arg_ty (Type Int))
               (arg_mode ((staticity Dynamic) (erasure Unerased)))
               (ret_ty (Type Int))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (body
             (Var (id x) (ty (Type Int))
              (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
            (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
          (ty
           (Type
            (Arrow (arg_ty (Type Unit))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty
              (Type
               (Arrow (arg_ty (Type Int))
                (arg_mode ((staticity Dynamic) (erasure Unerased)))
                (ret_ty (Type Int))
                (ret_mode ((staticity Dynamic) (erasure Unerased))))))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Unerased))) (loc <opaque>))))
       (loc <opaque>))
      (Let (var x)
       (bind
        (Apply
         (fn
          (Apply
           (fn
            (Var (id id)
             (ty
              (Type
               (Arrow (arg_ty (Type Unit))
                (arg_mode ((staticity Dynamic) (erasure Unerased)))
                (ret_ty
                 (Type
                  (Arrow (arg_ty (Type Int))
                   (arg_mode ((staticity Dynamic) (erasure Unerased)))
                   (ret_ty (Type Int))
                   (ret_mode ((staticity Dynamic) (erasure Unerased))))))
                (ret_mode ((staticity Dynamic) (erasure Unerased))))))
             (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
           (arg
            (Literal (value Unit) (ty (Type Unit))
             (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (arg
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "dependent fun" =
  go
    {|
fun id1 (static erased t : type) : t -> t = fn (x : t) -> x;;
fun id2 (static erased t : type) : t -> t = id1 t;;
let x = id2 int (0 @ dynamic);;
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
          (mode ((staticity Static) (erasure Unerased))) (loc <opaque>))))
       (loc <opaque>))
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
          (mode ((staticity Static) (erasure Unerased))) (loc <opaque>))))
       (loc <opaque>))
      (Let (var x)
       (bind
        (Apply
         (fn
          (Symbol (id id2) (arg IntT)
           (mode ((staticity Dynamic) (erasure Unerased)))
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (loc ((line 4) (column 8)))))
         (arg
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "join" =
  go
    {|
fun a (_ : unit) : unit = ();;
fun b (_ : unit) : unit -> unit = fn (_ : unit) -> ();;
let x = if static false then a else b;;
let _ = x () ();;
|};
  [%expect
    {|
    (tst
     ((Fun
       (funs
        ((Lambda (var a) (arg _)
          (body
           (Literal (value Unit) (ty (Type Unit))
            (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
          (ty
           (Type
            (Arrow (arg_ty (Type Unit))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Unit))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Unerased))) (loc <opaque>))))
       (loc <opaque>))
      (Fun
       (funs
        ((Lambda (var b) (arg _)
          (body
           (Lambda (arg _)
            (ty
             (Type
              (Arrow (arg_ty (Type Unit))
               (arg_mode ((staticity Dynamic) (erasure Unerased)))
               (ret_ty (Type Unit))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (body
             (Literal (value Unit) (ty (Type Unit))
              (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
            (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
          (ty
           (Type
            (Arrow (arg_ty (Type Unit))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty
              (Type
               (Arrow (arg_ty (Type Unit))
                (arg_mode ((staticity Dynamic) (erasure Unerased)))
                (ret_ty (Type Unit))
                (ret_mode ((staticity Dynamic) (erasure Unerased))))))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Unerased))) (loc <opaque>))))
       (loc <opaque>))
      (Let (var x)
       (bind
        (Var (id b)
         (ty
          (Type
           (Arrow (arg_ty (Type Unit))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty
             (Type
              (Arrow (arg_ty (Type Unit))
               (arg_mode ((staticity Dynamic) (erasure Unerased)))
               (ret_ty (Type Unit))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (ret_mode ((staticity Dynamic) (erasure Unerased))))))
         (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Apply
           (fn
            (Var (id x)
             (ty
              (Type
               (Arrow (arg_ty (Type Unit))
                (arg_mode ((staticity Dynamic) (erasure Unerased)))
                (ret_ty
                 (Type
                  (Arrow (arg_ty (Type Unit))
                   (arg_mode ((staticity Dynamic) (erasure Unerased)))
                   (ret_ty (Type Unit))
                   (ret_mode ((staticity Dynamic) (erasure Unerased))))))
                (ret_mode ((staticity Dynamic) (erasure Unerased))))))
             (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
           (arg
            (Literal (value Unit) (ty (Type Unit))
             (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
           (ty
            (Type
             (Arrow (arg_ty (Type Unit))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Unit))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (arg
          (Literal (value Unit) (ty (Type Unit))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Unit)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "return fn" =
  go
    {|
fun x (_ : unit) : unit -> unit = fn (_ : unit) -> ();;
let _ = x () ();;
|};
  [%expect
    {|
    (tst
     ((Fun
       (funs
        ((Lambda (var x) (arg _)
          (body
           (Lambda (arg _)
            (ty
             (Type
              (Arrow (arg_ty (Type Unit))
               (arg_mode ((staticity Dynamic) (erasure Unerased)))
               (ret_ty (Type Unit))
               (ret_mode ((staticity Dynamic) (erasure Unerased))))))
            (body
             (Literal (value Unit) (ty (Type Unit))
              (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
            (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
          (ty
           (Type
            (Arrow (arg_ty (Type Unit))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty
              (Type
               (Arrow (arg_ty (Type Unit))
                (arg_mode ((staticity Dynamic) (erasure Unerased)))
                (ret_ty (Type Unit))
                (ret_mode ((staticity Dynamic) (erasure Unerased))))))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Unerased))) (loc <opaque>))))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Apply
           (fn
            (Var (id x)
             (ty
              (Type
               (Arrow (arg_ty (Type Unit))
                (arg_mode ((staticity Dynamic) (erasure Unerased)))
                (ret_ty
                 (Type
                  (Arrow (arg_ty (Type Unit))
                   (arg_mode ((staticity Dynamic) (erasure Unerased)))
                   (ret_ty (Type Unit))
                   (ret_mode ((staticity Dynamic) (erasure Unerased))))))
                (ret_mode ((staticity Dynamic) (erasure Unerased))))))
             (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
           (arg
            (Literal (value Unit) (ty (Type Unit))
             (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
           (ty
            (Type
             (Arrow (arg_ty (Type Unit))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Unit))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
         (arg
          (Literal (value Unit) (ty (Type Unit))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Unit)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "arg fn" =
  go
    {|
fun x (f : unit -> int) : int = f ();;
let _ = x (fn (_ : unit) -> 1);;
|};
  [%expect
    {|
    (tst
     ((Fun
       (funs
        ((Lambda (var x) (arg f)
          (body
           (Apply
            (fn
             (Var (id f)
              (ty
               (Type
                (Arrow (arg_ty (Type Unit))
                 (arg_mode ((staticity Dynamic) (erasure Unerased)))
                 (ret_ty (Type Int))
                 (ret_mode ((staticity Dynamic) (erasure Unerased))))))
              (mode ((staticity Dynamic) (erasure Unerased))) (loc <opaque>)))
            (arg
             (Literal (value Unit) (ty (Type Unit))
              (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
            (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
            (loc <opaque>)))
          (ty
           (Type
            (Arrow
             (arg_ty
              (Type
               (Arrow (arg_ty (Type Unit))
                (arg_mode ((staticity Dynamic) (erasure Unerased)))
                (ret_ty (Type Int))
                (ret_mode ((staticity Dynamic) (erasure Unerased))))))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (mode ((staticity Static) (erasure Unerased))) (loc <opaque>))))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id x)
           (ty
            (Type
             (Arrow
              (arg_ty
               (Type
                (Arrow (arg_ty (Type Unit))
                 (arg_mode ((staticity Dynamic) (erasure Unerased)))
                 (ret_ty (Type Int))
                 (ret_mode ((staticity Dynamic) (erasure Unerased))))))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (arg
          (Lambda (arg _)
           (ty
            (Type
             (Arrow (arg_ty (Type Unit))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (body
            (Literal (value (Int (T 1))) (ty (Type Int))
             (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "universal value" =
  go
    {|
let f = fn (static erased t : type) -> fn (static x : t) -> !x;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 60)))
     (reason (Type_mismatch (got (Var $0)) (need (Type Bool)))))
    |}]
;;

let%expect_test "arrow" =
  go
    {|
let f = 0 -> int;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 10)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Type)))))
    |}]
;;

let%expect_test "arrow" =
  go
    {|
let f = int -> 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 12)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Type)))))
    |}]
;;

let%expect_test "arrow" =
  go
    {|
let x = int @ dynamic;;
let f = int -> x;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 12)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "arrow" =
  go
    {|
let x = int @ dynamic;;
let f = x -> int;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 10)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "arrow" =
  go
    {|
let f = static type \ t -> (t @ dynamic);;
|};
  [%expect
    {|
    ((loc ((line 2) (column 20)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "arrow" =
  go
    {|
let f = static int \ t -> t;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 19)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Type)))))
    |}]
;;

let%expect_test "fun" =
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

let%expect_test "fun" =
  go
    {|
fun f (x : 0) : int = x;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 4)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Type)))))
    |}]
;;

let%expect_test "fun" =
  go
    {|
fun f (x : int) : 0 = x;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 4)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Type)))))
    |}]
;;

let%expect_test "fun" =
  go
    {|
fun f (x : (int @ dynamic)) : int = x;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 4)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "fun" =
  go
    {|
fun f (x : int) : (int @ dynamic) = x;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 4)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "fun" =
  go
    {|
fun f (static x : int) : x = x;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 4)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Type)))))
    |}]
;;

let%expect_test "fun" =
  go
    {|
fun f (x : type) : x = ();;
|};
  [%expect {| ((loc ((line 2) (column 19))) (reason (Unbound_ident x))) |}]
;;

let%expect_test "fun" =
  go
    {|
fun f (static x : type) : x = ();;
|};
  [%expect
    {|
    ((loc ((line 2) (column 4)))
     (reason (Type_mismatch (got (Type Unit)) (need (Var $1)))))
    |}]
;;

let%expect_test "fun" =
  go
    {|
fun f (static x : type) : (x @ dynamic) = ();;
|};
  [%expect
    {|
    ((loc ((line 2) (column 4)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "fun" =
  go
    {|
fun f (static x : type) : (x + 1) = ();;
|};
  [%expect
    {|
    ((loc ((line 2) (column 29)))
     (reason (Type_mismatch (got (Type Type)) (need (Type Int)))))
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (static erased g : int -> erased int) -> let _ = g 1; 2;;
let _ = f (fn (x : int) -> 0 @ erased);;
|};
  [%expect
    {|
    (tst
     ((Let (var f)
       (bind
        (Binder (arg g)
         (ty
          (Type
           (Pi
            (arg_ty
             (Type
              (Arrow (arg_ty (Type Int))
               (arg_mode ((staticity Dynamic) (erasure Unerased)))
               (ret_ty (Type Int))
               (ret_mode ((staticity Dynamic) (erasure Erased))))))
            (arg_mode ((staticity Static) (erasure Erased)))
            (ret_ty (T (Type Int)))
            (ret_mode ((staticity Static) (erasure Unerased))))))
         (body
          (Let (var _)
           (bind
            (Apply (fn (Var (id g) (loc ((line 2) (column 60)))))
             (arg (Literal (value (Int 1)) (loc ((line 2) (column 62)))))
             (loc ((line 2) (column 60)))))
           (rest (Literal (value (Int 2)) (loc ((line 2) (column 65)))))
           (loc ((line 2) (column 52)))))
         (mono <opaque>) (mode ((staticity Static) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))
      (Let (var _)
       (bind
        (Symbol (id f) (arg (Closure 1))
         (mode ((staticity Static) (erasure Unerased))) (ty (Type Int))
         (loc ((line 3) (column 8)))))
       (loc <opaque>))))
    |}]
;;

let%expect_test "external" =
  go
    {|
external f : int -> int = asdf;;
let _ = f 0;;
|};
  [%expect
    {|
    (tst
     ((External (var f) (symbol asdf)
       (ty
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret_ty (Type Int))
          (ret_mode ((staticity Dynamic) (erasure Unerased))))))
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
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (arg
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "external" =
  go
    {|
external f : int -> int = asdf;;
let _ = (f @ erased) 0;;
|};
  [%expect
    {|
    (tst
     ((External (var f) (symbol asdf)
       (ty
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret_ty (Type Int))
          (ret_mode ((staticity Dynamic) (erasure Unerased))))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Literal
           (value
            (External (symbol asdf)
             (ty
              (Type
               (Arrow (arg_ty (Type Int))
                (arg_mode ((staticity Dynamic) (erasure Unerased)))
                (ret_ty (Type Int))
                (ret_mode ((staticity Dynamic) (erasure Unerased))))))))
           (ty
            (Type
             (Arrow (arg_ty (Type Int))
              (arg_mode ((staticity Dynamic) (erasure Unerased)))
              (ret_ty (Type Int))
              (ret_mode ((staticity Dynamic) (erasure Unerased))))))
           (mode ((staticity Static) (erasure Erased))) (loc <opaque>)))
         (arg
          (Literal (value (Int (T 0))) (ty (Type Int))
           (mode ((staticity Static) (erasure Unerased))) (loc <opaque>)))
         (ty (Type Int)) (mode ((staticity Dynamic) (erasure Unerased)))
         (loc <opaque>)))
       (loc <opaque>))))
    |}]
;;

let%expect_test "external" =
  go
    {|
external f : static int -> int = asdf;;
let _ = f 0;;
|};
  [%expect {| ((loc ((line 2) (column 0))) (reason Static_external)) |}]
;;

let%expect_test "external" =
  go
    {|
external f : int -> static int = asdf;;
let _ = f 0;;
|};
  [%expect {| ((loc ((line 2) (column 0))) (reason Static_external)) |}]
;;
