open! Core
open! Syl

let go input =
  let cst = Parse.parse_exn input in
  let tst = Typecheck.typecheck_exn cst in
  let sst = Simplify.simplify tst in
  print_s [%message (sst : Sst.Program.t)]
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
    (sst
     ((Let (var _)
       (bind (Scalar (value Unit) (ty Unit) (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind (Scalar (value (Bool true)) (ty Bool) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind (Scalar (value (Int 123)) (ty Int) (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))
      (Let (var _)
       (bind (Scalar (value Unit) (ty Unit) (loc ((line 8) (column 8)))))
       (loc ((line 8) (column 0))))
      (Let (var _)
       (bind (Scalar (value (Bool true)) (ty Bool) (loc ((line 9) (column 8)))))
       (loc ((line 9) (column 0))))
      (Let (var _)
       (bind (Scalar (value (Int 123)) (ty Int) (loc ((line 10) (column 8)))))
       (loc ((line 10) (column 0))))))
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
    (sst
     ((Let (var _)
       (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 2)))))
       (loc ((line 2) (column 0))))))
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
    (sst
     ((Let (var _)
       (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 2)))))
       (loc ((line 2) (column 0))))))
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
    (sst
     ((Let (var dyn)
       (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 10)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "Mode annotation valid" =
  go
    {|
let dyn = 1 @ erased;;
let _ =
  dyn @ dynamic erased
;;|};
  [%expect {| (sst ()) |}]
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
    (sst
     ((Let (var dyn)
       (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 10)))))
       (loc ((line 2) (column 0))))
      (Let (var _) (bind (Var (id dyn) (ty Int) (loc ((line 4) (column 2)))))
       (loc ((line 3) (column 0))))))
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
    (sst
     ((Let (var dyn)
       (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 10)))))
       (loc ((line 2) (column 0))))))
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
    (sst
     ((Let (var _)
       (bind (Scalar (value (Bool false)) (ty Bool) (loc ((line 3) (column 3)))))
       (loc ((line 2) (column 0))))))
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
    (sst
     ((Let (var _)
       (bind (Scalar (value (Bool false)) (ty Bool) (loc ((line 3) (column 4)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "dynamic static erased" =
  go
    {|
let _ =
  (true @ static erased)
;;|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "dynamic erased" =
  go
    {|
let _ =
  (true @ dynamic erased)
;;|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "erased dynamic" =
  go
    {|
let _ =
  ((true @ erased) @ dynamic)
;;|};
  [%expect {| (sst ()) |}]
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
    (sst
     ((Let (var dyn)
       (bind (Scalar (value (Bool true)) (ty Bool) (loc ((line 2) (column 10)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Unop (op Not) (arg (Var (id dyn) (ty Bool) (loc ((line 4) (column 3)))))
         (ty Bool) (loc ((line 4) (column 2)))))
       (loc ((line 3) (column 0))))))
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
    (sst
     ((Let (var _)
       (bind (Scalar (value (Bool false)) (ty Bool) (loc ((line 4) (column 3)))))
       (loc ((line 3) (column 0))))))
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
    (sst
     ((Let (var _)
       (bind (Scalar (value (Bool true)) (ty Bool) (loc ((line 4) (column 9)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "Unop var erased" =
  go
    {|
let dyn = true @ erased;;
let _ =
  !(!dyn)
;;|};
  [%expect
    {|
    (sst
     ((Let (var _)
       (bind (Scalar (value (Bool true)) (ty Bool) (loc ((line 4) (column 5)))))
       (loc ((line 3) (column 0))))))
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
    (sst
     ((Let (var dyn)
       (bind (Scalar (value (Bool true)) (ty Bool) (loc ((line 2) (column 10)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Unop (op Not) (arg (Var (id dyn) (ty Bool) (loc ((line 4) (column 3)))))
         (ty Bool) (loc ((line 4) (column 2)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "Unop var dynamic" =
  go
    {|
let dyn = true @ dynamic;;
let x = !dyn @ erased;;|};
  [%expect
    {|
    (sst
     ((Let (var dyn)
       (bind (Scalar (value (Bool true)) (ty Bool) (loc ((line 2) (column 10)))))
       (loc ((line 2) (column 0))))))
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
    (sst
     ((Let (var _)
       (bind (Scalar (value (Int 3)) (ty Int) (loc ((line 3) (column 2)))))
       (loc ((line 2) (column 0))))))
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
    (sst
     ((Let (var _)
       (bind (Scalar (value (Int 3)) (ty Int) (loc ((line 3) (column 2)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "Binop erased dynamic" =
  go
    {|
let _ =
  1 + (2 @ dynamic)
;;|};
  [%expect
    {|
    (sst
     ((Let (var _)
       (bind (Scalar (value (Int 3)) (ty Int) (loc ((line 3) (column 2)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "Binop erased dynamic" =
  go
    {|
let _ =
  1 + (2 @ dynamic) + (3 @ erased)
;;|};
  [%expect
    {|
    (sst
     ((Let (var _)
       (bind (Scalar (value (Int 6)) (ty Int) (loc ((line 3) (column 2)))))
       (loc ((line 2) (column 0))))))
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
    (sst
     ((Let (var _)
       (bind (Scalar (value (Int 6)) (ty Int) (loc ((line 3) (column 2)))))
       (loc ((line 2) (column 0))))))
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
    (sst
     ((Let (var dyn)
       (bind (Scalar (value (Int 2)) (ty Int) (loc ((line 2) (column 10)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Binop (op Add)
         (lhs (Scalar (value (Int 1)) (ty Int) (loc ((line 4) (column 2)))))
         (rhs (Var (id dyn) (ty Int) (loc ((line 4) (column 6))))) (ty Int)
         (loc ((line 4) (column 4)))))
       (loc ((line 3) (column 0))))))
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
    (sst
     ((Let (var dyn)
       (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 10)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Binop (op Add) (lhs (Var (id dyn) (ty Int) (loc ((line 4) (column 2)))))
         (rhs (Scalar (value (Int 2)) (ty Int) (loc ((line 4) (column 8)))))
         (ty Int) (loc ((line 4) (column 6)))))
       (loc ((line 3) (column 0))))))
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
    (sst
     ((Let (var dyn1)
       (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 11)))))
       (loc ((line 2) (column 0))))
      (Let (var dyn2)
       (bind (Scalar (value (Int 2)) (ty Int) (loc ((line 3) (column 11)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Binop (op Add)
         (lhs (Var (id dyn1) (ty Int) (loc ((line 5) (column 2)))))
         (rhs (Var (id dyn2) (ty Int) (loc ((line 5) (column 9))))) (ty Int)
         (loc ((line 5) (column 7)))))
       (loc ((line 4) (column 0))))))
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
    (sst
     ((Let (var dyn2)
       (bind (Scalar (value (Int 2)) (ty Int) (loc ((line 3) (column 11)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Binop (op Add)
         (lhs (Scalar (value (Int 1)) (ty Int) (loc ((line 5) (column 2)))))
         (rhs (Var (id dyn2) (ty Int) (loc ((line 5) (column 9))))) (ty Int)
         (loc ((line 5) (column 7)))))
       (loc ((line 4) (column 0))))))
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
    (sst
     ((Let (var _)
       (bind (Scalar (value (Int 3)) (ty Int) (loc ((line 5) (column 2)))))
       (loc ((line 4) (column 0))))))
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
    (sst
     ((Let (var _)
       (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 15)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "If erased" =
  go
    {|
let _ =
  (if true then int else int) @ erased
;;|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "If erased" =
  go
    {|
let _ =
  if true @ erased then 1 else 2
;;|};
  [%expect
    {|
    (sst
     ((Let (var _)
       (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 24)))))
       (loc ((line 2) (column 0))))))
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
    (sst
     ((Let (var _)
       (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 4) (column 12)))))
       (loc ((line 3) (column 0))))))
    |}]
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
    (sst
     ((Let (var dyn)
       (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 10)))))
       (loc ((line 2) (column 0))))
      (Let (var _) (bind (Var (id dyn) (ty Int) (loc ((line 4) (column 15)))))
       (loc ((line 3) (column 0))))))
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
    (sst
     ((Let (var dyn)
       (bind (Scalar (value (Bool true)) (ty Bool) (loc ((line 2) (column 10)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (If (cond (Var (id dyn) (ty Bool) (loc ((line 4) (column 5)))))
         (then_ (Scalar (value (Int 1)) (ty Int) (loc ((line 4) (column 14)))))
         (else_ (Scalar (value (Int 2)) (ty Int) (loc ((line 4) (column 21)))))
         (ty Int) (loc ((line 4) (column 2)))))
       (loc ((line 3) (column 0))))))
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
    (sst
     ((Let (var x)
       (bind (Scalar (value (Bool true)) (ty Bool) (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 4) (column 2)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "if erased branch" =
  go
    {|
let _ = if 1==2 then unit else int;;|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "if erased branch" =
  go
    {|
let cond = true @ dynamic;;
let t = if cond then unit else int;;|};
  [%expect
    {|
    (sst
     ((Let (var cond)
       (bind (Scalar (value (Bool true)) (ty Bool) (loc ((line 2) (column 11)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "if erased branch" =
  go
    {|
let cond = true @ dynamic;;
let t = (if cond then false else cond) @ erased;;|};
  [%expect
    {|
    (sst
     ((Let (var cond)
       (bind (Scalar (value (Bool true)) (ty Bool) (loc ((line 2) (column 11)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "Let static" =
  go
    {|
let _ =
  let x = 1 in
  x
;;|};
  [%expect {|
    (sst
     ((Let (var _)
       (bind
        (Let (var x)
         (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 10)))))
         (rest (Var (id x) (ty Int) (loc ((line 4) (column 2))))) (ty Int)
         (loc ((line 3) (column 2)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "Let dynamic" =
  go
    {|
let dyn = 1 @ dynamic;;
let _ =
  let x = dyn in
  x
;;|};
  [%expect {|
    (sst
     ((Let (var dyn)
       (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 10)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Let (var x) (bind (Var (id dyn) (ty Int) (loc ((line 4) (column 10)))))
         (rest (Var (id x) (ty Int) (loc ((line 5) (column 2))))) (ty Int)
         (loc ((line 4) (column 2)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "Let dynamic" =
  go
    {|
let dyn = 1 @ erased;;
let _ =
  let x = dyn + 1 @ dynamic in
  x
;;|};
  [%expect {|
    (sst
     ((Let (var _)
       (bind
        (Let (var x)
         (bind (Scalar (value (Int 2)) (ty Int) (loc ((line 4) (column 10)))))
         (rest (Var (id x) (ty Int) (loc ((line 5) (column 2))))) (ty Int)
         (loc ((line 4) (column 2)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "Let erased" =
  go
    {|
let dyn = 1 @ erased;;
let _ =
  let x = dyn in
  x
;;|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "Let erased" =
  go
    {|
let _ =
  let x = 1 @ erased in
  x + 1
;;|};
  [%expect {|
    (sst
     ((Let (var _)
       (bind (Scalar (value (Int 2)) (ty Int) (loc ((line 4) (column 2)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "Let erased" =
  go
    {|
let _ =
  let x = 1 @ erased in
  let y = 1 @ dynamic in
  x + y
;;|};
  [%expect {|
    (sst
     ((Let (var _)
       (bind
        (Let (var y)
         (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 4) (column 10)))))
         (rest
          (Binop (op Add)
           (lhs (Scalar (value (Int 1)) (ty Int) (loc ((line 5) (column 2)))))
           (rhs (Var (id y) (ty Int) (loc ((line 5) (column 6))))) (ty Int)
           (loc ((line 5) (column 4)))))
         (ty Int) (loc ((line 4) (column 2)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "Let erased" =
  go
    {|
let _ =
  let x = 1 @ erased in
  let y = 1 @ erased in
  0 + (x + y)
;;|};
  [%expect {|
    (sst
     ((Let (var _)
       (bind (Scalar (value (Int 2)) (ty Int) (loc ((line 5) (column 2)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "Let erased" =
  go
    {|
let _ =
  let x = 1 in
  let y = 1 in
  0 + ((x + y) @ erased)
;;|};
  [%expect {|
    (sst
     ((Let (var _)
       (bind
        (Let (var x)
         (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 10)))))
         (rest
          (Let (var y)
           (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 4) (column 10)))))
           (rest (Scalar (value (Int 2)) (ty Int) (loc ((line 5) (column 2)))))
           (ty Int) (loc ((line 4) (column 2)))))
         (ty Int) (loc ((line 3) (column 2)))))
       (loc ((line 2) (column 0))))))
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
    (sst
     ((Let (var _)
       (bind
        (Lambda (arg x) (body (Var (id x) (ty Int) (loc ((line 3) (column 19)))))
         (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 3) (column 3)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "erased closure" =
  go
    {|
let _ =
  (fn (x : int) -> x) @ erased
;;|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "closure return type" =
  go
    {|
let _ =
  (fn (x : int) -> int)
;;|};
  [%expect {| (sst ()) |}]
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
    (sst
     ((Let (var y)
       (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Lambda (arg x)
         (body
          (Binop (op Add)
           (lhs (Var (id x) (ty Int) (loc ((line 4) (column 19)))))
           (rhs (Var (id y) (ty Int) (loc ((line 4) (column 23))))) (ty Int)
           (loc ((line 4) (column 21)))))
         (fvs ((y Int))) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 4) (column 3)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (erased x : int) -> x;;
let _ = f 0;;
|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = (fn (erased x : int) -> 1) @ erased;;
let _ = f 0;;
|};
  [%expect
    {|
    (sst
     ((Let (var _)
       (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 32)))))
       (loc ((line 3) (column 0))))))
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
    (sst
     ((Let (var f)
       (bind
        (Lambda (arg x)
         (body (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 31)))))
         (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id f) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 3) (column 8)))))
         (arg (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 10)))))
         (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (static erased g : int -> erased int) -> let _ = g 1 in 2;;
let _ = f (fn (x : int) -> 0 @ erased);;
|};
  [%expect {|
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Closure 1)) (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
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
    (sst
     ((Let (var f)
       (bind
        (Lambda (arg x)
         (body (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 38)))))
         (fvs ())
         (ty (Arrow (arg_ty (Arrow (arg_ty Int) (ret_ty Int))) (ret_ty Int)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id f)
           (ty (Arrow (arg_ty (Arrow (arg_ty Int) (ret_ty Int))) (ret_ty Int)))
           (loc ((line 3) (column 8)))))
         (arg
          (Lambda (arg x)
           (body
            (Binop (op Add)
             (lhs (Var (id x) (ty Int) (loc ((line 3) (column 28)))))
             (rhs (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 32)))))
             (ty Int) (loc ((line 3) (column 30)))))
           (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 3) (column 35)))))
         (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = (fn (erased x : int -> int) -> 1) @ erased;;
let _ = f ((fn (x : int) -> x + 1) @ erased);;
|};
  [%expect
    {|
    (sst
     ((Let (var _)
       (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 39)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "static erased closure arg" =
  go
    {|
let _ =
  (fn (static erased x : int) -> x)
;;|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "dependent closure arg" =
  go
    {|
let _ =
  (fn (x : type) -> x)
;;|};
  [%expect
    {|
    (sst
     ((Let (var _)
       (bind
        (Lambda (arg x)
         (body (Var (id x) (ty Unit) (loc ((line 3) (column 20))))) (fvs ())
         (ty (Arrow (arg_ty Unit) (ret_ty Unit))) (loc ((line 3) (column 3)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "dependent closure arg" =
  go
    {|
fun f (x : type) : type = x;;|};
  [%expect
    {|
    (sst
     ((Fun
       (funs
        ((Mono (var f) (arg x)
          (body (Var (id x) (ty Unit) (loc ((line 2) (column 26))))) (fvs ())
          (ty (Arrow (arg_ty Unit) (ret_ty Unit))) (loc ((line 2) (column 4))))))
       (fvs ()) (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "dependent closure arg" =
  go
    {|
fun f (static x : int) : static erased type = int;;|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "dependent closure arg" =
  go
    {|
let _ =
  (fn (static x : type) -> x)
;;|};
  [%expect
    {|
    (sst
     ((Let (var _)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 3) (column 3)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "dependent closure arg" =
  go
    {|
let _ =
  (fn (erased x : type) -> x)
;;|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "closure erased" =
  go
    {|
let x = (fn (x : int) -> x) @ erased;;
|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "closure erased" =
  go
    {|
let x = (fn (erased x : int) -> x) 0;;
|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "closure erased" =
  go
    {|
let x = (fn (erased x : int) -> 1) 0;;
|};
  [%expect
    {|
    (sst
     ((Let (var x)
       (bind
        (Let (var x)
         (bind (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 35)))))
         (rest (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 32)))))
         (ty Int) (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "closure erased" =
  go
    {|
let x = ((fn (erased x : int) -> 1) @ erased) 0;;
|};
  [%expect
    {|
    (sst
     ((Let (var x)
       (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 33)))))
       (loc ((line 2) (column 0))))))
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
    (sst
     ((Let (var f)
       (bind
        (Lambda (arg x)
         (body (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 32)))))
         (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 2) (column 9)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 3) (column 9)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Var (id f) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 4) (column 21)))))
       (loc ((line 4) (column 0))))))
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
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 9)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 3) (column 9)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind (Var (id f) (ty (Pack <opaque>)) (loc ((line 4) (column 21)))))
       (loc ((line 4) (column 0))))))
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
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 9)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 3) (column 9)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind (Var (id f) (ty (Pack <opaque>)) (loc ((line 4) (column 21)))))
       (loc ((line 4) (column 0))))))
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
    (sst
     ((Let (var c)
       (bind
        (Lambda (arg _)
         (body
          (Scalar (value (Bool true)) (ty Bool) (loc ((line 2) (column 25)))))
         (fvs ()) (ty (Arrow (arg_ty Unit) (ret_ty Bool)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var f)
       (bind
        (Lambda (arg x)
         (body (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 25)))))
         (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 3) (column 9)))))
       (loc ((line 3) (column 0))))
      (Let (var g)
       (bind
        (Lambda (arg x)
         (body (Scalar (value (Int 2)) (ty Int) (loc ((line 4) (column 32)))))
         (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 4) (column 9)))))
       (loc ((line 4) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (If
           (cond
            (Apply
             (fn
              (Var (id c) (ty (Arrow (arg_ty Unit) (ret_ty Bool)))
               (loc ((line 5) (column 12)))))
             (arg (Scalar (value Unit) (ty Unit) (loc ((line 5) (column 14)))))
             (ty Bool) (loc ((line 5) (column 12)))))
           (then_
            (Var (id f) (ty (Arrow (arg_ty Int) (ret_ty Int)))
             (loc ((line 5) (column 22)))))
           (else_
            (Var (id g) (ty (Arrow (arg_ty Int) (ret_ty Int)))
             (loc ((line 5) (column 29)))))
           (ty (Arrow (arg_ty Int) (ret_ty Int))) (loc ((line 5) (column 9)))))
         (arg (Scalar (value (Int 0)) (ty Int) (loc ((line 5) (column 32)))))
         (ty Int) (loc ((line 5) (column 8)))))
       (loc ((line 5) (column 0))))))
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
    (sst
     ((Let (var f)
       (bind
        (Lambda (arg x)
         (body (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 24)))))
         (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Lambda (arg f)
         (body
          (Apply
           (fn
            (Var (id f) (ty (Arrow (arg_ty Int) (ret_ty Int)))
             (loc ((line 3) (column 31)))))
           (arg (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 33)))))
           (ty Int) (loc ((line 3) (column 31)))))
         (fvs ())
         (ty (Arrow (arg_ty (Arrow (arg_ty Int) (ret_ty Int))) (ret_ty Int)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id g)
           (ty (Arrow (arg_ty (Arrow (arg_ty Int) (ret_ty Int))) (ret_ty Int)))
           (loc ((line 4) (column 8)))))
         (arg
          (Var (id f) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 4) (column 10)))))
         (ty Int) (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
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
    (sst
     ((Let (var f)
       (bind
        (Lambda (arg x)
         (body (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 24)))))
         (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id g) (ty (Pack <opaque>)) (loc ((line 4) (column 8)))))
         (arg (Closure 1)) (ty Int) (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
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
    (sst
     ((Let (var g)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id g) (ty (Pack <opaque>)) (loc ((line 4) (column 8)))))
         (arg (Closure 1)) (ty Int) (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
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
    (sst
     ((Let (var f1)
       (bind
        (Lambda (arg x)
         (body (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 32)))))
         (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 2) (column 9)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Lambda (arg f2)
         (body
          (Apply
           (fn
            (Var (id f2) (ty (Arrow (arg_ty Int) (ret_ty Int)))
             (loc ((line 3) (column 32)))))
           (arg (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 35)))))
           (ty Int) (loc ((line 3) (column 32)))))
         (fvs ())
         (ty (Arrow (arg_ty (Arrow (arg_ty Int) (ret_ty Int))) (ret_ty Int)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id g)
           (ty (Arrow (arg_ty (Arrow (arg_ty Int) (ret_ty Int))) (ret_ty Int)))
           (loc ((line 4) (column 8)))))
         (arg
          (Var (id f1) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 4) (column 10)))))
         (ty Int) (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "inlined closure nest" =
  go
    {|
let f1 = fn (erased x : int) -> 1;;
let g = fn (static f2 : erased int -> int) -> f2 0;;
let _ = g f1;;
|};
  [%expect {|
    (sst
     ((Let (var f1)
       (bind
        (Lambda (arg x)
         (body (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 32)))))
         (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 2) (column 9)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id g) (ty (Pack <opaque>)) (loc ((line 4) (column 8)))))
         (arg (Closure 1)) (ty Int) (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "inlined closure nest" =
  go
    {|
let f1 = (fn (erased x : int) -> 1) @ erased;;
let g = fn (static erased f2 : erased int -> int) -> f2 0;;
let _ = g f1;;
|};
  [%expect
    {|
    (sst
     ((Let (var g)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id g) (ty (Pack <opaque>)) (loc ((line 4) (column 8)))))
         (arg (Closure 1)) (ty Int) (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
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
    (sst
     ((Let (var f1)
       (bind
        (Lambda (arg x)
         (body (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 32)))))
         (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 2) (column 9)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id g) (ty (Pack <opaque>)) (loc ((line 4) (column 8)))))
         (arg (Closure 1)) (ty Int) (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "closure static" =
  go
    {|
let x = (fn (static x : int) -> x) 0;;
|};
  [%expect {|
    (sst
     ((Let (var x)
       (bind
        (Let (var x)
         (bind (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 9)))))
         (rest (Var (id x) (ty Int) (loc ((line 2) (column 32))))) (ty Int)
         (loc ((line 2) (column 9)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "closure static erased" =
  go
    {|
let x = (fn (static erased x : int) -> 1) 0;;
|};
  [%expect
    {|
    (sst
     ((Let (var x)
       (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 39)))))
       (loc ((line 2) (column 0))))))
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
    (sst
     ((Let (var _)
       (bind (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
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
    (sst
     ((Let (var _)
       (bind (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
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
    (sst
     ((Let (var _)
       (bind
        (Let (var x)
         (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 22)))))
         (rest (Var (id x) (ty Int) (loc ((line 3) (column 19))))) (ty Int)
         (loc ((line 3) (column 2)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "Apply static erased fn static arg" =
  go
    {|
let _ =
  (fn (static erased x : int) -> x) 1
;;|};
  [%expect {| (sst ()) |}]
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
    (sst
     ((Let (var dyn)
       (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 10)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Let (var x) (bind (Var (id dyn) (ty Int) (loc ((line 4) (column 22)))))
         (rest (Var (id x) (ty Int) (loc ((line 4) (column 19))))) (ty Int)
         (loc ((line 4) (column 2)))))
       (loc ((line 3) (column 0))))))
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
    (sst
     ((Let (var dyn)
       (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 10)))))
       (loc ((line 2) (column 0))))
      (Let (var y)
       (bind
        (Let (var x)
         (bind
          (Binop (op Sub)
           (lhs (Var (id dyn) (ty Int) (loc ((line 4) (column 30)))))
           (rhs (Scalar (value (Int 1)) (ty Int) (loc ((line 4) (column 34)))))
           (ty Int) (loc ((line 4) (column 33)))))
         (rest (Scalar (value (Int 5)) (ty Int) (loc ((line 4) (column 26)))))
         (ty Int) (loc ((line 4) (column 2)))))
       (loc ((line 3) (column 0))))
      (Let (var _) (bind (Var (id y) (ty Int) (loc ((line 6) (column 8)))))
       (loc ((line 6) (column 0))))))
    |}]
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
    (sst
     ((Let (var y)
       (bind
        (Let (var x)
         (bind
          (Let (var x)
           (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 4) (column 32)))))
           (rest (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 33)))))
           (ty Int) (loc ((line 4) (column 30)))))
         (rest (Scalar (value (Int 5)) (ty Int) (loc ((line 4) (column 26)))))
         (ty Int) (loc ((line 4) (column 2)))))
       (loc ((line 3) (column 0))))
      (Let (var _) (bind (Var (id y) (ty Int) (loc ((line 6) (column 8)))))
       (loc ((line 6) (column 0))))))
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
    (sst
     ((Let (var dyn)
       (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 10)))))
       (loc ((line 2) (column 0))))
      (Let (var y)
       (bind
        (Let (var x)
         (bind
          (Binop (op Sub)
           (lhs (Var (id dyn) (ty Int) (loc ((line 4) (column 30)))))
           (rhs (Scalar (value (Int 1)) (ty Int) (loc ((line 4) (column 34)))))
           (ty Int) (loc ((line 4) (column 33)))))
         (rest (Scalar (value (Int 5)) (ty Int) (loc ((line 4) (column 26)))))
         (ty Int) (loc ((line 4) (column 2)))))
       (loc ((line 3) (column 0))))
      (Let (var _) (bind (Var (id y) (ty Int) (loc ((line 6) (column 8)))))
       (loc ((line 6) (column 0))))))
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
    (sst
     ((Let (var dyn_fn)
       (bind
        (Lambda (arg x) (body (Var (id x) (ty Int) (loc ((line 2) (column 30)))))
         (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 2) (column 14)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id dyn_fn) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 4) (column 2)))))
         (arg (Scalar (value (Int 1)) (ty Int) (loc ((line 4) (column 9)))))
         (ty Int) (loc ((line 4) (column 2)))))
       (loc ((line 3) (column 0))))))
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
    (sst
     ((Let (var dyn_fn)
       (bind
        (Lambda (arg x) (body (Var (id x) (ty Int) (loc ((line 2) (column 30)))))
         (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 2) (column 14)))))
       (loc ((line 2) (column 0))))
      (Let (var dyn_arg)
       (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 14)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id dyn_fn) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 5) (column 2)))))
         (arg (Var (id dyn_arg) (ty Int) (loc ((line 5) (column 9))))) (ty Int)
         (loc ((line 5) (column 2)))))
       (loc ((line 4) (column 0))))))
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
    (sst
     ((Let (var _)
       (bind
        (Lambda (arg x) (body (Var (id x) (ty Int) (loc ((line 3) (column 18)))))
         (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 3) (column 2)))))
       (loc ((line 2) (column 0))))))
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
    (sst
     ((Let (var _)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 3) (column 2)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "Lambda erased arg" =
  go
    {|
let _ =
  fn (erased x : int) -> x
;;|};
  [%expect {| (sst ()) |}]
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
    (sst
     ((Let (var x)
       (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Lambda (arg y) (body (Var (id x) (ty Int) (loc ((line 4) (column 18)))))
         (fvs ((x Int))) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 4) (column 2)))))
       (loc ((line 3) (column 0))))))
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
    (sst
     ((Let (var x)
       (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Lambda (arg y) (body (Var (id x) (ty Int) (loc ((line 4) (column 18)))))
         (fvs ((x Int))) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 4) (column 2)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "Lambda capturing erased var" =
  go
    {|
let x = 1 @ static erased;;
let _ =
  fn (y : int) -> x
;;|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "Lambda capturing erased var" =
  go
    {|
let x = 1 @ static erased;;
let _ =
  (fn (y : int) -> x) 0
;;|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "Lambda capturing type" =
  go
    {|
let f = fn (static _ : unit) -> int;;
let g = fn (x : f ()) -> x + 1;;|};
  [%expect
    {|
    (sst
     ((Let (var g)
       (bind
        (Lambda (arg x)
         (body
          (Binop (op Add)
           (lhs (Var (id x) (ty Int) (loc ((line 3) (column 25)))))
           (rhs (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 29)))))
           (ty Int) (loc ((line 3) (column 27)))))
         (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "Lambda take type" =
  go
    {|
let f = fn (erased ty : type) -> ty;;
let _ = f int;;
|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "Lambda take type" =
  go
    {|
let f = fn (static erased ty : type) -> ty;;
let _ = 0 : f int;;
|};
  [%expect
    {|
    (sst
     ((Let (var _)
       (bind (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "mono fn" =
  go
    {|
let x = fn (static erased x : type) -> x;;
let y = x int;;
|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "mono fn" =
  go
    {|
let x = fn (static x : type) -> x;;
let y = x @ dynamic;;
|};
  [%expect
    {|
    (sst
     ((Let (var x)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var y)
       (bind (Var (id x) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "mono fn" =
  go
    {|
let x = fn (erased x : type) -> x;;
let y = x @ dynamic;;
|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "mono fn" =
  go
    {|
let x = fn (static x : type) -> x;;
let y = x @ unerased;;
|};
  [%expect
    {|
    (sst
     ((Let (var x)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var y)
       (bind (Var (id x) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "mono fn" =
  go
    {|
let x = fn (static erased x : type) -> x;;
let y = (x int) @ dynamic;;
|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "Dependent lambda" =
  go
    {|
let f = fn (static erased ty : type) -> fn (x : ty) -> x;;
|};
  [%expect
    {|
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "static erased lambda" =
  go
    {|
let f = fn (static x : int) -> fn (_ : unit) -> x;;
let g = (f 1 ()) @ unerased;;
|};
  [%expect {|
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Apply
         (fn
          (Symbol
           (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 9)))))
           (arg (Int 1)) (ty (Arrow (arg_ty Unit) (ret_ty Int)))
           (loc ((line 3) (column 9)))))
         (arg (Scalar (value Unit) (ty Unit) (loc ((line 3) (column 13)))))
         (ty Int) (loc ((line 3) (column 9)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "lift universal type" =
  go
    {|
let f = fn (static ty : type) -> ty @ erased;;
|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "lift universal int" =
  go
    {|
let f = fn (static x : int) -> x @ erased;;
let _ = f 0;;
|};
  [%expect {| (sst ()) |}]
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
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg IntT) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id g) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 4) (column 8)))))
         (arg (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 10)))))
         (ty Int) (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))
      (Let (var g)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 5) (column 8)))))
         (arg BoolT) (ty (Arrow (arg_ty Bool) (ret_ty Bool)))
         (loc ((line 5) (column 8)))))
       (loc ((line 5) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id g) (ty (Arrow (arg_ty Bool) (ret_ty Bool)))
           (loc ((line 6) (column 8)))))
         (arg
          (Scalar (value (Bool true)) (ty Bool) (loc ((line 6) (column 10)))))
         (ty Bool) (loc ((line 6) (column 8)))))
       (loc ((line 6) (column 0))))))
    |}]
;;

let%expect_test "Dependent lambda" =
  go
    {|
let f = fn (static g : int -> int) -> g 0;;
let _ = f (fn (x : int) -> x + 1);;
|};
  [%expect {|
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Closure 1)) (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
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
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg Unit) (ty Unit) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "dependent bool" =
  go
    {|
let f = fn (static x : bool) -> !x;;
let _ = f true;;
|};
  [%expect {|
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Bool true)) (ty Bool) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "dependent int" =
  go
    {|
let f = fn (static x : int) -> -x;;
let _ = f 1;;
|};
  [%expect {|
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Int 1)) (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "dependent type" =
  go
    {|
let f = fn (static erased t : type) -> fn (x : t) -> if true then x else x;;
|};
  [%expect
    {|
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))))
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
    (sst
     ((Let (var f)
       (bind
        (Lambda (arg g)
         (body
          (Apply
           (fn
            (Var (id g) (ty (Arrow (arg_ty Int) (ret_ty Int)))
             (loc ((line 2) (column 31)))))
           (arg (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 33)))))
           (ty Int) (loc ((line 2) (column 31)))))
         (fvs ())
         (ty (Arrow (arg_ty (Arrow (arg_ty Int) (ret_ty Int))) (ret_ty Int)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id f)
           (ty (Arrow (arg_ty (Arrow (arg_ty Int) (ret_ty Int))) (ret_ty Int)))
           (loc ((line 3) (column 8)))))
         (arg
          (Lambda (arg x)
           (body (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 27)))))
           (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 3) (column 11)))))
         (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "arrow typechecking" =
  go
    {|
let f = fn (static g : static int -> int) -> g 0;;
let _ = f (fn (static x : int) -> 0);;
|};
  [%expect {|
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Closure 4)) (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "arrow typechecking" =
  go
    {|
let f = fn (static g : static int -> int) -> g 0;;
let _ = f (fn (x : int) -> 0);;
|};
  [%expect {|
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Closure 3)) (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (static x : int) -> x else fn (x : int) -> x;;
|};
  [%expect
    {|
    (sst
     ((Let (var _)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 21)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (static x : int) -> x else fn (erased x : int) -> x;;
|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (static erased x : int) -> x else fn (x : int) -> x;;
|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (erased x : int) -> x else fn (x : int) -> x;;
|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (x : int) -> x else fn (static x : int) -> x;;
|};
  [%expect
    {|
    (sst
     ((Let (var _)
       (bind
        (Lambda (arg x) (body (Var (id x) (ty Int) (loc ((line 2) (column 37)))))
         (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 2) (column 21)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (x : int) -> x else fn (erased x : int) -> x;;
|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (erased x : int) -> x else fn (static x : int) -> x;;
|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (x : int) -> x else fn (static erased x : int) -> x;;
|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "return erased" =
  go
    {|
let f = fn (x : int) -> 0 @ erased;;
let _ = f 1;;
|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "pi typechecking" =
  go
    {|
  let f = fn (static g : erased int -> int) -> g 0;;
  let _ = f (fn (erased x : int) -> 0);;
  |};
  [%expect {|
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 10)))))
       (loc ((line 2) (column 2))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 10)))))
         (arg (Closure 1)) (ty Int) (loc ((line 3) (column 10)))))
       (loc ((line 3) (column 2))))))
    |}]
;;

let%expect_test "arrow-pi typechecking" =
  go
    {|
let f = fn (static g : static int -> int) -> g 1;;
let _ = f (fn (x : int) -> 0);;
|};
  [%expect {|
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Closure 3)) (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
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
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Closure 4)) (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "dependent lambda" =
  go
    {|
let f = fn (static g : static erased type -> int -> int) -> g int;;
let _ = f (fn (static erased t : type) -> fn (x : int) -> x);;
|};
  [%expect {|
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Closure 4)) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
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
    (sst
     ((Let (var id)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 9)))))
       (loc ((line 2) (column 0))))
      (Let (var x)
       (bind
        (Apply
         (fn
          (Symbol
           (fn (Var (id id) (ty (Pack <opaque>)) (loc ((line 3) (column 9)))))
           (arg IntT) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 3) (column 9)))))
         (arg (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 18)))))
         (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var y)
       (bind
        (Apply
         (fn
          (Symbol
           (fn (Var (id id) (ty (Pack <opaque>)) (loc ((line 4) (column 9)))))
           (arg BoolT) (ty (Arrow (arg_ty Bool) (ret_ty Bool)))
           (loc ((line 4) (column 9)))))
         (arg
          (Scalar (value (Bool true)) (ty Bool) (loc ((line 4) (column 19)))))
         (ty Bool) (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
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
    (sst
     ((Let (var x)
       (bind
        (Let (var x)
         (bind (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 18)))))
         (rest (Var (id x) (ty Int) (loc ((line 2) (column 55))))) (ty Int)
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var y)
       (bind
        (Let (var x)
         (bind
          (Scalar (value (Bool true)) (ty Bool) (loc ((line 4) (column 19)))))
         (rest (Var (id x) (ty Bool) (loc ((line 2) (column 55))))) (ty Bool)
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
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
    (sst
     ((Let (var apply_int)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 3) (column 16)))))
       (loc ((line 3) (column 0))))))
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
    (sst
     ((Let (var apply_int)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 3) (column 16)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "dependent arrow" =
  go
    {|
let apply_int = fn (static f : static erased type \ t -> t -> t) -> f int;;
let _ = apply_int (fn (static erased t : type) -> fn (x : t) -> x);;
|};
  [%expect {|
    (sst
     ((Let (var apply_int)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 16)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn
          (Var (id apply_int) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Closure 4)) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
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
  [%expect {|
    (sst
     ((Let (var apply)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 12)))))
       (loc ((line 2) (column 0))))
      (Let (var f)
       (bind
        (Symbol
         (fn (Var (id apply) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Closure 7)) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var g)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 4) (column 8)))))
         (arg IntT) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))
      (Let (var h)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 5) (column 8)))))
         (arg BoolT) (ty (Arrow (arg_ty Bool) (ret_ty Bool)))
         (loc ((line 5) (column 8)))))
       (loc ((line 5) (column 0))))))
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
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Int 0)) (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 4) (column 8)))))
         (arg (Int 1)) (ty Bool) (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
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
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 4) (column 21)))))
         (arg (Int 0)) (ty Int) (loc ((line 4) (column 21)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "Fun recursive dynamic arg" =
  go
    {|
fun f (x : int) : int = f x;;
|};
  [%expect
    {|
    (sst
     ((Fun
       (funs
        ((Mono (var f) (arg x)
          (body
           (Apply
            (fn
             (Var (id f) (ty (Arrow (arg_ty Int) (ret_ty Int)))
              (loc ((line 2) (column 24)))))
            (arg (Var (id x) (ty Int) (loc ((line 2) (column 26))))) (ty Int)
            (loc ((line 2) (column 24)))))
          (fvs ((f (Arrow (arg_ty Int) (ret_ty Int)))))
          (ty (Arrow (arg_ty Int) (ret_ty Int))) (loc ((line 2) (column 4))))))
       (fvs ((f (Arrow (arg_ty Int) (ret_ty Int))))) (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "Fun erased arg" =
  go
    {|
fun f (erased x : int) : erased int = x;;
|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "Fun return static" =
  go
    {|
fun f (x : int) : int = 1;;
|};
  [%expect
    {|
    (sst
     ((Fun
       (funs
        ((Mono (var f) (arg x)
          (body (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 24)))))
          (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
          (loc ((line 2) (column 4))))))
       (fvs ()) (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "Fun return erased" =
  go
    {|
fun f (x : int) : erased int = 1 @ erased;;
|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "mono fun" =
  go
    {|
fun x (static x : int) : int = x;;
let y = x 0;;
|};
  [%expect
    {|
    (sst
     ((Fun
       (funs
        ((Pack (var x) (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
          (loc ((line 2) (column 4))))))
       (fvs ()) (loc ((line 2) (column 0))))
      (Let (var y)
       (bind
        (Symbol
         (fn (Var (id x) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Int 0)) (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
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
    (sst
     ((Let (var y)
       (bind (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "types are erased" =
  go
    {|
fun f (static _ : unit) : static erased type = int;;
let y = (f () @ dynamic);;
|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "types are erased" =
  go
    {|
fun f (static _ : unit) : static erased type = int;;
let y = 5 : f ();;
|};
  [%expect
    {|
    (sst
     ((Let (var y)
       (bind (Scalar (value (Int 5)) (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "dependent fun " =
  go
    {|
fun id (static erased t : type) : t -> t = fn (x : t) -> x;;
let i = id int;;
|};
  [%expect
    {|
    (sst
     ((Fun
       (funs
        ((Pack (var id) (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
          (loc ((line 2) (column 4))))))
       (fvs ()) (loc ((line 2) (column 0))))
      (Let (var i)
       (bind
        (Symbol
         (fn (Var (id id) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg IntT) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "erased fun " =
  go
    {|
fun id (erased x : int) : erased int = x;;
let _ = id 0;;
|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "dependent fun erased" =
  go
    {|
fun id (static erased t : type) : t -> t = fn (x : t) -> x;;
let x = (id int) (0 @ dynamic);;
|};
  [%expect
    {|
    (sst
     ((Fun
       (funs
        ((Pack (var id) (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
          (loc ((line 2) (column 4))))))
       (fvs ()) (loc ((line 2) (column 0))))
      (Let (var x)
       (bind
        (Apply
         (fn
          (Symbol
           (fn (Var (id id) (ty (Pack <opaque>)) (loc ((line 3) (column 9)))))
           (arg IntT) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 3) (column 9)))))
         (arg (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 18)))))
         (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
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
    (sst
     ((Fun
       (funs
        ((Mono (var id) (arg _)
          (body
           (Lambda (arg x)
            (body (Var (id x) (ty Int) (loc ((line 3) (column 44))))) (fvs ())
            (ty (Arrow (arg_ty Int) (ret_ty Int))) (loc ((line 3) (column 28)))))
          (fvs ())
          (ty (Arrow (arg_ty Unit) (ret_ty (Arrow (arg_ty Int) (ret_ty Int)))))
          (loc ((line 3) (column 4))))))
       (fvs ()) (loc ((line 3) (column 0))))
      (Let (var x)
       (bind
        (Apply
         (fn
          (Apply
           (fn
            (Var (id id)
             (ty
              (Arrow (arg_ty Unit) (ret_ty (Arrow (arg_ty Int) (ret_ty Int)))))
             (loc ((line 4) (column 8)))))
           (arg (Scalar (value Unit) (ty Unit) (loc ((line 4) (column 11)))))
           (ty (Arrow (arg_ty Int) (ret_ty Int))) (loc ((line 4) (column 8)))))
         (arg (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 14)))))
         (ty Int) (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
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
    (sst
     ((Fun
       (funs
        ((Pack (var id1) (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
          (loc ((line 2) (column 4))))))
       (fvs ()) (loc ((line 2) (column 0))))
      (Fun
       (funs
        ((Pack (var id2) (pack <opaque>) (fvs ((id1 (Pack <opaque>))))
          (ty (Pack <opaque>)) (loc ((line 3) (column 4))))))
       (fvs ((id1 (Pack <opaque>)))) (loc ((line 3) (column 0))))
      (Let (var x)
       (bind
        (Apply
         (fn
          (Symbol
           (fn (Var (id id2) (ty (Pack <opaque>)) (loc ((line 4) (column 8)))))
           (arg IntT) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 4) (column 8)))))
         (arg (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 17)))))
         (ty Int) (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
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
    (sst
     ((Fun
       (funs
        ((Mono (var a) (arg _)
          (body (Scalar (value Unit) (ty Unit) (loc ((line 2) (column 26)))))
          (fvs ()) (ty (Arrow (arg_ty Unit) (ret_ty Unit)))
          (loc ((line 2) (column 4))))))
       (fvs ()) (loc ((line 2) (column 0))))
      (Fun
       (funs
        ((Mono (var b) (arg _)
          (body
           (Lambda (arg _)
            (body (Scalar (value Unit) (ty Unit) (loc ((line 3) (column 51)))))
            (fvs ()) (ty (Arrow (arg_ty Unit) (ret_ty Unit)))
            (loc ((line 3) (column 34)))))
          (fvs ())
          (ty (Arrow (arg_ty Unit) (ret_ty (Arrow (arg_ty Unit) (ret_ty Unit)))))
          (loc ((line 3) (column 4))))))
       (fvs ()) (loc ((line 3) (column 0))))
      (Let (var x)
       (bind
        (Var (id b)
         (ty (Arrow (arg_ty Unit) (ret_ty (Arrow (arg_ty Unit) (ret_ty Unit)))))
         (loc ((line 4) (column 36)))))
       (loc ((line 4) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Apply
           (fn
            (Var (id x)
             (ty
              (Arrow (arg_ty Unit) (ret_ty (Arrow (arg_ty Unit) (ret_ty Unit)))))
             (loc ((line 5) (column 8)))))
           (arg (Scalar (value Unit) (ty Unit) (loc ((line 5) (column 10)))))
           (ty (Arrow (arg_ty Unit) (ret_ty Unit))) (loc ((line 5) (column 8)))))
         (arg (Scalar (value Unit) (ty Unit) (loc ((line 5) (column 13)))))
         (ty Unit) (loc ((line 5) (column 8)))))
       (loc ((line 5) (column 0))))))
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
    (sst
     ((Fun
       (funs
        ((Mono (var x) (arg _)
          (body
           (Lambda (arg _)
            (body (Scalar (value Unit) (ty Unit) (loc ((line 2) (column 51)))))
            (fvs ()) (ty (Arrow (arg_ty Unit) (ret_ty Unit)))
            (loc ((line 2) (column 34)))))
          (fvs ())
          (ty (Arrow (arg_ty Unit) (ret_ty (Arrow (arg_ty Unit) (ret_ty Unit)))))
          (loc ((line 2) (column 4))))))
       (fvs ()) (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Apply
           (fn
            (Var (id x)
             (ty
              (Arrow (arg_ty Unit) (ret_ty (Arrow (arg_ty Unit) (ret_ty Unit)))))
             (loc ((line 3) (column 8)))))
           (arg (Scalar (value Unit) (ty Unit) (loc ((line 3) (column 10)))))
           (ty (Arrow (arg_ty Unit) (ret_ty Unit))) (loc ((line 3) (column 8)))))
         (arg (Scalar (value Unit) (ty Unit) (loc ((line 3) (column 13)))))
         (ty Unit) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
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
    (sst
     ((Fun
       (funs
        ((Mono (var x) (arg f)
          (body
           (Apply
            (fn
             (Var (id f) (ty (Arrow (arg_ty Unit) (ret_ty Int)))
              (loc ((line 2) (column 32)))))
            (arg (Scalar (value Unit) (ty Unit) (loc ((line 2) (column 34)))))
            (ty Int) (loc ((line 2) (column 32)))))
          (fvs ())
          (ty (Arrow (arg_ty (Arrow (arg_ty Unit) (ret_ty Int))) (ret_ty Int)))
          (loc ((line 2) (column 4))))))
       (fvs ()) (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id x)
           (ty (Arrow (arg_ty (Arrow (arg_ty Unit) (ret_ty Int))) (ret_ty Int)))
           (loc ((line 3) (column 8)))))
         (arg
          (Lambda (arg _)
           (body (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 28)))))
           (fvs ()) (ty (Arrow (arg_ty Unit) (ret_ty Int)))
           (loc ((line 3) (column 11)))))
         (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "dependent if unify" =
  go
    {|
let f = fn (static erased x : int) -> if static x == 0 then 1 else true;;
let g = fn (static erased x : int) -> if static x == 0 then 0 else false;;
let h = if true then f else g;;
let _ = h 0;;
let _ = h 1;;
|};
  [%expect
    {|
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var h)
       (bind (Var (id f) (ty (Pack <opaque>)) (loc ((line 4) (column 21)))))
       (loc ((line 4) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id h) (ty (Pack <opaque>)) (loc ((line 5) (column 8)))))
         (arg (Int 0)) (ty Int) (loc ((line 5) (column 8)))))
       (loc ((line 5) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id h) (ty (Pack <opaque>)) (loc ((line 6) (column 8)))))
         (arg (Int 1)) (ty Bool) (loc ((line 6) (column 8)))))
       (loc ((line 6) (column 0))))))
    |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static erased x : int) -> (if static x == 0 then 1 else true) : (if x == 0 then int else bool);;
let _ = f 0;;
let _ = f 1;;
|};
  [%expect
    {|
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Int 0)) (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 4) (column 8)))))
         (arg (Int 1)) (ty Bool) (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static erased x : int) -> (if static x == 1 + 1 then 1 else true) : (if x == 2 then int else bool);;
let _ = f 1;;
let _ = f 2;;
|};
  [%expect
    {|
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Int 1)) (ty Bool) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 4) (column 8)))))
         (arg (Int 2)) (ty Int) (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static erased x : int) -> (if static x == (if true then x else 0) then 1 else true) : (if x == x then int else bool);;
let _ = f 0;;
|};
  [%expect
    {|
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Int 0)) (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static erased x : int) -> (if static x == x + 1 then 1 else true) : (if x == x + 1 then int else bool);;
let _ = f 0;;
|};
  [%expect
    {|
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Int 0)) (ty Bool) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static erased x : int) -> (if static x == (if x == 1 then x else 0) then 1 else true) : (if x == (if x == 1 then x else 0) then int else bool);;
let _ = f 0;;
let _ = f 1;;
let _ = f 2;;
|};
  [%expect
    {|
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Int 0)) (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 4) (column 8)))))
         (arg (Int 1)) (ty Int) (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 5) (column 8)))))
         (arg (Int 2)) (ty Bool) (loc ((line 5) (column 8)))))
       (loc ((line 5) (column 0))))))
    |}]
;;

let%expect_test "dependent abstraction" =
  go
    {|
let choose = fn (static f : static int \ x -> if x == 0 then int else bool) -> f 0;;
let _ = choose (fn (static x : int) -> if static x == 0 then 0 else true);;
|};
  [%expect {|
    (sst
     ((Let (var choose)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 13)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id choose) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Closure 4)) (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "weaken mode: static unerased -> static erased (literal substitution)" =
  go
    {|
let _ = 1 @ erased;;
|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "weaken mode: dynamic unerased -> dynamic erased (erased marker)" =
  go
    {|
let x = 1 @ dynamic;;
let _ = x @ erased;;
|};
  [%expect
    {|
    (sst
     ((Let (var x)
       (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "weaken mode: static -> dynamic (staticity only)" =
  go
    {|
let _ = (fn (x : int) -> x) @ dynamic;;
|};
  [%expect
    {|
    (sst
     ((Let (var _)
       (bind
        (Lambda (arg x) (body (Var (id x) (ty Int) (loc ((line 2) (column 25)))))
         (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
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
    (sst
     ((Let (var f)
       (bind
        (Lambda (arg x) (body (Var (id x) (ty Int) (loc ((line 2) (column 24)))))
         (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Var (id f) (ty (Arrow (arg_ty Int) (ret_ty Int)))
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
    (sst
     ((Let (var f)
       (bind
        (Lambda (arg x)
         (body (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 31)))))
         (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Var (id f) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "weaken if non-split: mode erasure on branch" =
  go
    {|
let _ = if true then 1 else 1 @ erased;;
|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "weaken if non-split: arrow type join" =
  go
    {|
let _ = if true then fn (erased x : int) -> 1 else fn (x : int) -> 1;;
|};
  [%expect
    {|
    (sst
     ((Let (var _)
       (bind
        (Lambda (arg x)
         (body (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 44)))))
         (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 2) (column 21)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "weaken if split: mode only" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else 1 @ erased;;
|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "weaken binder apply: body weakened to ret_mode" =
  go
    {|
let f = fn (static x : int) -> x;;
let _ = f 0;;
|};
  [%expect {|
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Int 0)) (ty Int) (loc ((line 3) (column 8)))))
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
    (sst
     ((Let (var f)
       (bind
        (Lambda (arg x) (body (Var (id x) (ty Int) (loc ((line 2) (column 24)))))
         (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Let (var x)
         (bind (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 21)))))
         (rest (Var (id x) (ty Int) (loc ((line 2) (column 24))))) (ty Int)
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
    (sst
     ((Let (var f)
       (bind
        (Lambda (arg x)
         (body (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 24)))))
         (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "weaken mode: both axes (static unerased -> dynamic erased)" =
  go
    {|
let _ = 1 @ dynamic erased;;
|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "weaken if non-split: staticity on branch" =
  go
    {|
let x = 1 @ dynamic;;
let _ = if true then 1 else x;;
|};
  [%expect
    {|
    (sst
     ((Let (var x)
       (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 21)))))
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
    (sst
     ((Let (var x)
       (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))))
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
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Bool false)) (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "weaken if split: both axes on branch" =
  go
    {|
let f = fn (static b : bool) -> if static b then 1 @ dynamic erased else 1;;
let _ = f false;;
|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "weaken binder apply: erasure on body" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else 1 @ erased;;
let _ = f 0;;
|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "weaken arrow closure apply erased: staticity on body" =
  go
    {|
let f = fn (x : int) -> 1;;
let _ = (f @ erased) 0;;
|};
  [%expect
    {|
    (sst
     ((Let (var f)
       (bind
        (Lambda (arg x)
         (body (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 24)))))
         (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Let (var x)
         (bind (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 21)))))
         (rest (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 24)))))
         (ty Int) (loc ((line 3) (column 8)))))
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
    (sst
     ((Let (var f)
       (bind
        (Lambda (arg x) (body (Var (id x) (ty Int) (loc ((line 2) (column 24)))))
         (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))))
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
    (sst
     ((Let (var f)
       (bind
        (Lambda (arg x)
         (body (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 24)))))
         (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Let (var x)
         (bind (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 43)))))
         (rest (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 24)))))
         (ty Int) (loc ((line 4) (column 8)))))
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
    (sst
     ((Let (var apply)
       (bind
        (Lambda (arg f)
         (body
          (Apply
           (fn
            (Var (id f) (ty (Arrow (arg_ty Int) (ret_ty Int)))
             (loc ((line 2) (column 35)))))
           (arg (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 37)))))
           (ty Int) (loc ((line 2) (column 35)))))
         (fvs ())
         (ty (Arrow (arg_ty (Arrow (arg_ty Int) (ret_ty Int))) (ret_ty Int)))
         (loc ((line 2) (column 12)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Lambda (arg x)
         (body (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 31)))))
         (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id apply)
           (ty (Arrow (arg_ty (Arrow (arg_ty Int) (ret_ty Int))) (ret_ty Int)))
           (loc ((line 4) (column 8)))))
         (arg
          (Var (id g) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 4) (column 14)))))
         (ty Int) (loc ((line 4) (column 8)))))
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
    (sst
     ((Let (var g)
       (bind
        (Lambda (arg x) (body (Var (id x) (ty Int) (loc ((line 3) (column 24)))))
         (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
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
    (sst
     ((Let (var g)
       (bind
        (Lambda (arg x)
         (body (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 31)))))
         (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "closure where binder expected: Arrow leq Pi" =
  go
    {|
let apply = fn (static f : static int -> int) -> f 0;;
let g = fn (x : int) -> x;;
let _ = apply g;;
|};
  [%expect {|
    (sst
     ((Let (var apply)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 12)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Lambda (arg x) (body (Var (id x) (ty Int) (loc ((line 3) (column 24)))))
         (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id apply) (ty (Pack <opaque>)) (loc ((line 4) (column 8)))))
         (arg (Closure 3)) (ty Int) (loc ((line 4) (column 8)))))
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
  [%expect {|
    (sst
     ((Let (var apply)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 12)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id apply) (ty (Pack <opaque>)) (loc ((line 4) (column 8)))))
         (arg (Closure 4)) (ty Int) (loc ((line 4) (column 8)))))
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
    (sst
     ((Let (var apply)
       (bind
        (Lambda (arg f)
         (body
          (Apply
           (fn
            (Var (id f) (ty (Arrow (arg_ty Int) (ret_ty Int)))
             (loc ((line 2) (column 35)))))
           (arg (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 37)))))
           (ty Int) (loc ((line 2) (column 35)))))
         (fvs ())
         (ty (Arrow (arg_ty (Arrow (arg_ty Int) (ret_ty Int))) (ret_ty Int)))
         (loc ((line 2) (column 12)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Let (var f)
         (bind
          (Lambda (arg x)
           (body (Var (id x) (ty Int) (loc ((line 3) (column 42))))) (fvs ())
           (ty (Arrow (arg_ty Int) (ret_ty Int))) (loc ((line 3) (column 26)))))
         (rest
          (Apply
           (fn
            (Var (id f) (ty (Arrow (arg_ty Int) (ret_ty Int)))
             (loc ((line 2) (column 35)))))
           (arg (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 37)))))
           (ty Int) (loc ((line 2) (column 35)))))
         (ty Int) (loc ((line 3) (column 8)))))
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
    (sst
     ((Let (var g)
       (bind
        (Lambda (arg x)
         (body (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 24)))))
         (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "static lambda identity returns dependent type" =
  go
    {|
let f = fn (static x : int) -> x;;
let _ = f 42;;
|};
  [%expect {|
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Int 42)) (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "static lambda with arithmetic on static arg" =
  go
    {|
let f = fn (static x : int) -> x + 1;;
let _ = f 10;;
|};
  [%expect {|
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Int 10)) (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "static lambda with boolean op on static arg" =
  go
    {|
let f = fn (static x : bool) -> x && true;;
let _ = f false;;
|};
  [%expect {|
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Bool false)) (ty Bool) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "nested static lambdas" =
  go
    {|
let f = fn (static x : int) -> fn (static y : int) -> x + y;;
let _ = f 1 2;;
|};
  [%expect {|
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn
          (Symbol
           (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
           (arg (Int 1)) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Int 2)) (ty Int) (loc ((line 3) (column 8)))))
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
  [%expect {|
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Int 1)) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id g) (ty (Pack <opaque>)) (loc ((line 4) (column 8)))))
         (arg (Int 2)) (ty Int) (loc ((line 4) (column 8)))))
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
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Int 1)) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id g) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 4) (column 8)))))
         (arg (Scalar (value (Int 2)) (ty Int) (loc ((line 4) (column 10)))))
         (ty Int) (loc ((line 4) (column 8)))))
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
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg IntT) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id g) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 4) (column 8)))))
         (arg (Scalar (value (Int 42)) (ty Int) (loc ((line 4) (column 10)))))
         (ty Int) (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))
      (Let (var h)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 5) (column 8)))))
         (arg BoolT) (ty (Arrow (arg_ty Bool) (ret_ty Bool)))
         (loc ((line 5) (column 8)))))
       (loc ((line 5) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id h) (ty (Arrow (arg_ty Bool) (ret_ty Bool)))
           (loc ((line 6) (column 8)))))
         (arg
          (Scalar (value (Bool true)) (ty Bool) (loc ((line 6) (column 10)))))
         (ty Bool) (loc ((line 6) (column 8)))))
       (loc ((line 6) (column 0))))))
    |}]
;;

let%expect_test "if static with literal condition true" =
  go
    {|
let _ = if static true then 1 else true;;
|};
  [%expect
    {|
    (sst
     ((Let (var _)
       (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 28)))))
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
    (sst
     ((Let (var _)
       (bind (Scalar (value (Bool true)) (ty Bool) (loc ((line 2) (column 36)))))
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
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var a)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Int 0)) (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var b)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 4) (column 8)))))
         (arg (Int 1)) (ty Bool) (loc ((line 4) (column 8)))))
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
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
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
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
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
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Int 0)) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id g) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 4) (column 8)))))
         (arg (Scalar (value (Int 42)) (ty Int) (loc ((line 4) (column 10)))))
         (ty Int) (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))
      (Let (var h)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 5) (column 8)))))
         (arg (Int 1)) (ty (Arrow (arg_ty Bool) (ret_ty Bool)))
         (loc ((line 5) (column 8)))))
       (loc ((line 5) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id h) (ty (Arrow (arg_ty Bool) (ret_ty Bool)))
           (loc ((line 6) (column 8)))))
         (arg
          (Scalar (value (Bool true)) (ty Bool) (loc ((line 6) (column 10)))))
         (ty Bool) (loc ((line 6) (column 8)))))
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
    (sst
     ((Let (var _)
       (bind (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 29)))))
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
    (sst
     ((Let (var _)
       (bind (Scalar (value (Bool true)) (ty Bool) (loc ((line 2) (column 37)))))
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
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
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
  [%expect {|
    (sst
     ((Let (var apply_type)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 17)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn
          (Var (id apply_type) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Closure 4)) (ty (Arrow (arg_ty Int) (ret_ty Int)))
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
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
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
  [%expect {|
    (sst
     ((Fun
       (funs
        ((Pack (var f) (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
          (loc ((line 2) (column 4))))))
       (fvs ()) (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Int 0)) (ty Int) (loc ((line 3) (column 8)))))
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
    (sst
     ((Fun
       (funs
        ((Pack (var id) (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
          (loc ((line 2) (column 4))))))
       (fvs ()) (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Symbol
           (fn (Var (id id) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
           (arg IntT) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 3) (column 8)))))
         (arg (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 15)))))
         (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Symbol
           (fn (Var (id id) (ty (Pack <opaque>)) (loc ((line 4) (column 8)))))
           (arg BoolT) (ty (Arrow (arg_ty Bool) (ret_ty Bool)))
           (loc ((line 4) (column 8)))))
         (arg
          (Scalar (value (Bool true)) (ty Bool) (loc ((line 4) (column 16)))))
         (ty Bool) (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "fun dynamic recursion is allowed" =
  go
    {|
fun f (x : int) : int = f x;;
|};
  [%expect
    {|
    (sst
     ((Fun
       (funs
        ((Mono (var f) (arg x)
          (body
           (Apply
            (fn
             (Var (id f) (ty (Arrow (arg_ty Int) (ret_ty Int)))
              (loc ((line 2) (column 24)))))
            (arg (Var (id x) (ty Int) (loc ((line 2) (column 26))))) (ty Int)
            (loc ((line 2) (column 24)))))
          (fvs ((f (Arrow (arg_ty Int) (ret_ty Int)))))
          (ty (Arrow (arg_ty Int) (ret_ty Int))) (loc ((line 2) (column 4))))))
       (fvs ((f (Arrow (arg_ty Int) (ret_ty Int))))) (loc ((line 2) (column 0))))))
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
    (sst
     ((Fun
       (funs
        ((Pack (var id1) (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
          (loc ((line 2) (column 4))))))
       (fvs ()) (loc ((line 2) (column 0))))
      (Fun
       (funs
        ((Pack (var id2) (pack <opaque>) (fvs ((id1 (Pack <opaque>))))
          (ty (Pack <opaque>)) (loc ((line 3) (column 4))))))
       (fvs ((id1 (Pack <opaque>)))) (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Symbol
           (fn (Var (id id2) (ty (Pack <opaque>)) (loc ((line 4) (column 8)))))
           (arg IntT) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 4) (column 8)))))
         (arg (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 16)))))
         (ty Int) (loc ((line 4) (column 8)))))
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
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Int 1)) (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "lift static value through Pi" =
  go
    {|
let f = fn (static x : int) -> x @ erased;;
let _ = f 0;;
|};
  [%expect {| (sst ()) |}]
;;

let%expect_test "fun returning static erased type" =
  go
    {|
fun f (static _ : unit) : static erased type = int;;
let _ = 5 : f ();;
|};
  [%expect
    {|
    (sst
     ((Let (var _)
       (bind (Scalar (value (Int 5)) (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
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
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind (Var (id f) (ty (Pack <opaque>)) (loc ((line 4) (column 21)))))
       (loc ((line 4) (column 0))))))
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
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 4) (column 21)))))
         (arg (Int 0)) (ty Int) (loc ((line 4) (column 21)))))
       (loc ((line 4) (column 0))))))
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
  [%expect {|
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn
          (Symbol
           (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 7) (column 8)))))
           (arg (Int 0)) (ty (Pack <opaque>)) (loc ((line 7) (column 8)))))
         (arg (Int 0)) (ty Int) (loc ((line 7) (column 8)))))
       (loc ((line 7) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn
          (Symbol
           (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 8) (column 8)))))
           (arg (Int 0)) (ty (Pack <opaque>)) (loc ((line 8) (column 8)))))
         (arg (Int 1)) (ty Bool) (loc ((line 8) (column 8)))))
       (loc ((line 8) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn
          (Symbol
           (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 9) (column 8)))))
           (arg (Int 1)) (ty (Pack <opaque>)) (loc ((line 9) (column 8)))))
         (arg (Int 0)) (ty Unit) (loc ((line 9) (column 8)))))
       (loc ((line 9) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn
          (Symbol
           (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 10) (column 8)))))
           (arg (Int 1)) (ty (Pack <opaque>)) (loc ((line 10) (column 8)))))
         (arg (Int 1)) (ty Int) (loc ((line 10) (column 8)))))
       (loc ((line 10) (column 0))))))
    |}]
;;

let%expect_test "nested if static with different types per level" =
  go
    {|
fun f (static x : int) : static
  (static int \ y ->
   if x == 0
   then if y == 0 then int else bool
   else if y == 0 then unit else int)
  =
  fun g (static y : int) :
    if x == 0
    then if y == 0 then int else bool
    else if y == 0 then unit else int
  =
    if static x == 0
    then if static y == 0 then 1 else true
    else if static y == 0 then () else 2
   in
  g
;;
let _ = f 0 0;;
let _ = f 0 1;;
let _ = f 1 0;;
let _ = f 1 1;;
|};
  [%expect {|
    (sst
     ((Fun
       (funs
        ((Pack (var f) (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
          (loc ((line 2) (column 4))))))
       (fvs ()) (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn
          (Symbol
           (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 19) (column 8)))))
           (arg (Int 0)) (ty (Pack <opaque>)) (loc ((line 19) (column 8)))))
         (arg (Int 0)) (ty Int) (loc ((line 19) (column 8)))))
       (loc ((line 19) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn
          (Symbol
           (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 20) (column 8)))))
           (arg (Int 0)) (ty (Pack <opaque>)) (loc ((line 20) (column 8)))))
         (arg (Int 1)) (ty Bool) (loc ((line 20) (column 8)))))
       (loc ((line 20) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn
          (Symbol
           (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 21) (column 8)))))
           (arg (Int 1)) (ty (Pack <opaque>)) (loc ((line 21) (column 8)))))
         (arg (Int 0)) (ty Unit) (loc ((line 21) (column 8)))))
       (loc ((line 21) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn
          (Symbol
           (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 22) (column 8)))))
           (arg (Int 1)) (ty (Pack <opaque>)) (loc ((line 22) (column 8)))))
         (arg (Int 1)) (ty Int) (loc ((line 22) (column 8)))))
       (loc ((line 22) (column 0))))))
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
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Int 0)) (ty Int) (loc ((line 3) (column 8)))))
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
    (sst
     ((Let (var id)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 9)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Apply
           (fn
            (Symbol
             (fn (Var (id id) (ty (Pack <opaque>)) (loc ((line 3) (column 9)))))
             (arg
              (ArrowT (arg IntT)
               (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret IntT)
               (ret_mode ((staticity Dynamic) (erasure Unerased)))))
             (ty
              (Arrow (arg_ty (Arrow (arg_ty Int) (ret_ty Int)))
               (ret_ty (Arrow (arg_ty Int) (ret_ty Int)))))
             (loc ((line 3) (column 9)))))
           (arg
            (Lambda (arg x)
             (body (Var (id x) (ty Int) (loc ((line 3) (column 43))))) (fvs ())
             (ty (Arrow (arg_ty Int) (ret_ty Int))) (loc ((line 3) (column 27)))))
           (ty (Arrow (arg_ty Int) (ret_ty Int))) (loc ((line 3) (column 8)))))
         (arg (Scalar (value (Int 5)) (ty Int) (loc ((line 3) (column 46)))))
         (ty Int) (loc ((line 3) (column 8)))))
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
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Bool true)) (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 4) (column 8)))))
         (arg (Bool false)) (ty Bool) (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "static arg used in arithmetic, result applied" =
  go
    {|
let double = fn (static x : int) -> x + x;;
let _ = double 5;;
|};
  [%expect {|
    (sst
     ((Let (var double)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 13)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id double) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Int 5)) (ty Int) (loc ((line 3) (column 8)))))
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
    (sst
     ((Let (var id)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 9)))))
       (loc ((line 2) (column 0))))
      (Let (var f)
       (bind
        (Symbol
         (fn (Var (id id) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg IntT) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var g)
       (bind
        (Symbol
         (fn (Var (id id) (ty (Pack <opaque>)) (loc ((line 4) (column 8)))))
         (arg BoolT) (ty (Arrow (arg_ty Bool) (ret_ty Bool)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id f) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 5) (column 8)))))
         (arg (Scalar (value (Int 0)) (ty Int) (loc ((line 5) (column 10)))))
         (ty Int) (loc ((line 5) (column 8)))))
       (loc ((line 5) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id g) (ty (Arrow (arg_ty Bool) (ret_ty Bool)))
           (loc ((line 6) (column 8)))))
         (arg
          (Scalar (value (Bool true)) (ty Bool) (loc ((line 6) (column 10)))))
         (ty Bool) (loc ((line 6) (column 8)))))
       (loc ((line 6) (column 0))))))
    |}]
;;

let%expect_test "symbolic arrow type as static arg" =
  go
    {|
let choose = fn (static f : static int \ x -> if x == 0 then int else bool) -> f 0;;
let _ = choose (fn (static x : int) -> if static x == 0 then 0 else true);;
|};
  [%expect {|
    (sst
     ((Let (var choose)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 13)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id choose) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Closure 4)) (ty Int) (loc ((line 3) (column 8)))))
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
  [%expect {|
    (sst
     ((Let (var n)
       (bind (Scalar (value (Int 10)) (ty Int) (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ((n Int))) (ty (Pack <opaque>))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 4) (column 8)))))
         (arg (Int 5)) (ty Int) (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "static lambda with type annotation on body" =
  go
    {|
let f = fn (static x : int) -> (x : int);;
let _ = f 42;;
|};
  [%expect {|
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Int 42)) (ty Int) (loc ((line 3) (column 8)))))
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
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Bool true)) (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 4) (column 8)))))
         (arg (Bool false)) (ty Bool) (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "higher-order static: take a static function and apply it" =
  go
    {|
let apply = fn (static f : static int -> int) -> f 5;;
let _ = apply (fn (static x : int) -> x + 1);;
|};
  [%expect {|
    (sst
     ((Let (var apply)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 12)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id apply) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Closure 4)) (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "multiple static erased type args" =
  go
    {|
let f = fn (static erased t1 : type) -> fn (static erased t2 : type) -> fn (x : t1) -> fn (y : t2) -> x;;
let _ = f int bool 0 true;;
|};
  [%expect {|
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
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
               (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
               (arg IntT) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
             (arg BoolT)
             (ty
              (Arrow (arg_ty Int) (ret_ty (Arrow (arg_ty Bool) (ret_ty Int)))))
             (loc ((line 3) (column 8)))))
           (arg (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 19)))))
           (ty (Arrow (arg_ty Bool) (ret_ty Int))) (loc ((line 3) (column 8)))))
         (arg
          (Scalar (value (Bool true)) (ty Bool) (loc ((line 3) (column 21)))))
         (ty Int) (loc ((line 3) (column 8)))))
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
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 5) (column 8)))))
         (arg (Int 0)) (ty Int) (loc ((line 5) (column 8)))))
       (loc ((line 5) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 6) (column 8)))))
         (arg (Int 1)) (ty Bool) (loc ((line 6) (column 8)))))
       (loc ((line 6) (column 0))))))
    |}]
;;

let%expect_test "join Pi/Pi function-type arg returning type: fresh var issue" =
  go
    {|
let f = fn (static erased g : static int -> static erased type) -> fn (x : g 0) -> x;;
let h = fn (static erased g : static int -> static erased type) -> fn (x : g 0) -> x;;
let x = if true then f else h;;
let _ = x (fn (static x : int) -> int);;
|};
  [%expect
    {|
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var h)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var x)
       (bind (Var (id f) (ty (Pack <opaque>)) (loc ((line 4) (column 21)))))
       (loc ((line 4) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id x) (ty (Pack <opaque>)) (loc ((line 5) (column 8)))))
         (arg (Closure 10)) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 5) (column 8)))))
       (loc ((line 5) (column 0))))))
    |}]
;;

let%expect_test "leq Pi/Pi function-type arg returning type" =
  go
    {|
let wrap = fn (static erased f : static int -> static erased type) -> fn (x : f 0) -> x;;
let wrap2 = wrap : static erased (static int -> static erased type) \ f -> f 0 -> f 0;;
let _ = wrap2 (fn (static x : int) -> int);;
|};
  [%expect
    {|
    (sst
     ((Let (var wrap)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 11)))))
       (loc ((line 2) (column 0))))
      (Let (var wrap2)
       (bind (Var (id wrap) (ty (Pack <opaque>)) (loc ((line 3) (column 12)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id wrap2) (ty (Pack <opaque>)) (loc ((line 4) (column 8)))))
         (arg (Closure 9)) (ty (Arrow (arg_ty Int) (ret_ty Int)))
         (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "meet Pi/Pi function-type arg: via arg contravariance in join" =
  go
    {|
let f = fn (static apply : static (static int -> int) -> int) -> apply (fn (static x : int) -> 0);;
let g = fn (static apply : static (static erased int -> int) -> int) -> apply (fn (static erased x : int) -> 0);;
let x = if true then f else g;;
let _ = x (fn (static f : static int -> int) -> f 0);;
|};
  [%expect {|
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var g)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))
      (Let (var x)
       (bind (Var (id f) (ty (Pack <opaque>)) (loc ((line 4) (column 21)))))
       (loc ((line 4) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id x) (ty (Pack <opaque>)) (loc ((line 5) (column 8)))))
         (arg (Closure 17)) (ty Int) (loc ((line 5) (column 8)))))
       (loc ((line 5) (column 0))))))
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
    (sst
     ((Fun
       (funs
        ((Pack (var f) (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
          (loc ((line 2) (column 4))))
         (Mono (var g) (arg x)
          (body
           (Apply
            (fn
             (Symbol
              (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 24)))))
              (arg IntT) (ty (Arrow (arg_ty Int) (ret_ty Int)))
              (loc ((line 3) (column 24)))))
            (arg (Var (id x) (ty Int) (loc ((line 3) (column 30))))) (ty Int)
            (loc ((line 3) (column 24)))))
          (fvs ((f (Pack <opaque>)))) (ty (Arrow (arg_ty Int) (ret_ty Int)))
          (loc ((line 3) (column 4))))))
       (fvs ((f (Pack <opaque>)))) (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id g) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 4) (column 8)))))
         (arg (Scalar (value (Int 5)) (ty Int) (loc ((line 4) (column 10)))))
         (ty Int) (loc ((line 4) (column 8)))))
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
    (sst
     ((Fun
       (funs
        ((Mono (var inc) (arg x)
          (body
           (Binop (op Add)
            (lhs (Var (id x) (ty Int) (loc ((line 2) (column 26)))))
            (rhs (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 30)))))
            (ty Int) (loc ((line 2) (column 28)))))
          (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
          (loc ((line 2) (column 4))))
         (Pack (var choose) (pack <opaque>)
          (fvs ((inc (Arrow (arg_ty Int) (ret_ty Int))))) (ty (Pack <opaque>))
          (loc ((line 3) (column 4))))))
       (fvs ((inc (Arrow (arg_ty Int) (ret_ty Int)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Symbol
           (fn
            (Var (id choose) (ty (Pack <opaque>)) (loc ((line 5) (column 8)))))
           (arg (Bool true)) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 5) (column 8)))))
         (arg (Scalar (value (Int 5)) (ty Int) (loc ((line 5) (column 20)))))
         (ty Int) (loc ((line 5) (column 8)))))
       (loc ((line 5) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Symbol
           (fn
            (Var (id choose) (ty (Pack <opaque>)) (loc ((line 6) (column 8)))))
           (arg (Bool false)) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 6) (column 8)))))
         (arg (Scalar (value (Int 5)) (ty Int) (loc ((line 6) (column 21)))))
         (ty Int) (loc ((line 6) (column 8)))))
       (loc ((line 6) (column 0))))))
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : erased int = g x
and g (y : int) : int = let _ = f y in 0;;
let _ = g 0;;
|};
  [%expect {|
    (sst
     ((Fun
       (funs
        ((Mono (var g) (arg y)
          (body (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 39)))))
          (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
          (loc ((line 3) (column 4))))))
       (fvs ()) (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id g) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 4) (column 8)))))
         (arg (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 10)))))
         (ty Int) (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = let _ = g x in 0
and g (y : int) : erased int = f y;;
let _ = f 0;;
|};
  [%expect {|
    (sst
     ((Fun
       (funs
        ((Mono (var f) (arg x)
          (body (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 39)))))
          (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
          (loc ((line 2) (column 4))))))
       (fvs ()) (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id f) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 4) (column 8)))))
         (arg (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 10)))))
         (ty Int) (loc ((line 4) (column 8)))))
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
    (sst
     ((Let (var _)
       (bind
        (Let (var x)
         (bind (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 10)))))
         (rest (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 31)))))
         (ty Int) (loc ((line 3) (column 8)))))
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
    (sst
     ((Let (var _)
       (bind
        (Let (var x)
         (bind (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 10)))))
         (rest (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 31)))))
         (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "erased fn" =
  go
    {|
fun erased f (erased x : int) : int = 0;;
let _ = f 0;;
|};
  [%expect
    {|
    (sst
     ((Let (var _)
       (bind (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 38)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "erased fn" =
  go
    {|
let f = fn erased (erased x : int) -> 0;;
let _ = f 0;;
|};
  [%expect
    {|
    (sst
     ((Let (var _)
       (bind (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 38)))))
       (loc ((line 3) (column 0))))))
    |}]
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
    (sst
     ((Fun
       (funs
        ((Mono (var f) (arg x)
          (body
           (Let (var y) (bind (Var (id x) (ty Int) (loc ((line 2) (column 26)))))
            (rest
             (Apply
              (fn
               (Var (id f) (ty (Arrow (arg_ty Int) (ret_ty Int)))
                (loc ((line 3) (column 31)))))
              (arg (Var (id y) (ty Int) (loc ((line 3) (column 33))))) (ty Int)
              (loc ((line 3) (column 31)))))
            (ty Int) (loc ((line 2) (column 24)))))
          (fvs ((f (Arrow (arg_ty Int) (ret_ty Int)))))
          (ty (Arrow (arg_ty Int) (ret_ty Int))) (loc ((line 2) (column 4))))))
       (fvs ((f (Arrow (arg_ty Int) (ret_ty Int))))) (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Let (var x)
         (bind (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 21)))))
         (rest
          (Let (var y) (bind (Var (id x) (ty Int) (loc ((line 2) (column 26)))))
           (rest
            (Apply
             (fn
              (Var (id f) (ty (Arrow (arg_ty Int) (ret_ty Int)))
               (loc ((line 3) (column 31)))))
             (arg (Var (id y) (ty Int) (loc ((line 3) (column 33))))) (ty Int)
             (loc ((line 3) (column 31)))))
           (ty Int) (loc ((line 2) (column 24)))))
         (ty Int) (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "static lambda" =
  go
    {|
let _ = (fn (static x : int) -> x + 1) 0;;
|};
  [%expect {|
    (sst
     ((Let (var _)
       (bind
        (Let (var x)
         (bind (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 9)))))
         (rest
          (Binop (op Add)
           (lhs (Var (id x) (ty Int) (loc ((line 2) (column 32)))))
           (rhs (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 36)))))
           (ty Int) (loc ((line 2) (column 34)))))
         (ty Int) (loc ((line 2) (column 9)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "pi function calling arrow function in same group" =
  go
    {|
fun inc (x : int) : int = x + 1
and f (static erased t : type) : t -> t = fn (x : t) -> x;;
let _ = f int 0;;
|};
  [%expect
    {|
    (sst
     ((Fun
       (funs
        ((Mono (var inc) (arg x)
          (body
           (Binop (op Add)
            (lhs (Var (id x) (ty Int) (loc ((line 2) (column 26)))))
            (rhs (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 30)))))
            (ty Int) (loc ((line 2) (column 28)))))
          (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
          (loc ((line 2) (column 4))))
         (Pack (var f) (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
          (loc ((line 3) (column 4))))))
       (fvs ()) (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Symbol
           (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 4) (column 8)))))
           (arg IntT) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 4) (column 8)))))
         (arg (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 14)))))
         (ty Int) (loc ((line 4) (column 8)))))
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
    (sst
     ((Fun
       (funs
        ((Pack (var f) (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
          (loc ((line 2) (column 4))))
         (Mono (var g) (arg x)
          (body
           (Apply
            (fn
             (Symbol
              (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 24)))))
              (arg IntT) (ty (Arrow (arg_ty Int) (ret_ty Int)))
              (loc ((line 3) (column 24)))))
            (arg (Var (id x) (ty Int) (loc ((line 3) (column 30))))) (ty Int)
            (loc ((line 3) (column 24)))))
          (fvs ((f (Pack <opaque>)))) (ty (Arrow (arg_ty Int) (ret_ty Int)))
          (loc ((line 3) (column 4))))))
       (fvs ((f (Pack <opaque>)))) (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id g) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 4) (column 8)))))
         (arg (Scalar (value (Int 5)) (ty Int) (loc ((line 4) (column 10)))))
         (ty Int) (loc ((line 4) (column 8)))))
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
    (sst
     ((Fun
       (funs
        ((Mono (var inc) (arg x)
          (body
           (Binop (op Add)
            (lhs (Var (id x) (ty Int) (loc ((line 2) (column 26)))))
            (rhs (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 30)))))
            (ty Int) (loc ((line 2) (column 28)))))
          (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
          (loc ((line 2) (column 4))))
         (Pack (var choose) (pack <opaque>)
          (fvs ((inc (Arrow (arg_ty Int) (ret_ty Int))))) (ty (Pack <opaque>))
          (loc ((line 3) (column 4))))))
       (fvs ((inc (Arrow (arg_ty Int) (ret_ty Int)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Symbol
           (fn
            (Var (id choose) (ty (Pack <opaque>)) (loc ((line 5) (column 8)))))
           (arg (Bool true)) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 5) (column 8)))))
         (arg (Scalar (value (Int 5)) (ty Int) (loc ((line 5) (column 20)))))
         (ty Int) (loc ((line 5) (column 8)))))
       (loc ((line 5) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Symbol
           (fn
            (Var (id choose) (ty (Pack <opaque>)) (loc ((line 6) (column 8)))))
           (arg (Bool false)) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 6) (column 8)))))
         (arg (Scalar (value (Int 5)) (ty Int) (loc ((line 6) (column 21)))))
         (ty Int) (loc ((line 6) (column 8)))))
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
    (sst
     ((Fun
       (funs
        ((Pack (var f) (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
          (loc ((line 2) (column 4))))
         (Pack (var g) (pack <opaque>) (fvs ((f (Pack <opaque>))))
          (ty (Pack <opaque>)) (loc ((line 3) (column 4))))))
       (fvs ((f (Pack <opaque>)))) (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Symbol
           (fn (Var (id g) (ty (Pack <opaque>)) (loc ((line 4) (column 8)))))
           (arg IntT) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 4) (column 8)))))
         (arg (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 14)))))
         (ty Int) (loc ((line 4) (column 8)))))
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
    (sst
     ((Fun
       (funs
        ((Pack (var f) (pack <opaque>) (fvs ((f (Pack <opaque>))))
          (ty (Pack <opaque>)) (loc ((line 2) (column 4))))))
       (fvs ((f (Pack <opaque>)))) (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Int 3)) (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (static x : int) : erased int = (if static x == 0 then 42 else f (x - 1)) @ erased;;
let _ = f 3;;
|};
  [%expect {| (sst ()) |}]
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
    (sst
     ((Fun
       (funs
        ((Mono (var double) (arg x)
          (body
           (Binop (op Add)
            (lhs (Var (id x) (ty Int) (loc ((line 2) (column 29)))))
            (rhs (Var (id x) (ty Int) (loc ((line 2) (column 33))))) (ty Int)
            (loc ((line 2) (column 31)))))
          (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
          (loc ((line 2) (column 4))))
         (Pack (var apply_double) (pack <opaque>)
          (fvs ((double (Arrow (arg_ty Int) (ret_ty Int))))) (ty (Pack <opaque>))
          (loc ((line 3) (column 4))))))
       (fvs ((double (Arrow (arg_ty Int) (ret_ty Int)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Symbol
           (fn
            (Var (id apply_double) (ty (Pack <opaque>))
             (loc ((line 4) (column 8)))))
           (arg IntT) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 4) (column 8)))))
         (arg (Scalar (value (Int 5)) (ty Int) (loc ((line 4) (column 25)))))
         (ty Int) (loc ((line 4) (column 8)))))
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
    (sst
     ((Fun
       (funs
        ((Pack (var id1) (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
          (loc ((line 2) (column 4))))
         (Pack (var id2) (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
          (loc ((line 3) (column 4))))))
       (fvs ()) (loc ((line 2) (column 0))))))
    |}]
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
    (sst
     ((Fun
       (funs
        ((Mono (var f) (arg x)
          (body
           (Let (var y) (bind (Var (id x) (ty Int) (loc ((line 2) (column 26)))))
            (rest (Var (id y) (ty Int) (loc ((line 3) (column 31))))) (ty Int)
            (loc ((line 2) (column 24)))))
          (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
          (loc ((line 2) (column 4))))))
       (fvs ()) (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id f) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 4) (column 8)))))
         (arg (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 10)))))
         (ty Int) (loc ((line 4) (column 8)))))
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
    (sst
     ((Fun
       (funs
        ((Mono (var f) (arg x)
          (body
           (Let (var y) (bind (Var (id x) (ty Int) (loc ((line 2) (column 37)))))
            (rest (Var (id y) (ty Int) (loc ((line 3) (column 24))))) (ty Int)
            (loc ((line 2) (column 24)))))
          (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
          (loc ((line 2) (column 4))))
         (Mono (var g) (arg y)
          (body (Var (id y) (ty Int) (loc ((line 3) (column 24))))) (fvs ())
          (ty (Arrow (arg_ty Int) (ret_ty Int))) (loc ((line 3) (column 4))))))
       (fvs ()) (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id f) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 4) (column 8)))))
         (arg (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 10)))))
         (ty Int) (loc ((line 4) (column 8)))))
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
  [%expect {| (sst ()) |}]
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
    (sst
     ((Fun
       (funs
        ((Mono (var g) (arg y)
          (body (Var (id y) (ty Int) (loc ((line 3) (column 24))))) (fvs ())
          (ty (Arrow (arg_ty Int) (ret_ty Int))) (loc ((line 3) (column 4))))))
       (fvs ()) (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Let (var x)
         (bind (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 10)))))
         (rest
          (Let (var y) (bind (Var (id x) (ty Int) (loc ((line 2) (column 44)))))
           (rest (Var (id y) (ty Int) (loc ((line 3) (column 24))))) (ty Int)
           (loc ((line 2) (column 31)))))
         (ty Int) (loc ((line 4) (column 8)))))
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
    (sst
     ((Fun
       (funs
        ((Mono (var f) (arg x)
          (body
           (Apply
            (fn
             (Var (id g) (ty (Arrow (arg_ty Int) (ret_ty Int)))
              (loc ((line 2) (column 24)))))
            (arg (Var (id x) (ty Int) (loc ((line 2) (column 26))))) (ty Int)
            (loc ((line 2) (column 24)))))
          (fvs ((g (Arrow (arg_ty Int) (ret_ty Int)))))
          (ty (Arrow (arg_ty Int) (ret_ty Int))) (loc ((line 2) (column 4))))
         (Mono (var g) (arg y)
          (body
           (Apply
            (fn
             (Var (id f) (ty (Arrow (arg_ty Int) (ret_ty Int)))
              (loc ((line 3) (column 24)))))
            (arg (Var (id y) (ty Int) (loc ((line 3) (column 26))))) (ty Int)
            (loc ((line 3) (column 24)))))
          (fvs ((f (Arrow (arg_ty Int) (ret_ty Int)))))
          (ty (Arrow (arg_ty Int) (ret_ty Int))) (loc ((line 3) (column 4))))))
       (fvs
        ((f (Arrow (arg_ty Int) (ret_ty Int)))
         (g (Arrow (arg_ty Int) (ret_ty Int)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id f) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 4) (column 8)))))
         (arg (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 10)))))
         (ty Int) (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
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
    (sst
     ((Fun
       (funs
        ((Mono (var f) (arg x)
          (body
           (Let (var y) (bind (Var (id x) (ty Int) (loc ((line 2) (column 26)))))
            (rest
             (Apply
              (fn
               (Var (id f) (ty (Arrow (arg_ty Int) (ret_ty Int)))
                (loc ((line 3) (column 31)))))
              (arg (Var (id y) (ty Int) (loc ((line 3) (column 33))))) (ty Int)
              (loc ((line 3) (column 31)))))
            (ty Int) (loc ((line 2) (column 24)))))
          (fvs ((f (Arrow (arg_ty Int) (ret_ty Int)))))
          (ty (Arrow (arg_ty Int) (ret_ty Int))) (loc ((line 2) (column 4))))))
       (fvs ((f (Arrow (arg_ty Int) (ret_ty Int))))) (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Let (var x)
         (bind (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 21)))))
         (rest
          (Let (var y) (bind (Var (id x) (ty Int) (loc ((line 2) (column 26)))))
           (rest
            (Apply
             (fn
              (Var (id f) (ty (Arrow (arg_ty Int) (ret_ty Int)))
               (loc ((line 3) (column 31)))))
             (arg (Var (id y) (ty Int) (loc ((line 3) (column 33))))) (ty Int)
             (loc ((line 3) (column 31)))))
           (ty Int) (loc ((line 2) (column 24)))))
         (ty Int) (loc ((line 4) (column 8)))))
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
    (sst
     ((Fun
       (funs
        ((Mono (var f) (arg x)
          (body
           (Apply
            (fn
             (Var (id g) (ty (Arrow (arg_ty Int) (ret_ty Int)))
              (loc ((line 2) (column 24)))))
            (arg (Var (id x) (ty Int) (loc ((line 2) (column 26))))) (ty Int)
            (loc ((line 2) (column 24)))))
          (fvs ((g (Arrow (arg_ty Int) (ret_ty Int)))))
          (ty (Arrow (arg_ty Int) (ret_ty Int))) (loc ((line 2) (column 4))))
         (Mono (var g) (arg y)
          (body
           (Let (var x) (bind (Var (id y) (ty Int) (loc ((line 3) (column 37)))))
            (rest
             (Apply
              (fn
               (Var (id g) (ty (Arrow (arg_ty Int) (ret_ty Int)))
                (loc ((line 2) (column 24)))))
              (arg (Var (id x) (ty Int) (loc ((line 2) (column 26))))) (ty Int)
              (loc ((line 2) (column 24)))))
            (ty Int) (loc ((line 3) (column 24)))))
          (fvs ((g (Arrow (arg_ty Int) (ret_ty Int)))))
          (ty (Arrow (arg_ty Int) (ret_ty Int))) (loc ((line 3) (column 4))))))
       (fvs ((g (Arrow (arg_ty Int) (ret_ty Int))))) (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id f) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 4) (column 8)))))
         (arg (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 10)))))
         (ty Int) (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))))
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : erased int = g x
and g (y : int) : int = let _ = f y in 0;;
let _ = g 0;;
|};
  [%expect {|
    (sst
     ((Fun
       (funs
        ((Mono (var g) (arg y)
          (body (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 39)))))
          (fvs ()) (ty (Arrow (arg_ty Int) (ret_ty Int)))
          (loc ((line 3) (column 4))))))
       (fvs ()) (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id g) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 4) (column 8)))))
         (arg (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 10)))))
         (ty Int) (loc ((line 4) (column 8)))))
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
    (sst
     ((Let (var _)
       (bind
        (Let (var x)
         (bind (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 10)))))
         (rest (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 31)))))
         (ty Int) (loc ((line 3) (column 8)))))
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
    (sst
     ((Let (var _)
       (bind
        (Let (var x)
         (bind (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 10)))))
         (rest (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 31)))))
         (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "static int" =
  go
    {|
let f = fn (static x : int) -> x;;
let _ = f 0;;
|};
  [%expect {|
    (sst
     ((Let (var f)
       (bind
        (Pack (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id f) (ty (Pack <opaque>)) (loc ((line 3) (column 8)))))
         (arg (Int 0)) (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
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
    (sst
     ((External (var f) (symbol asdf) (ty (Arrow (arg_ty Int) (ret_ty Int)))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Var (id f) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 3) (column 8)))))
         (arg (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 10)))))
         (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
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
    (sst
     ((External (var f) (symbol asdf) (ty (Arrow (arg_ty Int) (ret_ty Int)))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (External (symbol asdf) (ty (Arrow (arg_ty Int) (ret_ty Int)))
           (loc ((line 3) (column 11)))))
         (arg (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 21)))))
         (ty Int) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test "static lambda effects" =
  go
    {|
external print_int : int -> unit = syl_print_int;;
let print = fn (static x : int) -> print_int x;;
let _ = print 0;;
let _ = print 1;;
|};
  [%expect {|
    (sst
     ((External (var print_int) (symbol syl_print_int)
       (ty (Arrow (arg_ty Int) (ret_ty Unit))) (loc ((line 2) (column 0))))
      (Let (var print)
       (bind
        (Pack (pack <opaque>)
         (fvs ((print_int (Arrow (arg_ty Int) (ret_ty Unit)))))
         (ty (Pack <opaque>)) (loc ((line 3) (column 12)))))
       (loc ((line 3) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id print) (ty (Pack <opaque>)) (loc ((line 4) (column 8)))))
         (arg (Int 0)) (ty Unit) (loc ((line 4) (column 8)))))
       (loc ((line 4) (column 0))))
      (Let (var _)
       (bind
        (Symbol
         (fn (Var (id print) (ty (Pack <opaque>)) (loc ((line 5) (column 8)))))
         (arg (Int 1)) (ty Unit) (loc ((line 5) (column 8)))))
       (loc ((line 5) (column 0))))))
    |}]
;;

let%expect_test "static lambda effects" =
  go
    {|
fun mk_ident (static erased pick_t : static unit -> static erased type) : static (pick_t () -> pick_t ()) =
  fn (x : pick_t ()) -> x
;;

let _ = mk_ident (fn (static _ : unit) -> if 1 + 1 == 2 then bool else unit) true;;
|};
  [%expect
    {|
    (sst
     ((Fun
       (funs
        ((Pack (var mk_ident) (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
          (loc ((line 2) (column 4))))))
       (fvs ()) (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Symbol
           (fn
            (Var (id mk_ident) (ty (Pack <opaque>)) (loc ((line 6) (column 8)))))
           (arg (Closure 6)) (ty (Arrow (arg_ty Bool) (ret_ty Bool)))
           (loc ((line 6) (column 8)))))
         (arg
          (Scalar (value (Bool true)) (ty Bool) (loc ((line 6) (column 77)))))
         (ty Bool) (loc ((line 6) (column 8)))))
       (loc ((line 6) (column 0))))))
    |}]
;;

let%expect_test "static lambda effects" =
  go
    {|
fun mk_ident (static pick_t : static unit -> static int) : static (let t = if pick_t () == 0 then int else bool in t -> t) =
  fn (x : if pick_t () == 0 then int else bool) -> x
;;

let _ = mk_ident (fn (static _ : unit) -> 1) true;;
|};
  [%expect {|
    (sst
     ((Fun
       (funs
        ((Pack (var mk_ident) (pack <opaque>) (fvs ()) (ty (Pack <opaque>))
          (loc ((line 2) (column 4))))))
       (fvs ()) (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply
         (fn
          (Symbol
           (fn
            (Var (id mk_ident) (ty (Pack <opaque>)) (loc ((line 6) (column 8)))))
           (arg (Closure 6)) (ty (Arrow (arg_ty Bool) (ret_ty Bool)))
           (loc ((line 6) (column 8)))))
         (arg
          (Scalar (value (Bool true)) (ty Bool) (loc ((line 6) (column 45)))))
         (ty Bool) (loc ((line 6) (column 8)))))
       (loc ((line 6) (column 0))))))
    |}]
;;
