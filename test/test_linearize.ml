open! Core
open! Syl

let go input =
  let cst = Parse.parse_exn input in
  let tst = Typecheck.typecheck_exn cst in
  let sst = Simplify.simplify tst in
  let lst = Linearize.linearize sst in
  print_s [%message (lst : Lst.Program.t)]
;;

let%expect_test "names" =
  go
    {|
let x = ();;
let x = ();;
|};
  [%expect
    {|
    (lst
     ((Values
       ((exprs
         ((((Id x)) (Scalar (value Unit) (ty Unit) (loc ((line 2) (column 8)))))))
        (bind ()) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Shadow 1) (Id x))
           (Scalar (value Unit) (ty Unit) (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
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
    (lst
     ((Values
       ((exprs
         ((((Id _)) (Scalar (value Unit) (ty Unit) (loc ((line 2) (column 8)))))))
        (bind ()) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Shadow 1) (Id _))
           (Scalar (value (Bool true)) (ty Bool) (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Shadow 2) (Id _))
           (Scalar (value (Int 123)) (ty Int) (loc ((line 4) (column 8)))))))
        (bind ()) (loc ((line 4) (column 0)))))
      (Values
       ((exprs
         ((((Shadow 3) (Id _))
           (Scalar (value Unit) (ty Unit) (loc ((line 8) (column 8)))))))
        (bind ()) (loc ((line 8) (column 0)))))
      (Values
       ((exprs
         ((((Shadow 4) (Id _))
           (Scalar (value (Bool true)) (ty Bool) (loc ((line 9) (column 8)))))))
        (bind ()) (loc ((line 9) (column 0)))))
      (Values
       ((exprs
         ((((Shadow 5) (Id _))
           (Scalar (value (Int 123)) (ty Int) (loc ((line 10) (column 8)))))))
        (bind ()) (loc ((line 10) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 2)))))))
        (bind ()) (loc ((line 2) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 2)))))))
        (bind ()) (loc ((line 2) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id dyn))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 10)))))))
        (bind ()) (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "Mode annotation valid" =
  go
    {|
let dyn = 1 @ erased;;
let _ =
  dyn @ dynamic erased
;;|};
  [%expect {| (lst ()) |}]
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
    (lst
     ((Values
       ((exprs
         ((((Id dyn))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 10)))))))
        (bind ()) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Ident (path ((Id dyn))) (ty Int) (loc ((line 4) (column 2)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id dyn))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 10)))))))
        (bind ()) (loc ((line 2) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Bool false)) (ty Bool) (loc ((line 3) (column 3)))))))
        (bind ()) (loc ((line 2) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Bool false)) (ty Bool) (loc ((line 3) (column 4)))))))
        (bind ()) (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "dynamic static erased" =
  go
    {|
let _ =
  (true @ static erased)
;;|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "dynamic erased" =
  go
    {|
let _ =
  (true @ dynamic erased)
;;|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "erased dynamic" =
  go
    {|
let _ =
  ((true @ erased) @ dynamic)
;;|};
  [%expect {| (lst ()) |}]
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
    (lst
     ((Values
       ((exprs
         ((((Id dyn))
           (Scalar (value (Bool true)) (ty Bool) (loc ((line 2) (column 10)))))))
        (bind ()) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Unop (op Not) (arg ((Id dyn))) (ty Bool) (loc ((line 4) (column 2)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Bool false)) (ty Bool) (loc ((line 4) (column 3)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Bool true)) (ty Bool) (loc ((line 4) (column 9)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Bool true)) (ty Bool) (loc ((line 4) (column 5)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id dyn))
           (Scalar (value (Bool true)) (ty Bool) (loc ((line 2) (column 10)))))))
        (bind ()) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Unop (op Not) (arg ((Id dyn))) (ty Bool) (loc ((line 4) (column 2)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "Unop var dynamic" =
  go
    {|
let dyn = true @ dynamic;;
let x = !dyn @ erased;;|};
  [%expect
    {|
    (lst
     ((Values
       ((exprs
         ((((Id dyn))
           (Scalar (value (Bool true)) (ty Bool) (loc ((line 2) (column 10)))))))
        (bind ()) (loc ((line 2) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 3)) (ty Int) (loc ((line 3) (column 2)))))))
        (bind ()) (loc ((line 2) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 3)) (ty Int) (loc ((line 3) (column 2)))))))
        (bind ()) (loc ((line 2) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 3)) (ty Int) (loc ((line 3) (column 2)))))))
        (bind ()) (loc ((line 2) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 6)) (ty Int) (loc ((line 3) (column 2)))))))
        (bind ()) (loc ((line 2) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 6)) (ty Int) (loc ((line 3) (column 2)))))))
        (bind ()) (loc ((line 2) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id dyn))
           (Scalar (value (Int 2)) (ty Int) (loc ((line 2) (column 10)))))))
        (bind ()) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Binop (op Add) (lhs ((Id $))) (rhs ((Id dyn))) (ty Int)
            (loc ((line 4) (column 4)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value (Int 1)) (ty Int) (loc ((line 4) (column 2)))))))
            (bind ()) (loc ((line 4) (column 2)))))))
        (loc ((line 3) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id dyn))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 10)))))))
        (bind ()) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Binop (op Add) (lhs ((Id dyn))) (rhs ((Id $))) (ty Int)
            (loc ((line 4) (column 6)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value (Int 2)) (ty Int) (loc ((line 4) (column 8)))))))
            (bind ()) (loc ((line 4) (column 8)))))))
        (loc ((line 3) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id dyn1))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 11)))))))
        (bind ()) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id dyn2))
           (Scalar (value (Int 2)) (ty Int) (loc ((line 3) (column 11)))))))
        (bind ()) (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Binop (op Add) (lhs ((Id dyn1))) (rhs ((Id dyn2))) (ty Int)
            (loc ((line 5) (column 7)))))))
        (bind ()) (loc ((line 4) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id dyn2))
           (Scalar (value (Int 2)) (ty Int) (loc ((line 3) (column 11)))))))
        (bind ()) (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Binop (op Add) (lhs ((Id $))) (rhs ((Id dyn2))) (ty Int)
            (loc ((line 5) (column 7)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value (Int 1)) (ty Int) (loc ((line 5) (column 2)))))))
            (bind ()) (loc ((line 5) (column 2)))))))
        (loc ((line 4) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 3)) (ty Int) (loc ((line 5) (column 2)))))))
        (bind ()) (loc ((line 4) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 15)))))))
        (bind ()) (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "If erased" =
  go
    {|
let _ =
  (if true then int else int) @ erased
;;|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "If erased" =
  go
    {|
let _ =
  if true @ erased then 1 else 2
;;|};
  [%expect
    {|
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 24)))))))
        (bind ()) (loc ((line 2) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 4) (column 12)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id dyn))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 10)))))))
        (bind ()) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Ident (path ((Id dyn))) (ty Int) (loc ((line 4) (column 15)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id dyn))
           (Scalar (value (Bool true)) (ty Bool) (loc ((line 2) (column 10)))))))
        (bind ()) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Ident (path ((Id if) (Id _))) (ty Int) (loc ((line 4) (column 2)))))))
        (bind
         ((If (path ((Id if) (Id _)))
           (cond (Ident (path ((Id dyn))) (ty Bool) (loc ((line 4) (column 5)))))
           (then_bind ())
           (then_ (Scalar (value (Int 1)) (ty Int) (loc ((line 4) (column 14)))))
           (else_bind ())
           (else_ (Scalar (value (Int 2)) (ty Int) (loc ((line 4) (column 21)))))
           (loc ((line 4) (column 2))))))
        (loc ((line 3) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id x))
           (Scalar (value (Bool true)) (ty Bool) (loc ((line 2) (column 8)))))))
        (bind ()) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 4) (column 2)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "if erased branch" =
  go
    {|
let _ = if 1==2 then unit else int;;|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "if erased branch" =
  go
    {|
let cond = true @ dynamic;;
let t = if cond then unit else int;;|};
  [%expect
    {|
    (lst
     ((Values
       ((exprs
         ((((Id cond))
           (Scalar (value (Bool true)) (ty Bool) (loc ((line 2) (column 11)))))))
        (bind ()) (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "if erased branch" =
  go
    {|
let cond = true @ dynamic;;
let t = (if cond then false else cond) @ erased;;|};
  [%expect
    {|
    (lst
     ((Values
       ((exprs
         ((((Id cond))
           (Scalar (value (Bool true)) (ty Bool) (loc ((line 2) (column 11)))))))
        (bind ()) (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "Let static" =
  go
    {|
let _ =
  let x = 1 in
  x
;;|};
  [%expect
    {|
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Ident (path ((Id x) (Id _))) (ty Int) (loc ((line 4) (column 2)))))))
        (bind
         ((Values
           ((exprs
             ((((Id x) (Id _))
               (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 10)))))))
            (bind ()) (loc ((line 3) (column 2)))))))
        (loc ((line 2) (column 0)))))))
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
  [%expect
    {|
    (lst
     ((Values
       ((exprs
         ((((Id dyn))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 10)))))))
        (bind ()) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Ident (path ((Id x) (Id _))) (ty Int) (loc ((line 5) (column 2)))))))
        (bind
         ((Values
           ((exprs
             ((((Id x) (Id _))
               (Ident (path ((Id dyn))) (ty Int) (loc ((line 4) (column 10)))))))
            (bind ()) (loc ((line 4) (column 2)))))))
        (loc ((line 3) (column 0)))))))
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
  [%expect
    {|
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Ident (path ((Id x) (Id _))) (ty Int) (loc ((line 5) (column 2)))))))
        (bind
         ((Values
           ((exprs
             ((((Id x) (Id _))
               (Scalar (value (Int 2)) (ty Int) (loc ((line 4) (column 10)))))))
            (bind ()) (loc ((line 4) (column 2)))))))
        (loc ((line 3) (column 0)))))))
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
  [%expect {| (lst ()) |}]
;;

let%expect_test "Let erased" =
  go
    {|
let _ =
  let x = 1 @ erased in
  x + 1
;;|};
  [%expect
    {|
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 2)) (ty Int) (loc ((line 4) (column 2)))))))
        (bind ()) (loc ((line 2) (column 0)))))))
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
  [%expect
    {|
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Binop (op Add) (lhs ((Id $))) (rhs ((Id y) (Id _))) (ty Int)
            (loc ((line 5) (column 4)))))))
        (bind
         ((Values
           ((exprs
             ((((Id y) (Id _))
               (Scalar (value (Int 1)) (ty Int) (loc ((line 4) (column 10)))))))
            (bind ()) (loc ((line 4) (column 2)))))
          (Values
           ((exprs
             ((((Id $))
               (Scalar (value (Int 1)) (ty Int) (loc ((line 5) (column 2)))))))
            (bind ()) (loc ((line 5) (column 2)))))))
        (loc ((line 2) (column 0)))))))
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
  [%expect
    {|
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 2)) (ty Int) (loc ((line 5) (column 2)))))))
        (bind ()) (loc ((line 2) (column 0)))))))
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
  [%expect
    {|
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 2)) (ty Int) (loc ((line 5) (column 2)))))))
        (bind
         ((Values
           ((exprs
             ((((Id x) (Id _))
               (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 10)))))))
            (bind ()) (loc ((line 3) (column 2)))))
          (Values
           ((exprs
             ((((Id y) (Id _))
               (Scalar (value (Int 1)) (ty Int) (loc ((line 4) (column 10)))))))
            (bind ()) (loc ((line 4) (column 2)))))))
        (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "static closure" =
  go
    {|
let _ =
  (fn (x : int) -> x)
;;|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id _))) (arg ((Id x))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 3) (column 19)))))
       (loc ((line 3) (column 3))))
      (Values
       ((exprs
         ((((Id _))
           (Make_closure (body ((Id "\206\187") (Id _)))
            (env (((Id env) (Id _)))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 3) (column 3)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id _))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 3)))))))
            (bind ()) (loc ((line 3) (column 3)))))))
        (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "erased closure" =
  go
    {|
let _ =
  (fn (x : int) -> x) @ erased
;;|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "closure return type" =
  go
    {|
let _ =
  (fn (x : int) -> int)
;;|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "dynamic closure" =
  go
    {|
let y = 1 @ dynamic;;
let _ =
  (fn (x : int) -> x + y)
;;|};
  [%expect {|
    (lst
     ((Values
       ((exprs
         ((((Id y))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 8)))))))
        (bind ()) (loc ((line 2) (column 0)))))
      (Closure_body (path ((Id "\206\187") (Id _))) (arg ((Id x))) (arg_ty Int)
       (captures (((path ((Id y))) (ty Int) (offset_in_bytes 0)))) (bind ())
       (return
        (Binop (op Add) (lhs ((Id x))) (rhs ((Id y))) (ty Int)
         (loc ((line 4) (column 21)))))
       (loc ((line 4) (column 3))))
      (Values
       ((exprs
         ((((Id _))
           (Make_closure (body ((Id "\206\187") (Id _)))
            (env (((Id env) (Id _)))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 4) (column 3)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id _))
               (Make_env
                (captures
                 ((entries (((path ((Id y))) (ty Int) (offset_in_bytes 0))))
                  (size_in_bytes 8)))
                (ty Env) (loc ((line 4) (column 3)))))))
            (bind ()) (loc ((line 4) (column 3)))))))
        (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (erased x : int) -> x;;
let _ = f 0;;
|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (erased x : int) -> 1;;
let _ = f 0;;
|};
  [%expect
    {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id f))) (arg ((Id x))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 31)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Id f))
           (Make_closure (body ((Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id f))) (arg ((Id $))) (arg_ty Int) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 10)))))))
            (bind ()) (loc ((line 3) (column 10)))))))
        (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = (fn (erased x : int) -> 1) @ erased;;
let _ = f 0;;
|};
  [%expect
    {|
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 32)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = (fn (erased x : int) -> 1) ;;
let _ = f 0;;
|};
  [%expect
    {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id f))) (arg ((Id x))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 32)))))
       (loc ((line 2) (column 9))))
      (Values
       ((exprs
         ((((Id f))
           (Make_closure (body ((Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 2) (column 9)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 9)))))))
            (bind ()) (loc ((line 2) (column 9)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id f))) (arg ((Id $))) (arg_ty Int) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 10)))))))
            (bind ()) (loc ((line 3) (column 10)))))))
        (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = (fn (erased x : int) -> 1) ;;
let _ = f (0 @ erased);;
|};
  [%expect
    {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id f))) (arg ((Id x))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 32)))))
       (loc ((line 2) (column 9))))
      (Values
       ((exprs
         ((((Id f))
           (Make_closure (body ((Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 2) (column 9)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 9)))))))
            (bind ()) (loc ((line 2) (column 9)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id f))) (arg ((Id $))) (arg_ty Int) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 13)))))))
            (bind ()) (loc ((line 3) (column 13)))))))
        (loc ((line 3) (column 0)))))))
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
    (lst
     ((Closure_body (path ((Id "\206\187") (Id f))) (arg ((Id x))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 31)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Id f))
           (Make_closure (body ((Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id f))) (arg ((Id $))) (arg_ty Int) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 10)))))))
            (bind ()) (loc ((line 3) (column 10)))))))
        (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let _ = (fn (erased x : int) -> 1) 0;;
|};
  [%expect
    {|
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 32)))))))
        (bind
         ((Values
           ((exprs
             ((((Id x) (Id _))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 35)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let _ = (fn (erased x : int) -> 1) (0 @ erased);;
|};
  [%expect
    {|
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 32)))))))
        (bind
         ((Values
           ((exprs
             ((((Id x) (Id _))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 38)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let _ = (fn (static x : int) -> 1) 0;;
|};
  [%expect
    {|
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 32)))))))
        (bind
         ((Values
           ((exprs
             ((((Id x) (Id _))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 9)))))))
            (bind ()) (loc ((line 2) (column 9)))))))
        (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let _ = (fn (static erased x : int) -> 1) 0;;
|};
  [%expect
    {|
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 39)))))))
        (bind ()) (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let _ = (fn (static erased x : int) -> 1) (0 @ erased);;
|};
  [%expect
    {|
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 39)))))))
        (bind ()) (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (static erased g : int -> erased int) -> let _ = g 1 in 2;;
let _ = f (fn (x : int) -> 0 @ erased);;
|};
  [%expect {|
    (lst
     ((Thunk_body (path ((Key (Closure 1)) (Id "\206\187") (Id f))) (captures ())
       (bind ())
       (return (Scalar (value (Int 2)) (ty Int) (loc ((line 2) (column 67)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Closure 1)) (Id f))
           (Make_closure (body ((Key (Closure 1)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Int))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Closure 1)) (Id f))) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (erased x : int -> int) -> 1;;
let _ = f ((fn (x : int) -> x + 1) @ erased);;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id f))) (arg ((Id x)))
       (arg_ty (Closure (arg_ty Int) (ret_ty Int))) (captures ()) (bind ())
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 38)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Id f))
           (Make_closure (body ((Id "\206\187") (Id f)))
            (env (((Id env) (Id f))))
            (ty
             (Closure (arg_ty (Closure (arg_ty Int) (ret_ty Int))) (ret_ty Int)))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Closure_body (path ((Id "\206\187") (Id _))) (arg ((Id x))) (arg_ty Int)
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id $))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 32)))))))
           (bind ()) (loc ((line 3) (column 32)))))))
       (return
        (Binop (op Add) (lhs ((Id x))) (rhs ((Id $))) (ty Int)
         (loc ((line 3) (column 30)))))
       (loc ((line 3) (column 35))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id f))) (arg ((Id $)))
            (arg_ty (Closure (arg_ty Int) (ret_ty Int))) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id _))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 35)))))))
            (bind ()) (loc ((line 3) (column 35)))))
          (Values
           ((exprs
             ((((Id $))
               (Make_closure (body ((Id "\206\187") (Id _)))
                (env (((Id env) (Id _))))
                (ty (Closure (arg_ty Int) (ret_ty Int)))
                (loc ((line 3) (column 35)))))))
            (bind ()) (loc ((line 3) (column 35)))))))
        (loc ((line 3) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 39)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "static erased closure arg" =
  go
    {|
let _ =
  (fn (static erased x : int) -> x)
;;|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "dependent closure arg" =
  go
    {|
let _ =
  (fn (x : type) -> x)
;;|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id _))) (arg ((Id x))) (arg_ty Unit)
       (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Unit) (loc ((line 3) (column 20)))))
       (loc ((line 3) (column 3))))
      (Values
       ((exprs
         ((((Id _))
           (Make_closure (body ((Id "\206\187") (Id _)))
            (env (((Id env) (Id _)))) (ty (Closure (arg_ty Unit) (ret_ty Unit)))
            (loc ((line 3) (column 3)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id _))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 3)))))))
            (bind ()) (loc ((line 3) (column 3)))))))
        (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "dependent closure arg" =
  go
    {|
fun f (x : type) : type = x;;|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id f))) (arg ((Id x))) (arg_ty Unit)
       (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Unit) (loc ((line 2) (column 26)))))
       (loc ((line 2) (column 4))))
      (Functions
       ((paths
         ((((Id f)) (Closure (arg_ty Unit) (ret_ty Unit))
           ((Id "\206\187") (Id f)))))
        (captures ((entries ()) (size_in_bytes 0))) (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "dependent closure arg" =
  go
    {|
fun f (static x : int) : static erased type = int;;|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "dependent closure arg" =
  go
    {|
let _ =
  (fn (static x : type) -> x)
;;|};
  [%expect {|
    (lst
     ((Values
       ((exprs ())
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id _))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 3)))))))
            (bind ()) (loc ((line 3) (column 3)))))))
        (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "dependent closure arg" =
  go
    {|
let _ =
  (fn (erased x : type) -> x)
;;|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "closure erased" =
  go
    {|
let x = (fn (x : int) -> x) @ erased;;
|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "closure erased" =
  go
    {|
let x = (fn (erased x : int) -> x) 0;;
|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "closure erased" =
  go
    {|
let x = (fn (erased x : int) -> 1) 0;;
|};
  [%expect
    {|
    (lst
     ((Values
       ((exprs
         ((((Id x))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 32)))))))
        (bind
         ((Values
           ((exprs
             ((((Id x) (Id x))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 35)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "closure erased" =
  go
    {|
let x = ((fn (erased x : int) -> 1) @ erased) 0;;
|};
  [%expect
    {|
    (lst
     ((Values
       ((exprs
         ((((Id x))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 33)))))))
        (bind ()) (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "closure branches" =
  go
    {|
let f = (fn (erased x : int) -> 1);;
let g = (fn (static x : int) -> 2);;
let _ = if true then f else g;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id f))) (arg ((Id x))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 32)))))
       (loc ((line 2) (column 9))))
      (Values
       ((exprs
         ((((Id f))
           (Make_closure (body ((Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 2) (column 9)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 9)))))))
            (bind ()) (loc ((line 2) (column 9)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs ())
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id g))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 9)))))))
            (bind ()) (loc ((line 3) (column 9)))))))
        (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Ident (path ((Id f))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 4) (column 21)))))))
        (bind ()) (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "closure branches" =
  go
    {|
let f = (fn (static erased x : int) -> 1);;
let g = (fn (static x : int) -> 2);;
let h = if true then f else g;;
let _ = h 0;;
|};
  [%expect {|
    (lst
     ((Thunk_body (path ((Key (Int 0)) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 0)) (Id "\206\187") (Id f))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 9)))))))
           (bind ()) (loc ((line 2) (column 9)))))))
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 39)))))
       (loc ((line 2) (column 9))))
      (Values
       ((exprs
         ((((Key (Int 0)) (Id f))
           (Make_closure (body ((Key (Int 0)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Int))
            (loc ((line 2) (column 9)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 9)))))))
            (bind ()) (loc ((line 2) (column 9)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs ())
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id g))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 9)))))))
            (bind ()) (loc ((line 3) (column 9)))))))
        (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Key (Int 0)) (Id h))
           (Ident (path ((Key (Int 0)) (Id f))) (ty (Thunk Int))
            (loc ((line 4) (column 21)))))))
        (bind ()) (loc ((line 4) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Int 0)) (Id h))) (ty Int)
            (loc ((line 5) (column 8)))))))
        (bind ()) (loc ((line 5) (column 0)))))))
    |}]
;;

let%expect_test "closure branches" =
  go
    {|
let f = (fn (static erased x : int) -> 1);;
let g = (fn (static erased x : int) -> 2);;
let _ = if true then f else g;;
|};
  [%expect {|
    (lst
     ((Values
       ((exprs ())
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 9)))))))
            (bind ()) (loc ((line 2) (column 9)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs ())
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id g))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 9)))))))
            (bind ()) (loc ((line 3) (column 9)))))))
        (loc ((line 3) (column 0)))))
      (Values ((exprs ()) (bind ()) (loc ((line 4) (column 0)))))))
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
    (lst
     ((Closure_body (path ((Id "\206\187") (Id c))) (arg ((Id _))) (arg_ty Unit)
       (captures ()) (bind ())
       (return
        (Scalar (value (Bool true)) (ty Bool) (loc ((line 2) (column 25)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Id c))
           (Make_closure (body ((Id "\206\187") (Id c)))
            (env (((Id env) (Id c)))) (ty (Closure (arg_ty Unit) (ret_ty Bool)))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id c))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Closure_body (path ((Id "\206\187") (Id f))) (arg ((Id x))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 25)))))
       (loc ((line 3) (column 9))))
      (Values
       ((exprs
         ((((Id f))
           (Make_closure (body ((Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 3) (column 9)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 9)))))))
            (bind ()) (loc ((line 3) (column 9)))))))
        (loc ((line 3) (column 0)))))
      (Closure_body (path ((Id "\206\187") (Id g))) (arg ((Id x))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Scalar (value (Int 2)) (ty Int) (loc ((line 4) (column 32)))))
       (loc ((line 4) (column 9))))
      (Values
       ((exprs
         ((((Id g))
           (Make_closure (body ((Id "\206\187") (Id g)))
            (env (((Id env) (Id g)))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 4) (column 9)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id g))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 4) (column 9)))))))
            (bind ()) (loc ((line 4) (column 9)))))))
        (loc ((line 4) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id if) (Id _))) (arg ((Shadow 1) (Id $)))
            (arg_ty Int) (ty Int) (loc ((line 5) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value Unit) (ty Unit) (loc ((line 5) (column 14)))))))
            (bind ()) (loc ((line 5) (column 14)))))
          (If (path ((Id if) (Id _)))
           (cond
            (Apply_closure (fn ((Id c))) (arg ((Id $))) (arg_ty Unit) (ty Bool)
             (loc ((line 5) (column 12)))))
           (then_bind ())
           (then_
            (Ident (path ((Id f))) (ty (Closure (arg_ty Int) (ret_ty Int)))
             (loc ((line 5) (column 22)))))
           (else_bind ())
           (else_
            (Ident (path ((Id g))) (ty (Closure (arg_ty Int) (ret_ty Int)))
             (loc ((line 5) (column 29)))))
           (loc ((line 5) (column 9))))
          (Values
           ((exprs
             ((((Shadow 1) (Id $))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 5) (column 32)))))))
            (bind ()) (loc ((line 5) (column 32)))))))
        (loc ((line 5) (column 0)))))))
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
    (lst
     ((Closure_body (path ((Id "\206\187") (Id f))) (arg ((Id x))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 24)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Id f))
           (Make_closure (body ((Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Closure_body (path ((Id "\206\187") (Id g))) (arg ((Id f)))
       (arg_ty (Closure (arg_ty Int) (ret_ty Int))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id $))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 33)))))))
           (bind ()) (loc ((line 3) (column 33)))))))
       (return
        (Apply_closure (fn ((Id f))) (arg ((Id $))) (arg_ty Int) (ty Int)
         (loc ((line 3) (column 31)))))
       (loc ((line 3) (column 8))))
      (Values
       ((exprs
         ((((Id g))
           (Make_closure (body ((Id "\206\187") (Id g)))
            (env (((Id env) (Id g))))
            (ty
             (Closure (arg_ty (Closure (arg_ty Int) (ret_ty Int))) (ret_ty Int)))
            (loc ((line 3) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id g))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 8)))))))
            (bind ()) (loc ((line 3) (column 8)))))))
        (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id g))) (arg ((Id f)))
            (arg_ty (Closure (arg_ty Int) (ret_ty Int))) (ty Int)
            (loc ((line 4) (column 8)))))))
        (bind ()) (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "closure nest" =
  go
    {|
let f = fn (x : int) -> 1;;
let g = fn (static f : int -> int) -> f 0;;
let _ = g f;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id f))) (arg ((Id x))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 24)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Id f))
           (Make_closure (body ((Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Closure_body
       (path ((Id "\206\187") (Id f) (Key (Closure 1)) (Id "\206\187") (Id g)))
       (arg ((Id x))) (arg_ty Int) (captures ()) (bind ())
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 24)))))
       (loc ((line 3) (column 8))))
      (Thunk_body (path ((Key (Closure 1)) (Id "\206\187") (Id g))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id f) (Key (Closure 1)) (Id "\206\187") (Id g))
              (Make_closure
               (body
                ((Id "\206\187") (Id f) (Key (Closure 1)) (Id "\206\187") (Id g)))
               (env (((Id env) (Id f) (Key (Closure 1)) (Id "\206\187") (Id g))))
               (ty (Closure (arg_ty Int) (ret_ty Int)))
               (loc ((line 3) (column 8)))))))
           (bind
            ((Values
              ((exprs
                ((((Id env) (Id f) (Key (Closure 1)) (Id "\206\187") (Id g))
                  (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                   (loc ((line 3) (column 8)))))))
               (bind ()) (loc ((line 3) (column 8)))))))
           (loc ((line 3) (column 8)))))
         (Values
          ((exprs
            ((((Id $))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 40)))))))
           (bind ()) (loc ((line 3) (column 40)))))))
       (return
        (Apply_closure (fn ((Id f) (Key (Closure 1)) (Id "\206\187") (Id g)))
         (arg ((Id $))) (arg_ty Int) (ty Int) (loc ((line 3) (column 38)))))
       (loc ((line 3) (column 8))))
      (Values
       ((exprs
         ((((Key (Closure 1)) (Id g))
           (Make_closure (body ((Key (Closure 1)) (Id "\206\187") (Id g)))
            (env (((Id env) (Id g)))) (ty (Thunk Int))
            (loc ((line 3) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id g))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 8)))))))
            (bind ()) (loc ((line 3) (column 8)))))))
        (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Closure 1)) (Id g))) (ty Int)
            (loc ((line 4) (column 8)))))))
        (bind ()) (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "closure nest" =
  go
    {|
let f = fn (erased x : int) -> 1;;
let g = fn (static f : erased int -> int) -> (f @ erased) 0;;
let _ = g f;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id f))) (arg ((Id x))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 31)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Id f))
           (Make_closure (body ((Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Closure_body
       (path ((Id "\206\187") (Id f) (Key (Closure 1)) (Id "\206\187") (Id g)))
       (arg ((Id x))) (arg_ty Int) (captures ()) (bind ())
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 31)))))
       (loc ((line 3) (column 8))))
      (Thunk_body (path ((Key (Closure 1)) (Id "\206\187") (Id g))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id f) (Key (Closure 1)) (Id "\206\187") (Id g))
              (Make_closure
               (body
                ((Id "\206\187") (Id f) (Key (Closure 1)) (Id "\206\187") (Id g)))
               (env (((Id env) (Id f) (Key (Closure 1)) (Id "\206\187") (Id g))))
               (ty (Closure (arg_ty Int) (ret_ty Int)))
               (loc ((line 3) (column 8)))))))
           (bind
            ((Values
              ((exprs
                ((((Id env) (Id f) (Key (Closure 1)) (Id "\206\187") (Id g))
                  (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                   (loc ((line 3) (column 8)))))))
               (bind ()) (loc ((line 3) (column 8)))))))
           (loc ((line 3) (column 8)))))))
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 31)))))
       (loc ((line 3) (column 8))))
      (Values
       ((exprs
         ((((Key (Closure 1)) (Id g))
           (Make_closure (body ((Key (Closure 1)) (Id "\206\187") (Id g)))
            (env (((Id env) (Id g)))) (ty (Thunk Int))
            (loc ((line 3) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id g))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 8)))))))
            (bind ()) (loc ((line 3) (column 8)))))))
        (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Closure 1)) (Id g))) (ty Int)
            (loc ((line 4) (column 8)))))))
        (bind ()) (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "closure nest" =
  go
    {|
let f1 = (fn (x : int) -> 1) @ erased;;
let g = fn (static erased f2 : int -> int) -> f2 0;;
let _ = g f1;;
|};
  [%expect {|
    (lst
     ((Thunk_body (path ((Key (Closure 1)) (Id "\206\187") (Id g))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Closure 1)) (Id "\206\187") (Id g))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 49)))))))
           (bind ()) (loc ((line 3) (column 46)))))))
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 26)))))
       (loc ((line 3) (column 8))))
      (Values
       ((exprs
         ((((Key (Closure 1)) (Id g))
           (Make_closure (body ((Key (Closure 1)) (Id "\206\187") (Id g)))
            (env (((Id env) (Id g)))) (ty (Thunk Int))
            (loc ((line 3) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id g))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 8)))))))
            (bind ()) (loc ((line 3) (column 8)))))))
        (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Closure 1)) (Id g))) (ty Int)
            (loc ((line 4) (column 8)))))))
        (bind ()) (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "inlined closure nest" =
  go
    {|
let f1 = fn (erased x : int) -> 1;;
let g = fn (f2 : int -> int) -> f2 0;;
let _ = g f1;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id f1))) (arg ((Id x))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 32)))))
       (loc ((line 2) (column 9))))
      (Values
       ((exprs
         ((((Id f1))
           (Make_closure (body ((Id "\206\187") (Id f1)))
            (env (((Id env) (Id f1)))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 2) (column 9)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f1))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 9)))))))
            (bind ()) (loc ((line 2) (column 9)))))))
        (loc ((line 2) (column 0)))))
      (Closure_body (path ((Id "\206\187") (Id g))) (arg ((Id f2)))
       (arg_ty (Closure (arg_ty Int) (ret_ty Int))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id $))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 35)))))))
           (bind ()) (loc ((line 3) (column 35)))))))
       (return
        (Apply_closure (fn ((Id f2))) (arg ((Id $))) (arg_ty Int) (ty Int)
         (loc ((line 3) (column 32)))))
       (loc ((line 3) (column 8))))
      (Values
       ((exprs
         ((((Id g))
           (Make_closure (body ((Id "\206\187") (Id g)))
            (env (((Id env) (Id g))))
            (ty
             (Closure (arg_ty (Closure (arg_ty Int) (ret_ty Int))) (ret_ty Int)))
            (loc ((line 3) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id g))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 8)))))))
            (bind ()) (loc ((line 3) (column 8)))))))
        (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id g))) (arg ((Id f1)))
            (arg_ty (Closure (arg_ty Int) (ret_ty Int))) (ty Int)
            (loc ((line 4) (column 8)))))))
        (bind ()) (loc ((line 4) (column 0)))))))
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
    (lst
     ((Closure_body (path ((Id "\206\187") (Id f1))) (arg ((Id x))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 32)))))
       (loc ((line 2) (column 9))))
      (Values
       ((exprs
         ((((Id f1))
           (Make_closure (body ((Id "\206\187") (Id f1)))
            (env (((Id env) (Id f1)))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 2) (column 9)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f1))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 9)))))))
            (bind ()) (loc ((line 2) (column 9)))))))
        (loc ((line 2) (column 0)))))
      (Closure_body
       (path ((Id "\206\187") (Id f2) (Key (Closure 1)) (Id "\206\187") (Id g)))
       (arg ((Id x))) (arg_ty Int) (captures ()) (bind ())
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 32)))))
       (loc ((line 3) (column 8))))
      (Thunk_body (path ((Key (Closure 1)) (Id "\206\187") (Id g))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id f2) (Key (Closure 1)) (Id "\206\187") (Id g))
              (Make_closure
               (body
                ((Id "\206\187") (Id f2) (Key (Closure 1)) (Id "\206\187")
                 (Id g)))
               (env
                (((Id env) (Id f2) (Key (Closure 1)) (Id "\206\187") (Id g))))
               (ty (Closure (arg_ty Int) (ret_ty Int)))
               (loc ((line 3) (column 8)))))))
           (bind
            ((Values
              ((exprs
                ((((Id env) (Id f2) (Key (Closure 1)) (Id "\206\187") (Id g))
                  (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                   (loc ((line 3) (column 8)))))))
               (bind ()) (loc ((line 3) (column 8)))))))
           (loc ((line 3) (column 8)))))
         (Values
          ((exprs
            ((((Id $))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 49)))))))
           (bind ()) (loc ((line 3) (column 49)))))))
       (return
        (Apply_closure (fn ((Id f2) (Key (Closure 1)) (Id "\206\187") (Id g)))
         (arg ((Id $))) (arg_ty Int) (ty Int) (loc ((line 3) (column 46)))))
       (loc ((line 3) (column 8))))
      (Values
       ((exprs
         ((((Key (Closure 1)) (Id g))
           (Make_closure (body ((Key (Closure 1)) (Id "\206\187") (Id g)))
            (env (((Id env) (Id g)))) (ty (Thunk Int))
            (loc ((line 3) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id g))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 8)))))))
            (bind ()) (loc ((line 3) (column 8)))))))
        (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Closure 1)) (Id g))) (ty Int)
            (loc ((line 4) (column 8)))))))
        (bind ()) (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "inlined closure nest" =
  go
    {|
let f1 = (fn (erased x : int) -> 1) @ erased;;
let g = fn (static erased f2 : erased int -> int) -> f2 0;;
let _ = g f1;;
|};
  [%expect {|
    (lst
     ((Thunk_body (path ((Key (Closure 1)) (Id "\206\187") (Id g))) (captures ())
       (bind ())
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 33)))))
       (loc ((line 3) (column 8))))
      (Values
       ((exprs
         ((((Key (Closure 1)) (Id g))
           (Make_closure (body ((Key (Closure 1)) (Id "\206\187") (Id g)))
            (env (((Id env) (Id g)))) (ty (Thunk Int))
            (loc ((line 3) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id g))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 8)))))))
            (bind ()) (loc ((line 3) (column 8)))))))
        (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Closure 1)) (Id g))) (ty Int)
            (loc ((line 4) (column 8)))))))
        (bind ()) (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "inlined closure nest" =
  go
    {|
let f1 = fn (erased x : int) -> 1;;
let g = fn (static erased f2 : erased int -> int) -> f2 0;;
let _ = g f1;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id f1))) (arg ((Id x))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 32)))))
       (loc ((line 2) (column 9))))
      (Values
       ((exprs
         ((((Id f1))
           (Make_closure (body ((Id "\206\187") (Id f1)))
            (env (((Id env) (Id f1)))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 2) (column 9)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f1))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 9)))))))
            (bind ()) (loc ((line 2) (column 9)))))))
        (loc ((line 2) (column 0)))))
      (Thunk_body (path ((Key (Closure 1)) (Id "\206\187") (Id g))) (captures ())
       (bind ())
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 32)))))
       (loc ((line 3) (column 8))))
      (Values
       ((exprs
         ((((Key (Closure 1)) (Id g))
           (Make_closure (body ((Key (Closure 1)) (Id "\206\187") (Id g)))
            (env (((Id env) (Id g)))) (ty (Thunk Int))
            (loc ((line 3) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id g))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 8)))))))
            (bind ()) (loc ((line 3) (column 8)))))))
        (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Closure 1)) (Id g))) (ty Int)
            (loc ((line 4) (column 8)))))))
        (bind ()) (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "closure static" =
  go
    {|
let x = (fn (static x : int) -> x) 0;;
|};
  [%expect {|
    (lst
     ((Values
       ((exprs
         ((((Id x))
           (Ident (path ((Id x) (Id x))) (ty Int) (loc ((line 2) (column 32)))))))
        (bind
         ((Values
           ((exprs
             ((((Id x) (Id x))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 9)))))))
            (bind ()) (loc ((line 2) (column 9)))))))
        (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "closure static erased" =
  go
    {|
let x = (fn (static erased x : int) -> 1) 0;;
|};
  [%expect
    {|
    (lst
     ((Values
       ((exprs
         ((((Id x))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 39)))))))
        (bind ()) (loc ((line 2) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Ident (path ((Id x) (Id _))) (ty Int) (loc ((line 3) (column 19)))))))
        (bind
         ((Values
           ((exprs
             ((((Id x) (Id _))
               (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 22)))))))
            (bind ()) (loc ((line 3) (column 2)))))))
        (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "Apply static erased fn static arg" =
  go
    {|
let _ =
  (fn (static erased x : int) -> x) 1
;;|};
  [%expect {| (lst ()) |}]
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
    (lst
     ((Values
       ((exprs
         ((((Id dyn))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 10)))))))
        (bind ()) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Ident (path ((Id x) (Id _))) (ty Int) (loc ((line 4) (column 19)))))))
        (bind
         ((Values
           ((exprs
             ((((Id x) (Id _))
               (Ident (path ((Id dyn))) (ty Int) (loc ((line 4) (column 22)))))))
            (bind ()) (loc ((line 4) (column 2)))))))
        (loc ((line 3) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id dyn))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 10)))))))
        (bind ()) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id y))
           (Scalar (value (Int 5)) (ty Int) (loc ((line 4) (column 26)))))))
        (bind
         ((Values
           ((exprs
             ((((Id x) (Id y))
               (Binop (op Sub) (lhs ((Id dyn))) (rhs ((Id $))) (ty Int)
                (loc ((line 4) (column 33)))))))
            (bind
             ((Values
               ((exprs
                 ((((Id $))
                   (Scalar (value (Int 1)) (ty Int) (loc ((line 4) (column 34)))))))
                (bind ()) (loc ((line 4) (column 34)))))))
            (loc ((line 4) (column 2)))))))
        (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id _)) (Ident (path ((Id y))) (ty Int) (loc ((line 6) (column 8)))))))
        (bind ()) (loc ((line 6) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id y))
           (Scalar (value (Int 5)) (ty Int) (loc ((line 4) (column 26)))))))
        (bind
         ((Values
           ((exprs
             ((((Id x) (Id y))
               (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 33)))))))
            (bind
             ((Values
               ((exprs
                 ((((Id x) (Id x) (Id y))
                   (Scalar (value (Int 1)) (ty Int) (loc ((line 4) (column 32)))))))
                (bind ()) (loc ((line 4) (column 30)))))))
            (loc ((line 4) (column 2)))))))
        (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id _)) (Ident (path ((Id y))) (ty Int) (loc ((line 6) (column 8)))))))
        (bind ()) (loc ((line 6) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id dyn))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 10)))))))
        (bind ()) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id y))
           (Scalar (value (Int 5)) (ty Int) (loc ((line 4) (column 26)))))))
        (bind
         ((Values
           ((exprs
             ((((Id x) (Id y))
               (Binop (op Sub) (lhs ((Id dyn))) (rhs ((Id $))) (ty Int)
                (loc ((line 4) (column 33)))))))
            (bind
             ((Values
               ((exprs
                 ((((Id $))
                   (Scalar (value (Int 1)) (ty Int) (loc ((line 4) (column 34)))))))
                (bind ()) (loc ((line 4) (column 34)))))))
            (loc ((line 4) (column 2)))))))
        (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id _)) (Ident (path ((Id y))) (ty Int) (loc ((line 6) (column 8)))))))
        (bind ()) (loc ((line 6) (column 0)))))))
    |}]
;;

let%expect_test "Apply erased fn stat arg" =
  go
    {|
let dyn = 1;;
let y =
  (fn (static x : int) -> 5) (dyn-1)
;;
let _ = y @ unerased;;|};
  [%expect
    {|
    (lst
     ((Values
       ((exprs
         ((((Id dyn))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 10)))))))
        (bind ()) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id y))
           (Scalar (value (Int 5)) (ty Int) (loc ((line 4) (column 26)))))))
        (bind
         ((Values
           ((exprs
             ((((Id x) (Id y))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 3)))))))
            (bind ()) (loc ((line 4) (column 3)))))))
        (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id _)) (Ident (path ((Id y))) (ty Int) (loc ((line 6) (column 8)))))))
        (bind ()) (loc ((line 6) (column 0)))))))
    |}]
;;

let%expect_test "Apply erased fn stat arg" =
  go
    {|
let dyn = 1;;
let y =
  (fn (static erased x : int) -> 5) (dyn-1)
;;
let _ = y @ unerased;;|};
  [%expect
    {|
    (lst
     ((Values
       ((exprs
         ((((Id dyn))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 10)))))))
        (bind ()) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id y))
           (Scalar (value (Int 5)) (ty Int) (loc ((line 4) (column 33)))))))
        (bind ()) (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id _)) (Ident (path ((Id y))) (ty Int) (loc ((line 6) (column 8)))))))
        (bind ()) (loc ((line 6) (column 0)))))))
    |}]
;;

let%expect_test "Apply dynamic fn static arg" =
  go
    {|
let dyn_fn = (fn (x : int) -> x) @ dynamic;;
let _ =
  dyn_fn 1
;;|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id dyn_fn))) (arg ((Id x)))
       (arg_ty Int) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 2) (column 30)))))
       (loc ((line 2) (column 14))))
      (Values
       ((exprs
         ((((Id dyn_fn))
           (Make_closure (body ((Id "\206\187") (Id dyn_fn)))
            (env (((Id env) (Id dyn_fn))))
            (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 2) (column 14)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id dyn_fn))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 14)))))))
            (bind ()) (loc ((line 2) (column 14)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id dyn_fn))) (arg ((Id $))) (arg_ty Int)
            (ty Int) (loc ((line 4) (column 2)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value (Int 1)) (ty Int) (loc ((line 4) (column 9)))))))
            (bind ()) (loc ((line 4) (column 9)))))))
        (loc ((line 3) (column 0)))))))
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
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id dyn_fn))) (arg ((Id x)))
       (arg_ty Int) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 2) (column 30)))))
       (loc ((line 2) (column 14))))
      (Values
       ((exprs
         ((((Id dyn_fn))
           (Make_closure (body ((Id "\206\187") (Id dyn_fn)))
            (env (((Id env) (Id dyn_fn))))
            (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 2) (column 14)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id dyn_fn))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 14)))))))
            (bind ()) (loc ((line 2) (column 14)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id dyn_arg))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 14)))))))
        (bind ()) (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id dyn_fn))) (arg ((Id dyn_arg))) (arg_ty Int)
            (ty Int) (loc ((line 5) (column 2)))))))
        (bind ()) (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "Lambda dynamic arg" =
  go
    {|
let _ =
  fn (x : int) -> x
;;|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id _))) (arg ((Id x))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 3) (column 18)))))
       (loc ((line 3) (column 2))))
      (Values
       ((exprs
         ((((Id _))
           (Make_closure (body ((Id "\206\187") (Id _)))
            (env (((Id env) (Id _)))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 3) (column 2)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id _))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 2)))))))
            (bind ()) (loc ((line 3) (column 2)))))))
        (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "Lambda static arg" =
  go
    {|
let _ =
  fn (static x : int) -> 1
;;|};
  [%expect {|
    (lst
     ((Values
       ((exprs ())
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id _))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 2)))))))
            (bind ()) (loc ((line 3) (column 2)))))))
        (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "Lambda erased arg" =
  go
    {|
let _ =
  fn (erased x : int) -> x
;;|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "Lambda capturing dynamic var" =
  go
    {|
let x = 1 @ dynamic;;
let _ =
  fn (y : int) -> x
;;|};
  [%expect {|
    (lst
     ((Values
       ((exprs
         ((((Id x))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 8)))))))
        (bind ()) (loc ((line 2) (column 0)))))
      (Closure_body (path ((Id "\206\187") (Id _))) (arg ((Id y))) (arg_ty Int)
       (captures (((path ((Id x))) (ty Int) (offset_in_bytes 0)))) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 4) (column 18)))))
       (loc ((line 4) (column 2))))
      (Values
       ((exprs
         ((((Id _))
           (Make_closure (body ((Id "\206\187") (Id _)))
            (env (((Id env) (Id _)))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 4) (column 2)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id _))
               (Make_env
                (captures
                 ((entries (((path ((Id x))) (ty Int) (offset_in_bytes 0))))
                  (size_in_bytes 8)))
                (ty Env) (loc ((line 4) (column 2)))))))
            (bind ()) (loc ((line 4) (column 2)))))))
        (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "Lambda capturing static var" =
  go
    {|
let x = 1 @ static;;
let _ =
  fn (y : int) -> x
;;|};
  [%expect {|
    (lst
     ((Values
       ((exprs
         ((((Id x))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 8)))))))
        (bind ()) (loc ((line 2) (column 0)))))
      (Closure_body (path ((Id "\206\187") (Id _))) (arg ((Id y))) (arg_ty Int)
       (captures (((path ((Id x))) (ty Int) (offset_in_bytes 0)))) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 4) (column 18)))))
       (loc ((line 4) (column 2))))
      (Values
       ((exprs
         ((((Id _))
           (Make_closure (body ((Id "\206\187") (Id _)))
            (env (((Id env) (Id _)))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 4) (column 2)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id _))
               (Make_env
                (captures
                 ((entries (((path ((Id x))) (ty Int) (offset_in_bytes 0))))
                  (size_in_bytes 8)))
                (ty Env) (loc ((line 4) (column 2)))))))
            (bind ()) (loc ((line 4) (column 2)))))))
        (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "Lambda capturing erased var" =
  go
    {|
let x = 1 @ static erased;;
let _ =
  fn (y : int) -> x
;;|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "Lambda capturing erased var" =
  go
    {|
let x = 1 @ static erased;;
let _ =
  (fn (y : int) -> x) 0
;;|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "Lambda capturing type" =
  go
    {|
let f = fn (static _ : unit) -> int;;
let g = fn (x : f ()) -> x + 1;;|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id g))) (arg ((Id x))) (arg_ty Int)
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id $))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 29)))))))
           (bind ()) (loc ((line 3) (column 29)))))))
       (return
        (Binop (op Add) (lhs ((Id x))) (rhs ((Id $))) (ty Int)
         (loc ((line 3) (column 27)))))
       (loc ((line 3) (column 8))))
      (Values
       ((exprs
         ((((Id g))
           (Make_closure (body ((Id "\206\187") (Id g)))
            (env (((Id env) (Id g)))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 3) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id g))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 8)))))))
            (bind ()) (loc ((line 3) (column 8)))))))
        (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "Lambda take type" =
  go
    {|
let f = fn (erased ty : type) -> ty;;
let _ = f int;;
|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "Lambda take type" =
  go
    {|
let f = fn (static erased ty : type) -> ty;;
let _ = 0 : f int;;
|};
  [%expect
    {|
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "mono fn" =
  go
    {|
let x = fn (static erased x : type) -> x;;
let y = x int;;
|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "mono fn" =
  go
    {|
let x = fn (static x : type) -> x;;
let y = x @ dynamic;;
|};
  [%expect {|
    (lst
     ((Values
       ((exprs ())
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id x))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values ((exprs ()) (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "mono fn" =
  go
    {|
let x = fn (erased x : type) -> x;;
let y = x @ dynamic;;
|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "mono fn" =
  go
    {|
let x = fn (static x : type) -> x;;
let y = x @ unerased;;
|};
  [%expect {|
    (lst
     ((Values
       ((exprs ())
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id x))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values ((exprs ()) (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "mono fn" =
  go
    {|
let x = fn (static erased x : type) -> x;;
let y = (x int) @ dynamic;;
|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "Dependent lambda" =
  go
    {|
let f = fn (static erased ty : type) -> fn (x : ty) -> x;;
|};
  [%expect {|
    (lst
     ((Values
       ((exprs ())
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "static erased lambda" =
  go
    {|
let f = fn (static x : int) -> fn (_ : unit) -> x;;
let g = (f 1 ()) @ unerased;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Key (Int 1)) (Id "\206\187") (Id f)))
       (arg ((Id _))) (arg_ty Unit)
       (captures (((path ((Id x))) (ty Int) (offset_in_bytes 0)))) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 2) (column 48)))))
       (loc ((line 2) (column 31))))
      (Thunk_body (path ((Key (Int 1)) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 1)) (Id "\206\187") (Id f))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))
         (Values
          ((exprs
            ((((Id env) (Key (Int 1)) (Id "\206\187") (Id f))
              (Make_env
               (captures
                ((entries
                  (((path ((Id x) (Key (Int 1)) (Id "\206\187") (Id f)))
                    (ty Int) (offset_in_bytes 0))))
                 (size_in_bytes 8)))
               (ty Env) (loc ((line 2) (column 31)))))))
           (bind ()) (loc ((line 2) (column 31)))))))
       (return
        (Make_closure
         (body ((Id "\206\187") (Key (Int 1)) (Id "\206\187") (Id f)))
         (env (((Id env) (Key (Int 1)) (Id "\206\187") (Id f))))
         (ty (Closure (arg_ty Unit) (ret_ty Int))) (loc ((line 2) (column 31)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Int 1)) (Id f))
           (Make_closure (body ((Key (Int 1)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f))))
            (ty (Thunk (Closure (arg_ty Unit) (ret_ty Int))))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id g))
           (Apply_closure (fn ((Id $))) (arg ((Shadow 1) (Id $))) (arg_ty Unit)
            (ty Int) (loc ((line 3) (column 9)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Apply_thunk (fn ((Key (Int 1)) (Id f)))
                (ty (Closure (arg_ty Unit) (ret_ty Int)))
                (loc ((line 3) (column 9)))))))
            (bind ()) (loc ((line 3) (column 9)))))
          (Values
           ((exprs
             ((((Shadow 1) (Id $))
               (Scalar (value Unit) (ty Unit) (loc ((line 3) (column 13)))))))
            (bind ()) (loc ((line 3) (column 13)))))))
        (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "lift universal type" =
  go
    {|
let f = fn (static ty : type) -> ty @ erased;;
|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "lift universal int" =
  go
    {|
let f = fn (static x : int) -> x @ erased;;
let _ = f 0;;
|};
  [%expect {| (lst ()) |}]
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
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Key BoolT) (Id "\206\187") (Id f)))
       (arg ((Id x))) (arg_ty Bool) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Bool) (loc ((line 2) (column 55)))))
       (loc ((line 2) (column 40))))
      (Thunk_body (path ((Key BoolT) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Key BoolT) (Id "\206\187") (Id f))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 2) (column 40)))))))
           (bind ()) (loc ((line 2) (column 40)))))))
       (return
        (Make_closure (body ((Id "\206\187") (Key BoolT) (Id "\206\187") (Id f)))
         (env (((Id env) (Key BoolT) (Id "\206\187") (Id f))))
         (ty (Closure (arg_ty Bool) (ret_ty Bool))) (loc ((line 2) (column 40)))))
       (loc ((line 2) (column 8))))
      (Closure_body (path ((Id "\206\187") (Key IntT) (Id "\206\187") (Id f)))
       (arg ((Id x))) (arg_ty Int) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 2) (column 55)))))
       (loc ((line 2) (column 40))))
      (Thunk_body (path ((Key IntT) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Key IntT) (Id "\206\187") (Id f))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 2) (column 40)))))))
           (bind ()) (loc ((line 2) (column 40)))))))
       (return
        (Make_closure (body ((Id "\206\187") (Key IntT) (Id "\206\187") (Id f)))
         (env (((Id env) (Key IntT) (Id "\206\187") (Id f))))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 2) (column 40)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key BoolT) (Id f))
           (Make_closure (body ((Key BoolT) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f))))
            (ty (Thunk (Closure (arg_ty Bool) (ret_ty Bool))))
            (loc ((line 2) (column 8)))))
          (((Key IntT) (Id f))
           (Make_closure (body ((Key IntT) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f))))
            (ty (Thunk (Closure (arg_ty Int) (ret_ty Int))))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id g))
           (Apply_thunk (fn ((Key IntT) (Id f)))
            (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id g))) (arg ((Id $))) (arg_ty Int) (ty Int)
            (loc ((line 4) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 10)))))))
            (bind ()) (loc ((line 4) (column 10)))))))
        (loc ((line 4) (column 0)))))
      (Values
       ((exprs
         ((((Shadow 1) (Id g))
           (Apply_thunk (fn ((Key BoolT) (Id f)))
            (ty (Closure (arg_ty Bool) (ret_ty Bool)))
            (loc ((line 5) (column 8)))))))
        (bind ()) (loc ((line 5) (column 0)))))
      (Values
       ((exprs
         ((((Shadow 1) (Id _))
           (Apply_closure (fn ((Shadow 1) (Id g))) (arg ((Id $))) (arg_ty Bool)
            (ty Bool) (loc ((line 6) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value (Bool true)) (ty Bool)
                (loc ((line 6) (column 10)))))))
            (bind ()) (loc ((line 6) (column 10)))))))
        (loc ((line 6) (column 0)))))))
    |}]
;;

let%expect_test "Dependent lambda" =
  go
    {|
let f = fn (static g : int -> int) -> g 0;;
let _ = f (fn (x : int) -> x + 1);;
|};
  [%expect {|
    (lst
     ((Closure_body
       (path ((Id "\206\187") (Id g) (Key (Closure 1)) (Id "\206\187") (Id f)))
       (arg ((Id x))) (arg_ty Int) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id $))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 31)))))))
           (bind ()) (loc ((line 3) (column 31)))))))
       (return
        (Binop (op Add) (lhs ((Id x))) (rhs ((Id $))) (ty Int)
         (loc ((line 3) (column 29)))))
       (loc ((line 2) (column 8))))
      (Thunk_body (path ((Key (Closure 1)) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id g) (Key (Closure 1)) (Id "\206\187") (Id f))
              (Make_closure
               (body
                ((Id "\206\187") (Id g) (Key (Closure 1)) (Id "\206\187") (Id f)))
               (env (((Id env) (Id g) (Key (Closure 1)) (Id "\206\187") (Id f))))
               (ty (Closure (arg_ty Int) (ret_ty Int)))
               (loc ((line 2) (column 8)))))))
           (bind
            ((Values
              ((exprs
                ((((Id env) (Id g) (Key (Closure 1)) (Id "\206\187") (Id f))
                  (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                   (loc ((line 2) (column 8)))))))
               (bind ()) (loc ((line 2) (column 8)))))))
           (loc ((line 2) (column 8)))))
         (Values
          ((exprs
            ((((Id $))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 40)))))))
           (bind ()) (loc ((line 2) (column 40)))))))
       (return
        (Apply_closure (fn ((Id g) (Key (Closure 1)) (Id "\206\187") (Id f)))
         (arg ((Id $))) (arg_ty Int) (ty Int) (loc ((line 2) (column 38)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Closure 1)) (Id f))
           (Make_closure (body ((Key (Closure 1)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Int))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Closure 1)) (Id f))) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "dependent unit" =
  go
    {|
let f = fn (static x : unit) -> ();;
let _ = f ();;
|};
  [%expect {|
    (lst
     ((Thunk_body (path ((Key Unit) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key Unit) (Id "\206\187") (Id f))
              (Scalar (value Unit) (ty Unit) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))))
       (return (Scalar (value Unit) (ty Unit) (loc ((line 2) (column 32)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key Unit) (Id f))
           (Make_closure (body ((Key Unit) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Unit))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key Unit) (Id f))) (ty Unit)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "dependent bool" =
  go
    {|
let f = fn (static x : bool) -> !x;;
let _ = f true;;
|};
  [%expect {|
    (lst
     ((Thunk_body (path ((Key (Bool true)) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Bool true)) (Id "\206\187") (Id f))
              (Scalar (value (Bool true)) (ty Bool) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))))
       (return
        (Unop (op Not) (arg ((Id x) (Key (Bool true)) (Id "\206\187") (Id f)))
         (ty Bool) (loc ((line 2) (column 32)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Bool true)) (Id f))
           (Make_closure (body ((Key (Bool true)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Bool))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Bool true)) (Id f))) (ty Bool)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "dependent int" =
  go
    {|
let f = fn (static x : int) -> -x;;
let _ = f 1;;
|};
  [%expect {|
    (lst
     ((Thunk_body (path ((Key (Int 1)) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 1)) (Id "\206\187") (Id f))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))))
       (return
        (Unop (op Neg) (arg ((Id x) (Key (Int 1)) (Id "\206\187") (Id f)))
         (ty Int) (loc ((line 2) (column 31)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Int 1)) (Id f))
           (Make_closure (body ((Key (Int 1)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Int))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Int 1)) (Id f))) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "dependent type" =
  go
    {|
let f = fn (static erased t : type) -> fn (x : t) -> if true then x else x;;
|};
  [%expect {|
    (lst
     ((Values
       ((exprs ())
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "arrow typechecking" =
  go
    {|
let f = fn (g : int -> int) -> g 0;;
let _ = f (fn (x : int) -> 0);;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id f))) (arg ((Id g)))
       (arg_ty (Closure (arg_ty Int) (ret_ty Int))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id $))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 33)))))))
           (bind ()) (loc ((line 2) (column 33)))))))
       (return
        (Apply_closure (fn ((Id g))) (arg ((Id $))) (arg_ty Int) (ty Int)
         (loc ((line 2) (column 31)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Id f))
           (Make_closure (body ((Id "\206\187") (Id f)))
            (env (((Id env) (Id f))))
            (ty
             (Closure (arg_ty (Closure (arg_ty Int) (ret_ty Int))) (ret_ty Int)))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Closure_body (path ((Id "\206\187") (Id _))) (arg ((Id x))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 27)))))
       (loc ((line 3) (column 11))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id f))) (arg ((Id $)))
            (arg_ty (Closure (arg_ty Int) (ret_ty Int))) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id _))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 11)))))))
            (bind ()) (loc ((line 3) (column 11)))))
          (Values
           ((exprs
             ((((Id $))
               (Make_closure (body ((Id "\206\187") (Id _)))
                (env (((Id env) (Id _))))
                (ty (Closure (arg_ty Int) (ret_ty Int)))
                (loc ((line 3) (column 11)))))))
            (bind ()) (loc ((line 3) (column 11)))))))
        (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "arrow typechecking" =
  go
    {|
let f = fn (static g : static int -> int) -> g 0;;
let _ = f (fn (static x : int) -> 0);;
|};
  [%expect {|
    (lst
     ((Thunk_body
       (path
        ((Key (Int 0)) (Id "\206\187") (Id g) (Key (Closure 4)) (Id "\206\187")
         (Id f)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 0)) (Id "\206\187") (Id g) (Key (Closure 4))
               (Id "\206\187") (Id f))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))))
       (return (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 34)))))
       (loc ((line 2) (column 8))))
      (Thunk_body (path ((Key (Closure 4)) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Key (Int 0)) (Id g) (Key (Closure 4)) (Id "\206\187") (Id f))
              (Make_closure
               (body
                ((Key (Int 0)) (Id "\206\187") (Id g) (Key (Closure 4))
                 (Id "\206\187") (Id f)))
               (env (((Id env) (Id g) (Key (Closure 4)) (Id "\206\187") (Id f))))
               (ty (Thunk Int)) (loc ((line 2) (column 8)))))))
           (bind
            ((Values
              ((exprs
                ((((Id env) (Id g) (Key (Closure 4)) (Id "\206\187") (Id f))
                  (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                   (loc ((line 2) (column 8)))))))
               (bind ()) (loc ((line 2) (column 8)))))))
           (loc ((line 2) (column 8)))))))
       (return
        (Apply_thunk
         (fn ((Key (Int 0)) (Id g) (Key (Closure 4)) (Id "\206\187") (Id f)))
         (ty Int) (loc ((line 2) (column 45)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Closure 4)) (Id f))
           (Make_closure (body ((Key (Closure 4)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Int))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Closure 4)) (Id f))) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "arrow typechecking" =
  go
    {|
let f = fn (static g : static int -> int) -> g 0;;
let _ = f (fn (x : int) -> 0);;
|};
  [%expect {|
    (lst
     ((Closure_body
       (path ((Id "\206\187") (Id g) (Key (Closure 3)) (Id "\206\187") (Id f)))
       (arg ((Id x))) (arg_ty Int) (captures ()) (bind ())
       (return (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 27)))))
       (loc ((line 2) (column 8))))
      (Thunk_body (path ((Key (Closure 3)) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id g) (Key (Closure 3)) (Id "\206\187") (Id f))
              (Make_closure
               (body
                ((Id "\206\187") (Id g) (Key (Closure 3)) (Id "\206\187") (Id f)))
               (env (((Id env) (Id g) (Key (Closure 3)) (Id "\206\187") (Id f))))
               (ty (Closure (arg_ty Int) (ret_ty Int)))
               (loc ((line 2) (column 8)))))))
           (bind
            ((Values
              ((exprs
                ((((Id env) (Id g) (Key (Closure 3)) (Id "\206\187") (Id f))
                  (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                   (loc ((line 2) (column 8)))))))
               (bind ()) (loc ((line 2) (column 8)))))))
           (loc ((line 2) (column 8)))))
         (Values
          ((exprs
            ((((Id $))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 47)))))))
           (bind ()) (loc ((line 2) (column 47)))))))
       (return
        (Apply_closure (fn ((Id g) (Key (Closure 3)) (Id "\206\187") (Id f)))
         (arg ((Id $))) (arg_ty Int) (ty Int) (loc ((line 2) (column 45)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Closure 3)) (Id f))
           (Make_closure (body ((Key (Closure 3)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Int))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Closure 3)) (Id f))) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (static x : int) -> x else fn (x : int) -> x;;
|};
  [%expect {|
    (lst
     ((Values
       ((exprs ())
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id _))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 21)))))))
            (bind ()) (loc ((line 2) (column 21)))))))
        (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (static x : int) -> x else fn (erased x : int) -> x;;
|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (static erased x : int) -> x else fn (x : int) -> x;;
|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (erased x : int) -> x else fn (x : int) -> x;;
|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (x : int) -> x else fn (static x : int) -> x;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id _))) (arg ((Id x))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 2) (column 37)))))
       (loc ((line 2) (column 21))))
      (Values
       ((exprs
         ((((Id _))
           (Make_closure (body ((Id "\206\187") (Id _)))
            (env (((Id env) (Id _)))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 2) (column 21)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id _))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 21)))))))
            (bind ()) (loc ((line 2) (column 21)))))))
        (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (x : int) -> x else fn (erased x : int) -> x;;
|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (erased x : int) -> x else fn (static x : int) -> x;;
|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (x : int) -> x else fn (static erased x : int) -> x;;
|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "return erased" =
  go
    {|
let f = fn (x : int) -> 0 @ erased;;
let _ = f 1;;
|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "pi typechecking" =
  go
    {|
  let f = fn (static g : erased int -> int) -> g 0;;
  let _ = f (fn (erased x : int) -> 0);;
  |};
  [%expect {|
    (lst
     ((Closure_body
       (path ((Id "\206\187") (Id g) (Key (Closure 1)) (Id "\206\187") (Id f)))
       (arg ((Id x))) (arg_ty Int) (captures ()) (bind ())
       (return (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 36)))))
       (loc ((line 2) (column 10))))
      (Thunk_body (path ((Key (Closure 1)) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id g) (Key (Closure 1)) (Id "\206\187") (Id f))
              (Make_closure
               (body
                ((Id "\206\187") (Id g) (Key (Closure 1)) (Id "\206\187") (Id f)))
               (env (((Id env) (Id g) (Key (Closure 1)) (Id "\206\187") (Id f))))
               (ty (Closure (arg_ty Int) (ret_ty Int)))
               (loc ((line 2) (column 10)))))))
           (bind
            ((Values
              ((exprs
                ((((Id env) (Id g) (Key (Closure 1)) (Id "\206\187") (Id f))
                  (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                   (loc ((line 2) (column 10)))))))
               (bind ()) (loc ((line 2) (column 10)))))))
           (loc ((line 2) (column 10)))))
         (Values
          ((exprs
            ((((Id $))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 49)))))))
           (bind ()) (loc ((line 2) (column 49)))))))
       (return
        (Apply_closure (fn ((Id g) (Key (Closure 1)) (Id "\206\187") (Id f)))
         (arg ((Id $))) (arg_ty Int) (ty Int) (loc ((line 2) (column 47)))))
       (loc ((line 2) (column 10))))
      (Values
       ((exprs
         ((((Key (Closure 1)) (Id f))
           (Make_closure (body ((Key (Closure 1)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Int))
            (loc ((line 2) (column 10)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 10)))))))
            (bind ()) (loc ((line 2) (column 10)))))))
        (loc ((line 2) (column 2)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Closure 1)) (Id f))) (ty Int)
            (loc ((line 3) (column 10)))))))
        (bind ()) (loc ((line 3) (column 2)))))))
    |}]
;;

let%expect_test "arrow-pi typechecking" =
  go
    {|
let f = fn (static g : static int -> int) -> g 1;;
let _ = f (fn (x : int) -> 0);;
|};
  [%expect {|
    (lst
     ((Closure_body
       (path ((Id "\206\187") (Id g) (Key (Closure 3)) (Id "\206\187") (Id f)))
       (arg ((Id x))) (arg_ty Int) (captures ()) (bind ())
       (return (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 27)))))
       (loc ((line 2) (column 8))))
      (Thunk_body (path ((Key (Closure 3)) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id g) (Key (Closure 3)) (Id "\206\187") (Id f))
              (Make_closure
               (body
                ((Id "\206\187") (Id g) (Key (Closure 3)) (Id "\206\187") (Id f)))
               (env (((Id env) (Id g) (Key (Closure 3)) (Id "\206\187") (Id f))))
               (ty (Closure (arg_ty Int) (ret_ty Int)))
               (loc ((line 2) (column 8)))))))
           (bind
            ((Values
              ((exprs
                ((((Id env) (Id g) (Key (Closure 3)) (Id "\206\187") (Id f))
                  (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                   (loc ((line 2) (column 8)))))))
               (bind ()) (loc ((line 2) (column 8)))))))
           (loc ((line 2) (column 8)))))
         (Values
          ((exprs
            ((((Id $))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 47)))))))
           (bind ()) (loc ((line 2) (column 47)))))))
       (return
        (Apply_closure (fn ((Id g) (Key (Closure 3)) (Id "\206\187") (Id f)))
         (arg ((Id $))) (arg_ty Int) (ty Int) (loc ((line 2) (column 45)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Closure 3)) (Id f))
           (Make_closure (body ((Key (Closure 3)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Int))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Closure 3)) (Id f))) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "Pi typechecking" =
  go
    {|
let f = fn (static erased g : static int -> int) -> g 0;;
let _ = f (fn (static x : int) -> x + 1);;
|};
  [%expect {|
    (lst
     ((Thunk_body (path ((Key (Closure 4)) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Closure 4)) (Id "\206\187") (Id f))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 54)))))))
           (bind ()) (loc ((line 2) (column 52)))))
         (Values
          ((exprs
            ((((Id $))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 38)))))))
           (bind ()) (loc ((line 3) (column 38)))))))
       (return
        (Binop (op Add) (lhs ((Id x) (Key (Closure 4)) (Id "\206\187") (Id f)))
         (rhs ((Id $))) (ty Int) (loc ((line 3) (column 36)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Closure 4)) (Id f))
           (Make_closure (body ((Key (Closure 4)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Int))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Closure 4)) (Id f))) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "dependent lambda" =
  go
    {|
let f = fn (static g : static erased type -> int -> int) -> g int;;
let _ = f (fn (static erased t : type) -> fn (x : int) -> x);;
|};
  [%expect {|
    (lst
     ((Closure_body
       (path
        ((Id "\206\187") (Key IntT) (Id "\206\187") (Id g) (Key (Closure 4))
         (Id "\206\187") (Id f)))
       (arg ((Id x))) (arg_ty Int) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 3) (column 58)))))
       (loc ((line 3) (column 42))))
      (Thunk_body
       (path
        ((Key IntT) (Id "\206\187") (Id g) (Key (Closure 4)) (Id "\206\187")
         (Id f)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Key IntT) (Id "\206\187") (Id g) (Key (Closure 4))
               (Id "\206\187") (Id f))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 3) (column 42)))))))
           (bind ()) (loc ((line 3) (column 42)))))))
       (return
        (Make_closure
         (body
          ((Id "\206\187") (Key IntT) (Id "\206\187") (Id g) (Key (Closure 4))
           (Id "\206\187") (Id f)))
         (env
          (((Id env) (Key IntT) (Id "\206\187") (Id g) (Key (Closure 4))
            (Id "\206\187") (Id f))))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 3) (column 42)))))
       (loc ((line 2) (column 8))))
      (Thunk_body (path ((Key (Closure 4)) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Key IntT) (Id g) (Key (Closure 4)) (Id "\206\187") (Id f))
              (Make_closure
               (body
                ((Key IntT) (Id "\206\187") (Id g) (Key (Closure 4))
                 (Id "\206\187") (Id f)))
               (env (((Id env) (Id g) (Key (Closure 4)) (Id "\206\187") (Id f))))
               (ty (Thunk (Closure (arg_ty Int) (ret_ty Int))))
               (loc ((line 2) (column 8)))))))
           (bind
            ((Values
              ((exprs
                ((((Id env) (Id g) (Key (Closure 4)) (Id "\206\187") (Id f))
                  (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                   (loc ((line 2) (column 8)))))))
               (bind ()) (loc ((line 2) (column 8)))))))
           (loc ((line 2) (column 8)))))))
       (return
        (Apply_thunk
         (fn ((Key IntT) (Id g) (Key (Closure 4)) (Id "\206\187") (Id f)))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 2) (column 60)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Closure 4)) (Id f))
           (Make_closure (body ((Key (Closure 4)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f))))
            (ty (Thunk (Closure (arg_ty Int) (ret_ty Int))))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Closure 4)) (Id f)))
            (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "dependent fn" =
  go
    {|
let id = fn (static erased t : type) -> (fn (x : t) -> x);;
let x = (id int) (0 @ dynamic);;
let y = (id bool) (true @ dynamic);;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Key BoolT) (Id "\206\187") (Id id)))
       (arg ((Id x))) (arg_ty Bool) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Bool) (loc ((line 2) (column 55)))))
       (loc ((line 2) (column 41))))
      (Thunk_body (path ((Key BoolT) (Id "\206\187") (Id id))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Key BoolT) (Id "\206\187") (Id id))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 2) (column 41)))))))
           (bind ()) (loc ((line 2) (column 41)))))))
       (return
        (Make_closure
         (body ((Id "\206\187") (Key BoolT) (Id "\206\187") (Id id)))
         (env (((Id env) (Key BoolT) (Id "\206\187") (Id id))))
         (ty (Closure (arg_ty Bool) (ret_ty Bool))) (loc ((line 2) (column 41)))))
       (loc ((line 2) (column 9))))
      (Closure_body (path ((Id "\206\187") (Key IntT) (Id "\206\187") (Id id)))
       (arg ((Id x))) (arg_ty Int) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 2) (column 55)))))
       (loc ((line 2) (column 41))))
      (Thunk_body (path ((Key IntT) (Id "\206\187") (Id id))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Key IntT) (Id "\206\187") (Id id))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 2) (column 41)))))))
           (bind ()) (loc ((line 2) (column 41)))))))
       (return
        (Make_closure (body ((Id "\206\187") (Key IntT) (Id "\206\187") (Id id)))
         (env (((Id env) (Key IntT) (Id "\206\187") (Id id))))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 2) (column 41)))))
       (loc ((line 2) (column 9))))
      (Values
       ((exprs
         ((((Key BoolT) (Id id))
           (Make_closure (body ((Key BoolT) (Id "\206\187") (Id id)))
            (env (((Id env) (Id id))))
            (ty (Thunk (Closure (arg_ty Bool) (ret_ty Bool))))
            (loc ((line 2) (column 9)))))
          (((Key IntT) (Id id))
           (Make_closure (body ((Key IntT) (Id "\206\187") (Id id)))
            (env (((Id env) (Id id))))
            (ty (Thunk (Closure (arg_ty Int) (ret_ty Int))))
            (loc ((line 2) (column 9)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id id))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 9)))))))
            (bind ()) (loc ((line 2) (column 9)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id x))
           (Apply_closure (fn ((Id $))) (arg ((Shadow 1) (Id $))) (arg_ty Int)
            (ty Int) (loc ((line 3) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Apply_thunk (fn ((Key IntT) (Id id)))
                (ty (Closure (arg_ty Int) (ret_ty Int)))
                (loc ((line 3) (column 9)))))))
            (bind ()) (loc ((line 3) (column 9)))))
          (Values
           ((exprs
             ((((Shadow 1) (Id $))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 18)))))))
            (bind ()) (loc ((line 3) (column 18)))))))
        (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id y))
           (Apply_closure (fn ((Id $))) (arg ((Shadow 1) (Id $))) (arg_ty Bool)
            (ty Bool) (loc ((line 4) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Apply_thunk (fn ((Key BoolT) (Id id)))
                (ty (Closure (arg_ty Bool) (ret_ty Bool)))
                (loc ((line 4) (column 9)))))))
            (bind ()) (loc ((line 4) (column 9)))))
          (Values
           ((exprs
             ((((Shadow 1) (Id $))
               (Scalar (value (Bool true)) (ty Bool)
                (loc ((line 4) (column 19)))))))
            (bind ()) (loc ((line 4) (column 19)))))))
        (loc ((line 4) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id x))
           (Ident (path ((Id x) (Id x))) (ty Int) (loc ((line 2) (column 55)))))))
        (bind
         ((Values
           ((exprs
             ((((Id x) (Id x))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 18)))))))
            (bind ()) (loc ((line 3) (column 8)))))))
        (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id y))
           (Ident (path ((Id x) (Id y))) (ty Bool) (loc ((line 2) (column 55)))))))
        (bind
         ((Values
           ((exprs
             ((((Id x) (Id y))
               (Scalar (value (Bool true)) (ty Bool)
                (loc ((line 4) (column 19)))))))
            (bind ()) (loc ((line 4) (column 8)))))))
        (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "dependent arrow" =
  go
    {|
let mk_int = fn (static x : int) -> int;;
let apply_int = fn (static f : static int \ x -> mk_int x) -> 2;;
|};
  [%expect {|
    (lst
     ((Values
       ((exprs ())
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id apply_int))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 16)))))))
            (bind ()) (loc ((line 3) (column 16)))))))
        (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "dependent arrow" =
  go
    {|
let mk_int = fn (static x : int) -> int;;
let apply_int = fn (static f : static int \ x -> unit -> mk_int x) -> f 2;;
|};
  [%expect {|
    (lst
     ((Values
       ((exprs ())
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id apply_int))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 16)))))))
            (bind ()) (loc ((line 3) (column 16)))))))
        (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "dependent arrow" =
  go
    {|
let apply_int = fn (static f : static erased type \ t -> t -> t) -> f int;;
let _ = apply_int (fn (static erased t : type) -> fn (x : t) -> x);;
|};
  [%expect {|
    (lst
     ((Closure_body
       (path
        ((Id "\206\187") (Key IntT) (Id "\206\187") (Id f) (Key (Closure 4))
         (Id "\206\187") (Id apply_int)))
       (arg ((Id x))) (arg_ty Int) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 3) (column 64)))))
       (loc ((line 3) (column 50))))
      (Thunk_body
       (path
        ((Key IntT) (Id "\206\187") (Id f) (Key (Closure 4)) (Id "\206\187")
         (Id apply_int)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Key IntT) (Id "\206\187") (Id f) (Key (Closure 4))
               (Id "\206\187") (Id apply_int))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 3) (column 50)))))))
           (bind ()) (loc ((line 3) (column 50)))))))
       (return
        (Make_closure
         (body
          ((Id "\206\187") (Key IntT) (Id "\206\187") (Id f) (Key (Closure 4))
           (Id "\206\187") (Id apply_int)))
         (env
          (((Id env) (Key IntT) (Id "\206\187") (Id f) (Key (Closure 4))
            (Id "\206\187") (Id apply_int))))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 3) (column 50)))))
       (loc ((line 2) (column 16))))
      (Thunk_body (path ((Key (Closure 4)) (Id "\206\187") (Id apply_int)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Key IntT) (Id f) (Key (Closure 4)) (Id "\206\187")
               (Id apply_int))
              (Make_closure
               (body
                ((Key IntT) (Id "\206\187") (Id f) (Key (Closure 4))
                 (Id "\206\187") (Id apply_int)))
               (env
                (((Id env) (Id f) (Key (Closure 4)) (Id "\206\187")
                  (Id apply_int))))
               (ty (Thunk (Closure (arg_ty Int) (ret_ty Int))))
               (loc ((line 2) (column 16)))))))
           (bind
            ((Values
              ((exprs
                ((((Id env) (Id f) (Key (Closure 4)) (Id "\206\187")
                   (Id apply_int))
                  (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                   (loc ((line 2) (column 16)))))))
               (bind ()) (loc ((line 2) (column 16)))))))
           (loc ((line 2) (column 16)))))))
       (return
        (Apply_thunk
         (fn
          ((Key IntT) (Id f) (Key (Closure 4)) (Id "\206\187") (Id apply_int)))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 2) (column 68)))))
       (loc ((line 2) (column 16))))
      (Values
       ((exprs
         ((((Key (Closure 4)) (Id apply_int))
           (Make_closure
            (body ((Key (Closure 4)) (Id "\206\187") (Id apply_int)))
            (env (((Id env) (Id apply_int))))
            (ty (Thunk (Closure (arg_ty Int) (ret_ty Int))))
            (loc ((line 2) (column 16)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id apply_int))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 16)))))))
            (bind ()) (loc ((line 2) (column 16)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Closure 4)) (Id apply_int)))
            (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
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
    (lst
     ((Closure_body
       (path
        ((Id "\206\187") (Key BoolT) (Id "\206\187") (Id f) (Key (Closure 7))
         (Id "\206\187") (Id apply)))
       (arg ((Id x))) (arg_ty Bool) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Bool) (loc ((line 3) (column 60)))))
       (loc ((line 3) (column 46))))
      (Thunk_body
       (path
        ((Key BoolT) (Id "\206\187") (Id f) (Key (Closure 7)) (Id "\206\187")
         (Id apply)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Key BoolT) (Id "\206\187") (Id f) (Key (Closure 7))
               (Id "\206\187") (Id apply))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 3) (column 46)))))))
           (bind ()) (loc ((line 3) (column 46)))))))
       (return
        (Make_closure
         (body
          ((Id "\206\187") (Key BoolT) (Id "\206\187") (Id f) (Key (Closure 7))
           (Id "\206\187") (Id apply)))
         (env
          (((Id env) (Key BoolT) (Id "\206\187") (Id f) (Key (Closure 7))
            (Id "\206\187") (Id apply))))
         (ty (Closure (arg_ty Bool) (ret_ty Bool))) (loc ((line 3) (column 46)))))
       (loc ((line 2) (column 12))))
      (Closure_body
       (path
        ((Id "\206\187") (Key IntT) (Id "\206\187") (Id f) (Key (Closure 7))
         (Id "\206\187") (Id apply)))
       (arg ((Id x))) (arg_ty Int) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 3) (column 60)))))
       (loc ((line 3) (column 46))))
      (Thunk_body
       (path
        ((Key IntT) (Id "\206\187") (Id f) (Key (Closure 7)) (Id "\206\187")
         (Id apply)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Key IntT) (Id "\206\187") (Id f) (Key (Closure 7))
               (Id "\206\187") (Id apply))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 3) (column 46)))))))
           (bind ()) (loc ((line 3) (column 46)))))))
       (return
        (Make_closure
         (body
          ((Id "\206\187") (Key IntT) (Id "\206\187") (Id f) (Key (Closure 7))
           (Id "\206\187") (Id apply)))
         (env
          (((Id env) (Key IntT) (Id "\206\187") (Id f) (Key (Closure 7))
            (Id "\206\187") (Id apply))))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 3) (column 46)))))
       (loc ((line 2) (column 12))))
      (Thunk_body
       (path
        ((Key BoolT) (Id "\206\187") (Key (Closure 7)) (Id "\206\187")
         (Id apply)))
       (captures
        (((path ((Key BoolT) (Id f)))
          (ty (Thunk (Closure (arg_ty Bool) (ret_ty Bool))))
          (offset_in_bytes 16))))
       (bind ())
       (return
        (Apply_thunk (fn ((Key BoolT) (Id f)))
         (ty (Closure (arg_ty Bool) (ret_ty Bool))) (loc ((line 2) (column 96)))))
       (loc ((line 2) (column 64))))
      (Thunk_body
       (path
        ((Key IntT) (Id "\206\187") (Key (Closure 7)) (Id "\206\187") (Id apply)))
       (captures
        (((path ((Key IntT) (Id f)))
          (ty (Thunk (Closure (arg_ty Int) (ret_ty Int)))) (offset_in_bytes 0))))
       (bind ())
       (return
        (Apply_thunk (fn ((Key IntT) (Id f)))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 2) (column 96)))))
       (loc ((line 2) (column 64))))
      (Thunk_body
       (path ((Key BoolT) (Key (Closure 7)) (Id "\206\187") (Id apply)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Key BoolT) (Id f) (Key (Closure 7)) (Id "\206\187") (Id apply))
              (Make_closure
               (body
                ((Key BoolT) (Id "\206\187") (Id f) (Key (Closure 7))
                 (Id "\206\187") (Id apply)))
               (env
                (((Id env) (Id f) (Key (Closure 7)) (Id "\206\187") (Id apply))))
               (ty (Thunk (Closure (arg_ty Bool) (ret_ty Bool))))
               (loc ((line 2) (column 12)))))
             (((Key IntT) (Id f) (Key (Closure 7)) (Id "\206\187") (Id apply))
              (Make_closure
               (body
                ((Key IntT) (Id "\206\187") (Id f) (Key (Closure 7))
                 (Id "\206\187") (Id apply)))
               (env
                (((Id env) (Id f) (Key (Closure 7)) (Id "\206\187") (Id apply))))
               (ty (Thunk (Closure (arg_ty Int) (ret_ty Int))))
               (loc ((line 2) (column 12)))))))
           (bind
            ((Values
              ((exprs
                ((((Id env) (Id f) (Key (Closure 7)) (Id "\206\187") (Id apply))
                  (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                   (loc ((line 2) (column 12)))))))
               (bind ()) (loc ((line 2) (column 12)))))))
           (loc ((line 2) (column 12)))))
         (Values
          ((exprs
            ((((Id env) (Key (Closure 7)) (Id "\206\187") (Id apply))
              (Make_env
               (captures
                ((entries
                  (((path
                     ((Key IntT) (Id f) (Key (Closure 7)) (Id "\206\187")
                      (Id apply)))
                    (ty (Thunk (Closure (arg_ty Int) (ret_ty Int))))
                    (offset_in_bytes 0))
                   ((path
                     ((Key BoolT) (Id f) (Key (Closure 7)) (Id "\206\187")
                      (Id apply)))
                    (ty (Thunk (Closure (arg_ty Bool) (ret_ty Bool))))
                    (offset_in_bytes 16))))
                 (size_in_bytes 32)))
               (ty Env) (loc ((line 2) (column 64)))))))
           (bind ()) (loc ((line 2) (column 64)))))))
       (return
        (Make_closure
         (body
          ((Key BoolT) (Id "\206\187") (Key (Closure 7)) (Id "\206\187")
           (Id apply)))
         (env (((Id env) (Key (Closure 7)) (Id "\206\187") (Id apply))))
         (ty (Thunk (Closure (arg_ty Bool) (ret_ty Bool))))
         (loc ((line 2) (column 64)))))
       (loc ((line 2) (column 12))))
      (Thunk_body
       (path ((Key IntT) (Key (Closure 7)) (Id "\206\187") (Id apply)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Key BoolT) (Id f) (Key (Closure 7)) (Id "\206\187") (Id apply))
              (Make_closure
               (body
                ((Key BoolT) (Id "\206\187") (Id f) (Key (Closure 7))
                 (Id "\206\187") (Id apply)))
               (env
                (((Id env) (Id f) (Key (Closure 7)) (Id "\206\187") (Id apply))))
               (ty (Thunk (Closure (arg_ty Bool) (ret_ty Bool))))
               (loc ((line 2) (column 12)))))
             (((Key IntT) (Id f) (Key (Closure 7)) (Id "\206\187") (Id apply))
              (Make_closure
               (body
                ((Key IntT) (Id "\206\187") (Id f) (Key (Closure 7))
                 (Id "\206\187") (Id apply)))
               (env
                (((Id env) (Id f) (Key (Closure 7)) (Id "\206\187") (Id apply))))
               (ty (Thunk (Closure (arg_ty Int) (ret_ty Int))))
               (loc ((line 2) (column 12)))))))
           (bind
            ((Values
              ((exprs
                ((((Id env) (Id f) (Key (Closure 7)) (Id "\206\187") (Id apply))
                  (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                   (loc ((line 2) (column 12)))))))
               (bind ()) (loc ((line 2) (column 12)))))))
           (loc ((line 2) (column 12)))))
         (Values
          ((exprs
            ((((Id env) (Key (Closure 7)) (Id "\206\187") (Id apply))
              (Make_env
               (captures
                ((entries
                  (((path
                     ((Key IntT) (Id f) (Key (Closure 7)) (Id "\206\187")
                      (Id apply)))
                    (ty (Thunk (Closure (arg_ty Int) (ret_ty Int))))
                    (offset_in_bytes 0))
                   ((path
                     ((Key BoolT) (Id f) (Key (Closure 7)) (Id "\206\187")
                      (Id apply)))
                    (ty (Thunk (Closure (arg_ty Bool) (ret_ty Bool))))
                    (offset_in_bytes 16))))
                 (size_in_bytes 32)))
               (ty Env) (loc ((line 2) (column 64)))))))
           (bind ()) (loc ((line 2) (column 64)))))))
       (return
        (Make_closure
         (body
          ((Key IntT) (Id "\206\187") (Key (Closure 7)) (Id "\206\187")
           (Id apply)))
         (env (((Id env) (Key (Closure 7)) (Id "\206\187") (Id apply))))
         (ty (Thunk (Closure (arg_ty Int) (ret_ty Int))))
         (loc ((line 2) (column 64)))))
       (loc ((line 2) (column 12))))
      (Values
       ((exprs
         ((((Key BoolT) (Key (Closure 7)) (Id apply))
           (Make_closure
            (body ((Key BoolT) (Key (Closure 7)) (Id "\206\187") (Id apply)))
            (env (((Id env) (Id apply))))
            (ty (Thunk (Thunk (Closure (arg_ty Bool) (ret_ty Bool)))))
            (loc ((line 2) (column 12)))))
          (((Key IntT) (Key (Closure 7)) (Id apply))
           (Make_closure
            (body ((Key IntT) (Key (Closure 7)) (Id "\206\187") (Id apply)))
            (env (((Id env) (Id apply))))
            (ty (Thunk (Thunk (Closure (arg_ty Int) (ret_ty Int)))))
            (loc ((line 2) (column 12)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id apply))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 12)))))))
            (bind ()) (loc ((line 2) (column 12)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Key BoolT) (Id f))
           (Apply_thunk (fn ((Key BoolT) (Key (Closure 7)) (Id apply)))
            (ty (Thunk (Closure (arg_ty Bool) (ret_ty Bool))))
            (loc ((line 3) (column 8)))))
          (((Key IntT) (Id f))
           (Apply_thunk (fn ((Key IntT) (Key (Closure 7)) (Id apply)))
            (ty (Thunk (Closure (arg_ty Int) (ret_ty Int))))
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id g))
           (Apply_thunk (fn ((Key IntT) (Id f)))
            (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 4) (column 8)))))))
        (bind ()) (loc ((line 4) (column 0)))))
      (Values
       ((exprs
         ((((Id h))
           (Apply_thunk (fn ((Key BoolT) (Id f)))
            (ty (Closure (arg_ty Bool) (ret_ty Bool)))
            (loc ((line 5) (column 8)))))))
        (bind ()) (loc ((line 5) (column 0)))))))
    |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else true;;
let _ = f 0;;
let _ = f 1;;
|};
  [%expect {|
    (lst
     ((Thunk_body (path ((Key (Int 1)) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 1)) (Id "\206\187") (Id f))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))))
       (return
        (Scalar (value (Bool true)) (ty Bool) (loc ((line 2) (column 60)))))
       (loc ((line 2) (column 8))))
      (Thunk_body (path ((Key (Int 0)) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 0)) (Id "\206\187") (Id f))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))))
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 53)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Int 1)) (Id f))
           (Make_closure (body ((Key (Int 1)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Bool))
            (loc ((line 2) (column 8)))))
          (((Key (Int 0)) (Id f))
           (Make_closure (body ((Key (Int 0)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Int))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Int 0)) (Id f))) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Shadow 1) (Id _))
           (Apply_thunk (fn ((Key (Int 1)) (Id f))) (ty Bool)
            (loc ((line 4) (column 8)))))))
        (bind ()) (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "dependent if unify" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else true;;
let g = fn (static x : int) -> if static x == 0 then true else 2;;
let _ = if true then f 0 else g 1;;
|};
  [%expect {|
    (lst
     ((Thunk_body (path ((Key (Int 0)) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 0)) (Id "\206\187") (Id f))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))))
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 53)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Int 0)) (Id f))
           (Make_closure (body ((Key (Int 0)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Int))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Thunk_body (path ((Key (Int 1)) (Id "\206\187") (Id g))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 1)) (Id "\206\187") (Id g))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 8)))))))
           (bind ()) (loc ((line 3) (column 8)))))))
       (return (Scalar (value (Int 2)) (ty Int) (loc ((line 3) (column 63)))))
       (loc ((line 3) (column 8))))
      (Values
       ((exprs
         ((((Key (Int 1)) (Id g))
           (Make_closure (body ((Key (Int 1)) (Id "\206\187") (Id g)))
            (env (((Id env) (Id g)))) (ty (Thunk Int))
            (loc ((line 3) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id g))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 8)))))))
            (bind ()) (loc ((line 3) (column 8)))))))
        (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Int 0)) (Id f))) (ty Int)
            (loc ((line 4) (column 21)))))))
        (bind ()) (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "Fun recursive dynamic arg" =
  go
    {|
fun f (x : int) : int = f x;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id f))) (arg ((Id x))) (arg_ty Int)
       (captures
        (((path ((Id f))) (ty (Closure (arg_ty Int) (ret_ty Int)))
          (offset_in_bytes 0))))
       (bind ())
       (return
        (Apply_closure (fn ((Id f))) (arg ((Id x))) (arg_ty Int) (ty Int)
         (loc ((line 2) (column 24)))))
       (loc ((line 2) (column 4))))
      (Functions
       ((paths
         ((((Id f)) (Closure (arg_ty Int) (ret_ty Int)) ((Id "\206\187") (Id f)))))
        (captures
         ((entries
           (((path ((Id f))) (ty (Closure (arg_ty Int) (ret_ty Int)))
             (offset_in_bytes 0))))
          (size_in_bytes 16)))
        (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "Fun erased arg" =
  go
    {|
fun f (erased x : int) : erased int = x;;
|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "Fun return static" =
  go
    {|
fun f (x : int) : int = 1;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id f))) (arg ((Id x))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 24)))))
       (loc ((line 2) (column 4))))
      (Functions
       ((paths
         ((((Id f)) (Closure (arg_ty Int) (ret_ty Int)) ((Id "\206\187") (Id f)))))
        (captures ((entries ()) (size_in_bytes 0))) (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "Fun return erased" =
  go
    {|
fun f (x : int) : erased int = 1 @ erased;;
|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "mono fun" =
  go
    {|
fun x (static x : int) : int = x;;
let y = x 0;;
|};
  [%expect {|
    (lst
     ((Thunk_body (path ((Key (Int 0)) (Id "\206\187") (Id x))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 0)) (Id "\206\187") (Id x))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 4)))))))
           (bind ()) (loc ((line 2) (column 4)))))))
       (return
        (Ident (path ((Id x) (Key (Int 0)) (Id "\206\187") (Id x))) (ty Int)
         (loc ((line 2) (column 31)))))
       (loc ((line 2) (column 4))))
      (Functions
       ((paths
         ((((Key (Int 0)) (Id x)) (Thunk Int)
           ((Key (Int 0)) (Id "\206\187") (Id x)))))
        (captures ((entries ()) (size_in_bytes 0))) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id y))
           (Apply_thunk (fn ((Key (Int 0)) (Id x))) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id y))
           (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "types are erased" =
  go
    {|
fun f (static _ : unit) : static erased type = int;;
let y = (f () @ dynamic);;
|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "types are erased" =
  go
    {|
fun f (static _ : unit) : static erased type = int;;
let y = 5 : f ();;
|};
  [%expect
    {|
    (lst
     ((Values
       ((exprs
         ((((Id y))
           (Scalar (value (Int 5)) (ty Int) (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "dependent fun " =
  go
    {|
fun id (static erased t : type) : t -> t = fn (x : t) -> x;;
let i = id int;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Key IntT) (Id "\206\187") (Id id)))
       (arg ((Id x))) (arg_ty Int) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 2) (column 57)))))
       (loc ((line 2) (column 43))))
      (Thunk_body (path ((Key IntT) (Id "\206\187") (Id id))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Key IntT) (Id "\206\187") (Id id))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 2) (column 43)))))))
           (bind ()) (loc ((line 2) (column 43)))))))
       (return
        (Make_closure (body ((Id "\206\187") (Key IntT) (Id "\206\187") (Id id)))
         (env (((Id env) (Key IntT) (Id "\206\187") (Id id))))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 2) (column 43)))))
       (loc ((line 2) (column 4))))
      (Functions
       ((paths
         ((((Key IntT) (Id id)) (Thunk (Closure (arg_ty Int) (ret_ty Int)))
           ((Key IntT) (Id "\206\187") (Id id)))))
        (captures ((entries ()) (size_in_bytes 0))) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id i))
           (Apply_thunk (fn ((Key IntT) (Id id)))
            (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "erased fun " =
  go
    {|
fun id (erased x : int) : erased int = x;;
let _ = id 0;;
|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "dependent fun erased" =
  go
    {|
fun id (static erased t : type) : t -> t = fn (x : t) -> x;;
let x = (id int) (0 @ dynamic);;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Key IntT) (Id "\206\187") (Id id)))
       (arg ((Id x))) (arg_ty Int) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 2) (column 57)))))
       (loc ((line 2) (column 43))))
      (Thunk_body (path ((Key IntT) (Id "\206\187") (Id id))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Key IntT) (Id "\206\187") (Id id))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 2) (column 43)))))))
           (bind ()) (loc ((line 2) (column 43)))))))
       (return
        (Make_closure (body ((Id "\206\187") (Key IntT) (Id "\206\187") (Id id)))
         (env (((Id env) (Key IntT) (Id "\206\187") (Id id))))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 2) (column 43)))))
       (loc ((line 2) (column 4))))
      (Functions
       ((paths
         ((((Key IntT) (Id id)) (Thunk (Closure (arg_ty Int) (ret_ty Int)))
           ((Key IntT) (Id "\206\187") (Id id)))))
        (captures ((entries ()) (size_in_bytes 0))) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id x))
           (Apply_closure (fn ((Id $))) (arg ((Shadow 1) (Id $))) (arg_ty Int)
            (ty Int) (loc ((line 3) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Apply_thunk (fn ((Key IntT) (Id id)))
                (ty (Closure (arg_ty Int) (ret_ty Int)))
                (loc ((line 3) (column 9)))))))
            (bind ()) (loc ((line 3) (column 9)))))
          (Values
           ((exprs
             ((((Shadow 1) (Id $))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 18)))))))
            (bind ()) (loc ((line 3) (column 18)))))))
        (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "dependent fun" =
  go
    {|
let ty = fn (static _ : unit) -> int -> int;;
fun id (_ : unit) : ty () = fn (x : int) -> x;;
let x = id () 0;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id "\206\187") (Id id)))
       (arg ((Id x))) (arg_ty Int) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 3) (column 44)))))
       (loc ((line 3) (column 28))))
      (Closure_body (path ((Id "\206\187") (Id id))) (arg ((Id _))) (arg_ty Unit)
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Id "\206\187") (Id id))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 3) (column 28)))))))
           (bind ()) (loc ((line 3) (column 28)))))))
       (return
        (Make_closure (body ((Id "\206\187") (Id "\206\187") (Id id)))
         (env (((Id env) (Id "\206\187") (Id id))))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 3) (column 28)))))
       (loc ((line 3) (column 4))))
      (Functions
       ((paths
         ((((Id id))
           (Closure (arg_ty Unit) (ret_ty (Closure (arg_ty Int) (ret_ty Int))))
           ((Id "\206\187") (Id id)))))
        (captures ((entries ()) (size_in_bytes 0))) (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id x))
           (Apply_closure (fn ((Shadow 1) (Id $))) (arg ((Shadow 2) (Id $)))
            (arg_ty Int) (ty Int) (loc ((line 4) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value Unit) (ty Unit) (loc ((line 4) (column 11)))))))
            (bind ()) (loc ((line 4) (column 11)))))
          (Values
           ((exprs
             ((((Shadow 1) (Id $))
               (Apply_closure (fn ((Id id))) (arg ((Id $))) (arg_ty Unit)
                (ty (Closure (arg_ty Int) (ret_ty Int)))
                (loc ((line 4) (column 8)))))))
            (bind ()) (loc ((line 4) (column 8)))))
          (Values
           ((exprs
             ((((Shadow 2) (Id $))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 14)))))))
            (bind ()) (loc ((line 4) (column 14)))))))
        (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "dependent fun" =
  go
    {|
fun id1 (static erased t : type) : t -> t = fn (x : t) -> x;;
fun id2 (static erased t : type) : t -> t = id1 t;;
let x = id2 int (0 @ dynamic);;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Key IntT) (Id "\206\187") (Id id1)))
       (arg ((Id x))) (arg_ty Int) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 2) (column 58)))))
       (loc ((line 2) (column 44))))
      (Thunk_body (path ((Key IntT) (Id "\206\187") (Id id1))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Key IntT) (Id "\206\187") (Id id1))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 2) (column 44)))))))
           (bind ()) (loc ((line 2) (column 44)))))))
       (return
        (Make_closure
         (body ((Id "\206\187") (Key IntT) (Id "\206\187") (Id id1)))
         (env (((Id env) (Key IntT) (Id "\206\187") (Id id1))))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 2) (column 44)))))
       (loc ((line 2) (column 4))))
      (Functions
       ((paths
         ((((Key IntT) (Id id1)) (Thunk (Closure (arg_ty Int) (ret_ty Int)))
           ((Key IntT) (Id "\206\187") (Id id1)))))
        (captures ((entries ()) (size_in_bytes 0))) (loc ((line 2) (column 0)))))
      (Thunk_body (path ((Key IntT) (Id "\206\187") (Id id2)))
       (captures
        (((path ((Key IntT) (Id id1)))
          (ty (Thunk (Closure (arg_ty Int) (ret_ty Int)))) (offset_in_bytes 0))))
       (bind ())
       (return
        (Apply_thunk (fn ((Key IntT) (Id id1)))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 3) (column 44)))))
       (loc ((line 3) (column 4))))
      (Functions
       ((paths
         ((((Key IntT) (Id id2)) (Thunk (Closure (arg_ty Int) (ret_ty Int)))
           ((Key IntT) (Id "\206\187") (Id id2)))))
        (captures
         ((entries
           (((path ((Key IntT) (Id id1)))
             (ty (Thunk (Closure (arg_ty Int) (ret_ty Int))))
             (offset_in_bytes 0))))
          (size_in_bytes 16)))
        (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id x))
           (Apply_closure (fn ((Id $))) (arg ((Shadow 1) (Id $))) (arg_ty Int)
            (ty Int) (loc ((line 4) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Apply_thunk (fn ((Key IntT) (Id id2)))
                (ty (Closure (arg_ty Int) (ret_ty Int)))
                (loc ((line 4) (column 8)))))))
            (bind ()) (loc ((line 4) (column 8)))))
          (Values
           ((exprs
             ((((Shadow 1) (Id $))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 17)))))))
            (bind ()) (loc ((line 4) (column 17)))))))
        (loc ((line 4) (column 0)))))))
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
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id a))) (arg ((Id _))) (arg_ty Unit)
       (captures ()) (bind ())
       (return (Scalar (value Unit) (ty Unit) (loc ((line 2) (column 26)))))
       (loc ((line 2) (column 4))))
      (Functions
       ((paths
         ((((Id a)) (Closure (arg_ty Unit) (ret_ty Unit))
           ((Id "\206\187") (Id a)))))
        (captures ((entries ()) (size_in_bytes 0))) (loc ((line 2) (column 0)))))
      (Closure_body (path ((Id "\206\187") (Id "\206\187") (Id b)))
       (arg ((Id _))) (arg_ty Unit) (captures ()) (bind ())
       (return (Scalar (value Unit) (ty Unit) (loc ((line 3) (column 51)))))
       (loc ((line 3) (column 34))))
      (Closure_body (path ((Id "\206\187") (Id b))) (arg ((Id _))) (arg_ty Unit)
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Id "\206\187") (Id b))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 3) (column 34)))))))
           (bind ()) (loc ((line 3) (column 34)))))))
       (return
        (Make_closure (body ((Id "\206\187") (Id "\206\187") (Id b)))
         (env (((Id env) (Id "\206\187") (Id b))))
         (ty (Closure (arg_ty Unit) (ret_ty Unit))) (loc ((line 3) (column 34)))))
       (loc ((line 3) (column 4))))
      (Functions
       ((paths
         ((((Id b))
           (Closure (arg_ty Unit) (ret_ty (Closure (arg_ty Unit) (ret_ty Unit))))
           ((Id "\206\187") (Id b)))))
        (captures ((entries ()) (size_in_bytes 0))) (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id x))
           (Ident (path ((Id b)))
            (ty
             (Closure (arg_ty Unit)
              (ret_ty (Closure (arg_ty Unit) (ret_ty Unit)))))
            (loc ((line 4) (column 36)))))))
        (bind ()) (loc ((line 4) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Shadow 1) (Id $))) (arg ((Shadow 2) (Id $)))
            (arg_ty Unit) (ty Unit) (loc ((line 5) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value Unit) (ty Unit) (loc ((line 5) (column 10)))))))
            (bind ()) (loc ((line 5) (column 10)))))
          (Values
           ((exprs
             ((((Shadow 1) (Id $))
               (Apply_closure (fn ((Id x))) (arg ((Id $))) (arg_ty Unit)
                (ty (Closure (arg_ty Unit) (ret_ty Unit)))
                (loc ((line 5) (column 8)))))))
            (bind ()) (loc ((line 5) (column 8)))))
          (Values
           ((exprs
             ((((Shadow 2) (Id $))
               (Scalar (value Unit) (ty Unit) (loc ((line 5) (column 13)))))))
            (bind ()) (loc ((line 5) (column 13)))))))
        (loc ((line 5) (column 0)))))))
    |}]
;;

let%expect_test "return fn" =
  go
    {|
fun x (_ : unit) : unit -> unit = fn (_ : unit) -> ();;
let _ = x () ();;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id "\206\187") (Id x)))
       (arg ((Id _))) (arg_ty Unit) (captures ()) (bind ())
       (return (Scalar (value Unit) (ty Unit) (loc ((line 2) (column 51)))))
       (loc ((line 2) (column 34))))
      (Closure_body (path ((Id "\206\187") (Id x))) (arg ((Id _))) (arg_ty Unit)
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Id "\206\187") (Id x))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 2) (column 34)))))))
           (bind ()) (loc ((line 2) (column 34)))))))
       (return
        (Make_closure (body ((Id "\206\187") (Id "\206\187") (Id x)))
         (env (((Id env) (Id "\206\187") (Id x))))
         (ty (Closure (arg_ty Unit) (ret_ty Unit))) (loc ((line 2) (column 34)))))
       (loc ((line 2) (column 4))))
      (Functions
       ((paths
         ((((Id x))
           (Closure (arg_ty Unit) (ret_ty (Closure (arg_ty Unit) (ret_ty Unit))))
           ((Id "\206\187") (Id x)))))
        (captures ((entries ()) (size_in_bytes 0))) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Shadow 1) (Id $))) (arg ((Shadow 2) (Id $)))
            (arg_ty Unit) (ty Unit) (loc ((line 3) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value Unit) (ty Unit) (loc ((line 3) (column 10)))))))
            (bind ()) (loc ((line 3) (column 10)))))
          (Values
           ((exprs
             ((((Shadow 1) (Id $))
               (Apply_closure (fn ((Id x))) (arg ((Id $))) (arg_ty Unit)
                (ty (Closure (arg_ty Unit) (ret_ty Unit)))
                (loc ((line 3) (column 8)))))))
            (bind ()) (loc ((line 3) (column 8)))))
          (Values
           ((exprs
             ((((Shadow 2) (Id $))
               (Scalar (value Unit) (ty Unit) (loc ((line 3) (column 13)))))))
            (bind ()) (loc ((line 3) (column 13)))))))
        (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "arg fn" =
  go
    {|
fun x (f : unit -> int) : int = f ();;
let _ = x (fn (_ : unit) -> 1);;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id x))) (arg ((Id f)))
       (arg_ty (Closure (arg_ty Unit) (ret_ty Int))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id $))
              (Scalar (value Unit) (ty Unit) (loc ((line 2) (column 34)))))))
           (bind ()) (loc ((line 2) (column 34)))))))
       (return
        (Apply_closure (fn ((Id f))) (arg ((Id $))) (arg_ty Unit) (ty Int)
         (loc ((line 2) (column 32)))))
       (loc ((line 2) (column 4))))
      (Functions
       ((paths
         ((((Id x))
           (Closure (arg_ty (Closure (arg_ty Unit) (ret_ty Int))) (ret_ty Int))
           ((Id "\206\187") (Id x)))))
        (captures ((entries ()) (size_in_bytes 0))) (loc ((line 2) (column 0)))))
      (Closure_body (path ((Id "\206\187") (Id _))) (arg ((Id _))) (arg_ty Unit)
       (captures ()) (bind ())
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 28)))))
       (loc ((line 3) (column 11))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id x))) (arg ((Id $)))
            (arg_ty (Closure (arg_ty Unit) (ret_ty Int))) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id _))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 11)))))))
            (bind ()) (loc ((line 3) (column 11)))))
          (Values
           ((exprs
             ((((Id $))
               (Make_closure (body ((Id "\206\187") (Id _)))
                (env (((Id env) (Id _))))
                (ty (Closure (arg_ty Unit) (ret_ty Int)))
                (loc ((line 3) (column 11)))))))
            (bind ()) (loc ((line 3) (column 11)))))))
        (loc ((line 3) (column 0)))))))
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
  [%expect {|
    (lst
     ((Thunk_body (path ((Key (Int 1)) (Id "\206\187") (Id f))) (captures ())
       (bind ())
       (return
        (Scalar (value (Bool true)) (ty Bool) (loc ((line 2) (column 67)))))
       (loc ((line 2) (column 8))))
      (Thunk_body (path ((Key (Int 0)) (Id "\206\187") (Id f))) (captures ())
       (bind ())
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 60)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Int 1)) (Id f))
           (Make_closure (body ((Key (Int 1)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Bool))
            (loc ((line 2) (column 8)))))
          (((Key (Int 0)) (Id f))
           (Make_closure (body ((Key (Int 0)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Int))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs ())
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id g))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 8)))))))
            (bind ()) (loc ((line 3) (column 8)))))))
        (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Key (Int 1)) (Id h))
           (Ident (path ((Key (Int 1)) (Id f))) (ty (Thunk Bool))
            (loc ((line 4) (column 21)))))
          (((Key (Int 0)) (Id h))
           (Ident (path ((Key (Int 0)) (Id f))) (ty (Thunk Int))
            (loc ((line 4) (column 21)))))))
        (bind ()) (loc ((line 4) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Int 0)) (Id h))) (ty Int)
            (loc ((line 5) (column 8)))))))
        (bind ()) (loc ((line 5) (column 0)))))
      (Values
       ((exprs
         ((((Shadow 1) (Id _))
           (Apply_thunk (fn ((Key (Int 1)) (Id h))) (ty Bool)
            (loc ((line 6) (column 8)))))))
        (bind ()) (loc ((line 6) (column 0)))))))
    |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static erased x : int) -> (if static x == 0 then 1 else true) : (if x == 0 then int else bool);;
let _ = f 0;;
let _ = f 1;;
|};
  [%expect {|
    (lst
     ((Thunk_body (path ((Key (Int 1)) (Id "\206\187") (Id f))) (captures ())
       (bind ())
       (return
        (Scalar (value (Bool true)) (ty Bool) (loc ((line 2) (column 68)))))
       (loc ((line 2) (column 8))))
      (Thunk_body (path ((Key (Int 0)) (Id "\206\187") (Id f))) (captures ())
       (bind ())
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 61)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Int 1)) (Id f))
           (Make_closure (body ((Key (Int 1)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Bool))
            (loc ((line 2) (column 8)))))
          (((Key (Int 0)) (Id f))
           (Make_closure (body ((Key (Int 0)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Int))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Int 0)) (Id f))) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Shadow 1) (Id _))
           (Apply_thunk (fn ((Key (Int 1)) (Id f))) (ty Bool)
            (loc ((line 4) (column 8)))))))
        (bind ()) (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static erased x : int) -> (if static x == 1 + 1 then 1 else true) : (if x == 2 then int else bool);;
let _ = f 1;;
let _ = f 2;;
|};
  [%expect {|
    (lst
     ((Thunk_body (path ((Key (Int 1)) (Id "\206\187") (Id f))) (captures ())
       (bind ())
       (return
        (Scalar (value (Bool true)) (ty Bool) (loc ((line 2) (column 72)))))
       (loc ((line 2) (column 8))))
      (Thunk_body (path ((Key (Int 2)) (Id "\206\187") (Id f))) (captures ())
       (bind ())
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 65)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Int 1)) (Id f))
           (Make_closure (body ((Key (Int 1)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Bool))
            (loc ((line 2) (column 8)))))
          (((Key (Int 2)) (Id f))
           (Make_closure (body ((Key (Int 2)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Int))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Int 1)) (Id f))) (ty Bool)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Shadow 1) (Id _))
           (Apply_thunk (fn ((Key (Int 2)) (Id f))) (ty Int)
            (loc ((line 4) (column 8)))))))
        (bind ()) (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static erased x : int) -> (if static x == (if true then x else 0) then 1 else true) : (if x == x then int else bool);;
let _ = f 0;;
|};
  [%expect {|
    (lst
     ((Thunk_body (path ((Key (Int 0)) (Id "\206\187") (Id f))) (captures ())
       (bind ())
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 83)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Int 0)) (Id f))
           (Make_closure (body ((Key (Int 0)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Int))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Int 0)) (Id f))) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static erased x : int) -> (if static x == x + 1 then 1 else true) : (if x == x + 1 then int else bool);;
let _ = f 0;;
|};
  [%expect {|
    (lst
     ((Thunk_body (path ((Key (Int 0)) (Id "\206\187") (Id f))) (captures ())
       (bind ())
       (return
        (Scalar (value (Bool true)) (ty Bool) (loc ((line 2) (column 72)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Int 0)) (Id f))
           (Make_closure (body ((Key (Int 0)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Bool))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Int 0)) (Id f))) (ty Bool)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
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
  [%expect {|
    (lst
     ((Thunk_body (path ((Key (Int 1)) (Id "\206\187") (Id f))) (captures ())
       (bind ())
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 85)))))
       (loc ((line 2) (column 8))))
      (Thunk_body (path ((Key (Int 2)) (Id "\206\187") (Id f))) (captures ())
       (bind ())
       (return
        (Scalar (value (Bool true)) (ty Bool) (loc ((line 2) (column 92)))))
       (loc ((line 2) (column 8))))
      (Thunk_body (path ((Key (Int 0)) (Id "\206\187") (Id f))) (captures ())
       (bind ())
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 85)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Int 1)) (Id f))
           (Make_closure (body ((Key (Int 1)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Int))
            (loc ((line 2) (column 8)))))
          (((Key (Int 2)) (Id f))
           (Make_closure (body ((Key (Int 2)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Bool))
            (loc ((line 2) (column 8)))))
          (((Key (Int 0)) (Id f))
           (Make_closure (body ((Key (Int 0)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Int))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Int 0)) (Id f))) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Shadow 1) (Id _))
           (Apply_thunk (fn ((Key (Int 1)) (Id f))) (ty Int)
            (loc ((line 4) (column 8)))))))
        (bind ()) (loc ((line 4) (column 0)))))
      (Values
       ((exprs
         ((((Shadow 2) (Id _))
           (Apply_thunk (fn ((Key (Int 2)) (Id f))) (ty Bool)
            (loc ((line 5) (column 8)))))))
        (bind ()) (loc ((line 5) (column 0)))))))
    |}]
;;

let%expect_test "dependent abstraction" =
  go
    {|
let choose = fn (static f : static int \ x -> if x == 0 then int else bool) -> f 0;;
let _ = choose (fn (static x : int) -> if static x == 0 then 0 else true);;
|};
  [%expect {|
    (lst
     ((Thunk_body
       (path
        ((Key (Int 0)) (Id "\206\187") (Id f) (Key (Closure 4)) (Id "\206\187")
         (Id choose)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 0)) (Id "\206\187") (Id f) (Key (Closure 4))
               (Id "\206\187") (Id choose))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 13)))))))
           (bind ()) (loc ((line 2) (column 13)))))))
       (return (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 61)))))
       (loc ((line 2) (column 13))))
      (Thunk_body (path ((Key (Closure 4)) (Id "\206\187") (Id choose)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Key (Int 0)) (Id f) (Key (Closure 4)) (Id "\206\187")
               (Id choose))
              (Make_closure
               (body
                ((Key (Int 0)) (Id "\206\187") (Id f) (Key (Closure 4))
                 (Id "\206\187") (Id choose)))
               (env
                (((Id env) (Id f) (Key (Closure 4)) (Id "\206\187") (Id choose))))
               (ty (Thunk Int)) (loc ((line 2) (column 13)))))))
           (bind
            ((Values
              ((exprs
                ((((Id env) (Id f) (Key (Closure 4)) (Id "\206\187") (Id choose))
                  (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                   (loc ((line 2) (column 13)))))))
               (bind ()) (loc ((line 2) (column 13)))))))
           (loc ((line 2) (column 13)))))))
       (return
        (Apply_thunk
         (fn
          ((Key (Int 0)) (Id f) (Key (Closure 4)) (Id "\206\187") (Id choose)))
         (ty Int) (loc ((line 2) (column 79)))))
       (loc ((line 2) (column 13))))
      (Values
       ((exprs
         ((((Key (Closure 4)) (Id choose))
           (Make_closure (body ((Key (Closure 4)) (Id "\206\187") (Id choose)))
            (env (((Id env) (Id choose)))) (ty (Thunk Int))
            (loc ((line 2) (column 13)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id choose))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 13)))))))
            (bind ()) (loc ((line 2) (column 13)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Closure 4)) (Id choose))) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "weaken mode: static unerased -> static erased (literal substitution)" =
  go
    {|
let _ = 1 @ erased;;
|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "weaken mode: dynamic unerased -> dynamic erased (erased marker)" =
  go
    {|
let x = 1 @ dynamic;;
let _ = x @ erased;;
|};
  [%expect
    {|
    (lst
     ((Values
       ((exprs
         ((((Id x))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 8)))))))
        (bind ()) (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "weaken mode: static -> dynamic (staticity only)" =
  go
    {|
let _ = (fn (x : int) -> x) @ dynamic;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id _))) (arg ((Id x))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 2) (column 25)))))
       (loc ((line 2) (column 9))))
      (Values
       ((exprs
         ((((Id _))
           (Make_closure (body ((Id "\206\187") (Id _)))
            (env (((Id env) (Id _)))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 2) (column 9)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id _))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 9)))))))
            (bind ()) (loc ((line 2) (column 9)))))))
        (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "weaken type: arrow ret_mode covariant" =
  go
    {|
let f = fn (x : int) -> x;;
let _ = f : int -> erased int;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id f))) (arg ((Id x))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 2) (column 24)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Id f))
           (Make_closure (body ((Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Ident (path ((Id f))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
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
    (lst
     ((Closure_body (path ((Id "\206\187") (Id f))) (arg ((Id x))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 31)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Id f))
           (Make_closure (body ((Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Ident (path ((Id f))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "weaken if non-split: mode erasure on branch" =
  go
    {|
let _ = if true then 1 else 1 @ erased;;
|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "weaken if non-split: arrow type join" =
  go
    {|
let _ = if true then fn (erased x : int) -> 1 else fn (x : int) -> 1;;
|};
  [%expect
    {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id _))) (arg ((Id x))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 44)))))
       (loc ((line 2) (column 21))))
      (Values
       ((exprs
         ((((Id _))
           (Make_closure (body ((Id "\206\187") (Id _)))
            (env (((Id env) (Id _)))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 2) (column 21)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id _))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 21)))))))
            (bind ()) (loc ((line 2) (column 21)))))))
        (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "weaken if split: mode only" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else 1 @ erased;;
|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "weaken binder apply: body weakened to ret_mode" =
  go
    {|
let f = fn (static x : int) -> x;;
let _ = f 0;;
|};
  [%expect {|
    (lst
     ((Thunk_body (path ((Key (Int 0)) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 0)) (Id "\206\187") (Id f))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))))
       (return
        (Ident (path ((Id x) (Key (Int 0)) (Id "\206\187") (Id f))) (ty Int)
         (loc ((line 2) (column 31)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Int 0)) (Id f))
           (Make_closure (body ((Key (Int 0)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Int))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Int 0)) (Id f))) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "weaken arrow closure apply erased: body weakened" =
  go
    {|
let f = fn (x : int) -> x;;
let _ = (f @ erased) 0;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id f))) (arg ((Id x))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 2) (column 24)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Id f))
           (Make_closure (body ((Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Ident (path ((Id x) (Id _))) (ty Int) (loc ((line 2) (column 24)))))))
        (bind
         ((Values
           ((exprs
             ((((Id x) (Id _))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 21)))))))
            (bind ()) (loc ((line 3) (column 8)))))))
        (loc ((line 3) (column 0)))))))
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
    (lst
     ((Closure_body (path ((Id "\206\187") (Id f))) (arg ((Id x))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 24)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Id f))
           (Make_closure (body ((Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "weaken mode: both axes (static unerased -> dynamic erased)" =
  go
    {|
let _ = 1 @ dynamic erased;;
|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "weaken if non-split: staticity on branch" =
  go
    {|
let x = 1 @ dynamic;;
let _ = if true then 1 else x;;
|};
  [%expect
    {|
    (lst
     ((Values
       ((exprs
         ((((Id x))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 8)))))))
        (bind ()) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 21)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id x))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 8)))))))
        (bind ()) (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "weaken if split: staticity on branch" =
  go
    {|
let f = fn (static b : bool) -> if static b then 1 @ dynamic else 1;;
let _ = f false;;
|};
  [%expect {|
    (lst
     ((Thunk_body (path ((Key (Bool false)) (Id "\206\187") (Id f)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id b) (Key (Bool false)) (Id "\206\187") (Id f))
              (Scalar (value (Bool false)) (ty Bool) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))))
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 66)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Bool false)) (Id f))
           (Make_closure (body ((Key (Bool false)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Int))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Bool false)) (Id f))) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "weaken if split: both axes on branch" =
  go
    {|
let f = fn (static b : bool) -> if static b then 1 @ dynamic erased else 1;;
let _ = f false;;
|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "weaken binder apply: erasure on body" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else 1 @ erased;;
let _ = f 0;;
|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "weaken arrow closure apply erased: staticity on body" =
  go
    {|
let f = fn (x : int) -> 1;;
let _ = (f @ erased) 0;;
|};
  [%expect
    {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id f))) (arg ((Id x))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 24)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Id f))
           (Make_closure (body ((Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 24)))))))
        (bind
         ((Values
           ((exprs
             ((((Id x) (Id _))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 21)))))))
            (bind ()) (loc ((line 3) (column 8)))))))
        (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "weaken pi closure apply erased: erasure only" =
  go
    {|
let f = fn (x : int) -> x;;
let g = fn (static erased x : int) -> x;;
let _ = ((if true then f else g) @ erased) 0;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id f))) (arg ((Id x))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 2) (column 24)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Id f))
           (Make_closure (body ((Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "weaken pi closure apply erased: staticity only" =
  go
    {|
let f = fn (x : int) -> 1;;
let g = fn (static x : int) -> x;;
let _ = ((if true then f else g) @ erased) 0;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id f))) (arg ((Id x))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 24)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Id f))
           (Make_closure (body ((Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs ())
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id g))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 8)))))))
            (bind ()) (loc ((line 3) (column 8)))))))
        (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 24)))))))
        (bind
         ((Values
           ((exprs
             ((((Id x) (Id _))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 43)))))))
            (bind ()) (loc ((line 4) (column 8)))))))
        (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "closure to closure: arg erasure contravariant" =
  go
    {|
let apply = fn (f : int -> int) -> f 0;;
let g = fn (erased x : int) -> 1;;
let _ = apply g;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id apply))) (arg ((Id f)))
       (arg_ty (Closure (arg_ty Int) (ret_ty Int))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id $))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 37)))))))
           (bind ()) (loc ((line 2) (column 37)))))))
       (return
        (Apply_closure (fn ((Id f))) (arg ((Id $))) (arg_ty Int) (ty Int)
         (loc ((line 2) (column 35)))))
       (loc ((line 2) (column 12))))
      (Values
       ((exprs
         ((((Id apply))
           (Make_closure (body ((Id "\206\187") (Id apply)))
            (env (((Id env) (Id apply))))
            (ty
             (Closure (arg_ty (Closure (arg_ty Int) (ret_ty Int))) (ret_ty Int)))
            (loc ((line 2) (column 12)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id apply))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 12)))))))
            (bind ()) (loc ((line 2) (column 12)))))))
        (loc ((line 2) (column 0)))))
      (Closure_body (path ((Id "\206\187") (Id g))) (arg ((Id x))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 31)))))
       (loc ((line 3) (column 8))))
      (Values
       ((exprs
         ((((Id g))
           (Make_closure (body ((Id "\206\187") (Id g)))
            (env (((Id env) (Id g)))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 3) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id g))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 8)))))))
            (bind ()) (loc ((line 3) (column 8)))))))
        (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id apply))) (arg ((Id g)))
            (arg_ty (Closure (arg_ty Int) (ret_ty Int))) (ty Int)
            (loc ((line 4) (column 8)))))))
        (bind ()) (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "closure to closure: ret erasure covariant" =
  go
    {|
let apply = fn (f : int -> erased int) -> f 0;;
let g = fn (x : int) -> x;;
let _ = apply g;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id g))) (arg ((Id x))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 3) (column 24)))))
       (loc ((line 3) (column 8))))
      (Values
       ((exprs
         ((((Id g))
           (Make_closure (body ((Id "\206\187") (Id g)))
            (env (((Id env) (Id g)))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 3) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id g))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 8)))))))
            (bind ()) (loc ((line 3) (column 8)))))))
        (loc ((line 3) (column 0)))))))
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
    (lst
     ((Closure_body (path ((Id "\206\187") (Id g))) (arg ((Id x))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 31)))))
       (loc ((line 3) (column 8))))
      (Values
       ((exprs
         ((((Id g))
           (Make_closure (body ((Id "\206\187") (Id g)))
            (env (((Id env) (Id g)))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 3) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id g))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 8)))))))
            (bind ()) (loc ((line 3) (column 8)))))))
        (loc ((line 3) (column 0)))))))
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
    (lst
     ((Closure_body
       (path
        ((Id "\206\187") (Id f) (Key (Closure 3)) (Id "\206\187") (Id apply)))
       (arg ((Id x))) (arg_ty Int) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 3) (column 24)))))
       (loc ((line 2) (column 12))))
      (Thunk_body (path ((Key (Closure 3)) (Id "\206\187") (Id apply)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id f) (Key (Closure 3)) (Id "\206\187") (Id apply))
              (Make_closure
               (body
                ((Id "\206\187") (Id f) (Key (Closure 3)) (Id "\206\187")
                 (Id apply)))
               (env
                (((Id env) (Id f) (Key (Closure 3)) (Id "\206\187") (Id apply))))
               (ty (Closure (arg_ty Int) (ret_ty Int)))
               (loc ((line 2) (column 12)))))))
           (bind
            ((Values
              ((exprs
                ((((Id env) (Id f) (Key (Closure 3)) (Id "\206\187") (Id apply))
                  (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                   (loc ((line 2) (column 12)))))))
               (bind ()) (loc ((line 2) (column 12)))))))
           (loc ((line 2) (column 12)))))
         (Values
          ((exprs
            ((((Id $))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 51)))))))
           (bind ()) (loc ((line 2) (column 51)))))))
       (return
        (Apply_closure (fn ((Id f) (Key (Closure 3)) (Id "\206\187") (Id apply)))
         (arg ((Id $))) (arg_ty Int) (ty Int) (loc ((line 2) (column 49)))))
       (loc ((line 2) (column 12))))
      (Values
       ((exprs
         ((((Key (Closure 3)) (Id apply))
           (Make_closure (body ((Key (Closure 3)) (Id "\206\187") (Id apply)))
            (env (((Id env) (Id apply)))) (ty (Thunk Int))
            (loc ((line 2) (column 12)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id apply))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 12)))))))
            (bind ()) (loc ((line 2) (column 12)))))))
        (loc ((line 2) (column 0)))))
      (Closure_body (path ((Id "\206\187") (Id g))) (arg ((Id x))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 3) (column 24)))))
       (loc ((line 3) (column 8))))
      (Values
       ((exprs
         ((((Id g))
           (Make_closure (body ((Id "\206\187") (Id g)))
            (env (((Id env) (Id g)))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 3) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id g))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 8)))))))
            (bind ()) (loc ((line 3) (column 8)))))))
        (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Closure 3)) (Id apply))) (ty Int)
            (loc ((line 4) (column 8)))))))
        (bind ()) (loc ((line 4) (column 0)))))))
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
    (lst
     ((Thunk_body
       (path
        ((Key (Int 0)) (Id "\206\187") (Id f) (Key (Closure 4)) (Id "\206\187")
         (Id apply)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 0)) (Id "\206\187") (Id f) (Key (Closure 4))
               (Id "\206\187") (Id apply))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 12)))))))
           (bind ()) (loc ((line 2) (column 12)))))))
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 38)))))
       (loc ((line 2) (column 12))))
      (Thunk_body (path ((Key (Closure 4)) (Id "\206\187") (Id apply)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Key (Int 0)) (Id f) (Key (Closure 4)) (Id "\206\187") (Id apply))
              (Make_closure
               (body
                ((Key (Int 0)) (Id "\206\187") (Id f) (Key (Closure 4))
                 (Id "\206\187") (Id apply)))
               (env
                (((Id env) (Id f) (Key (Closure 4)) (Id "\206\187") (Id apply))))
               (ty (Thunk Int)) (loc ((line 2) (column 12)))))))
           (bind
            ((Values
              ((exprs
                ((((Id env) (Id f) (Key (Closure 4)) (Id "\206\187") (Id apply))
                  (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                   (loc ((line 2) (column 12)))))))
               (bind ()) (loc ((line 2) (column 12)))))))
           (loc ((line 2) (column 12)))))))
       (return
        (Apply_thunk
         (fn ((Key (Int 0)) (Id f) (Key (Closure 4)) (Id "\206\187") (Id apply)))
         (ty Int) (loc ((line 2) (column 49)))))
       (loc ((line 2) (column 12))))
      (Values
       ((exprs
         ((((Key (Closure 4)) (Id apply))
           (Make_closure (body ((Key (Closure 4)) (Id "\206\187") (Id apply)))
            (env (((Id env) (Id apply)))) (ty (Thunk Int))
            (loc ((line 2) (column 12)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id apply))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 12)))))))
            (bind ()) (loc ((line 2) (column 12)))))))
        (loc ((line 2) (column 0)))))
      (Thunk_body (path ((Key (Int 0)) (Id "\206\187") (Id g))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 0)) (Id "\206\187") (Id g))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 8)))))))
           (bind ()) (loc ((line 3) (column 8)))))))
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 38)))))
       (loc ((line 3) (column 8))))
      (Values
       ((exprs
         ((((Key (Int 0)) (Id g))
           (Make_closure (body ((Key (Int 0)) (Id "\206\187") (Id g)))
            (env (((Id env) (Id g)))) (ty (Thunk Int))
            (loc ((line 3) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id g))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 8)))))))
            (bind ()) (loc ((line 3) (column 8)))))))
        (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Closure 4)) (Id apply))) (ty Int)
            (loc ((line 4) (column 8)))))))
        (bind ()) (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "erased closure taking closure arg" =
  go
    {|
let apply = fn (f : int -> int) -> f 0;;
let _ = (apply @ erased) (fn (x : int) -> x);;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id apply))) (arg ((Id f)))
       (arg_ty (Closure (arg_ty Int) (ret_ty Int))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id $))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 37)))))))
           (bind ()) (loc ((line 2) (column 37)))))))
       (return
        (Apply_closure (fn ((Id f))) (arg ((Id $))) (arg_ty Int) (ty Int)
         (loc ((line 2) (column 35)))))
       (loc ((line 2) (column 12))))
      (Values
       ((exprs
         ((((Id apply))
           (Make_closure (body ((Id "\206\187") (Id apply)))
            (env (((Id env) (Id apply))))
            (ty
             (Closure (arg_ty (Closure (arg_ty Int) (ret_ty Int))) (ret_ty Int)))
            (loc ((line 2) (column 12)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id apply))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 12)))))))
            (bind ()) (loc ((line 2) (column 12)))))))
        (loc ((line 2) (column 0)))))
      (Closure_body (path ((Id "\206\187") (Id f) (Id _))) (arg ((Id x)))
       (arg_ty Int) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 3) (column 42)))))
       (loc ((line 3) (column 26))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id f) (Id _))) (arg ((Id $))) (arg_ty Int)
            (ty Int) (loc ((line 2) (column 35)))))))
        (bind
         ((Values
           ((exprs
             ((((Id f) (Id _))
               (Make_closure (body ((Id "\206\187") (Id f) (Id _)))
                (env (((Id env) (Id f) (Id _))))
                (ty (Closure (arg_ty Int) (ret_ty Int)))
                (loc ((line 3) (column 26)))))))
            (bind
             ((Values
               ((exprs
                 ((((Id env) (Id f) (Id _))
                   (Make_env (captures ((entries ()) (size_in_bytes 0)))
                    (ty Env) (loc ((line 3) (column 26)))))))
                (bind ()) (loc ((line 3) (column 26)))))))
            (loc ((line 3) (column 8)))))
          (Values
           ((exprs
             ((((Id $))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 37)))))))
            (bind ()) (loc ((line 2) (column 37)))))))
        (loc ((line 3) (column 0)))))))
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
    (lst
     ((Closure_body (path ((Id "\206\187") (Id g))) (arg ((Id x))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 24)))))
       (loc ((line 3) (column 8))))
      (Values
       ((exprs
         ((((Id g))
           (Make_closure (body ((Id "\206\187") (Id g)))
            (env (((Id env) (Id g)))) (ty (Closure (arg_ty Int) (ret_ty Int)))
            (loc ((line 3) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id g))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 8)))))))
            (bind ()) (loc ((line 3) (column 8)))))))
        (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "static lambda identity returns dependent type" =
  go
    {|
let f = fn (static x : int) -> x;;
let _ = f 42;;
|};
  [%expect {|
    (lst
     ((Thunk_body (path ((Key (Int 42)) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 42)) (Id "\206\187") (Id f))
              (Scalar (value (Int 42)) (ty Int) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))))
       (return
        (Ident (path ((Id x) (Key (Int 42)) (Id "\206\187") (Id f))) (ty Int)
         (loc ((line 2) (column 31)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Int 42)) (Id f))
           (Make_closure (body ((Key (Int 42)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Int))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Int 42)) (Id f))) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "static lambda with arithmetic on static arg" =
  go
    {|
let f = fn (static x : int) -> x + 1;;
let _ = f 10;;
|};
  [%expect {|
    (lst
     ((Thunk_body (path ((Key (Int 10)) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 10)) (Id "\206\187") (Id f))
              (Scalar (value (Int 10)) (ty Int) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))
         (Values
          ((exprs
            ((((Id $))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 35)))))))
           (bind ()) (loc ((line 2) (column 35)))))))
       (return
        (Binop (op Add) (lhs ((Id x) (Key (Int 10)) (Id "\206\187") (Id f)))
         (rhs ((Id $))) (ty Int) (loc ((line 2) (column 33)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Int 10)) (Id f))
           (Make_closure (body ((Key (Int 10)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Int))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Int 10)) (Id f))) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "static lambda with boolean op on static arg" =
  go
    {|
let f = fn (static x : bool) -> x && true;;
let _ = f false;;
|};
  [%expect {|
    (lst
     ((Thunk_body (path ((Key (Bool false)) (Id "\206\187") (Id f)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Bool false)) (Id "\206\187") (Id f))
              (Scalar (value (Bool false)) (ty Bool) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))
         (Values
          ((exprs
            ((((Id $))
              (Scalar (value (Bool true)) (ty Bool) (loc ((line 2) (column 37)))))))
           (bind ()) (loc ((line 2) (column 37)))))))
       (return
        (Binop (op And) (lhs ((Id x) (Key (Bool false)) (Id "\206\187") (Id f)))
         (rhs ((Id $))) (ty Bool) (loc ((line 2) (column 34)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Bool false)) (Id f))
           (Make_closure (body ((Key (Bool false)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Bool))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Bool false)) (Id f))) (ty Bool)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "nested static lambdas" =
  go
    {|
let f = fn (static x : int) -> fn (static y : int) -> x + y;;
let _ = f 1 2;;
|};
  [%expect {|
    (lst
     ((Thunk_body
       (path
        ((Key (Int 2)) (Id "\206\187") (Key (Int 1)) (Id "\206\187") (Id f)))
       (captures (((path ((Id x))) (ty Int) (offset_in_bytes 0))))
       (bind
        ((Values
          ((exprs
            ((((Id y) (Key (Int 2)) (Id "\206\187") (Key (Int 1)) (Id "\206\187")
               (Id f))
              (Scalar (value (Int 2)) (ty Int) (loc ((line 2) (column 31)))))))
           (bind ()) (loc ((line 2) (column 31)))))))
       (return
        (Binop (op Add) (lhs ((Id x)))
         (rhs
          ((Id y) (Key (Int 2)) (Id "\206\187") (Key (Int 1)) (Id "\206\187")
           (Id f)))
         (ty Int) (loc ((line 2) (column 56)))))
       (loc ((line 2) (column 31))))
      (Thunk_body (path ((Key (Int 2)) (Key (Int 1)) (Id "\206\187") (Id f)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 1)) (Id "\206\187") (Id f))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))
         (Values
          ((exprs
            ((((Id env) (Key (Int 1)) (Id "\206\187") (Id f))
              (Make_env
               (captures
                ((entries
                  (((path ((Id x) (Key (Int 1)) (Id "\206\187") (Id f)))
                    (ty Int) (offset_in_bytes 0))))
                 (size_in_bytes 8)))
               (ty Env) (loc ((line 2) (column 31)))))))
           (bind ()) (loc ((line 2) (column 31)))))))
       (return
        (Make_closure
         (body
          ((Key (Int 2)) (Id "\206\187") (Key (Int 1)) (Id "\206\187") (Id f)))
         (env (((Id env) (Key (Int 1)) (Id "\206\187") (Id f)))) (ty (Thunk Int))
         (loc ((line 2) (column 31)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Int 2)) (Key (Int 1)) (Id f))
           (Make_closure
            (body ((Key (Int 2)) (Key (Int 1)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk (Thunk Int)))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Int 2)) (Id $))) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Key (Int 2)) (Id $))
               (Apply_thunk (fn ((Key (Int 2)) (Key (Int 1)) (Id f)))
                (ty (Thunk Int)) (loc ((line 3) (column 8)))))))
            (bind ()) (loc ((line 3) (column 8)))))))
        (loc ((line 3) (column 0)))))))
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
    (lst
     ((Thunk_body
       (path
        ((Key (Int 2)) (Id "\206\187") (Key (Int 1)) (Id "\206\187") (Id f)))
       (captures (((path ((Id x))) (ty Int) (offset_in_bytes 0))))
       (bind
        ((Values
          ((exprs
            ((((Id y) (Key (Int 2)) (Id "\206\187") (Key (Int 1)) (Id "\206\187")
               (Id f))
              (Scalar (value (Int 2)) (ty Int) (loc ((line 2) (column 31)))))))
           (bind ()) (loc ((line 2) (column 31)))))))
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 2) (column 54)))))
       (loc ((line 2) (column 31))))
      (Thunk_body (path ((Key (Int 2)) (Key (Int 1)) (Id "\206\187") (Id f)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 1)) (Id "\206\187") (Id f))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))
         (Values
          ((exprs
            ((((Id env) (Key (Int 1)) (Id "\206\187") (Id f))
              (Make_env
               (captures
                ((entries
                  (((path ((Id x) (Key (Int 1)) (Id "\206\187") (Id f)))
                    (ty Int) (offset_in_bytes 0))))
                 (size_in_bytes 8)))
               (ty Env) (loc ((line 2) (column 31)))))))
           (bind ()) (loc ((line 2) (column 31)))))))
       (return
        (Make_closure
         (body
          ((Key (Int 2)) (Id "\206\187") (Key (Int 1)) (Id "\206\187") (Id f)))
         (env (((Id env) (Key (Int 1)) (Id "\206\187") (Id f)))) (ty (Thunk Int))
         (loc ((line 2) (column 31)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Int 2)) (Key (Int 1)) (Id f))
           (Make_closure
            (body ((Key (Int 2)) (Key (Int 1)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk (Thunk Int)))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Key (Int 2)) (Id g))
           (Apply_thunk (fn ((Key (Int 2)) (Key (Int 1)) (Id f)))
            (ty (Thunk Int)) (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Int 2)) (Id g))) (ty Int)
            (loc ((line 4) (column 8)))))))
        (bind ()) (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "static lambda mixed with dynamic lambda" =
  go
    {|
let f = fn (static x : int) -> fn (y : int) -> y;;
let g = f 1;;
let _ = g 2;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Key (Int 1)) (Id "\206\187") (Id f)))
       (arg ((Id y))) (arg_ty Int) (captures ()) (bind ())
       (return (Ident (path ((Id y))) (ty Int) (loc ((line 2) (column 47)))))
       (loc ((line 2) (column 31))))
      (Thunk_body (path ((Key (Int 1)) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 1)) (Id "\206\187") (Id f))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))
         (Values
          ((exprs
            ((((Id env) (Key (Int 1)) (Id "\206\187") (Id f))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 2) (column 31)))))))
           (bind ()) (loc ((line 2) (column 31)))))))
       (return
        (Make_closure
         (body ((Id "\206\187") (Key (Int 1)) (Id "\206\187") (Id f)))
         (env (((Id env) (Key (Int 1)) (Id "\206\187") (Id f))))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 2) (column 31)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Int 1)) (Id f))
           (Make_closure (body ((Key (Int 1)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f))))
            (ty (Thunk (Closure (arg_ty Int) (ret_ty Int))))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id g))
           (Apply_thunk (fn ((Key (Int 1)) (Id f)))
            (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id g))) (arg ((Id $))) (arg_ty Int) (ty Int)
            (loc ((line 4) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value (Int 2)) (ty Int) (loc ((line 4) (column 10)))))))
            (bind ()) (loc ((line 4) (column 10)))))))
        (loc ((line 4) (column 0)))))))
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
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Key BoolT) (Id "\206\187") (Id f)))
       (arg ((Id x))) (arg_ty Bool) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Bool) (loc ((line 2) (column 53)))))
       (loc ((line 2) (column 39))))
      (Thunk_body (path ((Key BoolT) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Key BoolT) (Id "\206\187") (Id f))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 2) (column 39)))))))
           (bind ()) (loc ((line 2) (column 39)))))))
       (return
        (Make_closure (body ((Id "\206\187") (Key BoolT) (Id "\206\187") (Id f)))
         (env (((Id env) (Key BoolT) (Id "\206\187") (Id f))))
         (ty (Closure (arg_ty Bool) (ret_ty Bool))) (loc ((line 2) (column 39)))))
       (loc ((line 2) (column 8))))
      (Closure_body (path ((Id "\206\187") (Key IntT) (Id "\206\187") (Id f)))
       (arg ((Id x))) (arg_ty Int) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 2) (column 53)))))
       (loc ((line 2) (column 39))))
      (Thunk_body (path ((Key IntT) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Key IntT) (Id "\206\187") (Id f))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 2) (column 39)))))))
           (bind ()) (loc ((line 2) (column 39)))))))
       (return
        (Make_closure (body ((Id "\206\187") (Key IntT) (Id "\206\187") (Id f)))
         (env (((Id env) (Key IntT) (Id "\206\187") (Id f))))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 2) (column 39)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key BoolT) (Id f))
           (Make_closure (body ((Key BoolT) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f))))
            (ty (Thunk (Closure (arg_ty Bool) (ret_ty Bool))))
            (loc ((line 2) (column 8)))))
          (((Key IntT) (Id f))
           (Make_closure (body ((Key IntT) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f))))
            (ty (Thunk (Closure (arg_ty Int) (ret_ty Int))))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id g))
           (Apply_thunk (fn ((Key IntT) (Id f)))
            (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id g))) (arg ((Id $))) (arg_ty Int) (ty Int)
            (loc ((line 4) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value (Int 42)) (ty Int) (loc ((line 4) (column 10)))))))
            (bind ()) (loc ((line 4) (column 10)))))))
        (loc ((line 4) (column 0)))))
      (Values
       ((exprs
         ((((Id h))
           (Apply_thunk (fn ((Key BoolT) (Id f)))
            (ty (Closure (arg_ty Bool) (ret_ty Bool)))
            (loc ((line 5) (column 8)))))))
        (bind ()) (loc ((line 5) (column 0)))))
      (Values
       ((exprs
         ((((Shadow 1) (Id _))
           (Apply_closure (fn ((Id h))) (arg ((Id $))) (arg_ty Bool) (ty Bool)
            (loc ((line 6) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value (Bool true)) (ty Bool)
                (loc ((line 6) (column 10)))))))
            (bind ()) (loc ((line 6) (column 10)))))))
        (loc ((line 6) (column 0)))))))
    |}]
;;

let%expect_test "if static with literal condition true" =
  go
    {|
let _ = if static true then 1 else true;;
|};
  [%expect
    {|
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 28)))))))
        (bind ()) (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "if static with literal condition false" =
  go
    {|
let _ = if static false then 1 else true;;
|};
  [%expect
    {|
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Bool true)) (ty Bool) (loc ((line 2) (column 36)))))))
        (bind ()) (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "if static with static variable condition" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else true;;
let a = f 0;;
let b = f 1;;
|};
  [%expect {|
    (lst
     ((Thunk_body (path ((Key (Int 1)) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 1)) (Id "\206\187") (Id f))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))))
       (return
        (Scalar (value (Bool true)) (ty Bool) (loc ((line 2) (column 60)))))
       (loc ((line 2) (column 8))))
      (Thunk_body (path ((Key (Int 0)) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 0)) (Id "\206\187") (Id f))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))))
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 53)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Int 1)) (Id f))
           (Make_closure (body ((Key (Int 1)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Bool))
            (loc ((line 2) (column 8)))))
          (((Key (Int 0)) (Id f))
           (Make_closure (body ((Key (Int 0)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Int))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id a))
           (Apply_thunk (fn ((Key (Int 0)) (Id f))) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id b))
           (Apply_thunk (fn ((Key (Int 1)) (Id f))) (ty Bool)
            (loc ((line 4) (column 8)))))))
        (bind ()) (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "if static with mismatched branch types without annotation" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else true;;
|};
  [%expect {|
    (lst
     ((Values
       ((exprs ())
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "if static with correct type annotation using non-static if" =
  go
    {|
let f = fn (static x : int) -> (if static x == 0 then 1 else true) : (if x == 0 then int else bool);;
|};
  [%expect {|
    (lst
     ((Values
       ((exprs ())
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))))
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
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Key (Int 1)) (Id "\206\187") (Id f)))
       (arg ((Id y))) (arg_ty Bool) (captures ()) (bind ())
       (return (Ident (path ((Id y))) (ty Bool) (loc ((line 2) (column 93)))))
       (loc ((line 2) (column 76))))
      (Thunk_body (path ((Key (Int 1)) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 1)) (Id "\206\187") (Id f))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))
         (Values
          ((exprs
            ((((Id env) (Key (Int 1)) (Id "\206\187") (Id f))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 2) (column 76)))))))
           (bind ()) (loc ((line 2) (column 76)))))))
       (return
        (Make_closure
         (body ((Id "\206\187") (Key (Int 1)) (Id "\206\187") (Id f)))
         (env (((Id env) (Key (Int 1)) (Id "\206\187") (Id f))))
         (ty (Closure (arg_ty Bool) (ret_ty Bool))) (loc ((line 2) (column 76)))))
       (loc ((line 2) (column 8))))
      (Closure_body (path ((Id "\206\187") (Key (Int 0)) (Id "\206\187") (Id f)))
       (arg ((Id y))) (arg_ty Int) (captures ()) (bind ())
       (return (Ident (path ((Id y))) (ty Int) (loc ((line 2) (column 69)))))
       (loc ((line 2) (column 53))))
      (Thunk_body (path ((Key (Int 0)) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 0)) (Id "\206\187") (Id f))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))
         (Values
          ((exprs
            ((((Id env) (Key (Int 0)) (Id "\206\187") (Id f))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 2) (column 53)))))))
           (bind ()) (loc ((line 2) (column 53)))))))
       (return
        (Make_closure
         (body ((Id "\206\187") (Key (Int 0)) (Id "\206\187") (Id f)))
         (env (((Id env) (Key (Int 0)) (Id "\206\187") (Id f))))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 2) (column 53)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Int 1)) (Id f))
           (Make_closure (body ((Key (Int 1)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f))))
            (ty (Thunk (Closure (arg_ty Bool) (ret_ty Bool))))
            (loc ((line 2) (column 8)))))
          (((Key (Int 0)) (Id f))
           (Make_closure (body ((Key (Int 0)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f))))
            (ty (Thunk (Closure (arg_ty Int) (ret_ty Int))))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id g))
           (Apply_thunk (fn ((Key (Int 0)) (Id f)))
            (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id g))) (arg ((Id $))) (arg_ty Int) (ty Int)
            (loc ((line 4) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value (Int 42)) (ty Int) (loc ((line 4) (column 10)))))))
            (bind ()) (loc ((line 4) (column 10)))))))
        (loc ((line 4) (column 0)))))
      (Values
       ((exprs
         ((((Id h))
           (Apply_thunk (fn ((Key (Int 1)) (Id f)))
            (ty (Closure (arg_ty Bool) (ret_ty Bool)))
            (loc ((line 5) (column 8)))))))
        (bind ()) (loc ((line 5) (column 0)))))
      (Values
       ((exprs
         ((((Shadow 1) (Id _))
           (Apply_closure (fn ((Id h))) (arg ((Id $))) (arg_ty Bool) (ty Bool)
            (loc ((line 6) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value (Bool true)) (ty Bool)
                (loc ((line 6) (column 10)))))))
            (bind ()) (loc ((line 6) (column 10)))))))
        (loc ((line 6) (column 0)))))))
    |}]
;;

let%expect_test "if static true selects then branch type" =
  go
    {|
let _ = (if static true then 1 else true) : (if true then int else bool);;
|};
  [%expect
    {|
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 29)))))))
        (bind ()) (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "if static false selects else branch type" =
  go
    {|
let _ = (if static false then 1 else true) : (if false then int else bool);;
|};
  [%expect
    {|
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Bool true)) (ty Bool) (loc ((line 2) (column 37)))))))
        (bind ()) (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "dependent arrow type with backslash binder" =
  go
    {|
let f = fn (static g : static int \ x -> int) -> g 0;;
|};
  [%expect {|
    (lst
     ((Values
       ((exprs ())
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "dependent arrow applied to matching function" =
  go
    {|
let apply_type = fn (static f : static erased type \ t -> t -> t) -> f int;;
let _ = apply_type (fn (static erased t : type) -> fn (x : t) -> x);;
|};
  [%expect {|
    (lst
     ((Closure_body
       (path
        ((Id "\206\187") (Key IntT) (Id "\206\187") (Id f) (Key (Closure 4))
         (Id "\206\187") (Id apply_type)))
       (arg ((Id x))) (arg_ty Int) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 3) (column 65)))))
       (loc ((line 3) (column 51))))
      (Thunk_body
       (path
        ((Key IntT) (Id "\206\187") (Id f) (Key (Closure 4)) (Id "\206\187")
         (Id apply_type)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Key IntT) (Id "\206\187") (Id f) (Key (Closure 4))
               (Id "\206\187") (Id apply_type))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 3) (column 51)))))))
           (bind ()) (loc ((line 3) (column 51)))))))
       (return
        (Make_closure
         (body
          ((Id "\206\187") (Key IntT) (Id "\206\187") (Id f) (Key (Closure 4))
           (Id "\206\187") (Id apply_type)))
         (env
          (((Id env) (Key IntT) (Id "\206\187") (Id f) (Key (Closure 4))
            (Id "\206\187") (Id apply_type))))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 3) (column 51)))))
       (loc ((line 2) (column 17))))
      (Thunk_body (path ((Key (Closure 4)) (Id "\206\187") (Id apply_type)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Key IntT) (Id f) (Key (Closure 4)) (Id "\206\187")
               (Id apply_type))
              (Make_closure
               (body
                ((Key IntT) (Id "\206\187") (Id f) (Key (Closure 4))
                 (Id "\206\187") (Id apply_type)))
               (env
                (((Id env) (Id f) (Key (Closure 4)) (Id "\206\187")
                  (Id apply_type))))
               (ty (Thunk (Closure (arg_ty Int) (ret_ty Int))))
               (loc ((line 2) (column 17)))))))
           (bind
            ((Values
              ((exprs
                ((((Id env) (Id f) (Key (Closure 4)) (Id "\206\187")
                   (Id apply_type))
                  (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                   (loc ((line 2) (column 17)))))))
               (bind ()) (loc ((line 2) (column 17)))))))
           (loc ((line 2) (column 17)))))))
       (return
        (Apply_thunk
         (fn
          ((Key IntT) (Id f) (Key (Closure 4)) (Id "\206\187") (Id apply_type)))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 2) (column 69)))))
       (loc ((line 2) (column 17))))
      (Values
       ((exprs
         ((((Key (Closure 4)) (Id apply_type))
           (Make_closure
            (body ((Key (Closure 4)) (Id "\206\187") (Id apply_type)))
            (env (((Id env) (Id apply_type))))
            (ty (Thunk (Closure (arg_ty Int) (ret_ty Int))))
            (loc ((line 2) (column 17)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id apply_type))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 17)))))))
            (bind ()) (loc ((line 2) (column 17)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Closure 4)) (Id apply_type)))
            (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "dependent arrow with return type depending on arg" =
  go
    {|
let mk_int = fn (static x : int) -> int;;
let f = fn (static g : static int \ x -> mk_int x) -> g 0;;
|};
  [%expect {|
    (lst
     ((Values
       ((exprs ())
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 8)))))))
            (bind ()) (loc ((line 3) (column 8)))))))
        (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "fun with static arg creates Pi type" =
  go
    {|
fun f (static x : int) : int = x;;
let _ = f 0;;
|};
  [%expect {|
    (lst
     ((Thunk_body (path ((Key (Int 0)) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 0)) (Id "\206\187") (Id f))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 4)))))))
           (bind ()) (loc ((line 2) (column 4)))))))
       (return
        (Ident (path ((Id x) (Key (Int 0)) (Id "\206\187") (Id f))) (ty Int)
         (loc ((line 2) (column 31)))))
       (loc ((line 2) (column 4))))
      (Functions
       ((paths
         ((((Key (Int 0)) (Id f)) (Thunk Int)
           ((Key (Int 0)) (Id "\206\187") (Id f)))))
        (captures ((entries ()) (size_in_bytes 0))) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Int 0)) (Id f))) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "fun with static erased type arg — polymorphic identity" =
  go
    {|
fun id (static erased t : type) : t -> t = fn (x : t) -> x;;
let _ = id int 0;;
let _ = id bool true;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Key BoolT) (Id "\206\187") (Id id)))
       (arg ((Id x))) (arg_ty Bool) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Bool) (loc ((line 2) (column 57)))))
       (loc ((line 2) (column 43))))
      (Thunk_body (path ((Key BoolT) (Id "\206\187") (Id id))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Key BoolT) (Id "\206\187") (Id id))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 2) (column 43)))))))
           (bind ()) (loc ((line 2) (column 43)))))))
       (return
        (Make_closure
         (body ((Id "\206\187") (Key BoolT) (Id "\206\187") (Id id)))
         (env (((Id env) (Key BoolT) (Id "\206\187") (Id id))))
         (ty (Closure (arg_ty Bool) (ret_ty Bool))) (loc ((line 2) (column 43)))))
       (loc ((line 2) (column 4))))
      (Closure_body (path ((Id "\206\187") (Key IntT) (Id "\206\187") (Id id)))
       (arg ((Id x))) (arg_ty Int) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 2) (column 57)))))
       (loc ((line 2) (column 43))))
      (Thunk_body (path ((Key IntT) (Id "\206\187") (Id id))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Key IntT) (Id "\206\187") (Id id))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 2) (column 43)))))))
           (bind ()) (loc ((line 2) (column 43)))))))
       (return
        (Make_closure (body ((Id "\206\187") (Key IntT) (Id "\206\187") (Id id)))
         (env (((Id env) (Key IntT) (Id "\206\187") (Id id))))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 2) (column 43)))))
       (loc ((line 2) (column 4))))
      (Functions
       ((paths
         ((((Key BoolT) (Id id)) (Thunk (Closure (arg_ty Bool) (ret_ty Bool)))
           ((Key BoolT) (Id "\206\187") (Id id)))
          (((Key IntT) (Id id)) (Thunk (Closure (arg_ty Int) (ret_ty Int)))
           ((Key IntT) (Id "\206\187") (Id id)))))
        (captures ((entries ()) (size_in_bytes 0))) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id $))) (arg ((Shadow 1) (Id $))) (arg_ty Int)
            (ty Int) (loc ((line 3) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Apply_thunk (fn ((Key IntT) (Id id)))
                (ty (Closure (arg_ty Int) (ret_ty Int)))
                (loc ((line 3) (column 8)))))))
            (bind ()) (loc ((line 3) (column 8)))))
          (Values
           ((exprs
             ((((Shadow 1) (Id $))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 15)))))))
            (bind ()) (loc ((line 3) (column 15)))))))
        (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Shadow 1) (Id _))
           (Apply_closure (fn ((Id $))) (arg ((Shadow 1) (Id $))) (arg_ty Bool)
            (ty Bool) (loc ((line 4) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Apply_thunk (fn ((Key BoolT) (Id id)))
                (ty (Closure (arg_ty Bool) (ret_ty Bool)))
                (loc ((line 4) (column 8)))))))
            (bind ()) (loc ((line 4) (column 8)))))
          (Values
           ((exprs
             ((((Shadow 1) (Id $))
               (Scalar (value (Bool true)) (ty Bool)
                (loc ((line 4) (column 16)))))))
            (bind ()) (loc ((line 4) (column 16)))))))
        (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "fun dynamic recursion is allowed" =
  go
    {|
fun f (x : int) : int = f x;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id f))) (arg ((Id x))) (arg_ty Int)
       (captures
        (((path ((Id f))) (ty (Closure (arg_ty Int) (ret_ty Int)))
          (offset_in_bytes 0))))
       (bind ())
       (return
        (Apply_closure (fn ((Id f))) (arg ((Id x))) (arg_ty Int) (ty Int)
         (loc ((line 2) (column 24)))))
       (loc ((line 2) (column 4))))
      (Functions
       ((paths
         ((((Id f)) (Closure (arg_ty Int) (ret_ty Int)) ((Id "\206\187") (Id f)))))
        (captures
         ((entries
           (((path ((Id f))) (ty (Closure (arg_ty Int) (ret_ty Int)))
             (offset_in_bytes 0))))
          (size_in_bytes 16)))
        (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "fun with static erased type arg, two sequential funs" =
  go
    {|
fun id1 (static erased t : type) : t -> t = fn (x : t) -> x;;
fun id2 (static erased t : type) : t -> t = id1 t;;
let _ = id2 int 0;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Key IntT) (Id "\206\187") (Id id1)))
       (arg ((Id x))) (arg_ty Int) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 2) (column 58)))))
       (loc ((line 2) (column 44))))
      (Thunk_body (path ((Key IntT) (Id "\206\187") (Id id1))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Key IntT) (Id "\206\187") (Id id1))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 2) (column 44)))))))
           (bind ()) (loc ((line 2) (column 44)))))))
       (return
        (Make_closure
         (body ((Id "\206\187") (Key IntT) (Id "\206\187") (Id id1)))
         (env (((Id env) (Key IntT) (Id "\206\187") (Id id1))))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 2) (column 44)))))
       (loc ((line 2) (column 4))))
      (Functions
       ((paths
         ((((Key IntT) (Id id1)) (Thunk (Closure (arg_ty Int) (ret_ty Int)))
           ((Key IntT) (Id "\206\187") (Id id1)))))
        (captures ((entries ()) (size_in_bytes 0))) (loc ((line 2) (column 0)))))
      (Thunk_body (path ((Key IntT) (Id "\206\187") (Id id2)))
       (captures
        (((path ((Key IntT) (Id id1)))
          (ty (Thunk (Closure (arg_ty Int) (ret_ty Int)))) (offset_in_bytes 0))))
       (bind ())
       (return
        (Apply_thunk (fn ((Key IntT) (Id id1)))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 3) (column 44)))))
       (loc ((line 3) (column 4))))
      (Functions
       ((paths
         ((((Key IntT) (Id id2)) (Thunk (Closure (arg_ty Int) (ret_ty Int)))
           ((Key IntT) (Id "\206\187") (Id id2)))))
        (captures
         ((entries
           (((path ((Key IntT) (Id id1)))
             (ty (Thunk (Closure (arg_ty Int) (ret_ty Int))))
             (offset_in_bytes 0))))
          (size_in_bytes 16)))
        (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id $))) (arg ((Shadow 1) (Id $))) (arg_ty Int)
            (ty Int) (loc ((line 4) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Apply_thunk (fn ((Key IntT) (Id id2)))
                (ty (Closure (arg_ty Int) (ret_ty Int)))
                (loc ((line 4) (column 8)))))))
            (bind ()) (loc ((line 4) (column 8)))))
          (Values
           ((exprs
             ((((Shadow 1) (Id $))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 16)))))))
            (bind ()) (loc ((line 4) (column 16)))))))
        (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "static erased lambda captures no runtime value" =
  go
    {|
let f = fn (static erased x : int) -> 0;;
let _ = f 1;;
|};
  [%expect {|
    (lst
     ((Thunk_body (path ((Key (Int 1)) (Id "\206\187") (Id f))) (captures ())
       (bind ())
       (return (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 38)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Int 1)) (Id f))
           (Make_closure (body ((Key (Int 1)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Int))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Int 1)) (Id f))) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "lift static value through Pi" =
  go
    {|
let f = fn (static x : int) -> x @ erased;;
let _ = f 0;;
|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "fun returning static erased type" =
  go
    {|
fun f (static _ : unit) : static erased type = int;;
let _ = 5 : f ();;
|};
  [%expect
    {|
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 5)) (ty Int) (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "pi and arrow join — if choosing between Pi and Arrow" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else true;;
let g = fn (static x : int) -> if static x == 0 then 1 else true;;
let _ = if true then f else g;;
|};
  [%expect {|
    (lst
     ((Values
       ((exprs ())
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs ())
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id g))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 8)))))))
            (bind ()) (loc ((line 3) (column 8)))))))
        (loc ((line 3) (column 0)))))
      (Values ((exprs ()) (bind ()) (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "joining f 0 and g 1 resolves dependent types" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else true;;
let g = fn (static x : int) -> if static x == 0 then true else 2;;
let _ = if true then f 0 else g 1;;
|};
  [%expect {|
    (lst
     ((Thunk_body (path ((Key (Int 0)) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 0)) (Id "\206\187") (Id f))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))))
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 53)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Int 0)) (Id f))
           (Make_closure (body ((Key (Int 0)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Int))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Thunk_body (path ((Key (Int 1)) (Id "\206\187") (Id g))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 1)) (Id "\206\187") (Id g))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 8)))))))
           (bind ()) (loc ((line 3) (column 8)))))))
       (return (Scalar (value (Int 2)) (ty Int) (loc ((line 3) (column 63)))))
       (loc ((line 3) (column 8))))
      (Values
       ((exprs
         ((((Key (Int 1)) (Id g))
           (Make_closure (body ((Key (Int 1)) (Id "\206\187") (Id g)))
            (env (((Id env) (Id g)))) (ty (Thunk Int))
            (loc ((line 3) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id g))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 8)))))))
            (bind ()) (loc ((line 3) (column 8)))))))
        (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Int 0)) (Id f))) (ty Int)
            (loc ((line 4) (column 21)))))))
        (bind ()) (loc ((line 4) (column 0)))))))
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
    (lst
     ((Thunk_body
       (path
        ((Key (Int 1)) (Id "\206\187") (Key (Int 1)) (Id "\206\187") (Id f)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id y) (Key (Int 1)) (Id "\206\187") (Key (Int 1)) (Id "\206\187")
               (Id f))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 31)))))))
           (bind ()) (loc ((line 2) (column 31)))))))
       (return (Scalar (value (Int 2)) (ty Int) (loc ((line 6) (column 35)))))
       (loc ((line 2) (column 31))))
      (Thunk_body
       (path
        ((Key (Int 0)) (Id "\206\187") (Key (Int 1)) (Id "\206\187") (Id f)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id y) (Key (Int 0)) (Id "\206\187") (Key (Int 1)) (Id "\206\187")
               (Id f))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 31)))))))
           (bind ()) (loc ((line 2) (column 31)))))))
       (return (Scalar (value Unit) (ty Unit) (loc ((line 6) (column 27)))))
       (loc ((line 2) (column 31))))
      (Thunk_body (path ((Key (Int 1)) (Key (Int 1)) (Id "\206\187") (Id f)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 1)) (Id "\206\187") (Id f))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))
         (Values
          ((exprs
            ((((Id env) (Key (Int 1)) (Id "\206\187") (Id f))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 2) (column 31)))))))
           (bind ()) (loc ((line 2) (column 31)))))))
       (return
        (Make_closure
         (body
          ((Key (Int 1)) (Id "\206\187") (Key (Int 1)) (Id "\206\187") (Id f)))
         (env (((Id env) (Key (Int 1)) (Id "\206\187") (Id f)))) (ty (Thunk Int))
         (loc ((line 2) (column 31)))))
       (loc ((line 2) (column 8))))
      (Thunk_body (path ((Key (Int 0)) (Key (Int 1)) (Id "\206\187") (Id f)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 1)) (Id "\206\187") (Id f))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))
         (Values
          ((exprs
            ((((Id env) (Key (Int 1)) (Id "\206\187") (Id f))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 2) (column 31)))))))
           (bind ()) (loc ((line 2) (column 31)))))))
       (return
        (Make_closure
         (body
          ((Key (Int 0)) (Id "\206\187") (Key (Int 1)) (Id "\206\187") (Id f)))
         (env (((Id env) (Key (Int 1)) (Id "\206\187") (Id f))))
         (ty (Thunk Unit)) (loc ((line 2) (column 31)))))
       (loc ((line 2) (column 8))))
      (Thunk_body
       (path
        ((Key (Int 1)) (Id "\206\187") (Key (Int 0)) (Id "\206\187") (Id f)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id y) (Key (Int 1)) (Id "\206\187") (Key (Int 0)) (Id "\206\187")
               (Id f))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 31)))))))
           (bind ()) (loc ((line 2) (column 31)))))))
       (return
        (Scalar (value (Bool true)) (ty Bool) (loc ((line 4) (column 34)))))
       (loc ((line 2) (column 31))))
      (Thunk_body
       (path
        ((Key (Int 0)) (Id "\206\187") (Key (Int 0)) (Id "\206\187") (Id f)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id y) (Key (Int 0)) (Id "\206\187") (Key (Int 0)) (Id "\206\187")
               (Id f))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 31)))))))
           (bind ()) (loc ((line 2) (column 31)))))))
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 4) (column 27)))))
       (loc ((line 2) (column 31))))
      (Thunk_body (path ((Key (Int 1)) (Key (Int 0)) (Id "\206\187") (Id f)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 0)) (Id "\206\187") (Id f))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))
         (Values
          ((exprs
            ((((Id env) (Key (Int 0)) (Id "\206\187") (Id f))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 2) (column 31)))))))
           (bind ()) (loc ((line 2) (column 31)))))))
       (return
        (Make_closure
         (body
          ((Key (Int 1)) (Id "\206\187") (Key (Int 0)) (Id "\206\187") (Id f)))
         (env (((Id env) (Key (Int 0)) (Id "\206\187") (Id f))))
         (ty (Thunk Bool)) (loc ((line 2) (column 31)))))
       (loc ((line 2) (column 8))))
      (Thunk_body (path ((Key (Int 0)) (Key (Int 0)) (Id "\206\187") (Id f)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 0)) (Id "\206\187") (Id f))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))
         (Values
          ((exprs
            ((((Id env) (Key (Int 0)) (Id "\206\187") (Id f))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 2) (column 31)))))))
           (bind ()) (loc ((line 2) (column 31)))))))
       (return
        (Make_closure
         (body
          ((Key (Int 0)) (Id "\206\187") (Key (Int 0)) (Id "\206\187") (Id f)))
         (env (((Id env) (Key (Int 0)) (Id "\206\187") (Id f)))) (ty (Thunk Int))
         (loc ((line 2) (column 31)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Int 1)) (Key (Int 1)) (Id f))
           (Make_closure
            (body ((Key (Int 1)) (Key (Int 1)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk (Thunk Int)))
            (loc ((line 2) (column 8)))))
          (((Key (Int 0)) (Key (Int 1)) (Id f))
           (Make_closure
            (body ((Key (Int 0)) (Key (Int 1)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk (Thunk Unit)))
            (loc ((line 2) (column 8)))))
          (((Key (Int 1)) (Key (Int 0)) (Id f))
           (Make_closure
            (body ((Key (Int 1)) (Key (Int 0)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk (Thunk Bool)))
            (loc ((line 2) (column 8)))))
          (((Key (Int 0)) (Key (Int 0)) (Id f))
           (Make_closure
            (body ((Key (Int 0)) (Key (Int 0)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk (Thunk Int)))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Int 0)) (Id $))) (ty Int)
            (loc ((line 7) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Key (Int 0)) (Id $))
               (Apply_thunk (fn ((Key (Int 0)) (Key (Int 0)) (Id f)))
                (ty (Thunk Int)) (loc ((line 7) (column 8)))))))
            (bind ()) (loc ((line 7) (column 8)))))))
        (loc ((line 7) (column 0)))))
      (Values
       ((exprs
         ((((Shadow 1) (Id _))
           (Apply_thunk (fn ((Key (Int 1)) (Id $))) (ty Bool)
            (loc ((line 8) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Key (Int 1)) (Id $))
               (Apply_thunk (fn ((Key (Int 1)) (Key (Int 0)) (Id f)))
                (ty (Thunk Bool)) (loc ((line 8) (column 8)))))))
            (bind ()) (loc ((line 8) (column 8)))))))
        (loc ((line 8) (column 0)))))
      (Values
       ((exprs
         ((((Shadow 2) (Id _))
           (Apply_thunk (fn ((Key (Int 0)) (Id $))) (ty Unit)
            (loc ((line 9) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Key (Int 0)) (Id $))
               (Apply_thunk (fn ((Key (Int 0)) (Key (Int 1)) (Id f)))
                (ty (Thunk Unit)) (loc ((line 9) (column 8)))))))
            (bind ()) (loc ((line 9) (column 8)))))))
        (loc ((line 9) (column 0)))))
      (Values
       ((exprs
         ((((Shadow 3) (Id _))
           (Apply_thunk (fn ((Key (Int 1)) (Id $))) (ty Int)
            (loc ((line 10) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Key (Int 1)) (Id $))
               (Apply_thunk (fn ((Key (Int 1)) (Key (Int 1)) (Id f)))
                (ty (Thunk Int)) (loc ((line 10) (column 8)))))))
            (bind ()) (loc ((line 10) (column 8)))))))
        (loc ((line 10) (column 0)))))))
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
    (lst
     ((Thunk_body
       (path
        ((Key (Int 1)) (Id "\206\187") (Id g) (Key (Int 1)) (Id "\206\187")
         (Id f)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id y) (Key (Int 1)) (Id "\206\187") (Id g) (Key (Int 1))
               (Id "\206\187") (Id f))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 8) (column 6)))))))
           (bind ()) (loc ((line 8) (column 6)))))))
       (return (Scalar (value (Int 2)) (ty Int) (loc ((line 15) (column 39)))))
       (loc ((line 8) (column 6))))
      (Thunk_body
       (path
        ((Key (Int 0)) (Id "\206\187") (Id g) (Key (Int 1)) (Id "\206\187")
         (Id f)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id y) (Key (Int 0)) (Id "\206\187") (Id g) (Key (Int 1))
               (Id "\206\187") (Id f))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 8) (column 6)))))))
           (bind ()) (loc ((line 8) (column 6)))))))
       (return (Scalar (value Unit) (ty Unit) (loc ((line 15) (column 31)))))
       (loc ((line 8) (column 6))))
      (Thunk_body (path ((Key (Int 1)) (Key (Int 1)) (Id "\206\187") (Id f)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 1)) (Id "\206\187") (Id f))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 4)))))))
           (bind ()) (loc ((line 2) (column 4)))))
         (Functions
          ((paths
            ((((Key (Int 1)) (Id g) (Key (Int 1)) (Id "\206\187") (Id f))
              (Thunk Int)
              ((Key (Int 1)) (Id "\206\187") (Id g) (Key (Int 1)) (Id "\206\187")
               (Id f)))
             (((Key (Int 0)) (Id g) (Key (Int 1)) (Id "\206\187") (Id f))
              (Thunk Unit)
              ((Key (Int 0)) (Id "\206\187") (Id g) (Key (Int 1)) (Id "\206\187")
               (Id f)))))
           (captures ((entries ()) (size_in_bytes 0)))
           (loc ((line 8) (column 2)))))))
       (return
        (Ident (path ((Key (Int 1)) (Id g) (Key (Int 1)) (Id "\206\187") (Id f)))
         (ty (Thunk Int)) (loc ((line 17) (column 2)))))
       (loc ((line 2) (column 4))))
      (Thunk_body (path ((Key (Int 0)) (Key (Int 1)) (Id "\206\187") (Id f)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 1)) (Id "\206\187") (Id f))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 4)))))))
           (bind ()) (loc ((line 2) (column 4)))))
         (Functions
          ((paths
            ((((Key (Int 1)) (Id g) (Key (Int 1)) (Id "\206\187") (Id f))
              (Thunk Int)
              ((Key (Int 1)) (Id "\206\187") (Id g) (Key (Int 1)) (Id "\206\187")
               (Id f)))
             (((Key (Int 0)) (Id g) (Key (Int 1)) (Id "\206\187") (Id f))
              (Thunk Unit)
              ((Key (Int 0)) (Id "\206\187") (Id g) (Key (Int 1)) (Id "\206\187")
               (Id f)))))
           (captures ((entries ()) (size_in_bytes 0)))
           (loc ((line 8) (column 2)))))))
       (return
        (Ident (path ((Key (Int 0)) (Id g) (Key (Int 1)) (Id "\206\187") (Id f)))
         (ty (Thunk Unit)) (loc ((line 17) (column 2)))))
       (loc ((line 2) (column 4))))
      (Thunk_body
       (path
        ((Key (Int 1)) (Id "\206\187") (Id g) (Key (Int 0)) (Id "\206\187")
         (Id f)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id y) (Key (Int 1)) (Id "\206\187") (Id g) (Key (Int 0))
               (Id "\206\187") (Id f))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 8) (column 6)))))))
           (bind ()) (loc ((line 8) (column 6)))))))
       (return
        (Scalar (value (Bool true)) (ty Bool) (loc ((line 14) (column 38)))))
       (loc ((line 8) (column 6))))
      (Thunk_body
       (path
        ((Key (Int 0)) (Id "\206\187") (Id g) (Key (Int 0)) (Id "\206\187")
         (Id f)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id y) (Key (Int 0)) (Id "\206\187") (Id g) (Key (Int 0))
               (Id "\206\187") (Id f))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 8) (column 6)))))))
           (bind ()) (loc ((line 8) (column 6)))))))
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 14) (column 31)))))
       (loc ((line 8) (column 6))))
      (Thunk_body (path ((Key (Int 1)) (Key (Int 0)) (Id "\206\187") (Id f)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 0)) (Id "\206\187") (Id f))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 4)))))))
           (bind ()) (loc ((line 2) (column 4)))))
         (Functions
          ((paths
            ((((Key (Int 1)) (Id g) (Key (Int 0)) (Id "\206\187") (Id f))
              (Thunk Bool)
              ((Key (Int 1)) (Id "\206\187") (Id g) (Key (Int 0)) (Id "\206\187")
               (Id f)))
             (((Key (Int 0)) (Id g) (Key (Int 0)) (Id "\206\187") (Id f))
              (Thunk Int)
              ((Key (Int 0)) (Id "\206\187") (Id g) (Key (Int 0)) (Id "\206\187")
               (Id f)))))
           (captures ((entries ()) (size_in_bytes 0)))
           (loc ((line 8) (column 2)))))))
       (return
        (Ident (path ((Key (Int 1)) (Id g) (Key (Int 0)) (Id "\206\187") (Id f)))
         (ty (Thunk Bool)) (loc ((line 17) (column 2)))))
       (loc ((line 2) (column 4))))
      (Thunk_body (path ((Key (Int 0)) (Key (Int 0)) (Id "\206\187") (Id f)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 0)) (Id "\206\187") (Id f))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 4)))))))
           (bind ()) (loc ((line 2) (column 4)))))
         (Functions
          ((paths
            ((((Key (Int 1)) (Id g) (Key (Int 0)) (Id "\206\187") (Id f))
              (Thunk Bool)
              ((Key (Int 1)) (Id "\206\187") (Id g) (Key (Int 0)) (Id "\206\187")
               (Id f)))
             (((Key (Int 0)) (Id g) (Key (Int 0)) (Id "\206\187") (Id f))
              (Thunk Int)
              ((Key (Int 0)) (Id "\206\187") (Id g) (Key (Int 0)) (Id "\206\187")
               (Id f)))))
           (captures ((entries ()) (size_in_bytes 0)))
           (loc ((line 8) (column 2)))))))
       (return
        (Ident (path ((Key (Int 0)) (Id g) (Key (Int 0)) (Id "\206\187") (Id f)))
         (ty (Thunk Int)) (loc ((line 17) (column 2)))))
       (loc ((line 2) (column 4))))
      (Functions
       ((paths
         ((((Key (Int 1)) (Key (Int 1)) (Id f)) (Thunk (Thunk Int))
           ((Key (Int 1)) (Key (Int 1)) (Id "\206\187") (Id f)))
          (((Key (Int 0)) (Key (Int 1)) (Id f)) (Thunk (Thunk Unit))
           ((Key (Int 0)) (Key (Int 1)) (Id "\206\187") (Id f)))
          (((Key (Int 1)) (Key (Int 0)) (Id f)) (Thunk (Thunk Bool))
           ((Key (Int 1)) (Key (Int 0)) (Id "\206\187") (Id f)))
          (((Key (Int 0)) (Key (Int 0)) (Id f)) (Thunk (Thunk Int))
           ((Key (Int 0)) (Key (Int 0)) (Id "\206\187") (Id f)))))
        (captures ((entries ()) (size_in_bytes 0))) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Int 0)) (Id $))) (ty Int)
            (loc ((line 19) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Key (Int 0)) (Id $))
               (Apply_thunk (fn ((Key (Int 0)) (Key (Int 0)) (Id f)))
                (ty (Thunk Int)) (loc ((line 19) (column 8)))))))
            (bind ()) (loc ((line 19) (column 8)))))))
        (loc ((line 19) (column 0)))))
      (Values
       ((exprs
         ((((Shadow 1) (Id _))
           (Apply_thunk (fn ((Key (Int 1)) (Id $))) (ty Bool)
            (loc ((line 20) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Key (Int 1)) (Id $))
               (Apply_thunk (fn ((Key (Int 1)) (Key (Int 0)) (Id f)))
                (ty (Thunk Bool)) (loc ((line 20) (column 8)))))))
            (bind ()) (loc ((line 20) (column 8)))))))
        (loc ((line 20) (column 0)))))
      (Values
       ((exprs
         ((((Shadow 2) (Id _))
           (Apply_thunk (fn ((Key (Int 0)) (Id $))) (ty Unit)
            (loc ((line 21) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Key (Int 0)) (Id $))
               (Apply_thunk (fn ((Key (Int 0)) (Key (Int 1)) (Id f)))
                (ty (Thunk Unit)) (loc ((line 21) (column 8)))))))
            (bind ()) (loc ((line 21) (column 8)))))))
        (loc ((line 21) (column 0)))))
      (Values
       ((exprs
         ((((Shadow 3) (Id _))
           (Apply_thunk (fn ((Key (Int 1)) (Id $))) (ty Int)
            (loc ((line 22) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Key (Int 1)) (Id $))
               (Apply_thunk (fn ((Key (Int 1)) (Key (Int 1)) (Id f)))
                (ty (Thunk Int)) (loc ((line 22) (column 8)))))))
            (bind ()) (loc ((line 22) (column 8)))))))
        (loc ((line 22) (column 0)))))))
    |}]
;;

let%expect_test "static lambda unused arg" =
  go
    {|
let f = fn (static _ : int) -> 42;;
let _ = f 0;;
|};
  [%expect {|
    (lst
     ((Thunk_body (path ((Key (Int 0)) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id _) (Key (Int 0)) (Id "\206\187") (Id f))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))))
       (return (Scalar (value (Int 42)) (ty Int) (loc ((line 2) (column 31)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Int 0)) (Id f))
           (Make_closure (body ((Key (Int 0)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Int))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Int 0)) (Id f))) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
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
    (lst
     ((External (path ((Id "\206\187") (Id syl_print_int)))
       (symbol syl_print_int) (arg_ty Int) (ret_ty Unit)
       (loc ((line 2) (column 0))))
      (Values
       ((exprs
         ((((Id print_int))
           (Make_closure (body ((Id "\206\187") (Id syl_print_int))) (env ())
            (ty (Closure (arg_ty Int) (ret_ty Unit)))
            (loc ((line 2) (column 0)))))))
        (bind ()) (loc ((line 2) (column 0)))))
      (Thunk_body (path ((Key (Int 1)) (Id "\206\187") (Id print)))
       (captures
        (((path ((Id print_int))) (ty (Closure (arg_ty Int) (ret_ty Unit)))
          (offset_in_bytes 0))))
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 1)) (Id "\206\187") (Id print))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 12)))))))
           (bind ()) (loc ((line 3) (column 12)))))))
       (return
        (Apply_closure (fn ((Id print_int)))
         (arg ((Id x) (Key (Int 1)) (Id "\206\187") (Id print))) (arg_ty Int)
         (ty Unit) (loc ((line 3) (column 35)))))
       (loc ((line 3) (column 12))))
      (Thunk_body (path ((Key (Int 0)) (Id "\206\187") (Id print)))
       (captures
        (((path ((Id print_int))) (ty (Closure (arg_ty Int) (ret_ty Unit)))
          (offset_in_bytes 0))))
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 0)) (Id "\206\187") (Id print))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 12)))))))
           (bind ()) (loc ((line 3) (column 12)))))))
       (return
        (Apply_closure (fn ((Id print_int)))
         (arg ((Id x) (Key (Int 0)) (Id "\206\187") (Id print))) (arg_ty Int)
         (ty Unit) (loc ((line 3) (column 35)))))
       (loc ((line 3) (column 12))))
      (Values
       ((exprs
         ((((Key (Int 1)) (Id print))
           (Make_closure (body ((Key (Int 1)) (Id "\206\187") (Id print)))
            (env (((Id env) (Id print)))) (ty (Thunk Unit))
            (loc ((line 3) (column 12)))))
          (((Key (Int 0)) (Id print))
           (Make_closure (body ((Key (Int 0)) (Id "\206\187") (Id print)))
            (env (((Id env) (Id print)))) (ty (Thunk Unit))
            (loc ((line 3) (column 12)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id print))
               (Make_env
                (captures
                 ((entries
                   (((path ((Id print_int)))
                     (ty (Closure (arg_ty Int) (ret_ty Unit)))
                     (offset_in_bytes 0))))
                  (size_in_bytes 16)))
                (ty Env) (loc ((line 3) (column 12)))))))
            (bind ()) (loc ((line 3) (column 12)))))))
        (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Int 0)) (Id print))) (ty Unit)
            (loc ((line 4) (column 8)))))))
        (bind ()) (loc ((line 4) (column 0)))))
      (Values
       ((exprs
         ((((Shadow 1) (Id _))
           (Apply_thunk (fn ((Key (Int 1)) (Id print))) (ty Unit)
            (loc ((line 5) (column 8)))))))
        (bind ()) (loc ((line 5) (column 0)))))))
    |}]
;;

let%expect_test "dependent type: apply polymorphic id to itself" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = (id (int -> int)) (fn (x : int) -> x + 1) 5;;
|};
  [%expect {|
    (lst
     ((Closure_body
       (path
        ((Id "\206\187")
         (Key
          (ArrowT (arg IntT) (arg_mode ((staticity Dynamic) (erasure Unerased)))
           (ret IntT) (ret_mode ((staticity Dynamic) (erasure Unerased)))))
         (Id "\206\187") (Id id)))
       (arg ((Id x))) (arg_ty (Closure (arg_ty Int) (ret_ty Int))) (captures ())
       (bind ())
       (return
        (Ident (path ((Id x))) (ty (Closure (arg_ty Int) (ret_ty Int)))
         (loc ((line 2) (column 54)))))
       (loc ((line 2) (column 40))))
      (Thunk_body
       (path
        ((Key
          (ArrowT (arg IntT) (arg_mode ((staticity Dynamic) (erasure Unerased)))
           (ret IntT) (ret_mode ((staticity Dynamic) (erasure Unerased)))))
         (Id "\206\187") (Id id)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env)
               (Key
                (ArrowT (arg IntT)
                 (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret IntT)
                 (ret_mode ((staticity Dynamic) (erasure Unerased)))))
               (Id "\206\187") (Id id))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 2) (column 40)))))))
           (bind ()) (loc ((line 2) (column 40)))))))
       (return
        (Make_closure
         (body
          ((Id "\206\187")
           (Key
            (ArrowT (arg IntT)
             (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret IntT)
             (ret_mode ((staticity Dynamic) (erasure Unerased)))))
           (Id "\206\187") (Id id)))
         (env
          (((Id env)
            (Key
             (ArrowT (arg IntT)
              (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret IntT)
              (ret_mode ((staticity Dynamic) (erasure Unerased)))))
            (Id "\206\187") (Id id))))
         (ty
          (Closure (arg_ty (Closure (arg_ty Int) (ret_ty Int)))
           (ret_ty (Closure (arg_ty Int) (ret_ty Int)))))
         (loc ((line 2) (column 40)))))
       (loc ((line 2) (column 9))))
      (Values
       ((exprs
         ((((Key
             (ArrowT (arg IntT)
              (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret IntT)
              (ret_mode ((staticity Dynamic) (erasure Unerased)))))
            (Id id))
           (Make_closure
            (body
             ((Key
               (ArrowT (arg IntT)
                (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret IntT)
                (ret_mode ((staticity Dynamic) (erasure Unerased)))))
              (Id "\206\187") (Id id)))
            (env (((Id env) (Id id))))
            (ty
             (Thunk
              (Closure (arg_ty (Closure (arg_ty Int) (ret_ty Int)))
               (ret_ty (Closure (arg_ty Int) (ret_ty Int))))))
            (loc ((line 2) (column 9)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id id))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 9)))))))
            (bind ()) (loc ((line 2) (column 9)))))))
        (loc ((line 2) (column 0)))))
      (Closure_body (path ((Id "\206\187") (Id _))) (arg ((Id x))) (arg_ty Int)
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id $))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 47)))))))
           (bind ()) (loc ((line 3) (column 47)))))))
       (return
        (Binop (op Add) (lhs ((Id x))) (rhs ((Id $))) (ty Int)
         (loc ((line 3) (column 45)))))
       (loc ((line 3) (column 27))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Shadow 2) (Id $))) (arg ((Shadow 3) (Id $)))
            (arg_ty Int) (ty Int) (loc ((line 3) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Apply_thunk
                (fn
                 ((Key
                   (ArrowT (arg IntT)
                    (arg_mode ((staticity Dynamic) (erasure Unerased)))
                    (ret IntT)
                    (ret_mode ((staticity Dynamic) (erasure Unerased)))))
                  (Id id)))
                (ty
                 (Closure (arg_ty (Closure (arg_ty Int) (ret_ty Int)))
                  (ret_ty (Closure (arg_ty Int) (ret_ty Int)))))
                (loc ((line 3) (column 9)))))))
            (bind ()) (loc ((line 3) (column 9)))))
          (Values
           ((exprs
             ((((Id env) (Id _))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 27)))))))
            (bind ()) (loc ((line 3) (column 27)))))
          (Values
           ((exprs
             ((((Shadow 1) (Id $))
               (Make_closure (body ((Id "\206\187") (Id _)))
                (env (((Id env) (Id _))))
                (ty (Closure (arg_ty Int) (ret_ty Int)))
                (loc ((line 3) (column 27)))))))
            (bind ()) (loc ((line 3) (column 27)))))
          (Values
           ((exprs
             ((((Shadow 2) (Id $))
               (Apply_closure (fn ((Id $))) (arg ((Shadow 1) (Id $)))
                (arg_ty (Closure (arg_ty Int) (ret_ty Int)))
                (ty (Closure (arg_ty Int) (ret_ty Int)))
                (loc ((line 3) (column 8)))))))
            (bind ()) (loc ((line 3) (column 8)))))
          (Values
           ((exprs
             ((((Shadow 3) (Id $))
               (Scalar (value (Int 5)) (ty Int) (loc ((line 3) (column 50)))))))
            (bind ()) (loc ((line 3) (column 50)))))))
        (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "if static with bool static arg" =
  go
    {|
let f = fn (static b : bool) -> if static b then 1 else true;;
let _ = f true;;
let _ = f false;;
|};
  [%expect {|
    (lst
     ((Thunk_body (path ((Key (Bool false)) (Id "\206\187") (Id f)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id b) (Key (Bool false)) (Id "\206\187") (Id f))
              (Scalar (value (Bool false)) (ty Bool) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))))
       (return
        (Scalar (value (Bool true)) (ty Bool) (loc ((line 2) (column 56)))))
       (loc ((line 2) (column 8))))
      (Thunk_body (path ((Key (Bool true)) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id b) (Key (Bool true)) (Id "\206\187") (Id f))
              (Scalar (value (Bool true)) (ty Bool) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))))
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 49)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Bool false)) (Id f))
           (Make_closure (body ((Key (Bool false)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Bool))
            (loc ((line 2) (column 8)))))
          (((Key (Bool true)) (Id f))
           (Make_closure (body ((Key (Bool true)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Int))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Bool true)) (Id f))) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Shadow 1) (Id _))
           (Apply_thunk (fn ((Key (Bool false)) (Id f))) (ty Bool)
            (loc ((line 4) (column 8)))))))
        (bind ()) (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "static arg used in arithmetic, result applied" =
  go
    {|
let double = fn (static x : int) -> x + x;;
let _ = double 5;;
|};
  [%expect {|
    (lst
     ((Thunk_body (path ((Key (Int 5)) (Id "\206\187") (Id double)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 5)) (Id "\206\187") (Id double))
              (Scalar (value (Int 5)) (ty Int) (loc ((line 2) (column 13)))))))
           (bind ()) (loc ((line 2) (column 13)))))))
       (return
        (Binop (op Add) (lhs ((Id x) (Key (Int 5)) (Id "\206\187") (Id double)))
         (rhs ((Id x) (Key (Int 5)) (Id "\206\187") (Id double))) (ty Int)
         (loc ((line 2) (column 38)))))
       (loc ((line 2) (column 13))))
      (Values
       ((exprs
         ((((Key (Int 5)) (Id double))
           (Make_closure (body ((Key (Int 5)) (Id "\206\187") (Id double)))
            (env (((Id env) (Id double)))) (ty (Thunk Int))
            (loc ((line 2) (column 13)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id double))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 13)))))))
            (bind ()) (loc ((line 2) (column 13)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Int 5)) (Id double))) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
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
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Key BoolT) (Id "\206\187") (Id id)))
       (arg ((Id x))) (arg_ty Bool) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Bool) (loc ((line 2) (column 54)))))
       (loc ((line 2) (column 40))))
      (Thunk_body (path ((Key BoolT) (Id "\206\187") (Id id))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Key BoolT) (Id "\206\187") (Id id))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 2) (column 40)))))))
           (bind ()) (loc ((line 2) (column 40)))))))
       (return
        (Make_closure
         (body ((Id "\206\187") (Key BoolT) (Id "\206\187") (Id id)))
         (env (((Id env) (Key BoolT) (Id "\206\187") (Id id))))
         (ty (Closure (arg_ty Bool) (ret_ty Bool))) (loc ((line 2) (column 40)))))
       (loc ((line 2) (column 9))))
      (Closure_body (path ((Id "\206\187") (Key IntT) (Id "\206\187") (Id id)))
       (arg ((Id x))) (arg_ty Int) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 2) (column 54)))))
       (loc ((line 2) (column 40))))
      (Thunk_body (path ((Key IntT) (Id "\206\187") (Id id))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Key IntT) (Id "\206\187") (Id id))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 2) (column 40)))))))
           (bind ()) (loc ((line 2) (column 40)))))))
       (return
        (Make_closure (body ((Id "\206\187") (Key IntT) (Id "\206\187") (Id id)))
         (env (((Id env) (Key IntT) (Id "\206\187") (Id id))))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 2) (column 40)))))
       (loc ((line 2) (column 9))))
      (Values
       ((exprs
         ((((Key BoolT) (Id id))
           (Make_closure (body ((Key BoolT) (Id "\206\187") (Id id)))
            (env (((Id env) (Id id))))
            (ty (Thunk (Closure (arg_ty Bool) (ret_ty Bool))))
            (loc ((line 2) (column 9)))))
          (((Key IntT) (Id id))
           (Make_closure (body ((Key IntT) (Id "\206\187") (Id id)))
            (env (((Id env) (Id id))))
            (ty (Thunk (Closure (arg_ty Int) (ret_ty Int))))
            (loc ((line 2) (column 9)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id id))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 9)))))))
            (bind ()) (loc ((line 2) (column 9)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id f))
           (Apply_thunk (fn ((Key IntT) (Id id)))
            (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id g))
           (Apply_thunk (fn ((Key BoolT) (Id id)))
            (ty (Closure (arg_ty Bool) (ret_ty Bool)))
            (loc ((line 4) (column 8)))))))
        (bind ()) (loc ((line 4) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id f))) (arg ((Id $))) (arg_ty Int) (ty Int)
            (loc ((line 5) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 5) (column 10)))))))
            (bind ()) (loc ((line 5) (column 10)))))))
        (loc ((line 5) (column 0)))))
      (Values
       ((exprs
         ((((Shadow 1) (Id _))
           (Apply_closure (fn ((Id g))) (arg ((Id $))) (arg_ty Bool) (ty Bool)
            (loc ((line 6) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value (Bool true)) (ty Bool)
                (loc ((line 6) (column 10)))))))
            (bind ()) (loc ((line 6) (column 10)))))))
        (loc ((line 6) (column 0)))))))
    |}]
;;

let%expect_test "symbolic arrow type as static arg" =
  go
    {|
let choose = fn (static f : static int \ x -> if x == 0 then int else bool) -> f 0;;
let _ = choose (fn (static x : int) -> if static x == 0 then 0 else true);;
|};
  [%expect {|
    (lst
     ((Thunk_body
       (path
        ((Key (Int 0)) (Id "\206\187") (Id f) (Key (Closure 4)) (Id "\206\187")
         (Id choose)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 0)) (Id "\206\187") (Id f) (Key (Closure 4))
               (Id "\206\187") (Id choose))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 13)))))))
           (bind ()) (loc ((line 2) (column 13)))))))
       (return (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 61)))))
       (loc ((line 2) (column 13))))
      (Thunk_body (path ((Key (Closure 4)) (Id "\206\187") (Id choose)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Key (Int 0)) (Id f) (Key (Closure 4)) (Id "\206\187")
               (Id choose))
              (Make_closure
               (body
                ((Key (Int 0)) (Id "\206\187") (Id f) (Key (Closure 4))
                 (Id "\206\187") (Id choose)))
               (env
                (((Id env) (Id f) (Key (Closure 4)) (Id "\206\187") (Id choose))))
               (ty (Thunk Int)) (loc ((line 2) (column 13)))))))
           (bind
            ((Values
              ((exprs
                ((((Id env) (Id f) (Key (Closure 4)) (Id "\206\187") (Id choose))
                  (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                   (loc ((line 2) (column 13)))))))
               (bind ()) (loc ((line 2) (column 13)))))))
           (loc ((line 2) (column 13)))))))
       (return
        (Apply_thunk
         (fn
          ((Key (Int 0)) (Id f) (Key (Closure 4)) (Id "\206\187") (Id choose)))
         (ty Int) (loc ((line 2) (column 79)))))
       (loc ((line 2) (column 13))))
      (Values
       ((exprs
         ((((Key (Closure 4)) (Id choose))
           (Make_closure (body ((Key (Closure 4)) (Id "\206\187") (Id choose)))
            (env (((Id env) (Id choose)))) (ty (Thunk Int))
            (loc ((line 2) (column 13)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id choose))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 13)))))))
            (bind ()) (loc ((line 2) (column 13)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Closure 4)) (Id choose))) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id n))
           (Scalar (value (Int 10)) (ty Int) (loc ((line 2) (column 8)))))))
        (bind ()) (loc ((line 2) (column 0)))))
      (Thunk_body (path ((Key (Int 5)) (Id "\206\187") (Id f)))
       (captures (((path ((Id n))) (ty Int) (offset_in_bytes 0))))
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 5)) (Id "\206\187") (Id f))
              (Scalar (value (Int 5)) (ty Int) (loc ((line 3) (column 8)))))))
           (bind ()) (loc ((line 3) (column 8)))))))
       (return
        (Binop (op Add) (lhs ((Id x) (Key (Int 5)) (Id "\206\187") (Id f)))
         (rhs ((Id n))) (ty Int) (loc ((line 3) (column 33)))))
       (loc ((line 3) (column 8))))
      (Values
       ((exprs
         ((((Key (Int 5)) (Id f))
           (Make_closure (body ((Key (Int 5)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Int))
            (loc ((line 3) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env
                (captures
                 ((entries (((path ((Id n))) (ty Int) (offset_in_bytes 0))))
                  (size_in_bytes 8)))
                (ty Env) (loc ((line 3) (column 8)))))))
            (bind ()) (loc ((line 3) (column 8)))))))
        (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Int 5)) (Id f))) (ty Int)
            (loc ((line 4) (column 8)))))))
        (bind ()) (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "static lambda with type annotation on body" =
  go
    {|
let f = fn (static x : int) -> (x : int);;
let _ = f 42;;
|};
  [%expect {|
    (lst
     ((Thunk_body (path ((Key (Int 42)) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 42)) (Id "\206\187") (Id f))
              (Scalar (value (Int 42)) (ty Int) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))))
       (return
        (Ident (path ((Id x) (Key (Int 42)) (Id "\206\187") (Id f))) (ty Int)
         (loc ((line 2) (column 32)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Int 42)) (Id f))
           (Make_closure (body ((Key (Int 42)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Int))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Int 42)) (Id f))) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "if static in type annotation position" =
  go
    {|
let f = fn (static b : bool) -> (if static b then 0 else true) : (if b then int else bool);;
let _ = f true;;
let _ = f false;;
|};
  [%expect {|
    (lst
     ((Thunk_body (path ((Key (Bool false)) (Id "\206\187") (Id f)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id b) (Key (Bool false)) (Id "\206\187") (Id f))
              (Scalar (value (Bool false)) (ty Bool) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))))
       (return
        (Scalar (value (Bool true)) (ty Bool) (loc ((line 2) (column 57)))))
       (loc ((line 2) (column 8))))
      (Thunk_body (path ((Key (Bool true)) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id b) (Key (Bool true)) (Id "\206\187") (Id f))
              (Scalar (value (Bool true)) (ty Bool) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))))
       (return (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 50)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Bool false)) (Id f))
           (Make_closure (body ((Key (Bool false)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Bool))
            (loc ((line 2) (column 8)))))
          (((Key (Bool true)) (Id f))
           (Make_closure (body ((Key (Bool true)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Int))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Bool true)) (Id f))) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Shadow 1) (Id _))
           (Apply_thunk (fn ((Key (Bool false)) (Id f))) (ty Bool)
            (loc ((line 4) (column 8)))))))
        (bind ()) (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "higher-order static: take a static function and apply it" =
  go
    {|
let apply = fn (static f : static int -> int) -> f 5;;
let _ = apply (fn (static x : int) -> x + 1);;
|};
  [%expect {|
    (lst
     ((Thunk_body
       (path
        ((Key (Int 5)) (Id "\206\187") (Id f) (Key (Closure 4)) (Id "\206\187")
         (Id apply)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 5)) (Id "\206\187") (Id f) (Key (Closure 4))
               (Id "\206\187") (Id apply))
              (Scalar (value (Int 5)) (ty Int) (loc ((line 2) (column 12)))))))
           (bind ()) (loc ((line 2) (column 12)))))
         (Values
          ((exprs
            ((((Id $))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 42)))))))
           (bind ()) (loc ((line 3) (column 42)))))))
       (return
        (Binop (op Add)
         (lhs
          ((Id x) (Key (Int 5)) (Id "\206\187") (Id f) (Key (Closure 4))
           (Id "\206\187") (Id apply)))
         (rhs ((Id $))) (ty Int) (loc ((line 3) (column 40)))))
       (loc ((line 2) (column 12))))
      (Thunk_body (path ((Key (Closure 4)) (Id "\206\187") (Id apply)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Key (Int 5)) (Id f) (Key (Closure 4)) (Id "\206\187") (Id apply))
              (Make_closure
               (body
                ((Key (Int 5)) (Id "\206\187") (Id f) (Key (Closure 4))
                 (Id "\206\187") (Id apply)))
               (env
                (((Id env) (Id f) (Key (Closure 4)) (Id "\206\187") (Id apply))))
               (ty (Thunk Int)) (loc ((line 2) (column 12)))))))
           (bind
            ((Values
              ((exprs
                ((((Id env) (Id f) (Key (Closure 4)) (Id "\206\187") (Id apply))
                  (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                   (loc ((line 2) (column 12)))))))
               (bind ()) (loc ((line 2) (column 12)))))))
           (loc ((line 2) (column 12)))))))
       (return
        (Apply_thunk
         (fn ((Key (Int 5)) (Id f) (Key (Closure 4)) (Id "\206\187") (Id apply)))
         (ty Int) (loc ((line 2) (column 49)))))
       (loc ((line 2) (column 12))))
      (Values
       ((exprs
         ((((Key (Closure 4)) (Id apply))
           (Make_closure (body ((Key (Closure 4)) (Id "\206\187") (Id apply)))
            (env (((Id env) (Id apply)))) (ty (Thunk Int))
            (loc ((line 2) (column 12)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id apply))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 12)))))))
            (bind ()) (loc ((line 2) (column 12)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Closure 4)) (Id apply))) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "multiple static erased type args" =
  go
    {|
let f = fn (static erased t1 : type) -> fn (static erased t2 : type) -> fn (x : t1) -> fn (y : t2) -> x;;
let _ = f int bool 0 true;;
|};
  [%expect {|
    (lst
     ((Closure_body
       (path
        ((Id "\206\187") (Id "\206\187") (Key BoolT) (Id "\206\187") (Key IntT)
         (Id "\206\187") (Id f)))
       (arg ((Id y))) (arg_ty Bool)
       (captures (((path ((Id x))) (ty Int) (offset_in_bytes 0)))) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 2) (column 102)))))
       (loc ((line 2) (column 87))))
      (Closure_body
       (path
        ((Id "\206\187") (Key BoolT) (Id "\206\187") (Key IntT) (Id "\206\187")
         (Id f)))
       (arg ((Id x))) (arg_ty Int) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Id "\206\187") (Key BoolT) (Id "\206\187") (Key IntT)
               (Id "\206\187") (Id f))
              (Make_env
               (captures
                ((entries (((path ((Id x))) (ty Int) (offset_in_bytes 0))))
                 (size_in_bytes 8)))
               (ty Env) (loc ((line 2) (column 87)))))))
           (bind ()) (loc ((line 2) (column 87)))))))
       (return
        (Make_closure
         (body
          ((Id "\206\187") (Id "\206\187") (Key BoolT) (Id "\206\187") (Key IntT)
           (Id "\206\187") (Id f)))
         (env
          (((Id env) (Id "\206\187") (Key BoolT) (Id "\206\187") (Key IntT)
            (Id "\206\187") (Id f))))
         (ty (Closure (arg_ty Bool) (ret_ty Int))) (loc ((line 2) (column 87)))))
       (loc ((line 2) (column 72))))
      (Thunk_body
       (path ((Key BoolT) (Id "\206\187") (Key IntT) (Id "\206\187") (Id f)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Key BoolT) (Id "\206\187") (Key IntT) (Id "\206\187")
               (Id f))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 2) (column 72)))))))
           (bind ()) (loc ((line 2) (column 72)))))))
       (return
        (Make_closure
         (body
          ((Id "\206\187") (Key BoolT) (Id "\206\187") (Key IntT) (Id "\206\187")
           (Id f)))
         (env
          (((Id env) (Key BoolT) (Id "\206\187") (Key IntT) (Id "\206\187")
            (Id f))))
         (ty
          (Closure (arg_ty Int) (ret_ty (Closure (arg_ty Bool) (ret_ty Int)))))
         (loc ((line 2) (column 72)))))
       (loc ((line 2) (column 40))))
      (Thunk_body (path ((Key BoolT) (Key IntT) (Id "\206\187") (Id f)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Key IntT) (Id "\206\187") (Id f))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 2) (column 40)))))))
           (bind ()) (loc ((line 2) (column 40)))))))
       (return
        (Make_closure
         (body ((Key BoolT) (Id "\206\187") (Key IntT) (Id "\206\187") (Id f)))
         (env (((Id env) (Key IntT) (Id "\206\187") (Id f))))
         (ty
          (Thunk
           (Closure (arg_ty Int) (ret_ty (Closure (arg_ty Bool) (ret_ty Int))))))
         (loc ((line 2) (column 40)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key BoolT) (Key IntT) (Id f))
           (Make_closure (body ((Key BoolT) (Key IntT) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f))))
            (ty
             (Thunk
              (Thunk
               (Closure (arg_ty Int)
                (ret_ty (Closure (arg_ty Bool) (ret_ty Int)))))))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Shadow 3) (Id $))) (arg ((Shadow 4) (Id $)))
            (arg_ty Bool) (ty Int) (loc ((line 3) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Key BoolT) (Id $))
               (Apply_thunk (fn ((Key BoolT) (Key IntT) (Id f)))
                (ty
                 (Thunk
                  (Closure (arg_ty Int)
                   (ret_ty (Closure (arg_ty Bool) (ret_ty Int))))))
                (loc ((line 3) (column 8)))))))
            (bind ()) (loc ((line 3) (column 8)))))
          (Values
           ((exprs
             ((((Shadow 1) (Id $))
               (Apply_thunk (fn ((Key BoolT) (Id $)))
                (ty
                 (Closure (arg_ty Int)
                  (ret_ty (Closure (arg_ty Bool) (ret_ty Int)))))
                (loc ((line 3) (column 8)))))))
            (bind ()) (loc ((line 3) (column 8)))))
          (Values
           ((exprs
             ((((Shadow 2) (Id $))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 19)))))))
            (bind ()) (loc ((line 3) (column 19)))))
          (Values
           ((exprs
             ((((Shadow 3) (Id $))
               (Apply_closure (fn ((Shadow 1) (Id $))) (arg ((Shadow 2) (Id $)))
                (arg_ty Int) (ty (Closure (arg_ty Bool) (ret_ty Int)))
                (loc ((line 3) (column 8)))))))
            (bind ()) (loc ((line 3) (column 8)))))
          (Values
           ((exprs
             ((((Shadow 4) (Id $))
               (Scalar (value (Bool true)) (ty Bool)
                (loc ((line 3) (column 21)))))))
            (bind ()) (loc ((line 3) (column 21)))))))
        (loc ((line 3) (column 0)))))))
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
    (lst
     ((Thunk_body (path ((Key (Int 1)) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 1)) (Id "\206\187") (Id f))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))
         (Values
          ((exprs
            ((((Id y) (Key (Int 1)) (Id "\206\187") (Id f))
              (Scalar (value (Bool true)) (ty Bool) (loc ((line 3) (column 39)))))))
           (bind ()) (loc ((line 3) (column 2)))))))
       (return
        (Ident (path ((Id y) (Key (Int 1)) (Id "\206\187") (Id f))) (ty Bool)
         (loc ((line 4) (column 2)))))
       (loc ((line 2) (column 8))))
      (Thunk_body (path ((Key (Int 0)) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 0)) (Id "\206\187") (Id f))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))
         (Values
          ((exprs
            ((((Id y) (Key (Int 0)) (Id "\206\187") (Id f))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 32)))))))
           (bind ()) (loc ((line 3) (column 2)))))))
       (return
        (Ident (path ((Id y) (Key (Int 0)) (Id "\206\187") (Id f))) (ty Int)
         (loc ((line 4) (column 2)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Int 1)) (Id f))
           (Make_closure (body ((Key (Int 1)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Bool))
            (loc ((line 2) (column 8)))))
          (((Key (Int 0)) (Id f))
           (Make_closure (body ((Key (Int 0)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Int))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Int 0)) (Id f))) (ty Int)
            (loc ((line 5) (column 8)))))))
        (bind ()) (loc ((line 5) (column 0)))))
      (Values
       ((exprs
         ((((Shadow 1) (Id _))
           (Apply_thunk (fn ((Key (Int 1)) (Id f))) (ty Bool)
            (loc ((line 6) (column 8)))))))
        (bind ()) (loc ((line 6) (column 0)))))))
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
  [%expect {|
    (lst
     ((Closure_body
       (path ((Id "\206\187") (Key (Closure 10)) (Id "\206\187") (Id f)))
       (arg ((Id x))) (arg_ty Int) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 2) (column 83)))))
       (loc ((line 2) (column 67))))
      (Thunk_body (path ((Key (Closure 10)) (Id "\206\187") (Id f)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Key (Closure 10)) (Id "\206\187") (Id f))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 2) (column 67)))))))
           (bind ()) (loc ((line 2) (column 67)))))))
       (return
        (Make_closure
         (body ((Id "\206\187") (Key (Closure 10)) (Id "\206\187") (Id f)))
         (env (((Id env) (Key (Closure 10)) (Id "\206\187") (Id f))))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 2) (column 67)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Closure 10)) (Id f))
           (Make_closure (body ((Key (Closure 10)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f))))
            (ty (Thunk (Closure (arg_ty Int) (ret_ty Int))))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs ())
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id h))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 8)))))))
            (bind ()) (loc ((line 3) (column 8)))))))
        (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Key (Closure 10)) (Id x))
           (Ident (path ((Key (Closure 10)) (Id f)))
            (ty (Thunk (Closure (arg_ty Int) (ret_ty Int))))
            (loc ((line 4) (column 21)))))))
        (bind ()) (loc ((line 4) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Closure 10)) (Id x)))
            (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 5) (column 8)))))))
        (bind ()) (loc ((line 5) (column 0)))))))
    |}]
;;

let%expect_test "leq Pi/Pi function-type arg returning type" =
  go
    {|
let wrap = fn (static erased f : static int -> static erased type) -> fn (x : f 0) -> x;;
let wrap2 = wrap : static erased (static int -> static erased type) \ f -> f 0 -> f 0;;
let _ = wrap2 (fn (static x : int) -> int);;
|};
  [%expect {|
    (lst
     ((Closure_body
       (path ((Id "\206\187") (Key (Closure 9)) (Id "\206\187") (Id wrap)))
       (arg ((Id x))) (arg_ty Int) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 2) (column 86)))))
       (loc ((line 2) (column 70))))
      (Thunk_body (path ((Key (Closure 9)) (Id "\206\187") (Id wrap)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Key (Closure 9)) (Id "\206\187") (Id wrap))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 2) (column 70)))))))
           (bind ()) (loc ((line 2) (column 70)))))))
       (return
        (Make_closure
         (body ((Id "\206\187") (Key (Closure 9)) (Id "\206\187") (Id wrap)))
         (env (((Id env) (Key (Closure 9)) (Id "\206\187") (Id wrap))))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 2) (column 70)))))
       (loc ((line 2) (column 11))))
      (Values
       ((exprs
         ((((Key (Closure 9)) (Id wrap))
           (Make_closure (body ((Key (Closure 9)) (Id "\206\187") (Id wrap)))
            (env (((Id env) (Id wrap))))
            (ty (Thunk (Closure (arg_ty Int) (ret_ty Int))))
            (loc ((line 2) (column 11)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id wrap))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 11)))))))
            (bind ()) (loc ((line 2) (column 11)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Key (Closure 9)) (Id wrap2))
           (Ident (path ((Key (Closure 9)) (Id wrap)))
            (ty (Thunk (Closure (arg_ty Int) (ret_ty Int))))
            (loc ((line 3) (column 12)))))))
        (bind ()) (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Closure 9)) (Id wrap2)))
            (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 4) (column 8)))))))
        (bind ()) (loc ((line 4) (column 0)))))))
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
    (lst
     ((Thunk_body
       (path
        ((Key (Int 0)) (Id "\206\187") (Id f) (Key (Closure 20)) (Id "\206\187")
         (Id apply) (Key (Closure 17)) (Id "\206\187") (Id f)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 0)) (Id "\206\187") (Id f) (Key (Closure 20))
               (Id "\206\187") (Id apply) (Key (Closure 17)) (Id "\206\187")
               (Id f))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))))
       (return (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 95)))))
       (loc ((line 2) (column 8))))
      (Thunk_body
       (path
        ((Key (Closure 20)) (Id "\206\187") (Id apply) (Key (Closure 17))
         (Id "\206\187") (Id f)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Key (Int 0)) (Id f) (Key (Closure 20)) (Id "\206\187") (Id apply)
               (Key (Closure 17)) (Id "\206\187") (Id f))
              (Make_closure
               (body
                ((Key (Int 0)) (Id "\206\187") (Id f) (Key (Closure 20))
                 (Id "\206\187") (Id apply) (Key (Closure 17)) (Id "\206\187")
                 (Id f)))
               (env
                (((Id env) (Id f) (Key (Closure 20)) (Id "\206\187") (Id apply)
                  (Key (Closure 17)) (Id "\206\187") (Id f))))
               (ty (Thunk Int)) (loc ((line 2) (column 8)))))))
           (bind
            ((Values
              ((exprs
                ((((Id env) (Id f) (Key (Closure 20)) (Id "\206\187") (Id apply)
                   (Key (Closure 17)) (Id "\206\187") (Id f))
                  (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                   (loc ((line 2) (column 8)))))))
               (bind ()) (loc ((line 2) (column 8)))))))
           (loc ((line 2) (column 8)))))))
       (return
        (Apply_thunk
         (fn
          ((Key (Int 0)) (Id f) (Key (Closure 20)) (Id "\206\187") (Id apply)
           (Key (Closure 17)) (Id "\206\187") (Id f)))
         (ty Int) (loc ((line 5) (column 48)))))
       (loc ((line 2) (column 8))))
      (Thunk_body (path ((Key (Closure 17)) (Id "\206\187") (Id f)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Key (Closure 20)) (Id apply) (Key (Closure 17)) (Id "\206\187")
               (Id f))
              (Make_closure
               (body
                ((Key (Closure 20)) (Id "\206\187") (Id apply) (Key (Closure 17))
                 (Id "\206\187") (Id f)))
               (env
                (((Id env) (Id apply) (Key (Closure 17)) (Id "\206\187") (Id f))))
               (ty (Thunk Int)) (loc ((line 2) (column 8)))))))
           (bind
            ((Values
              ((exprs
                ((((Id env) (Id apply) (Key (Closure 17)) (Id "\206\187") (Id f))
                  (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                   (loc ((line 2) (column 8)))))))
               (bind ()) (loc ((line 2) (column 8)))))))
           (loc ((line 2) (column 8)))))))
       (return
        (Apply_thunk
         (fn
          ((Key (Closure 20)) (Id apply) (Key (Closure 17)) (Id "\206\187")
           (Id f)))
         (ty Int) (loc ((line 2) (column 65)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Closure 17)) (Id f))
           (Make_closure (body ((Key (Closure 17)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Int))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs ())
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id g))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 3) (column 8)))))))
            (bind ()) (loc ((line 3) (column 8)))))))
        (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Key (Closure 17)) (Id x))
           (Ident (path ((Key (Closure 17)) (Id f))) (ty (Thunk Int))
            (loc ((line 4) (column 21)))))))
        (bind ()) (loc ((line 4) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Closure 17)) (Id x))) (ty Int)
            (loc ((line 5) (column 8)))))))
        (bind ()) (loc ((line 5) (column 0)))))))
    |}]
;;

let%expect_test "arrow function calling pi function in same group" =
  go
    {|
fun f (static erased t : type) : t -> t = fn (x : t) -> x
and g (x : int) : int = f int x;;
let _ = g 5;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Key IntT) (Id "\206\187") (Id f)))
       (arg ((Id x))) (arg_ty Int) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 2) (column 56)))))
       (loc ((line 2) (column 42))))
      (Thunk_body (path ((Key IntT) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Key IntT) (Id "\206\187") (Id f))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 2) (column 42)))))))
           (bind ()) (loc ((line 2) (column 42)))))))
       (return
        (Make_closure (body ((Id "\206\187") (Key IntT) (Id "\206\187") (Id f)))
         (env (((Id env) (Key IntT) (Id "\206\187") (Id f))))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 2) (column 42)))))
       (loc ((line 2) (column 4))))
      (Closure_body (path ((Id "\206\187") (Id g))) (arg ((Id x))) (arg_ty Int)
       (captures
        (((path ((Key IntT) (Id f)))
          (ty (Thunk (Closure (arg_ty Int) (ret_ty Int)))) (offset_in_bytes 0))))
       (bind
        ((Values
          ((exprs
            ((((Id $))
              (Apply_thunk (fn ((Key IntT) (Id f)))
               (ty (Closure (arg_ty Int) (ret_ty Int)))
               (loc ((line 3) (column 24)))))))
           (bind ()) (loc ((line 3) (column 24)))))))
       (return
        (Apply_closure (fn ((Id $))) (arg ((Id x))) (arg_ty Int) (ty Int)
         (loc ((line 3) (column 24)))))
       (loc ((line 3) (column 4))))
      (Functions
       ((paths
         ((((Key IntT) (Id f)) (Thunk (Closure (arg_ty Int) (ret_ty Int)))
           ((Key IntT) (Id "\206\187") (Id f)))
          (((Id g)) (Closure (arg_ty Int) (ret_ty Int)) ((Id "\206\187") (Id g)))))
        (captures
         ((entries
           (((path ((Key IntT) (Id f)))
             (ty (Thunk (Closure (arg_ty Int) (ret_ty Int))))
             (offset_in_bytes 0))))
          (size_in_bytes 16)))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id g))) (arg ((Id $))) (arg_ty Int) (ty Int)
            (loc ((line 4) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value (Int 5)) (ty Int) (loc ((line 4) (column 10)))))))
            (bind ()) (loc ((line 4) (column 10)))))))
        (loc ((line 4) (column 0)))))))
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
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id inc))) (arg ((Id x))) (arg_ty Int)
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id $))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 30)))))))
           (bind ()) (loc ((line 2) (column 30)))))))
       (return
        (Binop (op Add) (lhs ((Id x))) (rhs ((Id $))) (ty Int)
         (loc ((line 2) (column 28)))))
       (loc ((line 2) (column 4))))
      (Closure_body
       (path ((Id "\206\187") (Key (Bool false)) (Id "\206\187") (Id choose)))
       (arg ((Id x))) (arg_ty Int) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 4) (column 62)))))
       (loc ((line 4) (column 46))))
      (Thunk_body (path ((Key (Bool false)) (Id "\206\187") (Id choose)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Key (Bool false)) (Id "\206\187") (Id choose))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 4) (column 46)))))))
           (bind ()) (loc ((line 4) (column 46)))))))
       (return
        (Make_closure
         (body ((Id "\206\187") (Key (Bool false)) (Id "\206\187") (Id choose)))
         (env (((Id env) (Key (Bool false)) (Id "\206\187") (Id choose))))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 4) (column 46)))))
       (loc ((line 3) (column 4))))
      (Closure_body
       (path ((Id "\206\187") (Key (Bool true)) (Id "\206\187") (Id choose)))
       (arg ((Id x))) (arg_ty Int)
       (captures
        (((path ((Id inc))) (ty (Closure (arg_ty Int) (ret_ty Int)))
          (offset_in_bytes 0))))
       (bind ())
       (return
        (Apply_closure (fn ((Id inc))) (arg ((Id x))) (arg_ty Int) (ty Int)
         (loc ((line 4) (column 35)))))
       (loc ((line 4) (column 19))))
      (Thunk_body (path ((Key (Bool true)) (Id "\206\187") (Id choose)))
       (captures
        (((path ((Id inc))) (ty (Closure (arg_ty Int) (ret_ty Int)))
          (offset_in_bytes 0))))
       (bind
        ((Values
          ((exprs
            ((((Id env) (Key (Bool true)) (Id "\206\187") (Id choose))
              (Make_env
               (captures
                ((entries
                  (((path ((Id inc))) (ty (Closure (arg_ty Int) (ret_ty Int)))
                    (offset_in_bytes 0))))
                 (size_in_bytes 16)))
               (ty Env) (loc ((line 4) (column 19)))))))
           (bind ()) (loc ((line 4) (column 19)))))))
       (return
        (Make_closure
         (body ((Id "\206\187") (Key (Bool true)) (Id "\206\187") (Id choose)))
         (env (((Id env) (Key (Bool true)) (Id "\206\187") (Id choose))))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 4) (column 19)))))
       (loc ((line 3) (column 4))))
      (Functions
       ((paths
         ((((Id inc)) (Closure (arg_ty Int) (ret_ty Int))
           ((Id "\206\187") (Id inc)))
          (((Key (Bool false)) (Id choose))
           (Thunk (Closure (arg_ty Int) (ret_ty Int)))
           ((Key (Bool false)) (Id "\206\187") (Id choose)))
          (((Key (Bool true)) (Id choose))
           (Thunk (Closure (arg_ty Int) (ret_ty Int)))
           ((Key (Bool true)) (Id "\206\187") (Id choose)))))
        (captures
         ((entries
           (((path ((Id inc))) (ty (Closure (arg_ty Int) (ret_ty Int)))
             (offset_in_bytes 0))))
          (size_in_bytes 16)))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id $))) (arg ((Shadow 1) (Id $))) (arg_ty Int)
            (ty Int) (loc ((line 5) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Apply_thunk (fn ((Key (Bool true)) (Id choose)))
                (ty (Closure (arg_ty Int) (ret_ty Int)))
                (loc ((line 5) (column 8)))))))
            (bind ()) (loc ((line 5) (column 8)))))
          (Values
           ((exprs
             ((((Shadow 1) (Id $))
               (Scalar (value (Int 5)) (ty Int) (loc ((line 5) (column 20)))))))
            (bind ()) (loc ((line 5) (column 20)))))))
        (loc ((line 5) (column 0)))))
      (Values
       ((exprs
         ((((Shadow 1) (Id _))
           (Apply_closure (fn ((Id $))) (arg ((Shadow 1) (Id $))) (arg_ty Int)
            (ty Int) (loc ((line 6) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Apply_thunk (fn ((Key (Bool false)) (Id choose)))
                (ty (Closure (arg_ty Int) (ret_ty Int)))
                (loc ((line 6) (column 8)))))))
            (bind ()) (loc ((line 6) (column 8)))))
          (Values
           ((exprs
             ((((Shadow 1) (Id $))
               (Scalar (value (Int 5)) (ty Int) (loc ((line 6) (column 21)))))))
            (bind ()) (loc ((line 6) (column 21)))))))
        (loc ((line 6) (column 0)))))))
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
    (lst
     ((Closure_body (path ((Id "\206\187") (Id g))) (arg ((Id y))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 39)))))
       (loc ((line 3) (column 4))))
      (Functions
       ((paths
         ((((Id g)) (Closure (arg_ty Int) (ret_ty Int)) ((Id "\206\187") (Id g)))))
        (captures ((entries ()) (size_in_bytes 0))) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id g))) (arg ((Id $))) (arg_ty Int) (ty Int)
            (loc ((line 4) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 10)))))))
            (bind ()) (loc ((line 4) (column 10)))))))
        (loc ((line 4) (column 0)))))))
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
    (lst
     ((Closure_body (path ((Id "\206\187") (Id f))) (arg ((Id x))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 39)))))
       (loc ((line 2) (column 4))))
      (Functions
       ((paths
         ((((Id f)) (Closure (arg_ty Int) (ret_ty Int)) ((Id "\206\187") (Id f)))))
        (captures ((entries ()) (size_in_bytes 0))) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id f))) (arg ((Id $))) (arg_ty Int) (ty Int)
            (loc ((line 4) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 10)))))))
            (bind ()) (loc ((line 4) (column 10)))))))
        (loc ((line 4) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 31)))))))
        (bind
         ((Values
           ((exprs
             ((((Id x) (Id _))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 10)))))))
            (bind ()) (loc ((line 3) (column 8)))))))
        (loc ((line 3) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 31)))))))
        (bind
         ((Values
           ((exprs
             ((((Id x) (Id _))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 10)))))))
            (bind ()) (loc ((line 3) (column 8)))))))
        (loc ((line 3) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 38)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 38)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = g x
and erased g (y : int) : int = f y;;
let _ = (f @ erased) 0;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id f))) (arg ((Id x))) (arg_ty Int)
       (captures
        (((path ((Id f))) (ty (Closure (arg_ty Int) (ret_ty Int)))
          (offset_in_bytes 0))))
       (bind
        ((Values
          ((exprs
            ((((Id y) (Id "\206\187") (Id f))
              (Ident (path ((Id x))) (ty Int) (loc ((line 2) (column 26)))))))
           (bind ()) (loc ((line 2) (column 24)))))))
       (return
        (Apply_closure (fn ((Id f))) (arg ((Id y) (Id "\206\187") (Id f)))
         (arg_ty Int) (ty Int) (loc ((line 3) (column 31)))))
       (loc ((line 2) (column 4))))
      (Functions
       ((paths
         ((((Id f)) (Closure (arg_ty Int) (ret_ty Int)) ((Id "\206\187") (Id f)))))
        (captures
         ((entries
           (((path ((Id f))) (ty (Closure (arg_ty Int) (ret_ty Int)))
             (offset_in_bytes 0))))
          (size_in_bytes 16)))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id f))) (arg ((Id y) (Id _))) (arg_ty Int)
            (ty Int) (loc ((line 3) (column 31)))))))
        (bind
         ((Values
           ((exprs
             ((((Id x) (Id _))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 21)))))))
            (bind ()) (loc ((line 4) (column 8)))))
          (Values
           ((exprs
             ((((Id y) (Id _))
               (Ident (path ((Id x) (Id _))) (ty Int)
                (loc ((line 2) (column 26)))))))
            (bind ()) (loc ((line 2) (column 24)))))))
        (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "static lambda" =
  go
    {|
let _ = (fn (static x : int) -> x + 1) 0;;
|};
  [%expect {|
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Binop (op Add) (lhs ((Id x) (Id _))) (rhs ((Id $))) (ty Int)
            (loc ((line 2) (column 34)))))))
        (bind
         ((Values
           ((exprs
             ((((Id x) (Id _))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 9)))))))
            (bind ()) (loc ((line 2) (column 9)))))
          (Values
           ((exprs
             ((((Id $))
               (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 36)))))))
            (bind ()) (loc ((line 2) (column 36)))))))
        (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "pi function calling arrow function in same group" =
  go
    {|
fun inc (x : int) : int = x + 1
and f (static erased t : type) : t -> t = fn (x : t) -> x;;
let _ = f int 0;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id inc))) (arg ((Id x))) (arg_ty Int)
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id $))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 30)))))))
           (bind ()) (loc ((line 2) (column 30)))))))
       (return
        (Binop (op Add) (lhs ((Id x))) (rhs ((Id $))) (ty Int)
         (loc ((line 2) (column 28)))))
       (loc ((line 2) (column 4))))
      (Closure_body (path ((Id "\206\187") (Key IntT) (Id "\206\187") (Id f)))
       (arg ((Id x))) (arg_ty Int) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 3) (column 56)))))
       (loc ((line 3) (column 42))))
      (Thunk_body (path ((Key IntT) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Key IntT) (Id "\206\187") (Id f))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 3) (column 42)))))))
           (bind ()) (loc ((line 3) (column 42)))))))
       (return
        (Make_closure (body ((Id "\206\187") (Key IntT) (Id "\206\187") (Id f)))
         (env (((Id env) (Key IntT) (Id "\206\187") (Id f))))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 3) (column 42)))))
       (loc ((line 3) (column 4))))
      (Functions
       ((paths
         ((((Id inc)) (Closure (arg_ty Int) (ret_ty Int))
           ((Id "\206\187") (Id inc)))
          (((Key IntT) (Id f)) (Thunk (Closure (arg_ty Int) (ret_ty Int)))
           ((Key IntT) (Id "\206\187") (Id f)))))
        (captures ((entries ()) (size_in_bytes 0))) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id $))) (arg ((Shadow 1) (Id $))) (arg_ty Int)
            (ty Int) (loc ((line 4) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Apply_thunk (fn ((Key IntT) (Id f)))
                (ty (Closure (arg_ty Int) (ret_ty Int)))
                (loc ((line 4) (column 8)))))))
            (bind ()) (loc ((line 4) (column 8)))))
          (Values
           ((exprs
             ((((Shadow 1) (Id $))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 14)))))))
            (bind ()) (loc ((line 4) (column 14)))))))
        (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "arrow function calling pi function in same group" =
  go
    {|
fun f (static erased t : type) : t -> t = fn (x : t) -> x
and g (x : int) : int = f int x;;
let _ = g 5;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Key IntT) (Id "\206\187") (Id f)))
       (arg ((Id x))) (arg_ty Int) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 2) (column 56)))))
       (loc ((line 2) (column 42))))
      (Thunk_body (path ((Key IntT) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Key IntT) (Id "\206\187") (Id f))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 2) (column 42)))))))
           (bind ()) (loc ((line 2) (column 42)))))))
       (return
        (Make_closure (body ((Id "\206\187") (Key IntT) (Id "\206\187") (Id f)))
         (env (((Id env) (Key IntT) (Id "\206\187") (Id f))))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 2) (column 42)))))
       (loc ((line 2) (column 4))))
      (Closure_body (path ((Id "\206\187") (Id g))) (arg ((Id x))) (arg_ty Int)
       (captures
        (((path ((Key IntT) (Id f)))
          (ty (Thunk (Closure (arg_ty Int) (ret_ty Int)))) (offset_in_bytes 0))))
       (bind
        ((Values
          ((exprs
            ((((Id $))
              (Apply_thunk (fn ((Key IntT) (Id f)))
               (ty (Closure (arg_ty Int) (ret_ty Int)))
               (loc ((line 3) (column 24)))))))
           (bind ()) (loc ((line 3) (column 24)))))))
       (return
        (Apply_closure (fn ((Id $))) (arg ((Id x))) (arg_ty Int) (ty Int)
         (loc ((line 3) (column 24)))))
       (loc ((line 3) (column 4))))
      (Functions
       ((paths
         ((((Key IntT) (Id f)) (Thunk (Closure (arg_ty Int) (ret_ty Int)))
           ((Key IntT) (Id "\206\187") (Id f)))
          (((Id g)) (Closure (arg_ty Int) (ret_ty Int)) ((Id "\206\187") (Id g)))))
        (captures
         ((entries
           (((path ((Key IntT) (Id f)))
             (ty (Thunk (Closure (arg_ty Int) (ret_ty Int))))
             (offset_in_bytes 0))))
          (size_in_bytes 16)))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id g))) (arg ((Id $))) (arg_ty Int) (ty Int)
            (loc ((line 4) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value (Int 5)) (ty Int) (loc ((line 4) (column 10)))))))
            (bind ()) (loc ((line 4) (column 10)))))))
        (loc ((line 4) (column 0)))))))
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
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id inc))) (arg ((Id x))) (arg_ty Int)
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id $))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 30)))))))
           (bind ()) (loc ((line 2) (column 30)))))))
       (return
        (Binop (op Add) (lhs ((Id x))) (rhs ((Id $))) (ty Int)
         (loc ((line 2) (column 28)))))
       (loc ((line 2) (column 4))))
      (Closure_body
       (path ((Id "\206\187") (Key (Bool false)) (Id "\206\187") (Id choose)))
       (arg ((Id x))) (arg_ty Int) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 4) (column 62)))))
       (loc ((line 4) (column 46))))
      (Thunk_body (path ((Key (Bool false)) (Id "\206\187") (Id choose)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Key (Bool false)) (Id "\206\187") (Id choose))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 4) (column 46)))))))
           (bind ()) (loc ((line 4) (column 46)))))))
       (return
        (Make_closure
         (body ((Id "\206\187") (Key (Bool false)) (Id "\206\187") (Id choose)))
         (env (((Id env) (Key (Bool false)) (Id "\206\187") (Id choose))))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 4) (column 46)))))
       (loc ((line 3) (column 4))))
      (Closure_body
       (path ((Id "\206\187") (Key (Bool true)) (Id "\206\187") (Id choose)))
       (arg ((Id x))) (arg_ty Int)
       (captures
        (((path ((Id inc))) (ty (Closure (arg_ty Int) (ret_ty Int)))
          (offset_in_bytes 0))))
       (bind ())
       (return
        (Apply_closure (fn ((Id inc))) (arg ((Id x))) (arg_ty Int) (ty Int)
         (loc ((line 4) (column 35)))))
       (loc ((line 4) (column 19))))
      (Thunk_body (path ((Key (Bool true)) (Id "\206\187") (Id choose)))
       (captures
        (((path ((Id inc))) (ty (Closure (arg_ty Int) (ret_ty Int)))
          (offset_in_bytes 0))))
       (bind
        ((Values
          ((exprs
            ((((Id env) (Key (Bool true)) (Id "\206\187") (Id choose))
              (Make_env
               (captures
                ((entries
                  (((path ((Id inc))) (ty (Closure (arg_ty Int) (ret_ty Int)))
                    (offset_in_bytes 0))))
                 (size_in_bytes 16)))
               (ty Env) (loc ((line 4) (column 19)))))))
           (bind ()) (loc ((line 4) (column 19)))))))
       (return
        (Make_closure
         (body ((Id "\206\187") (Key (Bool true)) (Id "\206\187") (Id choose)))
         (env (((Id env) (Key (Bool true)) (Id "\206\187") (Id choose))))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 4) (column 19)))))
       (loc ((line 3) (column 4))))
      (Functions
       ((paths
         ((((Id inc)) (Closure (arg_ty Int) (ret_ty Int))
           ((Id "\206\187") (Id inc)))
          (((Key (Bool false)) (Id choose))
           (Thunk (Closure (arg_ty Int) (ret_ty Int)))
           ((Key (Bool false)) (Id "\206\187") (Id choose)))
          (((Key (Bool true)) (Id choose))
           (Thunk (Closure (arg_ty Int) (ret_ty Int)))
           ((Key (Bool true)) (Id "\206\187") (Id choose)))))
        (captures
         ((entries
           (((path ((Id inc))) (ty (Closure (arg_ty Int) (ret_ty Int)))
             (offset_in_bytes 0))))
          (size_in_bytes 16)))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id $))) (arg ((Shadow 1) (Id $))) (arg_ty Int)
            (ty Int) (loc ((line 5) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Apply_thunk (fn ((Key (Bool true)) (Id choose)))
                (ty (Closure (arg_ty Int) (ret_ty Int)))
                (loc ((line 5) (column 8)))))))
            (bind ()) (loc ((line 5) (column 8)))))
          (Values
           ((exprs
             ((((Shadow 1) (Id $))
               (Scalar (value (Int 5)) (ty Int) (loc ((line 5) (column 20)))))))
            (bind ()) (loc ((line 5) (column 20)))))))
        (loc ((line 5) (column 0)))))
      (Values
       ((exprs
         ((((Shadow 1) (Id _))
           (Apply_closure (fn ((Id $))) (arg ((Shadow 1) (Id $))) (arg_ty Int)
            (ty Int) (loc ((line 6) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Apply_thunk (fn ((Key (Bool false)) (Id choose)))
                (ty (Closure (arg_ty Int) (ret_ty Int)))
                (loc ((line 6) (column 8)))))))
            (bind ()) (loc ((line 6) (column 8)))))
          (Values
           ((exprs
             ((((Shadow 1) (Id $))
               (Scalar (value (Int 5)) (ty Int) (loc ((line 6) (column 21)))))))
            (bind ()) (loc ((line 6) (column 21)))))))
        (loc ((line 6) (column 0)))))))
    |}]
;;

let%expect_test "mutual pi recursion" =
  go
    {|
fun f (static erased t : type) : t -> t = fn (x : t) -> x
and g (static erased t : type) : t -> t = f t;;
let _ = g int 0;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Key IntT) (Id "\206\187") (Id f)))
       (arg ((Id x))) (arg_ty Int) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 2) (column 56)))))
       (loc ((line 2) (column 42))))
      (Thunk_body (path ((Key IntT) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Key IntT) (Id "\206\187") (Id f))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 2) (column 42)))))))
           (bind ()) (loc ((line 2) (column 42)))))))
       (return
        (Make_closure (body ((Id "\206\187") (Key IntT) (Id "\206\187") (Id f)))
         (env (((Id env) (Key IntT) (Id "\206\187") (Id f))))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 2) (column 42)))))
       (loc ((line 2) (column 4))))
      (Thunk_body (path ((Key IntT) (Id "\206\187") (Id g)))
       (captures
        (((path ((Key IntT) (Id f)))
          (ty (Thunk (Closure (arg_ty Int) (ret_ty Int)))) (offset_in_bytes 0))))
       (bind ())
       (return
        (Apply_thunk (fn ((Key IntT) (Id f)))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 3) (column 42)))))
       (loc ((line 3) (column 4))))
      (Functions
       ((paths
         ((((Key IntT) (Id f)) (Thunk (Closure (arg_ty Int) (ret_ty Int)))
           ((Key IntT) (Id "\206\187") (Id f)))
          (((Key IntT) (Id g)) (Thunk (Closure (arg_ty Int) (ret_ty Int)))
           ((Key IntT) (Id "\206\187") (Id g)))))
        (captures
         ((entries
           (((path ((Key IntT) (Id f)))
             (ty (Thunk (Closure (arg_ty Int) (ret_ty Int))))
             (offset_in_bytes 0))))
          (size_in_bytes 16)))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id $))) (arg ((Shadow 1) (Id $))) (arg_ty Int)
            (ty Int) (loc ((line 4) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Apply_thunk (fn ((Key IntT) (Id g)))
                (ty (Closure (arg_ty Int) (ret_ty Int)))
                (loc ((line 4) (column 8)))))))
            (bind ()) (loc ((line 4) (column 8)))))
          (Values
           ((exprs
             ((((Shadow 1) (Id $))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 14)))))))
            (bind ()) (loc ((line 4) (column 14)))))))
        (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "static recursion with base case" =
  go
    {|
fun f (static x : int) : int = if static x == 0 then x else f (x - 1);;
let _ = f 3;;
|};
  [%expect {|
    (lst
     ((Thunk_body (path ((Key (Int 1)) (Id "\206\187") (Id f)))
       (captures
        (((path ((Key (Int 0)) (Id f))) (ty (Thunk Int)) (offset_in_bytes 0))))
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 1)) (Id "\206\187") (Id f))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 4)))))))
           (bind ()) (loc ((line 2) (column 4)))))))
       (return
        (Apply_thunk (fn ((Key (Int 0)) (Id f))) (ty Int)
         (loc ((line 2) (column 60)))))
       (loc ((line 2) (column 4))))
      (Thunk_body (path ((Key (Int 2)) (Id "\206\187") (Id f)))
       (captures
        (((path ((Key (Int 1)) (Id f))) (ty (Thunk Int)) (offset_in_bytes 32))))
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 2)) (Id "\206\187") (Id f))
              (Scalar (value (Int 2)) (ty Int) (loc ((line 2) (column 4)))))))
           (bind ()) (loc ((line 2) (column 4)))))))
       (return
        (Apply_thunk (fn ((Key (Int 1)) (Id f))) (ty Int)
         (loc ((line 2) (column 60)))))
       (loc ((line 2) (column 4))))
      (Thunk_body (path ((Key (Int 3)) (Id "\206\187") (Id f)))
       (captures
        (((path ((Key (Int 2)) (Id f))) (ty (Thunk Int)) (offset_in_bytes 16))))
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 3)) (Id "\206\187") (Id f))
              (Scalar (value (Int 3)) (ty Int) (loc ((line 2) (column 4)))))))
           (bind ()) (loc ((line 2) (column 4)))))))
       (return
        (Apply_thunk (fn ((Key (Int 2)) (Id f))) (ty Int)
         (loc ((line 2) (column 60)))))
       (loc ((line 2) (column 4))))
      (Thunk_body (path ((Key (Int 0)) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 0)) (Id "\206\187") (Id f))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 4)))))))
           (bind ()) (loc ((line 2) (column 4)))))))
       (return
        (Ident (path ((Id x) (Key (Int 0)) (Id "\206\187") (Id f))) (ty Int)
         (loc ((line 2) (column 53)))))
       (loc ((line 2) (column 4))))
      (Functions
       ((paths
         ((((Key (Int 1)) (Id f)) (Thunk Int)
           ((Key (Int 1)) (Id "\206\187") (Id f)))
          (((Key (Int 2)) (Id f)) (Thunk Int)
           ((Key (Int 2)) (Id "\206\187") (Id f)))
          (((Key (Int 3)) (Id f)) (Thunk Int)
           ((Key (Int 3)) (Id "\206\187") (Id f)))
          (((Key (Int 0)) (Id f)) (Thunk Int)
           ((Key (Int 0)) (Id "\206\187") (Id f)))))
        (captures
         ((entries
           (((path ((Key (Int 0)) (Id f))) (ty (Thunk Int)) (offset_in_bytes 0))
            ((path ((Key (Int 2)) (Id f))) (ty (Thunk Int)) (offset_in_bytes 16))
            ((path ((Key (Int 1)) (Id f))) (ty (Thunk Int)) (offset_in_bytes 32))))
          (size_in_bytes 48)))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Int 3)) (Id f))) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (static x : int) : erased int = (if static x == 0 then 42 else f (x - 1)) @ erased;;
let _ = f 3;;
|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "arrow and pi mutual recursion with application" =
  go
    {|
fun double (x : int) : int = x + x
and apply_double (static erased t : type) : int -> int = fn (x : int) -> double x;;
let _ = apply_double int 5;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id double))) (arg ((Id x)))
       (arg_ty Int) (captures ()) (bind ())
       (return
        (Binop (op Add) (lhs ((Id x))) (rhs ((Id x))) (ty Int)
         (loc ((line 2) (column 31)))))
       (loc ((line 2) (column 4))))
      (Closure_body
       (path ((Id "\206\187") (Key IntT) (Id "\206\187") (Id apply_double)))
       (arg ((Id x))) (arg_ty Int)
       (captures
        (((path ((Id double))) (ty (Closure (arg_ty Int) (ret_ty Int)))
          (offset_in_bytes 0))))
       (bind ())
       (return
        (Apply_closure (fn ((Id double))) (arg ((Id x))) (arg_ty Int) (ty Int)
         (loc ((line 3) (column 73)))))
       (loc ((line 3) (column 57))))
      (Thunk_body (path ((Key IntT) (Id "\206\187") (Id apply_double)))
       (captures
        (((path ((Id double))) (ty (Closure (arg_ty Int) (ret_ty Int)))
          (offset_in_bytes 0))))
       (bind
        ((Values
          ((exprs
            ((((Id env) (Key IntT) (Id "\206\187") (Id apply_double))
              (Make_env
               (captures
                ((entries
                  (((path ((Id double))) (ty (Closure (arg_ty Int) (ret_ty Int)))
                    (offset_in_bytes 0))))
                 (size_in_bytes 16)))
               (ty Env) (loc ((line 3) (column 57)))))))
           (bind ()) (loc ((line 3) (column 57)))))))
       (return
        (Make_closure
         (body ((Id "\206\187") (Key IntT) (Id "\206\187") (Id apply_double)))
         (env (((Id env) (Key IntT) (Id "\206\187") (Id apply_double))))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 3) (column 57)))))
       (loc ((line 3) (column 4))))
      (Functions
       ((paths
         ((((Id double)) (Closure (arg_ty Int) (ret_ty Int))
           ((Id "\206\187") (Id double)))
          (((Key IntT) (Id apply_double))
           (Thunk (Closure (arg_ty Int) (ret_ty Int)))
           ((Key IntT) (Id "\206\187") (Id apply_double)))))
        (captures
         ((entries
           (((path ((Id double))) (ty (Closure (arg_ty Int) (ret_ty Int)))
             (offset_in_bytes 0))))
          (size_in_bytes 16)))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id $))) (arg ((Shadow 1) (Id $))) (arg_ty Int)
            (ty Int) (loc ((line 4) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Apply_thunk (fn ((Key IntT) (Id apply_double)))
                (ty (Closure (arg_ty Int) (ret_ty Int)))
                (loc ((line 4) (column 8)))))))
            (bind ()) (loc ((line 4) (column 8)))))
          (Values
           ((exprs
             ((((Shadow 1) (Id $))
               (Scalar (value (Int 5)) (ty Int) (loc ((line 4) (column 25)))))))
            (bind ()) (loc ((line 4) (column 25)))))))
        (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "mutually recursive fun with static arg" =
  go
    {|
fun id1 (static erased t : type) : t -> t = fn (x : t) -> x
and id2 (static erased t : type) : t -> t = id1 t;;
|};
  [%expect {|
    (lst
     ((Functions
       ((paths ()) (captures ((entries ()) (size_in_bytes 0)))
        (loc ((line 2) (column 0)))))))
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = g x
and erased g (y : int) : int = y;;
let _ = f 0;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id f))) (arg ((Id x))) (arg_ty Int)
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id y) (Id "\206\187") (Id f))
              (Ident (path ((Id x))) (ty Int) (loc ((line 2) (column 26)))))))
           (bind ()) (loc ((line 2) (column 24)))))))
       (return
        (Ident (path ((Id y) (Id "\206\187") (Id f))) (ty Int)
         (loc ((line 3) (column 31)))))
       (loc ((line 2) (column 4))))
      (Functions
       ((paths
         ((((Id f)) (Closure (arg_ty Int) (ret_ty Int)) ((Id "\206\187") (Id f)))))
        (captures ((entries ()) (size_in_bytes 0))) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id f))) (arg ((Id $))) (arg_ty Int) (ty Int)
            (loc ((line 4) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 10)))))))
            (bind ()) (loc ((line 4) (column 10)))))))
        (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = (g @ erased) x
and g (y : int) : int = y;;
let _ = f 0;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id f))) (arg ((Id x))) (arg_ty Int)
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id y) (Id "\206\187") (Id f))
              (Ident (path ((Id x))) (ty Int) (loc ((line 2) (column 37)))))))
           (bind ()) (loc ((line 2) (column 24)))))))
       (return
        (Ident (path ((Id y) (Id "\206\187") (Id f))) (ty Int)
         (loc ((line 3) (column 24)))))
       (loc ((line 2) (column 4))))
      (Closure_body (path ((Id "\206\187") (Id g))) (arg ((Id y))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Ident (path ((Id y))) (ty Int) (loc ((line 3) (column 24)))))
       (loc ((line 3) (column 4))))
      (Functions
       ((paths
         ((((Id f)) (Closure (arg_ty Int) (ret_ty Int)) ((Id "\206\187") (Id f)))
          (((Id g)) (Closure (arg_ty Int) (ret_ty Int)) ((Id "\206\187") (Id g)))))
        (captures ((entries ()) (size_in_bytes 0))) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id f))) (arg ((Id $))) (arg_ty Int) (ty Int)
            (loc ((line 4) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 10)))))))
            (bind ()) (loc ((line 4) (column 10)))))))
        (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : erased int = g x
and g (y : int) : erased int = y;;
let _ = f 0;;
|};
  [%expect {| (lst ()) |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun erased f (x : int) : int = (g @ erased) x
and g (y : int) : int = y;;
let _ = f 0;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id g))) (arg ((Id y))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Ident (path ((Id y))) (ty Int) (loc ((line 3) (column 24)))))
       (loc ((line 3) (column 4))))
      (Functions
       ((paths
         ((((Id g)) (Closure (arg_ty Int) (ret_ty Int)) ((Id "\206\187") (Id g)))))
        (captures ((entries ()) (size_in_bytes 0))) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Ident (path ((Id y) (Id _))) (ty Int) (loc ((line 3) (column 24)))))))
        (bind
         ((Values
           ((exprs
             ((((Id x) (Id _))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 10)))))))
            (bind ()) (loc ((line 4) (column 8)))))
          (Values
           ((exprs
             ((((Id y) (Id _))
               (Ident (path ((Id x) (Id _))) (ty Int)
                (loc ((line 2) (column 44)))))))
            (bind ()) (loc ((line 2) (column 31)))))))
        (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = g x
and g (y : int) : int = f y;;
let _ = f 0;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id f))) (arg ((Id x))) (arg_ty Int)
       (captures
        (((path ((Id g))) (ty (Closure (arg_ty Int) (ret_ty Int)))
          (offset_in_bytes 16))))
       (bind ())
       (return
        (Apply_closure (fn ((Id g))) (arg ((Id x))) (arg_ty Int) (ty Int)
         (loc ((line 2) (column 24)))))
       (loc ((line 2) (column 4))))
      (Closure_body (path ((Id "\206\187") (Id g))) (arg ((Id y))) (arg_ty Int)
       (captures
        (((path ((Id f))) (ty (Closure (arg_ty Int) (ret_ty Int)))
          (offset_in_bytes 0))))
       (bind ())
       (return
        (Apply_closure (fn ((Id f))) (arg ((Id y))) (arg_ty Int) (ty Int)
         (loc ((line 3) (column 24)))))
       (loc ((line 3) (column 4))))
      (Functions
       ((paths
         ((((Id f)) (Closure (arg_ty Int) (ret_ty Int)) ((Id "\206\187") (Id f)))
          (((Id g)) (Closure (arg_ty Int) (ret_ty Int)) ((Id "\206\187") (Id g)))))
        (captures
         ((entries
           (((path ((Id f))) (ty (Closure (arg_ty Int) (ret_ty Int)))
             (offset_in_bytes 0))
            ((path ((Id g))) (ty (Closure (arg_ty Int) (ret_ty Int)))
             (offset_in_bytes 16))))
          (size_in_bytes 32)))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id f))) (arg ((Id $))) (arg_ty Int) (ty Int)
            (loc ((line 4) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 10)))))))
            (bind ()) (loc ((line 4) (column 10)))))))
        (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "fun env" =
  go
    {|
let a = 0 @ dynamic;;
fun f (x : int) : int = let _ = a in x;;
|};
  [%expect {|
    (lst
     ((Values
       ((exprs
         ((((Id a))
           (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 8)))))))
        (bind ()) (loc ((line 2) (column 0)))))
      (Closure_body (path ((Id "\206\187") (Id f))) (arg ((Id x))) (arg_ty Int)
       (captures (((path ((Id a))) (ty Int) (offset_in_bytes 0))))
       (bind
        ((Values
          ((exprs
            ((((Id _) (Id "\206\187") (Id f))
              (Ident (path ((Id a))) (ty Int) (loc ((line 3) (column 32)))))))
           (bind ()) (loc ((line 3) (column 24)))))))
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 3) (column 37)))))
       (loc ((line 3) (column 4))))
      (Functions
       ((paths
         ((((Id f)) (Closure (arg_ty Int) (ret_ty Int)) ((Id "\206\187") (Id f)))))
        (captures
         ((entries (((path ((Id a))) (ty Int) (offset_in_bytes 0))))
          (size_in_bytes 8)))
        (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "recursive env" =
  go
    {|
let a = 0 @ dynamic;;
let b = 1 @ dynamic;;
let c = 2 @ dynamic;;
fun f (x : int) : int = let _ = a in let _ = b in g x
and g (y : int) : int = let _ = a in let _ = c in f y;;
|};
  [%expect {|
    (lst
     ((Values
       ((exprs
         ((((Id a))
           (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 8)))))))
        (bind ()) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id b))
           (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id c))
           (Scalar (value (Int 2)) (ty Int) (loc ((line 4) (column 8)))))))
        (bind ()) (loc ((line 4) (column 0)))))
      (Closure_body (path ((Id "\206\187") (Id f))) (arg ((Id x))) (arg_ty Int)
       (captures
        (((path ((Id g))) (ty (Closure (arg_ty Int) (ret_ty Int)))
          (offset_in_bytes 40))
         ((path ((Id b))) (ty Int) (offset_in_bytes 8))
         ((path ((Id a))) (ty Int) (offset_in_bytes 0))))
       (bind
        ((Values
          ((exprs
            ((((Id _) (Id "\206\187") (Id f))
              (Ident (path ((Id a))) (ty Int) (loc ((line 5) (column 32)))))))
           (bind ()) (loc ((line 5) (column 24)))))
         (Values
          ((exprs
            ((((Shadow 1) (Id _) (Id "\206\187") (Id f))
              (Ident (path ((Id b))) (ty Int) (loc ((line 5) (column 45)))))))
           (bind ()) (loc ((line 5) (column 37)))))))
       (return
        (Apply_closure (fn ((Id g))) (arg ((Id x))) (arg_ty Int) (ty Int)
         (loc ((line 5) (column 50)))))
       (loc ((line 5) (column 4))))
      (Closure_body (path ((Id "\206\187") (Id g))) (arg ((Id y))) (arg_ty Int)
       (captures
        (((path ((Id f))) (ty (Closure (arg_ty Int) (ret_ty Int)))
          (offset_in_bytes 24))
         ((path ((Id c))) (ty Int) (offset_in_bytes 16))
         ((path ((Id a))) (ty Int) (offset_in_bytes 0))))
       (bind
        ((Values
          ((exprs
            ((((Id _) (Id "\206\187") (Id g))
              (Ident (path ((Id a))) (ty Int) (loc ((line 6) (column 32)))))))
           (bind ()) (loc ((line 6) (column 24)))))
         (Values
          ((exprs
            ((((Shadow 1) (Id _) (Id "\206\187") (Id g))
              (Ident (path ((Id c))) (ty Int) (loc ((line 6) (column 45)))))))
           (bind ()) (loc ((line 6) (column 37)))))))
       (return
        (Apply_closure (fn ((Id f))) (arg ((Id y))) (arg_ty Int) (ty Int)
         (loc ((line 6) (column 50)))))
       (loc ((line 6) (column 4))))
      (Functions
       ((paths
         ((((Id f)) (Closure (arg_ty Int) (ret_ty Int)) ((Id "\206\187") (Id f)))
          (((Id g)) (Closure (arg_ty Int) (ret_ty Int)) ((Id "\206\187") (Id g)))))
        (captures
         ((entries
           (((path ((Id a))) (ty Int) (offset_in_bytes 0))
            ((path ((Id b))) (ty Int) (offset_in_bytes 8))
            ((path ((Id c))) (ty Int) (offset_in_bytes 16))
            ((path ((Id f))) (ty (Closure (arg_ty Int) (ret_ty Int)))
             (offset_in_bytes 24))
            ((path ((Id g))) (ty (Closure (arg_ty Int) (ret_ty Int)))
             (offset_in_bytes 40))))
          (size_in_bytes 56)))
        (loc ((line 5) (column 0)))))))
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = g x
and erased g (y : int) : int = f y;;
let _ = (f @ erased) 0;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id f))) (arg ((Id x))) (arg_ty Int)
       (captures
        (((path ((Id f))) (ty (Closure (arg_ty Int) (ret_ty Int)))
          (offset_in_bytes 0))))
       (bind
        ((Values
          ((exprs
            ((((Id y) (Id "\206\187") (Id f))
              (Ident (path ((Id x))) (ty Int) (loc ((line 2) (column 26)))))))
           (bind ()) (loc ((line 2) (column 24)))))))
       (return
        (Apply_closure (fn ((Id f))) (arg ((Id y) (Id "\206\187") (Id f)))
         (arg_ty Int) (ty Int) (loc ((line 3) (column 31)))))
       (loc ((line 2) (column 4))))
      (Functions
       ((paths
         ((((Id f)) (Closure (arg_ty Int) (ret_ty Int)) ((Id "\206\187") (Id f)))))
        (captures
         ((entries
           (((path ((Id f))) (ty (Closure (arg_ty Int) (ret_ty Int)))
             (offset_in_bytes 0))))
          (size_in_bytes 16)))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id f))) (arg ((Id y) (Id _))) (arg_ty Int)
            (ty Int) (loc ((line 3) (column 31)))))))
        (bind
         ((Values
           ((exprs
             ((((Id x) (Id _))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 21)))))))
            (bind ()) (loc ((line 4) (column 8)))))
          (Values
           ((exprs
             ((((Id y) (Id _))
               (Ident (path ((Id x) (Id _))) (ty Int)
                (loc ((line 2) (column 26)))))))
            (bind ()) (loc ((line 2) (column 24)))))))
        (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = g x
and g (y : int) : int = (f @ erased) y;;
let _ = f 0;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id f))) (arg ((Id x))) (arg_ty Int)
       (captures
        (((path ((Id g))) (ty (Closure (arg_ty Int) (ret_ty Int)))
          (offset_in_bytes 0))))
       (bind ())
       (return
        (Apply_closure (fn ((Id g))) (arg ((Id x))) (arg_ty Int) (ty Int)
         (loc ((line 2) (column 24)))))
       (loc ((line 2) (column 4))))
      (Closure_body (path ((Id "\206\187") (Id g))) (arg ((Id y))) (arg_ty Int)
       (captures
        (((path ((Id g))) (ty (Closure (arg_ty Int) (ret_ty Int)))
          (offset_in_bytes 0))))
       (bind
        ((Values
          ((exprs
            ((((Id x) (Id "\206\187") (Id g))
              (Ident (path ((Id y))) (ty Int) (loc ((line 3) (column 37)))))))
           (bind ()) (loc ((line 3) (column 24)))))))
       (return
        (Apply_closure (fn ((Id g))) (arg ((Id x) (Id "\206\187") (Id g)))
         (arg_ty Int) (ty Int) (loc ((line 2) (column 24)))))
       (loc ((line 3) (column 4))))
      (Functions
       ((paths
         ((((Id f)) (Closure (arg_ty Int) (ret_ty Int)) ((Id "\206\187") (Id f)))
          (((Id g)) (Closure (arg_ty Int) (ret_ty Int)) ((Id "\206\187") (Id g)))))
        (captures
         ((entries
           (((path ((Id g))) (ty (Closure (arg_ty Int) (ret_ty Int)))
             (offset_in_bytes 0))))
          (size_in_bytes 16)))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id f))) (arg ((Id $))) (arg_ty Int) (ty Int)
            (loc ((line 4) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 10)))))))
            (bind ()) (loc ((line 4) (column 10)))))))
        (loc ((line 4) (column 0)))))))
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
    (lst
     ((Closure_body (path ((Id "\206\187") (Id g))) (arg ((Id y))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 39)))))
       (loc ((line 3) (column 4))))
      (Functions
       ((paths
         ((((Id g)) (Closure (arg_ty Int) (ret_ty Int)) ((Id "\206\187") (Id g)))))
        (captures ((entries ()) (size_in_bytes 0))) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id g))) (arg ((Id $))) (arg_ty Int) (ty Int)
            (loc ((line 4) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 10)))))))
            (bind ()) (loc ((line 4) (column 10)))))))
        (loc ((line 4) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 31)))))))
        (bind
         ((Values
           ((exprs
             ((((Id x) (Id _))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 10)))))))
            (bind ()) (loc ((line 3) (column 8)))))))
        (loc ((line 3) (column 0)))))))
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
    (lst
     ((Values
       ((exprs
         ((((Id _))
           (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 31)))))))
        (bind
         ((Values
           ((exprs
             ((((Id x) (Id _))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 10)))))))
            (bind ()) (loc ((line 3) (column 8)))))))
        (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "static int" =
  go
    {|
let f = fn (static x : int) -> x;;
let _ = f (f 1);;
|};
  [%expect {|
    (lst
     ((Thunk_body (path ((Key (Int 1)) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 1)) (Id "\206\187") (Id f))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 8)))))))
           (bind ()) (loc ((line 2) (column 8)))))))
       (return
        (Ident (path ((Id x) (Key (Int 1)) (Id "\206\187") (Id f))) (ty Int)
         (loc ((line 2) (column 31)))))
       (loc ((line 2) (column 8))))
      (Values
       ((exprs
         ((((Key (Int 1)) (Id f))
           (Make_closure (body ((Key (Int 1)) (Id "\206\187") (Id f)))
            (env (((Id env) (Id f)))) (ty (Thunk Int))
            (loc ((line 2) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id f))
               (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                (loc ((line 2) (column 8)))))))
            (bind ()) (loc ((line 2) (column 8)))))))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Int 1)) (Id f))) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
    |}]
;;

let%expect_test "top-level mutual recursion calling each other" =
  go
    {|
fun f (a : int) : int = g a
and g (b : int) : int = f b;;
let _ = f 0;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id f))) (arg ((Id a))) (arg_ty Int)
       (captures
        (((path ((Id g))) (ty (Closure (arg_ty Int) (ret_ty Int)))
          (offset_in_bytes 16))))
       (bind ())
       (return
        (Apply_closure (fn ((Id g))) (arg ((Id a))) (arg_ty Int) (ty Int)
         (loc ((line 2) (column 24)))))
       (loc ((line 2) (column 4))))
      (Closure_body (path ((Id "\206\187") (Id g))) (arg ((Id b))) (arg_ty Int)
       (captures
        (((path ((Id f))) (ty (Closure (arg_ty Int) (ret_ty Int)))
          (offset_in_bytes 0))))
       (bind ())
       (return
        (Apply_closure (fn ((Id f))) (arg ((Id b))) (arg_ty Int) (ty Int)
         (loc ((line 3) (column 24)))))
       (loc ((line 3) (column 4))))
      (Functions
       ((paths
         ((((Id f)) (Closure (arg_ty Int) (ret_ty Int)) ((Id "\206\187") (Id f)))
          (((Id g)) (Closure (arg_ty Int) (ret_ty Int)) ((Id "\206\187") (Id g)))))
        (captures
         ((entries
           (((path ((Id f))) (ty (Closure (arg_ty Int) (ret_ty Int)))
             (offset_in_bytes 0))
            ((path ((Id g))) (ty (Closure (arg_ty Int) (ret_ty Int)))
             (offset_in_bytes 16))))
          (size_in_bytes 32)))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id f))) (arg ((Id $))) (arg_ty Int) (ty Int)
            (loc ((line 4) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 10)))))))
            (bind ()) (loc ((line 4) (column 10)))))))
        (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "top-level mutual recursion with different bodies" =
  go
    {|
fun f (a : int) : int = g (a + 1)
and g (b : int) : int = b;;
let _ = f 0;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id f))) (arg ((Id a))) (arg_ty Int)
       (captures
        (((path ((Id g))) (ty (Closure (arg_ty Int) (ret_ty Int)))
          (offset_in_bytes 0))))
       (bind
        ((Values
          ((exprs
            ((((Id $))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 2) (column 31)))))))
           (bind ()) (loc ((line 2) (column 31)))))
         (Values
          ((exprs
            ((((Shadow 1) (Id $))
              (Binop (op Add) (lhs ((Id a))) (rhs ((Id $))) (ty Int)
               (loc ((line 2) (column 29)))))))
           (bind ()) (loc ((line 2) (column 29)))))))
       (return
        (Apply_closure (fn ((Id g))) (arg ((Shadow 1) (Id $))) (arg_ty Int)
         (ty Int) (loc ((line 2) (column 24)))))
       (loc ((line 2) (column 4))))
      (Closure_body (path ((Id "\206\187") (Id g))) (arg ((Id b))) (arg_ty Int)
       (captures ()) (bind ())
       (return (Ident (path ((Id b))) (ty Int) (loc ((line 3) (column 24)))))
       (loc ((line 3) (column 4))))
      (Functions
       ((paths
         ((((Id f)) (Closure (arg_ty Int) (ret_ty Int)) ((Id "\206\187") (Id f)))
          (((Id g)) (Closure (arg_ty Int) (ret_ty Int)) ((Id "\206\187") (Id g)))))
        (captures
         ((entries
           (((path ((Id g))) (ty (Closure (arg_ty Int) (ret_ty Int)))
             (offset_in_bytes 0))))
          (size_in_bytes 16)))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id f))) (arg ((Id $))) (arg_ty Int) (ty Int)
            (loc ((line 4) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 10)))))))
            (bind ()) (loc ((line 4) (column 10)))))))
        (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "top-level mutual static recursion" =
  go
    {|
fun f (static x : int) : int = if static x == 0 then 0 else g (x - 1)
and g (static y : int) : int = if static y == 0 then 1 else f (y - 1);;
let _ = f 2;;
|};
  [%expect {|
    (lst
     ((Thunk_body (path ((Key (Int 2)) (Id "\206\187") (Id f)))
       (captures
        (((path ((Key (Int 1)) (Id g))) (ty (Thunk Int)) (offset_in_bytes 16))))
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 2)) (Id "\206\187") (Id f))
              (Scalar (value (Int 2)) (ty Int) (loc ((line 2) (column 4)))))))
           (bind ()) (loc ((line 2) (column 4)))))))
       (return
        (Apply_thunk (fn ((Key (Int 1)) (Id g))) (ty Int)
         (loc ((line 2) (column 60)))))
       (loc ((line 2) (column 4))))
      (Thunk_body (path ((Key (Int 0)) (Id "\206\187") (Id f))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 0)) (Id "\206\187") (Id f))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 4)))))))
           (bind ()) (loc ((line 2) (column 4)))))))
       (return (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 53)))))
       (loc ((line 2) (column 4))))
      (Thunk_body (path ((Key (Int 1)) (Id "\206\187") (Id g)))
       (captures
        (((path ((Key (Int 0)) (Id f))) (ty (Thunk Int)) (offset_in_bytes 0))))
       (bind
        ((Values
          ((exprs
            ((((Id y) (Key (Int 1)) (Id "\206\187") (Id g))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 4)))))))
           (bind ()) (loc ((line 3) (column 4)))))))
       (return
        (Apply_thunk (fn ((Key (Int 0)) (Id f))) (ty Int)
         (loc ((line 3) (column 60)))))
       (loc ((line 3) (column 4))))
      (Functions
       ((paths
         ((((Key (Int 2)) (Id f)) (Thunk Int)
           ((Key (Int 2)) (Id "\206\187") (Id f)))
          (((Key (Int 0)) (Id f)) (Thunk Int)
           ((Key (Int 0)) (Id "\206\187") (Id f)))
          (((Key (Int 1)) (Id g)) (Thunk Int)
           ((Key (Int 1)) (Id "\206\187") (Id g)))))
        (captures
         ((entries
           (((path ((Key (Int 0)) (Id f))) (ty (Thunk Int)) (offset_in_bytes 0))
            ((path ((Key (Int 1)) (Id g))) (ty (Thunk Int)) (offset_in_bytes 16))))
          (size_in_bytes 32)))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Int 2)) (Id f))) (ty Int)
            (loc ((line 4) (column 8)))))))
        (bind ()) (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "static mutual recursion cross-monomorphization" =
  go
    {|
fun f (static erased t : type) : t -> t = g t
and g (static erased t : type) : t -> t = fn (x : t) -> x;;
let _ = f int 0;;
|};
  [%expect {|
    (lst
     ((Thunk_body (path ((Key IntT) (Id "\206\187") (Id f)))
       (captures
        (((path ((Key IntT) (Id g)))
          (ty (Thunk (Closure (arg_ty Int) (ret_ty Int)))) (offset_in_bytes 0))))
       (bind ())
       (return
        (Apply_thunk (fn ((Key IntT) (Id g)))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 2) (column 42)))))
       (loc ((line 2) (column 4))))
      (Closure_body (path ((Id "\206\187") (Key IntT) (Id "\206\187") (Id g)))
       (arg ((Id x))) (arg_ty Int) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Int) (loc ((line 3) (column 56)))))
       (loc ((line 3) (column 42))))
      (Thunk_body (path ((Key IntT) (Id "\206\187") (Id g))) (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Key IntT) (Id "\206\187") (Id g))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 3) (column 42)))))))
           (bind ()) (loc ((line 3) (column 42)))))))
       (return
        (Make_closure (body ((Id "\206\187") (Key IntT) (Id "\206\187") (Id g)))
         (env (((Id env) (Key IntT) (Id "\206\187") (Id g))))
         (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 3) (column 42)))))
       (loc ((line 3) (column 4))))
      (Functions
       ((paths
         ((((Key IntT) (Id f)) (Thunk (Closure (arg_ty Int) (ret_ty Int)))
           ((Key IntT) (Id "\206\187") (Id f)))
          (((Key IntT) (Id g)) (Thunk (Closure (arg_ty Int) (ret_ty Int)))
           ((Key IntT) (Id "\206\187") (Id g)))))
        (captures
         ((entries
           (((path ((Key IntT) (Id g)))
             (ty (Thunk (Closure (arg_ty Int) (ret_ty Int))))
             (offset_in_bytes 0))))
          (size_in_bytes 16)))
        (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id $))) (arg ((Shadow 1) (Id $))) (arg_ty Int)
            (ty Int) (loc ((line 4) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Apply_thunk (fn ((Key IntT) (Id f)))
                (ty (Closure (arg_ty Int) (ret_ty Int)))
                (loc ((line 4) (column 8)))))))
            (bind ()) (loc ((line 4) (column 8)))))
          (Values
           ((exprs
             ((((Shadow 1) (Id $))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 4) (column 14)))))))
            (bind ()) (loc ((line 4) (column 14)))))))
        (loc ((line 4) (column 0)))))))
    |}]
;;

let%expect_test "local fun inside top-level fun" =
  go
    {|
fun outer (x : int) : int =
  fun inner (y : int) : int = y + x in
  inner x;;
let _ = outer 5;;
|};
  [%expect {|
    (lst
     ((Closure_body
       (path ((Id "\206\187") (Id inner) (Id "\206\187") (Id outer)))
       (arg ((Id y))) (arg_ty Int)
       (captures (((path ((Id x))) (ty Int) (offset_in_bytes 0)))) (bind ())
       (return
        (Binop (op Add) (lhs ((Id y))) (rhs ((Id x))) (ty Int)
         (loc ((line 3) (column 32)))))
       (loc ((line 3) (column 6))))
      (Closure_body (path ((Id "\206\187") (Id outer))) (arg ((Id x)))
       (arg_ty Int) (captures ())
       (bind
        ((Functions
          ((paths
            ((((Id inner) (Id "\206\187") (Id outer))
              (Closure (arg_ty Int) (ret_ty Int))
              ((Id "\206\187") (Id inner) (Id "\206\187") (Id outer)))))
           (captures
            ((entries (((path ((Id x))) (ty Int) (offset_in_bytes 0))))
             (size_in_bytes 8)))
           (loc ((line 3) (column 2)))))))
       (return
        (Apply_closure (fn ((Id inner) (Id "\206\187") (Id outer)))
         (arg ((Id x))) (arg_ty Int) (ty Int) (loc ((line 4) (column 2)))))
       (loc ((line 2) (column 4))))
      (Functions
       ((paths
         ((((Id outer)) (Closure (arg_ty Int) (ret_ty Int))
           ((Id "\206\187") (Id outer)))))
        (captures ((entries ()) (size_in_bytes 0))) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id outer))) (arg ((Id $))) (arg_ty Int) (ty Int)
            (loc ((line 5) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value (Int 5)) (ty Int) (loc ((line 5) (column 14)))))))
            (bind ()) (loc ((line 5) (column 14)))))))
        (loc ((line 5) (column 0)))))))
    |}]
;;

let%expect_test "mutually recursive local closures share environment" =
  go
    {|
fun outer (x : int) : int =
  fun f (a : int) : int = g (a + x)
  and g (b : int) : int = f (b + x) in
  f 0;;
let _ = outer 5;;
|};
  [%expect {|
    (lst
     ((Closure_body (path ((Id "\206\187") (Id f) (Id "\206\187") (Id outer)))
       (arg ((Id a))) (arg_ty Int)
       (captures
        (((path ((Id x))) (ty Int) (offset_in_bytes 32))
         ((path ((Id g))) (ty (Closure (arg_ty Int) (ret_ty Int)))
          (offset_in_bytes 16))))
       (bind
        ((Values
          ((exprs
            ((((Id $))
              (Binop (op Add) (lhs ((Id a))) (rhs ((Id x))) (ty Int)
               (loc ((line 3) (column 31)))))))
           (bind ()) (loc ((line 3) (column 31)))))))
       (return
        (Apply_closure (fn ((Id g))) (arg ((Id $))) (arg_ty Int) (ty Int)
         (loc ((line 3) (column 26)))))
       (loc ((line 3) (column 6))))
      (Closure_body (path ((Id "\206\187") (Id g) (Id "\206\187") (Id outer)))
       (arg ((Id b))) (arg_ty Int)
       (captures
        (((path ((Id x))) (ty Int) (offset_in_bytes 32))
         ((path ((Id f))) (ty (Closure (arg_ty Int) (ret_ty Int)))
          (offset_in_bytes 0))))
       (bind
        ((Values
          ((exprs
            ((((Id $))
              (Binop (op Add) (lhs ((Id b))) (rhs ((Id x))) (ty Int)
               (loc ((line 4) (column 31)))))))
           (bind ()) (loc ((line 4) (column 31)))))))
       (return
        (Apply_closure (fn ((Id f))) (arg ((Id $))) (arg_ty Int) (ty Int)
         (loc ((line 4) (column 26)))))
       (loc ((line 4) (column 6))))
      (Closure_body (path ((Id "\206\187") (Id outer))) (arg ((Id x)))
       (arg_ty Int) (captures ())
       (bind
        ((Functions
          ((paths
            ((((Id f) (Id "\206\187") (Id outer))
              (Closure (arg_ty Int) (ret_ty Int))
              ((Id "\206\187") (Id f) (Id "\206\187") (Id outer)))
             (((Id g) (Id "\206\187") (Id outer))
              (Closure (arg_ty Int) (ret_ty Int))
              ((Id "\206\187") (Id g) (Id "\206\187") (Id outer)))))
           (captures
            ((entries
              (((path ((Id f) (Id "\206\187") (Id outer)))
                (ty (Closure (arg_ty Int) (ret_ty Int))) (offset_in_bytes 0))
               ((path ((Id g) (Id "\206\187") (Id outer)))
                (ty (Closure (arg_ty Int) (ret_ty Int))) (offset_in_bytes 16))
               ((path ((Id x))) (ty Int) (offset_in_bytes 32))))
             (size_in_bytes 40)))
           (loc ((line 3) (column 2)))))
         (Values
          ((exprs
            ((((Id $))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 5) (column 4)))))))
           (bind ()) (loc ((line 5) (column 4)))))))
       (return
        (Apply_closure (fn ((Id f) (Id "\206\187") (Id outer))) (arg ((Id $)))
         (arg_ty Int) (ty Int) (loc ((line 5) (column 2)))))
       (loc ((line 2) (column 4))))
      (Functions
       ((paths
         ((((Id outer)) (Closure (arg_ty Int) (ret_ty Int))
           ((Id "\206\187") (Id outer)))))
        (captures ((entries ()) (size_in_bytes 0))) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id outer))) (arg ((Id $))) (arg_ty Int) (ty Int)
            (loc ((line 6) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value (Int 5)) (ty Int) (loc ((line 6) (column 14)))))))
            (bind ()) (loc ((line 6) (column 14)))))))
        (loc ((line 6) (column 0)))))))
    |}]
;;

let%expect_test "monomorphizing side effects" =
  go
    {|
external print_int : int -> unit = syl_print_int;;
fun print (static _ : unit) : unit = print_int 0;;
let _ = print ();;
|};
  [%expect {|
    (lst
     ((External (path ((Id "\206\187") (Id syl_print_int)))
       (symbol syl_print_int) (arg_ty Int) (ret_ty Unit)
       (loc ((line 2) (column 0))))
      (Values
       ((exprs
         ((((Id print_int))
           (Make_closure (body ((Id "\206\187") (Id syl_print_int))) (env ())
            (ty (Closure (arg_ty Int) (ret_ty Unit)))
            (loc ((line 2) (column 0)))))))
        (bind ()) (loc ((line 2) (column 0)))))
      (Thunk_body (path ((Key Unit) (Id "\206\187") (Id print)))
       (captures
        (((path ((Id print_int))) (ty (Closure (arg_ty Int) (ret_ty Unit)))
          (offset_in_bytes 0))))
       (bind
        ((Values
          ((exprs
            ((((Id _) (Key Unit) (Id "\206\187") (Id print))
              (Scalar (value Unit) (ty Unit) (loc ((line 3) (column 4)))))))
           (bind ()) (loc ((line 3) (column 4)))))
         (Values
          ((exprs
            ((((Id $))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 47)))))))
           (bind ()) (loc ((line 3) (column 47)))))))
       (return
        (Apply_closure (fn ((Id print_int))) (arg ((Id $))) (arg_ty Int)
         (ty Unit) (loc ((line 3) (column 37)))))
       (loc ((line 3) (column 4))))
      (Functions
       ((paths
         ((((Key Unit) (Id print)) (Thunk Unit)
           ((Key Unit) (Id "\206\187") (Id print)))))
        (captures
         ((entries
           (((path ((Id print_int))) (ty (Closure (arg_ty Int) (ret_ty Unit)))
             (offset_in_bytes 0))))
          (size_in_bytes 16)))
        (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key Unit) (Id print))) (ty Unit)
            (loc ((line 4) (column 8)))))))
        (bind ()) (loc ((line 4) (column 0)))))))
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
    (lst
     ((External (path ((Id "\206\187") (Id asdf))) (symbol asdf) (arg_ty Int)
       (ret_ty Int) (loc ((line 2) (column 0))))
      (Values
       ((exprs
         ((((Id f))
           (Make_closure (body ((Id "\206\187") (Id asdf))) (env ())
            (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 2) (column 0)))))))
        (bind ()) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id f))) (arg ((Id $))) (arg_ty Int) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 10)))))))
            (bind ()) (loc ((line 3) (column 10)))))))
        (loc ((line 3) (column 0)))))))
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
    (lst
     ((External (path ((Id "\206\187") (Id asdf))) (symbol asdf) (arg_ty Int)
       (ret_ty Int) (loc ((line 2) (column 0))))
      (Values
       ((exprs
         ((((Id f))
           (Make_closure (body ((Id "\206\187") (Id asdf))) (env ())
            (ty (Closure (arg_ty Int) (ret_ty Int))) (loc ((line 2) (column 0)))))))
        (bind ()) (loc ((line 2) (column 0)))))
      (External (path ((Id "\206\187") (Id asdf) (Id _))) (symbol asdf)
       (arg_ty Int) (ret_ty Int) (loc ((line 3) (column 11))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id $))) (arg ((Shadow 1) (Id $))) (arg_ty Int)
            (ty Int) (loc ((line 3) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Make_closure (body ((Id "\206\187") (Id asdf) (Id _))) (env ())
                (ty (Closure (arg_ty Int) (ret_ty Int)))
                (loc ((line 3) (column 11)))))))
            (bind ()) (loc ((line 3) (column 11)))))
          (Values
           ((exprs
             ((((Shadow 1) (Id $))
               (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 21)))))))
            (bind ()) (loc ((line 3) (column 21)))))))
        (loc ((line 3) (column 0)))))))
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
    (lst
     ((External (path ((Id "\206\187") (Id syl_print_int)))
       (symbol syl_print_int) (arg_ty Int) (ret_ty Unit)
       (loc ((line 2) (column 0))))
      (Values
       ((exprs
         ((((Id print_int))
           (Make_closure (body ((Id "\206\187") (Id syl_print_int))) (env ())
            (ty (Closure (arg_ty Int) (ret_ty Unit)))
            (loc ((line 2) (column 0)))))))
        (bind ()) (loc ((line 2) (column 0)))))
      (Thunk_body (path ((Key (Int 1)) (Id "\206\187") (Id print)))
       (captures
        (((path ((Id print_int))) (ty (Closure (arg_ty Int) (ret_ty Unit)))
          (offset_in_bytes 0))))
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 1)) (Id "\206\187") (Id print))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 12)))))))
           (bind ()) (loc ((line 3) (column 12)))))))
       (return
        (Apply_closure (fn ((Id print_int)))
         (arg ((Id x) (Key (Int 1)) (Id "\206\187") (Id print))) (arg_ty Int)
         (ty Unit) (loc ((line 3) (column 35)))))
       (loc ((line 3) (column 12))))
      (Thunk_body (path ((Key (Int 0)) (Id "\206\187") (Id print)))
       (captures
        (((path ((Id print_int))) (ty (Closure (arg_ty Int) (ret_ty Unit)))
          (offset_in_bytes 0))))
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Int 0)) (Id "\206\187") (Id print))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 3) (column 12)))))))
           (bind ()) (loc ((line 3) (column 12)))))))
       (return
        (Apply_closure (fn ((Id print_int)))
         (arg ((Id x) (Key (Int 0)) (Id "\206\187") (Id print))) (arg_ty Int)
         (ty Unit) (loc ((line 3) (column 35)))))
       (loc ((line 3) (column 12))))
      (Values
       ((exprs
         ((((Key (Int 1)) (Id print))
           (Make_closure (body ((Key (Int 1)) (Id "\206\187") (Id print)))
            (env (((Id env) (Id print)))) (ty (Thunk Unit))
            (loc ((line 3) (column 12)))))
          (((Key (Int 0)) (Id print))
           (Make_closure (body ((Key (Int 0)) (Id "\206\187") (Id print)))
            (env (((Id env) (Id print)))) (ty (Thunk Unit))
            (loc ((line 3) (column 12)))))))
        (bind
         ((Values
           ((exprs
             ((((Id env) (Id print))
               (Make_env
                (captures
                 ((entries
                   (((path ((Id print_int)))
                     (ty (Closure (arg_ty Int) (ret_ty Unit)))
                     (offset_in_bytes 0))))
                  (size_in_bytes 16)))
                (ty Env) (loc ((line 3) (column 12)))))))
            (bind ()) (loc ((line 3) (column 12)))))))
        (loc ((line 3) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_thunk (fn ((Key (Int 0)) (Id print))) (ty Unit)
            (loc ((line 4) (column 8)))))))
        (bind ()) (loc ((line 4) (column 0)))))
      (Values
       ((exprs
         ((((Shadow 1) (Id _))
           (Apply_thunk (fn ((Key (Int 1)) (Id print))) (ty Unit)
            (loc ((line 5) (column 8)))))))
        (bind ()) (loc ((line 5) (column 0)))))))
    |}]
;;

let%expect_test "static erased lambda" =
  go
    {|
fun apply (static erased f : int -> int) : int = f 0;;
let x = apply (fn (x : int) -> x + 1);;
|};
  [%expect {|
    (lst
     ((Thunk_body (path ((Key (Closure 2)) (Id "\206\187") (Id apply)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id x) (Key (Closure 2)) (Id "\206\187") (Id apply))
              (Scalar (value (Int 0)) (ty Int) (loc ((line 2) (column 51)))))))
           (bind ()) (loc ((line 2) (column 49)))))
         (Values
          ((exprs
            ((((Id $))
              (Scalar (value (Int 1)) (ty Int) (loc ((line 3) (column 35)))))))
           (bind ()) (loc ((line 3) (column 35)))))))
       (return
        (Binop (op Add)
         (lhs ((Id x) (Key (Closure 2)) (Id "\206\187") (Id apply)))
         (rhs ((Id $))) (ty Int) (loc ((line 3) (column 33)))))
       (loc ((line 2) (column 4))))
      (Functions
       ((paths
         ((((Key (Closure 2)) (Id apply)) (Thunk Int)
           ((Key (Closure 2)) (Id "\206\187") (Id apply)))))
        (captures ((entries ()) (size_in_bytes 0))) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id x))
           (Apply_thunk (fn ((Key (Closure 2)) (Id apply))) (ty Int)
            (loc ((line 3) (column 8)))))))
        (bind ()) (loc ((line 3) (column 0)))))))
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
  [%expect {|
    (lst
     ((Closure_body
       (path ((Id "\206\187") (Key (Closure 6)) (Id "\206\187") (Id mk_ident)))
       (arg ((Id x))) (arg_ty Bool) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Bool) (loc ((line 3) (column 24)))))
       (loc ((line 3) (column 2))))
      (Thunk_body (path ((Key (Closure 6)) (Id "\206\187") (Id mk_ident)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id env) (Key (Closure 6)) (Id "\206\187") (Id mk_ident))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 3) (column 2)))))))
           (bind ()) (loc ((line 3) (column 2)))))))
       (return
        (Make_closure
         (body ((Id "\206\187") (Key (Closure 6)) (Id "\206\187") (Id mk_ident)))
         (env (((Id env) (Key (Closure 6)) (Id "\206\187") (Id mk_ident))))
         (ty (Closure (arg_ty Bool) (ret_ty Bool))) (loc ((line 3) (column 2)))))
       (loc ((line 2) (column 4))))
      (Functions
       ((paths
         ((((Key (Closure 6)) (Id mk_ident))
           (Thunk (Closure (arg_ty Bool) (ret_ty Bool)))
           ((Key (Closure 6)) (Id "\206\187") (Id mk_ident)))))
        (captures ((entries ()) (size_in_bytes 0))) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id $))) (arg ((Shadow 1) (Id $))) (arg_ty Bool)
            (ty Bool) (loc ((line 6) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Apply_thunk (fn ((Key (Closure 6)) (Id mk_ident)))
                (ty (Closure (arg_ty Bool) (ret_ty Bool)))
                (loc ((line 6) (column 8)))))))
            (bind ()) (loc ((line 6) (column 8)))))
          (Values
           ((exprs
             ((((Shadow 1) (Id $))
               (Scalar (value (Bool true)) (ty Bool)
                (loc ((line 6) (column 77)))))))
            (bind ()) (loc ((line 6) (column 77)))))))
        (loc ((line 6) (column 0)))))))
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
    (lst
     ((Thunk_body
       (path
        ((Key Unit) (Id "\206\187") (Id pick_t) (Key (Closure 6)) (Id "\206\187")
         (Id mk_ident)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Id _) (Key Unit) (Id "\206\187") (Id pick_t) (Key (Closure 6))
               (Id "\206\187") (Id mk_ident))
              (Scalar (value Unit) (ty Unit) (loc ((line 2) (column 4)))))))
           (bind ()) (loc ((line 2) (column 4)))))))
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 6) (column 42)))))
       (loc ((line 2) (column 4))))
      (Closure_body
       (path ((Id "\206\187") (Key (Closure 6)) (Id "\206\187") (Id mk_ident)))
       (arg ((Id x))) (arg_ty Bool) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Bool) (loc ((line 3) (column 51)))))
       (loc ((line 3) (column 2))))
      (Thunk_body (path ((Key (Closure 6)) (Id "\206\187") (Id mk_ident)))
       (captures ())
       (bind
        ((Values
          ((exprs
            ((((Key Unit) (Id pick_t) (Key (Closure 6)) (Id "\206\187")
               (Id mk_ident))
              (Make_closure
               (body
                ((Key Unit) (Id "\206\187") (Id pick_t) (Key (Closure 6))
                 (Id "\206\187") (Id mk_ident)))
               (env
                (((Id env) (Id pick_t) (Key (Closure 6)) (Id "\206\187")
                  (Id mk_ident))))
               (ty (Thunk Int)) (loc ((line 2) (column 4)))))))
           (bind
            ((Values
              ((exprs
                ((((Id env) (Id pick_t) (Key (Closure 6)) (Id "\206\187")
                   (Id mk_ident))
                  (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
                   (loc ((line 2) (column 4)))))))
               (bind ()) (loc ((line 2) (column 4)))))))
           (loc ((line 2) (column 4)))))
         (Values
          ((exprs
            ((((Id env) (Key (Closure 6)) (Id "\206\187") (Id mk_ident))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 3) (column 2)))))))
           (bind ()) (loc ((line 3) (column 2)))))))
       (return
        (Make_closure
         (body ((Id "\206\187") (Key (Closure 6)) (Id "\206\187") (Id mk_ident)))
         (env (((Id env) (Key (Closure 6)) (Id "\206\187") (Id mk_ident))))
         (ty (Closure (arg_ty Bool) (ret_ty Bool))) (loc ((line 3) (column 2)))))
       (loc ((line 2) (column 4))))
      (Functions
       ((paths
         ((((Key (Closure 6)) (Id mk_ident))
           (Thunk (Closure (arg_ty Bool) (ret_ty Bool)))
           ((Key (Closure 6)) (Id "\206\187") (Id mk_ident)))))
        (captures ((entries ()) (size_in_bytes 0))) (loc ((line 2) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id $))) (arg ((Shadow 1) (Id $))) (arg_ty Bool)
            (ty Bool) (loc ((line 6) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Apply_thunk (fn ((Key (Closure 6)) (Id mk_ident)))
                (ty (Closure (arg_ty Bool) (ret_ty Bool)))
                (loc ((line 6) (column 8)))))))
            (bind ()) (loc ((line 6) (column 8)))))
          (Values
           ((exprs
             ((((Shadow 1) (Id $))
               (Scalar (value (Bool true)) (ty Bool)
                (loc ((line 6) (column 45)))))))
            (bind ()) (loc ((line 6) (column 45)))))))
        (loc ((line 6) (column 0)))))))
    |}]
;;

let%expect_test "static lambda effects are thunked" =
  go
    {|
external print_int : int -> unit = syl_print_int;;

fun mk_ident (static pick_t : static unit -> static int) : static (let t = if pick_t () == 0 then int else bool in t -> t) =
  let _ = pick_t () in
  let _ = pick_t () in
  fn (x : if pick_t () == 0 then int else bool) -> x
;;

let _ = mk_ident (fn (static _ : unit) -> let _ = print_int 10 in 1) true;;
|};
  [%expect {|
    (lst
     ((External (path ((Id "\206\187") (Id syl_print_int)))
       (symbol syl_print_int) (arg_ty Int) (ret_ty Unit)
       (loc ((line 2) (column 0))))
      (Values
       ((exprs
         ((((Id print_int))
           (Make_closure (body ((Id "\206\187") (Id syl_print_int))) (env ())
            (ty (Closure (arg_ty Int) (ret_ty Unit)))
            (loc ((line 2) (column 0)))))))
        (bind ()) (loc ((line 2) (column 0)))))
      (Thunk_body
       (path
        ((Key Unit) (Id "\206\187") (Id pick_t) (Key (Closure 6)) (Id "\206\187")
         (Id mk_ident)))
       (captures
        (((path ((Id print_int))) (ty (Closure (arg_ty Int) (ret_ty Unit)))
          (offset_in_bytes 0))))
       (bind
        ((Values
          ((exprs
            ((((Id _) (Key Unit) (Id "\206\187") (Id pick_t) (Key (Closure 6))
               (Id "\206\187") (Id mk_ident))
              (Scalar (value Unit) (ty Unit) (loc ((line 4) (column 4)))))))
           (bind ()) (loc ((line 4) (column 4)))))
         (Values
          ((exprs
            ((((Shadow 1) (Id _) (Key Unit) (Id "\206\187") (Id pick_t)
               (Key (Closure 6)) (Id "\206\187") (Id mk_ident))
              (Apply_closure (fn ((Id print_int))) (arg ((Id $))) (arg_ty Int)
               (ty Unit) (loc ((line 10) (column 50)))))))
           (bind
            ((Values
              ((exprs
                ((((Id $))
                  (Scalar (value (Int 10)) (ty Int)
                   (loc ((line 10) (column 60)))))))
               (bind ()) (loc ((line 10) (column 60)))))))
           (loc ((line 10) (column 42)))))))
       (return (Scalar (value (Int 1)) (ty Int) (loc ((line 10) (column 66)))))
       (loc ((line 4) (column 4))))
      (Closure_body
       (path ((Id "\206\187") (Key (Closure 6)) (Id "\206\187") (Id mk_ident)))
       (arg ((Id x))) (arg_ty Bool) (captures ()) (bind ())
       (return (Ident (path ((Id x))) (ty Bool) (loc ((line 7) (column 51)))))
       (loc ((line 7) (column 2))))
      (Thunk_body (path ((Key (Closure 6)) (Id "\206\187") (Id mk_ident)))
       (captures
        (((path ((Id print_int))) (ty (Closure (arg_ty Int) (ret_ty Unit)))
          (offset_in_bytes 0))))
       (bind
        ((Values
          ((exprs
            ((((Key Unit) (Id pick_t) (Key (Closure 6)) (Id "\206\187")
               (Id mk_ident))
              (Make_closure
               (body
                ((Key Unit) (Id "\206\187") (Id pick_t) (Key (Closure 6))
                 (Id "\206\187") (Id mk_ident)))
               (env
                (((Id env) (Id pick_t) (Key (Closure 6)) (Id "\206\187")
                  (Id mk_ident))))
               (ty (Thunk Int)) (loc ((line 4) (column 4)))))))
           (bind
            ((Values
              ((exprs
                ((((Id env) (Id pick_t) (Key (Closure 6)) (Id "\206\187")
                   (Id mk_ident))
                  (Make_env
                   (captures
                    ((entries
                      (((path ((Id print_int)))
                        (ty (Closure (arg_ty Int) (ret_ty Unit)))
                        (offset_in_bytes 0))))
                     (size_in_bytes 16)))
                   (ty Env) (loc ((line 4) (column 4)))))))
               (bind ()) (loc ((line 4) (column 4)))))))
           (loc ((line 4) (column 4)))))
         (Values
          ((exprs
            ((((Id _) (Key (Closure 6)) (Id "\206\187") (Id mk_ident))
              (Apply_thunk
               (fn
                ((Key Unit) (Id pick_t) (Key (Closure 6)) (Id "\206\187")
                 (Id mk_ident)))
               (ty Int) (loc ((line 5) (column 10)))))))
           (bind ()) (loc ((line 5) (column 2)))))
         (Values
          ((exprs
            ((((Shadow 1) (Id _) (Key (Closure 6)) (Id "\206\187") (Id mk_ident))
              (Apply_thunk
               (fn
                ((Key Unit) (Id pick_t) (Key (Closure 6)) (Id "\206\187")
                 (Id mk_ident)))
               (ty Int) (loc ((line 6) (column 10)))))))
           (bind ()) (loc ((line 6) (column 2)))))
         (Values
          ((exprs
            ((((Id env) (Key (Closure 6)) (Id "\206\187") (Id mk_ident))
              (Make_env (captures ((entries ()) (size_in_bytes 0))) (ty Env)
               (loc ((line 7) (column 2)))))))
           (bind ()) (loc ((line 7) (column 2)))))))
       (return
        (Make_closure
         (body ((Id "\206\187") (Key (Closure 6)) (Id "\206\187") (Id mk_ident)))
         (env (((Id env) (Key (Closure 6)) (Id "\206\187") (Id mk_ident))))
         (ty (Closure (arg_ty Bool) (ret_ty Bool))) (loc ((line 7) (column 2)))))
       (loc ((line 4) (column 4))))
      (Functions
       ((paths
         ((((Key (Closure 6)) (Id mk_ident))
           (Thunk (Closure (arg_ty Bool) (ret_ty Bool)))
           ((Key (Closure 6)) (Id "\206\187") (Id mk_ident)))))
        (captures
         ((entries
           (((path ((Id print_int))) (ty (Closure (arg_ty Int) (ret_ty Unit)))
             (offset_in_bytes 0))))
          (size_in_bytes 16)))
        (loc ((line 4) (column 0)))))
      (Values
       ((exprs
         ((((Id _))
           (Apply_closure (fn ((Id $))) (arg ((Shadow 1) (Id $))) (arg_ty Bool)
            (ty Bool) (loc ((line 10) (column 8)))))))
        (bind
         ((Values
           ((exprs
             ((((Id $))
               (Apply_thunk (fn ((Key (Closure 6)) (Id mk_ident)))
                (ty (Closure (arg_ty Bool) (ret_ty Bool)))
                (loc ((line 10) (column 8)))))))
            (bind ()) (loc ((line 10) (column 8)))))
          (Values
           ((exprs
             ((((Shadow 1) (Id $))
               (Scalar (value (Bool true)) (ty Bool)
                (loc ((line 10) (column 69)))))))
            (bind ()) (loc ((line 10) (column 69)))))))
        (loc ((line 10) (column 0)))))))
    |}]
;;

let%expect_test "external" =
  go
    {|
external print_int : int -> unit = syl_print_int;;
|};
  [%expect
    {|
    (lst
     ((External (path ((Id "\206\187") (Id syl_print_int)))
       (symbol syl_print_int) (arg_ty Int) (ret_ty Unit)
       (loc ((line 2) (column 0))))
      (Values
       ((exprs
         ((((Id print_int))
           (Make_closure (body ((Id "\206\187") (Id syl_print_int))) (env ())
            (ty (Closure (arg_ty Int) (ret_ty Unit)))
            (loc ((line 2) (column 0)))))))
        (bind ()) (loc ((line 2) (column 0)))))))
    |}]
;;
