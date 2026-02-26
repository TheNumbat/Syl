open! Core
open! Syl

let go input =
  match Parse.parse input with
  | Ok cst ->
    Cst.Program.print () cst |> print_endline;
    print_s [%message (cst : Cst.Program.t)]
  | Error { loc; reason } -> print_s [%message (loc : Lex.Location.t) (reason : Parse.Error.t)]
;;

let%expect_test "arrow" =
  go "let _ = t -> static t -> t;;";
  [%expect
    {|
    let _ = t -> static t -> t;;
    (cst
     ((Let (var _)
       (bind
        (Arrow (arg (Var (id t) (loc ((line 1) (column 8))))) (arg_id ())
         (arg_mode ((staticity ()) (erasure ())))
         (ret
          (Arrow (arg (Var (id t) (loc ((line 1) (column 20))))) (arg_id ())
           (arg_mode ((staticity (Static)) (erasure ())))
           (ret (Var (id t) (loc ((line 1) (column 25)))))
           (ret_mode ((staticity ()) (erasure ()))) (loc ((line 1) (column 22)))))
         (ret_mode ((staticity ()) (erasure ()))) (loc ((line 1) (column 10)))))
       (loc ((line 1) (column 0))))))
    |}];
  go "let _ = t -> (static t -> t);;";
  [%expect
    {|
    let _ = t -> (static t -> t);;
    (cst
     ((Let (var _)
       (bind
        (Arrow (arg (Var (id t) (loc ((line 1) (column 8))))) (arg_id ())
         (arg_mode ((staticity ()) (erasure ())))
         (ret
          (Paren
           (expr
            (Arrow (arg (Var (id t) (loc ((line 1) (column 21))))) (arg_id ())
             (arg_mode ((staticity (Static)) (erasure ())))
             (ret (Var (id t) (loc ((line 1) (column 26)))))
             (ret_mode ((staticity ()) (erasure ())))
             (loc ((line 1) (column 23)))))
           (loc ((line 1) (column 13)))))
         (ret_mode ((staticity ()) (erasure ()))) (loc ((line 1) (column 10)))))
       (loc ((line 1) (column 0))))))
    |}];
  go "let _ = t -> static (t -> t);;";
  [%expect
    {|
    let _ = t -> static (t -> t);;
    (cst
     ((Let (var _)
       (bind
        (Arrow (arg (Var (id t) (loc ((line 1) (column 8))))) (arg_id ())
         (arg_mode ((staticity ()) (erasure ())))
         (ret
          (Paren
           (expr
            (Arrow (arg (Var (id t) (loc ((line 1) (column 21))))) (arg_id ())
             (arg_mode ((staticity ()) (erasure ())))
             (ret (Var (id t) (loc ((line 1) (column 26)))))
             (ret_mode ((staticity ()) (erasure ())))
             (loc ((line 1) (column 23)))))
           (loc ((line 1) (column 20)))))
         (ret_mode ((staticity (Static)) (erasure ())))
         (loc ((line 1) (column 10)))))
       (loc ((line 1) (column 0))))))
    |}];
  go "let _ = t -> static (static t -> t);;";
  [%expect
    {|
    let _ = t -> static (static t -> t);;
    (cst
     ((Let (var _)
       (bind
        (Arrow (arg (Var (id t) (loc ((line 1) (column 8))))) (arg_id ())
         (arg_mode ((staticity ()) (erasure ())))
         (ret
          (Paren
           (expr
            (Arrow (arg (Var (id t) (loc ((line 1) (column 28))))) (arg_id ())
             (arg_mode ((staticity (Static)) (erasure ())))
             (ret (Var (id t) (loc ((line 1) (column 33)))))
             (ret_mode ((staticity ()) (erasure ())))
             (loc ((line 1) (column 30)))))
           (loc ((line 1) (column 20)))))
         (ret_mode ((staticity (Static)) (erasure ())))
         (loc ((line 1) (column 10)))))
       (loc ((line 1) (column 0))))))
    |}];
  go "let _ = t -> static static t -> t;;";
  [%expect {| ((loc ((line 1) (column 13))) (reason (Duplicate_mode Staticity))) |}];
  go "let _ = t -> static t;;";
  [%expect
    {|
    let _ = t -> static t;;
    (cst
     ((Let (var _)
       (bind
        (Arrow (arg (Var (id t) (loc ((line 1) (column 8))))) (arg_id ())
         (arg_mode ((staticity ()) (erasure ())))
         (ret (Var (id t) (loc ((line 1) (column 20)))))
         (ret_mode ((staticity (Static)) (erasure ())))
         (loc ((line 1) (column 10)))))
       (loc ((line 1) (column 0))))))
    |}]
;;

let%expect_test "arrow" =
  go "let _ = (int : type) \\ name -> static (erased type -> static name);;";
  [%expect
    {|
    let _ = (int : type) \ name -> static (erased type -> static name);;
    (cst
     ((Let (var _)
       (bind
        (Arrow
         (arg
          (Paren
           (expr
            (Type_annotation (expr (Var (id int) (loc ((line 1) (column 9)))))
             (ty (Var (id type) (loc ((line 1) (column 15)))))
             (loc ((line 1) (column 13)))))
           (loc ((line 1) (column 8)))))
         (arg_id (name)) (arg_mode ((staticity ()) (erasure ())))
         (ret
          (Paren
           (expr
            (Arrow (arg (Var (id type) (loc ((line 1) (column 46))))) (arg_id ())
             (arg_mode ((staticity ()) (erasure (Erased))))
             (ret (Var (id name) (loc ((line 1) (column 61)))))
             (ret_mode ((staticity (Static)) (erasure ())))
             (loc ((line 1) (column 51)))))
           (loc ((line 1) (column 38)))))
         (ret_mode ((staticity (Static)) (erasure ())))
         (loc ((line 1) (column 21)))))
       (loc ((line 1) (column 0))))))
    |}]
;;

let%expect_test "funs" =
  go "let _ = fun x (_ : unit) : unit = () in x;;";
  [%expect
    {|
    let _ = fun x ( _ : unit) : unit = (); x;;
    (cst
     ((Let (var _)
       (bind
        (Fun
         (funs
          (((var x) (arg _) (erased Unerased)
            (arg_mode ((staticity ()) (erasure ())))
            (arg_ty (Var (id unit) (loc ((line 1) (column 19)))))
            (ret_mode ((staticity ()) (erasure ())))
            (ret_ty (Var (id unit) (loc ((line 1) (column 27)))))
            (body (Literal (value Unit) (loc ((line 1) (column 34)))))
            (loc ((line 1) (column 12))))))
         (rest (Var (id x) (loc ((line 1) (column 40)))))
         (loc ((line 1) (column 8)))))
       (loc ((line 1) (column 0))))))
    |}]
;;

let%expect_test "bad modes" =
  go "fun x (_ : unit @ ) : unit = ();;";
  [%expect {| ((loc ((line 1) (column 18))) (reason (Unexpected Rparen))) |}];
  go "fun x (_ : unit @ static static) : unit = ();;";
  [%expect {| ((loc ((line 1) (column 18))) (reason (Duplicate_mode Staticity))) |}];
  go "fun x (_ : unit @ erased static erased) : unit = ();;";
  [%expect {| ((loc ((line 1) (column 18))) (reason (Duplicate_mode Erasure))) |}]
;;

let%expect_test "arrow" =
  go "let _ = 1 + 2 -> 3 @ static;;";
  [%expect
    {|
    let _ = 1 + 2 -> 3 @ static ;;
    (cst
     ((Let (var _)
       (bind
        (Mode_annotation
         (expr
          (Arrow
           (arg
            (Binop (op Add)
             (lhs (Literal (value (Int 1)) (loc ((line 1) (column 8)))))
             (rhs (Literal (value (Int 2)) (loc ((line 1) (column 12)))))
             (loc ((line 1) (column 10)))))
           (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
           (ret (Literal (value (Int 3)) (loc ((line 1) (column 17)))))
           (ret_mode ((staticity ()) (erasure ()))) (loc ((line 1) (column 14)))))
         (mode ((staticity (Static)) (erasure ()))) (loc ((line 1) (column 19)))))
       (loc ((line 1) (column 0))))))
    |}];
  go "let x = x : type -> x * x;;";
  [%expect
    {|
    let x = x : type -> x * x;;
    (cst
     ((Let (var x)
       (bind
        (Type_annotation (expr (Var (id x) (loc ((line 1) (column 8)))))
         (ty
          (Arrow (arg (Var (id type) (loc ((line 1) (column 12))))) (arg_id ())
           (arg_mode ((staticity ()) (erasure ())))
           (ret
            (Binop (op Mul) (lhs (Var (id x) (loc ((line 1) (column 20)))))
             (rhs (Var (id x) (loc ((line 1) (column 24)))))
             (loc ((line 1) (column 22)))))
           (ret_mode ((staticity ()) (erasure ()))) (loc ((line 1) (column 17)))))
         (loc ((line 1) (column 10)))))
       (loc ((line 1) (column 0))))))
    |}];
  go "let x = (x : type) -> x * x;;";
  [%expect
    {|
    let x = (x : type) -> x * x;;
    (cst
     ((Let (var x)
       (bind
        (Arrow
         (arg
          (Paren
           (expr
            (Type_annotation (expr (Var (id x) (loc ((line 1) (column 9)))))
             (ty (Var (id type) (loc ((line 1) (column 13)))))
             (loc ((line 1) (column 11)))))
           (loc ((line 1) (column 8)))))
         (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
         (ret
          (Binop (op Mul) (lhs (Var (id x) (loc ((line 1) (column 22)))))
           (rhs (Var (id x) (loc ((line 1) (column 26)))))
           (loc ((line 1) (column 24)))))
         (ret_mode ((staticity ()) (erasure ()))) (loc ((line 1) (column 19)))))
       (loc ((line 1) (column 0))))))
    |}];
  go "let _ = x -> y;;";
  [%expect
    {|
    let _ = x -> y;;
    (cst
     ((Let (var _)
       (bind
        (Arrow (arg (Var (id x) (loc ((line 1) (column 8))))) (arg_id ())
         (arg_mode ((staticity ()) (erasure ())))
         (ret (Var (id y) (loc ((line 1) (column 13)))))
         (ret_mode ((staticity ()) (erasure ()))) (loc ((line 1) (column 10)))))
       (loc ((line 1) (column 0))))))
    |}];
  go "let _ = erased static x -> y;;";
  [%expect
    {|
    let _ = static erased x -> y;;
    (cst
     ((Let (var _)
       (bind
        (Arrow (arg (Var (id x) (loc ((line 1) (column 22))))) (arg_id ())
         (arg_mode ((staticity (Static)) (erasure (Erased))))
         (ret (Var (id y) (loc ((line 1) (column 27)))))
         (ret_mode ((staticity ()) (erasure ()))) (loc ((line 1) (column 24)))))
       (loc ((line 1) (column 0))))))
    |}];
  go "let _ = static x -> erased y;;";
  [%expect
    {|
    let _ = static x -> erased y;;
    (cst
     ((Let (var _)
       (bind
        (Arrow (arg (Var (id x) (loc ((line 1) (column 15))))) (arg_id ())
         (arg_mode ((staticity (Static)) (erasure ())))
         (ret (Var (id y) (loc ((line 1) (column 27)))))
         (ret_mode ((staticity ()) (erasure (Erased))))
         (loc ((line 1) (column 17)))))
       (loc ((line 1) (column 0))))))
    |}];
  go "let _ = static x -> (erased y -> z);;";
  [%expect
    {|
    let _ = static x -> (erased y -> z);;
    (cst
     ((Let (var _)
       (bind
        (Arrow (arg (Var (id x) (loc ((line 1) (column 15))))) (arg_id ())
         (arg_mode ((staticity (Static)) (erasure ())))
         (ret
          (Paren
           (expr
            (Arrow (arg (Var (id y) (loc ((line 1) (column 28))))) (arg_id ())
             (arg_mode ((staticity ()) (erasure (Erased))))
             (ret (Var (id z) (loc ((line 1) (column 33)))))
             (ret_mode ((staticity ()) (erasure ())))
             (loc ((line 1) (column 30)))))
           (loc ((line 1) (column 20)))))
         (ret_mode ((staticity ()) (erasure ()))) (loc ((line 1) (column 17)))))
       (loc ((line 1) (column 0))))))
    |}];
  go "let _ = (static x -> erased y) -> z;;";
  [%expect
    {|
    let _ = (static x -> erased y) -> z;;
    (cst
     ((Let (var _)
       (bind
        (Arrow
         (arg
          (Paren
           (expr
            (Arrow (arg (Var (id x) (loc ((line 1) (column 16))))) (arg_id ())
             (arg_mode ((staticity (Static)) (erasure ())))
             (ret (Var (id y) (loc ((line 1) (column 28)))))
             (ret_mode ((staticity ()) (erasure (Erased))))
             (loc ((line 1) (column 18)))))
           (loc ((line 1) (column 8)))))
         (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
         (ret (Var (id z) (loc ((line 1) (column 34)))))
         (ret_mode ((staticity ()) (erasure ()))) (loc ((line 1) (column 31)))))
       (loc ((line 1) (column 0))))))
    |}];
  go "let _ = static x -> (y -> z);;";
  [%expect
    {|
    let _ = static x -> (y -> z);;
    (cst
     ((Let (var _)
       (bind
        (Arrow (arg (Var (id x) (loc ((line 1) (column 15))))) (arg_id ())
         (arg_mode ((staticity (Static)) (erasure ())))
         (ret
          (Paren
           (expr
            (Arrow (arg (Var (id y) (loc ((line 1) (column 21))))) (arg_id ())
             (arg_mode ((staticity ()) (erasure ())))
             (ret (Var (id z) (loc ((line 1) (column 26)))))
             (ret_mode ((staticity ()) (erasure ())))
             (loc ((line 1) (column 23)))))
           (loc ((line 1) (column 20)))))
         (ret_mode ((staticity ()) (erasure ()))) (loc ((line 1) (column 17)))))
       (loc ((line 1) (column 0))))))
    |}];
  go "let _ = static x -> erased y -> z;;";
  [%expect
    {|
    let _ = static x -> erased y -> z;;
    (cst
     ((Let (var _)
       (bind
        (Arrow (arg (Var (id x) (loc ((line 1) (column 15))))) (arg_id ())
         (arg_mode ((staticity (Static)) (erasure ())))
         (ret
          (Arrow (arg (Var (id y) (loc ((line 1) (column 27))))) (arg_id ())
           (arg_mode ((staticity ()) (erasure (Erased))))
           (ret (Var (id z) (loc ((line 1) (column 32)))))
           (ret_mode ((staticity ()) (erasure ()))) (loc ((line 1) (column 29)))))
         (ret_mode ((staticity ()) (erasure ()))) (loc ((line 1) (column 17)))))
       (loc ((line 1) (column 0))))))
    |}]
;;

let%expect_test "static" =
  go "let _ = fn (static x : int @ static) -> ();;";
  [%expect
    {|
    let _ = fn (static x : int @ static ) -> ();;
    (cst
     ((Let (var _)
       (bind
        (Lambda (arg x) (erased Unerased)
         (arg_mode ((staticity (Static)) (erasure ())))
         (arg_ty
          (Mode_annotation (expr (Var (id int) (loc ((line 1) (column 23)))))
           (mode ((staticity (Static)) (erasure ())))
           (loc ((line 1) (column 27)))))
         (body (Literal (value Unit) (loc ((line 1) (column 40)))))
         (loc ((line 1) (column 8)))))
       (loc ((line 1) (column 0))))))
    |}];
  go "let _ = fn (x : (int)) -> ();;";
  [%expect
    {|
    let _ = fn (x : (int)) -> ();;
    (cst
     ((Let (var _)
       (bind
        (Lambda (arg x) (erased Unerased)
         (arg_mode ((staticity ()) (erasure ())))
         (arg_ty
          (Paren (expr (Var (id int) (loc ((line 1) (column 17)))))
           (loc ((line 1) (column 16)))))
         (body (Literal (value Unit) (loc ((line 1) (column 26)))))
         (loc ((line 1) (column 8)))))
       (loc ((line 1) (column 0))))))
    |}];
  go "let _ = fn (x : int @ static static) -> ();;";
  [%expect {| ((loc ((line 1) (column 22))) (reason (Duplicate_mode Staticity))) |}];
  go "let _ = fn (x : int @ (static)) -> ();;";
  [%expect {| ((loc ((line 1) (column 22))) (reason (Unexpected Lparen))) |}];
  go "let _ = fn (x : (int) @ static) -> ();;";
  [%expect
    {|
    let _ = fn (x : (int) @ static ) -> ();;
    (cst
     ((Let (var _)
       (bind
        (Lambda (arg x) (erased Unerased)
         (arg_mode ((staticity ()) (erasure ())))
         (arg_ty
          (Mode_annotation
           (expr
            (Paren (expr (Var (id int) (loc ((line 1) (column 17)))))
             (loc ((line 1) (column 16)))))
           (mode ((staticity (Static)) (erasure ())))
           (loc ((line 1) (column 22)))))
         (body (Literal (value Unit) (loc ((line 1) (column 35)))))
         (loc ((line 1) (column 8)))))
       (loc ((line 1) (column 0))))))
    |}];
  go "let _ = fn (x : (int @ static) @ static) -> ();;";
  [%expect
    {|
    let _ = fn (x : (int @ static ) @ static ) -> ();;
    (cst
     ((Let (var _)
       (bind
        (Lambda (arg x) (erased Unerased)
         (arg_mode ((staticity ()) (erasure ())))
         (arg_ty
          (Mode_annotation
           (expr
            (Paren
             (expr
              (Mode_annotation (expr (Var (id int) (loc ((line 1) (column 17)))))
               (mode ((staticity (Static)) (erasure ())))
               (loc ((line 1) (column 21)))))
             (loc ((line 1) (column 16)))))
           (mode ((staticity (Static)) (erasure ())))
           (loc ((line 1) (column 31)))))
         (body (Literal (value Unit) (loc ((line 1) (column 44)))))
         (loc ((line 1) (column 8)))))
       (loc ((line 1) (column 0))))))
    |}];
  go "let _ = fn (x : (int @ static)) -> ();;";
  [%expect
    {|
    let _ = fn (x : (int @ static )) -> ();;
    (cst
     ((Let (var _)
       (bind
        (Lambda (arg x) (erased Unerased)
         (arg_mode ((staticity ()) (erasure ())))
         (arg_ty
          (Paren
           (expr
            (Mode_annotation (expr (Var (id int) (loc ((line 1) (column 17)))))
             (mode ((staticity (Static)) (erasure ())))
             (loc ((line 1) (column 21)))))
           (loc ((line 1) (column 16)))))
         (body (Literal (value Unit) (loc ((line 1) (column 35)))))
         (loc ((line 1) (column 8)))))
       (loc ((line 1) (column 0))))))
    |}];
  go "let _ = fn (x : int @ static -> int @ static) -> ();;";
  [%expect {| ((loc ((line 1) (column 29))) (reason (Unexpected (Op Arrow)))) |}];
  go "let _ = fn (x : (int -> int) @ static ) -> ();;";
  [%expect
    {|
    let _ = fn (x : (int -> int) @ static ) -> ();;
    (cst
     ((Let (var _)
       (bind
        (Lambda (arg x) (erased Unerased)
         (arg_mode ((staticity ()) (erasure ())))
         (arg_ty
          (Mode_annotation
           (expr
            (Paren
             (expr
              (Arrow (arg (Var (id int) (loc ((line 1) (column 17)))))
               (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
               (ret (Var (id int) (loc ((line 1) (column 24)))))
               (ret_mode ((staticity ()) (erasure ())))
               (loc ((line 1) (column 21)))))
             (loc ((line 1) (column 16)))))
           (mode ((staticity (Static)) (erasure ())))
           (loc ((line 1) (column 29)))))
         (body (Literal (value Unit) (loc ((line 1) (column 43)))))
         (loc ((line 1) (column 8)))))
       (loc ((line 1) (column 0))))))
    |}];
  go "let _ = fn (x : (int @ static -> int @ static) @ static) -> ();;";
  [%expect {| ((loc ((line 1) (column 30))) (reason (Unexpected (Op Arrow)))) |}]
;;

let%expect_test _ =
  go "let _ = 2 - 1;;";
  [%expect
    {|
    let _ = 2 - 1;;
    (cst
     ((Let (var _)
       (bind
        (Binop (op Sub)
         (lhs (Literal (value (Int 2)) (loc ((line 1) (column 8)))))
         (rhs (Literal (value (Int 1)) (loc ((line 1) (column 12)))))
         (loc ((line 1) (column 10)))))
       (loc ((line 1) (column 0))))))
    |}]
;;

let%expect_test _ =
  go "let _ = 2 - 1;;";
  [%expect
    {|
    let _ = 2 - 1;;
    (cst
     ((Let (var _)
       (bind
        (Binop (op Sub)
         (lhs (Literal (value (Int 2)) (loc ((line 1) (column 8)))))
         (rhs (Literal (value (Int 1)) (loc ((line 1) (column 12)))))
         (loc ((line 1) (column 10)))))
       (loc ((line 1) (column 0))))))
    |}]
;;

let%expect_test _ =
  go "let _ = a b c d;;";
  [%expect
    {|
    let _ = a b c d;;
    (cst
     ((Let (var _)
       (bind
        (Apply
         (fn
          (Apply
           (fn
            (Apply (fn (Var (id a) (loc ((line 1) (column 8)))))
             (arg (Var (id b) (loc ((line 1) (column 10)))))
             (loc ((line 1) (column 8)))))
           (arg (Var (id c) (loc ((line 1) (column 12)))))
           (loc ((line 1) (column 8)))))
         (arg (Var (id d) (loc ((line 1) (column 14)))))
         (loc ((line 1) (column 8)))))
       (loc ((line 1) (column 0))))))
    |}]
;;

let%expect_test _ =
  go "let _ = x == y == z;;";
  [%expect
    {|
    let _ = x == y == z;;
    (cst
     ((Let (var _)
       (bind
        (Binop (op Eq)
         (lhs
          (Binop (op Eq) (lhs (Var (id x) (loc ((line 1) (column 8)))))
           (rhs (Var (id y) (loc ((line 1) (column 13)))))
           (loc ((line 1) (column 10)))))
         (rhs (Var (id z) (loc ((line 1) (column 18)))))
         (loc ((line 1) (column 15)))))
       (loc ((line 1) (column 0))))))
    |}]
;;

let%expect_test _ =
  go "let _ = a a + b - c * d || e && f g == x;;";
  [%expect
    {|
    let _ = a a + b - c * d || e && f g == x;;
    (cst
     ((Let (var _)
       (bind
        (Binop (op Or)
         (lhs
          (Binop (op Sub)
           (lhs
            (Binop (op Add)
             (lhs
              (Apply (fn (Var (id a) (loc ((line 1) (column 8)))))
               (arg (Var (id a) (loc ((line 1) (column 10)))))
               (loc ((line 1) (column 8)))))
             (rhs (Var (id b) (loc ((line 1) (column 14)))))
             (loc ((line 1) (column 12)))))
           (rhs
            (Binop (op Mul) (lhs (Var (id c) (loc ((line 1) (column 18)))))
             (rhs (Var (id d) (loc ((line 1) (column 22)))))
             (loc ((line 1) (column 20)))))
           (loc ((line 1) (column 16)))))
         (rhs
          (Binop (op And) (lhs (Var (id e) (loc ((line 1) (column 27)))))
           (rhs
            (Binop (op Eq)
             (lhs
              (Apply (fn (Var (id f) (loc ((line 1) (column 32)))))
               (arg (Var (id g) (loc ((line 1) (column 34)))))
               (loc ((line 1) (column 32)))))
             (rhs (Var (id x) (loc ((line 1) (column 39)))))
             (loc ((line 1) (column 36)))))
           (loc ((line 1) (column 29)))))
         (loc ((line 1) (column 24)))))
       (loc ((line 1) (column 0))))))
    |}]
;;

let%expect_test _ =
  go "let _ = let x = false in x;;";
  [%expect
    {|
    let _ = let x = false; x;;
    (cst
     ((Let (var _)
       (bind
        (Let (var x)
         (bind (Literal (value (Bool false)) (loc ((line 1) (column 16)))))
         (rest (Var (id x) (loc ((line 1) (column 25)))))
         (loc ((line 1) (column 8)))))
       (loc ((line 1) (column 0))))))
    |}]
;;

let%expect_test _ =
  go "let _ = fn (x : bool -> (bool -> bool)) -> true false;;";
  [%expect
    {|
    let _ = fn (x : bool -> (bool -> bool)) -> true false;;
    (cst
     ((Let (var _)
       (bind
        (Lambda (arg x) (erased Unerased)
         (arg_mode ((staticity ()) (erasure ())))
         (arg_ty
          (Arrow (arg (Var (id bool) (loc ((line 1) (column 16))))) (arg_id ())
           (arg_mode ((staticity ()) (erasure ())))
           (ret
            (Paren
             (expr
              (Arrow (arg (Var (id bool) (loc ((line 1) (column 25)))))
               (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
               (ret (Var (id bool) (loc ((line 1) (column 33)))))
               (ret_mode ((staticity ()) (erasure ())))
               (loc ((line 1) (column 30)))))
             (loc ((line 1) (column 24)))))
           (ret_mode ((staticity ()) (erasure ()))) (loc ((line 1) (column 21)))))
         (body
          (Apply (fn (Literal (value (Bool true)) (loc ((line 1) (column 43)))))
           (arg (Literal (value (Bool false)) (loc ((line 1) (column 48)))))
           (loc ((line 1) (column 43)))))
         (loc ((line 1) (column 8)))))
       (loc ((line 1) (column 0))))))
    |}]
;;

let%expect_test _ =
  go "let _ = (true);;";
  [%expect
    {|
    let _ = (true);;
    (cst
     ((Let (var _)
       (bind
        (Paren (expr (Literal (value (Bool true)) (loc ((line 1) (column 9)))))
         (loc ((line 1) (column 8)))))
       (loc ((line 1) (column 0))))))
    |}]
;;

let%expect_test _ =
  go "let _ = if true then false else true;;";
  [%expect
    {|
    let _ = if true then false else true;;
    (cst
     ((Let (var _)
       (bind
        (If (cond (Literal (value (Bool true)) (loc ((line 1) (column 11)))))
         (then_ (Literal (value (Bool false)) (loc ((line 1) (column 21)))))
         (else_ (Literal (value (Bool true)) (loc ((line 1) (column 32)))))
         (static false) (loc ((line 1) (column 8)))))
       (loc ((line 1) (column 0))))))
    |}]
;;

let%expect_test _ =
  go "let _ = true false;;";
  [%expect
    {|
    let _ = true false;;
    (cst
     ((Let (var _)
       (bind
        (Apply (fn (Literal (value (Bool true)) (loc ((line 1) (column 8)))))
         (arg (Literal (value (Bool false)) (loc ((line 1) (column 13)))))
         (loc ((line 1) (column 8)))))
       (loc ((line 1) (column 0))))))
    |}]
;;

let%expect_test _ =
  go "let _ = (fn (x : bool) -> (fn (y : bool) -> x) false) true;;";
  [%expect
    {|
    let _ = (fn (x : bool) -> (fn (y : bool) -> x) false) true;;
    (cst
     ((Let (var _)
       (bind
        (Apply
         (fn
          (Paren
           (expr
            (Lambda (arg x) (erased Unerased)
             (arg_mode ((staticity ()) (erasure ())))
             (arg_ty (Var (id bool) (loc ((line 1) (column 17)))))
             (body
              (Apply
               (fn
                (Paren
                 (expr
                  (Lambda (arg y) (erased Unerased)
                   (arg_mode ((staticity ()) (erasure ())))
                   (arg_ty (Var (id bool) (loc ((line 1) (column 35)))))
                   (body (Var (id x) (loc ((line 1) (column 44)))))
                   (loc ((line 1) (column 27)))))
                 (loc ((line 1) (column 26)))))
               (arg (Literal (value (Bool false)) (loc ((line 1) (column 47)))))
               (loc ((line 1) (column 26)))))
             (loc ((line 1) (column 9)))))
           (loc ((line 1) (column 8)))))
         (arg (Literal (value (Bool true)) (loc ((line 1) (column 54)))))
         (loc ((line 1) (column 8)))))
       (loc ((line 1) (column 0))))))
    |}]
;;

let%expect_test _ =
  go "let _ = (fn (x : bool) -> fn (y : bool) -> x) false true;;";
  [%expect
    {|
    let _ = (fn (x : bool) -> fn (y : bool) -> x) false true;;
    (cst
     ((Let (var _)
       (bind
        (Apply
         (fn
          (Apply
           (fn
            (Paren
             (expr
              (Lambda (arg x) (erased Unerased)
               (arg_mode ((staticity ()) (erasure ())))
               (arg_ty (Var (id bool) (loc ((line 1) (column 17)))))
               (body
                (Lambda (arg y) (erased Unerased)
                 (arg_mode ((staticity ()) (erasure ())))
                 (arg_ty (Var (id bool) (loc ((line 1) (column 34)))))
                 (body (Var (id x) (loc ((line 1) (column 43)))))
                 (loc ((line 1) (column 26)))))
               (loc ((line 1) (column 9)))))
             (loc ((line 1) (column 8)))))
           (arg (Literal (value (Bool false)) (loc ((line 1) (column 46)))))
           (loc ((line 1) (column 8)))))
         (arg (Literal (value (Bool true)) (loc ((line 1) (column 52)))))
         (loc ((line 1) (column 8)))))
       (loc ((line 1) (column 0))))))
    |}]
;;

let%expect_test _ =
  go "let _ = (let x = fn (y : bool) -> fn (z : bool) -> y in x true) false;;";
  [%expect
    {|
    let _ = (let x = fn (y : bool) -> fn (z : bool) -> y; x true) false;;
    (cst
     ((Let (var _)
       (bind
        (Apply
         (fn
          (Paren
           (expr
            (Let (var x)
             (bind
              (Lambda (arg y) (erased Unerased)
               (arg_mode ((staticity ()) (erasure ())))
               (arg_ty (Var (id bool) (loc ((line 1) (column 25)))))
               (body
                (Lambda (arg z) (erased Unerased)
                 (arg_mode ((staticity ()) (erasure ())))
                 (arg_ty (Var (id bool) (loc ((line 1) (column 42)))))
                 (body (Var (id y) (loc ((line 1) (column 51)))))
                 (loc ((line 1) (column 34)))))
               (loc ((line 1) (column 17)))))
             (rest
              (Apply (fn (Var (id x) (loc ((line 1) (column 56)))))
               (arg (Literal (value (Bool true)) (loc ((line 1) (column 58)))))
               (loc ((line 1) (column 56)))))
             (loc ((line 1) (column 9)))))
           (loc ((line 1) (column 8)))))
         (arg (Literal (value (Bool false)) (loc ((line 1) (column 64)))))
         (loc ((line 1) (column 8)))))
       (loc ((line 1) (column 0))))))
    |}]
;;

let%expect_test _ =
  go "let _ = let a = (let x = fn (y : bool) -> fn (z : bool) -> y in x true) in a false;;";
  [%expect
    {|
    let _ = let a = (let x = fn (y : bool) -> fn (z : bool) -> y; x true); a false;;
    (cst
     ((Let (var _)
       (bind
        (Let (var a)
         (bind
          (Paren
           (expr
            (Let (var x)
             (bind
              (Lambda (arg y) (erased Unerased)
               (arg_mode ((staticity ()) (erasure ())))
               (arg_ty (Var (id bool) (loc ((line 1) (column 33)))))
               (body
                (Lambda (arg z) (erased Unerased)
                 (arg_mode ((staticity ()) (erasure ())))
                 (arg_ty (Var (id bool) (loc ((line 1) (column 50)))))
                 (body (Var (id y) (loc ((line 1) (column 59)))))
                 (loc ((line 1) (column 42)))))
               (loc ((line 1) (column 25)))))
             (rest
              (Apply (fn (Var (id x) (loc ((line 1) (column 64)))))
               (arg (Literal (value (Bool true)) (loc ((line 1) (column 66)))))
               (loc ((line 1) (column 64)))))
             (loc ((line 1) (column 17)))))
           (loc ((line 1) (column 16)))))
         (rest
          (Apply (fn (Var (id a) (loc ((line 1) (column 75)))))
           (arg (Literal (value (Bool false)) (loc ((line 1) (column 77)))))
           (loc ((line 1) (column 75)))))
         (loc ((line 1) (column 8)))))
       (loc ((line 1) (column 0))))))
    |}]
;;

let%expect_test _ =
  go "let _ = let x = fn (y : bool) -> y in x true;;";
  [%expect
    {|
    let _ = let x = fn (y : bool) -> y; x true;;
    (cst
     ((Let (var _)
       (bind
        (Let (var x)
         (bind
          (Lambda (arg y) (erased Unerased)
           (arg_mode ((staticity ()) (erasure ())))
           (arg_ty (Var (id bool) (loc ((line 1) (column 24)))))
           (body (Var (id y) (loc ((line 1) (column 33)))))
           (loc ((line 1) (column 16)))))
         (rest
          (Apply (fn (Var (id x) (loc ((line 1) (column 38)))))
           (arg (Literal (value (Bool true)) (loc ((line 1) (column 40)))))
           (loc ((line 1) (column 38)))))
         (loc ((line 1) (column 8)))))
       (loc ((line 1) (column 0))))))
    |}]
;;

let%expect_test _ =
  go "let _ = let x = fn (y : lol) -> y in x true;;";
  [%expect
    {|
    let _ = let x = fn (y : lol) -> y; x true;;
    (cst
     ((Let (var _)
       (bind
        (Let (var x)
         (bind
          (Lambda (arg y) (erased Unerased)
           (arg_mode ((staticity ()) (erasure ())))
           (arg_ty (Var (id lol) (loc ((line 1) (column 24)))))
           (body (Var (id y) (loc ((line 1) (column 32)))))
           (loc ((line 1) (column 16)))))
         (rest
          (Apply (fn (Var (id x) (loc ((line 1) (column 37)))))
           (arg (Literal (value (Bool true)) (loc ((line 1) (column 39)))))
           (loc ((line 1) (column 37)))))
         (loc ((line 1) (column 8)))))
       (loc ((line 1) (column 0))))))
    |}]
;;

let%expect_test _ =
  go "let _ = let x ? fn (y : lol) -> y in x true;;";
  [%expect {| ((loc ((line 1) (column 14))) (reason (Unexpected (Unknown ?)))) |}]
;;

let%expect_test _ =
  go
    {|
let a =
    let second = fn (y : bool) -> y in
    second (second (true))
;;|};
  [%expect
    {|
    let a = let second = fn (y : bool) -> y; second (second (true));;
    (cst
     ((Let (var a)
       (bind
        (Let (var second)
         (bind
          (Lambda (arg y) (erased Unerased)
           (arg_mode ((staticity ()) (erasure ())))
           (arg_ty (Var (id bool) (loc ((line 3) (column 25)))))
           (body (Var (id y) (loc ((line 3) (column 34)))))
           (loc ((line 3) (column 17)))))
         (rest
          (Apply (fn (Var (id second) (loc ((line 4) (column 4)))))
           (arg
            (Paren
             (expr
              (Apply (fn (Var (id second) (loc ((line 4) (column 12)))))
               (arg
                (Paren
                 (expr
                  (Literal (value (Bool true)) (loc ((line 4) (column 20)))))
                 (loc ((line 4) (column 19)))))
               (loc ((line 4) (column 12)))))
             (loc ((line 4) (column 11)))))
           (loc ((line 4) (column 4)))))
         (loc ((line 3) (column 4)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test _ =
  go
    {|
let a = (
    let second = fn (y : bool) -> y in
    second (second (true))
);;|};
  [%expect
    {|
    let a = (let second = fn (y : bool) -> y; second (second (true)));;
    (cst
     ((Let (var a)
       (bind
        (Paren
         (expr
          (Let (var second)
           (bind
            (Lambda (arg y) (erased Unerased)
             (arg_mode ((staticity ()) (erasure ())))
             (arg_ty (Var (id bool) (loc ((line 3) (column 25)))))
             (body (Var (id y) (loc ((line 3) (column 34)))))
             (loc ((line 3) (column 17)))))
           (rest
            (Apply (fn (Var (id second) (loc ((line 4) (column 4)))))
             (arg
              (Paren
               (expr
                (Apply (fn (Var (id second) (loc ((line 4) (column 12)))))
                 (arg
                  (Paren
                   (expr
                    (Literal (value (Bool true)) (loc ((line 4) (column 20)))))
                   (loc ((line 4) (column 19)))))
                 (loc ((line 4) (column 12)))))
               (loc ((line 4) (column 11)))))
             (loc ((line 4) (column 4)))))
           (loc ((line 3) (column 4)))))
         (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test _ =
  go
    {|
fun first (x : bool -> bool) : bool = if x then first false else false;;
let _ = first true;;
|};
  [%expect
    {|
    fun first ( x : bool -> bool) : bool = if x then first false else false;;
    let _ = first true;;
    (cst
     ((Fun
       (funs
        (((var first) (arg x) (erased Unerased)
          (arg_mode ((staticity ()) (erasure ())))
          (arg_ty
           (Arrow (arg (Var (id bool) (loc ((line 2) (column 15))))) (arg_id ())
            (arg_mode ((staticity ()) (erasure ())))
            (ret (Var (id bool) (loc ((line 2) (column 23)))))
            (ret_mode ((staticity ()) (erasure ())))
            (loc ((line 2) (column 20)))))
          (ret_mode ((staticity ()) (erasure ())))
          (ret_ty (Var (id bool) (loc ((line 2) (column 31)))))
          (body
           (If (cond (Var (id x) (loc ((line 2) (column 41)))))
            (then_
             (Apply (fn (Var (id first) (loc ((line 2) (column 48)))))
              (arg (Literal (value (Bool false)) (loc ((line 2) (column 54)))))
              (loc ((line 2) (column 48)))))
            (else_ (Literal (value (Bool false)) (loc ((line 2) (column 65)))))
            (static false) (loc ((line 2) (column 38)))))
          (loc ((line 2) (column 4))))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply (fn (Var (id first) (loc ((line 3) (column 8)))))
         (arg (Literal (value (Bool true)) (loc ((line 3) (column 14)))))
         (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test _ =
  go
    {|
fun first (x : bool) : bool -> (bool -> bool) = x;;
let _ = first;;
|};
  [%expect
    {|
    fun first ( x : bool) : bool -> (bool -> bool) = x;;
    let _ = first;;
    (cst
     ((Fun
       (funs
        (((var first) (arg x) (erased Unerased)
          (arg_mode ((staticity ()) (erasure ())))
          (arg_ty (Var (id bool) (loc ((line 2) (column 15)))))
          (ret_mode ((staticity ()) (erasure ())))
          (ret_ty
           (Arrow (arg (Var (id bool) (loc ((line 2) (column 23))))) (arg_id ())
            (arg_mode ((staticity ()) (erasure ())))
            (ret
             (Paren
              (expr
               (Arrow (arg (Var (id bool) (loc ((line 2) (column 32)))))
                (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
                (ret (Var (id bool) (loc ((line 2) (column 40)))))
                (ret_mode ((staticity ()) (erasure ())))
                (loc ((line 2) (column 37)))))
              (loc ((line 2) (column 31)))))
            (ret_mode ((staticity ()) (erasure ())))
            (loc ((line 2) (column 28)))))
          (body (Var (id x) (loc ((line 2) (column 48)))))
          (loc ((line 2) (column 4))))))
       (loc ((line 2) (column 0))))
      (Let (var _) (bind (Var (id first) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test _ =
  go
    {|
fun first (x : bool) : (bool -> bool) -> bool = x;;
let _ = first;;
|};
  [%expect
    {|
    fun first ( x : bool) : (bool -> bool) -> bool = x;;
    let _ = first;;
    (cst
     ((Fun
       (funs
        (((var first) (arg x) (erased Unerased)
          (arg_mode ((staticity ()) (erasure ())))
          (arg_ty (Var (id bool) (loc ((line 2) (column 15)))))
          (ret_mode ((staticity ()) (erasure ())))
          (ret_ty
           (Arrow
            (arg
             (Paren
              (expr
               (Arrow (arg (Var (id bool) (loc ((line 2) (column 24)))))
                (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
                (ret (Var (id bool) (loc ((line 2) (column 32)))))
                (ret_mode ((staticity ()) (erasure ())))
                (loc ((line 2) (column 29)))))
              (loc ((line 2) (column 23)))))
            (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
            (ret (Var (id bool) (loc ((line 2) (column 41)))))
            (ret_mode ((staticity ()) (erasure ())))
            (loc ((line 2) (column 38)))))
          (body (Var (id x) (loc ((line 2) (column 48)))))
          (loc ((line 2) (column 4))))))
       (loc ((line 2) (column 0))))
      (Let (var _) (bind (Var (id first) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test _ =
  go
    {|
fun first (x : bool) : bool -> bool -> bool = x;;
let _ = first;;
|};
  [%expect
    {|
    fun first ( x : bool) : bool -> bool -> bool = x;;
    let _ = first;;
    (cst
     ((Fun
       (funs
        (((var first) (arg x) (erased Unerased)
          (arg_mode ((staticity ()) (erasure ())))
          (arg_ty (Var (id bool) (loc ((line 2) (column 15)))))
          (ret_mode ((staticity ()) (erasure ())))
          (ret_ty
           (Arrow (arg (Var (id bool) (loc ((line 2) (column 23))))) (arg_id ())
            (arg_mode ((staticity ()) (erasure ())))
            (ret
             (Arrow (arg (Var (id bool) (loc ((line 2) (column 31)))))
              (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
              (ret (Var (id bool) (loc ((line 2) (column 39)))))
              (ret_mode ((staticity ()) (erasure ())))
              (loc ((line 2) (column 36)))))
            (ret_mode ((staticity ()) (erasure ())))
            (loc ((line 2) (column 28)))))
          (body (Var (id x) (loc ((line 2) (column 46)))))
          (loc ((line 2) (column 4))))))
       (loc ((line 2) (column 0))))
      (Let (var _) (bind (Var (id first) (loc ((line 3) (column 8)))))
       (loc ((line 3) (column 0))))))
    |}]
;;

let%expect_test _ =
  go
    {|
  let _ =
    fun aux (x : int) : int = (
      x
    ) in
    aux 1
  ;;
|};
  [%expect
    {|
    let _ = fun aux ( x : int) : int = (x); aux 1;;
    (cst
     ((Let (var _)
       (bind
        (Fun
         (funs
          (((var aux) (arg x) (erased Unerased)
            (arg_mode ((staticity ()) (erasure ())))
            (arg_ty (Var (id int) (loc ((line 3) (column 17)))))
            (ret_mode ((staticity ()) (erasure ())))
            (ret_ty (Var (id int) (loc ((line 3) (column 24)))))
            (body
             (Paren (expr (Var (id x) (loc ((line 4) (column 6)))))
              (loc ((line 3) (column 30)))))
            (loc ((line 3) (column 8))))))
         (rest
          (Apply (fn (Var (id aux) (loc ((line 6) (column 4)))))
           (arg (Literal (value (Int 1)) (loc ((line 6) (column 8)))))
           (loc ((line 6) (column 4)))))
         (loc ((line 3) (column 4)))))
       (loc ((line 2) (column 2))))))
    |}]
;;

let%expect_test _ =
  go
    {|
  let _ =
    let capture = 10 in
    fun aux (x : int) : int = (
      if x > 0
      then aux (x - 1)
      else capture
    ) in
    print_int (aux 5)
  ;;
|};
  [%expect
    {|
    let _ = let capture = 10; fun aux ( x : int) : int = (if x > 0 then aux (x - 1) else capture); print_int (aux 5);;
    (cst
     ((Let (var _)
       (bind
        (Let (var capture)
         (bind (Literal (value (Int 10)) (loc ((line 3) (column 18)))))
         (rest
          (Fun
           (funs
            (((var aux) (arg x) (erased Unerased)
              (arg_mode ((staticity ()) (erasure ())))
              (arg_ty (Var (id int) (loc ((line 4) (column 17)))))
              (ret_mode ((staticity ()) (erasure ())))
              (ret_ty (Var (id int) (loc ((line 4) (column 24)))))
              (body
               (Paren
                (expr
                 (If
                  (cond
                   (Binop (op Gt) (lhs (Var (id x) (loc ((line 5) (column 9)))))
                    (rhs (Literal (value (Int 0)) (loc ((line 5) (column 13)))))
                    (loc ((line 5) (column 11)))))
                  (then_
                   (Apply (fn (Var (id aux) (loc ((line 6) (column 11)))))
                    (arg
                     (Paren
                      (expr
                       (Binop (op Sub)
                        (lhs (Var (id x) (loc ((line 6) (column 16)))))
                        (rhs
                         (Literal (value (Int 1)) (loc ((line 6) (column 20)))))
                        (loc ((line 6) (column 18)))))
                      (loc ((line 6) (column 15)))))
                    (loc ((line 6) (column 11)))))
                  (else_ (Var (id capture) (loc ((line 7) (column 11)))))
                  (static false) (loc ((line 5) (column 6)))))
                (loc ((line 4) (column 30)))))
              (loc ((line 4) (column 8))))))
           (rest
            (Apply (fn (Var (id print_int) (loc ((line 9) (column 4)))))
             (arg
              (Paren
               (expr
                (Apply (fn (Var (id aux) (loc ((line 9) (column 15)))))
                 (arg (Literal (value (Int 5)) (loc ((line 9) (column 19)))))
                 (loc ((line 9) (column 15)))))
               (loc ((line 9) (column 14)))))
             (loc ((line 9) (column 4)))))
           (loc ((line 4) (column 4)))))
         (loc ((line 3) (column 4)))))
       (loc ((line 2) (column 2))))))
    |}]
;;

let%expect_test "mutual" =
  go
    {|
fun f (x : int) : int = g x
and g (x : int) : int = f x
;;
let _ = print_int (f 0);;
  |};
  [%expect
    {|
    fun f ( x : int) : int = g x
    and g ( x : int) : int = f x;;
    let _ = print_int (f 0);;
    (cst
     ((Fun
       (funs
        (((var f) (arg x) (erased Unerased)
          (arg_mode ((staticity ()) (erasure ())))
          (arg_ty (Var (id int) (loc ((line 2) (column 11)))))
          (ret_mode ((staticity ()) (erasure ())))
          (ret_ty (Var (id int) (loc ((line 2) (column 18)))))
          (body
           (Apply (fn (Var (id g) (loc ((line 2) (column 24)))))
            (arg (Var (id x) (loc ((line 2) (column 26)))))
            (loc ((line 2) (column 24)))))
          (loc ((line 2) (column 4))))
         ((var g) (arg x) (erased Unerased)
          (arg_mode ((staticity ()) (erasure ())))
          (arg_ty (Var (id int) (loc ((line 3) (column 11)))))
          (ret_mode ((staticity ()) (erasure ())))
          (ret_ty (Var (id int) (loc ((line 3) (column 18)))))
          (body
           (Apply (fn (Var (id f) (loc ((line 3) (column 24)))))
            (arg (Var (id x) (loc ((line 3) (column 26)))))
            (loc ((line 3) (column 24)))))
          (loc ((line 3) (column 4))))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply (fn (Var (id print_int) (loc ((line 5) (column 8)))))
         (arg
          (Paren
           (expr
            (Apply (fn (Var (id f) (loc ((line 5) (column 19)))))
             (arg (Literal (value (Int 0)) (loc ((line 5) (column 21)))))
             (loc ((line 5) (column 19)))))
           (loc ((line 5) (column 18)))))
         (loc ((line 5) (column 8)))))
       (loc ((line 5) (column 0))))))
    |}]
;;

let%expect_test "mutual2" =
  go
    {|
fun f (x : int) : (unit -> int) =
  if x == 0 then (fn (_ : unit) -> 0)
  else (fn (_ : unit) -> 1 + g (x - 1) ())
and g (x : int) : (unit -> int) =
  if x == 0 then (fn (_ : unit) -> 0)
  else (fn (_ : unit) -> 1 + f (x - 1) ())
;;
let _ = print_int (f 10 ());;
  |};
  [%expect
    {|
    fun f ( x : int) : (unit -> int) = if x == 0 then (fn (_ : unit) -> 0) else (fn (_ : unit) -> 1 + g (x - 1) ())
    and g ( x : int) : (unit -> int) = if x == 0 then (fn (_ : unit) -> 0) else (fn (_ : unit) -> 1 + f (x - 1) ());;
    let _ = print_int (f 10 ());;
    (cst
     ((Fun
       (funs
        (((var f) (arg x) (erased Unerased)
          (arg_mode ((staticity ()) (erasure ())))
          (arg_ty (Var (id int) (loc ((line 2) (column 11)))))
          (ret_mode ((staticity ()) (erasure ())))
          (ret_ty
           (Paren
            (expr
             (Arrow (arg (Var (id unit) (loc ((line 2) (column 19)))))
              (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
              (ret (Var (id int) (loc ((line 2) (column 27)))))
              (ret_mode ((staticity ()) (erasure ())))
              (loc ((line 2) (column 24)))))
            (loc ((line 2) (column 18)))))
          (body
           (If
            (cond
             (Binop (op Eq) (lhs (Var (id x) (loc ((line 3) (column 5)))))
              (rhs (Literal (value (Int 0)) (loc ((line 3) (column 10)))))
              (loc ((line 3) (column 7)))))
            (then_
             (Paren
              (expr
               (Lambda (arg _) (erased Unerased)
                (arg_mode ((staticity ()) (erasure ())))
                (arg_ty (Var (id unit) (loc ((line 3) (column 26)))))
                (body (Literal (value (Int 0)) (loc ((line 3) (column 35)))))
                (loc ((line 3) (column 18)))))
              (loc ((line 3) (column 17)))))
            (else_
             (Paren
              (expr
               (Lambda (arg _) (erased Unerased)
                (arg_mode ((staticity ()) (erasure ())))
                (arg_ty (Var (id unit) (loc ((line 4) (column 16)))))
                (body
                 (Binop (op Add)
                  (lhs (Literal (value (Int 1)) (loc ((line 4) (column 25)))))
                  (rhs
                   (Apply
                    (fn
                     (Apply (fn (Var (id g) (loc ((line 4) (column 29)))))
                      (arg
                       (Paren
                        (expr
                         (Binop (op Sub)
                          (lhs (Var (id x) (loc ((line 4) (column 32)))))
                          (rhs
                           (Literal (value (Int 1)) (loc ((line 4) (column 36)))))
                          (loc ((line 4) (column 34)))))
                        (loc ((line 4) (column 31)))))
                      (loc ((line 4) (column 29)))))
                    (arg (Literal (value Unit) (loc ((line 4) (column 39)))))
                    (loc ((line 4) (column 29)))))
                  (loc ((line 4) (column 27)))))
                (loc ((line 4) (column 8)))))
              (loc ((line 4) (column 7)))))
            (static false) (loc ((line 3) (column 2)))))
          (loc ((line 2) (column 4))))
         ((var g) (arg x) (erased Unerased)
          (arg_mode ((staticity ()) (erasure ())))
          (arg_ty (Var (id int) (loc ((line 5) (column 11)))))
          (ret_mode ((staticity ()) (erasure ())))
          (ret_ty
           (Paren
            (expr
             (Arrow (arg (Var (id unit) (loc ((line 5) (column 19)))))
              (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
              (ret (Var (id int) (loc ((line 5) (column 27)))))
              (ret_mode ((staticity ()) (erasure ())))
              (loc ((line 5) (column 24)))))
            (loc ((line 5) (column 18)))))
          (body
           (If
            (cond
             (Binop (op Eq) (lhs (Var (id x) (loc ((line 6) (column 5)))))
              (rhs (Literal (value (Int 0)) (loc ((line 6) (column 10)))))
              (loc ((line 6) (column 7)))))
            (then_
             (Paren
              (expr
               (Lambda (arg _) (erased Unerased)
                (arg_mode ((staticity ()) (erasure ())))
                (arg_ty (Var (id unit) (loc ((line 6) (column 26)))))
                (body (Literal (value (Int 0)) (loc ((line 6) (column 35)))))
                (loc ((line 6) (column 18)))))
              (loc ((line 6) (column 17)))))
            (else_
             (Paren
              (expr
               (Lambda (arg _) (erased Unerased)
                (arg_mode ((staticity ()) (erasure ())))
                (arg_ty (Var (id unit) (loc ((line 7) (column 16)))))
                (body
                 (Binop (op Add)
                  (lhs (Literal (value (Int 1)) (loc ((line 7) (column 25)))))
                  (rhs
                   (Apply
                    (fn
                     (Apply (fn (Var (id f) (loc ((line 7) (column 29)))))
                      (arg
                       (Paren
                        (expr
                         (Binop (op Sub)
                          (lhs (Var (id x) (loc ((line 7) (column 32)))))
                          (rhs
                           (Literal (value (Int 1)) (loc ((line 7) (column 36)))))
                          (loc ((line 7) (column 34)))))
                        (loc ((line 7) (column 31)))))
                      (loc ((line 7) (column 29)))))
                    (arg (Literal (value Unit) (loc ((line 7) (column 39)))))
                    (loc ((line 7) (column 29)))))
                  (loc ((line 7) (column 27)))))
                (loc ((line 7) (column 8)))))
              (loc ((line 7) (column 7)))))
            (static false) (loc ((line 6) (column 2)))))
          (loc ((line 5) (column 4))))))
       (loc ((line 2) (column 0))))
      (Let (var _)
       (bind
        (Apply (fn (Var (id print_int) (loc ((line 9) (column 8)))))
         (arg
          (Paren
           (expr
            (Apply
             (fn
              (Apply (fn (Var (id f) (loc ((line 9) (column 19)))))
               (arg (Literal (value (Int 10)) (loc ((line 9) (column 21)))))
               (loc ((line 9) (column 19)))))
             (arg (Literal (value Unit) (loc ((line 9) (column 24)))))
             (loc ((line 9) (column 19)))))
           (loc ((line 9) (column 18)))))
         (loc ((line 9) (column 8)))))
       (loc ((line 9) (column 0))))))
    |}]
;;

let%expect_test "mutual inner" =
  go
    {|
let _ =
  fun f (x : int) : int = g x
  and g (x : int) : int = f x
  in print_int (f 0)
;;
  |};
  [%expect
    {|
    let _ = fun f ( x : int) : int = g x and g ( x : int) : int = f x; print_int (f 0);;
    (cst
     ((Let (var _)
       (bind
        (Fun
         (funs
          (((var f) (arg x) (erased Unerased)
            (arg_mode ((staticity ()) (erasure ())))
            (arg_ty (Var (id int) (loc ((line 3) (column 13)))))
            (ret_mode ((staticity ()) (erasure ())))
            (ret_ty (Var (id int) (loc ((line 3) (column 20)))))
            (body
             (Apply (fn (Var (id g) (loc ((line 3) (column 26)))))
              (arg (Var (id x) (loc ((line 3) (column 28)))))
              (loc ((line 3) (column 26)))))
            (loc ((line 3) (column 6))))
           ((var g) (arg x) (erased Unerased)
            (arg_mode ((staticity ()) (erasure ())))
            (arg_ty (Var (id int) (loc ((line 4) (column 13)))))
            (ret_mode ((staticity ()) (erasure ())))
            (ret_ty (Var (id int) (loc ((line 4) (column 20)))))
            (body
             (Apply (fn (Var (id f) (loc ((line 4) (column 26)))))
              (arg (Var (id x) (loc ((line 4) (column 28)))))
              (loc ((line 4) (column 26)))))
            (loc ((line 4) (column 6))))))
         (rest
          (Apply (fn (Var (id print_int) (loc ((line 5) (column 5)))))
           (arg
            (Paren
             (expr
              (Apply (fn (Var (id f) (loc ((line 5) (column 16)))))
               (arg (Literal (value (Int 0)) (loc ((line 5) (column 18)))))
               (loc ((line 5) (column 16)))))
             (loc ((line 5) (column 15)))))
           (loc ((line 5) (column 5)))))
         (loc ((line 3) (column 2)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "mutual2 inner" =
  go
    {|
let _ =
  fun f (x : int) : (unit -> int) =
    if x == 0 then (fn (_ : unit) -> 0)
    else (fn (_ : unit) -> 1 + g (x - 1) ())
  and g (x : int) : (unit -> int) =
    if x == 0 then (fn (_ : unit) -> 0)
    else (fn (_ : unit) -> 1 + f (x - 1) ())
  in print_int (f 10 ())
;;
  |};
  [%expect
    {|
    let _ = fun f ( x : int) : (unit -> int) = if x == 0 then (fn (_ : unit) -> 0) else (fn (_ : unit) -> 1 + g (x - 1) ()) and g ( x : int) : (unit -> int) = if x == 0 then (fn (_ : unit) -> 0) else (fn (_ : unit) -> 1 + f (x - 1) ()); print_int (f 10 ());;
    (cst
     ((Let (var _)
       (bind
        (Fun
         (funs
          (((var f) (arg x) (erased Unerased)
            (arg_mode ((staticity ()) (erasure ())))
            (arg_ty (Var (id int) (loc ((line 3) (column 13)))))
            (ret_mode ((staticity ()) (erasure ())))
            (ret_ty
             (Paren
              (expr
               (Arrow (arg (Var (id unit) (loc ((line 3) (column 21)))))
                (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
                (ret (Var (id int) (loc ((line 3) (column 29)))))
                (ret_mode ((staticity ()) (erasure ())))
                (loc ((line 3) (column 26)))))
              (loc ((line 3) (column 20)))))
            (body
             (If
              (cond
               (Binop (op Eq) (lhs (Var (id x) (loc ((line 4) (column 7)))))
                (rhs (Literal (value (Int 0)) (loc ((line 4) (column 12)))))
                (loc ((line 4) (column 9)))))
              (then_
               (Paren
                (expr
                 (Lambda (arg _) (erased Unerased)
                  (arg_mode ((staticity ()) (erasure ())))
                  (arg_ty (Var (id unit) (loc ((line 4) (column 28)))))
                  (body (Literal (value (Int 0)) (loc ((line 4) (column 37)))))
                  (loc ((line 4) (column 20)))))
                (loc ((line 4) (column 19)))))
              (else_
               (Paren
                (expr
                 (Lambda (arg _) (erased Unerased)
                  (arg_mode ((staticity ()) (erasure ())))
                  (arg_ty (Var (id unit) (loc ((line 5) (column 18)))))
                  (body
                   (Binop (op Add)
                    (lhs (Literal (value (Int 1)) (loc ((line 5) (column 27)))))
                    (rhs
                     (Apply
                      (fn
                       (Apply (fn (Var (id g) (loc ((line 5) (column 31)))))
                        (arg
                         (Paren
                          (expr
                           (Binop (op Sub)
                            (lhs (Var (id x) (loc ((line 5) (column 34)))))
                            (rhs
                             (Literal (value (Int 1))
                              (loc ((line 5) (column 38)))))
                            (loc ((line 5) (column 36)))))
                          (loc ((line 5) (column 33)))))
                        (loc ((line 5) (column 31)))))
                      (arg (Literal (value Unit) (loc ((line 5) (column 41)))))
                      (loc ((line 5) (column 31)))))
                    (loc ((line 5) (column 29)))))
                  (loc ((line 5) (column 10)))))
                (loc ((line 5) (column 9)))))
              (static false) (loc ((line 4) (column 4)))))
            (loc ((line 3) (column 6))))
           ((var g) (arg x) (erased Unerased)
            (arg_mode ((staticity ()) (erasure ())))
            (arg_ty (Var (id int) (loc ((line 6) (column 13)))))
            (ret_mode ((staticity ()) (erasure ())))
            (ret_ty
             (Paren
              (expr
               (Arrow (arg (Var (id unit) (loc ((line 6) (column 21)))))
                (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
                (ret (Var (id int) (loc ((line 6) (column 29)))))
                (ret_mode ((staticity ()) (erasure ())))
                (loc ((line 6) (column 26)))))
              (loc ((line 6) (column 20)))))
            (body
             (If
              (cond
               (Binop (op Eq) (lhs (Var (id x) (loc ((line 7) (column 7)))))
                (rhs (Literal (value (Int 0)) (loc ((line 7) (column 12)))))
                (loc ((line 7) (column 9)))))
              (then_
               (Paren
                (expr
                 (Lambda (arg _) (erased Unerased)
                  (arg_mode ((staticity ()) (erasure ())))
                  (arg_ty (Var (id unit) (loc ((line 7) (column 28)))))
                  (body (Literal (value (Int 0)) (loc ((line 7) (column 37)))))
                  (loc ((line 7) (column 20)))))
                (loc ((line 7) (column 19)))))
              (else_
               (Paren
                (expr
                 (Lambda (arg _) (erased Unerased)
                  (arg_mode ((staticity ()) (erasure ())))
                  (arg_ty (Var (id unit) (loc ((line 8) (column 18)))))
                  (body
                   (Binop (op Add)
                    (lhs (Literal (value (Int 1)) (loc ((line 8) (column 27)))))
                    (rhs
                     (Apply
                      (fn
                       (Apply (fn (Var (id f) (loc ((line 8) (column 31)))))
                        (arg
                         (Paren
                          (expr
                           (Binop (op Sub)
                            (lhs (Var (id x) (loc ((line 8) (column 34)))))
                            (rhs
                             (Literal (value (Int 1))
                              (loc ((line 8) (column 38)))))
                            (loc ((line 8) (column 36)))))
                          (loc ((line 8) (column 33)))))
                        (loc ((line 8) (column 31)))))
                      (arg (Literal (value Unit) (loc ((line 8) (column 41)))))
                      (loc ((line 8) (column 31)))))
                    (loc ((line 8) (column 29)))))
                  (loc ((line 8) (column 10)))))
                (loc ((line 8) (column 9)))))
              (static false) (loc ((line 7) (column 4)))))
            (loc ((line 6) (column 6))))))
         (rest
          (Apply (fn (Var (id print_int) (loc ((line 9) (column 5)))))
           (arg
            (Paren
             (expr
              (Apply
               (fn
                (Apply (fn (Var (id f) (loc ((line 9) (column 16)))))
                 (arg (Literal (value (Int 10)) (loc ((line 9) (column 18)))))
                 (loc ((line 9) (column 16)))))
               (arg (Literal (value Unit) (loc ((line 9) (column 21)))))
               (loc ((line 9) (column 16)))))
             (loc ((line 9) (column 15)))))
           (loc ((line 9) (column 5)))))
         (loc ((line 3) (column 2)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "comment" =
  go
    {|
let _ = (* hello *) 0;;
  |};
  [%expect
    {|
    let _ = 0;;
    (cst
     ((Let (var _) (bind (Literal (value (Int 0)) (loc ((line 2) (column 8)))))
       (loc ((line 2) (column 0))))))
    |}]
;;

let%expect_test "comment" =
  go
    {|
(* hello *)
  |};
  [%expect {| (cst ()) |}]
;;

let%expect_test "comment" =
  go
    {|
(* (* *)
  |};
  [%expect {| (cst ()) |}]
;;

let%expect_test "comment" =
  go
    {|
(* (* *) *)
  |};
  [%expect {| ((loc ((line 2) (column 0))) (reason (Unexpected (Op Star)))) |}]
;;

let%expect_test "comment" =
  go
    {|
(* *) fun (* *) erased (* *) f (* *) ((* *) static (* *) erased (* *) x (* *) : (* *) int (* *)) (* *) : (* *) static (* *) erased (* *) int (* *) = (* *) x (* *);;(* *)
  |};
  [%expect {|
    fun f (static erased  x : int) : static erased int = x;;
    (cst
     ((Fun
       (funs
        (((var f) (arg x) (erased Erased)
          (arg_mode ((staticity (Static)) (erasure (Erased))))
          (arg_ty (Var (id int) (loc ((line 2) (column 80)))))
          (ret_mode ((staticity (Static)) (erasure (Erased))))
          (ret_ty (Var (id int) (loc ((line 2) (column 131)))))
          (body (Var (id x) (loc ((line 2) (column 149)))))
          (loc ((line 2) (column 10))))))
       (loc ((line 2) (column 0))))))
    |}]
;;
