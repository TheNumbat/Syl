open! Core
open! Syl

let go = Common.format_round_trip
let cst input = print_s (Parse.parse_exn input |> Cst.Program.strip |> [%sexp_of: Cst.Program.t])

let%expect_test "tokens" =
  Common.tokenize "& && &&& &(t) a&b";
  [%expect
    {|
    (token (Op Amp))
    (token (Op And))
    (token (Op And))
    (token (Op Amp))
    (token (Op Amp))
    (token Lparen)
    (token (Ident t))
    (token Rparen)
    (token (Ident a))
    (token (Op Amp))
    (token (Ident b))
    (token Eof)
    |}]
;;

let%expect_test "ref types" =
  go "let x = &int;;";
  [%expect {| let x = &int;; |}];
  go "let x = &(list t);;";
  [%expect {| let x = &(list t);; |}];
  go "let x = &(&int);;";
  [%expect {| let x = &(&int);; |}];
  go "fun list (erased t : type) : erased type = variant { nil, cons : t ^ &(list t) };;";
  [%expect
    {|
    fun list (erased t : type) : erased type =
      variant {
        nil,
        cons : t ^ &(list t),
      }
    ;;
    |}]
;;

let%expect_test "ref precedence" =
  (* Ref binds below application, like !: &list t is (&list) t. *)
  cst "let x = &list t;;";
  [%expect
    {|
    ((items
      (((node
         (Let (var (Id x)) (erased Unerased)
          (bind
           ((node
             (Apply
              (fn
               ((node
                 (Ref
                  (arg
                   ((node (Var (id (Id list)))) (loc ((line 1) (column 0)))
                    (before ()) (after ())))))
                (loc ((line 1) (column 0))) (before ()) (after ())))
              (arg
               ((node (Var (id (Id t)))) (loc ((line 1) (column 0))) (before ())
                (after ())))))
            (loc ((line 1) (column 0))) (before ()) (after ())))
          (before_erased ()) (after_erased ()) (after_var ())))
        (loc ((line 1) (column 0))) (before ()) (after ()))))
     (after ()))
    |}];
  (* An application spine accepts boxed arguments: f &x is f (&x). *)
  cst "let x = f &y;;";
  [%expect
    {|
    ((items
      (((node
         (Let (var (Id x)) (erased Unerased)
          (bind
           ((node
             (Apply
              (fn
               ((node (Var (id (Id f)))) (loc ((line 1) (column 0))) (before ())
                (after ())))
              (arg
               ((node
                 (Ref
                  (arg
                   ((node (Var (id (Id y)))) (loc ((line 1) (column 0)))
                    (before ()) (after ())))))
                (loc ((line 1) (column 0))) (before ()) (after ())))))
            (loc ((line 1) (column 0))) (before ()) (after ())))
          (before_erased ()) (after_erased ()) (after_var ())))
        (loc ((line 1) (column 0))) (before ()) (after ()))))
     (after ()))
    |}];
  (* Postfix selection stays tightest: &x.y is &(x.y). *)
  cst "let x = &y.fst;;";
  [%expect
    {|
    ((items
      (((node
         (Let (var (Id x)) (erased Unerased)
          (bind
           ((node
             (Ref
              (arg
               ((node
                 (Select
                  (expr
                   ((node (Var (id (Id y)))) (loc ((line 1) (column 0)))
                    (before ()) (after ())))
                  (label fst)))
                (loc ((line 1) (column 0))) (before ()) (after ())))))
            (loc ((line 1) (column 0))) (before ()) (after ())))
          (before_erased ()) (after_erased ()) (after_var ())))
        (loc ((line 1) (column 0))) (before ()) (after ()))))
     (after ()))
    |}];
  (* Arrow is looser: & a -> b is (&a) -> b. *)
  cst "let x = &a -> b;;";
  [%expect
    {|
    ((items
      (((node
         (Let (var (Id x)) (erased Unerased)
          (bind
           ((node
             (Arrow
              (arg
               ((node
                 (Ref
                  (arg
                   ((node (Var (id (Id a)))) (loc ((line 1) (column 0)))
                    (before ()) (after ())))))
                (loc ((line 1) (column 0))) (before ()) (after ())))
              (arg_id ()) (arg_mode ((staticity ()) (erasure ())))
              (ret
               ((node (Var (id (Id b)))) (loc ((line 1) (column 0))) (before ())
                (after ())))
              (ret_mode ((staticity ()) (erasure ())))))
            (loc ((line 1) (column 0))) (before ()) (after ())))
          (before_erased ()) (after_erased ()) (after_var ())))
        (loc ((line 1) (column 0))) (before ()) (after ()))))
     (after ()))
    |}]
;;

let%expect_test "double ref needs parens" =
  (* No whitespace escape hatch: & &t does not parse; write &(&t). *)
  go "let x = & &t;;";
  [%expect {| ((loc ((line 1) (column 10))) (reason (Unexpected (Op Amp)))) |}]
;;

let%expect_test "amp on terms still parses" =
  (* [&expr] is reserved for address-of once lifetimes exist; it parses today
     and the typechecker rejects non-type arguments. *)
  go "let x = &5;;";
  [%expect {| let x = &5;; |}];
  go "let x = f &y &z;;";
  [%expect {| let x = f &y &z;; |}];
  go "let x = (1, &y);;";
  [%expect {| let x = (1, &y);; |}]
;;

let%expect_test "box tokens" =
  Common.tokenize "box boxed box(x)";
  [%expect
    {|
    (token Box)
    (token (Ident boxed))
    (token Box)
    (token Lparen)
    (token (Ident x))
    (token Rparen)
    (token Eof)
    |}]
;;

let%expect_test "box terms" =
  go "let x = box 5;;";
  [%expect {| let x = box 5;; |}];
  go "let x = box (box 5);;";
  [%expect {| let x = box (box 5);; |}];
  go "let x = f (box y) (box z);;";
  [%expect {| let x = f (box y) (box z);; |}];
  go "let x = (1, box y);;";
  [%expect {| let x = (1, box y);; |}]
;;

let%expect_test "box precedence" =
  (* The operand is one atom, like assert's: [box x + 1] is [(box x) + 1] and
     a box in an application spine takes the next atom. *)
  cst "let x = box y + 1;;";
  [%expect
    {|
    ((items
      (((node
         (Let (var (Id x)) (erased Unerased)
          (bind
           ((node
             (Binop (op Add)
              (lhs
               ((node
                 (Box
                  (arg
                   ((node (Var (id (Id y)))) (loc ((line 1) (column 0)))
                    (before ()) (after ())))))
                (loc ((line 1) (column 0))) (before ()) (after ())))
              (rhs
               ((node (Literal (value (Int 1)))) (loc ((line 1) (column 0)))
                (before ()) (after ())))))
            (loc ((line 1) (column 0))) (before ()) (after ())))
          (before_erased ()) (after_erased ()) (after_var ())))
        (loc ((line 1) (column 0))) (before ()) (after ()))))
     (after ()))
    |}];
  cst "let x = f box y;;";
  [%expect
    {|
    ((items
      (((node
         (Let (var (Id x)) (erased Unerased)
          (bind
           ((node
             (Apply
              (fn
               ((node (Var (id (Id f)))) (loc ((line 1) (column 0))) (before ())
                (after ())))
              (arg
               ((node
                 (Box
                  (arg
                   ((node (Var (id (Id y)))) (loc ((line 1) (column 0)))
                    (before ()) (after ())))))
                (loc ((line 1) (column 0))) (before ()) (after ())))))
            (loc ((line 1) (column 0))) (before ()) (after ())))
          (before_erased ()) (after_erased ()) (after_var ())))
        (loc ((line 1) (column 0))) (before ()) (after ()))))
     (after ()))
    |}]
;;

let%expect_test "ref patterns" =
  go "let f = match l { .nil -> 0, .cons (h, &t) -> h };;";
  [%expect
    {|
    let f =
      match l {
        .nil -> 0,
        .cons (h, &t) -> h,
      }
    ;;
    |}];
  go "let f = match l { &x -> x };;";
  [%expect {| let f = match l { &x -> x };; |}];
  go "let f = match l { &(x | y) -> x };;";
  [%expect {| let f = match l { &(x | y) -> x };; |}];
  go "let f = match l { &(&x) -> x };;";
  [%expect {| let f = match l { &(&x) -> x };; |}];
  go "let f = match l { .cons &t -> t, _ -> l };;";
  [%expect
    {|
    let f =
      match l {
        .cons &t -> t,
        _ -> l,
      }
    ;;
    |}]
;;

let%expect_test "ref pattern cst" =
  cst "let f = match l { &(a, b) -> a };;";
  [%expect
    {|
    ((items
      (((node
         (Let (var (Id f)) (erased Unerased)
          (bind
           ((node
             (Match
              (cond
               ((node (Var (id (Id l)))) (loc ((line 1) (column 0))) (before ())
                (after ())))
              (arms
               ((((node
                   (Ref
                    (payload
                     ((node
                       (Tuple
                        (elts
                         (((node (Var (id (Id a)))) (loc ((line 1) (column 0)))
                           (before ()) (after ()))
                          ((node (Var (id (Id b)))) (loc ((line 1) (column 0)))
                           (before ()) (after ()))))))
                      (loc ((line 1) (column 0))) (before ()) (after ())))))
                  (loc ((line 1) (column 0))) (before ()) (after ()))
                 ((node (Var (id (Id a)))) (loc ((line 1) (column 0)))
                  (before ()) (after ())))))
              (eliminator Dynamic) (before_elimination ())))
            (loc ((line 1) (column 0))) (before ()) (after ())))
          (before_erased ()) (after_erased ()) (after_var ())))
        (loc ((line 1) (column 0))) (before ()) (after ()))))
     (after ()))
    |}]
;;
