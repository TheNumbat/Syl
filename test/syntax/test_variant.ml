open! Core
open! Syl

let go = Common.format_round_trip
let cst input = print_s (Parse.parse_exn input |> Cst.Program.strip |> [%sexp_of: Cst.Program.t])

let%expect_test "tokens" =
  Common.tokenize "variant { none, some : t } .some x.fst x .fst";
  [%expect
    {|
    (token Variant)
    (token Lbrace)
    (token (Ident none))
    (token (Op Comma))
    (token (Ident some))
    (token Colon)
    (token (Ident t))
    (token Rbrace)
    (token (Label some))
    (token (Ident x))
    (token (Label fst))
    (token (Ident x))
    (token (Label fst))
    (token Eof)
    |}]
;;

let%expect_test "variant definitions" =
  go "let option = fn (erased t : type) -> variant { none, some : t };;";
  [%expect
    {|
    let option =
      fn (erased t : type) ->
        variant {
          none,
          some : t,
        }
    ;;
    |}];
  go "let empty = variant { empty };;";
  [%expect {| let empty = variant { empty };; |}];
  go "fun list (erased t : type) : erased type = variant { nil, cons : t ^ list t };;";
  [%expect
    {|
    fun list (erased t : type) : erased type =
      variant {
        nil,
        cons : t ^ list t,
      }
    ;;
    |}]
;;

let%expect_test "trailing comma" =
  go "let option = variant { none, some : t, };;";
  [%expect
    {|
    let option =
      variant {
        none,
        some : t,
      }
    ;;
    |}]
;;

let%expect_test "variant in if branches" =
  go
    "fun vec (erased t : type) : erased type = if erased true then variant { empty } else variant \
     { more : t ^ vec t };;";
  [%expect
    {|
    fun vec (erased t : type) : erased type =
      if erased true then variant { empty } else variant { more : t ^ vec t }
    ;;
    |}]
;;

let%expect_test "variant nests in match arms without parens" =
  go "let f = match n { 0 -> variant { empty }, _ -> variant { more : t } };;";
  [%expect
    {|
    let f =
      match n {
        0 -> variant { empty },
        _ -> variant { more : t },
      }
    ;;
    |}]
;;

let%expect_test "variant breaks across lines" =
  go
    ~width:40
    "let shape = variant { circle : int, rectangle : int ^ int, triangle : int ^ int ^ int };;";
  [%expect
    {|
    let shape =
      variant {
        circle : int,
        rectangle : int ^ int,
        triangle : int ^ int ^ int,
      }
    ;;
    |}]
;;

let%expect_test "arrow payload needs no parens" =
  go "let wrap = variant { wrap : int -> bool };;";
  [%expect {| let wrap = variant { wrap : int -> bool };; |}];
  go "let wrap = variant { wrap : static erased type \\ x -> x -> x };;";
  [%expect {| let wrap = variant { wrap : static erased type \ x -> x -> x };; |}]
;;

let%expect_test "constructor expressions" =
  go "let xs = .cons (1, .cons (2, .nil));;";
  [%expect {| let xs = .cons (1, .cons (2, .nil));; |}];
  go "let x = .some 1;;";
  [%expect {| let x = .some 1;; |}];
  go "let y = f (.none) (.some 2);;";
  [%expect {| let y = f (.none) (.some 2);; |}]
;;

let%expect_test "qualified constructors via selection" =
  go "let x = (option int).some 1;;";
  [%expect {| let x = (option int).some 1;; |}];
  go "let xs = (list int).cons (1, (list int).nil);;";
  [%expect {| let xs = (list int).cons (1, (list int).nil);; |}];
  go "let x = v.fst.snd;;";
  [%expect {| let x = v.fst.snd;; |}]
;;

let%expect_test "spacing does not affect the dot" =
  go "let a = f x.lbl;;";
  [%expect {| let a = f x.lbl;; |}];
  (* Postfix dot is always selection; the formatter normalizes stray spacing. *)
  go "let b = f x .lbl;;";
  [%expect {| let b = f x.lbl;; |}]
;;

let%expect_test "injection arguments take parens" =
  go "let a = f (.none);;";
  [%expect {| let a = f (.none);; |}];
  (* Bare, the dot selects from f instead. *)
  go "let b = f .none;;";
  [%expect {| let b = f.none;; |}]
;;

let%expect_test "constructor patterns" =
  go "let len = match xs { .nil -> 0, .cons (_, tl) -> 1 + len tl };;";
  [%expect
    {|
    let len =
      match xs {
        .nil -> 0,
        .cons (_, tl) -> 1 + len tl,
      }
    ;;
    |}];
  go "let _ = match x { .none | .some _ -> 0 };;";
  [%expect {| let _ = match x { .none | .some _ -> 0 };; |}];
  go "let _ = match x { .some (.some y) -> y, .some .none -> 0, .none -> 1 };;";
  [%expect
    {|
    let _ =
      match x {
        .some (.some y) -> y,
        .some .none -> 0,
        .none -> 1,
      }
    ;;
    |}]
;;

let%expect_test "bare tuple arm patterns need parens" =
  go "let _ = match x { (.none, y) -> y, (.some z, _) -> z };;";
  [%expect
    {|
    let _ =
      match x {
        (.none, y) -> y,
        (.some z, _) -> z,
      }
    ;;
    |}];
  go "let _ = match x { .none, y -> y };;";
  [%expect {| ((loc ((line 1) (column 23))) (reason (Unexpected (Op Comma)))) |}]
;;

let%expect_test "case is not semantic" =
  go "let Foo = 1;;";
  [%expect {| let Foo = 1;; |}];
  go "let _ = variant { Weird, ok : int };;";
  [%expect
    {|
    let _ =
      variant {
        Weird,
        ok : int,
      }
    ;;
    |}]
;;

let%expect_test "empty variant and empty match are rejected" =
  go "let _ = variant {};;";
  [%expect {| ((loc ((line 1) (column 17))) (reason (Unexpected Rbrace))) |}];
  go "let _ = match x { };;";
  [%expect {| ((loc ((line 1) (column 18))) (reason (Unexpected Rbrace))) |}]
;;

let%expect_test "comments survive" =
  go "let option = variant { (* none *) none, some (* some *) : (* payload *) t };;";
  [%expect
    {|
    let option =
      variant {
        (* none *)
        none,
        some (* some *) : (* payload *) t,
      }
    ;;
    |}]
;;

let%expect_test "selection binds inside prefix operators" =
  go "let a = !x.lbl;;";
  [%expect {| let a = !x.lbl;; |}];
  go "let a = (!x).lbl;;";
  [%expect {| let a = (!x).lbl;; |}];
  go "let a = assert x.lbl;;";
  [%expect {| let a = assert x.lbl;; |}];
  cst "let a = !x.lbl;;";
  [%expect
    {|
    ((items
      (((node
         (Let (var (Id a)) (erased Unerased)
          (bind
           ((node
             (Unop (op Not)
              (arg
               ((node
                 (Select
                  (expr
                   ((node (Var (id (Id x)))) (loc ((line 1) (column 0)))
                    (before ()) (after ())))
                  (label lbl)))
                (loc ((line 1) (column 0))) (before ()) (after ())))))
            (loc ((line 1) (column 0))) (before ()) (after ())))
          (before_erased ()) (after_erased ()) (after_var ())))
        (loc ((line 1) (column 0))) (before ()) (after ()))))
     (after ()))
    |}]
;;

let%expect_test "selection binds off braces" =
  go "let x = variant { a }.b;;";
  [%expect {| let x = variant { a }.b;; |}];
  go "let y = match v { p -> p }.fst;;";
  [%expect {| let y = match v { p -> p }.fst;; |}]
;;

let%expect_test "comments do not affect the dot" =
  Common.tokenize "x.y x (* c *).y x(* c *).y";
  [%expect
    {|
    (token (Ident x))
    (token (Label y))
    (token (Ident x))
    (token (Label y))
    (token (Ident x))
    (token (Label y))
    (token Eof)
    |}]
;;

let%expect_test "dot without a label is unknown" =
  Common.tokenize "x.5 .";
  [%expect
    {|
    (token (Ident x))
    (token (Unknown .))
    (token (Int 5))
    (token (Unknown .))
    (token Eof)
    |}];
  go "let _ = x.5;;";
  [%expect {| ((loc ((line 1) (column 9))) (reason (Unexpected (Unknown .)))) |}]
;;
