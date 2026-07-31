open! Core
open! Syl

let fmt = Common.format_round_trip

let%expect_test "simple let" =
  fmt "let x = 42;;";
  [%expect {| let x = 42;; |}]
;;

let%expect_test "fun" =
  fmt "fun f (x : int) : int = x;;";
  [%expect
    {|
    fun f (x : int) : int =
      x
    ;;
    |}]
;;

let%expect_test "mutual fun" =
  fmt
    {|
fun g (x : int) : int = h x
and h (y : int) : int = g y;;
|};
  [%expect
    {|
    fun g (x : int) : int =
      h x
    and h (y : int) : int =
      g y
    ;;
    |}]
;;

let%expect_test "if-then-else short" =
  fmt "let _ = if true then 1 else 0;;";
  [%expect {| let _ = if true then 1 else 0;; |}]
;;

let%expect_test "if-then-else breaks" =
  fmt ~width:30 "let _ = if true then 1 else 0;;";
  [%expect
    {|
    let _ =
      if true then 1 else 0
    ;;
    |}]
;;

let%expect_test "nested if" =
  fmt ~width:40 "let _ = if a then 1 else if b then 2 else 3;;";
  [%expect
    {|
    let _ =
      if a
      then 1
      else if b
           then 2
           else 3
    ;;
    |}]
;;

let%expect_test "nested if" =
  fmt ~width:40 "let _ = if a then if c then 4 else 5 else if b then 2 else 3;;";
  [%expect
    {|
    let _ =
      if a
      then if c
           then 4
           else 5
      else if b
           then 2
           else 3
    ;;
    |}]
;;

let%expect_test "nested if" =
  fmt ~width:40 "let _ = if a then (if c then 4 else 5) else (if b then 2 else 3);;";
  [%expect
    {|
    let _ =
      if a
      then (if c then 4 else 5)
      else (if b then 2 else 3)
    ;;
    |}]
;;

let%expect_test "nested if" =
  fmt ~width:20 "let _ = if a then (if c then 4 else 5) else (if b then 2 else 3);;";
  [%expect
    {|
    let _ =
      if a
      then
        (if c
         then 4
         else 5)
      else
        (if b
         then 2
         else 3)
    ;;
    |}]
;;

let%expect_test "lambda" =
  fmt "let _ = fn (x : int) -> x + 1;;";
  [%expect {| let _ = fn (x : int) -> x + 1;; |}]
;;

let%expect_test "lambda breaks" =
  fmt ~width:30 "let _ = fn (x : int) -> x + 1;;";
  [%expect
    {|
    let _ =
      fn (x : int) -> x + 1
    ;;
    |}]
;;

let%expect_test "application" =
  fmt "let _ = f x y z;;";
  [%expect {| let _ = f x y z;; |}]
;;

let%expect_test "let-in chain" =
  fmt "let _ = let x = 1 in let y = 2 in x + y;;";
  [%expect {| let _ = let x = 1 in let y = 2 in x + y;; |}]
;;

let%expect_test "let-in chain breaks" =
  fmt ~width:40 "let _ = let x = 1 in let y = 2 in x + y;;";
  [%expect
    {|
    let _ =
      let x = 1 in let y = 2 in x + y
    ;;
    |}]
;;

let%expect_test "binop" =
  fmt "let _ = 1 + 2 * 3;;";
  [%expect {| let _ = 1 + 2 * 3;; |}]
;;

let%expect_test "tuple types" =
  fmt "let t = int ^ bool ^ unit;;";
  [%expect {| let t = int ^ bool ^ unit;; |}]
;;

let%expect_test "comma" =
  fmt "let t = 1, 2, 3;;";
  [%expect {| let t = 1, 2, 3;; |}]
;;

let%expect_test "arrow" =
  fmt "let _ = int -> bool -> unit;;";
  [%expect {| let _ = int -> bool -> unit;; |}]
;;

let%expect_test "dependent arrow" =
  fmt "let _ = static erased type \\ t -> t -> t;;";
  [%expect {| let _ = static erased type \ t -> t -> t;; |}]
;;

let%expect_test "type annotation" =
  fmt "let _ = (1 + 2) : int;;";
  [%expect {| let _ = (1 + 2) : int;; |}]
;;

let%expect_test "mode annotation" =
  fmt "let _ = 1 @ static;;";
  [%expect {| let _ = 1 @ static;; |}]
;;

let%expect_test "assert" =
  fmt "let _ = assert true;;";
  [%expect {| let _ = assert true;; |}]
;;

let%expect_test "assert erased" =
  fmt "let _ = assert erased true;;";
  [%expect {| let _ = assert erased true;; |}]
;;

let%expect_test "external" =
  fmt "external print_int : int -> unit = syl_std_print_int;;";
  [%expect {| external print_int : int -> unit = syl_std_print_int;; |}]
;;

let%expect_test "builtin" =
  fmt "builtin unit = syl_unit_t;;";
  [%expect {| builtin unit = syl_unit_t;; |}]
;;

let%expect_test "comment before decl" =
  fmt "(* hello *) let x = 0;;";
  [%expect
    {|
    (* hello *)
    let x = 0;;
    |}]
;;

let%expect_test "consecutive comments" =
  fmt "(* a *) (* b *) (* c *) let x = 0;;";
  [%expect
    {|
    (* a *)

    (* b *)

    (* c *)
    let x = 0;;
    |}]
;;

let%expect_test "comment before expr" =
  fmt "let _ = (* hi *) 0;;";
  [%expect {| let _ = (* hi *) 0;; |}]
;;

let%expect_test "trailing comment" =
  fmt
    {|
let x = 0;;
(* trailing *)
  |};
  [%expect
    {|
    let x = 0;;

    (* trailing *)
    |}]
;;

let%expect_test "multiple decls" =
  fmt
    {|
let x = 1;;
let y = 2;;
let z = 3;;
  |};
  [%expect
    {|
    let x = 1;;

    let y = 2;;

    let z = 3;;
    |}]
;;

let%expect_test "fun breaks long body" =
  fmt
    ~width:40
    {|
fun collatz (x : int) : int =
  if x == 1
  then 0
  else if x % 2 == 0
       then 1 + collatz (x / 2)
       else 1 + collatz (3 * x + 1)
;;
|};
  [%expect
    {|
    fun collatz (x : int) : int =
      if x == 1
      then 0
      else if x % 2 == 0
           then 1 + collatz (x / 2)
           else 1 + collatz (3 * x + 1)
    ;;
    |}]
;;

let%expect_test "fn erased" =
  fmt "let _ = fn erased (x : int) -> x;;";
  [%expect {| let _ = fn erased (x : int) -> x;; |}]
;;

let%expect_test "let erased" =
  fmt "let erased f = fn (x : int) -> x;;";
  [%expect {| let erased f = fn (x : int) -> x;; |}]
;;

let%expect_test "modes on arg" =
  fmt "fun id (static erased t : type) : t -> t = fn (x : t) -> x;;";
  [%expect
    {|
    fun id (static erased t : type) : t -> t =
      fn (x : t) -> x
    ;;
    |}]
;;

let%expect_test "local fun" =
  fmt "let _ = fun f (x : int) : int = x + 1 in f 0;;";
  [%expect
    {|
    let _ =
      fun f (x : int) : int =
        x + 1
      in
      f 0
    ;;
    |}]
;;

let%expect_test "unreachable" =
  fmt "let _ = unreachable;;";
  [%expect {| let _ = unreachable;; |}]
;;

let%expect_test "comment after lhs of binop" =
  fmt "let _ = 1 (* asdf *) + 2 ;;";
  [%expect {| let _ = 1 (* asdf *) + 2;; |}]
;;

let%expect_test "comment before rhs of binop" =
  fmt "let _ = 1 + (* asdf *) 2 ;;";
  [%expect {| let _ = 1 + (* asdf *) 2;; |}]
;;

let%expect_test "comment after fn arg" =
  fmt "let _ = f (* c *) x;;";
  [%expect {| let _ = f (* c *) x;; |}]
;;

let%expect_test "comment before fn arg" =
  fmt "let _ = f (* c *) x;;";
  [%expect {| let _ = f (* c *) x;; |}]
;;

let%expect_test "comment in if" =
  fmt "let _ = if (* a *) true then (* b *) 1 else (* c *) 0;;";
  [%expect {| let _ = if (* a *) true then (* b *) 1 else (* c *) 0;; |}]
;;

let%expect_test "comment in let bind and rest" =
  fmt "let _ = let x = (* a *) 1 in (* b *) x;;";
  [%expect {| let _ = let x = (* a *) 1 in (* b *) x;; |}]
;;

let%expect_test "comment in lambda body" =
  fmt "let _ = fn (x : int) -> (* a *) x;;";
  [%expect {| let _ = fn (x : int) -> (* a *) x;; |}]
;;

let%expect_test "comment in paren" =
  fmt "let _ = ((* a *) 1 + 2);;";
  [%expect {| let _ = ((* a *) 1 + 2);; |}]
;;

let%expect_test "comment between decls" =
  fmt
    {|
let x = 1;;
(* between *)
let y = 2;;
|};
  [%expect
    {|
    let x = 1;;

    (* between *)
    let y = 2;;
    |}]
;;

let%expect_test "comment in type annotation" =
  fmt "let _ = (* a *) 1 : (* b *) int;;";
  [%expect {| let _ = (* a *) 1 : (* b *) int;; |}]
;;

let%expect_test "comment in mode annotation" =
  fmt "let _ = (* a *) 1 @ static;;";
  [%expect {| let _ = (* a *) 1 @ static;; |}]
;;

let%expect_test "comment after caret" =
  fmt "let _ = int (* a *) ^ bool;;";
  [%expect {| let _ = int (* a *) ^ bool;; |}]
;;

let%expect_test "comment in comma" =
  fmt "let _ = 1 (* a *), 2 (* b *), 3;;";
  [%expect {| let _ = 1 (* a *), 2 (* b *), 3;; |}]
;;

let%expect_test "comment before colon" =
  fmt "let _ = x (* c *) : int;;";
  [%expect {| let _ = x (* c *) : int;; |}]
;;

let%expect_test "comment before @" =
  fmt "let _ = x (* c *) @ static;;";
  [%expect {| let _ = x (* c *) @ static;; |}]
;;

let%expect_test "comment before arrow" =
  fmt "let _ = a (* c *) -> b;;";
  [%expect {| let _ = a (* c *) -> b;; |}]
;;

let%expect_test "comment before backslash" =
  fmt "let _ = int (* c *) \\ x -> x;;";
  [%expect {| let _ = int (* c *) \ x -> x;; |}]
;;

let%expect_test "comment between app args" =
  fmt "let _ = f a (* c *) b;;";
  [%expect {| let _ = f a (* c *) b;; |}]
;;

let%expect_test "comment after last app arg" =
  fmt "let _ = f a b (* c *);;";
  [%expect {| let _ = f a b (* c *);; |}]
;;

let%expect_test "comment after fn arg" =
  fmt "let _ = fn (x : int) (* c *) -> x + 1;;";
  [%expect {| let _ = fn (x : int) (* c *) -> x + 1;; |}]
;;

let%expect_test "comment after fun arg" =
  fmt "fun f (x : int) (* c *) : int = x + 1;;";
  [%expect
    {|
    fun f (x : int) (* c *) : int =
      x + 1
    ;;
    |}]
;;

let%expect_test "comment before match pattern" =
  fmt "let _ = match x { (* c *) a -> 1, b -> 2 };;";
  [%expect
    {|
    let _ =
      match x {
        (* c *) a -> 1,
        b -> 2,
      }
    ;;
    |}]
;;

let%expect_test "comment before second match pattern" =
  fmt "let _ = match x { a -> 1, (* c *) b -> 2 };;";
  [%expect
    {|
    let _ =
      match x {
        a -> 1,
        (* c *) b -> 2,
      }
    ;;
    |}]
;;

let%expect_test "comment before mutual fun" =
  fmt "fun f (x : int) : int = g x and (* c *) g (y : int) : int = f y;;";
  [%expect
    {|
    fun f (x : int) : int =
      g x
    and (* c *) g (y : int) : int =
      f y
    ;;
    |}]
;;

let%expect_test "comment before local mutual fun" =
  fmt "let _ = fun f (x : int) : int = g x and (* c *) g (y : int) : int = f y in f 0;;";
  [%expect
    {|
    let _ =
      fun f (x : int) : int =
        g x
      and (* c *) g (y : int) : int =
        f y
      in
      f 0
    ;;
    |}]
;;

let%expect_test "comment before match pattern in static match" =
  fmt "let _ = match static x { (* c *) a -> 1, b -> 2 };;";
  [%expect
    {|
    let _ =
      match static x {
        (* c *) a -> 1,
        b -> 2,
      }
    ;;
    |}]
;;

let%expect_test "fn arg comment before paren" =
  fmt "let _ = fn (* c *) (x : int) -> x;;";
  [%expect {| let _ = fn (* c *) (x : int) -> x;; |}]
;;

let%expect_test "fn arg comment after mode" =
  fmt "let _ = fn (static (* c *) x : int) -> x;;";
  [%expect {| let _ = fn (static (* c *) x : int) -> x;; |}]
;;

let%expect_test "fn arg comment after ident" =
  fmt "let _ = fn (x (* c *) : int) -> x;;";
  [%expect {| let _ = fn (x (* c *) : int) -> x;; |}]
;;

let%expect_test "fn arg comment before type" =
  fmt "let _ = fn (x : (* c *) int) -> x;;";
  [%expect {| let _ = fn (x : (* c *) int) -> x;; |}]
;;

let%expect_test "fn arg comment after type" =
  fmt "let _ = fn (x : int (* c *)) -> x;;";
  [%expect {| let _ = fn (x : int (* c *)) -> x;; |}]
;;

let%expect_test "fun arg comment before paren" =
  fmt "fun f (* c *) (x : int) : int = x;;";
  [%expect
    {|
    fun f (* c *) (x : int) : int =
      x
    ;;
    |}]
;;

let%expect_test "fun arg comment after mode" =
  fmt "fun f (static (* c *) x : int) : int = x;;";
  [%expect
    {|
    fun f (static (* c *) x : int) : int =
      x
    ;;
    |}]
;;

let%expect_test "fun arg comment after ident" =
  fmt "fun f (x (* c *) : int) : int = x;;";
  [%expect
    {|
    fun f (x (* c *) : int) : int =
      x
    ;;
    |}]
;;

let%expect_test "fun arg comment before type" =
  fmt "fun f (x : (* c *) int) : int = x;;";
  [%expect
    {|
    fun f (x : (* c *) int) : int =
      x
    ;;
    |}]
;;

let%expect_test "fun arg comment after type" =
  fmt "fun f (x : int (* c *)) : int = x;;";
  [%expect
    {|
    fun f (x : int (* c *)) : int =
      x
    ;;
    |}]
;;

let%expect_test "comment before match cond" =
  fmt "let _ = match (* c *) x { a -> 1 };;";
  [%expect {| let _ = match (* c *) x { a -> 1 };; |}]
;;

let%expect_test "comment after match cond" =
  fmt "let _ = match x (* c *) { a -> 1 };;";
  [%expect {| let _ = match x (* c *) { a -> 1 };; |}]
;;

let%expect_test "comment in match arm body" =
  fmt "let _ = match x { a -> (* c *) 1, b -> 2 };;";
  [%expect
    {|
    let _ =
      match x {
        a -> (* c *) 1,
        b -> 2,
      }
    ;;
    |}]
;;

let%expect_test "comment after match arm body" =
  fmt "let _ = match x { a -> 1 (* c *), b -> 2 };;";
  [%expect
    {|
    let _ =
      match x {
        a -> 1 (* c *),
        b -> 2,
      }
    ;;
    |}]
;;

let%expect_test "comment in every match position" =
  fmt
    "let _ = match (* a *) x (* b *) { (* c *) a -> (* d *) 1 (* e *), (* f *) b -> (* g *) 2 (* h \
     *) };;";
  [%expect
    {|
    let _ =
      match (* a *) x (* b *) {
        (* c *) a -> (* d *) 1 (* e *),
        (* f *) b -> (* g *) 2 (* h *),
      }
    ;;
    |}]
;;

let%expect_test "comment before first pipe in match" =
  fmt "let _ = match x { (* c *)  a -> 1, b -> 2 };;";
  [%expect
    {|
    let _ =
      match x {
        (* c *) a -> 1,
        b -> 2,
      }
    ;;
    |}]
;;

let%expect_test "match arm: comment after pattern before arrow" =
  fmt "let _ = match x { a (* c *) -> 1, b -> 2 };;";
  [%expect
    {|
    let _ =
      match x {
        a (* c *) -> 1,
        b -> 2,
      }
    ;;
    |}]
;;

let%expect_test "fun: comment after args before colon" =
  fmt "fun f (x : int) (* c *) : int = x;;";
  [%expect
    {|
    fun f (x : int) (* c *) : int =
      x
    ;;
    |}]
;;

let%expect_test "fun: comment after erased before name" =
  fmt "fun erased (* c *) f (x : int) : int = x;;";
  [%expect
    {|
    fun erased (* c *) f (x : int) : int =
      x
    ;;
    |}]
;;

let%expect_test "let expr: comment before erased" =
  fmt "let _ = let (* c *) erased x = 1 in x;;";
  [%expect {| let _ = let (* c *) erased x = 1 in x;; |}]
;;

let%expect_test "let expr: comment after erased before var" =
  fmt "let _ = let erased (* c *) x = 1 in x;;";
  [%expect {| let _ = let erased (* c *) x = 1 in x;; |}]
;;

let%expect_test "let expr: comment after var before equals" =
  fmt "let _ = let x (* c *) = 1 in x;;";
  [%expect {| let _ = let x (* c *) = 1 in x;; |}]
;;

let%expect_test "fn: comment before erased" =
  fmt "let _ = fn (* c *) erased (x : int) -> x;;";
  [%expect {| let _ = fn (* c *) erased (x : int) -> x;; |}]
;;

let%expect_test "fn: comment after args before arrow" =
  fmt "let _ = fn (x : int) (* c *) -> x;;";
  [%expect {| let _ = fn (x : int) (* c *) -> x;; |}]
;;

let%expect_test "if: comment before erased" =
  fmt "let _ = if (* c *) erased true then 1 else 0;;";
  [%expect {| let _ = if (* c *) erased true then 1 else 0;; |}]
;;

let%expect_test "match: comment before static" =
  fmt "let _ = match (* c *) static x { a -> 1 };;";
  [%expect {| let _ = match (* c *) static x { a -> 1 };; |}]
;;

let%expect_test "match: comment before erased" =
  fmt "let _ = match (* c *) erased x { a -> 1 };;";
  [%expect {| let _ = match (* c *) erased x { a -> 1 };; |}]
;;

let%expect_test "assert: comment before erased" =
  fmt "let _ = assert (* c *) erased true;;";
  [%expect {| let _ = assert (* c *) erased true;; |}]
;;

let%expect_test "assert: comment after erased" =
  fmt "let _ = assert erased (* c *) true;;";
  [%expect {| let _ = assert erased (* c *) true;; |}]
;;

let%expect_test "top-level let: comment before erased" =
  fmt "let (* c *) erased x = 1;;";
  [%expect {| let (* c *) erased x = 1;; |}]
;;

let%expect_test "application breaks" =
  fmt ~width:15 "let _ = f aaa bbb ccc;;";
  [%expect
    {|
    let _ =
      f aaa bbb ccc
    ;;
    |}]
;;

let%expect_test "application many args breaks" =
  fmt ~width:10 "let _ = f a b c d;;";
  [%expect
    {|
    let _ =
      f
        a
        b
        c
        d
    ;;
    |}]
;;

let%expect_test "binop breaks" =
  fmt ~width:15 "let _ = a + b + c;;";
  [%expect
    {|
    let _ =
      a + b + c
    ;;
    |}]
;;

let%expect_test "chained binop breaks" =
  fmt ~width:20 "let _ = aaa + bbb + ccc + ddd;;";
  [%expect
    {|
    let _ =
      aaa + bbb + ccc
      + ddd
    ;;
    |}]
;;

let%expect_test "arrow breaks" =
  fmt ~width:20 "let _ = int -> bool -> unit;;";
  [%expect
    {|
    let _ =
      int
        -> bool
        -> unit
    ;;
    |}]
;;

let%expect_test "comma breaks" =
  fmt ~width:10 "let _ = aaa, bbb, ccc;;";
  [%expect
    {|
    let _ =
      aaa,
      bbb,
      ccc
    ;;
    |}]
;;

let%expect_test "caret breaks" =
  fmt ~width:10 "let _ = int ^ bool ^ unit;;";
  [%expect
    {|
    let _ =
      int
      ^ bool
      ^ unit
    ;;
    |}]
;;

let%expect_test "paren breaks" =
  fmt ~width:10 "let _ = (aaa + bbb + ccc);;";
  [%expect
    {|
    let _ =
      (aaa
       + bbb
       + ccc)
    ;;
    |}]
;;

let%expect_test "type annotation breaks" =
  fmt ~width:20 "let _ = some_expr : some_type;;";
  [%expect
    {|
    let _ =
      some_expr
        : some_type
    ;;
    |}]
;;

let%expect_test "let-in fully breaks" =
  fmt ~width:20 "let _ = let x = 1 in let y = 2 in x + y;;";
  [%expect
    {|
    let _ =
      let x = 1 in
      let y = 2 in x + y
    ;;
    |}]
;;

let%expect_test "lambda breaks body" =
  fmt ~width:25 "let _ = fn (x : int) -> x + 1 + 2;;";
  [%expect
    {|
    let _ =
      fn (x : int) ->
        x + 1 + 2
    ;;
    |}]
;;

let%expect_test "lambda long arg breaks" =
  fmt ~width:25 "let _ = fn (some_long_name : int) -> some_long_name + 1;;";
  [%expect
    {|
    let _ =
      fn
        (some_long_name : int) ->
        some_long_name + 1
    ;;
    |}]
;;

let%expect_test "top-level let ;; on new line" =
  fmt ~width:20 "let some_long_name = some_long_value;;";
  [%expect
    {|
    let some_long_name =
      some_long_value
    ;;
    |}]
;;

let%expect_test "fun long arg breaks" =
  fmt ~width:30 "fun f (some_long_arg_name : int) : int = some_long_arg_name + 1;;";
  [%expect
    {|
    fun f
      (some_long_arg_name : int) : int =
      some_long_arg_name + 1
    ;;
    |}]
;;

let%expect_test "if-else-if narrow" =
  fmt ~width:20 "let _ = if a then 1 else if b then 2 else 3;;";
  [%expect
    {|
    let _ =
      if a
      then 1
      else if b
           then 2
           else 3
    ;;
    |}]
;;

let%expect_test "if-else-if-else-if chain" =
  fmt ~width:30 "let _ = if a then 1 else if b then 2 else if c then 3 else 4;;";
  [%expect
    {|
    let _ =
      if a
      then 1
      else if b
           then 2
           else if c
                then 3
                else 4
    ;;
    |}]
;;

let%expect_test "if in then branch" =
  fmt ~width:25 "let _ = if a then if b then 1 else 2 else 3;;";
  [%expect
    {|
    let _ =
      if a
      then if b
           then 1
           else 2
      else 3
    ;;
    |}]
;;

let%expect_test "if in else branch (not chained)" =
  fmt ~width:25 "let _ = if a then 1 else (if b then 2 else 3);;";
  [%expect
    {|
    let _ =
      if a
      then 1
      else
        (if b then 2 else 3)
    ;;
    |}]
;;

let%expect_test "deeply nested else-if" =
  fmt ~width:20 "let _ = if a then 1 else if b then 2 else if c then 3 else if d then 4 else 5;;";
  [%expect
    {|
    let _ =
      if a
      then 1
      else if b
           then 2
           else if c
                then 3
                else if d
                     then
                       4
                     else
                       5
    ;;
    |}]
;;

let%expect_test "if with long cond breaks" =
  fmt ~width:20 "let _ = if long_condition_here then 1 else 0;;";
  [%expect
    {|
    let _ =
      if long_condition_here
      then 1
      else 0
    ;;
    |}]
;;

let%expect_test "if with long then and else" =
  fmt ~width:20 "let _ = if a then long_result_here else other_long_result;;";
  [%expect
    {|
    let _ =
      if a
      then
        long_result_here
      else
        other_long_result
    ;;
    |}]
;;

let%expect_test "deeply nested application" =
  fmt ~width:20 "let _ = f (g (h x));;";
  [%expect
    {|
    let _ =
      f (g (h x))
    ;;
    |}]
;;

let%expect_test "dependent arrow breaks" =
  fmt ~width:25 "let _ = static erased type \\ t -> t -> t;;";
  [%expect
    {|
    let _ =
      static erased type \ t
        -> t
        -> t
    ;;
    |}]
;;

let%expect_test "assert breaks" =
  fmt ~width:20 "let _ = assert (1 + 2 + 3 == 6);;";
  [%expect
    {|
    let _ =
      assert
        (1 + 2 + 3 == 6)
    ;;
    |}]
;;

let%expect_test "assert breaks" =
  fmt ~width:10 "let _ = assert (1 + 2 + 3 == 6);;";
  [%expect
    {|
    let _ =
      assert
        (1 + 2
         + 3
         == 6)
    ;;
    |}]
;;

let%expect_test "assert erased breaks" =
  fmt ~width:20 "let _ = assert erased (1 + 2 + 3 == 6);;";
  [%expect
    {|
    let _ =
      assert erased
        (1 + 2 + 3 == 6)
    ;;
    |}]
;;

let%expect_test "assert erased breaks" =
  fmt ~width:10 "let _ = assert erased (1 + 2 + 3 == 6);;";
  [%expect
    {|
    let _ =
      assert erased
        (1 + 2
         + 3
         == 6)
    ;;
    |}]
;;

let%expect_test "mode annotation breaks" =
  fmt ~width:15 "let _ = some_expr @ static;;";
  [%expect
    {|
    let _ =
      some_expr @ static
    ;;
    |}]
;;

let%expect_test "if breaks cond and then" =
  fmt ~width:15 "let _ = if condition then result else other;;";
  [%expect
    {|
    let _ =
      if condition
      then result
      else other
    ;;
    |}]
;;

let%expect_test "external breaks" =
  fmt ~width:30 "external some_long_name : int -> bool -> unit = some_c_symbol;;";
  [%expect
    {|
    external some_long_name : int
      -> bool
      -> unit = some_c_symbol;;
    |}]
;;

let%expect_test "local fun breaks" =
  fmt ~width:25 "let _ = fun f (x : int) : int = x + 1 in f 0;;";
  [%expect
    {|
    let _ =
      fun f (x : int) : int =
        x + 1
      in
      f 0
    ;;
    |}]
;;

let%expect_test "mutual fun breaks" =
  fmt
    ~width:25
    {|
fun even (n : int) : bool = if n == 0 then true else odd (n - 1)
and odd (n : int) : bool = if n == 0 then false else even (n - 1);;
|};
  [%expect
    {|
    fun even
      (n : int) : bool =
      if n == 0
      then true
      else odd (n - 1)
    and odd
      (n : int) : bool =
      if n == 0
      then false
      else even (n - 1)
    ;;
    |}]
;;

let%expect_test "nested binop all breaks" =
  fmt ~width:10 "let _ = a + b * c - d;;";
  [%expect
    {|
    let _ =
      a
      + b * c
      - d
    ;;
    |}]
;;

let%expect_test "match arm bodies break" =
  fmt ~width:15 "let _ = match x { a -> a + 1 + 2 + 3, b -> b * c * d };;";
  [%expect
    {|
    let _ =
      match x {
        a ->
          a + 1 + 2
          + 3,
        b ->
          b * c
          * d,
      }
    ;;
    |}]
;;

let%expect_test "match complex cond breaks" =
  fmt ~width:15 "let _ = match f x y { a -> 1, b -> 2 };;";
  [%expect
    {|
    let _ =
      match f x y {
        a -> 1,
        b -> 2,
      }
    ;;
    |}]
;;

let%expect_test "match static narrow" =
  fmt ~width:15 "let _ = match static cond { a -> 1, b -> 2 };;";
  [%expect
    {|
    let _ =
      match static cond {
        a -> 1,
        b -> 2,
      }
    ;;
    |}]
;;

let%expect_test "match erased narrow" =
  fmt ~width:15 "let _ = match erased cond { a -> 1, b -> 2 };;";
  [%expect
    {|
    let _ =
      match erased cond {
        a -> 1,
        b -> 2,
      }
    ;;
    |}]
;;

let%expect_test "match nested in if" =
  fmt ~width:20 "let _ = if true then match x { a -> 1, b -> 2 } else 0;;";
  [%expect
    {|
    let _ =
      if true
      then
        match x {
          a -> 1,
          b -> 2,
        }
      else 0
    ;;
    |}]
;;

let%expect_test "nested matches need no parens" =
  fmt "let _ = match x { a -> match y { b -> 1 } };;";
  [%expect {| let _ = match x { a -> match y { b -> 1 } };; |}];
  fmt "let _ = match x { a -> match y { 0 -> 1, _ -> 2 }, b -> 3 };;";
  [%expect
    {|
    let _ =
      match x {
        a ->
          match y {
            0 -> 1,
            _ -> 2,
          },
        b -> 3,
      }
    ;;
    |}]
;;

let%expect_test "match in parens narrow" =
  fmt ~width:15 "let _ = f (match x { a -> 1, b -> 2 });;";
  [%expect
    {|
    let _ =
      f
        (match x {
           a -> 1,
           b -> 2,
         })
    ;;
    |}]
;;

let%expect_test "match arm with lambda" =
  fmt ~width:20 "let _ = match x { a -> fn (y : int) -> y + a };;";
  [%expect
    {|
    let _ =
      match x {
        a ->
          fn
            (y : int) ->
            y + a,
      }
    ;;
    |}]
;;

let%expect_test "match arm with let" =
  fmt ~width:20 "let _ = match x { a -> let r = a + 1 in r, b -> b };;";
  [%expect
    {|
    let _ =
      match x {
        a ->
          let r =
            a + 1
          in
          r,
        b -> b,
      }
    ;;
    |}]
;;

let%expect_test "match literal int patterns" =
  fmt "let _ = match x { 0 -> 1, 1 -> 2, n -> n };;";
  [%expect
    {|
    let _ =
      match x {
        0 -> 1,
        1 -> 2,
        n -> n,
      }
    ;;
    |}]
;;

let%expect_test "match literal bool patterns" =
  fmt "let _ = match x { true -> 1, false -> 0 };;";
  [%expect
    {|
    let _ =
      match x {
        true -> 1,
        false -> 0,
      }
    ;;
    |}]
;;

let%expect_test "match literal unit pattern" =
  fmt "let _ = match x { () -> 0 };;";
  [%expect {| let _ = match x { () -> 0 };; |}]
;;

let%expect_test "match tuple pattern unparenthesized" =
  fmt "let _ = match x { ( a, b) -> a };;";
  [%expect {| let _ = match x { (a, b) -> a };; |}]
;;

let%expect_test "match tuple pattern with parens drops them" =
  fmt "let _ = match x { (a, b) -> a };;";
  [%expect {| let _ = match x { (a, b) -> a };; |}]
;;

let%expect_test "match nested tuple pattern keeps inner parens" =
  fmt "let _ = match x { ( a, (b, c)) -> b };;";
  [%expect {| let _ = match x { (a, (b, c)) -> b };; |}]
;;

let%expect_test "match tuple pattern with literals" =
  fmt "let _ = match x { ( 0, true) -> 1, (n, b) -> n };;";
  [%expect
    {|
    let _ =
      match x {
        (0, true) -> 1,
        (n, b) -> n,
      }
    ;;
    |}]
;;

let%expect_test "match tuple pattern wide breaks" =
  fmt ~width:20 "let _ = match x { ( aaa, bbb, ccc, ddd) -> 1 };;";
  [%expect
    {|
    let _ =
      match x {
        (aaa,
         bbb,
         ccc,
         ddd) ->
          1,
      }
    ;;
    |}]
;;

let%expect_test "comment before literal pattern" =
  fmt "let _ = match x { (* c *) 0 -> 1, n -> n };;";
  [%expect
    {|
    let _ =
      match x {
        (* c *) 0 -> 1,
        n -> n,
      }
    ;;
    |}]
;;

let%expect_test "comment after tuple pattern" =
  fmt "let _ = match x { ( a, b (* c *)) -> a };;";
  [%expect {| let _ = match x { (a, b) (* c *) -> a };; |}]
;;

let%expect_test "match or pattern simple" =
  fmt "let _ = match x { 0 | 1 -> true, n -> false };;";
  [%expect
    {|
    let _ =
      match x {
        0 | 1 -> true,
        n -> false,
      }
    ;;
    |}]
;;

let%expect_test "match or pattern with three branches" =
  fmt "let _ = match x { 0 | 1 | 2 -> true, n -> false };;";
  [%expect
    {|
    let _ =
      match x {
        0 | 1 | 2 -> true,
        n -> false,
      }
    ;;
    |}]
;;

let%expect_test "match or pattern with tuple branches" =
  fmt "let _ = match x { ( 0, 1 | 1, 0) -> true, (a, b) -> false };;";
  [%expect
    {|
    let _ =
      match x {
        (0, 1) | (1, 0) -> true,
        (a, b) -> false,
      }
    ;;
    |}]
;;

let%expect_test "tuple with or pattern element parenthesizes" =
  fmt "let _ = match x { ( (0 | 1), b) -> b };;";
  [%expect {| let _ = match x { ((0 | 1), b) -> b };; |}]
;;

let%expect_test "or pattern wide breaks" =
  fmt ~width:20 "let _ = match x { aaa | bbb | ccc | ddd -> 1 };;";
  [%expect
    {|
    let _ =
      match x {
        aaa | bbb | ccc
        | ddd ->
          1,
      }
    ;;
    |}]
;;

let%expect_test "nested or pattern in parens" =
  fmt "let _ = match x { ( a, (b | c)) -> a };;";
  [%expect {| let _ = match x { (a, (b | c)) -> a };; |}]
;;
