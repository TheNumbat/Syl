open! Core
open! Syl
open Tst

(* Values reduce on construction; these tests pin down how the smart
   constructors propagate [Bottom]: an operation on an unreachable operand is
   itself unreachable. *)

let show value = print_s [%sexp (value : Value.t)]
let int i = Int.const (Int64.of_int i)

let%expect_test "int ops propagate an unreachable operand" =
  show (Int.add Value.bottom (int 1));
  [%expect {| Bottom |}];
  show (Int.sub (int 1) Value.bottom);
  [%expect {| Bottom |}];
  show (Int.mul Value.bottom (int 0));
  [%expect {| Bottom |}];
  show (Int.neg Value.bottom);
  [%expect {| Bottom |}]
;;

let%expect_test "an unreachable division never reports" =
  show (Int.div Value.bottom (int 0));
  [%expect {| Bottom |}];
  show (Int.div (int 1) Value.bottom);
  [%expect {| Bottom |}];
  show (Int.mod_ Value.bottom (int 0));
  [%expect {| Bottom |}];
  show (Int.mod_ Value.bottom (int (-1)));
  [%expect {| Bottom |}]
;;

let%expect_test "live divisor checks still report" =
  (try show (Int.div (int 1) (int 0)) with
   | Int.Divide_by_zero i -> print_s [%sexp (i : Int.t)]);
  [%expect {| (Div (Int (T 1)) (Int (T 0))) |}];
  (try show (Int.mod_ (int 1) (int (-1))) with
   | Int.Negative_modulus i -> print_s [%sexp (i : Int.t)]);
  [%expect {| (Mod (Int (T 1)) (Int (T -1))) |}]
;;

(* [Bottom] wins over the short-circuit rewrites: the operation is dead code,
   not false/true. *)
let%expect_test "bool ops propagate an unreachable operand" =
  show (Bool.and_ Value.bottom (Bool.const false));
  [%expect {| Bottom |}];
  show (Bool.or_ (Bool.const true) Value.bottom);
  [%expect {| Bottom |}];
  show (Bool.eq Value.bottom (int 0));
  [%expect {| Bottom |}];
  show (Bool.not_ Value.bottom);
  [%expect {| Bottom |}]
;;

let%expect_test "composite values with an unreachable part are unreachable" =
  show (Value.tuple [ int 1; Value.bottom ]);
  [%expect {| Bottom |}];
  show (Value.proj Value.bottom 0);
  [%expect {| Bottom |}];
  show (Value.apply ~fn:Value.bottom ~arg:(int 1));
  [%expect {| Bottom |}];
  show (Value.apply ~fn:(int 1) ~arg:Value.bottom);
  [%expect {| Bottom |}]
;;

let%expect_test "an unreachable assertion never reports" =
  show (Builtin.eval Builtin.Prim.Assert Value.bottom);
  [%expect {| Bottom |}];
  show (Builtin.eval Builtin.Prim.Assert_erased Value.bottom);
  [%expect {| Bottom |}]
;;

(* Exhaustive arms selecting one leaf everywhere make the conditional that
   leaf; the collapse keeps conditional representatives from nesting. *)
let%expect_test "a conditional with identical leaves is constant" =
  let cond = Value.var (Ident.create Ident.Raw.anon ~stamp:0) in
  show (Value.if_ ~cond ~then_:(int 5) ~else_:(int 5));
  [%expect {| (Int (T 5)) |}]
;;
