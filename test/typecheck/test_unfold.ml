open! Core
open! Syl

let go = Common.typecheck

(* Every [unfold] call site in the checker is pinned by a test in this file:
   removing the unfold makes at least one test here fail. Each program routes a
   reducible-but-stuck type (an application of a family to a symbolic argument
   the family ignores) into exactly one dispatch site. Tests are success-shaped:
   the expected output is empty, and a missing unfold surfaces as an error sexp
   (or an ICE) in its place. *)

(* [typecheck_apply]: the function's type is a stuck application; the unfold
   exposes the arrow so application checking can proceed. *)
let%expect_test "apply through a stuck arrow type" =
  go
    {|
fun arrow_of (static n : int) : erased type = int -> dynamic int;;
fun apply_it (static n : int) : arrow_of n -> dynamic int = fn (g : arrow_of n) -> g 1;;
|};
  [%expect {| |}]
;;

(* [Select]: the selected-from value is a suspended application; the unfold
   exposes the variant so the injection resolves. *)
let%expect_test "select through a stuck variant value" =
  go
    {|
fun variant_of (static n : int) : erased type = variant { mk : int };;
fun f (static n : int) : static int = let erased v = (variant_of n).mk 3 in 0;;
|};
  [%expect {| |}]
;;

(* Dynamic match on a scrutinee whose type is a stuck application: pattern
   checking ([pattern_bindings]'s constructor row) and exhaustiveness
   ([Match.compile]'s root occurrence) both need the variant exposed. *)
let%expect_test "dynamic match on a stuck variant type" =
  go
    {|
fun variant2 (static n : int) : erased type = variant { a, b };;
fun choose (static n : int) : variant2 n -> dynamic int =
  fn (v : variant2 n) -> match v { .a -> 1, .b -> 2 };;
|};
  [%expect {| |}]
;;

(* A stuck tuple type in payload position: [pattern_bindings]'s tuple row
   normalizes the component type, and the emitted bindings project through it
   ([project]'s rows). *)
let%expect_test "tuple pattern under a stuck payload type" =
  go
    {|
fun pair_of (static n : int) : erased type = int ^ bool;;
fun variant3 (static n : int) : erased type = variant { mk : pair_of n };;
fun first (static n : int) : variant3 n -> dynamic int =
  fn (v : variant3 n) -> match v { .mk (x, flag) -> x };;
|};
  [%expect {| |}]
;;

(* A stuck scalar type under a literal pattern: [pattern_bindings]'s literal
   row must normalize before comparing against the literal's type. *)
let%expect_test "literal pattern under a stuck payload type" =
  go
    {|
fun int_of (static n : int) : erased type = int;;
fun variant4 (static n : int) : erased type = variant { mk : int_of n };;
fun test (static n : int) : variant4 n -> dynamic int =
  fn (v : variant4 n) -> match v { .mk 0 -> 1, .mk _ -> 2 };;
|};
  [%expect {| |}]
;;

(* Exhaustiveness over a stuck bool payload: [Match.compile]'s constructor-head
   [deepen] must normalize the payload occurrence for [members] to enumerate
   the domain. *)
let%expect_test "exhaustiveness through a stuck payload type" =
  go
    {|
fun bool_of (static n : int) : erased type = bool;;
fun variant5 (static n : int) : erased type = variant { mk : bool_of n };;
fun test5 (static n : int) : variant5 n -> dynamic int =
  fn (v : variant5 n) -> match v { .mk true -> 1, .mk false -> 2 };;
|};
  [%expect {| |}]
;;

(* A stuck component inside a concrete tuple payload: [Match.compile]'s
   tuple-head [deepen] must normalize element occurrences, and the literal
   sub-pattern exercises the same through [pattern_bindings]. *)
let%expect_test "exhaustiveness through a stuck tuple component" =
  go
    {|
fun bool_of (static n : int) : erased type = bool;;
fun variant6 (static n : int) : erased type = variant { mk : bool_of n ^ int };;
fun test6 (static n : int) : variant6 n -> dynamic int =
  fn (v : variant6 n) -> match v { .mk (true, x) -> x, .mk (false, x) -> x };;
|};
  [%expect {| |}]
;;

(* A stuck variant type in payload position under a nested constructor
   pattern: [pattern_bindings]'s constructor row normalizes the payload type
   it dispatches on. *)
let%expect_test "nested constructor pattern under a stuck payload type" =
  go
    {|
fun variant2 (static n : int) : erased type = variant { a, b };;
fun variant7 (static n : int) : erased type = variant { mk : variant2 n };;
fun nested (static n : int) : variant7 n -> dynamic int =
  fn (v : variant7 n) -> match v { .mk .a -> 1, .mk .b -> 2 };;
|};
  [%expect {| |}]
;;

(* [require_static_type]'s kind check normalizes the type it inspects: a
   value whose type is a stuck application of a type-returning family is
   still a static type. The selection failure pins that checking got past
   the kind check (the var is opaque, so no variant can be exposed). *)
let%expect_test "kind check through a stuck type-of-type" =
  go
    {|
fun ty_of (static n : int) : erased type = type;;
fun g (static n : int) : static erased (ty_of n) \ t -> static int =
  fn (static erased t : ty_of n) -> let erased v = t.mk in 0;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 51)))
     (reason (Expected_variant (got (Var (Anon <opaque>))) (label mk))))
    |}]
;;

(* [leq]'s unfold fallback, right side: a concrete value checks against the
   stuck spelling of its type. *)
let%expect_test "leq unfolds the needed side" =
  go
    {|
fun int_of (static n : int) : erased type = int;;
fun f (static n : int) : static int = (3 : int_of n);;
|};
  [%expect {| |}]
;;

(* [leq]'s unfold fallback, left side: a value at a stuck type is used where
   the reduct is demanded. *)
let%expect_test "leq unfolds the given side" =
  go
    {|
fun int_of (static n : int) : erased type = int;;
fun g (static n : int) : int_of n -> dynamic int = fn (x : int_of n) -> x + 1;;
|};
  [%expect {| |}]
;;

(* The judgment fuel: comparing two isomorphic recursive families at the same
   index would descend through n-1, n-2, ... forever (each level's recursion
   guard unwinds before the next begins). The budget turns the descent into a
   finite conservative rejection. *)
let%expect_test "judgment unfolding gives up on regressive family comparisons" =
  go
    {|
fun vec (static n : int) : erased type =
  match static n { 0 -> variant { nil }, _ -> variant { cons : int ^ vec (n - 1) } };;
fun vec2 (static n : int) : erased type =
  match static n { 0 -> variant { nil }, _ -> variant { cons : int ^ vec2 (n - 1) } };;
fun f (static n : int) : vec n -> static int = fn (v : vec n) -> let _ = (v : vec2 n) in 0;;
|};
  [%expect
    {|
    ((loc ((line 6) (column 76)))
     (reason
      (Gave_up
       (Type_mismatch
        (got
         (Apply
          (fn
           (Binder
            ((arg ((Id n) <opaque>))
             (ty
              (Type
               (Pi (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (ret_ty (T (ty (Type Type)) (memo <opaque>)))
                (ret_mode ((staticity Static) (erasure Erased))))))
             (body_dst
              (Match
               (cond (Var (id ((Id n) <opaque>)) (loc ((line 3) (column 15)))))
               (arms
                (((Literal (value (Int 0)) (loc ((line 3) (column 19))))
                  (Variant (constructors (((label nil) (payload ()))))
                   (loc ((line 3) (column 24)))))
                 ((Var (id (Anon <opaque>)) (loc ((line 3) (column 41))))
                  (Variant
                   (constructors
                    (((label cons)
                      (payload
                       ((Tuple
                         (elts
                          ((Var (id ((Id int) <opaque>))
                            (loc ((line 3) (column 63))))
                           (Apply
                            (fn
                             (Var (id ((Id vec) <opaque>))
                              (loc ((line 3) (column 69)))))
                            (arg
                             (Apply
                              (fn
                               (Var (id ((Binop Sub) <opaque>))
                                (loc ((line 3) (column 76)))))
                              (arg
                               (Make_tuple
                                (elts
                                 ((Var (id ((Id n) <opaque>))
                                   (loc ((line 3) (column 74))))
                                  (Literal (value (Int 1))
                                   (loc ((line 3) (column 78))))))
                                (loc ((line 3) (column 76)))))
                              (loc ((line 3) (column 76)))))
                            (loc ((line 3) (column 69))))))
                         (loc ((line 3) (column 63)))))))))
                   (loc ((line 3) (column 46)))))))
               (eliminator Static) (loc ((line 3) (column 2)))))
             (env <opaque>) (family <opaque>) (uid <opaque>))))
          (arg (Var (Anon <opaque>)))))
        (need
         (Apply
          (fn
           (Binder
            ((arg ((Id n) <opaque>))
             (ty
              (Type
               (Pi (arg_ty (Type Int))
                (arg_mode ((staticity Static) (erasure Unerased)))
                (ret_ty (T (ty (Type Type)) (memo <opaque>)))
                (ret_mode ((staticity Static) (erasure Erased))))))
             (body_dst
              (Match
               (cond (Var (id ((Id n) <opaque>)) (loc ((line 5) (column 15)))))
               (arms
                (((Literal (value (Int 0)) (loc ((line 5) (column 19))))
                  (Variant (constructors (((label nil) (payload ()))))
                   (loc ((line 5) (column 24)))))
                 ((Var (id (Anon <opaque>)) (loc ((line 5) (column 41))))
                  (Variant
                   (constructors
                    (((label cons)
                      (payload
                       ((Tuple
                         (elts
                          ((Var (id ((Id int) <opaque>))
                            (loc ((line 5) (column 63))))
                           (Apply
                            (fn
                             (Var (id ((Id vec2) <opaque>))
                              (loc ((line 5) (column 69)))))
                            (arg
                             (Apply
                              (fn
                               (Var (id ((Binop Sub) <opaque>))
                                (loc ((line 5) (column 77)))))
                              (arg
                               (Make_tuple
                                (elts
                                 ((Var (id ((Id n) <opaque>))
                                   (loc ((line 5) (column 75))))
                                  (Literal (value (Int 1))
                                   (loc ((line 5) (column 79))))))
                                (loc ((line 5) (column 77)))))
                              (loc ((line 5) (column 77)))))
                            (loc ((line 5) (column 69))))))
                         (loc ((line 5) (column 63)))))))))
                   (loc ((line 5) (column 46)))))))
               (eliminator Static) (loc ((line 5) (column 2)))))
             (env <opaque>) (family <opaque>) (uid <opaque>))))
          (arg (Var (Anon <opaque>)))))))))
    |}]
;;

(* [join]'s ordered pick inherits leq's full strength: a dynamic conditional
   joins a stuck spelling of a type with its concrete spelling by unfolding
   inside the pick's comparison. *)
let%expect_test "join picks the ordered side through unfolding" =
  go
    {|
fun variant_of (static n : int) : erased type = variant { a, b };;
fun f (static n : int) : variant_of n -> bool -> dynamic int =
  fn (v : variant_of n) -> fn (c : bool) ->
    let w = if c then v else (variant { a, b }).a in 0;;
|};
  [%expect {| |}]
;;

(* [project_ty] on the resolved-selection path: a static match whose
   scrutinee value selects an arm types the bindings by projecting through
   the scrutinee's ascribed type, which may be spelled stuck at every step —
   the variant itself and the payload tuple both normalize before lookup. *)
let%expect_test "static match bindings project through stuck types" =
  go
    {|
fun pair_of (static n : int) : erased type = int ^ bool;;
fun variant8 (static n : int) : erased type = variant { mk : pair_of n };;
fun test8 (static n : int) : static int =
  let v = (variant8 n).mk (3, true) in
  match static v { .mk (x, flag) -> x };;
|};
  [%expect {| |}]
;;
