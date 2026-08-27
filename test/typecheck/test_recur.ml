open! Core
open! Syl

let go = Common.typecheck

let%expect_test "pi function calling arrow function in same group" =
  go
    {|
fun inc (x : int) : int = x + 1
and f (static erased t : type) : t -> t = fn (x : t) -> x;;
let _ = f int 0;;
|};
  [%expect {| |}]
;;

let%expect_test "arrow function calling pi function in same group" =
  go
    {|
fun f (static erased t : type) : t -> t = fn (x : t) -> x
and g (x : int) : int = f int x;;
let _ = g 5;;
|};
  [%expect {| |}]
;;

let%expect_test "pi calling arrow from same group at application" =
  go
    {|
fun inc (x : int) : int = x + 1
and choose (static erased b : bool) : int -> int =
  if erased b then fn (x : int) -> inc x else fn (x : int) -> x;;
let _ = choose true 5;;
let _ = choose false 5;;
|};
  [%expect {| |}]
;;

let%expect_test "mutual pi recursion" =
  go
    {|
fun f (static erased t : type) : t -> t = fn (x : t) -> x
and g (static erased t : type) : t -> t = f t;;
let _ = g int 0;;
|};
  [%expect {| |}]
;;

let%expect_test "static recursion with base case" =
  go
    {|
fun f (static x : int) : int = if erased x == 0 then 42 else f (x - 1);;
let _ = f 3;;
|};
  [%expect {| |}]
;;

let%expect_test "recursive erased application" =
  go
    {|
fun f (static x : int) : erased int = (if erased x == 0 then 42 else f (x - 1)) @ erased;;
let _ = f 3;;
|};
  [%expect {| |}]
;;

let%expect_test "arrow and pi mutual recursion with application" =
  go
    {|
fun double (x : int) : int = x + x
and apply_double (static erased t : type) : int -> int = fn (x : int) -> double x;;
let _ = apply_double int 5;;
|};
  [%expect {| |}]
;;

let%expect_test "mutually recursive fun with static arg" =
  go
    {|
fun id1 (static erased t : type) : t -> t = fn (x : t) -> x
and id2 (static erased t : type) : t -> t = id1 t;;
|};
  [%expect {| |}]
;;

let%expect_test "same-key static self-recursion hits the limit" =
  go
    {|
fun f (static x : int) : int = f x;;
let _ = f 0;;
|};
  [%expect {| ((loc ((line 2) (column 31))) (reason (Recursion_limit 1000))) |}]
;;

let%expect_test "same-key erased self-recursion is a ghost until demanded" =
  (* The recursion guard folds the self-application; an erased value that is
     never demanded never unfolds. *)
  go
    {|
fun f (static x : int) : erased int = f x;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "demanding a terminal self-recursion hits the limit" =
  go
    {|
fun f (static x : int) : erased int = f x;;
let _ = match erased (f 0) { 0 -> print_int 0, _ -> print_int 1 };;
|};
  [%expect {| ((loc ((line 2) (column 38))) (reason (Recursion_limit 1000))) |}]
;;

let%expect_test "recursive erased application" =
  go
    {|
fun f (x : int) : erased int = f x;;
let _ = f 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 31)))
     (reason
      (Erased_application
       (fn
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret_ty (Type Int))
          (ret_mode ((staticity Static) (erasure Erased))))))
       (result ((staticity Parametric) (erasure Erased))))))
    |}]
;;

let%expect_test "recursive erased application" =
  go
    {|
fun f (x : int) : int = g x
and erased g (y : int) : int = y;;
let _ = f 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 24)))
     (reason
      (Erased_application
       (fn
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret_ty (Type Int))
          (ret_mode ((staticity Static) (erasure Unerased))))))
       (result ((staticity Parametric) (erasure Unerased))))))
    |}]
;;

let%expect_test "recursive erased application" =
  go
    {|
fun f (x : int) : int = (g @ erased) x
and g (y : int) : int = y;;
let _ = f 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 24)))
     (reason
      (Erased_application
       (fn
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret_ty (Type Int))
          (ret_mode ((staticity Static) (erasure Unerased))))))
       (result ((staticity Parametric) (erasure Unerased))))))
    |}]
;;

let%expect_test "recursive erased application" =
  go
    {|
fun f (x : int) : erased int = g x
and g (y : int) : erased int = y;;
let _ = f 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 31)))
     (reason
      (Erased_application
       (fn
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret_ty (Type Int))
          (ret_mode ((staticity Static) (erasure Erased))))))
       (result ((staticity Parametric) (erasure Erased))))))
    |}]
;;

let%expect_test "recursive erased application" =
  go
    {|
fun f (static x : int) : erased int = g x
and g (static y : int) : erased int = y;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "recursive erased application" =
  go
    {|
fun f (static x : int) : erased int = g x
and g (y : int) : erased int = y;;
let _ = f 0;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 4)))
     (reason
      (Mode_mismatch (got ((staticity Parametric) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "recursive erased application" =
  go
    {|
fun erased f (x : int) : int = (g @ erased) x
and g (y : int) : int = y;;
let _ = f 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 31)))
     (reason
      (Erased_application
       (fn
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret_ty (Type Int))
          (ret_mode ((staticity Static) (erasure Unerased))))))
       (result ((staticity Parametric) (erasure Unerased))))))
    |}]
;;

let%expect_test "recursive erased application" =
  go
    {|
fun f (x : int) : int = g x
and g (y : int) : int = f y;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "recursive erased application" =
  go
    {|
fun erased f (x : int) : int = g x
and erased g (y : int) : int = f y;;
let _ = f 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 31)))
     (reason
      (Erased_application
       (fn
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret_ty (Type Int))
          (ret_mode ((staticity Static) (erasure Unerased))))))
       (result ((staticity Parametric) (erasure Unerased))))))
    |}]
;;

let%expect_test "recursive erased application" =
  go
    {|
fun f (x : int) : int = g x
and erased g (y : int) : int = f y;;
let _ = (f @ erased) 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 24)))
     (reason
      (Erased_application
       (fn
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret_ty (Type Int))
          (ret_mode ((staticity Static) (erasure Unerased))))))
       (result ((staticity Parametric) (erasure Unerased))))))
    |}]
;;

let%expect_test "recursive erased application" =
  go
    {|
fun f (x : int) : int = g x
and g (y : int) : int = (f @ erased) y;;
let _ = f 0;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 24)))
     (reason
      (Erased_application
       (fn
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret_ty (Type Int))
          (ret_mode ((staticity Static) (erasure Unerased))))))
       (result ((staticity Parametric) (erasure Unerased))))))
    |}]
;;

let%expect_test "recursive erased application" =
  go
    {|
fun f (x : int) : int = (g @ erased) x
and g (y : int) : int = (f @ erased) y;;
let _ = f 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 24)))
     (reason
      (Erased_application
       (fn
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret_ty (Type Int))
          (ret_mode ((staticity Static) (erasure Unerased))))))
       (result ((staticity Parametric) (erasure Unerased))))))
    |}]
;;

let%expect_test "recursive erased application" =
  go
    {|
fun f (x : int) : int = (g @ erased) x
and g (y : int) : int = (f @ erased) y;;
let _ = f 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 24)))
     (reason
      (Erased_application
       (fn
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret_ty (Type Int))
          (ret_mode ((staticity Static) (erasure Unerased))))))
       (result ((staticity Parametric) (erasure Unerased))))))
    |}]
;;

let%expect_test "recursive erased application" =
  go
    {|
fun f (x : int) : erased int = g x @ erased
and g (y : int) : erased int = (f @ erased) y;;
let _ = f 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 31)))
     (reason
      (Erased_application
       (fn
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret_ty (Type Int))
          (ret_mode ((staticity Static) (erasure Erased))))))
       (result ((staticity Parametric) (erasure Erased))))))
    |}]
;;

let%expect_test "recursive erased application" =
  go
    {|
fun f (x : int) : erased int = g x
and g (y : int) : int = let _ = f y in 0;;
let _ = g 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 4)))
     (reason
      (Mode_mismatch (got ((staticity Parametric) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

(* [h]'s static argument demands the static value of [f 0] while [f] is still
   being defined; the in-flight static is treated as an abstract var, which is
   unconstrained here and silently accepted (see TODO.md: good error
   messages). *)
let%expect_test "recursive static demanded during its own definition" =
  go
    {|
fun f (x : int) : static int = h (f 0)
and h (static z : int) : int = z;;
let _ = f 0;;
|};
  [%expect {| ((loc ((line 2) (column 34))) (reason (Recursion_limit 1000))) |}]
;;

let%expect_test "erased fn" =
  go
    {|
fun erased f (x : int) : int = 0;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "erased fn" =
  go
    {|
fun erased f (x : int) : dynamic int = 0;;
let _ = f 0;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Erased_application
       (fn
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret_ty (Type Int))
          (ret_mode ((staticity Dynamic) (erasure Unerased))))))
       (result ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "erased fn" =
  go
    {|
let f = fn erased (x : int) -> 0;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "erased fn" =
  go
    {|
let f = fn erased (x : int) -> x @ erased;;
let _ = f 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 33)))
     (reason
      (Mode_mismatch (got ((staticity Parametric) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "erased-if on a recursive static errors" =
  go
    {|
fun f (x : int) : static int = if erased (f 0 == 0) then 0 else 1;;
let _ = f 0;;
|};
  [%expect {| ((loc ((line 2) (column 42))) (reason (Recursion_limit 1000))) |}]
;;

let%expect_test "cyclic type function in an annotation hits the limit" =
  go
    {|
fun t (static n : int) : erased type = t n;;
fun f (static n : int) : t n -> int = fn (v : int) -> v;;
|};
  [%expect {| ((loc ((line 2) (column 39))) (reason (Recursion_limit 1000))) |}]
;;

let%expect_test "cyclic type function forced by an annotation hits the limit" =
  go
    {|
fun t (static n : int) : erased type = t n;;
let _ = 0 : t 0;;
|};
  [%expect {| ((loc ((line 2) (column 39))) (reason (Recursion_limit 1000))) |}]
;;

let%expect_test "unbounded different-key static recursion hits the limit" =
  go
    {|
fun f (static x : int) : int = f (x + 1);;
let _ = f 0;;
|};
  [%expect {| ((loc ((line 2) (column 31))) (reason (Recursion_limit 1000))) |}]
;;

(* -------------------------------------------------------------------------------------
   Curried recursion at a concrete outer key: the recursive call re-applies the binder
   at the same key from inside its own specialization, before the memo entry lands. The
   in-progress instance is black-holed — the re-entry returns the stuck application, the
   term-level analogue of the & name suspension — because the well-founded descent lives
   in a later argument. Terminal same-key self-applications are not black-holed: they
   demand their own value to produce themselves, so they keep re-entering and the
   recursion limit fires (the divergence proxy).
   ------------------------------------------------------------------------------------- *)

let%expect_test "curried recursion at a concrete outer key is black-holed" =
  go
    {|
fun nat (erased _ : unit) : erased type = variant { z, s : &(nat ()) };;
let nat = nat ();;
let one = nat.s box nat.z;;
fun tow (erased t : type) : erased (static nat -> erased type) =
  fn (static n : nat) ->
  match static n { .z -> unit, .s &m -> t ^ tow t m }
;;
fun g (x : tow bool one) : bool = true;;
|};
  [%expect {| |}]
;;

let%expect_test "an unerased curried family recurses at the outer key" =
  go
    {|
fun nat (erased _ : unit) : erased type = variant { z, s : &(nat ()) };;
let nat = nat ();;
let two = nat.s box (nat.s box nat.z);;
fun vec (erased t : type) : erased (static nat -> erased type) =
  fn (static n : nat) ->
    match static n {
      .z -> variant { nil },
      .s &m -> variant { cons : t ^ vec t m },
    }
;;
fun mk (erased t : type) : (static nat \ n -> t -> vec t n) =
  fn (static n : nat) ->
    fn (x : t) ->
      match static n {
        .z -> (vec t n).nil,
        .s &m -> (vec t n).cons (x, mk t m x),
      }
;;
let v = mk int two 7;;
|};
  [%expect {| |}]
;;

let%expect_test "a terminal same-key self-application still hits the limit" =
  (* The inner binder's recursion is at the same full spine — no argument
     descends — so the black hole must not rescue it. *)
  go
    {|
fun nat (erased _ : unit) : erased type = variant { z, s : &(nat ()) };;
let nat = nat ();;
let two = nat.s box (nat.s box nat.z);;
fun w (erased t : type) : erased (static nat -> erased type) = fn (static n : nat) -> w t n;;
let erased x = w bool two;;
|};
  [%expect {| |}]
;;

(* -------------------------------------------------------------------------------------
   Length-indexed append: the flat join of the arm types (vec t m against
   vec t (add (n', m))) has no finite form and burns its speculation fuel; the match's
   type falls back to the conditional over the scrutinee, and the comparison against the
   declared dependent return closes by definite reduction steps under each arm's fact.
   ------------------------------------------------------------------------------------- *)

let%expect_test "append over length-indexed vectors typechecks" =
  go
    {|
fun nat (erased _ : unit) : erased type = variant { z, s : &(nat ()) };;
let nat = nat ();;
fun add (static nm : nat ^ nat) : static nat =
  match static nm { (.z, n) -> n, (.s &m, n) -> nat.s box (add (m, n)) };;
fun vec (erased t : type) : erased (static nat -> erased type) =
  fn (static n : nat) ->
    match static n { .z -> variant { nil }, .s &m -> variant { cons : t ^ vec t m } };;
fun append
  (erased t : type) : (static nat \ n -> static nat \ m
                         -> vec t n -> vec t m -> vec t (add (n, m))) =
  fn (static n : nat) -> fn (static m : nat) ->
    fn (v1 : vec t n) -> fn (v2 : vec t m) ->
      match static n {
        .z -> v2,
        .s &k -> match v1 { .cons (x, xs) -> (vec t (add (n, m))).cons (x, append t k m xs v2) },
      }
;;
|};
  [%expect {| |}]
;;

let%expect_test "a wrong arm result against the dependent return rejects promptly" =
  (* The .s arm returns a nil-typed vector where the declared index demands a
     cons; the doomed speculation exhausts its fuel and the mismatch reports
     instead of hanging. *)
  go
    {|
fun nat (erased _ : unit) : erased type = variant { z, s : &(nat ()) };;
let nat = nat ();;
fun add (static nm : nat ^ nat) : static nat =
  match static nm { (.z, n) -> n, (.s &m, n) -> nat.s box (add (m, n)) };;
fun vec (erased t : type) : erased (static nat -> erased type) =
  fn (static n : nat) ->
    match static n { .z -> variant { nil }, .s &m -> variant { cons : t ^ vec t m } };;
fun w (erased t : type) : (static nat \ n -> static nat \ m -> vec t n -> vec t m -> vec t (add (n, m))) =
  fn (static n : nat) -> fn (static m : nat) -> fn (v1 : vec t n) -> fn (v2 : vec t m) ->
    match static n { .z -> v2, .s &k -> match v1 { .cons (x, xs) -> w t k m xs v2 } }
;;
|};
  [%expect
    {|
    ((loc ((line 9) (column 4)))
     (reason
      (Type_mismatch
       (got
        (Type
         (Pi
          (arg_ty
           (Type
            (Variant
             ((s ((Type (Ref (Apply (fn (Rec 210)) (arg Unit)))))) (z ())))))
          (arg_mode ((staticity Static) (erasure Unerased)))
          (ret_ty
           (Typecheck (env <opaque>) (arg ((Id n) <opaque>))
            (arg_ty
             (Type
              (Variant
               ((s ((Type (Ref (Apply (fn (Rec 210)) (arg Unit)))))) (z ())))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (body
             (Lambda (arg ((Id m) <opaque>))
              (arg_mode ((staticity (Static)) (erasure ())))
              (arg_ty
               (Var (id ((Id nat) <opaque>)) (loc ((line 10) (column 40)))))
              (body
               (Lambda (arg ((Id v1) <opaque>))
                (arg_mode ((staticity ()) (erasure ())))
                (arg_ty
                 (Apply
                  (fn
                   (Apply
                    (fn
                     (Var (id ((Id vec) <opaque>)) (loc ((line 10) (column 57)))))
                    (arg
                     (Var (id ((Id t) <opaque>)) (loc ((line 10) (column 61)))))
                    (loc ((line 10) (column 57)))))
                  (arg
                   (Var (id ((Id n) <opaque>)) (loc ((line 10) (column 63)))))
                  (loc ((line 10) (column 57)))))
                (body
                 (Lambda (arg ((Id v2) <opaque>))
                  (arg_mode ((staticity ()) (erasure ())))
                  (arg_ty
                   (Apply
                    (fn
                     (Apply
                      (fn
                       (Var (id ((Id vec) <opaque>))
                        (loc ((line 10) (column 78)))))
                      (arg
                       (Var (id ((Id t) <opaque>)) (loc ((line 10) (column 82)))))
                      (loc ((line 10) (column 78)))))
                    (arg
                     (Var (id ((Id m) <opaque>)) (loc ((line 10) (column 84)))))
                    (loc ((line 10) (column 78)))))
                  (body
                   (Match
                    (cond
                     (Var (id ((Id n) <opaque>)) (loc ((line 11) (column 17)))))
                    (arms
                     (((Constructor (label z) (payload ())
                        (loc ((line 11) (column 21))))
                       (Var (id ((Id v2) <opaque>))
                        (loc ((line 11) (column 27)))))
                      ((Constructor (label s)
                        (payload
                         ((Ref
                           (payload
                            (Var (id ((Id k) <opaque>))
                             (loc ((line 11) (column 35)))))
                           (loc ((line 11) (column 34))))))
                        (loc ((line 11) (column 31))))
                       (Match
                        (cond
                         (Var (id ((Id v1) <opaque>))
                          (loc ((line 11) (column 46)))))
                        (arms
                         (((Constructor (label cons)
                            (payload
                             ((Tuple
                               (elts
                                ((Var (id ((Id x) <opaque>))
                                  (loc ((line 11) (column 58))))
                                 (Var (id ((Id xs) <opaque>))
                                  (loc ((line 11) (column 61))))))
                               (loc ((line 11) (column 57))))))
                            (loc ((line 11) (column 51))))
                           (Apply
                            (fn
                             (Apply
                              (fn
                               (Apply
                                (fn
                                 (Apply
                                  (fn
                                   (Apply
                                    (fn
                                     (Var (id ((Id w) <opaque>))
                                      (loc ((line 11) (column 68)))))
                                    (arg
                                     (Var (id ((Id t) <opaque>))
                                      (loc ((line 11) (column 70)))))
                                    (loc ((line 11) (column 68)))))
                                  (arg
                                   (Var (id ((Id k) <opaque>))
                                    (loc ((line 11) (column 72)))))
                                  (loc ((line 11) (column 68)))))
                                (arg
                                 (Var (id ((Id m) <opaque>))
                                  (loc ((line 11) (column 74)))))
                                (loc ((line 11) (column 68)))))
                              (arg
                               (Var (id ((Id xs) <opaque>))
                                (loc ((line 11) (column 76)))))
                              (loc ((line 11) (column 68)))))
                            (arg
                             (Var (id ((Id v2) <opaque>))
                              (loc ((line 11) (column 79)))))
                            (loc ((line 11) (column 68)))))))
                        (eliminator Dynamic) (loc ((line 11) (column 40)))))))
                    (eliminator Static) (loc ((line 11) (column 4)))))
                  (loc ((line 10) (column 69)))))
                (loc ((line 10) (column 48)))))
              (loc ((line 10) (column 25)))))
            (memo <opaque>) (uid <opaque>)))
          (ret_mode ((staticity Static) (erasure Unerased))))))
       (need
        (Type
         (Pi
          (arg_ty
           (Type
            (Variant
             ((s ((Type (Ref (Apply (fn (Rec 210)) (arg Unit)))))) (z ())))))
          (arg_mode ((staticity Static) (erasure Unerased)))
          (ret_ty
           (Reduce (env <opaque>) (arg ((Id n) <opaque>))
            (arg_ty
             (Type
              (Variant
               ((s ((Type (Ref (Apply (fn (Rec 210)) (arg Unit)))))) (z ())))))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (ret_ty
             (Arrow
              (arg (Var (id ((Id nat) <opaque>)) (loc ((line 9) (column 52)))))
              (arg_id ((Id m) <opaque>))
              (arg_mode ((staticity (Static)) (erasure ())))
              (ret
               (Arrow
                (arg
                 (Apply
                  (fn
                   (Apply
                    (fn
                     (Var (id ((Id vec) <opaque>)) (loc ((line 9) (column 63)))))
                    (arg
                     (Var (id ((Id t) <opaque>)) (loc ((line 9) (column 67)))))
                    (loc ((line 9) (column 63)))))
                  (arg (Var (id ((Id n) <opaque>)) (loc ((line 9) (column 69)))))
                  (loc ((line 9) (column 63)))))
                (arg_id (Anon <opaque>)) (arg_mode ((staticity ()) (erasure ())))
                (ret
                 (Arrow
                  (arg
                   (Apply
                    (fn
                     (Apply
                      (fn
                       (Var (id ((Id vec) <opaque>))
                        (loc ((line 9) (column 74)))))
                      (arg
                       (Var (id ((Id t) <opaque>)) (loc ((line 9) (column 78)))))
                      (loc ((line 9) (column 74)))))
                    (arg
                     (Var (id ((Id m) <opaque>)) (loc ((line 9) (column 80)))))
                    (loc ((line 9) (column 74)))))
                  (arg_id (Anon <opaque>))
                  (arg_mode ((staticity ()) (erasure ())))
                  (ret
                   (Apply
                    (fn
                     (Apply
                      (fn
                       (Var (id ((Id vec) <opaque>))
                        (loc ((line 9) (column 85)))))
                      (arg
                       (Var (id ((Id t) <opaque>)) (loc ((line 9) (column 89)))))
                      (loc ((line 9) (column 85)))))
                    (arg
                     (Apply
                      (fn
                       (Var (id ((Id add) <opaque>))
                        (loc ((line 9) (column 92)))))
                      (arg
                       (Make_tuple
                        (elts
                         ((Var (id ((Id n) <opaque>))
                           (loc ((line 9) (column 97))))
                          (Var (id ((Id m) <opaque>))
                           (loc ((line 9) (column 100))))))
                        (loc ((line 9) (column 97)))))
                      (loc ((line 9) (column 92)))))
                    (loc ((line 9) (column 85)))))
                  (ret_mode ((staticity ()) (erasure ())))
                  (loc ((line 9) (column 63)))))
                (ret_mode ((staticity ()) (erasure ())))
                (loc ((line 9) (column 52)))))
              (ret_mode ((staticity ()) (erasure ())))
              (loc ((line 9) (column 34)))))
            (memo <opaque>) (uid <opaque>)))
          (ret_mode ((staticity Static) (erasure Unerased)))))))))
    |}]
;;

(* -------------------------------------------------------------------------------------
   Non-uniform curried recursion: instantiating [grow int] reduces the inner body at a
   symbolic depth, where the recursive call's key is concrete and doubles every level.
   A group's own function-typed applications never specialize while the group
   elaborates — eager specialization would burn the recursion limit at ever-fresh keys
   no instance demands. The deferred mono computes where a demand reaches it: a later
   application with the group settled, or emission reachability at collection. A
   deferred target nothing demands is simply absent.
   ------------------------------------------------------------------------------------- *)

let%expect_test "a non-uniform curried chain defers while its group elaborates" =
  go
    {|fun grow (erased t : type) : (static int -> dynamic int) =
        fn (static d : int) -> if erased (d == 0) then 0 else grow (t ^ t) (d - 1)
      ;;
      let g = grow int;;
      let _ = g 3;;|};
  [%expect {| |}]
;;

let%expect_test "a deferred instance nothing demands never specializes" =
  go
    {|fun grow (erased t : type) : (static int -> dynamic int) =
        fn (static d : int) -> if erased (d == 0) then 0 else grow (t ^ t) (d - 1)
      ;;
      let g = grow int;;|};
  [%expect {| |}]
;;

let%expect_test "mutual non-uniform recursion defers across the group" =
  go
    {|fun f (erased t : type) : (static int -> dynamic int) =
        fn (static d : int) -> if erased (d == 0) then 0 else g (t ^ t) (d - 1)
      and g (erased t : type) : (static int -> dynamic int) =
        fn (static d : int) -> if erased (d == 0) then 1 else f (t ^ t) (d - 1)
      ;;
      let _ = f int 4;;|};
  [%expect {| |}]
;;

let%expect_test "deferral spans a dynamic lambda under the static binder" =
  (* The recursive call defers wherever [f] is still elaborating — including
     inside an emitted-shape lambda; each concrete instance re-elaborates the
     body with the group settled and specializes the descent eagerly. *)
  go
    {|fun f (erased t : type) : (static int -> int -> dynamic int) =
        fn (static d : int) -> fn (x : int) ->
          if erased (d == 0) then x else f (t ^ t) (d - 1) x
      ;;
      let _ = f int 2 5;;|};
  [%expect {| |}]
;;

let%expect_test "an unbounded non-uniform ascent errors at the concrete demand" =
  (* The deferred call is fine to form; instancing [g 1] demands depths that
     never reach the base case, and the limit reports at the demand. *)
  go
    {|fun f (erased t : type) : (static int -> dynamic int) =
        fn (static d : int) -> if erased (d == 0) then 0 else f (t ^ t) (d + 1)
      ;;
      let g = f int;;
      let _ = g 1;;|};
  [%expect {| ((loc ((line 2) (column 62))) (reason (Recursion_limit 1000))) |}]
;;

let%expect_test "an inner group's settle does not discard an enclosing name's waiter" =
  (* The [f 0] demand waits on [f] specifically: a nested group settling in
     between — after the demand or before it — wakes only its own waiters,
     so the enclosing divergence still errors. *)
  go
    {|
fun f (static n : int) : int =
  let erased _ = f 0 in
  n
;;
|};
  [%expect {| ((loc ((line 3) (column 17))) (reason (Recursion_limit 1000))) |}];
  go
    {|
fun f (static n : int) : int =
  let erased _ = f 0 in
  fun h (static x : int) : int = x in
  n
;;
|};
  [%expect {| ((loc ((line 3) (column 17))) (reason (Recursion_limit 1000))) |}];
  go
    {|
fun f (static n : int) : int =
  fun h (static x : int) : int = x in
  let erased _ = f 0 in
  n
;;
|};
  [%expect {| ((loc ((line 4) (column 17))) (reason (Recursion_limit 1000))) |}]
;;

let%expect_test "waiters survive nested groups two and three levels deep" =
  go
    {|
fun f (static n : int) : int =
  let erased _ = f 0 in
  fun g (static x : int) : int =
    fun h (static y : int) : int = y in
    x
  in
  n
;;
|};
  [%expect {| ((loc ((line 3) (column 17))) (reason (Recursion_limit 1000))) |}];
  go
    {|
fun f (static n : int) : int =
  let erased _ = f 0 in
  fun g (static x : int) : int =
    fun h (static y : int) : int =
      fun k (static z : int) : int = z in
      y
    in
    x
  in
  n
;;
|};
  [%expect {| ((loc ((line 3) (column 17))) (reason (Recursion_limit 1000))) |}]
;;

let%expect_test "inner and outer waiters release at their own settles" =
  (* [g 1] waits on [g] and wakes at the inner settle; [f 1] waits on [f] and
     wakes at the outer one. Both terminate, so the program is accepted; when
     the outer demand diverges instead, only it errors. *)
  go
    {|
fun f (static n : int) : int =
  fun g (static x : int) : int =
    let erased _ = if erased (x == 1) then 0 else g 1 in
    x
  in
  let erased _ = if erased (n == 1) then 0 else f 1 in
  n
;;
|};
  [%expect {| |}];
  go
    {|
fun f (static n : int) : int =
  fun g (static x : int) : int =
    let erased _ = if erased (x == 1) then 0 else g 1 in
    x
  in
  let erased _ = f 0 in
  n
;;
|};
  [%expect {| ((loc ((line 7) (column 17))) (reason (Recursion_limit 1000))) |}]
;;

let%expect_test "an arrow application's waiter survives a nested group too" =
  (* The recursive arrow's static application goes stuck at force time and
     waits on [f]; the nested group's settle must not release it early. *)
  go
    {|
fun f (x : int) : int =
  let erased _ = if erased (f 0 == 0) then 0 else 0 in
  fun h (static y : int) : int = y in
  x
;;
|};
  [%expect {| ((loc ((line 3) (column 32))) (reason (Recursion_limit 1000))) |}]
;;

let%expect_test "an inner divergent waiter is not dropped by a terminating outer one" =
  go
    {|
fun f (static n : int) : int =
  fun g (static x : int) : int =
    let erased _ = g 1 in
    x
  in
  let erased _ = if erased (n == 1) then 0 else f 1 in
  n
;;
|};
  [%expect {| ((loc ((line 4) (column 19))) (reason (Recursion_limit 1000))) |}]
;;

let%expect_test "a waiter runs at settlement, or immediately if already settled" =
  let state = Typecheck.For_testing.create_state () in
  let uid = Ids.Fn.create () in
  Typecheck.For_testing.register_group
    state
    [ Ident.fresh Ident.Raw.anon, uid, Ids.Family.create (), Tst.Desc.of_type Tst.Ty.Int ];
  let woken = ref false in
  Typecheck.For_testing.wait state uid (fun () -> woken := true);
  print_s [%sexp (!woken : bool)];
  Typecheck.For_testing.settle_group state [ uid, Tst.Value.unit ];
  print_s [%sexp (!woken : bool)];
  let late = ref false in
  Typecheck.For_testing.wait state uid (fun () -> late := true);
  print_s [%sexp (!late : bool)];
  [%expect
    {|
    false
    true
    true
    |}]
;;

let%expect_test "a group-sibling demand resolves through the settle-time waiter" =
  (* [g 1] is demanded by the erased scrutinee while the group is still
     elaborating: the demand goes stuck on the unsettled name and waits on
     it, re-running when the group settles. The conditional resolves per instance, so the selected
     unreachable fires only when an instance is demanded. *)
  let program use =
    sprintf
      {|fun f (static n : int) : erased int = if erased (g 1 == 1) then unreachable else n
and g (static m : int) : int = m;;
%s|}
      use
  in
  go (program "");
  [%expect {| |}];
  go (program "let erased x = f 0;;");
  [%expect {| ((loc ((line 1) (column 64))) (reason Unreachable_reached)) |}]
;;

let%expect_test "a divergent waiting demand errors at declaration" =
  (* The scrutinee demand on the unsettled sibling re-runs at settle, where
     [g]'s ascent has no cut: divergent self-reference errors at elaboration
     with no use anywhere, instead of compiling into a ghost. *)
  go
    {|
fun f (static n : int) : erased int = if erased (g 1 == 1) then n else n
and g (static m : int) : int = g (m + 1);;
|};
  [%expect {| ((loc ((line 3) (column 31))) (reason (Recursion_limit 1000))) |}]
;;

let%expect_test "a completed family's application in an undemanded body waits for a demand" =
  (* [boom 1] sits in another group's runtime lambda body — validated, not
     evaluated. With no demand its instance is simply absent; instancing
     [user] emits the body, and collection computes the dispatch target and
     reports its selected unreachable. Deferral changes when the instance is
     checked, never whether. *)
  let program use =
    sprintf
      {|fun boom (static x : int) : static int = if erased (x == 0) then 1 else unreachable;;
      fun user (static erased t : type) : (t -> dynamic t) =
        fn (y : t) -> let _ = boom 1 in y;;
      %s|}
      use
  in
  go (program "");
  [%expect {| |}];
  go (program "let _ = user int 7;;");
  [%expect {| ((loc ((line 1) (column 72))) (reason Unreachable_reached)) |}]
;;

let%expect_test "a specialization under an enclosing guard is not reused after settling" =
  (* The erased call specializes [h] while [f] is active, so [f (n - 1)] is
     folded in that provisional result. Reusing the result after [f] settles
     would skip the concrete [n = 0] check and silently accept unreachable. *)
  go
    {|
fun f (static n : int) : (static unit -> int) =
  let h = fn (static _ : unit) ->
    if erased (n == 0) then unreachable else f (n - 1) ()
  in
  let erased _ = h () in h
;;
let h = f 1;;
let erased x = h ();;
|};
  [%expect {| ((loc ((line 4) (column 28))) (reason Unreachable_reached)) |}]
;;

let%expect_test "cached helpers propagate their enclosing guard dependency" =
  (* [g ()] is cached while [h] is first inferred. Specializing [h] then
     reuses [g], so its dependency on [f]'s guard must propagate through the
     cache hit. Otherwise both provisional results escape from [f 1]. *)
  go
    {|
fun f (static n : int) : (static unit -> int) =
  let g = fn (static _ : unit) ->
    if erased (n == 0) then unreachable else f (n - 1) ()
  in
  let h = fn (static _ : unit) -> g () in
  let erased _ = h () in h
;;
let h = f 1;;
let erased x = h ();;
|};
  [%expect {| ((loc ((line 4) (column 28))) (reason Unreachable_reached)) |}]
;;

(* -------------------------------------------------------------------------------------
   Deferral changes when a demanded instance is checked, never whether — and demand is
   one bit of env structure. Liveness ends at validated-but-unevaluated contexts
   (function bodies, unselected arms of unknown static conditionals), is restored by
   evaluation, and a specialization's spine takes its demander's liveness. A dead
   context computes nothing — every application there emits its dispatch and registers
   the frame — so there is nothing to track: checks (unreachable selection, the
   recursion limit) fire on the demand spine that actually reaches an instance, at
   elaboration for live sites and at collection for emitted ones, and work in an
   undemanded body is simply absent. The tests below pin demand liveness, cache
   behavior, and erased/unerased symmetry at those edges.
   ------------------------------------------------------------------------------------- *)

let%expect_test "a deferral in an abstract static branch is not an owed demand" =
  let program use =
    sprintf
      {|
fun f (static n : int) : (static unit -> int) =
  if erased (n == 0)
  then let erased _ = f 1 in fn (static _ : unit) -> 0
  else
    let erased _ = if erased (n == 1) then unreachable else 0 in
    fn (static _ : unit) -> 0
;;
%s
|}
      use
  in
  (* The generic pass checks both branches, but neither executes. The
     concrete [f 1] seen in the abstract then-branch must therefore remain
     speculative when there is no instance root. *)
  go (program "");
  [%expect {| |}];
  (* Once [f 0] selects that branch, [f 1] is an actual erased demand and its
     selected unreachable must still be drained. *)
  go (program "let _ = f 0;;");
  [%expect {| ((loc ((line 6) (column 43))) (reason Unreachable_reached)) |}]
;;

let%expect_test "a deferral in a speculative static match arm is not owed" =
  let program use =
    sprintf
      {|
fun f (static n : int) : (static unit -> int) =
  match static n {
    0 -> let erased _ = f 1 in fn (static _ : unit) -> 0,
    _ ->
      let erased _ = if erased (n == 1) then unreachable else 0 in
      fn (static _ : unit) -> 0,
  }
;;
%s
|}
      use
  in
  go (program "");
  [%expect {| |}];
  go (program "let _ = f 0;;");
  [%expect {| ((loc ((line 6) (column 45))) (reason Unreachable_reached)) |}]
;;

let%expect_test "a demanded cache hit inherits speculative deferred dependencies" =
  let program use =
    sprintf
      {|
fun h (static p : static int -> static unit -> int) : (static unit -> int) =
  let erased _ = p 1 in fn (static _ : unit) -> 0
;;
fun f (static n : int) : (static unit -> int) =
  if erased (n == 0)
  then let erased _ = h f in fn (static _ : unit) -> 0
  else
    let erased _ = if erased (n == 1) then unreachable else 0 in
    fn (static _ : unit) -> 0
;;
%s
|}
      use
  in
  (* Generic validation caches [h f] from a speculative branch and discovers
     [f 1], but that discovery is not itself a demand. *)
  go (program "");
  [%expect {| |}];
  (* The concrete [f 0] instance reuses [h f]. Its cache hit must replay the
     deferred [f 1] dependency as owed instead of silently skipping it. *)
  go (program "let _ = f 0;;");
  [%expect {| ((loc ((line 9) (column 43))) (reason Unreachable_reached)) |}]
;;

let%expect_test "nested cache hits propagate deferred dependencies" =
  let program use =
    sprintf
      {|
fun h (static p : static int -> static unit -> int) : (static unit -> int) =
  let erased _ = p 1 in fn (static _ : unit) -> 0
;;
fun g (static p : static int -> static unit -> int) : (static unit -> int) =
  let erased _ = h p in fn (static _ : unit) -> 0
;;
fun f (static n : int) : (static unit -> int) =
  if erased (n == 99)
  then let erased _ = h f in fn (static _ : unit) -> 0
  else if erased (n == 0)
  then let erased _ = g f in fn (static _ : unit) -> 0
  else
    let erased _ = if erased (n == 1) then unreachable else 0 in
    fn (static _ : unit) -> 0
;;
%s
|}
      use
  in
  (* The first speculative arm primes [h f]. The second computes [g f]
     through that cache hit, so [g]'s entry must inherit [h]'s [f 1]
     dependency even though no concrete root exists yet. *)
  go (program "");
  [%expect {| |}];
  (* [f 0] selects only the [g f] arm. Replaying [g]'s transitive dependency
     is therefore the sole route to the failing [f 1] instance. *)
  go (program "let _ = f 0;;");
  [%expect {| ((loc ((line 14) (column 43))) (reason Unreachable_reached)) |}]
;;

let%expect_test "a parent resumes static evaluation after its deferred child settles" =
  let program use =
    sprintf
      {|
fun f (static n : int) : (static unit -> int) =
  if erased (n == 0)
  then
    let x = f 1 () in
    if erased (x == 42) then unreachable else fn (static _ : unit) -> 0
  else
    fn (static _ : unit) -> 42
;;
%s
|}
      use
  in
  (* Generic validation leaves [f 1 ()] folded, so the inner condition is
     legitimately unknown until a concrete root selects the outer branch. *)
  go (program "");
  [%expect {| |}];
  (* Draining [f 1] alone is insufficient: [f 0] must be rechecked with the
     completed child so [x == 42] selects and reaches [unreachable]. *)
  go (program "let _ = f 0;;");
  [%expect {| ((loc ((line 6) (column 29))) (reason Unreachable_reached)) |}]
;;

let%expect_test "resuming a parent can mint another owed child" =
  go
    {|
fun f (static n : int) : (static unit -> int) =
  if erased (n == 0)
  then
    let x = f 1 () in
    if erased (x == 1)
    then
      let y = f 2 () in
      if erased (y == 42) then unreachable else fn (static _ : unit) -> 0
    else fn (static _ : unit) -> 0
  else
    fn (static _ : unit) -> if erased (n == 1) then 1 else 42
;;
let _ = f 0;;
|};
  (* Refreshing [f 0] after [f 1] settles selects a previously speculative
     path and creates the live [f 2] demand. The drain/refresh loop must settle
     that new child and resume [f 0] once more. *)
  [%expect {| ((loc ((line 9) (column 31))) (reason Unreachable_reached)) |}]
;;

let%expect_test "an uncalled named function body does not create an owed demand" =
  (* Named function bodies are generic lambda bodies too. [ghost] is erased
     and never called, so validating it inside the demanded [f 0] instance
     must not turn its dormant [f 1] call into an owed frame. Specializing [h]
     along the way must preserve, rather than reset, that lambda boundary. *)
  go
    {|
fun f (static n : int) : (static unit -> int) =
  fun erased h (static p : static int -> static unit -> int) : (static unit -> int) =
    let erased _ = p 1 in fn (static _ : unit) -> 0
  in
  fun erased ghost (x : int) : dynamic int =
    let erased _ = h f in x
  in
  let erased _ = if erased (n == 1) then unreachable else 0 in
  fn (static _ : unit) -> 0
;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "dormant work is not exported as a cache dependency" =
  (* The first root caches [f 0]. Its uncalled [ghost] body is not part of that
     instance's demand graph, so the later demanded cache hit from [g ()]
     must not replay [f 1] as owed. *)
  go
    {|
fun f (static n : int) : (static unit -> int) =
  fun erased ghost (x : int) : dynamic int =
    let erased _ = f 1 in x
  in
  let erased _ = if erased (n == 1) then unreachable else 0 in
  fn (static _ : unit) -> 0
;;
let _ = f 0;;
fun g (static _ : unit) : (static unit -> int) =
  let erased _ = f 0 in fn (static _ : unit) -> 0
;;
let _ = g ();;
|};
  [%expect {| |}]
;;

let%expect_test "calling a named static function creates a live demand" =
  let program call =
    sprintf
      {|
fun f (static n : int) : (static unit -> int) =
  fun erased ghost (static _ : unit) : (static unit -> int) =
    let erased _ = f 1 in fn (static _ : unit) -> 0
  in
  if erased (n == 0)
  then %s
  else
    let erased _ = if erased (n == 1) then unreachable else 0 in
    fn (static _ : unit) -> 0
;;
let _ = f 0;;
|}
      call
  in
  (* Generic validation of an uncalled named Pi body stays speculative. *)
  go (program "fn (static _ : unit) -> 0");
  [%expect {| |}];
  (* Specializing [ghost ()] happens after its generic validation boundary has
     unwound, so its deferred [f 1] is a real demand and must be drained. *)
  go (program "let erased _ = ghost () in fn (static _ : unit) -> 0");
  [%expect {| ((loc ((line 9) (column 43))) (reason Unreachable_reached)) |}]
;;

let%expect_test "an erased demand outside the group's own elaboration checks eagerly" =
  (* No deferral intervenes, so the erased [h ()] runs to completion and the
     selected unreachable is an error even though no runtime program ever
     contains it ([h] escapes unapplied). *)
  go
    {|
fun f (static n : int) : (static unit -> int) =
  let h = fn (static _ : unit) ->
    if erased (n == 0) then unreachable else 5
  in
  let erased _ = h () in h
;;
let h = f 0;;
|};
  [%expect {| ((loc ((line 4) (column 28))) (reason Unreachable_reached)) |}]
;;

let%expect_test "an erased demand stalled on a deferral is drained after elaboration" =
  (* Same program as "a specialization under an enclosing guard is not reused
     after settling" minus the top-level re-demand. The erased [h ()] defers
     [f 0] while [f] is active, and erasure drops the specialization node, so
     no re-demand and no reify walk ever reaches the frame. The end-of-program
     drain forces it and the unreachable that the settled evaluation of the
     erased demand selects still surfaces. *)
  go
    {|
fun f (static n : int) : (static unit -> int) =
  let h = fn (static _ : unit) ->
    if erased (n == 0) then unreachable else f (n - 1) ()
  in
  let erased _ = h () in h
;;
let h = f 1;;
|};
  [%expect {| ((loc ((line 4) (column 28))) (reason Unreachable_reached)) |}]
;;

let%expect_test "the stalled demand's unerased twin is caught identically" =
  (* The only change from the previous test is the unerased internal binding:
     the [h ()] specialization node survives erasure, so even without the
     drain reify's reachable walk would force the deferred [f 0]. Erasing the
     binding no longer changes the outcome. *)
  go
    {|
fun f (static n : int) : (static unit -> int) =
  let h = fn (static _ : unit) ->
    if erased (n == 0) then unreachable else f (n - 1) ()
  in
  let _ = h () in h
;;
let g = f 1;;
|};
  [%expect {| ((loc ((line 4) (column 28))) (reason Unreachable_reached)) |}]
;;

let%expect_test "a divergent erased demand stalled on a deferral still hits the limit" =
  (* No static cut exists, so the settled evaluation of the erased [h ()]
     diverges. Each drained frame's elaboration defers the next key, and the
     drain's budget fires just as the walk's would. *)
  go
    {|
fun f (static n : int) : (static unit -> int) =
  let h = fn (static _ : unit) -> f (n - 1) () in
  let erased _ = h () in h
;;
let h = f 1;;
|};
  [%expect {| ((loc ((line 3) (column 34))) (reason (Recursion_limit 1000))) |}]
;;

let%expect_test "the divergent demand's unerased twin hits the limit" =
  go
    {|
fun f (static n : int) : (static unit -> int) =
  let h = fn (static _ : unit) -> f (n - 1) () in
  let _ = h () in h
;;
let g = f 1;;
|};
  [%expect {| ((loc ((line 3) (column 34))) (reason (Recursion_limit 1000))) |}]
;;

let%expect_test "independent drained descents do not share demand depth" =
  (* The erased twin of "independent descents in one family do not share
     demand depth": two disjoint 501-deep erased-only chains total more
     drained instances than the limit, but each frame is forced at the depth
     of its own chain. *)
  go
    {|
fun f (static p : int ^ int) : (static unit -> int) =
  let h = fn (static _ : unit) ->
    match static p { (base, n) -> if erased (n == 0) then base else f (base, n - 1) () }
  in
  let erased _ = h () in h
;;
let _ = f (10, 501);;
let _ = f (20, 501);;
|};
  [%expect {| |}]
;;

let%expect_test "an owed frame minted during collection is drained after reify" =
  (* [f 0] is deferred speculatively (under the runtime lambda) and first
     computed by collection's walk. That compute's spine — the erased demand
     live only at [n = 0] — defers [f (-1)], whose elaboration selects
     unreachable. The frame appears after the first drain has run, so the
     drain after reify forces it; demanding [f 0] statically instead
     surfaces the same error through the first drain. *)
  go
    {|
fun f (static n : int) : (int -> dynamic int) =
  let erased _ = if erased (n == 0) then (let _ = f (n - 1) in 0) else 0 in
  let erased _ = if erased (n == -1) then unreachable else 0 in
  fn (x : int) -> if erased (n == 0) then x else f (n - 1) x
;;
let _ = f 1 0;;
|};
  [%expect {| ((loc ((line 4) (column 42))) (reason Unreachable_reached)) |}]
;;

let%expect_test "recursion descending only in a runtime argument still hits the limit" =
  (* No static cut exists: every instance's closure calls the next key's
     closure, so the chain of monos is genuinely infinite. Each defers in
     turn, and collection — computing deferred instance after deferred
     instance without ever closing — hits the demand budget at the recursive
     call. *)
  go
    {|fun f (erased t : type) : (int -> dynamic int) =
        fn (x : int) -> if x == 0 then 0 else f (t ^ t) (x - 1)
      ;;
      let _ = f int 3;;|};
  [%expect {| ((loc ((line 2) (column 46))) (reason (Recursion_limit 1000))) |}]
;;

let%expect_test "independent descents in one family do not share demand depth" =
  (* Two disjoint 501-deep chains in one family total more instances than the
     limit, but neither dependency path exceeds it. A single over-limit path
     still errors. *)
  go
    {|fun f (static p : int ^ int) : (int -> dynamic int) =
        match static p {
          (base, n) -> fn (x : int) ->
            if erased (n == 0) then x + base else f (base, n - 1) x,
        }
      ;;
      let _ = f (10, 501) 1;;
      let _ = f (20, 501) 2;;|};
  [%expect {| |}];
  go
    {|fun f (static n : int) : (int -> dynamic int) =
        fn (x : int) -> if erased (n == 0) then x else f (n - 1) x
      ;;
      let _ = f 1001 1;;|};
  [%expect {| ((loc ((line 2) (column 55))) (reason (Recursion_limit 1000))) |}]
;;
