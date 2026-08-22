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

let%expect_test "same-key static self-recursion, erased result" =
  go
    {|
fun f (static x : int) : erased int = f x;;
let _ = f 0;;
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
  [%expect {| ((loc ((line 2) (column 34))) (reason (Recursion_limit 1000))) |}]
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
  [%expect {| ((loc ((line 5) (column 86))) (reason (Recursion_limit 1000))) |}]
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
             ((s
               ((Type
                 (Ref
                  (Apply
                   (fn
                    (Binder
                     ((arg (Anon <opaque>))
                      (ty
                       (Type
                        (Pi (arg_ty (Type Unit))
                         (arg_mode ((staticity Static) (erasure Erased)))
                         (ret_ty (T (ty (Type Type)) (memo <opaque>)))
                         (ret_mode ((staticity Static) (erasure Erased))))))
                      (body_dst
                       (Variant
                        (constructors
                         (((label z) (payload ()))
                          ((label s)
                           (payload
                            ((Ref
                              (arg
                               (Apply
                                (fn
                                 (Var (id ((Id nat) <opaque>))
                                  (loc ((line 2) (column 61)))))
                                (arg
                                 (Literal (value Unit)
                                  (loc ((line 2) (column 65)))))
                                (loc ((line 2) (column 61)))))
                              (loc ((line 2) (column 59)))))))))
                        (loc ((line 2) (column 42)))))
                      (env <opaque>) (family <opaque>) (uid <opaque>))))
                   (arg Unit))))))
              (z ())))))
          (arg_mode ((staticity Static) (erasure Unerased)))
          (ret_ty
           (Typecheck (env <opaque>) (arg ((Id n) <opaque>))
            (arg_ty
             (Type
              (Variant
               ((s
                 ((Type
                   (Ref
                    (Apply
                     (fn
                      (Binder
                       ((arg (Anon <opaque>))
                        (ty
                         (Type
                          (Pi (arg_ty (Type Unit))
                           (arg_mode ((staticity Static) (erasure Erased)))
                           (ret_ty (T (ty (Type Type)) (memo <opaque>)))
                           (ret_mode ((staticity Static) (erasure Erased))))))
                        (body_dst
                         (Variant
                          (constructors
                           (((label z) (payload ()))
                            ((label s)
                             (payload
                              ((Ref
                                (arg
                                 (Apply
                                  (fn
                                   (Var (id ((Id nat) <opaque>))
                                    (loc ((line 2) (column 61)))))
                                  (arg
                                   (Literal (value Unit)
                                    (loc ((line 2) (column 65)))))
                                  (loc ((line 2) (column 61)))))
                                (loc ((line 2) (column 59)))))))))
                          (loc ((line 2) (column 42)))))
                        (env <opaque>) (family <opaque>) (uid <opaque>))))
                     (arg Unit))))))
                (z ())))))
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
             ((s
               ((Type
                 (Ref
                  (Apply
                   (fn
                    (Binder
                     ((arg (Anon <opaque>))
                      (ty
                       (Type
                        (Pi (arg_ty (Type Unit))
                         (arg_mode ((staticity Static) (erasure Erased)))
                         (ret_ty (T (ty (Type Type)) (memo <opaque>)))
                         (ret_mode ((staticity Static) (erasure Erased))))))
                      (body_dst
                       (Variant
                        (constructors
                         (((label z) (payload ()))
                          ((label s)
                           (payload
                            ((Ref
                              (arg
                               (Apply
                                (fn
                                 (Var (id ((Id nat) <opaque>))
                                  (loc ((line 2) (column 61)))))
                                (arg
                                 (Literal (value Unit)
                                  (loc ((line 2) (column 65)))))
                                (loc ((line 2) (column 61)))))
                              (loc ((line 2) (column 59)))))))))
                        (loc ((line 2) (column 42)))))
                      (env <opaque>) (family <opaque>) (uid <opaque>))))
                   (arg Unit))))))
              (z ())))))
          (arg_mode ((staticity Static) (erasure Unerased)))
          (ret_ty
           (Reduce (env <opaque>) (arg ((Id n) <opaque>))
            (arg_ty
             (Type
              (Variant
               ((s
                 ((Type
                   (Ref
                    (Apply
                     (fn
                      (Binder
                       ((arg (Anon <opaque>))
                        (ty
                         (Type
                          (Pi (arg_ty (Type Unit))
                           (arg_mode ((staticity Static) (erasure Erased)))
                           (ret_ty (T (ty (Type Type)) (memo <opaque>)))
                           (ret_mode ((staticity Static) (erasure Erased))))))
                        (body_dst
                         (Variant
                          (constructors
                           (((label z) (payload ()))
                            ((label s)
                             (payload
                              ((Ref
                                (arg
                                 (Apply
                                  (fn
                                   (Var (id ((Id nat) <opaque>))
                                    (loc ((line 2) (column 61)))))
                                  (arg
                                   (Literal (value Unit)
                                    (loc ((line 2) (column 65)))))
                                  (loc ((line 2) (column 61)))))
                                (loc ((line 2) (column 59)))))))))
                          (loc ((line 2) (column 42)))))
                        (env <opaque>) (family <opaque>) (uid <opaque>))))
                     (arg Unit))))))
                (z ())))))
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

let%expect_test "a non-uniform curried chain hits the recursion limit" =
  (* Instantiating [grow int] reduces the inner body at a symbolic depth, so
     the recursive call specializes at a concrete doubled key every level;
     the chain never repeats a key and the recursion limit cuts it. *)
  go
    {|fun grow (erased t : type) : (static int -> dynamic int) =
        fn (static d : int) -> if erased (d == 0) then 0 else grow (t ^ t) (d - 1)
      ;;
      let g = grow int;;
      let _ = g 3;;|};
  [%expect {| ((loc ((line 2) (column 42))) (reason (Recursion_limit 1000))) |}]
;;
