open! Core
open! Syl

let go = Common.typecheck

(* The flagship: a recursive type is finite behind the indirection. *)
let%expect_test "recursive list typechecks" =
  go "fun list (erased t : type) : erased type = variant { nil, cons : t ^ &(list t) };;";
  [%expect {| |}]
;;

let%expect_test "recursive list instantiates" =
  go
    {|fun list (erased t : type) : erased type = variant { nil, cons : t ^ &(list t) };;
      let x = (list int).nil;;|};
  [%expect {| |}]
;;

let%expect_test "the unguarded family still hits the recursion limit" =
  go
    {|fun bad (erased t : type) : erased type = variant { nil, cons : t ^ bad t };;
      let x = (bad int).nil;;|};
  [%expect {| ((loc ((line 1) (column 68))) (reason (Recursion_limit 1000))) |}]
;;

let%expect_test "non-recursive refs" =
  go "let t = &int;;";
  [%expect {| |}];
  go "let t = &int : type;;";
  [%expect {| |}];
  go "let t = &(&int);;";
  [%expect {| |}]
;;

(* The inner application computes on its own — its recursion is guarded by its
   own & — and only the head is suspended. *)
let%expect_test "nested instantiation computes the inner application" =
  go
    {|fun list (erased t : type) : erased type = variant { nil, cons : t ^ &(list t) };;
      let t = &(list (list int));;
      let x = (list (list int)).nil;;|};
  [%expect {| |}]
;;

let%expect_test "ref identity is structural via bisimulation" =
  (* Same body, different family: the same type — distinct names unfold
     coinductively. *)
  go
    {|fun list (erased t : type) : erased type = variant { nil, cons : t ^ &(list t) };;
      fun mylist (erased t : type) : erased type = variant { nil, cons : t ^ &(mylist t) };;
      fun f (dynamic x : &(list int)) : dynamic &(mylist int) = x;;|};
  [%expect {| |}]
;;

let%expect_test "name equality is transitive through the concrete form" =
  (* &(pair1 int) = &(int ^ int) = &(pair2 int) held while the direct
     comparison failed; bisimulation closes the triangle. *)
  go
    {|fun pair1 (erased t : type) : erased type = t ^ t;;
      fun pair2 (erased t : type) : erased type = t ^ t;;
      let a = box (1, 2) : &(pair1 int);;
      let c = a : &(pair2 int);;|};
  [%expect {| |}]
;;

let%expect_test "mutually recursive names bisimulate" =
  go
    {|fun tree1 (erased t : type) : erased type = variant { leaf : t, node : &(forest1 t) }
      and forest1 (erased t : type) : erased type = variant { nil, cons : &(tree1 t) ^ &(forest1 t) };;
      fun tree2 (erased t : type) : erased type = variant { leaf : t, node : &(forest2 t) }
      and forest2 (erased t : type) : erased type = variant { nil, cons : &(tree2 t) ^ &(forest2 t) };;
      let x = (tree1 int).leaf 5;;
      let y = x : tree2 int;;|};
  [%expect {| |}]
;;

let%expect_test "structurally different recursive names stay apart" =
  go
    {|fun list (erased t : type) : erased type = variant { nil, cons : t ^ &(list t) };;
      fun other (erased t : type) : erased type = variant { nil, cons : bool ^ &(other t) };;
      let x = (list int).nil;;
      let y = x : other int;;|};
  [%expect
    {|
    ((loc ((line 4) (column 16)))
     (reason
      (Type_mismatch
       (got
        (Type
         (Variant
          ((cons
            ((Type
              (Tuple
               ((Type Int)
                (Type
                 (Ref
                  (Apply
                   (fn
                    (Binder
                     ((arg ((Id t) <opaque>))
                      (ty
                       (Type
                        (Pi (arg_ty (Type Type))
                         (arg_mode ((staticity Static) (erasure Erased)))
                         (ret_ty (T (ty (Type Type)) (memo <opaque>)))
                         (ret_mode ((staticity Static) (erasure Erased))))))
                      (body_dst
                       (Variant
                        (constructors
                         (((label nil) (payload ()))
                          ((label cons)
                           (payload
                            ((Tuple
                              (elts
                               ((Var (id ((Id t) <opaque>))
                                 (loc ((line 1) (column 65))))
                                (Ref
                                 (arg
                                  (Apply
                                   (fn
                                    (Var (id ((Id list) <opaque>))
                                     (loc ((line 1) (column 71)))))
                                   (arg
                                    (Var (id ((Id t) <opaque>))
                                     (loc ((line 1) (column 76)))))
                                   (loc ((line 1) (column 71)))))
                                 (loc ((line 1) (column 69))))))
                              (loc ((line 1) (column 65)))))))))
                        (loc ((line 1) (column 43)))))
                      (env <opaque>) (family <opaque>) (uid <opaque>))))
                   (arg (Type Int))))))))))
           (nil ())))))
       (need
        (Type
         (Variant
          ((cons
            ((Type
              (Tuple
               ((Type Bool)
                (Type
                 (Ref
                  (Apply
                   (fn
                    (Binder
                     ((arg ((Id t) <opaque>))
                      (ty
                       (Type
                        (Pi (arg_ty (Type Type))
                         (arg_mode ((staticity Static) (erasure Erased)))
                         (ret_ty (T (ty (Type Type)) (memo <opaque>)))
                         (ret_mode ((staticity Static) (erasure Erased))))))
                      (body_dst
                       (Variant
                        (constructors
                         (((label nil) (payload ()))
                          ((label cons)
                           (payload
                            ((Tuple
                              (elts
                               ((Var (id ((Id bool) <opaque>))
                                 (loc ((line 2) (column 72))))
                                (Ref
                                 (arg
                                  (Apply
                                   (fn
                                    (Var (id ((Id other) <opaque>))
                                     (loc ((line 2) (column 81)))))
                                   (arg
                                    (Var (id ((Id t) <opaque>))
                                     (loc ((line 2) (column 87)))))
                                   (loc ((line 2) (column 81)))))
                                 (loc ((line 2) (column 79))))))
                              (loc ((line 2) (column 72)))))))))
                        (loc ((line 2) (column 50)))))
                      (env <opaque>) (family <opaque>) (uid <opaque>))))
                   (arg (Type Int))))))))))
           (nil ()))))))))
    |}]
;;

let%expect_test "non-uniform twins unify through their generators" =
  (* No chain of ground unfoldings closes (the key doubles every level), but
     the generators reduce to equal bodies at a shared fresh argument. *)
  go
    {|fun weird1 (erased t : type) : erased type = variant { leaf : t, node : &(weird1 (t ^ t)) };;
      fun weird2 (erased t : type) : erased type = variant { leaf : t, node : &(weird2 (t ^ t)) };;
      let x = (weird1 int).leaf 1;;
      let y = x : weird2 int;;|};
  [%expect {| |}]
;;

let%expect_test "ref distinguishes keys" =
  go
    {|fun list (erased t : type) : erased type = variant { nil, cons : t ^ &(list t) };;
      fun f (dynamic x : &(list int)) : dynamic &(list bool) = x;;|};
  [%expect
    {|
    ((loc ((line 2) (column 10)))
     (reason
      (Type_mismatch
       (got
        (Type
         (Ref
          (Apply
           (fn
            (Binder
             ((arg ((Id t) <opaque>))
              (ty
               (Type
                (Pi (arg_ty (Type Type))
                 (arg_mode ((staticity Static) (erasure Erased)))
                 (ret_ty (T (ty (Type Type)) (memo <opaque>)))
                 (ret_mode ((staticity Static) (erasure Erased))))))
              (body_dst
               (Variant
                (constructors
                 (((label nil) (payload ()))
                  ((label cons)
                   (payload
                    ((Tuple
                      (elts
                       ((Var (id ((Id t) <opaque>)) (loc ((line 1) (column 65))))
                        (Ref
                         (arg
                          (Apply
                           (fn
                            (Var (id ((Id list) <opaque>))
                             (loc ((line 1) (column 71)))))
                           (arg
                            (Var (id ((Id t) <opaque>))
                             (loc ((line 1) (column 76)))))
                           (loc ((line 1) (column 71)))))
                         (loc ((line 1) (column 69))))))
                      (loc ((line 1) (column 65)))))))))
                (loc ((line 1) (column 43)))))
              (env <opaque>) (family <opaque>) (uid <opaque>))))
           (arg (Type Int))))))
       (need
        (Type
         (Ref
          (Apply
           (fn
            (Binder
             ((arg ((Id t) <opaque>))
              (ty
               (Type
                (Pi (arg_ty (Type Type))
                 (arg_mode ((staticity Static) (erasure Erased)))
                 (ret_ty (T (ty (Type Type)) (memo <opaque>)))
                 (ret_mode ((staticity Static) (erasure Erased))))))
              (body_dst
               (Variant
                (constructors
                 (((label nil) (payload ()))
                  ((label cons)
                   (payload
                    ((Tuple
                      (elts
                       ((Var (id ((Id t) <opaque>)) (loc ((line 1) (column 65))))
                        (Ref
                         (arg
                          (Apply
                           (fn
                            (Var (id ((Id list) <opaque>))
                             (loc ((line 1) (column 71)))))
                           (arg
                            (Var (id ((Id t) <opaque>))
                             (loc ((line 1) (column 76)))))
                           (loc ((line 1) (column 71)))))
                         (loc ((line 1) (column 69))))))
                      (loc ((line 1) (column 65)))))))))
                (loc ((line 1) (column 43)))))
              (env <opaque>) (family <opaque>) (uid <opaque>))))
           (arg (Type Bool)))))))))
    |}]
;;

let%expect_test "same name written twice is the same type" =
  go
    {|fun list (erased t : type) : erased type = variant { nil, cons : t ^ &(list t) };;
      fun f (dynamic x : &(list int)) : dynamic &(list int) = x;;|};
  [%expect {| |}]
;;

let%expect_test "argument positions still reduce" =
  (* The suspension is only the head application; arguments normalize, so
     [&(list (id int))] and [&(list int)] are the same name. *)
  go
    {|fun list (erased t : type) : erased type = variant { nil, cons : t ^ &(list t) };;
      fun id (erased t : type) : erased type = t;;
      fun f (dynamic x : &(list (id int))) : dynamic &(list int) = x;;|};
  [%expect {| |}]
;;

let%expect_test "a ref is not its pointee" =
  go
    {|fun list (erased t : type) : erased type = variant { nil, cons : t ^ &(list t) };;
      let x = (list int).nil : &(list int);;|};
  [%expect
    {|
    ((loc ((line 2) (column 29)))
     (reason
      (Type_mismatch
       (got
        (Type
         (Variant
          ((cons
            ((Type
              (Tuple
               ((Type Int)
                (Type
                 (Ref
                  (Apply
                   (fn
                    (Binder
                     ((arg ((Id t) <opaque>))
                      (ty
                       (Type
                        (Pi (arg_ty (Type Type))
                         (arg_mode ((staticity Static) (erasure Erased)))
                         (ret_ty (T (ty (Type Type)) (memo <opaque>)))
                         (ret_mode ((staticity Static) (erasure Erased))))))
                      (body_dst
                       (Variant
                        (constructors
                         (((label nil) (payload ()))
                          ((label cons)
                           (payload
                            ((Tuple
                              (elts
                               ((Var (id ((Id t) <opaque>))
                                 (loc ((line 1) (column 65))))
                                (Ref
                                 (arg
                                  (Apply
                                   (fn
                                    (Var (id ((Id list) <opaque>))
                                     (loc ((line 1) (column 71)))))
                                   (arg
                                    (Var (id ((Id t) <opaque>))
                                     (loc ((line 1) (column 76)))))
                                   (loc ((line 1) (column 71)))))
                                 (loc ((line 1) (column 69))))))
                              (loc ((line 1) (column 65)))))))))
                        (loc ((line 1) (column 43)))))
                      (env <opaque>) (family <opaque>) (uid <opaque>))))
                   (arg (Type Int))))))))))
           (nil ())))))
       (need
        (Type
         (Ref
          (Apply
           (fn
            (Binder
             ((arg ((Id t) <opaque>))
              (ty
               (Type
                (Pi (arg_ty (Type Type))
                 (arg_mode ((staticity Static) (erasure Erased)))
                 (ret_ty (T (ty (Type Type)) (memo <opaque>)))
                 (ret_mode ((staticity Static) (erasure Erased))))))
              (body_dst
               (Variant
                (constructors
                 (((label nil) (payload ()))
                  ((label cons)
                   (payload
                    ((Tuple
                      (elts
                       ((Var (id ((Id t) <opaque>)) (loc ((line 1) (column 65))))
                        (Ref
                         (arg
                          (Apply
                           (fn
                            (Var (id ((Id list) <opaque>))
                             (loc ((line 1) (column 71)))))
                           (arg
                            (Var (id ((Id t) <opaque>))
                             (loc ((line 1) (column 76)))))
                           (loc ((line 1) (column 71)))))
                         (loc ((line 1) (column 69))))))
                      (loc ((line 1) (column 65)))))))))
                (loc ((line 1) (column 43)))))
              (env <opaque>) (family <opaque>) (uid <opaque>))))
           (arg (Type Int)))))))))
    |}]
;;

let%expect_test "mutual recursion through refs" =
  go
    {|fun tree (erased t : type) : erased type = variant { leaf : t, node : &(forest t) }
      and forest (erased t : type) : erased type = variant { nil, cons : &(tree t) ^ &(forest t) };;
      let x = (forest int).nil;;|};
  [%expect {| |}]
;;

let%expect_test "polymorphic recursion through refs" =
  go
    {|fun pair (erased t : type) : erased type = t ^ t;;
      fun nest (erased t : type) : erased type = variant { one : t, deep : &(nest (pair t)) };;
      let x = (nest int).one 1;;|};
  [%expect {| |}]
;;

let%expect_test "generic checking suspends symbolically" =
  (* Inside a generic body, &(list t) has a symbolic key and suspends through
     the existing symbolic branch. *)
  go
    {|fun list (erased t : type) : erased type = variant { nil, cons : t ^ &(list t) };;
      fun both (erased t : type) : erased type = &(list t) ^ &(list t);;
      let erased x = both int;;|};
  [%expect {| |}]
;;

let%expect_test "ref intro" =
  go "let x = box 5;;";
  [%expect {| |}];
  go "let x = box 5 : &int;;";
  [%expect {| |}];
  go "let x = box (box 5) : &(&int);;";
  [%expect {| |}]
;;

let%expect_test "a ref does not have its pointee's type" =
  (* The static value is transparent, but the type is not: a prim on ints
     rejects a ref of int. (Transparency is observed through the [&p] pattern
     in static matches.) *)
  go "let x = box 5;; let erased y = assert erased x == 5;;";
  [%expect
    {|
    ((loc ((line 1) (column 31)))
     (reason (Type_mismatch (got (Type (Ref (Type Int)))) (need (Type Bool)))))
    |}]
;;

let%expect_test "the roll: a built value checks against the recursive name" =
  go
    {|fun list (erased t : type) : erased type = variant { nil, cons : t ^ &(list t) };;
      let x = (list int).cons (1, box ((list int).nil));;|};
  [%expect {| |}];
  go
    {|fun list (erased t : type) : erased type = variant { nil, cons : t ^ &(list t) };;
      let x = box ((list int).nil) : &(list int);;|};
  [%expect {| |}];
  go
    {|fun list (erased t : type) : erased type = variant { nil, cons : t ^ &(list t) };;
      let two = (list int).cons (1, box ((list int).cons (2, box ((list int).nil))));;|};
  [%expect {| |}]
;;

let%expect_test "ref intro of the wrong pointee is rejected" =
  go
    {|fun list (erased t : type) : erased type = variant { nil, cons : t ^ &(list t) };;
      let x = (list int).cons (1, box 2);;|};
  [%expect
    {|
    ((loc ((line 2) (column 14)))
     (reason
      (Type_mismatch (got (Type (Tuple ((Type Int) (Type (Ref (Type Int)))))))
       (need
        (Type
         (Tuple
          ((Type Int)
           (Type
            (Ref
             (Apply
              (fn
               (Binder
                ((arg ((Id t) <opaque>))
                 (ty
                  (Type
                   (Pi (arg_ty (Type Type))
                    (arg_mode ((staticity Static) (erasure Erased)))
                    (ret_ty (T (ty (Type Type)) (memo <opaque>)))
                    (ret_mode ((staticity Static) (erasure Erased))))))
                 (body_dst
                  (Variant
                   (constructors
                    (((label nil) (payload ()))
                     ((label cons)
                      (payload
                       ((Tuple
                         (elts
                          ((Var (id ((Id t) <opaque>))
                            (loc ((line 1) (column 65))))
                           (Ref
                            (arg
                             (Apply
                              (fn
                               (Var (id ((Id list) <opaque>))
                                (loc ((line 1) (column 71)))))
                              (arg
                               (Var (id ((Id t) <opaque>))
                                (loc ((line 1) (column 76)))))
                              (loc ((line 1) (column 71)))))
                            (loc ((line 1) (column 69))))))
                         (loc ((line 1) (column 65)))))))))
                   (loc ((line 1) (column 43)))))
                 (env <opaque>) (family <opaque>) (uid <opaque>))))
              (arg (Type Int))))))))))))
    |}]
;;

let%expect_test "dynamic match through a ref pattern" =
  go
    {|fun list (erased t : type) : erased type = variant { nil, cons : t ^ &(list t) };;
      fun sum (dynamic l : list int) : dynamic int =
        match l { .nil -> 0, .cons (h, &t) -> h + sum t };;|};
  [%expect {| |}]
;;

let%expect_test "matching on a ref-typed scrutinee" =
  go
    {|fun list (erased t : type) : erased type = variant { nil, cons : t ^ &(list t) };;
      fun f (dynamic x : &(list int)) : dynamic int =
        match x { &.nil -> 0, &.cons (h, &t) -> h };;|};
  [%expect {| |}]
;;

let%expect_test "non-exhaustive match through a ref is reported" =
  go
    {|fun list (erased t : type) : erased type = variant { nil, cons : t ^ &(list t) };;
      fun f (dynamic x : &(list int)) : dynamic int = match x { &.nil -> 0 };;|};
  [%expect
    {|
    ((loc ((line 2) (column 54)))
     (reason
      (Match
       (Non_exhaustive
        ((Ref (Constructor (label cons) (payload ((Tuple (Wildcard Wildcard)))))))))))
    |}]
;;

let%expect_test "or patterns under a ref" =
  go "fun f (dynamic x : &int) : dynamic int = match x { &(0 | 1) -> 0, &_ -> 1 };;";
  [%expect {| |}]
;;

let%expect_test "static match sees through the ref" =
  (* The static value of a ref is transparent: matching statically selects the
     arm and the binding projects the pointee value. *)
  go "let x = box 5;; let erased _ = assert erased ((match static x { &p -> p }) == 5);;";
  [%expect {| |}]
;;

let%expect_test "erased match through a ref" =
  go "let x = box (box 5);; let erased y = match erased x { &(&p) -> p };;";
  [%expect {| |}]
;;

let%expect_test "static compute over a recursive list" =
  (* A statically-applied recursive function over ref-linked data: each list
     value is its own mono key; the interior refs are transparent statics. *)
  go
    {|fun list (erased t : type) : erased type = variant { nil, cons : t ^ &(list t) };;
      fun sum (l : list int) : int = match l { .nil -> 0, .cons (h, &t) -> h + sum t };;
      let erased _ = assert erased ((sum ((list int).cons (1, box ((list int).cons (2, box ((list int).nil)))))) == 3);;|};
  [%expect {| |}]
;;

let%expect_test "amp is not the intro: & of a term value is rejected" =
  (* [&expr] is the type former only ([box e] allocates; [&e] on terms is
     reserved for address-of once lifetimes exist). *)
  go "let x = &5;;";
  [%expect
    {|
    ((loc ((line 1) (column 8)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Type)))))
    |}]
;;

(* Refs are invariant: even where the pointees are strictly ordered, neither
   ref weakens to the other. *)
let%expect_test "refs do not vary with their pointees" =
  go
    {|
let sub = int -> int;;
let super = int -> dynamic int;;
fun up (r : &sub) : dynamic &super = r;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 4)))
     (reason
      (Type_mismatch
       (got
        (Type
         (Ref
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Static) (erasure Unerased))))))))
       (need
        (Type
         (Ref
          (Type
           (Arrow (arg_ty (Type Int))
            (arg_mode ((staticity Dynamic) (erasure Unerased)))
            (ret_ty (Type Int))
            (ret_mode ((staticity Dynamic) (erasure Unerased)))))))))))
    |}]
;;

let%expect_test "generators unify through helper definitions" =
  (* The generic-point reduction normalizes through [pair], so differently
     written but definitionally equal generators unify. *)
  go
    {|fun pair (erased t : type) : erased type = t ^ t;;
      fun weird1 (erased t : type) : erased type = variant { leaf : t, node : &(weird1 (t ^ t)) };;
      fun weird4 (erased t : type) : erased type = variant { leaf : t, node : &(weird4 (pair t)) };;
      let x = (weird1 int).leaf 1;;
      let y = x : weird4 int;;|};
  [%expect {| |}]
;;

let%expect_test "generators with different growth stay apart" =
  go
    {|fun weird1 (erased t : type) : erased type = variant { leaf : t, node : &(weird1 (t ^ t)) };;
      fun weird5 (erased t : type) : erased type = variant { leaf : t, node : &(weird5 (t ^ (t ^ t))) };;
      let x = (weird1 int).leaf 1;;
      let y = x : weird5 int;;|};
  [%expect
    {|
    ((loc ((line 4) (column 16)))
     (reason
      (Type_mismatch
       (got
        (Type
         (Variant
          ((leaf ((Type Int)))
           (node
            ((Type
              (Ref
               (Apply
                (fn
                 (Binder
                  ((arg ((Id t) <opaque>))
                   (ty
                    (Type
                     (Pi (arg_ty (Type Type))
                      (arg_mode ((staticity Static) (erasure Erased)))
                      (ret_ty (T (ty (Type Type)) (memo <opaque>)))
                      (ret_mode ((staticity Static) (erasure Erased))))))
                   (body_dst
                    (Variant
                     (constructors
                      (((label leaf)
                        (payload
                         ((Var (id ((Id t) <opaque>))
                           (loc ((line 1) (column 62)))))))
                       ((label node)
                        (payload
                         ((Ref
                           (arg
                            (Apply
                             (fn
                              (Var (id ((Id weird1) <opaque>))
                               (loc ((line 1) (column 74)))))
                             (arg
                              (Tuple
                               (elts
                                ((Var (id ((Id t) <opaque>))
                                  (loc ((line 1) (column 82))))
                                 (Var (id ((Id t) <opaque>))
                                  (loc ((line 1) (column 86))))))
                               (loc ((line 1) (column 82)))))
                             (loc ((line 1) (column 74)))))
                           (loc ((line 1) (column 72)))))))))
                     (loc ((line 1) (column 45)))))
                   (env <opaque>) (family <opaque>) (uid <opaque>))))
                (arg (Type (Tuple ((Type Int) (Type Int))))))))))))))
       (need
        (Type
         (Variant
          ((leaf ((Type Int)))
           (node
            ((Type
              (Ref
               (Apply
                (fn
                 (Binder
                  ((arg ((Id t) <opaque>))
                   (ty
                    (Type
                     (Pi (arg_ty (Type Type))
                      (arg_mode ((staticity Static) (erasure Erased)))
                      (ret_ty (T (ty (Type Type)) (memo <opaque>)))
                      (ret_mode ((staticity Static) (erasure Erased))))))
                   (body_dst
                    (Variant
                     (constructors
                      (((label leaf)
                        (payload
                         ((Var (id ((Id t) <opaque>))
                           (loc ((line 2) (column 68)))))))
                       ((label node)
                        (payload
                         ((Ref
                           (arg
                            (Apply
                             (fn
                              (Var (id ((Id weird5) <opaque>))
                               (loc ((line 2) (column 80)))))
                             (arg
                              (Tuple
                               (elts
                                ((Var (id ((Id t) <opaque>))
                                  (loc ((line 2) (column 88))))
                                 (Tuple
                                  (elts
                                   ((Var (id ((Id t) <opaque>))
                                     (loc ((line 2) (column 93))))
                                    (Var (id ((Id t) <opaque>))
                                     (loc ((line 2) (column 97))))))
                                  (loc ((line 2) (column 93))))))
                               (loc ((line 2) (column 88)))))
                             (loc ((line 2) (column 80)))))
                           (loc ((line 2) (column 78)))))))))
                     (loc ((line 2) (column 51)))))
                   (env <opaque>) (family <opaque>) (uid <opaque>))))
                (arg
                 (Type
                  (Tuple ((Type Int) (Type (Tuple ((Type Int) (Type Int))))))))))))))))))))
    |}]
;;

let%expect_test "static-returning closures unify through their bodies" =
  go
    {|fun fam (static erased f : int -> int) : erased type = int ^ int;;
      let h1 = fn (x : int) -> x + 1;;
      let h2 = fn (x : int) -> x + 1;;
      let r = box (1, 2) : &(fam h1);;
      let s = r : &(fam h2);;|};
  [%expect {| |}]
;;

let%expect_test "dynamic-returning closures stay nominal, names unfold instead" =
  (* The generator guard refuses a dynamic body (its static is poisoned); the
     names still unify because a phantom function argument cannot reach the
     unfolded structure. *)
  go
    {|fun fam (static erased f : int -> dynamic unit) : erased type = int ^ int;;
      let d1 = fn (x : int) -> print_int x;;
      let d2 = fn (x : int) -> print_int x;;
      let r = box (1, 2) : &(fam d1);;
      let s = r : &(fam d2);;|};
  [%expect {| |}]
;;

let%expect_test "closures in recursive keys unify through their bodies" =
  (* Each level keys the family at a freshly built closure, so no ground pair
     ever repeats; the closures' generic-point reduction closes it instead. *)
  go
    {|fun twice (static f : int -> int) : int -> int = fn (x : int) -> f (f x);;
      fun fam (static f : int -> int) : erased type = variant { leaf : int, node : &(fam (twice f)) };;
      let h1 = fn (x : int) -> x + 1;;
      let h2 = fn (x : int) -> x + 1;;
      let x = (fam h1).leaf 5;;
      let y = x : fam h2;;|};
  [%expect {| |}]
;;

let%expect_test "curried names suspend the whole spine" =
  (* The inner application of the & argument is name material: computing it
     would specialize the non-uniform family at a doubled key per level. *)
  go
    {|fun perfect (erased t : type) : erased (static int -> erased type) =
        fn (static d : int) ->
          if erased (d == 0) then t else &(perfect (t ^ t) (d - 1))
      ;;
      let quad = box (box (((1, 2), (3, 4)))) : perfect int 2;;
      let deep = perfect int 30;;|};
  [%expect {| |}]
;;

let%expect_test "missing cases stay finite for non-uniform families" =
  (* A witness never unfolds a ref, so a non-uniform family (whose name
     changes at every level) cannot diverge the diagnostic. *)
  go
    {|fun grow (static n : int) : erased type = variant { stop, next : &(grow (n + 1)) };;
      fun f (dynamic x : grow 0) : dynamic int = match x { .stop -> 0 };;|};
  [%expect
    {|
    ((loc ((line 2) (column 49)))
     (reason
      (Match (Non_exhaustive ((Constructor (label next) (payload (Wildcard))))))))
    |}]
;;
