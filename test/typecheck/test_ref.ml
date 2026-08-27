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

let%expect_test "one ref guards recursive occurrences inside its pointee" =
  go
    {|fun tree (erased t : type) : erased type =
        variant { leaf : t, node : &(tree t ^ tree t) }
      ;;
      let x = (tree int).node (box ((tree int).leaf 1, (tree int).leaf 2));;|};
  [%expect {| |}]
;;

let%expect_test "factoring the guarded pointee does not expose recursion" =
  go
    {|fun pair (erased t : type) : erased type = t ^ t;;
      fun tree (erased t : type) : erased type =
        variant { leaf : t, node : &(pair (tree t)) }
      ;;
      let x = (tree int).node (box ((tree int).leaf 1, (tree int).leaf 2));;|};
  [%expect {| |}]
;;

let%expect_test "the unguarded family has infinite size" =
  (* The row forms — the guard folds the self-application — and the error
     moves to where it belongs: layout. *)
  go
    {|fun bad (erased t : type) : erased type = variant { nil, cons : t ^ bad t };;
      let x = (bad int).nil;;|};
  [%expect
    {|
    ((loc ((line 2) (column 14)))
     (reason
      (Infinite_size
       (Type (Tuple ((Type Int) (Apply (fn (Rec 10)) (arg (Type Int)))))))))
    |}]
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
               ((Type Int) (Type (Ref (Apply (fn (Rec 32)) (arg (Type Int))))))))))
           (nil ())))))
       (need
        (Type
         (Variant
          ((cons
            ((Type
              (Tuple
               ((Type Bool) (Type (Ref (Apply (fn (Rec 34)) (arg (Type Int))))))))))
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
          (Type
           (Variant
            ((cons
              ((Type
                (Tuple
                 ((Type Int) (Type (Ref (Apply (fn (Rec 40)) (arg (Type Int))))))))))
             (nil ())))))))
       (need
        (Type
         (Ref
          (Type
           (Variant
            ((cons
              ((Type
                (Tuple
                 ((Type Bool)
                  (Type (Ref (Apply (fn (Rec 40)) (arg (Type Bool))))))))))
             (nil ()))))))))))
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
               ((Type Int) (Type (Ref (Apply (fn (Rec 54)) (arg (Type Int))))))))))
           (nil ())))))
       (need
        (Type
         (Ref
          (Type
           (Variant
            ((cons
              ((Type
                (Tuple
                 ((Type Int) (Type (Ref (Apply (fn (Rec 54)) (arg (Type Int))))))))))
             (nil ()))))))))))
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
         (Tuple ((Type Int) (Type (Ref (Apply (fn (Rec 74)) (arg (Type Int))))))))))))
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
               (Apply (fn (Rec 102))
                (arg (Type (Tuple ((Type Int) (Type Int))))))))))))))
       (need
        (Type
         (Variant
          ((leaf ((Type Int)))
           (node
            ((Type
              (Ref
               (Apply (fn (Rec 104))
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

(* Guardedness: the fold is the group's, not the &'s, so any spelling of the
   self-reference inside the pointee works. *)
let%expect_test "the guard covers a let alias" =
  go
    {|fun tree (erased t : type) : erased type =
        let self = tree t in
        variant { leaf : t, node : &(self ^ self) }
      ;;
      let x = (tree int).node (box ((tree int).leaf 1, (tree int).leaf 2));;|};
  [%expect {| |}]
;;

let%expect_test "one ref guards a nested variant former" =
  go
    {|fun tree (erased t : type) : erased type =
        variant { leaf : t, node : &(variant { one : tree t, two : tree t ^ tree t }) }
      ;;
      let x = (tree int).leaf 1;;|};
  [%expect {| |}]
;;

let%expect_test "a sibling annotation may reference the group" =
  go
    {|fun shape (erased t : type) : erased type = variant { s : &(shape t), z }
      and mk (erased t : type) : shape t = (shape t).z;;
      let x = mk int;;|};
  [%expect {| ((loc ((line 2) (column 33))) (reason (Unbound_ident ((Id shape) <opaque>)))) |}]
;;

let%expect_test "a partially guarded group is finite on the guarded side" =
  go
    {|fun tree (erased t : type) : erased type = variant { leaf : t, node : &(forest t) }
      and forest (erased t : type) : erased type = variant { nil, cons : tree t ^ forest t };;
      let x = (tree int).leaf 5;;
      let _ = match x { .leaf n -> n, .node _ -> 0 };;|};
  [%expect {| |}]
;;

let%expect_test "the unguarded side of a partial group has infinite size" =
  go
    {|fun tree (erased t : type) : erased type = variant { leaf : t, node : &(forest t) }
      and forest (erased t : type) : erased type = variant { nil, cons : tree t ^ forest t };;
      fun get (dynamic x : tree int) : dynamic int = match x { .leaf n -> n, .node (&f) -> 0 };;|};
  [%expect
    {|
    ((loc ((line 3) (column 53)))
     (reason
      (Infinite_size
       (Type
        (Tuple
         ((Apply (fn (Rec 144)) (arg (Type Int)))
          (Apply (fn (Rec 145)) (arg (Type Int)))))))))
    |}]
;;

let%expect_test "an infinite tuple type has no inhabitants" =
  go
    {|fun p (erased t : type) : erased type = t ^ p t;;
      let x = (1, 2) : p int;;|};
  [%expect
    {|
    ((loc ((line 2) (column 21)))
     (reason
      (Type_mismatch (got (Type (Tuple ((Type Int) (Type Int)))))
       (need (Type (Tuple ((Type Int) (Apply (fn (Rec 150)) (arg (Type Int))))))))))
    |}]
;;

let%expect_test "a family demanding its own unfolding hits the limit" =
  go
    {|fun weird (erased t : type) : erased type =
        match erased (weird t) { _ -> t };;
      let x = 1 : weird int;;|};
  [%expect {| ((loc ((line 2) (column 22))) (reason (Recursion_limit 1000))) |}]
;;

(* Demands unfold chains of folded names to a fixpoint. *)
let%expect_test "recursive erased computation unfolds on demand" =
  go
    {|fun count (static n : int) : erased int = if erased (n == 0) then 0 else count (n - 1);;
      let _ = match erased (count 3) { 0 -> print_int 7, _ -> print_int 8 };;|};
  [%expect
    {|
    ((loc ((line 2) (column 57)))
     (reason
      (Dead_branch
       (branch (Arm (Var (id (Anon <opaque>)) (loc ((line 2) (column 57))))))
       (value (Int (T 0))))))
    |}]
;;

let%expect_test "assert demands through the guard" =
  go
    {|fun evenp (static n : int) : erased bool =
        if erased (n == 0) then true else if erased (n == 1) then false else evenp (n - 2);;
      let _ = assert erased (evenp 4);;|};
  [%expect {| |}]
;;

(* A non-uniform unguarded family recurses at a fresh key every level, so
   layout resolution cannot close its cycle; the recursion limit brakes it. *)
let%expect_test "a non-uniform unguarded family hits the recursion limit" =
  go
    {|fun bad (erased t : type) : erased type = variant { z, next : bad (t ^ t) };;
      let _ = (bad int).z;;|};
  [%expect {| ((loc ((line 1) (column 67))) (reason (Recursion_limit 1000))) |}]
;;

(* Erased bindings never exist at runtime, so validating a ghost arm must not
   resolve the payload's layout. *)
let%expect_test "an erased inspector over an unguarded family is a ghost" =
  go
    {|fun bad (erased t : type) : erased type = variant { nil, cons : bad t };;
      fun probe (static erased b : bad int) : erased int =
        match erased b { .nil -> 0, .cons rest -> 1 }
      ;;|};
  [%expect {| |}]
;;

(* An external's declared type is a runtime calling convention, so it resolves
   like any emitted node's type. *)
let%expect_test "an external over an unguarded family has infinite size" =
  go
    {|fun bad (erased t : type) : erased type = variant { nil, cons : bad t };;
      external foo : bad int -> dynamic int = foo;;|};
  [%expect
    {|
    ((loc ((line 2) (column 6)))
     (reason (Infinite_size (Apply (fn (Rec 164)) (arg (Type Int))))))
    |}]
;;

(* An erased node never reaches runtime, so nothing under it resolves — the
   unerased element inside this ghost tuple included. *)
let%expect_test "a ghost tuple over an unguarded family is accepted" =
  go
    {|fun bad (erased t : type) : erased type = variant { nil, cons : bad t };;
      let ghost = ((bad int).nil, int);;|};
  [%expect {| |}]
;;

(* Reachability mirrors reification: a specialization referenced only under a
   ghost is dead, so it is neither resolved nor emitted. *)
let%expect_test "a ghost composite retains no monomorphizations" =
  go
    {|fun bad (erased t : type) : erased type = variant { nil, cons : bad t };;
      fun make (static erased u : unit) : bad int = (bad int).nil;;
      let ghost = (make (), int);;|};
  [%expect {| |}]
;;
