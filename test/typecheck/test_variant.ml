open! Core
open! Syl

let go = Common.typecheck

(* Bare constructor expressions don't typecheck yet; the stub raises. Catch
   and print the message without the backtrace. *)
let go_exn input =
  match Common.typecheck input with
  | () -> ()
  | exception exn -> print_s (Exn.sexp_of_t exn)
;;

let%expect_test "variant type checks" =
  go "let option = variant { none, some : int };;";
  [%expect {| |}]
;;

let%expect_test "variant is a type" =
  go "let t = variant { none, some : int } : type;;";
  [%expect {| |}]
;;

let%expect_test "payloads are full type expressions" =
  go "let t = variant { none, wrap : int -> bool, pair : int ^ bool };;";
  [%expect {| |}]
;;

let%expect_test "variant type representation is label-sorted" =
  go "let x = 1 : variant { some : int, none };;";
  [%expect
    {|
    ((loc ((line 1) (column 10)))
     (reason
      (Type_mismatch (got (Type Int))
       (need (Type (Variant ((none ()) (some ((Type Int))))))))))
    |}]
;;

let%expect_test "duplicate label" =
  go "let t = variant { some : int, none, some : bool };;";
  [%expect {| ((loc ((line 1) (column 8))) (reason (Duplicate_label some))) |}]
;;

let%expect_test "payload must be a type" =
  go "let t = variant { some : 5 };;";
  [%expect
    {|
    ((loc ((line 1) (column 8)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Type)))))
    |}]
;;

let%expect_test "payload must be static" =
  go "let f = fn (t : type) -> variant { some : t };;";
  [%expect
    {|
    ((loc ((line 1) (column 25)))
     (reason
      (Mode_mismatch (got ((staticity Parametric) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "polymorphic variant family" =
  go
    {|
let option = fn (erased t : type) -> variant { none, some : t };;
let t1 = option int : type;;
let t2 = option (int ^ bool) : type;;
|};
  [%expect {| |}]
;;

let%expect_test "variant family instance is reduced" =
  go
    {|
let option = fn (erased t : type) -> variant { none, some : t };;
let x = 1 : option bool;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 10)))
     (reason
      (Type_mismatch (got (Type Int))
       (need (Type (Variant ((none ()) (some ((Type Bool))))))))))
    |}]
;;

let%expect_test "variant family from a static conditional" =
  go
    {|
fun opt (static n : int) : erased type =
  if erased n == 0 then variant { none } else variant { some : int }
;;
let x = 1 : opt 0;;
|};
  [%expect
    {|
    ((loc ((line 5) (column 10)))
     (reason
      (Type_mismatch (got (Type Int)) (need (Type (Variant ((none ()))))))))
    |}]
;;

let%expect_test "variant argument and return types" =
  go
    {|
let f = fn (x : variant { none, some : int }) -> x;;
let g = f : variant { none, some : int } -> variant { none, some : int };;
|};
  [%expect {| |}]
;;

let%expect_test "constructor order is irrelevant" =
  go
    {|
let f = fn (x : variant { none, some : int }) -> x;;
let g = f : variant { some : int, none } -> variant { none, some : int };;
|};
  [%expect {| |}]
;;

let%expect_test "different constructor sets don't unify" =
  go
    {|
let f = fn (x : variant { a, b }) -> x;;
let g = f : variant { a, c } -> variant { a, b };;
|};
  [%expect
    {|
    ((loc ((line 3) (column 10)))
     (reason
      (Type_mismatch
       (got
        (Type
         (Arrow (arg_ty (Type (Variant ((a ()) (b ())))))
          (arg_mode ((staticity Dynamic) (erasure Unerased)))
          (ret_ty (Type (Variant ((a ()) (b ())))))
          (ret_mode ((staticity Static) (erasure Unerased))))))
       (need
        (Type
         (Arrow (arg_ty (Type (Variant ((a ()) (c ())))))
          (arg_mode ((staticity Dynamic) (erasure Unerased)))
          (ret_ty (Type (Variant ((a ()) (b ())))))
          (ret_mode ((staticity Static) (erasure Unerased)))))))))
    |}]
;;

let%expect_test "monomorphization keyed on a variant type" =
  go
    {|
let id = fn (erased t : type) -> fn (x : int) -> x;;
let _ = id (variant { none, some : int }) 0;;
let _ = id (variant { none, some : bool }) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "monomorphization keyed on a variant value" =
  go
    {|
let opt = variant { none, some : int };;
let f = fn (erased c : opt) -> fn (x : int) -> x;;
let _ = f (opt.some 1) 0;;
let _ = f (opt.none) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "monomorphization keyed on a constructor function" =
  go
    {|
let opt = variant { none, some : int };;
let g = fn (erased f : int -> opt) -> fn (x : int) -> x;;
let _ = g (opt.some) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "constructor expression is unimplemented" =
  go_exn "let x = .some 1;;";
  [%expect {| ("Unimplemented: variant constructors" (loc ((line 1) (column 8)))) |}]
;;

let%expect_test "nullary constructor selection" =
  go "let x = (variant { none, some : int }).none;;";
  [%expect {| |}]
;;

let%expect_test "constructor selection awaits its payload" =
  go
    {|
let some = (variant { none, some : int }).some;;
let x = some 1;;
let f = some : int -> variant { none, some : int };;
|};
  [%expect {| |}]
;;

let%expect_test "qualified constructor application" =
  go "let x = (variant { none, some : int }).some 1;;";
  [%expect {| |}]
;;

let%expect_test "qualified constructor from a variant family" =
  go
    {|
let option = fn (erased t : type) -> variant { none, some : t };;
let x = (option int).some 1;;
let y = (option int).none;;
let z = (option int).some 2 : option int;;
|};
  [%expect {| |}]
;;

let%expect_test "selection result type" =
  go "let x = (variant { none, some : int }).some 1 : int;;";
  [%expect
    {|
    ((loc ((line 1) (column 46)))
     (reason
      (Type_mismatch (got (Type (Variant ((none ()) (some ((Type Int)))))))
       (need (Type Int)))))
    |}]
;;

let%expect_test "constructor payload is typechecked" =
  go "let x = (variant { none, some : int }).some true;;";
  [%expect
    {|
    ((loc ((line 1) (column 8)))
     (reason (Type_mismatch (got (Type Bool)) (need (Type Int)))))
    |}]
;;

let%expect_test "unknown label" =
  go "let x = (variant { none, some : int }).what;;";
  [%expect
    {|
    ((loc ((line 1) (column 8)))
     (reason
      (Unknown_label (from (Type (Variant ((none ()) (some ((Type Int)))))))
       (label what))))
    |}]
;;

let%expect_test "selection from a non-variant type" =
  go "let x = int.some;;";
  [%expect
    {|
    ((loc ((line 1) (column 8)))
     (reason (Expected_variant (got (Type Int)) (label some))))
    |}]
;;

let%expect_test "selection from a non-type value" =
  go "let x = (5).some;;";
  [%expect
    {|
    ((loc ((line 1) (column 8)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Type)))))
    |}]
;;

let%expect_test "dynamic payload makes a dynamic value" =
  go
    {|
let d = 1 @ dynamic;;
let x = (variant { none, some : int }).some d;;
|};
  [%expect {| |}]
;;

let%expect_test "erased payload is rejected" =
  go
    {|
let e = 1 @ erased;;
let x = (variant { none, some : int }).some e;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

(* Selecting from an abstract family application unfolds it. *)
let%expect_test "selection unfolds a family application" =
  go
    {|
fun boxed (static erased t : type) : erased type = variant { box : t };;
fun box (static erased t : type) : t -> boxed t = fn (x : t) -> (boxed t).box x;;
let _ = box int 3 : boxed int;;
|};
  [%expect {| |}]
;;

(* An application that unfolds to an unresolved conditional is rejected:
   selecting an arm would assume its condition. *)
let%expect_test "selection from an unresolved conditional family" =
  go
    {|
fun opt (static b : bool) : erased type =
  if erased b then variant { some : int } else variant { none }
;;
fun f (static b : bool) : opt b = (opt b).none;;
|};
  [%expect {|
    ((loc ((line 5) (column 34)))
     (reason
      (Expected_variant
       (got
        (Match (scrutinee (Var (Anon <opaque>)))
         (arms
          (((Literal (value (Bool true)) (loc ((line 3) (column 2))))
            (Type (Variant ((some ((Type Int)))))))
           ((Literal (value (Bool false)) (loc ((line 3) (column 2))))
            (Type (Variant ((none ())))))))))
       (label none))))
    |}]
;;

(* Matching on the index refines it, so [vec n] resolves in the zero arm; the
   successor arm learns nothing about [n] and injects into its row spelled
   out. *)
let%expect_test "match refinement resolves an indexed family" =
  go
    {|
fun vec (static n : int) : erased type =
  match erased n { 0 -> variant { nil }, _ -> variant { cons : int ^ vec (n - 1) } }
;;
fun replicate (static n : int) : int -> vec n =
  fn (x : int) ->
    match erased n {
      0 -> (vec n).nil,
      _ -> (variant { cons : int ^ vec (n - 1) }).cons (x, replicate (n - 1) x),
    }
;;
let _ = replicate 3 7 : vec 3;;
|};
  [%expect {| |}]
;;

let%expect_test "constructor pattern on a non-variant scrutinee" =
  go "let _ = match 0 { .wat -> 1 };;";
  [%expect {|
    ((loc ((line 1) (column 18)))
     (reason (Expected_variant (got (Type Int)) (label wat))))
    |}]
;;

let%expect_test "match selects a constructor arm statically" =
  go
    {|
let opt = variant { none, some : int };;
let x = opt.some 1;;
let _ = assert erased ((match x { .none -> 0, .some v -> v }) == 1);;
let _ = assert erased ((match opt.none { .none -> 0, .some v -> v }) == 0);;
|};
  [%expect {| |}]
;;

let%expect_test "match static binds the payload as a projection" =
  go
    {|
let opt = variant { none, some : int };;
let x = opt.some 2;;
let y = match static x { .none -> 0, .some v -> v };;
let _ = assert erased (y == 2);;
|};
  [%expect {| |}]
;;

let%expect_test "match on a dynamic variant" =
  go
    {|
let opt = variant { none, some : int };;
let d = 1 @ dynamic;;
let x = opt.some d;;
let r = match x { .none -> 0, .some v -> v };;
|};
  [%expect {| |}]
;;

let%expect_test "nested constructor and tuple patterns" =
  go
    {|
let opt = variant { none, some : int ^ bool };;
let x = opt.some (1, true);;
let r = match x { .some (n, true) -> n, .some (n, false) -> 0 - n, .none -> 0 };;
let _ = assert erased (r == 1);;
|};
  [%expect {| |}]
;;

let%expect_test "or pattern across constructors" =
  go
    {|
let opt = variant { none, some : int };;
let x = opt.some 1;;
let _ = assert erased ((match x { .none | .some _ -> 5 }) == 5);;
|};
  [%expect {| |}]
;;

let%expect_test "static match on a dead payload typechecks" =
  go
    {|
let dir = variant { left : int, right : int };;
let x = dir.left 4;;
let _ = match x { .left v -> v, .right v -> match static v { 1 -> 0, _ -> 1 } };;
|};
  [%expect {| |}]
;;

(* A dead arm is checked against a fresh payload — like the refined dead
   branch of a resolved [if erased] — so asserts on the payload are
   undecidable and fail eagerly, exactly as in a generic body. *)
let%expect_test "erased assert on a dead payload fails eagerly" =
  go
    {|
let dir = variant { left : bool, right : bool };;
let x = dir.left true;;
let _ = match x { .left v -> 0, .right v -> let _ = assert erased v in 1 };;
|};
  [%expect {|
    ((loc ((line 4) (column 52)))
     (reason (Static_failure (Assert_failed (Var (Anon <opaque>))))))
    |}]
;;

let%expect_test "erased assert on a dead comparison fails eagerly" =
  go
    {|
let dir = variant { left : int, right : int };;
let x = dir.left 4;;
let _ = match x { .left v -> v, .right v -> let _ = assert erased (v == 1) in 1 };;
|};
  [%expect {|
    ((loc ((line 4) (column 52)))
     (reason
      (Static_failure
       (Assert_failed (Bool (Eq (Var (Anon <opaque>)) (Int (T 1))))))))
    |}]
;;

let%expect_test "or pattern binding payloads from both constructors" =
  go
    {|
let dir = variant { left : int, right : int };;
let x = dir.left 4;;
let _ = match x { .left v | .right v -> v };;
|};
  [%expect {| |}]
;;

let%expect_test "static match on a monomorphized variant argument" =
  go
    {|
let opt = variant { none, some : int };;
fun get (static o : opt) : int = match static o { .none -> 0, .some v -> v };;
let _ = assert erased (get (opt.some 3) == 3);;
let _ = assert erased (get (opt.none) == 0);;
|};
  [%expect {| |}]
;;

let%expect_test "erased match selects a type" =
  go
    {|
let opt = variant { none, some : int };;
let e = opt.some 1 @ erased;;
let t = match erased e { .none -> int, .some _ -> bool };;
let _ = true : t;;
|};
  [%expect {| |}]
;;

let%expect_test "non-exhaustive constructor match" =
  go
    {|
let opt = variant { none, some : int };;
let d = 1 @ dynamic;;
let _ = match opt.some d { .some v -> v };;
|};
  [%expect {|
    ((loc ((line 4) (column 8)))
     (reason (Match (Non_exhaustive ((Constructor (label none) (payload ())))))))
    |}]
;;

let%expect_test "redundant constructor arm" =
  go
    {|
let opt = variant { none, some : int };;
let d = 1 @ dynamic;;
let _ = match opt.some d { .none -> 0, .some v -> v, .none -> 2 };;
|};
  [%expect {|
    ((loc ((line 4) (column 8)))
     (reason
      (Match
       (Redundant
        ((Constructor (label none) (payload ()) (loc ((line 4) (column 53)))))))))
    |}]
;;

let%expect_test "unknown constructor label in a pattern" =
  go
    {|
let opt = variant { none, some : int };;
let d = 1 @ dynamic;;
let _ = match opt.some d { .whoops -> 1, .none -> 0, .some v -> v };;
|};
  [%expect {|
    ((loc ((line 4) (column 27)))
     (reason
      (Unknown_label (from (Type (Variant ((none ()) (some ((Type Int)))))))
       (label whoops))))
    |}]
;;

let%expect_test "nullary pattern for a payload constructor" =
  go
    {|
let opt = variant { none, some : int };;
let d = 1 @ dynamic;;
let _ = match opt.some d { .some -> 1, .none -> 0 };;
|};
  [%expect {|
    ((loc ((line 4) (column 27)))
     (reason (Match (Payload_mismatch (label some) (required true)))))
    |}]
;;

let%expect_test "payload pattern for a nullary constructor" =
  go
    {|
let opt = variant { none, some : int };;
let d = 1 @ dynamic;;
let _ = match opt.some d { .none v -> v, .some v -> v };;
|};
  [%expect {|
    ((loc ((line 4) (column 27)))
     (reason (Match (Payload_mismatch (label none) (required false)))))
    |}]
;;

(* ===== Type structure and subtyping ===== *)

let%expect_test "variant type as a tuple component" =
  go
    {|
let p = ((variant { a }).a, 3) : variant { a } ^ int;;
let f = fn (x : variant { a } ^ int) -> x;;
let _ = f p;;
|};
  [%expect {| |}]
;;

(* Rows compare payload-pointwise, so mode weakening inside a payload arrow is
   admitted. *)
let%expect_test "payload modes weaken pointwise through rows" =
  go
    {|
let v = (variant { f : int -> int }).f (fn (x : int) -> x) : variant { f : int -> dynamic int };;
|};
  [%expect {| |}]
;;

let%expect_test "no width subtyping between rows" =
  go "let x = (variant { a }).a : variant { a, b };;";
  [%expect {|
    ((loc ((line 1) (column 26)))
     (reason
      (Type_mismatch (got (Type (Variant ((a ())))))
       (need (Type (Variant ((a ()) (b ()))))))))
    |}]
;;

let%expect_test "duplicate label mixing nullary and payload forms" =
  go "let t = variant { a, a : int };;";
  [%expect {| ((loc ((line 1) (column 8))) (reason (Duplicate_label a))) |}]
;;

let%expect_test "labels do not clash with variable names" =
  go
    {|
let none = 5;;
let x = (variant { none }).none;;
let y = none + 1;;
|};
  [%expect {| |}]
;;

(* ===== Selection edge cases ===== *)

let%expect_test "selection through a family alias" =
  go
    {|
let option = fn (erased t : type) -> variant { none, some : t };;
let oi = option int;;
let x = oi.some 2 : option int;;
|};
  [%expect {| |}]
;;

let%expect_test "curried family selection with concrete arguments" =
  go
    {|
let pair = fn (static erased a : type) -> fn (static erased b : type) -> variant { fst : a, snd : b };;
let x = (pair int bool).fst 1;;
let y = (pair int bool).snd true;;
let z = (pair int bool).fst 1 : pair int bool;;
|};
  [%expect {| |}]
;;

(* A curried family stuck on an abstract first argument unfolds its whole
   application spine under selection, and the declared return type unfolds the
   same way when the body is checked against it. *)
let%expect_test "curried family selection under an abstract argument" =
  go
    {|
let pair = fn (static erased a : type) -> fn (static erased b : type) -> variant { fst : a };;
fun mk (static erased t : type) : t -> pair t bool = fn (x : t) -> (pair t bool).fst x;;
let _ = mk int 5 : pair int bool;;
|};
  [%expect {| |}]
;;

(* A partial application in function position reduces to another stuck spine;
   unfolding keeps peeling it with the pending arguments. *)
let%expect_test "selection through a partially applied family alias" =
  go
    {|
let pair = fn (static erased a : type) -> fn (static erased b : type) -> variant { fst : a, snd : b };;
let pair2 = fn (static erased a : type) -> pair a;;
fun mk (static erased t : type) : t -> pair2 t bool = fn (x : t) -> (pair2 t bool).fst x;;
let _ = mk int 5 : pair int bool;;
|};
  [%expect {| |}]
;;

(* Joins and meets unfold curried spines too: an [if] joins the unfolded row
   against the stuck application (in either branch order), and joining the
   arrow types meets their stuck argument types. *)
let%expect_test "curried family unfolds under join and meet" =
  go
    {|
let pair = fn (static erased a : type) -> fn (static erased b : type) -> variant { fst : a, snd : b };;
fun mk (static erased t : type) : t -> pair t bool = fn (x : t) -> (pair t bool).fst x;;
fun pick (static erased t : type) : bool -> t -> pair t bool =
  fn (c : bool) -> fn (x : t) -> if c then (pair t bool).fst x else mk t x
;;
fun kcip (static erased t : type) : bool -> t -> pair t bool =
  fn (c : bool) -> fn (x : t) -> if c then mk t x else (pair t bool).fst x
;;
fun meets (static erased t : type) : bool -> dynamic int =
  fn (c : bool) ->
    let f = fn (v : pair t bool) -> 0 in
    let g = fn (v : variant { fst : t, snd : bool }) -> 1 in
    let h = if c then f else g in
    2
;;
|};
  [%expect {| |}]
;;

(* Joining a stuck family application with its spelled-out conditional
   unfolds first, so the two sides merge arm-wise as correlated conditionals
   instead of decomposing one side against the whole other. *)
let%expect_test "stuck application joins its spelled conditional" =
  go
    {|
fun vec (static n : int) : erased type =
  match erased n { 0 -> variant { nil }, _ -> variant { cons : int ^ vec (n - 1) } }
;;
fun pick (static n : int) : bool -> vec n -> vec n =
  fn (c : bool) ->
    fn (v : vec n) ->
      if c
      then v
      else (v : match erased n { 0 -> variant { nil }, _ -> variant { cons : int ^ vec (n - 1) } })
;;
|};
  [%expect {| |}]
;;

let%expect_test "applying a nullary constructor value" =
  go "let x = (variant { none, some : int }).none 1;;";
  [%expect {|
    ((loc ((line 1) (column 8)))
     (reason
      (Expected_function (fn (Type (Variant ((none ()) (some ((Type Int)))))))
       (arg (Type Int)))))
    |}]
;;

let%expect_test "applying a saturated constructor application" =
  go "let x = (variant { none, some : int }).some 1 2;;";
  [%expect {|
    ((loc ((line 1) (column 8)))
     (reason
      (Expected_function (fn (Type (Variant ((none ()) (some ((Type Int)))))))
       (arg (Type Int)))))
    |}]
;;

(* Record projection from a variant value is not selection. *)
let%expect_test "selection from a variant value" =
  go "let x = ((variant { none, some : int }).some 1).some;;";
  [%expect {|
    ((loc ((line 1) (column 8)))
     (reason
      (Type_mismatch (got (Type (Variant ((none ()) (some ((Type Int)))))))
       (need (Type Type)))))
    |}]
;;

(* A [type]-typed payload is legal to form but impossible to inject: type
   values are erased and the injection argument must be unerased. *)
let%expect_test "type payloads are uninhabitable" =
  go
    {|
let tv = variant { none, ty : type };;
let x = tv.ty int;;
|};
  [%expect {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

(* ===== Monomorphization keys ===== *)

(* An unerased static parameter's instance re-binds the key as a literal; an
   injection function key survives reification. *)
let%expect_test "unerased mono key on a constructor function" =
  go
    {|
let opt = variant { none, some : int };;
fun call (static f : int -> opt) : dynamic opt = f 1;;
let _ = call (opt.some);;
|};
  [%expect {| |}]
;;

(* Same shape, but the key is a constructor whose payload is a closure; reify
   rebuilds it as an injection so the payload can be quoted. *)
let%expect_test "unerased mono key on a closure payload" =
  go
    {|
let fopt = variant { none, some : int -> int };;
fun call (static o : fopt) : dynamic int = match static o { .none -> 0, .some f -> f 1 };;
let _ = call (fopt.some (fn (x : int) -> x + 1));;
|};
  [%expect {| |}]
;;

(* A family whose payload mentions the same instance never terminates. *)
let%expect_test "self-referential payload" =
  go
    {|
fun bad (static n : int) : erased type = variant { wrap : bad n };;
let x = (bad 0).wrap;;
|};
  [%expect {| ((loc ((line 2) (column 58))) (reason (Recursion_limit 1000))) |}]
;;

(* ===== Match interactions ===== *)

let%expect_test "unreachable arm of a dynamic match" =
  go
    {|
let opt = variant { none, some : int };;
let d = 1 @ dynamic;;
let _ = match opt.some d { .none -> unreachable, .some v -> v };;
|};
  [%expect {| ((loc ((line 4) (column 36))) (reason Unreachable_reached)) |}]
;;

let%expect_test "or pattern binding payloads at different types" =
  go
    {|
let dir = variant { left : int, right : bool };;
let d = 1 @ dynamic;;
let x = dir.left d;;
let _ = match x { .left v | .right v -> 0 };;
|};
  [%expect {|
    ((loc ((line 5) (column 18)))
     (reason (Cannot_unify (lhs (Type Int)) (rhs (Type Bool)))))
    |}]
;;

let%expect_test "or pattern binding a payload against the whole variant" =
  go
    {|
let opt = variant { none, some : int };;
let d = 1 @ dynamic;;
let _ = match opt.some d { .some v | v -> 0 };;
|};
  [%expect {|
    ((loc ((line 4) (column 27)))
     (reason
      (Cannot_unify (lhs (Type Int))
       (rhs (Type (Variant ((none ()) (some ((Type Int))))))))))
    |}]
;;

(* With a static scrutinee the conflicting binding becomes a conditional type;
   using it at [int] must fail on the [bool] arm. *)
let%expect_test "static or pattern with conflicting payload types" =
  go
    {|
let dir = variant { left : int, right : bool };;
fun pick (static d : dir) : dynamic int = match static d { .left v | .right v -> v @ dynamic };;
|};
  [%expect {|
    ((loc ((line 3) (column 4)))
     (reason
      (Type_mismatch
       (got
        (Match (scrutinee (Var (Anon <opaque>)))
         (arms
          (((Constructor (label left)
             (payload
              ((Var (id ((Id v) <opaque>)) (loc ((line 3) (column 65))))))
             (loc ((line 3) (column 59))))
            (Type Int))
           ((Constructor (label right)
             (payload
              ((Var (id ((Id v) <opaque>)) (loc ((line 3) (column 76))))))
             (loc ((line 3) (column 69))))
            (Type Bool))))))
       (need (Type Int)))))
    |}]
;;

let%expect_test "static or pattern payload resolves per instance" =
  go
    {|
let dir = variant { left : int, right : int };;
fun pick (static d : dir) : int = match static d { .left v | .right v -> v };;
let _ = assert erased (pick (dir.left 4) == 4);;
let _ = assert erased (pick (dir.right 7) == 7);;
|};
  [%expect {| |}]
;;

(* Or-alternatives may bind the same names along different paths; selection
   projects each instance's actual paths. *)
let%expect_test "static or pattern with swapped tuple bindings" =
  go
    {|
let t = variant { a : int ^ int, b : int ^ int };;
fun swap (static v : t) : int = match static v { .a (x, y) | .b (y, x) -> x - y };;
let _ = assert erased (swap (t.a (10, 3)) == 7);;
let _ = assert erased (swap (t.b (10, 3)) == 0 - 7);;
|};
  [%expect {| |}]
;;

let%expect_test "or alternatives in a literal payload pattern" =
  go
    {|
let opt = variant { none, some : int };;
let r = match opt.some 2 { .some (1 | 2) -> 10, .none | .some _ -> 20 };;
let _ = assert erased (r == 10);;
|};
  [%expect {| |}]
;;

let%expect_test "redundant duplicate literal payload arm" =
  go
    {|
let opt = variant { none, some : int };;
let d = 1 @ dynamic;;
let _ = match opt.some d { .some 1 -> 0, .some 1 -> 1, .some _ -> 2, .none -> 3 };;
|};
  [%expect {|
    ((loc ((line 4) (column 8)))
     (reason
      (Match
       (Redundant
        ((Constructor (label some)
          (payload ((Literal (value (Int 1)) (loc ((line 4) (column 47))))))
          (loc ((line 4) (column 41)))))))))
    |}]
;;

let%expect_test "non-exhaustive literal payload match" =
  go
    {|
let opt = variant { none, some : int };;
let d = 1 @ dynamic;;
let _ = match opt.some d { .some 1 -> 0, .none -> 1 };;
|};
  [%expect {|
    ((loc ((line 4) (column 8)))
     (reason
      (Match (Non_exhaustive ((Constructor (label some) (payload (Wildcard))))))))
    |}]
;;

let%expect_test "non-exhaustive nested variant match" =
  go
    {|
let opt = variant { none, some : int };;
let w = variant { empty, wrap : opt };;
let d = 1 @ dynamic;;
let _ = match w.wrap (opt.some d) { .wrap (.some v) -> v };;
|};
  [%expect {|
    ((loc ((line 5) (column 8)))
     (reason
      (Match
       (Non_exhaustive
        ((Constructor (label empty) (payload ()))
         (Constructor (label wrap)
          (payload ((Constructor (label none) (payload ()))))))))))
    |}]
;;

let%expect_test "duplicate bindings in a payload pattern" =
  go
    {|
let p = variant { pair : int ^ int };;
let d = 1 @ dynamic;;
let _ = match p.pair (d, d) { .pair (v, v) -> v };;
|};
  [%expect {|
    ((loc ((line 4) (column 36)))
     (reason (Match (Multiple_bindings ((Id v) <opaque>)))))
    |}]
;;

let%expect_test "literal payload pattern at the wrong type" =
  go
    {|
let opt = variant { none, some : int };;
let d = 1 @ dynamic;;
let _ = match opt.some d { .some true -> 0, .some _ -> 1, .none -> 2 };;
|};
  [%expect {|
    ((loc ((line 4) (column 33)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Bool)))))
    |}]
;;

let%expect_test "payload tuple pattern arity mismatch" =
  go
    {|
let p = variant { pair : int ^ bool };;
let d = 1 @ dynamic;;
let _ = match p.pair (d, true) { .pair (a, b, c) -> a };;
|};
  [%expect {|
    ((loc ((line 4) (column 39)))
     (reason (Match (Expected_tuple (Type (Tuple ((Type Int) (Type Bool))))))))
    |}]
;;

(* ===== Modes through matches ===== *)

let%expect_test "dynamic match on an erased scrutinee" =
  go
    {|
let opt = variant { none, some : int };;
let e = opt.some 1 @ erased;;
let _ = match e { .none -> 0, .some v -> v };;
|};
  [%expect {|
    ((loc ((line 4) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "match static on an erased scrutinee" =
  go
    {|
let opt = variant { none, some : int };;
let e = opt.some 1 @ erased;;
let _ = match static e { .none -> 0, .some v -> v };;
|};
  [%expect {|
    ((loc ((line 4) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "match static on a dynamic payload" =
  go
    {|
let opt = variant { none, some : int };;
let d = 1 @ dynamic;;
let _ = match static opt.some d { .none -> 0, .some v -> v };;
|};
  [%expect {|
    ((loc ((line 4) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "erased match binding used dynamically" =
  go
    {|
let opt = variant { none, some : int };;
let e = opt.some 1 @ erased;;
let _ = match erased e { .some v -> v @ dynamic, .none -> 0 };;
|};
  [%expect {|
    ((loc ((line 4) (column 38)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "erased match result stays erased" =
  go
    {|
let opt = variant { none, some : int };;
let e = opt.some 1 @ erased;;
let r = match erased e { .some v -> v, .none -> 0 };;
let f = fn (x : int) -> x;;
let _ = f r;;
|};
  [%expect {|
    ((loc ((line 6) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "ghost function's variant result drives erased selection" =
  go
    {|
let opt = variant { none, some : int };;
fun erased ghost (x : int) : opt = opt.some (x + 1);;
let t = match erased (ghost 2) { .some v -> int, .none -> bool };;
let _ = 5 : t;;
|};
  [%expect {| |}]
;;

(* An erased payload binding is a ghost like any erased value: even a
   compile-time comparison demands unerased arguments. *)
let%expect_test "erased payload bindings cannot feed comparisons" =
  go
    {|
let opt = variant { none, some : int };;
let t = match erased (opt.some 3 @ erased) { .some v -> (if erased v == 3 then int else bool), .none -> unit };;
|};
  [%expect {|
    ((loc ((line 3) (column 69)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

(* The unerased spelling of the same refinement, via [match static]. *)
let%expect_test "static match payload refines a type" =
  go
    {|
let opt = variant { none, some : int };;
let x = opt.some 3;;
let t = match static x { .some v -> (if erased v == 3 then int else bool), .none -> unit };;
let _ = 5 : t;;
|};
  [%expect {| |}]
;;

(* A generic consumer cannot destructure a value of stuck family type: the
   scrutinee's type was fixed before the index was refined. *)
let%expect_test "generic consumer of an indexed family" =
  go
    {|
fun vec (static n : int) : erased type =
  match erased n { 0 -> variant { nil }, _ -> variant { cons : int ^ vec (n - 1) } }
;;
fun sum (static n : int) : vec n -> dynamic int =
  fn (v : vec n) ->
    match erased n {
      0 -> 0,
      _ -> match v { .cons (h, t) -> h + sum (n - 1) t },
    }
;;
|};
  [%expect {|
    ((loc ((line 9) (column 21)))
     (reason
      (Expected_variant
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
              (eliminator Erased) (loc ((line 3) (column 2)))))
            (env <opaque>) (family <opaque>) (hash <opaque>))))
         (arg (Var (Anon <opaque>)))))
       (label cons))))
    |}]
;;
