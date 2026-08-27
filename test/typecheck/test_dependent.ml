open! Core
open! Syl

let go = Common.typecheck

let%expect_test "static lambda identity returns dependent type" =
  go
    {|
let f = fn (static x : int) -> x;;
let _ = f 42;;
|};
  [%expect {| |}]
;;

let%expect_test "static lambda with arithmetic on static arg" =
  go
    {|
let f = fn (static x : int) -> x + 1;;
let _ = f 10;;
|};
  [%expect {| |}]
;;

let%expect_test "static lambda with boolean op on static arg" =
  go
    {|
let f = fn (static x : bool) -> x && true;;
let _ = f false;;
|};
  [%expect {| |}]
;;

let%expect_test "nested static lambdas" =
  go
    {|
let f = fn (static x : int) -> fn (static y : int) -> x + y;;
let _ = f 1 2;;
|};
  [%expect {| |}]
;;

let%expect_test "static lambda returning static lambda" =
  go
    {|
let f = fn (static x : int) -> fn (static y : int) -> x;;
let g = f 1;;
let _ = g 2;;
|};
  [%expect {| |}]
;;

let%expect_test "static lambda mixed with dynamic lambda" =
  go
    {|
let f = fn (static x : int) -> fn (y : int) -> y;;
let g = f 1;;
let _ = g 2;;
|};
  [%expect {| |}]
;;

let%expect_test "dynamic lambda inside static lambda uses static arg as type" =
  go
    {|
let f = fn (static erased t : type) -> fn (x : t) -> x;;
let g = f int;;
let _ = g 42;;
let h = f bool;;
let _ = h true;;
|};
  [%expect {| |}]
;;

let%expect_test "passing dynamic value to static arg fails" =
  go
    {|
let f = fn (static x : int) -> x;;
let y = 1 @ dynamic;;
let _ = f y;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "static lambda arg type must be a type" =
  go
    {|
let f = fn (static x : 0) -> 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 8)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Type)))))
    |}]
;;

let%expect_test "erased (not static) arg cannot be used as type" =
  go
    {|
let f = fn (erased ty : type) -> fn (x : ty) -> x;;
|};
  [%expect {| |}]
;;

let%expect_test "if erased with literal condition true" =
  go
    {|
let _ = if erased true then 1 else true;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 8)))
     (reason (Dead_branch (branch Else) (value (Bool (T true))))))
    |}]
;;

let%expect_test "if erased with literal condition false" =
  go
    {|
let _ = if erased false then 1 else true;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 8)))
     (reason (Dead_branch (branch Then) (value (Bool (T false))))))
    |}]
;;

(* Not just literals: a condition that reduces to a constant while the parameter
   is still symbolic is dead in every instance. *)
let%expect_test "if erased condition decided by reduction" =
  go
    {|
let f = fn (static x : int) -> if erased x == (if true then x else 0) then 1 else true;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 31)))
     (reason (Dead_branch (branch Else) (value (Bool (T true))))))
    |}]
;;

let%expect_test "if erased with static variable condition" =
  go
    {|
let f = fn (static x : int) -> if erased x == 0 then 1 else true;;
let a = f 0;;
let b = f 1;;
|};
  [%expect {| |}]
;;

let%expect_test "if erased with mismatched branch types without annotation" =
  go
    {|
let f = fn (static x : int) -> if erased x == 0 then 1 else true;;
|};
  [%expect {| |}]
;;

let%expect_test "if erased with correct type annotation using non-erased if" =
  go
    {|
let f = fn (static x : int) -> (if erased x == 0 then 1 else true) : (if x == 0 then int else bool);;
|};
  [%expect {| |}]
;;

let%expect_test "if (non-static) requires branches to unify" =
  go
    {|
let f = fn (static x : int) -> if x == 0 then 1 else true;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 31)))
     (reason (Cannot_unify (lhs (Type Int)) (rhs (Type Bool)))))
    |}]
;;

let%expect_test "if erased wrong annotation type" =
  go
    {|
let f = fn (static x : int) -> (if erased x == 0 then 1 else true) : int;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 67)))
     (reason
      (Type_mismatch
       (got
        (Match (scrutinee (Bool (Eq (Var (Anon <opaque>)) (Int (T 0)))))
         (arms
          (((Literal (Bool true)) (Type Int))
           ((Literal (Bool false)) (Type Bool))))))
       (need (Type Int)))))
    |}]
;;

let%expect_test
    "if erased annotation uses erased if — fails because type-level if erased produces If of types"
  =
  go
    {|
let f = fn (static x : int) -> (if erased x == 0 then 1 else true) : (if erased x == 0 then int else bool);;
|};
  [%expect {| |}]
;;

let%expect_test "if erased with nested dependent types in branches" =
  go
    {|
let f = fn (static x : int) -> if erased x == 0 then fn (y : int) -> y else fn (y : bool) -> y;;
let g = f 0;;
let _ = g 42;;
let h = f 1;;
let _ = h true;;
|};
  [%expect {| |}]
;;

let%expect_test "if erased true selects then branch type" =
  go
    {|
let f = fn (static c : bool) -> (if erased c then 1 else true) : (if c then int else bool);;
let _ = (f true) : int;;
|};
  [%expect {| |}]
;;

let%expect_test "if erased false selects else branch type" =
  go
    {|
let f = fn (static c : bool) -> (if erased c then 1 else true) : (if c then int else bool);;
let _ = (f false) : bool;;
|};
  [%expect {| |}]
;;

(* A branch's fact reaches every binding: [t]'s static was computed before the
   conditional and mentions [b]; the fact context rewrites it at lookup, so each
   branch sees its own selection. *)
let%expect_test "branch facts rewrite prior bindings" =
  go
    {|
let _ = fn (static b : bool) ->
  let t = if erased b then int else bool in
  if erased b then (0 : t) else (true : t);;
|};
  [%expect {| |}]
;;

let%expect_test "dependent arrow type with backslash binder" =
  go
    {|
let f = fn (static g : static int \ x -> int) -> g 0;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent arrow applied to matching function" =
  go
    {|
let apply_type = fn (static f : static erased type \ t -> t -> t) -> f int;;
let _ = apply_type (fn (static erased t : type) -> fn (x : t) -> x);;
|};
  [%expect {| |}]
;;

let%expect_test "dependent arrow with return type depending on arg" =
  go
    {|
let mk_int = fn (static x : int) -> int;;
let f = fn (static g : static int \ x -> mk_int x) -> g 0;;
|};
  [%expect {| |}]
;;

let%expect_test "fun with static arg creates Pi type" =
  go
    {|
fun f (static x : int) : int = x;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "fun with static erased type arg — polymorphic identity" =
  go
    {|
fun id (static erased t : type) : t -> t = fn (x : t) -> x;;
let _ = id int 0;;
let _ = id bool true;;
|};
  [%expect {| |}]
;;

let%expect_test "fun with erased type arg binds for dependent ret type (erased implies static)" =
  go
    {|
fun id (erased t : type) : t -> t = fn (x : t) -> x;;
|};
  [%expect {| |}]
;;

let%expect_test "dynamic app" =
  go
    {|
fun f (x : int) : static int = 0;;
let _ = 0 : if f 0 == 0 then int else bool;;
|};
  [%expect {| |}]
;;

let%expect_test "dynamic app" =
  go
    {|
fun f (x : int) : static int = 0;;
let a = 0 @ dynamic;;
let _ = 0 : if f a == 0 then int else bool;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 10)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "fun dynamic recursion is allowed" =
  go
    {|
fun f (x : int) : int = f x;;
|};
  [%expect {| |}]
;;

let%expect_test "static cond" =
  go
    {|
let c = true @ dynamic;;
let _ = if erased c then 0 else 1;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "fun with static arg, passing dynamic value fails" =
  go
    {|
fun f (static x : int) : int = x;;
let y = 0 @ dynamic;;
let _ = f y;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "fun with static erased type arg, two sequential funs" =
  go
    {|
fun id1 (static erased t : type) : t -> t = fn (x : t) -> x;;
fun id2 (static erased t : type) : t -> t = id1 t;;
let _ = id2 int 0;;
|};
  [%expect {| |}]
;;

let%expect_test "static erased lambda captures no runtime value" =
  go
    {|
let f = fn (static erased x : int) -> 0;;
let _ = f 1;;
|};
  [%expect {| |}]
;;

let%expect_test "lift static value through Pi" =
  go
    {|
let f = fn (static x : int) -> x @ erased;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "cannot return static from function with dynamic arg via fun" =
  go
    {|
fun f (x : int) : static int = x;;
|};
  [%expect {| |}]
;;

let%expect_test "fun returning static erased type" =
  go
    {|
fun f (static _ : unit) : static erased type = int;;
let _ = 5 : f ();;
|};
  [%expect {| |}]
;;

let%expect_test "fun returning static erased type, used as annotation" =
  go
    {|
fun f (static _ : unit) : static erased type = int;;
let _ = true : f ();;
|};
  [%expect
    {|
    ((loc ((line 3) (column 13)))
     (reason (Type_mismatch (got (Type Bool)) (need (Type Int)))))
    |}]
;;

let%expect_test "pi and arrow join — if choosing between Pi and Arrow" =
  go
    {|
let f = fn (static x : int) -> if erased x == 0 then 1 else true;;
let g = fn (static x : int) -> if erased x == 0 then 1 else true;;
let _ = if true then f else g;;
|};
  [%expect {| |}]
;;

let%expect_test "pi and arrow cannot join if return types differ structurally" =
  go
    {|
let f = fn (static x : int) -> if erased x == 0 then 1 else true;;
let g = fn (static x : int) -> if erased x == 0 then true else 2;;
let _ = if true then f else g;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason
      (Cannot_unify
       (lhs
        (Type
         (Pi (arg_ty (Type Int))
          (arg_mode ((staticity Static) (erasure Unerased)))
          (ret_ty
           (Typecheck (env <opaque>) (arg ((Id x) <opaque>)) (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (body
             (If
              (cond
               (Apply
                (fn
                 (Var (id ((Binop Eq) <opaque>)) (loc ((line 2) (column 43)))))
                (arg
                 (Make_tuple
                  (elts
                   ((Var (id ((Id x) <opaque>)) (loc ((line 2) (column 41))))
                    (Literal (value (Int 0)) (loc ((line 2) (column 46))))))
                  (loc ((line 2) (column 43)))))
                (loc ((line 2) (column 43)))))
              (then_ (Literal (value (Int 1)) (loc ((line 2) (column 53)))))
              (else_ (Literal (value (Bool true)) (loc ((line 2) (column 60)))))
              (erased Erased) (loc ((line 2) (column 31)))))
            (memo <opaque>) (uid <opaque>)))
          (ret_mode ((staticity Static) (erasure Unerased))))))
       (rhs
        (Type
         (Pi (arg_ty (Type Int))
          (arg_mode ((staticity Static) (erasure Unerased)))
          (ret_ty
           (Typecheck (env <opaque>) (arg ((Id x) <opaque>)) (arg_ty (Type Int))
            (arg_mode ((staticity Static) (erasure Unerased)))
            (body
             (If
              (cond
               (Apply
                (fn
                 (Var (id ((Binop Eq) <opaque>)) (loc ((line 3) (column 43)))))
                (arg
                 (Make_tuple
                  (elts
                   ((Var (id ((Id x) <opaque>)) (loc ((line 3) (column 41))))
                    (Literal (value (Int 0)) (loc ((line 3) (column 46))))))
                  (loc ((line 3) (column 43)))))
                (loc ((line 3) (column 43)))))
              (then_ (Literal (value (Bool true)) (loc ((line 3) (column 53)))))
              (else_ (Literal (value (Int 2)) (loc ((line 3) (column 63)))))
              (erased Erased) (loc ((line 3) (column 31)))))
            (memo <opaque>) (uid <opaque>)))
          (ret_mode ((staticity Static) (erasure Unerased)))))))))
    |}]
;;

let%expect_test "joining f 0 and g 1 resolves dependent types" =
  go
    {|
let f = fn (static x : int) -> if erased x == 0 then 1 else true;;
let g = fn (static x : int) -> if erased x == 0 then true else 2;;
let _ = if true then f 0 else g 1;;
|};
  [%expect {| |}]
;;

let%expect_test "apply static lambda to wrong type" =
  go
    {|
let f = fn (static x : int) -> x;;
let _ = f true;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason (Type_mismatch (got (Type Bool)) (need (Type Int)))))
    |}]
;;

let%expect_test "nested if erased with different types per level" =
  go
    {|
let f = fn (static x : int) -> fn (static y : int) ->
  if erased x == 0 then
    (if erased y == 0 then 1 else true)
  else
    (if erased y == 0 then () else 2);;
let _ = f 0 0;;
let _ = f 0 1;;
let _ = f 1 0;;
let _ = f 1 1;;
|};
  [%expect {| |}]
;;

let%expect_test "static lambda unused arg" =
  go
    {|
let f = fn (static _ : int) -> 42;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent type: apply polymorphic id to itself" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = (id (int -> int)) (fn (x : int) -> x) 5;;
|};
  [%expect {| |}]
;;

let%expect_test "if erased with bool static arg" =
  go
    {|
let f = fn (static b : bool) -> if erased b then 1 else true;;
let _ = f true;;
let _ = f false;;
|};
  [%expect {| |}]
;;

let%expect_test "static arg used in arithmetic, result applied" =
  go
    {|
let double = fn (static x : int) -> x + x;;
let _ = double 5;;
|};
  [%expect {| |}]
;;

let%expect_test "chaining dependent applications" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let f = id int;;
let g = id bool;;
let _ = f 0;;
let _ = g true;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent type mismatch: pass bool where int expected in body" =
  go
    {|
let f = fn (static erased t : type) -> fn (x : t) -> x;;
let g = f int;;
let _ = g true;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason (Type_mismatch (got (Type Bool)) (need (Type Int)))))
    |}]
;;

let%expect_test "symbolic arrow type as static arg" =
  go
    {|
let choose = fn (static f : static int \ x -> if x == 0 then int else bool) -> f 0;;
let _ = choose (fn (static x : int) -> if erased x == 0 then 0 else true);;
|};
  [%expect {| |}]
;;

let%expect_test "static lambda body references outer let binding" =
  go
    {|
let n = 10;;
let f = fn (static x : int) -> x + n;;
let _ = f 5;;
|};
  [%expect {| |}]
;;

let%expect_test "static lambda with type annotation on body" =
  go
    {|
let f = fn (static x : int) -> (x : int);;
let _ = f 42;;
|};
  [%expect {| |}]
;;

let%expect_test "if erased in type annotation position" =
  go
    {|
let f = fn (static b : bool) -> (if erased b then 0 else true) : (if b then int else bool);;
let _ = f true;;
let _ = f false;;
|};
  [%expect {| |}]
;;

let%expect_test "higher-order static: take a static function and apply it" =
  go
    {|
let apply = fn (static f : static int -> int) -> f 5;;
let _ = apply (fn (static x : int) -> x + 1);;
|};
  [%expect {| |}]
;;

let%expect_test "static erased type arg with wrong application type" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = id 0;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Type)))))
    |}]
;;

let%expect_test "multiple static erased type args" =
  go
    {|
let f = fn (static erased t1 : type) -> fn (static erased t2 : type) -> fn (x : t1) -> fn (y : t2) -> x;;
let _ = f int bool 0 true;;
|};
  [%expect {| |}]
;;

let%expect_test "if erased nested in let expression" =
  go
    {|
let f = fn (static x : int) ->
  let y = if erased x == 0 then 1 else true in
  y;;
let _ = f 0;;
let _ = f 1;;
|};
  [%expect {| |}]
;;

let%expect_test "cannot use dynamic erased as condition" =
  go
    {|
let x = true @ dynamic erased;;
let _ = if x then 1 else 2;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "inline case 1: binder with captured static var" =
  go
    {|
let f = fn (static x : int) -> fn (static y : int) -> x + y;;
let _ = f 10 20;;
|};
  [%expect {| |}]
;;

let%expect_test "inline case 1: triple nested binder" =
  go
    {|
let f = fn (static x : int) -> fn (static y : int) -> fn (static z : int) -> x + y + z;;
let _ = f 1 2 3;;
|};
  [%expect {| |}]
;;

let%expect_test "inline case 3: erased closure capturing static var" =
  go
    {|
let mk = fn (static a : int) -> (fn (y : int) -> a + y) @ erased;;
let f = mk 10;;
let _ = f 5;;
|};
  [%expect {| |}]
;;

let%expect_test "inline case 3: unerased closure capturing static var" =
  go
    {|
let mk = fn (static a : int) -> (fn (y : int) -> a + y);;
let f = mk 10;;
let _ = f 5;;
|};
  [%expect {| |}]
;;

let%expect_test "ghost closure applied for static result, two captures" =
  go
    {|
let mk = fn (static a : int) -> fn (static b : int) -> (fn (y : int) -> a + b + y) @ erased;;
let f = mk 10 20;;
let _ = f 5;;
|};
  [%expect {| |}]
;;

let%expect_test "inline case 3: unerased closure capturing two static vars" =
  go
    {|
let mk = fn (static a : int) -> fn (static b : int) -> (fn (y : int) -> a + b + y);;
let f = mk 10 20;;
let _ = f 5;;
|};
  [%expect {| |}]
;;

let%expect_test "inline case 1: binder with erased captured var" =
  go
    {|
let f = fn (static erased x : int) -> fn (static y : int) -> y;;
let _ = f 10 20;;
|};
  [%expect {| |}]
;;

let%expect_test "inline rebinds captured var shadowed at call site" =
  go
    {|
let x = 1;;
let f = fn (static a : int) -> x + a;;
let x = 2;;
let _ = f 10;;
|};
  [%expect {| |}]
;;

let%expect_test "smart constructors" =
  go
    {|
let f = fn (static x : int) -> fn (_ : int) -> true;;
let g = fn (static x : int) -> fn (_ : int) -> true;;
let _ = if true then f else g;;
|};
  [%expect {| |}]
;;

let%expect_test "smart constructors" =
  go
    {|
let f = fn (static x : int) -> fn (_ : int) -> true;;
let g = fn (static x : int) -> fn (static _ : int) -> true;;
let _ = if true then f else g;;
|};
  [%expect {| |}]
;;

let%expect_test "smart constructors" =
  go
    {|
let f = fn (static x : int) -> fn (static _ : int) -> true;;
let g = fn (static x : int) -> fn (_ : int) -> true;;
let _ = if true then f else g;;
|};
  [%expect {| |}]
;;

let%expect_test "smart constructors" =
  go
    {|
let f = fn (static x : int) -> fn (static _ : int) -> true;;
let g = fn (static x : int) -> fn (static _ : int) -> true;;
let _ = if true then f else g;;
|};
  [%expect {| |}]
;;

let%expect_test "smart constructors" =
  go
    {|
let f = fn (static x : int -> int) -> ();;
let g = fn (static x : int -> int) -> ();;
let _ = if true then f else g;;
|};
  [%expect {| |}]
;;

let%expect_test "smart constructors" =
  go
    {|
let f = fn (static x : static int -> int) -> ();;
let g = fn (static x : int -> int) -> ();;
let _ = if true then f else g;;
|};
  [%expect {| |}]
;;

let%expect_test "smart constructors" =
  go
    {|
let f = fn (static x : int -> int) -> ();;
let g = fn (static x : static int -> int) -> ();;
let _ = if true then f else g;;
|};
  [%expect {| |}]
;;

let%expect_test "smart constructors" =
  go
    {|
let f = fn (static x : static int -> int) -> ();;
let g = fn (static x : static int -> int) -> ();;
let _ = if true then f else g;;
|};
  [%expect {| |}]
;;

let%expect_test "smart constructors" =
  go
    {|
let f = fn (static x : int -> static int) -> ();;
let g = fn (static x : int -> int) -> ();;
let _ = if true then f else g;;
|};
  [%expect {| |}]
;;

let%expect_test "smart constructors" =
  go
    {|
let f = fn (static x : int -> int) -> ();;
let g = fn (static x : int -> static int) -> ();;
let _ = if true then f else g;;
|};
  [%expect {| |}]
;;

let%expect_test "smart constructors" =
  go
    {|
let f = fn (static x : int -> static int) -> ();;
let g = fn (static x : int -> static int) -> ();;
let _ = if true then f else g;;
|};
  [%expect {| |}]
;;

let%expect_test "smart constructors" =
  go
    {|
let f = fn (static x : static int -> int) -> ();;
let g = fn (static x : int -> static int) -> ();;
let _ = if true then f else g;;
|};
  [%expect {| |}]
;;

let%expect_test "assert erased" =
  go
    {|
let _ = fn (static x : bool) -> if erased x then () else assert erased x;;
  |};
  [%expect
    {|
    ((loc ((line 2) (column 57)))
     (reason (Static_failure (Assert_failed (Bool (T false))))))
    |}]
;;

let%expect_test "static bool reduction" =
  go
    {|
let _ = fn (static x : bool) ->
  let _ = assert erased !(x && false) in
  let _ = assert erased !(false && x) in
  let _ = assert erased !(x && !x) in
  let _ = assert erased (x || true) in
  let _ = assert erased (true || x) in
  let _ = assert erased (x || !x) in
  let _ = assert erased (!(!x) || !x) in
  let _ = assert erased ((x || false) || !x) in
  let _ = assert erased ((false || x) || !x) in
  let _ = assert erased (!x || (x || false)) in
  let _ = assert erased (!x || (false || x)) in
  let _ = assert erased ((x && true) || !x) in
  let _ = assert erased ((true && x) || !x) in
  let _ = assert erased (!x || (x && true)) in
  let _ = assert erased (!x || (true && x)) in
  ()
;;
  |};
  [%expect {| |}]
;;

let%expect_test "static int reduction" =
  go
    {|
let _ = fn (static x : int) ->
  let _ = assert erased (x == x) in
  let _ = assert erased !(x != x) in
  let _ = assert erased !(x > x) in
  let _ = assert erased (x >= x) in
  let _ = assert erased !(x < x) in
  let _ = assert erased (x <= x) in
  let _ = assert erased (x + 0 == x) in
  let _ = assert erased (0 + x == x) in
  let _ = assert erased (x - x == 0) in
  let _ = assert erased (x - 0 == x) in
  let _ = assert erased (0 - x == -x) in
  let _ = assert erased (1 - (-x) == 1 + x) in
  let _ = assert erased (x * 1 == x) in
  let _ = assert erased (1 * x == x) in
  let _ = assert erased (x / 1 == x) in
  let _ = assert erased (x / -1 == -x) in
  let _ = assert erased (x == x / 1) in
  let _ = assert erased (-x == x / -1) in
  let _ = assert erased (x % 1 == 0) in
  let _ = assert erased (-(-x) == x) in
  let _ = assert erased (x == -(-x)) in
  let _ = assert erased (x * -1 == -x) in
  let _ = assert erased (-x == x * -1) in
  let _ = assert erased (-1 * x == -x) in
  let _ = assert erased (-x == -1 * x) in
  ()
;;
  |};
  [%expect {| |}]
;;

(* x % x (like x / x) is not folded to a constant: both trap at x = 0, so the
   identity does not hold for every instance. *)
let%expect_test "same-operand division identities are not assumed" =
  go
    {|
let _ = fn (static x : int) ->
  let _ = assert erased (x % x == 0) in
  ()
;;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 10)))
     (reason
      (Static_failure
       (Assert_failed
        (Bool
         (Eq (Int (Mod (Var (Anon <opaque>)) (Var (Anon <opaque>)))) (Int (T 0))))))))
    |}]
;;

let%expect_test "static div by zero" =
  go
    {|
let _ = assert erased (0 / 0 == 0);;
  |};
  [%expect
    {|
    ((loc ((line 2) (column 25)))
     (reason (Static_failure (Divide_by_zero (Div (Int (T 0)) (Int (T 0)))))))
    |}]
;;

let%expect_test "static mod by zero" =
  go
    {|
let _ = assert erased (0 % 0 == 0);;
  |};
  [%expect
    {|
    ((loc ((line 2) (column 25)))
     (reason (Static_failure (Divide_by_zero (Mod (Int (T 0)) (Int (T 0)))))))
    |}]
;;

let%expect_test "static div by zero" =
  go
    {|
let _ = fn (static x : int) -> assert erased (x / 0 == 1);;
  |};
  [%expect
    {|
    ((loc ((line 2) (column 48)))
     (reason
      (Static_failure (Divide_by_zero (Div (Var (Anon <opaque>)) (Int (T 0)))))))
    |}]
;;

let%expect_test "static mod by zero" =
  go
    {|
let _ = fn (static x : int) -> assert erased (x % 0 == 1);;
  |};
  [%expect
    {|
    ((loc ((line 2) (column 48)))
     (reason
      (Static_failure (Divide_by_zero (Mod (Var (Anon <opaque>)) (Int (T 0)))))))
    |}]
;;

let%expect_test "var renaming" =
  go
    {|
let _ = fn (static x : bool) ->
  let y = !x in
  let z = if true then y else !y in
  assert erased (x || z)
;;
  |};
  [%expect {| |}]
;;

let%expect_test "unreachable in else branch of if erased" =
  go
    {|
let f = fn (static x : bool) -> if erased x then 42 else unreachable;;
let _ = f true;;
|};
  [%expect {| |}]
;;

let%expect_test "unreachable in then branch of if erased" =
  go
    {|
let f = fn (static x : bool) -> if erased x then unreachable else 42;;
let _ = f false;;
|};
  [%expect {| |}]
;;

let%expect_test "unreachable reached raises error" =
  go
    {|
let f = fn (static x : bool) -> if erased x then 42 else unreachable;;
let _ = f false;;
|};
  [%expect {| ((loc ((line 2) (column 57))) (reason Unreachable_reached)) |}]
;;

let%expect_test "unreachable with mismatched branch types" =
  go
    {|
let f = fn (static x : bool) -> if erased x then 42 else unreachable;;
let _ = f true;;
|};
  [%expect {| |}]
;;

let%expect_test "unreachable in fun partial function" =
  go
    {|
fun f (static x : int) : int = if erased x == 0 then 1 else unreachable;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "unreachable reached in fun" =
  go
    {|
fun f (static x : int) : int = if erased x == 0 then 1 else unreachable;;
let _ = f 1;;
|};
  [%expect {| ((loc ((line 2) (column 60))) (reason Unreachable_reached)) |}]
;;

let%expect_test "unreachable with function type" =
  go
    {|
let f = fn (static x : bool) ->
  if erased x then fn (y : int) -> y else unreachable;;
let g = f true;;
let _ = g 42;;
|};
  [%expect {| |}]
;;

let%expect_test "unreachable in nested if erased" =
  go
    {|
let f = fn (static x : int) ->
  if erased x == 0 then 42
  else if erased x == 1 then 99
  else unreachable;;
let _ = f 0;;
let _ = f 1;;
|};
  [%expect {| |}]
;;

let%expect_test "unreachable reached in nested if erased" =
  go
    {|
let f = fn (static x : int) ->
  if erased x == 0 then 42
  else if erased x == 1 then 99
  else unreachable;;
let _ = f 2;;
|};
  [%expect {| ((loc ((line 5) (column 7))) (reason Unreachable_reached)) |}]
;;

let%expect_test "unreachable with polymorphic identity — only valid branch used" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let f = fn (static x : bool) ->
  if erased x then id int 42 else unreachable;;
let _ = f true;;
|};
  [%expect {| |}]
;;

let%expect_test "unreachable does not poison mode of if erased" =
  go
    {|
let f = fn (static x : bool) -> if erased x then 42 else unreachable;;
let _ = f true + 1;;
|};
  [%expect {| |}]
;;

let%expect_test "unreachable at top level is rejected" =
  go
    {|
let _ = unreachable;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 8)))
     (reason (Misplaced_unreachable Not_in_tail_position)))
    |}]
;;

let%expect_test "unreachable at top level rejected before type check" =
  go
    {|
let _ = unreachable;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 8)))
     (reason (Misplaced_unreachable Not_in_tail_position)))
    |}]
;;

let%expect_test "unreachable in both branches of if erased" =
  go
    {|
let f = fn (static x : bool) ->
  if erased x then unreachable else unreachable;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 2)))
     (reason (Misplaced_unreachable All_paths_unreachable)))
    |}]
;;

let%expect_test "unreachable in nested binder defined during monomorphization" =
  go
    {|
let f = fn (static x : bool) ->
  let g = fn (static y : int) -> if erased y == 0 then 42 else unreachable in
  if erased x then g 0 else 99;;
let _ = f true;;
|};
  [%expect {| |}]
;;

let%expect_test "unreachable in nested binder defined during monomorphization" =
  go
    {|
let f = fn (static x : bool) ->
  let g = fn (static y : int) -> if erased y == 0 then 42 else unreachable in
  if erased x then g 0 else 1;;
let _ = f true;;
|};
  [%expect {| |}]
;;

let%expect_test "unreachable in inner binder, reached via inner monomorphization" =
  go
    {|
let f = fn (static x : bool) ->
  let g = fn (static y : int) -> if erased y == 0 then 42 else unreachable in
  if erased x then g 0 else g 1;;
let _ = f false;;
|};
  [%expect {| ((loc ((line 3) (column 63))) (reason Unreachable_reached)) |}]
;;

let%expect_test "unreachable in dynamic lambda inside static lambda" =
  go
    {|
let f = fn (static x : bool) ->
  if erased x then fn (y : int) -> y else unreachable;;
let _ = f true 42;;
|};
  [%expect {| |}]
;;

let%expect_test "unreachable in dependent arrow return type — not concrete during definition" =
  go
    {|
let f = fn (static g : static bool \ x -> if x then int else unreachable) -> g true;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 61)))
     (reason (Misplaced_unreachable Not_under_static_branch)))
    |}]
;;

let%expect_test "unreachable in dependent arrow return type with if erased — not concrete" =
  go
    {|
let f = fn (static g : static bool \ x -> if erased x then int else unreachable) -> g true;;
|};
  [%expect {| |}]
;;

let%expect_test "unreachable in dependent arrow return type — fires during dependent eval on apply" =
  go
    {|
let f = fn (static g : static bool \ x -> if erased x then int else unreachable) -> g true;;
let _ = f (fn (static x : bool) -> if erased x then 42 else unreachable);;
|};
  [%expect {| |}]
;;

let%expect_test "unreachable in fun return type annotation — fires during body validation" =
  go
    {|
fun f (static x : bool) : if erased x then int else unreachable = if erased x then 42 else unreachable;;
let _ = f true;;
|};
  [%expect {| |}]
;;

let%expect_test
    "unreachable in fun return type annotation { non-erased if — fires during definition }"
  =
  go
    {|
fun f (static x : bool) : if x then int else unreachable = if erased x then 42 else unreachable;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 45)))
     (reason (Misplaced_unreachable Not_under_static_branch)))
    |}]
;;

let%expect_test "unreachable in fun body — not concrete during definition" =
  go
    {|
fun f (static x : bool) : int = if erased x then 42 else unreachable;;
let _ = f true;;
|};
  [%expect {| |}]
;;

let%expect_test "unreachable in fun body — reached when applied with bad arg" =
  go
    {|
fun f (static x : bool) : int = if erased x then 42 else unreachable;;
let _ = f false;;
|};
  [%expect {| ((loc ((line 2) (column 57))) (reason Unreachable_reached)) |}]
;;

let%expect_test "unreachable in lambda body Pi computation — not concrete" =
  go
    {|
let f = fn (static x : bool) -> if erased x then 42 else unreachable;;
|};
  [%expect {| |}]
;;

let%expect_test "unreachable in fun returning lambda — not concrete during definition" =
  go
    {|
fun outer (static x : bool) : int -> int =
  if erased x then fn (y : int) -> y else unreachable;;
let g = outer true;;
let _ = g 42;;
|};
  [%expect {| |}]
;;

let%expect_test "unreachable in fun returning lambda — reached on application" =
  go
    {|
fun outer (static x : bool) : int -> int =
  if erased x then fn (y : int) -> y else unreachable;;
let _ = outer false;;
|};
  [%expect {| ((loc ((line 3) (column 42))) (reason Unreachable_reached)) |}]
;;

let%expect_test "unreachable in dependent arrow return type — non-erased if survives definition" =
  go
    {|
let f = fn (static g : static bool \ x -> if x then int else unreachable) -> ();;
|};
  [%expect
    {|
    ((loc ((line 2) (column 61)))
     (reason (Misplaced_unreachable Not_under_static_branch)))
    |}]
;;

let%expect_test "unreachable in dependent arrow return type — non-erased if survives definition" =
  go
    {|
let f = static bool \ x -> if erased x then if x then int else unreachable else unreachable;;
|};
  [%expect {| |}]
;;

let%expect_test "unreachable leq — Bottom type accepted where int expected" =
  go
    {|
let f = fn (static x : bool) -> if erased x then 42 else (unreachable : int);;
let _ = f true;;
|};
  [%expect {| |}]
;;

(* An argument is not a branch tail: passing a dead value is dead code. *)
let%expect_test "unreachable cannot be a function argument" =
  go
    {|
let f = fn (static x : bool) ->
  let g = fn (y : int) -> y + 1 in
  if erased x then g 42 else g unreachable;;
let _ = f true;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 31)))
     (reason (Misplaced_unreachable Not_in_tail_position)))
    |}]
;;

let%expect_test "unreachable leq — Bottom type accepted where function type expected" =
  go
    {|
let f = fn (static x : bool) ->
  if erased x then fn (y : int) -> y else unreachable;;
let g = f true;;
let _ = g 42;;
|};
  [%expect {| |}]
;;

let%expect_test "unreachable join — non-erased if joins Bottom with int (no application)" =
  go
    {|
let f = fn (static x : bool) ->
  if erased x then (if true then 42 else unreachable) else 99;;
|};
  [%expect {| |}]
;;

let%expect_test "unreachable join — non-erased if joins int with Bottom (no application)" =
  go
    {|
let f = fn (static x : bool) ->
  if erased x then (if true then unreachable else 42) else 99;;
|};
  [%expect {| |}]
;;

let%expect_test "unreachable join — non-erased if with unreachable, applied" =
  go
    {|
let f = fn (static x : bool) ->
  if erased x then (if true then 42 else unreachable) else 99;;
let _ = f true;;
|};
  [%expect {| ((loc ((line 3) (column 41))) (reason Unreachable_reached)) |}]
;;

let%expect_test "unreachable leq — Bottom in Pi return type comparison" =
  go
    {|
let f = fn (static x : bool) -> if erased x then 42 else unreachable;;
let g = fn (static x : bool) -> if erased x then 42 else 99;;
let _ = if true then f else g;;
|};
  [%expect {| |}]
;;

let%expect_test "if erased" =
  go
    {|
let f = fn (erased x : bool) -> if x then 1 else 2;;
let _ = f true;;
let _ = f false;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 32)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "pi capturing dynamic cannot be applied" =
  go
    {|
let outer = 1000 @ dynamic;;
let f = fn (static n : int) -> outer + n;;
let _ = print_int (f 1);;
|};
  [%expect
    {|
    ((loc ((line 4) (column 19)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "pi returning closure that captures dyn" =
  go
    {|
let outer = 1000 @ dynamic;;
let mk = fn (static n : int) -> fn (y : int) -> outer + n + y;;
let _ = print_int (mk 5 10);;
|};
  [%expect
    {|
    ((loc ((line 4) (column 19)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "pi capturing parametric (from outer fn arg)" =
  go
    {|
let f = fn (dyn : int) ->
  let g = fn (static n : int) -> n + dyn in
  g 5;;
let _ = print_int (f 10);;
|};
  [%expect
    {|
    ((loc ((line 4) (column 2)))
     (reason
      (Mode_mismatch (got ((staticity Parametric) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

(* Each true-world entails [t := unit] etc. from the [is_*] condition, so the branch
   checks at [erased t -> t]. The typecase spelling is [match erased repr t]. *)
let%expect_test "polymorphic unerase" =
  go
    {|
builtin is_unit = syl_type_is_unit;;
builtin is_bool = syl_type_is_bool;;
builtin is_int = syl_type_is_int;;

fun erased unerase (erased t : type) : erased (erased t -> t) =
  if erased is_unit t then unerase_unit
  else if erased is_bool t then unerase_bool
  else if erased is_int t then unerase_int
  else unreachable
;;
|};
  [%expect {| |}]
;;

let%expect_test "match static literal scrutinee shadows later arms" =
  go
    {|
let _ =
  (match static 0 {
     0 -> 1,
     _ -> true,
   })
    : int
;;
|};
  [%expect
    {|
    ((loc ((line 5) (column 5)))
     (reason
      (Dead_branch
       (branch (Arm (Var (id (Anon <opaque>)) (loc ((line 5) (column 5))))))
       (value (Int (T 0))))))
    |}]
;;

let%expect_test "match static literal scrutinee refutes earlier arms" =
  go
    {|
let _ =
  (match static 3 {
     0 -> 1,
     _ -> true,
   })
    : bool
;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 5)))
     (reason
      (Dead_branch
       (branch (Arm (Literal (value (Int 0)) (loc ((line 4) (column 5))))))
       (value (Int (T 3))))))
    |}]
;;

let%expect_test "match static with static variable scrutinee" =
  go
    {|
let f =
  fn (static x : int) ->
    match static x {
      0 -> 1,
      _ -> true,
    }
;;

let a = f 0;;

let b = f 1;;
|};
  [%expect {| |}]
;;

let%expect_test "match static annotated with the equivalent match" =
  go
    {|
let f =
  fn (static x : int) ->
    (match static x {
       0 -> 1,
       _ -> true,
     })
      : (match static x {
           0 -> int,
           _ -> bool,
         })
;;
|};
  [%expect {| |}]
;;

let%expect_test "match static wrong annotation shows the stuck case" =
  go
    {|
let f =
  fn (static x : int) ->
    (match static x {
       0 -> 1,
       _ -> true,
     })
      : int
;;
|};
  [%expect
    {|
    ((loc ((line 8) (column 6)))
     (reason
      (Type_mismatch
       (got
        (Match (scrutinee (Var (Anon <opaque>)))
         (arms (((Literal (Int 0)) (Type Int)) (Wild (Type Bool))))))
       (need (Type Int)))))
    |}]
;;

let%expect_test "match static refines the scrutinee value inside arms" =
  go
    {|
let g = fn (static n : int) -> fn (x : if n == 0 then int else bool) -> x;;

let h =
  fn (static n : int) ->
    match static n {
      0 -> g n 5,
      1 -> g n true,
      _ -> 0,
    }
;;

let _ = h 0;;

let _ = h 1;;

let _ = h 2;;
|};
  [%expect {| |}]
;;

let%expect_test "match static refines tuple components" =
  go
    {|
let g = fn (static n : int) -> fn (x : if n == 0 then int else bool) -> x;;

let h =
  fn (static p : int ^ int) ->
    match static p {
      (0, y) -> g 0 y,
      _ -> 0,
    }
;;

let _ = h ((0, 5) @ static);;

let _ = h ((1, 5) @ static);;
|};
  [%expect {| |}]
;;

let%expect_test "tuple scrutinees refine their components inside arms" =
  go
    {|
let g = fn (static n : int) -> fn (x : if n == 0 then int else bool) -> x;;

let h =
  fn (static a : int) ->
    fn (static b : int) ->
      match static (a, b) {
        (0, y) -> g a 5,
        _ -> 0,
      }
;;

let _ = h 0 1;;
|};
  [%expect {| |}]
;;

let%expect_test "or-pattern arms get no scrutinee refinement" =
  go
    {|
let g = fn (static m : int) -> fn (x : if m < 2 then int else bool) -> x;;

let h =
  fn (static n : int) ->
    match static n {
      0 | 1 -> g n 5,
      _ -> 0,
    }
;;
|};
  [%expect
    {|
    ((loc ((line 7) (column 15)))
     (reason
      (Type_mismatch (got (Type Int))
       (need
        (Match (scrutinee (Bool (Lt (Var (Anon <opaque>)) (Int (T 2)))))
         (arms
          (((Literal (Bool true)) (Type Int))
           ((Literal (Bool false)) (Type Bool)))))))))
    |}]
;;

let%expect_test "prims do not reduce over stuck matches" =
  go
    {|
let f =
  fn (static x : int) ->
    (0
       : if ((match static x {
                0 -> 1,
                _ -> 2,
              })
             < 3)
       then int
       else unit)
;;
|};
  [%expect
    {|
    ((loc ((line 5) (column 7)))
     (reason
      (Type_mismatch (got (Type Int))
       (need
        (Match
         (scrutinee
          (Bool
           (Lt
            (Match (scrutinee (Var (Anon <opaque>)))
             (arms (((Literal (Int 0)) (Int (T 1))) (Wild (Int (T 2))))))
            (Int (T 3)))))
         (arms
          (((Literal (Bool true)) (Type Int))
           ((Literal (Bool false)) (Type Unit)))))))))
    |}]
;;
