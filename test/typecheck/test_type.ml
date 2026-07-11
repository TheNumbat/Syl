open! Core
open! Syl

let go = Common.typecheck

let%expect_test "literals" =
  go
    {|
let _ = ();;
let _ = true;;
let _ = 123;;
let _ = () @ erased;;
let _ = true @ erased;;
let _ = 123 @ erased;;
let _ = () @ dynamic;;
let _ = true @ dynamic;;
let _ = 123 @ dynamic;;|};
  [%expect {| |}]
;;

let%expect_test "function error" =
  go
    {|
let _ = 0 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 8)))
     (reason (Expected_function (fn (Type Int)) (arg (Type Int)))))
    |}]
;;

let%expect_test "primitive" =
  go
    {|
builtin add = syl_int_add;;
let x = (add (1, 2)) @ static;;
let _ = assert erased (x == 3);;
|};
  [%expect {| |}]
;;

let%expect_test "Mode annotation valid static" =
  go
    {|
let _ =
  1 @ static
;;|};
  [%expect {| |}]
;;

let%expect_test "Mode annotation valid dynamic" =
  go
    {|
let _ =
  1 @ dynamic
;;|};
  [%expect {| |}]
;;

let%expect_test "Mode annotation invalid" =
  go
    {|
let dyn = 1 @ dynamic;;
let _ =
  dyn @ static
;;|};
  [%expect
    {|
    ((loc ((line 4) (column 6)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "Mode annotation valid" =
  go
    {|
let dyn = 1;;
let _ =
  dyn @ dynamic erased
;;|};
  [%expect {| |}]
;;

let%expect_test "Mode annotation valid" =
  go
    {|
let dyn = 1 @ erased;;
let _ =
  dyn @ dynamic erased
;;|};
  [%expect {| |}]
;;

let%expect_test "Mode annotation valid" =
  go
    {|
let dyn = 1;;
let _ =
  dyn @ dynamic
;;|};
  [%expect {| |}]
;;

let%expect_test "Mode annotation valid" =
  go
    {|
let dyn = 1 @ dynamic;;
let _ =
  dyn @ dynamic erased
;;|};
  [%expect {| |}]
;;

let%expect_test "Lambda return dynamic erased" =
  go
    {|
let x =
  (fn (x : int) -> 1 @ dynamic erased) 0
;;
let y = x @ static unerased;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 2)))
     (reason
      (Erased_application
       (fn
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret_ty (Type Int))
          (ret_mode ((staticity Dynamic) (erasure Erased))))))
       (result ((staticity Dynamic) (erasure Erased))))))
    |}]
;;

let%expect_test "Lambda return dynamic erased" =
  go
    {|
let x =
  ((fn (x : int) -> 1 @ dynamic erased) @ dynamic) 0
;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 40)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "Lambda return dynamic unerased" =
  go
    {|
let x =
  (fn (x : int) -> x) 0
;;
let y = x @ static unerased;;
|};
  [%expect {| |}]
;;

let%expect_test "Lambda capture static erased" =
  go
    {|
let x =
  (fn (x : int) -> int) 0
;;
let y = x @ unerased;;
|};
  [%expect
    {|
    ((loc ((line 5) (column 10)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "Unop static" =
  go
    {|
let _ =
  !true
;;|};
  [%expect {| |}]
;;

let%expect_test "Unop dynamic" =
  go
    {|
let _ =
  !(true @ dynamic)
;;|};
  [%expect {| |}]
;;

let%expect_test "dynamic static erased" =
  go
    {|
let _ =
  (true @ static erased)
;;|};
  [%expect {| |}]
;;

let%expect_test "dynamic erased" =
  go
    {|
let _ =
  (true @ dynamic erased)
;;|};
  [%expect {| |}]
;;

let%expect_test "erased dynamic" =
  go
    {|
let _ =
  ((true @ erased) @ dynamic)
;;|};
  [%expect
    {|
    ((loc ((line 3) (column 19)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "Unop var static" =
  go
    {|
let dyn = true;;
let _ =
  !dyn
;;|};
  [%expect {| |}]
;;

let%expect_test "Unop var dynamic" =
  go
    {|
let dyn = true @ dynamic;;
let _ =
  !dyn
;;|};
  [%expect {| |}]
;;

let%expect_test "Unop var dynamic erased" =
  go
    {|
let dyn = true @ erased dynamic;;
let _ = !dyn;;|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "Unop var dynamic" =
  go
    {|
let dyn = true @ dynamic;;
let x = !dyn @ erased;;|};
  [%expect
    {|
    ((loc ((line 3) (column 13)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "Binop static + static" =
  go
    {|
let _ =
  1 + 2
;;|};
  [%expect {| |}]
;;

let%expect_test "Binop static + static erased" =
  go
    {|
let _ =
  1 + (2 @ erased)
;;|};
  [%expect
    {|
    ((loc ((line 3) (column 4)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "Unop erased dynamic" =
  go
    {|
let _ =
  !(true @ erased dynamic)
;;|};
  [%expect
    {|
    ((loc ((line 3) (column 2)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "Binop erased dynamic" =
  go
    {|
let _ =
  1 + (2 + 3 @ erased dynamic)
;;|};
  [%expect
    {|
    ((loc ((line 3) (column 4)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "Binop erased dynamic" =
  go
    {|
let _ =
  1 + (2 @ dynamic) + (3 @ erased)
;;|};
  [%expect
    {|
    ((loc ((line 3) (column 20)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "Binop erased static" =
  go
    {|
let _ =
  1 + ((2 + 3) @ erased)
;;|};
  [%expect
    {|
    ((loc ((line 3) (column 4)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "Binop static + dynamic" =
  go
    {|
let dyn = 2 @ dynamic;;
let _ =
  1 + dyn
;;|};
  [%expect {| |}]
;;

let%expect_test "Binop dynamic + static" =
  go
    {|
let dyn = 1 @ dynamic;;
let _ =
  dyn + 2
;;|};
  [%expect {| |}]
;;

let%expect_test "Binop dynamic + dynamic" =
  go
    {|
let dyn1 = 1 @ dynamic;;
let dyn2 = 2 @ dynamic;;
let _ =
  dyn1 + dyn2
;;|};
  [%expect {| |}]
;;

let%expect_test "Binop erased + dynamic" =
  go
    {|
let dyn1 = 1 @ erased;;
let dyn2 = 2 @ dynamic;;
let _ =
  dyn1 + dyn2
;;|};
  [%expect
    {|
    ((loc ((line 5) (column 7)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "Binop erased + erased" =
  go
    {|
let dyn1 = 1 @ erased;;
let dyn2 = 2 @ erased;;
let _ =
  dyn1 + dyn2
;;|};
  [%expect
    {|
    ((loc ((line 5) (column 7)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "If static cond static branches" =
  go
    {|
let _ =
  if true then 1 else 2
;;|};
  [%expect {| |}]
;;

let%expect_test "If erased" =
  go
    {|
let _ =
  (if true then int else int) @ erased
;;|};
  [%expect {| |}]
;;

let%expect_test "If erased cond" =
  go
    {|
let x = true || false @ erased;;
let _ =
  if x then 1 else 2
;;|};
  [%expect
    {|
    ((loc ((line 4) (column 2)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "If dynamic erased cond" =
  go
    {|
let x = true @ dynamic erased;;
let _ =
  if x then 1 else 2
;;|};
  [%expect
    {|
    ((loc ((line 4) (column 2)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "If static cond dynamic branches" =
  go
    {|
let dyn = 1 @ dynamic;;
let _ =
  if true then dyn else 2
;;|};
  [%expect {| |}]
;;

let%expect_test "If dynamic cond" =
  go
    {|
let dyn = true @ dynamic;;
let _ =
  if dyn then 1 else 2
;;|};
  [%expect {| |}]
;;

let%expect_test "erased if expr" =
  go
    {|
let x = true;;
let _ =
  0 + ((if x then 1 else 2) @ erased)
;;|};
  [%expect
    {|
    ((loc ((line 4) (column 4)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "if branch checks" =
  go
    {|
let _ = if true then 0 else true;;|};
  [%expect
    {|
    ((loc ((line 2) (column 8)))
     (reason (Cannot_unify (lhs (Type Int)) (rhs (Type Bool)))))
    |}]
;;

let%expect_test "if branch checks" =
  go
    {|
let _ = if 0 then 0 else 0;;|};
  [%expect
    {|
    ((loc ((line 2) (column 8)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Bool)))))
    |}]
;;

let%expect_test "if branch checks" =
  go
    {|
let _ = if true then 1+1 else 1+false;;|};
  [%expect
    {|
    ((loc ((line 2) (column 31)))
     (reason
      (Type_mismatch (got (Type (Tuple ((Type Int) (Type Bool)))))
       (need (Type (Tuple ((Type Int) (Type Int))))))))
    |}]
;;

let%expect_test "if erased branch" =
  go
    {|
let _ = if true then 0 else (true @ erased);;|};
  [%expect
    {|
    ((loc ((line 2) (column 8)))
     (reason (Cannot_unify (lhs (Type Int)) (rhs (Type Bool)))))
    |}]
;;

let%expect_test "if erased branch" =
  go
    {|
let _ = if true then 0 else int;;|};
  [%expect
    {|
    ((loc ((line 2) (column 8)))
     (reason (Cannot_unify (lhs (Type Int)) (rhs (Type Type)))))
    |}]
;;

let%expect_test "if erased branch" =
  go
    {|
let _ = if 1==2 then unit else int;;|};
  [%expect {| |}]
;;

let%expect_test "if erased branch" =
  go
    {|
let cond = true @ dynamic;;
let t = if cond then unit else int;;|};
  [%expect {| |}]
;;

let%expect_test "if erased branch" =
  go
    {|
let cond = true @ dynamic;;
let t = (if cond then false else cond) @ erased;;|};
  [%expect
    {|
    ((loc ((line 3) (column 39)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "Let static" =
  go
    {|
let _ =
  let x = 1 in
  x
;;|};
  [%expect {| |}]
;;

let%expect_test "Let dynamic" =
  go
    {|
let dyn = 1 @ dynamic;;
let _ =
  let x = dyn in
  x
;;|};
  [%expect {| |}]
;;

let%expect_test "Let dynamic" =
  go
    {|
let dyn = 1 @ erased;;
let _ =
  let x = dyn + 1 @ dynamic in
  x
;;|};
  [%expect
    {|
    ((loc ((line 4) (column 14)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "Let erased" =
  go
    {|
let dyn = 1 @ erased;;
let _ =
  let x = dyn in
  x
;;|};
  [%expect {| |}]
;;

let%expect_test "Let erased" =
  go
    {|
let erased dyn = 1;;
let _ =
  let x = dyn in
  x
;;|};
  [%expect {| |}]
;;

let%expect_test "Let erased" =
  go
    {|
let erased dyn = 1 @ dynamic;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 0)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "Let erased" =
  go
    {|
let _ =
  let x = 1 @ erased in
  x + 1
;;|};
  [%expect
    {|
    ((loc ((line 4) (column 4)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "Let erased" =
  go
    {|
let _ =
  let x = 1 @ erased in
  let y = 1 @ dynamic in
  x + y
;;|};
  [%expect
    {|
    ((loc ((line 5) (column 4)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "Let erased" =
  go
    {|
let _ =
  let x = 1 @ erased in
  let y = 1 @ erased in
  0 + (x + y)
;;|};
  [%expect
    {|
    ((loc ((line 5) (column 9)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "Let erased" =
  go
    {|
let _ =
  let x = 1 in
  let y = 1 in
  0 + ((x + y) @ erased)
;;|};
  [%expect
    {|
    ((loc ((line 5) (column 4)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "static closure" =
  go
    {|
let _ =
  (fn (x : int) -> x)
;;|};
  [%expect {| |}]
;;

let%expect_test "erased closure" =
  go
    {|
let _ =
  (fn (x : int) -> x) @ erased
;;|};
  [%expect {| |}]
;;

let%expect_test "closure return type" =
  go
    {|
let _ =
  (fn (x : int) -> int)
;;|};
  [%expect {| |}]
;;

let%expect_test "dynamic capture" =
  go
    {|
let y = 1 @ dynamic;;
let _ =
  (fn (x : int) -> x + y)
;;|};
  [%expect {| |}]
;;

let%expect_test "dynamic capture inline" =
  go
    {|
let y = 1 @ dynamic;;
let f = (fn (x : int) -> x + y);;
let _ = (f @ erased) 0;;|};
  [%expect
    {|
    ((loc ((line 4) (column 11)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "static closure arg" =
  go
    {|
let f =
  (fn (static x : int) -> x + 1) @ dynamic;;
let x = f 1;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (erased x : int) -> x;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (erased x : int) -> 1;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (erased x : int) -> 1;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (g : int -> erased int) -> let _ = g 1 in 2;;
let _ = f (fn (x : int) -> 0 @ erased);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (erased g : int -> erased int) -> let _ = g 1 in 2;;
let _ = f (fn erased (x : int) -> 0);;
|};
  [%expect {| |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (erased g : int -> erased int) -> let _ = g 1 in 2;;
let _ = f ((fn erased (x : int) -> 0) @ dynamic);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 38)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (static erased g : int -> erased int) -> let _ = g 1 in 2;;
let _ = f (fn (x : int) -> 0 @ erased);;
|};
  [%expect {| |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (erased x : int -> int) -> 1;;
let _ = f ((fn (x : int) -> x + 1) @ erased);;
|};
  [%expect {| |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (x : int) -> x;;
let g = fn (erased x : int) -> f x;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 31)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "static erased closure arg" =
  go
    {|
let _ =
  (fn (static erased x : int) -> x)
;;|};
  [%expect {| |}]
;;

let%expect_test "dependent closure arg" =
  go
    {|
let _ =
  (fn (x : type) -> x)
;;|};
  [%expect {| |}]
;;

let%expect_test "closure erased" =
  go
    {|
let x = (fn (x : int) -> x) @ erased;;
|};
  [%expect {| |}]
;;

let%expect_test "closure erased" =
  go
    {|
let x = (fn (erased x : int) -> x) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "closure erased" =
  go
    {|
let x = (fn (erased x : int) -> 1) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "closure erased" =
  go
    {|
let f = (fn (erased x : int) -> 1);;
let g = (fn (erased x : int) -> 2);;
let _ = (if true then f else g) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "closure branches" =
  go
    {|
let f = (fn (erased x : int) -> 1);;
let g = (fn (static x : int) -> 2);;
let _ = if true then f else g;;
|};
  [%expect {| |}]
;;

let%expect_test "closure branches" =
  go
    {|
let f = (fn (static erased x : int) -> 1);;
let g = (fn (static x : int) -> 2);;
let _ = if true then f else g;;
|};
  [%expect {| |}]
;;

let%expect_test "closure branches" =
  go
    {|
let f = (fn (static erased x : int) -> 1);;
let g = (fn (static erased x : int) -> 2);;
let _ = if true then f else g;;
|};
  [%expect {| |}]
;;

let%expect_test "closure erased" =
  go
    {|
let c = fn (_ : unit) -> true;;
let f = (fn (erased x : int) -> 1);;
let g = (fn (erased x : int) -> 2);;
let _ = (if c () then f else g) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "closure erased" =
  go
    {|
let c = fn (_ : unit) -> true;;
let f = (fn (x : int) -> 1);;
let g = (fn (erased x : int) -> 2);;
let _ = (if c () then f else g) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "closure nest" =
  go
    {|
let f = fn (x : int) -> 1;;
let g = fn (f : int -> int) -> f 0;;
let _ = g f;;
|};
  [%expect {| |}]
;;

let%expect_test "closure nest" =
  go
    {|
let f = fn (x : int) -> 1;;
let g = fn (static f : int -> int) -> f 0;;
let _ = g f;;
|};
  [%expect {| |}]
;;

let%expect_test "closure nest" =
  go
    {|
let f = fn (x : int) -> 1;;
let g = fn (erased f : int -> int) -> f 0;;
let _ = g f;;
|};
  [%expect {| |}]
;;

let%expect_test "closure nest" =
  go
    {|
let f1 = (fn (x : int) -> 1) @ erased;;
let g = fn (f2 : int -> int) -> f2 0;;
let _ = g f1;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "closure nest" =
  go
    {|
let f1 = (fn (x : int) -> 1) @ erased;;
let g = fn (erased f2 : int -> int) -> f2 0;;
let _ = g f1;;
|};
  [%expect {| |}]
;;

let%expect_test "closure nest" =
  go
    {|
let f1 = (fn (x : int) -> 1) @ erased;;
let g = fn (static erased f2 : int -> int) -> f2 0;;
let _ = g f1;;
|};
  [%expect {| |}]
;;

let%expect_test "inlined closure nest" =
  go
    {|
let f1 = fn (erased x : int) -> 1;;
let g = fn (static f2 : erased int -> int) -> f2 0;;
let _ = g f1;;
|};
  [%expect {| |}]
;;

let%expect_test "inlined closure nest" =
  go
    {|
let f1 = fn (erased x : int) -> 1;;
let g = fn (f2 : erased int -> int) -> f2 0;;
let _ = g f1;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 39)))
     (reason
      (Mode_mismatch (got ((staticity Parametric) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "inlined closure nest" =
  go
    {|
let f1 = fn (erased x : int) -> 1;;
let g = fn (static f2 : erased int -> int) -> f2 0;;
let _ = g f1;;
|};
  [%expect {| |}]
;;

let%expect_test "inlined closure nest" =
  go
    {|
let f1 = fn (erased x : int) -> 1;;
let g = fn (static erased f2 : erased int -> int) -> f2 0;;
let _ = g f1;;
|};
  [%expect {| |}]
;;

let%expect_test "closure static" =
  go
    {|
let x = (fn (static x : int) -> x) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "closure dependent" =
  go
    {|
let x = (fn (static x : type) -> 0 : x);;
|};
  [%expect
    {|
    ((loc ((line 2) (column 35)))
     (reason (Type_mismatch (got (Type Int)) (need (Var (Anon <opaque>))))))
    |}]
;;

let%expect_test "closure dependent" =
  go
    {|
let x = (fn (static erased x : type) -> 0 : x);;
let _ = x int;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 42)))
     (reason (Type_mismatch (got (Type Int)) (need (Var (Anon <opaque>))))))
    |}]
;;

let%expect_test "closure dependent" =
  go
    {|
let x = (fn (static erased x : type) -> 0 : x);;
|};
  [%expect
    {|
    ((loc ((line 2) (column 42)))
     (reason (Type_mismatch (got (Type Int)) (need (Var (Anon <opaque>))))))
    |}]
;;

let%expect_test "closure static erased" =
  go
    {|
let x = (fn (static erased x : int) -> 1) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "closure return dynamic type" =
  go
    {|
let t = (fn (x : int) -> int) 0;;
let _ = 0 : t;;
|};
  [%expect {| |}]
;;

let%expect_test "closure return static type" =
  go
    {|
let t = (fn (static x : int) -> int) 0;;
let _ = 0 : t;;
|};
  [%expect {| |}]
;;

let%expect_test "closure return static type" =
  go
    {|
let t = (fn (static x : int) -> bool) 0;;
let _ = 0 : t;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 10)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Bool)))))
    |}]
;;

let%expect_test "closure return static type" =
  go
    {|
let t = (fn (static erased x : int) -> int) 0;;
let _ = 0 : t;;
|};
  [%expect {| |}]
;;

let%expect_test "Apply fn static arg" =
  go
    {|
let _ =
  (fn (x : int) -> x) 1
;;|};
  [%expect {| |}]
;;

let%expect_test "Apply static erased fn static arg" =
  go
    {|
let _ =
  (fn (static erased x : int) -> x) 1
;;|};
  [%expect {| |}]
;;

let%expect_test "Apply fn dynamic arg" =
  go
    {|
let dyn = 1 @ dynamic;;
let _ =
  (fn (x : int) -> x) dyn
;;|};
  [%expect {| |}]
;;

let%expect_test "Apply erased fn dynamic arg" =
  go
    {|
let dyn = 1 @ dynamic;;
let y =
  (fn (erased x : int) -> x) dyn
;;
let _ = y @ unerased;;|};
  [%expect
    {|
    ((loc ((line 4) (column 2)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "Apply erased fn dynamic arg" =
  go
    {|
let dyn = 1 @ dynamic;;
let y =
  (fn (erased x : int) -> 5) (dyn-1)
;;
let _ = y @ unerased;;|};
  [%expect
    {|
    ((loc ((line 4) (column 2)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "Apply erased fn dynamic arg" =
  go
    {|
let dyn = 1 @ dynamic erased;;
let y =
  (fn (erased x : int) -> 5) (dyn-1)
;;
let _ = y @ unerased;;|};
  [%expect
    {|
    ((loc ((line 4) (column 33)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "Apply erased fn dynamic arg" =
  go
    {|
let f = fn (static x : int) -> x @ erased;;
let y =
  (fn (erased x : int) -> 5) (f 1)
;;
let _ = y @ unerased;;|};
  [%expect {| |}]
;;

let%expect_test "Apply erased fn stat arg" =
  go
    {|
let dyn = 1;;
let y =
  (fn (erased x : int) -> 5) (dyn-1)
;;
let _ = y @ unerased;;|};
  [%expect {| |}]
;;

let%expect_test "Apply dynamic fn static arg" =
  go
    {|
let dyn_fn = (fn (x : int) -> x) @ dynamic;;
let _ =
  dyn_fn 1
;;|};
  [%expect {| |}]
;;

let%expect_test "Apply dynamic fn erased arg" =
  go
    {|
let dyn_fn = (fn (erased x : int) -> x) @ dynamic;;
let _ =
  dyn_fn 1
;;|};
  [%expect
    {|
    ((loc ((line 2) (column 40)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "Apply dynamic fn erased arg" =
  go
    {|
let dyn_fn = (fn (erased x : int) -> 0) @ dynamic;;
let _ =
  dyn_fn (1 @ dynamic)
;;|};
  [%expect
    {|
    ((loc ((line 4) (column 2)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "Apply dynamic fn dynamic arg" =
  go
    {|
let dyn_fn = (fn (x : int) -> x) @ dynamic;;
let dyn_arg = 1 @ dynamic;;
let _ =
  dyn_fn dyn_arg
;;|};
  [%expect {| |}]
;;

let%expect_test "Apply erased fn dynamic arg" =
  go
    {|
let dyn_fn = (fn (erased x : int) -> x) @ dynamic;;
let dyn_arg = 1 @ dynamic;;
let _ =
  dyn_fn dyn_arg
;;|};
  [%expect
    {|
    ((loc ((line 2) (column 40)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "Apply erased fn dynamic arg" =
  go
    {|
let dyn_fn = (fn (erased x : int) -> x) @ erased;;
let dyn_arg = 1 @ dynamic;;
let _ =
  dyn_fn dyn_arg
;;|};
  [%expect
    {|
    ((loc ((line 5) (column 2)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "Lambda dynamic arg" =
  go
    {|
let _ =
  fn (x : int) -> x
;;|};
  [%expect {| |}]
;;

let%expect_test "Lambda static arg" =
  go
    {|
let _ =
  fn (static x : int) -> 1
;;|};
  [%expect {| |}]
;;

let%expect_test "Lambda erased arg" =
  go
    {|
let _ =
  fn (erased x : int) -> x
;;|};
  [%expect {| |}]
;;

let%expect_test "Lambda capturing dynamic var" =
  go
    {|
let x = 1 @ dynamic;;
let _ =
  fn (y : int) -> x
;;|};
  [%expect {| |}]
;;

let%expect_test "Lambda capturing static var" =
  go
    {|
let x = 1 @ static;;
let _ =
  fn (y : int) -> x
;;|};
  [%expect {| |}]
;;

let%expect_test "Lambda capturing erased var" =
  go
    {|
let x = 1 @ static erased;;
let _ =
  fn (y : int) -> x
;;|};
  [%expect {| |}]
;;

let%expect_test "Lambda capturing erased var" =
  go
    {|
let x = 1 @ static erased;;
let _ =
  (fn (y : int) -> x) 0
;;|};
  [%expect {| |}]
;;

let%expect_test "Lambda capturing type" =
  go
    {|
let f = fn (_ : unit) -> int;;
let g = fn (x : f ()) -> x + 1;;
let _ = g 0;;|};
  [%expect {| |}]
;;

let%expect_test "Lambda capturing type" =
  go
    {|
let f = fn (static _ : unit) -> int;;
let g = fn (x : f ()) -> x + 1;;|};
  [%expect {| |}]
;;

let%expect_test "Lambda take type" =
  go
    {|
let f = fn (ty : type) -> ty;;
let _ = f int;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "Lambda take type" =
  go
    {|
let f = fn (erased ty : type) -> ty;;
let _ = f int;;
|};
  [%expect {| |}]
;;

let%expect_test "Lambda take type" =
  go
    {|
let f = fn (static erased ty : type) -> ty;;
let _ = 0 : f int;;
|};
  [%expect {| |}]
;;

let%expect_test "Lambda take type" =
  go
    {|
let f = fn (erased ty : type) -> ty;;
let g = fn (static f : type -> type) -> f int;;
let _ = g f;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 40)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "Lambda take type" =
  go
    {|
let f = fn (static erased ty : type) -> ty;;
let _ = 0 : f bool;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 10)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Bool)))))
    |}]
;;

let%expect_test "mono fn" =
  go
    {|
let x = fn (static erased x : type) -> x;;
let y = x int;;
|};
  [%expect {| |}]
;;

let%expect_test "mono fn" =
  go
    {|
let x = fn (static x : type) -> x;;
let y = x @ dynamic;;
|};
  [%expect {| |}]
;;

let%expect_test "mono fn" =
  go
    {|
let x = fn (erased x : type) -> x;;
let y = x @ dynamic;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 10)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "mono fn" =
  go
    {|
let x = fn (static x : type) -> x;;
let y = x @ unerased;;
|};
  [%expect {| |}]
;;

let%expect_test "mono fn" =
  go
    {|
let x = fn (static x : type) -> x;;
let y = x (0 @ dynamic);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "mono fn" =
  go
    {|
let x = fn (erased x : type) -> x;;
let y = x (0 @ dynamic);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "mono fn" =
  go
    {|
let x = fn (static erased x : type) -> x;;
let y = (x int) @ dynamic;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 16)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "Dependent lambda" =
  go
    {|
let f = fn (erased ty : type) -> fn (x : ty) -> x;;
|};
  [%expect {| |}]
;;

let%expect_test "Dependent lambda" =
  go
    {|
let f = fn (static erased ty : type) -> fn (x : ty) -> x;;
|};
  [%expect {| |}]
;;

let%expect_test "static erased lambda" =
  go
    {|
let f = fn (static x : int) -> fn (_ : unit) -> x;;
let g = (f 1 ()) @ unerased;;
|};
  [%expect {| |}]
;;

let%expect_test "lift universal type" =
  go
    {|
let f = fn (static ty : type) -> ty @ erased;;
|};
  [%expect {| |}]
;;

let%expect_test "lift universal int" =
  go
    {|
let f = fn (static x : int) -> x @ erased;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "Dependent lambda" =
  go
    {|
let f = fn (static erased ty : type) -> fn (x : ty) -> x;;
let g = f int;;
let _ = g 0;;
let g = f bool;;
let _ = g true;;
|};
  [%expect {| |}]
;;

let%expect_test "Dependent lambda" =
  go
    {|
let f = fn (static g : int -> int) -> g 0;;
let _ = f (fn (x : int) -> x + 1);;
|};
  [%expect {| |}]
;;

let%expect_test "dependent unit" =
  go
    {|
let f = fn (static x : unit) -> ();;
let _ = f ();;
|};
  [%expect {| |}]
;;

let%expect_test "dependent bool" =
  go
    {|
let f = fn (static x : bool) -> !x;;
let _ = f true;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent int" =
  go
    {|
let f = fn (static x : int) -> -x;;
let _ = f 1;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent type" =
  go
    {|
let f = fn (static erased t : type) -> fn (x : t) -> -x;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 53)))
     (reason (Type_mismatch (got (Var (Anon <opaque>))) (need (Type Int)))))
    |}]
;;

let%expect_test "dependent type" =
  go
    {|
let f = fn (static erased t : type) -> fn (x : t) -> if true then x else x;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent type" =
  go
    {|
let f = fn (static erased t1 : type) -> fn (static erased t2 : type) -> fn (x : t1) -> fn (y : t2) -> if true then x else y;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 102)))
     (reason
      (Cannot_unify (lhs (Var (Anon <opaque>))) (rhs (Var (Anon <opaque>))))))
    |}]
;;

let%expect_test "arrow force args" =
  go
    {|
let a = (let _ = assert erased false in int) -> int;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 17)))
     (reason (Static_failure (Assert_failed (Bool (T false))))))
    |}]
;;

let%expect_test "tuple force args" =
  go
    {|
let a = (let _ = assert erased false in int) ^ int;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 17)))
     (reason (Static_failure (Assert_failed (Bool (T false))))))
    |}]
;;

let%expect_test "arrow typechecking" =
  go
    {|
let f = fn (g : int -> int) -> g 0;;
let _ = f (fn (x : int) -> 0);;
|};
  [%expect {| |}]
;;

let%expect_test "arrow typechecking" =
  go
    {|
let f = fn (g : static int -> int) -> g 0;;
let _ = f (fn (static x : int) -> 0);;
|};
  [%expect
    {|
    ((loc ((line 2) (column 38)))
     (reason
      (Mode_mismatch (got ((staticity Parametric) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "arrow typechecking" =
  go
    {|
let f = fn (_ : int) -> 0 @ erased;;
let _ = (f @ dynamic) 0;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 11)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "arrow typechecking" =
  go
    {|
let f = fn (g : static int -> int) -> g 0;;
let _ = f (fn (x : int) -> 0);;
|};
  [%expect
    {|
    ((loc ((line 2) (column 38)))
     (reason
      (Mode_mismatch (got ((staticity Parametric) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "arrow typechecking" =
  go
    {|
let f = fn (static g : static int -> int) -> g 0;;
let _ = f (fn (static x : int) -> 0);;
|};
  [%expect {| |}]
;;

let%expect_test "arrow typechecking" =
  go
    {|
let f = fn (static g : static int -> int) -> g 0;;
let _ = f (fn (x : int) -> 0);;
|};
  [%expect {| |}]
;;

let%expect_test "arrow typechecking" =
  go
    {|
let f = fn (erased g : erased int -> int) -> g 0;;
let _ = f (fn (erased x : int) -> 0);;
|};
  [%expect {| |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (static x : int) -> x else fn (x : int) -> x;;
|};
  [%expect {| |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (static x : int) -> x else fn (erased x : int) -> x;;
|};
  [%expect {| |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (static erased x : int) -> x else fn (x : int) -> x;;
|};
  [%expect {| |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (erased x : int) -> x else fn (x : int) -> x;;
|};
  [%expect {| |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (x : int) -> x else fn (static x : int) -> x;;
|};
  [%expect {| |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (x : int) -> x else fn (erased x : int) -> x;;
|};
  [%expect {| |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (erased x : int) -> x else fn (static x : int) -> x;;
|};
  [%expect {| |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (x : int) -> x else fn (static erased x : int) -> x;;
|};
  [%expect {| |}]
;;

let%expect_test "return erased" =
  go
    {|
let f = fn (x : int) -> int;;
let _ = 0 : f 1;;
|};
  [%expect {| |}]
;;

let%expect_test "return erased" =
  go
    {|
let f = fn (x : int) -> 0 @ erased;;
let _ = f 1;;
|};
  [%expect {| |}]
;;

let%expect_test "arrow typechecking" =
  go
    {|
  let f = fn (static g : erased int -> int) -> g 0;;
  |};
  [%expect {| |}]
;;

let%expect_test "arrow typechecking" =
  go
    {|
  let f = fn (g : static int -> int) -> g 0;;
  |};
  [%expect
    {|
    ((loc ((line 2) (column 40)))
     (reason
      (Mode_mismatch (got ((staticity Parametric) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "pi typechecking" =
  go
    {|
  let f = fn (static g : erased int -> int) -> g 0;;
  let _ = f (fn (erased x : int) -> 0);;
  |};
  [%expect {| |}]
;;

let%expect_test "arrow-pi typechecking" =
  go
    {|
let f = fn (x : int) -> 0 @ dynamic;;
let s = fn (static x : int) -> ();;
let _ = s (f 0);;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "arrow-pi typechecking" =
  go
    {|
let f = fn (x : int) -> 0;;
let s = fn (static x : int) -> ();;
let _ = s (f 0);;
|};
  [%expect {| |}]
;;

let%expect_test "arrow-pi typechecking" =
  go
    {|
let f = fn (static g : static int -> int) -> g 1;;
let _ = f (fn (x : int) -> 0);;
|};
  [%expect {| |}]
;;

let%expect_test "Pi typechecking" =
  go
    {|
let f = fn (static erased g : static int -> int) -> g 0;;
let _ = f (fn (static x : int) -> x + 1);;
|};
  [%expect {| |}]
;;

let%expect_test "Pi typechecking" =
  go
    {|
let f = fn (static g : static erased type -> int) -> g unit;;
let _ = f (fn (static erased t : type) -> () : t);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 45)))
     (reason (Type_mismatch (got (Type Unit)) (need (Var (Anon <opaque>))))))
    |}]
;;

let%expect_test "Dependent lambda" =
  go
    {|
let f = fn (static g : static erased type -> unit) -> g unit;;
let _ = f (fn (static erased t : type) -> () : t);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 45)))
     (reason (Type_mismatch (got (Type Unit)) (need (Var (Anon <opaque>))))))
    |}]
;;

let%expect_test "Dependent lambda" =
  go
    {|
let f = fn (static g : static erased type -> unit) -> g int;;
let _ = f (fn (static erased t : type) -> () : t);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 45)))
     (reason (Type_mismatch (got (Type Unit)) (need (Var (Anon <opaque>))))))
    |}]
;;

let%expect_test "dependent lambda" =
  go
    {|
let f = fn (static g : static erased type -> int -> int) -> g int;;
let _ = f (fn (static erased t : type) -> fn (x : int) -> x);;
|};
  [%expect {| |}]
;;

let%expect_test "dependent fn" =
  go
    {|
let id = fn (static erased t : type) -> (fn (x : t) -> x);;
let x = (id int) (0 @ dynamic);;
let y = (id bool) (true @ dynamic);;
|};
  [%expect {| |}]
;;

let%expect_test "dependent fn" =
  go
    {|
let id = fn (static erased t : type) -> (fn (x : t) -> x) @ erased;;
let x = (id int) (0 @ dynamic);;
let y = (id bool) (true @ dynamic);;
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
          (ret_mode ((staticity Static) (erasure Unerased))))))
       (result ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "dependent arrow" =
  go
    {|
let mk_int = fn (static x : int) -> int;;
let apply_int = fn (static f : static int \ x -> mk_int x) -> 2;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent arrow" =
  go
    {|
let mk_int = fn (static x : int) -> int;;
let apply_int = fn (static f : static int \ x -> unit -> mk_int x) -> f 2;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent arrow" =
  go
    {|
let apply_int = fn (static f : static erased type \ t -> t -> t) -> f int;;
let _ = apply_int (fn (static erased t : type) -> fn (x : t) -> x);;
|};
  [%expect {| |}]
;;

let%expect_test "dependent arrow" =
  go
    {|
let apply = fn (static f : static erased type \ t -> t -> t) -> fn (static erased t2 : type) -> f t2;;
let f = apply (fn (static erased t : type) -> fn (x : t) -> x);;
let g = f int;;
let h = f bool;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static x : int) -> if erased x == 0 then 1 else true;;
let _ = f 0;;
let _ = f 1;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent if" =
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

let%expect_test "dependent if" =
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
          (((Literal (value (Bool true)) (loc ((line 2) (column 32))))
            (Type Int))
           ((Literal (value (Bool false)) (loc ((line 2) (column 32))))
            (Type Bool))))))
       (need (Type Int)))))
    |}]
;;

let%expect_test "dependent if" =
  go
    {|
let _ = (if erased true then 1 else true) : (if true then int else bool);;
|};
  [%expect {| |}]
;;

let%expect_test "dependent if" =
  go
    {|
let _ = (if erased true then 1 else true) : (if erased true then int else bool);;
|};
  [%expect {| |}]
;;

let%expect_test "dependent if" =
  go
    {|
let _ = (if erased true then 1 else true + 1) : int;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 41)))
     (reason
      (Type_mismatch (got (Type (Tuple ((Type Bool) (Type Int)))))
       (need (Type (Tuple ((Type Int) (Type Int))))))))
    |}]
;;

let%expect_test "dependent if" =
  go
    {|
let _ = fn (static x : int) -> (if erased x==0 then 1 else true + 1) : int;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 64)))
     (reason
      (Type_mismatch (got (Type (Tuple ((Type Bool) (Type Int)))))
       (need (Type (Tuple ((Type Int) (Type Int))))))))
    |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static _ : unit) -> int;;
let _ = (if erased true then 1 else (0 : f ())) : int;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static x : int) -> (if erased x == 0 then 1 else true) : (if x == 0 then int else bool);;
|};
  [%expect {| |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static x : int) -> (if erased x == 0 then 1 else true) : (if erased x == 0 then int else bool);;
|};
  [%expect {| |}]
;;

let%expect_test "bad annotation" =
  go
    {|
let f = fn (static x : int) -> (if erased x == 0 then 1 else true) : (if erased x == 0 then 0 else bool);;
|};
  [%expect
    {|
    ((loc ((line 2) (column 67)))
     (reason
      (Type_mismatch
       (got
        (Match (scrutinee (Bool (Eq (Var (Anon <opaque>)) (Int (T 0)))))
         (arms
          (((Literal (value (Bool true)) (loc ((line 2) (column 70))))
            (Type Int))
           ((Literal (value (Bool false)) (loc ((line 2) (column 70))))
            (Type Type))))))
       (need (Type Type)))))
    |}]
;;

let%expect_test "bad annotation" =
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

let%expect_test "bad annotation" =
  go
    {|
let f = 0 : 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 10)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Type)))))
    |}]
;;

let%expect_test "bad annotation" =
  go
    {|
fun f (x : 0) : int = 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 4)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Type)))))
    |}]
;;

let%expect_test "bad annotation" =
  go
    {|
fun f (x : int) : 0 = 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 4)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Type)))))
    |}]
;;

let%expect_test "dependent if unify" =
  go
    {|
let f = fn (static x : int) -> if erased x == 0 then 1 else true;;
let g = fn (static x : int) -> if erased x == 0 then 1 else true;;
let _ = if true then f else g;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent if unify" =
  go
    {|
let f = fn (static x : int) -> if erased x == 0 then 1 else true;;
let g = fn (static x : int) -> if erased x == 0 then true else 2;;
let _ = if true then f 0 else g 1;;
|};
  [%expect {| |}]
;;

let%expect_test "symbolic arrow type" =
  go
    {|
let choose = fn (static f : static int \ x -> if x == 0 then int else bool) -> f 0;;
let _ = choose (fn (static x : int) -> if erased x == 0 then 0 else true);;
|};
  [%expect {| |}]
;;

let%expect_test "Fun recursive dynamic arg" =
  go
    {|
fun f (x : int) : int = f x;;
|};
  [%expect {| |}]
;;

let%expect_test "fun recursive erased" =
  go
    {|
fun f (erased x : int) : int = f x;;
|};
  [%expect {| |}]
;;

let%expect_test "Fun erased arg" =
  go
    {|
fun f (erased x : int) : erased int = x;;
|};
  [%expect {| |}]
;;

let%expect_test "Fun erased ret" =
  go
    {|
fun f (erased x : int) : int = x;;
let y = (f 0) @ unerased;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 4)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "Fun return static" =
  go
    {|
fun f (x : int) : int = 1;;
|};
  [%expect {| |}]
;;

let%expect_test "Fun return erased" =
  go
    {|
fun f (x : int) : erased int = 1 @ erased;;
|};
  [%expect {| |}]
;;

let%expect_test "mono fun" =
  go
    {|
fun x (static x : int) : int = x;;
let y = x (0 @ dynamic);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "mono fun" =
  go
    {|
fun x (static x : int) : int = x;;
let y = x 0;;
|};
  [%expect {| |}]
;;

let%expect_test "static type" =
  go
    {|
fun f (static _ : unit) : static type = int;;
let y = 0 : f ();;
|};
  [%expect
    {|
    ((loc ((line 2) (column 4)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "static type" =
  go
    {|
fun f (static _ : unit) : static erased type = int;;
let y = 0 : f ();;
|};
  [%expect {| |}]
;;

let%expect_test "types are erased" =
  go
    {|
fun f (static _ : unit) : static erased type = int;;
let y = (f () @ dynamic);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 14)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "types are erased" =
  go
    {|
fun f (static _ : unit) : static erased type = int;;
let y = 5 : f ();;
|};
  [%expect {| |}]
;;

let%expect_test "types are erased" =
  go
    {|
fun f (static _ : unit) : static erased type = int;;
let y = f @ unerased;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 10)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "dependent fun (erased implies static)" =
  go
    {|
fun id (erased t : type) : t -> t = fn (x : t) -> x;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent fun " =
  go
    {|
fun id (static erased t : type) : t -> t = fn (x : t) -> x;;
let i = id int;;
|};
  [%expect {| |}]
;;

let%expect_test "erased fun " =
  go
    {|
fun id (erased x : int) : erased int = x;;
let _ = id 0;;
|};
  [%expect {| |}]
;;

let%expect_test "erased fun " =
  go
    {|
fun id (erased x : int) : erased int = x;;
let _ = (id @ dynamic) 0;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 12)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "dependent fun erased" =
  go
    {|
fun id (static erased t : type) : t -> t = fn (x : t) -> x;;
let x = (id int) (0 @ dynamic);;
|};
  [%expect {| |}]
;;

let%expect_test "dependent fun" =
  go
    {|
let ty = fn (static _ : unit) -> int -> int;;
fun id (_ : unit) : ty () = fn (x : int) -> x;;
let x = id () 0;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent fun" =
  go
    {|
fun id1 (static erased t : type) : t -> t = fn (x : t) -> x;;
fun id2 (static erased t : type) : t -> t = id1 t;;
let x = id2 int (0 @ dynamic);;
|};
  [%expect {| |}]
;;

let%expect_test "join" =
  go
    {|
fun a (_ : unit) : unit = ();;
fun b (_ : unit) : unit -> unit = fn (_ : unit) -> ();;
let x = if erased false then a else b;;
let _ = x () ();;
|};
  [%expect {| |}]
;;

let%expect_test "return fn" =
  go
    {|
fun x (_ : unit) : unit -> unit = fn (_ : unit) -> ();;
let _ = x () ();;
|};
  [%expect {| |}]
;;

let%expect_test "arg fn" =
  go
    {|
fun x (f : unit -> int) : int = f ();;
let _ = x (fn (_ : unit) -> 1);;
|};
  [%expect {| |}]
;;

let%expect_test "universal value" =
  go
    {|
let f = fn (static erased t : type) -> fn (static x : t) -> !x;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 60)))
     (reason (Type_mismatch (got (Var (Anon <opaque>))) (need (Type Bool)))))
    |}]
;;

let%expect_test "arrow" =
  go
    {|
let f = 0 -> int;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 8)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Type)))))
    |}]
;;

let%expect_test "arrow" =
  go
    {|
let f = int -> 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 8)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Type)))))
    |}]
;;

let%expect_test "arrow" =
  go
    {|
let x = int @ dynamic;;
let f = int -> x;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 12)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "arrow" =
  go
    {|
let x = int @ dynamic;;
let f = x -> int;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 12)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "arrow" =
  go
    {|
let f = static type \ t -> (t @ dynamic);;
|};
  [%expect
    {|
    ((loc ((line 2) (column 15)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "arrow" =
  go
    {|
let f = static int \ t -> t;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 15)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Type)))))
    |}]
;;

let%expect_test "fun" =
  go
    {|
fun f (x : int) : static int = x;;
|};
  [%expect {| |}]
;;

let%expect_test "fun" =
  go
    {|
fun f (x : 0) : int = x;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 4)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Type)))))
    |}]
;;

let%expect_test "fun" =
  go
    {|
fun f (x : int) : 0 = x;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 4)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Type)))))
    |}]
;;

let%expect_test "fun" =
  go
    {|
fun f (x : (int @ dynamic)) : int = x;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 16)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "fun" =
  go
    {|
fun f (x : int) : (int @ dynamic) = x;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 23)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "fun" =
  go
    {|
fun f (static x : int) : x = x;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 4)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Type)))))
    |}]
;;

let%expect_test "fun" =
  go
    {|
fun f (x : type) : x = ();;
|};
  [%expect {| ((loc ((line 2) (column 19))) (reason (Unbound_ident ((Id x) <opaque>)))) |}]
;;

let%expect_test "fun" =
  go
    {|
fun f (static x : type) : x = ();;
|};
  [%expect
    {|
    ((loc ((line 2) (column 4)))
     (reason (Type_mismatch (got (Type Unit)) (need (Var (Anon <opaque>))))))
    |}]
;;

let%expect_test "fun" =
  go
    {|
fun f (static x : type) : (x @ dynamic) = ();;
|};
  [%expect
    {|
    ((loc ((line 2) (column 4)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "fun" =
  go
    {|
fun f (static x : type) : (x + 1) = ();;
|};
  [%expect
    {|
    ((loc ((line 2) (column 29)))
     (reason
      (Type_mismatch (got (Type (Tuple ((Type Type) (Type Int)))))
       (need (Type (Tuple ((Type Int) (Type Int))))))))
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (static erased g : int -> erased int) -> let _ = g 1 in 2;;
let _ = f (fn (x : int) -> 0 @ erased);;
|};
  [%expect {| |}]
;;

let%expect_test "external" =
  go
    {|
external f : int -> dynamic int = asdf;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "external" =
  go
    {|
external f : int -> dynamic int = asdf;;
let _ = (f @ erased) 0;;
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

let%expect_test "external" =
  go
    {|
external f : static int -> int = asdf;;
let _ = f 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 0)))
     (reason (Static_external ((Id f) <opaque>) asdf)))
    |}]
;;

let%expect_test "external" =
  go
    {|
external f : int -> dynamic int = asdf;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "external" =
  go
    {|
external f : int -> static int = asdf;;
let _ = assert erased (f 0 == 0);;
|};
  [%expect
    {|
    ((loc ((line 2) (column 0)))
     (reason (Static_external ((Id f) <opaque>) asdf)))
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

let%expect_test "closure branches" =
  go
    {|
let f = (fn (static erased x : int) -> 1);;
let g = (fn (static x : int) -> 2);;
let h = if true then f else g;;
let _ = h 0;;
|};
  [%expect {| |}]
;;

let%expect_test "assert" =
  go
    {|
let _ = assert true;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert" =
  go
    {|
let x = true @ dynamic;;
let _ = assert x;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert erased" =
  go
    {|
let _ = assert erased true;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert erased" =
  go
    {|
let _ = assert erased false;;
  |};
  [%expect
    {|
    ((loc ((line 2) (column 8)))
     (reason (Static_failure (Assert_failed (Bool (T false))))))
    |}]
;;

let%expect_test "assert erased" =
  go
    {|
let x = true @ dynamic;;
let _ = assert erased x;;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "assert erased" =
  go
    {|
let _ = fn (static x : bool) -> if x then assert erased x else ();;
  |};
  [%expect
    {|
    ((loc ((line 2) (column 42)))
     (reason (Static_failure (Assert_failed (Var (Anon <opaque>))))))
    |}]
;;

let%expect_test "assert erased" =
  go
    {|
let _ = fn (static x : bool) -> if x then () else assert erased x;;
  |};
  [%expect
    {|
    ((loc ((line 2) (column 50)))
     (reason (Static_failure (Assert_failed (Var (Anon <opaque>))))))
    |}]
;;

let%expect_test "assert erased" =
  go
    {|
let _ = fn (static x : bool) -> assert erased x;;
  |};
  [%expect
    {|
    ((loc ((line 2) (column 32)))
     (reason (Static_failure (Assert_failed (Var (Anon <opaque>))))))
    |}]
;;

let%expect_test "assert erased" =
  go
    {|
let _ = fn (static x : bool) -> if erased x then assert erased x else ();;
  |};
  [%expect {| |}]
;;

let%expect_test "assert dynamic literal" =
  go
    {|
let _ = assert true;;
let _ = assert false;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert dynamic variable" =
  go
    {|
let x = true @ dynamic;;
let _ = assert x;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert dynamic variable" =
  go
    {|
builtin a = syl_assert;;
let x = true @ dynamic;;
let _ = a x;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert dynamic result is unit" =
  go
    {|
fun f (x : bool) : unit = assert x;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert dynamic result is static" =
  go
    {|
let x = true @ dynamic;;
let _ = (assert x) @ static;;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 19)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "assert dynamic result is static" =
  go
    {|
let x = true;;
let u = (assert x) @ static;;
let _ = u @ erased;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert dynamic result is static" =
  go
    {|
builtin a = syl_assert;;
let x = true;;
let u = (a x) @ static;;
let _ = u @ erased;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert erased result is static" =
  go
    {|
let x = false;;
let u = (assert erased x) @ static;;
let _ = u @ erased;;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 9)))
     (reason (Static_failure (Assert_failed (Bool (T false))))))
    |}]
;;

let%expect_test "assert dynamic result is static" =
  go
    {|
let x = false;;
let u = (assert x) @ static;;
let _ = u @ erased;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert dynamic result is static" =
  go
    {|
builtin a = syl_assert;;
let x = false;;
let u = (a x) @ static;;
let _ = u @ erased;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert dynamic result is static" =
  go
    {|
let x = false;;
let _ = (assert x) @ static;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert dynamic result is static" =
  go
    {|
let x = true @ dynamic;;
let _ = (assert x) @ static;;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 19)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "assert dynamic non-bool" =
  go
    {|
let _ = assert 1;;
  |};
  [%expect
    {|
    ((loc ((line 2) (column 8)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Bool)))))
    |}]
;;

let%expect_test "assert dynamic erased condition" =
  go
    {|
let x = true @ dynamic erased;;
let _ = assert x;;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "assert erased dynamic erased condition" =
  go
    {|
let x = true @ dynamic erased;;
let _ = assert erased x;;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "assert erased condition" =
  go
    {|
let x = true @ erased;;
let _ = assert x;;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "assert erased erased condition" =
  go
    {|
let x = true @ erased;;
let _ = assert erased x;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert dynamic in function body" =
  go
    {|
fun check (x : int) : unit =
  assert (x > 0);;
  |};
  [%expect {| |}]
;;

let%expect_test "assert dynamic in let body" =
  go
    {|
fun f (x : int) : int =
  let _ = assert (x >= 0) in
  x + 1;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert dynamic in if branch" =
  go
    {|
fun f (x : int) : int =
  if x > 0 then
    let _ = assert (x != 0) in
    x
  else
    0 - x;;
  |};
  [%expect {| |}]
;;

let%expect_test "erased lambda param" =
  go
    {|
let f = fn (erased x : int) -> x + 1;;
let _ = f 42;;
  |};
  [%expect
    {|
    ((loc ((line 2) (column 33)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "inline closure capturing static value" =
  go
    {|
let n = 10;;
let add_n = fn (x : int) -> x + n;;
let apply = fn (erased f : int -> int) -> fn (x : int) -> f x;;
let _ = apply add_n 5;;
  |};
  [%expect
    {|
    ((loc ((line 4) (column 58)))
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

let%expect_test "inline closure capturing multiple values" =
  go
    {|
let a = 10;;
let b = 20;;
let combine = fn (x : int) -> x + a + b;;
let apply = fn (erased f : int -> int) -> fn (x : int) -> f x;;
let _ = apply combine 5;;
  |};
  [%expect
    {|
    ((loc ((line 5) (column 58)))
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

let%expect_test "inline closure applied twice" =
  go
    {|
let n = 1;;
let inc = fn (x : int) -> x + n;;
let apply2 = fn (erased f : int -> int) -> f (f 0);;
let _ = apply2 inc;;
  |};
  [%expect
    {|
    ((loc ((line 4) (column 43)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "inline closure with polymorphic caller" =
  go
    {|
let map =
  fn (static erased t : type) ->
    fn (static erased u : type) -> fn (erased f : t -> u) -> fn (x : t) -> f x
;;
let base = 100;;
let _ = map int int (fn (x : int) -> x + base) 42;;
  |};
  [%expect
    {|
    ((loc ((line 4) (column 75)))
     (reason
      (Erased_application
       (fn
        (Type
         (Arrow (arg_ty (Var (Anon <opaque>)))
          (arg_mode ((staticity Dynamic) (erasure Unerased)))
          (ret_ty (Var (Anon <opaque>)))
          (ret_mode ((staticity Static) (erasure Unerased))))))
       (result ((staticity Parametric) (erasure Unerased))))))
    |}]
;;

let%expect_test "inline closure at multiple monomorphizations" =
  go
    {|
let apply = fn (static erased t : type) -> fn (erased f : t -> t) -> fn (x : t) -> f x;;
let n = 1;;
let _ = apply int (fn (x : int) -> x + n) 42;;
let _ = apply bool (fn (x : bool) -> x) true;;
  |};
  [%expect
    {|
    ((loc ((line 2) (column 83)))
     (reason
      (Erased_application
       (fn
        (Type
         (Arrow (arg_ty (Var (Anon <opaque>)))
          (arg_mode ((staticity Dynamic) (erasure Unerased)))
          (ret_ty (Var (Anon <opaque>)))
          (ret_mode ((staticity Static) (erasure Unerased))))))
       (result ((staticity Parametric) (erasure Unerased))))))
    |}]
;;

let%expect_test "inline composed closures capturing ambient" =
  go
    {|
let a = 1;;
let b = 2;;
let compose = fn (erased f : int -> int) -> fn (erased g : int -> int) -> fn (x : int) -> f (g x);;
let _ = compose (fn (x : int) -> x + a) (fn (x : int) -> x * b) 5;;
  |};
  [%expect
    {|
    ((loc ((line 4) (column 93)))
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

let%expect_test "inline closure from nested let" =
  go
    {|
let apply = fn (erased f : int -> int) -> fn (x : int) -> f x;;
let _ =
  let offset = 100 in
  let add_offset = fn (x : int) -> x + offset in
  apply add_offset 42
;;
  |};
  [%expect
    {|
    ((loc ((line 2) (column 58)))
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

let%expect_test "dynamic erased lambda param rejected" =
  go
    {|
let f = fn (dynamic erased x : int) -> x + 1;;
let _ = f 42;;
  |};
  [%expect
    {|
    ((loc ((line 2) (column 41)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "dynamic erased fun param rejected" =
  go
    {|
fun f (dynamic erased x : int) : int = x + 1;;
let _ = f 42;;
  |};
  [%expect
    {|
    ((loc ((line 2) (column 41)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "dynamic erased fun return rejected" =
  go
    {|
fun f (x : int) : dynamic erased int = x + 1;;
let _ = f 42;;
  |};
  [%expect
    {|
    ((loc ((line 2) (column 4)))
     (reason
      (Mode_mismatch (got ((staticity Parametric) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "erased fun return with dynamic body rejected" =
  go
    {|
fun f (x : int) : erased int = x @ dynamic;;
let _ = f 42;;
  |};
  [%expect
    {|
    ((loc ((line 2) (column 4)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "erased fun return with static body" =
  go
    {|
fun f (x : int) : erased int = x ;;
let _ = f 42;;
  |};
  [%expect
    {|
    ((loc ((line 2) (column 4)))
     (reason
      (Mode_mismatch (got ((staticity Parametric) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "pi calling arrow from same group at application" =
  go
    {|
fun inc (x : int) : int = let _ = choose true in x + 1
and choose (static erased b : bool) : int -> int =
  if erased b then fn (x : int) -> inc x else fn (x : int) -> x;;
let _ = choose true 5;;
let _ = choose false 5;;
|};
  [%expect {| |}]
;;

let%expect_test "dynamic erased" =
  go
    {|
let x = 1 @ dynamic;;
let _ = (x @ erased);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 11)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "dynamic erased" =
  go
    {|
let x = 1 @ dynamic;;
let _ = (x @ dynamic erased);;
|};
  [%expect {| |}]
;;

let%expect_test "static % with negative divisor reports Negative_modulus" =
  go
    {|
let _ = assert erased (5 % (-2) == 0);;
|};
  [%expect
    {|
    ((loc ((line 2) (column 25)))
     (reason (Static_failure (Negative_modulus (Mod (Int (T 5)) (Int (T -2)))))))
    |}]
;;

let%expect_test "static % with negative divisor inside static fn" =
  go
    {|
let f = fn (static x : int) -> x;;
let _ = print_int (f (5 % (-2)));;
|};
  [%expect
    {|
    ((loc ((line 3) (column 24)))
     (reason (Static_failure (Negative_modulus (Mod (Int (T 5)) (Int (T -2)))))))
    |}]
;;
