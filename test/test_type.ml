open! Core
open! Syl

let go ?(print = false) input =
  let cst = Parse.parse_exn input in
  let dst = Desugar.desugar cst in
  match Typecheck.typecheck dst with
  | Ok tst -> if print then print_s [%message (tst : Tst.Program.t)]
  | Error { loc; reason } -> print_s [%message (loc : Lex.Location.t) (reason : Typecheck.Error.t)]
;;

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
    ((loc ((line 5) (column 10)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "Lambda return dynamic erased" =
  go
    {|
let x =
  ((fn (x : int) -> 1 @ dynamic erased) @ dynamic) 0
;;
|};
  [%expect {| ((loc ((line 3) (column 2))) (reason Dynamic_erased)) |}]
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
  [%expect {| |}]
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

let%expect_test "Unop var erased" =
  go
    {|
let dyn = true @ erased;;
let _ =
  !dyn
;;|};
  [%expect {| |}]
;;

let%expect_test "Unop var erased" =
  go
    {|
let dyn = true @ erased;;
let _ =
  !(!dyn @ erased)
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
  [%expect {| ((loc ((line 3) (column 8))) (reason Dynamic_erased)) |}]
;;

let%expect_test "Unop var dynamic" =
  go
    {|
let dyn = true @ dynamic;;
let x = !dyn @ erased;;|};
  [%expect {| |}]
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
  [%expect {| |}]
;;

let%expect_test "Unop erased dynamic" =
  go
    {|
let _ =
  !(true @ erased dynamic)
;;|};
  [%expect {| ((loc ((line 3) (column 2))) (reason Dynamic_erased)) |}]
;;

let%expect_test "Binop erased dynamic" =
  go
    {|
let _ =
  1 + (2 + 3 @ erased dynamic)
;;|};
  [%expect {| ((loc ((line 3) (column 4))) (reason Dynamic_erased)) |}]
;;

let%expect_test "Binop erased dynamic" =
  go
    {|
let _ =
  1 + (2 @ dynamic) + (3 @ erased)
;;|};
  [%expect {| |}]
;;

let%expect_test "Binop erased static" =
  go
    {|
let _ =
  1 + ((2 + 3) @ erased)
;;|};
  [%expect {| |}]
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
  [%expect {| |}]
;;

let%expect_test "Binop erased + erased" =
  go
    {|
let dyn1 = 1 @ erased;;
let dyn2 = 2 @ erased;;
let _ =
  dyn1 + dyn2
;;|};
  [%expect {| |}]
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
  [%expect {| |}]
;;

let%expect_test "If dynamic erased cond" =
  go
    {|
let x = true @ dynamic erased;;
let _ =
  if x then 1 else 2
;;|};
  [%expect {| ((loc ((line 4) (column 2))) (reason Dynamic_erased)) |}]
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
  [%expect {| |}]
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
     (reason (Type_mismatch (got (Type Bool)) (need (Type Int)))))
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
  [%expect {| |}]
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
  [%expect {| |}]
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
let _ =
  let x = 1 @ erased in
  x + 1
;;|};
  [%expect {| |}]
;;

let%expect_test "Let erased" =
  go
    {|
let _ =
  let x = 1 @ erased in
  let y = 1 @ dynamic in
  x + y
;;|};
  [%expect {| |}]
;;

let%expect_test "Let erased" =
  go
    {|
let _ =
  let x = 1 @ erased in
  let y = 1 @ erased in
  0 + (x + y)
;;|};
  [%expect {| |}]
;;

let%expect_test "Let erased" =
  go
    {|
let _ =
  let x = 1 in
  let y = 1 in
  0 + ((x + y) @ erased)
;;|};
  [%expect {| |}]
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
    ((loc ((line 4) (column 8)))
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
  [%expect {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
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
  [%expect {|
    ((loc ((line 3) (column 31)))
     (reason
      (Mode_mismatch (got ((staticity Phase) (erasure Erased)))
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
let _ = (if true @ dynamic then f else g) 0;;
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
  [%expect {| ((loc ((line 3) (column 38))) (reason Dynamic_erased)) |}]
;;

let%expect_test "closure nest" =
  go
    {|
let f1 = (fn (x : int) -> 1) @ erased;;
let g = fn (f2 : int -> int) -> f2 0;;
let _ = g f1;;
|};
  [%expect {|
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
  [%expect {| ((loc ((line 3) (column 39))) (reason Dynamic_erased)) |}]
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
let g = fn (f2 : int -> int) -> f2 0;;
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
    ((loc ((line 6) (column 10)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
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
  [%expect {| |}]
;;

let%expect_test "Apply erased fn dynamic arg" =
  go
    {|
let dyn = 1 @ dynamic erased;;
let y =
  (fn (erased x : int) -> 5) (dyn-1)
;;
let _ = y @ unerased;;|};
  [%expect {| ((loc ((line 4) (column 33))) (reason Dynamic_erased)) |}]
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
  [%expect {| ((loc ((line 4) (column 2))) (reason Dynamic_erased)) |}]
;;

let%expect_test "Apply dynamic fn erased arg" =
  go
    {|
let dyn_fn = (fn (erased x : int) -> 0) @ dynamic;;
let _ =
  dyn_fn (1 @ dynamic)
;;|};
  [%expect {| |}]
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
  [%expect {| ((loc ((line 5) (column 2))) (reason Dynamic_erased)) |}]
;;

let%expect_test "Apply erased fn dynamic arg" =
  go
    {|
let dyn_fn = (fn (erased x : int) -> x) @ erased;;
let dyn_arg = 1 @ dynamic;;
let _ =
  dyn_fn dyn_arg
;;|};
  [%expect {| |}]
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
  [%expect {|
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
  [%expect {| |}]
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
     (reason (Type_mismatch (got (Type Int)) (need (Type Type)))))
    |}]
;;

let%expect_test "mono fn" =
  go
    {|
let x = fn (static erased x : type) -> x;;
let y = (x int) @ dynamic;;
|};
  [%expect {| |}]
;;

let%expect_test "Dependent lambda" =
  go
    {|
let f = fn (erased ty : type) -> fn (x : ty) -> x;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 33)))
     (reason
      (Mode_mismatch (got ((staticity Phase) (erasure Erased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
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
      (Mode_mismatch (got ((staticity Phase) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "arrow typechecking" =
  go
    {|
let f = fn (_ : int) -> 0 @ erased;;
let _ = (f @ dynamic) 0;;
|};
  [%expect {| ((loc ((line 3) (column 8))) (reason Dynamic_erased)) |}]
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
      (Mode_mismatch (got ((staticity Phase) (erasure Unerased)))
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
let _ = f (fn (x : int) -> 0);;
|};
  [%expect {| ((loc ((line 2) (column 45))) (reason Dynamic_erased)) |}]
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
  let f = fn (g : erased int -> int) -> g 0;;
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
      (Mode_mismatch (got ((staticity Phase) (erasure Unerased)))
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
let f = fn (static erased g : static erased type -> int) -> g unit;;
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
let f = fn (static erased g : static erased type -> unit) -> g unit;;
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
let f = fn (static erased g : static erased type -> unit) -> g int;;
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
  [%expect {| |}]
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
let f = fn (static x : int) -> if static x == 0 then 1 else true;;
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
let f = fn (static x : int) -> (if static x == 0 then 1 else true) : int;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 67)))
     (reason
      (Type_mismatch
       (got
        (If (cond (Bool (Eq (Var (Anon <opaque>)) (Int (T 0)))))
         (then_ (Type Int)) (else_ (Type Bool))))
       (need (Type Int)))))
    |}]
;;

let%expect_test "dependent if" =
  go
    {|
let _ = (if static true then 1 else true) : (if true then int else bool);;
|};
  [%expect {| |}]
;;

let%expect_test "dependent if" =
  go
    {|
let _ = (if static true then 1 else true) : (if static true then int else bool);;
|};
  [%expect {| |}]
;;

let%expect_test "dependent if" =
  go
    {|
let _ = (if static true then 1 else true + 1) : int;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 41)))
     (reason (Type_mismatch (got (Type Bool)) (need (Type Int)))))
    |}]
;;

let%expect_test "dependent if" =
  go
    {|
let _ = fn (static x : int) -> (if static x==0 then 1 else true + 1) : int;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 64)))
     (reason (Type_mismatch (got (Type Bool)) (need (Type Int)))))
    |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static _ : unit) -> int;;
let _ = (if static true then 1 else (0 : f ())) : int;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static x : int) -> (if static x == 0 then 1 else true) : (if x == 0 then int else bool);;
|};
  [%expect {| |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static x : int) -> (if static x == 0 then 1 else true) : (if static x == 0 then int else bool);;
|};
  [%expect {| |}]
;;

let%expect_test "bad annotation" =
  go
    {|
let f = fn (static x : int) -> (if static x == 0 then 1 else true) : (if static x == 0 then 0 else bool);;
|};
  [%expect
    {|
    ((loc ((line 2) (column 67)))
     (reason
      (Type_mismatch
       (got
        (If (cond (Bool (Eq (Var (Anon <opaque>)) (Int (T 0)))))
         (then_ (Type Int)) (else_ (Type Type))))
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
let f = fn (static x : int) -> if static x == 0 then 1 else true;;
let g = fn (static x : int) -> if static x == 0 then 1 else true;;
let _ = if true then f else g;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent if unify" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else true;;
let g = fn (static x : int) -> if static x == 0 then true else 2;;
let _ = if true then f 0 else g 1;;
|};
  [%expect {| |}]
;;

let%expect_test "symbolic arrow type" =
  go
    {|
let choose = fn (static f : static int \ x -> if x == 0 then int else bool) -> f 0;;
let _ = choose (fn (static x : int) -> if static x == 0 then 0 else true);;
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
      (Mode_mismatch (got ((staticity Phase) (erasure Erased)))
       (need ((staticity Phase) (erasure Unerased))))))
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
  [%expect {| |}]
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

let%expect_test "dependent fun " =
  go
    {|
fun id (erased t : type) : t -> t = fn (x : t) -> x;;
|};
  [%expect {| ((loc ((line 2) (column 27))) (reason (Unbound_ident ((Id t) <opaque>)))) |}]
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
  [%expect {| ((loc ((line 3) (column 8))) (reason Dynamic_erased)) |}]
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
let x = if static false then a else b;;
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
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Static) (erasure Erased))))))
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
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Static) (erasure Erased))))))
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
  [%expect
    {|
    ((loc ((line 2) (column 4)))
     (reason
      (Mode_mismatch (got ((staticity Phase) (erasure Unerased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
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
    ((loc ((line 2) (column 4)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "fun" =
  go
    {|
fun f (x : int) : (int @ dynamic) = x;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 4)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Static) (erasure Erased))))))
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
     (reason (Type_mismatch (got (Type Type)) (need (Type Int)))))
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
external f : int -> int = asdf;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "external" =
  go
    {|
external f : int -> int = asdf;;
let _ = (f @ erased) 0;;
|};
  [%expect {| |}]
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
external f : int -> int = asdf;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "external" =
  go
    {|
external f : int -> static int = asdf;;
let _ = assert static (f 0 == 0);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Static_assert
       (Bool
        (Eq
         (Apply
          (fn
           (External (symbol asdf)
            (ty
             (Type
              (Arrow (arg_ty (Type Int))
               (arg_mode ((staticity Dynamic) (erasure Unerased)))
               (ret_ty (Type Int))
               (ret_mode ((staticity Static) (erasure Unerased))))))))
          (arg (Int (T 0))))
         (Int (T 0)))))))
    |}]
;;

let%expect_test "nested if static with different types per level" =
  go
    {|
let f = fn (static x : int) -> fn (static y : int) ->
  if static x == 0 then
    (if static y == 0 then 1 else true)
  else
    (if static y == 0 then () else 2);;
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

let%expect_test "assert static" =
  go
    {|
let _ = assert static true;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert static" =
  go
    {|
let _ = assert static false;;
  |};
  [%expect {| ((loc ((line 2) (column 8))) (reason (Static_assert (Bool (T false))))) |}]
;;

let%expect_test "assert static" =
  go
    {|
let x = true @ dynamic;;
let _ = assert static x;;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "assert static" =
  go
    {|
let _ = fn (static x : bool) -> if x then assert static x else ();;
  |};
  [%expect {| ((loc ((line 2) (column 42))) (reason (Static_assert (Var (Anon <opaque>))))) |}]
;;

let%expect_test "assert static" =
  go
    {|
let _ = fn (static x : bool) -> if x then () else assert static x;;
  |};
  [%expect {| ((loc ((line 2) (column 50))) (reason (Static_assert (Var (Anon <opaque>))))) |}]
;;

let%expect_test "assert static" =
  go
    {|
let _ = fn (static x : bool) -> assert static x;;
  |};
  [%expect {| ((loc ((line 2) (column 32))) (reason (Static_assert (Var (Anon <opaque>))))) |}]
;;

let%expect_test "assert static" =
  go
    {|
let _ = fn (static x : bool) -> if static x then assert static x else ();;
  |};
  [%expect {| |}]
;;
