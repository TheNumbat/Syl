open! Core
open! Syl

let go ?print ?(check = `Run) input = Common.codegen ?print ~check input

let%expect_test "names" =
  go
    {|
let x = ();;
let x = ();;
|};
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

let%expect_test "lift universal int" =
  go
    {|
let f = fn (static x : int) -> x @ erased;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "if erased with literal condition true" =
  go
    {|
let _ = if erased true then 1 else true;;
|};
  [%expect {| |}]
;;

let%expect_test "if erased with literal condition false" =
  go
    {|
let _ = if erased false then 1 else true;;
|};
  [%expect {| |}]
;;

let%expect_test "if erased with static variable condition" =
  go
    {|
let f = fn (static x : int) -> if erased x == 0 then 1 else true;;
let a = f 0;;
let b = f 1;;
let _ = print_int a;;
let _ = print_bool b;;
|};
  [%expect
    {|
    1
    true
    |}]
;;

let%expect_test "if erased nested in let expression" =
  go
    {|
let f = fn (static x : int) ->
  let y = if erased x == 0 then 1 else true in
  y;;
let _ = print_int (f 0);;
let _ = print_bool (f 1);;
|};
  [%expect
    {|
    1
    true
    |}]
;;

let%expect_test "case body with local let" =
  go
    {|
builtin add = syl_int_add;;
builtin mul = syl_int_mul;;
fun compute (p : int ^ int) : int =
  match p with
  | (a, b) ->
    let sum = add (a, b) in
    let product = mul (a, b) in
    add (sum, product)
;;
let _ = assert (compute (3, 4) == 19);;
let _ = assert (compute (10, 10) == 120);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: nested dynamic ifs" =
  go
    {|
let a = true @ dynamic;;
let b = false @ dynamic;;
let _ = if a then (if b then 1 else 2) else (if b then 3 else 4);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: if returning unit" =
  go
    {|
let b = true @ dynamic;;
let _ = if b then () else ();;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: variable shadowing" =
  go
    {|
let x = 1;;
let x = 2;;
let x = 3;;
let _ = x;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: shadowing in nested let" =
  go
    {|
let x = 1;;
let _ = let x = 2 in let x = 3 in x;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: deep let nesting" =
  go
    {|
let _ = let a = 1 in let b = a + 1 in let c = b + 1 in let d = c + 1 in d;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: same name different scopes" =
  go
    {|
let f = fn (x : int) -> x;;
let g = fn (x : bool) -> x;;
let _ = f 1;;
let _ = g true;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: let-in in if condition" =
  go
    {|
let _ = if (let x = true in x) then 1 else 2;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: let-in in if branches" =
  go
    {|
let b = true @ dynamic;;
let _ = if b then (let x = 1 in x) else (let y = 2 in y);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: just unit" =
  go {|let _ = ();;|};
  [%expect {| |}]
;;

let%expect_test "fuzz: if with captures in both branches" =
  go
    {|
let threshold = 10 @ dynamic;;
let classify = fn (x : int) -> if x > threshold then true else false;;
let _ = classify (5 @ dynamic);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: deep if-else chain" =
  go
    {|
let x = 5 @ dynamic;;
let _ =
  if x > 10 then 100
  else if x > 5 then 50
  else if x > 0 then 25
  else 0;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: erased let in non-erased scope" =
  go
    {|
let T = int;;
let x = (42 : T);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: capture shadowed variables" =
  go
    {|
let x = 1 @ dynamic;;
let y = 2 @ dynamic;;
let x = 3 @ dynamic;;
let f = fn (_ : unit) -> x + y;;
let _ = f ();;
|};
  [%expect {| |}]
;;
