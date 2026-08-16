open! Core
open! Syl

let go ?print ?(check = `Run) input = Common.codegen ?print ~check input

let%expect_test "closure return type" =
  go
    {|
let _ =
  (fn (x : int) -> int)
;;|};
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
let t = (fn (static erased x : int) -> int) 0;;
let _ = 0 : t;;
|};
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

let%expect_test "lift universal type" =
  go
    {|
let f = fn (static ty : type) -> ty @ erased;;
|};
  [%expect {| |}]
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
let f = fn (static g : static int -> int) -> g 1;;
let _ = f (fn (x : int) -> 0);;
|};
  [%expect {| |}]
;;

let%expect_test "Pi typechecking" =
  go
    {|
let f = fn (static g : static int -> int) -> g 0;;
let _ = f (fn (static x : int) -> x + 1);;
|};
  [%expect {| |}]
;;

let%expect_test "static type" =
  go
    {|
fun f (static _ : unit) : static erased type = int;;
let y = 0 : f ();;
|};
  [%expect {| |}]
;;

let%expect_test "closure where binder expected: Arrow leq Pi" =
  go
    {|
let apply = fn (static f : static int -> int) -> f 0;;
let g = fn (x : int) -> x;;
let _ = apply g;;
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

let%expect_test "pi and arrow join — if choosing between Pi and Arrow" =
  go
    {|
let f = fn (static x : int) -> if erased x == 0 then 1 else true;;
let g = fn (static x : int) -> if erased x == 0 then 1 else true;;
let _ = if true then f else g;;
|};
  [%expect {| |}]
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

let%expect_test "nested if erased with different types per level" =
  go
    {|
fun f (static x : int) : static
  (static int \ y ->
   if x == 0
   then if y == 0 then int else bool
   else if y == 0 then unit else int)
  =
  fun g (static y : int) :
    if x == 0
    then if y == 0 then int else bool
    else if y == 0 then unit else int
  =
    if erased x == 0
    then if erased y == 0 then 1 else true
    else if erased y == 0 then () else 2
   in
  g
;;
let _ = f 0 0;;
let _ = f 0 1;;
let _ = f 1 0;;
let _ = f 1 1;;
|};
  [%expect {| |}]
;;

let%expect_test "nested if erased with different types per level" =
  go
    {|
fun f (static x : int) : static
  (static int \ y ->
   if x == 0
   then if y == 0 then int else bool
   else if y == 0 then unit else int)
  =
  fun g (static y : int) :
    if x == 0
    then if y == 0 then int else bool
    else if y == 0 then unit else int
  =
    if erased x == 0
    then if erased y == 0 then x+y else true
    else if erased y == 0 then () else x-y
   in
  g
;;
let _ = f 0 0;;
let _ = f 0 1;;
let _ = f 1 0;;
let _ = f 1 1;;
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

let%expect_test "join Pi/Pi function-type arg returning type: fresh var issue" =
  go
    {|
let f = fn (static erased g : static int -> static erased type) -> fn (x : g 0) -> x;;
let h = fn (static erased g : static int -> static erased type) -> fn (x : g 0) -> x;;
let x = if true then f else h;;
let _ = x (fn (static x : int) -> int);;
|};
  [%expect {| |}]
;;

let%expect_test "leq Pi/Pi function-type arg returning type" =
  go
    {|
let wrap = fn (static erased f : static int -> static erased type) -> fn (x : f 0) -> x;;
let wrap2 = wrap : static erased (static int -> static erased type) \ f -> f 0 -> f 0;;
let _ = wrap2 (fn (static x : int) -> int);;
|};
  [%expect {| |}]
;;

let%expect_test "meet Pi/Pi function-type arg: via arg contravariance in join" =
  go
    {|
let f = fn (static apply : static (static int -> int) -> int) -> apply (fn (static x : int) -> 0);;
let g = fn (static apply : static (static erased int -> int) -> int) -> apply (fn (static erased x : int) -> 0);;
let x = if true then f else g;;
let _ = x (fn (static f : static int -> int) -> f 0);;
|};
  [%expect {| |}]
;;

let%expect_test "arrow function calling pi function in same group" =
  go
    {|
fun f (static x : int) : int = x;;
let _ = (fn (_ : unit) -> f 0);;
|};
  [%expect {| |}]
;;

let%expect_test "arrow function calling pi function in same group" =
  go
    {|
fun f (static x : int) : int = x;;
let _ = (fn (_ : unit) -> f 0 + f 1);;
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
fun inc (x : int) : int = let _ = choose true in x + 1
and choose (static erased b : bool) : int -> int =
  if erased b then fn (x : int) -> inc x else fn (x : int) -> x;;
let _ = choose true 5;;
let _ = choose false 5;;
|};
  [%expect {| |}]
;;

let%expect_test "pi calling arrow from same group at application" =
  go
    {|
let _ =
fun inc (x : int) : int = let _ = choose true in x + 1
and choose (static erased b : bool) : int -> int =
  if erased b then fn (x : int) -> inc x else fn (x : int) -> x in
let _ = choose true 5 in
let _ = choose false 5 in
();;
|};
  [%expect {| |}]
;;

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

let%expect_test "fuzz: polymorphic pair constructor" =
  go
    {|
let pair_id = fn (static erased t : type) -> fn (x : t) -> fn (y : t) -> (x, y);;
let _ = pair_id int 1 2;;
let _ = pair_id bool true false;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: arrow type annotation" =
  go
    {|
let f = (fn (x : int) -> fn (y : int) -> x + y) : int -> int -> int;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: polymorphic with many type instantiations" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = id int 1;;
let _ = id bool true;;
let _ = id unit ();;
let _ = id (int -> int) (fn (x : int) -> x);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: arrow type as value" =
  go
    {|
let ty = int -> int;;
let f = (fn (x : int) -> x + 1) : ty;;
let _ = f 5;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: just a type" =
  go {|let _ = int;;|};
  [%expect {| |}]
;;

let%expect_test "fuzz: just type of types" =
  go {|let _ = type;;|};
  [%expect {| |}]
;;

let%expect_test "fuzz: type computed from int" =
  go
    {|
fun type_for (static x : int) : static erased type =
  if erased x > 0 then int else bool;;
let _ = (42 : type_for 1);;
let _ = (true : type_for 0);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: chain polymorphic through higher-order" =
  go
    {|
let f = fn (static erased t : type) -> fn (x : t) -> x;;
let g = f int;;
let h = fn (func : int -> int) -> func 42;;
let _ = h g;;
|};
  [%expect {| |}]
;;
