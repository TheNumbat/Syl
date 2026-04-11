open! Core
open! Syl

let go ?(print = false) input =
  let cst = Parse.parse_exn input in
  let dst = Desugar.desugar cst in
  match Typecheck.typecheck dst with
  | Ok tst -> if print then print_s [%message (tst : Tst.Program.t)]
  | Error { loc; here; reason } ->
    if print
    then
      print_s
        [%message
          (loc : Lex.Location.t) (here : Source_code_position.t) (reason : Typecheck.Error.t)]
    else print_s [%message (loc : Lex.Location.t) (reason : Typecheck.Error.t)]
;;

let%expect_test "is type" =
  go
    {|
builtin is_unit = syl_type_is_unit;;
builtin is_bool = syl_type_is_bool;;
builtin is_int = syl_type_is_int;;
builtin is_type = syl_type_is_type;;
builtin is_tuple = syl_type_is_tuple;;
builtin is_arrow = syl_type_is_arrow;;
builtin is_pi = syl_type_is_pi;;

let _ = assert static (is_unit unit);;
let _ = assert static (is_bool bool);;
let _ = assert static (is_int int);;
let _ = assert static (is_type type);;

let _ = assert static !(is_unit bool);;
let _ = assert static !(is_bool int);;
let _ = assert static !(is_int unit);;
let _ = assert static !(is_type (unit -> unit));;

let _ = assert static (is_tuple (int ^ bool));;
let _ = assert static (is_arrow (int -> int));;
let _ = assert static (is_pi (static int -> int));;

let _ = assert static !(is_tuple unit);;
let _ = assert static !(is_arrow (static int -> int));;
let _ = assert static !(is_pi (int -> int));;
|};
  [%expect {| |}]
;;

let%expect_test "arg ret get" =
  go
    {|
builtin arrow_arg = syl_type_arrow_arg;;
builtin arrow_ret = syl_type_arrow_ret;;
builtin pi_arg = syl_type_pi_arg;;

let _ = 0 : arrow_arg (int -> unit);;
let _ = 0 : arrow_ret (unit -> int);;
let _ = 0 : pi_arg (static int -> unit);;
|};
  [%expect {| |}]
;;

let%expect_test "arg get wrong type" =
  go
    {|
builtin arrow_arg = syl_type_arrow_arg;;

let _ = 0 : arrow_arg int;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 12)))
     (reason (Static_failure (Expected_arrow (Type Int)))))
    |}]
;;

let%expect_test "ret get wrong type" =
  go
    {|
builtin arrow_ret = syl_type_arrow_ret;;

let _ = 0 : arrow_ret int;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 12)))
     (reason (Static_failure (Expected_arrow (Type Int)))))
    |}]
;;

let%expect_test "pi get wrong type" =
  go
    {|
builtin pi_arg = syl_type_pi_arg;;

let _ = 0 : pi_arg (int -> int);;
|};
  [%expect
    {|
    ((loc ((line 4) (column 12)))
     (reason
      (Static_failure
       (Expected_pi
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret_ty (Type Int))
          (ret_mode ((staticity Dynamic) (erasure Unerased)))))))))
    |}]
;;

let%expect_test "tuple type length" =
  go
    {|
builtin length = syl_type_tuple_length;;
let t1 = int ^ bool;;
let _ = assert static (length t1 == 2);;
let _ = assert static (length (unit ^ (unit ^ bool)) == 2);;
let _ = assert static (length (unit ^ unit ^ bool) == 3);;
|};
  [%expect {| |}]
;;

let%expect_test "tuple type length wrong type" =
  go
    {|
builtin length = syl_type_tuple_length;;
let _ = assert static (length (int -> bool) == 2);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 23)))
     (reason
      (Static_failure
       (Expected_tuple
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased)))
          (ret_ty (Type Bool))
          (ret_mode ((staticity Dynamic) (erasure Unerased)))))))))
    |}]
;;

let%expect_test "tuple type get" =
  go
    {|
builtin get = syl_type_tuple_get;;
let t1 = int ^ bool;;
let _ = 0 : get (t1, 0);;
let _ = true : get (t1, 1);;
|};
  [%expect {| |}]
;;

let%expect_test "tuple wrong type" =
  go
    {|
builtin get = syl_type_tuple_get;;
let _ = get (int, 0);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason (Static_failure (Expected_tuple (Type Int)))))
    |}]
;;

let%expect_test "tuple type get out of bounds" =
  go
    {|
builtin get = syl_type_tuple_get;;
let t1 = int ^ bool;;
let _ = get (t1, -1);;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason (Static_failure (Out_of_bounds (idx -1) (len 2)))))
    |}]
;;

let%expect_test "tuple type get out of bounds" =
  go
    {|
builtin get = syl_type_tuple_get;;
let t1 = int ^ bool;;
let _ = get (t1, 2);;
|};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason (Static_failure (Out_of_bounds (idx 2) (len 2)))))
    |}]
;;

(* TODO need to match on t to narrow to tuple (same for value get) *)
let%expect_test "abstract tuple type get" =
  go
    {|
builtin get = syl_type_tuple_get;;
let get = fn (static erased t : type) -> fn (static erased idx : int) -> get (t, idx);;
let get = get (int ^ bool);;
let _ = 0 : get 0;;
let _ = true : get 1;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 73)))
     (reason (Static_failure (Expected_tuple (Var (Anon <opaque>))))))
    |}]
;;
