open! Core
open! Syl

let go = Common.typecheck

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

let _ = assert erased (is_unit unit);;
let _ = assert erased (is_bool bool);;
let _ = assert erased (is_int int);;
let _ = assert erased (is_type type);;

let _ = assert erased !(is_unit bool);;
let _ = assert erased !(is_bool int);;
let _ = assert erased !(is_int unit);;
let _ = assert erased !(is_type (unit -> unit));;

let _ = assert erased (is_tuple (int ^ bool));;
let _ = assert erased (is_arrow (int -> int));;
let _ = assert erased (is_pi (static int -> int));;

let _ = assert erased !(is_tuple unit);;
let _ = assert erased !(is_arrow (static int -> int));;
let _ = assert erased !(is_pi (int -> int));;
|};
  [%expect
    {|
    ((loc ((line 15) (column 22)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
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

(* Static computation is lazy, so the lengths are demanded through a static
   erased key. Asserting the values (length t1 == 2) needs prims that compute
   on erased ints (or unerase prims). *)
let%expect_test "tuple type length" =
  go
    {|
builtin length = syl_type_tuple_length;;
let use = fn (static erased n : int) -> ();;
let t1 = int ^ bool;;
let _ = use (length t1);;
let _ = use (length (unit ^ (unit ^ bool)));;
let _ = use (length (unit ^ unit ^ bool));;
|};
  [%expect {| |}]
;;

let%expect_test "tuple type length wrong type" =
  go
    {|
builtin length = syl_type_tuple_length;;
let use = fn (static erased n : int) -> ();;
let _ = use (length (int -> bool));;
|};
  [%expect
    {|
    ((loc ((line 4) (column 13)))
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
let _ = 0 : get (int, 0);;
|};
  [%expect
    {|
    ((loc ((line 3) (column 12)))
     (reason (Static_failure (Expected_tuple (Type Int)))))
    |}]
;;

let%expect_test "tuple type get out of bounds" =
  go
    {|
builtin get = syl_type_tuple_get;;
let t1 = int ^ bool;;
let _ = 0 : get (t1, -1);;
|};
  [%expect
    {|
    ((loc ((line 4) (column 12)))
     (reason (Static_failure (Out_of_bounds (idx -1) (len 2)))))
    |}]
;;

let%expect_test "tuple type get out of bounds" =
  go
    {|
builtin get = syl_type_tuple_get;;
let t1 = int ^ bool;;
let _ = 0 : get (t1, 2);;
|};
  [%expect
    {|
    ((loc ((line 4) (column 12)))
     (reason (Static_failure (Out_of_bounds (idx 2) (len 2)))))
    |}]
;;

(* Lazy statics defer the abstract [get (t, idx)] to monomorphization time,
   where t and idx are concrete. *)
let%expect_test "abstract tuple type get" =
  go
    {|
builtin get = syl_type_tuple_get;;
let get = fn (static erased t : type) -> fn (static erased idx : int) -> get (t, idx);;
let get = get (int ^ bool);;
let _ = 0 : get 0;;
let _ = true : get 1;;
|};
  [%expect {| |}]
;;
