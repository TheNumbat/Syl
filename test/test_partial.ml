open! Core
open! Syl

let go ?(print = false) input =
  let cst = Parse.parse_exn input in
  let dst = Desugar.desugar cst in
  match Typecheck.typecheck dst with
  | Ok tst -> if print then print_s [%message (tst : Tst.Program.t)]
  | Error { loc; reason } -> print_s [%message (loc : Lex.Location.t) (reason : Typecheck.Error.t)]
;;

let%expect_test "multi-arg fn" =
  go
    {|
let _ = fn (x : int) (y : int) -> x + y;;
  |};
  [%expect {| |}]
;;

let%expect_test "multi-arg fn type error" =
  go
    {|
let _ = fn (x : int) (y : bool) -> x + y;;
  |};
  [%expect
    {|
    ((loc ((line 2) (column 37)))
     (reason
      (Type_mismatch (got (Type (Tuple ((Type Int) (Type Bool)))))
       (need (Type (Tuple ((Type Int) (Type Int))))))))
    |}]
;;

let%expect_test "multi-arg fun" =
  go
    {|
fun add (x : int) (y : int) : int = x + y;;
let _ = add 1 2;;
  |};
  [%expect {| |}]
;;

let%expect_test "multi-arg fun type error" =
  go
    {|
fun add (x : int) (y : int) : bool = x + y;;
  |};
  [%expect
    {|
    ((loc ((line 2) (column 4)))
     (reason
      (Type_mismatch
       (got
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret_ty (Type Int))
          (ret_mode ((staticity Static) (erasure Unerased))))))
       (need
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased)))
          (ret_ty (Type Bool))
          (ret_mode ((staticity Dynamic) (erasure Unerased)))))))))
    |}]
;;

let%expect_test "multi-arg fun capture dynamic" =
  go
    {|
let z = 0 @ dynamic;;
fun add (x : int) (y : int) : int = x + y + z;;
  |};
  [%expect {| |}]
;;

let%expect_test "multi-arg fun capture dynamic return static" =
  go
    {|
let z = 0 @ dynamic;;
fun add (x : int) (y : int) : static int = x + y + z;;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 4)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "multi-arg fun return static" =
  go
    {|
fun add (x : int) (y : int) : static int = x + y;;
  |};
  [%expect {| |}]
;;

let%expect_test "multi-arg fun capture return erased" =
  go
    {|
fun add (x : int) (y : int) : erased int = x + y;;
  |};
  [%expect {| |}]
;;

let%expect_test "multi-arg fun capture return erased" =
  go
    {|
fun add (x : int) (y : int) : erased int = x + y;;
let f = (add 0) @ unerased;;
let g = f 1;;
  |};
  [%expect {| |}]
;;

let%expect_test "multi-arg lambda capture erased" =
  go
    {|
let add = fn (erased x : int) -> x + 1;;
let f = (add 0) @ unerased;;
  |};
  [%expect
    {|
    ((loc ((line 2) (column 35)))
     (reason
      (Mode_mismatch (got ((staticity Parametric) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "multi-arg lambda capture erased" =
  go
    {|
let add = fn (erased x : int) (y : int) -> x + y;;
let f = (add 0) @ unerased;;
  |};
  [%expect
    {|
    ((loc ((line 2) (column 45)))
     (reason
      (Mode_mismatch (got ((staticity Parametric) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "multi-arg lambda capture erased" =
  go
    {|
let add = fn (erased x : int) (erased y : int) -> x + y;;
let f = (add 0) @ unerased;;
  |};
  [%expect
    {|
    ((loc ((line 2) (column 52)))
     (reason
      (Mode_mismatch (got ((staticity Parametric) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "multi-arg fun capture erased" =
  go
    {|
fun add (erased x : int) (y : int) : int = x + y;;
let f = (add 0) @ unerased;;
  |};
  [%expect
    {|
    ((loc ((line 2) (column 45)))
     (reason
      (Mode_mismatch (got ((staticity Parametric) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "multi-arg fun capture erased" =
  go
    {|
fun add (erased x : int) (erased y : int) : int = x + y;;
let f = (add 0) @ unerased;;
  |};
  [%expect
    {|
    ((loc ((line 2) (column 52)))
     (reason
      (Mode_mismatch (got ((staticity Parametric) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "multi-arg fun capture return erased" =
  go
    {|
fun erased add (x : int) (y : int) : int = x + y;;
let f = (add 0) @ unerased;;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 16)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "multi-arg fun with static erased" =
  go
    {|
fun id (static erased t : type) (x : t) : t = x;;
let _ = id int 42;;
  |};
  [%expect {| |}]
;;

let%expect_test "multi-arg fun 3 args" =
  go
    {|
fun f (a : int) (b : int) (c : int) : int = a + b + c;;
let _ = f 1 2 3;;
  |};
  [%expect {| |}]
;;

let%expect_test "multi-arg fun partial application" =
  go
    {|
fun add (x : int) (y : int) : int = x + y;;
let inc = add 1;;
let _ = inc 5;;
  |};
  [%expect {| |}]
;;

let%expect_test "multi-arg fn erased" =
  go
    {|
let f = fn erased (x : int) (y : int) -> x + y;;
let _ = f 1 2;;
  |};
  [%expect {| |}]
;;

let%expect_test "multi-arg fn erased cannot weaken" =
  go
    {|
let f = fn erased (x : int) (y : int) -> x + y;;
let _ = (f @ unerased);;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 11)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "multi-arg fn erased partial app is erased" =
  go
    {|
let f = fn erased (x : int) (y : int) -> x + y;;
let _ = (f 1 @ unerased);;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 13)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "multi-arg fun erased" =
  go
    {|
fun erased f (x : int) (y : int) : int = x + y;;
let _ = f 1 2;;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "multi-arg fun erased cannot weaken" =
  go
    {|
fun erased f (x : int) (y : int) : int = x + y;;
let _ = (f @ unerased);;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 11)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "multi-arg fun erased partial app is erased" =
  go
    {|
fun erased f (x : int) (y : int) : int = x + y;;
let _ = (f 1 @ unerased);;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 13)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "multi-arg fun erased with static erased" =
  go
    {|
fun erased id (static erased t : type) (x : t) : t = x;;
let _ = id int 42;;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "let with args" =
  go
    {|
let add (x : int) (y : int) = x + y;;
let _ = add 1 2;;
  |};
  [%expect {| |}]
;;

let%expect_test "let with one arg" =
  go
    {|
let f (x : int) = x + 1;;
let _ = f 5;;
  |};
  [%expect {| |}]
;;

let%expect_test "let with args type error" =
  go
    {|
let add (x : int) (y : bool) = x + y;;
  |};
  [%expect
    {|
    ((loc ((line 2) (column 33)))
     (reason
      (Type_mismatch (got (Type (Tuple ((Type Int) (Type Bool)))))
       (need (Type (Tuple ((Type Int) (Type Int))))))))
    |}]
;;

let%expect_test "let with args inner" =
  go
    {|
let _ =
  let add (x : int) (y : int) = x + y in
  add 3 4
;;
  |};
  [%expect {| |}]
;;

let%expect_test "let with static erased arg" =
  go
    {|
let id (static erased t : type) (x : t) = x;;
let _ = id int 42;;
  |};
  [%expect {| |}]
;;

let%expect_test "let erased no args" =
  go
    {|
let erased x = 42;;
let _ = x;;
  |};
  [%expect {| |}]
;;

let%expect_test "let erased with args" =
  go
    {|
let erased f (x : int) = x + 1;;
let _ = f 5;;
  |};
  [%expect {| |}]
;;

let%expect_test "let erased with args multi" =
  go
    {|
let erased add (x : int) (y : int) = x + y;;
let _ = add 3 4;;
  |};
  [%expect {| |}]
;;

let%expect_test "let erased cannot weaken to unerased" =
  go
    {|
let erased x = 42;;
let _ = x @ unerased;;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 10)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "let erased inner" =
  go
    {|
let _ =
  let erased f (x : int) = x + 1 in
  f 5
;;
  |};
  [%expect {| |}]
;;

let%expect_test "multi-arg fun type matches annotation" =
  go
    {|
fun f (x : int) (y : int) : int = x + y;;
let _ = f : int -> int -> int;;
  |};
  [%expect {| |}]
;;

let%expect_test "multi-arg fun type mismatch annotation" =
  go
    {|
fun f (x : int) (y : int) : int = x + y;;
let _ = f : int -> int;;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 10)))
     (reason
      (Type_mismatch
       (got
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased)))
          (ret_ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (ret_mode ((staticity Dynamic) (erasure Unerased))))))
       (need
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased))) (ret_ty (Type Int))
          (ret_mode ((staticity Dynamic) (erasure Unerased)))))))))
    |}]
;;

let%expect_test "multi-arg let type matches annotation" =
  go
    {|
let add (x : int) (y : int) = x + y;;
let _ = add : int -> int -> int;;
  |};
  [%expect {| |}]
;;

let%expect_test "multi-arg let type matches annotation with static ret_mode" =
  go
    {|
let add (x : int) (y : int) = x + y;;
let _ = add : int -> static (int -> int);;
  |};
  [%expect {| |}]
;;

let%expect_test "multi-arg fn type matches annotation" =
  go
    {|
let f = fn (x : int) (y : bool) -> x;;
let _ = f : int -> bool -> int;;
  |};
  [%expect {| |}]
;;

let%expect_test "multi-arg fun 3 args type matches" =
  go
    {|
fun f (a : int) (b : bool) (c : unit) : int = a;;
let _ = f : int -> bool -> unit -> int;;
  |};
  [%expect {| |}]
;;

let%expect_test "multi-arg fun with static erased type matches" =
  go
    {|
fun id (static erased t : type) (x : t) : t = x;;
let _ = id : static erased type \ t -> t -> t;;
  |};
  [%expect {| |}]
;;

let%expect_test "multi-arg let with static erased type matches" =
  go
    {|
let id (static erased t : type) (x : t) = x;;
let _ = id : static erased type \ t -> t -> t;;
  |};
  [%expect {| |}]
;;

let%expect_test "multi-arg fun erased type matches with erased ret_mode" =
  go
    {|
fun erased f (x : int) (y : int) : erased int = x + y;;
let _ = f : int -> erased (int -> erased int);;
  |};
  [%expect {| |}]
;;

let%expect_test "multi-arg fun erased type mismatch unerased ret_mode" =
  go
    {|
fun erased f (x : int) (y : int) : int = x + y;;
let _ = f : int -> erased (int -> int);;
  |};
  [%expect {| |}]
;;

let%expect_test "multi-arg fun erased type mismatch unerased ret_mode" =
  go
    {|
fun erased f (x : int) (y : int) : int = x + y;;
let _ = f : int -> int -> int;;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 10)))
     (reason
      (Type_mismatch
       (got
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased)))
          (ret_ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (ret_mode ((staticity Dynamic) (erasure Erased))))))
       (need
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased)))
          (ret_ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (ret_mode ((staticity Dynamic) (erasure Unerased)))))))))
    |}]
;;

let%expect_test "multi-arg fun erased type mismatch unerased ret_mode" =
  go
    {|
fun erased f (x : int) (y : int) : static int = x + y;;
let _ = f : int -> static erased (int -> int);;
  |};
  [%expect {| |}]
;;

let%expect_test "multi-arg fun erased type mismatch unerased ret_mode" =
  go
    {|
fun erased f (x : int) (y : int) : static int = x + y;;
let _ = f : int -> static (int -> int);;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 10)))
     (reason
      (Type_mismatch
       (got
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased)))
          (ret_ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Static) (erasure Unerased))))))
          (ret_mode ((staticity Static) (erasure Erased))))))
       (need
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased)))
          (ret_ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (ret_mode ((staticity Static) (erasure Unerased)))))))))
    |}]
;;

let%expect_test "multi-arg let erased type matches with erased ret_mode" =
  go
    {|
let erased add (x : int) (y : int) = x + y;;
let _ = add : int -> static erased (int -> int);;
  |};
  [%expect {| |}]
;;

let%expect_test "multi-arg let erased type mismatch unerased ret_mode" =
  go
    {|
let erased add (x : int) (y : int) = x + y;;
let _ = add : int -> static erased (int -> int);;
  |};
  [%expect {| |}]
;;

let%expect_test "multi-arg let erased type mismatch unerased ret_mode" =
  go
    {|
let erased add (x : int) (y : int) = x + y;;
let _ = add : int -> static (int -> int);;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 12)))
     (reason
      (Type_mismatch
       (got
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased)))
          (ret_ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Static) (erasure Unerased))))))
          (ret_mode ((staticity Static) (erasure Erased))))))
       (need
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased)))
          (ret_ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (ret_mode ((staticity Static) (erasure Unerased)))))))))
    |}]
;;

let%expect_test "multi-arg fn erased type matches with erased ret_mode" =
  go
    {|
let f = fn erased (x : int) (y : int) -> x + y;;
let _ = f : int -> static erased (int -> int);;
  |};
  [%expect {| |}]
;;

let%expect_test "multi-arg fn erased type matches with erased ret_mode" =
  go
    {|
let f = fn erased (x : int) (y : int) -> x + y;;
let _ = f : int -> static (int -> int);;
  |};
  [%expect {|
    ((loc ((line 3) (column 10)))
     (reason
      (Type_mismatch
       (got
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased)))
          (ret_ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Static) (erasure Unerased))))))
          (ret_mode ((staticity Static) (erasure Erased))))))
       (need
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased)))
          (ret_ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (ret_mode ((staticity Static) (erasure Unerased)))))))))
    |}]
;;

let%expect_test "multi-arg fn erased type mismatch unerased ret_mode" =
  go
    {|
let f = fn erased (x : int) (y : int) -> x + y;;
let _ = f : int -> int -> int;;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 10)))
     (reason
      (Type_mismatch
       (got
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased)))
          (ret_ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Static) (erasure Unerased))))))
          (ret_mode ((staticity Static) (erasure Erased))))))
       (need
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased)))
          (ret_ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty (Type Int))
             (ret_mode ((staticity Dynamic) (erasure Unerased))))))
          (ret_mode ((staticity Dynamic) (erasure Unerased)))))))))
    |}]
;;

let%expect_test "multi-arg fun erased 3 args type matches" =
  go
    {|
fun erased f (a : int) (b : int) (c : int) : int = a + b + c;;
let _ = f : int -> static erased (int -> static erased (int -> int));;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 10)))
     (reason
      (Type_mismatch
       (got
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased)))
          (ret_ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty
              (Type
               (Arrow (arg_ty (Type Int))
                (arg_mode ((staticity Dynamic) (erasure Unerased)))
                (ret_ty (Type Int))
                (ret_mode ((staticity Dynamic) (erasure Unerased))))))
             (ret_mode ((staticity Dynamic) (erasure Erased))))))
          (ret_mode ((staticity Dynamic) (erasure Erased))))))
       (need
        (Type
         (Arrow (arg_ty (Type Int))
          (arg_mode ((staticity Dynamic) (erasure Unerased)))
          (ret_ty
           (Type
            (Arrow (arg_ty (Type Int))
             (arg_mode ((staticity Dynamic) (erasure Unerased)))
             (ret_ty
              (Type
               (Arrow (arg_ty (Type Int))
                (arg_mode ((staticity Dynamic) (erasure Unerased)))
                (ret_ty (Type Int))
                (ret_mode ((staticity Dynamic) (erasure Unerased))))))
             (ret_mode ((staticity Static) (erasure Erased))))))
          (ret_mode ((staticity Static) (erasure Erased)))))))))
    |}]
;;

let%expect_test "multi-arg fun erased 3 args full application" =
  go
    {|
fun erased f (a : int) (b : int) (c : int) : int = a + b + c;;
let _ = f 1 2 3;;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "multi-arg fun erased 3 args second partial app is erased" =
  go
    {|
fun erased f (a : int) (b : int) (c : int) : int = a + b + c;;
let _ = (f 1 2) @ unerased;;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 9)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Dynamic) (erasure Unerased))))))
    |}]
;;

let%expect_test "multi-arg fn erased 3 args type matches" =
  go
    {|
let f = fn erased (a : int) (b : int) (c : int) -> a + b + c;;
let _ = f : int -> static erased (int -> static erased (int -> int));;
  |};
  [%expect {| |}]
;;

let%expect_test "multi-arg fun erased partial app type annotation" =
  go
    {|
fun erased f (x : int) (y : int) : int = x + y;;
let _ = (f 1) : int -> int;;
  |};
  [%expect {| |}]
;;

let%expect_test "multi-arg fn erased partial app cannot weaken" =
  go
    {|
let f = fn erased (x : int) (y : int) -> x + y;;
let _ = (f 1) @ unerased;;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 14)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "multi-arg let erased partial app cannot weaken" =
  go
    {|
let erased add (x : int) (y : int) = x + y;;
let _ = (add 1) @ unerased;;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 16)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "fun partial app is dynamic" =
  go
    {|
fun f (x : int) (y : int) : int = x + y;;
let _ = (f 1) @ dynamic;;
  |};
  [%expect {| |}]
;;

let%expect_test "fun partial app is static" =
  go
    {|
fun f (x : int) (y : int) : int = x + y;;
let _ = (f 1) @ static;;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 14)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "fn partial app is static" =
  go
    {|
let f = fn (x : int) (y : int) -> x + y;;
let _ = (f 1) @ static;;
  |};
  [%expect {| |}]
;;

let%expect_test "let partial app is static" =
  go
    {|
let add (x : int) (y : int) = x + y;;
let _ = (add 1) @ static;;
  |};
  [%expect {| |}]
;;

let%expect_test "fun erased partial app is static erased" =
  go
    {|
fun erased f (x : int) (y : int) : int = x + y;;
let _ = (f 1) @ static erased;;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 14)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Erased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "fn erased partial app is static erased" =
  go
    {|
let f = fn erased (x : int) (y : int) -> x + y;;
let _ = (f 1) @ static erased;;
  |};
  [%expect {| |}]
;;

let%expect_test "let erased partial app is static erased" =
  go
    {|
let erased add (x : int) (y : int) = x + y;;
let _ = (add 1) @ static erased;;
  |};
  [%expect {| |}]
;;

let%expect_test "fun two static type args" =
  go
    {|
fun id (static erased t : type) (static erased u : type) (x : t) (y : u) : t = x;;
let _ = id int bool 1 true;;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "fn two static type args" =
  go
    {|
let f = fn (static erased t : type) (static erased u : type) (x : t) (y : u) -> x;;
let _ = f int bool 1 true;;
  |};
  [%expect {| |}]
;;

let%expect_test "let two static type args" =
  go
    {|
let f (static erased t : type) (static erased u : type) (x : t) (y : u) = x;;
let _ = f int bool 1 true;;
  |};
  [%expect {| |}]
;;

let%expect_test "fn static arg scopes into later arg types" =
  go
    {|
let pair = fn (static erased t : type) (static erased u : type) (x : t) (y : u) -> (x, y);;
let _ = pair int bool 42 true;;
  |};
  [%expect {| |}]
;;

let%expect_test "fn static arg scopes into later args" =
  go
    {|
let apply = fn (static erased t : type) (static erased u : type) (f : t -> u) (x : t) -> f x;;
let _ = apply int bool (fn (x : int) -> x == 0) 5;;
  |};
  [%expect {| |}]
;;

let%expect_test "let static arg scopes into later args" =
  go
    {|
let apply (static erased t : type) (static erased u : type) (f : t -> u) (x : t) = f x;;
let _ = apply int bool (fn (x : int) -> x == 0) 5;;
  |};
  [%expect {| |}]
;;

let%expect_test "fn static arg wrong type for later arg" =
  go
    {|
let f = fn (static erased t : type) (static erased u : type) (x : t) (y : u) -> x;;
let _ = f int bool 1 2;;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Bool)))))
    |}]
;;

let%expect_test "let static arg wrong type for later arg" =
  go
    {|
let f (static erased t : type) (static erased u : type) (x : t) (y : u) = x;;
let _ = f int bool 1 2;;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Bool)))))
    |}]
;;

let%expect_test "fn dynamic then static" =
  go
    {|
let f = fn (x : int) (static erased t : type) (y : t) -> x;;
let _ = f 1 int 2;;
  |};
  [%expect {| |}]
;;

let%expect_test "fn dynamic then static mode mismatch inline" =
  go
    {|
let f = fn (x : int) (static erased t : type) (y : t) -> x;;
let _ = f (1 @ dynamic) int 2;;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "fn dynamic then static mode mismatch partial app" =
  go
    {|
let f = fn (x : int) (static erased t : type) (y : t) -> x;;
let g = f (1 @ dynamic);;
let _ = g int;;
  |};
  [%expect
    {|
    ((loc ((line 4) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "let dynamic then static" =
  go
    {|
let f (x : int) (static erased t : type) (y : t) = x;;
let _ = f 1 int 2;;
  |};
  [%expect {| |}]
;;

let%expect_test "fun dynamic then static" =
  go
    {|
fun f (x : int) (static erased t : type) (y : t) : int = x;;
let _ = f 1 int 2;;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Erased))))))
    |}]
;;

let%expect_test "fn dynamic then static type mismatch" =
  go
    {|
let f = fn (x : int) (static erased t : type) (y : t) -> y;;
let _ = f 1 bool 2;;
  |};
  [%expect
    {|
    ((loc ((line 3) (column 8)))
     (reason (Type_mismatch (got (Type Int)) (need (Type Bool)))))
    |}]
;;

let%expect_test "fn dynamic static dynamic" =
  go
    {|
let f = fn (x : int) (static erased t : type) (y : t) (z : int) -> x + z;;
let _ = f 1 int 2 3;;
  |};
  [%expect {| |}]
;;
