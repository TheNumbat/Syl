open! Core
open! Syl

let go = Common.typecheck

(* A single arm suffices on a concrete scrutinee: the static value refutes the
   missing cases. *)
let%expect_test "repr folds on concrete types" =
  go
    {|
let _ = assert erased (match erased repr unit { .unit -> true });;
let _ = assert erased (match erased repr bool { .bool -> true });;
let _ = assert erased (match erased repr int { .int -> true });;
let _ = assert erased (match erased repr type { .type -> true });;
let _ = assert erased (match erased repr (int ^ bool) { .tuple -> true });;
let _ = assert erased (match erased repr (int -> bool) { .arrow -> true });;
let _ = assert erased (match erased repr (static int -> int) { .pi -> true });;
let _ = assert erased (match erased repr (variant { none, some : int }) { .variant -> true });;
let _ = assert erased (match erased repr (&int) { .ref -> true });;
|};
  [%expect {| |}]
;;

(* The scalar arms entail [t := unit] etc., so each arm checks at [erased t -> t]. *)
let%expect_test "typecase unerase" =
  go
    {|
fun erased unerase (erased t : type) : erased (erased t -> t) =
  match erased repr t {
    .unit -> unerase_unit,
    .bool -> unerase_bool,
    .int -> unerase_int,
    _ -> unreachable,
  }
;;

let _ = assert erased (unerase int 3 == 3);;
let _ = assert erased (unerase bool true);;
let _ = unerase unit ();;
|};
  [%expect {| |}]
;;

let%expect_test "instance selecting unreachable is an error" =
  go
    {|
fun erased unerase (erased t : type) : erased (erased t -> t) =
  match erased repr t {
    .unit -> unerase_unit,
    .bool -> unerase_bool,
    .int -> unerase_int,
    _ -> unreachable,
  }
;;

let _ = unerase (int ^ int) (1, 2);;
|};
  [%expect {| ((loc ((line 7) (column 9))) (reason Unreachable_reached)) |}]
;;

(* Refinement reaches the expected type inside an arm: the lambda is written
   against [t], checked at [int -> int] under the [.int] fact. *)
let%expect_test "arm refines expected type" =
  go
    {|
fun erased zero (erased t : type) : erased (erased t -> t) =
  match erased repr t {
    .int -> fn (erased x : t) -> 0,
    _ -> unreachable,
  }
;;

let _ = assert erased (unerase_int (zero int 5) == 0);;
|};
  [%expect {| |}]
;;

(* Refinement reaches bindings made before the match: [unerase_int x] only
   typechecks because [x : t] refines to [int] inside the [.int] arm. *)
let%expect_test "arm refines earlier bindings" =
  go
    {|
fun erased succ (erased t : type) : erased (erased t -> int) =
  fn (erased x : t) ->
    match erased repr t {
      .int -> unerase_int x + 1,
      _ -> 0,
    }
;;

let _ = assert erased (unerase_int (succ int 41) == 42);;
let _ = assert erased (unerase_int (succ bool true) == 0);;
|};
  [%expect {| |}]
;;

let%expect_test "typecase is exhaustive over the nine heads" =
  go
    {|
fun erased rank (erased t : type) : erased int =
  match erased repr t {
    .unit -> 0,
    .bool -> 1,
    .int -> 2,
    .type -> 3,
    .tuple -> 4,
    .arrow -> 5,
    .pi -> 6,
    .variant -> 7,
    .ref -> 8,
  }
;;

let _ = assert erased (unerase_int (rank (int -> int)) == 5);;
let _ = assert erased (unerase_int (rank (static int -> int)) == 6);;
let _ = assert erased (unerase_int (rank (&bool)) == 8);;
|};
  [%expect {| |}]
;;

let%expect_test "typecase missing heads is non-exhaustive" =
  go
    {|
fun erased f (erased t : type) : erased int =
  match erased repr t { .int -> 0 }
;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 2)))
     (reason
      (Match
       (Non_exhaustive
        ((Constructor (label arrow) (payload ()))
         (Constructor (label bool) (payload ()))
         (Constructor (label pi) (payload ()))
         (Constructor (label ref) (payload ()))
         (Constructor (label tuple) (payload ()))
         (Constructor (label type) (payload ()))
         (Constructor (label unit) (payload ()))
         (Constructor (label variant) (payload ())))))))
    |}]
;;

let%expect_test "typecase arm after wildcard is redundant" =
  go
    {|
fun erased f (erased t : type) : erased int =
  match erased repr t {
    _ -> 0,
    .int -> 1,
  }
;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 2)))
     (reason
      (Match
       (Redundant
        ((Constructor (label int) (payload ()) (loc ((line 5) (column 4)))))))))
    |}]
;;
