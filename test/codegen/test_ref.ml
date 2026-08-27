open! Core
open! Syl

let go ?print ?(check = `Run) input = Common.codegen ?print ~check input

(* Print the simplified user top-levels (the prelude is all [External]s), without locs. *)
let go_sst input =
  let rec strip_locs : Sexp.t -> Sexp.t = function
    | Atom _ as atom -> atom
    | List list ->
      List
        (List.filter_map list ~f:(function
           | Sexp.List (Atom "loc" :: _) -> None
           | sexp -> Some (strip_locs sexp)))
  in
  let sst =
    input |> Parse.parse_exn |> Desugar.desugar |> Typecheck.typecheck_exn |> Simplify.simplify
  in
  List.iter sst.top_levels ~f:(function
    | Sst.Top_level.External _ -> ()
    | top -> print_s (strip_locs (Sst.Top_level.sexp_of_t top)))
;;

(* A ref compiles to [syl_ref] (an opaque [char*]): [syl_ref_make] allocates
   and copies the payload in, [syl_deref] copies it back out. The pointee type
   never appears in the ref's own C++ type — which is what gives recursive
   variants a finite representation. *)

let%expect_test "the flagship: a recursive list, summed" =
  go
    {|
fun list (erased t : type) : erased type = variant { nil, cons : t ^ &(list t) };;
fun sum (dynamic l : list int) : dynamic int =
  match l { .nil -> 0, .cons (h, &t) -> h + sum t };;
let xs = (list int).cons (1, box ((list int).cons (2, box ((list int).nil))));;
let _ = print_int (sum xs);;
|};
  [%expect {| 3 |}]
;;

let%expect_test "one allocation can hold multiple recursive children" =
  go
    {|
fun pair (erased t : type) : erased type = t ^ t;;
fun tree (erased t : type) : erased type =
  variant { leaf : t, node : &(pair (tree t)) }
;;
fun sum (dynamic x : tree int) : dynamic int =
  match x { .leaf n -> n, .node &(left, right) -> sum left + sum right }
;;
let t = (tree int).node
  (box ((tree int).leaf 1, (tree int).node (box ((tree int).leaf 2, (tree int).leaf 3))));;
let _ = print_int (sum t);;
|};
  [%expect {| 6 |}]
;;

let%expect_test "ref round trip" =
  go
    {|
let r = box 5;;
let _ = print_int (match r { &x -> x });;
|};
  [%expect {| 5 |}]
;;

let%expect_test "double ref" =
  go
    {|
let r = box (box 7);;
let _ = print_int (match r { &(&x) -> x });;
|};
  [%expect {| 7 |}]
;;

let%expect_test "ref of a dynamic value" =
  go
    {|
let d = 11 @ dynamic;;
let r = box d;;
let _ = print_int (match r { &x -> x });;
|};
  [%expect {| 11 |}]
;;

let%expect_test "ref of unit is an empty allocation" =
  go
    {|
let r = box ();;
let _ = match r { &x -> print_unit x };;
|};
  [%expect {| () |}]
;;

let%expect_test "a static list flows to runtime" =
  (* The static constructor value carries its ref transparently; simplify
     rebuilds the indirection from the recorded unfolding. *)
  go
    {|
fun list (erased t : type) : erased type = variant { nil, cons : t ^ &(list t) };;
fun sum (dynamic l : list int) : dynamic int =
  match l { .nil -> 0, .cons (h, &t) -> h + sum t };;
let _ = print_int (sum ((list int).cons (10, box ((list int).nil))));;
|};
  [%expect {| 10 |}]
;;

let%expect_test "mutual recursion: a tree summed through its forest" =
  go
    {|
fun tree (erased t : type) : erased type = variant { leaf : t, node : &(forest t) }
and forest (erased t : type) : erased type = variant { nil, cons : &(tree t) ^ &(forest t) };;
fun tree_sum (dynamic x : tree int) : dynamic int =
  match x { .leaf v -> v, .node &f -> forest_sum f }
and forest_sum (dynamic f : forest int) : dynamic int =
  match f { .nil -> 0, .cons (&t, &rest) -> tree_sum t + forest_sum rest };;
let leaves = (forest int).cons
  (box ((tree int).leaf 1), box ((forest int).cons (box ((tree int).leaf 2), box ((forest int).nil))));;
let t = (tree int).node (box leaves);;
let _ = print_int (tree_sum t);;
|};
  [%expect {| 3 |}]
;;

let%expect_test "the ref intro/elim SST shape" =
  (* The match compiler rebinds the scrutinee, so the elim reads the ref back
     through a variable — [Ref_get (Var _)] over a let-bound [Make_ref] — and
     the direct [Ref_get (Make_ref e) -> e] fold cannot see through the
     binding, same as the [Payload_get]/[Tuple_get] folds. *)
  go_sst
    {|
let d = 3 @ dynamic;;
let _ = print_int (match box d { &x -> x });;
|};
  [%expect
    {|
    (Let (var ((Id d) <opaque>)) (bind (Scalar (value (Int 3)) (ty Int))))
    (Let (var (Anon <opaque>))
     (bind
      (Apply
       (fn
        (Var (id ((Id print_int) <opaque>))
         (ty (Arrow (arg_ty Int) (ret_ty Unit)))))
       (arg
        (Let (var (Anon <opaque>))
         (bind (Make_ref (payload (Scalar (value (Int 3)) (ty Int))) (ty Ref)))
         (rest
          (Match
           (cases
            (((body (Var (id ((Id x) <opaque>)) (ty Int)))
              (bindings ((((Id x) <opaque>) Int))))))
           (tree
            (Leaf (case 0)
             (bindings
              ((((Id x) <opaque>)
                (Ref_get (ref (Var (id (Anon <opaque>)) (ty Ref))) (ty Int)))))))
           (ty Int)))
         (ty Int)))
       (ty Unit))))
    |}]
;;

let%expect_test "polymorphic recursion instantiates finitely" =
  (* Each depth of a nested datatype is its own (name, key); a finite program
     demands finitely many. *)
  go
    {|
fun pair (erased t : type) : erased type = t ^ t;;
fun nest (erased t : type) : erased type = variant { one : t, deep : &(nest (pair t)) };;
let x = (nest int).deep (box ((nest (pair int)).one (8, 9)));;
let _ = match x { .one v -> print_int v, .deep &d -> match d { .one (a, b) -> print_int (a + b), .deep _ -> print_int 0 } };;
|};
  [%expect {| 17 |}]
;;

let%expect_test "static match through a ref at compile time, result at runtime" =
  go
    {|
let r = box 40;;
let _ = print_int (match static r { &x -> x + 2 });;
|};
  [%expect {| 42 |}]
;;

let%expect_test "erased refs vanish" =
  go
    {|
let erased t = &int;;
let erased x = box 5;;
let _ = print_int 6;;
|};
  [%expect {| 6 |}]
;;

let%expect_test "a ref in a closure environment" =
  go
    {|
fun list (erased t : type) : erased type = variant { nil, cons : t ^ &(list t) };;
fun sum (dynamic l : list int) : dynamic int =
  match l { .nil -> 0, .cons (h, &t) -> h + sum t };;
let r = box ((list int).cons (5, box ((list int).nil)));;
let f = fn (x : int) -> x + (match r { &l -> sum l });;
let _ = print_int (f 1);;
|};
  [%expect {| 6 |}]
;;

(* A deref reachable only under earlier tests must not run before them: tree
   conditions and leaf bindings carry their own binding blocks, emitted in
   place, so an unmatched arm's payload is never read through as a pointer. *)
let%expect_test "a split under a deref stays behind its guard" =
  go
    {|
fun shape (erased t : type) : erased type = variant { big : t ^ t ^ t, small : &t };;
let d = 1 @ dynamic;;
let s = (shape int).small (box d);;
let b = (shape int).big (10, 20, 30);;
fun get (dynamic x : shape int) : dynamic int =
  match x { .small (&1) -> 100, .small (&v) -> v, .big (a, b, c) -> a + b + c };;
let _ = print_int (get s);;
let _ = print_int (get b);;
|};
  [%expect
    {|
    100
    60
    |}]
;;

let%expect_test "nested deref bindings stay inside their arm" =
  go
    {|
fun shape (erased t : type) : erased type = variant { big : t ^ t ^ t, small : &(&t) };;
let d = 7 @ dynamic;;
let s = (shape int).small (box (box d));;
let b = (shape int).big (10, 20, 30);;
fun get (dynamic x : shape int) : dynamic int =
  match x { .small (&(&v)) -> v, .big (a, b, c) -> a + b + c };;
let _ = print_int (get s);;
let _ = print_int (get b);;
|};
  [%expect
    {|
    7
    60
    |}]
;;

let%expect_test "structurally equal recursive names interchange at runtime" =
  go
    {|
fun list (erased t : type) : erased type = variant { nil, cons : t ^ &(list t) };;
fun mylist (erased t : type) : erased type = variant { nil, cons : t ^ &(mylist t) };;
fun sum (dynamic l : mylist int) : dynamic int =
  match l { .nil -> 0, .cons (h, &t) -> h + sum t };;
let xs = (list int).cons (1, box ((list int).cons (2, box ((list int).nil))));;
let _ = print_int (sum (xs : mylist int));;
|};
  [%expect {| 3 |}]
;;

let%expect_test "a curried non-uniform tree round-trips" =
  go
    {|
fun perfect (erased t : type) : erased (static int -> erased type) =
  fn (static d : int) ->
    if erased (d == 0) then t else &(perfect (t ^ t) (d - 1))
;;
let quad = box (box (((1, 2), (3, 4)))) : perfect int 2;;
let _ = print_int (match quad { &(&(((a, b), (c, d)))) -> a + b + c + d });;
|};
  [%expect {| 10 |}]
;;

let%expect_test "guardedness spellings agree at runtime" =
  go
    {|
fun tree (erased t : type) : erased type =
  let self = tree t in
  variant { leaf : t, node : &(self ^ self) }
;;
fun sum (dynamic x : tree int) : dynamic int =
  match x { .leaf n -> n, .node &(a, b) -> sum a + sum b }
;;
let t = (tree int).node (box ((tree int).leaf 1, (tree int).node (box ((tree int).leaf 2, (tree int).leaf 3))));;
let _ = print_int (sum t);;
|};
  [%expect {| 6 |}]
;;

let%expect_test "a nested variant former under one ref round-trips" =
  go
    {|
fun tree (erased t : type) : erased type =
  variant { leaf : t, node : &(variant { one : tree t, two : tree t ^ tree t }) }
;;
fun sum (dynamic x : tree int) : dynamic int =
  match x {
    .leaf n -> n,
    .node (&inner) -> match inner { .one t -> sum t, .two (a, b) -> sum a + sum b },
  }
;;
let t = (tree int).node (box ((variant { one : tree int, two : tree int ^ tree int }).two ((tree int).leaf 1, (tree int).leaf 2)));;
let _ = print_int (sum t);;
|};
  [%expect {| 3 |}]
;;

let%expect_test "demanded recursive erased computation reaches runtime" =
  go
    {|
fun count (static n : int) : erased int = if erased (n == 0) then 0 else count (n - 1);;
fun use (static n : int) : dynamic unit =
  match erased (count n) { 0 -> print_int 7, _ -> print_int 8 };;
let _ = use 3;;
|};
  [%expect {| 7 |}]
;;

let%expect_test "quoting descends through folded content types" =
  go
    {|
fun pairs (erased _ : unit) : erased type = variant { num : int, pair : &(pairs () ^ pairs ()) };;
let p = (pairs ()).pair box ((pairs ()).num 1, (pairs ()).num 2);;
fun sum (s : pairs ()) : dynamic int = match s { .num n -> n, .pair &(a, b) -> sum a + sum b };;
let _ = print_int (sum p);;
|};
  [%expect {| 3 |}]
;;

(* Pi lowers to an opaque environment, so an erased parameter's type needs no
   finite layout even when the lambda itself is emitted. *)
let%expect_test "an erased parameter of infinite layout stays opaque" =
  go
    {|
fun bad (erased t : type) : erased type = variant { nil, cons : bad t };;
let f = fn (static erased x : bad int) -> 6;;
let _ = print_int (f ((bad int).nil));;
|};
  [%expect {| 6 |}]
;;
