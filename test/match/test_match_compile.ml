open! Core
open! Syl

let loc = Lex.Location.empty
let mode = Modes.bottom ()
let binding_desc ty = { Tst.Desc.ty; mode; static = Lazy.from_val Tst.Value.bottom }
let id name stamp = Ident.create (Ident.Raw.id name) ~stamp
let bool_pat value = Syl.Match.Pattern.Literal { value = Bool value; loc }
let int_pat value = Syl.Match.Pattern.Literal { value = Int (Int64.of_int value); loc }
let unit_pat = Syl.Match.Pattern.Literal { value = Unit; loc }
let var_pat name stamp = Syl.Match.Pattern.Var { id = id name stamp; loc }
let tuple_pat elts = Syl.Match.Pattern.Tuple { elts = Nonempty_list.of_list_exn elts; loc }
let or_pat left right = Syl.Match.Pattern.Or { left; right; loc }

let compile patterns ~scrutinee_ty =
  let patterns =
    Nonempty_list.of_list_exn (List.map patterns ~f:(fun (pattern, _bindings) -> pattern))
  in
  let { Match.Result.tree; redundant; missing } = Syl.Match.compile ~ty:scrutinee_ty patterns in
  print_s
    [%message
      (tree : Match.Tree.t)
        (redundant : Match.Pattern.t list)
        (missing : Match.Result.Missing.t list)]
;;

let%expect_test "var" =
  let x = id "x" 0 in
  let int_ty = Tst.Value.type_ Tst.Ty.Int in
  compile [ var_pat "x" 0, Ident.Map.singleton x (binding_desc int_ty) ] ~scrutinee_ty:int_ty;
  [%expect
    {|
    ((tree
      (Leaf (case 0)
       (bindings ((((Id x) <opaque>) ((path ()) (ty (Type Int))))))))
     (redundant ()) (missing ()))
    |}]
;;

let%expect_test "bool exhaustive" =
  compile
    [ bool_pat true, Ident.Map.empty; bool_pat false, Ident.Map.empty ]
    ~scrutinee_ty:(Tst.Value.type_ Tst.Ty.Bool);
  [%expect
    {|
    ((tree
      (Switch (occurrence ((path ()) (ty (Type Bool))))
       (cases
        (((Literal (Bool true)) (Leaf (case 0) (bindings ())))
         ((Literal (Bool false)) (Leaf (case 1) (bindings ())))))
       (default ())))
     (redundant ()) (missing ()))
    |}]
;;

let%expect_test "bool missing case" =
  compile [ bool_pat true, Ident.Map.empty ] ~scrutinee_ty:(Tst.Value.type_ Tst.Ty.Bool);
  [%expect
    {|
    ((tree
      (Switch (occurrence ((path ()) (ty (Type Bool))))
       (cases (((Literal (Bool true)) (Leaf (case 0) (bindings ())))))
       (default (Fail))))
     (redundant ()) (missing ((Literal (Bool false)))))
    |}]
;;

let%expect_test "or pattern exhaustive" =
  compile
    [ or_pat (bool_pat true) (bool_pat false), Ident.Map.empty ]
    ~scrutinee_ty:(Tst.Value.type_ Tst.Ty.Bool);
  [%expect
    {|
    ((tree
      (Switch (occurrence ((path ()) (ty (Type Bool))))
       (cases
        (((Literal (Bool true)) (Leaf (case 0) (bindings ())))
         ((Literal (Bool false)) (Leaf (case 0) (bindings ())))))
       (default ())))
     (redundant ()) (missing ()))
    |}]
;;

let%expect_test "redundant" =
  let x = id "x" 0 in
  let y = id "y" 1 in
  let int_ty = Tst.Value.type_ Tst.Ty.Int in
  compile
    [ var_pat "x" 0, Ident.Map.singleton x (binding_desc int_ty)
    ; var_pat "y" 1, Ident.Map.singleton y (binding_desc int_ty)
    ]
    ~scrutinee_ty:int_ty;
  [%expect
    {|
    ((tree
      (Leaf (case 0)
       (bindings ((((Id x) <opaque>) ((path ()) (ty (Type Int))))))))
     (redundant ((Var (id ((Id y) <opaque>)) (loc ((line 1) (column 0))))))
     (missing ()))
    |}]
;;

let%expect_test "tuple paths" =
  let x = id "x" 0 in
  let y = id "y" 0 in
  let bool_ty = Tst.Value.type_ Tst.Ty.Bool in
  compile
    [ tuple_pat [ bool_pat true; var_pat "x" 0 ], Ident.Map.singleton x (binding_desc bool_ty)
    ; tuple_pat [ bool_pat false; var_pat "y" 0 ], Ident.Map.singleton y (binding_desc bool_ty)
    ]
    ~scrutinee_ty:
      (Tst.Value.type_ (Tst.Ty.Tuple [ Tst.Value.type_ Tst.Ty.Bool; Tst.Value.type_ Tst.Ty.Bool ]));
  [%expect
    {|
    ((tree
      (Switch
       (occurrence ((path ()) (ty (Type (Tuple ((Type Bool) (Type Bool)))))))
       (cases
        (((Tuple 2)
          (Switch (occurrence ((path (0)) (ty (Type Bool))))
           (cases
            (((Literal (Bool true))
              (Leaf (case 0)
               (bindings ((((Id x) <opaque>) ((path (1)) (ty (Type Bool))))))))
             ((Literal (Bool false))
              (Leaf (case 1)
               (bindings ((((Id y) <opaque>) ((path (1)) (ty (Type Bool))))))))))
           (default ())))))
       (default ())))
     (redundant ()) (missing ()))
    |}]
;;

let%expect_test "nested or" =
  let x = id "x" 0 in
  let bool_ty = Tst.Value.type_ Tst.Ty.Bool in
  compile
    [ ( tuple_pat [ or_pat (bool_pat true) (bool_pat false); var_pat "x" 0 ]
      , Ident.Map.singleton x (binding_desc bool_ty) )
    ]
    ~scrutinee_ty:
      (Tst.Value.type_ (Tst.Ty.Tuple [ Tst.Value.type_ Tst.Ty.Bool; Tst.Value.type_ Tst.Ty.Bool ]));
  [%expect
    {|
    ((tree
      (Switch
       (occurrence ((path ()) (ty (Type (Tuple ((Type Bool) (Type Bool)))))))
       (cases
        (((Tuple 2)
          (Switch (occurrence ((path (0)) (ty (Type Bool))))
           (cases
            (((Literal (Bool true))
              (Leaf (case 0)
               (bindings ((((Id x) <opaque>) ((path (1)) (ty (Type Bool))))))))
             ((Literal (Bool false))
              (Leaf (case 0)
               (bindings ((((Id x) <opaque>) ((path (1)) (ty (Type Bool))))))))))
           (default ())))))
       (default ())))
     (redundant ()) (missing ()))
    |}]
;;

let%expect_test "bool only false - missing true" =
  compile [ bool_pat false, Ident.Map.empty ] ~scrutinee_ty:(Tst.Value.type_ Tst.Ty.Bool);
  [%expect
    {|
    ((tree
      (Switch (occurrence ((path ()) (ty (Type Bool))))
       (cases (((Literal (Bool false)) (Leaf (case 0) (bindings ())))))
       (default (Fail))))
     (redundant ()) (missing ((Literal (Bool true)))))
    |}]
;;

let%expect_test "int literal without wildcard - non-exhaustive" =
  compile [ int_pat 42, Ident.Map.empty ] ~scrutinee_ty:(Tst.Value.type_ Tst.Ty.Int);
  [%expect
    {|
    ((tree
      (Switch (occurrence ((path ()) (ty (Type Int))))
       (cases (((Literal (Int 42)) (Leaf (case 0) (bindings ())))))
       (default (Fail))))
     (redundant ()) (missing (Wildcard)))
    |}]
;;

let%expect_test "int literal with wildcard - wildcard not redundant" =
  let x = id "x" 0 in
  let int_ty = Tst.Value.type_ Tst.Ty.Int in
  compile
    [ int_pat 42, Ident.Map.empty; var_pat "x" 0, Ident.Map.singleton x (binding_desc int_ty) ]
    ~scrutinee_ty:int_ty;
  [%expect
    {|
    ((tree
      (Switch (occurrence ((path ()) (ty (Type Int))))
       (cases (((Literal (Int 42)) (Leaf (case 0) (bindings ())))))
       (default
        ((Leaf (case 1)
          (bindings ((((Id x) <opaque>) ((path ()) (ty (Type Int)))))))))))
     (redundant ()) (missing ()))
    |}]
;;

let%expect_test "two int literals without wildcard - non-exhaustive" =
  compile
    [ int_pat 0, Ident.Map.empty; int_pat 1, Ident.Map.empty ]
    ~scrutinee_ty:(Tst.Value.type_ Tst.Ty.Int);
  [%expect
    {|
    ((tree
      (Switch (occurrence ((path ()) (ty (Type Int))))
       (cases
        (((Literal (Int 0)) (Leaf (case 0) (bindings ())))
         ((Literal (Int 1)) (Leaf (case 1) (bindings ())))))
       (default (Fail))))
     (redundant ()) (missing (Wildcard)))
    |}]
;;

let%expect_test "int wildcard then literal - literal is redundant" =
  let x = id "x" 0 in
  let int_ty = Tst.Value.type_ Tst.Ty.Int in
  compile
    [ var_pat "x" 0, Ident.Map.singleton x (binding_desc int_ty); int_pat 42, Ident.Map.empty ]
    ~scrutinee_ty:int_ty;
  [%expect
    {|
    ((tree
      (Leaf (case 0)
       (bindings ((((Id x) <opaque>) ((path ()) (ty (Type Int))))))))
     (redundant ((Literal (value (Int 42)) (loc ((line 1) (column 0))))))
     (missing ()))
    |}]
;;

let%expect_test "unit is exhaustive" =
  compile [ unit_pat, Ident.Map.empty ] ~scrutinee_ty:(Tst.Value.type_ Tst.Ty.Unit);
  [%expect
    {|
    ((tree
      (Switch (occurrence ((path ()) (ty (Type Unit))))
       (cases (((Literal Unit) (Leaf (case 0) (bindings ()))))) (default ())))
     (redundant ()) (missing ()))
    |}]
;;

let%expect_test "bool wildcard is exhaustive" =
  let x = id "x" 0 in
  let bool_ty = Tst.Value.type_ Tst.Ty.Bool in
  compile [ var_pat "x" 0, Ident.Map.singleton x (binding_desc bool_ty) ] ~scrutinee_ty:bool_ty;
  [%expect
    {|
    ((tree
      (Leaf (case 0)
       (bindings ((((Id x) <opaque>) ((path ()) (ty (Type Bool))))))))
     (redundant ()) (missing ()))
    |}]
;;

let%expect_test "bool true then wildcard - exhaustive, wildcard not redundant" =
  let x = id "x" 0 in
  let bool_ty = Tst.Value.type_ Tst.Ty.Bool in
  compile
    [ bool_pat true, Ident.Map.empty; var_pat "x" 0, Ident.Map.singleton x (binding_desc bool_ty) ]
    ~scrutinee_ty:bool_ty;
  [%expect
    {|
    ((tree
      (Switch (occurrence ((path ()) (ty (Type Bool))))
       (cases (((Literal (Bool true)) (Leaf (case 0) (bindings ())))))
       (default
        ((Leaf (case 1)
          (bindings ((((Id x) <opaque>) ((path ()) (ty (Type Bool)))))))))))
     (redundant ()) (missing ()))
    |}]
;;

let%expect_test "bool wildcard then true - true is redundant" =
  let x = id "x" 0 in
  let bool_ty = Tst.Value.type_ Tst.Ty.Bool in
  compile
    [ var_pat "x" 0, Ident.Map.singleton x (binding_desc bool_ty); bool_pat true, Ident.Map.empty ]
    ~scrutinee_ty:bool_ty;
  [%expect
    {|
    ((tree
      (Leaf (case 0)
       (bindings ((((Id x) <opaque>) ((path ()) (ty (Type Bool))))))))
     (redundant ((Literal (value (Bool true)) (loc ((line 1) (column 0))))))
     (missing ()))
    |}]
;;

let%expect_test "tuple (bool, bool) one case - three missing" =
  let tuple_ty =
    Tst.Value.type_ (Tst.Ty.Tuple [ Tst.Value.type_ Tst.Ty.Bool; Tst.Value.type_ Tst.Ty.Bool ])
  in
  compile [ tuple_pat [ bool_pat true; bool_pat true ], Ident.Map.empty ] ~scrutinee_ty:tuple_ty;
  [%expect
    {|
    ((tree
      (Switch
       (occurrence ((path ()) (ty (Type (Tuple ((Type Bool) (Type Bool)))))))
       (cases
        (((Tuple 2)
          (Switch (occurrence ((path (0)) (ty (Type Bool))))
           (cases
            (((Literal (Bool true))
              (Switch (occurrence ((path (1)) (ty (Type Bool))))
               (cases (((Literal (Bool true)) (Leaf (case 0) (bindings ())))))
               (default (Fail))))))
           (default (Fail))))))
       (default ())))
     (redundant ())
     (missing
      ((Tuple ((Literal (Bool true)) (Literal (Bool false))))
       (Tuple
        ((Literal (Bool false))
         (Or ((Literal (Bool true)) (Literal (Bool false)))))))))
    |}]
;;

let%expect_test "tuple (bool, bool) full coverage" =
  let tuple_ty =
    Tst.Value.type_ (Tst.Ty.Tuple [ Tst.Value.type_ Tst.Ty.Bool; Tst.Value.type_ Tst.Ty.Bool ])
  in
  compile
    [ tuple_pat [ bool_pat true; bool_pat true ], Ident.Map.empty
    ; tuple_pat [ bool_pat true; bool_pat false ], Ident.Map.empty
    ; tuple_pat [ bool_pat false; bool_pat true ], Ident.Map.empty
    ; tuple_pat [ bool_pat false; bool_pat false ], Ident.Map.empty
    ]
    ~scrutinee_ty:tuple_ty;
  [%expect
    {|
    ((tree
      (Switch
       (occurrence ((path ()) (ty (Type (Tuple ((Type Bool) (Type Bool)))))))
       (cases
        (((Tuple 2)
          (Switch (occurrence ((path (0)) (ty (Type Bool))))
           (cases
            (((Literal (Bool true))
              (Switch (occurrence ((path (1)) (ty (Type Bool))))
               (cases
                (((Literal (Bool true)) (Leaf (case 0) (bindings ())))
                 ((Literal (Bool false)) (Leaf (case 1) (bindings ())))))
               (default ())))
             ((Literal (Bool false))
              (Switch (occurrence ((path (1)) (ty (Type Bool))))
               (cases
                (((Literal (Bool true)) (Leaf (case 2) (bindings ())))
                 ((Literal (Bool false)) (Leaf (case 3) (bindings ())))))
               (default ())))))
           (default ())))))
       (default ())))
     (redundant ()) (missing ()))
    |}]
;;

let%expect_test "tuple (bool, bool) wildcard second - exhaustive" =
  let x = id "x" 0 in
  let bool_ty = Tst.Value.type_ Tst.Ty.Bool in
  let tuple_ty =
    Tst.Value.type_ (Tst.Ty.Tuple [ Tst.Value.type_ Tst.Ty.Bool; Tst.Value.type_ Tst.Ty.Bool ])
  in
  compile
    [ tuple_pat [ bool_pat true; var_pat "x" 0 ], Ident.Map.singleton x (binding_desc bool_ty)
    ; tuple_pat [ bool_pat false; var_pat "x" 0 ], Ident.Map.singleton x (binding_desc bool_ty)
    ]
    ~scrutinee_ty:tuple_ty;
  [%expect
    {|
    ((tree
      (Switch
       (occurrence ((path ()) (ty (Type (Tuple ((Type Bool) (Type Bool)))))))
       (cases
        (((Tuple 2)
          (Switch (occurrence ((path (0)) (ty (Type Bool))))
           (cases
            (((Literal (Bool true))
              (Leaf (case 0)
               (bindings ((((Id x) <opaque>) ((path (1)) (ty (Type Bool))))))))
             ((Literal (Bool false))
              (Leaf (case 1)
               (bindings ((((Id x) <opaque>) ((path (1)) (ty (Type Bool))))))))))
           (default ())))))
       (default ())))
     (redundant ()) (missing ()))
    |}]
;;

let%expect_test "tuple (int, bool) - int column needs default" =
  let tuple_ty =
    Tst.Value.type_ (Tst.Ty.Tuple [ Tst.Value.type_ Tst.Ty.Int; Tst.Value.type_ Tst.Ty.Bool ])
  in
  compile
    [ tuple_pat [ int_pat 0; bool_pat true ], Ident.Map.empty
    ; tuple_pat [ int_pat 0; bool_pat false ], Ident.Map.empty
    ]
    ~scrutinee_ty:tuple_ty;
  [%expect
    {|
    ((tree
      (Switch
       (occurrence ((path ()) (ty (Type (Tuple ((Type Int) (Type Bool)))))))
       (cases
        (((Tuple 2)
          (Switch (occurrence ((path (0)) (ty (Type Int))))
           (cases
            (((Literal (Int 0))
              (Switch (occurrence ((path (1)) (ty (Type Bool))))
               (cases
                (((Literal (Bool true)) (Leaf (case 0) (bindings ())))
                 ((Literal (Bool false)) (Leaf (case 1) (bindings ())))))
               (default ())))))
           (default (Fail))))))
       (default ())))
     (redundant ())
     (missing
      ((Tuple (Wildcard (Or ((Literal (Bool true)) (Literal (Bool false)))))))))
    |}]
;;

let%expect_test "or pattern missing one case" =
  compile
    [ or_pat (bool_pat true) (bool_pat true), Ident.Map.empty ]
    ~scrutinee_ty:(Tst.Value.type_ Tst.Ty.Bool);
  [%expect
    {|
    ((tree
      (Switch (occurrence ((path ()) (ty (Type Bool))))
       (cases (((Literal (Bool true)) (Leaf (case 0) (bindings ())))))
       (default (Fail))))
     (redundant ((Literal (value (Bool true)) (loc ((line 1) (column 0))))))
     (missing ((Literal (Bool false)))))
    |}]
;;

let%expect_test "three bool cases - middle redundant" =
  compile
    [ bool_pat true, Ident.Map.empty
    ; bool_pat true, Ident.Map.empty
    ; bool_pat false, Ident.Map.empty
    ]
    ~scrutinee_ty:(Tst.Value.type_ Tst.Ty.Bool);
  [%expect
    {|
    ((tree
      (Switch (occurrence ((path ()) (ty (Type Bool))))
       (cases
        (((Literal (Bool true)) (Leaf (case 0) (bindings ())))
         ((Literal (Bool false)) (Leaf (case 2) (bindings ())))))
       (default ())))
     (redundant ((Literal (value (Bool true)) (loc ((line 1) (column 0))))))
     (missing ()))
    |}]
;;

let%expect_test "deeply nested tuple ((bool, bool), bool)" =
  let inner_ty =
    Tst.Value.type_ (Tst.Ty.Tuple [ Tst.Value.type_ Tst.Ty.Bool; Tst.Value.type_ Tst.Ty.Bool ])
  in
  let outer_ty = Tst.Value.type_ (Tst.Ty.Tuple [ inner_ty; Tst.Value.type_ Tst.Ty.Bool ]) in
  compile
    [ tuple_pat [ tuple_pat [ bool_pat true; bool_pat true ]; bool_pat true ], Ident.Map.empty ]
    ~scrutinee_ty:outer_ty;
  [%expect
    {|
    ((tree
      (Switch
       (occurrence
        ((path ())
         (ty
          (Type (Tuple ((Type (Tuple ((Type Bool) (Type Bool)))) (Type Bool)))))))
       (cases
        (((Tuple 2)
          (Switch
           (occurrence
            ((path (0)) (ty (Type (Tuple ((Type Bool) (Type Bool)))))))
           (cases
            (((Tuple 2)
              (Switch (occurrence ((path (0 0)) (ty (Type Bool))))
               (cases
                (((Literal (Bool true))
                  (Switch (occurrence ((path (0 1)) (ty (Type Bool))))
                   (cases
                    (((Literal (Bool true))
                      (Switch (occurrence ((path (1)) (ty (Type Bool))))
                       (cases
                        (((Literal (Bool true)) (Leaf (case 0) (bindings ())))))
                       (default (Fail))))))
                   (default (Fail))))))
               (default (Fail))))))
           (default ())))))
       (default ())))
     (redundant ())
     (missing
      ((Tuple
        ((Tuple ((Literal (Bool true)) (Literal (Bool true))))
         (Literal (Bool false))))
       (Tuple
        ((Tuple ((Literal (Bool true)) (Literal (Bool false))))
         (Or ((Literal (Bool true)) (Literal (Bool false))))))
       (Tuple
        ((Tuple
          ((Literal (Bool false))
           (Or ((Literal (Bool true)) (Literal (Bool false))))))
         (Or ((Literal (Bool true)) (Literal (Bool false)))))))))
    |}]
;;

let%expect_test "or pattern tuple cross product" =
  let tuple_ty =
    Tst.Value.type_ (Tst.Ty.Tuple [ Tst.Value.type_ Tst.Ty.Bool; Tst.Value.type_ Tst.Ty.Bool ])
  in
  compile
    [ ( tuple_pat
          [ or_pat (bool_pat true) (bool_pat false); or_pat (bool_pat true) (bool_pat false) ]
      , Ident.Map.empty )
    ]
    ~scrutinee_ty:tuple_ty;
  [%expect
    {|
    ((tree
      (Switch
       (occurrence ((path ()) (ty (Type (Tuple ((Type Bool) (Type Bool)))))))
       (cases
        (((Tuple 2)
          (Switch (occurrence ((path (0)) (ty (Type Bool))))
           (cases
            (((Literal (Bool true))
              (Switch (occurrence ((path (1)) (ty (Type Bool))))
               (cases
                (((Literal (Bool true)) (Leaf (case 0) (bindings ())))
                 ((Literal (Bool false)) (Leaf (case 0) (bindings ())))))
               (default ())))
             ((Literal (Bool false))
              (Switch (occurrence ((path (1)) (ty (Type Bool))))
               (cases
                (((Literal (Bool true)) (Leaf (case 0) (bindings ())))
                 ((Literal (Bool false)) (Leaf (case 0) (bindings ())))))
               (default ())))))
           (default ())))))
       (default ())))
     (redundant ()) (missing ()))
    |}]
;;

let%expect_test "multiple int literals with wildcard" =
  let x = id "x" 0 in
  let int_ty = Tst.Value.type_ Tst.Ty.Int in
  compile
    [ int_pat 0, Ident.Map.empty
    ; int_pat 1, Ident.Map.empty
    ; int_pat 2, Ident.Map.empty
    ; var_pat "x" 0, Ident.Map.singleton x (binding_desc int_ty)
    ]
    ~scrutinee_ty:int_ty;
  [%expect
    {|
    ((tree
      (Switch (occurrence ((path ()) (ty (Type Int))))
       (cases
        (((Literal (Int 0)) (Leaf (case 0) (bindings ())))
         ((Literal (Int 1)) (Leaf (case 1) (bindings ())))
         ((Literal (Int 2)) (Leaf (case 2) (bindings ())))))
       (default
        ((Leaf (case 3)
          (bindings ((((Id x) <opaque>) ((path ()) (ty (Type Int)))))))))))
     (redundant ()) (missing ()))
    |}]
;;

let%expect_test "int overlapping literals - second redundant" =
  compile
    [ int_pat 42, Ident.Map.empty; int_pat 42, Ident.Map.empty ]
    ~scrutinee_ty:(Tst.Value.type_ Tst.Ty.Int);
  [%expect
    {|
    ((tree
      (Switch (occurrence ((path ()) (ty (Type Int))))
       (cases (((Literal (Int 42)) (Leaf (case 0) (bindings ())))))
       (default (Fail))))
     (redundant ((Literal (value (Int 42)) (loc ((line 1) (column 0))))))
     (missing (Wildcard)))
    |}]
;;

let%expect_test "bool false then true - exhaustive" =
  compile
    [ bool_pat false, Ident.Map.empty; bool_pat true, Ident.Map.empty ]
    ~scrutinee_ty:(Tst.Value.type_ Tst.Ty.Bool);
  [%expect
    {|
    ((tree
      (Switch (occurrence ((path ()) (ty (Type Bool))))
       (cases
        (((Literal (Bool false)) (Leaf (case 0) (bindings ())))
         ((Literal (Bool true)) (Leaf (case 1) (bindings ())))))
       (default ())))
     (redundant ()) (missing ()))
    |}]
;;

let%expect_test "tuple (bool, bool) diagonal - two missing" =
  let tuple_ty =
    Tst.Value.type_ (Tst.Ty.Tuple [ Tst.Value.type_ Tst.Ty.Bool; Tst.Value.type_ Tst.Ty.Bool ])
  in
  compile
    [ tuple_pat [ bool_pat true; bool_pat false ], Ident.Map.empty
    ; tuple_pat [ bool_pat false; bool_pat true ], Ident.Map.empty
    ]
    ~scrutinee_ty:tuple_ty;
  [%expect
    {|
    ((tree
      (Switch
       (occurrence ((path ()) (ty (Type (Tuple ((Type Bool) (Type Bool)))))))
       (cases
        (((Tuple 2)
          (Switch (occurrence ((path (0)) (ty (Type Bool))))
           (cases
            (((Literal (Bool true))
              (Switch (occurrence ((path (1)) (ty (Type Bool))))
               (cases (((Literal (Bool false)) (Leaf (case 0) (bindings ())))))
               (default (Fail))))
             ((Literal (Bool false))
              (Switch (occurrence ((path (1)) (ty (Type Bool))))
               (cases (((Literal (Bool true)) (Leaf (case 1) (bindings ())))))
               (default (Fail))))))
           (default ())))))
       (default ())))
     (redundant ())
     (missing
      ((Tuple ((Literal (Bool true)) (Literal (Bool true))))
       (Tuple ((Literal (Bool false)) (Literal (Bool false)))))))
    |}]
;;

let%expect_test "tuple (bool, bool) three of four" =
  let tuple_ty =
    Tst.Value.type_ (Tst.Ty.Tuple [ Tst.Value.type_ Tst.Ty.Bool; Tst.Value.type_ Tst.Ty.Bool ])
  in
  compile
    [ tuple_pat [ bool_pat true; bool_pat true ], Ident.Map.empty
    ; tuple_pat [ bool_pat true; bool_pat false ], Ident.Map.empty
    ; tuple_pat [ bool_pat false; bool_pat true ], Ident.Map.empty
    ]
    ~scrutinee_ty:tuple_ty;
  [%expect
    {|
    ((tree
      (Switch
       (occurrence ((path ()) (ty (Type (Tuple ((Type Bool) (Type Bool)))))))
       (cases
        (((Tuple 2)
          (Switch (occurrence ((path (0)) (ty (Type Bool))))
           (cases
            (((Literal (Bool true))
              (Switch (occurrence ((path (1)) (ty (Type Bool))))
               (cases
                (((Literal (Bool true)) (Leaf (case 0) (bindings ())))
                 ((Literal (Bool false)) (Leaf (case 1) (bindings ())))))
               (default ())))
             ((Literal (Bool false))
              (Switch (occurrence ((path (1)) (ty (Type Bool))))
               (cases (((Literal (Bool true)) (Leaf (case 2) (bindings ())))))
               (default (Fail))))))
           (default ())))))
       (default ())))
     (redundant ())
     (missing ((Tuple ((Literal (Bool false)) (Literal (Bool false)))))))
    |}]
;;

let%expect_test "tuple with all wildcards - exhaustive" =
  let x = id "x" 0 in
  let tuple_ty =
    Tst.Value.type_ (Tst.Ty.Tuple [ Tst.Value.type_ Tst.Ty.Bool; Tst.Value.type_ Tst.Ty.Bool ])
  in
  compile
    [ ( tuple_pat [ var_pat "x" 0; var_pat "x" 0 ]
      , Ident.Map.singleton x (binding_desc (Tst.Value.type_ Tst.Ty.Bool)) )
    ]
    ~scrutinee_ty:tuple_ty;
  [%expect
    {|
    ((tree
      (Switch
       (occurrence ((path ()) (ty (Type (Tuple ((Type Bool) (Type Bool)))))))
       (cases
        (((Tuple 2)
          (Leaf (case 0)
           (bindings ((((Id x) <opaque>) ((path (0)) (ty (Type Bool))))))))))
       (default ())))
     (redundant ()) (missing ()))
    |}]
;;

let%expect_test "tuple wildcard then specific - specific redundant" =
  let x = id "x" 0 in
  let tuple_ty =
    Tst.Value.type_ (Tst.Ty.Tuple [ Tst.Value.type_ Tst.Ty.Bool; Tst.Value.type_ Tst.Ty.Bool ])
  in
  compile
    [ ( tuple_pat [ var_pat "x" 0; var_pat "x" 0 ]
      , Ident.Map.singleton x (binding_desc (Tst.Value.type_ Tst.Ty.Bool)) )
    ; tuple_pat [ bool_pat true; bool_pat false ], Ident.Map.empty
    ]
    ~scrutinee_ty:tuple_ty;
  [%expect
    {|
    ((tree
      (Switch
       (occurrence ((path ()) (ty (Type (Tuple ((Type Bool) (Type Bool)))))))
       (cases
        (((Tuple 2)
          (Leaf (case 0)
           (bindings ((((Id x) <opaque>) ((path (0)) (ty (Type Bool))))))))))
       (default ())))
     (redundant
      ((Tuple
        (elts
         ((Literal (value (Bool true)) (loc ((line 1) (column 0))))
          (Literal (value (Bool false)) (loc ((line 1) (column 0))))))
        (loc ((line 1) (column 0))))))
     (missing ()))
    |}]
;;

let%expect_test "or pattern both sides same" =
  compile
    [ or_pat (bool_pat false) (bool_pat false), Ident.Map.empty; bool_pat true, Ident.Map.empty ]
    ~scrutinee_ty:(Tst.Value.type_ Tst.Ty.Bool);
  [%expect
    {|
    ((tree
      (Switch (occurrence ((path ()) (ty (Type Bool))))
       (cases
        (((Literal (Bool false)) (Leaf (case 0) (bindings ())))
         ((Literal (Bool true)) (Leaf (case 1) (bindings ())))))
       (default ())))
     (redundant ((Literal (value (Bool false)) (loc ((line 1) (column 0))))))
     (missing ()))
    |}]
;;

let%expect_test "nested or left-deep" =
  compile
    [ or_pat (or_pat (bool_pat true) (bool_pat false)) (bool_pat true), Ident.Map.empty ]
    ~scrutinee_ty:(Tst.Value.type_ Tst.Ty.Bool);
  [%expect
    {|
    ((tree
      (Switch (occurrence ((path ()) (ty (Type Bool))))
       (cases
        (((Literal (Bool true)) (Leaf (case 0) (bindings ())))
         ((Literal (Bool false)) (Leaf (case 0) (bindings ())))))
       (default ())))
     (redundant ((Literal (value (Bool true)) (loc ((line 1) (column 0))))))
     (missing ()))
    |}]
;;

let%expect_test "tuple (int, int) both wildcards - exhaustive" =
  let int_ty = Tst.Value.type_ Tst.Ty.Int in
  let tuple_ty =
    Tst.Value.type_ (Tst.Ty.Tuple [ Tst.Value.type_ Tst.Ty.Int; Tst.Value.type_ Tst.Ty.Int ])
  in
  compile
    [ ( tuple_pat [ var_pat "x" 0; var_pat "y" 1 ]
      , Ident.Map.of_alist_exn [ id "x" 0, binding_desc int_ty; id "y" 1, binding_desc int_ty ] )
    ]
    ~scrutinee_ty:tuple_ty;
  [%expect
    {|
    ((tree
      (Switch
       (occurrence ((path ()) (ty (Type (Tuple ((Type Int) (Type Int)))))))
       (cases
        (((Tuple 2)
          (Leaf (case 0)
           (bindings
            ((((Id x) <opaque>) ((path (0)) (ty (Type Int))))
             (((Id y) <opaque>) ((path (1)) (ty (Type Int))))))))))
       (default ())))
     (redundant ()) (missing ()))
    |}]
;;

let%expect_test "tuple (int, bool) partial with wildcard fallback" =
  let x = id "x" 0 in
  let int_ty = Tst.Value.type_ Tst.Ty.Int in
  let tuple_ty =
    Tst.Value.type_ (Tst.Ty.Tuple [ Tst.Value.type_ Tst.Ty.Int; Tst.Value.type_ Tst.Ty.Bool ])
  in
  compile
    [ tuple_pat [ int_pat 0; bool_pat true ], Ident.Map.empty
    ; tuple_pat [ var_pat "x" 0; bool_pat false ], Ident.Map.singleton x (binding_desc int_ty)
    ; tuple_pat [ var_pat "x" 0; bool_pat true ], Ident.Map.singleton x (binding_desc int_ty)
    ]
    ~scrutinee_ty:tuple_ty;
  [%expect
    {|
    ((tree
      (Switch
       (occurrence ((path ()) (ty (Type (Tuple ((Type Int) (Type Bool)))))))
       (cases
        (((Tuple 2)
          (Switch (occurrence ((path (1)) (ty (Type Bool))))
           (cases
            (((Literal (Bool true))
              (Switch (occurrence ((path (0)) (ty (Type Int))))
               (cases (((Literal (Int 0)) (Leaf (case 0) (bindings ())))))
               (default
                ((Leaf (case 2)
                  (bindings ((((Id x) <opaque>) ((path (0)) (ty (Type Int)))))))))))
             ((Literal (Bool false))
              (Leaf (case 1)
               (bindings ((((Id x) <opaque>) ((path (0)) (ty (Type Int))))))))))
           (default ())))))
       (default ())))
     (redundant ()) (missing ()))
    |}]
;;

let%expect_test "all redundant after wildcard" =
  let x = id "x" 0 in
  let int_ty = Tst.Value.type_ Tst.Ty.Int in
  compile
    [ var_pat "x" 0, Ident.Map.singleton x (binding_desc int_ty)
    ; int_pat 0, Ident.Map.empty
    ; int_pat 1, Ident.Map.empty
    ; int_pat 2, Ident.Map.empty
    ]
    ~scrutinee_ty:int_ty;
  [%expect
    {|
    ((tree
      (Leaf (case 0)
       (bindings ((((Id x) <opaque>) ((path ()) (ty (Type Int))))))))
     (redundant
      ((Literal (value (Int 0)) (loc ((line 1) (column 0))))
       (Literal (value (Int 1)) (loc ((line 1) (column 0))))
       (Literal (value (Int 2)) (loc ((line 1) (column 0))))))
     (missing ()))
    |}]
;;

let%expect_test "bool triple redundancy" =
  compile
    [ bool_pat true, Ident.Map.empty
    ; bool_pat false, Ident.Map.empty
    ; bool_pat true, Ident.Map.empty
    ; bool_pat false, Ident.Map.empty
    ]
    ~scrutinee_ty:(Tst.Value.type_ Tst.Ty.Bool);
  [%expect
    {|
    ((tree
      (Switch (occurrence ((path ()) (ty (Type Bool))))
       (cases
        (((Literal (Bool true)) (Leaf (case 0) (bindings ())))
         ((Literal (Bool false)) (Leaf (case 1) (bindings ())))))
       (default ())))
     (redundant
      ((Literal (value (Bool true)) (loc ((line 1) (column 0))))
       (Literal (value (Bool false)) (loc ((line 1) (column 0))))))
     (missing ()))
    |}]
;;

let%expect_test "unit then unit - second redundant" =
  compile
    [ unit_pat, Ident.Map.empty; unit_pat, Ident.Map.empty ]
    ~scrutinee_ty:(Tst.Value.type_ Tst.Ty.Unit);
  [%expect
    {|
    ((tree
      (Switch (occurrence ((path ()) (ty (Type Unit))))
       (cases (((Literal Unit) (Leaf (case 0) (bindings ()))))) (default ())))
     (redundant ((Literal (value Unit) (loc ((line 1) (column 0))))))
     (missing ()))
    |}]
;;

let%expect_test "triple tuple (bool, bool, bool) one case" =
  let triple_ty =
    Tst.Value.type_
      (Tst.Ty.Tuple
         [ Tst.Value.type_ Tst.Ty.Bool; Tst.Value.type_ Tst.Ty.Bool; Tst.Value.type_ Tst.Ty.Bool ])
  in
  compile
    [ tuple_pat [ bool_pat true; bool_pat true; bool_pat true ], Ident.Map.empty ]
    ~scrutinee_ty:triple_ty;
  [%expect
    {|
    ((tree
      (Switch
       (occurrence
        ((path ()) (ty (Type (Tuple ((Type Bool) (Type Bool) (Type Bool)))))))
       (cases
        (((Tuple 3)
          (Switch (occurrence ((path (0)) (ty (Type Bool))))
           (cases
            (((Literal (Bool true))
              (Switch (occurrence ((path (1)) (ty (Type Bool))))
               (cases
                (((Literal (Bool true))
                  (Switch (occurrence ((path (2)) (ty (Type Bool))))
                   (cases
                    (((Literal (Bool true)) (Leaf (case 0) (bindings ())))))
                   (default (Fail))))))
               (default (Fail))))))
           (default (Fail))))))
       (default ())))
     (redundant ())
     (missing
      ((Tuple
        ((Literal (Bool true)) (Literal (Bool true)) (Literal (Bool false))))
       (Tuple
        ((Literal (Bool true)) (Literal (Bool false))
         (Or ((Literal (Bool true)) (Literal (Bool false))))))
       (Tuple
        ((Literal (Bool false))
         (Or ((Literal (Bool true)) (Literal (Bool false))))
         (Or ((Literal (Bool true)) (Literal (Bool false)))))))))
    |}]
;;

let%expect_test "tuple (bool, int) - column selection prefers bool" =
  let tuple_ty =
    Tst.Value.type_ (Tst.Ty.Tuple [ Tst.Value.type_ Tst.Ty.Bool; Tst.Value.type_ Tst.Ty.Int ])
  in
  compile
    [ tuple_pat [ bool_pat true; int_pat 0 ], Ident.Map.empty
    ; tuple_pat [ bool_pat false; int_pat 1 ], Ident.Map.empty
    ]
    ~scrutinee_ty:tuple_ty;
  [%expect
    {|
    ((tree
      (Switch
       (occurrence ((path ()) (ty (Type (Tuple ((Type Bool) (Type Int)))))))
       (cases
        (((Tuple 2)
          (Switch (occurrence ((path (0)) (ty (Type Bool))))
           (cases
            (((Literal (Bool true))
              (Switch (occurrence ((path (1)) (ty (Type Int))))
               (cases (((Literal (Int 0)) (Leaf (case 0) (bindings ())))))
               (default (Fail))))
             ((Literal (Bool false))
              (Switch (occurrence ((path (1)) (ty (Type Int))))
               (cases (((Literal (Int 1)) (Leaf (case 1) (bindings ())))))
               (default (Fail))))))
           (default ())))))
       (default ())))
     (redundant ())
     (missing
      ((Tuple ((Literal (Bool true)) Wildcard))
       (Tuple ((Literal (Bool false)) Wildcard)))))
    |}]
;;

let%expect_test "or pattern in tuple with wildcard fallback" =
  let x = id "x" 0 in
  let bool_ty = Tst.Value.type_ Tst.Ty.Bool in
  let tuple_ty =
    Tst.Value.type_ (Tst.Ty.Tuple [ Tst.Value.type_ Tst.Ty.Bool; Tst.Value.type_ Tst.Ty.Bool ])
  in
  compile
    [ tuple_pat [ or_pat (bool_pat true) (bool_pat false); bool_pat true ], Ident.Map.empty
    ; tuple_pat [ var_pat "x" 0; bool_pat false ], Ident.Map.singleton x (binding_desc bool_ty)
    ]
    ~scrutinee_ty:tuple_ty;
  [%expect
    {|
    ((tree
      (Switch
       (occurrence ((path ()) (ty (Type (Tuple ((Type Bool) (Type Bool)))))))
       (cases
        (((Tuple 2)
          (Switch (occurrence ((path (1)) (ty (Type Bool))))
           (cases
            (((Literal (Bool true))
              (Switch (occurrence ((path (0)) (ty (Type Bool))))
               (cases
                (((Literal (Bool true)) (Leaf (case 0) (bindings ())))
                 ((Literal (Bool false)) (Leaf (case 0) (bindings ())))))
               (default ())))
             ((Literal (Bool false))
              (Leaf (case 1)
               (bindings ((((Id x) <opaque>) ((path (0)) (ty (Type Bool))))))))))
           (default ())))))
       (default ())))
     (redundant ()) (missing ()))
    |}]
;;

let%expect_test "or-pattern makes subsequent case redundant" =
  compile
    [ or_pat (bool_pat true) (bool_pat false), Ident.Map.empty; bool_pat true, Ident.Map.empty ]
    ~scrutinee_ty:(Tst.Value.type_ Tst.Ty.Bool);
  [%expect
    {|
    ((tree
      (Switch (occurrence ((path ()) (ty (Type Bool))))
       (cases
        (((Literal (Bool true)) (Leaf (case 0) (bindings ())))
         ((Literal (Bool false)) (Leaf (case 0) (bindings ())))))
       (default ())))
     (redundant ((Literal (value (Bool true)) (loc ((line 1) (column 0))))))
     (missing ()))
    |}]
;;

let%expect_test "redundant tuple after full tuple coverage" =
  let tuple_ty =
    Tst.Value.type_ (Tst.Ty.Tuple [ Tst.Value.type_ Tst.Ty.Bool; Tst.Value.type_ Tst.Ty.Bool ])
  in
  compile
    [ tuple_pat [ bool_pat true; bool_pat true ], Ident.Map.empty
    ; tuple_pat [ bool_pat true; bool_pat false ], Ident.Map.empty
    ; tuple_pat [ bool_pat false; bool_pat true ], Ident.Map.empty
    ; tuple_pat [ bool_pat false; bool_pat false ], Ident.Map.empty
    ; tuple_pat [ bool_pat true; bool_pat true ], Ident.Map.empty
    ]
    ~scrutinee_ty:tuple_ty;
  [%expect
    {|
    ((tree
      (Switch
       (occurrence ((path ()) (ty (Type (Tuple ((Type Bool) (Type Bool)))))))
       (cases
        (((Tuple 2)
          (Switch (occurrence ((path (0)) (ty (Type Bool))))
           (cases
            (((Literal (Bool true))
              (Switch (occurrence ((path (1)) (ty (Type Bool))))
               (cases
                (((Literal (Bool true)) (Leaf (case 0) (bindings ())))
                 ((Literal (Bool false)) (Leaf (case 1) (bindings ())))))
               (default ())))
             ((Literal (Bool false))
              (Switch (occurrence ((path (1)) (ty (Type Bool))))
               (cases
                (((Literal (Bool true)) (Leaf (case 2) (bindings ())))
                 ((Literal (Bool false)) (Leaf (case 3) (bindings ())))))
               (default ())))))
           (default ())))))
       (default ())))
     (redundant
      ((Tuple
        (elts
         ((Literal (value (Bool true)) (loc ((line 1) (column 0))))
          (Literal (value (Bool true)) (loc ((line 1) (column 0))))))
        (loc ((line 1) (column 0))))))
     (missing ()))
    |}]
;;

let%expect_test "partial tuple redundancy - (true, _) then (true, false)" =
  let x = id "x" 0 in
  let bool_ty = Tst.Value.type_ Tst.Ty.Bool in
  let tuple_ty =
    Tst.Value.type_ (Tst.Ty.Tuple [ Tst.Value.type_ Tst.Ty.Bool; Tst.Value.type_ Tst.Ty.Bool ])
  in
  compile
    [ tuple_pat [ bool_pat true; var_pat "x" 0 ], Ident.Map.singleton x (binding_desc bool_ty)
    ; tuple_pat [ bool_pat true; bool_pat false ], Ident.Map.empty
    ; tuple_pat [ bool_pat false; bool_pat true ], Ident.Map.empty
    ; tuple_pat [ bool_pat false; bool_pat false ], Ident.Map.empty
    ]
    ~scrutinee_ty:tuple_ty;
  [%expect
    {|
    ((tree
      (Switch
       (occurrence ((path ()) (ty (Type (Tuple ((Type Bool) (Type Bool)))))))
       (cases
        (((Tuple 2)
          (Switch (occurrence ((path (0)) (ty (Type Bool))))
           (cases
            (((Literal (Bool true))
              (Leaf (case 0)
               (bindings ((((Id x) <opaque>) ((path (1)) (ty (Type Bool))))))))
             ((Literal (Bool false))
              (Switch (occurrence ((path (1)) (ty (Type Bool))))
               (cases
                (((Literal (Bool true)) (Leaf (case 2) (bindings ())))
                 ((Literal (Bool false)) (Leaf (case 3) (bindings ())))))
               (default ())))))
           (default ())))))
       (default ())))
     (redundant
      ((Tuple
        (elts
         ((Literal (value (Bool true)) (loc ((line 1) (column 0))))
          (Literal (value (Bool false)) (loc ((line 1) (column 0))))))
        (loc ((line 1) (column 0))))))
     (missing ()))
    |}]
;;

let%expect_test "or-pattern partial coverage - missing with or" =
  compile
    [ or_pat (bool_pat true) (bool_pat true), Ident.Map.empty; bool_pat true, Ident.Map.empty ]
    ~scrutinee_ty:(Tst.Value.type_ Tst.Ty.Bool);
  [%expect
    {|
    ((tree
      (Switch (occurrence ((path ()) (ty (Type Bool))))
       (cases (((Literal (Bool true)) (Leaf (case 0) (bindings ())))))
       (default (Fail))))
     (redundant
      ((Literal (value (Bool true)) (loc ((line 1) (column 0))))
       (Literal (value (Bool true)) (loc ((line 1) (column 0))))))
     (missing ((Literal (Bool false)))))
    |}]
;;

let%expect_test "mixed redundant and missing" =
  compile
    [ bool_pat true, Ident.Map.empty; bool_pat true, Ident.Map.empty ]
    ~scrutinee_ty:(Tst.Value.type_ Tst.Ty.Bool);
  [%expect
    {|
    ((tree
      (Switch (occurrence ((path ()) (ty (Type Bool))))
       (cases (((Literal (Bool true)) (Leaf (case 0) (bindings ())))))
       (default (Fail))))
     (redundant ((Literal (value (Bool true)) (loc ((line 1) (column 0))))))
     (missing ((Literal (Bool false)))))
    |}]
;;

let%expect_test "nested tuple with wildcard - missing inner cases" =
  let inner_ty =
    Tst.Value.type_ (Tst.Ty.Tuple [ Tst.Value.type_ Tst.Ty.Bool; Tst.Value.type_ Tst.Ty.Bool ])
  in
  let outer_ty = Tst.Value.type_ (Tst.Ty.Tuple [ inner_ty; Tst.Value.type_ Tst.Ty.Bool ]) in
  let x = id "x" 0 in
  compile
    [ ( tuple_pat [ tuple_pat [ bool_pat true; var_pat "x" 0 ]; bool_pat true ]
      , Ident.Map.singleton x (binding_desc (Tst.Value.type_ Tst.Ty.Bool)) )
    ]
    ~scrutinee_ty:outer_ty;
  [%expect
    {|
    ((tree
      (Switch
       (occurrence
        ((path ())
         (ty
          (Type (Tuple ((Type (Tuple ((Type Bool) (Type Bool)))) (Type Bool)))))))
       (cases
        (((Tuple 2)
          (Switch
           (occurrence
            ((path (0)) (ty (Type (Tuple ((Type Bool) (Type Bool)))))))
           (cases
            (((Tuple 2)
              (Switch (occurrence ((path (0 0)) (ty (Type Bool))))
               (cases
                (((Literal (Bool true))
                  (Switch (occurrence ((path (1)) (ty (Type Bool))))
                   (cases
                    (((Literal (Bool true))
                      (Leaf (case 0)
                       (bindings
                        ((((Id x) <opaque>) ((path (0 1)) (ty (Type Bool))))))))))
                   (default (Fail))))))
               (default (Fail))))))
           (default ())))))
       (default ())))
     (redundant ())
     (missing
      ((Tuple
        ((Tuple
          ((Literal (Bool true))
           (Or ((Literal (Bool true)) (Literal (Bool false))))))
         (Literal (Bool false))))
       (Tuple
        ((Tuple
          ((Literal (Bool false))
           (Or ((Literal (Bool true)) (Literal (Bool false))))))
         (Or ((Literal (Bool true)) (Literal (Bool false)))))))))
    |}]
;;

let%expect_test "tuple (int, bool) wildcard int exhaustive" =
  let x = id "x" 0 in
  let int_ty = Tst.Value.type_ Tst.Ty.Int in
  let tuple_ty =
    Tst.Value.type_ (Tst.Ty.Tuple [ Tst.Value.type_ Tst.Ty.Int; Tst.Value.type_ Tst.Ty.Bool ])
  in
  compile
    [ tuple_pat [ var_pat "x" 0; bool_pat true ], Ident.Map.singleton x (binding_desc int_ty)
    ; tuple_pat [ var_pat "x" 0; bool_pat false ], Ident.Map.singleton x (binding_desc int_ty)
    ]
    ~scrutinee_ty:tuple_ty;
  [%expect
    {|
    ((tree
      (Switch
       (occurrence ((path ()) (ty (Type (Tuple ((Type Int) (Type Bool)))))))
       (cases
        (((Tuple 2)
          (Switch (occurrence ((path (1)) (ty (Type Bool))))
           (cases
            (((Literal (Bool true))
              (Leaf (case 0)
               (bindings ((((Id x) <opaque>) ((path (0)) (ty (Type Int))))))))
             ((Literal (Bool false))
              (Leaf (case 1)
               (bindings ((((Id x) <opaque>) ((path (0)) (ty (Type Int))))))))))
           (default ())))))
       (default ())))
     (redundant ()) (missing ()))
    |}]
;;

let%expect_test "multiple or-patterns both redundant and missing" =
  compile
    [ or_pat (bool_pat true) (bool_pat true), Ident.Map.empty
    ; or_pat (bool_pat true) (bool_pat true), Ident.Map.empty
    ]
    ~scrutinee_ty:(Tst.Value.type_ Tst.Ty.Bool);
  [%expect
    {|
    ((tree
      (Switch (occurrence ((path ()) (ty (Type Bool))))
       (cases (((Literal (Bool true)) (Leaf (case 0) (bindings ())))))
       (default (Fail))))
     (redundant
      ((Literal (value (Bool true)) (loc ((line 1) (column 0))))
       (Literal (value (Bool true)) (loc ((line 1) (column 0))))
       (Literal (value (Bool true)) (loc ((line 1) (column 0))))))
     (missing ((Literal (Bool false)))))
    |}]
;;

let%expect_test "tuple (bool, bool) wildcard first, constructor second - partial" =
  let x = id "x" 0 in
  let bool_ty = Tst.Value.type_ Tst.Ty.Bool in
  let tuple_ty =
    Tst.Value.type_ (Tst.Ty.Tuple [ Tst.Value.type_ Tst.Ty.Bool; Tst.Value.type_ Tst.Ty.Bool ])
  in
  compile
    [ tuple_pat [ var_pat "x" 0; bool_pat true ], Ident.Map.singleton x (binding_desc bool_ty) ]
    ~scrutinee_ty:tuple_ty;
  [%expect
    {|
    ((tree
      (Switch
       (occurrence ((path ()) (ty (Type (Tuple ((Type Bool) (Type Bool)))))))
       (cases
        (((Tuple 2)
          (Switch (occurrence ((path (1)) (ty (Type Bool))))
           (cases
            (((Literal (Bool true))
              (Leaf (case 0)
               (bindings ((((Id x) <opaque>) ((path (0)) (ty (Type Bool))))))))))
           (default (Fail))))))
       (default ())))
     (redundant ())
     (missing
      ((Tuple
        ((Or ((Literal (Bool true)) (Literal (Bool false))))
         (Literal (Bool false)))))))
    |}]
;;

let%expect_test "int three literals then wildcard - wildcard needed" =
  let x = id "x" 0 in
  let int_ty = Tst.Value.type_ Tst.Ty.Int in
  compile
    [ int_pat 0, Ident.Map.empty
    ; int_pat 1, Ident.Map.empty
    ; int_pat 0, Ident.Map.empty
    ; var_pat "x" 0, Ident.Map.singleton x (binding_desc int_ty)
    ]
    ~scrutinee_ty:int_ty;
  [%expect
    {|
    ((tree
      (Switch (occurrence ((path ()) (ty (Type Int))))
       (cases
        (((Literal (Int 0)) (Leaf (case 0) (bindings ())))
         ((Literal (Int 1)) (Leaf (case 1) (bindings ())))))
       (default
        ((Leaf (case 3)
          (bindings ((((Id x) <opaque>) ((path ()) (ty (Type Int)))))))))))
     (redundant ((Literal (value (Int 0)) (loc ((line 1) (column 0))))))
     (missing ()))
    |}]
;;
