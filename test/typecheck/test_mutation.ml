open! Core
open! Syl

let go = Common.typecheck

(* These cases pin small checker boundaries that survived targeted mutations. *)

let%expect_test "dynamic match result is tainted by its scrutinee" =
  go
    {|
let d = true @ dynamic;;
let x = match d { true -> 0, false -> 1 };;
let _ = x @ static;;
|};
  [%expect
    {|
    ((loc ((line 4) (column 10)))
     (reason
      (Mode_mismatch (got ((staticity Dynamic) (erasure Unerased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "mode annotations do not hide consumed unreachable" =
  go
    {|
let g = fn (x : int) -> 0;;
let _ = (g unreachable) @ static;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 11)))
     (reason (Misplaced_unreachable Not_in_tail_position)))
    |}]
;;

let%expect_test "variant payload types consume unreachable" =
  go
    {|
let _ = variant { bad : unreachable };;
|};
  [%expect
    {|
    ((loc ((line 2) (column 24)))
     (reason (Misplaced_unreachable Not_in_tail_position)))
    |}]
;;

let%expect_test "ref types consume unreachable" =
  go
    {|
let _ = &unreachable;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 9)))
     (reason (Misplaced_unreachable Not_in_tail_position)))
    |}]
;;

let%expect_test "box payloads consume unreachable" =
  go
    {|
let f = fn (static b : bool) ->
  if erased b then box unreachable else box 0;;
|};
  [%expect
    {|
    ((loc ((line 3) (column 23)))
     (reason (Misplaced_unreachable Not_in_tail_position)))
    |}]
;;

let%expect_test "ref type formers stay in the erased world" =
  go
    {|
let _ = (&int) @ unerased;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 15)))
     (reason
      (Mode_mismatch (got ((staticity Static) (erasure Erased)))
       (need ((staticity Static) (erasure Unerased))))))
    |}]
;;

let%expect_test "neutral tuple projections keep their indices distinct" =
  go
    {|
let bad = fn (static erased pair : type ^ type) ->
  match erased pair { (a, b) -> fn (x : a) -> (x : b) };;
|};
  [%expect
    {|
    ((loc ((line 3) (column 49)))
     (reason
      (Type_mismatch (got (Proj (tuple (Var (Anon <opaque>))) (index 0)))
       (need (Proj (tuple (Var (Anon <opaque>))) (index 1))))))
    |}]
;;

let%expect_test "externals require a function type" =
  go
    {|
external x : int = x;;
|};
  [%expect {| ((loc ((line 2) (column 0))) (reason (Static_external ((Id x) <opaque>) x))) |}]
;;

let%expect_test "ref patterns require a ref scrutinee" =
  go
    {|
let d = 1 @ dynamic;;
let _ = match d { &x -> x };;
|};
  [%expect {| ((loc ((line 3) (column 18))) (reason (Match (Expected_ref (Type Int))))) |}]
;;

let%expect_test "unknown builtin declarations are rejected" =
  go
    {|
builtin nope = syl_definitely_not_a_builtin;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 0)))
     (reason (Unknown_builtin ((Id nope) <opaque>) syl_definitely_not_a_builtin)))
    |}]
;;

let%expect_test "selection consumes its receiver" =
  go
    {|
let _ = unreachable.some;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 8)))
     (reason (Misplaced_unreachable Not_in_tail_position)))
    |}]
;;

let%expect_test "external types consume unreachable" =
  go
    {|
external f : unreachable = f;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 13)))
     (reason (Misplaced_unreachable Not_in_tail_position)))
    |}]
;;

let%expect_test "type annotation types consume unreachable" =
  go
    {|
let _ = 0 : unreachable;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 12)))
     (reason (Misplaced_unreachable Not_in_tail_position)))
    |}]
;;

let%expect_test "named function argument types consume unreachable" =
  go
    {|
fun f (x : unreachable) : int = 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 11)))
     (reason (Misplaced_unreachable Not_in_tail_position)))
    |}]
;;

let%expect_test "local named function argument types consume unreachable" =
  go
    {|
let _ = fun f (x : unreachable) : int = 0 in 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 19)))
     (reason (Misplaced_unreachable Not_in_tail_position)))
    |}]
;;

let%expect_test "local named function return types consume unreachable" =
  go
    {|
let _ = fun f (x : int) : unreachable = 0 in 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 26)))
     (reason (Misplaced_unreachable Not_in_tail_position)))
    |}]
;;

let%expect_test "local named function bodies reject bare unreachable" =
  go
    {|
let _ = fun f (x : int) : int = unreachable in 0;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 32)))
     (reason (Misplaced_unreachable Not_under_static_branch)))
    |}]
;;

let%expect_test "local named function continuations reject unreachable" =
  go
    {|
let _ = fun f (x : int) : int = 0 in unreachable;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 37)))
     (reason (Misplaced_unreachable Not_in_head_position)))
    |}]
;;

let%expect_test "arrow argument types consume unreachable" =
  go
    {|
let _ = unreachable -> int;;
|};
  [%expect
    {|
    ((loc ((line 2) (column 8)))
     (reason (Misplaced_unreachable Not_in_tail_position)))
    |}]
;;

let%expect_test "static Pi application demands an arrow closure" =
  go
    {|
fun boom (static x : int) : static int =
  if erased (x == 0) then 1 else unreachable
;;
fun user (static erased t : type) : (t -> dynamic t) =
  fn (y : t) ->
    let h = ((fn (x : int) -> boom 1) : static int -> int) in
    let _ = h 0 in y
;;
|};
  [%expect {| ((loc ((line 3) (column 33))) (reason Unreachable_reached)) |}]
;;

let%expect_test "static arrow application demands its closure" =
  go
    {|
fun boom (static x : int) : static int =
  if erased (x == 0) then 1 else unreachable
;;
fun user (static erased t : type) : (t -> dynamic t) =
  fn (y : t) ->
    let h = fn (x : int) -> let _ = boom 1 in 0 in
    let erased _ = assert erased (h 0 == 0) in y
;;
|};
  [%expect {| ((loc ((line 3) (column 33))) (reason Unreachable_reached)) |}]
;;

let%expect_test "specialization demands its captured environment" =
  go
    {|
fun boom (static x : int) : static int =
  if erased (x == 0) then 1 else unreachable
;;
fun user (static erased t : type) : (t -> dynamic t) =
  fn (y : t) ->
    let g = fn (static z : int) -> let _ = boom 1 in 0 in
    let h = fn (x : int) -> g 0 in
    let erased _ = assert erased (h 0 == 0) in y
;;
|};
  [%expect {| ((loc ((line 3) (column 33))) (reason Unreachable_reached)) |}]
;;

(* The old checker represents every erased occurrence explicitly.  This is a
   pass-boundary contract: mode weakening, erased variable lookup, and erased
   box construction must not leave runtime-shaped nodes behind. *)
let%test_unit "erased occurrences elaborate to erased nodes" =
  let program =
    {|
let x = 5 @ erased;;
let y = x;;
let z = box (5 @ erased);;
let r = repr int;;
|}
    |> Common.desugar
    |> Typecheck.typecheck_exn
  in
  let bindings =
    List.filter_map program.top_levels ~f:(function
      | Tst.Top_level.Let { bind; _ } -> Some bind
      | Erased _ | Fun _ | External _ | Builtin _ -> None)
  in
  match bindings with
  | [ Tst.Expr.Erased _
    ; Tst.Expr.Erased _
    ; Tst.Expr.Erased { ty = { node = Tst.Value.Type (Tst.Ty.Ref _); _ }; mode = box_mode; _ }
    ; Tst.Expr.Erased { ty = { node = Tst.Value.Type (Tst.Ty.Variant _); _ }; mode = repr_mode; _ }
    ]
    when Modes.is_erased box_mode && Modes.is_erased repr_mode -> ()
  | bindings -> raise_s [%message "runtime-shaped erased node" (bindings : Tst.Expr.t list)]
;;

let%test_unit "functions with erased results elaborate to erased nodes" =
  let assert_erased_function name expected source =
    let program = source |> Common.desugar |> Typecheck.typecheck_exn in
    match List.last_exn program.top_levels with
    | Tst.Top_level.Let { bind = Tst.Expr.Erased { ty; mode; _ }; _ }
      when Modes.is_erased mode
           &&
           match expected, ty.node with
           | `Arrow, Tst.Value.Type (Tst.Ty.Arrow _) -> true
           | `Pi, Tst.Value.Type (Tst.Ty.Pi _) -> true
           | (`Arrow | `Pi), _ -> false -> ()
    | Let { bind; _ } ->
      raise_s [%message "runtime-shaped erased function" (name : string) (bind : Tst.Expr.t)]
    | top_level ->
      raise_s [%message "expected a let binding" (name : string) (top_level : Tst.Top_level.t)]
  in
  assert_erased_function
    "ordinary lambda"
    `Arrow
    {|
let f = fn (x : int) -> 0 @ erased;;
|};
  assert_erased_function
    "dependent lambda"
    `Pi
    {|
let f = fn (static x : int) -> x @ erased;;
|}
;;

let%test_unit "erased named functions are omitted from the TST" =
  let assert_omitted name source =
    let program = source |> Common.desugar |> Typecheck.typecheck_exn in
    match List.last_exn program.top_levels with
    | Tst.Top_level.Erased _ -> ()
    | top_level ->
      raise_s [%message "erased function was emitted" (name : string) (top_level : Tst.Top_level.t)]
  in
  assert_omitted
    "ordinary function"
    {|
fun erased f (x : int) : erased int = 0;;
|};
  assert_omitted
    "dependent function"
    {|
fun erased f (static x : int) : erased int = x;;
|}
;;

let%test_unit "erased applications retain their result metadata" =
  let assert_erased_result name source =
    let program = source |> Common.desugar |> Typecheck.typecheck_exn in
    match List.last_exn program.top_levels with
    | Tst.Top_level.Let
        { bind = Tst.Expr.Erased { ty = { node = Tst.Value.Type Tst.Ty.Bool; _ }; mode; _ }; _ }
      when Modes.is_erased mode -> ()
    | Let { bind; _ } ->
      raise_s [%message "runtime-shaped erased application" (name : string) (bind : Tst.Expr.t)]
    | top_level ->
      raise_s [%message "expected a let binding" (name : string) (top_level : Tst.Top_level.t)]
  in
  assert_erased_result
    "dependent specialization"
    {|
let f = (fn (static x : int) -> true) @ erased;;
let y = f 1;;
|};
  assert_erased_result
    "ordinary application"
    {|
let f = (fn (x : int) -> true) @ erased;;
let y = f 1;;
|}
;;

let%test_unit "specializing an erased parameter does not rebind it" =
  let program =
    {|
let f = fn (static erased x : int) -> 0;;
let y = f 1;;
|}
    |> Common.desugar
    |> Typecheck.typecheck_exn
  in
  match
    List.find_map program.top_levels ~f:(function
      | Tst.Top_level.Let { bind = Tst.Expr.Binder { body; _ }; _ } -> Some body
      | Erased _ | Let _ | Fun _ | External _ | Builtin _ -> None)
  with
  | Some body ->
    (match Map.data body with
     | [ Tst.Expr.Literal _ ] -> ()
     | bodies -> raise_s [%message "erased parameter was materialized" (bodies : Tst.Expr.t list)])
  | None -> raise_s [%message "expected a specialized binder"]
;;

let%test_unit "erased specialization arguments retain their actual type" =
  let program =
    {|
let use = fn (static erased f : int -> dynamic int) -> 0;;
let y = use (fn (x : int) -> 0);;
|}
    |> Common.desugar
    |> Typecheck.typecheck_exn
  in
  match List.last_exn program.top_levels with
  | Tst.Top_level.Let
      { bind =
          Tst.Expr.Specialize
            { arg =
                Tst.Expr.Erased
                  { ty = { node = Tst.Value.Type (Tst.Ty.Arrow { ret_mode; _ }); _ }
                  ; loc = arg_loc
                  ; _
                  }
            ; loc = app_loc
            ; _
            }
      ; _
      }
    when Modes.is_static ret_mode && Lex.Location.equal arg_loc app_loc -> ()
  | top_level ->
    raise_s [%message "erased argument lost its actual type" (top_level : Tst.Top_level.t)]
;;
