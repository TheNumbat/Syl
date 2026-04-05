open! Core
open! Syl

let check_all = `No

let compile c =
  let tmp = Stdlib.Filename.temp_file "syl_test" ".cpp" in
  Out_channel.write_all tmp ~data:c;
  let cmd = Printf.sprintf "clang++ -fsyntax-only -w %s 2>/dev/null" tmp in
  (match Core_unix.system cmd with
   | Ok () -> ()
   | Error (`Exit_non_zero exit_code) -> raise_s [%message "Clang failed" (exit_code : int)]
   | Error (`Signal signal) -> raise_s [%message "Clang killed" (signal : Signal.t)]);
  Core_unix.unlink tmp
;;

let compile_and_run c =
  let tmp_c = Stdlib.Filename.temp_file "syl_test" ".cpp" in
  let tmp_exe = Stdlib.Filename.temp_file "syl_test" ".exe" in
  Out_channel.write_all tmp_c ~data:c;
  let cmd =
    Printf.sprintf
      "clang++ -fsanitize=address -fsanitize=alignment -o %s -w %s 2>/dev/null"
      tmp_exe
      tmp_c
  in
  (match Core_unix.system cmd with
   | Ok () -> ()
   | Error (`Exit_non_zero exit_code) -> raise_s [%message "Clang failed" (exit_code : int)]
   | Error (`Signal signal) -> raise_s [%message "Clang killed" (signal : Signal.t)]);
  Core_unix.unlink tmp_c;
  (match Core_unix.system ("ASAN_OPTIONS=detect_leaks=0 " ^ tmp_exe) with
   | Ok () -> ()
   | Error (`Exit_non_zero exit_code) -> raise_s [%message "Program failed" (exit_code : int)]
   | Error (`Signal signal) -> raise_s [%message "Program killed" (signal : Signal.t)]);
  Core_unix.unlink tmp_exe
;;

let strip_prelude c =
  let c_preamble_end = "//SYL_STD_END" in
  match String.substr_index c ~pattern:c_preamble_end with
  | None -> c
  | Some i ->
    let c = String.drop_prefix c (i + String.length c_preamble_end) in
    (match String.chop_prefix c ~prefix:"\n" with
     | Some c -> c
     | None -> c)
;;

let go ?(print = false) ?(check = check_all) input =
  let cst = Parse.parse_exn input in
  let dst = Desugar.desugar cst in
  let tst = Typecheck.typecheck_exn dst in
  let sst = Simplify.simplify tst in
  let lst = Linearize.linearize sst in
  let c = Codegen.c lst in
  if print then print_string (strip_prelude c);
  match check with
  | `Compile -> compile c
  | `Run -> compile_and_run c
  | _ -> ()
;;

let%expect_test "primitive" =
  go
    {|
builtin add = syl_int_add;;
let _ = add (1, 2);;
|};
  [%expect {| |}]
;;

let%expect_test "names" =
  go
    {|
let x = ();;
let x = ();;
|};
  [%expect {| |}]
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

let%expect_test "Unop var dynamic" =
  go
    {|
let dyn = true @ dynamic;;
let _ =
  !dyn
;;|};
  [%expect {| |}]
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

let%expect_test "Binop erased dynamic" =
  go
    {|
let _ =
  1 + (2 @ dynamic)
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

let%expect_test "dynamic closure" =
  go
    {|
let y = 1 @ dynamic;;
let _ =
  (fn (x : int) -> x + y)
;;|};
  [%expect {| |}]
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
let f = (fn (erased x : int) -> 1) @ erased;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = (fn (erased x : int) -> 1) ;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = (fn (erased x : int) -> 1) ;;
let _ = f (0 @ erased);;
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
let _ = (fn (erased x : int) -> 1) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let _ = (fn (erased x : int) -> 1) (0 @ erased);;
|};
  [%expect {| |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let _ = (fn (static x : int) -> 1) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let _ = (fn (static erased x : int) -> 1) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let _ = (fn (static erased x : int) -> 1) (0 @ erased);;
|};
  [%expect {| |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (static erased g : int -> erased int) -> let _ = g 1 in 2;;
let _ = f (fn (x : int) -> 0 @ erased);;
|};
  [%expect {| |}]
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
let f = (fn (erased x : int -> int) -> 1) @ erased;;
let _ = f ((fn (x : int) -> x + 1) @ erased);;
|};
  [%expect {| |}]
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

let%expect_test "dependent closure arg" =
  go
    {|
fun f (x : type) : type = x;;|};
  [%expect {| |}]
;;

let%expect_test "dependent closure arg" =
  go
    {|
fun f (static x : int) : static erased type = int;;|};
  [%expect {| |}]
;;

let%expect_test "dependent closure arg" =
  go
    {|
let _ =
  (fn (static x : type) -> x)
;;|};
  [%expect {| |}]
;;

let%expect_test "dependent closure arg" =
  go
    {|
let _ =
  (fn (erased x : type) -> x)
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
let x = ((fn (erased x : int) -> 1) @ erased) 0;;
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

let%expect_test "static erased arg" =
  go
    {|
let f = (fn (static erased x : int) -> 1);;
let _ = f 0;;
let _ = f 1;;
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
let f = fn (erased x : int) -> 1;;
let g = fn (static f : erased int -> int) -> (f @ erased) 0;;
let _ = g f;;
|};
  [%expect {| |}]
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
let g = fn (static f2 : erased int -> int) -> f2 0;;
let _ = g f1;;
|};
  [%expect {| |}]
;;

let%expect_test "inlined closure nest" =
  go
    {|
let f1 = (fn (erased x : int) -> 1) @ erased;;
let g = fn (static erased f2 : erased int -> int) -> f2 0;;
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

let%expect_test "closure static erased" =
  go
    {|
let x = (fn (static erased x : int) -> 1) 0;;
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
  (fn (erased x : int) -> 5) (dyn-1)
;;
let _ = y @ unerased;;|};
  [%expect {| |}]
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

let%expect_test "Apply erased fn stat arg" =
  go
    {|
let dyn = 1;;
let y =
  (fn (static x : int) -> 5) (dyn-1)
;;
let _ = y @ unerased;;|};
  [%expect {| |}]
;;

let%expect_test "Apply erased fn stat arg" =
  go
    {|
let dyn = 1;;
let y =
  (fn (static erased x : int) -> 5) (dyn-1)
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
let x = fn (static erased x : type) -> x;;
let y = (x int) @ dynamic;;
|};
  [%expect {| |}]
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
let f = fn (static x : unit) -> let x = 0 in ();;
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
let f = fn (static erased t : type) -> fn (x : t) -> if true then x else x;;
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

let%expect_test "return erased" =
  go
    {|
let f = fn (x : int) -> 0 @ erased;;
let _ = f 1;;
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
let f = fn (static erased g : static int -> int) -> g 0;;
let _ = f (fn (static x : int) -> x + 1);;
|};
  [%expect {| |}]
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

let%expect_test "dependent if unify" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else true;;
let g = fn (static x : int) -> if static x == 0 then true else 2;;
let _ = if true then f 0 else g 1;;
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

let%expect_test "Fun erased arg" =
  go
    {|
fun f (erased x : int) : erased int = x;;
|};
  [%expect {| |}]
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
let y = x 0;;
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

let%expect_test "dependent if unify" =
  go
    {|
let f = fn (static erased x : int) -> if static x == 0 then 1 else true;;
let g = fn (static erased x : int) -> if static x == 0 then 0 else false;;
let h = if true then f else g;;
let _ = h 0;;
let _ = h 1;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static erased x : int) -> (if static x == 0 then 1 else true) : (if x == 0 then int else bool);;
let _ = f 0;;
let _ = f 1;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static erased x : int) -> (if static x == 1 + 1 then 1 else true) : (if x == 2 then int else bool);;
let _ = f 1;;
let _ = f 2;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static erased x : int) -> (if static x == (if true then x else 0) then 1 else true) : (if x == x then int else bool);;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static erased x : int) -> (if static x == x + 1 then 1 else true) : (if x == x + 1 then int else bool);;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static erased x : int) -> (if static x == (if x == 1 then x else 0) then 1 else true) : (if x == (if x == 1 then x else 0) then int else bool);;
let _ = f 0;;
let _ = f 1;;
let _ = f 2;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent abstraction" =
  go
    {|
let choose = fn (static f : static int \ x -> if x == 0 then int else bool) -> f 0;;
let _ = choose (fn (static x : int) -> if static x == 0 then 0 else true);;
|};
  [%expect {| |}]
;;

let%expect_test "weaken mode: static unerased -> static erased (literal substitution)" =
  go
    {|
let _ = 1 @ erased;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken mode: dynamic unerased -> dynamic erased (erased marker)" =
  go
    {|
let x = 1 @ dynamic;;
let _ = x @ erased;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken mode: static -> dynamic (staticity only)" =
  go
    {|
let _ = (fn (x : int) -> x) @ dynamic;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken type: arrow ret_mode covariant" =
  go
    {|
let f = fn (x : int) -> x;;
let _ = f : int -> erased int;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken type: arrow arg_mode contravariant" =
  go
    {|
let f = fn (erased x : int) -> 1;;
let _ = f : int -> int;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken if non-split: mode erasure on branch" =
  go
    {|
let _ = if true then 1 else 1 @ erased;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken if non-split: arrow type join" =
  go
    {|
let _ = if true then fn (erased x : int) -> 1 else fn (x : int) -> 1;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken if split: mode only" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else 1 @ erased;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken binder apply: body weakened to ret_mode" =
  go
    {|
let f = fn (static x : int) -> x;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken arrow closure apply erased: body weakened" =
  go
    {|
let f = fn (x : int) -> x;;
let _ = (f @ erased) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken pi closure apply erased: both axes" =
  go
    {|
let f = fn (x : int) -> 1;;
let g = fn (static erased x : int) -> x;;
let _ = ((if true then f else g) @ erased) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken mode: both axes (static unerased -> dynamic erased)" =
  go
    {|
let _ = 1 @ dynamic erased;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken if non-split: staticity on branch" =
  go
    {|
let x = 1 @ dynamic;;
let _ = if true then 1 else x;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken if non-split: both axes on branch" =
  go
    {|
let x = 1 @ dynamic;;
let _ = if true then 1 else x @ erased;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken if split: staticity on branch" =
  go
    {|
let f = fn (static b : bool) -> if static b then 1 @ dynamic else 1;;
let _ = f false;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken if split: both axes on branch" =
  go
    {|
let f = fn (static b : bool) -> if static b then 1 @ dynamic erased else 1;;
let _ = f false;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken binder apply: erasure on body" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else 1 @ erased;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken arrow closure apply erased: staticity on body" =
  go
    {|
let f = fn (x : int) -> 1;;
let _ = (f @ erased) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken pi closure apply erased: erasure only" =
  go
    {|
let f = fn (x : int) -> x;;
let g = fn (static erased x : int) -> x;;
let _ = ((if true then f else g) @ erased) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "weaken pi closure apply erased: staticity only" =
  go
    {|
let f = fn (x : int) -> 1;;
let g = fn (static x : int) -> x;;
let _ = ((if true then f else g) @ erased) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "closure to closure: arg erasure contravariant" =
  go
    {|
let apply = fn (f : int -> int) -> f 0;;
let g = fn (erased x : int) -> 1;;
let _ = apply g;;
|};
  [%expect {| |}]
;;

let%expect_test "closure to closure: ret erasure covariant" =
  go
    {|
let apply = fn (f : int -> erased int) -> f 0;;
let g = fn (x : int) -> x;;
let _ = apply g;;
|};
  [%expect {| |}]
;;

let%expect_test "closure to closure: both arg and ret subtyping" =
  go
    {|
let apply = fn (f : int -> erased int) -> f 0;;
let g = fn (erased x : int) -> 1;;
let _ = apply g;;
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

let%expect_test "binder to binder: arg erasure contravariant" =
  go
    {|
let apply = fn (static f : static int -> int) -> f 0;;
let g = fn (static erased x : int) -> 1;;
let _ = apply g;;
|};
  [%expect {| |}]
;;

let%expect_test "erased closure taking closure arg" =
  go
    {|
let apply = fn (f : int -> int) -> f 0;;
let _ = (apply @ erased) (fn (x : int) -> x);;
|};
  [%expect {| |}]
;;

let%expect_test "binder taking closure, applied erased inside" =
  go
    {|
let apply = fn (static f : static int -> erased int) -> (f @ erased) 0;;
let g = fn (x : int) -> 1;;
let _ = apply g;;
|};
  [%expect {| |}]
;;

let%expect_test "static lambda identity returns dependent type" =
  go
    {|
fun f (static x : int) : int = x;;
let _ = f 42;;
let _ = f 69;;
|};
  [%expect {| |}]
;;

let%expect_test "static lambda identity returns dependent type" =
  go
    {|
let f = fn (static x : int) -> x;;
let _ = f 42;;
let _ = f 69;;
|};
  [%expect {| |}]
;;

let%expect_test "static lambda with arithmetic on static arg" =
  go
    {|
let f = fn (static x : int) -> x + 1;;
let _ = f 10;;
|};
  [%expect {| |}]
;;

let%expect_test "static lambda with capture" =
  go
    {|
let y = 1;;
let f = fn (static x : int) -> x + y;;
let _ = f 10;;
|};
  [%expect {| |}]
;;

let%expect_test "static lambda with capture" =
  go
    {|
let _ =
let y = 1 in
let f = fn (static x : int) -> x + y in
f 10;;
|};
  [%expect {| |}]
;;

let%expect_test "static lambda with boolean op on static arg" =
  go
    {|
let f = fn (static x : bool) -> x && true;;
let _ = f false;;
|};
  [%expect {| |}]
;;

let%expect_test "nested static lambdas" =
  go
    {|
let f = fn (static x : int) -> fn (static y : int) -> x + y;;
let _ = f 1 2;;
let _ = f 1 3;;
|};
  [%expect {| |}]
;;

let%expect_test "static lambda returning static lambda" =
  go
    {|
let f = fn (static x : int) -> fn (static y : int) -> x;;
let g = f 1;;
let _ = g 2;;
|};
  [%expect {| |}]
;;

let%expect_test "static lambda mixed with dynamic lambda" =
  go
    {|
let f = fn (static x : int) -> fn (y : int) -> y;;
let g = f 1;;
let _ = g 2;;
|};
  [%expect {| |}]
;;

let%expect_test "dynamic lambda inside static lambda uses static arg as type" =
  go
    {|
let f = fn (static erased t : type) -> fn (x : t) -> x;;
let g = f int;;
let _ = g 42;;
let h = f bool;;
let _ = h true;;
|};
  [%expect {| |}]
;;

let%expect_test "if static with literal condition true" =
  go
    {|
let _ = if static true then 1 else true;;
|};
  [%expect {| |}]
;;

let%expect_test "if static with literal condition false" =
  go
    {|
let _ = if static false then 1 else true;;
|};
  [%expect {| |}]
;;

let%expect_test "if static with static variable condition" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else true;;
let a = f 0;;
let b = f 1;;
|};
  [%expect {| |}]
;;

let%expect_test "if static with mismatched branch types without annotation" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else true;;
|};
  [%expect {| |}]
;;

let%expect_test "if static with correct type annotation using non-static if" =
  go
    {|
let f = fn (static x : int) -> (if static x == 0 then 1 else true) : (if x == 0 then int else bool);;
|};
  [%expect {| |}]
;;

let%expect_test "if static with nested dependent types in branches" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then fn (y : int) -> y else fn (y : bool) -> y;;
let g = f 0;;
let _ = g 42;;
let h = f 1;;
let _ = h true;;
|};
  [%expect {| |}]
;;

let%expect_test "if static true selects then branch type" =
  go
    {|
let _ = (if static true then 1 else true) : (if true then int else bool);;
|};
  [%expect {| |}]
;;

let%expect_test "if static false selects else branch type" =
  go
    {|
let _ = (if static false then 1 else true) : (if false then int else bool);;
|};
  [%expect {| |}]
;;

let%expect_test "dependent arrow type with backslash binder" =
  go
    {|
let f = fn (static g : static int \ x -> int) -> g 0;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent arrow applied to matching function" =
  go
    {|
let apply_type = fn (static f : static erased type \ t -> t -> t) -> f int;;
let _ = apply_type (fn (static erased t : type) -> fn (x : t) -> x);;
|};
  [%expect {| |}]
;;

let%expect_test "dependent arrow with return type depending on arg" =
  go
    {|
let mk_int = fn (static x : int) -> int;;
let f = fn (static g : static int \ x -> mk_int x) -> g 0;;
|};
  [%expect {| |}]
;;

let%expect_test "fun with static arg creates Pi type" =
  go
    {|
fun f (static x : int) : int = x;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "fun with static arg creates Pi type" =
  go
    {|
fun f (static erased x : int) : int = x+0;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "fun with static arg creates Pi type" =
  go
    {|
let _ =
  fun f (static x : int) : int = x in
  let _ = f 0
in ();;
|};
  [%expect {| |}]
;;

let%expect_test "fun with static erased type arg — polymorphic identity" =
  go
    {|
fun id (static erased t : type) : t -> t = fn (x : t) -> x;;
let _ = id int 0;;
let _ = id bool true;;
|};
  [%expect {| |}]
;;

let%expect_test "fun dynamic recursion is allowed" =
  go
    {|
fun f (x : int) : int = f x;;
|};
  [%expect {| |}]
;;

let%expect_test "fun with static erased type arg, two sequential funs" =
  go
    {|
fun id1 (static erased t : type) : t -> t = fn (x : t) -> x;;
fun id2 (static erased t : type) : t -> t = id1 t;;
let _ = id2 int 0;;
|};
  [%expect {| |}]
;;

let%expect_test "static erased lambda captures no runtime value" =
  go
    {|
let f = fn (static erased x : int) -> 0;;
let _ = f 1;;
|};
  [%expect {| |}]
;;

let%expect_test "lift static value through Pi" =
  go
    {|
let f = fn (static x : int) -> x @ erased;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "fun returning static erased type" =
  go
    {|
fun f (static _ : unit) : static erased type = int;;
let _ = 5 : f ();;
|};
  [%expect {| |}]
;;

let%expect_test "pi and arrow join — if choosing between Pi and Arrow" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else true;;
let g = fn (static x : int) -> if static x == 0 then 1 else true;;
let _ = if true then f else g;;
|};
  [%expect {| |}]
;;

let%expect_test "joining f 0 and g 1 resolves dependent types" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else true;;
let g = fn (static x : int) -> if static x == 0 then true else 2;;
let _ = if true then f 0 else g 1;;
|};
  [%expect {| |}]
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

let%expect_test "nested if static with different types per level" =
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
    if static x == 0
    then if static y == 0 then 1 else true
    else if static y == 0 then () else 2
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

let%expect_test "nested if static with different types per level" =
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
    if static x == 0
    then if static y == 0 then x+y else true
    else if static y == 0 then () else x-y
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

let%expect_test "static lambda unused arg" =
  go
    {|
let f = fn (static _ : int) -> 42;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "static lambda effects" =
  go
    {|
external print_int : int -> unit = syl_std_print_int;;
let print = fn (static x : int) -> print_int x;;
let _ = print 0;;
let _ = print 0;;
let _ = print 1;;
|};
  [%expect {| |}]
;;

let%expect_test "static lambda effects" =
  go
    {|
external print_int : int -> unit = syl_std_print_int;;
let _ =
let print = fn (static x : int) -> print_int x in
let _ = print 0 in
let _ = print 0 in
let _ = print 1 in
();;
|};
  [%expect {| |}]
;;

let%expect_test "static lambda effects" =
  go
    {|
external print_int : int -> unit = syl_std_print_int;;
let print = fn (static x : int) ->
  let _ = print_int x in
  fn (static y : int) -> print_int y;;
let _ = print 0 1;;
let _ = print 1 2;;
let _ = print 1 3;;
|};
  [%expect {| |}]
;;

let%expect_test "dependent type: apply polymorphic id to itself" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = (id (int -> int)) (fn (x : int) -> x + 1) 5;;
|};
  [%expect {| |}]
;;

let%expect_test "if static with bool static arg" =
  go
    {|
let f = fn (static b : bool) -> if static b then 1 else true;;
let _ = f true;;
let _ = f false;;
|};
  [%expect {| |}]
;;

let%expect_test "static arg used in arithmetic, result applied" =
  go
    {|
let double = fn (static x : int) -> x + x;;
let _ = double 5;;
|};
  [%expect {| |}]
;;

let%expect_test "chaining dependent applications" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let f = id int;;
let g = id bool;;
let _ = f 0;;
let _ = g true;;
|};
  [%expect {| |}]
;;

let%expect_test "symbolic arrow type as static arg" =
  go
    {|
let choose = fn (static f : static int \ x -> if x == 0 then int else bool) -> f 0;;
let _ = choose (fn (static x : int) -> if static x == 0 then 0 else true);;
|};
  [%expect {| |}]
;;

let%expect_test "static lambda body references outer let binding" =
  go
    {|
let n = 10;;
let f = fn (static x : int) -> x + n;;
let _ = f 5;;
|};
  [%expect {| |}]
;;

let%expect_test "static lambda with type annotation on body" =
  go
    {|
let f = fn (static x : int) -> (x : int);;
let _ = f 42;;
|};
  [%expect {| |}]
;;

let%expect_test "if static in type annotation position" =
  go
    {|
let f = fn (static b : bool) -> (if static b then 0 else true) : (if b then int else bool);;
let _ = f true;;
let _ = f false;;
|};
  [%expect {| |}]
;;

let%expect_test "higher-order static: take a static function and apply it" =
  go
    {|
let apply = fn (static f : static int -> int) -> f 5;;
let _ = apply (fn (static x : int) -> x + 1);;
|};
  [%expect {| |}]
;;

let%expect_test "multiple static erased type args" =
  go
    {|
let f = fn (static erased t1 : type) -> fn (static erased t2 : type) -> fn (x : t1) -> fn (y : t2) -> x;;
let _ = f int bool 0 true;;
|};
  [%expect {| |}]
;;

let%expect_test "if static nested in let expression" =
  go
    {|
let f = fn (static x : int) ->
  let y = if static x == 0 then 1 else true in
  y;;
let _ = f 0;;
let _ = f 1;;
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
  if static b then fn (x : int) -> inc x else fn (x : int) -> x;;
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
  if static b then fn (x : int) -> inc x else fn (x : int) -> x in
let _ = choose true 5 in
let _ = choose false 5 in
();;
|};
  [%expect {| |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun erased f (x : int) : int = g x
and g (y : int) : int = let _ = if y < 0 then f y else 0 in 0;;
let _ = g 0;;
|};
  [%expect {| |}]
;;

let%expect_test "fun env" =
  go
    {|
let a = 0 @ dynamic;;
fun f (x : int) : int = let _ = a in x;;
|};
  [%expect {| |}]
;;

let%expect_test "fun recurse" =
  go
    {|
let a = 0 @ dynamic;;
fun f (x : int) : int = let _ = a in f x;;
|};
  [%expect {| |}]
;;

let%expect_test "fun recurse" =
  go
    {|
let a = 0 @ dynamic;;
let _ =
fun f (x : int) : int = let _ = a in f x in
()
;;
|};
  [%expect {| |}]
;;

let%expect_test "recursive env" =
  go
    {|
let a = 0 @ dynamic;;
let b = 1 @ dynamic;;
let c = 2 @ dynamic;;
fun f (x : int) : int = let _ = a in let _ = b in g x
and g (y : int) : int = let _ = a in let _ = c in f y;;
|};
  [%expect {| |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = let _ = if x < 0 then g x else 0 in 0
and erased g (y : int) : int = f y;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "erased fn" =
  go
    {|
fun erased f (x : int) : int = 0;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "erased fn" =
  go
    {|
let f = fn erased (x : int) -> 0;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "erased fn" =
  go
    {|
fun erased f (erased x : int) : int = 0;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "erased fn" =
  go
    {|
let f = fn erased (erased x : int) -> 0;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = g x
and erased g (y : int) : int = if y == 0 then 0 else f (y-1);;
let _ = (f @ erased) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "static lambda" =
  go
    {|
let _ = (fn (static x : int) -> x + 1) 0;;
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
  if static b then fn (x : int) -> inc x else fn (x : int) -> x;;
let _ = choose true 5;;
let _ = choose false 5;;
|};
  [%expect {| |}]
;;

let%expect_test "mutual pi recursion" =
  go
    {|
fun f (static erased t : type) : t -> t = fn (x : t) -> x
and g (static erased t : type) : t -> t = f t;;
let _ = g int 0;;
|};
  [%expect {| |}]
;;

let%expect_test "static recursion with base case" =
  go
    {|
fun f (static x : int) : int = if static x == 0 then x else f (x - 1);;
let _ = f 3;;
|};
  [%expect {| |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (static x : int) : erased int = (if static x == 0 then 42 else f (x - 1)) @ erased;;
let _ = f 3;;
|};
  [%expect {| |}]
;;

let%expect_test "arrow and pi mutual recursion with application" =
  go
    {|
fun double (x : int) : int = x + x
and apply_double (static erased t : type) : int -> int = fn (x : int) -> double x;;
let _ = apply_double int 5;;
|};
  [%expect {| |}]
;;

let%expect_test "mutually recursive fun with static arg" =
  go
    {|
fun id1 (static erased t : type) : t -> t = fn (x : t) -> x
and id2 (static erased t : type) : t -> t = id1 t;;
|};
  [%expect {| |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = g x
and erased g (y : int) : int = y;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = (g @ erased) x
and g (y : int) : int = y;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : erased int = g x
and g (y : int) : erased int = y;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun erased f (x : int) : int = (g @ erased) x
and g (y : int) : int = y;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = if x == 0 then 0 else g (x - 1)
and g (y : int) : int = if y == 0 then 0 else f (y - 1);;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = if x == 0 then 0 else g (x - 1)
and erased g (y : int) : int = if y == 0 then 0 else f (y - 1);;
let _ = (f @ erased) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = if x == 0 then 0 else g (x - 1)
and g (y : int) : int = if y == 0 then 0 else (f @ erased) (y - 1);;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : erased int = g x
and g (y : int) : int = let _ = f y in 0;;
let _ = g 0;;
|};
  [%expect {| |}]
;;

let%expect_test "erased fn" =
  go
    {|
fun erased f (x : int) : int = 0;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "erased fn" =
  go
    {|
let f = fn erased (x : int) -> 0;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "static int" =
  go
    {|
let f = fn (static x : int) -> x;;
let _ = f (f 1);;
|};
  [%expect {| |}]
;;

let%expect_test "top-level mutual recursion with different bodies" =
  go
    {|
fun f (a : int) : int = g (a + 1)
and g (b : int) : int = b;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "top-level mutual static recursion" =
  go
    {|
fun f (static x : int) : int = if static x == 0 then 0 else g (x - 1)
and g (static y : int) : int = if static y == 0 then 1 else f (y - 1);;
let _ = f 2;;
|};
  [%expect {| |}]
;;

let%expect_test "static mutual recursion cross-monomorphization" =
  go
    {|
fun f (static erased t : type) : t -> t = g t
and g (static erased t : type) : t -> t = fn (x : t) -> x;;
let _ = f int 0;;
|};
  [%expect {| |}]
;;

let%expect_test "static mutual recursion cross-monomorphization" =
  go
    {|
fun f (static erased t : type) : t -> t = fn (x : t) -> x
and g (static erased t : type) : t -> t = f t;;
let _ = g int 0;;
|};
  [%expect {| |}]
;;

let%expect_test "local fun inside top-level fun" =
  go
    {|
fun outer (x : int) : int =
  fun inner (y : int) : int = y + x in
  inner x;;
let _ = outer 5;;
|};
  [%expect {| |}]
;;

let%expect_test "mutually recursive local closures share environment" =
  go
    {|
fun outer (x : int) : int =
  fun f (a : int) : int = if a == 0 then 0 else g (a + x)
  and g (b : int) : int = if b == 0 then 0 else f (b + x) in
  f 0;;
let _ = outer 5;;
|};
  [%expect {| |}]
;;

let%expect_test "monomorphizing side effects" =
  go
    {|
external print_int : int -> unit = syl_std_print_int;;
fun print (static _ : unit) : unit = print_int 0;;
let _ = print ();;
|};
  [%expect {| |}]
;;

let%expect_test "external" =
  go
    {|
external f : int -> unit = syl_std_print_int;;
let _ = f 0;;
|};
  [%expect {| |}]
;;

let%expect_test "external" =
  go
    {|
external f : int -> unit = syl_std_print_int;;
let _ = (f @ erased) 0;;
|};
  [%expect {| |}]
;;

let%expect_test "static lambda effects" =
  go
    {|
external print_int : int -> unit = syl_std_print_int;;
let print = fn (static x : int) -> print_int x;;
let _ = print 0;;
let _ = print 1;;
|};
  [%expect {| |}]
;;

let%expect_test "static erased lambda" =
  go
    {|
fun apply (static erased f : int -> int) : int = f 0;;
let x = apply (fn (x : int) -> x + 1);;
|};
  [%expect {| |}]
;;

let%expect_test "static lambda effects" =
  go
    {|
fun mk_ident (static erased pick_t : static unit -> static erased type) : static (pick_t () -> pick_t ()) =
  fn (x : pick_t ()) -> x
;;

let _ = mk_ident (fn (static _ : unit) -> if 1 + 1 == 2 then bool else unit) true;;
|};
  [%expect {| |}]
;;

let%expect_test "static lambda effects" =
  go
    {|
fun mk_ident (static pick_t : static unit -> static int) : static (let t = if pick_t () == 0 then int else bool in t -> t) =
  fn (x : if pick_t () == 0 then int else bool) -> x
;;

let _ = mk_ident (fn (static _ : unit) -> 1) true;;
|};
  [%expect {| |}]
;;

let%expect_test "static lambda effects are thunked" =
  go
    {|
external print_int : int -> unit = syl_std_print_int;;

fun mk_ident (static pick_t : static unit -> static int) : static (let t = if pick_t () == 0 then int else bool in t -> t) =
  let _ = pick_t () in
  let _ = pick_t () in
  fn (x : if pick_t () == 0 then int else bool) -> x
;;

let _ = mk_ident (fn (static _ : unit) -> let _ = print_int 10 in 1) true;;
|};
  [%expect {| |}]
;;

let%expect_test "external" =
  go
    {|
external print_int : int -> unit = syl_std_print_int;;
|};
  [%expect {| |}]
;;

let%expect_test "scoping" =
  go
    {|
let x = 1 @ dynamic;;
let _ = let _ = let _ = x + x in x + x in x + x;;
|};
  [%expect {| |}]
;;

let%expect_test "scoping" =
  go
    {|
let c = true @ dynamic;;
let _ = if c then 0 else if !c then 1 else 2;;
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

let%expect_test "tuple type basics" =
  go
    {|
let t1 = int ^ int;;
let t2 = int ^ (bool ^ unit);;
let t3 = (int ^ bool) ^ unit;;
let t4 = int ^ bool ^ unit;;
|};
  [%expect {| |}]
;;

let%expect_test "tuple type used as annotation" =
  go
    {|
let t = (1, 2) : int ^ int;;
|};
  [%expect {| |}]
;;

let%expect_test "tuple type annotation nested" =
  go
    {|
let t = (1, (true, ())) : int ^ (bool ^ unit);;
|};
  [%expect {| |}]
;;

let%expect_test "tuple type in function argument" =
  go
    {|
let f = fn (t : int ^ int) -> t;;
let _ = f (1, 2);;
|};
  [%expect {| |}]
;;

let%expect_test "tuple type in arrow return" =
  go
    {|
let f = (fn (x : int) -> (x, x)) : int -> int ^ int;;
|};
  [%expect {| |}]
;;

let%expect_test "tuple type computed statically" =
  go
    {|
let T = int ^ bool;;
let t = (1, true) : T;;
|};
  [%expect {| |}]
;;

let%expect_test "tuple values" =
  go
    {|
let t1 = 1, 2;;
let t2 = 1, (true, ());;
let t3 = (1, true), ();;
let t4 = 1, true, ();;
|};
  [%expect {| |}]
;;

let%expect_test "tuple with dynamic elements" =
  go
    {|
let x = 1 @ dynamic;;
let t1 = x, 2;;
let t2 = 1, x;;
let t3 = x, x;;
|};
  [%expect {| |}]
;;

let%expect_test "tuple as function return" =
  go
    {|
let f = fn (x : int) -> (x, x);;
let _ = f 1;;
|};
  [%expect {| |}]
;;

let%expect_test "nested tuple" =
  go
    {|
let t = (1, (2, 3)), (4, 5);;
|};
  [%expect {| |}]
;;

let%expect_test "tuple with arithmetic" =
  go
    {|
let t = 1 + 2, 3 * 4;;
|};
  [%expect {| |}]
;;

let%expect_test "tuple in let binding used later" =
  go
    {|
let t = 1, 2;;
let _ = t;;
|};
  [%expect {| |}]
;;

let%expect_test "tuple with if expression elements" =
  go
    {|
let t = (if true then 1 else 2), (if false then 3 else 4);;
|};
  [%expect {| |}]
;;

let%expect_test "tuple of functions" =
  go
    {|
let t = (fn (x : int) -> x), (fn (y : bool) -> y);;
|};
  [%expect {| |}]
;;

let%expect_test "large tuple" =
  go
    {|
let t = 1, 2, (), 4, 5;;
|};
  [%expect {| |}]
;;

let%expect_test "tuple with erased elements" =
  go
    {|
let x = 1 @ erased;;
let t = x, 2;;
|};
  [%expect {| |}]
;;

let%expect_test "tuple mixing types and values" =
  go
    {|
let t = int, 1;;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: gte 5 >= 3 gives wrong constant" =
  go
    {|
let _ = assert static (5 >= 3);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: gte 3 >= 5 gives wrong constant" =
  go
    {|
let _ = assert static !(3 >= 5);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: gte equal values" =
  go
    {|
let _ = assert static (3 >= 3);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: all comparison operators" =
  go
    {|
let _ = assert static (1 < 2);;
let _ = assert static !(2 < 1);;
let _ = assert static (1 <= 2);;
let _ = assert static !(2 <= 1);;
let _ = assert static !(1 > 2);;
let _ = assert static (2 > 1);;
let _ = assert static (1 == 1);;
let _ = assert static (1 != 2);;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: capture bool then int (misaligned access)" =
  go
    {|
let b = true @ dynamic;;
let n = 42 @ dynamic;;
let f = fn (_ : unit) -> if b then n else 0;;
let _ = f ();;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: capture int then bool then int" =
  go
    {|
let a = 1 @ dynamic;;
let b = true @ dynamic;;
let c = 2 @ dynamic;;
let f = fn (_ : unit) -> if b then a else c;;
let _ = f ();;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: capture bool then int (misaligned access)" =
  go
    {|
let b = true @ dynamic;;
let n = 42 @ dynamic;;
let f = fn (_ : unit) -> if b then n else 0;;
let _ = f ();;
|};
  [%expect {| |}]
;;

let%expect_test "fuzz: capture int then bool then int" =
  go
    {|
let a = 1 @ dynamic;;
let b = true @ dynamic;;
let c = 2 @ dynamic;;
let f = fn (_ : unit) -> if b then a else c;;
let _ = f ();;
|};
  [%expect {| |}]
;;

let%expect_test "assert dynamic literal" =
  go
    {|
let _ = assert true;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert dynamic variable" =
  go
    {|
let x = true @ dynamic;;
let _ = assert x;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert dynamic result is unit" =
  go
    {|
fun f (x : bool) : unit = assert x;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert dynamic result is static" =
  go
    {|
let x = true;;
let _ = (assert x) @ static;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert dynamic result is static" =
  go
    {|
let x = true;;
let _ = (assert x) @ static erased;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert dynamic in function body" =
  go
    {|
fun check (x : int) : unit =
  assert (x > 0);;
  |};
  [%expect {| |}]
;;

let%expect_test "assert dynamic in let body" =
  go
    {|
fun f (x : int) : int =
  let _ = assert (x >= 0) in
  x + 1;;
  |};
  [%expect {| |}]
;;

let%expect_test "assert dynamic in if branch" =
  go
    {|
fun f (x : int) : int =
  if x > 0 then
    let _ = assert (x != 0) in
    x
  else
    0 - x;;
  |};
  [%expect {| |}]
;;

let%expect_test "var" =
  go
    {|
let x = true @ dynamic;;
let _ =
  match !x with
  | x -> assert !x
;;
|};
  [%expect {| |}]
;;
