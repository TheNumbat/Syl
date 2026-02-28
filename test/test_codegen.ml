open! Core
open! Syl

let check = `No

let compile c =
  let tmp = Stdlib.Filename.temp_file "syl_test" ".c" in
  Out_channel.write_all tmp ~data:c;
  let cmd = Printf.sprintf "clang -fsyntax-only -w %s 2>/dev/null" tmp in
  (match Core_unix.system cmd with
   | Ok () -> ()
   | Error (`Exit_non_zero exit_code) -> raise_s [%message "Clang failed" (exit_code : int)]
   | Error (`Signal signal) -> raise_s [%message "Clang killed" (signal : Signal.t)]);
  Core_unix.unlink tmp
;;

let compile_and_run c =
  let tmp_c = Stdlib.Filename.temp_file "syl_test" ".c" in
  let tmp_exe = Stdlib.Filename.temp_file "syl_test" ".exe" in
  Out_channel.write_all tmp_c ~data:c;
  let cmd = Printf.sprintf "clang -o %s -w %s 2>/dev/null" tmp_exe tmp_c in
  (match Core_unix.system cmd with
   | Ok () -> ()
   | Error (`Exit_non_zero exit_code) -> raise_s [%message "Clang failed" (exit_code : int)]
   | Error (`Signal signal) -> raise_s [%message "Clang killed" (signal : Signal.t)]);
  Core_unix.unlink tmp_c;
  (match Core_unix.system tmp_exe with
   | Ok () -> ()
   | Error (`Exit_non_zero exit_code) -> raise_s [%message "Program failed" (exit_code : int)]
   | Error (`Signal signal) -> raise_s [%message "Program killed" (signal : Signal.t)]);
  Core_unix.unlink tmp_exe
;;

let strip_prelude c =
  let c_preamble_end = "//SYL_PRELUDE_END" in
  match String.substr_index c ~pattern:c_preamble_end with
  | None -> c
  | Some i ->
    let c = String.drop_prefix c (i + String.length c_preamble_end) in
    (match String.chop_prefix c ~prefix:"\n" with
     | Some c -> c
     | None -> c)
;;

let go input =
  let cst = Parse.parse_exn input in
  let tst = Typecheck.typecheck_exn cst in
  let sst = Simplify.simplify tst in
  let lst = Linearize.linearize sst in
  let c = Codegen.c lst in
  print_string (strip_prelude c);
  match check with
  | `Compile -> compile c
  | `Run -> compile_and_run c
  | _ -> ()
;;

let%expect_test "names" =
  go
    {|
let x = ();;
let x = ();;
|};
  [%expect
    {|
    static syl_unit _x;
    static syl_unit _xˢ1;
    int main()
    {
      _x = 0;
      _xˢ1 = 0;
      return 0;
    }
    |}]
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
  [%expect
    {|
    static syl_unit __;
    static syl_bool __ˢ1;
    static syl_int __ˢ2;
    static syl_unit __ˢ3;
    static syl_bool __ˢ4;
    static syl_int __ˢ5;
    int main()
    {
      __ = 0;
      __ˢ1 = true;
      __ˢ2 = 123;
      __ˢ3 = 0;
      __ˢ4 = true;
      __ˢ5 = 123;
      return 0;
    }
    |}]
;;

let%expect_test "Mode annotation valid static" =
  go
    {|
let _ =
  1 @ static
;;|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      __ = 1;
      return 0;
    }
    |}]
;;

let%expect_test "Mode annotation valid dynamic" =
  go
    {|
let _ =
  1 @ dynamic
;;|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      __ = 1;
      return 0;
    }
    |}]
;;

let%expect_test "Mode annotation valid" =
  go
    {|
let dyn = 1;;
let _ =
  dyn @ dynamic erased
;;|};
  [%expect
    {|
    static syl_int _dyn;
    int main()
    {
      _dyn = 1;
      return 0;
    }
    |}]
;;

let%expect_test "Mode annotation valid" =
  go
    {|
let dyn = 1 @ erased;;
let _ =
  dyn @ dynamic erased
;;|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "Mode annotation valid" =
  go
    {|
let dyn = 1;;
let _ =
  dyn @ dynamic
;;|};
  [%expect
    {|
    static syl_int _dyn;
    static syl_int __;
    int main()
    {
      _dyn = 1;
      __ = _dyn;
      return 0;
    }
    |}]
;;

let%expect_test "Mode annotation valid" =
  go
    {|
let dyn = 1 @ dynamic;;
let _ =
  dyn @ dynamic erased
;;|};
  [%expect
    {|
    static syl_int _dyn;
    int main()
    {
      _dyn = 1;
      return 0;
    }
    |}]
;;

let%expect_test "Unop static" =
  go
    {|
let _ =
  !true
;;|};
  [%expect
    {|
    static syl_bool __;
    int main()
    {
      __ = false;
      return 0;
    }
    |}]
;;

let%expect_test "Unop dynamic" =
  go
    {|
let _ =
  !(true @ dynamic)
;;|};
  [%expect
    {|
    static syl_bool __;
    int main()
    {
      __ = false;
      return 0;
    }
    |}]
;;

let%expect_test "dynamic static erased" =
  go
    {|
let _ =
  (true @ static erased)
;;|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "dynamic erased" =
  go
    {|
let _ =
  (true @ dynamic erased)
;;|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "erased dynamic" =
  go
    {|
let _ =
  ((true @ erased) @ dynamic)
;;|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "Unop var static" =
  go
    {|
let dyn = true;;
let _ =
  !dyn
;;|};
  [%expect
    {|
    static syl_bool _dyn;
    static syl_bool __;
    int main()
    {
      _dyn = true;
      __ = !_dyn;
      return 0;
    }
    |}]
;;

let%expect_test "Unop var erased" =
  go
    {|
let dyn = true @ erased;;
let _ =
  !dyn
;;|};
  [%expect
    {|
    static syl_bool __;
    int main()
    {
      __ = false;
      return 0;
    }
    |}]
;;

let%expect_test "Unop var erased" =
  go
    {|
let dyn = true @ erased;;
let _ =
  !(!dyn @ erased)
;;|};
  [%expect
    {|
    static syl_bool __;
    int main()
    {
      __ = true;
      return 0;
    }
    |}]
;;

let%expect_test "Unop var erased" =
  go
    {|
let dyn = true @ erased;;
let _ =
  !(!dyn)
;;|};
  [%expect
    {|
    static syl_bool __;
    int main()
    {
      __ = true;
      return 0;
    }
    |}]
;;

let%expect_test "Unop var dynamic" =
  go
    {|
let dyn = true @ dynamic;;
let _ =
  !dyn
;;|};
  [%expect
    {|
    static syl_bool _dyn;
    static syl_bool __;
    int main()
    {
      _dyn = true;
      __ = !_dyn;
      return 0;
    }
    |}]
;;

let%expect_test "Unop var dynamic" =
  go
    {|
let dyn = true @ dynamic;;
let x = !dyn @ erased;;|};
  [%expect
    {|
    static syl_bool _dyn;
    int main()
    {
      _dyn = true;
      return 0;
    }
    |}]
;;

let%expect_test "Binop static + static" =
  go
    {|
let _ =
  1 + 2
;;|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      __ = 3;
      return 0;
    }
    |}]
;;

let%expect_test "Binop static + static erased" =
  go
    {|
let _ =
  1 + (2 @ erased)
;;|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      __ = 3;
      return 0;
    }
    |}]
;;

let%expect_test "Binop erased dynamic" =
  go
    {|
let _ =
  1 + (2 @ dynamic)
;;|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      __ = 3;
      return 0;
    }
    |}]
;;

let%expect_test "Binop erased dynamic" =
  go
    {|
let _ =
  1 + (2 @ dynamic) + (3 @ erased)
;;|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      __ = 6;
      return 0;
    }
    |}]
;;

let%expect_test "Binop erased static" =
  go
    {|
let _ =
  1 + ((2 + 3) @ erased)
;;|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      __ = 6;
      return 0;
    }
    |}]
;;

let%expect_test "Binop static + dynamic" =
  go
    {|
let dyn = 2 @ dynamic;;
let _ =
  1 + dyn
;;|};
  [%expect
    {|
    static syl_int _dyn;
    static syl_int __;
    int main()
    {
      _dyn = 2;
      {
        syl_int _$ = 1;
        __ = _$ + _dyn;
      }
      return 0;
    }
    |}]
;;

let%expect_test "Binop dynamic + static" =
  go
    {|
let dyn = 1 @ dynamic;;
let _ =
  dyn + 2
;;|};
  [%expect
    {|
    static syl_int _dyn;
    static syl_int __;
    int main()
    {
      _dyn = 1;
      {
        syl_int _$ = 2;
        __ = _dyn + _$;
      }
      return 0;
    }
    |}]
;;

let%expect_test "Binop dynamic + dynamic" =
  go
    {|
let dyn1 = 1 @ dynamic;;
let dyn2 = 2 @ dynamic;;
let _ =
  dyn1 + dyn2
;;|};
  [%expect
    {|
    static syl_int _dyn1;
    static syl_int _dyn2;
    static syl_int __;
    int main()
    {
      _dyn1 = 1;
      _dyn2 = 2;
      __ = _dyn1 + _dyn2;
      return 0;
    }
    |}]
;;

let%expect_test "Binop erased + dynamic" =
  go
    {|
let dyn1 = 1 @ erased;;
let dyn2 = 2 @ dynamic;;
let _ =
  dyn1 + dyn2
;;|};
  [%expect
    {|
    static syl_int _dyn2;
    static syl_int __;
    int main()
    {
      _dyn2 = 2;
      {
        syl_int _$ = 1;
        __ = _$ + _dyn2;
      }
      return 0;
    }
    |}]
;;

let%expect_test "Binop erased + erased" =
  go
    {|
let dyn1 = 1 @ erased;;
let dyn2 = 2 @ erased;;
let _ =
  dyn1 + dyn2
;;|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      __ = 3;
      return 0;
    }
    |}]
;;

let%expect_test "If static cond static branches" =
  go
    {|
let _ =
  if true then 1 else 2
;;|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      __ = 1;
      return 0;
    }
    |}]
;;

let%expect_test "If erased" =
  go
    {|
let _ =
  (if true then int else int) @ erased
;;|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "If erased" =
  go
    {|
let _ =
  if true @ erased then 1 else 2
;;|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      __ = 1;
      return 0;
    }
    |}]
;;

let%expect_test "If erased cond" =
  go
    {|
let x = true || false @ erased;;
let _ =
  if x then 1 else 2
;;|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      __ = 1;
      return 0;
    }
    |}]
;;

let%expect_test "If static cond dynamic branches" =
  go
    {|
let dyn = 1 @ dynamic;;
let _ =
  if true then dyn else 2
;;|};
  [%expect
    {|
    static syl_int _dyn;
    static syl_int __;
    int main()
    {
      _dyn = 1;
      __ = _dyn;
      return 0;
    }
    |}]
;;

let%expect_test "If dynamic cond" =
  go
    {|
let dyn = true @ dynamic;;
let _ =
  if dyn then 1 else 2
;;|};
  [%expect
    {|
    static syl_bool _dyn;
    static syl_int __;
    int main()
    {
      _dyn = true;
      {
        syl_int __·if;
        if(_dyn)
        {
          __·if = 1;
        }
        else
        {
          __·if = 2;
        }
        __ = __·if;
      }
      return 0;
    }
    |}]
;;

let%expect_test "erased if expr" =
  go
    {|
let x = true;;
let _ =
  0 + ((if x then 1 else 2) @ erased)
;;|};
  [%expect
    {|
    static syl_bool _x;
    static syl_int __;
    int main()
    {
      _x = true;
      __ = 1;
      return 0;
    }
    |}]
;;

let%expect_test "if erased branch" =
  go
    {|
let _ = if 1==2 then unit else int;;|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "if erased branch" =
  go
    {|
let cond = true @ dynamic;;
let t = if cond then unit else int;;|};
  [%expect
    {|
    static syl_bool _cond;
    int main()
    {
      _cond = true;
      return 0;
    }
    |}]
;;

let%expect_test "if erased branch" =
  go
    {|
let cond = true @ dynamic;;
let t = (if cond then false else cond) @ erased;;|};
  [%expect
    {|
    static syl_bool _cond;
    int main()
    {
      _cond = true;
      return 0;
    }
    |}]
;;

let%expect_test "Let static" =
  go
    {|
let _ =
  let x = 1 in
  x
;;|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      {
        syl_int __·x = 1;
        __ = __·x;
      }
      return 0;
    }
    |}]
;;

let%expect_test "Let dynamic" =
  go
    {|
let dyn = 1 @ dynamic;;
let _ =
  let x = dyn in
  x
;;|};
  [%expect
    {|
    static syl_int _dyn;
    static syl_int __;
    int main()
    {
      _dyn = 1;
      {
        syl_int __·x = _dyn;
        __ = __·x;
      }
      return 0;
    }
    |}]
;;

let%expect_test "Let dynamic" =
  go
    {|
let dyn = 1 @ erased;;
let _ =
  let x = dyn + 1 @ dynamic in
  x
;;|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      {
        syl_int __·x = 2;
        __ = __·x;
      }
      return 0;
    }
    |}]
;;

let%expect_test "Let erased" =
  go
    {|
let dyn = 1 @ erased;;
let _ =
  let x = dyn in
  x
;;|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "Let erased" =
  go
    {|
let _ =
  let x = 1 @ erased in
  x + 1
;;|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      __ = 2;
      return 0;
    }
    |}]
;;

let%expect_test "Let erased" =
  go
    {|
let _ =
  let x = 1 @ erased in
  let y = 1 @ dynamic in
  x + y
;;|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      {
        syl_int __·y = 1;
        syl_int _$ = 1;
        __ = _$ + __·y;
      }
      return 0;
    }
    |}]
;;

let%expect_test "Let erased" =
  go
    {|
let _ =
  let x = 1 @ erased in
  let y = 1 @ erased in
  0 + (x + y)
;;|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      __ = 2;
      return 0;
    }
    |}]
;;

let%expect_test "Let erased" =
  go
    {|
let _ =
  let x = 1 in
  let y = 1 in
  0 + ((x + y) @ erased)
;;|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      {
        syl_int __·x = 1;
        syl_int __·y = 1;
        __ = 2;
      }
      return 0;
    }
    |}]
;;

let%expect_test "static closure" =
  go
    {|
let _ =
  (fn (x : int) -> x)
;;|};
  [%expect
    {|
    static syl_int __·λ(syl_int, syl_env);
    static syl_closure __;
    static syl_int __·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    int main()
    {
      {
        syl_env __·env = NULL;
        __ = syl_mk_closure(__·λ, __·env);
      }
      return 0;
    }
    |}]
;;

let%expect_test "erased closure" =
  go
    {|
let _ =
  (fn (x : int) -> x) @ erased
;;|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "closure return type" =
  go
    {|
let _ =
  (fn (x : int) -> int)
;;|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "dynamic closure" =
  go
    {|
let y = 1 @ dynamic;;
let _ =
  (fn (x : int) -> x + y)
;;|};
  [%expect
    {|
    static syl_int _y;
    static syl_int __·λ(syl_int, syl_env);
    static syl_closure __;
    static syl_int __·λ(syl_int _x, syl_env 𝒰)
    {
      syl_int _y = 𝒰[0];
      return _x + _y;
    }
    int main()
    {
      _y = 1;
      {
        syl_env __·env = syl_capture(1, _y);
        __ = syl_mk_closure(__·λ, __·env);
      }
      return 0;
    }
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (erased x : int) -> x;;
let _ = f 0;;
|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (erased x : int) -> 1;;
let _ = f 0;;
|};
  [%expect
    {|
    static syl_int _f·λ(syl_int, syl_env);
    static syl_closure _f;
    static syl_int __;
    static syl_int _f·λ(syl_int _x, syl_env 𝒰)
    {
      return 1;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _f = syl_mk_closure(_f·λ, _f·env);
      }
      {
        syl_int _$ = 0;
        __ = syl_app_closure(_f, _$);
      }
      return 0;
    }
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = (fn (erased x : int) -> 1) @ erased;;
let _ = f 0;;
|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      __ = 1;
      return 0;
    }
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = (fn (erased x : int) -> 1) ;;
let _ = f 0;;
|};
  [%expect
    {|
    static syl_int _f·λ(syl_int, syl_env);
    static syl_closure _f;
    static syl_int __;
    static syl_int _f·λ(syl_int _x, syl_env 𝒰)
    {
      return 1;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _f = syl_mk_closure(_f·λ, _f·env);
      }
      {
        syl_int _$ = 0;
        __ = syl_app_closure(_f, _$);
      }
      return 0;
    }
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = (fn (erased x : int) -> 1) ;;
let _ = f (0 @ erased);;
|};
  [%expect
    {|
    static syl_int _f·λ(syl_int, syl_env);
    static syl_closure _f;
    static syl_int __;
    static syl_int _f·λ(syl_int _x, syl_env 𝒰)
    {
      return 1;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _f = syl_mk_closure(_f·λ, _f·env);
      }
      {
        syl_int _$ = 0;
        __ = syl_app_closure(_f, _$);
      }
      return 0;
    }
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (erased x : int) -> 1;;
let _ = f 0;;
|};
  [%expect
    {|
    static syl_int _f·λ(syl_int, syl_env);
    static syl_closure _f;
    static syl_int __;
    static syl_int _f·λ(syl_int _x, syl_env 𝒰)
    {
      return 1;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _f = syl_mk_closure(_f·λ, _f·env);
      }
      {
        syl_int _$ = 0;
        __ = syl_app_closure(_f, _$);
      }
      return 0;
    }
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let _ = (fn (erased x : int) -> 1) 0;;
|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      {
        syl_int __·x = 0;
        __ = 1;
      }
      return 0;
    }
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let _ = (fn (erased x : int) -> 1) (0 @ erased);;
|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      {
        syl_int __·x = 0;
        __ = 1;
      }
      return 0;
    }
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let _ = (fn (static x : int) -> 1) 0;;
|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      {
        syl_int __·x = 0;
        __ = 1;
      }
      return 0;
    }
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let _ = (fn (static erased x : int) -> 1) 0;;
|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      __ = 1;
      return 0;
    }
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let _ = (fn (static erased x : int) -> 1) (0 @ erased);;
|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      __ = 1;
      return 0;
    }
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (static erased g : int -> erased int) -> let _ = g 1 in 2;;
let _ = f (fn (x : int) -> 0 @ erased);;
|};
  [%expect
    {|
    static syl_int _f·λₒλ1(syl_env);
    static syl_int _fₒλ1;
    static syl_int __;
    static syl_int _f·λₒλ1(syl_env 𝒰)
    {
      return 2;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒλ1 = syl_mk_thunk(_f·λₒλ1, _f·env);
      }
      __ = syl_app_thunk(_fₒλ1);
      return 0;
    }
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (erased x : int -> int) -> 1;;
let _ = f ((fn (x : int) -> x + 1) @ erased);;
|};
  [%expect
    {|
    static syl_int _f·λ(syl_closure, syl_env);
    static syl_closure _f;
    static syl_int __·λ(syl_int, syl_env);
    static syl_int __;
    static syl_int _f·λ(syl_closure _x, syl_env 𝒰)
    {
      return 1;
    }
    static syl_int __·λ(syl_int _x, syl_env 𝒰)
    {
      syl_int _$ = 1;
      return _x + _$;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _f = syl_mk_closure(_f·λ, _f·env);
      }
      {
        syl_env __·env = NULL;
        syl_closure _$ = syl_mk_closure(__·λ, __·env);
        __ = syl_app_closure(_f, _$);
      }
      return 0;
    }
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = (fn (erased x : int -> int) -> 1) @ erased;;
let _ = f ((fn (x : int) -> x + 1) @ erased);;
|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      __ = 1;
      return 0;
    }
    |}]
;;

let%expect_test "static erased closure arg" =
  go
    {|
let _ =
  (fn (static erased x : int) -> x)
;;|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "dependent closure arg" =
  go
    {|
let _ =
  (fn (x : type) -> x)
;;|};
  [%expect
    {|
    static syl_unit __·λ(syl_unit, syl_env);
    static syl_closure __;
    static syl_unit __·λ(syl_unit _x, syl_env 𝒰)
    {
      return _x;
    }
    int main()
    {
      {
        syl_env __·env = NULL;
        __ = syl_mk_closure(__·λ, __·env);
      }
      return 0;
    }
    |}]
;;

let%expect_test "dependent closure arg" =
  go
    {|
fun f (x : type) : type = x;;|};
  [%expect
    {|
    static syl_unit _f·λ(syl_unit, syl_env);
    static syl_closure _f;
    static syl_unit _f·λ(syl_unit _x, syl_env 𝒰)
    {
      return _x;
    }
    int main()
    {
      {
        syl_env 𝒰 = NULL;
        _f = syl_mk_closure(_f·λ, 𝒰);
      }
      return 0;
    }
    |}]
;;

let%expect_test "dependent closure arg" =
  go
    {|
fun f (static x : int) : static erased type = int;;|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "dependent closure arg" =
  go
    {|
let _ =
  (fn (static x : type) -> x)
;;|};
  [%expect
    {|
    int main()
    {
      {
        syl_env __·env = NULL;
      }
      return 0;
    }
    |}]
;;

let%expect_test "dependent closure arg" =
  go
    {|
let _ =
  (fn (erased x : type) -> x)
;;|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "closure erased" =
  go
    {|
let x = (fn (x : int) -> x) @ erased;;
|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "closure erased" =
  go
    {|
let x = (fn (erased x : int) -> x) 0;;
|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "closure erased" =
  go
    {|
let x = (fn (erased x : int) -> 1) 0;;
|};
  [%expect
    {|
    static syl_int _x;
    int main()
    {
      {
        syl_int _x·x = 0;
        _x = 1;
      }
      return 0;
    }
    |}]
;;

let%expect_test "closure erased" =
  go
    {|
let x = ((fn (erased x : int) -> 1) @ erased) 0;;
|};
  [%expect
    {|
    static syl_int _x;
    int main()
    {
      _x = 1;
      return 0;
    }
    |}]
;;

let%expect_test "closure branches" =
  go
    {|
let f = (fn (erased x : int) -> 1);;
let g = (fn (static x : int) -> 2);;
let _ = if true then f else g;;
|};
  [%expect
    {|
    static syl_int _f·λ(syl_int, syl_env);
    static syl_closure _f;
    static syl_closure __;
    static syl_int _f·λ(syl_int _x, syl_env 𝒰)
    {
      return 1;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _f = syl_mk_closure(_f·λ, _f·env);
      }
      {
        syl_env _g·env = NULL;
      }
      __ = _f;
      return 0;
    }
    |}]
;;

let%expect_test "static erased arg" =
  go
    {|
let f = (fn (static erased x : int) -> 1);;
let _ = f 0;;
let _ = f 1;;
|};
  [%expect
    {|
    static syl_int _f·λₒ1(syl_env);
    static syl_int _f·λₒ0(syl_env);
    static syl_int _fₒ1;
    static syl_int _fₒ0;
    static syl_int __;
    static syl_int __ˢ1;
    static syl_int _f·λₒ1(syl_env 𝒰)
    {
      return 1;
    }
    static syl_int _f·λₒ0(syl_env 𝒰)
    {
      return 1;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒ1 = syl_mk_thunk(_f·λₒ1, _f·env);
        _fₒ0 = syl_mk_thunk(_f·λₒ0, _f·env);
      }
      __ = syl_app_thunk(_fₒ0);
      __ˢ1 = syl_app_thunk(_fₒ1);
      return 0;
    }
    |}]
;;

let%expect_test "closure branches" =
  go
    {|
let f = (fn (static erased x : int) -> 1);;
let g = (fn (static x : int) -> 2);;
let h = if true then f else g;;
let _ = h 0;;
|};
  [%expect
    {|
    static syl_int _f·λₒ0(syl_env);
    static syl_int _fₒ0;
    static syl_int _hₒ0;
    static syl_int __;
    static syl_int _f·λₒ0(syl_env 𝒰)
    {
      syl_int _f·λₒ0·x = 0;
      return 1;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒ0 = syl_mk_thunk(_f·λₒ0, _f·env);
      }
      {
        syl_env _g·env = NULL;
      }
      _hₒ0 = _fₒ0;
      __ = syl_app_thunk(_hₒ0);
      return 0;
    }
    |}]
;;

let%expect_test "closure branches" =
  go
    {|
let f = (fn (static erased x : int) -> 1);;
let g = (fn (static erased x : int) -> 2);;
let _ = if true then f else g;;
|};
  [%expect
    {|
    int main()
    {
      {
        syl_env _f·env = NULL;
      }
      {
        syl_env _g·env = NULL;
      }
      return 0;
    }
    |}]
;;

let%expect_test "closure erased" =
  go
    {|
let c = fn (_ : unit) -> true;;
let f = (fn (x : int) -> 1);;
let g = (fn (erased x : int) -> 2);;
let _ = (if c () then f else g) 0;;
|};
  [%expect
    {|
    static syl_bool _c·λ(syl_unit, syl_env);
    static syl_closure _c;
    static syl_int _f·λ(syl_int, syl_env);
    static syl_closure _f;
    static syl_int _g·λ(syl_int, syl_env);
    static syl_closure _g;
    static syl_int __;
    static syl_bool _c·λ(syl_unit __, syl_env 𝒰)
    {
      return true;
    }
    static syl_int _f·λ(syl_int _x, syl_env 𝒰)
    {
      return 1;
    }
    static syl_int _g·λ(syl_int _x, syl_env 𝒰)
    {
      return 2;
    }
    int main()
    {
      {
        syl_env _c·env = NULL;
        _c = syl_mk_closure(_c·λ, _c·env);
      }
      {
        syl_env _f·env = NULL;
        _f = syl_mk_closure(_f·λ, _f·env);
      }
      {
        syl_env _g·env = NULL;
        _g = syl_mk_closure(_g·λ, _g·env);
      }
      {
        syl_unit _$ = 0;
        syl_closure __·if;
        if(syl_app_closure(_c, _$))
        {
          __·if = _f;
        }
        else
        {
          __·if = _g;
        }
        syl_int _$ˢ1 = 0;
        __ = syl_app_closure(__·if, _$ˢ1);
      }
      return 0;
    }
    |}]
;;

let%expect_test "closure nest" =
  go
    {|
let f = fn (x : int) -> 1;;
let g = fn (f : int -> int) -> f 0;;
let _ = g f;;
|};
  [%expect
    {|
    static syl_int _f·λ(syl_int, syl_env);
    static syl_closure _f;
    static syl_int _g·λ(syl_closure, syl_env);
    static syl_closure _g;
    static syl_int __;
    static syl_int _f·λ(syl_int _x, syl_env 𝒰)
    {
      return 1;
    }
    static syl_int _g·λ(syl_closure _f, syl_env 𝒰)
    {
      syl_int _$ = 0;
      return syl_app_closure(_f, _$);
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _f = syl_mk_closure(_f·λ, _f·env);
      }
      {
        syl_env _g·env = NULL;
        _g = syl_mk_closure(_g·λ, _g·env);
      }
      __ = syl_app_closure(_g, _f);
      return 0;
    }
    |}]
;;

let%expect_test "closure nest" =
  go
    {|
let f = fn (x : int) -> 1;;
let g = fn (static f : int -> int) -> f 0;;
let _ = g f;;
|};
  [%expect
    {|
    static syl_int _f·λ(syl_int, syl_env);
    static syl_closure _f;
    static syl_int _g·λₒλ1·f·λ(syl_int, syl_env);
    static syl_int _g·λₒλ1(syl_env);
    static syl_int _gₒλ1;
    static syl_int __;
    static syl_int _f·λ(syl_int _x, syl_env 𝒰)
    {
      return 1;
    }
    static syl_int _g·λₒλ1·f·λ(syl_int _x, syl_env 𝒰)
    {
      return 1;
    }
    static syl_int _g·λₒλ1(syl_env 𝒰)
    {
      syl_closure _g·λₒλ1·f;
      {
        syl_env _g·λₒλ1·f·env = NULL;
        _g·λₒλ1·f = syl_mk_closure(_g·λₒλ1·f·λ, _g·λₒλ1·f·env);
      }
      syl_int _$ = 0;
      return syl_app_closure(_g·λₒλ1·f, _$);
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _f = syl_mk_closure(_f·λ, _f·env);
      }
      {
        syl_env _g·env = NULL;
        _gₒλ1 = syl_mk_thunk(_g·λₒλ1, _g·env);
      }
      __ = syl_app_thunk(_gₒλ1);
      return 0;
    }
    |}]
;;

let%expect_test "closure nest" =
  go
    {|
let f = fn (erased x : int) -> 1;;
let g = fn (static f : erased int -> int) -> (f @ erased) 0;;
let _ = g f;;
|};
  [%expect
    {|
    static syl_int _f·λ(syl_int, syl_env);
    static syl_closure _f;
    static syl_int _g·λₒλ1·f·λ(syl_int, syl_env);
    static syl_int _g·λₒλ1(syl_env);
    static syl_int _gₒλ1;
    static syl_int __;
    static syl_int _f·λ(syl_int _x, syl_env 𝒰)
    {
      return 1;
    }
    static syl_int _g·λₒλ1·f·λ(syl_int _x, syl_env 𝒰)
    {
      return 1;
    }
    static syl_int _g·λₒλ1(syl_env 𝒰)
    {
      syl_closure _g·λₒλ1·f;
      {
        syl_env _g·λₒλ1·f·env = NULL;
        _g·λₒλ1·f = syl_mk_closure(_g·λₒλ1·f·λ, _g·λₒλ1·f·env);
      }
      return 1;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _f = syl_mk_closure(_f·λ, _f·env);
      }
      {
        syl_env _g·env = NULL;
        _gₒλ1 = syl_mk_thunk(_g·λₒλ1, _g·env);
      }
      __ = syl_app_thunk(_gₒλ1);
      return 0;
    }
    |}]
;;

let%expect_test "closure nest" =
  go
    {|
let f1 = (fn (x : int) -> 1) @ erased;;
let g = fn (static erased f2 : int -> int) -> f2 0;;
let _ = g f1;;
|};
  [%expect
    {|
    static syl_int _g·λₒλ1(syl_env);
    static syl_int _gₒλ1;
    static syl_int __;
    static syl_int _g·λₒλ1(syl_env 𝒰)
    {
      syl_int _g·λₒλ1·x = 0;
      return 1;
    }
    int main()
    {
      {
        syl_env _g·env = NULL;
        _gₒλ1 = syl_mk_thunk(_g·λₒλ1, _g·env);
      }
      __ = syl_app_thunk(_gₒλ1);
      return 0;
    }
    |}]
;;

let%expect_test "inlined closure nest" =
  go
    {|
let f1 = fn (erased x : int) -> 1;;
let g = fn (f2 : int -> int) -> f2 0;;
let _ = g f1;;
|};
  [%expect
    {|
    static syl_int _f1·λ(syl_int, syl_env);
    static syl_closure _f1;
    static syl_int _g·λ(syl_closure, syl_env);
    static syl_closure _g;
    static syl_int __;
    static syl_int _f1·λ(syl_int _x, syl_env 𝒰)
    {
      return 1;
    }
    static syl_int _g·λ(syl_closure _f2, syl_env 𝒰)
    {
      syl_int _$ = 0;
      return syl_app_closure(_f2, _$);
    }
    int main()
    {
      {
        syl_env _f1·env = NULL;
        _f1 = syl_mk_closure(_f1·λ, _f1·env);
      }
      {
        syl_env _g·env = NULL;
        _g = syl_mk_closure(_g·λ, _g·env);
      }
      __ = syl_app_closure(_g, _f1);
      return 0;
    }
    |}]
;;

let%expect_test "inlined closure nest" =
  go
    {|
let f1 = fn (erased x : int) -> 1;;
let g = fn (static f2 : erased int -> int) -> f2 0;;
let _ = g f1;;
|};
  [%expect
    {|
    static syl_int _f1·λ(syl_int, syl_env);
    static syl_closure _f1;
    static syl_int _g·λₒλ1·f2·λ(syl_int, syl_env);
    static syl_int _g·λₒλ1(syl_env);
    static syl_int _gₒλ1;
    static syl_int __;
    static syl_int _f1·λ(syl_int _x, syl_env 𝒰)
    {
      return 1;
    }
    static syl_int _g·λₒλ1·f2·λ(syl_int _x, syl_env 𝒰)
    {
      return 1;
    }
    static syl_int _g·λₒλ1(syl_env 𝒰)
    {
      syl_closure _g·λₒλ1·f2;
      {
        syl_env _g·λₒλ1·f2·env = NULL;
        _g·λₒλ1·f2 = syl_mk_closure(_g·λₒλ1·f2·λ, _g·λₒλ1·f2·env);
      }
      syl_int _$ = 0;
      return syl_app_closure(_g·λₒλ1·f2, _$);
    }
    int main()
    {
      {
        syl_env _f1·env = NULL;
        _f1 = syl_mk_closure(_f1·λ, _f1·env);
      }
      {
        syl_env _g·env = NULL;
        _gₒλ1 = syl_mk_thunk(_g·λₒλ1, _g·env);
      }
      __ = syl_app_thunk(_gₒλ1);
      return 0;
    }
    |}]
;;

let%expect_test "inlined closure nest" =
  go
    {|
let f1 = (fn (erased x : int) -> 1) @ erased;;
let g = fn (static erased f2 : erased int -> int) -> f2 0;;
let _ = g f1;;
|};
  [%expect
    {|
    static syl_int _g·λₒλ1(syl_env);
    static syl_int _gₒλ1;
    static syl_int __;
    static syl_int _g·λₒλ1(syl_env 𝒰)
    {
      return 1;
    }
    int main()
    {
      {
        syl_env _g·env = NULL;
        _gₒλ1 = syl_mk_thunk(_g·λₒλ1, _g·env);
      }
      __ = syl_app_thunk(_gₒλ1);
      return 0;
    }
    |}]
;;

let%expect_test "inlined closure nest" =
  go
    {|
let f1 = fn (erased x : int) -> 1;;
let g = fn (static erased f2 : erased int -> int) -> f2 0;;
let _ = g f1;;
|};
  [%expect
    {|
    static syl_int _f1·λ(syl_int, syl_env);
    static syl_closure _f1;
    static syl_int _g·λₒλ1(syl_env);
    static syl_int _gₒλ1;
    static syl_int __;
    static syl_int _f1·λ(syl_int _x, syl_env 𝒰)
    {
      return 1;
    }
    static syl_int _g·λₒλ1(syl_env 𝒰)
    {
      return 1;
    }
    int main()
    {
      {
        syl_env _f1·env = NULL;
        _f1 = syl_mk_closure(_f1·λ, _f1·env);
      }
      {
        syl_env _g·env = NULL;
        _gₒλ1 = syl_mk_thunk(_g·λₒλ1, _g·env);
      }
      __ = syl_app_thunk(_gₒλ1);
      return 0;
    }
    |}]
;;

let%expect_test "closure static" =
  go
    {|
let x = (fn (static x : int) -> x) 0;;
|};
  [%expect
    {|
    static syl_int _x;
    int main()
    {
      {
        syl_int _x·x = 0;
        _x = _x·x;
      }
      return 0;
    }
    |}]
;;

let%expect_test "closure static erased" =
  go
    {|
let x = (fn (static erased x : int) -> 1) 0;;
|};
  [%expect
    {|
    static syl_int _x;
    int main()
    {
      _x = 1;
      return 0;
    }
    |}]
;;

let%expect_test "closure return static type" =
  go
    {|
let t = (fn (static x : int) -> int) 0;;
let _ = 0 : t;;
|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      __ = 0;
      return 0;
    }
    |}]
;;

let%expect_test "closure return static type" =
  go
    {|
let t = (fn (static erased x : int) -> int) 0;;
let _ = 0 : t;;
|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      __ = 0;
      return 0;
    }
    |}]
;;

let%expect_test "Apply fn static arg" =
  go
    {|
let _ =
  (fn (x : int) -> x) 1
;;|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      {
        syl_int __·x = 1;
        __ = __·x;
      }
      return 0;
    }
    |}]
;;

let%expect_test "Apply static erased fn static arg" =
  go
    {|
let _ =
  (fn (static erased x : int) -> x) 1
;;|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "Apply fn dynamic arg" =
  go
    {|
let dyn = 1 @ dynamic;;
let _ =
  (fn (x : int) -> x) dyn
;;|};
  [%expect
    {|
    static syl_int _dyn;
    static syl_int __;
    int main()
    {
      _dyn = 1;
      {
        syl_int __·x = _dyn;
        __ = __·x;
      }
      return 0;
    }
    |}]
;;

let%expect_test "Apply erased fn dynamic arg" =
  go
    {|
let dyn = 1 @ dynamic;;
let y =
  (fn (erased x : int) -> 5) (dyn-1)
;;
let _ = y @ unerased;;|};
  [%expect
    {|
    static syl_int _dyn;
    static syl_int _y;
    static syl_int __;
    int main()
    {
      _dyn = 1;
      {
        syl_int _y·x;
        {
          syl_int _$ = 1;
          _y·x = _dyn - _$;
        }
        _y = 5;
      }
      __ = _y;
      return 0;
    }
    |}]
;;

let%expect_test "Apply erased fn dynamic arg" =
  go
    {|
let f = fn (static x : int) -> x @ erased;;
let y =
  (fn (erased x : int) -> 5) (f 1)
;;
let _ = y @ unerased;;|};
  [%expect
    {|
    static syl_int _y;
    static syl_int __;
    int main()
    {
      {
        syl_int _y·x;
        {
          syl_int _y·x·x = 1;
          _y·x = 1;
        }
        _y = 5;
      }
      __ = _y;
      return 0;
    }
    |}]
;;

let%expect_test "Apply erased fn stat arg" =
  go
    {|
let dyn = 1;;
let y =
  (fn (erased x : int) -> 5) (dyn-1)
;;
let _ = y @ unerased;;|};
  [%expect
    {|
    static syl_int _dyn;
    static syl_int _y;
    static syl_int __;
    int main()
    {
      _dyn = 1;
      {
        syl_int _y·x;
        {
          syl_int _$ = 1;
          _y·x = _dyn - _$;
        }
        _y = 5;
      }
      __ = _y;
      return 0;
    }
    |}]
;;

let%expect_test "Apply erased fn stat arg" =
  go
    {|
let dyn = 1;;
let y =
  (fn (static x : int) -> 5) (dyn-1)
;;
let _ = y @ unerased;;|};
  [%expect
    {|
    static syl_int _dyn;
    static syl_int _y;
    static syl_int __;
    int main()
    {
      _dyn = 1;
      {
        syl_int _y·x = 0;
        _y = 5;
      }
      __ = _y;
      return 0;
    }
    |}]
;;

let%expect_test "Apply erased fn stat arg" =
  go
    {|
let dyn = 1;;
let y =
  (fn (static erased x : int) -> 5) (dyn-1)
;;
let _ = y @ unerased;;|};
  [%expect
    {|
    static syl_int _dyn;
    static syl_int _y;
    static syl_int __;
    int main()
    {
      _dyn = 1;
      _y = 5;
      __ = _y;
      return 0;
    }
    |}]
;;

let%expect_test "Apply dynamic fn static arg" =
  go
    {|
let dyn_fn = (fn (x : int) -> x) @ dynamic;;
let _ =
  dyn_fn 1
;;|};
  [%expect
    {|
    static syl_int _dyn_fn·λ(syl_int, syl_env);
    static syl_closure _dyn_fn;
    static syl_int __;
    static syl_int _dyn_fn·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    int main()
    {
      {
        syl_env _dyn_fn·env = NULL;
        _dyn_fn = syl_mk_closure(_dyn_fn·λ, _dyn_fn·env);
      }
      {
        syl_int _$ = 1;
        __ = syl_app_closure(_dyn_fn, _$);
      }
      return 0;
    }
    |}]
;;

let%expect_test "Apply dynamic fn dynamic arg" =
  go
    {|
let dyn_fn = (fn (x : int) -> x) @ dynamic;;
let dyn_arg = 1 @ dynamic;;
let _ =
  dyn_fn dyn_arg
;;|};
  [%expect
    {|
    static syl_int _dyn_fn·λ(syl_int, syl_env);
    static syl_closure _dyn_fn;
    static syl_int _dyn_arg;
    static syl_int __;
    static syl_int _dyn_fn·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    int main()
    {
      {
        syl_env _dyn_fn·env = NULL;
        _dyn_fn = syl_mk_closure(_dyn_fn·λ, _dyn_fn·env);
      }
      _dyn_arg = 1;
      __ = syl_app_closure(_dyn_fn, _dyn_arg);
      return 0;
    }
    |}]
;;

let%expect_test "Lambda dynamic arg" =
  go
    {|
let _ =
  fn (x : int) -> x
;;|};
  [%expect
    {|
    static syl_int __·λ(syl_int, syl_env);
    static syl_closure __;
    static syl_int __·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    int main()
    {
      {
        syl_env __·env = NULL;
        __ = syl_mk_closure(__·λ, __·env);
      }
      return 0;
    }
    |}]
;;

let%expect_test "Lambda static arg" =
  go
    {|
let _ =
  fn (static x : int) -> 1
;;|};
  [%expect
    {|
    int main()
    {
      {
        syl_env __·env = NULL;
      }
      return 0;
    }
    |}]
;;

let%expect_test "Lambda erased arg" =
  go
    {|
let _ =
  fn (erased x : int) -> x
;;|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "Lambda capturing dynamic var" =
  go
    {|
let x = 1 @ dynamic;;
let _ =
  fn (y : int) -> x
;;|};
  [%expect
    {|
    static syl_int _x;
    static syl_int __·λ(syl_int, syl_env);
    static syl_closure __;
    static syl_int __·λ(syl_int _y, syl_env 𝒰)
    {
      syl_int _x = 𝒰[0];
      return _x;
    }
    int main()
    {
      _x = 1;
      {
        syl_env __·env = syl_capture(1, _x);
        __ = syl_mk_closure(__·λ, __·env);
      }
      return 0;
    }
    |}]
;;

let%expect_test "Lambda capturing static var" =
  go
    {|
let x = 1 @ static;;
let _ =
  fn (y : int) -> x
;;|};
  [%expect
    {|
    static syl_int _x;
    static syl_int __·λ(syl_int, syl_env);
    static syl_closure __;
    static syl_int __·λ(syl_int _y, syl_env 𝒰)
    {
      syl_int _x = 𝒰[0];
      return _x;
    }
    int main()
    {
      _x = 1;
      {
        syl_env __·env = syl_capture(1, _x);
        __ = syl_mk_closure(__·λ, __·env);
      }
      return 0;
    }
    |}]
;;

let%expect_test "Lambda capturing erased var" =
  go
    {|
let x = 1 @ static erased;;
let _ =
  fn (y : int) -> x
;;|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "Lambda capturing erased var" =
  go
    {|
let x = 1 @ static erased;;
let _ =
  (fn (y : int) -> x) 0
;;|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "Lambda capturing type" =
  go
    {|
let f = fn (static _ : unit) -> int;;
let g = fn (x : f ()) -> x + 1;;|};
  [%expect
    {|
    static syl_int _g·λ(syl_int, syl_env);
    static syl_closure _g;
    static syl_int _g·λ(syl_int _x, syl_env 𝒰)
    {
      syl_int _$ = 1;
      return _x + _$;
    }
    int main()
    {
      {
        syl_env _g·env = NULL;
        _g = syl_mk_closure(_g·λ, _g·env);
      }
      return 0;
    }
    |}]
;;

let%expect_test "Lambda take type" =
  go
    {|
let f = fn (erased ty : type) -> ty;;
let _ = f int;;
|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "Lambda take type" =
  go
    {|
let f = fn (static erased ty : type) -> ty;;
let _ = 0 : f int;;
|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      __ = 0;
      return 0;
    }
    |}]
;;

let%expect_test "mono fn" =
  go
    {|
let x = fn (static erased x : type) -> x;;
let y = x int;;
|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "mono fn" =
  go
    {|
let x = fn (static x : type) -> x;;
let y = x @ dynamic;;
|};
  [%expect
    {|
    int main()
    {
      {
        syl_env _x·env = NULL;
      }
      return 0;
    }
    |}]
;;

let%expect_test "mono fn" =
  go
    {|
let x = fn (erased x : type) -> x;;
let y = x @ dynamic;;
|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "mono fn" =
  go
    {|
let x = fn (static x : type) -> x;;
let y = x @ unerased;;
|};
  [%expect
    {|
    int main()
    {
      {
        syl_env _x·env = NULL;
      }
      return 0;
    }
    |}]
;;

let%expect_test "mono fn" =
  go
    {|
let x = fn (static erased x : type) -> x;;
let y = (x int) @ dynamic;;
|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "Dependent lambda" =
  go
    {|
let f = fn (static erased ty : type) -> fn (x : ty) -> x;;
|};
  [%expect
    {|
    int main()
    {
      {
        syl_env _f·env = NULL;
      }
      return 0;
    }
    |}]
;;

let%expect_test "static erased lambda" =
  go
    {|
let f = fn (static x : int) -> fn (_ : unit) -> x;;
let g = (f 1 ()) @ unerased;;
|};
  [%expect
    {|
    static syl_int _f·λₒ1·λ(syl_unit, syl_env);
    static syl_closure _f·λₒ1(syl_env);
    static syl_closure _fₒ1;
    static syl_int _g;
    static syl_int _f·λₒ1·λ(syl_unit __, syl_env 𝒰)
    {
      syl_int _x = 𝒰[0];
      return _x;
    }
    static syl_closure _f·λₒ1(syl_env 𝒰)
    {
      syl_int _f·λₒ1·x = 1;
      syl_env _f·λₒ1·env = syl_capture(1, _f·λₒ1·x);
      return syl_mk_closure(_f·λₒ1·λ, _f·λₒ1·env);
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒ1 = syl_mk_thunk(_f·λₒ1, _f·env);
      }
      {
        syl_closure _$ = syl_app_thunk(_fₒ1);
        syl_unit _$ˢ1 = 0;
        _g = syl_app_closure(_$, _$ˢ1);
      }
      return 0;
    }
    |}]
;;

let%expect_test "lift universal type" =
  go
    {|
let f = fn (static ty : type) -> ty @ erased;;
|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "lift universal int" =
  go
    {|
let f = fn (static x : int) -> x @ erased;;
let _ = f 0;;
|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
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
  [%expect
    {|
    static syl_bool _f·λₒ𝔹·λ(syl_bool, syl_env);
    static syl_closure _f·λₒ𝔹(syl_env);
    static syl_int _f·λₒ𝕀·λ(syl_int, syl_env);
    static syl_closure _f·λₒ𝕀(syl_env);
    static syl_closure _fₒ𝔹;
    static syl_closure _fₒ𝕀;
    static syl_closure _g;
    static syl_int __;
    static syl_closure _gˢ1;
    static syl_bool __ˢ1;
    static syl_bool _f·λₒ𝔹·λ(syl_bool _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure _f·λₒ𝔹(syl_env 𝒰)
    {
      syl_env _f·λₒ𝔹·env = NULL;
      return syl_mk_closure(_f·λₒ𝔹·λ, _f·λₒ𝔹·env);
    }
    static syl_int _f·λₒ𝕀·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure _f·λₒ𝕀(syl_env 𝒰)
    {
      syl_env _f·λₒ𝕀·env = NULL;
      return syl_mk_closure(_f·λₒ𝕀·λ, _f·λₒ𝕀·env);
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒ𝔹 = syl_mk_thunk(_f·λₒ𝔹, _f·env);
        _fₒ𝕀 = syl_mk_thunk(_f·λₒ𝕀, _f·env);
      }
      _g = syl_app_thunk(_fₒ𝕀);
      {
        syl_int _$ = 0;
        __ = syl_app_closure(_g, _$);
      }
      _gˢ1 = syl_app_thunk(_fₒ𝔹);
      {
        syl_bool _$ = true;
        __ˢ1 = syl_app_closure(_gˢ1, _$);
      }
      return 0;
    }
    |}]
;;

let%expect_test "Dependent lambda" =
  go
    {|
let f = fn (static g : int -> int) -> g 0;;
let _ = f (fn (x : int) -> x + 1);;
|};
  [%expect
    {|
    static syl_int _f·λₒλ1·g·λ(syl_int, syl_env);
    static syl_int _f·λₒλ1(syl_env);
    static syl_int _fₒλ1;
    static syl_int __;
    static syl_int _f·λₒλ1·g·λ(syl_int _x, syl_env 𝒰)
    {
      syl_int _$ = 1;
      return _x + _$;
    }
    static syl_int _f·λₒλ1(syl_env 𝒰)
    {
      syl_closure _f·λₒλ1·g;
      {
        syl_env _f·λₒλ1·g·env = NULL;
        _f·λₒλ1·g = syl_mk_closure(_f·λₒλ1·g·λ, _f·λₒλ1·g·env);
      }
      syl_int _$ = 0;
      return syl_app_closure(_f·λₒλ1·g, _$);
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒλ1 = syl_mk_thunk(_f·λₒλ1, _f·env);
      }
      __ = syl_app_thunk(_fₒλ1);
      return 0;
    }
    |}]
;;

let%expect_test "dependent unit" =
  go
    {|
let f = fn (static x : unit) -> ();;
let _ = f ();;
|};
  [%expect
    {|
    static syl_unit _f·λₒø(syl_env);
    static syl_unit _fₒø;
    static syl_unit __;
    static syl_unit _f·λₒø(syl_env 𝒰)
    {
      syl_unit _f·λₒø·x = 0;
      return 0;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒø = syl_mk_thunk(_f·λₒø, _f·env);
      }
      __ = syl_app_thunk(_fₒø);
      return 0;
    }
    |}]
;;

let%expect_test "dependent bool" =
  go
    {|
let f = fn (static x : bool) -> !x;;
let _ = f true;;
|};
  [%expect
    {|
    static syl_bool _f·λₒT(syl_env);
    static syl_bool _fₒT;
    static syl_bool __;
    static syl_bool _f·λₒT(syl_env 𝒰)
    {
      syl_bool _f·λₒT·x = true;
      return !_f·λₒT·x;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒT = syl_mk_thunk(_f·λₒT, _f·env);
      }
      __ = syl_app_thunk(_fₒT);
      return 0;
    }
    |}]
;;

let%expect_test "dependent int" =
  go
    {|
let f = fn (static x : int) -> -x;;
let _ = f 1;;
|};
  [%expect
    {|
    static syl_int _f·λₒ1(syl_env);
    static syl_int _fₒ1;
    static syl_int __;
    static syl_int _f·λₒ1(syl_env 𝒰)
    {
      syl_int _f·λₒ1·x = 1;
      return -_f·λₒ1·x;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒ1 = syl_mk_thunk(_f·λₒ1, _f·env);
      }
      __ = syl_app_thunk(_fₒ1);
      return 0;
    }
    |}]
;;

let%expect_test "dependent type" =
  go
    {|
let f = fn (static erased t : type) -> fn (x : t) -> if true then x else x;;
|};
  [%expect
    {|
    int main()
    {
      {
        syl_env _f·env = NULL;
      }
      return 0;
    }
    |}]
;;

let%expect_test "arrow typechecking" =
  go
    {|
let f = fn (g : int -> int) -> g 0;;
let _ = f (fn (x : int) -> 0);;
|};
  [%expect
    {|
    static syl_int _f·λ(syl_closure, syl_env);
    static syl_closure _f;
    static syl_int __·λ(syl_int, syl_env);
    static syl_int __;
    static syl_int _f·λ(syl_closure _g, syl_env 𝒰)
    {
      syl_int _$ = 0;
      return syl_app_closure(_g, _$);
    }
    static syl_int __·λ(syl_int _x, syl_env 𝒰)
    {
      return 0;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _f = syl_mk_closure(_f·λ, _f·env);
      }
      {
        syl_env __·env = NULL;
        syl_closure _$ = syl_mk_closure(__·λ, __·env);
        __ = syl_app_closure(_f, _$);
      }
      return 0;
    }
    |}]
;;

let%expect_test "arrow typechecking" =
  go
    {|
let f = fn (static g : static int -> int) -> g 0;;
let _ = f (fn (static x : int) -> 0);;
|};
  [%expect
    {|
    static syl_int _f·λₒλ4·g·λₒ0(syl_env);
    static syl_int _f·λₒλ4(syl_env);
    static syl_int _fₒλ4;
    static syl_int __;
    static syl_int _f·λₒλ4·g·λₒ0(syl_env 𝒰)
    {
      syl_int _f·λₒλ4·g·λₒ0·x = 0;
      return 0;
    }
    static syl_int _f·λₒλ4(syl_env 𝒰)
    {
      syl_int _f·λₒλ4·gₒ0;
      {
        syl_env _f·λₒλ4·g·env = NULL;
        _f·λₒλ4·gₒ0 = syl_mk_thunk(_f·λₒλ4·g·λₒ0, _f·λₒλ4·g·env);
      }
      return syl_app_thunk(_f·λₒλ4·gₒ0);
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒλ4 = syl_mk_thunk(_f·λₒλ4, _f·env);
      }
      __ = syl_app_thunk(_fₒλ4);
      return 0;
    }
    |}]
;;

let%expect_test "arrow typechecking" =
  go
    {|
let f = fn (static g : static int -> int) -> g 0;;
let _ = f (fn (x : int) -> 0);;
|};
  [%expect
    {|
    static syl_int _f·λₒλ3·g·λ(syl_int, syl_env);
    static syl_int _f·λₒλ3(syl_env);
    static syl_int _fₒλ3;
    static syl_int __;
    static syl_int _f·λₒλ3·g·λ(syl_int _x, syl_env 𝒰)
    {
      return 0;
    }
    static syl_int _f·λₒλ3(syl_env 𝒰)
    {
      syl_closure _f·λₒλ3·g;
      {
        syl_env _f·λₒλ3·g·env = NULL;
        _f·λₒλ3·g = syl_mk_closure(_f·λₒλ3·g·λ, _f·λₒλ3·g·env);
      }
      syl_int _$ = 0;
      return syl_app_closure(_f·λₒλ3·g, _$);
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒλ3 = syl_mk_thunk(_f·λₒλ3, _f·env);
      }
      __ = syl_app_thunk(_fₒλ3);
      return 0;
    }
    |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (static x : int) -> x else fn (x : int) -> x;;
|};
  [%expect
    {|
    int main()
    {
      {
        syl_env __·env = NULL;
      }
      return 0;
    }
    |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (static x : int) -> x else fn (erased x : int) -> x;;
|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (static erased x : int) -> x else fn (x : int) -> x;;
|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (erased x : int) -> x else fn (x : int) -> x;;
|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (x : int) -> x else fn (static x : int) -> x;;
|};
  [%expect
    {|
    static syl_int __·λ(syl_int, syl_env);
    static syl_closure __;
    static syl_int __·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    int main()
    {
      {
        syl_env __·env = NULL;
        __ = syl_mk_closure(__·λ, __·env);
      }
      return 0;
    }
    |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (x : int) -> x else fn (erased x : int) -> x;;
|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (erased x : int) -> x else fn (static x : int) -> x;;
|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "arrow-pi join" =
  go
    {|
let _ = if true then fn (x : int) -> x else fn (static erased x : int) -> x;;
|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "return erased" =
  go
    {|
let f = fn (x : int) -> 0 @ erased;;
let _ = f 1;;
|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "pi typechecking" =
  go
    {|
  let f = fn (static g : erased int -> int) -> g 0;;
  let _ = f (fn (erased x : int) -> 0);;
  |};
  [%expect
    {|
    static syl_int _f·λₒλ1·g·λ(syl_int, syl_env);
    static syl_int _f·λₒλ1(syl_env);
    static syl_int _fₒλ1;
    static syl_int __;
    static syl_int _f·λₒλ1·g·λ(syl_int _x, syl_env 𝒰)
    {
      return 0;
    }
    static syl_int _f·λₒλ1(syl_env 𝒰)
    {
      syl_closure _f·λₒλ1·g;
      {
        syl_env _f·λₒλ1·g·env = NULL;
        _f·λₒλ1·g = syl_mk_closure(_f·λₒλ1·g·λ, _f·λₒλ1·g·env);
      }
      syl_int _$ = 0;
      return syl_app_closure(_f·λₒλ1·g, _$);
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒλ1 = syl_mk_thunk(_f·λₒλ1, _f·env);
      }
      __ = syl_app_thunk(_fₒλ1);
      return 0;
    }
    |}]
;;

let%expect_test "arrow-pi typechecking" =
  go
    {|
let f = fn (static g : static int -> int) -> g 1;;
let _ = f (fn (x : int) -> 0);;
|};
  [%expect
    {|
    static syl_int _f·λₒλ3·g·λ(syl_int, syl_env);
    static syl_int _f·λₒλ3(syl_env);
    static syl_int _fₒλ3;
    static syl_int __;
    static syl_int _f·λₒλ3·g·λ(syl_int _x, syl_env 𝒰)
    {
      return 0;
    }
    static syl_int _f·λₒλ3(syl_env 𝒰)
    {
      syl_closure _f·λₒλ3·g;
      {
        syl_env _f·λₒλ3·g·env = NULL;
        _f·λₒλ3·g = syl_mk_closure(_f·λₒλ3·g·λ, _f·λₒλ3·g·env);
      }
      syl_int _$ = 1;
      return syl_app_closure(_f·λₒλ3·g, _$);
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒλ3 = syl_mk_thunk(_f·λₒλ3, _f·env);
      }
      __ = syl_app_thunk(_fₒλ3);
      return 0;
    }
    |}]
;;

let%expect_test "Pi typechecking" =
  go
    {|
let f = fn (static erased g : static int -> int) -> g 0;;
let _ = f (fn (static x : int) -> x + 1);;
|};
  [%expect
    {|
    static syl_int _f·λₒλ4(syl_env);
    static syl_int _fₒλ4;
    static syl_int __;
    static syl_int _f·λₒλ4(syl_env 𝒰)
    {
      syl_int _f·λₒλ4·x = 0;
      syl_int _$ = 1;
      return _f·λₒλ4·x + _$;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒλ4 = syl_mk_thunk(_f·λₒλ4, _f·env);
      }
      __ = syl_app_thunk(_fₒλ4);
      return 0;
    }
    |}]
;;

let%expect_test "dependent lambda" =
  go
    {|
let f = fn (static g : static erased type -> int -> int) -> g int;;
let _ = f (fn (static erased t : type) -> fn (x : int) -> x);;
|};
  [%expect
    {|
    static syl_int _f·λₒλ4·g·λₒ𝕀·λ(syl_int, syl_env);
    static syl_closure _f·λₒλ4·g·λₒ𝕀(syl_env);
    static syl_closure _f·λₒλ4(syl_env);
    static syl_closure _fₒλ4;
    static syl_closure __;
    static syl_int _f·λₒλ4·g·λₒ𝕀·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure _f·λₒλ4·g·λₒ𝕀(syl_env 𝒰)
    {
      syl_env _f·λₒλ4·g·λₒ𝕀·env = NULL;
      return syl_mk_closure(_f·λₒλ4·g·λₒ𝕀·λ, _f·λₒλ4·g·λₒ𝕀·env);
    }
    static syl_closure _f·λₒλ4(syl_env 𝒰)
    {
      syl_closure _f·λₒλ4·gₒ𝕀;
      {
        syl_env _f·λₒλ4·g·env = NULL;
        _f·λₒλ4·gₒ𝕀 = syl_mk_thunk(_f·λₒλ4·g·λₒ𝕀, _f·λₒλ4·g·env);
      }
      return syl_app_thunk(_f·λₒλ4·gₒ𝕀);
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒλ4 = syl_mk_thunk(_f·λₒλ4, _f·env);
      }
      __ = syl_app_thunk(_fₒλ4);
      return 0;
    }
    |}]
;;

let%expect_test "dependent fn" =
  go
    {|
let id = fn (static erased t : type) -> (fn (x : t) -> x);;
let x = (id int) (0 @ dynamic);;
let y = (id bool) (true @ dynamic);;
|};
  [%expect
    {|
    static syl_bool _id·λₒ𝔹·λ(syl_bool, syl_env);
    static syl_closure _id·λₒ𝔹(syl_env);
    static syl_int _id·λₒ𝕀·λ(syl_int, syl_env);
    static syl_closure _id·λₒ𝕀(syl_env);
    static syl_closure _idₒ𝔹;
    static syl_closure _idₒ𝕀;
    static syl_int _x;
    static syl_bool _y;
    static syl_bool _id·λₒ𝔹·λ(syl_bool _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure _id·λₒ𝔹(syl_env 𝒰)
    {
      syl_env _id·λₒ𝔹·env = NULL;
      return syl_mk_closure(_id·λₒ𝔹·λ, _id·λₒ𝔹·env);
    }
    static syl_int _id·λₒ𝕀·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure _id·λₒ𝕀(syl_env 𝒰)
    {
      syl_env _id·λₒ𝕀·env = NULL;
      return syl_mk_closure(_id·λₒ𝕀·λ, _id·λₒ𝕀·env);
    }
    int main()
    {
      {
        syl_env _id·env = NULL;
        _idₒ𝔹 = syl_mk_thunk(_id·λₒ𝔹, _id·env);
        _idₒ𝕀 = syl_mk_thunk(_id·λₒ𝕀, _id·env);
      }
      {
        syl_closure _$ = syl_app_thunk(_idₒ𝕀);
        syl_int _$ˢ1 = 0;
        _x = syl_app_closure(_$, _$ˢ1);
      }
      {
        syl_closure _$ = syl_app_thunk(_idₒ𝔹);
        syl_bool _$ˢ1 = true;
        _y = syl_app_closure(_$, _$ˢ1);
      }
      return 0;
    }
    |}]
;;

let%expect_test "dependent fn" =
  go
    {|
let id = fn (static erased t : type) -> (fn (x : t) -> x) @ erased;;
let x = (id int) (0 @ dynamic);;
let y = (id bool) (true @ dynamic);;
|};
  [%expect
    {|
    static syl_int _x;
    static syl_bool _y;
    int main()
    {
      {
        syl_int _x·x = 0;
        _x = _x·x;
      }
      {
        syl_bool _y·x = true;
        _y = _y·x;
      }
      return 0;
    }
    |}]
;;

let%expect_test "dependent arrow" =
  go
    {|
let mk_int = fn (static x : int) -> int;;
let apply_int = fn (static f : static int \ x -> mk_int x) -> 2;;
|};
  [%expect
    {|
    int main()
    {
      {
        syl_env _apply_int·env = NULL;
      }
      return 0;
    }
    |}]
;;

let%expect_test "dependent arrow" =
  go
    {|
let mk_int = fn (static x : int) -> int;;
let apply_int = fn (static f : static int \ x -> unit -> mk_int x) -> f 2;;
|};
  [%expect
    {|
    int main()
    {
      {
        syl_env _apply_int·env = NULL;
      }
      return 0;
    }
    |}]
;;

let%expect_test "dependent arrow" =
  go
    {|
let apply_int = fn (static f : static erased type \ t -> t -> t) -> f int;;
let _ = apply_int (fn (static erased t : type) -> fn (x : t) -> x);;
|};
  [%expect
    {|
    static syl_int _apply_int·λₒλ4·f·λₒ𝕀·λ(syl_int, syl_env);
    static syl_closure _apply_int·λₒλ4·f·λₒ𝕀(syl_env);
    static syl_closure _apply_int·λₒλ4(syl_env);
    static syl_closure _apply_intₒλ4;
    static syl_closure __;
    static syl_int _apply_int·λₒλ4·f·λₒ𝕀·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure _apply_int·λₒλ4·f·λₒ𝕀(syl_env 𝒰)
    {
      syl_env _apply_int·λₒλ4·f·λₒ𝕀·env = NULL;
      return syl_mk_closure(_apply_int·λₒλ4·f·λₒ𝕀·λ, _apply_int·λₒλ4·f·λₒ𝕀·env);
    }
    static syl_closure _apply_int·λₒλ4(syl_env 𝒰)
    {
      syl_closure _apply_int·λₒλ4·fₒ𝕀;
      {
        syl_env _apply_int·λₒλ4·f·env = NULL;
        _apply_int·λₒλ4·fₒ𝕀 = syl_mk_thunk(_apply_int·λₒλ4·f·λₒ𝕀, _apply_int·λₒλ4·f·env);
      }
      return syl_app_thunk(_apply_int·λₒλ4·fₒ𝕀);
    }
    int main()
    {
      {
        syl_env _apply_int·env = NULL;
        _apply_intₒλ4 = syl_mk_thunk(_apply_int·λₒλ4, _apply_int·env);
      }
      __ = syl_app_thunk(_apply_intₒλ4);
      return 0;
    }
    |}]
;;

let%expect_test "dependent arrow" =
  go
    {|
let apply = fn (static f : static erased type \ t -> t -> t) -> fn (static erased t2 : type) -> f t2;;
let f = apply (fn (static erased t : type) -> fn (x : t) -> x);;
let g = f int;;
let h = f bool;;
|};
  [%expect
    {|
    static syl_bool _apply·λₒλ7·f·λₒ𝔹·λ(syl_bool, syl_env);
    static syl_closure _apply·λₒλ7·f·λₒ𝔹(syl_env);
    static syl_int _apply·λₒλ7·f·λₒ𝕀·λ(syl_int, syl_env);
    static syl_closure _apply·λₒλ7·f·λₒ𝕀(syl_env);
    static syl_closure _apply·λₒλ7·λₒ𝔹(syl_env);
    static syl_closure _apply·λₒλ7·λₒ𝕀(syl_env);
    static syl_closure _apply·λₒλ7ₒ𝔹(syl_env);
    static syl_closure _apply·λₒλ7ₒ𝕀(syl_env);
    static syl_closure _applyₒλ7ₒ𝔹;
    static syl_closure _applyₒλ7ₒ𝕀;
    static syl_closure _fₒ𝔹;
    static syl_closure _fₒ𝕀;
    static syl_closure _g;
    static syl_closure _h;
    static syl_bool _apply·λₒλ7·f·λₒ𝔹·λ(syl_bool _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure _apply·λₒλ7·f·λₒ𝔹(syl_env 𝒰)
    {
      syl_env _apply·λₒλ7·f·λₒ𝔹·env = NULL;
      return syl_mk_closure(_apply·λₒλ7·f·λₒ𝔹·λ, _apply·λₒλ7·f·λₒ𝔹·env);
    }
    static syl_int _apply·λₒλ7·f·λₒ𝕀·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure _apply·λₒλ7·f·λₒ𝕀(syl_env 𝒰)
    {
      syl_env _apply·λₒλ7·f·λₒ𝕀·env = NULL;
      return syl_mk_closure(_apply·λₒλ7·f·λₒ𝕀·λ, _apply·λₒλ7·f·λₒ𝕀·env);
    }
    static syl_closure _apply·λₒλ7·λₒ𝔹(syl_env 𝒰)
    {
      syl_closure _fₒ𝔹 = 𝒰[1];
      return syl_app_thunk(_fₒ𝔹);
    }
    static syl_closure _apply·λₒλ7·λₒ𝕀(syl_env 𝒰)
    {
      syl_closure _fₒ𝕀 = 𝒰[0];
      return syl_app_thunk(_fₒ𝕀);
    }
    static syl_closure _apply·λₒλ7ₒ𝔹(syl_env 𝒰)
    {
      syl_closure _apply·λₒλ7·fₒ𝔹;
      syl_closure _apply·λₒλ7·fₒ𝕀;
      {
        syl_env _apply·λₒλ7·f·env = NULL;
        _apply·λₒλ7·fₒ𝔹 = syl_mk_thunk(_apply·λₒλ7·f·λₒ𝔹, _apply·λₒλ7·f·env);
        _apply·λₒλ7·fₒ𝕀 = syl_mk_thunk(_apply·λₒλ7·f·λₒ𝕀, _apply·λₒλ7·f·env);
      }
      syl_env _apply·λₒλ7·env = syl_capture(2, _apply·λₒλ7·fₒ𝕀, _apply·λₒλ7·fₒ𝔹);
      return syl_mk_thunk(_apply·λₒλ7·λₒ𝔹, _apply·λₒλ7·env);
    }
    static syl_closure _apply·λₒλ7ₒ𝕀(syl_env 𝒰)
    {
      syl_closure _apply·λₒλ7·fₒ𝔹;
      syl_closure _apply·λₒλ7·fₒ𝕀;
      {
        syl_env _apply·λₒλ7·f·env = NULL;
        _apply·λₒλ7·fₒ𝔹 = syl_mk_thunk(_apply·λₒλ7·f·λₒ𝔹, _apply·λₒλ7·f·env);
        _apply·λₒλ7·fₒ𝕀 = syl_mk_thunk(_apply·λₒλ7·f·λₒ𝕀, _apply·λₒλ7·f·env);
      }
      syl_env _apply·λₒλ7·env = syl_capture(2, _apply·λₒλ7·fₒ𝕀, _apply·λₒλ7·fₒ𝔹);
      return syl_mk_thunk(_apply·λₒλ7·λₒ𝕀, _apply·λₒλ7·env);
    }
    int main()
    {
      {
        syl_env _apply·env = NULL;
        _applyₒλ7ₒ𝔹 = syl_mk_thunk(_apply·λₒλ7ₒ𝔹, _apply·env);
        _applyₒλ7ₒ𝕀 = syl_mk_thunk(_apply·λₒλ7ₒ𝕀, _apply·env);
      }
      _fₒ𝔹 = syl_app_thunk(_applyₒλ7ₒ𝔹);
      _fₒ𝕀 = syl_app_thunk(_applyₒλ7ₒ𝕀);
      _g = syl_app_thunk(_fₒ𝕀);
      _h = syl_app_thunk(_fₒ𝔹);
      return 0;
    }
    |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else true;;
let _ = f 0;;
let _ = f 1;;
|};
  [%expect
    {|
    static syl_bool _f·λₒ1(syl_env);
    static syl_int _f·λₒ0(syl_env);
    static syl_bool _fₒ1;
    static syl_int _fₒ0;
    static syl_int __;
    static syl_bool __ˢ1;
    static syl_bool _f·λₒ1(syl_env 𝒰)
    {
      syl_int _f·λₒ1·x = 1;
      return true;
    }
    static syl_int _f·λₒ0(syl_env 𝒰)
    {
      syl_int _f·λₒ0·x = 0;
      return 1;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒ1 = syl_mk_thunk(_f·λₒ1, _f·env);
        _fₒ0 = syl_mk_thunk(_f·λₒ0, _f·env);
      }
      __ = syl_app_thunk(_fₒ0);
      __ˢ1 = syl_app_thunk(_fₒ1);
      return 0;
    }
    |}]
;;

let%expect_test "dependent if unify" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else true;;
let g = fn (static x : int) -> if static x == 0 then true else 2;;
let _ = if true then f 0 else g 1;;
|};
  [%expect
    {|
    static syl_int _f·λₒ0(syl_env);
    static syl_int _fₒ0;
    static syl_int _g·λₒ1(syl_env);
    static syl_int _gₒ1;
    static syl_int __;
    static syl_int _f·λₒ0(syl_env 𝒰)
    {
      syl_int _f·λₒ0·x = 0;
      return 1;
    }
    static syl_int _g·λₒ1(syl_env 𝒰)
    {
      syl_int _g·λₒ1·x = 1;
      return 2;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒ0 = syl_mk_thunk(_f·λₒ0, _f·env);
      }
      {
        syl_env _g·env = NULL;
        _gₒ1 = syl_mk_thunk(_g·λₒ1, _g·env);
      }
      __ = syl_app_thunk(_fₒ0);
      return 0;
    }
    |}]
;;

let%expect_test "Fun recursive dynamic arg" =
  go
    {|
fun f (x : int) : int = f x;;
|};
  [%expect
    {|
    static syl_int _f·λ(syl_int, syl_env);
    static syl_closure _f;
    static syl_int _f·λ(syl_int _x, syl_env 𝒰)
    {
      syl_closure _f = 𝒰[0];
      return syl_app_closure(_f, _x);
    }
    int main()
    {
      {
        syl_env 𝒰 = syl_env_rec(1);
        _f = syl_mk_closure(_f·λ, 𝒰);
        𝒰[0] = _f;
      }
      return 0;
    }
    |}]
;;

let%expect_test "Fun erased arg" =
  go
    {|
fun f (erased x : int) : erased int = x;;
|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "Fun return static" =
  go
    {|
fun f (x : int) : int = 1;;
|};
  [%expect
    {|
    static syl_int _f·λ(syl_int, syl_env);
    static syl_closure _f;
    static syl_int _f·λ(syl_int _x, syl_env 𝒰)
    {
      return 1;
    }
    int main()
    {
      {
        syl_env 𝒰 = NULL;
        _f = syl_mk_closure(_f·λ, 𝒰);
      }
      return 0;
    }
    |}]
;;

let%expect_test "Fun return erased" =
  go
    {|
fun f (x : int) : erased int = 1 @ erased;;
|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "mono fun" =
  go
    {|
fun x (static x : int) : int = x;;
let y = x 0;;
|};
  [%expect
    {|
    static syl_int _x·λₒ0(syl_env);
    static syl_thunk _xₒ0;
    static syl_int _y;
    static syl_int _x·λₒ0(syl_env 𝒰)
    {
      syl_int _x·λₒ0·x = 0;
      return _x·λₒ0·x;
    }
    int main()
    {
      {
        syl_env 𝒰 = NULL;
        _xₒ0 = syl_mk_thunk(_x·λₒ0, 𝒰);
      }
      _y = syl_app_thunk(_xₒ0);
      return 0;
    }
    |}]
;;

let%expect_test "static type" =
  go
    {|
fun f (static _ : unit) : static erased type = int;;
let y = 0 : f ();;
|};
  [%expect
    {|
    static syl_int _y;
    int main()
    {
      _y = 0;
      return 0;
    }
    |}]
;;

let%expect_test "types are erased" =
  go
    {|
fun f (static _ : unit) : static erased type = int;;
let y = (f () @ dynamic);;
|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "types are erased" =
  go
    {|
fun f (static _ : unit) : static erased type = int;;
let y = 5 : f ();;
|};
  [%expect
    {|
    static syl_int _y;
    int main()
    {
      _y = 5;
      return 0;
    }
    |}]
;;

let%expect_test "dependent fun " =
  go
    {|
fun id (static erased t : type) : t -> t = fn (x : t) -> x;;
let i = id int;;
|};
  [%expect
    {|
    static syl_int _id·λₒ𝕀·λ(syl_int, syl_env);
    static syl_closure _id·λₒ𝕀(syl_env);
    static syl_thunk _idₒ𝕀;
    static syl_closure _i;
    static syl_int _id·λₒ𝕀·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure _id·λₒ𝕀(syl_env 𝒰)
    {
      syl_env _id·λₒ𝕀·env = NULL;
      return syl_mk_closure(_id·λₒ𝕀·λ, _id·λₒ𝕀·env);
    }
    int main()
    {
      {
        syl_env 𝒰 = NULL;
        _idₒ𝕀 = syl_mk_thunk(_id·λₒ𝕀, 𝒰);
      }
      _i = syl_app_thunk(_idₒ𝕀);
      return 0;
    }
    |}]
;;

let%expect_test "erased fun " =
  go
    {|
fun id (erased x : int) : erased int = x;;
let _ = id 0;;
|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "dependent fun erased" =
  go
    {|
fun id (static erased t : type) : t -> t = fn (x : t) -> x;;
let x = (id int) (0 @ dynamic);;
|};
  [%expect
    {|
    static syl_int _id·λₒ𝕀·λ(syl_int, syl_env);
    static syl_closure _id·λₒ𝕀(syl_env);
    static syl_thunk _idₒ𝕀;
    static syl_int _x;
    static syl_int _id·λₒ𝕀·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure _id·λₒ𝕀(syl_env 𝒰)
    {
      syl_env _id·λₒ𝕀·env = NULL;
      return syl_mk_closure(_id·λₒ𝕀·λ, _id·λₒ𝕀·env);
    }
    int main()
    {
      {
        syl_env 𝒰 = NULL;
        _idₒ𝕀 = syl_mk_thunk(_id·λₒ𝕀, 𝒰);
      }
      {
        syl_closure _$ = syl_app_thunk(_idₒ𝕀);
        syl_int _$ˢ1 = 0;
        _x = syl_app_closure(_$, _$ˢ1);
      }
      return 0;
    }
    |}]
;;

let%expect_test "dependent fun" =
  go
    {|
let ty = fn (static _ : unit) -> int -> int;;
fun id (_ : unit) : ty () = fn (x : int) -> x;;
let x = id () 0;;
|};
  [%expect
    {|
    static syl_int _id·λ·λ(syl_int, syl_env);
    static syl_closure _id·λ(syl_unit, syl_env);
    static syl_closure _id;
    static syl_int _x;
    static syl_int _id·λ·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure _id·λ(syl_unit __, syl_env 𝒰)
    {
      syl_env _id·λ·env = NULL;
      return syl_mk_closure(_id·λ·λ, _id·λ·env);
    }
    int main()
    {
      {
        syl_env 𝒰 = NULL;
        _id = syl_mk_closure(_id·λ, 𝒰);
      }
      {
        syl_unit _$ = 0;
        syl_closure _$ˢ1 = syl_app_closure(_id, _$);
        syl_int _$ˢ2 = 0;
        _x = syl_app_closure(_$ˢ1, _$ˢ2);
      }
      return 0;
    }
    |}]
;;

let%expect_test "dependent fun" =
  go
    {|
fun id1 (static erased t : type) : t -> t = fn (x : t) -> x;;
fun id2 (static erased t : type) : t -> t = id1 t;;
let x = id2 int (0 @ dynamic);;
|};
  [%expect
    {|
    static syl_int _id1·λₒ𝕀·λ(syl_int, syl_env);
    static syl_closure _id1·λₒ𝕀(syl_env);
    static syl_thunk _id1ₒ𝕀;
    static syl_closure _id2·λₒ𝕀(syl_env);
    static syl_thunk _id2ₒ𝕀;
    static syl_int _x;
    static syl_int _id1·λₒ𝕀·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure _id1·λₒ𝕀(syl_env 𝒰)
    {
      syl_env _id1·λₒ𝕀·env = NULL;
      return syl_mk_closure(_id1·λₒ𝕀·λ, _id1·λₒ𝕀·env);
    }
    static syl_closure _id2·λₒ𝕀(syl_env 𝒰)
    {
      syl_closure _id1ₒ𝕀 = 𝒰[0];
      return syl_app_thunk(_id1ₒ𝕀);
    }
    int main()
    {
      {
        syl_env 𝒰 = NULL;
        _id1ₒ𝕀 = syl_mk_thunk(_id1·λₒ𝕀, 𝒰);
      }
      {
        syl_env 𝒰 = syl_env_rec(1);
        _id2ₒ𝕀 = syl_mk_thunk(_id2·λₒ𝕀, 𝒰);
        𝒰[0] = _id1ₒ𝕀;
      }
      {
        syl_closure _$ = syl_app_thunk(_id2ₒ𝕀);
        syl_int _$ˢ1 = 0;
        _x = syl_app_closure(_$, _$ˢ1);
      }
      return 0;
    }
    |}]
;;

let%expect_test "join" =
  go
    {|
fun a (_ : unit) : unit = ();;
fun b (_ : unit) : unit -> unit = fn (_ : unit) -> ();;
let x = if static false then a else b;;
let _ = x () ();;
|};
  [%expect
    {|
    static syl_unit _a·λ(syl_unit, syl_env);
    static syl_closure _a;
    static syl_unit _b·λ·λ(syl_unit, syl_env);
    static syl_closure _b·λ(syl_unit, syl_env);
    static syl_closure _b;
    static syl_closure _x;
    static syl_unit __;
    static syl_unit _a·λ(syl_unit __, syl_env 𝒰)
    {
      return 0;
    }
    static syl_unit _b·λ·λ(syl_unit __, syl_env 𝒰)
    {
      return 0;
    }
    static syl_closure _b·λ(syl_unit __, syl_env 𝒰)
    {
      syl_env _b·λ·env = NULL;
      return syl_mk_closure(_b·λ·λ, _b·λ·env);
    }
    int main()
    {
      {
        syl_env 𝒰 = NULL;
        _a = syl_mk_closure(_a·λ, 𝒰);
      }
      {
        syl_env 𝒰 = NULL;
        _b = syl_mk_closure(_b·λ, 𝒰);
      }
      _x = _b;
      {
        syl_unit _$ = 0;
        syl_closure _$ˢ1 = syl_app_closure(_x, _$);
        syl_unit _$ˢ2 = 0;
        __ = syl_app_closure(_$ˢ1, _$ˢ2);
      }
      return 0;
    }
    |}]
;;

let%expect_test "return fn" =
  go
    {|
fun x (_ : unit) : unit -> unit = fn (_ : unit) -> ();;
let _ = x () ();;
|};
  [%expect
    {|
    static syl_unit _x·λ·λ(syl_unit, syl_env);
    static syl_closure _x·λ(syl_unit, syl_env);
    static syl_closure _x;
    static syl_unit __;
    static syl_unit _x·λ·λ(syl_unit __, syl_env 𝒰)
    {
      return 0;
    }
    static syl_closure _x·λ(syl_unit __, syl_env 𝒰)
    {
      syl_env _x·λ·env = NULL;
      return syl_mk_closure(_x·λ·λ, _x·λ·env);
    }
    int main()
    {
      {
        syl_env 𝒰 = NULL;
        _x = syl_mk_closure(_x·λ, 𝒰);
      }
      {
        syl_unit _$ = 0;
        syl_closure _$ˢ1 = syl_app_closure(_x, _$);
        syl_unit _$ˢ2 = 0;
        __ = syl_app_closure(_$ˢ1, _$ˢ2);
      }
      return 0;
    }
    |}]
;;

let%expect_test "arg fn" =
  go
    {|
fun x (f : unit -> int) : int = f ();;
let _ = x (fn (_ : unit) -> 1);;
|};
  [%expect
    {|
    static syl_int _x·λ(syl_closure, syl_env);
    static syl_closure _x;
    static syl_int __·λ(syl_unit, syl_env);
    static syl_int __;
    static syl_int _x·λ(syl_closure _f, syl_env 𝒰)
    {
      syl_unit _$ = 0;
      return syl_app_closure(_f, _$);
    }
    static syl_int __·λ(syl_unit __, syl_env 𝒰)
    {
      return 1;
    }
    int main()
    {
      {
        syl_env 𝒰 = NULL;
        _x = syl_mk_closure(_x·λ, 𝒰);
      }
      {
        syl_env __·env = NULL;
        syl_closure _$ = syl_mk_closure(__·λ, __·env);
        __ = syl_app_closure(_x, _$);
      }
      return 0;
    }
    |}]
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
  [%expect
    {|
    static syl_bool _f·λₒ1(syl_env);
    static syl_int _f·λₒ0(syl_env);
    static syl_bool _fₒ1;
    static syl_int _fₒ0;
    static syl_bool _hₒ1;
    static syl_int _hₒ0;
    static syl_int __;
    static syl_bool __ˢ1;
    static syl_bool _f·λₒ1(syl_env 𝒰)
    {
      return true;
    }
    static syl_int _f·λₒ0(syl_env 𝒰)
    {
      return 1;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒ1 = syl_mk_thunk(_f·λₒ1, _f·env);
        _fₒ0 = syl_mk_thunk(_f·λₒ0, _f·env);
      }
      {
        syl_env _g·env = NULL;
      }
      _hₒ1 = _fₒ1;
      _hₒ0 = _fₒ0;
      __ = syl_app_thunk(_hₒ0);
      __ˢ1 = syl_app_thunk(_hₒ1);
      return 0;
    }
    |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static erased x : int) -> (if static x == 0 then 1 else true) : (if x == 0 then int else bool);;
let _ = f 0;;
let _ = f 1;;
|};
  [%expect
    {|
    static syl_bool _f·λₒ1(syl_env);
    static syl_int _f·λₒ0(syl_env);
    static syl_bool _fₒ1;
    static syl_int _fₒ0;
    static syl_int __;
    static syl_bool __ˢ1;
    static syl_bool _f·λₒ1(syl_env 𝒰)
    {
      return true;
    }
    static syl_int _f·λₒ0(syl_env 𝒰)
    {
      return 1;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒ1 = syl_mk_thunk(_f·λₒ1, _f·env);
        _fₒ0 = syl_mk_thunk(_f·λₒ0, _f·env);
      }
      __ = syl_app_thunk(_fₒ0);
      __ˢ1 = syl_app_thunk(_fₒ1);
      return 0;
    }
    |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static erased x : int) -> (if static x == 1 + 1 then 1 else true) : (if x == 2 then int else bool);;
let _ = f 1;;
let _ = f 2;;
|};
  [%expect
    {|
    static syl_bool _f·λₒ1(syl_env);
    static syl_int _f·λₒ2(syl_env);
    static syl_bool _fₒ1;
    static syl_int _fₒ2;
    static syl_bool __;
    static syl_int __ˢ1;
    static syl_bool _f·λₒ1(syl_env 𝒰)
    {
      return true;
    }
    static syl_int _f·λₒ2(syl_env 𝒰)
    {
      return 1;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒ1 = syl_mk_thunk(_f·λₒ1, _f·env);
        _fₒ2 = syl_mk_thunk(_f·λₒ2, _f·env);
      }
      __ = syl_app_thunk(_fₒ1);
      __ˢ1 = syl_app_thunk(_fₒ2);
      return 0;
    }
    |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static erased x : int) -> (if static x == (if true then x else 0) then 1 else true) : (if x == x then int else bool);;
let _ = f 0;;
|};
  [%expect
    {|
    static syl_int _f·λₒ0(syl_env);
    static syl_int _fₒ0;
    static syl_int __;
    static syl_int _f·λₒ0(syl_env 𝒰)
    {
      return 1;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒ0 = syl_mk_thunk(_f·λₒ0, _f·env);
      }
      __ = syl_app_thunk(_fₒ0);
      return 0;
    }
    |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static erased x : int) -> (if static x == x + 1 then 1 else true) : (if x == x + 1 then int else bool);;
let _ = f 0;;
|};
  [%expect
    {|
    static syl_bool _f·λₒ0(syl_env);
    static syl_bool _fₒ0;
    static syl_bool __;
    static syl_bool _f·λₒ0(syl_env 𝒰)
    {
      return true;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒ0 = syl_mk_thunk(_f·λₒ0, _f·env);
      }
      __ = syl_app_thunk(_fₒ0);
      return 0;
    }
    |}]
;;

let%expect_test "dependent if" =
  go
    {|
let f = fn (static erased x : int) -> (if static x == (if x == 1 then x else 0) then 1 else true) : (if x == (if x == 1 then x else 0) then int else bool);;
let _ = f 0;;
let _ = f 1;;
let _ = f 2;;
|};
  [%expect
    {|
    static syl_int _f·λₒ1(syl_env);
    static syl_bool _f·λₒ2(syl_env);
    static syl_int _f·λₒ0(syl_env);
    static syl_int _fₒ1;
    static syl_bool _fₒ2;
    static syl_int _fₒ0;
    static syl_int __;
    static syl_int __ˢ1;
    static syl_bool __ˢ2;
    static syl_int _f·λₒ1(syl_env 𝒰)
    {
      return 1;
    }
    static syl_bool _f·λₒ2(syl_env 𝒰)
    {
      return true;
    }
    static syl_int _f·λₒ0(syl_env 𝒰)
    {
      return 1;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒ1 = syl_mk_thunk(_f·λₒ1, _f·env);
        _fₒ2 = syl_mk_thunk(_f·λₒ2, _f·env);
        _fₒ0 = syl_mk_thunk(_f·λₒ0, _f·env);
      }
      __ = syl_app_thunk(_fₒ0);
      __ˢ1 = syl_app_thunk(_fₒ1);
      __ˢ2 = syl_app_thunk(_fₒ2);
      return 0;
    }
    |}]
;;

let%expect_test "dependent abstraction" =
  go
    {|
let choose = fn (static f : static int \ x -> if x == 0 then int else bool) -> f 0;;
let _ = choose (fn (static x : int) -> if static x == 0 then 0 else true);;
|};
  [%expect
    {|
    static syl_int _choose·λₒλ4·f·λₒ0(syl_env);
    static syl_int _choose·λₒλ4(syl_env);
    static syl_int _chooseₒλ4;
    static syl_int __;
    static syl_int _choose·λₒλ4·f·λₒ0(syl_env 𝒰)
    {
      syl_int _choose·λₒλ4·f·λₒ0·x = 0;
      return 0;
    }
    static syl_int _choose·λₒλ4(syl_env 𝒰)
    {
      syl_int _choose·λₒλ4·fₒ0;
      {
        syl_env _choose·λₒλ4·f·env = NULL;
        _choose·λₒλ4·fₒ0 = syl_mk_thunk(_choose·λₒλ4·f·λₒ0, _choose·λₒλ4·f·env);
      }
      return syl_app_thunk(_choose·λₒλ4·fₒ0);
    }
    int main()
    {
      {
        syl_env _choose·env = NULL;
        _chooseₒλ4 = syl_mk_thunk(_choose·λₒλ4, _choose·env);
      }
      __ = syl_app_thunk(_chooseₒλ4);
      return 0;
    }
    |}]
;;

let%expect_test "weaken mode: static unerased -> static erased (literal substitution)" =
  go
    {|
let _ = 1 @ erased;;
|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "weaken mode: dynamic unerased -> dynamic erased (erased marker)" =
  go
    {|
let x = 1 @ dynamic;;
let _ = x @ erased;;
|};
  [%expect
    {|
    static syl_int _x;
    int main()
    {
      _x = 1;
      return 0;
    }
    |}]
;;

let%expect_test "weaken mode: static -> dynamic (staticity only)" =
  go
    {|
let _ = (fn (x : int) -> x) @ dynamic;;
|};
  [%expect
    {|
    static syl_int __·λ(syl_int, syl_env);
    static syl_closure __;
    static syl_int __·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    int main()
    {
      {
        syl_env __·env = NULL;
        __ = syl_mk_closure(__·λ, __·env);
      }
      return 0;
    }
    |}]
;;

let%expect_test "weaken type: arrow ret_mode covariant" =
  go
    {|
let f = fn (x : int) -> x;;
let _ = f : int -> erased int;;
|};
  [%expect
    {|
    static syl_int _f·λ(syl_int, syl_env);
    static syl_closure _f;
    static syl_closure __;
    static syl_int _f·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _f = syl_mk_closure(_f·λ, _f·env);
      }
      __ = _f;
      return 0;
    }
    |}]
;;

let%expect_test "weaken type: arrow arg_mode contravariant" =
  go
    {|
let f = fn (erased x : int) -> 1;;
let _ = f : int -> int;;
|};
  [%expect
    {|
    static syl_int _f·λ(syl_int, syl_env);
    static syl_closure _f;
    static syl_closure __;
    static syl_int _f·λ(syl_int _x, syl_env 𝒰)
    {
      return 1;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _f = syl_mk_closure(_f·λ, _f·env);
      }
      __ = _f;
      return 0;
    }
    |}]
;;

let%expect_test "weaken if non-split: mode erasure on branch" =
  go
    {|
let _ = if true then 1 else 1 @ erased;;
|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "weaken if non-split: arrow type join" =
  go
    {|
let _ = if true then fn (erased x : int) -> 1 else fn (x : int) -> 1;;
|};
  [%expect
    {|
    static syl_int __·λ(syl_int, syl_env);
    static syl_closure __;
    static syl_int __·λ(syl_int _x, syl_env 𝒰)
    {
      return 1;
    }
    int main()
    {
      {
        syl_env __·env = NULL;
        __ = syl_mk_closure(__·λ, __·env);
      }
      return 0;
    }
    |}]
;;

let%expect_test "weaken if split: mode only" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else 1 @ erased;;
|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "weaken binder apply: body weakened to ret_mode" =
  go
    {|
let f = fn (static x : int) -> x;;
let _ = f 0;;
|};
  [%expect
    {|
    static syl_int _f·λₒ0(syl_env);
    static syl_int _fₒ0;
    static syl_int __;
    static syl_int _f·λₒ0(syl_env 𝒰)
    {
      syl_int _f·λₒ0·x = 0;
      return _f·λₒ0·x;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒ0 = syl_mk_thunk(_f·λₒ0, _f·env);
      }
      __ = syl_app_thunk(_fₒ0);
      return 0;
    }
    |}]
;;

let%expect_test "weaken arrow closure apply erased: body weakened" =
  go
    {|
let f = fn (x : int) -> x;;
let _ = (f @ erased) 0;;
|};
  [%expect
    {|
    static syl_int _f·λ(syl_int, syl_env);
    static syl_closure _f;
    static syl_int __;
    static syl_int _f·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _f = syl_mk_closure(_f·λ, _f·env);
      }
      {
        syl_int __·x = 0;
        __ = __·x;
      }
      return 0;
    }
    |}]
;;

let%expect_test "weaken pi closure apply erased: both axes" =
  go
    {|
let f = fn (x : int) -> 1;;
let g = fn (static erased x : int) -> x;;
let _ = ((if true then f else g) @ erased) 0;;
|};
  [%expect
    {|
    static syl_int _f·λ(syl_int, syl_env);
    static syl_closure _f;
    static syl_int _f·λ(syl_int _x, syl_env 𝒰)
    {
      return 1;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _f = syl_mk_closure(_f·λ, _f·env);
      }
      return 0;
    }
    |}]
;;

let%expect_test "weaken mode: both axes (static unerased -> dynamic erased)" =
  go
    {|
let _ = 1 @ dynamic erased;;
|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "weaken if non-split: staticity on branch" =
  go
    {|
let x = 1 @ dynamic;;
let _ = if true then 1 else x;;
|};
  [%expect
    {|
    static syl_int _x;
    static syl_int __;
    int main()
    {
      _x = 1;
      __ = 1;
      return 0;
    }
    |}]
;;

let%expect_test "weaken if non-split: both axes on branch" =
  go
    {|
let x = 1 @ dynamic;;
let _ = if true then 1 else x @ erased;;
|};
  [%expect
    {|
    static syl_int _x;
    int main()
    {
      _x = 1;
      return 0;
    }
    |}]
;;

let%expect_test "weaken if split: staticity on branch" =
  go
    {|
let f = fn (static b : bool) -> if static b then 1 @ dynamic else 1;;
let _ = f false;;
|};
  [%expect
    {|
    static syl_int _f·λₒF(syl_env);
    static syl_int _fₒF;
    static syl_int __;
    static syl_int _f·λₒF(syl_env 𝒰)
    {
      syl_bool _f·λₒF·b = false;
      return 1;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒF = syl_mk_thunk(_f·λₒF, _f·env);
      }
      __ = syl_app_thunk(_fₒF);
      return 0;
    }
    |}]
;;

let%expect_test "weaken if split: both axes on branch" =
  go
    {|
let f = fn (static b : bool) -> if static b then 1 @ dynamic erased else 1;;
let _ = f false;;
|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "weaken binder apply: erasure on body" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else 1 @ erased;;
let _ = f 0;;
|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "weaken arrow closure apply erased: staticity on body" =
  go
    {|
let f = fn (x : int) -> 1;;
let _ = (f @ erased) 0;;
|};
  [%expect
    {|
    static syl_int _f·λ(syl_int, syl_env);
    static syl_closure _f;
    static syl_int __;
    static syl_int _f·λ(syl_int _x, syl_env 𝒰)
    {
      return 1;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _f = syl_mk_closure(_f·λ, _f·env);
      }
      {
        syl_int __·x = 0;
        __ = 1;
      }
      return 0;
    }
    |}]
;;

let%expect_test "weaken pi closure apply erased: erasure only" =
  go
    {|
let f = fn (x : int) -> x;;
let g = fn (static erased x : int) -> x;;
let _ = ((if true then f else g) @ erased) 0;;
|};
  [%expect
    {|
    static syl_int _f·λ(syl_int, syl_env);
    static syl_closure _f;
    static syl_int _f·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _f = syl_mk_closure(_f·λ, _f·env);
      }
      return 0;
    }
    |}]
;;

let%expect_test "weaken pi closure apply erased: staticity only" =
  go
    {|
let f = fn (x : int) -> 1;;
let g = fn (static x : int) -> x;;
let _ = ((if true then f else g) @ erased) 0;;
|};
  [%expect
    {|
    static syl_int _f·λ(syl_int, syl_env);
    static syl_closure _f;
    static syl_int __;
    static syl_int _f·λ(syl_int _x, syl_env 𝒰)
    {
      return 1;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _f = syl_mk_closure(_f·λ, _f·env);
      }
      {
        syl_env _g·env = NULL;
      }
      {
        syl_int __·x = 0;
        __ = 1;
      }
      return 0;
    }
    |}]
;;

let%expect_test "closure to closure: arg erasure contravariant" =
  go
    {|
let apply = fn (f : int -> int) -> f 0;;
let g = fn (erased x : int) -> 1;;
let _ = apply g;;
|};
  [%expect
    {|
    static syl_int _apply·λ(syl_closure, syl_env);
    static syl_closure _apply;
    static syl_int _g·λ(syl_int, syl_env);
    static syl_closure _g;
    static syl_int __;
    static syl_int _apply·λ(syl_closure _f, syl_env 𝒰)
    {
      syl_int _$ = 0;
      return syl_app_closure(_f, _$);
    }
    static syl_int _g·λ(syl_int _x, syl_env 𝒰)
    {
      return 1;
    }
    int main()
    {
      {
        syl_env _apply·env = NULL;
        _apply = syl_mk_closure(_apply·λ, _apply·env);
      }
      {
        syl_env _g·env = NULL;
        _g = syl_mk_closure(_g·λ, _g·env);
      }
      __ = syl_app_closure(_apply, _g);
      return 0;
    }
    |}]
;;

let%expect_test "closure to closure: ret erasure covariant" =
  go
    {|
let apply = fn (f : int -> erased int) -> f 0;;
let g = fn (x : int) -> x;;
let _ = apply g;;
|};
  [%expect
    {|
    static syl_int _g·λ(syl_int, syl_env);
    static syl_closure _g;
    static syl_int _g·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    int main()
    {
      {
        syl_env _g·env = NULL;
        _g = syl_mk_closure(_g·λ, _g·env);
      }
      return 0;
    }
    |}]
;;

let%expect_test "closure to closure: both arg and ret subtyping" =
  go
    {|
let apply = fn (f : int -> erased int) -> f 0;;
let g = fn (erased x : int) -> 1;;
let _ = apply g;;
|};
  [%expect
    {|
    static syl_int _g·λ(syl_int, syl_env);
    static syl_closure _g;
    static syl_int _g·λ(syl_int _x, syl_env 𝒰)
    {
      return 1;
    }
    int main()
    {
      {
        syl_env _g·env = NULL;
        _g = syl_mk_closure(_g·λ, _g·env);
      }
      return 0;
    }
    |}]
;;

let%expect_test "closure where binder expected: Arrow leq Pi" =
  go
    {|
let apply = fn (static f : static int -> int) -> f 0;;
let g = fn (x : int) -> x;;
let _ = apply g;;
|};
  [%expect
    {|
    static syl_int _apply·λₒλ3·f·λ(syl_int, syl_env);
    static syl_int _apply·λₒλ3(syl_env);
    static syl_int _applyₒλ3;
    static syl_int _g·λ(syl_int, syl_env);
    static syl_closure _g;
    static syl_int __;
    static syl_int _apply·λₒλ3·f·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_int _apply·λₒλ3(syl_env 𝒰)
    {
      syl_closure _apply·λₒλ3·f;
      {
        syl_env _apply·λₒλ3·f·env = NULL;
        _apply·λₒλ3·f = syl_mk_closure(_apply·λₒλ3·f·λ, _apply·λₒλ3·f·env);
      }
      syl_int _$ = 0;
      return syl_app_closure(_apply·λₒλ3·f, _$);
    }
    static syl_int _g·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    int main()
    {
      {
        syl_env _apply·env = NULL;
        _applyₒλ3 = syl_mk_thunk(_apply·λₒλ3, _apply·env);
      }
      {
        syl_env _g·env = NULL;
        _g = syl_mk_closure(_g·λ, _g·env);
      }
      __ = syl_app_thunk(_applyₒλ3);
      return 0;
    }
    |}]
;;

let%expect_test "binder to binder: arg erasure contravariant" =
  go
    {|
let apply = fn (static f : static int -> int) -> f 0;;
let g = fn (static erased x : int) -> 1;;
let _ = apply g;;
|};
  [%expect
    {|
    static syl_int _apply·λₒλ4·f·λₒ0(syl_env);
    static syl_int _apply·λₒλ4(syl_env);
    static syl_int _applyₒλ4;
    static syl_int _g·λₒ0(syl_env);
    static syl_int _gₒ0;
    static syl_int __;
    static syl_int _apply·λₒλ4·f·λₒ0(syl_env 𝒰)
    {
      syl_int _apply·λₒλ4·f·λₒ0·x = 0;
      return 1;
    }
    static syl_int _apply·λₒλ4(syl_env 𝒰)
    {
      syl_int _apply·λₒλ4·fₒ0;
      {
        syl_env _apply·λₒλ4·f·env = NULL;
        _apply·λₒλ4·fₒ0 = syl_mk_thunk(_apply·λₒλ4·f·λₒ0, _apply·λₒλ4·f·env);
      }
      return syl_app_thunk(_apply·λₒλ4·fₒ0);
    }
    static syl_int _g·λₒ0(syl_env 𝒰)
    {
      syl_int _g·λₒ0·x = 0;
      return 1;
    }
    int main()
    {
      {
        syl_env _apply·env = NULL;
        _applyₒλ4 = syl_mk_thunk(_apply·λₒλ4, _apply·env);
      }
      {
        syl_env _g·env = NULL;
        _gₒ0 = syl_mk_thunk(_g·λₒ0, _g·env);
      }
      __ = syl_app_thunk(_applyₒλ4);
      return 0;
    }
    |}]
;;

let%expect_test "erased closure taking closure arg" =
  go
    {|
let apply = fn (f : int -> int) -> f 0;;
let _ = (apply @ erased) (fn (x : int) -> x);;
|};
  [%expect
    {|
    static syl_int _apply·λ(syl_closure, syl_env);
    static syl_closure _apply;
    static syl_int __·f·λ(syl_int, syl_env);
    static syl_int __;
    static syl_int _apply·λ(syl_closure _f, syl_env 𝒰)
    {
      syl_int _$ = 0;
      return syl_app_closure(_f, _$);
    }
    static syl_int __·f·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    int main()
    {
      {
        syl_env _apply·env = NULL;
        _apply = syl_mk_closure(_apply·λ, _apply·env);
      }
      {
        syl_closure __·f;
        {
          syl_env __·f·env = NULL;
          __·f = syl_mk_closure(__·f·λ, __·f·env);
        }
        syl_int _$ = 0;
        __ = syl_app_closure(__·f, _$);
      }
      return 0;
    }
    |}]
;;

let%expect_test "binder taking closure, applied erased inside" =
  go
    {|
let apply = fn (static f : static int -> erased int) -> (f @ erased) 0;;
let g = fn (x : int) -> 1;;
let _ = apply g;;
|};
  [%expect
    {|
    static syl_int _g·λ(syl_int, syl_env);
    static syl_closure _g;
    static syl_int _g·λ(syl_int _x, syl_env 𝒰)
    {
      return 1;
    }
    int main()
    {
      {
        syl_env _g·env = NULL;
        _g = syl_mk_closure(_g·λ, _g·env);
      }
      return 0;
    }
    |}]
;;

let%expect_test "static lambda identity returns dependent type" =
  go
    {|
fun f (static x : int) : int = x;;
let _ = f 42;;
let _ = f 69;;
|};
  [%expect
    {|
    static syl_int _f·λₒ69(syl_env);
    static syl_int _f·λₒ42(syl_env);
    static syl_thunk _fₒ69;
    static syl_thunk _fₒ42;
    static syl_int __;
    static syl_int __ˢ1;
    static syl_int _f·λₒ69(syl_env 𝒰)
    {
      syl_int _f·λₒ69·x = 69;
      return _f·λₒ69·x;
    }
    static syl_int _f·λₒ42(syl_env 𝒰)
    {
      syl_int _f·λₒ42·x = 42;
      return _f·λₒ42·x;
    }
    int main()
    {
      {
        syl_env 𝒰 = NULL;
        _fₒ69 = syl_mk_thunk(_f·λₒ69, 𝒰);
        _fₒ42 = syl_mk_thunk(_f·λₒ42, 𝒰);
      }
      __ = syl_app_thunk(_fₒ42);
      __ˢ1 = syl_app_thunk(_fₒ69);
      return 0;
    }
    |}]
;;

let%expect_test "static lambda identity returns dependent type" =
  go
    {|
let f = fn (static x : int) -> x;;
let _ = f 42;;
let _ = f 69;;
|};
  [%expect
    {|
    static syl_int _f·λₒ69(syl_env);
    static syl_int _f·λₒ42(syl_env);
    static syl_int _fₒ69;
    static syl_int _fₒ42;
    static syl_int __;
    static syl_int __ˢ1;
    static syl_int _f·λₒ69(syl_env 𝒰)
    {
      syl_int _f·λₒ69·x = 69;
      return _f·λₒ69·x;
    }
    static syl_int _f·λₒ42(syl_env 𝒰)
    {
      syl_int _f·λₒ42·x = 42;
      return _f·λₒ42·x;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒ69 = syl_mk_thunk(_f·λₒ69, _f·env);
        _fₒ42 = syl_mk_thunk(_f·λₒ42, _f·env);
      }
      __ = syl_app_thunk(_fₒ42);
      __ˢ1 = syl_app_thunk(_fₒ69);
      return 0;
    }
    |}]
;;

let%expect_test "static lambda with arithmetic on static arg" =
  go
    {|
let f = fn (static x : int) -> x + 1;;
let _ = f 10;;
|};
  [%expect
    {|
    static syl_int _f·λₒ10(syl_env);
    static syl_int _fₒ10;
    static syl_int __;
    static syl_int _f·λₒ10(syl_env 𝒰)
    {
      syl_int _f·λₒ10·x = 10;
      syl_int _$ = 1;
      return _f·λₒ10·x + _$;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒ10 = syl_mk_thunk(_f·λₒ10, _f·env);
      }
      __ = syl_app_thunk(_fₒ10);
      return 0;
    }
    |}]
;;

let%expect_test "static lambda with capture" =
  go
    {|
let y = 1;;
let f = fn (static x : int) -> x + y;;
let _ = f 10;;
|};
  [%expect
    {|
    static syl_int _y;
    static syl_int _f·λₒ10(syl_env);
    static syl_int _fₒ10;
    static syl_int __;
    static syl_int _f·λₒ10(syl_env 𝒰)
    {
      syl_int _y = 𝒰[0];
      syl_int _f·λₒ10·x = 10;
      return _f·λₒ10·x + _y;
    }
    int main()
    {
      _y = 1;
      {
        syl_env _f·env = syl_capture(1, _y);
        _fₒ10 = syl_mk_thunk(_f·λₒ10, _f·env);
      }
      __ = syl_app_thunk(_fₒ10);
      return 0;
    }
    |}]
;;

let%expect_test "static lambda with dynamic capture" =
  go
    {|
let y = 1 @ dynamic;;
let f = fn (static x : int) -> x + y;;
let _ = f 10;;
|};
  [%expect
    {|
    static syl_int _y;
    static syl_int _f·λₒ10(syl_env);
    static syl_int _fₒ10;
    static syl_int __;
    static syl_int _f·λₒ10(syl_env 𝒰)
    {
      syl_int _y = 𝒰[0];
      syl_int _f·λₒ10·x = 10;
      return _f·λₒ10·x + _y;
    }
    int main()
    {
      _y = 1;
      {
        syl_env _f·env = syl_capture(1, _y);
        _fₒ10 = syl_mk_thunk(_f·λₒ10, _f·env);
      }
      __ = syl_app_thunk(_fₒ10);
      return 0;
    }
    |}]
;;

let%expect_test "static lambda with capture" =
  go
    {|
let _ =
let y = 1 in
let f = fn (static x : int) -> x + y in
f 10;;
|};
  [%expect
    {|
    static syl_int __·f·λₒ10(syl_env);
    static syl_int __;
    static syl_int __·f·λₒ10(syl_env 𝒰)
    {
      syl_int _y = 𝒰[0];
      syl_int __·f·λₒ10·x = 10;
      return __·f·λₒ10·x + _y;
    }
    int main()
    {
      {
        syl_int __·y = 1;
        syl_int __·fₒ10;
        {
          syl_env __·f·env = syl_capture(1, __·y);
          __·fₒ10 = syl_mk_thunk(__·f·λₒ10, __·f·env);
        }
        __ = syl_app_thunk(__·fₒ10);
      }
      return 0;
    }
    |}]
;;

let%expect_test "static lambda with boolean op on static arg" =
  go
    {|
let f = fn (static x : bool) -> x && true;;
let _ = f false;;
|};
  [%expect
    {|
    static syl_bool _f·λₒF(syl_env);
    static syl_bool _fₒF;
    static syl_bool __;
    static syl_bool _f·λₒF(syl_env 𝒰)
    {
      syl_bool _f·λₒF·x = false;
      syl_bool _$ = true;
      return _f·λₒF·x && _$;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒF = syl_mk_thunk(_f·λₒF, _f·env);
      }
      __ = syl_app_thunk(_fₒF);
      return 0;
    }
    |}]
;;

let%expect_test "nested static lambdas" =
  go
    {|
let f = fn (static x : int) -> fn (static y : int) -> x + y;;
let _ = f 1 2;;
let _ = f 1 3;;
|};
  [%expect
    {|
    static syl_int _f·λₒ1·λₒ2(syl_env);
    static syl_int _f·λₒ1·λₒ3(syl_env);
    static syl_int _f·λₒ1ₒ2(syl_env);
    static syl_int _f·λₒ1ₒ3(syl_env);
    static syl_int _fₒ1ₒ2;
    static syl_int _fₒ1ₒ3;
    static syl_int __;
    static syl_int __ˢ1;
    static syl_int _f·λₒ1·λₒ2(syl_env 𝒰)
    {
      syl_int _x = 𝒰[0];
      syl_int _f·λₒ1·λₒ2·y = 2;
      return _x + _f·λₒ1·λₒ2·y;
    }
    static syl_int _f·λₒ1·λₒ3(syl_env 𝒰)
    {
      syl_int _x = 𝒰[0];
      syl_int _f·λₒ1·λₒ3·y = 3;
      return _x + _f·λₒ1·λₒ3·y;
    }
    static syl_int _f·λₒ1ₒ2(syl_env 𝒰)
    {
      syl_int _f·λₒ1·x = 1;
      syl_env _f·λₒ1·env = syl_capture(1, _f·λₒ1·x);
      return syl_mk_thunk(_f·λₒ1·λₒ2, _f·λₒ1·env);
    }
    static syl_int _f·λₒ1ₒ3(syl_env 𝒰)
    {
      syl_int _f·λₒ1·x = 1;
      syl_env _f·λₒ1·env = syl_capture(1, _f·λₒ1·x);
      return syl_mk_thunk(_f·λₒ1·λₒ3, _f·λₒ1·env);
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒ1ₒ2 = syl_mk_thunk(_f·λₒ1ₒ2, _f·env);
        _fₒ1ₒ3 = syl_mk_thunk(_f·λₒ1ₒ3, _f·env);
      }
      {
        syl_int _$ₒ2 = syl_app_thunk(_fₒ1ₒ2);
        __ = syl_app_thunk(_$ₒ2);
      }
      {
        syl_int _$ₒ3 = syl_app_thunk(_fₒ1ₒ3);
        __ˢ1 = syl_app_thunk(_$ₒ3);
      }
      return 0;
    }
    |}]
;;

let%expect_test "static lambda returning static lambda" =
  go
    {|
let f = fn (static x : int) -> fn (static y : int) -> x;;
let g = f 1;;
let _ = g 2;;
|};
  [%expect
    {|
    static syl_int _f·λₒ1·λₒ2(syl_env);
    static syl_int _f·λₒ1ₒ2(syl_env);
    static syl_int _fₒ1ₒ2;
    static syl_int _gₒ2;
    static syl_int __;
    static syl_int _f·λₒ1·λₒ2(syl_env 𝒰)
    {
      syl_int _x = 𝒰[0];
      syl_int _f·λₒ1·λₒ2·y = 2;
      return _x;
    }
    static syl_int _f·λₒ1ₒ2(syl_env 𝒰)
    {
      syl_int _f·λₒ1·x = 1;
      syl_env _f·λₒ1·env = syl_capture(1, _f·λₒ1·x);
      return syl_mk_thunk(_f·λₒ1·λₒ2, _f·λₒ1·env);
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒ1ₒ2 = syl_mk_thunk(_f·λₒ1ₒ2, _f·env);
      }
      _gₒ2 = syl_app_thunk(_fₒ1ₒ2);
      __ = syl_app_thunk(_gₒ2);
      return 0;
    }
    |}]
;;

let%expect_test "static lambda mixed with dynamic lambda" =
  go
    {|
let f = fn (static x : int) -> fn (y : int) -> y;;
let g = f 1;;
let _ = g 2;;
|};
  [%expect
    {|
    static syl_int _f·λₒ1·λ(syl_int, syl_env);
    static syl_closure _f·λₒ1(syl_env);
    static syl_closure _fₒ1;
    static syl_closure _g;
    static syl_int __;
    static syl_int _f·λₒ1·λ(syl_int _y, syl_env 𝒰)
    {
      return _y;
    }
    static syl_closure _f·λₒ1(syl_env 𝒰)
    {
      syl_int _f·λₒ1·x = 1;
      syl_env _f·λₒ1·env = NULL;
      return syl_mk_closure(_f·λₒ1·λ, _f·λₒ1·env);
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒ1 = syl_mk_thunk(_f·λₒ1, _f·env);
      }
      _g = syl_app_thunk(_fₒ1);
      {
        syl_int _$ = 2;
        __ = syl_app_closure(_g, _$);
      }
      return 0;
    }
    |}]
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
  [%expect
    {|
    static syl_bool _f·λₒ𝔹·λ(syl_bool, syl_env);
    static syl_closure _f·λₒ𝔹(syl_env);
    static syl_int _f·λₒ𝕀·λ(syl_int, syl_env);
    static syl_closure _f·λₒ𝕀(syl_env);
    static syl_closure _fₒ𝔹;
    static syl_closure _fₒ𝕀;
    static syl_closure _g;
    static syl_int __;
    static syl_closure _h;
    static syl_bool __ˢ1;
    static syl_bool _f·λₒ𝔹·λ(syl_bool _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure _f·λₒ𝔹(syl_env 𝒰)
    {
      syl_env _f·λₒ𝔹·env = NULL;
      return syl_mk_closure(_f·λₒ𝔹·λ, _f·λₒ𝔹·env);
    }
    static syl_int _f·λₒ𝕀·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure _f·λₒ𝕀(syl_env 𝒰)
    {
      syl_env _f·λₒ𝕀·env = NULL;
      return syl_mk_closure(_f·λₒ𝕀·λ, _f·λₒ𝕀·env);
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒ𝔹 = syl_mk_thunk(_f·λₒ𝔹, _f·env);
        _fₒ𝕀 = syl_mk_thunk(_f·λₒ𝕀, _f·env);
      }
      _g = syl_app_thunk(_fₒ𝕀);
      {
        syl_int _$ = 42;
        __ = syl_app_closure(_g, _$);
      }
      _h = syl_app_thunk(_fₒ𝔹);
      {
        syl_bool _$ = true;
        __ˢ1 = syl_app_closure(_h, _$);
      }
      return 0;
    }
    |}]
;;

let%expect_test "if static with literal condition true" =
  go
    {|
let _ = if static true then 1 else true;;
|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      __ = 1;
      return 0;
    }
    |}]
;;

let%expect_test "if static with literal condition false" =
  go
    {|
let _ = if static false then 1 else true;;
|};
  [%expect
    {|
    static syl_bool __;
    int main()
    {
      __ = true;
      return 0;
    }
    |}]
;;

let%expect_test "if static with static variable condition" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else true;;
let a = f 0;;
let b = f 1;;
|};
  [%expect
    {|
    static syl_bool _f·λₒ1(syl_env);
    static syl_int _f·λₒ0(syl_env);
    static syl_bool _fₒ1;
    static syl_int _fₒ0;
    static syl_int _a;
    static syl_bool _b;
    static syl_bool _f·λₒ1(syl_env 𝒰)
    {
      syl_int _f·λₒ1·x = 1;
      return true;
    }
    static syl_int _f·λₒ0(syl_env 𝒰)
    {
      syl_int _f·λₒ0·x = 0;
      return 1;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒ1 = syl_mk_thunk(_f·λₒ1, _f·env);
        _fₒ0 = syl_mk_thunk(_f·λₒ0, _f·env);
      }
      _a = syl_app_thunk(_fₒ0);
      _b = syl_app_thunk(_fₒ1);
      return 0;
    }
    |}]
;;

let%expect_test "if static with mismatched branch types without annotation" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else true;;
|};
  [%expect
    {|
    int main()
    {
      {
        syl_env _f·env = NULL;
      }
      return 0;
    }
    |}]
;;

let%expect_test "if static with correct type annotation using non-static if" =
  go
    {|
let f = fn (static x : int) -> (if static x == 0 then 1 else true) : (if x == 0 then int else bool);;
|};
  [%expect
    {|
    int main()
    {
      {
        syl_env _f·env = NULL;
      }
      return 0;
    }
    |}]
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
  [%expect
    {|
    static syl_bool _f·λₒ1·λ(syl_bool, syl_env);
    static syl_closure _f·λₒ1(syl_env);
    static syl_int _f·λₒ0·λ(syl_int, syl_env);
    static syl_closure _f·λₒ0(syl_env);
    static syl_closure _fₒ1;
    static syl_closure _fₒ0;
    static syl_closure _g;
    static syl_int __;
    static syl_closure _h;
    static syl_bool __ˢ1;
    static syl_bool _f·λₒ1·λ(syl_bool _y, syl_env 𝒰)
    {
      return _y;
    }
    static syl_closure _f·λₒ1(syl_env 𝒰)
    {
      syl_int _f·λₒ1·x = 1;
      syl_env _f·λₒ1·env = NULL;
      return syl_mk_closure(_f·λₒ1·λ, _f·λₒ1·env);
    }
    static syl_int _f·λₒ0·λ(syl_int _y, syl_env 𝒰)
    {
      return _y;
    }
    static syl_closure _f·λₒ0(syl_env 𝒰)
    {
      syl_int _f·λₒ0·x = 0;
      syl_env _f·λₒ0·env = NULL;
      return syl_mk_closure(_f·λₒ0·λ, _f·λₒ0·env);
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒ1 = syl_mk_thunk(_f·λₒ1, _f·env);
        _fₒ0 = syl_mk_thunk(_f·λₒ0, _f·env);
      }
      _g = syl_app_thunk(_fₒ0);
      {
        syl_int _$ = 42;
        __ = syl_app_closure(_g, _$);
      }
      _h = syl_app_thunk(_fₒ1);
      {
        syl_bool _$ = true;
        __ˢ1 = syl_app_closure(_h, _$);
      }
      return 0;
    }
    |}]
;;

let%expect_test "if static true selects then branch type" =
  go
    {|
let _ = (if static true then 1 else true) : (if true then int else bool);;
|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      __ = 1;
      return 0;
    }
    |}]
;;

let%expect_test "if static false selects else branch type" =
  go
    {|
let _ = (if static false then 1 else true) : (if false then int else bool);;
|};
  [%expect
    {|
    static syl_bool __;
    int main()
    {
      __ = true;
      return 0;
    }
    |}]
;;

let%expect_test "dependent arrow type with backslash binder" =
  go
    {|
let f = fn (static g : static int \ x -> int) -> g 0;;
|};
  [%expect
    {|
    int main()
    {
      {
        syl_env _f·env = NULL;
      }
      return 0;
    }
    |}]
;;

let%expect_test "dependent arrow applied to matching function" =
  go
    {|
let apply_type = fn (static f : static erased type \ t -> t -> t) -> f int;;
let _ = apply_type (fn (static erased t : type) -> fn (x : t) -> x);;
|};
  [%expect
    {|
    static syl_int _apply_type·λₒλ4·f·λₒ𝕀·λ(syl_int, syl_env);
    static syl_closure _apply_type·λₒλ4·f·λₒ𝕀(syl_env);
    static syl_closure _apply_type·λₒλ4(syl_env);
    static syl_closure _apply_typeₒλ4;
    static syl_closure __;
    static syl_int _apply_type·λₒλ4·f·λₒ𝕀·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure _apply_type·λₒλ4·f·λₒ𝕀(syl_env 𝒰)
    {
      syl_env _apply_type·λₒλ4·f·λₒ𝕀·env = NULL;
      return syl_mk_closure(_apply_type·λₒλ4·f·λₒ𝕀·λ, _apply_type·λₒλ4·f·λₒ𝕀·env);
    }
    static syl_closure _apply_type·λₒλ4(syl_env 𝒰)
    {
      syl_closure _apply_type·λₒλ4·fₒ𝕀;
      {
        syl_env _apply_type·λₒλ4·f·env = NULL;
        _apply_type·λₒλ4·fₒ𝕀 = syl_mk_thunk(_apply_type·λₒλ4·f·λₒ𝕀, _apply_type·λₒλ4·f·env);
      }
      return syl_app_thunk(_apply_type·λₒλ4·fₒ𝕀);
    }
    int main()
    {
      {
        syl_env _apply_type·env = NULL;
        _apply_typeₒλ4 = syl_mk_thunk(_apply_type·λₒλ4, _apply_type·env);
      }
      __ = syl_app_thunk(_apply_typeₒλ4);
      return 0;
    }
    |}]
;;

let%expect_test "dependent arrow with return type depending on arg" =
  go
    {|
let mk_int = fn (static x : int) -> int;;
let f = fn (static g : static int \ x -> mk_int x) -> g 0;;
|};
  [%expect
    {|
    int main()
    {
      {
        syl_env _f·env = NULL;
      }
      return 0;
    }
    |}]
;;

let%expect_test "fun with static arg creates Pi type" =
  go
    {|
fun f (static x : int) : int = x;;
let _ = f 0;;
|};
  [%expect
    {|
    static syl_int _f·λₒ0(syl_env);
    static syl_thunk _fₒ0;
    static syl_int __;
    static syl_int _f·λₒ0(syl_env 𝒰)
    {
      syl_int _f·λₒ0·x = 0;
      return _f·λₒ0·x;
    }
    int main()
    {
      {
        syl_env 𝒰 = NULL;
        _fₒ0 = syl_mk_thunk(_f·λₒ0, 𝒰);
      }
      __ = syl_app_thunk(_fₒ0);
      return 0;
    }
    |}]
;;

let%expect_test "fun with static arg creates Pi type" =
  go
    {|
fun f (static erased x : int) : int = x+0;;
let _ = f 0;;
|};
  [%expect
    {|
    static syl_int _f·λₒ0(syl_env);
    static syl_thunk _fₒ0;
    static syl_int __;
    static syl_int _f·λₒ0(syl_env 𝒰)
    {
      return 0;
    }
    int main()
    {
      {
        syl_env 𝒰 = NULL;
        _fₒ0 = syl_mk_thunk(_f·λₒ0, 𝒰);
      }
      __ = syl_app_thunk(_fₒ0);
      return 0;
    }
    |}]
;;

let%expect_test "fun with static arg creates Pi type" =
  go
    {|
let _ =
  fun f (static x : int) : int = x in
  let _ = f 0
in ();;
|};
  [%expect
    {|
    static syl_int __·f·λₒ0(syl_env);
    static syl_unit __;
    static syl_int __·f·λₒ0(syl_env 𝒰)
    {
      syl_int __·f·λₒ0·x = 0;
      return __·f·λₒ0·x;
    }
    int main()
    {
      {
        syl_thunk __·fₒ0;
        {
          syl_env 𝒰 = NULL;
          __·fₒ0 = syl_mk_thunk(__·f·λₒ0, 𝒰);
        }
        syl_int __·_ = syl_app_thunk(__·fₒ0);
        __ = 0;
      }
      return 0;
    }
    |}]
;;

let%expect_test "fun with static erased type arg — polymorphic identity" =
  go
    {|
fun id (static erased t : type) : t -> t = fn (x : t) -> x;;
let _ = id int 0;;
let _ = id bool true;;
|};
  [%expect
    {|
    static syl_bool _id·λₒ𝔹·λ(syl_bool, syl_env);
    static syl_closure _id·λₒ𝔹(syl_env);
    static syl_int _id·λₒ𝕀·λ(syl_int, syl_env);
    static syl_closure _id·λₒ𝕀(syl_env);
    static syl_thunk _idₒ𝔹;
    static syl_thunk _idₒ𝕀;
    static syl_int __;
    static syl_bool __ˢ1;
    static syl_bool _id·λₒ𝔹·λ(syl_bool _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure _id·λₒ𝔹(syl_env 𝒰)
    {
      syl_env _id·λₒ𝔹·env = NULL;
      return syl_mk_closure(_id·λₒ𝔹·λ, _id·λₒ𝔹·env);
    }
    static syl_int _id·λₒ𝕀·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure _id·λₒ𝕀(syl_env 𝒰)
    {
      syl_env _id·λₒ𝕀·env = NULL;
      return syl_mk_closure(_id·λₒ𝕀·λ, _id·λₒ𝕀·env);
    }
    int main()
    {
      {
        syl_env 𝒰 = NULL;
        _idₒ𝔹 = syl_mk_thunk(_id·λₒ𝔹, 𝒰);
        _idₒ𝕀 = syl_mk_thunk(_id·λₒ𝕀, 𝒰);
      }
      {
        syl_closure _$ = syl_app_thunk(_idₒ𝕀);
        syl_int _$ˢ1 = 0;
        __ = syl_app_closure(_$, _$ˢ1);
      }
      {
        syl_closure _$ = syl_app_thunk(_idₒ𝔹);
        syl_bool _$ˢ1 = true;
        __ˢ1 = syl_app_closure(_$, _$ˢ1);
      }
      return 0;
    }
    |}]
;;

let%expect_test "fun dynamic recursion is allowed" =
  go
    {|
fun f (x : int) : int = f x;;
|};
  [%expect
    {|
    static syl_int _f·λ(syl_int, syl_env);
    static syl_closure _f;
    static syl_int _f·λ(syl_int _x, syl_env 𝒰)
    {
      syl_closure _f = 𝒰[0];
      return syl_app_closure(_f, _x);
    }
    int main()
    {
      {
        syl_env 𝒰 = syl_env_rec(1);
        _f = syl_mk_closure(_f·λ, 𝒰);
        𝒰[0] = _f;
      }
      return 0;
    }
    |}]
;;

let%expect_test "fun with static erased type arg, two sequential funs" =
  go
    {|
fun id1 (static erased t : type) : t -> t = fn (x : t) -> x;;
fun id2 (static erased t : type) : t -> t = id1 t;;
let _ = id2 int 0;;
|};
  [%expect
    {|
    static syl_int _id1·λₒ𝕀·λ(syl_int, syl_env);
    static syl_closure _id1·λₒ𝕀(syl_env);
    static syl_thunk _id1ₒ𝕀;
    static syl_closure _id2·λₒ𝕀(syl_env);
    static syl_thunk _id2ₒ𝕀;
    static syl_int __;
    static syl_int _id1·λₒ𝕀·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure _id1·λₒ𝕀(syl_env 𝒰)
    {
      syl_env _id1·λₒ𝕀·env = NULL;
      return syl_mk_closure(_id1·λₒ𝕀·λ, _id1·λₒ𝕀·env);
    }
    static syl_closure _id2·λₒ𝕀(syl_env 𝒰)
    {
      syl_closure _id1ₒ𝕀 = 𝒰[0];
      return syl_app_thunk(_id1ₒ𝕀);
    }
    int main()
    {
      {
        syl_env 𝒰 = NULL;
        _id1ₒ𝕀 = syl_mk_thunk(_id1·λₒ𝕀, 𝒰);
      }
      {
        syl_env 𝒰 = syl_env_rec(1);
        _id2ₒ𝕀 = syl_mk_thunk(_id2·λₒ𝕀, 𝒰);
        𝒰[0] = _id1ₒ𝕀;
      }
      {
        syl_closure _$ = syl_app_thunk(_id2ₒ𝕀);
        syl_int _$ˢ1 = 0;
        __ = syl_app_closure(_$, _$ˢ1);
      }
      return 0;
    }
    |}]
;;

let%expect_test "static erased lambda captures no runtime value" =
  go
    {|
let f = fn (static erased x : int) -> 0;;
let _ = f 1;;
|};
  [%expect
    {|
    static syl_int _f·λₒ1(syl_env);
    static syl_int _fₒ1;
    static syl_int __;
    static syl_int _f·λₒ1(syl_env 𝒰)
    {
      return 0;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒ1 = syl_mk_thunk(_f·λₒ1, _f·env);
      }
      __ = syl_app_thunk(_fₒ1);
      return 0;
    }
    |}]
;;

let%expect_test "lift static value through Pi" =
  go
    {|
let f = fn (static x : int) -> x @ erased;;
let _ = f 0;;
|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "fun returning static erased type" =
  go
    {|
fun f (static _ : unit) : static erased type = int;;
let _ = 5 : f ();;
|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      __ = 5;
      return 0;
    }
    |}]
;;

let%expect_test "pi and arrow join — if choosing between Pi and Arrow" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else true;;
let g = fn (static x : int) -> if static x == 0 then 1 else true;;
let _ = if true then f else g;;
|};
  [%expect
    {|
    int main()
    {
      {
        syl_env _f·env = NULL;
      }
      {
        syl_env _g·env = NULL;
      }
      return 0;
    }
    |}]
;;

let%expect_test "joining f 0 and g 1 resolves dependent types" =
  go
    {|
let f = fn (static x : int) -> if static x == 0 then 1 else true;;
let g = fn (static x : int) -> if static x == 0 then true else 2;;
let _ = if true then f 0 else g 1;;
|};
  [%expect
    {|
    static syl_int _f·λₒ0(syl_env);
    static syl_int _fₒ0;
    static syl_int _g·λₒ1(syl_env);
    static syl_int _gₒ1;
    static syl_int __;
    static syl_int _f·λₒ0(syl_env 𝒰)
    {
      syl_int _f·λₒ0·x = 0;
      return 1;
    }
    static syl_int _g·λₒ1(syl_env 𝒰)
    {
      syl_int _g·λₒ1·x = 1;
      return 2;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒ0 = syl_mk_thunk(_f·λₒ0, _f·env);
      }
      {
        syl_env _g·env = NULL;
        _gₒ1 = syl_mk_thunk(_g·λₒ1, _g·env);
      }
      __ = syl_app_thunk(_fₒ0);
      return 0;
    }
    |}]
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
  [%expect
    {|
    static syl_int _f·λₒ1·λₒ1(syl_env);
    static syl_unit _f·λₒ1·λₒ0(syl_env);
    static syl_int _f·λₒ1ₒ1(syl_env);
    static syl_unit _f·λₒ1ₒ0(syl_env);
    static syl_bool _f·λₒ0·λₒ1(syl_env);
    static syl_int _f·λₒ0·λₒ0(syl_env);
    static syl_bool _f·λₒ0ₒ1(syl_env);
    static syl_int _f·λₒ0ₒ0(syl_env);
    static syl_int _fₒ1ₒ1;
    static syl_unit _fₒ1ₒ0;
    static syl_bool _fₒ0ₒ1;
    static syl_int _fₒ0ₒ0;
    static syl_int __;
    static syl_bool __ˢ1;
    static syl_unit __ˢ2;
    static syl_int __ˢ3;
    static syl_int _f·λₒ1·λₒ1(syl_env 𝒰)
    {
      syl_int _f·λₒ1·λₒ1·y = 1;
      return 2;
    }
    static syl_unit _f·λₒ1·λₒ0(syl_env 𝒰)
    {
      syl_int _f·λₒ1·λₒ0·y = 0;
      return 0;
    }
    static syl_int _f·λₒ1ₒ1(syl_env 𝒰)
    {
      syl_int _f·λₒ1·x = 1;
      syl_env _f·λₒ1·env = NULL;
      return syl_mk_thunk(_f·λₒ1·λₒ1, _f·λₒ1·env);
    }
    static syl_unit _f·λₒ1ₒ0(syl_env 𝒰)
    {
      syl_int _f·λₒ1·x = 1;
      syl_env _f·λₒ1·env = NULL;
      return syl_mk_thunk(_f·λₒ1·λₒ0, _f·λₒ1·env);
    }
    static syl_bool _f·λₒ0·λₒ1(syl_env 𝒰)
    {
      syl_int _f·λₒ0·λₒ1·y = 1;
      return true;
    }
    static syl_int _f·λₒ0·λₒ0(syl_env 𝒰)
    {
      syl_int _f·λₒ0·λₒ0·y = 0;
      return 1;
    }
    static syl_bool _f·λₒ0ₒ1(syl_env 𝒰)
    {
      syl_int _f·λₒ0·x = 0;
      syl_env _f·λₒ0·env = NULL;
      return syl_mk_thunk(_f·λₒ0·λₒ1, _f·λₒ0·env);
    }
    static syl_int _f·λₒ0ₒ0(syl_env 𝒰)
    {
      syl_int _f·λₒ0·x = 0;
      syl_env _f·λₒ0·env = NULL;
      return syl_mk_thunk(_f·λₒ0·λₒ0, _f·λₒ0·env);
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒ1ₒ1 = syl_mk_thunk(_f·λₒ1ₒ1, _f·env);
        _fₒ1ₒ0 = syl_mk_thunk(_f·λₒ1ₒ0, _f·env);
        _fₒ0ₒ1 = syl_mk_thunk(_f·λₒ0ₒ1, _f·env);
        _fₒ0ₒ0 = syl_mk_thunk(_f·λₒ0ₒ0, _f·env);
      }
      {
        syl_int _$ₒ0 = syl_app_thunk(_fₒ0ₒ0);
        __ = syl_app_thunk(_$ₒ0);
      }
      {
        syl_bool _$ₒ1 = syl_app_thunk(_fₒ0ₒ1);
        __ˢ1 = syl_app_thunk(_$ₒ1);
      }
      {
        syl_unit _$ₒ0 = syl_app_thunk(_fₒ1ₒ0);
        __ˢ2 = syl_app_thunk(_$ₒ0);
      }
      {
        syl_int _$ₒ1 = syl_app_thunk(_fₒ1ₒ1);
        __ˢ3 = syl_app_thunk(_$ₒ1);
      }
      return 0;
    }
    |}]
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
  [%expect
    {|
    static syl_int _f·λₒ1·g·λₒ1(syl_env);
    static syl_unit _f·λₒ1·g·λₒ0(syl_env);
    static syl_int _f·λₒ1ₒ1(syl_env);
    static syl_unit _f·λₒ1ₒ0(syl_env);
    static syl_bool _f·λₒ0·g·λₒ1(syl_env);
    static syl_int _f·λₒ0·g·λₒ0(syl_env);
    static syl_bool _f·λₒ0ₒ1(syl_env);
    static syl_int _f·λₒ0ₒ0(syl_env);
    static syl_thunk _fₒ1ₒ1;
    static syl_thunk _fₒ1ₒ0;
    static syl_thunk _fₒ0ₒ1;
    static syl_thunk _fₒ0ₒ0;
    static syl_int __;
    static syl_bool __ˢ1;
    static syl_unit __ˢ2;
    static syl_int __ˢ3;
    static syl_int _f·λₒ1·g·λₒ1(syl_env 𝒰)
    {
      syl_int _f·λₒ1·g·λₒ1·y = 1;
      return 2;
    }
    static syl_unit _f·λₒ1·g·λₒ0(syl_env 𝒰)
    {
      syl_int _f·λₒ1·g·λₒ0·y = 0;
      return 0;
    }
    static syl_int _f·λₒ1ₒ1(syl_env 𝒰)
    {
      syl_int _f·λₒ1·x = 1;
      syl_thunk _f·λₒ1·gₒ1;
      syl_thunk _f·λₒ1·gₒ0;
      {
        syl_env 𝒰 = NULL;
        _f·λₒ1·gₒ1 = syl_mk_thunk(_f·λₒ1·g·λₒ1, 𝒰);
        _f·λₒ1·gₒ0 = syl_mk_thunk(_f·λₒ1·g·λₒ0, 𝒰);
      }
      return _f·λₒ1·gₒ1;
    }
    static syl_unit _f·λₒ1ₒ0(syl_env 𝒰)
    {
      syl_int _f·λₒ1·x = 1;
      syl_thunk _f·λₒ1·gₒ1;
      syl_thunk _f·λₒ1·gₒ0;
      {
        syl_env 𝒰 = NULL;
        _f·λₒ1·gₒ1 = syl_mk_thunk(_f·λₒ1·g·λₒ1, 𝒰);
        _f·λₒ1·gₒ0 = syl_mk_thunk(_f·λₒ1·g·λₒ0, 𝒰);
      }
      return _f·λₒ1·gₒ0;
    }
    static syl_bool _f·λₒ0·g·λₒ1(syl_env 𝒰)
    {
      syl_int _f·λₒ0·g·λₒ1·y = 1;
      return true;
    }
    static syl_int _f·λₒ0·g·λₒ0(syl_env 𝒰)
    {
      syl_int _f·λₒ0·g·λₒ0·y = 0;
      return 1;
    }
    static syl_bool _f·λₒ0ₒ1(syl_env 𝒰)
    {
      syl_int _f·λₒ0·x = 0;
      syl_thunk _f·λₒ0·gₒ1;
      syl_thunk _f·λₒ0·gₒ0;
      {
        syl_env 𝒰 = NULL;
        _f·λₒ0·gₒ1 = syl_mk_thunk(_f·λₒ0·g·λₒ1, 𝒰);
        _f·λₒ0·gₒ0 = syl_mk_thunk(_f·λₒ0·g·λₒ0, 𝒰);
      }
      return _f·λₒ0·gₒ1;
    }
    static syl_int _f·λₒ0ₒ0(syl_env 𝒰)
    {
      syl_int _f·λₒ0·x = 0;
      syl_thunk _f·λₒ0·gₒ1;
      syl_thunk _f·λₒ0·gₒ0;
      {
        syl_env 𝒰 = NULL;
        _f·λₒ0·gₒ1 = syl_mk_thunk(_f·λₒ0·g·λₒ1, 𝒰);
        _f·λₒ0·gₒ0 = syl_mk_thunk(_f·λₒ0·g·λₒ0, 𝒰);
      }
      return _f·λₒ0·gₒ0;
    }
    int main()
    {
      {
        syl_env 𝒰 = NULL;
        _fₒ1ₒ1 = syl_mk_thunk(_f·λₒ1ₒ1, 𝒰);
        _fₒ1ₒ0 = syl_mk_thunk(_f·λₒ1ₒ0, 𝒰);
        _fₒ0ₒ1 = syl_mk_thunk(_f·λₒ0ₒ1, 𝒰);
        _fₒ0ₒ0 = syl_mk_thunk(_f·λₒ0ₒ0, 𝒰);
      }
      {
        syl_int _$ₒ0 = syl_app_thunk(_fₒ0ₒ0);
        __ = syl_app_thunk(_$ₒ0);
      }
      {
        syl_bool _$ₒ1 = syl_app_thunk(_fₒ0ₒ1);
        __ˢ1 = syl_app_thunk(_$ₒ1);
      }
      {
        syl_unit _$ₒ0 = syl_app_thunk(_fₒ1ₒ0);
        __ˢ2 = syl_app_thunk(_$ₒ0);
      }
      {
        syl_int _$ₒ1 = syl_app_thunk(_fₒ1ₒ1);
        __ˢ3 = syl_app_thunk(_$ₒ1);
      }
      return 0;
    }
    |}]
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
  [%expect
    {|
    static syl_int _f·λₒ1·g·λₒ1(syl_env);
    static syl_unit _f·λₒ1·g·λₒ0(syl_env);
    static syl_int _f·λₒ1ₒ1(syl_env);
    static syl_unit _f·λₒ1ₒ0(syl_env);
    static syl_bool _f·λₒ0·g·λₒ1(syl_env);
    static syl_int _f·λₒ0·g·λₒ0(syl_env);
    static syl_bool _f·λₒ0ₒ1(syl_env);
    static syl_int _f·λₒ0ₒ0(syl_env);
    static syl_thunk _fₒ1ₒ1;
    static syl_thunk _fₒ1ₒ0;
    static syl_thunk _fₒ0ₒ1;
    static syl_thunk _fₒ0ₒ0;
    static syl_int __;
    static syl_bool __ˢ1;
    static syl_unit __ˢ2;
    static syl_int __ˢ3;
    static syl_int _f·λₒ1·g·λₒ1(syl_env 𝒰)
    {
      syl_int _x = 𝒰[0];
      syl_int _f·λₒ1·g·λₒ1·y = 1;
      return _x - _f·λₒ1·g·λₒ1·y;
    }
    static syl_unit _f·λₒ1·g·λₒ0(syl_env 𝒰)
    {
      syl_int _f·λₒ1·g·λₒ0·y = 0;
      return 0;
    }
    static syl_int _f·λₒ1ₒ1(syl_env 𝒰)
    {
      syl_int _f·λₒ1·x = 1;
      syl_thunk _f·λₒ1·gₒ1;
      syl_thunk _f·λₒ1·gₒ0;
      {
        syl_env 𝒰 = syl_env_rec(1);
        _f·λₒ1·gₒ1 = syl_mk_thunk(_f·λₒ1·g·λₒ1, 𝒰);
        _f·λₒ1·gₒ0 = syl_mk_thunk(_f·λₒ1·g·λₒ0, 𝒰);
        𝒰[0] = _f·λₒ1·x;
      }
      return _f·λₒ1·gₒ1;
    }
    static syl_unit _f·λₒ1ₒ0(syl_env 𝒰)
    {
      syl_int _f·λₒ1·x = 1;
      syl_thunk _f·λₒ1·gₒ1;
      syl_thunk _f·λₒ1·gₒ0;
      {
        syl_env 𝒰 = syl_env_rec(1);
        _f·λₒ1·gₒ1 = syl_mk_thunk(_f·λₒ1·g·λₒ1, 𝒰);
        _f·λₒ1·gₒ0 = syl_mk_thunk(_f·λₒ1·g·λₒ0, 𝒰);
        𝒰[0] = _f·λₒ1·x;
      }
      return _f·λₒ1·gₒ0;
    }
    static syl_bool _f·λₒ0·g·λₒ1(syl_env 𝒰)
    {
      syl_int _f·λₒ0·g·λₒ1·y = 1;
      return true;
    }
    static syl_int _f·λₒ0·g·λₒ0(syl_env 𝒰)
    {
      syl_int _x = 𝒰[0];
      syl_int _f·λₒ0·g·λₒ0·y = 0;
      return _x + _f·λₒ0·g·λₒ0·y;
    }
    static syl_bool _f·λₒ0ₒ1(syl_env 𝒰)
    {
      syl_int _f·λₒ0·x = 0;
      syl_thunk _f·λₒ0·gₒ1;
      syl_thunk _f·λₒ0·gₒ0;
      {
        syl_env 𝒰 = syl_env_rec(1);
        _f·λₒ0·gₒ1 = syl_mk_thunk(_f·λₒ0·g·λₒ1, 𝒰);
        _f·λₒ0·gₒ0 = syl_mk_thunk(_f·λₒ0·g·λₒ0, 𝒰);
        𝒰[0] = _f·λₒ0·x;
      }
      return _f·λₒ0·gₒ1;
    }
    static syl_int _f·λₒ0ₒ0(syl_env 𝒰)
    {
      syl_int _f·λₒ0·x = 0;
      syl_thunk _f·λₒ0·gₒ1;
      syl_thunk _f·λₒ0·gₒ0;
      {
        syl_env 𝒰 = syl_env_rec(1);
        _f·λₒ0·gₒ1 = syl_mk_thunk(_f·λₒ0·g·λₒ1, 𝒰);
        _f·λₒ0·gₒ0 = syl_mk_thunk(_f·λₒ0·g·λₒ0, 𝒰);
        𝒰[0] = _f·λₒ0·x;
      }
      return _f·λₒ0·gₒ0;
    }
    int main()
    {
      {
        syl_env 𝒰 = NULL;
        _fₒ1ₒ1 = syl_mk_thunk(_f·λₒ1ₒ1, 𝒰);
        _fₒ1ₒ0 = syl_mk_thunk(_f·λₒ1ₒ0, 𝒰);
        _fₒ0ₒ1 = syl_mk_thunk(_f·λₒ0ₒ1, 𝒰);
        _fₒ0ₒ0 = syl_mk_thunk(_f·λₒ0ₒ0, 𝒰);
      }
      {
        syl_int _$ₒ0 = syl_app_thunk(_fₒ0ₒ0);
        __ = syl_app_thunk(_$ₒ0);
      }
      {
        syl_bool _$ₒ1 = syl_app_thunk(_fₒ0ₒ1);
        __ˢ1 = syl_app_thunk(_$ₒ1);
      }
      {
        syl_unit _$ₒ0 = syl_app_thunk(_fₒ1ₒ0);
        __ˢ2 = syl_app_thunk(_$ₒ0);
      }
      {
        syl_int _$ₒ1 = syl_app_thunk(_fₒ1ₒ1);
        __ˢ3 = syl_app_thunk(_$ₒ1);
      }
      return 0;
    }
    |}]
;;

let%expect_test "static lambda unused arg" =
  go
    {|
let f = fn (static _ : int) -> 42;;
let _ = f 0;;
|};
  [%expect
    {|
    static syl_int _f·λₒ0(syl_env);
    static syl_int _fₒ0;
    static syl_int __;
    static syl_int _f·λₒ0(syl_env 𝒰)
    {
      syl_int _f·λₒ0·_ = 0;
      return 42;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒ0 = syl_mk_thunk(_f·λₒ0, _f·env);
      }
      __ = syl_app_thunk(_fₒ0);
      return 0;
    }
    |}]
;;

let%expect_test "static lambda effects" =
  go
    {|
external print_int : int -> unit = syl_print_int;;
let print = fn (static x : int) -> print_int x;;
let _ = print 0;;
let _ = print 0;;
let _ = print 1;;
|};
  [%expect
    {|
    extern syl_unit syl_print_int(syl_int);
    static syl_unit _syl_print_int·λ(syl_int _, syl_env 𝒰)
    {
      return syl_print_int(_);
    }
    static syl_closure _print_int;
    static syl_unit _print·λₒ1(syl_env);
    static syl_unit _print·λₒ0(syl_env);
    static syl_unit _printₒ1;
    static syl_unit _printₒ0;
    static syl_unit __;
    static syl_unit __ˢ1;
    static syl_unit __ˢ2;
    static syl_unit _print·λₒ1(syl_env 𝒰)
    {
      syl_closure _print_int = 𝒰[0];
      syl_int _print·λₒ1·x = 1;
      return syl_app_closure(_print_int, _print·λₒ1·x);
    }
    static syl_unit _print·λₒ0(syl_env 𝒰)
    {
      syl_closure _print_int = 𝒰[0];
      syl_int _print·λₒ0·x = 0;
      return syl_app_closure(_print_int, _print·λₒ0·x);
    }
    int main()
    {
      _print_int = syl_mk_closure(_syl_print_int·λ, SYL_ENV_EMPTY);
      {
        syl_env _print·env = syl_capture(1, _print_int);
        _printₒ1 = syl_mk_thunk(_print·λₒ1, _print·env);
        _printₒ0 = syl_mk_thunk(_print·λₒ0, _print·env);
      }
      __ = syl_app_thunk(_printₒ0);
      __ˢ1 = syl_app_thunk(_printₒ0);
      __ˢ2 = syl_app_thunk(_printₒ1);
      return 0;
    }
    |}]
;;

let%expect_test "static lambda effects" =
  go
    {|
external print_int : int -> unit = syl_print_int;;
let _ =
let print = fn (static x : int) -> print_int x in
let _ = print 0 in
let _ = print 0 in
let _ = print 1 in
();;
|};
  [%expect
    {|
    extern syl_unit syl_print_int(syl_int);
    static syl_unit _syl_print_int·λ(syl_int _, syl_env 𝒰)
    {
      return syl_print_int(_);
    }
    static syl_closure _print_int;
    static syl_unit __·print·λₒ1(syl_env);
    static syl_unit __·print·λₒ0(syl_env);
    static syl_unit __;
    static syl_unit __·print·λₒ1(syl_env 𝒰)
    {
      syl_closure _print_int = 𝒰[0];
      syl_int __·print·λₒ1·x = 1;
      return syl_app_closure(_print_int, __·print·λₒ1·x);
    }
    static syl_unit __·print·λₒ0(syl_env 𝒰)
    {
      syl_closure _print_int = 𝒰[0];
      syl_int __·print·λₒ0·x = 0;
      return syl_app_closure(_print_int, __·print·λₒ0·x);
    }
    int main()
    {
      _print_int = syl_mk_closure(_syl_print_int·λ, SYL_ENV_EMPTY);
      {
        syl_unit __·printₒ1;
        syl_unit __·printₒ0;
        {
          syl_env __·print·env = syl_capture(1, _print_int);
          __·printₒ1 = syl_mk_thunk(__·print·λₒ1, __·print·env);
          __·printₒ0 = syl_mk_thunk(__·print·λₒ0, __·print·env);
        }
        syl_unit __·_ = syl_app_thunk(__·printₒ0);
        syl_unit __·_ˢ1 = syl_app_thunk(__·printₒ0);
        syl_unit __·_ˢ2 = syl_app_thunk(__·printₒ1);
        __ = 0;
      }
      return 0;
    }
    |}]
;;

let%expect_test "static lambda effects" =
  go
    {|
external print_int : int -> unit = syl_print_int;;
let print = fn (static x : int) ->
  let _ = print_int x in
  fn (static y : int) -> print_int y;;
let _ = print 0 1;;
let _ = print 1 2;;
let _ = print 1 3;;
|};
  [%expect
    {|
    extern syl_unit syl_print_int(syl_int);
    static syl_unit _syl_print_int·λ(syl_int _, syl_env 𝒰)
    {
      return syl_print_int(_);
    }
    static syl_closure _print_int;
    static syl_unit _print·λₒ1·λₒ2(syl_env);
    static syl_unit _print·λₒ1·λₒ3(syl_env);
    static syl_unit _print·λₒ1ₒ2(syl_env);
    static syl_unit _print·λₒ1ₒ3(syl_env);
    static syl_unit _print·λₒ0·λₒ1(syl_env);
    static syl_unit _print·λₒ0ₒ1(syl_env);
    static syl_unit _printₒ1ₒ2;
    static syl_unit _printₒ1ₒ3;
    static syl_unit _printₒ0ₒ1;
    static syl_unit __;
    static syl_unit __ˢ1;
    static syl_unit __ˢ2;
    static syl_unit _print·λₒ1·λₒ2(syl_env 𝒰)
    {
      syl_closure _print_int = 𝒰[0];
      syl_int _print·λₒ1·λₒ2·y = 2;
      return syl_app_closure(_print_int, _print·λₒ1·λₒ2·y);
    }
    static syl_unit _print·λₒ1·λₒ3(syl_env 𝒰)
    {
      syl_closure _print_int = 𝒰[0];
      syl_int _print·λₒ1·λₒ3·y = 3;
      return syl_app_closure(_print_int, _print·λₒ1·λₒ3·y);
    }
    static syl_unit _print·λₒ1ₒ2(syl_env 𝒰)
    {
      syl_closure _print_int = 𝒰[0];
      syl_int _print·λₒ1·x = 1;
      syl_unit _print·λₒ1·_ = syl_app_closure(_print_int, _print·λₒ1·x);
      syl_env _print·λₒ1·env = syl_capture(1, _print_int);
      return syl_mk_thunk(_print·λₒ1·λₒ2, _print·λₒ1·env);
    }
    static syl_unit _print·λₒ1ₒ3(syl_env 𝒰)
    {
      syl_closure _print_int = 𝒰[0];
      syl_int _print·λₒ1·x = 1;
      syl_unit _print·λₒ1·_ = syl_app_closure(_print_int, _print·λₒ1·x);
      syl_env _print·λₒ1·env = syl_capture(1, _print_int);
      return syl_mk_thunk(_print·λₒ1·λₒ3, _print·λₒ1·env);
    }
    static syl_unit _print·λₒ0·λₒ1(syl_env 𝒰)
    {
      syl_closure _print_int = 𝒰[0];
      syl_int _print·λₒ0·λₒ1·y = 1;
      return syl_app_closure(_print_int, _print·λₒ0·λₒ1·y);
    }
    static syl_unit _print·λₒ0ₒ1(syl_env 𝒰)
    {
      syl_closure _print_int = 𝒰[0];
      syl_int _print·λₒ0·x = 0;
      syl_unit _print·λₒ0·_ = syl_app_closure(_print_int, _print·λₒ0·x);
      syl_env _print·λₒ0·env = syl_capture(1, _print_int);
      return syl_mk_thunk(_print·λₒ0·λₒ1, _print·λₒ0·env);
    }
    int main()
    {
      _print_int = syl_mk_closure(_syl_print_int·λ, SYL_ENV_EMPTY);
      {
        syl_env _print·env = syl_capture(1, _print_int);
        _printₒ1ₒ2 = syl_mk_thunk(_print·λₒ1ₒ2, _print·env);
        _printₒ1ₒ3 = syl_mk_thunk(_print·λₒ1ₒ3, _print·env);
        _printₒ0ₒ1 = syl_mk_thunk(_print·λₒ0ₒ1, _print·env);
      }
      {
        syl_unit _$ₒ1 = syl_app_thunk(_printₒ0ₒ1);
        __ = syl_app_thunk(_$ₒ1);
      }
      {
        syl_unit _$ₒ2 = syl_app_thunk(_printₒ1ₒ2);
        __ˢ1 = syl_app_thunk(_$ₒ2);
      }
      {
        syl_unit _$ₒ3 = syl_app_thunk(_printₒ1ₒ3);
        __ˢ2 = syl_app_thunk(_$ₒ3);
      }
      return 0;
    }
    |}]
;;

let%expect_test "dependent type: apply polymorphic id to itself" =
  go
    {|
let id = fn (static erased t : type) -> fn (x : t) -> x;;
let _ = (id (int -> int)) (fn (x : int) -> x + 1) 5;;
|};
  [%expect
    {|
    static syl_closure _id·λₒ𝕀🡒𝕀·λ(syl_closure, syl_env);
    static syl_closure _id·λₒ𝕀🡒𝕀(syl_env);
    static syl_closure _idₒ𝕀🡒𝕀;
    static syl_int __·λ(syl_int, syl_env);
    static syl_int __;
    static syl_closure _id·λₒ𝕀🡒𝕀·λ(syl_closure _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure _id·λₒ𝕀🡒𝕀(syl_env 𝒰)
    {
      syl_env _id·λₒ𝕀🡒𝕀·env = NULL;
      return syl_mk_closure(_id·λₒ𝕀🡒𝕀·λ, _id·λₒ𝕀🡒𝕀·env);
    }
    static syl_int __·λ(syl_int _x, syl_env 𝒰)
    {
      syl_int _$ = 1;
      return _x + _$;
    }
    int main()
    {
      {
        syl_env _id·env = NULL;
        _idₒ𝕀🡒𝕀 = syl_mk_thunk(_id·λₒ𝕀🡒𝕀, _id·env);
      }
      {
        syl_closure _$ = syl_app_thunk(_idₒ𝕀🡒𝕀);
        syl_env __·env = NULL;
        syl_closure _$ˢ1 = syl_mk_closure(__·λ, __·env);
        syl_closure _$ˢ2 = syl_app_closure(_$, _$ˢ1);
        syl_int _$ˢ3 = 5;
        __ = syl_app_closure(_$ˢ2, _$ˢ3);
      }
      return 0;
    }
    |}]
;;

let%expect_test "if static with bool static arg" =
  go
    {|
let f = fn (static b : bool) -> if static b then 1 else true;;
let _ = f true;;
let _ = f false;;
|};
  [%expect
    {|
    static syl_bool _f·λₒF(syl_env);
    static syl_int _f·λₒT(syl_env);
    static syl_bool _fₒF;
    static syl_int _fₒT;
    static syl_int __;
    static syl_bool __ˢ1;
    static syl_bool _f·λₒF(syl_env 𝒰)
    {
      syl_bool _f·λₒF·b = false;
      return true;
    }
    static syl_int _f·λₒT(syl_env 𝒰)
    {
      syl_bool _f·λₒT·b = true;
      return 1;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒF = syl_mk_thunk(_f·λₒF, _f·env);
        _fₒT = syl_mk_thunk(_f·λₒT, _f·env);
      }
      __ = syl_app_thunk(_fₒT);
      __ˢ1 = syl_app_thunk(_fₒF);
      return 0;
    }
    |}]
;;

let%expect_test "static arg used in arithmetic, result applied" =
  go
    {|
let double = fn (static x : int) -> x + x;;
let _ = double 5;;
|};
  [%expect
    {|
    static syl_int _double·λₒ5(syl_env);
    static syl_int _doubleₒ5;
    static syl_int __;
    static syl_int _double·λₒ5(syl_env 𝒰)
    {
      syl_int _double·λₒ5·x = 5;
      return _double·λₒ5·x + _double·λₒ5·x;
    }
    int main()
    {
      {
        syl_env _double·env = NULL;
        _doubleₒ5 = syl_mk_thunk(_double·λₒ5, _double·env);
      }
      __ = syl_app_thunk(_doubleₒ5);
      return 0;
    }
    |}]
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
  [%expect
    {|
    static syl_bool _id·λₒ𝔹·λ(syl_bool, syl_env);
    static syl_closure _id·λₒ𝔹(syl_env);
    static syl_int _id·λₒ𝕀·λ(syl_int, syl_env);
    static syl_closure _id·λₒ𝕀(syl_env);
    static syl_closure _idₒ𝔹;
    static syl_closure _idₒ𝕀;
    static syl_closure _f;
    static syl_closure _g;
    static syl_int __;
    static syl_bool __ˢ1;
    static syl_bool _id·λₒ𝔹·λ(syl_bool _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure _id·λₒ𝔹(syl_env 𝒰)
    {
      syl_env _id·λₒ𝔹·env = NULL;
      return syl_mk_closure(_id·λₒ𝔹·λ, _id·λₒ𝔹·env);
    }
    static syl_int _id·λₒ𝕀·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure _id·λₒ𝕀(syl_env 𝒰)
    {
      syl_env _id·λₒ𝕀·env = NULL;
      return syl_mk_closure(_id·λₒ𝕀·λ, _id·λₒ𝕀·env);
    }
    int main()
    {
      {
        syl_env _id·env = NULL;
        _idₒ𝔹 = syl_mk_thunk(_id·λₒ𝔹, _id·env);
        _idₒ𝕀 = syl_mk_thunk(_id·λₒ𝕀, _id·env);
      }
      _f = syl_app_thunk(_idₒ𝕀);
      _g = syl_app_thunk(_idₒ𝔹);
      {
        syl_int _$ = 0;
        __ = syl_app_closure(_f, _$);
      }
      {
        syl_bool _$ = true;
        __ˢ1 = syl_app_closure(_g, _$);
      }
      return 0;
    }
    |}]
;;

let%expect_test "symbolic arrow type as static arg" =
  go
    {|
let choose = fn (static f : static int \ x -> if x == 0 then int else bool) -> f 0;;
let _ = choose (fn (static x : int) -> if static x == 0 then 0 else true);;
|};
  [%expect
    {|
    static syl_int _choose·λₒλ4·f·λₒ0(syl_env);
    static syl_int _choose·λₒλ4(syl_env);
    static syl_int _chooseₒλ4;
    static syl_int __;
    static syl_int _choose·λₒλ4·f·λₒ0(syl_env 𝒰)
    {
      syl_int _choose·λₒλ4·f·λₒ0·x = 0;
      return 0;
    }
    static syl_int _choose·λₒλ4(syl_env 𝒰)
    {
      syl_int _choose·λₒλ4·fₒ0;
      {
        syl_env _choose·λₒλ4·f·env = NULL;
        _choose·λₒλ4·fₒ0 = syl_mk_thunk(_choose·λₒλ4·f·λₒ0, _choose·λₒλ4·f·env);
      }
      return syl_app_thunk(_choose·λₒλ4·fₒ0);
    }
    int main()
    {
      {
        syl_env _choose·env = NULL;
        _chooseₒλ4 = syl_mk_thunk(_choose·λₒλ4, _choose·env);
      }
      __ = syl_app_thunk(_chooseₒλ4);
      return 0;
    }
    |}]
;;

let%expect_test "static lambda body references outer let binding" =
  go
    {|
let n = 10;;
let f = fn (static x : int) -> x + n;;
let _ = f 5;;
|};
  [%expect
    {|
    static syl_int _n;
    static syl_int _f·λₒ5(syl_env);
    static syl_int _fₒ5;
    static syl_int __;
    static syl_int _f·λₒ5(syl_env 𝒰)
    {
      syl_int _n = 𝒰[0];
      syl_int _f·λₒ5·x = 5;
      return _f·λₒ5·x + _n;
    }
    int main()
    {
      _n = 10;
      {
        syl_env _f·env = syl_capture(1, _n);
        _fₒ5 = syl_mk_thunk(_f·λₒ5, _f·env);
      }
      __ = syl_app_thunk(_fₒ5);
      return 0;
    }
    |}]
;;

let%expect_test "static lambda with type annotation on body" =
  go
    {|
let f = fn (static x : int) -> (x : int);;
let _ = f 42;;
|};
  [%expect
    {|
    static syl_int _f·λₒ42(syl_env);
    static syl_int _fₒ42;
    static syl_int __;
    static syl_int _f·λₒ42(syl_env 𝒰)
    {
      syl_int _f·λₒ42·x = 42;
      return _f·λₒ42·x;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒ42 = syl_mk_thunk(_f·λₒ42, _f·env);
      }
      __ = syl_app_thunk(_fₒ42);
      return 0;
    }
    |}]
;;

let%expect_test "if static in type annotation position" =
  go
    {|
let f = fn (static b : bool) -> (if static b then 0 else true) : (if b then int else bool);;
let _ = f true;;
let _ = f false;;
|};
  [%expect
    {|
    static syl_bool _f·λₒF(syl_env);
    static syl_int _f·λₒT(syl_env);
    static syl_bool _fₒF;
    static syl_int _fₒT;
    static syl_int __;
    static syl_bool __ˢ1;
    static syl_bool _f·λₒF(syl_env 𝒰)
    {
      syl_bool _f·λₒF·b = false;
      return true;
    }
    static syl_int _f·λₒT(syl_env 𝒰)
    {
      syl_bool _f·λₒT·b = true;
      return 0;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒF = syl_mk_thunk(_f·λₒF, _f·env);
        _fₒT = syl_mk_thunk(_f·λₒT, _f·env);
      }
      __ = syl_app_thunk(_fₒT);
      __ˢ1 = syl_app_thunk(_fₒF);
      return 0;
    }
    |}]
;;

let%expect_test "higher-order static: take a static function and apply it" =
  go
    {|
let apply = fn (static f : static int -> int) -> f 5;;
let _ = apply (fn (static x : int) -> x + 1);;
|};
  [%expect
    {|
    static syl_int _apply·λₒλ4·f·λₒ5(syl_env);
    static syl_int _apply·λₒλ4(syl_env);
    static syl_int _applyₒλ4;
    static syl_int __;
    static syl_int _apply·λₒλ4·f·λₒ5(syl_env 𝒰)
    {
      syl_int _apply·λₒλ4·f·λₒ5·x = 5;
      syl_int _$ = 1;
      return _apply·λₒλ4·f·λₒ5·x + _$;
    }
    static syl_int _apply·λₒλ4(syl_env 𝒰)
    {
      syl_int _apply·λₒλ4·fₒ5;
      {
        syl_env _apply·λₒλ4·f·env = NULL;
        _apply·λₒλ4·fₒ5 = syl_mk_thunk(_apply·λₒλ4·f·λₒ5, _apply·λₒλ4·f·env);
      }
      return syl_app_thunk(_apply·λₒλ4·fₒ5);
    }
    int main()
    {
      {
        syl_env _apply·env = NULL;
        _applyₒλ4 = syl_mk_thunk(_apply·λₒλ4, _apply·env);
      }
      __ = syl_app_thunk(_applyₒλ4);
      return 0;
    }
    |}]
;;

let%expect_test "multiple static erased type args" =
  go
    {|
let f = fn (static erased t1 : type) -> fn (static erased t2 : type) -> fn (x : t1) -> fn (y : t2) -> x;;
let _ = f int bool 0 true;;
|};
  [%expect
    {|
    static syl_int _f·λₒ𝕀·λₒ𝔹·λ·λ(syl_bool, syl_env);
    static syl_closure _f·λₒ𝕀·λₒ𝔹·λ(syl_int, syl_env);
    static syl_closure _f·λₒ𝕀·λₒ𝔹(syl_env);
    static syl_closure _f·λₒ𝕀ₒ𝔹(syl_env);
    static syl_closure _fₒ𝕀ₒ𝔹;
    static syl_int __;
    static syl_int _f·λₒ𝕀·λₒ𝔹·λ·λ(syl_bool _y, syl_env 𝒰)
    {
      syl_int _x = 𝒰[0];
      return _x;
    }
    static syl_closure _f·λₒ𝕀·λₒ𝔹·λ(syl_int _x, syl_env 𝒰)
    {
      syl_env _f·λₒ𝕀·λₒ𝔹·λ·env = syl_capture(1, _x);
      return syl_mk_closure(_f·λₒ𝕀·λₒ𝔹·λ·λ, _f·λₒ𝕀·λₒ𝔹·λ·env);
    }
    static syl_closure _f·λₒ𝕀·λₒ𝔹(syl_env 𝒰)
    {
      syl_env _f·λₒ𝕀·λₒ𝔹·env = NULL;
      return syl_mk_closure(_f·λₒ𝕀·λₒ𝔹·λ, _f·λₒ𝕀·λₒ𝔹·env);
    }
    static syl_closure _f·λₒ𝕀ₒ𝔹(syl_env 𝒰)
    {
      syl_env _f·λₒ𝕀·env = NULL;
      return syl_mk_thunk(_f·λₒ𝕀·λₒ𝔹, _f·λₒ𝕀·env);
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒ𝕀ₒ𝔹 = syl_mk_thunk(_f·λₒ𝕀ₒ𝔹, _f·env);
      }
      {
        syl_closure _$ₒ𝔹 = syl_app_thunk(_fₒ𝕀ₒ𝔹);
        syl_closure _$ˢ1 = syl_app_thunk(_$ₒ𝔹);
        syl_int _$ˢ2 = 0;
        syl_closure _$ˢ3 = syl_app_closure(_$ˢ1, _$ˢ2);
        syl_bool _$ˢ4 = true;
        __ = syl_app_closure(_$ˢ3, _$ˢ4);
      }
      return 0;
    }
    |}]
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
  [%expect
    {|
    static syl_bool _f·λₒ1(syl_env);
    static syl_int _f·λₒ0(syl_env);
    static syl_bool _fₒ1;
    static syl_int _fₒ0;
    static syl_int __;
    static syl_bool __ˢ1;
    static syl_bool _f·λₒ1(syl_env 𝒰)
    {
      syl_int _f·λₒ1·x = 1;
      syl_bool _f·λₒ1·y = true;
      return _f·λₒ1·y;
    }
    static syl_int _f·λₒ0(syl_env 𝒰)
    {
      syl_int _f·λₒ0·x = 0;
      syl_int _f·λₒ0·y = 1;
      return _f·λₒ0·y;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒ1 = syl_mk_thunk(_f·λₒ1, _f·env);
        _fₒ0 = syl_mk_thunk(_f·λₒ0, _f·env);
      }
      __ = syl_app_thunk(_fₒ0);
      __ˢ1 = syl_app_thunk(_fₒ1);
      return 0;
    }
    |}]
;;

let%expect_test "join Pi/Pi function-type arg returning type: fresh var issue" =
  go
    {|
let f = fn (static erased g : static int -> static erased type) -> fn (x : g 0) -> x;;
let h = fn (static erased g : static int -> static erased type) -> fn (x : g 0) -> x;;
let x = if true then f else h;;
let _ = x (fn (static x : int) -> int);;
|};
  [%expect
    {|
    static syl_int _f·λₒλ10·λ(syl_int, syl_env);
    static syl_closure _f·λₒλ10(syl_env);
    static syl_closure _fₒλ10;
    static syl_closure _xₒλ10;
    static syl_closure __;
    static syl_int _f·λₒλ10·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure _f·λₒλ10(syl_env 𝒰)
    {
      syl_env _f·λₒλ10·env = NULL;
      return syl_mk_closure(_f·λₒλ10·λ, _f·λₒλ10·env);
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒλ10 = syl_mk_thunk(_f·λₒλ10, _f·env);
      }
      {
        syl_env _h·env = NULL;
      }
      _xₒλ10 = _fₒλ10;
      __ = syl_app_thunk(_xₒλ10);
      return 0;
    }
    |}]
;;

let%expect_test "leq Pi/Pi function-type arg returning type" =
  go
    {|
let wrap = fn (static erased f : static int -> static erased type) -> fn (x : f 0) -> x;;
let wrap2 = wrap : static erased (static int -> static erased type) \ f -> f 0 -> f 0;;
let _ = wrap2 (fn (static x : int) -> int);;
|};
  [%expect
    {|
    static syl_int _wrap·λₒλ9·λ(syl_int, syl_env);
    static syl_closure _wrap·λₒλ9(syl_env);
    static syl_closure _wrapₒλ9;
    static syl_closure _wrap2ₒλ9;
    static syl_closure __;
    static syl_int _wrap·λₒλ9·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure _wrap·λₒλ9(syl_env 𝒰)
    {
      syl_env _wrap·λₒλ9·env = NULL;
      return syl_mk_closure(_wrap·λₒλ9·λ, _wrap·λₒλ9·env);
    }
    int main()
    {
      {
        syl_env _wrap·env = NULL;
        _wrapₒλ9 = syl_mk_thunk(_wrap·λₒλ9, _wrap·env);
      }
      _wrap2ₒλ9 = _wrapₒλ9;
      __ = syl_app_thunk(_wrap2ₒλ9);
      return 0;
    }
    |}]
;;

let%expect_test "meet Pi/Pi function-type arg: via arg contravariance in join" =
  go
    {|
let f = fn (static apply : static (static int -> int) -> int) -> apply (fn (static x : int) -> 0);;
let g = fn (static apply : static (static erased int -> int) -> int) -> apply (fn (static erased x : int) -> 0);;
let x = if true then f else g;;
let _ = x (fn (static f : static int -> int) -> f 0);;
|};
  [%expect
    {|
    static syl_int _f·λₒλ17·apply·λₒλ20·f·λₒ0(syl_env);
    static syl_int _f·λₒλ17·apply·λₒλ20(syl_env);
    static syl_int _f·λₒλ17(syl_env);
    static syl_int _fₒλ17;
    static syl_int _xₒλ17;
    static syl_int __;
    static syl_int _f·λₒλ17·apply·λₒλ20·f·λₒ0(syl_env 𝒰)
    {
      syl_int _f·λₒλ17·apply·λₒλ20·f·λₒ0·x = 0;
      return 0;
    }
    static syl_int _f·λₒλ17·apply·λₒλ20(syl_env 𝒰)
    {
      syl_int _f·λₒλ17·apply·λₒλ20·fₒ0;
      {
        syl_env _f·λₒλ17·apply·λₒλ20·f·env = NULL;
        _f·λₒλ17·apply·λₒλ20·fₒ0 = syl_mk_thunk(_f·λₒλ17·apply·λₒλ20·f·λₒ0, _f·λₒλ17·apply·λₒλ20·f·env);
      }
      return syl_app_thunk(_f·λₒλ17·apply·λₒλ20·fₒ0);
    }
    static syl_int _f·λₒλ17(syl_env 𝒰)
    {
      syl_int _f·λₒλ17·applyₒλ20;
      {
        syl_env _f·λₒλ17·apply·env = NULL;
        _f·λₒλ17·applyₒλ20 = syl_mk_thunk(_f·λₒλ17·apply·λₒλ20, _f·λₒλ17·apply·env);
      }
      return syl_app_thunk(_f·λₒλ17·applyₒλ20);
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒλ17 = syl_mk_thunk(_f·λₒλ17, _f·env);
      }
      {
        syl_env _g·env = NULL;
      }
      _xₒλ17 = _fₒλ17;
      __ = syl_app_thunk(_xₒλ17);
      return 0;
    }
    |}]
;;

let%expect_test "arrow function calling pi function in same group" =
  go
    {|
fun f (static x : int) : int = x;;
let _ = (fn (_ : unit) -> f 0);;
|};
  [%expect
    {|
    static syl_int _f·λₒ0(syl_env);
    static syl_thunk _fₒ0;
    static syl_int __·λ(syl_unit, syl_env);
    static syl_closure __;
    static syl_int _f·λₒ0(syl_env 𝒰)
    {
      syl_int _f·λₒ0·x = 0;
      return _f·λₒ0·x;
    }
    static syl_int __·λ(syl_unit __, syl_env 𝒰)
    {
      syl_int _fₒ0 = 𝒰[0];
      return syl_app_thunk(_fₒ0);
    }
    int main()
    {
      {
        syl_env 𝒰 = NULL;
        _fₒ0 = syl_mk_thunk(_f·λₒ0, 𝒰);
      }
      {
        syl_env __·env = syl_capture(1, _fₒ0);
        __ = syl_mk_closure(__·λ, __·env);
      }
      return 0;
    }
    |}]
;;

let%expect_test "arrow function calling pi function in same group" =
  go
    {|
fun f (static x : int) : int = x;;
let _ = (fn (_ : unit) -> f 0 + f 1);;
|};
  [%expect
    {|
    static syl_int _f·λₒ1(syl_env);
    static syl_int _f·λₒ0(syl_env);
    static syl_thunk _fₒ1;
    static syl_thunk _fₒ0;
    static syl_int __·λ(syl_unit, syl_env);
    static syl_closure __;
    static syl_int _f·λₒ1(syl_env 𝒰)
    {
      syl_int _f·λₒ1·x = 1;
      return _f·λₒ1·x;
    }
    static syl_int _f·λₒ0(syl_env 𝒰)
    {
      syl_int _f·λₒ0·x = 0;
      return _f·λₒ0·x;
    }
    static syl_int __·λ(syl_unit __, syl_env 𝒰)
    {
      syl_int _fₒ0 = 𝒰[0];
      syl_int _fₒ1 = 𝒰[1];
      syl_int _$ = syl_app_thunk(_fₒ0);
      syl_int _$ˢ1 = syl_app_thunk(_fₒ1);
      return _$ + _$ˢ1;
    }
    int main()
    {
      {
        syl_env 𝒰 = NULL;
        _fₒ1 = syl_mk_thunk(_f·λₒ1, 𝒰);
        _fₒ0 = syl_mk_thunk(_f·λₒ0, 𝒰);
      }
      {
        syl_env __·env = syl_capture(2, _fₒ0, _fₒ1);
        __ = syl_mk_closure(__·λ, __·env);
      }
      return 0;
    }
    |}]
;;

let%expect_test "arrow function calling pi function in same group" =
  go
    {|
fun f (static erased t : type) : t -> t = fn (x : t) -> x
and g (x : int) : int = f int x;;
let _ = g 5;;
|};
  [%expect
    {|
    static syl_int _f·λₒ𝕀·λ(syl_int, syl_env);
    static syl_closure _f·λₒ𝕀(syl_env);
    static syl_int _g·λ(syl_int, syl_env);
    static syl_closure _g;
    static syl_thunk _fₒ𝕀;
    static syl_int __;
    static syl_int _f·λₒ𝕀·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure _f·λₒ𝕀(syl_env 𝒰)
    {
      syl_env _f·λₒ𝕀·env = NULL;
      return syl_mk_closure(_f·λₒ𝕀·λ, _f·λₒ𝕀·env);
    }
    static syl_int _g·λ(syl_int _x, syl_env 𝒰)
    {
      syl_closure _fₒ𝕀 = 𝒰[0];
      syl_closure _$ = syl_app_thunk(_fₒ𝕀);
      return syl_app_closure(_$, _x);
    }
    int main()
    {
      {
        syl_env 𝒰 = syl_env_rec(1);
        _g = syl_mk_closure(_g·λ, 𝒰);
        _fₒ𝕀 = syl_mk_thunk(_f·λₒ𝕀, 𝒰);
        𝒰[0] = _fₒ𝕀;
      }
      {
        syl_int _$ = 5;
        __ = syl_app_closure(_g, _$);
      }
      return 0;
    }
    |}]
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
  [%expect
    {|
    static syl_int _inc·λ(syl_int, syl_env);
    static syl_int _choose·λₒF·λ(syl_int, syl_env);
    static syl_closure _choose·λₒF(syl_env);
    static syl_int _choose·λₒT·λ(syl_int, syl_env);
    static syl_closure _choose·λₒT(syl_env);
    static syl_closure _inc;
    static syl_thunk _chooseₒF;
    static syl_thunk _chooseₒT;
    static syl_int __;
    static syl_int __ˢ1;
    static syl_int _inc·λ(syl_int _x, syl_env 𝒰)
    {
      syl_closure _chooseₒT = 𝒰[0];
      syl_closure _inc·λ·_ = syl_app_thunk(_chooseₒT);
      syl_int _$ = 1;
      return _x + _$;
    }
    static syl_int _choose·λₒF·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure _choose·λₒF(syl_env 𝒰)
    {
      syl_env _choose·λₒF·env = NULL;
      return syl_mk_closure(_choose·λₒF·λ, _choose·λₒF·env);
    }
    static syl_int _choose·λₒT·λ(syl_int _x, syl_env 𝒰)
    {
      syl_closure _inc = 𝒰[0];
      return syl_app_closure(_inc, _x);
    }
    static syl_closure _choose·λₒT(syl_env 𝒰)
    {
      syl_closure _inc = 𝒰[1];
      syl_env _choose·λₒT·env = syl_capture(1, _inc);
      return syl_mk_closure(_choose·λₒT·λ, _choose·λₒT·env);
    }
    int main()
    {
      {
        syl_env 𝒰 = syl_env_rec(2);
        _inc = syl_mk_closure(_inc·λ, 𝒰);
        _chooseₒF = syl_mk_thunk(_choose·λₒF, 𝒰);
        _chooseₒT = syl_mk_thunk(_choose·λₒT, 𝒰);
        𝒰[0] = _chooseₒT;
        𝒰[1] = _inc;
      }
      {
        syl_closure _$ = syl_app_thunk(_chooseₒT);
        syl_int _$ˢ1 = 5;
        __ = syl_app_closure(_$, _$ˢ1);
      }
      {
        syl_closure _$ = syl_app_thunk(_chooseₒF);
        syl_int _$ˢ1 = 5;
        __ˢ1 = syl_app_closure(_$, _$ˢ1);
      }
      return 0;
    }
    |}]
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
  [%expect
    {|
    static syl_int __·inc·λ(syl_int, syl_env);
    static syl_int __·choose·λₒF·λ(syl_int, syl_env);
    static syl_closure __·choose·λₒF(syl_env);
    static syl_int __·choose·λₒT·λ(syl_int, syl_env);
    static syl_closure __·choose·λₒT(syl_env);
    static syl_unit __;
    static syl_int __·inc·λ(syl_int _x, syl_env 𝒰)
    {
      syl_closure _chooseₒT = 𝒰[0];
      syl_closure __·inc·λ·_ = syl_app_thunk(_chooseₒT);
      syl_int _$ = 1;
      return _x + _$;
    }
    static syl_int __·choose·λₒF·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure __·choose·λₒF(syl_env 𝒰)
    {
      syl_env __·choose·λₒF·env = NULL;
      return syl_mk_closure(__·choose·λₒF·λ, __·choose·λₒF·env);
    }
    static syl_int __·choose·λₒT·λ(syl_int _x, syl_env 𝒰)
    {
      syl_closure _inc = 𝒰[0];
      return syl_app_closure(_inc, _x);
    }
    static syl_closure __·choose·λₒT(syl_env 𝒰)
    {
      syl_closure _inc = 𝒰[1];
      syl_env __·choose·λₒT·env = syl_capture(1, _inc);
      return syl_mk_closure(__·choose·λₒT·λ, __·choose·λₒT·env);
    }
    int main()
    {
      {
        syl_closure __·inc;
        syl_thunk __·chooseₒF;
        syl_thunk __·chooseₒT;
        {
          syl_env 𝒰 = syl_env_rec(2);
          __·inc = syl_mk_closure(__·inc·λ, 𝒰);
          __·chooseₒF = syl_mk_thunk(__·choose·λₒF, 𝒰);
          __·chooseₒT = syl_mk_thunk(__·choose·λₒT, 𝒰);
          𝒰[0] = __·chooseₒT;
          𝒰[1] = __·inc;
        }
        syl_int __·_;
        {
          syl_closure _$ = syl_app_thunk(__·chooseₒT);
          syl_int _$ˢ1 = 5;
          __·_ = syl_app_closure(_$, _$ˢ1);
        }
        syl_int __·_ˢ1;
        {
          syl_closure _$ = syl_app_thunk(__·chooseₒF);
          syl_int _$ˢ1 = 5;
          __·_ˢ1 = syl_app_closure(_$, _$ˢ1);
        }
        __ = 0;
      }
      return 0;
    }
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : erased int = g x
and g (y : int) : int = let _ = f y in 0;;
let _ = g 0;;
|};
  [%expect
    {|
    static syl_int _g·λ(syl_int, syl_env);
    static syl_closure _g;
    static syl_int __;
    static syl_int _g·λ(syl_int _y, syl_env 𝒰)
    {
      return 0;
    }
    int main()
    {
      {
        syl_env 𝒰 = NULL;
        _g = syl_mk_closure(_g·λ, 𝒰);
      }
      {
        syl_int _$ = 0;
        __ = syl_app_closure(_g, _$);
      }
      return 0;
    }
    |}]
;;

let%expect_test "fun env" =
  go
    {|
let a = 0 @ dynamic;;
fun f (x : int) : int = let _ = a in x;;
|};
  [%expect
    {|
    static syl_int _a;
    static syl_int _f·λ(syl_int, syl_env);
    static syl_closure _f;
    static syl_int _f·λ(syl_int _x, syl_env 𝒰)
    {
      syl_int _a = 𝒰[0];
      syl_int _f·λ·_ = _a;
      return _x;
    }
    int main()
    {
      _a = 0;
      {
        syl_env 𝒰 = syl_env_rec(1);
        _f = syl_mk_closure(_f·λ, 𝒰);
        𝒰[0] = _a;
      }
      return 0;
    }
    |}]
;;

let%expect_test "fun recurse" =
  go
    {|
let a = 0 @ dynamic;;
fun f (x : int) : int = let _ = a in f x;;
|};
  [%expect
    {|
    static syl_int _a;
    static syl_int _f·λ(syl_int, syl_env);
    static syl_closure _f;
    static syl_int _f·λ(syl_int _x, syl_env 𝒰)
    {
      syl_closure _f = 𝒰[1];
      syl_int _a = 𝒰[0];
      syl_int _f·λ·_ = _a;
      return syl_app_closure(_f, _x);
    }
    int main()
    {
      _a = 0;
      {
        syl_env 𝒰 = syl_env_rec(2);
        _f = syl_mk_closure(_f·λ, 𝒰);
        𝒰[0] = _a;
        𝒰[1] = _f;
      }
      return 0;
    }
    |}]
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
  [%expect
    {|
    static syl_int _a;
    static syl_int __·f·λ(syl_int, syl_env);
    static syl_unit __;
    static syl_int __·f·λ(syl_int _x, syl_env 𝒰)
    {
      syl_closure _f = 𝒰[1];
      syl_int _a = 𝒰[0];
      syl_int __·f·λ·_ = _a;
      return syl_app_closure(_f, _x);
    }
    int main()
    {
      _a = 0;
      {
        syl_closure __·f;
        {
          syl_env 𝒰 = syl_env_rec(2);
          __·f = syl_mk_closure(__·f·λ, 𝒰);
          𝒰[0] = _a;
          𝒰[1] = __·f;
        }
        __ = 0;
      }
      return 0;
    }
    |}]
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
  [%expect
    {|
    static syl_int _a;
    static syl_int _b;
    static syl_int _c;
    static syl_int _f·λ(syl_int, syl_env);
    static syl_int _g·λ(syl_int, syl_env);
    static syl_closure _f;
    static syl_closure _g;
    static syl_int _f·λ(syl_int _x, syl_env 𝒰)
    {
      syl_closure _g = 𝒰[4];
      syl_int _b = 𝒰[1];
      syl_int _a = 𝒰[0];
      syl_int _f·λ·_ = _a;
      syl_int _f·λ·_ˢ1 = _b;
      return syl_app_closure(_g, _x);
    }
    static syl_int _g·λ(syl_int _y, syl_env 𝒰)
    {
      syl_closure _f = 𝒰[3];
      syl_int _c = 𝒰[2];
      syl_int _a = 𝒰[0];
      syl_int _g·λ·_ = _a;
      syl_int _g·λ·_ˢ1 = _c;
      return syl_app_closure(_f, _y);
    }
    int main()
    {
      _a = 0;
      _b = 1;
      _c = 2;
      {
        syl_env 𝒰 = syl_env_rec(5);
        _f = syl_mk_closure(_f·λ, 𝒰);
        _g = syl_mk_closure(_g·λ, 𝒰);
        𝒰[0] = _a;
        𝒰[1] = _b;
        𝒰[2] = _c;
        𝒰[3] = _f;
        𝒰[4] = _g;
      }
      return 0;
    }
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = let _ = g x in 0
and g (y : int) : erased int = f y;;
let _ = f 0;;
|};
  [%expect
    {|
    static syl_int _f·λ(syl_int, syl_env);
    static syl_closure _f;
    static syl_int __;
    static syl_int _f·λ(syl_int _x, syl_env 𝒰)
    {
      return 0;
    }
    int main()
    {
      {
        syl_env 𝒰 = NULL;
        _f = syl_mk_closure(_f·λ, 𝒰);
      }
      {
        syl_int _$ = 0;
        __ = syl_app_closure(_f, _$);
      }
      return 0;
    }
    |}]
;;

let%expect_test "erased fn" =
  go
    {|
fun erased f (x : int) : int = 0;;
let _ = f 0;;
|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      {
        syl_int __·x = 0;
        __ = 0;
      }
      return 0;
    }
    |}]
;;

let%expect_test "erased fn" =
  go
    {|
let f = fn erased (x : int) -> 0;;
let _ = f 0;;
|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      {
        syl_int __·x = 0;
        __ = 0;
      }
      return 0;
    }
    |}]
;;

let%expect_test "erased fn" =
  go
    {|
fun erased f (erased x : int) : int = 0;;
let _ = f 0;;
|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      __ = 0;
      return 0;
    }
    |}]
;;

let%expect_test "erased fn" =
  go
    {|
let f = fn erased (erased x : int) -> 0;;
let _ = f 0;;
|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      __ = 0;
      return 0;
    }
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = g x
and erased g (y : int) : int = if y == 0 then 0 else f (y-1);;
let _ = (f @ erased) 0;;
|};
  [%expect
    {|
    static syl_int _f·λ(syl_int, syl_env);
    static syl_closure _f;
    static syl_int __;
    static syl_int _f·λ(syl_int _x, syl_env 𝒰)
    {
      syl_closure _f = 𝒰[0];
      syl_int _f·λ·y = _x;
      syl_int _$ = 0;
      syl_int _f·λ·if;
      if(_f·λ·y == _$)
      {
        _f·λ·if = 0;
      }
      else
      {
        syl_int _$ = 1;
        syl_int _$ˢ1 = _f·λ·y - _$;
        _f·λ·if = syl_app_closure(_f, _$ˢ1);
      }
      return _f·λ·if;
    }
    int main()
    {
      {
        syl_env 𝒰 = syl_env_rec(1);
        _f = syl_mk_closure(_f·λ, 𝒰);
        𝒰[0] = _f;
      }
      {
        syl_int __·x = 0;
        syl_int __·y = __·x;
        syl_int _$ = 0;
        syl_int __·if;
        if(__·y == _$)
        {
          __·if = 0;
        }
        else
        {
          syl_int _$ = 1;
          syl_int _$ˢ1 = __·y - _$;
          __·if = syl_app_closure(_f, _$ˢ1);
        }
        __ = __·if;
      }
      return 0;
    }
    |}]
;;

let%expect_test "static lambda" =
  go
    {|
let _ = (fn (static x : int) -> x + 1) 0;;
|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      {
        syl_int __·x = 0;
        syl_int _$ = 1;
        __ = __·x + _$;
      }
      return 0;
    }
    |}]
;;

let%expect_test "pi function calling arrow function in same group" =
  go
    {|
fun inc (x : int) : int = x + 1
and f (static erased t : type) : t -> t = fn (x : t) -> x;;
let _ = f int 0;;
|};
  [%expect
    {|
    static syl_int _inc·λ(syl_int, syl_env);
    static syl_int _f·λₒ𝕀·λ(syl_int, syl_env);
    static syl_closure _f·λₒ𝕀(syl_env);
    static syl_closure _inc;
    static syl_thunk _fₒ𝕀;
    static syl_int __;
    static syl_int _inc·λ(syl_int _x, syl_env 𝒰)
    {
      syl_int _$ = 1;
      return _x + _$;
    }
    static syl_int _f·λₒ𝕀·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure _f·λₒ𝕀(syl_env 𝒰)
    {
      syl_env _f·λₒ𝕀·env = NULL;
      return syl_mk_closure(_f·λₒ𝕀·λ, _f·λₒ𝕀·env);
    }
    int main()
    {
      {
        syl_env 𝒰 = NULL;
        _inc = syl_mk_closure(_inc·λ, 𝒰);
        _fₒ𝕀 = syl_mk_thunk(_f·λₒ𝕀, 𝒰);
      }
      {
        syl_closure _$ = syl_app_thunk(_fₒ𝕀);
        syl_int _$ˢ1 = 0;
        __ = syl_app_closure(_$, _$ˢ1);
      }
      return 0;
    }
    |}]
;;

let%expect_test "arrow function calling pi function in same group" =
  go
    {|
fun f (static erased t : type) : t -> t = fn (x : t) -> x
and g (x : int) : int = f int x;;
let _ = g 5;;
|};
  [%expect
    {|
    static syl_int _f·λₒ𝕀·λ(syl_int, syl_env);
    static syl_closure _f·λₒ𝕀(syl_env);
    static syl_int _g·λ(syl_int, syl_env);
    static syl_closure _g;
    static syl_thunk _fₒ𝕀;
    static syl_int __;
    static syl_int _f·λₒ𝕀·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure _f·λₒ𝕀(syl_env 𝒰)
    {
      syl_env _f·λₒ𝕀·env = NULL;
      return syl_mk_closure(_f·λₒ𝕀·λ, _f·λₒ𝕀·env);
    }
    static syl_int _g·λ(syl_int _x, syl_env 𝒰)
    {
      syl_closure _fₒ𝕀 = 𝒰[0];
      syl_closure _$ = syl_app_thunk(_fₒ𝕀);
      return syl_app_closure(_$, _x);
    }
    int main()
    {
      {
        syl_env 𝒰 = syl_env_rec(1);
        _g = syl_mk_closure(_g·λ, 𝒰);
        _fₒ𝕀 = syl_mk_thunk(_f·λₒ𝕀, 𝒰);
        𝒰[0] = _fₒ𝕀;
      }
      {
        syl_int _$ = 5;
        __ = syl_app_closure(_g, _$);
      }
      return 0;
    }
    |}]
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
  [%expect
    {|
    static syl_int _inc·λ(syl_int, syl_env);
    static syl_int _choose·λₒF·λ(syl_int, syl_env);
    static syl_closure _choose·λₒF(syl_env);
    static syl_int _choose·λₒT·λ(syl_int, syl_env);
    static syl_closure _choose·λₒT(syl_env);
    static syl_closure _inc;
    static syl_thunk _chooseₒF;
    static syl_thunk _chooseₒT;
    static syl_int __;
    static syl_int __ˢ1;
    static syl_int _inc·λ(syl_int _x, syl_env 𝒰)
    {
      syl_int _$ = 1;
      return _x + _$;
    }
    static syl_int _choose·λₒF·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure _choose·λₒF(syl_env 𝒰)
    {
      syl_env _choose·λₒF·env = NULL;
      return syl_mk_closure(_choose·λₒF·λ, _choose·λₒF·env);
    }
    static syl_int _choose·λₒT·λ(syl_int _x, syl_env 𝒰)
    {
      syl_closure _inc = 𝒰[0];
      return syl_app_closure(_inc, _x);
    }
    static syl_closure _choose·λₒT(syl_env 𝒰)
    {
      syl_closure _inc = 𝒰[0];
      syl_env _choose·λₒT·env = syl_capture(1, _inc);
      return syl_mk_closure(_choose·λₒT·λ, _choose·λₒT·env);
    }
    int main()
    {
      {
        syl_env 𝒰 = syl_env_rec(1);
        _inc = syl_mk_closure(_inc·λ, 𝒰);
        _chooseₒF = syl_mk_thunk(_choose·λₒF, 𝒰);
        _chooseₒT = syl_mk_thunk(_choose·λₒT, 𝒰);
        𝒰[0] = _inc;
      }
      {
        syl_closure _$ = syl_app_thunk(_chooseₒT);
        syl_int _$ˢ1 = 5;
        __ = syl_app_closure(_$, _$ˢ1);
      }
      {
        syl_closure _$ = syl_app_thunk(_chooseₒF);
        syl_int _$ˢ1 = 5;
        __ˢ1 = syl_app_closure(_$, _$ˢ1);
      }
      return 0;
    }
    |}]
;;

let%expect_test "mutual pi recursion" =
  go
    {|
fun f (static erased t : type) : t -> t = fn (x : t) -> x
and g (static erased t : type) : t -> t = f t;;
let _ = g int 0;;
|};
  [%expect
    {|
    static syl_int _f·λₒ𝕀·λ(syl_int, syl_env);
    static syl_closure _f·λₒ𝕀(syl_env);
    static syl_closure _g·λₒ𝕀(syl_env);
    static syl_thunk _fₒ𝕀;
    static syl_thunk _gₒ𝕀;
    static syl_int __;
    static syl_int _f·λₒ𝕀·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure _f·λₒ𝕀(syl_env 𝒰)
    {
      syl_env _f·λₒ𝕀·env = NULL;
      return syl_mk_closure(_f·λₒ𝕀·λ, _f·λₒ𝕀·env);
    }
    static syl_closure _g·λₒ𝕀(syl_env 𝒰)
    {
      syl_closure _fₒ𝕀 = 𝒰[0];
      return syl_app_thunk(_fₒ𝕀);
    }
    int main()
    {
      {
        syl_env 𝒰 = syl_env_rec(1);
        _fₒ𝕀 = syl_mk_thunk(_f·λₒ𝕀, 𝒰);
        _gₒ𝕀 = syl_mk_thunk(_g·λₒ𝕀, 𝒰);
        𝒰[0] = _fₒ𝕀;
      }
      {
        syl_closure _$ = syl_app_thunk(_gₒ𝕀);
        syl_int _$ˢ1 = 0;
        __ = syl_app_closure(_$, _$ˢ1);
      }
      return 0;
    }
    |}]
;;

let%expect_test "static recursion with base case" =
  go
    {|
fun f (static x : int) : int = if static x == 0 then x else f (x - 1);;
let _ = f 3;;
|};
  [%expect
    {|
    static syl_int _f·λₒ1(syl_env);
    static syl_int _f·λₒ2(syl_env);
    static syl_int _f·λₒ3(syl_env);
    static syl_int _f·λₒ0(syl_env);
    static syl_thunk _fₒ1;
    static syl_thunk _fₒ2;
    static syl_thunk _fₒ3;
    static syl_thunk _fₒ0;
    static syl_int __;
    static syl_int _f·λₒ1(syl_env 𝒰)
    {
      syl_int _fₒ0 = 𝒰[0];
      syl_int _f·λₒ1·x = 1;
      return syl_app_thunk(_fₒ0);
    }
    static syl_int _f·λₒ2(syl_env 𝒰)
    {
      syl_int _fₒ1 = 𝒰[2];
      syl_int _f·λₒ2·x = 2;
      return syl_app_thunk(_fₒ1);
    }
    static syl_int _f·λₒ3(syl_env 𝒰)
    {
      syl_int _fₒ2 = 𝒰[1];
      syl_int _f·λₒ3·x = 3;
      return syl_app_thunk(_fₒ2);
    }
    static syl_int _f·λₒ0(syl_env 𝒰)
    {
      syl_int _f·λₒ0·x = 0;
      return _f·λₒ0·x;
    }
    int main()
    {
      {
        syl_env 𝒰 = syl_env_rec(3);
        _fₒ1 = syl_mk_thunk(_f·λₒ1, 𝒰);
        _fₒ2 = syl_mk_thunk(_f·λₒ2, 𝒰);
        _fₒ3 = syl_mk_thunk(_f·λₒ3, 𝒰);
        _fₒ0 = syl_mk_thunk(_f·λₒ0, 𝒰);
        𝒰[0] = _fₒ0;
        𝒰[1] = _fₒ2;
        𝒰[2] = _fₒ1;
      }
      __ = syl_app_thunk(_fₒ3);
      return 0;
    }
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (static x : int) : erased int = (if static x == 0 then 42 else f (x - 1)) @ erased;;
let _ = f 3;;
|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "arrow and pi mutual recursion with application" =
  go
    {|
fun double (x : int) : int = x + x
and apply_double (static erased t : type) : int -> int = fn (x : int) -> double x;;
let _ = apply_double int 5;;
|};
  [%expect
    {|
    static syl_int _double·λ(syl_int, syl_env);
    static syl_int _apply_double·λₒ𝕀·λ(syl_int, syl_env);
    static syl_closure _apply_double·λₒ𝕀(syl_env);
    static syl_closure _double;
    static syl_thunk _apply_doubleₒ𝕀;
    static syl_int __;
    static syl_int _double·λ(syl_int _x, syl_env 𝒰)
    {
      return _x + _x;
    }
    static syl_int _apply_double·λₒ𝕀·λ(syl_int _x, syl_env 𝒰)
    {
      syl_closure _double = 𝒰[0];
      return syl_app_closure(_double, _x);
    }
    static syl_closure _apply_double·λₒ𝕀(syl_env 𝒰)
    {
      syl_closure _double = 𝒰[0];
      syl_env _apply_double·λₒ𝕀·env = syl_capture(1, _double);
      return syl_mk_closure(_apply_double·λₒ𝕀·λ, _apply_double·λₒ𝕀·env);
    }
    int main()
    {
      {
        syl_env 𝒰 = syl_env_rec(1);
        _double = syl_mk_closure(_double·λ, 𝒰);
        _apply_doubleₒ𝕀 = syl_mk_thunk(_apply_double·λₒ𝕀, 𝒰);
        𝒰[0] = _double;
      }
      {
        syl_closure _$ = syl_app_thunk(_apply_doubleₒ𝕀);
        syl_int _$ˢ1 = 5;
        __ = syl_app_closure(_$, _$ˢ1);
      }
      return 0;
    }
    |}]
;;

let%expect_test "mutually recursive fun with static arg" =
  go
    {|
fun id1 (static erased t : type) : t -> t = fn (x : t) -> x
and id2 (static erased t : type) : t -> t = id1 t;;
|};
  [%expect
    {|
    int main()
    {
      {
        syl_env 𝒰 = NULL;
      }
      return 0;
    }
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = g x
and erased g (y : int) : int = y;;
let _ = f 0;;
|};
  [%expect
    {|
    static syl_int _f·λ(syl_int, syl_env);
    static syl_closure _f;
    static syl_int __;
    static syl_int _f·λ(syl_int _x, syl_env 𝒰)
    {
      syl_int _f·λ·y = _x;
      return _f·λ·y;
    }
    int main()
    {
      {
        syl_env 𝒰 = NULL;
        _f = syl_mk_closure(_f·λ, 𝒰);
      }
      {
        syl_int _$ = 0;
        __ = syl_app_closure(_f, _$);
      }
      return 0;
    }
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = (g @ erased) x
and g (y : int) : int = y;;
let _ = f 0;;
|};
  [%expect
    {|
    static syl_int _f·λ(syl_int, syl_env);
    static syl_int _g·λ(syl_int, syl_env);
    static syl_closure _f;
    static syl_closure _g;
    static syl_int __;
    static syl_int _f·λ(syl_int _x, syl_env 𝒰)
    {
      syl_int _f·λ·y = _x;
      return _f·λ·y;
    }
    static syl_int _g·λ(syl_int _y, syl_env 𝒰)
    {
      return _y;
    }
    int main()
    {
      {
        syl_env 𝒰 = NULL;
        _f = syl_mk_closure(_f·λ, 𝒰);
        _g = syl_mk_closure(_g·λ, 𝒰);
      }
      {
        syl_int _$ = 0;
        __ = syl_app_closure(_f, _$);
      }
      return 0;
    }
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : erased int = g x
and g (y : int) : erased int = y;;
let _ = f 0;;
|};
  [%expect
    {|
    int main()
    {
      return 0;
    }
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun erased f (x : int) : int = (g @ erased) x
and g (y : int) : int = y;;
let _ = f 0;;
|};
  [%expect
    {|
    static syl_int _g·λ(syl_int, syl_env);
    static syl_closure _g;
    static syl_int __;
    static syl_int _g·λ(syl_int _y, syl_env 𝒰)
    {
      return _y;
    }
    int main()
    {
      {
        syl_env 𝒰 = NULL;
        _g = syl_mk_closure(_g·λ, 𝒰);
      }
      {
        syl_int __·x = 0;
        syl_int __·y = __·x;
        __ = __·y;
      }
      return 0;
    }
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = if x == 0 then 0 else g (x - 1)
and g (y : int) : int = if y == 0 then 0 else f (y - 1);;
let _ = f 0;;
|};
  [%expect
    {|
    static syl_int _f·λ(syl_int, syl_env);
    static syl_int _g·λ(syl_int, syl_env);
    static syl_closure _f;
    static syl_closure _g;
    static syl_int __;
    static syl_int _f·λ(syl_int _x, syl_env 𝒰)
    {
      syl_closure _g = 𝒰[1];
      syl_int _$ = 0;
      syl_int _f·λ·if;
      if(_x == _$)
      {
        _f·λ·if = 0;
      }
      else
      {
        syl_int _$ = 1;
        syl_int _$ˢ1 = _x - _$;
        _f·λ·if = syl_app_closure(_g, _$ˢ1);
      }
      return _f·λ·if;
    }
    static syl_int _g·λ(syl_int _y, syl_env 𝒰)
    {
      syl_closure _f = 𝒰[0];
      syl_int _$ = 0;
      syl_int _g·λ·if;
      if(_y == _$)
      {
        _g·λ·if = 0;
      }
      else
      {
        syl_int _$ = 1;
        syl_int _$ˢ1 = _y - _$;
        _g·λ·if = syl_app_closure(_f, _$ˢ1);
      }
      return _g·λ·if;
    }
    int main()
    {
      {
        syl_env 𝒰 = syl_env_rec(2);
        _f = syl_mk_closure(_f·λ, 𝒰);
        _g = syl_mk_closure(_g·λ, 𝒰);
        𝒰[0] = _f;
        𝒰[1] = _g;
      }
      {
        syl_int _$ = 0;
        __ = syl_app_closure(_f, _$);
      }
      return 0;
    }
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = if x == 0 then 0 else g (x - 1)
and erased g (y : int) : int = if y == 0 then 0 else f (y - 1);;
let _ = (f @ erased) 0;;
|};
  [%expect
    {|
    static syl_int _f·λ(syl_int, syl_env);
    static syl_closure _f;
    static syl_int __;
    static syl_int _f·λ(syl_int _x, syl_env 𝒰)
    {
      syl_closure _f = 𝒰[0];
      syl_int _$ = 0;
      syl_int _f·λ·if;
      if(_x == _$)
      {
        _f·λ·if = 0;
      }
      else
      {
        syl_int _f·λ·if·y;
        {
          syl_int _$ = 1;
          _f·λ·if·y = _x - _$;
        }
        syl_int _$ = 0;
        syl_int _f·λ·if·if;
        if(_f·λ·if·y == _$)
        {
          _f·λ·if·if = 0;
        }
        else
        {
          syl_int _$ = 1;
          syl_int _$ˢ1 = _f·λ·if·y - _$;
          _f·λ·if·if = syl_app_closure(_f, _$ˢ1);
        }
        _f·λ·if = _f·λ·if·if;
      }
      return _f·λ·if;
    }
    int main()
    {
      {
        syl_env 𝒰 = syl_env_rec(1);
        _f = syl_mk_closure(_f·λ, 𝒰);
        𝒰[0] = _f;
      }
      {
        syl_int __·x = 0;
        syl_int _$ = 0;
        syl_int __·if;
        if(__·x == _$)
        {
          __·if = 0;
        }
        else
        {
          syl_int __·if·y;
          {
            syl_int _$ = 1;
            __·if·y = __·x - _$;
          }
          syl_int _$ = 0;
          syl_int __·if·if;
          if(__·if·y == _$)
          {
            __·if·if = 0;
          }
          else
          {
            syl_int _$ = 1;
            syl_int _$ˢ1 = __·if·y - _$;
            __·if·if = syl_app_closure(_f, _$ˢ1);
          }
          __·if = __·if·if;
        }
        __ = __·if;
      }
      return 0;
    }
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = if x == 0 then 0 else g (x - 1)
and g (y : int) : int = if y == 0 then 0 else (f @ erased) (y - 1);;
let _ = f 0;;
|};
  [%expect
    {|
    static syl_int _f·λ(syl_int, syl_env);
    static syl_int _g·λ(syl_int, syl_env);
    static syl_closure _f;
    static syl_closure _g;
    static syl_int __;
    static syl_int _f·λ(syl_int _x, syl_env 𝒰)
    {
      syl_closure _g = 𝒰[0];
      syl_int _$ = 0;
      syl_int _f·λ·if;
      if(_x == _$)
      {
        _f·λ·if = 0;
      }
      else
      {
        syl_int _$ = 1;
        syl_int _$ˢ1 = _x - _$;
        _f·λ·if = syl_app_closure(_g, _$ˢ1);
      }
      return _f·λ·if;
    }
    static syl_int _g·λ(syl_int _y, syl_env 𝒰)
    {
      syl_closure _g = 𝒰[0];
      syl_int _$ = 0;
      syl_int _g·λ·if;
      if(_y == _$)
      {
        _g·λ·if = 0;
      }
      else
      {
        syl_int _g·λ·if·x;
        {
          syl_int _$ = 1;
          _g·λ·if·x = _y - _$;
        }
        syl_int _$ = 0;
        syl_int _g·λ·if·if;
        if(_g·λ·if·x == _$)
        {
          _g·λ·if·if = 0;
        }
        else
        {
          syl_int _$ = 1;
          syl_int _$ˢ1 = _g·λ·if·x - _$;
          _g·λ·if·if = syl_app_closure(_g, _$ˢ1);
        }
        _g·λ·if = _g·λ·if·if;
      }
      return _g·λ·if;
    }
    int main()
    {
      {
        syl_env 𝒰 = syl_env_rec(1);
        _f = syl_mk_closure(_f·λ, 𝒰);
        _g = syl_mk_closure(_g·λ, 𝒰);
        𝒰[0] = _g;
      }
      {
        syl_int _$ = 0;
        __ = syl_app_closure(_f, _$);
      }
      return 0;
    }
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : erased int = g x
and g (y : int) : int = let _ = f y in 0;;
let _ = g 0;;
|};
  [%expect
    {|
    static syl_int _g·λ(syl_int, syl_env);
    static syl_closure _g;
    static syl_int __;
    static syl_int _g·λ(syl_int _y, syl_env 𝒰)
    {
      return 0;
    }
    int main()
    {
      {
        syl_env 𝒰 = NULL;
        _g = syl_mk_closure(_g·λ, 𝒰);
      }
      {
        syl_int _$ = 0;
        __ = syl_app_closure(_g, _$);
      }
      return 0;
    }
    |}]
;;

let%expect_test "erased fn" =
  go
    {|
fun erased f (x : int) : int = 0;;
let _ = f 0;;
|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      {
        syl_int __·x = 0;
        __ = 0;
      }
      return 0;
    }
    |}]
;;

let%expect_test "erased fn" =
  go
    {|
let f = fn erased (x : int) -> 0;;
let _ = f 0;;
|};
  [%expect
    {|
    static syl_int __;
    int main()
    {
      {
        syl_int __·x = 0;
        __ = 0;
      }
      return 0;
    }
    |}]
;;

let%expect_test "static int" =
  go
    {|
let f = fn (static x : int) -> x;;
let _ = f (f 1);;
|};
  [%expect
    {|
    static syl_int _f·λₒ1(syl_env);
    static syl_int _fₒ1;
    static syl_int __;
    static syl_int _f·λₒ1(syl_env 𝒰)
    {
      syl_int _f·λₒ1·x = 1;
      return _f·λₒ1·x;
    }
    int main()
    {
      {
        syl_env _f·env = NULL;
        _fₒ1 = syl_mk_thunk(_f·λₒ1, _f·env);
      }
      __ = syl_app_thunk(_fₒ1);
      return 0;
    }
    |}]
;;

let%expect_test "top-level mutual recursion with different bodies" =
  go
    {|
fun f (a : int) : int = g (a + 1)
and g (b : int) : int = b;;
let _ = f 0;;
|};
  [%expect
    {|
    static syl_int _f·λ(syl_int, syl_env);
    static syl_int _g·λ(syl_int, syl_env);
    static syl_closure _f;
    static syl_closure _g;
    static syl_int __;
    static syl_int _f·λ(syl_int _a, syl_env 𝒰)
    {
      syl_closure _g = 𝒰[0];
      syl_int _$ = 1;
      syl_int _$ˢ1 = _a + _$;
      return syl_app_closure(_g, _$ˢ1);
    }
    static syl_int _g·λ(syl_int _b, syl_env 𝒰)
    {
      return _b;
    }
    int main()
    {
      {
        syl_env 𝒰 = syl_env_rec(1);
        _f = syl_mk_closure(_f·λ, 𝒰);
        _g = syl_mk_closure(_g·λ, 𝒰);
        𝒰[0] = _g;
      }
      {
        syl_int _$ = 0;
        __ = syl_app_closure(_f, _$);
      }
      return 0;
    }
    |}]
;;

let%expect_test "top-level mutual static recursion" =
  go
    {|
fun f (static x : int) : int = if static x == 0 then 0 else g (x - 1)
and g (static y : int) : int = if static y == 0 then 1 else f (y - 1);;
let _ = f 2;;
|};
  [%expect
    {|
    static syl_int _f·λₒ2(syl_env);
    static syl_int _f·λₒ0(syl_env);
    static syl_int _g·λₒ1(syl_env);
    static syl_thunk _fₒ2;
    static syl_thunk _fₒ0;
    static syl_thunk _gₒ1;
    static syl_int __;
    static syl_int _f·λₒ2(syl_env 𝒰)
    {
      syl_int _gₒ1 = 𝒰[1];
      syl_int _f·λₒ2·x = 2;
      return syl_app_thunk(_gₒ1);
    }
    static syl_int _f·λₒ0(syl_env 𝒰)
    {
      syl_int _f·λₒ0·x = 0;
      return 0;
    }
    static syl_int _g·λₒ1(syl_env 𝒰)
    {
      syl_int _fₒ0 = 𝒰[0];
      syl_int _g·λₒ1·y = 1;
      return syl_app_thunk(_fₒ0);
    }
    int main()
    {
      {
        syl_env 𝒰 = syl_env_rec(2);
        _fₒ2 = syl_mk_thunk(_f·λₒ2, 𝒰);
        _fₒ0 = syl_mk_thunk(_f·λₒ0, 𝒰);
        _gₒ1 = syl_mk_thunk(_g·λₒ1, 𝒰);
        𝒰[0] = _fₒ0;
        𝒰[1] = _gₒ1;
      }
      __ = syl_app_thunk(_fₒ2);
      return 0;
    }
    |}]
;;

let%expect_test "static mutual recursion cross-monomorphization" =
  go
    {|
fun f (static erased t : type) : t -> t = g t
and g (static erased t : type) : t -> t = fn (x : t) -> x;;
let _ = f int 0;;
|};
  [%expect
    {|
    static syl_closure _f·λₒ𝕀(syl_env);
    static syl_int _g·λₒ𝕀·λ(syl_int, syl_env);
    static syl_closure _g·λₒ𝕀(syl_env);
    static syl_thunk _fₒ𝕀;
    static syl_thunk _gₒ𝕀;
    static syl_int __;
    static syl_closure _f·λₒ𝕀(syl_env 𝒰)
    {
      syl_closure _gₒ𝕀 = 𝒰[0];
      return syl_app_thunk(_gₒ𝕀);
    }
    static syl_int _g·λₒ𝕀·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure _g·λₒ𝕀(syl_env 𝒰)
    {
      syl_env _g·λₒ𝕀·env = NULL;
      return syl_mk_closure(_g·λₒ𝕀·λ, _g·λₒ𝕀·env);
    }
    int main()
    {
      {
        syl_env 𝒰 = syl_env_rec(1);
        _fₒ𝕀 = syl_mk_thunk(_f·λₒ𝕀, 𝒰);
        _gₒ𝕀 = syl_mk_thunk(_g·λₒ𝕀, 𝒰);
        𝒰[0] = _gₒ𝕀;
      }
      {
        syl_closure _$ = syl_app_thunk(_fₒ𝕀);
        syl_int _$ˢ1 = 0;
        __ = syl_app_closure(_$, _$ˢ1);
      }
      return 0;
    }
    |}]
;;

let%expect_test "static mutual recursion cross-monomorphization" =
  go
    {|
fun f (static erased t : type) : t -> t = fn (x : t) -> x
and g (static erased t : type) : t -> t = f t;;
let _ = g int 0;;
|};
  [%expect
    {|
    static syl_int _f·λₒ𝕀·λ(syl_int, syl_env);
    static syl_closure _f·λₒ𝕀(syl_env);
    static syl_closure _g·λₒ𝕀(syl_env);
    static syl_thunk _fₒ𝕀;
    static syl_thunk _gₒ𝕀;
    static syl_int __;
    static syl_int _f·λₒ𝕀·λ(syl_int _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure _f·λₒ𝕀(syl_env 𝒰)
    {
      syl_env _f·λₒ𝕀·env = NULL;
      return syl_mk_closure(_f·λₒ𝕀·λ, _f·λₒ𝕀·env);
    }
    static syl_closure _g·λₒ𝕀(syl_env 𝒰)
    {
      syl_closure _fₒ𝕀 = 𝒰[0];
      return syl_app_thunk(_fₒ𝕀);
    }
    int main()
    {
      {
        syl_env 𝒰 = syl_env_rec(1);
        _fₒ𝕀 = syl_mk_thunk(_f·λₒ𝕀, 𝒰);
        _gₒ𝕀 = syl_mk_thunk(_g·λₒ𝕀, 𝒰);
        𝒰[0] = _fₒ𝕀;
      }
      {
        syl_closure _$ = syl_app_thunk(_gₒ𝕀);
        syl_int _$ˢ1 = 0;
        __ = syl_app_closure(_$, _$ˢ1);
      }
      return 0;
    }
    |}]
;;

let%expect_test "local fun inside top-level fun" =
  go
    {|
fun outer (x : int) : int =
  fun inner (y : int) : int = y + x in
  inner x;;
let _ = outer 5;;
|};
  [%expect
    {|
    static syl_int _outer·λ·inner·λ(syl_int, syl_env);
    static syl_int _outer·λ(syl_int, syl_env);
    static syl_closure _outer;
    static syl_int __;
    static syl_int _outer·λ·inner·λ(syl_int _y, syl_env 𝒰)
    {
      syl_int _x = 𝒰[0];
      return _y + _x;
    }
    static syl_int _outer·λ(syl_int _x, syl_env 𝒰)
    {
      syl_closure _outer·λ·inner;
      {
        syl_env 𝒰 = syl_env_rec(1);
        _outer·λ·inner = syl_mk_closure(_outer·λ·inner·λ, 𝒰);
        𝒰[0] = _x;
      }
      return syl_app_closure(_outer·λ·inner, _x);
    }
    int main()
    {
      {
        syl_env 𝒰 = NULL;
        _outer = syl_mk_closure(_outer·λ, 𝒰);
      }
      {
        syl_int _$ = 5;
        __ = syl_app_closure(_outer, _$);
      }
      return 0;
    }
    |}]
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
  [%expect
    {|
    static syl_int _outer·λ·f·λ(syl_int, syl_env);
    static syl_int _outer·λ·g·λ(syl_int, syl_env);
    static syl_int _outer·λ(syl_int, syl_env);
    static syl_closure _outer;
    static syl_int __;
    static syl_int _outer·λ·f·λ(syl_int _a, syl_env 𝒰)
    {
      syl_int _x = 𝒰[2];
      syl_closure _g = 𝒰[1];
      syl_int _$ = 0;
      syl_int _outer·λ·f·λ·if;
      if(_a == _$)
      {
        _outer·λ·f·λ·if = 0;
      }
      else
      {
        syl_int _$ = _a + _x;
        _outer·λ·f·λ·if = syl_app_closure(_g, _$);
      }
      return _outer·λ·f·λ·if;
    }
    static syl_int _outer·λ·g·λ(syl_int _b, syl_env 𝒰)
    {
      syl_int _x = 𝒰[2];
      syl_closure _f = 𝒰[0];
      syl_int _$ = 0;
      syl_int _outer·λ·g·λ·if;
      if(_b == _$)
      {
        _outer·λ·g·λ·if = 0;
      }
      else
      {
        syl_int _$ = _b + _x;
        _outer·λ·g·λ·if = syl_app_closure(_f, _$);
      }
      return _outer·λ·g·λ·if;
    }
    static syl_int _outer·λ(syl_int _x, syl_env 𝒰)
    {
      syl_closure _outer·λ·f;
      syl_closure _outer·λ·g;
      {
        syl_env 𝒰 = syl_env_rec(3);
        _outer·λ·f = syl_mk_closure(_outer·λ·f·λ, 𝒰);
        _outer·λ·g = syl_mk_closure(_outer·λ·g·λ, 𝒰);
        𝒰[0] = _outer·λ·f;
        𝒰[1] = _outer·λ·g;
        𝒰[2] = _x;
      }
      syl_int _$ = 0;
      return syl_app_closure(_outer·λ·f, _$);
    }
    int main()
    {
      {
        syl_env 𝒰 = NULL;
        _outer = syl_mk_closure(_outer·λ, 𝒰);
      }
      {
        syl_int _$ = 5;
        __ = syl_app_closure(_outer, _$);
      }
      return 0;
    }
    |}]
;;

let%expect_test "monomorphizing side effects" =
  go
    {|
external print_int : int -> unit = syl_print_int;;
fun print (static _ : unit) : unit = print_int 0;;
let _ = print ();;
|};
  [%expect
    {|
    extern syl_unit syl_print_int(syl_int);
    static syl_unit _syl_print_int·λ(syl_int _, syl_env 𝒰)
    {
      return syl_print_int(_);
    }
    static syl_closure _print_int;
    static syl_unit _print·λₒø(syl_env);
    static syl_thunk _printₒø;
    static syl_unit __;
    static syl_unit _print·λₒø(syl_env 𝒰)
    {
      syl_closure _print_int = 𝒰[0];
      syl_unit _print·λₒø·_ = 0;
      syl_int _$ = 0;
      return syl_app_closure(_print_int, _$);
    }
    int main()
    {
      _print_int = syl_mk_closure(_syl_print_int·λ, SYL_ENV_EMPTY);
      {
        syl_env 𝒰 = syl_env_rec(1);
        _printₒø = syl_mk_thunk(_print·λₒø, 𝒰);
        𝒰[0] = _print_int;
      }
      __ = syl_app_thunk(_printₒø);
      return 0;
    }
    |}]
;;

let%expect_test "external" =
  go
    {|
external f : int -> unit = syl_print_int;;
let _ = f 0;;
|};
  [%expect
    {|
    extern syl_unit syl_print_int(syl_int);
    static syl_unit _syl_print_int·λ(syl_int _, syl_env 𝒰)
    {
      return syl_print_int(_);
    }
    static syl_closure _f;
    static syl_unit __;
    int main()
    {
      _f = syl_mk_closure(_syl_print_int·λ, SYL_ENV_EMPTY);
      {
        syl_int _$ = 0;
        __ = syl_app_closure(_f, _$);
      }
      return 0;
    }
    |}]
;;

let%expect_test "external" =
  go
    {|
external f : int -> unit = syl_print_int;;
let _ = (f @ erased) 0;;
|};
  [%expect
    {|
    extern syl_unit syl_print_int(syl_int);
    static syl_unit _syl_print_int·λ(syl_int _, syl_env 𝒰)
    {
      return syl_print_int(_);
    }
    static syl_closure _f;
    extern syl_unit syl_print_int(syl_int);
    static syl_unit __·syl_print_int·λ(syl_int _, syl_env 𝒰)
    {
      return syl_print_int(_);
    }
    static syl_unit __;
    int main()
    {
      _f = syl_mk_closure(_syl_print_int·λ, SYL_ENV_EMPTY);
      {
        syl_closure _$ = syl_mk_closure(__·syl_print_int·λ, SYL_ENV_EMPTY);
        syl_int _$ˢ1 = 0;
        __ = syl_app_closure(_$, _$ˢ1);
      }
      return 0;
    }
    |}]
;;

let%expect_test "static lambda effects" =
  go
    {|
external print_int : int -> unit = syl_print_int;;
let print = fn (static x : int) -> print_int x;;
let _ = print 0;;
let _ = print 1;;
|};
  [%expect
    {|
    extern syl_unit syl_print_int(syl_int);
    static syl_unit _syl_print_int·λ(syl_int _, syl_env 𝒰)
    {
      return syl_print_int(_);
    }
    static syl_closure _print_int;
    static syl_unit _print·λₒ1(syl_env);
    static syl_unit _print·λₒ0(syl_env);
    static syl_unit _printₒ1;
    static syl_unit _printₒ0;
    static syl_unit __;
    static syl_unit __ˢ1;
    static syl_unit _print·λₒ1(syl_env 𝒰)
    {
      syl_closure _print_int = 𝒰[0];
      syl_int _print·λₒ1·x = 1;
      return syl_app_closure(_print_int, _print·λₒ1·x);
    }
    static syl_unit _print·λₒ0(syl_env 𝒰)
    {
      syl_closure _print_int = 𝒰[0];
      syl_int _print·λₒ0·x = 0;
      return syl_app_closure(_print_int, _print·λₒ0·x);
    }
    int main()
    {
      _print_int = syl_mk_closure(_syl_print_int·λ, SYL_ENV_EMPTY);
      {
        syl_env _print·env = syl_capture(1, _print_int);
        _printₒ1 = syl_mk_thunk(_print·λₒ1, _print·env);
        _printₒ0 = syl_mk_thunk(_print·λₒ0, _print·env);
      }
      __ = syl_app_thunk(_printₒ0);
      __ˢ1 = syl_app_thunk(_printₒ1);
      return 0;
    }
    |}]
;;

let%expect_test "static erased lambda" =
  go
    {|
fun apply (static erased f : int -> int) : int = f 0;;
let x = apply (fn (x : int) -> x + 1);;
|};
  [%expect
    {|
    static syl_int _apply·λₒλ2(syl_env);
    static syl_thunk _applyₒλ2;
    static syl_int _x;
    static syl_int _apply·λₒλ2(syl_env 𝒰)
    {
      syl_int _apply·λₒλ2·x = 0;
      syl_int _$ = 1;
      return _apply·λₒλ2·x + _$;
    }
    int main()
    {
      {
        syl_env 𝒰 = NULL;
        _applyₒλ2 = syl_mk_thunk(_apply·λₒλ2, 𝒰);
      }
      _x = syl_app_thunk(_applyₒλ2);
      return 0;
    }
    |}]
;;

let%expect_test "static lambda effects" =
  go
    {|
fun mk_ident (static erased pick_t : static unit -> static erased type) : static (pick_t () -> pick_t ()) =
  fn (x : pick_t ()) -> x
;;

let _ = mk_ident (fn (static _ : unit) -> if 1 + 1 == 2 then bool else unit) true;;
|};
  [%expect
    {|
    static syl_bool _mk_ident·λₒλ6·λ(syl_bool, syl_env);
    static syl_closure _mk_ident·λₒλ6(syl_env);
    static syl_thunk _mk_identₒλ6;
    static syl_bool __;
    static syl_bool _mk_ident·λₒλ6·λ(syl_bool _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure _mk_ident·λₒλ6(syl_env 𝒰)
    {
      syl_env _mk_ident·λₒλ6·env = NULL;
      return syl_mk_closure(_mk_ident·λₒλ6·λ, _mk_ident·λₒλ6·env);
    }
    int main()
    {
      {
        syl_env 𝒰 = NULL;
        _mk_identₒλ6 = syl_mk_thunk(_mk_ident·λₒλ6, 𝒰);
      }
      {
        syl_closure _$ = syl_app_thunk(_mk_identₒλ6);
        syl_bool _$ˢ1 = true;
        __ = syl_app_closure(_$, _$ˢ1);
      }
      return 0;
    }
    |}]
;;

let%expect_test "static lambda effects" =
  go
    {|
fun mk_ident (static pick_t : static unit -> static int) : static (let t = if pick_t () == 0 then int else bool in t -> t) =
  fn (x : if pick_t () == 0 then int else bool) -> x
;;

let _ = mk_ident (fn (static _ : unit) -> 1) true;;
|};
  [%expect
    {|
    static syl_int _mk_ident·λₒλ6·pick_t·λₒø(syl_env);
    static syl_bool _mk_ident·λₒλ6·λ(syl_bool, syl_env);
    static syl_closure _mk_ident·λₒλ6(syl_env);
    static syl_thunk _mk_identₒλ6;
    static syl_bool __;
    static syl_int _mk_ident·λₒλ6·pick_t·λₒø(syl_env 𝒰)
    {
      syl_unit _mk_ident·λₒλ6·pick_t·λₒø·_ = 0;
      return 1;
    }
    static syl_bool _mk_ident·λₒλ6·λ(syl_bool _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure _mk_ident·λₒλ6(syl_env 𝒰)
    {
      syl_int _mk_ident·λₒλ6·pick_tₒø;
      {
        syl_env _mk_ident·λₒλ6·pick_t·env = NULL;
        _mk_ident·λₒλ6·pick_tₒø = syl_mk_thunk(_mk_ident·λₒλ6·pick_t·λₒø, _mk_ident·λₒλ6·pick_t·env);
      }
      syl_env _mk_ident·λₒλ6·env = NULL;
      return syl_mk_closure(_mk_ident·λₒλ6·λ, _mk_ident·λₒλ6·env);
    }
    int main()
    {
      {
        syl_env 𝒰 = NULL;
        _mk_identₒλ6 = syl_mk_thunk(_mk_ident·λₒλ6, 𝒰);
      }
      {
        syl_closure _$ = syl_app_thunk(_mk_identₒλ6);
        syl_bool _$ˢ1 = true;
        __ = syl_app_closure(_$, _$ˢ1);
      }
      return 0;
    }
    |}]
;;

let%expect_test "static lambda effects are thunked" =
  go
    {|
external print_int : int -> unit = syl_print_int;;

fun mk_ident (static pick_t : static unit -> static int) : static (let t = if pick_t () == 0 then int else bool in t -> t) =
  let _ = pick_t () in
  let _ = pick_t () in
  fn (x : if pick_t () == 0 then int else bool) -> x
;;

let _ = mk_ident (fn (static _ : unit) -> let _ = print_int 10 in 1) true;;
|};
  [%expect
    {|
    extern syl_unit syl_print_int(syl_int);
    static syl_unit _syl_print_int·λ(syl_int _, syl_env 𝒰)
    {
      return syl_print_int(_);
    }
    static syl_closure _print_int;
    static syl_int _mk_ident·λₒλ6·pick_t·λₒø(syl_env);
    static syl_bool _mk_ident·λₒλ6·λ(syl_bool, syl_env);
    static syl_closure _mk_ident·λₒλ6(syl_env);
    static syl_thunk _mk_identₒλ6;
    static syl_bool __;
    static syl_int _mk_ident·λₒλ6·pick_t·λₒø(syl_env 𝒰)
    {
      syl_closure _print_int = 𝒰[0];
      syl_unit _mk_ident·λₒλ6·pick_t·λₒø·_ = 0;
      syl_unit _mk_ident·λₒλ6·pick_t·λₒø·_ˢ1;
      {
        syl_int _$ = 10;
        _mk_ident·λₒλ6·pick_t·λₒø·_ˢ1 = syl_app_closure(_print_int, _$);
      }
      return 1;
    }
    static syl_bool _mk_ident·λₒλ6·λ(syl_bool _x, syl_env 𝒰)
    {
      return _x;
    }
    static syl_closure _mk_ident·λₒλ6(syl_env 𝒰)
    {
      syl_closure _print_int = 𝒰[0];
      syl_int _mk_ident·λₒλ6·pick_tₒø;
      {
        syl_env _mk_ident·λₒλ6·pick_t·env = syl_capture(1, _print_int);
        _mk_ident·λₒλ6·pick_tₒø = syl_mk_thunk(_mk_ident·λₒλ6·pick_t·λₒø, _mk_ident·λₒλ6·pick_t·env);
      }
      syl_int _mk_ident·λₒλ6·_ = syl_app_thunk(_mk_ident·λₒλ6·pick_tₒø);
      syl_int _mk_ident·λₒλ6·_ˢ1 = syl_app_thunk(_mk_ident·λₒλ6·pick_tₒø);
      syl_env _mk_ident·λₒλ6·env = NULL;
      return syl_mk_closure(_mk_ident·λₒλ6·λ, _mk_ident·λₒλ6·env);
    }
    int main()
    {
      _print_int = syl_mk_closure(_syl_print_int·λ, SYL_ENV_EMPTY);
      {
        syl_env 𝒰 = syl_env_rec(1);
        _mk_identₒλ6 = syl_mk_thunk(_mk_ident·λₒλ6, 𝒰);
        𝒰[0] = _print_int;
      }
      {
        syl_closure _$ = syl_app_thunk(_mk_identₒλ6);
        syl_bool _$ˢ1 = true;
        __ = syl_app_closure(_$, _$ˢ1);
      }
      return 0;
    }
    |}]
;;

let%expect_test "external" =
  go
    {|
external print_int : int -> unit = syl_print_int;;
|};
  [%expect
    {|
    extern syl_unit syl_print_int(syl_int);
    static syl_unit _syl_print_int·λ(syl_int _, syl_env 𝒰)
    {
      return syl_print_int(_);
    }
    static syl_closure _print_int;
    int main()
    {
      _print_int = syl_mk_closure(_syl_print_int·λ, SYL_ENV_EMPTY);
      return 0;
    }
    |}]
;;

let%expect_test "scoping" =
  go
    {|
let x = 1 @ dynamic;;
let _ = let _ = let _ = x + x in x + x in x + x;;
|};
  [%expect
    {|
    static syl_int _x;
    static syl_int __;
    int main()
    {
      _x = 1;
      {
        syl_int __·_;
        {
          syl_int __·_·_ = _x + _x;
          __·_ = _x + _x;
        }
        __ = _x + _x;
      }
      return 0;
    }
    |}]
;;

let%expect_test "scoping" =
  go
    {|
let c = true @ dynamic;;
let _ = if c then 0 else if !c then 1 else 2;;
|};
  [%expect
    {|
    static syl_bool _c;
    static syl_int __;
    int main()
    {
      _c = true;
      {
        syl_int __·if;
        if(_c)
        {
          __·if = 0;
        }
        else
        {
          syl_int __·if·if;
          if(!_c)
          {
            __·if·if = 1;
          }
          else
          {
            __·if·if = 2;
          }
          __·if = __·if·if;
        }
        __ = __·if;
      }
      return 0;
    }
    |}]
;;
