open! Core
open! Syl

let strip_preamble c =
  let c_preamble_end = "// SYL_PREAMBLE_END" in
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
  let ir = Simplify.simplify tst in
  match Codegen.codegen ir with
  | c -> print_endline (strip_preamble c)
  | exception exn -> print_s [%message (exn : exn)]
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
    int main(void) {
        (void)0;
        (void)1;
        (void)123LL;
        (void)0;
        (void)1;
        (void)123LL;
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
    int main(void) {
        (void)1LL;
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
    int main(void) {
        (void)1LL;
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
    static val_t _dyn_0;
    int main(void) {
        _dyn_0 = 1LL;
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
    int main(void) {
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
    static val_t _dyn_0;
    int main(void) {
        _dyn_0 = 1LL;
        (void)_dyn_0;
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
    static val_t _dyn_0;
    int main(void) {
        _dyn_0 = 1LL;
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
    int main(void) {
        (void)0;
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
    int main(void) {
        (void)0;
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
    int main(void) {
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
    int main(void) {
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
    int main(void) {
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
    static val_t _dyn_0;
    int main(void) {
        _dyn_0 = 1;
        (void)(!_dyn_0);
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
    int main(void) {
        (void)0;
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
    int main(void) {
        (void)1;
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
    int main(void) {
        (void)1;
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
    static val_t _dyn_0;
    int main(void) {
        _dyn_0 = 1;
        (void)(!_dyn_0);
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
    static val_t _dyn_0;
    int main(void) {
        _dyn_0 = 1;
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
    int main(void) {
        (void)3LL;
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
    int main(void) {
        (void)3LL;
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
    int main(void) {
        (void)3LL;
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
    int main(void) {
        (void)6LL;
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
    int main(void) {
        (void)6LL;
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
    static val_t _dyn_0;
    int main(void) {
        _dyn_0 = 2LL;
        (void)(1LL + _dyn_0);
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
    static val_t _dyn_0;
    int main(void) {
        _dyn_0 = 1LL;
        (void)(_dyn_0 + 2LL);
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
    static val_t _dyn1_0;
    static val_t _dyn2_0;
    int main(void) {
        _dyn1_0 = 1LL;
        _dyn2_0 = 2LL;
        (void)(_dyn1_0 + _dyn2_0);
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
    static val_t _dyn2_0;
    int main(void) {
        _dyn2_0 = 2LL;
        (void)(1LL + _dyn2_0);
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
    int main(void) {
        (void)3LL;
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
    int main(void) {
        (void)1LL;
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
    int main(void) {
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
    int main(void) {
        (void)1LL;
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
    int main(void) {
        (void)1LL;
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
    static val_t _dyn_0;
    int main(void) {
        _dyn_0 = 1LL;
        (void)_dyn_0;
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
    static val_t _dyn_0;
    int main(void) {
        _dyn_0 = 1;
        val_t _c0 = _dyn_0;
        val_t _if0;
        if (_c0) {
            _if0 = 1LL;
        } else {
            _if0 = 2LL;
        }
        (void)_if0;
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
    static val_t _x_0;
    int main(void) {
        _x_0 = 1;
        (void)1LL;
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
    int main(void) {
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
    static val_t _cond_0;
    int main(void) {
        _cond_0 = 1;
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
    static val_t _cond_0;
    int main(void) {
        _cond_0 = 1;
        return 0;
    }
    |}]
;;

let%expect_test "Let static" =
  go
    {|
let _ =
  let x = 1;
  x
;;|};
  [%expect
    {|
    int main(void) {
        val_t _x_0 = 1LL;
        (void)_x_0;
        return 0;
    }
    |}]
;;

let%expect_test "Let dynamic" =
  go
    {|
let dyn = 1 @ dynamic;;
let _ =
  let x = dyn;
  x
;;|};
  [%expect
    {|
    static val_t _dyn_0;
    int main(void) {
        _dyn_0 = 1LL;
        val_t _x_0 = _dyn_0;
        (void)_x_0;
        return 0;
    }
    |}]
;;

let%expect_test "Let dynamic" =
  go
    {|
let dyn = 1 @ erased;;
let _ =
  let x = dyn + 1 @ dynamic;
  x
;;|};
  [%expect
    {|
    int main(void) {
        val_t _x_0 = 2LL;
        (void)_x_0;
        return 0;
    }
    |}]
;;

let%expect_test "Let erased" =
  go
    {|
let dyn = 1 @ erased;;
let _ =
  let x = dyn;
  x
;;|};
  [%expect
    {|
    int main(void) {
        return 0;
    }
    |}]
;;

let%expect_test "Let erased" =
  go
    {|
let _ =
  let x = 1 @ erased;
  x + 1
;;|};
  [%expect
    {|
    int main(void) {
        (void)2LL;
        return 0;
    }
    |}]
;;

let%expect_test "Let erased" =
  go
    {|
let _ =
  let x = 1 @ erased;
  let y = 1 @ dynamic;
  x + y
;;|};
  [%expect
    {|
    int main(void) {
        val_t _y_0 = 1LL;
        (void)(1LL + _y_0);
        return 0;
    }
    |}]
;;

let%expect_test "Let erased" =
  go
    {|
let _ =
  let x = 1 @ erased;
  let y = 1 @ erased;
  0 + (x + y)
;;|};
  [%expect
    {|
    int main(void) {
        (void)2LL;
        return 0;
    }
    |}]
;;

let%expect_test "Let erased" =
  go
    {|
let _ =
  let x = 1;
  let y = 1;
  0 + ((x + y) @ erased)
;;|};
  [%expect
    {|
    int main(void) {
        val_t _x_0 = 1LL;
        val_t _y_0 = 1LL;
        (void)2LL;
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return _x_0;
    }

    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        (void)(val_t)_clo0;
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
    int main(void) {
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
    int main(void) {
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
    static val_t _y_0;
    static val_t _lam0(val_t* _env, val_t _x_0) {
        val_t _y_1 = _env[0];
        return (_x_0 + _y_1);
    }

    int main(void) {
        _y_0 = 1LL;
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = (val_t*)malloc(1 * sizeof(val_t));
        _clo0->env[0] = _y_0;
        (void)(val_t)_clo0;
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
    int main(void) {
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
    int main(void) {
        (void)1LL;
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return 1LL;
    }

    static val_t _f_0;
    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        _f_0 = (val_t)_clo0;
        val_t _r0 = syl_call(_f_0, 0LL);
        (void)_r0;
        return 0;
    }
    |}]
;;

let%expect_test "erased closure arg" =
  go
    {|
let f = fn (static erased g : int -> erased int) -> let _ = g 1; 2;;
let _ = f (fn (x : int) -> 0 @ erased);;
|};
  [%expect
    {|
    static val_t _f__l1_fn0(void) {
        return 2LL;
    }

    int main(void) {
        (void)_f__l1_fn0();
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return 1LL;
    }

    static val_t _f_0;
    static val_t _lam1(val_t* _env, val_t _x_1) {
        return (_x_1 + 1LL);
    }

    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        _f_0 = (val_t)_clo0;
        closure_t* _clo1 = (closure_t*)malloc(sizeof(closure_t));
        _clo1->fn = _lam1;
        _clo1->env = NULL;
        val_t _r0 = syl_call(_f_0, (val_t)_clo1);
        (void)_r0;
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
    int main(void) {
        (void)1LL;
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
    int main(void) {
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return _x_0;
    }

    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        (void)(val_t)_clo0;
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
    static val_t _f_0;
    static val_t _f_fn0(val_t* _env, val_t _arg);

    static val_t _f_fn0(val_t* _env, val_t _x_0) {
        return _x_0;
    }

    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _f_fn0;
            _c->env = NULL;
            _f_0 = (val_t)_c;
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
    int main(void) {
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
    int main(void) {
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
    int main(void) {
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
    int main(void) {
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
    int main(void) {
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
    static val_t _x_1;
    int main(void) {
        val_t _x_0 = 0LL;
        _x_1 = 1LL;
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
    static val_t _x_0;
    int main(void) {
        _x_0 = 1LL;
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return 1LL;
    }

    static val_t _f_0;
    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        _f_0 = (val_t)_clo0;
        (void)_f_0;
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
    static val_t _f__0_fn0(void) {
        val_t _x_0 = 0LL;
        return 1LL;
    }

    int main(void) {
        (void)_f__0_fn0();
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
    int main(void) {
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
    static val_t _lam0(val_t* _env, val_t _a_0) {
        return 1;
    }

    static val_t _c_0;
    static val_t _lam1(val_t* _env, val_t _x_0) {
        return 1LL;
    }

    static val_t _f_0;
    static val_t _lam2(val_t* _env, val_t _x_1) {
        return 2LL;
    }

    static val_t _g_0;
    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        _c_0 = (val_t)_clo0;
        closure_t* _clo1 = (closure_t*)malloc(sizeof(closure_t));
        _clo1->fn = _lam1;
        _clo1->env = NULL;
        _f_0 = (val_t)_clo1;
        closure_t* _clo2 = (closure_t*)malloc(sizeof(closure_t));
        _clo2->fn = _lam2;
        _clo2->env = NULL;
        _g_0 = (val_t)_clo2;
        val_t _r0 = syl_call(_c_0, 0);
        val_t _c0 = _r0;
        val_t _if0;
        if (_c0) {
            _if0 = _f_0;
        } else {
            _if0 = _g_0;
        }
        val_t _r1 = syl_call(_if0, 0LL);
        (void)_r1;
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return 1LL;
    }

    static val_t _f_0;
    static val_t _lam1(val_t* _env, val_t _f_1) {
        val_t _r0 = syl_call(_f_1, 0LL);
        return _r0;
    }

    static val_t _g_0;
    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        _f_0 = (val_t)_clo0;
        closure_t* _clo1 = (closure_t*)malloc(sizeof(closure_t));
        _clo1->fn = _lam1;
        _clo1->env = NULL;
        _g_0 = (val_t)_clo1;
        val_t _r1 = syl_call(_g_0, _f_0);
        (void)_r1;
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return 1LL;
    }

    static val_t _f_0;
    static val_t _lam1(val_t* _env, val_t _x_1) {
        return 1LL;
    }

    static val_t _g__l1_fn0(void) {
        closure_t* _clo1 = (closure_t*)malloc(sizeof(closure_t));
        _clo1->fn = _lam1;
        _clo1->env = NULL;
        val_t _f_1 = (val_t)_clo1;
        val_t _r0 = syl_call(_f_1, 0LL);
        return _r0;
    }

    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        _f_0 = (val_t)_clo0;
        (void)_g__l1_fn0();
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return 1LL;
    }

    static val_t _f_0;
    static val_t _lam1(val_t* _env, val_t _x_1) {
        return 1LL;
    }

    static val_t _g__l1_fn0(void) {
        closure_t* _clo1 = (closure_t*)malloc(sizeof(closure_t));
        _clo1->fn = _lam1;
        _clo1->env = NULL;
        val_t _f_1 = (val_t)_clo1;
        return 1LL;
    }

    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        _f_0 = (val_t)_clo0;
        (void)_g__l1_fn0();
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
    static val_t _g__l1_fn0(void) {
        val_t _x_0 = 0LL;
        return 1LL;
    }

    int main(void) {
        (void)_g__l1_fn0();
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return 1LL;
    }

    static val_t _f1_0;
    static val_t _lam1(val_t* _env, val_t _f2_0) {
        val_t _r0 = syl_call(_f2_0, 0LL);
        return _r0;
    }

    static val_t _g_0;
    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        _f1_0 = (val_t)_clo0;
        closure_t* _clo1 = (closure_t*)malloc(sizeof(closure_t));
        _clo1->fn = _lam1;
        _clo1->env = NULL;
        _g_0 = (val_t)_clo1;
        val_t _r1 = syl_call(_g_0, _f1_0);
        (void)_r1;
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return 1LL;
    }

    static val_t _f1_0;
    static val_t _lam1(val_t* _env, val_t _x_1) {
        return 1LL;
    }

    static val_t _g__l1_fn0(void) {
        closure_t* _clo1 = (closure_t*)malloc(sizeof(closure_t));
        _clo1->fn = _lam1;
        _clo1->env = NULL;
        val_t _f2_0 = (val_t)_clo1;
        val_t _r0 = syl_call(_f2_0, 0LL);
        return _r0;
    }

    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        _f1_0 = (val_t)_clo0;
        (void)_g__l1_fn0();
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
    static val_t _g__l1_fn0(void) {
        return 1LL;
    }

    int main(void) {
        (void)_g__l1_fn0();
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return 1LL;
    }

    static val_t _f1_0;
    static val_t _g__l1_fn0(void) {
        return 1LL;
    }

    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        _f1_0 = (val_t)_clo0;
        (void)_g__l1_fn0();
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
    static val_t _x_1;
    int main(void) {
        val_t _x_0 = 0LL;
        _x_1 = _x_0;
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
    static val_t _x_0;
    int main(void) {
        _x_0 = 1LL;
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
    int main(void) {
        (void)0LL;
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
    int main(void) {
        (void)0LL;
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
    int main(void) {
        val_t _x_0 = 1LL;
        (void)_x_0;
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
    int main(void) {
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
    static val_t _dyn_0;
    int main(void) {
        _dyn_0 = 1LL;
        val_t _x_0 = _dyn_0;
        (void)_x_0;
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
    static val_t _dyn_0;
    static val_t _y_0;
    int main(void) {
        _dyn_0 = 1LL;
        val_t _x_0 = (_dyn_0 - 1LL);
        _y_0 = 5LL;
        (void)_y_0;
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
    static val_t _y_0;
    int main(void) {
        val_t _x_0 = 1LL;
        val_t _x_1 = 1LL;
        _y_0 = 5LL;
        (void)_y_0;
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
    static val_t _dyn_0;
    static val_t _y_0;
    int main(void) {
        _dyn_0 = 1LL;
        val_t _x_0 = (_dyn_0 - 1LL);
        _y_0 = 5LL;
        (void)_y_0;
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return _x_0;
    }

    static val_t _dyn_fn_0;
    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        _dyn_fn_0 = (val_t)_clo0;
        val_t _r0 = syl_call(_dyn_fn_0, 1LL);
        (void)_r0;
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return _x_0;
    }

    static val_t _dyn_fn_0;
    static val_t _dyn_arg_0;
    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        _dyn_fn_0 = (val_t)_clo0;
        _dyn_arg_0 = 1LL;
        val_t _r0 = syl_call(_dyn_fn_0, _dyn_arg_0);
        (void)_r0;
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return _x_0;
    }

    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        (void)(val_t)_clo0;
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
    int main(void) {
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
    int main(void) {
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
    static val_t _x_0;
    static val_t _lam0(val_t* _env, val_t _y_0) {
        val_t _x_1 = _env[0];
        return _x_1;
    }

    int main(void) {
        _x_0 = 1LL;
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = (val_t*)malloc(1 * sizeof(val_t));
        _clo0->env[0] = _x_0;
        (void)(val_t)_clo0;
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
    static val_t _x_0;
    static val_t _lam0(val_t* _env, val_t _y_0) {
        val_t _x_1 = _env[0];
        return _x_1;
    }

    int main(void) {
        _x_0 = 1LL;
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = (val_t*)malloc(1 * sizeof(val_t));
        _clo0->env[0] = _x_0;
        (void)(val_t)_clo0;
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
    int main(void) {
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
    int main(void) {
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return (_x_0 + 1LL);
    }

    static val_t _g_0;
    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        _g_0 = (val_t)_clo0;
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
    int main(void) {
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
    int main(void) {
        (void)0LL;
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
    int main(void) {
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
    int main(void) {
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
    int main(void) {
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
    int main(void) {
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
    int main(void) {
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
    int main(void) {
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
    static val_t _lam0(val_t* _env, val_t _a_0) {
        val_t _x_1 = _env[0];
        return _x_1;
    }

    static val_t _f__1_fn0(void) {
        val_t _x_0 = 1LL;
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = (val_t*)malloc(1 * sizeof(val_t));
        _clo0->env[0] = _x_0;
        return (val_t)_clo0;
    }

    static val_t _g_0;
    int main(void) {
        val_t _r0 = syl_call(_f__1_fn0(), 0);
        _g_0 = _r0;
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
    int main(void) {
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
    int main(void) {
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return _x_0;
    }

    static val_t _f__B_fn0(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        return (val_t)_clo0;
    }

    static val_t _lam1(val_t* _env, val_t _x_1) {
        return _x_1;
    }

    static val_t _f__I_fn0(void) {
        closure_t* _clo1 = (closure_t*)malloc(sizeof(closure_t));
        _clo1->fn = _lam1;
        _clo1->env = NULL;
        return (val_t)_clo1;
    }

    static val_t _g_0;
    static val_t _g_1;
    int main(void) {
        _g_0 = _f__I_fn0();
        val_t _r0 = syl_call(_g_0, 0LL);
        (void)_r0;
        _g_1 = _f__B_fn0();
        val_t _r1 = syl_call(_g_1, 1);
        (void)_r1;
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return (_x_0 + 1LL);
    }

    static val_t _f__l1_fn0(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        val_t _g_0 = (val_t)_clo0;
        val_t _r0 = syl_call(_g_0, 0LL);
        return _r0;
    }

    int main(void) {
        (void)_f__l1_fn0();
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
    static val_t _f__u_fn0(void) {
        val_t _x_0 = 0;
        return 0;
    }

    int main(void) {
        (void)_f__u_fn0();
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
    static val_t _f__t_fn0(void) {
        val_t _x_0 = 1;
        return (!_x_0);
    }

    int main(void) {
        (void)_f__t_fn0();
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
    static val_t _f__1_fn0(void) {
        val_t _x_0 = 1LL;
        return (-_x_0);
    }

    int main(void) {
        (void)_f__1_fn0();
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
    int main(void) {
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
    static val_t _lam0(val_t* _env, val_t _g_0) {
        val_t _r0 = syl_call(_g_0, 0LL);
        return _r0;
    }

    static val_t _f_0;
    static val_t _lam1(val_t* _env, val_t _x_0) {
        return 0LL;
    }

    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        _f_0 = (val_t)_clo0;
        closure_t* _clo1 = (closure_t*)malloc(sizeof(closure_t));
        _clo1->fn = _lam1;
        _clo1->env = NULL;
        val_t _r1 = syl_call(_f_0, (val_t)_clo1);
        (void)_r1;
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
    static val_t _f__l4_fn0(void) {
        val_t _x_0 = 0LL;
        val_t _g__0_0 = 0LL;
        return _g__0_0;
    }

    int main(void) {
        (void)_f__l4_fn0();
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return 0LL;
    }

    static val_t _f__l3_fn0(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        val_t _g_0 = (val_t)_clo0;
        val_t _r0 = syl_call(_g_0, 0LL);
        return _r0;
    }

    int main(void) {
        (void)_f__l3_fn0();
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
    int main(void) {
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
    int main(void) {
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
    int main(void) {
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
    int main(void) {
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return _x_0;
    }

    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        (void)(val_t)_clo0;
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
    int main(void) {
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
    int main(void) {
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
    int main(void) {
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
    int main(void) {
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return 0LL;
    }

    static val_t _f__l1_fn0(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        val_t _g_0 = (val_t)_clo0;
        val_t _r0 = syl_call(_g_0, 0LL);
        return _r0;
    }

    int main(void) {
        (void)_f__l1_fn0();
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return 0LL;
    }

    static val_t _f__l3_fn0(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        val_t _g_0 = (val_t)_clo0;
        val_t _r0 = syl_call(_g_0, 1LL);
        return _r0;
    }

    int main(void) {
        (void)_f__l3_fn0();
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
    static val_t _f__l4_fn0(void) {
        val_t _x_0 = 0LL;
        return (_x_0 + 1LL);
    }

    int main(void) {
        (void)_f__l4_fn0();
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return _x_0;
    }

    static val_t _f__l4_fn0(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        val_t _g__I_0 = (val_t)_clo0;
        return _g__I_0;
    }

    int main(void) {
        (void)_f__l4_fn0();
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return _x_0;
    }

    static val_t _id__B_fn0(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        return (val_t)_clo0;
    }

    static val_t _lam1(val_t* _env, val_t _x_1) {
        return _x_1;
    }

    static val_t _id__I_fn0(void) {
        closure_t* _clo1 = (closure_t*)malloc(sizeof(closure_t));
        _clo1->fn = _lam1;
        _clo1->env = NULL;
        return (val_t)_clo1;
    }

    static val_t _x_2;
    static val_t _y_0;
    int main(void) {
        val_t _r0 = syl_call(_id__I_fn0(), 0LL);
        _x_2 = _r0;
        val_t _r1 = syl_call(_id__B_fn0(), 1);
        _y_0 = _r1;
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
    static val_t _x_1;
    static val_t _y_0;
    int main(void) {
        val_t _x_0 = 0LL;
        _x_1 = _x_0;
        val_t _x_2 = 1;
        _y_0 = _x_2;
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
    int main(void) {
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
    int main(void) {
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return _x_0;
    }

    static val_t _apply_int__l4_fn0(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        val_t _f__I_0 = (val_t)_clo0;
        return _f__I_0;
    }

    int main(void) {
        (void)_apply_int__l4_fn0();
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return _x_0;
    }

    static val_t _apply__l7_f__B_fn0(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        return (val_t)_clo0;
    }

    static val_t _lam1(val_t* _env, val_t _x_1) {
        return _x_1;
    }

    static val_t _apply__l7_f__I_fn0(void) {
        closure_t* _clo1 = (closure_t*)malloc(sizeof(closure_t));
        _clo1->fn = _lam1;
        _clo1->env = NULL;
        return (val_t)_clo1;
    }

    static val_t _apply__l7__B_fn0(void) {
        return _apply__l7_f__B_fn0();
    }

    static val_t _apply__l7__I_fn0(void) {
        return _apply__l7_f__I_fn0();
    }

    static val_t _g_0;
    static val_t _h_0;
    int main(void) {
        _g_0 = _apply__l7__I_fn0();
        _h_0 = _apply__l7__B_fn0();
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
    static val_t _f__1_fn0(void) {
        val_t _x_0 = 1LL;
        return 1;
    }

    static val_t _f__0_fn0(void) {
        val_t _x_1 = 0LL;
        return 1LL;
    }

    int main(void) {
        (void)_f__0_fn0();
        (void)_f__1_fn0();
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
    static val_t _f__0_fn0(void) {
        val_t _x_0 = 0LL;
        return 1LL;
    }

    static val_t _g__1_fn0(void) {
        val_t _x_1 = 1LL;
        return 2LL;
    }

    int main(void) {
        (void)_f__0_fn0();
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
    static val_t _f_0;
    static val_t _f_fn0(val_t* _env, val_t _arg);

    static val_t _f_fn0(val_t* _env, val_t _x_0) {
        val_t _r0 = syl_call(_f_0, _x_0);
        return _r0;
    }

    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _f_fn0;
            _c->env = NULL;
            _f_0 = (val_t)_c;
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
    int main(void) {
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
    static val_t _f_0;
    static val_t _f_fn0(val_t* _env, val_t _arg);

    static val_t _f_fn0(val_t* _env, val_t _x_0) {
        return 1LL;
    }

    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _f_fn0;
            _c->env = NULL;
            _f_0 = (val_t)_c;
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
    int main(void) {
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
    static val_t _x__0_fn0(void);

    static val_t _x__0_fn0(void) {
        val_t _x_0 = 0LL;
        return _x_0;
    }

    static val_t _y_0;
    int main(void) {
        _y_0 = _x__0_fn0();
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
    static val_t _y_0;
    int main(void) {
        _y_0 = 0LL;
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
    int main(void) {
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
    static val_t _y_0;
    int main(void) {
        _y_0 = 5LL;
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
    static val_t _id__I_0;
    static val_t _id__I_fn0(val_t* _env, val_t _arg);

    static val_t _id__I_fn0(val_t* _env, val_t _x_0) {
        return _x_0;
    }

    static val_t _i_0;
    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _id__I_fn0;
            _c->env = NULL;
            _id__I_0 = (val_t)_c;
        }
        _i_0 = _id__I_0;
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
    int main(void) {
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
    static val_t _id__I_0;
    static val_t _id__I_fn0(val_t* _env, val_t _arg);

    static val_t _id__I_fn0(val_t* _env, val_t _x_0) {
        return _x_0;
    }

    static val_t _x_1;
    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _id__I_fn0;
            _c->env = NULL;
            _id__I_0 = (val_t)_c;
        }
        val_t _r0 = syl_call(_id__I_0, 0LL);
        _x_1 = _r0;
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
    static val_t _id_0;
    static val_t _id_fn0(val_t* _env, val_t _arg);

    static val_t _lam0(val_t* _env, val_t _x_0) {
        return _x_0;
    }

    static val_t _id_fn0(val_t* _env, val_t _a_0) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        return (val_t)_clo0;
    }

    static val_t _x_1;
    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _id_fn0;
            _c->env = NULL;
            _id_0 = (val_t)_c;
        }
        val_t _r0 = syl_call(_id_0, 0);
        val_t _r1 = syl_call(_r0, 0LL);
        _x_1 = _r1;
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
    static val_t _id1__I_0;
    static val_t _id1__I_fn0(val_t* _env, val_t _arg);

    static val_t _id1__I_fn0(val_t* _env, val_t _x_0) {
        return _x_0;
    }


    static val_t _x_1;
    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _id1__I_fn0;
            _c->env = NULL;
            _id1__I_0 = (val_t)_c;
        }
        val_t _r0 = syl_call(_id1__I_0, 0LL);
        _x_1 = _r0;
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
    static val_t _a_0;
    static val_t _a_fn0(val_t* _env, val_t _arg);

    static val_t _a_fn0(val_t* _env, val_t _a_1) {
        return 0;
    }

    static val_t _b_0;
    static val_t _b_fn0(val_t* _env, val_t _arg);

    static val_t _lam0(val_t* _env, val_t _a_3) {
        return 0;
    }

    static val_t _b_fn0(val_t* _env, val_t _a_2) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        return (val_t)_clo0;
    }

    static val_t _x_0;
    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _a_fn0;
            _c->env = NULL;
            _a_0 = (val_t)_c;
        }
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _b_fn0;
            _c->env = NULL;
            _b_0 = (val_t)_c;
        }
        _x_0 = _b_0;
        val_t _r0 = syl_call(_x_0, 0);
        val_t _r1 = syl_call(_r0, 0);
        (void)_r1;
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
    static val_t _x_0;
    static val_t _x_fn0(val_t* _env, val_t _arg);

    static val_t _lam0(val_t* _env, val_t _a_1) {
        return 0;
    }

    static val_t _x_fn0(val_t* _env, val_t _a_0) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        return (val_t)_clo0;
    }

    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _x_fn0;
            _c->env = NULL;
            _x_0 = (val_t)_c;
        }
        val_t _r0 = syl_call(_x_0, 0);
        val_t _r1 = syl_call(_r0, 0);
        (void)_r1;
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
    static val_t _x_0;
    static val_t _x_fn0(val_t* _env, val_t _arg);

    static val_t _x_fn0(val_t* _env, val_t _f_0) {
        val_t _r0 = syl_call(_f_0, 0);
        return _r0;
    }

    static val_t _lam0(val_t* _env, val_t _a_0) {
        return 1LL;
    }

    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _x_fn0;
            _c->env = NULL;
            _x_0 = (val_t)_c;
        }
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        val_t _r1 = syl_call(_x_0, (val_t)_clo0);
        (void)_r1;
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
    static val_t _f__1_fn0(void) {
        return 1;
    }

    static val_t _f__0_fn0(void) {
        return 1LL;
    }

    int main(void) {
        (void)_f__0_fn0();
        (void)_f__1_fn0();
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
    static val_t _f__1_fn0(void) {
        return 1;
    }

    static val_t _f__0_fn0(void) {
        return 1LL;
    }

    int main(void) {
        (void)_f__0_fn0();
        (void)_f__1_fn0();
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
    static val_t _f__1_fn0(void) {
        return 1;
    }

    static val_t _f__2_fn0(void) {
        return 1LL;
    }

    int main(void) {
        (void)_f__1_fn0();
        (void)_f__2_fn0();
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
    static val_t _f__0_fn0(void) {
        return 1LL;
    }

    int main(void) {
        (void)_f__0_fn0();
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
    static val_t _f__0_fn0(void) {
        return 1;
    }

    int main(void) {
        (void)_f__0_fn0();
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
    static val_t _f__1_fn0(void) {
        return 1LL;
    }

    static val_t _f__2_fn0(void) {
        return 1;
    }

    static val_t _f__0_fn0(void) {
        return 1LL;
    }

    int main(void) {
        (void)_f__0_fn0();
        (void)_f__1_fn0();
        (void)_f__2_fn0();
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
    static val_t _choose__l4_fn0(void) {
        val_t _x_0 = 0LL;
        val_t _f__0_0 = 0LL;
        return _f__0_0;
    }

    int main(void) {
        (void)_choose__l4_fn0();
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
    int main(void) {
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
    static val_t _x_0;
    int main(void) {
        _x_0 = 1LL;
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return _x_0;
    }

    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        (void)(val_t)_clo0;
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return _x_0;
    }

    static val_t _f_0;
    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        _f_0 = (val_t)_clo0;
        (void)_f_0;
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return 1LL;
    }

    static val_t _f_0;
    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        _f_0 = (val_t)_clo0;
        (void)_f_0;
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
    int main(void) {
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return 1LL;
    }

    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        (void)(val_t)_clo0;
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
    int main(void) {
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
    static val_t _f__0_fn0(void) {
        val_t _x_0 = 0LL;
        return _x_0;
    }

    int main(void) {
        (void)_f__0_fn0();
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return _x_0;
    }

    static val_t _f_0;
    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        _f_0 = (val_t)_clo0;
        val_t _x_1 = 0LL;
        (void)_x_1;
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return 1LL;
    }

    static val_t _f_0;
    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        _f_0 = (val_t)_clo0;
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
    int main(void) {
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
    static val_t _x_0;
    int main(void) {
        _x_0 = 1LL;
        (void)1LL;
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
    static val_t _x_0;
    int main(void) {
        _x_0 = 1LL;
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
    static val_t _f__f_fn0(void) {
        val_t _b_0 = 0;
        return 1LL;
    }

    int main(void) {
        (void)_f__f_fn0();
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
    int main(void) {
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
    int main(void) {
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return 1LL;
    }

    static val_t _f_0;
    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        _f_0 = (val_t)_clo0;
        val_t _x_1 = 0LL;
        (void)1LL;
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return _x_0;
    }

    static val_t _f_0;
    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        _f_0 = (val_t)_clo0;
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return 1LL;
    }

    static val_t _f_0;
    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        _f_0 = (val_t)_clo0;
        val_t _x_1 = 0LL;
        (void)1LL;
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
    static val_t _lam0(val_t* _env, val_t _f_0) {
        val_t _r0 = syl_call(_f_0, 0LL);
        return _r0;
    }

    static val_t _apply_0;
    static val_t _lam1(val_t* _env, val_t _x_0) {
        return 1LL;
    }

    static val_t _g_0;
    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        _apply_0 = (val_t)_clo0;
        closure_t* _clo1 = (closure_t*)malloc(sizeof(closure_t));
        _clo1->fn = _lam1;
        _clo1->env = NULL;
        _g_0 = (val_t)_clo1;
        val_t _r1 = syl_call(_apply_0, _g_0);
        (void)_r1;
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return _x_0;
    }

    static val_t _g_0;
    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        _g_0 = (val_t)_clo0;
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return 1LL;
    }

    static val_t _g_0;
    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        _g_0 = (val_t)_clo0;
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return _x_0;
    }

    static val_t _apply__l3_fn0(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        val_t _f_0 = (val_t)_clo0;
        val_t _r0 = syl_call(_f_0, 0LL);
        return _r0;
    }

    static val_t _lam1(val_t* _env, val_t _x_1) {
        return _x_1;
    }

    static val_t _g_0;
    int main(void) {
        closure_t* _clo1 = (closure_t*)malloc(sizeof(closure_t));
        _clo1->fn = _lam1;
        _clo1->env = NULL;
        _g_0 = (val_t)_clo1;
        (void)_apply__l3_fn0();
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
    static val_t _apply__l4_fn0(void) {
        val_t _x_0 = 0LL;
        val_t _f__0_0 = 1LL;
        return _f__0_0;
    }

    static val_t _g__0_fn0(void) {
        val_t _x_1 = 0LL;
        return 1LL;
    }

    int main(void) {
        (void)_apply__l4_fn0();
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
    static val_t _lam0(val_t* _env, val_t _f_0) {
        val_t _r0 = syl_call(_f_0, 0LL);
        return _r0;
    }

    static val_t _apply_0;
    static val_t _lam1(val_t* _env, val_t _x_0) {
        return _x_0;
    }

    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        _apply_0 = (val_t)_clo0;
        closure_t* _clo1 = (closure_t*)malloc(sizeof(closure_t));
        _clo1->fn = _lam1;
        _clo1->env = NULL;
        val_t _f_1 = (val_t)_clo1;
        val_t _r1 = syl_call(_f_1, 0LL);
        (void)_r1;
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return 1LL;
    }

    static val_t _g_0;
    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        _g_0 = (val_t)_clo0;
        return 0;
    }
    |}]
;;

let%expect_test "static lambda identity returns dependent type" =
  go
    {|
let f = fn (static x : int) -> x;;
let _ = f 42;;
|};
  [%expect
    {|
    static val_t _f__42_fn0(void) {
        val_t _x_0 = 42LL;
        return _x_0;
    }

    int main(void) {
        (void)_f__42_fn0();
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
    static val_t _f__10_fn0(void) {
        val_t _x_0 = 10LL;
        return (_x_0 + 1LL);
    }

    int main(void) {
        (void)_f__10_fn0();
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
    static val_t _f__f_fn0(void) {
        val_t _x_0 = 0;
        return (_x_0 && 1);
    }

    int main(void) {
        (void)_f__f_fn0();
        return 0;
    }
    |}]
;;

let%expect_test "nested static lambdas" =
  go
    {|
let f = fn (static x : int) -> fn (static y : int) -> x + y;;
let _ = f 1 2;;
|};
  [%expect
    {|
    static val_t _x_0;
    static val_t _f__1__2_fn0(void) {
        val_t _y_0 = 2LL;
        return (_x_0 + _y_0);
    }

    int main(void) {
        _x_0 = 1LL;
        val_t _x_1 = 1LL;
        val_t _y_1 = 2LL;
        (void)(_x_1 + _y_1);
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
    static val_t _x_0;
    static val_t _f__1__2_fn0(void) {
        val_t _y_0 = 2LL;
        return _x_0;
    }

    int main(void) {
        _x_0 = 1LL;
        (void)_f__1__2_fn0();
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
    static val_t _lam0(val_t* _env, val_t _y_0) {
        return _y_0;
    }

    static val_t _f__1_fn0(void) {
        val_t _x_0 = 1LL;
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        return (val_t)_clo0;
    }

    static val_t _g_0;
    int main(void) {
        _g_0 = _f__1_fn0();
        val_t _r0 = syl_call(_g_0, 2LL);
        (void)_r0;
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return _x_0;
    }

    static val_t _f__B_fn0(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        return (val_t)_clo0;
    }

    static val_t _lam1(val_t* _env, val_t _x_1) {
        return _x_1;
    }

    static val_t _f__I_fn0(void) {
        closure_t* _clo1 = (closure_t*)malloc(sizeof(closure_t));
        _clo1->fn = _lam1;
        _clo1->env = NULL;
        return (val_t)_clo1;
    }

    static val_t _g_0;
    static val_t _h_0;
    int main(void) {
        _g_0 = _f__I_fn0();
        val_t _r0 = syl_call(_g_0, 42LL);
        (void)_r0;
        _h_0 = _f__B_fn0();
        val_t _r1 = syl_call(_h_0, 1);
        (void)_r1;
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
    int main(void) {
        (void)1LL;
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
    int main(void) {
        (void)1;
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
    static val_t _f__1_fn0(void) {
        val_t _x_0 = 1LL;
        return 1;
    }

    static val_t _f__0_fn0(void) {
        val_t _x_1 = 0LL;
        return 1LL;
    }

    static val_t _a_0;
    static val_t _b_0;
    int main(void) {
        _a_0 = _f__0_fn0();
        _b_0 = _f__1_fn0();
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
    int main(void) {
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
    int main(void) {
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
    static val_t _lam0(val_t* _env, val_t _y_0) {
        return _y_0;
    }

    static val_t _f__1_fn0(void) {
        val_t _x_0 = 1LL;
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        return (val_t)_clo0;
    }

    static val_t _lam1(val_t* _env, val_t _y_1) {
        return _y_1;
    }

    static val_t _f__0_fn0(void) {
        val_t _x_1 = 0LL;
        closure_t* _clo1 = (closure_t*)malloc(sizeof(closure_t));
        _clo1->fn = _lam1;
        _clo1->env = NULL;
        return (val_t)_clo1;
    }

    static val_t _g_0;
    static val_t _h_0;
    int main(void) {
        _g_0 = _f__0_fn0();
        val_t _r0 = syl_call(_g_0, 42LL);
        (void)_r0;
        _h_0 = _f__1_fn0();
        val_t _r1 = syl_call(_h_0, 1);
        (void)_r1;
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
    int main(void) {
        (void)1LL;
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
    int main(void) {
        (void)1;
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
    int main(void) {
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return _x_0;
    }

    static val_t _apply_type__l4_fn0(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        val_t _f__I_0 = (val_t)_clo0;
        return _f__I_0;
    }

    int main(void) {
        (void)_apply_type__l4_fn0();
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
    int main(void) {
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
    static val_t _f__0_fn0(void);

    static val_t _f__0_fn0(void) {
        val_t _x_0 = 0LL;
        return _x_0;
    }

    int main(void) {
        (void)_f__0_fn0();
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
    static val_t _id__I_0;
    static val_t _id__B_0;
    static val_t _id__I_fn0(val_t* _env, val_t _arg);
    static val_t _id__B_fn0(val_t* _env, val_t _arg);

    static val_t _id__I_fn0(val_t* _env, val_t _x_0) {
        return _x_0;
    }

    static val_t _id__B_fn0(val_t* _env, val_t _x_1) {
        return _x_1;
    }

    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _id__I_fn0;
            _c->env = NULL;
            _id__I_0 = (val_t)_c;
        }
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _id__B_fn0;
            _c->env = NULL;
            _id__B_0 = (val_t)_c;
        }
        val_t _r0 = syl_call(_id__I_0, 0LL);
        (void)_r0;
        val_t _r1 = syl_call(_id__B_0, 1);
        (void)_r1;
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
    static val_t _f_0;
    static val_t _f_fn0(val_t* _env, val_t _arg);

    static val_t _f_fn0(val_t* _env, val_t _x_0) {
        val_t _r0 = syl_call(_f_0, _x_0);
        return _r0;
    }

    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _f_fn0;
            _c->env = NULL;
            _f_0 = (val_t)_c;
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
    static val_t _id1__I_0;
    static val_t _id1__I_fn0(val_t* _env, val_t _arg);

    static val_t _id1__I_fn0(val_t* _env, val_t _x_0) {
        return _x_0;
    }


    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _id1__I_fn0;
            _c->env = NULL;
            _id1__I_0 = (val_t)_c;
        }
        val_t _r0 = syl_call(_id1__I_0, 0LL);
        (void)_r0;
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
    static val_t _f__1_fn0(void) {
        return 0LL;
    }

    int main(void) {
        (void)_f__1_fn0();
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
    int main(void) {
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
    int main(void) {
        (void)5LL;
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
    int main(void) {
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
    static val_t _f__0_fn0(void) {
        val_t _x_0 = 0LL;
        return 1LL;
    }

    static val_t _g__1_fn0(void) {
        val_t _x_1 = 1LL;
        return 2LL;
    }

    int main(void) {
        (void)_f__0_fn0();
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
    static val_t _x_0;
    static val_t _f__1__1_fn0(void) {
        val_t _y_0 = 1LL;
        return 2LL;
    }

    static val_t _f__1__0_fn0(void) {
        val_t _y_1 = 0LL;
        return 0;
    }

    static val_t _x_1;
    static val_t _f__0__1_fn0(void) {
        val_t _y_2 = 1LL;
        return 1;
    }

    static val_t _f__0__0_fn0(void) {
        val_t _y_3 = 0LL;
        return 1LL;
    }

    int main(void) {
        _x_0 = 1LL;
        _x_1 = 0LL;
        val_t _y_4 = 0LL;
        (void)1LL;
        val_t _y_5 = 1LL;
        (void)1;
        val_t _y_6 = 0LL;
        (void)0;
        val_t _y_7 = 1LL;
        (void)2LL;
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
    static val_t _f__0_fn0(void) {
        (void)0LL;
        return 42LL;
    }

    int main(void) {
        (void)_f__0_fn0();
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
    extern val_t syl_print_int(val_t);

    static val_t _syl_print_int_wrap0(val_t* _env, val_t _arg) {
        return syl_print_int(_arg);
    }

    static val_t _print_int_0;
    static val_t _print__1_fn0(void) {
        val_t _x_0 = 1LL;
        val_t _r0 = syl_call(_print_int_0, _x_0);
        return _r0;
    }

    static val_t _print__0_fn0(void) {
        val_t _x_1 = 0LL;
        val_t _r1 = syl_call(_print_int_0, _x_1);
        return _r1;
    }

    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _syl_print_int_wrap0;
        _clo0->env = NULL;
        _print_int_0 = (val_t)_clo0;
        (void)_print__0_fn0();
        (void)_print__1_fn0();
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return _x_0;
    }

    static val_t _id__I_I_fn0(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        return (val_t)_clo0;
    }

    static val_t _lam1(val_t* _env, val_t _x_1) {
        return (_x_1 + 1LL);
    }

    int main(void) {
        closure_t* _clo1 = (closure_t*)malloc(sizeof(closure_t));
        _clo1->fn = _lam1;
        _clo1->env = NULL;
        val_t _r0 = syl_call(_id__I_I_fn0(), (val_t)_clo1);
        val_t _r1 = syl_call(_r0, 5LL);
        (void)_r1;
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
    static val_t _f__f_fn0(void) {
        val_t _b_0 = 0;
        return 1;
    }

    static val_t _f__t_fn0(void) {
        val_t _b_1 = 1;
        return 1LL;
    }

    int main(void) {
        (void)_f__t_fn0();
        (void)_f__f_fn0();
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
    static val_t _double__5_fn0(void) {
        val_t _x_0 = 5LL;
        return (_x_0 + _x_0);
    }

    int main(void) {
        (void)_double__5_fn0();
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return _x_0;
    }

    static val_t _id__B_fn0(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        return (val_t)_clo0;
    }

    static val_t _lam1(val_t* _env, val_t _x_1) {
        return _x_1;
    }

    static val_t _id__I_fn0(void) {
        closure_t* _clo1 = (closure_t*)malloc(sizeof(closure_t));
        _clo1->fn = _lam1;
        _clo1->env = NULL;
        return (val_t)_clo1;
    }

    static val_t _f_0;
    static val_t _g_0;
    int main(void) {
        _f_0 = _id__I_fn0();
        _g_0 = _id__B_fn0();
        val_t _r0 = syl_call(_f_0, 0LL);
        (void)_r0;
        val_t _r1 = syl_call(_g_0, 1);
        (void)_r1;
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
    static val_t _choose__l4_fn0(void) {
        val_t _x_0 = 0LL;
        val_t _f__0_0 = 0LL;
        return _f__0_0;
    }

    int main(void) {
        (void)_choose__l4_fn0();
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
    static val_t _n_0;
    static val_t _f__5_fn0(void) {
        val_t _x_0 = 5LL;
        return (_x_0 + _n_0);
    }

    int main(void) {
        _n_0 = 10LL;
        (void)_f__5_fn0();
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
    static val_t _f__42_fn0(void) {
        val_t _x_0 = 42LL;
        return _x_0;
    }

    int main(void) {
        (void)_f__42_fn0();
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
    static val_t _f__f_fn0(void) {
        val_t _b_0 = 0;
        return 1;
    }

    static val_t _f__t_fn0(void) {
        val_t _b_1 = 1;
        return 0LL;
    }

    int main(void) {
        (void)_f__t_fn0();
        (void)_f__f_fn0();
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
    static val_t _apply__l4_fn0(void) {
        val_t _x_0 = 5LL;
        val_t _f__5_0 = (_x_0 + 1LL);
        return _f__5_0;
    }

    int main(void) {
        (void)_apply__l4_fn0();
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
    static val_t _lam1(val_t* _env, val_t _y_0) {
        val_t _x_1 = _env[0];
        return _x_1;
    }

    static val_t _lam0(val_t* _env, val_t _x_0) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam1;
        _clo0->env = (val_t*)malloc(1 * sizeof(val_t));
        _clo0->env[0] = _x_0;
        return (val_t)_clo0;
    }

    static val_t _f__I__B_fn0(void) {
        closure_t* _clo1 = (closure_t*)malloc(sizeof(closure_t));
        _clo1->fn = _lam0;
        _clo1->env = NULL;
        return (val_t)_clo1;
    }

    static val_t _lam2(val_t* _env, val_t _y_1) {
        val_t _x_3 = _env[0];
        return _x_3;
    }

    int main(void) {
        val_t _x_2 = 0LL;
        closure_t* _clo2 = (closure_t*)malloc(sizeof(closure_t));
        _clo2->fn = _lam2;
        _clo2->env = (val_t*)malloc(1 * sizeof(val_t));
        _clo2->env[0] = _x_2;
        val_t _r0 = syl_call((val_t)_clo2, 1);
        (void)_r0;
        return 0;
    }
    |}]
;;

let%expect_test "if static nested in let expression" =
  go
    {|
let f = fn (static x : int) ->
  let y = if static x == 0 then 1 else true;
  y;;
let _ = f 0;;
let _ = f 1;;
|};
  [%expect
    {|
    static val_t _f__1_fn0(void) {
        val_t _x_0 = 1LL;
        val_t _y_0 = 1;
        return _y_0;
    }

    static val_t _f__0_fn0(void) {
        val_t _x_1 = 0LL;
        val_t _y_1 = 1LL;
        return _y_1;
    }

    int main(void) {
        (void)_f__0_fn0();
        (void)_f__1_fn0();
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return _x_0;
    }

    static val_t _f__l10_fn0(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        return (val_t)_clo0;
    }

    int main(void) {
        (void)_f__l10_fn0();
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
    static val_t _lam0(val_t* _env, val_t _x_0) {
        return _x_0;
    }

    static val_t _wrap__l9_fn0(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _lam0;
        _clo0->env = NULL;
        return (val_t)_clo0;
    }

    int main(void) {
        (void)_wrap__l9_fn0();
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
    static val_t _f__l17_fn0(void) {
        val_t _x_0 = 0LL;
        val_t _apply__l20_f__0_0 = 0LL;
        val_t _apply__l20_0 = _apply__l20_f__0_0;
        return _apply__l20_0;
    }

    int main(void) {
        (void)_f__l17_fn0();
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
    static val_t _f__I_0;
    static val_t _g_0;
    static val_t _f__I_fn0(val_t* _env, val_t _arg);
    static val_t _g_fn0(val_t* _env, val_t _arg);

    static val_t _f__I_fn0(val_t* _env, val_t _x_0) {
        return _x_0;
    }

    static val_t _g_fn0(val_t* _env, val_t _x_1) {
        val_t _r0 = syl_call(_f__I_0, _x_1);
        return _r0;
    }

    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _f__I_fn0;
            _c->env = NULL;
            _f__I_0 = (val_t)_c;
        }
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _g_fn0;
            _c->env = NULL;
            _g_0 = (val_t)_c;
        }
        val_t _r1 = syl_call(_g_0, 5LL);
        (void)_r1;
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
    static val_t _inc_0;
    static val_t _choose__t_0;
    static val_t _choose__f_0;
    static val_t _inc_fn0(val_t* _env, val_t _arg);
    static val_t _choose__t_fn0(val_t* _env, val_t _arg);
    static val_t _choose__f_fn0(val_t* _env, val_t _arg);

    static val_t _inc_fn0(val_t* _env, val_t _x_0) {
        return (_x_0 + 1LL);
    }

    static val_t _choose__t_fn0(val_t* _env, val_t _x_1) {
        val_t _r0 = syl_call(_inc_0, _x_1);
        return _r0;
    }

    static val_t _choose__f_fn0(val_t* _env, val_t _x_2) {
        return _x_2;
    }

    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _inc_fn0;
            _c->env = NULL;
            _inc_0 = (val_t)_c;
        }
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _choose__t_fn0;
            _c->env = NULL;
            _choose__t_0 = (val_t)_c;
        }
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _choose__f_fn0;
            _c->env = NULL;
            _choose__f_0 = (val_t)_c;
        }
        val_t _r1 = syl_call(_choose__t_0, 5LL);
        (void)_r1;
        val_t _r2 = syl_call(_choose__f_0, 5LL);
        (void)_r2;
        return 0;
    }
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : erased int = g x
and g (y : int) : int = let _ = f y; 0;;
let _ = g 0;;
|};
  [%expect
    {|
    static val_t _g_0;
    static val_t _g_fn0(val_t* _env, val_t _arg);

    static val_t _g_fn0(val_t* _env, val_t _y_0) {
        return 0LL;
    }

    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _g_fn0;
            _c->env = NULL;
            _g_0 = (val_t)_c;
        }
        val_t _r0 = syl_call(_g_0, 0LL);
        (void)_r0;
        return 0;
    }
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = let _ = g x; 0
and g (y : int) : erased int = f y;;
let _ = f 0;;
|};
  [%expect
    {|
    static val_t _f_0;
    static val_t _f_fn0(val_t* _env, val_t _arg);

    static val_t _f_fn0(val_t* _env, val_t _x_0) {
        return 0LL;
    }

    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _f_fn0;
            _c->env = NULL;
            _f_0 = (val_t)_c;
        }
        val_t _r0 = syl_call(_f_0, 0LL);
        (void)_r0;
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
    int main(void) {
        val_t _x_0 = 0LL;
        (void)0LL;
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
    int main(void) {
        val_t _x_0 = 0LL;
        (void)0LL;
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
    int main(void) {
        (void)0LL;
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
    int main(void) {
        (void)0LL;
        return 0;
    }
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = g x
and erased g (y : int) : int = f y;;
let _ = (f @ erased) 0;;
|};
  [%expect
    {|
    static val_t _f_0;
    static val_t _f_fn0(val_t* _env, val_t _arg);

    static val_t _f_fn0(val_t* _env, val_t _x_0) {
        val_t _y_0 = _x_0;
        val_t _r0 = syl_call(_f_0, _y_0);
        return _r0;
    }

    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _f_fn0;
            _c->env = NULL;
            _f_0 = (val_t)_c;
        }
        val_t _x_1 = 0LL;
        val_t _y_1 = _x_1;
        val_t _r1 = syl_call(_f_0, _y_1);
        (void)_r1;
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
    int main(void) {
        val_t _x_0 = 0LL;
        (void)(_x_0 + 1LL);
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
    static val_t _inc_0;
    static val_t _f__I_0;
    static val_t _inc_fn0(val_t* _env, val_t _arg);
    static val_t _f__I_fn0(val_t* _env, val_t _arg);

    static val_t _inc_fn0(val_t* _env, val_t _x_0) {
        return (_x_0 + 1LL);
    }

    static val_t _f__I_fn0(val_t* _env, val_t _x_1) {
        return _x_1;
    }

    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _inc_fn0;
            _c->env = NULL;
            _inc_0 = (val_t)_c;
        }
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _f__I_fn0;
            _c->env = NULL;
            _f__I_0 = (val_t)_c;
        }
        val_t _r0 = syl_call(_f__I_0, 0LL);
        (void)_r0;
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
    static val_t _f__I_0;
    static val_t _g_0;
    static val_t _f__I_fn0(val_t* _env, val_t _arg);
    static val_t _g_fn0(val_t* _env, val_t _arg);

    static val_t _f__I_fn0(val_t* _env, val_t _x_0) {
        return _x_0;
    }

    static val_t _g_fn0(val_t* _env, val_t _x_1) {
        val_t _r0 = syl_call(_f__I_0, _x_1);
        return _r0;
    }

    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _f__I_fn0;
            _c->env = NULL;
            _f__I_0 = (val_t)_c;
        }
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _g_fn0;
            _c->env = NULL;
            _g_0 = (val_t)_c;
        }
        val_t _r1 = syl_call(_g_0, 5LL);
        (void)_r1;
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
    static val_t _inc_0;
    static val_t _choose__t_0;
    static val_t _choose__f_0;
    static val_t _inc_fn0(val_t* _env, val_t _arg);
    static val_t _choose__t_fn0(val_t* _env, val_t _arg);
    static val_t _choose__f_fn0(val_t* _env, val_t _arg);

    static val_t _inc_fn0(val_t* _env, val_t _x_0) {
        return (_x_0 + 1LL);
    }

    static val_t _choose__t_fn0(val_t* _env, val_t _x_1) {
        val_t _r0 = syl_call(_inc_0, _x_1);
        return _r0;
    }

    static val_t _choose__f_fn0(val_t* _env, val_t _x_2) {
        return _x_2;
    }

    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _inc_fn0;
            _c->env = NULL;
            _inc_0 = (val_t)_c;
        }
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _choose__t_fn0;
            _c->env = NULL;
            _choose__t_0 = (val_t)_c;
        }
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _choose__f_fn0;
            _c->env = NULL;
            _choose__f_0 = (val_t)_c;
        }
        val_t _r1 = syl_call(_choose__t_0, 5LL);
        (void)_r1;
        val_t _r2 = syl_call(_choose__f_0, 5LL);
        (void)_r2;
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
    static val_t _f__I_0;
    static val_t _f__I_fn0(val_t* _env, val_t _arg);

    static val_t _f__I_fn0(val_t* _env, val_t _x_0) {
        return _x_0;
    }

    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _f__I_fn0;
            _c->env = NULL;
            _f__I_0 = (val_t)_c;
        }
        val_t _r0 = syl_call(_f__I_0, 0LL);
        (void)_r0;
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
    static val_t _f__0_fn0(void);
    static val_t _f__3_fn0(void);
    static val_t _f__2_fn0(void);
    static val_t _f__1_fn0(void);

    static val_t _f__0_fn0(void) {
        val_t _x_0 = 0LL;
        return _x_0;
    }

    static val_t _f__3_fn0(void) {
        val_t _x_1 = 3LL;
        return _f__2_fn0();
    }

    static val_t _f__2_fn0(void) {
        val_t _x_2 = 2LL;
        return _f__1_fn0();
    }

    static val_t _f__1_fn0(void) {
        val_t _x_3 = 1LL;
        return _f__0_fn0();
    }

    int main(void) {
        (void)_f__3_fn0();
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
    int main(void) {
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
    static val_t _double_0;
    static val_t _apply_double__I_0;
    static val_t _double_fn0(val_t* _env, val_t _arg);
    static val_t _apply_double__I_fn0(val_t* _env, val_t _arg);

    static val_t _double_fn0(val_t* _env, val_t _x_0) {
        return (_x_0 + _x_0);
    }

    static val_t _apply_double__I_fn0(val_t* _env, val_t _x_1) {
        val_t _r0 = syl_call(_double_0, _x_1);
        return _r0;
    }

    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _double_fn0;
            _c->env = NULL;
            _double_0 = (val_t)_c;
        }
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _apply_double__I_fn0;
            _c->env = NULL;
            _apply_double__I_0 = (val_t)_c;
        }
        val_t _r1 = syl_call(_apply_double__I_0, 5LL);
        (void)_r1;
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
    int main(void) {
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
    static val_t _f_0;
    static val_t _f_fn0(val_t* _env, val_t _arg);

    static val_t _f_fn0(val_t* _env, val_t _x_0) {
        val_t _y_0 = _x_0;
        return _y_0;
    }

    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _f_fn0;
            _c->env = NULL;
            _f_0 = (val_t)_c;
        }
        val_t _r0 = syl_call(_f_0, 0LL);
        (void)_r0;
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
    static val_t _f_0;
    static val_t _g_0;
    static val_t _f_fn0(val_t* _env, val_t _arg);
    static val_t _g_fn0(val_t* _env, val_t _arg);

    static val_t _f_fn0(val_t* _env, val_t _x_0) {
        val_t _y_0 = _x_0;
        return _y_0;
    }

    static val_t _g_fn0(val_t* _env, val_t _y_1) {
        return _y_1;
    }

    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _f_fn0;
            _c->env = NULL;
            _f_0 = (val_t)_c;
        }
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _g_fn0;
            _c->env = NULL;
            _g_0 = (val_t)_c;
        }
        val_t _r0 = syl_call(_f_0, 0LL);
        (void)_r0;
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
    int main(void) {
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
    static val_t _g_0;
    static val_t _g_fn0(val_t* _env, val_t _arg);

    static val_t _g_fn0(val_t* _env, val_t _y_0) {
        return _y_0;
    }

    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _g_fn0;
            _c->env = NULL;
            _g_0 = (val_t)_c;
        }
        val_t _x_0 = 0LL;
        val_t _y_1 = _x_0;
        (void)_y_1;
        return 0;
    }
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = g x
and g (y : int) : int = f y;;
let _ = f 0;;
|};
  [%expect
    {|
    static val_t _f_0;
    static val_t _g_0;
    static val_t _f_fn0(val_t* _env, val_t _arg);
    static val_t _g_fn0(val_t* _env, val_t _arg);

    static val_t _f_fn0(val_t* _env, val_t _x_0) {
        val_t _r0 = syl_call(_g_0, _x_0);
        return _r0;
    }

    static val_t _g_fn0(val_t* _env, val_t _y_0) {
        val_t _r1 = syl_call(_f_0, _y_0);
        return _r1;
    }

    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _f_fn0;
            _c->env = NULL;
            _f_0 = (val_t)_c;
        }
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _g_fn0;
            _c->env = NULL;
            _g_0 = (val_t)_c;
        }
        val_t _r2 = syl_call(_f_0, 0LL);
        (void)_r2;
        return 0;
    }
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = g x
and erased g (y : int) : int = f y;;
let _ = (f @ erased) 0;;
|};
  [%expect
    {|
    static val_t _f_0;
    static val_t _f_fn0(val_t* _env, val_t _arg);

    static val_t _f_fn0(val_t* _env, val_t _x_0) {
        val_t _y_0 = _x_0;
        val_t _r0 = syl_call(_f_0, _y_0);
        return _r0;
    }

    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _f_fn0;
            _c->env = NULL;
            _f_0 = (val_t)_c;
        }
        val_t _x_1 = 0LL;
        val_t _y_1 = _x_1;
        val_t _r1 = syl_call(_f_0, _y_1);
        (void)_r1;
        return 0;
    }
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : int = g x
and g (y : int) : int = (f @ erased) y;;
let _ = f 0;;
|};
  [%expect
    {|
    static val_t _f_0;
    static val_t _g_0;
    static val_t _f_fn0(val_t* _env, val_t _arg);
    static val_t _g_fn0(val_t* _env, val_t _arg);

    static val_t _f_fn0(val_t* _env, val_t _x_0) {
        val_t _r0 = syl_call(_g_0, _x_0);
        return _r0;
    }

    static val_t _g_fn0(val_t* _env, val_t _y_0) {
        val_t _x_1 = _y_0;
        val_t _r1 = syl_call(_g_0, _x_1);
        return _r1;
    }

    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _f_fn0;
            _c->env = NULL;
            _f_0 = (val_t)_c;
        }
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _g_fn0;
            _c->env = NULL;
            _g_0 = (val_t)_c;
        }
        val_t _r2 = syl_call(_f_0, 0LL);
        (void)_r2;
        return 0;
    }
    |}]
;;

let%expect_test "recursive inlining" =
  go
    {|
fun f (x : int) : erased int = g x
and g (y : int) : int = let _ = f y; 0;;
let _ = g 0;;
|};
  [%expect
    {|
    static val_t _g_0;
    static val_t _g_fn0(val_t* _env, val_t _arg);

    static val_t _g_fn0(val_t* _env, val_t _y_0) {
        return 0LL;
    }

    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _g_fn0;
            _c->env = NULL;
            _g_0 = (val_t)_c;
        }
        val_t _r0 = syl_call(_g_0, 0LL);
        (void)_r0;
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
    int main(void) {
        val_t _x_0 = 0LL;
        (void)0LL;
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
    int main(void) {
        val_t _x_0 = 0LL;
        (void)0LL;
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
    static val_t _f__1_fn0(void) {
        val_t _x_0 = 1LL;
        return _x_0;
    }

    int main(void) {
        (void)_f__1_fn0();
        return 0;
    }
    |}]
;;

let%expect_test "top-level mutual recursion calling each other" =
  go
    {|
fun f (a : int) : int = g a
and g (b : int) : int = f b;;
let _ = f 0;;
|};
  [%expect
    {|
    static val_t _f_0;
    static val_t _g_0;
    static val_t _f_fn0(val_t* _env, val_t _arg);
    static val_t _g_fn0(val_t* _env, val_t _arg);

    static val_t _f_fn0(val_t* _env, val_t _a_0) {
        val_t _r0 = syl_call(_g_0, _a_0);
        return _r0;
    }

    static val_t _g_fn0(val_t* _env, val_t _b_0) {
        val_t _r1 = syl_call(_f_0, _b_0);
        return _r1;
    }

    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _f_fn0;
            _c->env = NULL;
            _f_0 = (val_t)_c;
        }
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _g_fn0;
            _c->env = NULL;
            _g_0 = (val_t)_c;
        }
        val_t _r2 = syl_call(_f_0, 0LL);
        (void)_r2;
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
    static val_t _f_0;
    static val_t _g_0;
    static val_t _f_fn0(val_t* _env, val_t _arg);
    static val_t _g_fn0(val_t* _env, val_t _arg);

    static val_t _f_fn0(val_t* _env, val_t _a_0) {
        val_t _r0 = syl_call(_g_0, (_a_0 + 1LL));
        return _r0;
    }

    static val_t _g_fn0(val_t* _env, val_t _b_0) {
        return _b_0;
    }

    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _f_fn0;
            _c->env = NULL;
            _f_0 = (val_t)_c;
        }
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _g_fn0;
            _c->env = NULL;
            _g_0 = (val_t)_c;
        }
        val_t _r1 = syl_call(_f_0, 0LL);
        (void)_r1;
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
    static val_t _f__0_fn0(void);
    static val_t _f__2_fn0(void);
    static val_t _g__1_fn0(void);

    static val_t _f__0_fn0(void) {
        val_t _x_0 = 0LL;
        return 0LL;
    }

    static val_t _f__2_fn0(void) {
        val_t _x_1 = 2LL;
        return _g__1_fn0();
    }

    static val_t _g__1_fn0(void) {
        val_t _y_0 = 1LL;
        return _f__0_fn0();
    }

    int main(void) {
        (void)_f__2_fn0();
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
    static val_t _g__I_0;
    static val_t _g__I_fn0(val_t* _env, val_t _arg);

    static val_t _g__I_fn0(val_t* _env, val_t _x_0) {
        return _x_0;
    }

    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _g__I_fn0;
            _c->env = NULL;
            _g__I_0 = (val_t)_c;
        }
        val_t _r0 = syl_call(_g__I_0, 0LL);
        (void)_r0;
        return 0;
    }
    |}]
;;

let%expect_test "local fun inside top-level fun" =
  go
    {|
fun outer (x : int) : int =
  fun inner (y : int) : int = y + x;
  inner x;;
let _ = outer 5;;
|};
  [%expect
    {|
    static val_t _outer_0;
    static val_t _outer_fn0(val_t* _env, val_t _arg);

    static val_t _inner_fn0(val_t* _env, val_t _arg);

    static val_t _inner_fn0(val_t* _env, val_t _y_0) {
        val_t _x_1 = _env[0];
        val_t _inner_0 = _env[1];
        return (_y_0 + _x_1);
    }

    static val_t _outer_fn0(val_t* _env, val_t _x_0) {
        val_t* _env0 = (val_t*)malloc(2 * sizeof(val_t));
        closure_t* _inner_1 = (closure_t*)malloc(sizeof(closure_t));
        _inner_1->fn = _inner_fn0;
        _inner_1->env = _env0;
        _env0[0] = _x_0;
        _env0[1] = (val_t)_inner_1;
        val_t _inner_2 = (val_t)_inner_1;
        val_t _r0 = syl_call(_inner_2, _x_0);
        return _r0;
    }

    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _outer_fn0;
            _c->env = NULL;
            _outer_0 = (val_t)_c;
        }
        val_t _r1 = syl_call(_outer_0, 5LL);
        (void)_r1;
        return 0;
    }
    |}]
;;

let%expect_test "mutually recursive local closures share environment" =
  go
    {|
fun outer (x : int) : int =
  fun f (a : int) : int = g (a + x)
  and g (b : int) : int = f (b + x);
  f 0;;
let _ = outer 5;;
|};
  [%expect
    {|
    static val_t _outer_0;
    static val_t _outer_fn0(val_t* _env, val_t _arg);

    static val_t _f_fn0(val_t* _env, val_t _arg);
    static val_t _g_fn0(val_t* _env, val_t _arg);

    static val_t _f_fn0(val_t* _env, val_t _a_0) {
        val_t _x_1 = _env[0];
        val_t _f_0 = _env[1];
        val_t _g_0 = _env[2];
        val_t _r0 = syl_call(_g_0, (_a_0 + _x_1));
        return _r0;
    }

    static val_t _g_fn0(val_t* _env, val_t _b_0) {
        val_t _x_2 = _env[0];
        val_t _f_1 = _env[1];
        val_t _g_1 = _env[2];
        val_t _r1 = syl_call(_f_1, (_b_0 + _x_2));
        return _r1;
    }

    static val_t _outer_fn0(val_t* _env, val_t _x_0) {
        val_t* _env0 = (val_t*)malloc(3 * sizeof(val_t));
        closure_t* _f_2 = (closure_t*)malloc(sizeof(closure_t));
        _f_2->fn = _f_fn0;
        _f_2->env = _env0;
        closure_t* _g_2 = (closure_t*)malloc(sizeof(closure_t));
        _g_2->fn = _g_fn0;
        _g_2->env = _env0;
        _env0[0] = _x_0;
        _env0[1] = (val_t)_f_2;
        _env0[2] = (val_t)_g_2;
        val_t _f_3 = (val_t)_f_2;
        val_t _g_3 = (val_t)_g_2;
        val_t _r2 = syl_call(_f_3, 0LL);
        return _r2;
    }

    int main(void) {
        {
            closure_t* _c = (closure_t*)malloc(sizeof(closure_t));
            _c->fn = _outer_fn0;
            _c->env = NULL;
            _outer_0 = (val_t)_c;
        }
        val_t _r3 = syl_call(_outer_0, 5LL);
        (void)_r3;
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
    extern val_t syl_print_int(val_t);

    static val_t _syl_print_int_wrap0(val_t* _env, val_t _arg) {
        return syl_print_int(_arg);
    }

    static val_t _print_int_0;
    static val_t _print__u_fn0(void);

    static val_t _print__u_fn0(void) {
        (void)0;
        val_t _r0 = syl_call(_print_int_0, 0LL);
        return _r0;
    }

    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _syl_print_int_wrap0;
        _clo0->env = NULL;
        _print_int_0 = (val_t)_clo0;
        (void)_print__u_fn0();
        return 0;
    }
    |}]
;;

let%expect_test "external" =
  go
    {|
external f : int -> int = asdf;;
let _ = f 0;;
|};
  [%expect
    {|
    extern val_t asdf(val_t);

    static val_t _asdf_wrap0(val_t* _env, val_t _arg) {
        return asdf(_arg);
    }

    static val_t _f_0;
    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _asdf_wrap0;
        _clo0->env = NULL;
        _f_0 = (val_t)_clo0;
        val_t _r0 = syl_call(_f_0, 0LL);
        (void)_r0;
        return 0;
    }
    |}]
;;

let%expect_test "external" =
  go
    {|
external f : int -> int = asdf;;
let _ = (f @ erased) 0;;
|};
  [%expect
    {|
    extern val_t asdf(val_t);

    static val_t _asdf_wrap0(val_t* _env, val_t _arg) {
        return asdf(_arg);
    }

    static val_t _f_0;
    extern val_t asdf(val_t);

    static val_t _asdf_wrap1(val_t* _env, val_t _arg) {
        return asdf(_arg);
    }

    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _asdf_wrap0;
        _clo0->env = NULL;
        _f_0 = (val_t)_clo0;
        closure_t* _clo1 = (closure_t*)malloc(sizeof(closure_t));
        _clo1->fn = _asdf_wrap1;
        _clo1->env = NULL;
        val_t _r0 = syl_call((val_t)_clo1, 0LL);
        (void)_r0;
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
    extern val_t syl_print_int(val_t);

    static val_t _syl_print_int_wrap0(val_t* _env, val_t _arg) {
        return syl_print_int(_arg);
    }

    static val_t _print_int_0;
    static val_t _print__1_fn0(void) {
        val_t _x_0 = 1LL;
        val_t _r0 = syl_call(_print_int_0, _x_0);
        return _r0;
    }

    static val_t _print__0_fn0(void) {
        val_t _x_1 = 0LL;
        val_t _r1 = syl_call(_print_int_0, _x_1);
        return _r1;
    }

    int main(void) {
        closure_t* _clo0 = (closure_t*)malloc(sizeof(closure_t));
        _clo0->fn = _syl_print_int_wrap0;
        _clo0->env = NULL;
        _print_int_0 = (val_t)_clo0;
        (void)_print__0_fn0();
        (void)_print__1_fn0();
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
    static val_t _apply__l2_fn0(void);

    static val_t _apply__l2_fn0(void) {
        val_t _x_0 = 0LL;
        return (_x_0 + 1LL);
    }

    static val_t _x_1;
    int main(void) {
        _x_1 = _apply__l2_fn0();
        return 0;
    }
    |}]
;;
