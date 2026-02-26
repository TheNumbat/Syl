open! Core
open Lst

let prelude =
  {|
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef int64_t syl_repr;
typedef syl_repr* syl_env;
typedef struct {
  syl_repr(*fn)(syl_repr, syl_env);
  syl_env env;
} syl_unboxed_closure;
 typedef struct {
  syl_repr(*fn)(syl_env);
  syl_env env;
} syl_unboxed_thunk;

#define SYL_ENV_EMPTY NULL

typedef syl_repr syl_unit;
typedef syl_repr syl_bool;
typedef syl_repr syl_int;
typedef syl_repr syl_closure;
typedef syl_repr syl_thunk;

static syl_closure syl_mk_closure(syl_repr(*fn)(syl_repr, syl_env), syl_env env) {
  syl_unboxed_closure* closure = (syl_unboxed_closure*)malloc(sizeof(syl_unboxed_closure));
  closure->fn = fn;
  closure->env = env;
  return (syl_closure)closure;
}

static syl_closure syl_mk_thunk(syl_repr(*fn)(syl_env), syl_env env) {
  syl_unboxed_thunk* thunk = (syl_unboxed_thunk*)malloc(sizeof(syl_unboxed_thunk));
  thunk->fn = fn;
  thunk->env = env;
  return (syl_thunk)thunk;
}

static syl_env syl_env_rec(int n) {
  return (syl_env)malloc(n * sizeof(syl_repr));
}

static syl_env syl_capture(int n, ...) {
  syl_env env = (syl_env)malloc(n * sizeof(syl_repr));
  va_list args;
  va_start(args, n);
  for (int i = 0; i < n; i++) {
    env[i] = va_arg(args, syl_repr);
  }
  va_end(args);
  return env;
}

static syl_repr syl_app_closure(syl_closure closurev, syl_repr argv) {
  syl_unboxed_closure* closure = (syl_unboxed_closure*)closurev;
  return closure->fn(argv, closure->env);
}

static syl_repr syl_app_thunk(syl_thunk thunkv) {
  syl_unboxed_thunk* thunk = (syl_unboxed_thunk*)thunkv;
  return thunk->fn(thunk->env);
}

syl_unit syl_print_bool(syl_bool b) {
  printf("%s\n", b ? "true" : "false");
  return 0;
}
syl_unit syl_print_int(syl_int i) {
  printf("%ld\n", i);
  return 0;
}

//SYL_PRELUDE_END
|}
;;

module State = struct
  type t =
    { buf : Buffer.t
    ; mutable indent : int
    }

  let create () = { buf = Buffer.create (1 lsl 16); indent = 0 }

  let line t fmt =
    for _ = 1 to 2 * t.indent do
      Buffer.add_char t.buf ' '
    done;
    Printf.kbprintf (fun buf -> Buffer.add_char buf '\n') t.buf fmt
  ;;

  let scope t ~f =
    line t "{";
    t.indent <- t.indent + 1;
    let res = f () in
    t.indent <- t.indent - 1;
    line t "}";
    res
  ;;
end

let print_static (mode : Modes.Staticity.t) =
  match mode with
  | Static -> "𝒮"
  | Dynamic -> ""
;;

let print_erased (mode : Modes.Erasure.t) =
  match mode with
  | Erased -> "ℰ"
  | Unerased -> ""
;;

let print_mode (mode : Modes.Modes.t) = print_static mode.staticity ^ print_erased mode.erasure

let rec print_key (key : Tst.Value.Concrete.t) =
  match key with
  | Unit -> "ø"
  | Bool true -> "T"
  | Bool false -> "F"
  | Int i -> Int64.to_string i
  | Closure i -> "λ" ^ Int.to_string i
  | UnitT -> "𝕌"
  | BoolT -> "𝔹"
  | IntT -> "𝕀"
  | TypeT -> "𝕋"
  | ArrowT { arg; arg_mode; ret; ret_mode } ->
    let arg = print_key arg in
    let ret = print_key ret in
    let arg_mode = print_mode arg_mode in
    let ret_mode = print_mode ret_mode in
    Printf.sprintf "%s%s🡒%s%s" arg_mode arg ret_mode ret
;;

let print_path (path : Path.t) =
  List.concat_map path ~f:(function
    | Id id -> [ Ident.to_string id; "·" ]
    | Shadow n -> [ Int.to_string n; "ˢ" ]
    | Key k -> [ print_key k; "ₒ" ])
  |> List.rev
  |> List.tl_exn
  |> String.concat
  |> ( ^ ) "_"
;;

let print_ty (ty : Ty.t) =
  match ty with
  | Unit -> "syl_unit"
  | Bool -> "syl_bool"
  | Int -> "syl_int"
  | Env -> "syl_env"
  | Closure -> "syl_closure"
  | Thunk -> "syl_thunk"
  | Pack _ -> raise_s [%message "Unexpected type" (ty : Ty.t)]
;;

let print_unop (op : Cst.Unop.t) =
  match op with
  | Not -> "!"
  | Neg -> "-"
;;

let print_binop (op : Cst.Binop.t) =
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | And -> "&&"
  | Or -> "||"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Lte -> "<="
  | Gt -> ">"
  | Gte -> ">="
;;

let print_expr (expr : Expr.t) =
  match expr with
  | Scalar { value = Unit; _ } -> "0"
  | Scalar { value = Bool true; _ } -> "true"
  | Scalar { value = Bool false; _ } -> "false"
  | Scalar { value = Int i; _ } -> Int64.to_string i
  | Make_env { captures; _ } when Array.length captures = 0 -> "NULL"
  | Make_env { captures; _ } ->
    Array.map captures ~f:(fun (path, _) -> print_path path)
    |> String.concat_array ~sep:", "
    |> Printf.sprintf "syl_capture(%d, %s)" (Array.length captures)
  | Make_closure { body; env; _ } ->
    let env = Option.value_map env ~default:"SYL_ENV_EMPTY" ~f:print_path in
    Printf.sprintf "syl_mk_closure(%s, %s)" (print_path body) env
  | Make_thunk { body; env; _ } ->
    let env = Option.value_map env ~default:"SYL_ENV_EMPTY" ~f:print_path in
    Printf.sprintf "syl_mk_thunk(%s, %s)" (print_path body) env
  | Apply_closure { fn; arg; _ } ->
    Printf.sprintf "syl_app_closure(%s, %s)" (print_path fn) (print_path arg)
  | Apply_thunk { fn; _ } -> Printf.sprintf "syl_app_thunk(%s)" (print_path fn)
  | Unop { op; arg; _ } -> Printf.sprintf "%s%s" (print_unop op) (print_path arg)
  | Binop { op; lhs; rhs; _ } ->
    Printf.sprintf "%s %s %s" (print_path lhs) (print_binop op) (print_path rhs)
  | Ident { path; _ } -> print_path path
;;

let emit_return state return = State.line state "return %s;" (print_expr return)

let rec emit_decl state (decl : Decl.t) =
  match decl with
  | Values { exprs; _ } ->
    Array.iter exprs ~f:(fun (path, expr) ->
      State.line state "static %s %s;" (print_ty (Expr.ty expr)) (print_path path))
  | Functions { closures; thunks; _ } ->
    Array.iter closures ~f:(fun (path, _) ->
      State.line state "static syl_closure %s;" (print_path path));
    Array.iter thunks ~f:(fun (path, _) ->
      State.line state "static syl_thunk %s;" (print_path path))
  | Closure_body { path; arg_ty; return; _ } ->
    State.line
      state
      "static %s %s(%s, syl_env);"
      (print_ty (Expr.ty return))
      (print_path path)
      (print_ty arg_ty)
  | Thunk_body { path; return; _ } ->
    State.line state "static %s %s(syl_env);" (print_ty (Expr.ty return)) (print_path path)
  | External { path; symbol; arg_ty; ret_ty; _ } ->
    let arg = print_ty arg_ty in
    let ret = print_ty ret_ty in
    State.line state "extern %s %s(%s);" ret symbol arg;
    State.line state "static %s %s(%s _, syl_env 𝒰)" ret (print_path path) arg;
    State.scope state ~f:(fun () -> State.line state "return %s(_);" symbol)

and emit_decls state decls = Array.iter decls ~f:(emit_decl state)

let rec emit_stmt state (stmt : Stmt.t) =
  match stmt with
  | Values { bind; exprs; _ } when Array.is_empty bind ->
    Array.iter exprs ~f:(fun (path, expr) ->
      State.line state "%s %s = %s;" (print_ty (Expr.ty expr)) (print_path path) (print_expr expr))
  | Values ({ exprs; _ } as values) ->
    Array.iter exprs ~f:(fun (path, expr) ->
      State.line state "%s %s;" (print_ty (Expr.ty expr)) (print_path path));
    emit_bind_values state values
  | If { path; cond; then_bind; then_; else_bind; else_; _ } ->
    let path = print_path path in
    State.line state "%s %s;" (print_ty (Expr.ty then_)) path;
    State.line state "if(%s)" (print_expr cond);
    State.scope state ~f:(fun () ->
      emit_stmts state then_bind;
      State.line state "%s = %s;" path (print_expr then_));
    State.line state "else";
    State.scope state ~f:(fun () ->
      emit_stmts state else_bind;
      State.line state "%s = %s;" path (print_expr else_))
  | Functions ({ closures; thunks; _ } as functions) ->
    Array.iter closures ~f:(fun (path, _) -> State.line state "syl_closure %s;" (print_path path));
    Array.iter thunks ~f:(fun (path, _) -> State.line state "syl_thunk %s;" (print_path path));
    emit_bind_functions state functions

and emit_stmts state (stmts : Stmt.t array) = Array.iter stmts ~f:(fun stmt -> emit_stmt state stmt)

and emit_bind_values state ({ exprs; bind; _ } : Stmt.values) =
  match Array.is_empty bind with
  | true ->
    Array.iter exprs ~f:(fun (path, expr) ->
      State.line state "%s = %s;" (print_path path) (print_expr expr))
  | false ->
    State.scope state ~f:(fun () ->
      emit_stmts state bind;
      Array.iter exprs ~f:(fun (path, expr) ->
        State.line state "%s = %s;" (print_path path) (print_expr expr)))

and emit_bind_functions state { Stmt.closures; thunks; captures; _ } =
  State.scope state ~f:(fun () ->
    (match Array.length captures with
     | 0 -> State.line state "syl_env 𝒰 = NULL;"
     | n -> State.line state "syl_env 𝒰 = syl_env_rec(%d);" n);
    Array.iter closures ~f:(fun (path, proc) ->
      State.line state "%s = syl_mk_closure(%s, 𝒰);" (print_path path) (print_path proc));
    Array.iter thunks ~f:(fun (path, proc) ->
      State.line state "%s = syl_mk_thunk(%s, 𝒰);" (print_path path) (print_path proc));
    Array.iteri captures ~f:(fun i (path, _) -> State.line state "𝒰[%d] = %s;" i (print_path path)))
;;

let emit_captures state captures =
  Array.iteri captures ~f:(fun idx (path, ty) ->
    State.line state "%s %s = 𝒰[%d];" (print_ty ty) (print_path path) idx)
;;

let emit_procs_and_thunks state (lst : Program.t) =
  Array.iter lst ~f:(function
    | Closure_body { path; arg; arg_ty; captures; bind; return; _ } ->
      State.line
        state
        "static %s %s(%s %s, syl_env 𝒰)"
        (print_ty (Expr.ty return))
        (print_path path)
        (print_ty arg_ty)
        (print_path arg);
      State.scope state ~f:(fun () ->
        emit_captures state captures;
        emit_stmts state bind;
        emit_return state return)
    | Thunk_body { path; captures; bind; return; _ } ->
      State.line state "static %s %s(syl_env 𝒰)" (print_ty (Expr.ty return)) (print_path path);
      State.scope state ~f:(fun () ->
        emit_captures state captures;
        emit_stmts state bind;
        emit_return state return)
    | Functions _ | Values _ | External _ -> ())
;;

let emit_main state (lst : Program.t) =
  State.line state "int main()";
  State.scope state ~f:(fun () ->
    Array.iter lst ~f:(function
      | Values values -> emit_bind_values state values
      | Functions functions -> emit_bind_functions state functions
      | Closure_body _ | Thunk_body _ | External _ -> ());
    State.line state "return 0;")
;;

let c (lst : Program.t) : string =
  let state = State.create () in
  Buffer.add_string state.buf prelude;
  emit_decls state lst;
  emit_procs_and_thunks state lst;
  emit_main state lst;
  Buffer.contents state.buf
;;
