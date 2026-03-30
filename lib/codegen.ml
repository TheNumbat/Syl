open! Core
open Lst

let prelude =
  {|
#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

using syl_env = char*;
using syl_unit = void;
using syl_bool = bool;
using syl_int = int64_t;

template<typename Arg, typename Ret>
struct syl_closure {
  Ret(*fn)(Arg, syl_env);
  syl_env env;

  Ret operator()(Arg arg) { return fn(arg, env); }
};

template<typename Ret>
struct syl_closure<syl_unit,Ret> {
  Ret(*fn)(syl_env);
  syl_env env;

  Ret operator()() { return fn(env); }
};

template<typename Ret>
using syl_thunk = syl_closure<syl_unit, Ret>;

template<typename...> struct syl_tuple;

template<>
struct syl_tuple<> {};

template<typename T>
struct syl_tuple<T> {
  T first;
};

template<typename T, typename... Ts>
struct syl_tuple<T, Ts...> {
  T first;
  syl_tuple<Ts...> rest;
};

template<typename... Ts>
struct syl_tuple<syl_unit, Ts...> {
  syl_tuple<Ts...> rest;
};

static syl_env syl_env_rec(size_t size) {
  return (syl_env)malloc(size);
}

template<typename... Env>
static syl_env syl_capture(Env... captures) {
  syl_env env = (syl_env)malloc(sizeof(syl_tuple<decltype(captures)...>));
  size_t offset = 0;
  ((offset = (offset + alignof(decltype(captures)) - 1) & ~(alignof(decltype(captures)) - 1),
    *(decltype(captures)*)(env + offset) = captures,
    offset += sizeof(captures)),
   ...);
  return env;
}

static syl_unit syl_assert(syl_bool cond) {
  assert(cond);
}
|}
  ^ Builtin.prelude
  ^ "//SYL_PRELUDE_END"
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
  | Dynamic | Phase -> ""
;;

let print_erased (mode : Modes.Erasure.t) =
  match mode with
  | Erased -> "ℰ"
  | Unerased -> ""
;;

let print_mode (mode : Modes.t) = print_static mode.staticity ^ print_erased mode.erasure

let rec print_key (key : Tst.Value.Concrete.t) =
  match key with
  | Scalar Unit -> "𝕌"
  | Scalar Bool -> "𝔹"
  | Scalar Int -> "𝕀"
  | Scalar Type -> "𝕋"
  | Unit -> "ø"
  | Bool true -> "T"
  | Bool false -> "F"
  | Int i -> Int64.to_string i
  | Closure i -> "λ" ^ Int.to_string i
  | Tuple elts ->
    let elts = List.map elts ~f:print_key in
    String.concat elts ~sep:"ₓ"
  | Arrow { arg; arg_mode; ret; ret_mode } ->
    let arg = print_key arg in
    let ret = print_key ret in
    let arg_mode = print_mode arg_mode in
    let ret_mode = print_mode ret_mode in
    Printf.sprintf "%s%sᐳ%s%s" arg_mode arg ret_mode ret
;;

let print_path (path : Path.t) =
  List.concat_map path ~f:(function
    | Id id -> [ Ident.name () id; "·" ]
    | Key k -> [ print_key k; "ₒ" ])
  |> List.rev
  |> List.tl_exn
  |> String.concat
  |> ( ^ ) "_"
;;

let rec print_ty (ty : Ty.t) =
  match ty with
  | Unit -> "syl_unit"
  | Bool -> "syl_bool"
  | Int -> "syl_int"
  | Env -> "syl_env"
  | Closure { arg_ty; ret_ty } ->
    sprintf "syl_closure<%s,%s>" (print_ty arg_ty) (print_ty_or_void ret_ty)
  | Thunk ty -> sprintf "syl_thunk<%s>" (print_ty_or_void ty)
  | Tuple elts -> sprintf "syl_tuple<%s>" (String.concat ~sep:", " (List.map elts ~f:print_ty))
  | Pack _ -> raise_s [%message "Unexpected type" (ty : Ty.t)]

and print_ty_or_void (ty : Ty.t) = if Ty.is_zero_size ty then "void" else print_ty ty

let print_expr_nonzero (expr : Expr.t) =
  match expr with
  | Scalar { value = Unit; _ } -> assert false
  | Scalar { value = Bool true; _ } -> "true"
  | Scalar { value = Bool false; _ } -> "false"
  | Scalar { value = Int i; _ } -> Int64.to_string i ^ "ll"
  | Make_env { captures = { size_in_bytes; _ }; _ } when size_in_bytes = 0 -> "NULL"
  | Make_env { captures = { entries; _ }; _ } ->
    let paths =
      Array.filter_map entries ~f:(fun { path; ty; _ } ->
        if Ty.is_zero_size ty then None else Some (print_path path))
    in
    Printf.sprintf "syl_capture(%s)" (String.concat_array ~sep:", " paths)
  | Make_closure { body; env; ty; _ } ->
    let env = Option.value_map env ~default:"NULL" ~f:print_path in
    Printf.sprintf "%s{%s, %s}" (print_ty ty) (print_path body) env
  | Apply_closure { fn; arg; arg_ty; _ } ->
    if Ty.is_zero_size arg_ty
    then Printf.sprintf "%s()" (print_path fn)
    else Printf.sprintf "%s(%s)" (print_path fn) (print_path arg)
  | Apply_thunk { fn; _ } -> Printf.sprintf "%s()" (print_path fn)
  | Make_tuple { elts; ty; _ } ->
    let paths =
      Array.filter_map elts ~f:(fun (path, ty) ->
        if Ty.is_zero_size ty then None else Some (print_path path))
    in
    Printf.sprintf "%s{%s}" (print_ty ty) (String.concat_array ~sep:", " paths)
  | Ident { path; _ } -> print_path path
;;

let print_expr_zero (expr : Expr.t) =
  match expr with
  | Scalar { value = Unit; _ } | Ident _ | Make_tuple _ -> ""
  | Apply_closure { fn; arg; arg_ty; _ } ->
    if Ty.is_zero_size arg_ty
    then Printf.sprintf "%s()" (print_path fn)
    else Printf.sprintf "%s(%s)" (print_path fn) (print_path arg)
  | Apply_thunk { fn; _ } -> Printf.sprintf "%s()" (print_path fn)
  | _ -> assert false
;;

let emit_return state return =
  if Ty.is_zero_size (Expr.ty return)
  then State.line state "%s;" (print_expr_zero return)
  else State.line state "return %s;" (print_expr_nonzero return)
;;

let rec emit_decl state (decl : Decl.t) =
  match decl with
  | Values { exprs; _ } ->
    Array.iter exprs ~f:(fun (path, expr) ->
      let ty = Expr.ty expr in
      if not (Ty.is_zero_size ty)
      then State.line state "static %s %s;" (print_ty ty) (print_path path))
  | Functions { paths; _ } ->
    Array.iter paths ~f:(fun (path, ty, _) ->
      State.line state "static %s %s;" (print_ty ty) (print_path path))
  | Closure_body { path; arg_ty; return; _ } ->
    if Ty.is_zero_size arg_ty
    then
      State.line
        state
        "static %s %s(syl_env);"
        (print_ty_or_void (Expr.ty return))
        (print_path path)
    else
      State.line
        state
        "static %s %s(%s, syl_env);"
        (print_ty (Expr.ty return))
        (print_path path)
        (print_ty arg_ty)
  | Thunk_body { path; return; _ } ->
    State.line state "static %s %s(syl_env);" (print_ty_or_void (Expr.ty return)) (print_path path)
  | External { path; symbol; arg_ty; ret_ty; _ } ->
    let arg = print_ty arg_ty in
    let ret = print_ty_or_void ret_ty in
    State.line state "extern %s %s(%s);" ret symbol arg;
    if Ty.is_zero_size arg_ty
    then State.line state "static %s %s(syl_env 𝒰)" ret (print_path path)
    else State.line state "static %s %s(%s _, syl_env 𝒰)" ret (print_path path) arg;
    (match Ty.is_zero_size arg_ty, Ty.is_zero_size ret_ty with
     | true, true -> State.scope state ~f:(fun () -> State.line state "%s();" symbol)
     | false, true -> State.scope state ~f:(fun () -> State.line state "%s(_);" symbol)
     | true, false -> State.scope state ~f:(fun () -> State.line state "return %s();" symbol)
     | false, false -> State.scope state ~f:(fun () -> State.line state "return %s(_);" symbol))

and emit_decls state decls = Array.iter decls ~f:(emit_decl state)

let rec emit_stmt state (stmt : Stmt.t) =
  match stmt with
  | Values { bind; exprs; _ } when Array.is_empty bind ->
    Array.iter exprs ~f:(fun (path, expr) ->
      let ty = Expr.ty expr in
      if Ty.is_zero_size ty
      then State.line state "%s;" (print_expr_zero expr)
      else State.line state "%s %s = %s;" (print_ty ty) (print_path path) (print_expr_nonzero expr))
  | Values ({ exprs; _ } as values) ->
    Array.iter exprs ~f:(fun (path, expr) ->
      let ty = Expr.ty expr in
      if not (Ty.is_zero_size ty) then State.line state "%s %s;" (print_ty ty) (print_path path));
    emit_bind_values state values
  | If { path; cond; then_bind; then_; else_bind; else_; _ } ->
    let path = print_path path in
    let ty = Expr.ty then_ in
    if not (Ty.is_zero_size ty) then State.line state "%s %s;" (print_ty ty) path;
    State.line state "if(%s)" (print_expr_nonzero cond);
    State.scope state ~f:(fun () ->
      emit_stmts state then_bind;
      if Ty.is_zero_size ty
      then State.line state "%s;" (print_expr_zero then_)
      else State.line state "%s = %s;" path (print_expr_nonzero then_));
    State.line state "else";
    State.scope state ~f:(fun () ->
      emit_stmts state else_bind;
      if Ty.is_zero_size ty
      then State.line state "%s;" (print_expr_zero else_)
      else State.line state "%s = %s;" path (print_expr_nonzero else_))
  | Functions ({ paths; _ } as functions) ->
    Array.iter paths ~f:(fun (path, ty, _) ->
      State.line state "%s %s;" (print_ty ty) (print_path path));
    emit_bind_functions state functions

and emit_stmts state (stmts : Stmt.t array) = Array.iter stmts ~f:(fun stmt -> emit_stmt state stmt)

and emit_bind_values state ({ exprs; bind; _ } : Stmt.values) =
  match Array.is_empty bind with
  | true ->
    Array.iter exprs ~f:(fun (path, expr) ->
      if Ty.is_zero_size (Expr.ty expr)
      then State.line state "%s;" (print_expr_zero expr)
      else State.line state "%s = %s;" (print_path path) (print_expr_nonzero expr))
  | false ->
    State.scope state ~f:(fun () ->
      emit_stmts state bind;
      Array.iter exprs ~f:(fun (path, expr) ->
        if Ty.is_zero_size (Expr.ty expr)
        then State.line state "%s;" (print_expr_zero expr)
        else State.line state "%s = %s;" (print_path path) (print_expr_nonzero expr)))

and emit_bind_functions state { Stmt.paths; captures = { entries; size_in_bytes }; _ } =
  State.scope state ~f:(fun () ->
    (match size_in_bytes with
     | 0 -> State.line state "syl_env 𝒰 = NULL;"
     | _ -> State.line state "syl_env 𝒰 = syl_env_rec(%d);" size_in_bytes);
    Array.iter paths ~f:(fun (path, ty, proc) ->
      State.line state "%s = %s{%s, 𝒰};" (print_path path) (print_ty ty) (print_path proc));
    Array.iter entries ~f:(fun { path; ty; offset_in_bytes } ->
      if not (Ty.is_zero_size ty)
      then State.line state "*(%s*)(𝒰 + %d) = %s;" (print_ty ty) offset_in_bytes (print_path path)))
;;

let emit_env_sub state captures =
  Array.iter captures ~f:(fun { Env.path; ty; offset_in_bytes } ->
    if not (Ty.is_zero_size ty)
    then (
      let ty = print_ty ty in
      State.line state "%s %s = *(%s*)(𝒰 + %d);" ty (print_path path) ty offset_in_bytes))
;;

let emit_procs_and_thunks state (lst : Program.t) =
  Array.iter lst ~f:(function
    | Closure_body { path; arg; arg_ty; captures; bind; return; _ } ->
      if Ty.is_zero_size arg_ty
      then
        State.line
          state
          "static %s %s(syl_env 𝒰)"
          (print_ty_or_void (Expr.ty return))
          (print_path path)
      else
        State.line
          state
          "static %s %s(%s %s, syl_env 𝒰)"
          (print_ty_or_void (Expr.ty return))
          (print_path path)
          (print_ty arg_ty)
          (print_path arg);
      State.scope state ~f:(fun () ->
        emit_env_sub state captures;
        emit_stmts state bind;
        emit_return state return)
    | Thunk_body { path; captures; bind; return; _ } ->
      State.line
        state
        "static %s %s(syl_env 𝒰)"
        (print_ty_or_void (Expr.ty return))
        (print_path path);
      State.scope state ~f:(fun () ->
        emit_env_sub state captures;
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
  Buffer.add_string state.buf Syl_std.runtime;
  emit_decls state lst;
  emit_procs_and_thunks state lst;
  emit_main state lst;
  Buffer.contents state.buf
;;
