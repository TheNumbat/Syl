open! Core
open Lst

let prelude =
  {|
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

using syl_unit = void;
using syl_type = void;
using syl_tuple_void = void;
using syl_bool = bool;
using syl_int = int64_t;
using syl_env = char*;
using syl_ref = char*;

struct syl_fn {
  void* fn;
  syl_env env;

  operator syl_env() { return env; }
};

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

template<size_t Size, size_t Align>
struct syl_variant {
  syl_int tag;
  alignas(Align) char payload[Size];
};

template<size_t Align>
struct syl_variant<0, Align> {
  syl_int tag;
};

template<typename Variant>
static Variant syl_inject(syl_int tag) {
  Variant v{};
  v.tag = tag;
  return v;
}

template<typename Variant, typename Payload>
static Variant syl_inject(syl_int tag, Payload payload) {
  Variant v{};
  v.tag = tag;
  memcpy(v.payload, &payload, sizeof(Payload));
  return v;
}

template<typename Payload, typename Variant>
static Payload syl_project(Variant v) {
  Payload payload;
  memcpy(&payload, v.payload, sizeof(Payload));
  return payload;
}

static syl_ref syl_ref_empty() {
  return NULL;
}

template<typename T>
static syl_ref syl_ref_make(T payload) {
  syl_ref p = (syl_ref)malloc(sizeof(T));
  memcpy(p, &payload, sizeof(T));
  return p;
}

template<typename T>
static T syl_deref(syl_ref p) {
  T payload;
  memcpy(&payload, p, sizeof(T));
  return payload;
}

static syl_env syl_env_rec(size_t size) {
  return (syl_env)malloc(size);
}

template<typename... Env>
static syl_unit syl_fill_env(syl_env env, Env... captures) {
  size_t offset = 0;
  ((offset = (offset + alignof(decltype(captures)) - 1) & ~(alignof(decltype(captures)) - 1),
    *(decltype(captures)*)(env + offset) = captures,
    offset += sizeof(captures)),
   ...);
}

template<typename... Env>
static syl_env syl_capture(Env... captures) {
  syl_env env = syl_env_rec(sizeof(syl_tuple<decltype(captures)...>));
  syl_fill_env(env, captures...);
  return env;
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

let print_key key = "ᵏ" ^ Int.to_string (Hashcons.Tag.to_int key)

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
  | Type -> "syl_type"
  | Env -> "syl_env"
  | Fn -> "syl_fn"
  | Tuple _ when Ty.is_zero_size ty -> "syl_tuple_void"
  | Tuple elts ->
    let args =
      Nonempty_list.map elts ~f:print_ty |> Nonempty_list.to_list |> String.concat ~sep:", "
    in
    sprintf "syl_tuple<%s>" args
  | Variant constructors ->
    sprintf
      "syl_variant<%d,%d>"
      (Ty.payload_size_in_mem constructors)
      (Ty.payload_align_in_mem constructors)
  | Ref -> "syl_ref"
;;

let variant_tag (ty : Ty.t) label =
  match ty with
  | Variant constructors ->
    (match Map.rank constructors label with
     | Some rank -> rank
     | None -> raise_s [%message "Bug: unknown constructor" (label : Ident.Label.t) (ty : Ty.t)])
  | _ -> raise_s [%message "Bug: expected variant type" (ty : Ty.t)]
;;

let print_expr_nonzero (expr : Expr.t) =
  match expr with
  | Scalar { value = Unit | Type; _ } | Fill_env_rec _ ->
    raise_s [%message "Bug: expected non-zero size" (expr : Expr.t)]
  | Scalar { value = Bool true; _ } -> "true"
  | Scalar { value = Bool false; _ } -> "false"
  | Scalar { value = Int i; _ } -> Int64.to_string i ^ "ll"
  | (Make_env { env = { length; _ }; _ } | Make_env_rec { length; _ }) when length = 0 -> "NULL"
  | Make_env { env = { entries; _ }; _ } ->
    let paths =
      Array.filter_map entries ~f:(fun { path; ty; _ } ->
        if Ty.is_zero_size ty then None else Some (print_path path))
    in
    Printf.sprintf "syl_capture(%s)" (String.concat_array ~sep:", " paths)
  | Make_env_rec { length; _ } -> Printf.sprintf "syl_env_rec(%d)" length
  | Make_closure { body; env; _ } ->
    let env = Option.value_map env ~f:print_path ~default:"NULL" in
    Printf.sprintf "syl_fn{(void*)%s, %s}" (print_path body) env
  | Make_binder { env; _ } -> Printf.sprintf "syl_fn{(void*)0, %s}" (print_path env)
  | Apply_closure { fn; arg; arg_ty; ty; _ } ->
    if Ty.is_zero_size arg_ty
    then
      Printf.sprintf "((%s(*)(syl_env))%s.fn)(%s.env)" (print_ty ty) (print_path fn) (print_path fn)
    else
      Printf.sprintf
        "((%s(*)(%s, syl_env))%s.fn)(%s, %s.env)"
        (print_ty ty)
        (print_ty arg_ty)
        (print_path fn)
        (print_path arg)
        (print_path fn)
  | Extcall { symbol; arg; arg_ty; _ } ->
    if Ty.is_zero_size arg_ty
    then Printf.sprintf "%s()" symbol
    else Printf.sprintf "%s(%s)" symbol (print_path arg)
  | Apply_thunk { fn; env; _ } -> Printf.sprintf "%s(%s)" (print_path fn) (print_path env)
  | Apply_proc { fn; arg; arg_ty; env; _ } ->
    if Ty.is_zero_size arg_ty
    then Printf.sprintf "%s(%s)" (print_path fn) (print_path env)
    else Printf.sprintf "%s(%s, %s)" (print_path fn) (print_path arg) (print_path env)
  | Make_tuple { elts; ty; _ } ->
    let paths =
      Array.filter_map elts ~f:(fun (path, ty) ->
        if Ty.is_zero_size ty then None else Some (print_path path))
    in
    Printf.sprintf "%s{%s}" (print_ty ty) (String.concat_array ~sep:", " paths)
  | Make_variant { label; payload; ty; _ } ->
    let tag = variant_tag ty label in
    (match payload with
     | Some (path, payload_ty) when not (Ty.is_zero_size payload_ty) ->
       sprintf "syl_inject<%s>(%dll, %s)" (print_ty ty) tag (print_path path)
     | Some _ | None -> sprintf "syl_inject<%s>(%dll)" (print_ty ty) tag)
  | Ident { path; _ } -> print_path path
  | Tuple_get { tuple; index; _ } ->
    let buf = Buffer.create 32 in
    Buffer.add_string buf (print_path tuple);
    for _ = 1 to index do
      Buffer.add_string buf ".rest"
    done;
    Buffer.add_string buf ".first";
    Buffer.contents buf
  | Payload_get { variant; ty; _ } ->
    sprintf "syl_project<%s>(%s)" (print_ty ty) (print_path variant)
  | Tag_test { variant; variant_ty; label; _ } ->
    sprintf "%s.tag == %dll" (print_path variant) (variant_tag variant_ty label)
  | Make_ref { payload; payload_ty; _ } ->
    if Ty.is_zero_size payload_ty
    then "syl_ref_empty()"
    else sprintf "syl_ref_make(%s)" (print_path payload)
  | Ref_get { ref; ty; _ } -> sprintf "syl_deref<%s>(%s)" (print_ty ty) (print_path ref)
;;

let print_expr_zero (expr : Expr.t) =
  match expr with
  | Scalar { value = Unit | Type; _ }
  | Ident _ | Make_tuple _ | Tuple_get _ | Payload_get _ | Ref_get _ -> ""
  | Fill_env_rec { env = { length; _ }; _ } when length = 0 -> ""
  | Fill_env_rec { path; env = { entries; _ }; _ } ->
    let paths =
      Array.filter_map entries ~f:(fun { path; ty; _ } ->
        if Ty.is_zero_size ty then None else Some (print_path path))
    in
    Printf.sprintf "syl_fill_env(%s, %s)" (print_path path) (String.concat_array ~sep:", " paths)
  | Apply_closure { fn; arg; arg_ty; ty; _ } ->
    if Ty.is_zero_size arg_ty
    then
      Printf.sprintf "((%s(*)(syl_env))%s.fn)(%s.env)" (print_ty ty) (print_path fn) (print_path fn)
    else
      Printf.sprintf
        "((%s(*)(%s, syl_env))%s.fn)(%s, %s.env)"
        (print_ty ty)
        (print_ty arg_ty)
        (print_path fn)
        (print_path arg)
        (print_path fn)
  | Apply_thunk { fn; env; _ } -> Printf.sprintf "%s(%s)" (print_path fn) (print_path env)
  | Apply_proc { fn; arg; arg_ty; env; _ } ->
    if Ty.is_zero_size arg_ty
    then Printf.sprintf "%s(%s)" (print_path fn) (print_path env)
    else Printf.sprintf "%s(%s, %s)" (print_path fn) (print_path arg) (print_path env)
  | Extcall { symbol; arg; arg_ty; _ } ->
    if Ty.is_zero_size arg_ty
    then Printf.sprintf "%s()" symbol
    else Printf.sprintf "%s(%s)" symbol (print_path arg)
  | Scalar _
  | Make_env _
  | Make_env_rec _
  | Make_closure _
  | Make_binder _
  | Make_variant _
  | Make_ref _
  | Tag_test _ -> raise_s [%message "Bug: expected zero-size" (expr : Expr.t)]
;;

let emit_decl state path ty =
  if not (Ty.is_zero_size ty) then State.line state "%s %s;" (print_ty ty) (print_path path)
;;

let emit_bind state path expr =
  if Ty.is_zero_size (Expr.ty expr)
  then State.line state "%s;" (print_expr_zero expr)
  else State.line state "%s = %s;" (print_path path) (print_expr_nonzero expr)
;;

let emit_return state expr =
  if Ty.is_zero_size (Expr.ty expr)
  then State.line state "%s;" (print_expr_zero expr)
  else State.line state "return %s;" (print_expr_nonzero expr)
;;

let rec emit_block state path (block : Block.t) =
  match block with
  | Block { bindings; return } when Array.is_empty bindings -> emit_bind state path return
  | Block { bindings; return } ->
    Array.iter bindings ~f:(fun (path, block) -> emit_decl state path (Block.ty block));
    State.scope state ~f:(fun () ->
      Array.iter bindings ~f:(fun (path, block) -> emit_block state path block));
    emit_bind state path return
  | If { cond; then_; else_; _ } ->
    State.line state "if(%s)" (print_expr_nonzero cond);
    State.scope state ~f:(fun () -> emit_block state path then_);
    State.line state "else";
    State.scope state ~f:(fun () -> emit_block state path else_)
  | Match { cases; tree; _ } ->
    State.scope state ~f:(fun () ->
      Array.iter cases ~f:(fun { bindings; _ } ->
        Array.iter bindings ~f:(fun (path, ty) -> emit_decl state path ty));
      emit_switch state path tree;
      Array.iteri cases ~f:(fun i { block; _ } ->
        State.line state "%s_%d:" (print_path path) i;
        State.scope state ~f:(fun () -> emit_block state path block);
        State.line state "goto %s_exit;" (print_path path));
      State.line state "%s_exit:;" (print_path path))

and emit_block_return state (block : Block.t) =
  match block with
  | Block { bindings; return } when Array.is_empty bindings -> emit_return state return
  | Block { bindings; return } ->
    Array.iter bindings ~f:(fun (path, block) -> emit_decl state path (Block.ty block));
    State.scope state ~f:(fun () ->
      Array.iter bindings ~f:(fun (path, block) -> emit_block state path block));
    emit_return state return
  | _ -> raise_s [%message "Bug: expected block" (block : Block.t)]

and emit_switch state path (tree : Block.tree) =
  match tree with
  | Leaf { case; bindings } ->
    State.scope state ~f:(fun () ->
      Array.iter bindings ~f:(fun (path, block) -> emit_block state path block);
      State.line state "goto %s_%d;" (print_path path) case)
  | Split { cond; then_; else_ } ->
    State.scope state ~f:(fun () ->
      let cond =
        match cond with
        | Block.Block { bindings; return } ->
          Array.iter bindings ~f:(fun (path, block) ->
            emit_decl state path (Block.ty block);
            emit_block state path block);
          return
        | _ -> raise_s [%message "Bug: expected block condition" (cond : Block.t)]
      in
      State.line state "if(%s)" (print_expr_nonzero cond);
      emit_switch state path then_;
      State.line state "else";
      emit_switch state path else_)
;;

let emit_proc_decl state (proc : Proc.t) =
  match proc with
  | Closure { path; arg_ty; body; _ } ->
    if Ty.is_zero_size arg_ty
    then State.line state "static %s %s(syl_env);" (print_ty (Block.ty body)) (print_path path)
    else
      State.line
        state
        "static %s %s(%s, syl_env);"
        (print_ty (Block.ty body))
        (print_path path)
        (print_ty arg_ty)
  | External { path; arg_ty; ret_ty; _ } ->
    if Ty.is_zero_size arg_ty
    then State.line state "static %s %s(syl_env);" (print_ty ret_ty) (print_path path)
    else
      State.line
        state
        "static %s %s(%s, syl_env);"
        (print_ty ret_ty)
        (print_path path)
        (print_ty arg_ty)
  | Thunk { path; body; _ } ->
    State.line state "static %s %s(syl_env);" (print_ty (Block.ty body)) (print_path path)
;;

let emit_bind_env state captures =
  Array.iter captures ~f:(fun { Env.path; ty; offset } ->
    if not (Ty.is_zero_size ty)
    then (
      let ty = print_ty ty in
      State.line state "%s %s = *(%s*)(𝒰 + %d);" ty (print_path path) ty offset))
;;

let emit_proc state (proc : Proc.t) =
  match proc with
  | Closure { path; arg; arg_ty; env; body; _ } ->
    if Ty.is_zero_size arg_ty
    then State.line state "static %s %s(syl_env 𝒰)" (print_ty (Block.ty body)) (print_path path)
    else
      State.line
        state
        "static %s %s(%s %s, syl_env 𝒰)"
        (print_ty (Block.ty body))
        (print_path path)
        (print_ty arg_ty)
        (print_path arg);
    State.scope state ~f:(fun () ->
      emit_bind_env state env.entries;
      emit_block_return state body)
  | External { path; symbol; arg_ty; ret_ty; _ } ->
    let arg = print_ty arg_ty in
    let ret = print_ty ret_ty in
    if Ty.is_zero_size arg_ty
    then State.line state "static %s %s(syl_env 𝒰)" ret (print_path path)
    else State.line state "static %s %s(%s _, syl_env 𝒰)" ret (print_path path) arg;
    (match Ty.is_zero_size arg_ty, Ty.is_zero_size ret_ty with
     | true, true -> State.scope state ~f:(fun () -> State.line state "%s();" symbol)
     | false, true -> State.scope state ~f:(fun () -> State.line state "%s(_);" symbol)
     | true, false -> State.scope state ~f:(fun () -> State.line state "return %s();" symbol)
     | false, false -> State.scope state ~f:(fun () -> State.line state "return %s(_);" symbol))
  | Thunk { path; env; body; _ } ->
    State.line state "static %s %s(syl_env 𝒰)" (print_ty (Block.ty body)) (print_path path);
    State.scope state ~f:(fun () ->
      emit_bind_env state env.entries;
      emit_block_return state body)
;;

let emit_main state main =
  State.line state "int main()";
  State.scope state ~f:(fun () ->
    Array.iter main ~f:(fun (path, block) -> emit_decl state path (Block.ty block));
    Array.iter main ~f:(fun (path, block) -> emit_block state path block);
    State.line state "return 0;")
;;

let cpp (lst : Program.t) : string =
  let state = State.create () in
  Buffer.add_string state.buf prelude;
  Buffer.add_string state.buf Syl_std.runtime;
  Array.iter lst.procs ~f:(emit_proc_decl state);
  Array.iter lst.procs ~f:(emit_proc state);
  emit_main state lst.bindings;
  Buffer.contents state.buf
;;
