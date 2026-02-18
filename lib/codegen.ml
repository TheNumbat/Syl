open! Core

(* Entirely vibe coded; to be replaced with llvm eventually *)

let preamble =
  {|#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef int64_t val_t;

typedef struct {
    val_t (*fn)(val_t* env, val_t arg);
    val_t* env;
} closure_t;

static val_t syl_call(val_t f, val_t arg) {
    closure_t* c = (closure_t*)f;
    return c->fn(c->env, arg);
}

static val_t syl_print_unit(val_t x) {
    (void)x;
    printf("()\n");
    return 0;
}

static val_t syl_print_int(val_t x) {
    printf("%lld\n", (long long)x);
    return 0;
}

static val_t syl_print_bool(val_t x) {
    printf("%s\n", x ? "true" : "false");
    return 0;
}
// SYL_PREAMBLE_END
|}
;;

type name_prefix =
  | Var of string
  | Result
  | Cond
  | If_result
  | Lambda
  | Closure
  | Env
  | Fn of string
  | Wrap of string

let name_prefix_to_string = function
  | Var name -> Printf.sprintf "_%s_" name
  | Result -> "_r"
  | Cond -> "_c"
  | If_result -> "_if"
  | Lambda -> "_lam"
  | Closure -> "_clo"
  | Env -> "_env"
  | Fn name -> Printf.sprintf "_%s_fn" name
  | Wrap name -> Printf.sprintf "_%s_wrap" name
;;

type ctx =
  { top : Buffer.t
  ; main : Buffer.t
  ; counters : (string, int) Hashtbl.t
  }

type scope =
  { buf : Buffer.t
  ; indent : int
  ; is_top : bool
  }

type env =
  { vars : string Map.M(String).t
  ; prefixes : string Map.M(String).t
  }

let empty_env = { vars = Map.empty (module String); prefixes = Map.empty (module String) }
let id = Ident.to_string

let set_pack_prefix env ~name ~prefix =
  { env with prefixes = Map.set env.prefixes ~key:name ~data:prefix }
;;

let find_pack_prefix env name = Map.find env.prefixes name

let rec mangled (m : Tst.Value.Concrete.t) : string =
  match m with
  | Unit -> "u"
  | Bool b -> if b then "t" else "f"
  | Int i -> if Int64.(i >= zero) then Int64.to_string i else "m" ^ Int64.to_string (Int64.neg i)
  | Closure n -> Printf.sprintf "l%d" n
  | UnitT -> "U"
  | BoolT -> "B"
  | IntT -> "I"
  | TypeT -> "T"
  | ArrowT { arg; ret; _ } -> Printf.sprintf "%s_%s" (mangled arg) (mangled ret)
;;

let mangled_key name mangle = Printf.sprintf "%s__%s" name (mangled mangle)

type flat_fun =
  | Fn of
      { key : string
      ; arg : Ident.t
      ; body : Ir.Expr.t
      }
  | Alias of
      { key : string
      ; target : string
      }
  | Expr of
      { key : string
      ; body : Ir.Expr.t
      }

let flat_key = function
  | Fn { key; _ } | Alias { key; _ } | Expr { key; _ } -> key
;;

let flatten_funs funs =
  Nonempty_list.to_list funs
  |> List.concat_map ~f:(fun (f : Ir.Expr.fun_) ->
    match f with
    | Mono { var; arg; body; _ } -> [ Fn { key = id var; arg; body } ]
    | Pack { var; pack; _ } ->
      Vec.fold pack ~init:[] ~f:(fun acc { arg; body; _ } ->
        let key = mangled_key (id var) arg in
        match (body : Ir.Expr.t) with
        | Lambda { arg; body; _ } -> Fn { key; arg; body } :: acc
        | Symbol { id = v; arg; _ } -> Alias { key; target = mangled_key (id v) arg } :: acc
        | _ -> Expr { key; body } :: acc))
;;

let fresh ctx prefix =
  let s = name_prefix_to_string prefix in
  let n = Hashtbl.find ctx.counters s |> Option.value ~default:0 in
  Hashtbl.set ctx.counters ~key:s ~data:(n + 1);
  Printf.sprintf "%s%d" s n
;;

let emit buf indent fmt =
  Printf.ksprintf
    (fun s ->
       for _ = 1 to indent do
         Buffer.add_string buf "    "
       done;
       Buffer.add_string buf s;
       Buffer.add_char buf '\n')
    fmt
;;

let declare_var ctx scope name rhs env =
  let c_var = fresh ctx (Var name) in
  if scope.is_top
  then (
    emit ctx.top 0 "static val_t %s;" c_var;
    emit scope.buf scope.indent "%s = %s;" c_var rhs)
  else emit scope.buf scope.indent "val_t %s = %s;" c_var rhs;
  { env with vars = Map.set env.vars ~key:name ~data:c_var }
;;

let find_env_exn vars key =
  match Map.find vars key with
  | Some c -> c
  | None -> raise_s [%message "Internal codegen error: unbound symbol" (key : string)]
;;

let rec free_vars (expr : Ir.Expr.t) : String.Set.t =
  match expr with
  | Literal { value; _ } -> free_vars_value value
  | Var { id = v; _ } -> String.Set.singleton (id v)
  | Let { var; bind; rest; _ } -> Set.union (free_vars bind) (Set.remove (free_vars rest) (id var))
  | Fun { funs; rest; _ } ->
    let flat = flatten_funs funs in
    let bound = List.map flat ~f:flat_key |> String.Set.of_list in
    let fv_funs =
      List.map flat ~f:(function
        | Fn { arg; body; _ } -> Set.remove (free_vars body) (id arg)
        | Alias { target; _ } -> String.Set.singleton target
        | Expr { body; _ } -> free_vars body)
      |> String.Set.union_list
    in
    Set.diff (Set.union fv_funs (free_vars rest)) bound
  | Lambda { arg; body; _ } -> Set.remove (free_vars body) (id arg)
  | Apply { fn; arg; _ } -> Set.union (free_vars fn) (free_vars arg)
  | Unop { arg; _ } -> free_vars arg
  | Binop { lhs; rhs; _ } -> Set.union (free_vars lhs) (free_vars rhs)
  | If { cond; then_; else_; _ } ->
    String.Set.union_list [ free_vars cond; free_vars then_; free_vars else_ ]
  | Pack { pack; _ } ->
    Vec.fold pack ~init:String.Set.empty ~f:(fun acc { body; _ } -> Set.union acc (free_vars body))
  | Symbol { id = v; arg; _ } -> String.Set.singleton (mangled_key (id v) arg)

and free_vars_value (value : Ir.Value.t) : String.Set.t =
  match value with
  | Unit | Bool _ | Int _ | External _ -> String.Set.empty
  | Closure { arg; body; _ } -> Set.remove (free_vars body) (id arg)
  | Pack pack ->
    Vec.fold pack ~init:String.Set.empty ~f:(fun acc { body; _ } -> Set.union acc (free_vars body))
;;

(* Copy all env entries under [src_prefix]__* to [dst_name]__* *)
let alias_pack env src_prefix dst_name =
  let pfx = src_prefix ^ "__" in
  let vars =
    Map.fold env.vars ~init:env.vars ~f:(fun ~key ~data acc ->
      match String.chop_prefix key ~prefix:pfx with
      | Some suffix -> Map.set acc ~key:(dst_name ^ "__" ^ suffix) ~data
      | None -> acc)
  in
  { vars; prefixes = Map.set env.prefixes ~key:dst_name ~data:src_prefix }
;;

let local_pack_prefix prefix var = Printf.sprintf "%s_%s" prefix var

let resolve_pack_source env id arg =
  let direct = mangled_key id arg in
  if Map.mem env.vars direct
  then direct
  else (
    match find_pack_prefix env id with
    | Some prefix -> mangled_key prefix arg
    | None -> direct)
;;

type pack_bind =
  | Inline_pack of Ir.Pack.t
  | Pack_symbol of
      { id : Ident.t
      ; arg : Tst.Value.Concrete.t
      }
  | Pack_var of Ident.t

let classify_pack_bind (expr : Ir.Expr.t) : pack_bind option =
  match expr with
  | Pack { pack; _ } | Literal { value = Pack pack; _ } -> Some (Inline_pack pack)
  | Symbol { id; arg; ty = Pack; _ } -> Some (Pack_symbol { id; arg })
  | Var { id; ty = Pack; _ } -> Some (Pack_var id)
  | _ -> None
;;

let rec arity = function
  | Ir.Ty.Arrow { ret_ty; _ } -> 1 + arity ret_ty
  | _ -> 0
;;

(* Generate currying wrappers for an external C function.
   Emits wrapper functions to ctx.top and closure allocation to buf at indent.
   Returns a C expression string for the resulting closure/value. *)
let compile_external_inline ctx buf indent symbol ty =
  let n = arity ty in
  if n = 0
  then (
    emit ctx.top 0 "extern val_t %s;" symbol;
    symbol)
  else (
    let args_decl = List.init n ~f:(fun _ -> "val_t") |> String.concat ~sep:", " in
    emit ctx.top 0 "extern val_t %s(%s);" symbol args_decl;
    emit ctx.top 0 "";
    (* Generate wrapper functions: level 0 (outer) to n-1 (inner).
       Level i has i captured args in env and takes one more arg.
       Level n-1 (innermost) calls the actual C symbol. *)
    let wrapper_names = List.init n ~f:(fun _ -> fresh ctx (Wrap symbol)) in
    for level = n - 1 downto 0 do
      let fn_name = List.nth_exn wrapper_names level in
      emit ctx.top 0 "static val_t %s(val_t* _env, val_t _arg) {" fn_name;
      if level = n - 1
      then (
        let args = List.init level ~f:(fun i -> Printf.sprintf "_env[%d]" i) @ [ "_arg" ] in
        emit ctx.top 1 "return %s(%s);" symbol (String.concat ~sep:", " args))
      else (
        let next_fn = List.nth_exn wrapper_names (level + 1) in
        let n_captured = level + 1 in
        emit ctx.top 1 "closure_t* _c = (closure_t*)malloc(sizeof(closure_t));";
        emit ctx.top 1 "_c->fn = %s;" next_fn;
        emit ctx.top 1 "_c->env = (val_t*)malloc(%d * sizeof(val_t));" n_captured;
        for i = 0 to level - 1 do
          emit ctx.top 1 "_c->env[%d] = _env[%d];" i i
        done;
        emit ctx.top 1 "_c->env[%d] = _arg;" level;
        emit ctx.top 1 "return (val_t)_c;");
      emit ctx.top 0 "}";
      emit ctx.top 0 ""
    done;
    let clo = fresh ctx Closure in
    emit buf indent "closure_t* %s = (closure_t*)malloc(sizeof(closure_t));" clo;
    emit buf indent "%s->fn = %s;" clo (List.hd_exn wrapper_names);
    emit buf indent "%s->env = NULL;" clo;
    Printf.sprintf "(val_t)%s" clo)
;;

(* Compile an expression. Emits C statements to [buf].
   Returns a C expression string for the result value. *)
let rec compile_expr ctx (env : env) buf indent (expr : Ir.Expr.t) : string =
  match expr with
  | Literal { value; _ } -> compile_value ctx env buf indent value
  | Var { id = v; _ } ->
    let key = id v in
    (match Map.find env.vars key with
     | Some c -> c
     | None -> "0")
  | Let { var; bind; rest; _ } ->
    let env =
      match classify_pack_bind bind with
      | Some (Inline_pack pack) ->
        let scope = { buf; indent; is_top = false } in
        expand_pack ctx env scope (id var) pack |> set_pack_prefix ~name:(id var) ~prefix:(id var)
      | Some (Pack_symbol { id = v; arg }) ->
        let src = resolve_pack_source env (id v) arg in
        alias_pack env src (id var)
      | Some (Pack_var v) ->
        let src = Option.value (find_pack_prefix env (id v)) ~default:(id v) in
        alias_pack env src (id var)
      | None ->
        let rhs = compile_expr ctx env buf indent bind in
        if Ident.is_anon var
        then (
          emit buf indent "(void)%s;" rhs;
          env)
        else (
          let c_var = fresh ctx (Var (id var)) in
          emit buf indent "val_t %s = %s;" c_var rhs;
          { env with vars = Map.set env.vars ~key:(id var) ~data:c_var })
    in
    compile_expr ctx env buf indent rest
  | Lambda { arg; body; _ } -> compile_closure ctx env buf indent arg body
  | Fun { funs; rest; _ } ->
    let env = compile_local_funs ctx env buf indent funs in
    compile_expr ctx env buf indent rest
  | Apply { fn; arg; _ } ->
    let f = compile_expr ctx env buf indent fn in
    let a = compile_expr ctx env buf indent arg in
    let r = fresh ctx Result in
    emit buf indent "val_t %s = syl_call(%s, %s);" r f a;
    r
  | Unop { op; arg; _ } ->
    let a = compile_expr ctx env buf indent arg in
    (match (op : Cst.Unop.t) with
     | Not -> Printf.sprintf "(!%s)" a
     | Neg -> Printf.sprintf "(-%s)" a)
  | Binop { op; lhs; rhs; _ } ->
    let l = compile_expr ctx env buf indent lhs in
    let r = compile_expr ctx env buf indent rhs in
    let c_op =
      match (op : Cst.Binop.t) with
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
    in
    Printf.sprintf "(%s %s %s)" l c_op r
  | If { cond; then_; else_; _ } ->
    let c = compile_expr ctx env buf indent cond in
    let cv = fresh ctx Cond in
    emit buf indent "val_t %s = %s;" cv c;
    let r = fresh ctx If_result in
    emit buf indent "val_t %s;" r;
    emit buf indent "if (%s) {" cv;
    let t = compile_expr ctx env buf (indent + 1) then_ in
    emit buf (indent + 1) "%s = %s;" r t;
    emit buf indent "} else {";
    let e = compile_expr ctx env buf (indent + 1) else_ in
    emit buf (indent + 1) "%s = %s;" r e;
    emit buf indent "}";
    r
  | Pack _ -> "0"
  | Symbol { id = v; arg; _ } ->
    let key = mangled_key (id v) arg in
    (match Map.find env.vars key with
     | Some c -> c
     | None ->
       (match find_pack_prefix env (id v) with
        | Some prefix -> find_env_exn env.vars (mangled_key prefix arg)
        | None -> find_env_exn env.vars key))

(* Recursively expand Pack entries, handling nested Packs with compound keys.
   Non-Pack entries at top level are emitted as thunks (plain C functions). *)
and expand_pack ctx env scope prefix mono =
  Vec.fold mono ~init:env ~f:(fun env { Ir.Pack.Mono.arg; body; ty } ->
    let key = mangled_key prefix arg in
    match scope.is_top, (ty : Ir.Ty.t) with
    | true, (Unit | Bool | Int | Arrow _) ->
      let thunk_name = fresh ctx (Fn key) in
      let fn_buf = Buffer.create 256 in
      let result = compile_expr ctx env fn_buf 1 body in
      emit fn_buf 1 "return %s;" result;
      emit ctx.top 0 "static val_t %s(void) {" thunk_name;
      Buffer.add_buffer ctx.top fn_buf;
      emit ctx.top 0 "}";
      emit ctx.top 0 "";
      { env with vars = Map.set env.vars ~key ~data:(Printf.sprintf "%s()" thunk_name) }
    | _ -> expand_pack_entry ctx env scope key body)

(* Expand a single pack entry. Dispatches on pack-typed bindings, compiles others. *)
and expand_pack_entry ctx env scope prefix (body : Ir.Expr.t) =
  match body with
  | Pack { pack; _ } -> expand_pack ctx env scope prefix pack
  | Symbol { id = v; arg; ty = Pack; _ } ->
    alias_pack env (resolve_pack_source env (id v) arg) prefix
  | Var { id = v; ty = Pack; _ } ->
    let src = Option.value (find_pack_prefix env (id v)) ~default:(id v) in
    alias_pack env src prefix
  | Let { var; bind; rest; _ } ->
    let env =
      match classify_pack_bind bind with
      | Some (Inline_pack pack) ->
        let nested = local_pack_prefix prefix (id var) in
        expand_pack ctx env scope nested pack |> set_pack_prefix ~name:(id var) ~prefix:nested
      | Some (Pack_symbol { id = v; arg }) ->
        let src = resolve_pack_source env (id v) arg in
        let nested = local_pack_prefix prefix (id var) in
        alias_pack env src nested |> set_pack_prefix ~name:(id var) ~prefix:nested
      | Some (Pack_var v) ->
        let src = Option.value (find_pack_prefix env (id v)) ~default:(id v) in
        let nested = local_pack_prefix prefix (id var) in
        alias_pack env src nested |> set_pack_prefix ~name:(id var) ~prefix:nested
      | None ->
        let rhs = compile_expr ctx env scope.buf scope.indent bind in
        if Ident.is_anon var
        then (
          emit scope.buf scope.indent "(void)%s;" rhs;
          env)
        else declare_var ctx scope (id var) rhs env
    in
    expand_pack_entry ctx env scope prefix rest
  | _ ->
    let rhs = compile_expr ctx env scope.buf scope.indent body in
    declare_var ctx scope prefix rhs env

and compile_value ctx env buf indent (value : Ir.Value.t) : string =
  match value with
  | Unit -> "0"
  | Bool b -> if b then "1" else "0"
  | Int i -> Printf.sprintf "%LdLL" i
  | Closure { arg; body; _ } -> compile_closure ctx env buf indent arg body
  | Pack _ -> "0"
  | External { symbol; ty } -> compile_external_inline ctx buf indent symbol ty

(* Lift a lambda/closure to a top-level C function. Returns the closure val_t expression. *)
and compile_closure ctx env buf indent arg body =
  let arg_s = id arg in
  let fvs = Set.remove (free_vars body) arg_s |> Set.to_list in
  let fn_name = fresh ctx Lambda in
  let fn_buf = Buffer.create 256 in
  (* Unpack captured variables from env array *)
  let fn_env =
    List.foldi fvs ~init:empty_env ~f:(fun i acc v ->
      let c = fresh ctx (Var v) in
      emit fn_buf 1 "val_t %s = _env[%d];" c i;
      { acc with vars = Map.set acc.vars ~key:v ~data:c })
  in
  let c_arg = fresh ctx (Var (if Ident.is_anon arg then "a" else arg_s)) in
  let fn_env =
    if Ident.is_anon arg
    then fn_env
    else { fn_env with vars = Map.set fn_env.vars ~key:arg_s ~data:c_arg }
  in
  let result = compile_expr ctx fn_env fn_buf 1 body in
  emit fn_buf 1 "return %s;" result;
  emit ctx.top 0 "static val_t %s(val_t* _env, val_t %s) {" fn_name c_arg;
  Buffer.add_buffer ctx.top fn_buf;
  emit ctx.top 0 "}";
  emit ctx.top 0 "";
  (* Allocate closure at use site *)
  let n = List.length fvs in
  let clo = fresh ctx Closure in
  emit buf indent "closure_t* %s = (closure_t*)malloc(sizeof(closure_t));" clo;
  emit buf indent "%s->fn = %s;" clo fn_name;
  if n = 0
  then emit buf indent "%s->env = NULL;" clo
  else emit buf indent "%s->env = (val_t*)malloc(%d * sizeof(val_t));" clo n;
  List.iteri fvs ~f:(fun i v ->
    let c_v = Map.find_exn env.vars v in
    emit buf indent "%s->env[%d] = %s;" clo i c_v);
  Printf.sprintf "(val_t)%s" clo

(* Compile expression-level mutually recursive functions. Returns updated env. *)
and compile_local_funs ctx env buf indent funs : env =
  let flat = flatten_funs funs in
  let flat_fns =
    List.filter flat ~f:(function
      | Fn _ -> true
      | Alias _ | Expr _ -> false)
  in
  let fun_keys =
    List.map flat_fns ~f:(function
      | Fn { key; _ } -> key
      | Alias _ | Expr _ -> assert false)
  in
  let fun_key_set = String.Set.of_list fun_keys in
  (* Free vars across all function bodies, excluding the mutual names and each fn's arg *)
  let all_fvs =
    List.map flat_fns ~f:(function
      | Fn { arg; body; _ } -> Set.remove (free_vars body) (id arg)
      | Alias _ | Expr _ -> assert false)
    |> String.Set.union_list
    |> fun s -> Set.diff s fun_key_set |> Set.to_list
  in
  let n_ext = List.length all_fvs in
  let n_funs = List.length flat_fns in
  let n_env = n_ext + n_funs in
  let shared_env = fresh ctx Env in
  (* Generate lifted C function names *)
  let fn_c_names =
    List.map flat_fns ~f:(function
      | Fn { key; _ } -> fresh ctx (Fn key)
      | Alias _ | Expr _ -> assert false)
  in
  (* Forward declare *)
  List.iter fn_c_names ~f:(fun fn_name ->
    emit ctx.top 0 "static val_t %s(val_t* _env, val_t _arg);" fn_name);
  emit ctx.top 0 "";
  (* Define each function *)
  List.iter2_exn flat_fns fn_c_names ~f:(fun fn fn_name ->
    let arg, body =
      match fn with
      | Fn { arg; body; _ } -> arg, body
      | Alias _ | Expr _ -> assert false
    in
    let fn_buf = Buffer.create 256 in
    let fn_env =
      List.foldi all_fvs ~init:empty_env ~f:(fun i acc v ->
        let c = fresh ctx (Var v) in
        emit fn_buf 1 "val_t %s = _env[%d];" c i;
        { acc with vars = Map.set acc.vars ~key:v ~data:c })
    in
    let fn_env =
      List.foldi flat_fns ~init:fn_env ~f:(fun i acc fn ->
        let rkey =
          match fn with
          | Fn { key; _ } -> key
          | Alias _ | Expr _ -> assert false
        in
        let c = fresh ctx (Var rkey) in
        emit fn_buf 1 "val_t %s = _env[%d];" c (n_ext + i);
        { acc with vars = Map.set acc.vars ~key:rkey ~data:c })
    in
    let c_arg = fresh ctx (Var (if Ident.is_anon arg then "a" else id arg)) in
    let fn_env =
      if Ident.is_anon arg
      then fn_env
      else { fn_env with vars = Map.set fn_env.vars ~key:(id arg) ~data:c_arg }
    in
    let result = compile_expr ctx fn_env fn_buf 1 body in
    emit fn_buf 1 "return %s;" result;
    emit ctx.top 0 "static val_t %s(val_t* _env, val_t %s) {" fn_name c_arg;
    Buffer.add_buffer ctx.top fn_buf;
    emit ctx.top 0 "}";
    emit ctx.top 0 "");
  (* Allocate closures *)
  emit buf indent "val_t* %s = (val_t*)malloc(%d * sizeof(val_t));" shared_env n_env;
  let clo_c_names =
    List.map2_exn flat_fns fn_c_names ~f:(fun fn fn_name ->
      let key =
        match fn with
        | Fn { key; _ } -> key
        | Alias _ | Expr _ -> assert false
      in
      let clo = fresh ctx (Var key) in
      emit buf indent "closure_t* %s = (closure_t*)malloc(sizeof(closure_t));" clo;
      emit buf indent "%s->fn = %s;" clo fn_name;
      emit buf indent "%s->env = %s;" clo shared_env;
      clo)
  in
  (* Fill shared environment once: external captures then mutual closure refs *)
  List.iteri all_fvs ~f:(fun i v ->
    let c_v = Map.find_exn env.vars v in
    emit buf indent "%s[%d] = %s;" shared_env i c_v);
  List.iteri clo_c_names ~f:(fun i rec_clo ->
    emit buf indent "%s[%d] = (val_t)%s;" shared_env (n_ext + i) rec_clo);
  (* Add val_t variables for each function to env *)
  List.fold2_exn flat_fns clo_c_names ~init:env ~f:(fun acc fn clo ->
    let key =
      match fn with
      | Fn { key; _ } -> key
      | Alias _ | Expr _ -> assert false
    in
    let v = fresh ctx (Var key) in
    emit buf indent "val_t %s = (val_t)%s;" v clo;
    { acc with vars = Map.set acc.vars ~key ~data:v })
;;

(* Compile a top-level Fun (no captures, references globals directly).
   Fn entries become closures; Expr entries become thunks (plain C functions);
   Alias entries resolve via env lookup. *)
let compile_top_funs ctx env funs : env =
  let flat = flatten_funs funs in
  let flat_fns =
    List.filter flat ~f:(function
      | Fn _ -> true
      | Alias _ | Expr _ -> false)
  in
  let flat_exprs =
    List.filter_map flat ~f:(function
      | Expr { key; body } -> Some (key, body)
      | _ -> None)
  in
  let flat_aliases =
    List.filter_map flat ~f:(function
      | Alias { key; target } -> Some (key, target)
      | _ -> None)
  in
  (* Create global val_t variables for Fn entries *)
  let fn_clo_vars =
    List.map flat_fns ~f:(fun f ->
      let key = flat_key f in
      let v = fresh ctx (Var key) in
      emit ctx.top 0 "static val_t %s;" v;
      v)
  in
  (* Generate thunk function names for Expr entries *)
  let expr_thunk_names =
    List.map flat_exprs ~f:(fun (key, _) -> fresh ctx (Fn key))
  in
  (* Add Fn entries to env before compiling bodies (mutual recursion) *)
  let env' =
    List.fold2_exn
      (List.map flat_fns ~f:flat_key)
      fn_clo_vars
      ~init:env
      ~f:(fun acc name c_var ->
        { acc with vars = Map.set acc.vars ~key:name ~data:c_var })
  in
  (* Add Expr entries to env as thunk call expressions *)
  let env' =
    List.fold2_exn flat_exprs expr_thunk_names ~init:env' ~f:(fun acc (key, _) thunk_name ->
      { acc with vars = Map.set acc.vars ~key ~data:(Printf.sprintf "%s()" thunk_name) })
  in
  (* Add Alias entries to env by resolving targets *)
  let env' =
    List.fold flat_aliases ~init:env' ~f:(fun acc (key, target) ->
      let target_c = find_env_exn acc.vars target in
      { acc with vars = Map.set acc.vars ~key ~data:target_c })
  in
  (* Generate C function names and forward declare *)
  let fn_c_names =
    List.map flat_fns ~f:(function
      | Fn { key; _ } -> fresh ctx (Fn key)
      | Alias _ | Expr _ -> assert false)
  in
  List.iter fn_c_names ~f:(fun fn_name ->
    emit ctx.top 0 "static val_t %s(val_t* _env, val_t _arg);" fn_name);
  List.iter expr_thunk_names ~f:(fun thunk_name ->
    emit ctx.top 0 "static val_t %s(void);" thunk_name);
  emit ctx.top 0 "";
  (* Define each Fn function body *)
  List.iter2_exn flat_fns fn_c_names ~f:(fun fn fn_name ->
    let arg, body =
      match fn with
      | Fn { arg; body; _ } -> arg, body
      | Alias _ | Expr _ -> assert false
    in
    let fn_buf = Buffer.create 256 in
    let arg_s = id arg in
    let c_arg = fresh ctx (Var (if Ident.is_anon arg then "a" else arg_s)) in
    let fn_env =
      if Ident.is_anon arg
      then env'
      else { env' with vars = Map.set env'.vars ~key:arg_s ~data:c_arg }
    in
    let result = compile_expr ctx fn_env fn_buf 1 body in
    emit fn_buf 1 "return %s;" result;
    emit ctx.top 0 "static val_t %s(val_t* _env, val_t %s) {" fn_name c_arg;
    Buffer.add_buffer ctx.top fn_buf;
    emit ctx.top 0 "}";
    emit ctx.top 0 "");
  (* Define each Expr thunk *)
  List.iter2_exn flat_exprs expr_thunk_names ~f:(fun (_, body) thunk_name ->
    let fn_buf = Buffer.create 256 in
    let result = compile_expr ctx env' fn_buf 1 body in
    emit fn_buf 1 "return %s;" result;
    emit ctx.top 0 "static val_t %s(void) {" thunk_name;
    Buffer.add_buffer ctx.top fn_buf;
    emit ctx.top 0 "}";
    emit ctx.top 0 "");
  (* In main: allocate closures for Fn entries *)
  List.iter2_exn
    (List.map flat_fns ~f:(function
       | Fn { key; _ } -> Map.find_exn env'.vars key
       | Alias _ | Expr _ -> assert false))
    fn_c_names
    ~f:(fun clo_var fn_name ->
      emit ctx.main 1 "{";
      emit ctx.main 2 "closure_t* _c = (closure_t*)malloc(sizeof(closure_t));";
      emit ctx.main 2 "_c->fn = %s;" fn_name;
      emit ctx.main 2 "_c->env = NULL;";
      emit ctx.main 2 "%s = (val_t)_c;" clo_var;
      emit ctx.main 1 "}");
  env'
;;

let compile_external ctx env var symbol ty : env =
  let rhs = compile_external_inline ctx ctx.main 1 symbol ty in
  let c_var = fresh ctx (Var (id var)) in
  emit ctx.top 0 "static val_t %s;" c_var;
  emit ctx.main 1 "%s = %s;" c_var rhs;
  { env with vars = Map.set env.vars ~key:(id var) ~data:c_var }
;;

let compile_top_level ctx env (top : Ir.Top_level.t) : env =
  let top_scope = { buf = ctx.main; indent = 1; is_top = true } in
  match top with
  | Let { var; bind; _ } ->
    (match classify_pack_bind bind with
     | Some (Inline_pack pack) ->
       expand_pack ctx env top_scope (id var) pack
       |> set_pack_prefix ~name:(id var) ~prefix:(id var)
     | Some (Pack_symbol { id = v; arg }) ->
       alias_pack env (resolve_pack_source env (id v) arg) (id var)
     | Some (Pack_var v) ->
       let src = Option.value (find_pack_prefix env (id v)) ~default:(id v) in
       alias_pack env src (id var)
     | None ->
       let rhs = compile_expr ctx env ctx.main 1 bind in
       if Ident.is_anon var
       then (
         emit ctx.main 1 "(void)%s;" rhs;
         env)
       else (
         let c_var = fresh ctx (Var (id var)) in
         emit ctx.top 0 "static val_t %s;" c_var;
         emit ctx.main 1 "%s = %s;" c_var rhs;
         { env with vars = Map.set env.vars ~key:(id var) ~data:c_var }))
  | Fun { funs; _ } -> compile_top_funs ctx env funs
  | External { var; symbol; ty; _ } -> compile_external ctx env var symbol ty
;;

let codegen (program : Ir.Program.t) : string =
  let ctx =
    { top = Buffer.create 1024
    ; main = Buffer.create 1024
    ; counters = Hashtbl.create (module String)
    }
  in
  let _env = List.fold program ~init:empty_env ~f:(fun env top -> compile_top_level ctx env top) in
  let result = Buffer.create 4096 in
  Buffer.add_string result preamble;
  Buffer.add_buffer result ctx.top;
  Buffer.add_string result "int main(void) {\n";
  Buffer.add_buffer result ctx.main;
  Buffer.add_string result "    return 0;\n}\n";
  Buffer.contents result
;;
