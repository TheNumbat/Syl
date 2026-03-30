open! Core
open Tst
include Builtin0

let static_erased = Modes.create ~staticity:Static ~erasure:Erased
let static_unerased = Modes.create ~staticity:Static ~erasure:Unerased
let dynamic_erased = Modes.create ~staticity:Dynamic ~erasure:Erased

let int_binop_ty : Value.t =
  Type
    (Arrow
       { arg_ty = Type (Tuple [ Type Int; Type Int ])
       ; ret_ty = Type Int
       ; arg_mode = dynamic_erased
       ; ret_mode = static_unerased
       })
;;

let int_cmp_ty : Value.t =
  Type
    (Arrow
       { arg_ty = Type (Tuple [ Type Int; Type Int ])
       ; ret_ty = Type Bool
       ; arg_mode = dynamic_erased
       ; ret_mode = static_unerased
       })
;;

let int_unop_ty : Value.t =
  Type
    (Arrow
       { arg_ty = Type Int
       ; ret_ty = Type Int
       ; arg_mode = dynamic_erased
       ; ret_mode = static_unerased
       })
;;

let bool_binop_ty : Value.t =
  Type
    (Arrow
       { arg_ty = Type (Tuple [ Type Bool; Type Bool ])
       ; ret_ty = Type Bool
       ; arg_mode = dynamic_erased
       ; ret_mode = static_unerased
       })
;;

let bool_unop_ty : Value.t =
  Type
    (Arrow
       { arg_ty = Type Bool
       ; ret_ty = Type Bool
       ; arg_mode = dynamic_erased
       ; ret_mode = static_unerased
       })
;;

let prim_desc (prim : Prim.t) : Desc.t =
  let ty =
    match prim with
    | Int (Add | Sub | Mul | Div | Mod) -> int_binop_ty
    | Int Neg -> int_unop_ty
    | Int (Eq | Neq | Lt | Lte | Gt | Gte) -> int_cmp_ty
    | Bool (And | Or) -> bool_binop_ty
    | Bool Not -> bool_unop_ty
  in
  { ty; mode = static_unerased; static = Lazy.from_val (Value.Prim prim) }
;;

let desc : t -> Desc.t = function
  | Type Unit -> { ty = Type Type; mode = static_erased; static = Lazy.from_val (Value.Type Unit) }
  | Type Bool -> { ty = Type Type; mode = static_erased; static = Lazy.from_val (Value.Type Bool) }
  | Type Int -> { ty = Type Type; mode = static_erased; static = Lazy.from_val (Value.Type Int) }
  | Type Type -> { ty = Type Type; mode = static_erased; static = Lazy.from_val (Value.Type Type) }
  | Prim prim -> prim_desc prim
;;

exception Divide_by_zero of Int.t

let pair (value : Value.t) : Value.t * Value.t =
  match value with
  | Tuple [ a; b ] -> a, b
  | _ -> assert false
;;

let eval_int (prim : Prim.Int.t) (value : Value.t) : Value.t =
  match prim with
  | Add ->
    let a, b = pair value in
    Value.reduce (Int (Add (a, b)))
  | Sub ->
    let a, b = pair value in
    Value.reduce (Int (Sub (a, b)))
  | Mul ->
    let a, b = pair value in
    Value.reduce (Int (Mul (a, b)))
  | Div ->
    let a, b = pair value in
    (match b with
     | Int (T 0L) -> raise (Divide_by_zero (Div (a, b)))
     | _ -> Value.reduce (Int (Div (a, b))))
  | Mod ->
    let a, b = pair value in
    (match b with
     | Int (T 0L) -> raise (Divide_by_zero (Mod (a, b)))
     | _ -> Value.reduce (Int (Mod (a, b))))
  | Neg -> Value.reduce (Int (Neg value))
  | Eq ->
    let a, b = pair value in
    Value.reduce (Bool (Eq (a, b)))
  | Neq ->
    let a, b = pair value in
    Value.reduce (Bool (Neq (a, b)))
  | Lt ->
    let a, b = pair value in
    Value.reduce (Bool (Lt (a, b)))
  | Lte ->
    let a, b = pair value in
    Value.reduce (Bool (Lte (a, b)))
  | Gt ->
    let a, b = pair value in
    Value.reduce (Bool (Gt (a, b)))
  | Gte ->
    let a, b = pair value in
    Value.reduce (Bool (Gte (a, b)))
;;

let eval_bool (prim : Prim.Bool.t) (value : Value.t) : Value.t =
  match prim with
  | And ->
    let a, b = pair value in
    Value.reduce (Bool (And (a, b)))
  | Or ->
    let a, b = pair value in
    Value.reduce (Bool (Or (a, b)))
  | Not -> Value.reduce (Bool (Not value))
;;

let eval (prim : Prim.t) (value : Value.t) : Value.t =
  match prim with
  | Int prim -> eval_int prim value
  | Bool prim -> eval_bool prim value
;;
