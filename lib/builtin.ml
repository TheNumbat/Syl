open! Core
open Tst
include Builtin0

exception Divide_by_zero of Int.t
exception Assert_failed of Value.t

let pair (value : Value.t) : Value.t * Value.t =
  match value with
  | Tuple [ a; b ] -> a, b
  | _ -> assert false
;;

let static_erased = Modes.create ~staticity:Static ~erasure:Erased
let static_unerased = Modes.create ~staticity:Static ~erasure:Unerased
let dynamic_erased = Modes.create ~staticity:Dynamic ~erasure:Erased

module Prim = struct
  include Prim

  module Int = struct
    include Int

    let binop : Value.t =
      Type
        (Arrow
           { arg_ty = Type (Tuple [ Type Int; Type Int ])
           ; ret_ty = Type Int
           ; arg_mode = dynamic_erased
           ; ret_mode = static_unerased
           })
    ;;

    let cmp : Value.t =
      Type
        (Arrow
           { arg_ty = Type (Tuple [ Type Int; Type Int ])
           ; ret_ty = Type Bool
           ; arg_mode = dynamic_erased
           ; ret_mode = static_unerased
           })
    ;;

    let unop : Value.t =
      Type
        (Arrow
           { arg_ty = Type Int
           ; ret_ty = Type Int
           ; arg_mode = dynamic_erased
           ; ret_mode = static_unerased
           })
    ;;

    let ty : t -> Value.t = function
      | Add | Sub | Mul | Div | Mod -> binop
      | Eq | Neq | Lt | Lte | Gt | Gte -> cmp
      | Neg -> unop
    ;;

    let eval (prim : t) (value : Value.t) : Value.t =
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
  end

  module Bool = struct
    include Bool

    let binop : Value.t =
      Type
        (Arrow
           { arg_ty = Type (Tuple [ Type Bool; Type Bool ])
           ; ret_ty = Type Bool
           ; arg_mode = dynamic_erased
           ; ret_mode = static_unerased
           })
    ;;

    let unop : Value.t =
      Type
        (Arrow
           { arg_ty = Type Bool
           ; ret_ty = Type Bool
           ; arg_mode = dynamic_erased
           ; ret_mode = static_unerased
           })
    ;;

    let ty : t -> Value.t = function
      | And | Or -> binop
      | Not -> unop
    ;;

    let eval (prim : t) (value : Value.t) : Value.t =
      match prim with
      | And ->
        let a, b = pair value in
        Value.reduce (Bool (And (a, b)))
      | Or ->
        let a, b = pair value in
        Value.reduce (Bool (Or (a, b)))
      | Not -> Value.reduce (Bool (Not value))
    ;;
  end

  let desc (prim : Prim.t) : Desc.t =
    let ty : Value.t =
      match prim with
      | Assert ->
        Type
          (Arrow
             { arg_ty = Type Bool
             ; ret_ty = Type Unit
             ; arg_mode = dynamic_erased
             ; ret_mode = static_unerased
             })
      | Int i -> Int.ty i
      | Bool b -> Bool.ty b
    in
    { ty; mode = static_unerased; static = Lazy.from_val (Value.Prim prim) }
  ;;
end

let desc : t -> Desc.t = function
  | Type Unit -> { ty = Type Type; mode = static_erased; static = Lazy.from_val (Value.Type Unit) }
  | Type Bool -> { ty = Type Type; mode = static_erased; static = Lazy.from_val (Value.Type Bool) }
  | Type Int -> { ty = Type Type; mode = static_erased; static = Lazy.from_val (Value.Type Int) }
  | Type Type -> { ty = Type Type; mode = static_erased; static = Lazy.from_val (Value.Type Type) }
  | Prim prim -> Prim.desc prim
;;

let eval_assert (value : Value.t) : Value.t =
  match value with
  | Bool (T true) -> Unit
  | _ -> raise (Assert_failed value)
;;

let eval (prim : Prim.t) (value : Value.t) : Value.t =
  match prim with
  | Assert -> eval_assert value
  | Int i -> Prim.Int.eval i value
  | Bool b -> Prim.Bool.eval b value
;;
