open! Core
open Tst
include Builtin0

module Error = struct
  type t =
    | Divide_by_zero of Int.t
    | Assert_failed of Value.t
    | Expected_tuple of Value.t
    | Expected_arrow of Value.t
    | Expected_pi of Value.t
    | Out_of_bounds of
        { idx : int64
        ; len : int64
        }
  [@@deriving sexp]
end

exception Error of Error.t

module Fail = struct
  let divide_by_zero i = raise (Error (Divide_by_zero i))
  let assert_failed b = raise (Error (Assert_failed b))
  let expected_tuple t = raise (Error (Expected_tuple t))
  let expected_arrow t = raise (Error (Expected_arrow t))
  let expected_pi t = raise (Error (Expected_pi t))
  let out_of_bounds idx len = raise (Error (Out_of_bounds { idx; len }))
end

let pair (value : Value.t) : Value.t * Value.t =
  match value with
  | Tuple [ a; b ] -> a, b
  | _ -> raise_s [%message "Bug: expected pair"]
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
         | Int (T 0L) -> Fail.divide_by_zero (Div (a, b))
         | _ -> Value.reduce (Int (Div (a, b))))
      | Mod ->
        let a, b = pair value in
        (match b with
         | Int (T 0L) -> Fail.divide_by_zero (Mod (a, b))
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

  module Type = struct
    include Type

    let ty : t -> Value.t = function
      | Is_unit | Is_bool | Is_int | Is_type | Is_tuple | Is_arrow | Is_pi ->
        Type
          (Pi
             { arg_ty = Type Type
             ; ret_ty = T (Type Bool)
             ; arg_mode = static_erased
             ; ret_mode = static_unerased
             })
      | Arrow_arg | Arrow_ret | Pi_arg ->
        Type
          (Pi
             { arg_ty = Type Type
             ; ret_ty = T (Type Type)
             ; arg_mode = static_erased
             ; ret_mode = static_erased
             })
      | Tuple_get ->
        Type
          (Pi
             { arg_ty = Type (Tuple [ Type Type; Type Int ])
             ; ret_ty = T (Type Type)
             ; arg_mode = static_erased
             ; ret_mode = static_erased
             })
      | Tuple_length ->
        Type
          (Pi
             { arg_ty = Type Type
             ; ret_ty = T (Type Int)
             ; arg_mode = static_erased
             ; ret_mode = static_unerased
             })
    ;;

    let eval (prim : t) (value : Value.t) : Value.t =
      match prim with
      | Is_unit ->
        (match value with
         | Type Unit -> Bool (T true)
         | _ -> Bool (T false))
      | Is_bool ->
        (match value with
         | Type Bool -> Bool (T true)
         | _ -> Bool (T false))
      | Is_int ->
        (match value with
         | Type Int -> Bool (T true)
         | _ -> Bool (T false))
      | Is_type ->
        (match value with
         | Type Type -> Bool (T true)
         | _ -> Bool (T false))
      | Is_tuple ->
        (match value with
         | Type (Tuple _) -> Bool (T true)
         | _ -> Bool (T false))
      | Is_arrow ->
        (match value with
         | Type (Arrow _) -> Bool (T true)
         | _ -> Bool (T false))
      | Is_pi ->
        (match value with
         | Type (Pi _) -> Bool (T true)
         | _ -> Bool (T false))
      | Arrow_arg ->
        (match value with
         | Type (Arrow { arg_ty; _ }) -> arg_ty
         | _ -> Fail.expected_arrow value)
      | Arrow_ret ->
        (match value with
         | Type (Arrow { ret_ty; _ }) -> ret_ty
         | _ -> Fail.expected_arrow value)
      | Pi_arg ->
        (match value with
         | Type (Pi { arg_ty; _ }) -> arg_ty
         | _ -> Fail.expected_pi value)
      | Tuple_get ->
        let tuple, idx = pair value in
        (match tuple, idx with
         | Type (Tuple elts), Int (T idx) ->
           let n = Int64.of_int (List.length elts) in
           if Int64.(idx >= 0L && idx < n)
           then List.nth_exn elts (Int64.to_int_exn idx)
           else Fail.out_of_bounds idx n
         | Type (Tuple _), _ -> Value.reduce (Apply { fn = Prim (Type Tuple_get); arg = value })
         | _ -> Fail.expected_tuple tuple)
      | Tuple_length ->
        (match value with
         | Type (Tuple elts) -> Int (T (Int64.of_int (List.length elts)))
         | _ -> Fail.expected_tuple value)
    ;;
  end

  let desc (prim : Prim.t) : Desc.t =
    match prim with
    | Assert ->
      let ty =
        Value.Type
          (Arrow
             { arg_ty = Type Bool
             ; ret_ty = Type Unit
             ; arg_mode = dynamic_erased
             ; ret_mode = static_unerased
             })
      in
      { ty; mode = static_unerased; static = Lazy.from_val (Value.Prim prim) }
    | Assert_static ->
      let ty =
        Value.Type
          (Pi
             { arg_ty = Type Bool
             ; ret_ty = T (Type Unit)
             ; arg_mode = static_erased
             ; ret_mode = static_unerased
             })
      in
      { ty; mode = static_erased; static = Lazy.from_val (Value.Prim prim) }
    | Type t -> { ty = Type.ty t; mode = static_erased; static = Lazy.from_val (Value.Prim prim) }
    | Int i -> { ty = Int.ty i; mode = static_unerased; static = Lazy.from_val (Value.Prim prim) }
    | Bool b -> { ty = Bool.ty b; mode = static_unerased; static = Lazy.from_val (Value.Prim prim) }
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
  | _ -> Fail.assert_failed value
;;

let eval (prim : Prim.t) (value : Value.t) : Value.t =
  match prim with
  | Assert | Assert_static -> eval_assert value
  | Int i -> Prim.Int.eval i value
  | Bool b -> Prim.Bool.eval b value
  | Type t -> Prim.Type.eval t value
;;
