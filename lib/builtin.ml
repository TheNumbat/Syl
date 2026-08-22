open! Core
open Tst
include Builtin0

module Error = struct
  type t =
    | Divide_by_zero of Int.t
    | Negative_modulus of Int.t
    | Assert_failed of Value.t
    | Expected_tuple of Value.t
    | Expected_arrow of Value.t
    | Expected_pi of Value.t
    | Out_of_bounds of
        { idx : int64
        ; len : int64
        }
  [@@deriving sexp_of]
end

exception Error of Error.t

module Fail = struct
  let divide_by_zero i = raise (Error (Divide_by_zero i))
  let negative_modulus i = raise (Error (Negative_modulus i))
  let assert_failed b = raise (Error (Assert_failed b))
  let expected_tuple t = raise (Error (Expected_tuple t))
  let expected_arrow t = raise (Error (Expected_arrow t))
  let expected_pi t = raise (Error (Expected_pi t))
  let out_of_bounds idx len = raise (Error (Out_of_bounds { idx; len }))
end

let pair (value : Value.t) : Value.t * Value.t =
  match value.node with
  | Tuple [ a; b ] -> a, b
  | _ -> raise_s [%message "Bug: expected pair"]
;;

let static_erased = Modes.create ~staticity:Static ~erasure:Erased
let static_unerased = Modes.create ~staticity:Static ~erasure:Unerased
let dynamic_unerased = Modes.create ~staticity:Dynamic ~erasure:Unerased

module Prim = struct
  include Prim

  module Int = struct
    include Int

    let binop : Value.t =
      Value.type_
        (Arrow
           { arg_ty = Value.type_ (Tuple [ Value.type_ Int; Value.type_ Int ])
           ; ret_ty = Value.type_ Int
           ; arg_mode = dynamic_unerased
           ; ret_mode = static_unerased
           })
    ;;

    let cmp : Value.t =
      Value.type_
        (Arrow
           { arg_ty = Value.type_ (Tuple [ Value.type_ Int; Value.type_ Int ])
           ; ret_ty = Value.type_ Bool
           ; arg_mode = dynamic_unerased
           ; ret_mode = static_unerased
           })
    ;;

    let unop : Value.t =
      Value.type_
        (Arrow
           { arg_ty = Value.type_ Int
           ; ret_ty = Value.type_ Int
           ; arg_mode = dynamic_unerased
           ; ret_mode = static_unerased
           })
    ;;

    let ty : t -> Value.t = function
      | Add | Sub | Mul | Div | Mod -> binop
      | Eq | Neq | Lt | Lte | Gt | Gte -> cmp
      | Neg -> unop
    ;;

    let eval (prim : t) (value : Value.t) : Value.t =
      try
        match prim with
        | Add ->
          let a, b = pair value in
          Tst.Int.add a b
        | Sub ->
          let a, b = pair value in
          Tst.Int.sub a b
        | Mul ->
          let a, b = pair value in
          Tst.Int.mul a b
        | Div ->
          let a, b = pair value in
          Tst.Int.div a b
        | Mod ->
          let a, b = pair value in
          Tst.Int.mod_ a b
        | Neg -> Tst.Int.neg value
        | Eq ->
          let a, b = pair value in
          Tst.Bool.eq a b
        | Neq ->
          let a, b = pair value in
          Tst.Bool.neq a b
        | Lt ->
          let a, b = pair value in
          Tst.Bool.lt a b
        | Lte ->
          let a, b = pair value in
          Tst.Bool.lte a b
        | Gt ->
          let a, b = pair value in
          Tst.Bool.gt a b
        | Gte ->
          let a, b = pair value in
          Tst.Bool.gte a b
      with
      | Tst.Int.Divide_by_zero i -> Fail.divide_by_zero i
      | Tst.Int.Negative_modulus i -> Fail.negative_modulus i
    ;;
  end

  module Bool = struct
    include Bool

    let binop : Value.t =
      Value.type_
        (Arrow
           { arg_ty = Value.type_ (Tuple [ Value.type_ Bool; Value.type_ Bool ])
           ; ret_ty = Value.type_ Bool
           ; arg_mode = dynamic_unerased
           ; ret_mode = static_unerased
           })
    ;;

    let unop : Value.t =
      Value.type_
        (Arrow
           { arg_ty = Value.type_ Bool
           ; ret_ty = Value.type_ Bool
           ; arg_mode = dynamic_unerased
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
        Tst.Bool.and_ a b
      | Or ->
        let a, b = pair value in
        Tst.Bool.or_ a b
      | Not -> Tst.Bool.not_ value
    ;;
  end

  module Type = struct
    include Type

    let ty : t -> Value.t = function
      | Is_unit | Is_bool | Is_int | Is_type | Is_tuple | Is_arrow | Is_pi ->
        Value.type_
          (Pi
             { arg_ty = Value.type_ Type
             ; ret_ty = Dependent.mono (Value.type_ Bool)
             ; arg_mode = static_erased
             ; ret_mode = static_erased
             })
      | Arrow_arg | Arrow_ret | Pi_arg ->
        Value.type_
          (Pi
             { arg_ty = Value.type_ Type
             ; ret_ty = Dependent.mono (Value.type_ Type)
             ; arg_mode = static_erased
             ; ret_mode = static_erased
             })
      | Tuple_get ->
        Value.type_
          (Pi
             { arg_ty = Value.type_ (Tuple [ Value.type_ Type; Value.type_ Int ])
             ; ret_ty = Dependent.mono (Value.type_ Type)
             ; arg_mode = static_erased
             ; ret_mode = static_erased
             })
      | Tuple_length ->
        Value.type_
          (Pi
             { arg_ty = Value.type_ Type
             ; ret_ty = Dependent.mono (Value.type_ Int)
             ; arg_mode = static_erased
             ; ret_mode = static_erased
             })
    ;;

    let eval (prim : t) (value : Value.t) : Value.t =
      match prim with
      | Is_unit ->
        (match value.node with
         | Type Unit -> Tst.Bool.const true
         | Type _ -> Tst.Bool.const false
         | _ -> Value.apply ~fn:(Value.prim (Type Is_unit)) ~arg:value)
      | Is_bool ->
        (match value.node with
         | Type Bool -> Tst.Bool.const true
         | Type _ -> Tst.Bool.const false
         | _ -> Value.apply ~fn:(Value.prim (Type Is_bool)) ~arg:value)
      | Is_int ->
        (match value.node with
         | Type Int -> Tst.Bool.const true
         | Type _ -> Tst.Bool.const false
         | _ -> Value.apply ~fn:(Value.prim (Type Is_int)) ~arg:value)
      | Is_type ->
        (match value.node with
         | Type Type -> Tst.Bool.const true
         | Type _ -> Tst.Bool.const false
         | _ -> Value.apply ~fn:(Value.prim (Type Is_type)) ~arg:value)
      | Is_tuple ->
        (match value.node with
         | Type (Tuple _) -> Tst.Bool.const true
         | Type _ -> Tst.Bool.const false
         | _ -> Value.apply ~fn:(Value.prim (Type Is_tuple)) ~arg:value)
      | Is_arrow ->
        (match value.node with
         | Type (Arrow _) -> Tst.Bool.const true
         | Type _ -> Tst.Bool.const false
         | _ -> Value.apply ~fn:(Value.prim (Type Is_arrow)) ~arg:value)
      | Is_pi ->
        (match value.node with
         | Type (Pi _) -> Tst.Bool.const true
         | Type _ -> Tst.Bool.const false
         | _ -> Value.apply ~fn:(Value.prim (Type Is_pi)) ~arg:value)
      | Arrow_arg ->
        (match value.node with
         | Type (Arrow { arg_ty; _ }) -> arg_ty
         | Type _ -> Fail.expected_arrow value
         | _ -> Value.apply ~fn:(Value.prim (Type Arrow_arg)) ~arg:value)
      | Arrow_ret ->
        (match value.node with
         | Type (Arrow { ret_ty; _ }) -> ret_ty
         | Type _ -> Fail.expected_arrow value
         | _ -> Value.apply ~fn:(Value.prim (Type Arrow_ret)) ~arg:value)
      | Pi_arg ->
        (match value.node with
         | Type (Pi { arg_ty; _ }) -> arg_ty
         | Type _ -> Fail.expected_pi value
         | _ -> Value.apply ~fn:(Value.prim (Type Pi_arg)) ~arg:value)
      | Tuple_get ->
        let tuple, idx = pair value in
        (match tuple.node, idx.node with
         | Type (Tuple elts), Int (T idx) ->
           let n = Int64.of_int (Nonempty_list.length elts) in
           if Int64.(idx >= 0L && idx < n)
           then Nonempty_list.nth_exn elts (Int64.to_int_exn idx)
           else Fail.out_of_bounds idx n
         | Type (Tuple _), _ -> Value.apply ~fn:(Value.prim (Type Tuple_get)) ~arg:value
         | _ -> Fail.expected_tuple tuple)
      | Tuple_length ->
        (match value.node with
         | Type (Tuple elts) -> Tst.Int.const (Int64.of_int (Nonempty_list.length elts))
         | Type _ -> Fail.expected_tuple value
         | _ -> Value.apply ~fn:(Value.prim (Type Tuple_length)) ~arg:value)
    ;;
  end

  module Unerase = struct
    include Unerase

    let scalar_ty : t -> Value.t = function
      | Unit -> Value.type_ Unit
      | Bool -> Value.type_ Bool
      | Int -> Value.type_ Int
    ;;

    let ty t : Value.t =
      let ty = scalar_ty t in
      Value.type_
        (Pi
           { arg_ty = ty
           ; ret_ty = Dependent.mono ty
           ; arg_mode = static_erased
           ; ret_mode = static_unerased
           })
    ;;

    let eval (_ : t) (value : Value.t) : Value.t = value
  end

  let desc (prim : Prim.t) : Desc.t =
    match prim with
    | Assert ->
      let ty =
        Value.type_
          (Arrow
             { arg_ty = Value.type_ Bool
             ; ret_ty = Value.type_ Unit
             ; arg_mode = dynamic_unerased
             ; ret_mode = static_unerased
             })
      in
      { ty; mode = static_unerased; static = Lazy.from_val (Value.prim prim) }
    | Assert_erased ->
      let ty =
        Value.type_
          (Pi
             { arg_ty = Value.type_ Bool
             ; ret_ty = Dependent.mono (Value.type_ Unit)
             ; arg_mode = static_erased
             ; ret_mode = static_erased
             })
      in
      { ty; mode = static_erased; static = Lazy.from_val (Value.prim prim) }
    | Type t -> { ty = Type.ty t; mode = static_erased; static = Lazy.from_val (Value.prim prim) }
    | Int i -> { ty = Int.ty i; mode = static_unerased; static = Lazy.from_val (Value.prim prim) }
    | Bool b -> { ty = Bool.ty b; mode = static_unerased; static = Lazy.from_val (Value.prim prim) }
    | Unerase u ->
      { ty = Unerase.ty u; mode = static_erased; static = Lazy.from_val (Value.prim prim) }
  ;;
end

let desc : t -> Desc.t = function
  | Type Unit ->
    { ty = Value.type_ Type; mode = static_erased; static = Lazy.from_val (Value.type_ Unit) }
  | Type Bool ->
    { ty = Value.type_ Type; mode = static_erased; static = Lazy.from_val (Value.type_ Bool) }
  | Type Int ->
    { ty = Value.type_ Type; mode = static_erased; static = Lazy.from_val (Value.type_ Int) }
  | Type Type ->
    { ty = Value.type_ Type; mode = static_erased; static = Lazy.from_val (Value.type_ Type) }
  | Prim prim -> Prim.desc prim
;;

let eval_assert (value : Value.t) : Value.t =
  match value.node with
  (* An unreachable assertion never reports. *)
  | Bottom -> Value.bottom
  | Bool (T true) -> Value.unit
  | _ -> Fail.assert_failed value
;;

let eval (prim : Prim.t) (value : Value.t) : Value.t =
  match value.node with
  (* A primitive applied to an unreachable argument is unreachable. *)
  | Bottom -> Value.bottom
  | _ ->
    (match prim with
     | Assert | Assert_erased -> eval_assert value
     | Int i -> Prim.Int.eval i value
     | Bool b -> Prim.Bool.eval b value
     | Type t -> Prim.Type.eval t value
     | Unerase u -> Prim.Unerase.eval u value)
;;

let apply ~loc (prim : Prim.t) (arg : Tst.Expr.t) (_arg_desc (* TODO *) : Desc.t) : Tst.Expr.t =
  let prim_desc = desc (Prim prim) in
  let ty = Tst.Ty.ret prim_desc.ty in
  let mode = Tst.Ty.ret_mode prim_desc.ty in
  let fn : Tst.Expr.t =
    Builtin { builtin = Prim prim; ty = prim_desc.ty; mode = prim_desc.mode; loc }
  in
  Tst.Expr.Apply { fn; arg; ty; mode; loc }
;;
