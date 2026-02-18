open! Core
open Modes
open Option.Let_syntax

type ty =
  | Unit
  | Bool
  | Int
  | Type
  | Arrow of
      { arg_ty : value
      ; arg_mode : Modes.t
      ; ret_ty : value
      ; ret_mode : Modes.t
      }
  | Pi of
      { arg_ty : value
      ; arg_mode : Modes.t
      ; ret_ty : dependent
      ; ret_mode : Modes.t
      }
[@@deriving sexp]

and dependent =
  | T of value
  | Meet of dependent * dependent
  | Join of dependent * dependent
  | Reduce of
      { env : (env[@sexp.opaque])
      ; arg : Ident.t
      ; arg_ty : value
      ; arg_mode : Modes.t
      ; memo : ((concrete, value) Hashtbl.t[@sexp.opaque])
      ; ret_ty : Cst.Expr.t
      }
  | Typecheck of
      { env : (env[@sexp.opaque])
      ; arg : Ident.t
      ; arg_ty : value
      ; arg_mode : Modes.t
      ; memo : ((concrete, value) Hashtbl.t[@sexp.opaque])
      ; body : Cst.Expr.t
      }

and vbool =
  | T of bool
  | And of value * value
  | Or of value * value
  | Eq of value * value
  | Neq of value * value
  | Lt of value * value
  | Lte of value * value
  | Gt of value * value
  | Gte of value * value
  | Not of value
[@@deriving sexp]

and vint =
  | T of int64
  | Add of value * value
  | Sub of value * value
  | Mul of value * value
  | Div of value * value
  | Mod of value * value
  | Neg of value
[@@deriving sexp]

and value =
  | Unit
  | Bool of vbool
  | Int of vint
  | Type of ty
  | Closure of closure
  | Binder of binder
  | Var of Ident.t
  | If of
      { cond : value
      ; then_ : value
      ; else_ : value
      }
  | Apply of
      { fn : value
      ; arg : value
      }
  | External of
      { symbol : string
      ; ty : value
      }
[@@deriving sexp]

and concrete =
  | Unit
  | Bool of bool
  | Int of int64
  | Closure of int
  | UnitT
  | BoolT
  | IntT
  | TypeT
  | ArrowT of
      { arg : concrete
      ; arg_mode : Modes.t
      ; ret : concrete
      ; ret_mode : Modes.t
      }
[@@deriving sexp]

and closure =
  { arg : Ident.t
  ; ty : value
  ; body : expr
  ; env : (env[@sexp.opaque])
  }
[@@deriving sexp]

and binder =
  { arg : Ident.t
  ; ty : value
  ; body : Cst.Expr.t
  ; mono : ((concrete, mono) Hashtbl.t[@sexp.opaque])
  ; env : (env[@sexp.opaque])
  }
[@@deriving sexp]

and mono =
  { arg : Ident.t
  ; arg_val : value
  ; arg_ty : value
  ; arg_mode : Modes.t
  ; body : expr
  ; body_desc : desc
  }
[@@deriving sexp]

and desc =
  { id : Ident.t option
  ; ty : value
  ; mode : Modes.t
  ; static : (value Lazy.t[@sexp.opaque])
  }
[@@deriving sexp]

and env = desc Ident.Map.t [@@deriving sexp]

and fun_ =
  | Lambda of
      { var : Ident.t
      ; arg : Ident.t
      ; body : expr
      ; ty : value
      ; mode : Modes.t
      ; loc : (Lex.Location.t[@sexp.opaque])
      }
  | Binder of
      { var : Ident.t
      ; arg : Ident.t
      ; body : Cst.Expr.t
      ; mono : ((concrete, mono) Hashtbl.t[@sexp.opaque])
      ; ty : value
      ; mode : Modes.t
      ; loc : (Lex.Location.t[@sexp.opaque])
      }
[@@deriving sexp]

and expr =
  | Literal of
      { value : value
      ; ty : value
      ; mode : Modes.t
      ; loc : (Lex.Location.t[@sexp.opaque])
      }
  | Fun of
      { funs : fun_ Nonempty_list.t
      ; rest : expr
      ; loc : (Lex.Location.t[@sexp.opaque])
      }
  | Lambda of
      { arg : Ident.t
      ; ty : value
      ; body : expr
      ; mode : Modes.t
      ; loc : (Lex.Location.t[@sexp.opaque])
      }
  | Binder of
      { arg : Ident.t
      ; ty : value
      ; body : Cst.Expr.t
      ; mono : ((concrete, mono) Hashtbl.t[@sexp.opaque])
      ; mode : Modes.t
      ; loc : (Lex.Location.t[@sexp.opaque])
      }
  | Apply of
      { fn : expr
      ; arg : expr
      ; ty : value
      ; mode : Modes.t
      ; loc : (Lex.Location.t[@sexp.opaque])
      }
  | Let of
      { var : Ident.t
      ; bind : expr
      ; rest : expr
      ; ty : value
      ; mode : Modes.t
      ; loc : (Lex.Location.t[@sexp.opaque])
      }
  | Unop of
      { op : Cst.Unop.t
      ; arg : expr
      ; ty : value
      ; mode : Modes.t
      ; loc : (Lex.Location.t[@sexp.opaque])
      }
  | Binop of
      { op : Cst.Binop.t
      ; lhs : expr
      ; rhs : expr
      ; ty : value
      ; mode : Modes.t
      ; loc : (Lex.Location.t[@sexp.opaque])
      }
  | If of
      { cond : expr
      ; then_ : expr
      ; else_ : expr
      ; ty : value
      ; mode : Modes.t
      ; loc : (Lex.Location.t[@sexp.opaque])
      }
  | Var of
      { id : Ident.t
      ; ty : value
      ; mode : Modes.t
      ; loc : (Lex.Location.t[@sexp.opaque])
      }
  | Symbol of
      { id : Ident.t
      ; arg : concrete
      ; mode : Modes.t
      ; ty : value
      ; loc : Lex.Location.t
      }
  | Erased of
      { ty : value
      ; mode : Modes.t
      ; loc : (Lex.Location.t[@sexp.opaque])
      }
[@@deriving sexp]

let rec is_concrete_value : value -> _ = function
  | Unit | Closure _ | Binder _ | External _ -> true
  | Bool b -> is_concrete_bool b
  | Int i -> is_concrete_int i
  | Type ty -> is_concrete_ty ty
  | Var _ | If _ | Apply _ -> false

and is_concrete_bool : vbool -> _ = function
  | T _ -> true
  | _ -> false

and is_concrete_int : vint -> _ = function
  | T _ -> true
  | _ -> false

and is_concrete_ty : ty -> _ = function
  | Unit | Bool | Int | Type -> true
  | Arrow { arg_ty; ret_ty; _ } -> is_concrete_value arg_ty && is_concrete_value ret_ty
  | Pi { arg_ty; ret_ty; _ } -> is_concrete_value arg_ty && is_concrete_dependent ret_ty

and is_concrete_dependent : dependent -> _ = function
  | T value -> is_concrete_value value
  | Meet _ | Join _ | Reduce _ | Typecheck _ -> false
;;

let rec join_concrete_ty (a : ty) (b : ty) : ty option =
  match a, b with
  | Unit, Unit -> Some Unit
  | Bool, Bool -> Some Bool
  | Int, Int -> Some Int
  | Type, Type -> Some Type
  | ( Arrow { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Arrow { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } )
    ->
    let arg_mode = Modes.meet a_arg_mode b_arg_mode in
    let ret_mode = Modes.join a_ret_mode b_ret_mode in
    let%map arg_ty = meet_concrete_value a_arg_ty b_arg_ty
    and ret_ty = join_concrete_value a_ret_ty b_ret_ty in
    Arrow { arg_ty; arg_mode; ret_ty; ret_mode }
  | ( Arrow { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Pi { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } ) ->
    let arg_mode = Modes.meet a_arg_mode b_arg_mode in
    let ret_mode = Modes.join a_ret_mode b_ret_mode in
    let%map arg_ty = meet_concrete_value a_arg_ty b_arg_ty
    and ret_ty = join_concrete_dependent (T a_ret_ty) b_ret_ty in
    Pi { arg_ty; arg_mode; ret_ty = T ret_ty; ret_mode }
  | ( Pi { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Arrow { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } )
    ->
    let arg_mode = Modes.meet a_arg_mode b_arg_mode in
    let ret_mode = Modes.join a_ret_mode b_ret_mode in
    let%map arg_ty = meet_concrete_value a_arg_ty b_arg_ty
    and ret_ty = join_concrete_dependent a_ret_ty (T b_ret_ty) in
    Pi { arg_ty; arg_mode; ret_ty = T ret_ty; ret_mode }
  | ( Pi { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Pi { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } ) ->
    let arg_mode = Modes.meet a_arg_mode b_arg_mode in
    let ret_mode = Modes.join a_ret_mode b_ret_mode in
    let%map arg_ty = meet_concrete_value a_arg_ty b_arg_ty
    and ret_ty = join_concrete_dependent a_ret_ty b_ret_ty in
    Pi { arg_ty; arg_mode; ret_ty = T ret_ty; ret_mode }
  | _ -> None

and meet_concrete_ty (a : ty) (b : ty) : ty option =
  match a, b with
  | Unit, Unit -> Some Unit
  | Bool, Bool -> Some Bool
  | Int, Int -> Some Int
  | Type, Type -> Some Type
  | ( Arrow { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Arrow { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } )
    ->
    let arg_mode = Modes.join a_arg_mode b_arg_mode in
    let ret_mode = Modes.meet a_ret_mode b_ret_mode in
    let%map arg_ty = join_concrete_value a_arg_ty b_arg_ty
    and ret_ty = meet_concrete_value a_ret_ty b_ret_ty in
    Arrow { arg_ty; arg_mode; ret_ty; ret_mode }
  | ( Arrow { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Pi { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } ) ->
    let arg_mode = Modes.join a_arg_mode b_arg_mode in
    let ret_mode = Modes.meet a_ret_mode b_ret_mode in
    let%map arg_ty = join_concrete_value a_arg_ty b_arg_ty
    and ret_ty = meet_concrete_dependent (T a_ret_ty) b_ret_ty in
    Arrow { arg_ty; arg_mode; ret_ty; ret_mode }
  | ( Pi { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Arrow { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } )
    ->
    let arg_mode = Modes.join a_arg_mode b_arg_mode in
    let ret_mode = Modes.meet a_ret_mode b_ret_mode in
    let%map arg_ty = join_concrete_value a_arg_ty b_arg_ty
    and ret_ty = meet_concrete_dependent a_ret_ty (T b_ret_ty) in
    Arrow { arg_ty; arg_mode; ret_ty; ret_mode }
  | ( Pi { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Pi { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } ) ->
    let arg_mode = Modes.join a_arg_mode b_arg_mode in
    let ret_mode = Modes.meet a_ret_mode b_ret_mode in
    let%map arg_ty = join_concrete_value a_arg_ty b_arg_ty
    and ret_ty = meet_concrete_dependent a_ret_ty b_ret_ty in
    Pi { arg_ty; arg_mode; ret_ty = T ret_ty; ret_mode }
  | _ -> None

and join_concrete_bool (a : vbool) (b : vbool) : vbool option =
  match a, b with
  | T a, T b when Bool.equal a b -> Some (T a : vbool)
  | _ -> None

and meet_concrete_bool a b = join_concrete_bool a b

and join_concrete_int (a : vint) (b : vint) : vint option =
  match a, b with
  | T a, T b when Int64.equal a b -> Some (T a : vint)
  | _ -> None

and meet_concrete_int a b = join_concrete_int a b

and join_concrete_value (a : value) (b : value) : value option =
  match a, b with
  | Unit, Unit -> Some Unit
  | Bool a, Bool b ->
    let%map b = join_concrete_bool a b in
    (Bool b : value)
  | Int a, Int b ->
    let%map i = join_concrete_int a b in
    (Int i : value)
  | Type a, Type b ->
    let%map ty = join_concrete_ty a b in
    (Type ty : value)
  | ( If { cond = a_cond; then_ = a_then; else_ = a_else }
    , If { cond = b_cond; then_ = b_then; else_ = b_else } ) ->
    let%map cond = join_concrete_value a_cond b_cond
    and then_ = join_concrete_value a_then b_then
    and else_ = join_concrete_value a_else b_else in
    If { cond; then_; else_ }
  | Apply { fn = a_fn; arg = a_arg }, Apply { fn = b_fn; arg = b_arg } ->
    let%map fn = join_concrete_value a_fn b_fn
    and arg = join_concrete_value a_arg b_arg in
    Apply { fn; arg }
  | Var a, Var b when Ident.equal a b -> Some (Var a)
  | _ -> None

and meet_concrete_value (a : value) (b : value) : value option =
  match a, b with
  | Unit, Unit -> Some Unit
  | Bool a, Bool b ->
    let%map b = meet_concrete_bool a b in
    (Bool b : value)
  | Int a, Int b ->
    let%map i = meet_concrete_int a b in
    (Int i : value)
  | Type a, Type b ->
    let%map ty = meet_concrete_ty a b in
    (Type ty : value)
  | ( If { cond = a_cond; then_ = a_then; else_ = a_else }
    , If { cond = b_cond; then_ = b_then; else_ = b_else } ) ->
    let%map cond = meet_concrete_value a_cond b_cond
    and then_ = meet_concrete_value a_then b_then
    and else_ = meet_concrete_value a_else b_else in
    If { cond; then_; else_ }
  | Apply { fn = a_fn; arg = a_arg }, Apply { fn = b_fn; arg = b_arg } ->
    let%map fn = meet_concrete_value a_fn b_fn
    and arg = meet_concrete_value a_arg b_arg in
    Apply { fn; arg }
  | Var a, Var b when Ident.equal a b -> Some (Var a)
  | _ -> None

and join_concrete_dependent (a : dependent) (b : dependent) : value option =
  match a, b with
  | T a, T b -> join_concrete_value a b
  | _ -> None

and meet_concrete_dependent (a : dependent) (b : dependent) : value option =
  match a, b with
  | T a, T b -> meet_concrete_value a b
  | _ -> None
;;

module Desc = struct
  type t = desc =
    { id : Ident.t option
    ; ty : value
    ; mode : Modes.t
    ; static : (value Lazy.t[@sexp.opaque])
    }
  [@@deriving sexp]

  let of_type id ty =
    { id = Some id
    ; ty = Type Type
    ; mode = Modes.create ~staticity:Static ~erasure:Erased
    ; static = Lazy.from_val (Type ty : value)
    }
  ;;
end

module Env = struct
  type t = env [@@deriving sexp]

  let bind t id value = if Ident.is_anon id then t else Map.set t ~key:id ~data:value
  let find t id = Map.find t id

  let initial =
    Ident.Map.of_alist_exn
      [ (let id = Ident.of_string "type" in
         id, Desc.of_type id Type)
      ; (let id = Ident.of_string "unit" in
         id, Desc.of_type id Unit)
      ; (let id = Ident.of_string "bool" in
         id, Desc.of_type id Bool)
      ; (let id = Ident.of_string "int" in
         id, Desc.of_type id Int)
      ]
  ;;
end

module Bool = struct
  type t = vbool =
    | T of bool
    | And of value * value
    | Or of value * value
    | Eq of value * value
    | Neq of value * value
    | Lt of value * value
    | Lte of value * value
    | Gt of value * value
    | Gte of value * value
    | Not of value
  [@@deriving sexp]
end

module Int = struct
  type t = vint =
    | T of int64
    | Add of value * value
    | Sub of value * value
    | Mul of value * value
    | Div of value * value
    | Mod of value * value
    | Neg of value
  [@@deriving sexp]
end

module Ty = struct
  type t = ty =
    | Unit
    | Bool
    | Int
    | Type
    | Arrow of
        { arg_ty : value
        ; arg_mode : Modes.t
        ; ret_ty : value
        ; ret_mode : Modes.t
        }
    | Pi of
        { arg_ty : value
        ; arg_mode : Modes.t
        ; ret_ty : dependent
        ; ret_mode : Modes.t
        }
  [@@deriving sexp]

  let of_literal : Cst.Literal.t -> t = function
    | Unit -> Unit
    | Bool _ -> Bool
    | Int _ -> Int
  ;;
end

module Value = struct
  module Concrete = struct
    type t = concrete =
      | Unit
      | Bool of bool
      | Int of int64
      | Closure of int
      | UnitT
      | BoolT
      | IntT
      | TypeT
      | ArrowT of
          { arg : t
          ; arg_mode : Modes.t
          ; ret : t
          ; ret_mode : Modes.t
          }
    [@@deriving sexp, hash, compare]
  end

  type t = value =
    | Unit
    | Bool of vbool
    | Int of vint
    | Type of ty
    | Closure of closure
    | Binder of binder
    | Var of Ident.t
    | If of
        { cond : t
        ; then_ : t
        ; else_ : t
        }
    | Apply of
        { fn : t
        ; arg : t
        }
    | External of
        { symbol : string
        ; ty : t
        }
  [@@deriving sexp]

  let is_concrete = is_concrete_value
  let is_abstract t = not (is_concrete t)

  let of_literal : Cst.Literal.t -> t = function
    | Unit -> Unit
    | Bool b -> Bool (T b)
    | Int i -> Int (T i)
  ;;
end

module Dependent = struct
  type t = dependent =
    | T of value
    | Meet of t * t
    | Join of t * t
    | Reduce of
        { env : (env[@sexp.opaque])
        ; arg : Ident.t
        ; arg_ty : value
        ; arg_mode : Modes.t
        ; memo : ((concrete, value) Hashtbl.t[@sexp.opaque])
        ; ret_ty : Cst.Expr.t
        }
    | Typecheck of
        { env : (env[@sexp.opaque])
        ; arg : Ident.t
        ; arg_ty : value
        ; arg_mode : Modes.t
        ; memo : ((concrete, value) Hashtbl.t[@sexp.opaque])
        ; body : Cst.Expr.t
        }
  [@@deriving sexp]

  let is_concrete = is_concrete_dependent
  let is_abstract t = not (is_concrete t)

  let join a b =
    Option.map (join_concrete_dependent a b) ~f:(fun v : t -> T v)
    |> Option.value ~default:(Join (a, b))
  ;;

  let meet a b =
    Option.map (meet_concrete_dependent a b) ~f:(fun v : t -> T v)
    |> Option.value ~default:(Meet (a, b))
  ;;

  let typecheck ty ~env ~arg ~arg_ty ~arg_mode ~body =
    if is_concrete_value ty
    then T ty
    else
      Typecheck { env; arg; arg_ty; arg_mode; memo = Hashtbl.create (module Value.Concrete); body }
  ;;

  let reduce ty ~env ~arg ~arg_ty ~arg_mode ~ret_ty =
    if is_concrete_value ty
    then T ty
    else
      Reduce { env; arg; arg_ty; arg_mode; memo = Hashtbl.create (module Value.Concrete); ret_ty }
  ;;
end

module Closure = struct
  type t = closure =
    { arg : Ident.t
    ; ty : value
    ; body : expr
    ; env : (env[@sexp.opaque])
    }
  [@@deriving sexp]
end

module Binder = struct
  module Mono = struct
    type t = mono =
      { arg : Ident.t
      ; arg_val : value
      ; arg_ty : value
      ; arg_mode : Modes.t
      ; body : expr
      ; body_desc : desc
      }
    [@@deriving sexp]
  end

  type t = binder =
    { arg : Ident.t
    ; ty : value
    ; body : Cst.Expr.t
    ; mono : ((concrete, mono) Hashtbl.t[@sexp.opaque])
    ; env : (env[@sexp.opaque])
    }
  [@@deriving sexp]
end

module Expr = struct
  type t = expr =
    | Literal of
        { value : value
        ; ty : value
        ; mode : Modes.t
        ; loc : (Lex.Location.t[@sexp.opaque])
        }
    | Fun of
        { funs : fun_ Nonempty_list.t
        ; rest : t
        ; loc : (Lex.Location.t[@sexp.opaque])
        }
    | Lambda of
        { arg : Ident.t
        ; ty : value
        ; body : t
        ; mode : Modes.t
        ; loc : (Lex.Location.t[@sexp.opaque])
        }
    | Binder of
        { arg : Ident.t
        ; ty : value
        ; body : Cst.Expr.t
        ; mono : ((concrete, mono) Hashtbl.t[@sexp.opaque])
        ; mode : Modes.t
        ; loc : (Lex.Location.t[@sexp.opaque])
        }
    | Apply of
        { fn : t
        ; arg : t
        ; ty : value
        ; mode : Modes.t
        ; loc : (Lex.Location.t[@sexp.opaque])
        }
    | Let of
        { var : Ident.t
        ; bind : t
        ; rest : t
        ; ty : value
        ; mode : Modes.t
        ; loc : (Lex.Location.t[@sexp.opaque])
        }
    | Unop of
        { op : Cst.Unop.t
        ; arg : t
        ; ty : value
        ; mode : Modes.t
        ; loc : (Lex.Location.t[@sexp.opaque])
        }
    | Binop of
        { op : Cst.Binop.t
        ; lhs : t
        ; rhs : t
        ; ty : value
        ; mode : Modes.t
        ; loc : (Lex.Location.t[@sexp.opaque])
        }
    | If of
        { cond : t
        ; then_ : t
        ; else_ : t
        ; ty : value
        ; mode : Modes.t
        ; loc : (Lex.Location.t[@sexp.opaque])
        }
    | Var of
        { id : Ident.t
        ; ty : value
        ; mode : Modes.t
        ; loc : (Lex.Location.t[@sexp.opaque])
        }
    | Symbol of
        { id : Ident.t
        ; arg : concrete
        ; mode : Modes.t
        ; ty : value
        ; loc : Lex.Location.t
        }
    | Erased of
        { ty : value
        ; mode : Modes.t
        ; loc : (Lex.Location.t[@sexp.opaque])
        }
  [@@deriving sexp]

  type nonrec fun_ = fun_ =
    | Lambda of
        { var : Ident.t
        ; arg : Ident.t
        ; body : t
        ; ty : value
        ; mode : Modes.t
        ; loc : (Lex.Location.t[@sexp.opaque])
        }
    | Binder of
        { var : Ident.t
        ; arg : Ident.t
        ; body : Cst.Expr.t
        ; mono : ((concrete, mono) Hashtbl.t[@sexp.opaque])
        ; ty : value
        ; mode : Modes.t
        ; loc : (Lex.Location.t[@sexp.opaque])
        }
  [@@deriving sexp]

  let rec free_vars (expr : t) : Ident.Set.t =
    match expr with
    | Literal _ | Erased _ -> Ident.Set.empty
    | Var { id; _ } | Symbol { id; _ } -> Ident.Set.singleton id
    | Unop { arg; _ } -> free_vars arg
    | Binop { lhs; rhs; _ } -> Set.union (free_vars lhs) (free_vars rhs)
    | Apply { fn; arg; _ } -> Set.union (free_vars fn) (free_vars arg)
    | If { cond; then_; else_; _ } ->
      Ident.Set.union_list [ free_vars cond; free_vars then_; free_vars else_ ]
    | Let { var; bind; rest; _ } -> Set.union (free_vars bind) (Set.remove (free_vars rest) var)
    | Lambda { arg; body; _ } -> Set.remove (free_vars body) arg
    | Binder { arg; body; _ } -> Set.remove (Cst.Expr.free_vars body) arg
    | Fun { funs; rest; _ } ->
      let bound_ids =
        Nonempty_list.map funs ~f:(fun (f : fun_) ->
          match f with
          | Lambda { var; _ } | Binder { var; _ } -> var)
        |> Nonempty_list.to_list
        |> Ident.Set.of_list
      in
      let fvs_in_funs =
        Nonempty_list.fold funs ~init:Ident.Set.empty ~f:(fun acc (f : fun_) ->
          match f with
          | Lambda { arg; body; _ } -> Set.union acc (Set.remove (free_vars body) arg)
          | Binder { arg; body; _ } -> Set.union acc (Set.remove (Cst.Expr.free_vars body) arg))
      in
      Set.diff (Set.union fvs_in_funs (free_vars rest)) bound_ids
  ;;

  let ty = function
    | Literal { ty; _ }
    | Apply { ty; _ }
    | Unop { ty; _ }
    | Binop { ty; _ }
    | If { ty; _ }
    | Var { ty; _ }
    | Lambda { ty; _ }
    | Binder { ty; _ }
    | Erased { ty; _ }
    | Let { ty; _ }
    | Symbol { ty; _ } -> ty
    | Fun _ -> assert false
  ;;

  let mode = function
    | Literal { mode; _ }
    | Apply { mode; _ }
    | Unop { mode; _ }
    | Binop { mode; _ }
    | If { mode; _ }
    | Var { mode; _ }
    | Lambda { mode; _ }
    | Binder { mode; _ }
    | Erased { mode; _ }
    | Let { mode; _ }
    | Symbol { mode; _ } -> mode
    | Fun _ -> assert false
  ;;

  let desc t static = { Desc.id = None; ty = ty t; mode = mode t; static }

  let with_ t ~ty ~mode =
    match t with
    | Literal t -> Literal { t with ty; mode }
    | Apply t -> Apply { t with ty; mode }
    | Unop t -> Unop { t with ty; mode }
    | Binop t -> Binop { t with ty; mode }
    | If t -> If { t with ty; mode }
    | Var t -> Var { t with ty; mode }
    | Lambda t -> Lambda { t with ty; mode }
    | Binder t -> Binder { t with ty; mode }
    | Erased t -> Erased { t with ty; mode }
    | Let t -> Let { t with ty; mode }
    | Symbol t -> Symbol { t with ty; mode }
    | Fun _ -> assert false
  ;;

  let with_ty t ty =
    match t with
    | Literal t -> Literal { t with ty }
    | Apply t -> Apply { t with ty }
    | Unop t -> Unop { t with ty }
    | Binop t -> Binop { t with ty }
    | If t -> If { t with ty }
    | Var t -> Var { t with ty }
    | Lambda t -> Lambda { t with ty }
    | Binder t -> Binder { t with ty }
    | Erased t -> Erased { t with ty }
    | Let t -> Let { t with ty }
    | Symbol t -> Symbol { t with ty }
    | Fun _ -> assert false
  ;;

  let with_mode t mode =
    match t with
    | Literal t -> Literal { t with mode }
    | Apply t -> Apply { t with mode }
    | Unop t -> Unop { t with mode }
    | Binop t -> Binop { t with mode }
    | If t -> If { t with mode }
    | Var t -> Var { t with mode }
    | Lambda t -> Lambda { t with mode }
    | Binder t -> Binder { t with mode }
    | Erased t -> Erased { t with mode }
    | Let t -> Let { t with mode }
    | Symbol t -> Symbol { t with mode }
    | Fun _ -> assert false
  ;;
end

module Top_level = struct
  type t =
    | Let of
        { var : Ident.t
        ; bind : Expr.t
        ; loc : (Lex.Location.t[@sexp.opaque])
        }
    | Fun of
        { funs : Expr.fun_ Nonempty_list.t
        ; loc : (Lex.Location.t[@sexp.opaque])
        }
    | External of
        { var : Ident.t
        ; symbol : string
        ; ty : Value.t
        ; loc : Lex.Location.t
        }
  [@@deriving sexp]
end

module Program = struct
  type t = Top_level.t list [@@deriving sexp]
end
