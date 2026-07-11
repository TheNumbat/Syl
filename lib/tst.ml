open! Core
open Option.Let_syntax

module Concrete = struct
  module T = struct
    type t =
      | Unit
      | Bool of bool
      | Int of int64
      | Tuple of t Nonempty_list.t
      | Closure of int
      | Prim of Builtin0.t
      | External of string
      | Arrow of
          { arg : t
          ; arg_mode : Modes.t
          ; ret : t
          ; ret_mode : Modes.t
          }
      | Tuple_t of t Nonempty_list.t
    [@@deriving sexp, hash, compare]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

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
  | Tuple of value Nonempty_list.t
[@@deriving sexp]

and dependent =
  | T of
      { ty : value
      ; memo : ((concrete, value) Hashtbl.t[@sexp.opaque])
      }
  | Meet of dependent * dependent
  | Join of dependent * dependent
  | Reduce of
      { env : (env[@sexp.opaque])
      ; arg : Ident.t
      ; arg_ty : value
      ; arg_mode : Modes.t
      ; memo : ((concrete, value) Hashtbl.t[@sexp.opaque])
      ; ret_ty : Dst.Expr.t
      }
  | Typecheck of
      { env : (env[@sexp.opaque])
      ; arg : Ident.t
      ; arg_ty : value
      ; arg_mode : Modes.t
      ; memo : ((concrete, value) Hashtbl.t[@sexp.opaque])
      ; body : Dst.Expr.t
      }
[@@deriving sexp]

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
  | Bottom
  | Unit
  | Bool of vbool
  | Int of vint
  | Type of ty
  | Closure of closure
  | Binder of binder
  | Var of Ident.t
  | Tuple of value Nonempty_list.t
  | Apply of
      { fn : value
      ; arg : value
      }
  | Proj of
      { tuple : value
      ; index : int
      }
  | Match of
      { scrutinee : value
      ; arms : (Dst.Expr.pattern * value) Nonempty_list.t
      }
  | External of
      { symbol : string
      ; ty : value
      }
  | Prim of Builtin0.Prim.t
[@@deriving sexp]

and concrete = Concrete.t [@@deriving sexp]

and closure =
  { arg : Ident.t
  ; ty : value
  ; body : expr
  ; body_dst : Dst.Expr.t
  ; env : (env[@sexp.opaque])
  ; family : (int[@sexp.opaque])
  ; hash : (int[@sexp.opaque])
  }
[@@deriving sexp]

and binder =
  { arg : Ident.t
  ; ty : value
  ; body_dst : Dst.Expr.t
  ; env : (env[@sexp.opaque])
  ; family : (int[@sexp.opaque])
  ; hash : (int[@sexp.opaque])
  }
[@@deriving sexp]

and desc =
  { ty : value
  ; mode : Modes.t
  ; static : value Lazy.t
  }
[@@deriving sexp]

and env = desc Ident.Map.t

and fun_ =
  | Lambda of
      { var : Ident.t
      ; arg : Ident.t
      ; body : expr
      ; ty : value
      ; mode : Modes.t
      ; family : (int[@sexp.opaque])
      ; loc : Lex.Location.t
      }
  | Binder of
      { var : Ident.t
      ; arg : Ident.t
      ; body : expr Concrete.Map.t
      ; ty : value
      ; mode : Modes.t
      ; family : (int[@sexp.opaque])
      ; loc : Lex.Location.t
      }
[@@deriving sexp]

and case =
  { bindings : value Ident.Map.t
  ; body : expr
  }
[@@deriving sexp]

and tree =
  | Leaf of
      { case : int
      ; bindings : expr Ident.Map.t
      }
  | Split of
      { cond : expr
      ; then_ : tree
      ; else_ : tree
      }
[@@deriving sexp]

and target =
  | Family of (int[@sexp.opaque])
  | Prim of Builtin0.Prim.t
[@@deriving sexp]

and expr =
  | Erased of
      { ty : value
      ; mode : Modes.t
      ; loc : Lex.Location.t
      }
  | Literal of
      { value : value
      ; ty : value
      ; mode : Modes.t
      ; loc : Lex.Location.t
      }
  | Fun of
      { funs : fun_ Nonempty_list.t
      ; rest : expr
      ; ty : value
      ; mode : Modes.t
      ; loc : Lex.Location.t
      }
  | Lambda of
      { arg : Ident.t
      ; body : expr
      ; ty : value
      ; mode : Modes.t
      ; family : (int[@sexp.opaque])
      ; loc : Lex.Location.t
      }
  | Binder of
      { arg : Ident.t
      ; body : expr Concrete.Map.t
      ; ty : value
      ; mode : Modes.t
      ; family : (int[@sexp.opaque])
      ; loc : Lex.Location.t
      }
  | Apply of
      { fn : expr
      ; arg : expr
      ; ty : value
      ; mode : Modes.t
      ; loc : Lex.Location.t
      }
  | Specialize of
      { fn : expr
      ; arg : expr
      ; target : target
      ; key : concrete option
      ; ty : value
      ; mode : Modes.t
      ; loc : Lex.Location.t
      }
  | Let of
      { var : Ident.t
      ; bind : expr
      ; rest : expr
      ; ty : value
      ; mode : Modes.t
      ; loc : Lex.Location.t
      }
  | Tuple of
      { elts : expr Nonempty_list.t
      ; ty : value
      ; mode : Modes.t
      ; loc : Lex.Location.t
      }
  | Tuple_get of
      { tuple : expr
      ; index : int
      ; ty : value
      ; mode : Modes.t
      ; loc : Lex.Location.t
      }
  | If of
      { cond : expr
      ; then_ : expr
      ; else_ : expr
      ; ty : value
      ; mode : Modes.t
      ; loc : Lex.Location.t
      }
  | Match of
      { cases : case Nonempty_list.t
      ; tree : tree
      ; ty : value
      ; mode : Modes.t
      ; loc : Lex.Location.t
      }
  | Var of
      { id : Ident.t
      ; ty : value
      ; mode : Modes.t
      ; loc : Lex.Location.t
      }
  | Builtin of
      { builtin : Builtin0.t
      ; ty : value
      ; mode : Modes.t
      ; loc : Lex.Location.t
      }
  | Extcall of
      { symbol : string
      ; arg : expr
      ; ty : value
      ; mode : Modes.t
      ; loc : Lex.Location.t
      }
[@@deriving sexp]

module Closure = struct
  type t = closure =
    { arg : (Ident.t[@compare.ignore] [@hash.ignore])
    ; ty : (value[@compare.ignore] [@hash.ignore])
    ; body : (expr[@compare.ignore] [@hash.ignore])
    ; body_dst : (Dst.Expr.t[@compare.ignore] [@hash.ignore])
    ; env : (env[@compare.ignore] [@hash.ignore] [@sexp.opaque])
    ; family : (int[@compare.ignore] [@hash.ignore] [@sexp.opaque])
    ; hash : (int[@sexp.opaque])
    }
  [@@deriving sexp, compare, hash]
end

module Binder = struct
  type t = binder =
    { arg : (Ident.t[@compare.ignore] [@hash.ignore])
    ; ty : (value[@compare.ignore] [@hash.ignore])
    ; body_dst : (Dst.Expr.t[@compare.ignore] [@hash.ignore])
    ; env : (env[@compare.ignore] [@hash.ignore] [@sexp.opaque])
    ; family : (int[@compare.ignore] [@hash.ignore] [@sexp.opaque])
    ; hash : (int[@sexp.opaque])
    }
  [@@deriving sexp, compare, hash]
end

let mono ty : dependent = T { ty; memo = Hashtbl.create (module Concrete) }

let rec patterns_unify (a : Dst.Expr.pattern) (b : Dst.Expr.pattern) =
  match a, b with
  | Var _, Var _ ->
    (* Arm leaves are closed, so binder names are irrelevant. *)
    true
  | Literal { value = a; _ }, Literal { value = b; _ } -> Dst.Literal.equal a b
  | Tuple { elts = a; _ }, Tuple { elts = b; _ } ->
    (match Nonempty_list.zip a b with
     | Ok zip -> Nonempty_list.for_all zip ~f:(fun (a, b) -> patterns_unify a b)
     | Unequal_lengths -> false)
  | Or { left = a_left; right = a_right; _ }, Or { left = b_left; right = b_right; _ } ->
    patterns_unify a_left b_left && patterns_unify a_right b_right
  | (Var _ | Literal _ | Tuple _ | Or _), _ -> false
;;

let arms_unify a_arms b_arms =
  match Nonempty_list.zip a_arms b_arms with
  | Ok zip ->
    let zip = Nonempty_list.to_list zip in
    let last = List.length zip - 1 in
    List.for_alli zip ~f:(fun i ((a_pattern, _), (b_pattern, _)) ->
      i = last || patterns_unify a_pattern b_pattern)
  | Unequal_lengths -> false
;;

let merge_arms a_arms b_arms ~f =
  if arms_unify a_arms b_arms
  then
    Nonempty_list.zip_exn a_arms b_arms
    |> Nonempty_list.map ~f:(fun ((pattern, a_leaf), (_, b_leaf)) ->
      Option.map (f a_leaf b_leaf) ~f:(fun leaf -> pattern, leaf))
    |> Nonempty_list.to_list
    |> Option.all
    |> Option.map ~f:Nonempty_list.of_list_exn
  else None
;;

let rec is_concrete_value : value -> _ = function
  | Unit | Closure _ | Binder _ | External _ | Prim _ -> true
  | Bool b -> is_concrete_bool b
  | Int i -> is_concrete_int i
  | Type ty -> is_concrete_ty ty
  | Tuple elts -> Nonempty_list.for_all elts ~f:is_concrete_value
  | Bottom | Var _ | Apply _ | Proj _ | Match _ -> false

and is_concrete_bool : vbool -> _ = function
  | T _ -> true
  | _ -> false

and is_concrete_int : vint -> _ = function
  | T _ -> true
  | _ -> false

and is_concrete_ty : ty -> _ = function
  | Unit | Bool | Int | Type -> true
  | Tuple elts -> Nonempty_list.for_all elts ~f:is_concrete_value
  | Arrow { arg_ty; ret_ty; _ } -> is_concrete_value arg_ty && is_concrete_value ret_ty
  | Pi { arg_ty; ret_ty; _ } -> is_concrete_value arg_ty && is_concrete_dependent ret_ty

and is_concrete_dependent : dependent -> _ = function
  | T { ty; _ } -> is_concrete_value ty
  | Meet _ | Join _ | Reduce _ | Typecheck _ -> false
;;

let rec join_concrete_ty (a : ty) (b : ty) : ty option =
  match a, b with
  | Unit, Unit -> Some Unit
  | Bool, Bool -> Some Bool
  | Int, Int -> Some Int
  | Type, Type -> Some Type
  | Tuple a_elts, Tuple b_elts ->
    (match Nonempty_list.map2 a_elts b_elts ~f:join_concrete_value with
     | Ok elts ->
       Nonempty_list.to_list elts
       |> Option.all
       |> Option.map ~f:(fun elts -> Tuple (Nonempty_list.of_list_exn elts))
     | Unequal_lengths -> None)
  | ( Arrow { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Arrow { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } )
    ->
    let arg_mode = Modes.meet a_arg_mode b_arg_mode in
    let ret_mode = Modes.join a_ret_mode b_ret_mode in
    let%bind arg_ty = meet_concrete_value a_arg_ty b_arg_ty in
    let%map ret_ty = join_concrete_value a_ret_ty b_ret_ty in
    Arrow { arg_ty; arg_mode; ret_ty; ret_mode }
  | ( Arrow { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Pi { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } ) ->
    let arg_mode = Modes.meet a_arg_mode b_arg_mode in
    let ret_mode = Modes.join a_ret_mode b_ret_mode in
    let%bind arg_ty = meet_concrete_value a_arg_ty b_arg_ty in
    let%map ret_ty = join_concrete_dependent (mono a_ret_ty) b_ret_ty in
    Pi { arg_ty; arg_mode; ret_ty = mono ret_ty; ret_mode }
  | ( Pi { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Arrow { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } )
    ->
    let arg_mode = Modes.meet a_arg_mode b_arg_mode in
    let ret_mode = Modes.join a_ret_mode b_ret_mode in
    let%bind arg_ty = meet_concrete_value a_arg_ty b_arg_ty in
    let%map ret_ty = join_concrete_dependent a_ret_ty (mono b_ret_ty) in
    Pi { arg_ty; arg_mode; ret_ty = mono ret_ty; ret_mode }
  | ( Pi { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Pi { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } ) ->
    let arg_mode = Modes.meet a_arg_mode b_arg_mode in
    let ret_mode = Modes.join a_ret_mode b_ret_mode in
    let%bind arg_ty = meet_concrete_value a_arg_ty b_arg_ty in
    let%map ret_ty = join_concrete_dependent a_ret_ty b_ret_ty in
    Pi { arg_ty; arg_mode; ret_ty = mono ret_ty; ret_mode }
  | (Unit | Bool | Int | Type | Arrow _ | Pi _ | Tuple _), _ -> None

and meet_concrete_ty (a : ty) (b : ty) : ty option =
  match a, b with
  | Unit, Unit -> Some Unit
  | Bool, Bool -> Some Bool
  | Int, Int -> Some Int
  | Type, Type -> Some Type
  | Tuple a_elts, Tuple b_elts ->
    (match Nonempty_list.map2 a_elts b_elts ~f:meet_concrete_value with
     | Ok elts ->
       Nonempty_list.to_list elts
       |> Option.all
       |> Option.map ~f:(fun elts -> Tuple (Nonempty_list.of_list_exn elts))
     | Unequal_lengths -> None)
  | ( Arrow { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Arrow { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } )
    ->
    let arg_mode = Modes.join a_arg_mode b_arg_mode in
    let ret_mode = Modes.meet a_ret_mode b_ret_mode in
    let%bind arg_ty = join_concrete_value a_arg_ty b_arg_ty in
    let%map ret_ty = meet_concrete_value a_ret_ty b_ret_ty in
    Arrow { arg_ty; arg_mode; ret_ty; ret_mode }
  | ( Arrow { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Pi { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } ) ->
    let arg_mode = Modes.join a_arg_mode b_arg_mode in
    let ret_mode = Modes.meet a_ret_mode b_ret_mode in
    let%bind arg_ty = join_concrete_value a_arg_ty b_arg_ty in
    let%map ret_ty = meet_concrete_dependent (mono a_ret_ty) b_ret_ty in
    Arrow { arg_ty; arg_mode; ret_ty; ret_mode }
  | ( Pi { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Arrow { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } )
    ->
    let arg_mode = Modes.join a_arg_mode b_arg_mode in
    let ret_mode = Modes.meet a_ret_mode b_ret_mode in
    let%bind arg_ty = join_concrete_value a_arg_ty b_arg_ty in
    let%map ret_ty = meet_concrete_dependent a_ret_ty (mono b_ret_ty) in
    Arrow { arg_ty; arg_mode; ret_ty; ret_mode }
  | ( Pi { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
    , Pi { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } ) ->
    let arg_mode = Modes.join a_arg_mode b_arg_mode in
    let ret_mode = Modes.meet a_ret_mode b_ret_mode in
    let%bind arg_ty = join_concrete_value a_arg_ty b_arg_ty in
    let%map ret_ty = meet_concrete_dependent a_ret_ty b_ret_ty in
    Pi { arg_ty; arg_mode; ret_ty = mono ret_ty; ret_mode }
  | (Unit | Bool | Int | Type | Arrow _ | Pi _ | Tuple _), _ -> None

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
  | Bottom, _ | _, Bottom -> Some Bottom
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
  | ( Match { scrutinee = a_scrutinee; arms = a_arms }
    , Match { scrutinee = b_scrutinee; arms = b_arms } ) ->
    let%bind arms = merge_arms a_arms b_arms ~f:join_concrete_value in
    let%map scrutinee = join_concrete_value a_scrutinee b_scrutinee in
    Match { scrutinee; arms }
  | Apply { fn = a_fn; arg = a_arg }, Apply { fn = b_fn; arg = b_arg } ->
    let%bind fn = join_concrete_value a_fn b_fn in
    let%map arg = join_concrete_value a_arg b_arg in
    Apply { fn; arg }
  | Proj a, Proj b when a.index = b.index ->
    let%map tuple = join_concrete_value a.tuple b.tuple in
    (Proj { tuple; index = a.index } : value)
  | Var a, Var b when Ident.equal a b -> Some (Var a)
  | Prim a, Prim b when Builtin0.Prim.equal a b -> Some (Prim a)
  | External a, External b when String.equal a.symbol b.symbol -> Some (External a)
  | Tuple a_elts, Tuple b_elts ->
    (match Nonempty_list.map2 a_elts b_elts ~f:join_concrete_value with
     | Ok elts ->
       Nonempty_list.to_list elts
       |> Option.all
       |> Option.map ~f:(fun elts : value -> Tuple (Nonempty_list.of_list_exn elts))
     | Unequal_lengths -> None)
  | ( ( Unit
      | Bool _
      | Int _
      | Type _
      | Apply _
      | Proj _
      | Match _
      | Var _
      | Tuple _
      | Closure _
      | Binder _
      | External _
      | Prim _ )
    , _ ) -> None

and meet_concrete_value (a : value) (b : value) : value option =
  match a, b with
  | a, Bottom -> Some a
  | Bottom, b -> Some b
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
  | ( Match { scrutinee = a_scrutinee; arms = a_arms }
    , Match { scrutinee = b_scrutinee; arms = b_arms } ) ->
    let%bind arms = merge_arms a_arms b_arms ~f:meet_concrete_value in
    let%map scrutinee = meet_concrete_value a_scrutinee b_scrutinee in
    Match { scrutinee; arms }
  | Apply { fn = a_fn; arg = a_arg }, Apply { fn = b_fn; arg = b_arg } ->
    let%bind fn = meet_concrete_value a_fn b_fn in
    let%map arg = meet_concrete_value a_arg b_arg in
    Apply { fn; arg }
  | Proj a, Proj b when a.index = b.index ->
    let%map tuple = meet_concrete_value a.tuple b.tuple in
    (Proj { tuple; index = a.index } : value)
  | Var a, Var b when Ident.equal a b -> Some (Var a)
  | Prim a, Prim b when Builtin0.Prim.equal a b -> Some (Prim a)
  | External a, External b when String.equal a.symbol b.symbol -> Some (External a)
  | Tuple a_elts, Tuple b_elts ->
    (match Nonempty_list.map2 a_elts b_elts ~f:meet_concrete_value with
     | Ok elts ->
       Nonempty_list.to_list elts
       |> Option.all
       |> Option.map ~f:(fun elts : value -> Tuple (Nonempty_list.of_list_exn elts))
     | Unequal_lengths -> None)
  | ( ( Unit
      | Bool _
      | Int _
      | Type _
      | Apply _
      | Proj _
      | Match _
      | Var _
      | Tuple _
      | Closure _
      | Binder _
      | External _
      | Prim _ )
    , _ ) -> None

and join_concrete_dependent (a : dependent) (b : dependent) : value option =
  match a, b with
  | T { ty = a; _ }, T { ty = b; _ } -> join_concrete_value a b
  | _ -> None

and meet_concrete_dependent (a : dependent) (b : dependent) : value option =
  match a, b with
  | T { ty = a; _ }, T { ty = b; _ } -> meet_concrete_value a b
  | _ -> None
;;

module Desc = struct
  type t = desc =
    { ty : value
    ; mode : Modes.t
    ; static : (value Lazy.t[@sexp.opaque])
    }
  [@@deriving sexp]

  let of_type ty =
    { ty = Type Type
    ; mode = Modes.create ~staticity:Static ~erasure:Erased
    ; static = Lazy.from_val (Type ty : value)
    }
  ;;
end

module Bool = struct
  open Int64.O

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

  let const b : value = Bool (T b)

  (* Only does rewrites that remove terms. *)
  let rec reduce : t -> value = function
    | And (Bottom, _)
    | And (_, Bottom)
    | Or (Bottom, _)
    | Or (_, Bottom)
    | Eq (Bottom, _)
    | Eq (_, Bottom)
    | Neq (Bottom, _)
    | Neq (_, Bottom)
    | Lt (Bottom, _)
    | Lt (_, Bottom)
    | Lte (Bottom, _)
    | Lte (_, Bottom)
    | Gt (Bottom, _)
    | Gt (_, Bottom)
    | Gte (Bottom, _)
    | Gte (_, Bottom)
    | Not Bottom -> Bottom
    | And (Bool (T x), Bool (T y)) -> const (x && y)
    | And (Bool (T false), _) | And (_, Bool (T false)) -> const false
    | (And (Var x, Bool (Not (Var y))) | And (Bool (Not (Var x)), Var y)) when Ident.equal x y ->
      const false
    | And (Bool (T true), Bool b) | And (Bool b, Bool (T true)) -> reduce b
    | And (Bool (T true), v) | And (v, Bool (T true)) -> v
    | Or (Bool (T x), Bool (T y)) -> const (x || y)
    | Or (Bool (T true), _) | Or (_, Bool (T true)) -> const true
    | (Or (Var x, Bool (Not (Var y))) | Or (Bool (Not (Var x)), Var y)) when Ident.equal x y ->
      const true
    | Or (Bool (T false), Bool b) | Or (Bool b, Bool (T false)) -> reduce b
    | Or (Bool (T false), v) | Or (v, Bool (T false)) -> v
    | Not (Bool (T x)) -> const (not x)
    | Not (Bool (Not (Bool b))) -> reduce b
    | Not (Bool (Not v)) -> v
    | Eq (Int (T x), Int (T y)) -> const (x = y)
    | Eq (Int x, Int y) when eq_int x y -> const true
    | Eq (Var x, Var y) when Ident.equal x y -> const true
    | Neq (Int (T x), Int (T y)) -> const (x <> y)
    | Neq (Int x, Int y) when eq_int x y -> const false
    | Neq (Var x, Var y) when Ident.equal x y -> const false
    | Lt (Int (T x), Int (T y)) -> const (x < y)
    | Lt (Int x, Int y) when eq_int x y -> const false
    | Lt (Var x, Var y) when Ident.equal x y -> const false
    | Lte (Int (T x), Int (T y)) -> const (x <= y)
    | Lte (Int x, Int y) when eq_int x y -> const true
    | Lte (Var x, Var y) when Ident.equal x y -> const true
    | Gt (Int (T x), Int (T y)) -> const (x > y)
    | Gt (Int x, Int y) when eq_int x y -> const false
    | Gt (Var x, Var y) when Ident.equal x y -> const false
    | Gte (Int (T x), Int (T y)) -> const (x >= y)
    | Gte (Int x, Int y) when eq_int x y -> const true
    | Gte (Var x, Var y) when Ident.equal x y -> const true
    | (T _ | And _ | Or _ | Eq _ | Neq _ | Lt _ | Lte _ | Gt _ | Gte _ | Not _) as expr -> Bool expr

  and and_ a b = reduce (And (a, b))
  and or_ a b = reduce (Or (a, b))
  and eq a b = reduce (Eq (a, b))
  and neq a b = reduce (Neq (a, b))
  and lt a b = reduce (Lt (a, b))
  and lte a b = reduce (Lte (a, b))
  and gt a b = reduce (Gt (a, b))
  and gte a b = reduce (Gte (a, b))
  and not_ v = reduce (Not v)

  and eq_int x y =
    match x, y with
    | T x, T y -> Int64.equal x y
    | Add (Int x0, Int x1), Add (Int y0, Int y1)
    | Sub (Int x0, Int x1), Sub (Int y0, Int y1)
    | Mul (Int x0, Int x1), Mul (Int y0, Int y1)
    | Div (Int x0, Int x1), Div (Int y0, Int y1)
    | Mod (Int x0, Int x1), Mod (Int y0, Int y1) -> eq_int x0 y0 && eq_int x1 y1
    | Add (Int x0, Var x1), Add (Int y0, Var y1)
    | Sub (Int x0, Var x1), Sub (Int y0, Var y1)
    | Mul (Int x0, Var x1), Mul (Int y0, Var y1)
    | Div (Int x0, Var x1), Div (Int y0, Var y1)
    | Mod (Int x0, Var x1), Mod (Int y0, Var y1) -> eq_int x0 y0 && Ident.equal x1 y1
    | Add (Var x0, Int x1), Add (Var y0, Int y1)
    | Sub (Var x0, Int x1), Sub (Var y0, Int y1)
    | Mul (Var x0, Int x1), Mul (Var y0, Int y1)
    | Div (Var x0, Int x1), Div (Var y0, Int y1)
    | Mod (Var x0, Int x1), Mod (Var y0, Int y1) -> Ident.equal x0 y0 && eq_int x1 y1
    | Add (Var x0, Var x1), Add (Var y0, Var y1)
    | Sub (Var x0, Var x1), Sub (Var y0, Var y1)
    | Mul (Var x0, Var x1), Mul (Var y0, Var y1)
    | Div (Var x0, Var x1), Div (Var y0, Var y1)
    | Mod (Var x0, Var x1), Mod (Var y0, Var y1) -> Ident.equal x0 y0 && Ident.equal x1 y1
    | Neg (Int x), Neg (Int y) -> eq_int x y
    | Neg (Var x), Neg (Var y) -> Ident.equal x y
    | (T _ | Add _ | Sub _ | Mul _ | Div _ | Mod _ | Neg _), _ -> false
  ;;
end

module Int = struct
  open Int64.O

  type t = vint =
    | T of int64
    | Add of value * value
    | Sub of value * value
    | Mul of value * value
    | Div of value * value
    | Mod of value * value
    | Neg of value
  [@@deriving sexp]

  let const i : value = Int (T i)

  (* Only does rewrites that remove terms. *)
  let rec reduce : t -> value = function
    | Add (Bottom, _)
    | Add (_, Bottom)
    | Sub (Bottom, _)
    | Sub (_, Bottom)
    | Mul (Bottom, _)
    | Mul (_, Bottom)
    | Div (Bottom, _)
    | Div (_, Bottom)
    | Mod (Bottom, _)
    | Mod (_, Bottom)
    | Neg Bottom -> Bottom
    | Add (Int (T x), Int (T y)) -> const (x + y)
    | (Add (Var x, Int (Neg (Var y))) | Add (Int (Neg (Var x)), Var y)) when Ident.equal x y ->
      const 0L
    | Add (Int (T 0L), Int i) | Add (Int i, Int (T 0L)) -> reduce i
    | Add (Int (T 0L), v) | Add (v, Int (T 0L)) -> v
    | Sub (Int (T x), Int (T y)) -> const (x - y)
    | Sub (Int i, Int (T 0L)) -> reduce i
    | Sub (v, Int (T 0L)) -> v
    | Sub (Int (T 0L), v) -> reduce (Neg v)
    | Sub (Var x, Var y) when Ident.equal x y -> const 0L
    | Sub (x, Int (Neg y)) -> reduce (Add (x, y))
    | Mul (Int (T x), Int (T y)) -> const (x * y)
    | Mul (_, Int (T 0L)) | Mul (Int (T 0L), _) -> const 0L
    | Mul (Int i, Int (T 1L)) | Mul (Int (T 1L), Int i) -> reduce i
    | Mul (v, Int (T -1L)) | Mul (Int (T -1L), v) -> reduce (Neg v)
    | Mul (v, Int (T 1L)) | Mul (Int (T 1L), v) -> v
    | Div (v, Int (T -1L)) -> reduce (Neg v) (* Before eval so INT_MIN/-1 wraps. *)
    | Div (Int (T x), Int (T y)) -> const (x / y)
    | Div (Int i, Int (T 1L)) -> reduce i
    | Div (v, Int (T 1L)) -> v
    | Mod (Int (T x), Int (T y)) -> const (x % y)
    | Mod (_, Int (T 1L)) -> const 0L
    | Mod (Var x, Var y) when Ident.equal x y -> const 0L
    | Neg (Int (T x)) -> const (-x)
    | Neg (Int (Neg (Int x))) -> reduce x
    | Neg (Int (Neg v)) -> v
    | (T _ | Add _ | Sub _ | Mul _ | Div _ | Mod _ | Neg _) as expr -> Int expr
  ;;

  exception Divide_by_zero of t
  exception Negative_modulus of t

  let add a b = reduce (Add (a, b))
  let sub a b = reduce (Sub (a, b))
  let mul a b = reduce (Mul (a, b))
  let neg v = reduce (Neg v)

  let div a (b : value) =
    match a, b with
    | Bottom, _ | _, Bottom -> Bottom
    | _, Int (T 0L) -> raise (Divide_by_zero (Div (a, b)))
    | _ -> reduce (Div (a, b))
  ;;

  let mod_ a (b : value) =
    match a, b with
    | Bottom, _ | _, Bottom -> Bottom
    | _, Int (T 0L) -> raise (Divide_by_zero (Mod (a, b)))
    | _, Int (T n) when Int64.is_negative n -> raise (Negative_modulus (Mod (a, b)))
    | _ -> reduce (Mod (a, b))
  ;;
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
    | Tuple of value Nonempty_list.t
  [@@deriving sexp]

  let arg : value -> value = function
    | Type (Arrow { arg_ty; _ } | Pi { arg_ty; _ }) -> arg_ty
    | _ -> raise_s [%message "Bug: expected function type"]
  ;;

  let arg_mode : value -> Modes.t = function
    | Type (Arrow { arg_mode; _ } | Pi { arg_mode; _ }) -> arg_mode
    | _ -> raise_s [%message "Bug: expected function type"]
  ;;

  let ret : value -> value = function
    | Type (Arrow { ret_ty; _ }) -> ret_ty
    | _ -> raise_s [%message "Bug: expected arrow type"]
  ;;

  let ret_mode : value -> Modes.t = function
    | Type (Arrow { ret_mode; _ }) -> ret_mode
    | _ -> raise_s [%message "Bug: expected arrow type"]
  ;;

  let of_literal : Dst.Literal.t -> t = function
    | Unit -> Unit
    | Bool _ -> Bool
    | Int _ -> Int
  ;;
end

module Value = struct
  module Concrete = Concrete

  type t = value =
    | Bottom
    | Unit
    | Bool of vbool
    | Int of vint
    | Type of ty
    | Closure of closure
    | Binder of binder
    | Var of Ident.t
    | Tuple of t Nonempty_list.t
    | Apply of
        { fn : t
        ; arg : t
        }
    | Proj of
        { tuple : t
        ; index : int
        }
    | Match of
        { scrutinee : t
        ; arms : (Dst.Expr.pattern * t) Nonempty_list.t
        }
    | External of
        { symbol : string
        ; ty : t
        }
    | Prim of Builtin0.Prim.t
  [@@deriving sexp]

  let ty = function
    | Type ty -> ty
    | value -> raise_s [%message "Bug: expected concrete type" (value : t)]
  ;;

  module Matched = struct
    type t =
      | Match of (Ident.t * int list) list
      | No_match
      | Unknown
  end

  (* Values are reduced on construction, so every value is in normal form. *)

  let bottom = Bottom
  let unit = Unit
  let type_ ty = Type ty
  let closure closure = Closure closure
  let binder binder = Binder binder
  let var id = Var id
  let prim prim = Prim prim
  let external_ ~symbol ~ty = External { symbol; ty }

  let tuple elts =
    (* A tuple with an unreachable component is unreachable. *)
    if
      Nonempty_list.exists elts ~f:(function
        | Bottom -> true
        | _ -> false)
    then Bottom
    else Tuple elts
  ;;

  let proj tuple index =
    match tuple with
    | Tuple elts -> Nonempty_list.nth_exn elts index
    | Bottom -> Bottom
    | tuple -> Proj { tuple; index }
  ;;

  let apply ~fn ~arg =
    match fn, arg with
    | Bottom, _ | _, Bottom -> Bottom
    | fn, arg -> Apply { fn; arg }
  ;;

  let rec matches_pattern value pattern : Matched.t = match_at [] value pattern

  and match_at path value (pattern : Dst.Expr.pattern) : Matched.t =
    match pattern with
    | Var { id; _ } -> Match (if Ident.is_anon id then [] else [ id, List.rev path ])
    | Literal { value = literal; _ } ->
      (match literal, value with
       | Unit, _ (* We know value has unit type. *) -> Match []
       | Bool want, Bool (T got) -> if Core.Bool.equal got want then Match [] else No_match
       | Int want, Int (T got) -> if Int64.equal got want then Match [] else No_match
       | (Bool _ | Int _), _ -> Unknown)
    | Tuple { elts; _ } ->
      Nonempty_list.to_list elts
      |> List.foldi ~init:(Matched.Match []) ~f:(fun index acc elt ->
        let matched = match_at (index :: path) (proj value index) elt in
        match acc, matched with
        | No_match, _ | _, No_match -> Matched.No_match
        | Unknown, _ | _, Unknown -> Unknown
        | Match bindings, Match elt_bindings -> Match (bindings @ elt_bindings))
    | Or { left; right; _ } ->
      (match match_at path value left with
       | (Match _ | Unknown) as matched -> matched
       | No_match -> match_at path value right)
  ;;

  let match_ ~scrutinee ~arms : t =
    match scrutinee with
    | Bottom -> Bottom
    | scrutinee ->
      (* Arms with [Bottom] bodies are provably dead. *)
      let arms =
        Nonempty_list.to_list arms
        |> List.filter ~f:(fun (_, leaf) ->
          match leaf with
          | Bottom -> false
          | _ -> true)
      in
      (* Matches are exhaustive, so a sole surviving arm is unconditional. *)
      let rec select = function
        | [] -> Some (Bottom : t)
        | [ (_, leaf) ] -> Some leaf
        | (pattern, leaf) :: rest ->
          (match matches_pattern scrutinee pattern with
           | Matched.Match _ -> Some leaf
           | No_match -> select rest
           | Unknown -> None)
      in
      (match select arms with
       | Some value -> value
       | None -> Match { scrutinee; arms = Nonempty_list.of_list_exn arms })
  ;;

  let if_ ~loc ~cond ~then_ ~else_ : t =
    match_
      ~scrutinee:cond
      ~arms:
        (Nonempty_list.create
           ((Literal { value = Bool true; loc } : Dst.Expr.pattern), then_)
           [ (Literal { value = Bool false; loc } : Dst.Expr.pattern), else_ ])
  ;;

  let arms_unify = arms_unify
  let merge_arms = merge_arms

  let of_literal : Dst.Literal.t -> t = function
    | Unit -> Unit
    | Bool b -> Bool (T b)
    | Int i -> Int (T i)
  ;;
end

module Dependent = struct
  type t = dependent =
    | T of
        { ty : value
        ; memo : ((concrete, value) Hashtbl.t[@sexp.opaque])
        }
    | Meet of t * t
    | Join of t * t
    | Reduce of
        { env : (env[@sexp.opaque])
        ; arg : Ident.t
        ; arg_ty : value
        ; arg_mode : Modes.t
        ; memo : ((concrete, value) Hashtbl.t[@sexp.opaque])
        ; ret_ty : Dst.Expr.t
        }
    | Typecheck of
        { env : (env[@sexp.opaque])
        ; arg : Ident.t
        ; arg_ty : value
        ; arg_mode : Modes.t
        ; memo : ((concrete, value) Hashtbl.t[@sexp.opaque])
        ; body : Dst.Expr.t
        }
  [@@deriving sexp]

  let mono = mono

  let join a b =
    Option.map (join_concrete_dependent a b) ~f:mono |> Option.value ~default:(Join (a, b))
  ;;

  let meet a b =
    Option.map (meet_concrete_dependent a b) ~f:mono |> Option.value ~default:(Meet (a, b))
  ;;

  let typecheck ty ~env ~arg ~arg_ty ~arg_mode ~body =
    if is_concrete_value ty
    then mono ty
    else
      Typecheck { env; arg; arg_ty; arg_mode; memo = Hashtbl.create (module Value.Concrete); body }
  ;;

  let reduce ty ~env ~arg ~arg_ty ~arg_mode ~ret_ty =
    if is_concrete_value ty
    then mono ty
    else
      Reduce { env; arg; arg_ty; arg_mode; memo = Hashtbl.create (module Value.Concrete); ret_ty }
  ;;
end

module Expr = struct
  type nonrec fun_ = fun_ =
    | Lambda of
        { var : Ident.t
        ; arg : Ident.t
        ; body : expr
        ; ty : value
        ; mode : Modes.t
        ; family : (int[@sexp.opaque])
        ; loc : Lex.Location.t
        }
    | Binder of
        { var : Ident.t
        ; arg : Ident.t
        ; body : expr Concrete.Map.t
        ; ty : value
        ; mode : Modes.t
        ; family : (int[@sexp.opaque])
        ; loc : Lex.Location.t
        }
  [@@deriving sexp]

  type nonrec case = case =
    { bindings : value Ident.Map.t
    ; body : expr
    }
  [@@deriving sexp]

  type nonrec tree = tree =
    | Leaf of
        { case : int
        ; bindings : expr Ident.Map.t
        }
    | Split of
        { cond : expr
        ; then_ : tree
        ; else_ : tree
        }
  [@@deriving sexp]

  type nonrec target = target =
    | Family of (int[@sexp.opaque])
    | Prim of Builtin0.Prim.t
  [@@deriving sexp]

  type t = expr =
    | Erased of
        { ty : value
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | Literal of
        { value : value
        ; ty : value
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | Fun of
        { funs : fun_ Nonempty_list.t
        ; rest : t
        ; ty : value
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | Lambda of
        { arg : Ident.t
        ; body : t
        ; ty : value
        ; mode : Modes.t
        ; family : (int[@sexp.opaque])
        ; loc : Lex.Location.t
        }
    | Binder of
        { arg : Ident.t
        ; body : expr Concrete.Map.t
        ; ty : value
        ; mode : Modes.t
        ; family : (int[@sexp.opaque])
        ; loc : Lex.Location.t
        }
    | Apply of
        { fn : t
        ; arg : t
        ; ty : value
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | Specialize of
        { fn : expr
        ; arg : expr
        ; target : target
        ; key : concrete option
        ; ty : value
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | Let of
        { var : Ident.t
        ; bind : t
        ; rest : t
        ; ty : value
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | Tuple of
        { elts : t Nonempty_list.t
        ; ty : value
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | Tuple_get of
        { tuple : t
        ; index : int
        ; ty : value
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | If of
        { cond : t
        ; then_ : t
        ; else_ : t
        ; ty : value
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | Match of
        { cases : case Nonempty_list.t
        ; tree : tree
        ; ty : value
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | Var of
        { id : Ident.t
        ; ty : value
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | Builtin of
        { builtin : Builtin0.t
        ; ty : value
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | Extcall of
        { symbol : string
        ; arg : t
        ; ty : value
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
  [@@deriving sexp]

  (* [monos] overrides what a [Binder] node binds: typecheck emits binder
     nodes with empty bodies (monos accumulate program-wide), so pre-reify
     callers supply the family's monos from the store; post-reify the node's
     own body is correct. *)
  let rec free_vars ?monos (expr : t) : Ident.Set.t =
    match expr with
    | Erased _ | Literal _ | Builtin _ -> Ident.Set.empty
    | Extcall { arg; _ } -> free_vars ?monos arg
    | Tuple_get { tuple; _ } -> free_vars ?monos tuple
    | Var { id; _ } -> Ident.Set.singleton id
    | Tuple { elts; _ } ->
      Nonempty_list.map elts ~f:(free_vars ?monos) |> Nonempty_list.to_list |> Ident.Set.union_list
    | Apply { fn; arg; _ } | Specialize { fn; arg; _ } ->
      Set.union (free_vars ?monos fn) (free_vars ?monos arg)
    | If { cond; then_; else_; _ } ->
      Ident.Set.union_list [ free_vars ?monos cond; free_vars ?monos then_; free_vars ?monos else_ ]
    | Match { cases; tree; _ } ->
      let cases =
        Nonempty_list.map cases ~f:(fun { bindings; body } ->
          Set.diff (free_vars ?monos body) (Map.key_set bindings))
        |> Nonempty_list.to_array
      in
      free_vars_tree ?monos tree cases
    | Let { var; bind; rest; _ } ->
      Set.union (free_vars ?monos bind) (Set.remove (free_vars ?monos rest) var)
    | Lambda { arg; body; _ } -> Set.remove (free_vars ?monos body) arg
    | Binder { arg; body; family; _ } -> binder_free_vars ?monos ~arg ~family body
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
          | Lambda { arg; body; _ } -> Set.union acc (Set.remove (free_vars ?monos body) arg)
          | Binder { arg; body; family; _ } ->
            Set.union acc (binder_free_vars ?monos ~arg ~family body))
      in
      Set.diff (Set.union fvs_in_funs (free_vars ?monos rest)) bound_ids

  and binder_free_vars ?monos ~arg ~family body =
    let body =
      match monos with
      | Some monos -> monos family
      | None -> body
    in
    Map.fold body ~init:Ident.Set.empty ~f:(fun ~key:_ ~data:body acc ->
      Set.union acc (Set.remove (free_vars ?monos body) arg))

  and free_vars_tree ?monos (tree : tree) cases =
    match tree with
    | Leaf { case; bindings } ->
      Map.fold bindings ~init:cases.(case) ~f:(fun ~key:_ ~data:body acc ->
        Set.union acc (free_vars ?monos body))
    | Split { cond; then_; else_ } ->
      Ident.Set.union_list
        [ free_vars ?monos cond
        ; free_vars_tree ?monos then_ cases
        ; free_vars_tree ?monos else_ cases
        ]
  ;;

  let ty = function
    | Erased { ty; _ }
    | Literal { ty; _ }
    | Apply { ty; _ }
    | Tuple { ty; _ }
    | If { ty; _ }
    | Match { ty; _ }
    | Var { ty; _ }
    | Lambda { ty; _ }
    | Binder { ty; _ }
    | Let { ty; _ }
    | Fun { ty; _ }
    | Specialize { ty; _ }
    | Builtin { ty; _ }
    | Tuple_get { ty; _ }
    | Extcall { ty; _ } -> ty
  ;;

  let mode = function
    | Erased { mode; _ }
    | Literal { mode; _ }
    | Apply { mode; _ }
    | Tuple { mode; _ }
    | If { mode; _ }
    | Match { mode; _ }
    | Var { mode; _ }
    | Lambda { mode; _ }
    | Binder { mode; _ }
    | Let { mode; _ }
    | Fun { mode; _ }
    | Specialize { mode; _ }
    | Builtin { mode; _ }
    | Tuple_get { mode; _ }
    | Extcall { mode; _ } -> mode
  ;;

  let loc = function
    | Erased { loc; _ }
    | Literal { loc; _ }
    | Apply { loc; _ }
    | Tuple { loc; _ }
    | If { loc; _ }
    | Match { loc; _ }
    | Var { loc; _ }
    | Lambda { loc; _ }
    | Binder { loc; _ }
    | Let { loc; _ }
    | Fun { loc; _ }
    | Specialize { loc; _ }
    | Builtin { loc; _ }
    | Tuple_get { loc; _ }
    | Extcall { loc; _ } -> loc
  ;;

  let desc t static = { ty = ty t; mode = mode t; static }

  let with_ t ~ty ~mode =
    match t with
    | Erased t -> Erased { t with ty; mode }
    | Literal t -> Literal { t with ty; mode }
    | Apply t -> Apply { t with ty; mode }
    | Tuple t -> Tuple { t with ty; mode }
    | If t -> If { t with ty; mode }
    | Match t -> Match { t with ty; mode }
    | Var t -> Var { t with ty; mode }
    | Lambda t -> Lambda { t with ty; mode }
    | Binder t -> Binder { t with ty; mode }
    | Let t -> Let { t with ty; mode }
    | Fun t -> Fun { t with ty; mode }
    | Specialize t -> Specialize { t with ty; mode }
    | Builtin t -> Builtin { t with ty; mode }
    | Tuple_get t -> Tuple_get { t with ty; mode }
    | Extcall t -> Extcall { t with ty; mode }
  ;;

  let with_ty t ty =
    match t with
    | Erased t -> Erased { t with ty }
    | Literal t -> Literal { t with ty }
    | Apply t -> Apply { t with ty }
    | Tuple t -> Tuple { t with ty }
    | If t -> If { t with ty }
    | Match t -> Match { t with ty }
    | Var t -> Var { t with ty }
    | Lambda t -> Lambda { t with ty }
    | Binder t -> Binder { t with ty }
    | Let t -> Let { t with ty }
    | Fun t -> Fun { t with ty }
    | Specialize t -> Specialize { t with ty }
    | Builtin t -> Builtin { t with ty }
    | Tuple_get t -> Tuple_get { t with ty }
    | Extcall t -> Extcall { t with ty }
  ;;

  let with_mode t mode =
    match t with
    | Erased t -> Erased { t with mode }
    | Literal t -> Literal { t with mode }
    | Apply t -> Apply { t with mode }
    | Tuple t -> Tuple { t with mode }
    | If t -> If { t with mode }
    | Match t -> Match { t with mode }
    | Var t -> Var { t with mode }
    | Lambda t -> Lambda { t with mode }
    | Binder t -> Binder { t with mode }
    | Let t -> Let { t with mode }
    | Fun t -> Fun { t with mode }
    | Specialize t -> Specialize { t with mode }
    | Builtin t -> Builtin { t with mode }
    | Tuple_get t -> Tuple_get { t with mode }
    | Extcall t -> Extcall { t with mode }
  ;;

  let literal ~loc value =
    let ty = Ty.of_literal value in
    let mode = Modes.create ~staticity:Static ~erasure:Erased in
    Literal { value = Value.of_literal value; ty = Type ty; mode; loc }
  ;;

  let rebind bind ~stamp ~f =
    let loc = loc bind in
    let var = Ident.create Ident.Raw.anon ~stamp in
    let ref = Var { id = var; ty = ty bind; mode = mode bind; loc } in
    let rest = f ref in
    Let { var; bind; rest; ty = ty rest; mode = mode rest; loc }
  ;;

  let tuple ~loc (elts : (t * desc) Nonempty_list.t) : t * desc =
    let exprs, descs = Nonempty_list.unzip elts in
    let ty = Value.Type (Tuple (Nonempty_list.map descs ~f:(fun (d : desc) -> d.ty))) in
    let mode =
      Nonempty_list.fold descs ~init:(Modes.bottom ()) ~f:(fun acc (d : desc) ->
        Modes.join acc d.mode)
    in
    let static =
      Nonempty_list.map descs ~f:(fun (d : desc) -> d.static)
      |> Nonempty_list.to_list
      |> Lazy.all
      |> Lazy.map ~f:(fun elts -> Value.tuple (Nonempty_list.of_list_exn elts))
    in
    Tuple { elts = exprs; ty; mode; loc }, { ty; mode; static }
  ;;

  (* Project a runtime component out of the scrutinee, tracking its type and
     static value. *)
  let project ~loc scrutinee (scrutinee_desc : desc) path =
    let rec aux path expr (desc : desc) =
      match path with
      | [] -> expr, desc
      | index :: rest ->
        (match desc.ty with
         | Type (Tuple elt_tys) ->
           let ty = Nonempty_list.nth_exn elt_tys index in
           let get = Tuple_get { tuple = expr; index; ty; mode = desc.mode; loc } in
           let static = Lazy.map desc.static ~f:(fun tuple -> Value.proj tuple index) in
           aux rest get { ty; mode = desc.mode; static }
         | ty -> raise_s [%message "Bug: expected tuple" (ty : value) (loc : Lex.Location.t)])
    in
    aux path scrutinee scrutinee_desc
  ;;
end

module Env = struct
  type t = env [@@deriving sexp]

  let bind t id value = if Ident.is_anon id then t else Map.set t ~key:id ~data:value
  let find t id = Map.find t id
  let find_exn t id = Map.find_exn t id
  let initial = Ident.Map.empty
end

module Top_level = struct
  type t =
    | Erased of { loc : Lex.Location.t }
    | Let of
        { var : Ident.t
        ; bind : Expr.t
        ; loc : Lex.Location.t
        }
    | Fun of
        { funs : Expr.fun_ Nonempty_list.t
        ; loc : Lex.Location.t
        }
    | External of
        { var : Ident.t
        ; symbol : string
        ; ty : Value.t
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | Builtin of
        { var : Ident.t
        ; builtin : Builtin0.t
        ; ty : Value.t
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
  [@@deriving sexp]
end

module Program = struct
  type t =
    { top_levels : Top_level.t list
    ; stamp : int
    }
  [@@deriving sexp]
end
