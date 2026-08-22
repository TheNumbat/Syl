open! Core

let seed = 42

let uid =
  let next = ref 0 in
  fun () ->
    let uid = !next in
    Core.Int.incr next;
    uid
;;

module Kind = struct
  type t =
    | Abstract
    | Speculative
    | Reducing
    | Instancing
  [@@deriving sexp_of, compare]

  let join a b = if compare a b >= 0 then a else b
end

module Canon = struct
  type t =
    | Wild
    | Literal of Dst.Literal.t
    | Tuple of t Nonempty_list.t
    | Constructor of
        { label : Ident.Label.t
        ; payload : t option
        }
    | Ref of t
    | Or of t * t
  [@@deriving sexp_of, equal, hash]

  let rec of_pattern : Dst.Expr.pattern -> t = function
    | Var _ -> Wild
    | Literal { value; _ } -> Literal value
    | Tuple { elts; _ } -> Tuple (Nonempty_list.map elts ~f:of_pattern)
    | Constructor { label; payload; _ } ->
      Constructor { label; payload = Option.map payload ~f:of_pattern }
    | Or { left; right; _ } -> Or (of_pattern left, of_pattern right)
    | Ref { payload; _ } -> Ref (of_pattern payload)
  ;;
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
  | Variant of value option Map.M(Ident.Label).t
  | Ref of value
[@@deriving sexp_of]

and dependent =
  | T of
      { ty : value
      ; memo : ((value, value) Hashtbl.t[@sexp.opaque])
      }
  | Reduce of
      { env : (env[@sexp.opaque])
      ; arg : Ident.t
      ; arg_ty : value
      ; arg_mode : Modes.t
      ; ret_ty : Dst.Expr.t
      ; memo : ((value, value) Hashtbl.t[@sexp.opaque])
      ; uid : (int[@sexp.opaque])
      }
  | Typecheck of
      { env : (env[@sexp.opaque])
      ; arg : Ident.t
      ; arg_ty : value
      ; arg_mode : Modes.t
      ; body : Dst.Expr.t
      ; memo : ((value, value) Hashtbl.t[@sexp.opaque])
      ; uid : (int[@sexp.opaque])
      }
[@@deriving sexp_of]

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
[@@deriving sexp_of]

and vint =
  | T of int64
  | Add of value * value
  | Sub of value * value
  | Mul of value * value
  | Div of value * value
  | Mod of value * value
  | Neg of value
[@@deriving sexp_of]

and value = node Hashcons.t

and node =
  | Bottom
  | Unit
  | Bool of vbool
  | Int of vint
  | Type of ty
  | Closure of closure
  | Binder of binder
  | Var of Ident.t
  | Tuple of value Nonempty_list.t
  | Inject of
      { label : Ident.Label.t
      ; ty : value
      }
  | Constructor of
      { label : Ident.Label.t
      ; payload : value option
      }
  | Apply of
      { fn : value
      ; arg : value
      }
  | Proj of
      { tuple : value
      ; index : int
      }
  | Payload of
      { variant : value
      ; label : Ident.Label.t
      }
  | Match of
      { scrutinee : value
      ; arms : (Canon.t * value) Nonempty_list.t
      }
  | External of
      { symbol : string
      ; ty : value
      }
  | Box of value
  | Deref of value
  | Prim of Builtin0.Prim.t
[@@deriving sexp_of]

and closure =
  { arg : Ident.t
  ; ty : value
  ; body : (expr Lazy.t[@sexp.opaque])
  ; body_dst : Dst.Expr.t
  ; env : (env[@sexp.opaque])
  ; family : (int[@sexp.opaque])
  ; uid : (int[@sexp.opaque])
  }
[@@deriving sexp_of]

and binder =
  { arg : Ident.t
  ; ty : value
  ; body_dst : Dst.Expr.t
  ; env : (env[@sexp.opaque])
  ; family : (int[@sexp.opaque])
  ; uid : (int[@sexp.opaque])
  }
[@@deriving sexp_of]

and desc =
  { ty : value
  ; mode : Modes.t
  ; static : value Lazy.t
  }
[@@deriving sexp_of]

and fact =
  { target : value
  ; replacement : value
  }
[@@deriving sexp_of]

and binding =
  { desc : desc
  ; level : int (* Only newer facts apply *)
  ; mutable cache : ((fact list * desc) option[@sexp.opaque])
  }
[@@deriving sexp_of]

and env =
  { bindings : binding Ident.Map.t
  ; facts : fact list
  ; level : int
  ; kind : Kind.t
  }

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
      ; body : expr Core.Int.Map.t
      ; ty : value
      ; mode : Modes.t
      ; family : (int[@sexp.opaque])
      ; loc : Lex.Location.t
      }
[@@deriving sexp_of]

and case =
  { bindings : value Ident.Map.t
  ; body : expr
  }
[@@deriving sexp_of]

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
[@@deriving sexp_of]

and target =
  | Family of (int[@sexp.opaque])
  | Prim of Builtin0.Prim.t
[@@deriving sexp_of]

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
      ; body : expr Core.Int.Map.t
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
      ; key : value option
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
  | Payload_get of
      { variant : expr
      ; label : Ident.Label.t
      ; ty : value
      ; mode : Modes.t
      ; loc : Lex.Location.t
      }
  | Tag_test of
      { variant : expr
      ; label : Ident.Label.t
      ; ty : value
      ; mode : Modes.t
      ; loc : Lex.Location.t
      }
  | Make_ref of
      { payload : expr
      ; ty : value
      ; mode : Modes.t
      ; loc : Lex.Location.t
      }
  | Ref_get of
      { ref : expr
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
[@@deriving sexp_of]

let equal_value = Hashcons.equal

let equal_vbool (x : vbool) (y : vbool) =
  match x, y with
  | T x, T y -> Bool.equal x y
  | And (xa, xb), And (ya, yb)
  | Or (xa, xb), Or (ya, yb)
  | Eq (xa, xb), Eq (ya, yb)
  | Neq (xa, xb), Neq (ya, yb)
  | Lt (xa, xb), Lt (ya, yb)
  | Lte (xa, xb), Lte (ya, yb)
  | Gt (xa, xb), Gt (ya, yb)
  | Gte (xa, xb), Gte (ya, yb) -> equal_value xa ya && equal_value xb yb
  | Not x, Not y -> equal_value x y
  | (T _ | And _ | Or _ | Eq _ | Neq _ | Lt _ | Lte _ | Gt _ | Gte _ | Not _), _ -> false
;;

let equal_vint (x : vint) (y : vint) =
  match x, y with
  | T x, T y -> Int64.equal x y
  | Add (xa, xb), Add (ya, yb)
  | Sub (xa, xb), Sub (ya, yb)
  | Mul (xa, xb), Mul (ya, yb)
  | Div (xa, xb), Div (ya, yb)
  | Mod (xa, xb), Mod (ya, yb) -> equal_value xa ya && equal_value xb yb
  | Neg x, Neg y -> equal_value x y
  | (T _ | Add _ | Sub _ | Mul _ | Div _ | Mod _ | Neg _), _ -> false
;;

let equal_dependent (x : dependent) (y : dependent) =
  match x, y with
  | T { ty = xty; _ }, T { ty = yty; _ } -> equal_value xty yty
  | Reduce { uid = xuid; _ }, Reduce { uid = yuid; _ }
  | Typecheck { uid = xuid; _ }, Typecheck { uid = yuid; _ } -> xuid = yuid
  | (T _ | Reduce _ | Typecheck _), _ -> false
;;

let equal_ty (x : ty) (y : ty) =
  match x, y with
  | Unit, Unit | Bool, Bool | Int, Int | Type, Type -> true
  | Arrow x, Arrow y ->
    equal_value x.arg_ty y.arg_ty
    && Modes.equal x.arg_mode y.arg_mode
    && equal_value x.ret_ty y.ret_ty
    && Modes.equal x.ret_mode y.ret_mode
  | Pi x, Pi y ->
    equal_value x.arg_ty y.arg_ty
    && Modes.equal x.arg_mode y.arg_mode
    && equal_dependent x.ret_ty y.ret_ty
    && Modes.equal x.ret_mode y.ret_mode
  | Tuple x, Tuple y ->
    (match Nonempty_list.zip x y with
     | Ok elts -> Nonempty_list.for_all elts ~f:(fun (x, y) -> equal_value x y)
     | Unequal_lengths -> false)
  | Variant x, Variant y ->
    with_return (fun { return } ->
      Map.iter2 x y ~f:(fun ~key:_ ~data:payload ->
        match payload with
        | `Left _ | `Right _ -> return false
        | `Both (xpayload, ypayload) ->
          if not (Option.equal equal_value xpayload ypayload) then return false);
      true)
  | Ref x, Ref y -> equal_value x y
  | (Unit | Bool | Int | Type | Arrow _ | Pi _ | Tuple _ | Variant _ | Ref _), _ -> false
;;

let equal_node (x : node) (y : node) =
  match x, y with
  | Bottom, Bottom -> true
  | Unit, Unit -> true
  | Bool x, Bool y -> equal_vbool x y
  | Int x, Int y -> equal_vint x y
  | Type x, Type y -> equal_ty x y
  | Closure x, Closure y -> x.uid = y.uid
  | Binder x, Binder y -> x.uid = y.uid
  | Var x, Var y -> Ident.equal x y
  | Tuple x, Tuple y ->
    (match Nonempty_list.zip x y with
     | Ok elts -> Nonempty_list.for_all elts ~f:(fun (x, y) -> equal_value x y)
     | Unequal_lengths -> false)
  | Inject x, Inject y -> Ident.Label.equal x.label y.label && equal_value x.ty y.ty
  | Constructor x, Constructor y ->
    Ident.Label.equal x.label y.label && Option.equal equal_value x.payload y.payload
  | Apply x, Apply y -> equal_value x.fn y.fn && equal_value x.arg y.arg
  | Proj x, Proj y -> equal_value x.tuple y.tuple && x.index = y.index
  | Payload x, Payload y -> equal_value x.variant y.variant && Ident.Label.equal x.label y.label
  | External x, External y -> String.equal x.symbol y.symbol && equal_value x.ty y.ty
  | Prim x, Prim y -> Builtin0.Prim.equal x y
  | Box x, Box y -> equal_value x y
  | Deref x, Deref y -> equal_value x y
  | Match x, Match y ->
    equal_value x.scrutinee y.scrutinee
    &&
      (match Nonempty_list.zip x.arms y.arms with
      | Ok arms ->
        Nonempty_list.for_all arms ~f:(fun ((xpattern, x), (ypattern, y)) ->
          Canon.equal xpattern ypattern && equal_value x y)
      | Unequal_lengths -> false)
  | ( ( Bottom
      | Unit
      | Bool _
      | Int _
      | Type _
      | Closure _
      | Binder _
      | Var _
      | Tuple _
      | Inject _
      | Constructor _
      | Apply _
      | Proj _
      | Payload _
      | Match _
      | External _
      | Box _
      | Deref _
      | Prim _ )
    , _ ) -> false
;;

let hash_fold_value = Hashcons.hash_fold_t

let hash_fold_vbool s (x : vbool) =
  match x with
  | T b -> hash_fold_int (hash_fold_bool s b) 0
  | And (a, b) -> hash_fold_int (hash_fold_value (hash_fold_value s a) b) 1
  | Or (a, b) -> hash_fold_int (hash_fold_value (hash_fold_value s a) b) 2
  | Eq (a, b) -> hash_fold_int (hash_fold_value (hash_fold_value s a) b) 3
  | Neq (a, b) -> hash_fold_int (hash_fold_value (hash_fold_value s a) b) 4
  | Lt (a, b) -> hash_fold_int (hash_fold_value (hash_fold_value s a) b) 5
  | Lte (a, b) -> hash_fold_int (hash_fold_value (hash_fold_value s a) b) 6
  | Gt (a, b) -> hash_fold_int (hash_fold_value (hash_fold_value s a) b) 7
  | Gte (a, b) -> hash_fold_int (hash_fold_value (hash_fold_value s a) b) 8
  | Not a -> hash_fold_int (hash_fold_value s a) 9
;;

let hash_fold_vint s (x : vint) =
  match x with
  | T i -> hash_fold_int (hash_fold_int64 s i) 0
  | Add (a, b) -> hash_fold_int (hash_fold_value (hash_fold_value s a) b) 1
  | Sub (a, b) -> hash_fold_int (hash_fold_value (hash_fold_value s a) b) 2
  | Mul (a, b) -> hash_fold_int (hash_fold_value (hash_fold_value s a) b) 3
  | Div (a, b) -> hash_fold_int (hash_fold_value (hash_fold_value s a) b) 4
  | Mod (a, b) -> hash_fold_int (hash_fold_value (hash_fold_value s a) b) 5
  | Neg a -> hash_fold_int (hash_fold_value s a) 6
;;

let hash_fold_dependent s (x : dependent) =
  match x with
  | T { ty; _ } -> hash_fold_int (hash_fold_value s ty) 0
  | Reduce { uid; _ } -> hash_fold_int (hash_fold_int s uid) 1
  | Typecheck { uid; _ } -> hash_fold_int (hash_fold_int s uid) 2
;;

let hash_fold_ty s (x : ty) =
  match x with
  | Unit -> hash_fold_int s 0
  | Bool -> hash_fold_int s 1
  | Int -> hash_fold_int s 2
  | Type -> hash_fold_int s 3
  | Arrow { arg_ty; arg_mode; ret_ty; ret_mode } ->
    hash_fold_int
      (Modes.hash_fold_t
         (hash_fold_value (Modes.hash_fold_t (hash_fold_value s arg_ty) arg_mode) ret_ty)
         ret_mode)
      4
  | Pi { arg_ty; arg_mode; ret_ty; ret_mode } ->
    hash_fold_int
      (Modes.hash_fold_t
         (hash_fold_dependent (Modes.hash_fold_t (hash_fold_value s arg_ty) arg_mode) ret_ty)
         ret_mode)
      5
  | Tuple elts ->
    hash_fold_int (Nonempty_list.fold elts ~init:s ~f:(fun s elt -> hash_fold_value s elt)) 6
  | Variant variants ->
    hash_fold_int
      (Map.fold variants ~init:s ~f:(fun ~key:label ~data:payload s ->
         let s = Ident.Label.hash_fold_t s label in
         hash_fold_option (fun s (payload : value) -> hash_fold_value s payload) s payload))
      7
  | Ref payload -> hash_fold_int (hash_fold_value s payload) 8
;;

let hash_fold_node s (x : node) =
  match x with
  | Bottom -> hash_fold_int s 1
  | Unit -> hash_fold_int s 2
  | Bool b -> hash_fold_int (hash_fold_vbool s b) 3
  | Int i -> hash_fold_int (hash_fold_vint s i) 4
  | Type ty -> hash_fold_int (hash_fold_ty s ty) 5
  | Closure closure -> hash_fold_int (hash_fold_int s closure.uid) 6
  | Binder binder -> hash_fold_int (hash_fold_int s binder.uid) 7
  | Var id -> hash_fold_int (Ident.hash_fold_t s id) 8
  | Tuple elts ->
    hash_fold_int (Nonempty_list.fold elts ~init:s ~f:(fun s elt -> hash_fold_value s elt)) 9
  | Inject { label; ty } -> hash_fold_int (hash_fold_value (Ident.Label.hash_fold_t s label) ty) 10
  | Constructor { label; payload } ->
    hash_fold_int
      (hash_fold_option
         (fun s (payload : value) -> hash_fold_value s payload)
         (Ident.Label.hash_fold_t s label)
         payload)
      11
  | Apply { fn; arg } -> hash_fold_int (hash_fold_value (hash_fold_value s fn) arg) 12
  | Proj { tuple; index } -> hash_fold_int (hash_fold_int (hash_fold_value s tuple) index) 13
  | Payload { variant; label } ->
    hash_fold_int (hash_fold_value (Ident.Label.hash_fold_t s label) variant) 14
  | Match { scrutinee; arms } ->
    hash_fold_int
      (Nonempty_list.fold arms ~init:(hash_fold_value s scrutinee) ~f:(fun s (pattern, leaf) ->
         hash_fold_value (Canon.hash_fold_t s pattern) leaf))
      15
  | External { symbol; ty } -> hash_fold_int (hash_fold_value (String.hash_fold_t s symbol) ty) 16
  | Prim prim -> hash_fold_int (Builtin0.Prim.hash_fold_t s prim) 17
  | Box payload -> hash_fold_int (hash_fold_value s payload) 18
  | Deref ref -> hash_fold_int (hash_fold_value s ref) 19
;;

module Value0 = struct
  module Key = struct
    type t = value [@@deriving sexp_of]

    let compare = Hashcons.compare
    let hash = Hashcons.hash
  end

  (* Values are reduced on construction, so every value is interned in normal form. *)
  module Table = Hashcons.Table (struct
      type t = node [@@deriving sexp_of]

      let equal = equal_node
      let hash_fold_t = hash_fold_node
      let hash = Hash.run ~seed hash_fold_t
    end)

  let intern =
    let table = Table.create () in
    fun node -> Table.intern table node
  ;;

  let bottom : value = intern Bottom
  let unit : value = intern Unit
  let type_ ty : value = intern (Type ty)
  let closure closure = intern (Closure closure)
  let binder binder = intern (Binder binder)
  let var id = intern (Var id)
  let prim prim = intern (Prim prim)
  let external_ ~symbol ~ty = intern (External { symbol; ty })

  let tuple (elts : value Nonempty_list.t) =
    (* A tuple with an unreachable component is unreachable. *)
    if
      Nonempty_list.exists elts ~f:(fun elt ->
        match elt.node with
        | Bottom -> true
        | _ -> false)
    then bottom
    else intern (Tuple elts)
  ;;

  let payload (value : value) ~label =
    match value.node with
    | Constructor { label = got; payload = got_payload } when Ident.Label.equal got label ->
      (match got_payload with
       | Some payload -> payload
       | None -> raise_s [%message "Bug: expected payload" (label : Ident.Label.t) (value : value)])
    | Bottom -> value
    | Var _ | Constructor _ | Apply _ | Proj _ | Payload _ | Match _ | Deref _ ->
      intern (Payload { variant = value; label })
    | Unit
    | Bool _
    | Int _
    | Type _
    | Closure _
    | Binder _
    | Tuple _
    | Inject _
    | External _
    | Box _
    | Prim _ -> raise_s [%message "Bug: expected variant" (label : Ident.Label.t) (value : value)]
  ;;

  let inject ~ty ~label = intern (Inject { ty; label })

  let constructor ~label ~(payload : value option) =
    (* A constructor with an unreachable payload is unreachable. *)
    match payload with
    | Some { node = Bottom; _ } -> bottom
    | payload -> intern (Constructor { label; payload })
  ;;

  let box (payload : value) =
    match payload.node with
    | Bottom -> bottom
    | _ -> intern (Box payload)
  ;;

  let deref (ref : value) =
    match ref.node with
    | Bottom -> bottom
    | Box payload -> payload
    | Var _ | Apply _ | Proj _ | Payload _ | Match _ | Deref _ -> intern (Deref ref)
    | Unit
    | Bool _
    | Int _
    | Type _
    | Closure _
    | Binder _
    | Tuple _
    | Inject _
    | Constructor _
    | External _
    | Prim _ -> raise_s [%message "Bug: expected ref" (ref : value)]
  ;;

  let apply ~(fn : value) ~(arg : value) =
    match fn.node, arg.node with
    | Bottom, _ | _, Bottom -> bottom
    | Inject { label; _ }, _ -> constructor ~label ~payload:(Some arg)
    | ( ( Closure _
        | Binder _
        | Var _
        | Apply _
        | Proj _
        | Payload _
        | Match _
        | External _
        | Deref _
        | Prim _ )
      , _ ) -> intern (Apply { fn; arg })
    | (Unit | Bool _ | Int _ | Type _ | Tuple _ | Constructor _ | Box _), _ ->
      raise_s [%message "Bug: expected function" (fn : value) (arg : value)]
  ;;

  let proj (tuple : value) index =
    match tuple.node with
    | Tuple elts -> Nonempty_list.nth_exn elts index
    | Bottom -> tuple
    | Var _ | Apply _ | Proj _ | Payload _ | Match _ | Deref _ -> intern (Proj { tuple; index })
    | Unit
    | Bool _
    | Int _
    | Type _
    | Closure _
    | Binder _
    | Inject _
    | Constructor _
    | External _
    | Box _
    | Prim _ -> raise_s [%message "Bug: expected tuple" (index : int) (tuple : value)]
  ;;
end

module Pattern = struct
  module Canon = Canon

  module Step = struct
    type t =
      | Index of int
      | Payload of Ident.Label.t
      | Deref
    [@@deriving sexp]
  end

  module Matched = struct
    type t =
      | Match
      | No_match
      | Unknown
    [@@deriving sexp]
  end

  let rec specialize (pattern : Canon.t) ~scrutinee : value =
    match pattern with
    | Wild -> scrutinee
    | Or _ ->
      (* Matching any alternative still implies the structure they all agree on;
         positions they disagree on stay the bare scrutinee position. *)
      specialize_alternatives (alternatives pattern) ~scrutinee
    | Literal Unit -> Value0.unit
    | Literal (Bool b) -> Value0.intern (Bool (T b))
    | Literal (Int i) -> Value0.intern (Int (T i))
    | Constructor { label; payload = payload_pattern } ->
      Value0.constructor
        ~label
        ~payload:
          (Option.map payload_pattern ~f:(fun pattern ->
             specialize pattern ~scrutinee:(Value0.payload scrutinee ~label)))
    | Tuple elts ->
      Value0.tuple
        (Nonempty_list.mapi elts ~f:(fun index pattern ->
           specialize pattern ~scrutinee:(Value0.proj scrutinee index)))
    | Ref pattern -> Value0.box (specialize pattern ~scrutinee:(Value0.deref scrutinee))

  and alternatives (pattern : Canon.t) : Canon.t list =
    match pattern with
    | Or (left, right) -> alternatives left @ alternatives right
    | pattern -> [ pattern ]

  and specialize_alternatives (patterns : Canon.t list) ~scrutinee : value =
    match List.concat_map patterns ~f:alternatives with
    | [] -> scrutinee
    | [ pattern ] -> specialize pattern ~scrutinee
    | first :: rest ->
      (match first with
       | Wild | Or _ -> scrutinee
       | Literal literal ->
         if
           List.for_all rest ~f:(function
             | Canon.Literal literal' -> Dst.Literal.equal literal literal'
             | Wild | Constructor _ | Tuple _ | Or _ | Ref _ -> false)
         then specialize first ~scrutinee
         else scrutinee
       | Ref payload ->
         (match
            List.map rest ~f:(function
              | Canon.Ref payload' -> Some payload'
              | Wild | Literal _ | Constructor _ | Tuple _ | Or _ -> None)
            |> Option.all
          with
          | None -> scrutinee
          | Some payloads ->
            Value0.box
              (specialize_alternatives (payload :: payloads) ~scrutinee:(Value0.deref scrutinee)))
       | Constructor { label; payload } ->
         (match
            List.map rest ~f:(function
              | Canon.Constructor { label = label'; payload = payload' }
                when Ident.Label.equal label label' -> Some payload'
              | Wild | Literal _ | Constructor _ | Tuple _ | Or _ | Ref _ -> None)
            |> Option.all
          with
          | None -> scrutinee
          | Some payloads ->
            (match payload, Option.all payloads with
             | Some payload, Some payloads ->
               Value0.constructor
                 ~label
                 ~payload:
                   (Some
                      (specialize_alternatives
                         (payload :: payloads)
                         ~scrutinee:(Value0.payload scrutinee ~label)))
             | None, _ when List.for_all payloads ~f:Option.is_none ->
               Value0.constructor ~label ~payload:None
             | (None | Some _), _ -> scrutinee))
       | Tuple elts ->
         let arity = Nonempty_list.length elts in
         (match
            List.map rest ~f:(function
              | Canon.Tuple elts' when Nonempty_list.length elts' = arity ->
                Some (Nonempty_list.to_list elts')
              | Wild | Literal _ | Constructor _ | Tuple _ | Or _ | Ref _ -> None)
            |> Option.all
          with
          | None -> scrutinee
          | Some rest_elts ->
            Value0.tuple
              (Nonempty_list.mapi elts ~f:(fun index elt ->
                 specialize_alternatives
                   (elt :: List.map rest_elts ~f:(fun elts -> List.nth_exn elts index))
                   ~scrutinee:(Value0.proj scrutinee index)))))
  ;;

  (* Whether a value definitely matches a canonical pattern. *)
  let rec matches (value : value) (pattern : Canon.t) : Matched.t =
    match pattern with
    | Wild -> Match
    | Literal literal ->
      (match literal, value.node with
       | Unit, _ -> Match
       | Bool want, Bool (T got) -> if Core.Bool.equal got want then Match else No_match
       | Int want, Int (T got) -> if Int64.equal got want then Match else No_match
       | (Bool _ | Int _), _ -> Unknown)
    | Tuple elts ->
      Nonempty_list.to_list elts
      |> List.foldi ~init:Matched.Match ~f:(fun index acc elt ->
        match acc, matches (Value0.proj value index) elt with
        | No_match, _ | _, No_match -> Matched.No_match
        | Unknown, _ | _, Unknown -> Unknown
        | Match, Match -> Match)
    | Constructor { label; payload } ->
      (match value.node with
       | Constructor { label = got; payload = got_payload } ->
         if not (Ident.Label.equal got label)
         then No_match
         else (
           match payload, got_payload with
           | None, None -> Match
           | Some payload, Some got_payload -> matches got_payload payload
           | Some _, None | None, Some _ ->
             raise_s [%message "Bug: constructor payload mismatch" (label : Ident.Label.t)])
       | _ -> Unknown)
    | Or (left, right) ->
      (match matches value left with
       | (Match | Unknown) as matched -> matched
       | No_match -> matches value right)
    | Ref pattern ->
      (match value.node with
       | Box payload -> matches payload pattern
       | _ -> Unknown)
  ;;

  (* Binding paths of a source pattern known to match: pure structure, except
     an or-pattern's paths follow the alternative the value selects. *)
  let rec bindings_at path (value : value) (pattern : Dst.Expr.pattern) =
    match pattern with
    | Var { id; _ } -> if Ident.is_anon id then [] else [ id, List.rev path ]
    | Literal _ -> []
    | Tuple { elts; _ } ->
      Nonempty_list.to_list elts
      |> List.concat_mapi ~f:(fun index elt ->
        bindings_at (Step.Index index :: path) (Value0.proj value index) elt)
    | Constructor { label; payload; _ } ->
      (match payload with
       | None -> []
       | Some payload ->
         let got =
           match value.node with
           | Constructor { payload = Some got; _ } -> got
           | _ -> Value0.payload value ~label
         in
         bindings_at (Step.Payload label :: path) got payload)
    | Or { left; right; _ } ->
      (match matches value (Canon.of_pattern left) with
       | Match -> bindings_at path value left
       | No_match -> bindings_at path value right
       | Unknown -> raise_s [%message "Bug: bindings of an undecided or pattern"])
    | Ref { payload; _ } -> bindings_at (Step.Deref :: path) (Value0.deref value) payload
  ;;

  let selects (scrutinee : value) patterns : _ Or_unknown.t =
    let rec aux index : _ -> _ Or_unknown.t = function
      | [] -> Unknown
      | pattern :: rest ->
        (match matches scrutinee (Canon.of_pattern pattern) with
         | Match -> Known (index, bindings_at [] scrutinee pattern)
         | No_match -> aux (index + 1) rest
         | Unknown -> Unknown)
    in
    match scrutinee.node with
    | Bottom -> Unknown
    | _ -> aux 0 (Nonempty_list.to_list patterns)
  ;;

  module World = struct
    type 'a t =
      { pattern : Dst.Expr.pattern
      ; positive : Dst.Expr.pattern
      ; speculative : bool
      ; body : 'a
      }

    let anon = Ident.create Ident.Raw.anon ~stamp:0
  end

  let wildcard_worlds ~unfold ~ty ~scrutinee ~earlier ~loc =
    let candidates =
      match (unfold ty : value).node with
      | Type (Variant constructors) ->
        Map.to_alist constructors
        |> List.map ~f:(fun (label, payload) ->
          ( Value0.constructor
              ~label
              ~payload:(Option.map payload ~f:(fun _ -> Value0.payload scrutinee ~label))
          , (Constructor
               { label
               ; payload =
                   Option.map payload ~f:(fun _ : Dst.Expr.pattern -> Var { id = World.anon; loc })
               ; loc
               }
             : Dst.Expr.pattern) ))
      | Type Bool ->
        List.map [ true; false ] ~f:(fun b ->
          Value0.intern (Bool (T b)), (Literal { value = Bool b; loc } : Dst.Expr.pattern))
      | _ -> []
    in
    List.filter_map candidates ~f:(fun (candidate, world) ->
      let captured =
        List.exists earlier ~f:(fun earlier ->
          match matches candidate (Canon.of_pattern earlier) with
          | Match -> true
          | No_match | Unknown -> false)
      in
      Option.some_if (not captured) world)
  ;;

  let worlds ~unfold ~ty ~scrutinee arms : _ World.t Nonempty_list.t =
    Nonempty_list.to_list arms
    |> List.folding_map ~init:[] ~f:(fun earlier ((pattern : Dst.Expr.pattern), body) ->
      let worlds =
        match pattern with
        | Var { loc; _ } -> wildcard_worlds ~unfold ~ty ~scrutinee ~earlier ~loc
        | Literal _ | Constructor _ | Tuple _ | Or _ | Ref _ -> []
      in
      let split =
        match worlds with
        | [] -> [ { World.pattern; positive = pattern; speculative = false; body } ]
        | worlds ->
          let speculative = List.length worlds > 1 in
          List.map worlds ~f:(fun positive -> { World.pattern; positive; speculative; body })
      in
      earlier @ [ pattern ], split)
    |> List.concat
    |> Nonempty_list.of_list_exn
  ;;

  let arms_agree a_arms b_arms =
    match Nonempty_list.zip a_arms b_arms with
    | Ok zip ->
      let zip = Nonempty_list.to_list zip in
      let last = List.length zip - 1 in
      List.for_alli zip ~f:(fun i ((a_pattern, _), (b_pattern, _)) ->
        i = last || Canon.equal a_pattern b_pattern)
    | Unequal_lengths -> false
  ;;

  let map2_arms a_arms b_arms ~f =
    if arms_agree a_arms b_arms
    then
      Nonempty_list.zip_exn a_arms b_arms
      |> Nonempty_list.map ~f:(fun ((pattern, a_leaf), (_, b_leaf)) ->
        Option.map (f a_leaf b_leaf) ~f:(fun leaf -> pattern, leaf))
      |> Nonempty_list.to_list
      |> Option.all
      |> Option.map ~f:Nonempty_list.of_list_exn
    else None
  ;;
end

module Closure = struct
  type t = closure =
    { arg : Ident.t
    ; ty : value
    ; body : (expr Lazy.t[@sexp.opaque])
    ; body_dst : Dst.Expr.t
    ; env : (env[@sexp.opaque])
    ; family : (int[@sexp.opaque])
    ; uid : (int[@sexp.opaque])
    }
  [@@deriving sexp_of]

  let equal x y = x.uid = y.uid
  let hash t = t.uid
  let hash_fold_t s t = hash_fold_int s t.uid

  let const ~arg ~ty ~body ~body_dst ~env ~family =
    { arg; ty; body; body_dst; env; family; uid = uid () }
  ;;
end

module Binder = struct
  type t = binder =
    { arg : Ident.t
    ; ty : value
    ; body_dst : Dst.Expr.t
    ; env : (env[@sexp.opaque])
    ; family : (int[@sexp.opaque])
    ; uid : (int[@sexp.opaque])
    }
  [@@deriving sexp_of]

  let equal x y = x.uid = y.uid
  let hash t = t.uid
  let hash_fold_t s t = hash_fold_int s t.uid
  let const ~arg ~ty ~body_dst ~env ~family = { arg; ty; body_dst; env; family; uid = uid () }
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
  [@@deriving sexp_of]

  let equal = equal_vbool
  let hash_fold_t = hash_fold_vbool
  let hash = Hash.run ~seed hash_fold_t
  let const b : value = Value0.intern (Bool (T b))

  let is_literal (v : value) =
    match v.node with
    | Bool (T _) | Int (T _) -> true
    | _ -> false
  ;;

  (* Normalize variable comparisons by comparison order. *)
  let reorder_vars (a : value) (b : value) =
    match a.node, b.node with
    | Var x, Var y -> Core.Int.( > ) (Ident.compare x y) 0
    | _ -> false
  ;;

  (* [shift o c] rewrites the comparison [o == c] into [v == c'] over the
     compound's variable side. *)
  let shift (o : value) (c : int64) =
    match o.node with
    | Int (Add (v, { node = Int (T k); _ })) | Int (Add ({ node = Int (T k); _ }, v)) ->
      Some (v, c - k)
    | Int (Sub (v, { node = Int (T k); _ })) -> Some (v, c + k)
    | Int (Sub ({ node = Int (T k); _ }, v)) -> Some (v, k - c)
    | Int (Neg v) -> Some (v, neg c)
    | _ -> None
  ;;

  (* Only does rewrites that remove terms. *)
  let rec reduce : t -> value = function
    | And ({ node = Bottom; _ }, _)
    | And (_, { node = Bottom; _ })
    | Or ({ node = Bottom; _ }, _)
    | Or (_, { node = Bottom; _ })
    | Eq ({ node = Bottom; _ }, _)
    | Eq (_, { node = Bottom; _ })
    | Neq ({ node = Bottom; _ }, _)
    | Neq (_, { node = Bottom; _ })
    | Lt ({ node = Bottom; _ }, _)
    | Lt (_, { node = Bottom; _ })
    | Lte ({ node = Bottom; _ }, _)
    | Lte (_, { node = Bottom; _ })
    | Gt ({ node = Bottom; _ }, _)
    | Gt (_, { node = Bottom; _ })
    | Gte ({ node = Bottom; _ }, _)
    | Gte (_, { node = Bottom; _ })
    | Not { node = Bottom; _ } -> Value0.bottom
    | And ({ node = Bool (T x); _ }, { node = Bool (T y); _ }) -> const (x && y)
    | And ({ node = Bool (T false); _ }, _) | And (_, { node = Bool (T false); _ }) -> const false
    | And ({ node = Var x; _ }, { node = Bool (Not { node = Var y; _ }); _ })
    | And ({ node = Bool (Not { node = Var x; _ }); _ }, { node = Var y; _ })
      when Ident.equal x y -> const false
    | And ({ node = Bool (T true); _ }, { node = Bool b; _ })
    | And ({ node = Bool b; _ }, { node = Bool (T true); _ }) -> reduce b
    | And ({ node = Bool (T true); _ }, v) | And (v, { node = Bool (T true); _ }) -> v
    | Or ({ node = Bool (T x); _ }, { node = Bool (T y); _ }) -> const (x || y)
    | Or ({ node = Bool (T true); _ }, _) | Or (_, { node = Bool (T true); _ }) -> const true
    | Or ({ node = Var x; _ }, { node = Bool (Not { node = Var y; _ }); _ })
    | Or ({ node = Bool (Not { node = Var x; _ }); _ }, { node = Var y; _ })
      when Ident.equal x y -> const true
    | Or ({ node = Bool (T false); _ }, { node = Bool b; _ })
    | Or ({ node = Bool b; _ }, { node = Bool (T false); _ }) -> reduce b
    | Or ({ node = Bool (T false); _ }, v) | Or (v, { node = Bool (T false); _ }) -> v
    | Not { node = Bool (T x); _ } -> const (not x)
    | Not { node = Bool (Not { node = Bool b; _ }); _ } -> reduce b
    | Not { node = Bool (Not v); _ } -> v
    | Eq ({ node = Int (T x); _ }, { node = Int (T y); _ }) -> const (x = y)
    | Eq (({ node = Bool (T _) | Int (T _); _ } as lit), value) when not (is_literal value) ->
      reduce (Eq (value, lit))
    | Eq (a, b) when reorder_vars a b -> reduce (Eq (b, a))
    | Eq (a, b) when equal_value a b -> const true
    | Eq (o, { node = Int (T c); _ }) when Option.is_some (shift o c) ->
      let v, c = Option.value_exn (shift o c) in
      reduce (Eq (v, Value0.intern (Int (T c))))
    | Neq ({ node = Int (T x); _ }, { node = Int (T y); _ }) -> const (x <> y)
    | Neq (({ node = Bool (T _) | Int (T _); _ } as lit), value) when not (is_literal value) ->
      reduce (Neq (value, lit))
    | Neq (a, b) when reorder_vars a b -> reduce (Neq (b, a))
    | Neq (a, b) when equal_value a b -> const false
    | Neq (o, { node = Int (T c); _ }) when Option.is_some (shift o c) ->
      let v, c = Option.value_exn (shift o c) in
      reduce (Neq (v, Value0.intern (Int (T c))))
    | Lt ({ node = Int (T x); _ }, { node = Int (T y); _ }) -> const (x < y)
    | Lt (a, b) when equal_value a b -> const false
    | Lte ({ node = Int (T x); _ }, { node = Int (T y); _ }) -> const (x <= y)
    | Lte (a, b) when equal_value a b -> const true
    | Gt ({ node = Int (T x); _ }, { node = Int (T y); _ }) -> const (x > y)
    | Gt (a, b) when equal_value a b -> const false
    | Gte ({ node = Int (T x); _ }, { node = Int (T y); _ }) -> const (x >= y)
    | Gte (a, b) when equal_value a b -> const true
    | (T _ | And _ | Or _ | Eq _ | Neq _ | Lt _ | Lte _ | Gt _ | Gte _ | Not _) as expr ->
      Value0.intern (Bool expr)

  and and_ a b = reduce (And (a, b))
  and or_ a b = reduce (Or (a, b))
  and eq a b = reduce (Eq (a, b))
  and neq a b = reduce (Neq (a, b))
  and lt a b = reduce (Lt (a, b))
  and lte a b = reduce (Lte (a, b))
  and gt a b = reduce (Gt (a, b))
  and gte a b = reduce (Gte (a, b))
  and not_ v = reduce (Not v)
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
  [@@deriving sexp_of]

  let equal = equal_vint
  let hash_fold_t = hash_fold_vint
  let hash = Hash.run ~seed hash_fold_t
  let const i : value = Value0.intern (Int (T i))

  let unwrap_neg (v : value) =
    match v.node with
    | Int (Neg w) -> Some w
    | _ -> None
  ;;

  (* [a + b] cancels: [a] is a var and [b] its negation. *)
  let cancels (a : value) b =
    match a.node, unwrap_neg b with
    | Var x, Some { node = Var y; _ } -> Ident.equal x y
    | _ -> false
  ;;

  (* Only does rewrites that remove terms. *)
  let rec reduce : t -> value = function
    | Add ({ node = Bottom; _ }, _)
    | Add (_, { node = Bottom; _ })
    | Sub ({ node = Bottom; _ }, _)
    | Sub (_, { node = Bottom; _ })
    | Mul ({ node = Bottom; _ }, _)
    | Mul (_, { node = Bottom; _ })
    | Div ({ node = Bottom; _ }, _)
    | Div (_, { node = Bottom; _ })
    | Mod ({ node = Bottom; _ }, _)
    | Mod (_, { node = Bottom; _ })
    | Neg { node = Bottom; _ } -> Value0.bottom
    | Add ({ node = Int (T x); _ }, { node = Int (T y); _ }) -> const (x + y)
    | Add (a, b) when cancels a b || cancels b a -> const 0L
    | Add ({ node = Int (T 0L); _ }, { node = Int i; _ })
    | Add ({ node = Int i; _ }, { node = Int (T 0L); _ }) -> reduce i
    | Add ({ node = Int (T 0L); _ }, v) | Add (v, { node = Int (T 0L); _ }) -> v
    | Sub ({ node = Int (T x); _ }, { node = Int (T y); _ }) -> const (x - y)
    | Sub ({ node = Int i; _ }, { node = Int (T 0L); _ }) -> reduce i
    | Sub (v, { node = Int (T 0L); _ }) -> v
    | Sub ({ node = Int (T 0L); _ }, v) -> reduce (Neg v)
    | Sub (a, b) when equal_value a b -> const 0L
    | Sub (x, y) when Option.is_some (unwrap_neg y) ->
      reduce (Add (x, Option.value_exn (unwrap_neg y)))
    | Mul ({ node = Int (T x); _ }, { node = Int (T y); _ }) -> const (x * y)
    | Mul (_, { node = Int (T 0L); _ }) | Mul ({ node = Int (T 0L); _ }, _) -> const 0L
    | Mul ({ node = Int i; _ }, { node = Int (T 1L); _ })
    | Mul ({ node = Int (T 1L); _ }, { node = Int i; _ }) -> reduce i
    | Mul (v, { node = Int (T -1L); _ }) | Mul ({ node = Int (T -1L); _ }, v) -> reduce (Neg v)
    | Mul (v, { node = Int (T 1L); _ }) | Mul ({ node = Int (T 1L); _ }, v) -> v
    | Div (v, { node = Int (T -1L); _ }) -> reduce (Neg v) (* INT_MIN/-1 wraps. *)
    | Div ({ node = Int (T x); _ }, { node = Int (T y); _ }) -> const (x / y)
    | Div ({ node = Int i; _ }, { node = Int (T 1L); _ }) -> reduce i
    | Div (v, { node = Int (T 1L); _ }) -> v
    | Mod ({ node = Int (T x); _ }, { node = Int (T y); _ }) -> const (x % y)
    | Mod (_, { node = Int (T 1L); _ }) -> const 0L
    (* No x % x -> 0 or x / x -> 1: both trap at x = 0. *)
    | Neg { node = Int (T x); _ } -> const (-x)
    | Neg v when Option.is_some (unwrap_neg v) -> Option.value_exn (unwrap_neg v)
    | (T _ | Add _ | Sub _ | Mul _ | Div _ | Mod _ | Neg _) as expr -> Value0.intern (Int expr)
  ;;

  exception Divide_by_zero of t
  exception Negative_modulus of t

  let add a b = reduce (Add (a, b))
  let sub a b = reduce (Sub (a, b))
  let mul a b = reduce (Mul (a, b))
  let neg v = reduce (Neg v)

  let div (a : value) (b : value) =
    match a.node, b.node with
    | Bottom, _ | _, Bottom -> Value0.bottom
    | _, Int (T 0L) -> raise (Divide_by_zero (Div (a, b)))
    | _ -> reduce (Div (a, b))
  ;;

  let mod_ (a : value) (b : value) =
    match a.node, b.node with
    | Bottom, _ | _, Bottom -> Value0.bottom
    | _, Int (T 0L) -> raise (Divide_by_zero (Mod (a, b)))
    | _, Int (T n) when Int64.is_negative n -> raise (Negative_modulus (Mod (a, b)))
    | _ -> reduce (Mod (a, b))
  ;;
end

module Value1 = struct
  include Value0

  let rec rewrite_value memo (value : value) ~target ~replacement =
    match Hashtbl.find memo value with
    | Some result -> result
    | None ->
      let result = rewrite_value' memo value ~target ~replacement in
      Hashtbl.set memo ~key:value ~data:result;
      result

  and rewrite_value' memo (value : value) ~target ~replacement =
    let rewrite value = rewrite_value memo value ~target ~replacement in
    if equal_value value target
    then replacement
    else (
      match value.node with
      | Bottom | Unit | Var _ | Closure _ | Binder _ | External _ | Prim _ -> value
      | Bool bool_value ->
        (match bool_value with
         | T _ -> value
         | And (a, b) -> Bool.and_ (rewrite a) (rewrite b)
         | Or (a, b) -> Bool.or_ (rewrite a) (rewrite b)
         | Eq (a, b) -> Bool.eq (rewrite a) (rewrite b)
         | Neq (a, b) -> Bool.neq (rewrite a) (rewrite b)
         | Lt (a, b) -> Bool.lt (rewrite a) (rewrite b)
         | Lte (a, b) -> Bool.lte (rewrite a) (rewrite b)
         | Gt (a, b) -> Bool.gt (rewrite a) (rewrite b)
         | Gte (a, b) -> Bool.gte (rewrite a) (rewrite b)
         | Not a -> Bool.not_ (rewrite a))
      | Int int_value ->
        (match int_value with
         | T _ -> value
         | Add (a, b) -> Int.add (rewrite a) (rewrite b)
         | Sub (a, b) -> Int.sub (rewrite a) (rewrite b)
         | Mul (a, b) -> Int.mul (rewrite a) (rewrite b)
         (* A divisor rewritten to zero would become a static failure; keep the node and
            let it resurface if the branch is ever actually entered. *)
         | Div (a, b) ->
           (match (rewrite b).node with
            | Int (T 0L) -> value
            | _ -> Int.div (rewrite a) (rewrite b))
         | Mod (a, b) ->
           (match (rewrite b).node with
            | Int (T n) when Int64.(n <= 0L) -> value
            | _ -> Int.mod_ (rewrite a) (rewrite b))
         | Neg a -> Int.neg (rewrite a))
      | Type ty -> Value0.type_ (rewrite_ty memo ty ~target ~replacement)
      | Tuple elts -> Value0.tuple (Nonempty_list.map elts ~f:rewrite)
      | Inject { label; ty } -> Value0.inject ~ty:(rewrite ty) ~label
      | Constructor { label; payload } ->
        Value0.constructor ~label ~payload:(Option.map payload ~f:rewrite)
      | Apply { fn; arg } -> Value0.apply ~fn:(rewrite fn) ~arg:(rewrite arg)
      | Proj { tuple = subject; index } -> Value0.proj (rewrite subject) index
      | Payload { variant; label } -> Value0.payload (rewrite variant) ~label
      | Match { scrutinee = match_scrutinee; arms } ->
        match_
          ~scrutinee:(rewrite match_scrutinee)
          ~arms:(Nonempty_list.map arms ~f:(fun (pattern, leaf) -> pattern, rewrite leaf))
      | Box payload -> Value0.box (rewrite payload)
      | Deref ref -> Value0.deref (rewrite ref))

  and rewrite_ty memo (ty : ty) ~target ~replacement =
    let rewrite value = rewrite_value memo value ~target ~replacement in
    match ty with
    | Unit | Bool | Int | Type -> ty
    | Arrow { arg_ty; arg_mode; ret_ty; ret_mode } ->
      Arrow { arg_ty = rewrite arg_ty; arg_mode; ret_ty = rewrite ret_ty; ret_mode }
    (* Dependent returns capture their env; substitution stops at the binder. *)
    | Pi { arg_ty; arg_mode; ret_ty; ret_mode } ->
      Pi { arg_ty = rewrite arg_ty; arg_mode; ret_ty; ret_mode }
    | Tuple elts -> Tuple (Nonempty_list.map elts ~f:rewrite)
    | Variant constructors -> Variant (Map.map constructors ~f:(Option.map ~f:rewrite))
    | Ref payload -> Ref (rewrite payload)

  and rewrite value ~target ~replacement =
    if equal_value replacement target
    then value
    else rewrite_value (Hashtbl.create (module Value0.Key)) value ~target ~replacement

  and match_ ~(scrutinee : value) ~arms =
    match scrutinee.node with
    | Bottom -> scrutinee
    | _ ->
      let rec select : _ -> value Or_unknown.t = function
        | [] -> Known Value0.bottom
        | [ (_, leaf) ] -> Known leaf
        | (pattern, leaf) :: rest ->
          (match Pattern.matches scrutinee pattern with
           | Match -> Known leaf
           | No_match -> select rest
           | Unknown -> Unknown)
      in
      (match select (Nonempty_list.to_list arms) with
       | Known value -> value
       | Unknown ->
         (* If all arms are the same value, use it *)
         let (first :: rest) = Nonempty_list.map arms ~f:snd in
         if List.for_all rest ~f:(Hashcons.equal first)
         then first
         else Value0.intern (Match { scrutinee; arms }))
  ;;

  let rec if_ ~(cond : value) ~then_ ~else_ : value =
    match cond.node with
    | Bool (Not cond) -> if_ ~cond ~then_:else_ ~else_:then_
    | Bool (Neq (a, b)) -> if_ ~cond:(Value0.intern (Bool (Eq (a, b)))) ~then_:else_ ~else_:then_
    | _ ->
      match_
        ~scrutinee:cond
        ~arms:
          (Nonempty_list.create
             ((Literal (Bool true) : Canon.t), then_)
             [ (Literal (Bool false) : Canon.t), else_ ])
  ;;
end

module Desc = struct
  type t = desc =
    { ty : value
    ; mode : Modes.t
    ; static : (value Lazy.t[@sexp.opaque])
    }
  [@@deriving sexp_of]

  let of_type ty =
    { ty = Value0.type_ Type
    ; mode = Modes.create ~staticity:Static ~erasure:Erased
    ; static = Lazy.from_val (Value0.type_ ty)
    }
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
    | Variant of value option Map.M(Ident.Label).t
    | Ref of value
  [@@deriving sexp_of]

  let equal = equal_ty
  let hash_fold_t = hash_fold_ty
  let hash = Hash.run ~seed hash_fold_t

  let arg (v : value) =
    match v.node with
    | Type (Arrow { arg_ty; _ } | Pi { arg_ty; _ }) -> arg_ty
    | _ -> raise_s [%message "Bug: expected function type"]
  ;;

  let arg_mode (v : value) =
    match v.node with
    | Type (Arrow { arg_mode; _ } | Pi { arg_mode; _ }) -> arg_mode
    | _ -> raise_s [%message "Bug: expected function type"]
  ;;

  let ret (v : value) =
    match v.node with
    | Type (Arrow { ret_ty; _ }) -> ret_ty
    | _ -> raise_s [%message "Bug: expected arrow type"]
  ;;

  let ret_mode (v : value) =
    match v.node with
    | Type (Arrow { ret_mode; _ } | Pi { ret_mode; _ }) -> ret_mode
    | _ -> raise_s [%message "Bug: expected function type"]
  ;;

  let of_literal : Dst.Literal.t -> t = function
    | Unit -> Unit
    | Bool _ -> Bool
    | Int _ -> Int
  ;;

  let unify_constructors ~f a_ctors b_ctors : ty option =
    with_return (fun { return } ->
      let constructors =
        Map.merge a_ctors b_ctors ~f:(fun ~key:_ -> function
          | `Both (None, None) -> Some None
          | `Both (Some a, Some b) ->
            (match f a b with
             | Some payload -> Some (Some payload)
             | None -> return None)
          | `Both (None, Some _) | `Both (Some _, None) | `Left _ | `Right _ -> return None)
      in
      Some (Variant constructors))
  ;;
end

module Value = struct
  type t = value [@@deriving sexp_of]

  type nonrec node = node =
    | Bottom
    | Unit
    | Bool of vbool
    | Int of vint
    | Type of ty
    | Closure of closure
    | Binder of binder
    | Var of Ident.t
    | Tuple of t Nonempty_list.t
    | Inject of
        { label : Ident.Label.t
        ; ty : t
        }
    | Constructor of
        { label : Ident.Label.t
        ; payload : t option
        }
    | Apply of
        { fn : t
        ; arg : t
        }
    | Proj of
        { tuple : t
        ; index : int
        }
    | Payload of
        { variant : t
        ; label : Ident.Label.t
        }
    | Match of
        { scrutinee : t
        ; arms : (Canon.t * t) Nonempty_list.t
        }
    | External of
        { symbol : string
        ; ty : t
        }
    | Box of t
    | Deref of t
    | Prim of Builtin0.Prim.t
  [@@deriving sexp_of]

  include Value1
  include Comparable.Make_plain (Key)

  let equal = equal_value
  let equal_node = equal_node
  let hash_fold_t = hash_fold_value
  let hash_fold_node = hash_fold_node
  let hash = Hash.run ~seed hash_fold_t
  let hash_node = Hash.run ~seed hash_fold_node

  let is_fn_type (value : t) =
    match value.node with
    | Type (Arrow _ | Pi _) -> true
    | _ -> false
  ;;

  let ty_exn (value : t) =
    match value.node with
    | Type ty -> ty
    | _ -> raise_s [%message "Bug: expected concrete type" (value : t)]
  ;;

  let of_literal : Dst.Literal.t -> value = function
    | Unit -> Value0.unit
    | Bool b -> Bool.const b
    | Int i -> Int.const i
  ;;

  module Eliminator = struct
    type t =
      | Apply of value
      | Proj of int
      | Payload of Ident.Label.t
      | Deref

    let rec peel (value : value) frames =
      match value.node with
      | Apply { fn; arg } -> peel fn (Apply arg :: frames)
      | Proj { tuple; index } -> peel tuple (Proj index :: frames)
      | Payload { variant; label } -> peel variant (Payload label :: frames)
      | Deref ref -> peel ref (Deref :: frames)
      | Bottom
      | Unit
      | Bool _
      | Int _
      | Type _
      | Closure _
      | Binder _
      | Var _
      | Tuple _
      | Inject _
      | Constructor _
      | Match _
      | External _
      | Box _
      | Prim _ -> value, frames
    ;;

    let unpeel leaf frames =
      List.fold frames ~init:leaf ~f:(fun value (frame : t) ->
        match frame with
        | Apply arg -> apply ~fn:value ~arg
        | Proj index -> proj value index
        | Payload label -> payload value ~label
        | Deref -> deref value)
    ;;
  end
end

module Dependent = struct
  type t = dependent =
    | T of
        { ty : value
        ; memo : ((value, value) Hashtbl.t[@sexp.opaque])
        }
    | Reduce of
        { env : (env[@sexp.opaque])
        ; arg : Ident.t
        ; arg_ty : value
        ; arg_mode : Modes.t
        ; ret_ty : Dst.Expr.t
        ; memo : ((value, value) Hashtbl.t[@sexp.opaque])
        ; uid : (int[@sexp.opaque])
        }
    | Typecheck of
        { env : (env[@sexp.opaque])
        ; arg : Ident.t
        ; arg_ty : value
        ; arg_mode : Modes.t
        ; body : Dst.Expr.t
        ; memo : ((value, value) Hashtbl.t[@sexp.opaque])
        ; uid : (int[@sexp.opaque])
        }
  [@@deriving sexp_of]

  let equal = equal_dependent
  let hash_fold_t = hash_fold_dependent
  let hash = Hash.run ~seed hash_fold_t
  let mono ty : t = T { ty; memo = Hashtbl.create (module Value) }

  let rec is_concrete_value seen (v : value) =
    match Hashtbl.find seen v with
    | Some r -> r
    | None ->
      let r = is_concrete_value' seen v in
      Hashtbl.set seen ~key:v ~data:r;
      r

  and is_concrete_value' seen (v : value) =
    match v.node with
    | Unit | Closure _ | Binder _ | External _ | Prim _ -> true
    | Bool b -> is_concrete_bool b
    | Int i -> is_concrete_int i
    | Type ty -> is_concrete_ty seen ty
    | Tuple elts -> Nonempty_list.for_all elts ~f:(is_concrete_value seen)
    | Inject { ty; _ } -> is_concrete_value seen ty
    | Constructor { payload; _ } -> Option.for_all payload ~f:(is_concrete_value seen)
    | Box payload -> is_concrete_value seen payload
    | Bottom | Var _ | Apply _ | Proj _ | Payload _ | Match _ | Deref _ -> false

  and is_concrete_bool : vbool -> _ = function
    | T _ -> true
    | _ -> false

  and is_concrete_int : vint -> _ = function
    | T _ -> true
    | _ -> false

  and is_concrete_ty seen : ty -> _ = function
    | Unit | Bool | Int | Type -> true
    | Tuple elts -> Nonempty_list.for_all elts ~f:(is_concrete_value seen)
    | Arrow { arg_ty; ret_ty; _ } -> is_concrete_value seen arg_ty && is_concrete_value seen ret_ty
    | Pi { arg_ty; ret_ty; _ } -> is_concrete_value seen arg_ty && is_concrete_dependent seen ret_ty
    | Variant constructors ->
      Map.for_all constructors ~f:(fun payload ->
        Option.for_all payload ~f:(is_concrete_value seen))
    | Ref payload -> is_concrete_ref_payload seen payload

  and is_concrete_ref_payload seen (v : value) =
    match v.node with
    | Apply { fn; arg } -> is_concrete_ref_payload seen fn && is_concrete_value seen arg
    | _ -> is_concrete_value seen v

  and is_concrete_dependent seen : dependent -> _ = function
    | T { ty; _ } -> is_concrete_value seen ty
    | Reduce _ | Typecheck _ -> false
  ;;

  let is_concrete v = is_concrete_value (Hashtbl.create (module Value)) v

  let typecheck ty ~env ~arg ~arg_ty ~arg_mode ~body =
    if is_concrete ty
    then mono ty
    else
      Typecheck
        { env; arg; arg_ty; arg_mode; body; memo = Hashtbl.create (module Value); uid = uid () }
  ;;

  let reduce ty ~env ~arg ~arg_ty ~arg_mode ~ret_ty =
    if is_concrete ty
    then mono ty
    else
      Reduce
        { env; arg; arg_ty; arg_mode; ret_ty; memo = Hashtbl.create (module Value); uid = uid () }
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
        ; body : expr Core.Int.Map.t
        ; ty : value
        ; mode : Modes.t
        ; family : (int[@sexp.opaque])
        ; loc : Lex.Location.t
        }
  [@@deriving sexp_of]

  type nonrec case = case =
    { bindings : value Ident.Map.t
    ; body : expr
    }
  [@@deriving sexp_of]

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
  [@@deriving sexp_of]

  type nonrec target = target =
    | Family of (int[@sexp.opaque])
    | Prim of Builtin0.Prim.t
  [@@deriving sexp_of]

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
        ; body : expr Core.Int.Map.t
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
        ; key : value option
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
    | Payload_get of
        { variant : t
        ; label : Ident.Label.t
        ; ty : value
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | Tag_test of
        { variant : t
        ; label : Ident.Label.t
        ; ty : value
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | Make_ref of
        { payload : t
        ; ty : value
        ; mode : Modes.t
        ; loc : Lex.Location.t
        }
    | Ref_get of
        { ref : t
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
  [@@deriving sexp_of]

  let rec free_vars ?monos (expr : t) : Ident.Set.t =
    match expr with
    | Erased _ | Literal _ | Builtin _ -> Ident.Set.empty
    | Extcall { arg; _ } -> free_vars ?monos arg
    | Tuple_get { tuple; _ } -> free_vars ?monos tuple
    | Payload_get { variant; _ } | Tag_test { variant; _ } -> free_vars ?monos variant
    | Make_ref { payload; _ } -> free_vars ?monos payload
    | Ref_get { ref; _ } -> free_vars ?monos ref
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
    | Payload_get { ty; _ }
    | Tag_test { ty; _ }
    | Make_ref { ty; _ }
    | Ref_get { ty; _ }
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
    | Payload_get { mode; _ }
    | Make_ref { mode; _ }
    | Ref_get { mode; _ }
    | Tag_test { mode; _ }
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
    | Payload_get { loc; _ }
    | Make_ref { loc; _ }
    | Ref_get { loc; _ }
    | Tag_test { loc; _ }
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
    | Payload_get t -> Payload_get { t with ty; mode }
    | Make_ref t -> Make_ref { t with ty; mode }
    | Ref_get t -> Ref_get { t with ty; mode }
    | Tag_test t -> Tag_test { t with ty; mode }
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
    | Payload_get t -> Payload_get { t with ty }
    | Make_ref t -> Make_ref { t with ty }
    | Ref_get t -> Ref_get { t with ty }
    | Tag_test t -> Tag_test { t with ty }
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
    | Payload_get t -> Payload_get { t with mode }
    | Make_ref t -> Make_ref { t with mode }
    | Ref_get t -> Ref_get { t with mode }
    | Tag_test t -> Tag_test { t with mode }
    | Extcall t -> Extcall { t with mode }
  ;;

  let literal ~loc value =
    let ty = Ty.of_literal value in
    let mode = Modes.create ~staticity:Static ~erasure:Erased in
    Literal { value = Value.of_literal value; ty = Value0.type_ ty; mode; loc }
  ;;

  let rebind bind ~id ~f =
    let loc = loc bind in
    let ref = Var { id; ty = ty bind; mode = mode bind; loc } in
    let rest = f ref in
    Let { var = id; bind; rest; ty = ty rest; mode = mode rest; loc }
  ;;

  let tuple ~loc (elts : (t * desc) Nonempty_list.t) : t * desc =
    let exprs, descs = Nonempty_list.unzip elts in
    let ty = Value0.type_ (Tuple (Nonempty_list.map descs ~f:(fun (d : desc) -> d.ty))) in
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
end

module Env = struct
  type t = env [@@deriving sexp_of]

  module Kind = Kind

  let initial = { bindings = Ident.Map.empty; facts = []; level = 0; kind = Abstract }
  let enter t kind = { t with kind = Kind.join t.kind kind }
  let abstract t = Kind.compare t.kind Abstract = 0
  let reducing t = Kind.compare t.kind Reducing >= 0
  let instancing t = Kind.compare t.kind Instancing >= 0

  let bind t id desc =
    if Ident.is_anon id
    then t
    else
      { t with bindings = Map.set t.bindings ~key:id ~data:{ desc; level = t.level; cache = None } }
  ;;

  let rec learn t ~target ~replacement =
    if equal_value target replacement
    then t
    else (
      match (target : value).node, (replacement : value).node with
      | Bottom, _ -> t
      | _, Bottom ->
        raise_s [%message "Bug: learned a contradicted fact" (target : value) (replacement : value)]
      | Tuple targets, Tuple replacements
        when Nonempty_list.length targets = Nonempty_list.length replacements ->
        Nonempty_list.zip_exn targets replacements
        |> Nonempty_list.fold ~init:t ~f:(fun t (target, replacement) ->
          learn t ~target ~replacement)
      | ( Constructor { label; payload = Some target }
        , Constructor { label = label'; payload = Some replacement } )
        when Ident.Label.equal label label' -> learn t ~target ~replacement
      | _ -> { t with facts = { target; replacement } :: t.facts; level = t.level + 1 })
  ;;

  let apply (t : t) binding =
    if binding.level = t.level
    then binding.desc
    else (
      match binding.cache with
      | Some (facts, applied) when phys_equal facts t.facts -> applied
      | _ ->
        let facts = List.take t.facts (t.level - binding.level) in
        let rewrite value =
          (* Newest first, so [fold_right] applies facts in the order learned. *)
          List.fold_right facts ~init:value ~f:(fun { target; replacement } value ->
            Value.rewrite value ~target ~replacement)
        in
        let applied =
          { binding.desc with
            ty = rewrite binding.desc.ty
          ; static = Lazy.map binding.desc.static ~f:rewrite
          }
        in
        binding.cache <- Some (t.facts, applied);
        applied)
  ;;

  let find t id = Option.map (Map.find t.bindings id) ~f:(apply t)
  let find_exn t id = apply t (Map.find_exn t.bindings id)

  let merge t other =
    let imported =
      if phys_equal t.facts other.facts
      then other.bindings
      else
        Map.map other.bindings ~f:(fun b -> { desc = apply other b; level = t.level; cache = None })
    in
    { t with
      bindings = Map.merge_skewed t.bindings imported ~combine:(fun ~key:_ existing _ -> existing)
    }
  ;;
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
  [@@deriving sexp_of]
end

module Program = struct
  type t =
    { top_levels : Top_level.t list
    ; stamp : int
    }
  [@@deriving sexp_of]
end
