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
      | Variant_t of t option Map.M(Ident.Label).t
      | Inject of
          { label : Ident.Label.t
          ; ty : t
          }
      | Constructor of
          { label : Ident.Label.t
          ; payload : t option
          }
    [@@deriving sexp, hash, compare]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

module Excluded0 = struct
  module T = struct
    type t =
      | Literal of Dst.Literal.t
      | Constructor of
          { label : Ident.Label.t
          ; payload : t option
          }
    [@@deriving sexp, compare, equal, hash]
  end

  include T
  include Comparable.Make (T)
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
      ; arms : (Dst.Expr.pattern * value) Nonempty_list.t
      }
  | Refine of
      { value : value
      ; excluded : Set.M(Excluded0).t
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

module Value0 = struct
  (* Values are reduced on construction, so every value is in normal form. *)

  let bottom : value = Bottom
  let unit : value = Unit
  let type_ ty : value = Type ty
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

  let collapse value ~excluded : value =
    let excludes b = Set.mem excluded (Excluded0.Literal (Bool b)) in
    match excludes true, excludes false with
    | true, true -> Bottom
    | true, false -> Bool (T false)
    | false, true -> Bool (T true)
    | false, false -> Refine { value; excluded }
  ;;

  let rec payload value ~label =
    match value with
    | Constructor { label = got; payload = got_payload } when Ident.Label.equal got label ->
      (match got_payload with
       | Some payload -> payload
       | None -> raise_s [%message "Bug: expected payload" (label : Ident.Label.t) (value : value)])
    | Bottom -> Bottom
    | Refine { value; excluded } ->
      if Set.mem excluded (Constructor { label; payload = None })
      then Bottom
      else (
        let payload_excluded =
          Set.fold excluded ~init:Excluded0.Set.empty ~f:(fun acc excluded ->
            match excluded with
            | Constructor { label = l; payload = Some shape } when Ident.Label.equal l label ->
              Set.add acc shape
            | Literal _ | Constructor _ -> acc)
        in
        let extracted = payload value ~label in
        if Set.is_empty payload_excluded
        then extracted
        else collapse extracted ~excluded:payload_excluded)
    | (Var _ | Constructor _ | Apply _ | Proj _ | Payload _ | Match _) as value ->
      Payload { variant = value; label }
    | ( Unit
      | Bool _
      | Int _
      | Type _
      | Closure _
      | Binder _
      | Tuple _
      | Inject _
      | External _
      | Prim _ ) as value ->
      raise_s [%message "Bug: expected variant" (label : Ident.Label.t) (value : value)]
  ;;

  let inject ~ty ~label = Inject { ty; label }

  let constructor ~label ~payload =
    (* A constructor with an unreachable payload is unreachable. *)
    match payload with
    | Some Bottom -> Bottom
    | payload -> Constructor { label; payload }
  ;;

  let apply ~fn ~arg =
    match fn, arg with
    | Bottom, _ | _, Bottom -> Bottom
    | Inject { label; _ }, arg -> constructor ~label ~payload:(Some arg)
    | ( (( Closure _
         | Binder _
         | Var _
         | Apply _
         | Proj _
         | Payload _
         | Match _
         | External _
         | Prim _ ) as fn)
      , arg ) -> Apply { fn; arg }
    | ((Unit | Bool _ | Int _ | Type _ | Tuple _ | Constructor _ | Refine _) as fn), arg ->
      (* [Refine] never has arrow type: exclusions are literals and tags. *)
      raise_s [%message "Bug: expected function" (fn : value) (arg : value)]
  ;;

  let proj (tuple : value) index =
    match tuple with
    | Tuple elts -> Nonempty_list.nth_exn elts index
    | Bottom -> Bottom
    | (Var _ | Apply _ | Proj _ | Payload _ | Match _ | Refine _) as tuple -> Proj { tuple; index }
    | ( Unit
      | Bool _
      | Int _
      | Type _
      | Closure _
      | Binder _
      | Inject _
      | Constructor _
      | External _
      | Prim _ ) as tuple -> raise_s [%message "Bug: expected tuple" (index : int) (tuple : value)]
  ;;
end

module Pattern = struct
  module Excluded = struct
    include Excluded0

    let rec is_excluded t (value : value) : bool Or_unknown.t =
      match value with
      | Int value ->
        (match t with
         | Literal (Int m) ->
           (match value with
            | T n -> Known (Int64.equal n m)
            | _ -> Unknown)
         | Literal (Unit | Bool _) | Constructor _ -> Known false)
      | Bool value ->
        (match t with
         | Literal (Bool c) ->
           (match value with
            | T b -> Known (Core.Bool.equal b c)
            | _ -> Unknown)
         | Literal (Unit | Int _) | Constructor _ -> Known false)
      | Unit ->
        (match t with
         | Literal Unit -> Known true
         | Literal (Bool _ | Int _) | Constructor _ -> Known false)
      | Constructor { label; payload } ->
        (match t with
         | Constructor { label = t_label; payload = t_payload } ->
           if not (Ident.Label.equal label t_label)
           then Known false
           else (
             match t_payload, payload with
             | None, _ -> Known true
             | Some t_payload, Some payload -> is_excluded t_payload payload
             | Some _, None -> Known false)
         | Literal _ -> Known false)
      | Refine { value; excluded } ->
        if Core.Set.mem excluded t then Known false else is_excluded t value
      | Match { scrutinee = _; arms } ->
        (match
           Nonempty_list.to_list arms
           |> List.map ~f:(fun (_, leaf) -> is_excluded t leaf)
           |> List.all_equal ~equal:(Or_unknown.equal Bool.equal)
         with
         | Some verdict -> verdict
         | None -> Unknown)
      | Bottom | Tuple _ | Closure _ | Binder _ | External _ | Prim _ | Inject _ | Type _ ->
        Known false
      | Var _ | Apply _ | Proj _ | Payload _ -> Unknown
    ;;

    let rec covers t (pattern : Dst.Expr.pattern) : bool =
      match t, pattern with
      | Literal t, Literal { value; _ } -> Dst.Literal.equal t value
      | Constructor { label = t_label; payload = None }, Constructor { label; _ } ->
        Ident.Label.equal t_label label
      | ( Constructor { label = t_label; payload = Some t_payload }
        , Constructor { label; payload = Some payload; _ } ) ->
        Ident.Label.equal t_label label && covers t_payload payload
      | t, Or { left; right; _ } -> covers t left && covers t right
      | (Literal _ | Constructor _), (Var _ | Literal _ | Constructor _ | Tuple _) -> false
    ;;
  end

  module Step = struct
    type t =
      | Index of int
      | Payload of Ident.Label.t
    [@@deriving sexp, compare, equal, hash]
  end

  module Matched = struct
    type t =
      | Match of (Ident.t * Step.t list) list
      | No_match
      | Unknown
    [@@deriving sexp, compare, equal, hash]
  end

  let rec is_irrefutable (pattern : Dst.Expr.pattern) =
    match pattern with
    | Var _ -> true
    | Literal { value = Unit; _ } -> true
    | Literal { value = Bool _ | Int _; _ } -> false
    | Tuple { elts; _ } -> Nonempty_list.for_all elts ~f:is_irrefutable
    | Constructor _ -> false
    | Or { left; right; _ } -> is_irrefutable left || is_irrefutable right
  ;;

  let rec shape (pattern : Dst.Expr.pattern) : Excluded.t option =
    match pattern with
    | Var _ | Literal { value = Unit; _ } -> None
    | Literal { value = (Bool _ | Int _) as literal; _ } -> Some (Literal literal)
    | Constructor { label; payload = None; _ } -> Some (Constructor { label; payload = None })
    | Constructor { label; payload = Some payload; _ } ->
      if is_irrefutable payload
      then Some (Constructor { label; payload = None })
      else
        Option.map (shape payload) ~f:(fun payload ->
          Excluded.Constructor { label; payload = Some payload })
    | Tuple _ | Or _ -> None
  ;;

  let rec excludes (pattern : Dst.Expr.pattern) : Excluded.Set.t =
    match pattern with
    | Var _ | Literal _ | Constructor _ | Tuple _ ->
      (match shape pattern with
       | Some shape -> Excluded.Set.singleton shape
       | None -> Excluded.Set.empty)
    | Or { left; right; _ } -> Set.union (excludes left) (excludes right)
  ;;

  let all_excludes refuted = List.map refuted ~f:excludes |> Excluded.Set.union_list

  let rec specialize (pattern : Dst.Expr.pattern) ~scrutinee : value =
    match pattern with
    | Var _ -> scrutinee
    | Or _ -> scrutinee (* Would need a disjunction *)
    | Literal { value = Unit; _ } -> Unit
    | Literal { value = Bool b; _ } -> Bool (T b)
    | Literal { value = Int i; _ } -> Int (T i)
    | Constructor { label; payload = payload_pattern; _ } ->
      Value0.constructor
        ~label
        ~payload:
          (Option.map payload_pattern ~f:(fun pattern ->
             specialize pattern ~scrutinee:(Value0.payload scrutinee ~label)))
    | Tuple { elts; _ } ->
      Value0.tuple
        (Nonempty_list.mapi elts ~f:(fun index pattern ->
           specialize pattern ~scrutinee:(Value0.proj scrutinee index)))
  ;;

  let rec matches (value : value) pattern : Matched.t = match_at [] value pattern

  and match_at path (value : value) (pattern : Dst.Expr.pattern) : Matched.t =
    match pattern with
    | Var { id; _ } -> Match (if Ident.is_anon id then [] else [ id, List.rev path ])
    | Literal { value = literal; _ } ->
      (match literal, value with
       | Unit, _ -> (* Unit's one value always matches. *) Match []
       | Bool want, Bool (T got) -> if Core.Bool.equal got want then Match [] else No_match
       | Int want, Int (T got) -> if Int64.equal got want then Match [] else No_match
       | (Bool _ | Int _), Refine { value; excluded } ->
         if Set.exists excluded ~f:(fun t -> Excluded.covers t pattern)
         then No_match
         else match_at path value pattern
       | (Bool _ | Int _), _ -> Unknown)
    | Tuple { elts; _ } ->
      Nonempty_list.to_list elts
      |> List.foldi ~init:(Matched.Match []) ~f:(fun index acc elt ->
        let matched = match_at (Step.Index index :: path) (Value0.proj value index) elt in
        match acc, matched with
        | No_match, _ | _, No_match -> Matched.No_match
        | Unknown, _ | _, Unknown -> Unknown
        | Match bindings, Match elt_bindings -> Match (bindings @ elt_bindings))
    | Constructor { label; payload; _ } ->
      (match value with
       | Constructor { label = got; payload = got_payload } ->
         if not (Ident.Label.equal got label)
         then No_match
         else (
           match payload, got_payload with
           | None, None -> Match []
           | Some payload, Some got_payload ->
             match_at (Step.Payload label :: path) got_payload payload
           | Some _, None | None, Some _ ->
             raise_s [%message "Bug: constructor payload mismatch" (label : Ident.Label.t)])
       | Refine { value; excluded } ->
         if Set.exists excluded ~f:(fun t -> Excluded.covers t pattern)
         then No_match
         else match_at path value pattern
       | _ -> Unknown)
    | Or { left; right; _ } ->
      (match match_at path value left with
       | (Match _ | Unknown) as matched -> matched
       | No_match -> match_at path value right)
  ;;

  let selects (scrutinee : value) patterns : _ Or_unknown.t =
    let rec aux index : _ -> _ Or_unknown.t = function
      | [] -> Unknown
      | pattern :: rest ->
        (match matches scrutinee pattern with
         | Match bindings -> Known (index, bindings)
         | No_match -> aux (index + 1) rest
         | Unknown -> Unknown)
    in
    match scrutinee with
    | Bottom -> Unknown
    | _ -> aux 0 (Nonempty_list.to_list patterns)
  ;;

  let rec implies (fact : Dst.Expr.pattern) (pattern : Dst.Expr.pattern) : bool Or_unknown.t =
    match fact, pattern with
    | _, Var _ -> Known true
    | Or { left; right; _ }, _ ->
      let%bind.Or_unknown left = implies left pattern in
      let%bind.Or_unknown right = implies right pattern in
      if Core.Bool.equal left right then Known left else Unknown
    | _, Or { left; right; _ } ->
      (match implies fact left, implies fact right with
       | Known true, _ | _, Known true -> Known true
       | Known false, Known false -> Known false
       | _, _ -> Unknown)
    | Var _, _ -> Unknown
    | Literal { value = fact; _ }, Literal { value = pattern; _ } ->
      Known (Dst.Literal.equal fact pattern)
    | ( Constructor { label = fact_label; payload = fact_payload; _ }
      , Constructor { label; payload; _ } ) ->
      if not (Ident.Label.equal fact_label label)
      then Known false
      else (
        match fact_payload, payload with
        | None, None -> Known true
        | Some fact_payload, Some payload -> implies fact_payload payload
        | None, Some _ | Some _, None -> Unknown)
    | Tuple { elts = fact_elts; _ }, Tuple { elts; _ } ->
      (match Nonempty_list.zip fact_elts elts with
       | Unequal_lengths -> Unknown
       | Ok pairs ->
         Nonempty_list.fold pairs ~init:(Or_unknown.Known true) ~f:(fun acc (fact, pattern) ->
           match acc, implies fact pattern with
           | Known false, _ | _, Known false -> Known false
           | Known true, Known true -> Known true
           | _, _ -> Unknown))
    | Literal _, (Constructor _ | Tuple _)
    | Constructor _, (Literal _ | Tuple _)
    | Tuple _, (Literal _ | Constructor _) -> Unknown
  ;;

  let rec patterns_agree (a : Dst.Expr.pattern) (b : Dst.Expr.pattern) =
    match a, b with
    | Var _, Var _ ->
      (* Arm leaves are closed, so binder names are irrelevant. *)
      true
    | Literal { value = a; _ }, Literal { value = b; _ } -> Dst.Literal.equal a b
    | Tuple { elts = a; _ }, Tuple { elts = b; _ } ->
      (match Nonempty_list.zip a b with
       | Ok zip -> Nonempty_list.for_all zip ~f:(fun (a, b) -> patterns_agree a b)
       | Unequal_lengths -> false)
    | ( Constructor { label = a; payload = a_payload; _ }
      , Constructor { label = b; payload = b_payload; _ } ) ->
      Ident.Label.equal a b
      &&
        (match a_payload, b_payload with
        | None, None -> true
        | Some a, Some b -> patterns_agree a b
        | None, Some _ | Some _, None -> false)
    | Or { left = a_left; right = a_right; _ }, Or { left = b_left; right = b_right; _ } ->
      patterns_agree a_left b_left && patterns_agree a_right b_right
    | (Var _ | Literal _ | Constructor _ | Tuple _ | Or _), _ -> false
  ;;

  (* The last patterns may differ: match values are exhaustive, so with the earlier
     patterns equal, each last arm covers the same remainder whatever its spelling. *)
  let arms_agree a_arms b_arms =
    match Nonempty_list.zip a_arms b_arms with
    | Ok zip ->
      let zip = Nonempty_list.to_list zip in
      let last = List.length zip - 1 in
      List.for_alli zip ~f:(fun i ((a_pattern, _), (b_pattern, _)) ->
        i = last || patterns_agree a_pattern b_pattern)
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

  let with_refuted arms =
    let patterns = Nonempty_list.map arms ~f:fst |> Nonempty_list.to_list in
    Nonempty_list.to_list arms
    |> List.mapi ~f:(fun index (pattern, leaf) -> List.take patterns index, pattern, leaf)
  ;;
end

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

  let equal = [%compare.equal: t]
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

  let equal = [%compare.equal: t]
end

(* TODO replace with some kind of hash cons *)
let rec identical (a : value) (b : value) =
  phys_equal a b
  ||
  match a, b with
  | Bottom, Bottom | Unit, Unit -> true
  | Var a, Var b -> Ident.equal a b
  | Bool a, Bool b -> identical_bool a b
  | Int a, Int b -> identical_int a b
  | Type a, Type b -> identical_ty a b
  | Closure a, Closure b -> a.hash = b.hash
  | Binder a, Binder b -> a.hash = b.hash
  | External a, External b -> String.equal a.symbol b.symbol
  | Prim a, Prim b -> Builtin0.Prim.equal a b
  | Tuple a, Tuple b -> identical_list a b
  | Inject a, Inject b -> Ident.Label.equal a.label b.label && identical a.ty b.ty
  | Constructor a, Constructor b ->
    Ident.Label.equal a.label b.label && Option.equal identical a.payload b.payload
  | Apply a, Apply b -> identical a.fn b.fn && identical a.arg b.arg
  | Proj a, Proj b -> a.index = b.index && identical a.tuple b.tuple
  | Payload a, Payload b -> Ident.Label.equal a.label b.label && identical a.variant b.variant
  | Refine a, Refine b -> identical a.value b.value && Set.equal a.excluded b.excluded
  | Match a, Match b ->
    identical a.scrutinee b.scrutinee
    &&
      (match Nonempty_list.zip a.arms b.arms with
      | Ok zip ->
        Nonempty_list.for_all zip ~f:(fun ((a_pattern, a_leaf), (b_pattern, b_leaf)) ->
          Pattern.patterns_agree a_pattern b_pattern && identical a_leaf b_leaf)
      | Unequal_lengths -> false)
  | ( ( Bottom
      | Unit
      | Var _
      | Bool _
      | Int _
      | Type _
      | Closure _
      | Binder _
      | External _
      | Prim _
      | Tuple _
      | Inject _
      | Constructor _
      | Apply _
      | Proj _
      | Payload _
      | Refine _
      | Match _ )
    , _ ) -> false

and identical_list a b =
  match Nonempty_list.zip a b with
  | Ok zip -> Nonempty_list.for_all zip ~f:(fun (a, b) -> identical a b)
  | Unequal_lengths -> false

and identical_bool (a : vbool) (b : vbool) =
  match a, b with
  | T a, T b -> Core.Bool.equal a b
  | And (a0, a1), And (b0, b1)
  | Or (a0, a1), Or (b0, b1)
  | Eq (a0, a1), Eq (b0, b1)
  | Neq (a0, a1), Neq (b0, b1)
  | Lt (a0, a1), Lt (b0, b1)
  | Lte (a0, a1), Lte (b0, b1)
  | Gt (a0, a1), Gt (b0, b1)
  | Gte (a0, a1), Gte (b0, b1) -> identical a0 b0 && identical a1 b1
  | Not a, Not b -> identical a b
  | (T _ | And _ | Or _ | Eq _ | Neq _ | Lt _ | Lte _ | Gt _ | Gte _ | Not _), _ -> false

and identical_int (a : vint) (b : vint) =
  match a, b with
  | T a, T b -> Int64.equal a b
  | Add (a0, a1), Add (b0, b1)
  | Sub (a0, a1), Sub (b0, b1)
  | Mul (a0, a1), Mul (b0, b1)
  | Div (a0, a1), Div (b0, b1)
  | Mod (a0, a1), Mod (b0, b1) -> identical a0 b0 && identical a1 b1
  | Neg a, Neg b -> identical a b
  | (T _ | Add _ | Sub _ | Mul _ | Div _ | Mod _ | Neg _), _ -> false

and identical_ty (a : ty) (b : ty) =
  match a, b with
  | Unit, Unit | Bool, Bool | Int, Int | Type, Type -> true
  | Arrow a, Arrow b ->
    Modes.equal a.arg_mode b.arg_mode
    && Modes.equal a.ret_mode b.ret_mode
    && identical a.arg_ty b.arg_ty
    && identical a.ret_ty b.ret_ty
  (* Dependent returns carry envs and memos; stay conservative. *)
  | Pi _, Pi _ -> false
  | Tuple a, Tuple b -> identical_list a b
  | Variant a, Variant b -> Map.equal (Option.equal identical) a b
  | (Unit | Bool | Int | Type | Arrow _ | Pi _ | Tuple _ | Variant _), _ -> false
;;

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

  let is_literal : value -> bool = function
    | Bool (T _) | Int (T _) -> true
    | _ -> false
  ;;

  let base : value -> value = function
    | Refine { value; _ } -> value
    | value -> value
  ;;

  (* Normalize variable comparisons by comparison order. *)
  let reorder_vars a b =
    match base a, base b with
    | Var x, Var y -> Core.Int.( > ) (Ident.compare x y) 0
    | _ -> false
  ;;

  let same_var a b =
    match base a, base b with
    | Var x, Var y -> Ident.equal x y
    | _ -> false
  ;;

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
    | Eq (((Bool (T _) | Int (T _)) as lit), value) when not (is_literal value) ->
      reduce (Eq (value, lit))
    | Eq (a, b) when reorder_vars a b -> reduce (Eq (b, a))
    | Eq (Int x, Int y) when identical_int x y -> const true
    | Eq ((Int (Add (v, Int (T k))) | Int (Add (Int (T k), v))), Int (T c)) ->
      reduce (Eq (v, Int (T (c - k))))
    | Eq (Int (Sub (v, Int (T k))), Int (T c)) -> reduce (Eq (v, Int (T (c + k))))
    | Eq (Int (Sub (Int (T k), v)), Int (T c)) -> reduce (Eq (v, Int (T (k - c))))
    | Eq (Int (Neg v), Int (T c)) -> reduce (Eq (v, Int (T (neg c))))
    | Eq (a, b) when same_var a b -> const true
    | Eq (Refine { excluded; _ }, Int (T n)) when Set.mem excluded (Literal (Int n)) -> const false
    | Neq (Int (T x), Int (T y)) -> const (x <> y)
    | Neq (((Bool (T _) | Int (T _)) as lit), value) when not (is_literal value) ->
      reduce (Neq (value, lit))
    | Neq (a, b) when reorder_vars a b -> reduce (Neq (b, a))
    | Neq (Int x, Int y) when identical_int x y -> const false
    | Neq ((Int (Add (v, Int (T k))) | Int (Add (Int (T k), v))), Int (T c)) ->
      reduce (Neq (v, Int (T (c - k))))
    | Neq (Int (Sub (v, Int (T k))), Int (T c)) -> reduce (Neq (v, Int (T (c + k))))
    | Neq (Int (Sub (Int (T k), v)), Int (T c)) -> reduce (Neq (v, Int (T (k - c))))
    | Neq (Int (Neg v), Int (T c)) -> reduce (Neq (v, Int (T (neg c))))
    | Neq (a, b) when same_var a b -> const false
    | Neq (Refine { excluded; _ }, Int (T n)) when Set.mem excluded (Literal (Int n)) -> const true
    | Lt (Int (T x), Int (T y)) -> const (x < y)
    | Lt (Int x, Int y) when identical_int x y -> const false
    | Lt (a, b) when same_var a b -> const false
    | Lte (Int (T x), Int (T y)) -> const (x <= y)
    | Lte (Int x, Int y) when identical_int x y -> const true
    | Lte (a, b) when same_var a b -> const true
    | Gt (Int (T x), Int (T y)) -> const (x > y)
    | Gt (Int x, Int y) when identical_int x y -> const false
    | Gt (a, b) when same_var a b -> const false
    | Gte (Int (T x), Int (T y)) -> const (x >= y)
    | Gte (Int x, Int y) when identical_int x y -> const true
    | Gte (a, b) when same_var a b -> const true
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
    (* No x % x -> 0 or x / x -> 1: both trap at x = 0. *)
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

module Value1 = struct
  include Value0

  let rec refine value ~excluded =
    let value, excluded =
      match value with
      | Refine { value; excluded = prior } -> value, Set.union prior excluded
      | value -> value, excluded
    in
    let matching_shape label (excluded : Pattern.Excluded.t) =
      match excluded with
      | Constructor { label = l; payload = Some _ } -> Ident.Label.equal l label
      | Literal _ | Constructor { payload = None; _ } -> false
    in
    match value with
    | Bottom -> Bottom
    | Constructor { label; payload = Some payload }
      when Set.exists excluded ~f:(matching_shape label) ->
      (* Push payload refinements into the payload subtree, which keeps
         payload reduction and match rebuilds at their fixpoint. *)
      let pushed, rest = Set.partition_tf excluded ~f:(matching_shape label) in
      let payload_excluded =
        Set.fold pushed ~init:Pattern.Excluded.Set.empty ~f:(fun acc excluded ->
          match excluded with
          | Constructor { payload = Some shape; _ } -> Set.add acc shape
          | Literal _ | Constructor { payload = None; _ } -> acc)
      in
      let payload = refine payload ~excluded:payload_excluded in
      refine (constructor ~label ~payload:(Some payload)) ~excluded:rest
    | value ->
      with_return (fun { return } ->
        let excluded =
          Set.filter excluded ~f:(fun excluded ->
            match Pattern.Excluded.is_excluded excluded value with
            | Known true -> return Bottom
            | Known false -> false
            | Unknown -> true)
        in
        if Set.is_empty excluded then value else collapse value ~excluded)
  ;;

  let refine_branch ~scrutinee ~pattern ~refuted =
    Pattern.specialize
      pattern
      ~scrutinee:(refine scrutinee ~excluded:(Pattern.all_excludes refuted))
  ;;

  let identical = identical

  let rec rewrite_value (value : value) ~target ~replacement =
    let rewrite value = rewrite_value value ~target ~replacement in
    if identical value target
    then replacement
    else (
      match value with
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
           (match rewrite b with
            | Int (T 0L) -> value
            | b -> Int.div (rewrite a) b)
         | Mod (a, b) ->
           (match rewrite b with
            | Int (T n) when Int64.(n <= 0L) -> value
            | b -> Int.mod_ (rewrite a) b)
         | Neg a -> Int.neg (rewrite a))
      | Type ty -> Type (rewrite_ty ty ~target ~replacement)
      | Tuple elts -> Value0.tuple (Nonempty_list.map elts ~f:rewrite)
      | Inject { label; ty } -> Value0.inject ~ty:(rewrite ty) ~label
      | Constructor { label; payload } ->
        Value0.constructor ~label ~payload:(Option.map payload ~f:rewrite)
      | Apply { fn; arg } -> Value0.apply ~fn:(rewrite fn) ~arg:(rewrite arg)
      | Proj { tuple = subject; index } -> Value0.proj (rewrite subject) index
      | Payload { variant; label } -> Value0.payload (rewrite variant) ~label
      | Refine { value; excluded } -> refine (rewrite value) ~excluded
      | Match { scrutinee = match_scrutinee; arms } ->
        match_
          ~scrutinee:(rewrite match_scrutinee)
          ~arms:(Nonempty_list.map arms ~f:(fun (pattern, leaf) -> pattern, rewrite leaf)))

  and rewrite_ty (ty : ty) ~target ~replacement =
    let rewrite value = rewrite_value value ~target ~replacement in
    match ty with
    | Unit | Bool | Int | Type -> ty
    | Arrow { arg_ty; arg_mode; ret_ty; ret_mode } ->
      Arrow { arg_ty = rewrite arg_ty; arg_mode; ret_ty = rewrite ret_ty; ret_mode }
    (* Dependent returns capture their env; substitution stops at the binder. *)
    | Pi { arg_ty; arg_mode; ret_ty; ret_mode } ->
      Pi { arg_ty = rewrite arg_ty; arg_mode; ret_ty; ret_mode }
    | Tuple elts -> Tuple (Nonempty_list.map elts ~f:rewrite)
    | Variant constructors -> Variant (Map.map constructors ~f:(Option.map ~f:rewrite))

  and rewrite value ~target ~replacement =
    if identical replacement target then value else rewrite_value value ~target ~replacement

  and match_ ~scrutinee ~arms =
    match scrutinee with
    | Bottom -> Bottom
    | scrutinee ->
      (* Each arm's leaf is specialized to its implied scrutinee. Arms whose specialized
         bodies are [Bottom] are dead — while dead source-level branches are errors, the
         value lattice will explore dead branches. *)
      let arms =
        Pattern.with_refuted arms
        |> List.filter_map ~f:(fun (refuted, pattern, leaf) ->
          match
            rewrite leaf ~target:scrutinee ~replacement:(refine_branch ~scrutinee ~pattern ~refuted)
          with
          | Bottom -> None
          | specialized -> Some (pattern, (leaf, specialized)))
      in
      (* Matches are exhaustive; a sole surviving arm is unconditional. Selection must
         return the *unspecialized* leaf: an arm's facts hold only relative to this
         match, and values beside the collapse were never rewritten with them. *)
      let rec select : _ -> value Or_unknown.t = function
        | [] -> Known Bottom
        | [ (_, (leaf, _)) ] -> Known leaf
        | (pattern, (leaf, _)) :: rest ->
          (match Pattern.matches scrutinee pattern with
           | Match _ -> Known leaf
           | No_match -> select rest
           | Unknown -> Unknown)
      in
      (match select arms with
       | Known value -> value
       | Unknown ->
         Match
           { scrutinee
           ; arms =
               List.map arms ~f:(fun (pattern, (_, specialized)) -> pattern, specialized)
               |> Nonempty_list.of_list_exn
           })
  ;;

  let rec if_ ~loc ~(cond : value) ~then_ ~else_ : value =
    match cond with
    | Bool (Not cond) -> if_ ~loc ~cond ~then_:else_ ~else_:then_
    | Bool (Neq (a, b)) -> if_ ~loc ~cond:(Bool (Eq (a, b))) ~then_:else_ ~else_:then_
    | cond ->
      match_
        ~scrutinee:cond
        ~arms:
          (Nonempty_list.create
             ((Literal { value = Bool true; loc } : Dst.Expr.pattern), then_)
             [ (Literal { value = Bool false; loc } : Dst.Expr.pattern), else_ ])
  ;;
end

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
  module Concrete = struct
    include Concrete

    let rec of_value (v : value) : t option =
      match v with
      | Unit -> Some Unit
      | Bool (T b) -> Some (Bool b)
      | Int (T i) -> Some (Int i)
      | Tuple elts ->
        Nonempty_list.map elts ~f:of_value
        |> Nonempty_list.to_list
        |> Option.all
        |> Option.map ~f:(fun elts -> Concrete.Tuple (Nonempty_list.of_list_exn elts))
      | Closure closure -> Some (Closure closure.hash)
      | Binder binder -> Some (Closure binder.hash)
      | Prim prim -> Some (Prim (Prim prim))
      | Type Unit -> Some (Prim (Type Unit))
      | Type Bool -> Some (Prim (Type Bool))
      | Type Int -> Some (Prim (Type Int))
      | Type Type -> Some (Prim (Type Type))
      | Type (Arrow { arg_ty; arg_mode; ret_ty; ret_mode })
      | Type (Pi { arg_ty; arg_mode; ret_ty = T { ty = ret_ty; _ }; ret_mode }) ->
        let%bind arg = of_value arg_ty in
        let%map ret = of_value ret_ty in
        Concrete.Arrow { arg; arg_mode; ret; ret_mode }
      | Type (Tuple elts) ->
        let%map elts = Nonempty_list.map elts ~f:of_value |> Nonempty_list.to_list |> Option.all in
        Concrete.Tuple_t (Nonempty_list.of_list_exn elts)
      | Type (Variant constructors) ->
        with_return (fun { return } ->
          Some
            (Concrete.Variant_t
               (Core.Map.map constructors ~f:(fun payload ->
                  Option.map payload ~f:(fun payload ->
                    match of_value payload with
                    | Some payload -> payload
                    | None -> return None)))))
      | External { symbol; _ } -> Some (External symbol)
      | Inject { label; ty } ->
        let%map ty = of_value ty in
        Concrete.Inject { label; ty }
      | Constructor { label; payload = None } ->
        Some (Concrete.Constructor { label; payload = None })
      | Constructor { label; payload = Some payload } ->
        let%map payload = of_value payload in
        Concrete.Constructor { label; payload = Some payload }
      | Bottom | Bool _ | Int _ | Var _ | Apply _ | Proj _ | Payload _ | Match _ | Refine _
      | Type (Pi _) -> None
    ;;
  end

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
        ; arms : (Dst.Expr.pattern * t) Nonempty_list.t
        }
    | Refine of
        { value : t
        ; excluded : Set.M(Excluded0).t
        }
    | External of
        { symbol : string
        ; ty : t
        }
    | Prim of Builtin0.Prim.t
  [@@deriving sexp]

  include Value1

  let ty = function
    | Type ty -> ty
    | value -> raise_s [%message "Bug: expected concrete type" (value : t)]
  ;;

  let of_literal : Dst.Literal.t -> value = function
    | Unit -> Unit
    | Bool b -> Bool (T b)
    | Int i -> Int (T i)
  ;;

  module Eliminator = struct
    type t =
      | Apply of value
      | Proj of int
      | Payload of Ident.Label.t

    let rec peel (value : value) frames =
      match value with
      | Apply { fn; arg } -> peel fn (Apply arg :: frames)
      | Proj { tuple; index } -> peel tuple (Proj index :: frames)
      | Payload { variant; label } -> peel variant (Payload label :: frames)
      | ( Bottom
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
        | Refine _
        | External _
        | Prim _ ) as head -> head, frames
    ;;

    let unpeel leaf frames =
      List.fold frames ~init:leaf ~f:(fun value (frame : t) ->
        match frame with
        | Apply arg -> apply ~fn:value ~arg
        | Proj index -> proj value index
        | Payload label -> payload value ~label)
    ;;
  end
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

  let mono ty : t = T { ty; memo = Hashtbl.create (module Concrete) }

  let rec is_concrete_value : value -> _ = function
    | Unit | Closure _ | Binder _ | External _ | Prim _ -> true
    | Bool b -> is_concrete_bool b
    | Int i -> is_concrete_int i
    | Type ty -> is_concrete_ty ty
    | Tuple elts -> Nonempty_list.for_all elts ~f:is_concrete_value
    | Inject { ty; _ } -> is_concrete_value ty
    | Constructor { payload; _ } -> Option.for_all payload ~f:is_concrete_value
    | Bottom | Var _ | Apply _ | Proj _ | Payload _ | Match _ | Refine _ -> false

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
    | Variant constructors ->
      Map.for_all constructors ~f:(fun payload -> Option.for_all payload ~f:is_concrete_value)

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
      , Arrow { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode }
      ) ->
      let arg_mode = Modes.meet a_arg_mode b_arg_mode in
      let ret_mode = Modes.join a_ret_mode b_ret_mode in
      let%bind arg_ty = meet_concrete_value a_arg_ty b_arg_ty in
      let%map ret_ty = join_concrete_value a_ret_ty b_ret_ty in
      Arrow { arg_ty; arg_mode; ret_ty; ret_mode }
    | ( Arrow { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
      , Pi { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } )
      ->
      let arg_mode = Modes.meet a_arg_mode b_arg_mode in
      let ret_mode = Modes.join a_ret_mode b_ret_mode in
      let%bind arg_ty = meet_concrete_value a_arg_ty b_arg_ty in
      let%map ret_ty = join_concrete_dependent (mono a_ret_ty) b_ret_ty in
      Pi { arg_ty; arg_mode; ret_ty = mono ret_ty; ret_mode }
    | ( Pi { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
      , Arrow { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode }
      ) ->
      let arg_mode = Modes.meet a_arg_mode b_arg_mode in
      let ret_mode = Modes.join a_ret_mode b_ret_mode in
      let%bind arg_ty = meet_concrete_value a_arg_ty b_arg_ty in
      let%map ret_ty = join_concrete_dependent a_ret_ty (mono b_ret_ty) in
      Pi { arg_ty; arg_mode; ret_ty = mono ret_ty; ret_mode }
    | ( Pi { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
      , Pi { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } )
      ->
      let arg_mode = Modes.meet a_arg_mode b_arg_mode in
      let ret_mode = Modes.join a_ret_mode b_ret_mode in
      let%bind arg_ty = meet_concrete_value a_arg_ty b_arg_ty in
      let%map ret_ty = join_concrete_dependent a_ret_ty b_ret_ty in
      Pi { arg_ty; arg_mode; ret_ty = mono ret_ty; ret_mode }
    | Variant a_ctors, Variant b_ctors ->
      Ty.unify_constructors ~f:join_concrete_value a_ctors b_ctors
    | (Unit | Bool | Int | Type | Arrow _ | Pi _ | Tuple _ | Variant _), _ -> None

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
      , Arrow { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode }
      ) ->
      let arg_mode = Modes.join a_arg_mode b_arg_mode in
      let ret_mode = Modes.meet a_ret_mode b_ret_mode in
      let%bind arg_ty = join_concrete_value a_arg_ty b_arg_ty in
      let%map ret_ty = meet_concrete_value a_ret_ty b_ret_ty in
      Arrow { arg_ty; arg_mode; ret_ty; ret_mode }
    | ( Arrow { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
      , Pi { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } )
      ->
      let arg_mode = Modes.join a_arg_mode b_arg_mode in
      let ret_mode = Modes.meet a_ret_mode b_ret_mode in
      let%bind arg_ty = join_concrete_value a_arg_ty b_arg_ty in
      let%map ret_ty = meet_concrete_dependent (mono a_ret_ty) b_ret_ty in
      Arrow { arg_ty; arg_mode; ret_ty; ret_mode }
    | ( Pi { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
      , Arrow { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode }
      ) ->
      let arg_mode = Modes.join a_arg_mode b_arg_mode in
      let ret_mode = Modes.meet a_ret_mode b_ret_mode in
      let%bind arg_ty = join_concrete_value a_arg_ty b_arg_ty in
      let%map ret_ty = meet_concrete_dependent a_ret_ty (mono b_ret_ty) in
      Arrow { arg_ty; arg_mode; ret_ty; ret_mode }
    | ( Pi { arg_ty = a_arg_ty; arg_mode = a_arg_mode; ret_ty = a_ret_ty; ret_mode = a_ret_mode }
      , Pi { arg_ty = b_arg_ty; arg_mode = b_arg_mode; ret_ty = b_ret_ty; ret_mode = b_ret_mode } )
      ->
      let arg_mode = Modes.join a_arg_mode b_arg_mode in
      let ret_mode = Modes.meet a_ret_mode b_ret_mode in
      let%bind arg_ty = join_concrete_value a_arg_ty b_arg_ty in
      let%map ret_ty = meet_concrete_dependent a_ret_ty b_ret_ty in
      Pi { arg_ty; arg_mode; ret_ty = mono ret_ty; ret_mode }
    | Variant a_ctors, Variant b_ctors ->
      Ty.unify_constructors ~f:meet_concrete_value a_ctors b_ctors
    | (Unit | Bool | Int | Type | Arrow _ | Pi _ | Tuple _ | Variant _), _ -> None

  and join_concrete_bool (a : vbool) (b : vbool) : vbool option =
    match a, b with
    | T a, T b when Core.Bool.equal a b -> Some (T a : vbool)
    | _ -> None

  and meet_concrete_bool a b = join_concrete_bool a b

  and join_concrete_int (a : vint) (b : vint) : vint option =
    match a, b with
    | T a, T b when Int64.equal a b -> Some (T a : vint)
    | _ -> None

  and meet_concrete_int a b = join_concrete_int a b

  and join_concrete_value (a : value) (b : value) : value option =
    match a, b with
    | a, Bottom -> Some a
    | Bottom, b -> Some b
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
      let%bind arms = Pattern.map2_arms a_arms b_arms ~f:join_concrete_value in
      let%map scrutinee = join_concrete_value a_scrutinee b_scrutinee in
      Match { scrutinee; arms }
    | Apply { fn = a_fn; arg = a_arg }, Apply { fn = b_fn; arg = b_arg } ->
      let%bind fn = join_concrete_value a_fn b_fn in
      let%map arg = join_concrete_value a_arg b_arg in
      Apply { fn; arg }
    | Proj a, Proj b when a.index = b.index ->
      let%map tuple = join_concrete_value a.tuple b.tuple in
      (Proj { tuple; index = a.index } : value)
    | Payload a, Payload b when Ident.Label.equal a.label b.label ->
      let%map variant = join_concrete_value a.variant b.variant in
      (Payload { variant; label = a.label } : value)
    | Refine a, Refine b ->
      let%map value = join_concrete_value a.value b.value in
      Value.refine value ~excluded:(Set.inter a.excluded b.excluded)
    | Refine refined, other | other, Refine refined -> join_concrete_value refined.value other
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
    | Inject a, Inject b when Ident.Label.equal a.label b.label ->
      let%map ty = join_concrete_value a.ty b.ty in
      Inject { ty; label = a.label }
    | Constructor a, Constructor b when Ident.Label.equal a.label b.label ->
      (match a.payload, b.payload with
       | None, None -> Some (Constructor { label = a.label; payload = None })
       | Some a_payload, Some b_payload ->
         let%map payload = join_concrete_value a_payload b_payload in
         Constructor { label = a.label; payload = Some payload }
       | None, Some _ | Some _, None -> None)
    | ( ( Unit
        | Bool _
        | Int _
        | Type _
        | Apply _
        | Proj _
        | Payload _
        | Match _
        | Var _
        | Tuple _
        | Inject _
        | Constructor _
        | Closure _
        | Binder _
        | External _
        | Prim _ )
      , _ ) -> None

  and meet_concrete_value (a : value) (b : value) : value option =
    match a, b with
    | Bottom, _ | _, Bottom -> Some Bottom
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
      let%bind arms = Pattern.map2_arms a_arms b_arms ~f:meet_concrete_value in
      let%map scrutinee = meet_concrete_value a_scrutinee b_scrutinee in
      Match { scrutinee; arms }
    | Apply { fn = a_fn; arg = a_arg }, Apply { fn = b_fn; arg = b_arg } ->
      let%bind fn = meet_concrete_value a_fn b_fn in
      let%map arg = meet_concrete_value a_arg b_arg in
      Apply { fn; arg }
    | Proj a, Proj b when a.index = b.index ->
      let%map tuple = meet_concrete_value a.tuple b.tuple in
      (Proj { tuple; index = a.index } : value)
    | Payload a, Payload b when Ident.Label.equal a.label b.label ->
      let%map variant = meet_concrete_value a.variant b.variant in
      (Payload { variant; label = a.label } : value)
    | Refine a, Refine b ->
      let%map value = meet_concrete_value a.value b.value in
      Value.refine value ~excluded:(Set.union a.excluded b.excluded)
    | Refine refined, other | other, Refine refined ->
      let%map value = meet_concrete_value refined.value other in
      Value.refine value ~excluded:refined.excluded
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
    | Inject a, Inject b when Ident.Label.equal a.label b.label ->
      let%map ty = meet_concrete_value a.ty b.ty in
      Inject { ty; label = a.label }
    | Constructor a, Constructor b when Ident.Label.equal a.label b.label ->
      (match a.payload, b.payload with
       | None, None -> Some (Constructor { label = a.label; payload = None })
       | Some a_payload, Some b_payload ->
         let%map payload = meet_concrete_value a_payload b_payload in
         Constructor { label = a.label; payload = Some payload }
       | None, Some _ | Some _, None -> None)
    | ( ( Unit
        | Bool _
        | Int _
        | Type _
        | Apply _
        | Proj _
        | Payload _
        | Match _
        | Var _
        | Tuple _
        | Inject _
        | Constructor _
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
    | Payload_get { variant; _ } | Tag_test { variant; _ } -> free_vars ?monos variant
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
    | Tag_test t -> Tag_test { t with mode }
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
