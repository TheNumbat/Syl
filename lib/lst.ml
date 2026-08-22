open! Core

module Path = struct
  module Entry = struct
    type t =
      | Id of Ident.t
      | Key of int
    [@@deriving sexp_of, compare, hash, equal]
  end

  module T = struct
    type t = Entry.t list [@@deriving sexp_of, compare, equal, hash]
  end

  open Entry
  include T
  include Hashable.Make_plain (T)
  include Comparable.Make_plain (T)

  let empty = []
  let id id = [ Id id ]
  let with_id t id = Id id :: t
  let with_key t key = Key key :: t

  let with_ ?id ?key t =
    let t = Option.value_map id ~default:t ~f:(with_id t) in
    Option.value_map key ~default:t ~f:(with_key t)
  ;;
end

module Ty = struct
  type t =
    | Unit
    | Bool
    | Int
    | Type
    | Tuple of t Nonempty_list.t
    | Variant of t option Ident.Label.Map.t
    | Ref
    | Env
    | Closure of
        { arg_ty : t
        ; ret_ty : t
        }
  [@@deriving sexp_of]

  let align_to x ~align =
    assert (Int.is_pow2 align);
    (x + align - 1) land lnot (align - 1)
  ;;

  (* TODO smaller tags, only tag when no payloads, zero-size when one constructor *)
  let tag_size = 8
  let tag_align = 8

  let rec size_align : t -> int * int = function
    | Unit | Type -> 0, 1
    | Bool -> 1, 1
    | Int -> 8, 8
    | Env -> 8, 8
    | Ref -> 8, 8
    | Closure _ -> 16, 8
    | Tuple elts ->
      let elts = Nonempty_list.to_list elts |> List.map ~f:size_align in
      if List.for_all elts ~f:(fun (size, _) -> size = 0) then 0, 1 else tuple_size_align elts
    | Variant constructors ->
      let payload_size, payload_align = payload_size_align constructors in
      let payload_offset = align_to tag_size ~align:payload_align in
      let align = Int.max tag_align payload_align in
      align_to (payload_offset + payload_size) ~align, align

  (* mirrors the recursive [syl_tuple] *)
  and tuple_size_align = function
    | [] -> 1, 1
    | (size, elt_align) :: rest ->
      (match size with
       | 0 -> tuple_size_align rest
       | size ->
         (match rest with
          | [] -> size, elt_align
          | _ :: _ ->
            let rest_size, rest_align = tuple_size_align rest in
            let align = Int.max elt_align rest_align in
            align_to (align_to size ~align:rest_align + rest_size) ~align, align))

  and payload_size_align constructors =
    Map.fold constructors ~init:(0, 1) ~f:(fun ~key:_ ~data (size_acc, align_acc) ->
      match data with
      | None -> size_acc, align_acc
      | Some ty ->
        let size, align = size_align ty in
        Int.max size_acc size, if size = 0 then align_acc else Int.max align_acc align)
  ;;

  let size_in_mem t = fst (size_align t)

  let align_in_mem (t : t) =
    match t with
    | Unit | Type -> raise_s [%message "Bug: undefined alignment"]
    | Bool | Int | Env | Ref | Closure _ | Tuple _ | Variant _ -> snd (size_align t)
  ;;

  let payload_size_in_mem constructors = fst (payload_size_align constructors)
  let payload_align_in_mem constructors = snd (payload_size_align constructors)
  let is_zero_size t = size_in_mem t = 0
end

module Env = struct
  type entry =
    { path : Path.t
    ; ty : Ty.t
    ; offset : int
    }
  [@@deriving sexp_of]

  type t =
    { entries : entry array
    ; length : int
    }
  [@@deriving sexp_of]

  let empty = { entries = [||]; length = 0 }
end

module Expr = struct
  type nonrec t =
    | Scalar of
        { value : Sst.Scalar.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Make_env of
        { env : Env.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Make_env_rec of
        { length : int
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Fill_env_rec of
        { path : Path.t
        ; env : Env.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Make_closure of
        { body : Path.t
        ; env : Path.t option
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Make_tuple of
        { elts : (Path.t * Ty.t) array
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Make_variant of
        { label : Ident.Label.t
        ; payload : (Path.t * Ty.t) option
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Make_ref of
        { payload : Path.t
        ; payload_ty : Ty.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Ref_get of
        { ref : Path.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Apply_closure of
        { fn : Path.t
        ; arg : Path.t
        ; arg_ty : Ty.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Apply_thunk of
        { fn : Path.t
        ; env : Path.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Apply_proc of
        { fn : Path.t
        ; arg : Path.t
        ; arg_ty : Ty.t
        ; env : Path.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Extcall of
        { symbol : string
        ; arg : Path.t
        ; arg_ty : Ty.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Ident of
        { path : Path.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Tuple_get of
        { tuple : Path.t
        ; index : int
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Payload_get of
        { variant : Path.t
        ; label : Ident.Label.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Tag_test of
        { variant : Path.t
        ; variant_ty : Ty.t
        ; label : Ident.Label.t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
  [@@deriving sexp_of]

  let ty = function
    | Make_env { ty; _ }
    | Make_env_rec { ty; _ }
    | Fill_env_rec { ty; _ }
    | Make_closure { ty; _ }
    | Apply_closure { ty; _ }
    | Apply_thunk { ty; _ }
    | Apply_proc { ty; _ }
    | Scalar { ty; _ }
    | Make_tuple { ty; _ }
    | Make_variant { ty; _ }
    | Make_ref { ty; _ }
    | Ref_get { ty; _ }
    | Ident { ty; _ }
    | Tuple_get { ty; _ }
    | Payload_get { ty; _ }
    | Tag_test { ty; _ } -> ty
    | Extcall { ty; _ } -> ty
  ;;

  let loc = function
    | Make_env { loc; _ }
    | Make_env_rec { loc; _ }
    | Fill_env_rec { loc; _ }
    | Make_closure { loc; _ }
    | Apply_closure { loc; _ }
    | Apply_thunk { loc; _ }
    | Apply_proc { loc; _ }
    | Scalar { loc; _ }
    | Make_tuple { loc; _ }
    | Make_variant { loc; _ }
    | Make_ref { loc; _ }
    | Ref_get { loc; _ }
    | Ident { loc; _ }
    | Tuple_get { loc; _ }
    | Payload_get { loc; _ }
    | Tag_test { loc; _ } -> loc
    | Extcall { loc; _ } -> loc
  ;;

  let with_ty t ty =
    match t with
    | Make_env expr -> Make_env { expr with ty }
    | Make_env_rec expr -> Make_env_rec { expr with ty }
    | Fill_env_rec expr -> Fill_env_rec { expr with ty }
    | Make_closure expr -> Make_closure { expr with ty }
    | Apply_closure expr -> Apply_closure { expr with ty }
    | Apply_thunk expr -> Apply_thunk { expr with ty }
    | Apply_proc expr -> Apply_proc { expr with ty }
    | Scalar expr -> Scalar { expr with ty }
    | Make_tuple expr -> Make_tuple { expr with ty }
    | Make_variant expr -> Make_variant { expr with ty }
    | Make_ref expr -> Make_ref { expr with ty }
    | Ref_get expr -> Ref_get { expr with ty }
    | Ident expr -> Ident { expr with ty }
    | Tuple_get expr -> Tuple_get { expr with ty }
    | Payload_get expr -> Payload_get { expr with ty }
    | Tag_test expr -> Tag_test { expr with ty }
    | Extcall expr -> Extcall { expr with ty }
  ;;
end

module Block = struct
  type case =
    { bindings : (Path.t * Ty.t) array
    ; block : t
    }
  [@@deriving sexp_of]

  and tree =
    | Leaf of
        { case : int
        ; bindings : (Path.t * t) array
        }
    | Split of
        { cond : t
        ; then_ : tree
        ; else_ : tree
        }
  [@@deriving sexp_of]

  and t =
    | Block of
        { bindings : (Path.t * t) array
        ; return : Expr.t
        }
    | If of
        { cond : Expr.t
        ; then_ : t
        ; else_ : t
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
    | Match of
        { tree : tree
        ; cases : case array
        ; ty : Ty.t
        ; loc : Lex.Location.t
        }
  [@@deriving sexp_of]

  let ty = function
    | Block { return; _ } -> Expr.ty return
    | If { ty; _ } | Match { ty; _ } -> ty
  ;;

  let loc = function
    | Block { return; _ } -> Expr.loc return
    | If { loc; _ } | Match { loc; _ } -> loc
  ;;
end

module Proc = struct
  type t =
    | Closure of
        { path : Path.t
        ; arg : Path.t
        ; arg_ty : Ty.t
        ; env : Env.t
        ; body : Block.t
        ; loc : Lex.Location.t
        }
    | Thunk of
        { path : Path.t
        ; env : Env.t
        ; body : Block.t
        ; loc : Lex.Location.t
        }
    | External of
        { path : Path.t
        ; symbol : string
        ; arg_ty : Ty.t
        ; ret_ty : Ty.t
        ; loc : Lex.Location.t
        }
  [@@deriving sexp_of]
end

module Program = struct
  type t =
    { procs : Proc.t array
    ; bindings : (Path.t * Block.t) array
    }
  [@@deriving sexp_of]
end
