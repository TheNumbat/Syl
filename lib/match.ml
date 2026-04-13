open! Core
module Literal = Cst.Literal

let skip col j = if j < col then j else j + 1

module Pattern = struct
  type t = Dst.Expr.pattern =
    | Var of
        { id : Ident.t
        ; loc : Lex.Location.t
        }
    | Literal of
        { value : Literal.t
        ; loc : Lex.Location.t
        }
    | Tuple of
        { elts : t Nonempty_list.t
        ; loc : Lex.Location.t
        }
    | Or of
        { left : t
        ; right : t
        ; loc : Lex.Location.t
        }
  [@@deriving sexp]

  let wild = Var { id = Ident.create Ident.Raw.anon ~stamp:0; loc = Lex.Location.empty }

  let is_wild : t -> bool = function
    | Var _ -> true
    | Literal _ | Tuple _ -> false
    | Or _ -> raise_s [%message "Bug: unexpanded or pattern"]
  ;;

  let rec expand pattern =
    match pattern with
    | Var _ | Literal _ -> [ pattern ]
    | Or { left; right; _ } -> expand left @ expand right
    | Tuple { elts; loc } ->
      let rec product acc = function
        | [] -> [ List.rev acc ]
        | choices :: rest -> List.concat_map choices ~f:(fun choice -> product (choice :: acc) rest)
      in
      Nonempty_list.map elts ~f:expand
      |> Nonempty_list.to_list
      |> product []
      |> List.map ~f:(fun elts -> Tuple { elts = Nonempty_list.of_list_exn elts; loc })
  ;;
end

module Case = struct
  type t =
    { pattern : Pattern.t
    ; body : Tst.Expr.t
    ; bindings : Tst.Desc.t Ident.Map.t
    }
  [@@deriving sexp]
end

module Tree = struct
  module Constructor = struct
    module T = struct
      type t =
        | Literal of Literal.t
        | Tuple of int
      [@@deriving sexp, compare, hash]
    end

    include T
    include Comparator.Make (T)
    include Hashable.Make (T)

    let arity = function
      | Literal _ -> 0
      | Tuple arity -> arity
    ;;

    let members =
      let unit = Some (Vec.of_array [| Literal Unit |]) in
      let bool = Some (Vec.of_array [| Literal (Bool true); Literal (Bool false) |]) in
      fun (ty : Tst.Ty.t) ->
        match ty with
        | Unit -> unit
        | Bool -> bool
        | Tuple elt_tys -> Some (Vec.of_array [| Tuple (Nonempty_list.length elt_tys) |])
        | Int | Arrow _ | Pi _ | Type -> None
    ;;

    let covers ts ty =
      match members ty with
      | None -> false
      | Some all ->
        let set = Hash_set.of_list (Vec.to_list ts) in
        Vec.for_all all ~f:(Core.Hash_set.mem set)
    ;;
  end

  module Occurrence = struct
    type t =
      { path : int Vec.t
      ; ty : Tst.Ty.t
      }
    [@@deriving sexp]

    let append sub rest ~exclude =
      let sub_len = Array.length sub in
      let rest_len = Array.length rest - 1 in
      Array.init (sub_len + rest_len) ~f:(fun i ->
        if i < sub_len then sub.(i) else rest.(skip exclude (i - sub_len)))
    ;;

    let remove occurrences col =
      let n = Array.length occurrences - 1 in
      if n <= 0 then None else Some (Array.init n ~f:(fun i -> occurrences.(skip col i)))
    ;;

    let deepen occurrence (constructor : Constructor.t) =
      match constructor with
      | Literal _ -> [||]
      | Tuple arity ->
        (match occurrence.ty with
         | Tuple elt_tys when Nonempty_list.length elt_tys = arity ->
           Array.of_list (Nonempty_list.to_list elt_tys)
           |> Array.mapi ~f:(fun index ty ->
             let path = Vec.copy occurrence.path in
             Vec.push_back path index;
             { path; ty = Tst.Value.ty ty })
         | ty -> raise_s [%message "Bug: expected tuple" (arity : int) (ty : Tst.Ty.t)])
    ;;
  end

  module Binding = struct
    type t =
      { occurrence : Occurrence.t
      ; desc : Tst.Desc.t
      }
    [@@deriving sexp]
  end

  type t =
    | Fail
    | Leaf of
        { action : Tst.Expr.t
        ; bindings : Binding.t Ident.Map.t
        }
    | Switch of
        { occurrence : Occurrence.t
        ; cases : (Constructor.t * t) array
        ; default : t option
        }
  [@@deriving sexp]

  let rec exhaustive = function
    | Fail -> false
    | Leaf _ -> true
    | Switch { cases; default; _ } ->
      Array.for_all cases ~f:(fun (_, t) -> exhaustive t) && Option.for_all default ~f:exhaustive
  ;;
end

module Result = struct
  module Missing = struct
    module T = struct
      type t =
        | Wildcard
        | Literal of Literal.t
        | Tuple of t list
        | Or of t list
      [@@deriving sexp, compare, hash]
    end

    include T
    include Comparator.Make (T)
    include Hashable.Make (T)

    let constructor (constructor : Tree.Constructor.t) args =
      match constructor with
      | Literal value -> Literal value
      | Tuple _ -> Tuple args
    ;;

    let dedup missing =
      let seen = Hash_set.create () in
      List.filter missing ~f:(fun item ->
        if Core.Hash_set.mem seen item
        then false
        else (
          Core.Hash_set.add seen item;
          true))
    ;;
  end

  type t =
    { tree : Tree.t
    ; redundant : Pattern.t list
    ; missing : Missing.t list
    }
  [@@deriving sexp]
end

module Row = struct
  type t =
    { patterns : Pattern.t array
    ; bindings : Tst.Desc.t Ident.Map.t
    ; body : Tst.Expr.t
    ; id : int
    ; bound : Tree.Binding.t Ident.Map.t
    }

  let all_wild t = Array.for_all t.patterns ~f:Pattern.is_wild

  let head ts ~col =
    let seen = Hash_set.create (module Tree.Constructor) in
    let result = Vec.create () in
    Vec.iter ts ~f:(fun row ->
      let constructor : Tree.Constructor.t option =
        match row.patterns.(col) with
        | Var _ -> None
        | Literal { value; _ } -> Some (Literal value)
        | Tuple { elts; _ } -> Some (Tuple (Nonempty_list.length elts))
        | Or _ -> raise_s [%message "Bug: unexpanded or pattern"]
      in
      Option.iter constructor ~f:(fun constructor ->
        if not (Hash_set.mem seen constructor)
        then (
          Hash_set.add seen constructor;
          Vec.push_back result constructor)));
    result
  ;;

  let select ts =
    let width = Array.length (Vec.get ts 0).patterns in
    let score ~col =
      let needed = not (Pattern.is_wild (Vec.get ts 0).patterns.(col)) in
      let defaults = ref 0 in
      let constructors = Tree.Constructor.Hash_set.create () in
      Vec.iter ts ~f:(fun row ->
        match row.patterns.(col) with
        | Var _ -> incr defaults
        | Literal { value; _ } -> Hash_set.add constructors (Literal value)
        | Tuple { elts; _ } -> Hash_set.add constructors (Tuple (Nonempty_list.length elts))
        | Or _ -> raise_s [%message "Bug: unexpanded or pattern"]);
      (if needed then 1 else 0), !defaults, Hash_set.length constructors
    in
    let better (n1, d1, b1) (n2, d2, b2) =
      n1 > n2 || (n1 = n2 && (d1 < d2 || (d1 = d2 && b1 < b2)))
    in
    let best_col = ref 0 in
    let best_score = ref (score ~col:0) in
    for col = 1 to width - 1 do
      let s = score ~col in
      if better s !best_score
      then (
        best_col := col;
        best_score := s)
    done;
    !best_col
  ;;
end

let bind bindings bound occurrence id =
  if Ident.is_anon id || Map.mem bound id
  then bound
  else (
    match Map.find bindings id with
    | Some desc -> Map.set bound ~key:id ~data:{ Tree.Binding.occurrence; desc }
    | None -> raise_s [%message "Bug: missing desc" (id : Ident.t)])
;;

let materialize (row : Row.t) (occurrences : Tree.Occurrence.t array) =
  Array.foldi row.patterns ~init:row.bound ~f:(fun i bound pattern ->
    let occurrence = occurrences.(i) in
    match pattern with
    | Var { id; _ } -> bind row.bindings bound occurrence id
    | _ -> raise_s [%message "Bug: expected wild row"])
;;

let specialize constructor occurrence col (rows : Row.t Vec.t) =
  let arity = Tree.Constructor.arity constructor in
  let result = Vec.create () in
  Vec.iter rows ~f:(fun row ->
    match row.patterns.(col), constructor with
    | Var { id; _ }, _ ->
      let bound = bind row.bindings row.bound occurrence id in
      let rest_len = Array.length row.patterns - 1 in
      let patterns =
        Array.init (arity + rest_len) ~f:(fun i ->
          if i < arity then Pattern.wild else row.patterns.(skip col (i - arity)))
      in
      Vec.push_back result { row with patterns; bound }
    | Literal { value = lhs; _ }, Literal rhs when Literal.equal lhs rhs ->
      let patterns =
        Array.init (Array.length row.patterns - 1) ~f:(fun i -> row.patterns.(skip col i))
      in
      Vec.push_back result { row with patterns }
    | Tuple { elts; _ }, Tuple a when Nonempty_list.length elts = a ->
      let elts = Array.of_list (Nonempty_list.to_list elts) in
      let rest_len = Array.length row.patterns - 1 in
      let patterns =
        Array.init (a + rest_len) ~f:(fun i ->
          if i < a then elts.(i) else row.patterns.(skip col (i - a)))
      in
      Vec.push_back result { row with patterns }
    | (Literal _ | Tuple _), _ -> ()
    | Or _, _ -> raise_s [%message "Bug: unexpanded or pattern"]);
  result
;;

let default occurrence col (rows : Row.t Vec.t) =
  let result = Vec.create () in
  Vec.iter rows ~f:(fun row ->
    match row.patterns.(col) with
    | Var { id; _ } ->
      let bound = bind row.bindings row.bound occurrence id in
      let patterns =
        Array.init (Array.length row.patterns - 1) ~f:(fun i -> row.patterns.(skip col i))
      in
      Vec.push_back result { row with patterns; bound }
    | Literal _ | Tuple _ -> ()
    | Or _ -> raise_s [%message "Bug: unexpanded or pattern"]);
  result
;;

let rec compile_matrix reached (occurrences : Tree.Occurrence.t array) (rows : Row.t Vec.t) : Tree.t
  =
  if Vec.is_empty rows
  then Fail
  else if Row.all_wild (Vec.get rows 0)
  then (
    let row = Vec.get rows 0 in
    Hash_set.add reached row.id;
    Leaf { action = row.body; bindings = materialize row occurrences })
  else (
    let col = Row.select rows in
    let occurrence = occurrences.(col) in
    let constructors = Row.head rows ~col in
    let cases =
      Array.init (Vec.length constructors) ~f:(fun i ->
        let constructor = Vec.get constructors i in
        let sub = Tree.Occurrence.deepen occurrence constructor in
        let occ = Tree.Occurrence.append sub occurrences ~exclude:col in
        let body = compile_matrix reached occ (specialize constructor occurrence col rows) in
        constructor, body)
    in
    let default =
      if Tree.Constructor.covers constructors occurrence.ty
      then None
      else
        let rest =
          Tree.Occurrence.remove occurrences col |> Option.value ~default:[||]
        in
        Some (compile_matrix reached rest (default occurrence col rows))
    in
    Switch { occurrence; cases; default })
;;

let rec enumerate_missing (occ : Tree.Occurrence.t) : Result.Missing.t =
  match Tree.Constructor.members occ.ty with
  | None -> Wildcard
  | Some constructors ->
    let items =
      Vec.to_list constructors
      |> List.map ~f:(fun constructor ->
        let sub = Tree.Occurrence.deepen occ constructor in
        Result.Missing.constructor constructor (Array.to_list sub |> List.map ~f:enumerate_missing))
    in
    (match items with
     | [ single ] -> single
     | _ -> Or items)

and missing_matrix (occurrences : Tree.Occurrence.t array) (rows : Row.t Vec.t) =
  if Vec.is_empty rows
  then [ Array.to_list occurrences |> List.map ~f:enumerate_missing ]
  else if Row.all_wild (Vec.get rows 0)
  then []
  else (
    let col = Row.select rows in
    let occurrence = occurrences.(col) in
    match Tree.Constructor.members occurrence.ty with
    | Some constructors ->
      Vec.fold constructors ~init:[] ~f:(fun acc constructor ->
        let sub = Tree.Occurrence.deepen occurrence constructor in
        let occ = Tree.Occurrence.append sub occurrences ~exclude:col in
        let missing = missing_matrix occ (specialize constructor occurrence col rows) in
        List.fold missing ~init:acc ~f:(fun acc missing ->
          let args, rest = List.split_n missing (Tree.Constructor.arity constructor) in
          let value = Result.Missing.constructor constructor args in
          let before, after = List.split_n rest col in
          (before @ [ value ] @ after) :: acc))
      |> List.rev
    | None ->
      let rest =
        Tree.Occurrence.remove occurrences col |> Option.value ~default:[||]
      in
      missing_matrix rest (default occurrence col rows)
      |> List.map ~f:(fun missing ->
        let before, after = List.split_n missing col in
        before @ [ Result.Missing.Wildcard ] @ after))
;;

let compile ~(ty : Tst.Value.t) (cases : Case.t Nonempty_list.t) =
  let next_id = ref 0 in
  let rows =
    Nonempty_list.to_list cases
    |> Sequence.of_list
    |> Sequence.concat_mapi ~f:(fun _source case ->
      Pattern.expand case.pattern
      |> Sequence.of_list
      |> Sequence.map ~f:(fun pattern ->
        let id = !next_id in
        incr next_id;
        { Row.patterns = [| pattern |]
        ; bindings = case.bindings
        ; body = case.body
        ; id
        ; bound = Ident.Map.empty
        }))
    |> Vec.of_sequence
  in
  let reached = Hash_set.create (module Int) in
  let occurrences = [| { Tree.Occurrence.path = Vec.create (); ty = Tst.Value.ty ty } |] in
  let tree = compile_matrix reached occurrences rows in
  let missing =
    if Tree.exhaustive tree
    then []
    else missing_matrix occurrences rows |> List.map ~f:List.hd_exn |> Result.Missing.dedup
  in
  let redundant =
    Vec.fold rows ~init:[] ~f:(fun acc row ->
      if Hash_set.mem reached row.id then acc else row.patterns.(0) :: acc)
    |> List.rev
  in
  { Result.tree; redundant; missing }
;;
