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
    | Constructor of
        { label : Ident.Label.t
        ; payload : t option
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
    | Literal _ | Constructor _ | Tuple _ -> false
    | Or _ -> raise_s [%message "Bug: unexpanded or pattern"]
  ;;

  let rec expand pattern =
    match pattern with
    | Var _ | Literal _ | Constructor { payload = None; _ } -> [ pattern ]
    | Constructor { label; payload = Some payload; loc } ->
      expand payload
      |> List.map ~f:(fun payload -> Constructor { label; payload = Some payload; loc })
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

module Tree = struct
  module Head = struct
    module T = struct
      type t =
        | Literal of Literal.t
        | Tuple of int
        | Constructor of
            { label : Ident.Label.t
            ; payload : bool
            }
      [@@deriving sexp, compare, hash]
    end

    include T
    include Comparator.Make (T)
    include Hashable.Make (T)

    let arity = function
      | Literal _ -> 0
      | Tuple arity -> arity
      | Constructor { payload; _ } -> if payload then 1 else 0
    ;;

    let members =
      let unit = Some (Vec.of_array [| Literal Unit |]) in
      let bool = Some (Vec.of_array [| Literal (Bool true); Literal (Bool false) |]) in
      fun (ty : Tst.Value.t) ->
        match ty with
        | Type Unit -> unit
        | Type Bool -> bool
        | Type (Tuple elt_tys) -> Some (Vec.of_array [| Tuple (Nonempty_list.length elt_tys) |])
        | Type (Variant constructors) ->
          Some
            (Map.to_alist constructors
             |> List.map ~f:(fun (label, payload) ->
               Constructor { label; payload = Option.is_some payload })
             |> Vec.of_list)
        | _ -> None
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
      { path : Tst.Pattern.Step.t Vec.t
      ; ty : Tst.Value.t
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

    (* Occurrence types are stored head-normalized: [members] and the dispatches here
       consult them syntactically. *)
    let deepen ~unfold occurrence (head : Head.t) =
      match head with
      | Literal _ | Constructor { payload = false; _ } -> [||]
      | Tuple arity ->
        (match occurrence.ty with
         | Type (Tuple elt_tys) when Nonempty_list.length elt_tys = arity ->
           Array.of_list (Nonempty_list.to_list elt_tys)
           |> Array.mapi ~f:(fun index ty ->
             let path = Vec.copy occurrence.path in
             Vec.push_back path (Tst.Pattern.Step.Index index);
             { path; ty = unfold ty })
         | ty -> raise_s [%message "Bug: expected tuple" (arity : int) (ty : Tst.Value.t)])
      | Constructor { label; payload = true } ->
        (match occurrence.ty with
         | Type (Variant constructors) ->
           (match Map.find constructors label with
            | Some (Some ty) ->
              let path = Vec.copy occurrence.path in
              Vec.push_back path (Tst.Pattern.Step.Payload label);
              [| { path; ty = unfold ty } |]
            | Some None | None ->
              raise_s
                [%message
                  "Bug: expected payload" (label : Ident.Label.t) (occurrence.ty : Tst.Value.t)])
         | ty ->
           raise_s [%message "Bug: expected variant" (label : Ident.Label.t) (ty : Tst.Value.t)])
    ;;
  end

  type t =
    | Fail
    | Leaf of
        { case : int
        ; bindings : Occurrence.t Ident.Map.t
        }
    | Switch of
        { occurrence : Occurrence.t
        ; cases : (Head.t * t) array
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
      (* [All_except] is a wildcard minus the shapes the rows test for;
         [All_except []] is a full wildcard. *)
      type t =
        | All_except of Tst.Pattern.Excluded.t list
        | Literal of Literal.t
        | Tuple of t list
        | Constructor of
            { label : Ident.Label.t
            ; payload : t option
            }
        | Or of t list
      [@@deriving sexp, compare, hash]
    end

    include T
    include Comparator.Make (T)
    include Hashable.Make (T)

    let of_head (head : Tree.Head.t) args =
      match head with
      | Literal value -> Literal value
      | Tuple _ -> Tuple args
      | Constructor { label; payload = _ } -> Constructor { label; payload = List.hd args }
    ;;

    let rec refuted_by (scrutinee : Tst.Value.t) t =
      let structure : Tst.Value.t -> Tst.Value.t = function
        | Refine { value; _ } -> value
        | value -> value
      in
      match (scrutinee : Tst.Value.t) with
      | Bottom -> true
      | _ ->
        (match t with
         | Or items -> List.for_all items ~f:(refuted_by scrutinee)
         | Literal lit ->
           Or_unknown.is_false (Tst.Pattern.Excluded.is_excluded (Literal lit) scrutinee)
         | All_except excluded ->
           List.exists excluded ~f:(fun excluded ->
             Or_unknown.is_true (Tst.Pattern.Excluded.is_excluded excluded scrutinee))
         | Constructor { label; payload } ->
           Or_unknown.is_false
             (Tst.Pattern.Excluded.is_excluded (Constructor { label; payload = None }) scrutinee)
           ||
             (match payload, structure scrutinee with
             | Some payload, Constructor { label = got; payload = Some payload_val }
               when Ident.Label.equal got label -> refuted_by payload_val payload
             | _ -> false)
         | Tuple elts ->
           (match structure scrutinee with
            | Tuple elt_vals ->
              (match List.zip (Nonempty_list.to_list elt_vals) elts with
               | Ok pairs -> List.exists pairs ~f:(fun (value, t) -> refuted_by value t)
               | Unequal_lengths -> false)
            | _ -> false))
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
    ; bound : Tree.Occurrence.t Ident.Map.t
    ; idx : int
    ; case : int
    }

  let all_wild t = Array.for_all t.patterns ~f:Pattern.is_wild

  let head ts ~col =
    let seen = Hash_set.create (module Tree.Head) in
    let result = Vec.create () in
    Vec.iter ts ~f:(fun row ->
      let head : Tree.Head.t option =
        match row.patterns.(col) with
        | Var _ -> None
        | Literal { value; _ } -> Some (Literal value)
        | Tuple { elts; _ } -> Some (Tuple (Nonempty_list.length elts))
        | Constructor { label; payload; _ } ->
          Some (Constructor { label; payload = Option.is_some payload })
        | Or _ -> raise_s [%message "Bug: unexpanded or pattern"]
      in
      Option.iter head ~f:(fun head ->
        if not (Hash_set.mem seen head)
        then (
          Hash_set.add seen head;
          Vec.push_back result head)));
    result
  ;;

  let select ts =
    let width = Array.length (Vec.get ts 0).patterns in
    let score ~col =
      let needed = not (Pattern.is_wild (Vec.get ts 0).patterns.(col)) in
      let defaults = ref 0 in
      let heads = Tree.Head.Hash_set.create () in
      Vec.iter ts ~f:(fun row ->
        match row.patterns.(col) with
        | Var _ -> incr defaults
        | Literal { value; _ } -> Hash_set.add heads (Literal value)
        | Tuple { elts; _ } -> Hash_set.add heads (Tuple (Nonempty_list.length elts))
        | Constructor { label; payload; _ } ->
          Hash_set.add heads (Constructor { label; payload = Option.is_some payload })
        | Or _ -> raise_s [%message "Bug: unexpanded or pattern"]);
      (if needed then 1 else 0), !defaults, Hash_set.length heads
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

let bind bound occurrence id =
  if Ident.is_anon id || Map.mem bound id then bound else Map.set bound ~key:id ~data:occurrence
;;

let materialize (row : Row.t) (occurrences : Tree.Occurrence.t array) =
  Array.foldi row.patterns ~init:row.bound ~f:(fun i bound pattern ->
    let occurrence = occurrences.(i) in
    match pattern with
    | Var { id; _ } -> bind bound occurrence id
    | _ -> raise_s [%message "Bug: expected wild row"])
;;

let specialize head occurrence col (rows : Row.t Vec.t) =
  let arity = Tree.Head.arity head in
  let result = Vec.create () in
  Vec.iter rows ~f:(fun row ->
    match row.patterns.(col), head with
    | Var { id; _ }, _ ->
      let bound = bind row.bound occurrence id in
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
    | Constructor { label; payload = None; _ }, Constructor { label = target; payload = false }
      when Ident.Label.equal label target ->
      let patterns =
        Array.init (Array.length row.patterns - 1) ~f:(fun i -> row.patterns.(skip col i))
      in
      Vec.push_back result { row with patterns }
    | ( Constructor { label; payload = Some payload; _ }
      , Constructor { label = target; payload = true } )
      when Ident.Label.equal label target ->
      let rest_len = Array.length row.patterns - 1 in
      let patterns =
        Array.init (1 + rest_len) ~f:(fun i ->
          if i = 0 then payload else row.patterns.(skip col (i - 1)))
      in
      Vec.push_back result { row with patterns }
    | (Literal _ | Tuple _ | Constructor _), _ -> ()
    | Or _, _ -> raise_s [%message "Bug: unexpanded or pattern"]);
  result
;;

let default occurrence col (rows : Row.t Vec.t) =
  let result = Vec.create () in
  Vec.iter rows ~f:(fun row ->
    match row.patterns.(col) with
    | Var { id; _ } ->
      let bound = bind row.bound occurrence id in
      let patterns =
        Array.init (Array.length row.patterns - 1) ~f:(fun i -> row.patterns.(skip col i))
      in
      Vec.push_back result { row with patterns; bound }
    | Literal _ | Tuple _ | Constructor _ -> ()
    | Or _ -> raise_s [%message "Bug: unexpanded or pattern"]);
  result
;;

let rec compile_matrix ~unfold reached (occurrences : Tree.Occurrence.t array) (rows : Row.t Vec.t)
  : Tree.t
  =
  if Vec.is_empty rows
  then Fail
  else if Row.all_wild (Vec.get rows 0)
  then (
    let row = Vec.get rows 0 in
    Hash_set.add reached row.idx;
    Leaf { case = row.case; bindings = materialize row occurrences })
  else (
    let col = Row.select rows in
    let occurrence = occurrences.(col) in
    let heads = Row.head rows ~col in
    let cases =
      Array.init (Vec.length heads) ~f:(fun i ->
        let head = Vec.get heads i in
        let sub = Tree.Occurrence.deepen ~unfold occurrence head in
        let occ = Tree.Occurrence.append sub occurrences ~exclude:col in
        let body = compile_matrix ~unfold reached occ (specialize head occurrence col rows) in
        head, body)
    in
    let default =
      if Tree.Head.covers heads occurrence.ty
      then None
      else (
        let rest = Tree.Occurrence.remove occurrences col |> Option.value ~default:[||] in
        Some (compile_matrix ~unfold reached rest (default occurrence col rows)))
    in
    Switch { occurrence; cases; default })
;;

let rec enumerate_missing ~unfold (occ : Tree.Occurrence.t) : Result.Missing.t =
  match Tree.Head.members occ.ty with
  | None -> All_except []
  | Some heads ->
    let items =
      Vec.to_list heads
      |> List.map ~f:(fun head ->
        let sub = Tree.Occurrence.deepen ~unfold occ head in
        Result.Missing.of_head head (Array.to_list sub |> List.map ~f:(enumerate_missing ~unfold)))
    in
    (match items with
     | [ single ] -> single
     | _ -> Or items)

and missing_matrix ~unfold (occurrences : Tree.Occurrence.t array) (rows : Row.t Vec.t) =
  if Vec.is_empty rows
  then [ Array.to_list occurrences |> List.map ~f:(enumerate_missing ~unfold) ]
  else if Row.all_wild (Vec.get rows 0)
  then []
  else (
    let col = Row.select rows in
    let occurrence = occurrences.(col) in
    match Tree.Head.members occurrence.ty with
    | Some heads ->
      Vec.fold heads ~init:[] ~f:(fun acc head ->
        let sub = Tree.Occurrence.deepen ~unfold occurrence head in
        let occ = Tree.Occurrence.append sub occurrences ~exclude:col in
        let missing = missing_matrix ~unfold occ (specialize head occurrence col rows) in
        List.fold missing ~init:acc ~f:(fun acc missing ->
          let args, rest = List.split_n missing (Tree.Head.arity head) in
          let value = Result.Missing.of_head head args in
          let before, after = List.split_n rest col in
          (before @ [ value ] @ after) :: acc))
      |> List.rev
    | None ->
      let missing_here : Result.Missing.t =
        All_except
          (Row.head rows ~col
           |> Vec.to_list
           |> List.map ~f:(fun (head : Tree.Head.t) : Tst.Pattern.Excluded.t ->
             match head with
             | Literal lit -> Literal lit
             | Constructor { label; payload = _ } -> Constructor { label; payload = None }
             | Tuple _ -> raise_s [%message "Bug: unexpected tuple" (occurrence.ty : Tst.Value.t)])
          )
      in
      let rest = Tree.Occurrence.remove occurrences col |> Option.value ~default:[||] in
      missing_matrix ~unfold rest (default occurrence col rows)
      |> List.map ~f:(fun missing ->
        let before, after = List.split_n missing col in
        before @ [ missing_here ] @ after))
;;

let compile ~scrutinee ~(ty : Tst.Value.t) ~unfold (patterns : Pattern.t Nonempty_list.t) =
  let next_idx = ref 0 in
  let rows =
    Nonempty_list.to_list patterns
    |> Sequence.of_list
    |> Sequence.concat_mapi ~f:(fun case pattern ->
      Pattern.expand pattern
      |> Sequence.of_list
      |> Sequence.map ~f:(fun pattern ->
        let idx = !next_idx in
        incr next_idx;
        { Row.patterns = [| pattern |]; case; idx; bound = Ident.Map.empty }))
    |> Vec.of_sequence
  in
  let reached = Hash_set.create (module Int) in
  let occurrences = [| { Tree.Occurrence.path = Vec.create (); ty = unfold ty } |] in
  let tree = compile_matrix ~unfold reached occurrences rows in
  let missing =
    if Tree.exhaustive tree
    then []
    else
      missing_matrix ~unfold occurrences rows
      |> List.map ~f:List.hd_exn
      |> Result.Missing.dedup
      |> List.filter ~f:(fun missing -> not (Result.Missing.refuted_by scrutinee missing))
  in
  let redundant =
    Vec.fold rows ~init:[] ~f:(fun acc row ->
      if Hash_set.mem reached row.idx then acc else row.patterns.(0) :: acc)
    |> List.rev
  in
  { Result.tree; redundant; missing }
;;
