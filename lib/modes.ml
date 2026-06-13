open! Core

module Axis = struct
  type t =
    | Erasure
    | Staticity
  [@@deriving sexp, compare, equal]
end

module Erasure = struct
  (*
     Erased
     |
     Unerased
  *)
  type t =
    | Erased
    | Unerased
  [@@deriving sexp, compare, equal, hash]

  let top = Erased
  let bottom = Unerased
  let default = Unerased

  let join t1 t2 =
    match t1, t2 with
    | Unerased, Unerased -> Unerased
    | Erased, Erased | Unerased, Erased | Erased, Unerased -> Erased
  ;;

  let meet t1 t2 =
    match t1, t2 with
    | Erased, Erased -> Erased
    | Unerased, Unerased | Unerased, Erased | Erased, Unerased -> Unerased
  ;;

  let leq t1 t2 =
    match t1, t2 with
    | Erased, Erased | Unerased, Erased | Unerased, Unerased -> true
    | Erased, Unerased -> false
  ;;

  let geq t1 t2 = leq t2 t1

  let print = function
    | Unerased -> "unerased"
    | Erased -> "erased"
  ;;
end

module Staticity = struct
  (*
     Dynamic
     |
     Parametric
     |
     Static
  *)
  type t =
    | Dynamic
    | Parametric
    | Static
  [@@deriving sexp, compare, equal, hash]

  let top = Dynamic
  let bottom = Static
  let default = Dynamic

  let join t1 t2 =
    match t1, t2 with
    | Static, Static -> Static
    | Dynamic, _ | _, Dynamic -> Dynamic
    | Parametric, _ | _, Parametric -> Parametric
  ;;

  let meet t1 t2 =
    match t1, t2 with
    | Dynamic, Dynamic -> Dynamic
    | Static, _ | _, Static -> Static
    | Parametric, _ | _, Parametric -> Parametric
  ;;

  let leq t1 t2 =
    match t1, t2 with
    | Static, _ -> true
    | Parametric, (Parametric | Dynamic) -> true
    | Dynamic, Dynamic -> true
    | (Parametric | Dynamic), Static -> false
    | Dynamic, Parametric -> false
  ;;

  let geq t1 t2 = leq t2 t1

  let resolve = function
    | Parametric -> Static
    | other -> other
  ;;

  let print = function
    | Static -> "static"
    | Parametric -> "parametric"
    | Dynamic -> "dynamic"
  ;;
end

module Eliminator = struct
  type t =
    | Dynamic
    | Static
    | Erased
  [@@deriving sexp, compare, equal, hash]

  let default = Dynamic

  let print = function
    | Dynamic -> "dynamic"
    | Static -> "static"
    | Erased -> "erased"
  ;;
end

module Maybe = struct
  type t =
    { staticity : Staticity.t option
    ; erasure : Erasure.t option
    }
  [@@deriving sexp, compare, equal, hash]

  let none = { staticity = None; erasure = None }

  let is_erased = function
    | { erasure = Some Erased; _ } -> true
    | _ -> false
  ;;

  let is_unerased = function
    | { erasure = Some Unerased; _ } -> true
    | _ -> false
  ;;

  let is_static = function
    | { staticity = Some Static; _ } -> true
    | _ -> false
  ;;

  let is_dynamic = function
    | { staticity = Some Dynamic; _ } -> true
    | _ -> false
  ;;

  let is_none { staticity; erasure } = Option.is_none staticity && Option.is_none erasure

  let print () { staticity; erasure } =
    List.filter_opt [ Option.map staticity ~f:Staticity.print; Option.map erasure ~f:Erasure.print ]
    |> String.concat ~sep:" "
  ;;
end

type t =
  { staticity : Staticity.t
  ; erasure : Erasure.t
  }
[@@deriving sexp, compare, equal, hash]

let join
      { staticity = staticity1; erasure = erasure1 }
      { staticity = staticity2; erasure = erasure2 }
  =
  let staticity = Staticity.join staticity1 staticity2 in
  let erasure = Erasure.join erasure1 erasure2 in
  { staticity; erasure }
;;

let meet
      { staticity = staticity1; erasure = erasure1 }
      { staticity = staticity2; erasure = erasure2 }
  =
  let staticity = Staticity.meet staticity1 staticity2 in
  let erasure = Erasure.meet erasure1 erasure2 in
  { staticity; erasure }
;;

let leq
      { staticity = staticity1; erasure = erasure1 }
      { staticity = staticity2; erasure = erasure2 }
  =
  let staticity = Staticity.leq staticity1 staticity2 in
  let erasure = Erasure.leq erasure1 erasure2 in
  staticity && erasure
;;

let geq
      { staticity = staticity1; erasure = erasure1 }
      { staticity = staticity2; erasure = erasure2 }
  =
  let staticity = Staticity.geq staticity1 staticity2 in
  let erasure = Erasure.geq erasure1 erasure2 in
  staticity && erasure
;;

let bottom ?(staticity = Staticity.bottom) ?(erasure = Erasure.bottom) () = { staticity; erasure }
let top ?(staticity = Staticity.top) ?(erasure = Erasure.top) () = { staticity; erasure }

let default ?(staticity = Staticity.default) ?(erasure = Erasure.default) () =
  { staticity; erasure }
;;

let is_static = function
  | { staticity = Static; _ } -> true
  | _ -> false
;;

let is_dynamic = function
  | { staticity = Dynamic; _ } -> true
  | _ -> false
;;

let is_parametric = function
  | { staticity = Parametric; _ } -> true
  | _ -> false
;;

let is_erased = function
  | { erasure = Erased; _ } -> true
  | _ -> false
;;

let is_unerased = function
  | { erasure = Unerased; _ } -> true
  | _ -> false
;;

let maybe t = { Maybe.staticity = Some t.staticity; erasure = Some t.erasure }

let annotate t (maybe : Maybe.t) =
  let staticity = Option.value maybe.staticity ~default:t.staticity in
  let erasure = Option.value maybe.erasure ~default:t.erasure in
  { staticity; erasure }
;;

let create ~staticity ~erasure = { staticity; erasure }
let return t ~ret = { t with erasure = Erasure.join t.erasure ret.erasure }
let capture t ~fv = { t with staticity = Staticity.join t.staticity fv.staticity }

let cond ~cond cases =
  let t = List.fold cases ~init:(bottom ()) ~f:join in
  { t with staticity = Staticity.join t.staticity cond.staticity }
;;

let print () { staticity; erasure } =
  [ Staticity.print staticity; Erasure.print erasure ]
  |> List.filter ~f:(fun m -> not (String.equal "" m))
  |> String.concat ~sep:" "
;;
