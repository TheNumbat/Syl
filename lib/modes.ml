open! Core

type t =
  | Erasure
  | Staticity
[@@deriving sexp, compare, equal]

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
  let default = bottom

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

  let geq t1 t2 =
    match t1, t2 with
    | Erased, Erased | Erased, Unerased | Unerased, Unerased -> true
    | Unerased, Erased -> false
  ;;

  let print = function
    | Unerased -> "unerased"
    | Erased -> "erased"
  ;;
end

module Staticity = struct
  (*
     Dynamic
     |
     Static
  *)
  type t =
    | Dynamic
    | Static
  [@@deriving sexp, compare, equal, hash]

  let top = Dynamic
  let bottom = Static
  let default = top

  let join t1 t2 =
    match t1, t2 with
    | Static, Static -> Static
    | Dynamic, Dynamic | Dynamic, Static | Static, Dynamic -> Dynamic
  ;;

  let meet t1 t2 =
    match t1, t2 with
    | Dynamic, Dynamic -> Dynamic
    | Static, Static | Dynamic, Static | Static, Dynamic -> Static
  ;;

  let leq t1 t2 =
    match t1, t2 with
    | Static, Static | Static, Dynamic | Dynamic, Dynamic -> true
    | Dynamic, Static -> false
  ;;

  let geq t1 t2 =
    match t1, t2 with
    | Static, Static | Dynamic, Static | Dynamic, Dynamic -> true
    | Static, Dynamic -> false
  ;;

  let print = function
    | Dynamic -> "dynamic"
    | Static -> "static"
  ;;
end

module Modes = struct
  module Maybe = struct
    type t =
      { staticity : Staticity.t option
      ; erasure : Erasure.t option
      }
    [@@deriving sexp, compare, equal, hash]

    let none = { staticity = None; erasure = None }
    let is_none { staticity; erasure } = Option.is_none staticity && Option.is_none erasure

    let print () { staticity; erasure } =
      let axes =
        List.filter_opt
          [ Option.map staticity ~f:Staticity.print; Option.map erasure ~f:Erasure.print ]
        |> String.concat ~sep:" "
      in
      if String.equal axes "" then axes else axes ^ " "
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

  let annotate t (maybe : Maybe.t) =
    let staticity = Option.value maybe.staticity ~default:t.staticity in
    let erasure = Option.value maybe.erasure ~default:t.erasure in
    { staticity; erasure }
  ;;

  let create ~staticity ~erasure = { staticity; erasure }
  let return t ~ret = { t with erasure = Erasure.join t.erasure ret.erasure }

  let cond ~cond then_ else_ =
    let t = join then_ else_ in
    { t with staticity = Staticity.join t.staticity cond.staticity }
  ;;

  let is_static = function
    | { staticity = Static; _ } -> true
    | _ -> false
  ;;

  let is_erased = function
    | { erasure = Erased; _ } -> true
    | _ -> false
  ;;

  let print () { staticity; erasure } =
    [ Staticity.print staticity; Erasure.print erasure ]
    |> List.filter ~f:(fun m -> not (String.equal "" m))
    |> String.concat ~sep:" "
  ;;
end
