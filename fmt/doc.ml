open! Core

module Sdoc = struct
  type t =
    | Nil
    | Text of
        { text : string
        ; rest : t
        }
    | Line of
        { indent : int
        ; rest : t
        }
  [@@deriving sexp]
end

module Mode = struct
  type t =
    | Flat
    | Break
  [@@deriving sexp]
end

type t =
  | Nil
  | Text of string
  | Cat of
      { left : t
      ; right : t
      }
  | Nest of
      { indent : int
      ; body : t
      }
  | Line of { flat : string }
  | Hard_line
  | Group of t
  | If_flat of
      { flat : t
      ; broken : t
      }
  | Align of t
[@@deriving sexp]

let nil = Nil

let text s =
  match s with
  | "" -> Nil
  | _ -> Text s
;;

let ( ^^ ) a b =
  match a, b with
  | Nil, x | x, Nil -> x
  | _ -> Cat { left = a; right = b }
;;

let nest i d = Nest { indent = i; body = d }
let line = Line { flat = " " }
let softline = Line { flat = "" }
let hardline = Hard_line
let group d = Group d
let if_flat ~flat ~broken = If_flat { flat; broken }
let align d = Align d
let space = Text " "

let concat ~sep ~f = function
  | [] -> Nil
  | [ x ] -> f x
  | x :: rest -> List.fold rest ~init:(f x) ~f:(fun acc y -> acc ^^ sep ^^ f y)
;;

let rec fits w = function
  | _ when w < 0 -> false
  | Sdoc.Nil -> true
  | Text { text; rest } -> fits (w - String.length text) rest
  | Line _ -> true
;;

let rec has_hardline = function
  | Nil | Text _ | Line _ -> false
  | Hard_line -> true
  | Cat { left; right } -> has_hardline left || has_hardline right
  | Nest { body; _ } | Group body | Align body -> has_hardline body
  | If_flat { flat; broken = _ } -> has_hardline flat
;;

let rec format width col = function
  | [] -> Sdoc.Nil
  | (_, _, Nil) :: rest -> format width col rest
  | (i, m, Cat { left; right }) :: rest -> format width col ((i, m, left) :: (i, m, right) :: rest)
  | (i, m, Nest { indent; body }) :: rest -> format width col ((i + indent, m, body) :: rest)
  | (_, m, Align body) :: rest -> format width col ((col, m, body) :: rest)
  | (_, _, Text s) :: rest ->
    Sdoc.Text { text = s; rest = format width (col + String.length s) rest }
  | (_, Mode.Flat, Line { flat }) :: rest ->
    Sdoc.Text { text = flat; rest = format width (col + String.length flat) rest }
  | (i, Break, Line _) :: rest -> Sdoc.Line { indent = i; rest = format width i rest }
  | (i, _, Hard_line) :: rest -> Sdoc.Line { indent = i; rest = format width i rest }
  | (i, Flat, If_flat { flat; _ }) :: rest -> format width col ((i, Flat, flat) :: rest)
  | (i, Break, If_flat { broken; _ }) :: rest -> format width col ((i, Break, broken) :: rest)
  | (i, Flat, Group x) :: rest -> format width col ((i, Flat, x) :: rest)
  | (i, Break, Group x) :: rest ->
    if has_hardline x
    then format width col ((i, Break, x) :: rest)
    else (
      let flat = format width col ((i, Flat, x) :: rest) in
      if fits (width - col) flat then flat else format width col ((i, Break, x) :: rest))
;;

let pretty ~width doc =
  let sdoc = format width 0 [ 0, Mode.Break, doc ] in
  let buf = Buffer.create 256 in
  let rec go = function
    | Sdoc.Nil -> ()
    | Text { text; rest } ->
      Buffer.add_string buf text;
      go rest
    | Line { indent; rest } ->
      Buffer.add_char buf '\n';
      for _ = 1 to indent do
        Buffer.add_char buf ' '
      done;
      go rest
  in
  go sdoc;
  Buffer.contents buf
;;
