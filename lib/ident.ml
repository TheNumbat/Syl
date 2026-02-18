open! Core
include String

let anon = "_"
let is_anon t = String.equal t anon
let print () t = t

module Path = struct
  type nonrec t =
    { path : t list
    ; next : int
    }
  [@@deriving sexp, hash, compare]

  let empty = { path = []; next = 0 }
  let append t id = { t with path = id :: t.path }

  let fresh t prefix =
    let id = of_string (prefix ^ Int.to_string t.next) in
    { path = id :: t.path; next = t.next + 1 }
  ;;

  let join t = List.rev_map ~f:to_string t.path |> String.concat ~sep:"_" |> of_string
end
