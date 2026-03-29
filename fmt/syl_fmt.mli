open! Core

(* Vibe coded. *)

module Config : sig
  type t =
    { margin : int
    ; indent : int
    }

  val default : t
end

val to_doc : ?cfg:Config.t -> Syl.Cst.Program.t -> Doc.t
val to_string : ?cfg:Config.t -> Syl.Cst.Program.t -> string
