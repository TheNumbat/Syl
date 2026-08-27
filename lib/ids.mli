open! Core

(* Closures, binders, and pi types have a unique [Fn] id. *)
module Fn : Unique_id.Id

(* Specialization contexts (location/argument path) have a unique [Family] id. *)
module Family : Unique_id.Id

(* Identifiers have a unique stamp. *)
module Stamp : Unique_id.Id
