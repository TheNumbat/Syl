open! Core

(* Closes static values materialized as literals into expressions and
   attaches reachable monomorphizations to binder expressions. *)
val program
  :  monomorphized:(family:Ids.Family.t -> key:Hashcons.Tag.t -> depth:int -> Tst.Expr.t option)
  -> fun_bindings:Ident.t Ids.Fn.Map.t
  -> resolve:(loc:Lex.Location.t -> Tst.Value.t -> Tst.Value.t)
  -> Tst.Top_level.t list
  -> Tst.Top_level.t list
