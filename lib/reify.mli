open! Core

(* Closes static values materialized as literals into expressions and
   attaches accumulated monomorphizations to binder expressions. *)
val program
  :  monos:Tst.Expr.t Core.Int.Map.t Core.Int.Map.t
  -> groups:Ident.t Core.Int.Map.t
  -> unfold:(Tst.Value.t -> Tst.Value.t)
  -> Tst.Top_level.t list
  -> Tst.Top_level.t list
