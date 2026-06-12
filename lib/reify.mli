open! Core

(* Closes static function values materialized as literals into expressions and
   attaches accumulated monomorphizations to binder expressions. Runs once all
   top levels are checked, because specializations accumulate for the whole
   program. [monos] is the full specialization store (family -> key -> body); only
   specializations reachable from a surviving dispatch are attached. [groups]
   names each fun's binding, which we need to rebuild recursive groups when quoting. *)
val program
  :  monos:Tst.Expr.t Tst.Value.Concrete.Map.t Core.Int.Map.t
  -> groups:Ident.t Core.Int.Map.t
  -> Tst.Top_level.t list
  -> Tst.Top_level.t list
