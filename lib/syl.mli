open! Core
module Cst = Cst
module Tst = Tst
module Ir = Ir
module Loc = Loc
module Lex = Lex
module Parse = Parse
module Typecheck = Typecheck
module Simplify = Simplify
module Codegen = Codegen

module Result : sig
  type 'a t =
    | Ok of 'a
    | Parse_error of Parse.Error.t Loc.t
    | Type_error of Typecheck.Error.t Loc.t
  [@@deriving sexp]
end

val to_cst : string -> Cst.Program.t Result.t
val to_tst : string -> Tst.Program.t Result.t
val to_ir : string -> Ir.Program.t Result.t
val to_c : string -> string Result.t
