open! Core
module Cst = Cst
module Dst = Dst
module Tst = Tst
module Sst = Sst
module Lst = Lst
module Loc = Loc
module Lex = Lex
module Parse = Parse
module Desugar = Desugar
module Typecheck = Typecheck
module Simplify = Simplify
module Linearize = Linearize
module Codegen = Codegen

module Result : sig
  type 'a t =
    | Ok of 'a
    | Parse_error of Parse.Error.t Loc.t
    | Type_error of Typecheck.Error.t Loc.t
  [@@deriving sexp]
end

val to_cst : string -> Cst.Program.t Result.t
val to_dst : string -> Dst.Program.t Result.t
val to_tst : string -> Tst.Program.t Result.t
val to_sst : string -> Sst.Program.t Result.t
val to_lst : string -> Lst.Program.t Result.t
val to_c : string -> string Result.t
