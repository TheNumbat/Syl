open! Core
module Modes = Modes
module Or_unknown = Or_unknown
module Ids = Ids
module Ident = Ident
module Builtin = Builtin
module Cst = Cst
module Dst = Dst
module Tst = Tst
module Sst = Sst
module Lst = Lst
module Loc = Loc
module Lex = Lex
module Parse = Parse
module Desugar = Desugar
module Match = Match
module Typecheck = Typecheck
module Simplify = Simplify
module Linearize = Linearize
module Codegen = Codegen

module Result = struct
  module T = struct
    type 'a t =
      | Ok of 'a
      | Parse_error of Parse.Error.t Loc.t
      | Type_error of Typecheck.Error.t Loc.t
    [@@deriving sexp_of]

    let parsed (x : (Cst.Program.t, Parse.Error.t Loc.t) result) =
      match x with
      | Ok x -> Ok x
      | Error err -> Parse_error err
    ;;

    let typechecked (x : (Tst.Program.t, Typecheck.Error.t Loc.t) result) =
      match x with
      | Ok x -> Ok x
      | Error err -> Type_error err
    ;;

    let return x = Ok x

    let bind t ~f =
      match t with
      | Ok x -> f x
      | Parse_error err -> Parse_error err
      | Type_error err -> Type_error err
    ;;

    let map = `Define_using_bind
  end

  include T
  include Monad.Make (T)
end

let to_cst input = Parse.parse input |> Result.parsed

let to_dst input =
  let open Result.Let_syntax in
  let%bind cst = to_cst input in
  Desugar.desugar cst |> Result.return
;;

let to_tst input =
  let open Result.Let_syntax in
  let%bind dst = to_dst input in
  Typecheck.typecheck dst |> Result.typechecked
;;

let to_sst input =
  let open Result.Let_syntax in
  let%bind tst = to_tst input in
  Simplify.simplify tst |> Result.return
;;

let to_lst input =
  let open Result.Let_syntax in
  let%bind sst = to_sst input in
  Linearize.linearize sst |> Result.return
;;

let to_cpp input =
  let open Result.Let_syntax in
  let%bind lst = to_lst input in
  Codegen.cpp lst |> Result.return
;;
