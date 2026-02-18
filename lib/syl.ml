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

module Result = struct
  module T = struct
    type 'a t =
      | Ok of 'a
      | Parse_error of Parse.Error.t Loc.t
      | Type_error of Typecheck.Error.t Loc.t
    [@@deriving sexp]

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

let to_tst input =
  let open Result.Let_syntax in
  let%bind cst = to_cst input in
  Typecheck.typecheck cst |> Result.typechecked
;;

let to_ir input =
  let open Result.Let_syntax in
  let%bind tst = to_tst input in
  Simplify.simplify tst |> Result.return
;;

let to_c input =
  let open Result.Let_syntax in
  let%bind ir = to_ir input in
  Codegen.codegen ir |> Result.return
;;
