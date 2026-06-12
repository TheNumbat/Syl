open! Core
open! Syl

let parse input = Parse.parse_exn input
let desugar input = input |> parse |> Desugar.desugar

let print_typecheck_error ?(print = false) ({ loc; here; reason } : Typecheck.Error.t Loc.t) =
  if print
  then
    print_s
      [%message (loc : Lex.Location.t) (here : Source_code_position.t) (reason : Typecheck.Error.t)]
  else print_s [%message (loc : Lex.Location.t) (reason : Typecheck.Error.t)]
;;

let print_parse_error ?(print = false) ({ loc; here; reason } : Parse.Error.t Loc.t) =
  if print
  then
    print_s
      [%message (loc : Lex.Location.t) (here : Source_code_position.t) (reason : Parse.Error.t)]
  else print_s [%message (loc : Lex.Location.t) (reason : Parse.Error.t)]
;;

let typecheck ?(print = false) input =
  match input |> desugar |> Typecheck.typecheck with
  | Ok tst -> if print then print_s [%message (tst : Tst.Program.t)]
  | Error error -> print_typecheck_error ~print error
;;

let parse_and_format ?(print = false) input =
  match Parse.parse input with
  | Ok cst ->
    print_string (Syl_fmt.to_string cst);
    if print then print_s [%message (cst : Cst.Program.t)]
  | Error error -> print_parse_error ~print error
;;

let format_round_trip ?(width = 100) input =
  match Parse.parse input with
  | Ok cst ->
    let cfg = { Syl_fmt.Config.margin = width; indent = 2 } in
    let output = Syl_fmt.to_string ~cfg cst in
    print_string output;
    let cst2 = Parse.parse_exn output in
    let s1 = Cst.Program.strip cst |> [%sexp_of: Cst.Program.t] in
    let s2 = Cst.Program.strip cst2 |> [%sexp_of: Cst.Program.t] in
    if not (Sexp.equal s1 s2)
    then (
      print_endline "ROUND-TRIP FAILED";
      print_s [%message (s1 : Sexp.t) (s2 : Sexp.t)])
  | Error { loc; reason; _ } -> print_s [%message (loc : Lex.Location.t) (reason : Parse.Error.t)]
;;

let tokenize ?(print = false) input =
  let tokenizer = Lex.Tokenizer.create input in
  let rec loop () =
    let loc = Lex.Tokenizer.loc tokenizer in
    let token = Lex.Tokenizer.next tokenizer in
    if print
    then print_s [%message (token : Lex.Token.t) (loc : Lex.Location.t)]
    else print_s [%message (token : Lex.Token.t)];
    match token with
    | Lex.Token.Eof -> ()
    | _ -> loop ()
  in
  loop ()
;;

let raise_clang_killed signal = raise_s [%message "Clang killed" (signal : Signal.t)]

let scrub_paths stderr ~paths =
  List.fold paths ~init:stderr ~f:(fun stderr path ->
    String.substr_replace_all stderr ~pattern:path ~with_:"<tmp>")
;;

let run_process ?stdin ?(env = Core_unix.environment ()) cmd =
  let process = Core_unix.open_process_full cmd ~env in
  Option.iter stdin ~f:(Out_channel.output_string process.stdin);
  Out_channel.close process.stdin;
  let stdout = In_channel.input_all process.stdout in
  let stderr = In_channel.input_all process.stderr in
  match Core_unix.close_process_full process with
  | Ok () -> Ok stdout
  | Error (`Exit_non_zero exit_code) -> Error (`Exit_non_zero (exit_code, stderr))
  | Error (`Signal signal) -> Error (`Signal signal)
;;

let with_env env_var =
  let name = String.prefix env_var (String.index_exn env_var '=') in
  Array.append
    [| env_var |]
    (Array.filter (Core_unix.environment ()) ~f:(fun existing ->
       not (String.is_prefix existing ~prefix:(name ^ "="))))
;;

let compile c =
  match run_process "clang++ -x c++ -fsyntax-only -w -" ~stdin:c with
  | Ok _ -> ()
  | Error (`Exit_non_zero (exit_code, stderr)) ->
    raise_s [%message "Clang failed" (exit_code : int) (stderr : string)]
  | Error (`Signal signal) -> raise_clang_killed signal
;;

let compile_and_run c =
  let tmp_exe = Stdlib.Filename.temp_file "syl_test" ".exe" in
  Exn.protect
    ~f:(fun () ->
      let cmd =
        Printf.sprintf
          "clang++ -x c++ -fsanitize=address -fsanitize=alignment -o %s -w -"
          (Filename.quote tmp_exe)
      in
      (match run_process cmd ~stdin:c with
       | Ok _ -> ()
       | Error (`Exit_non_zero (exit_code, stderr)) ->
         let stderr = scrub_paths stderr ~paths:[ tmp_exe ] in
         raise_s [%message "Clang failed" (exit_code : int) (stderr : string)]
       | Error (`Signal signal) -> raise_clang_killed signal);
      match run_process (Filename.quote tmp_exe) ~env:(with_env "ASAN_OPTIONS=detect_leaks=0") with
      | Ok stdout -> print_string stdout
      | Error (`Exit_non_zero (exit_code, stderr)) ->
        let stderr = scrub_paths stderr ~paths:[ tmp_exe ] in
        raise_s [%message "Program failed" (exit_code : int) (stderr : string)]
      | Error (`Signal signal) -> raise_s [%message "Program killed" (signal : Signal.t)])
    ~finally:(fun () ->
      try Core_unix.unlink tmp_exe with
      | _ -> ())
;;

let strip_prelude cpp =
  let preamble_end = "//SYL_STD_END" in
  match String.substr_index cpp ~pattern:preamble_end with
  | None -> cpp
  | Some i ->
    let cpp = String.drop_prefix cpp (i + String.length preamble_end) in
    (match String.chop_prefix cpp ~prefix:"\n" with
     | Some cpp -> cpp
     | None -> cpp)
;;

let codegen ?(print = false) ?(check = `No) input =
  ignore (print, check);
  let dst = desugar input in
  let tst = Typecheck.typecheck_exn dst in
  let sst = Simplify.simplify tst in
  let lst = Linearize.linearize sst in
  let cpp = Codegen.cpp lst in
  if print then print_string (strip_prelude cpp);
  match check with
  | `Compile -> compile cpp
  | `Run -> compile_and_run cpp
  | _ -> ()
;;
