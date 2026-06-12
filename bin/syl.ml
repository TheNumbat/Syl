open! Core

let unwrap ~f result =
  let open Syl in
  match result with
  | Syl.Result.Ok x -> f x
  | Syl.Result.Parse_error err -> print_s [%message (err : Parse.Error.t Loc.t)]
  | Syl.Result.Type_error err -> print_s [%message (err : Typecheck.Error.t Loc.t)]
;;

let dump_cst =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"print after parse"
    (let%map_open file = anon ("file" %: string) in
     fun () ->
       let contents = In_channel.read_all file in
       unwrap (Syl.to_cst contents) ~f:(fun cst -> print_s [%message (cst : Syl.Cst.Program.t)]))
;;

let dump_dst =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"print after desugar"
    (let%map_open file = anon ("file" %: string) in
     fun () ->
       let contents = In_channel.read_all file in
       unwrap (Syl.to_dst contents) ~f:(fun dst -> print_s [%message (dst : Syl.Dst.Program.t)]))
;;

let dump_tst =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"print after typecheck"
    (let%map_open file = anon ("file" %: string) in
     fun () ->
       let contents = In_channel.read_all file in
       unwrap (Syl.to_tst contents) ~f:(fun tst -> print_s [%message (tst : Syl.Tst.Program.t)]))
;;

let dump_sst =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"print after simplify"
    (let%map_open file = anon ("file" %: string) in
     fun () ->
       let contents = In_channel.read_all file in
       unwrap (Syl.to_sst contents) ~f:(fun ir -> print_s [%message (ir : Syl.Sst.Program.t)]))
;;

let dump_lst =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"print after linearize"
    (let%map_open file = anon ("file" %: string) in
     fun () ->
       let contents = In_channel.read_all file in
       unwrap (Syl.to_lst contents) ~f:(fun ir -> print_s [%message (ir : Syl.Lst.Program.t)]))
;;

let dump_cpp =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"print after code generation"
    (let%map_open file = anon ("file" %: string) in
     fun () ->
       let contents = In_channel.read_all file in
       unwrap (Syl.to_cpp contents) ~f:print_endline)
;;

let dump_all =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"print after every pass"
    (let%map_open file = anon ("file" %: string) in
     fun () ->
       let contents = In_channel.read_all file in
       unwrap (Syl.to_cst contents) ~f:(fun cst ->
         print_s [%message (cst : Syl.Cst.Program.t)];
         let dst = Syl.Desugar.desugar cst in
         print_s [%message (dst : Syl.Dst.Program.t)];
         unwrap (Syl.to_tst contents) ~f:(fun tst ->
           print_s [%message (tst : Syl.Tst.Program.t)];
           let sst = Syl.Simplify.simplify tst in
           print_s [%message (sst : Syl.Sst.Program.t)];
           let lst = Syl.Linearize.linearize sst in
           print_s [%message (lst : Syl.Lst.Program.t)];
           let cpp = Syl.Codegen.cpp lst in
           print_s [%message (cpp : string)])))
;;

let dump =
  Command.group
    ~summary:"print an IR"
    [ "cst", dump_cst
    ; "dst", dump_dst
    ; "tst", dump_tst
    ; "sst", dump_sst
    ; "lst", dump_lst
    ; "cpp", dump_cpp
    ; "all", dump_all
    ]
;;

let default_output file = Filename.basename (Filename.chop_extension file) ^ ".exe"

let compile_to_exe file output =
  let contents = In_channel.read_all file in
  unwrap (Syl.to_cpp contents) ~f:(fun c_code ->
    let c_file = Stdlib.Filename.temp_file "syl" ".cpp" in
    Out_channel.write_all c_file ~data:c_code;
    let cmd = sprintf "clang++ -O2 -o %s %s" (Filename.quote output) (Filename.quote c_file) in
    (match Core_unix.system cmd with
     | Ok () -> ()
     | Error (`Exit_non_zero exit_code) -> print_s [%message "Clang failed" (exit_code : int)]
     | Error (`Signal signal) -> print_s [%message "Clang killed" (signal : Signal.t)]);
    Core_unix.unlink c_file)
;;

let build =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"build an executable"
    (let%map_open file = anon ("file" %: string)
     and output = flag "-o" (optional string) ~doc:"FILE output executable path" in
     fun () ->
       let output = Option.value output ~default:(default_output file) in
       compile_to_exe file output)
;;

let run =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"build and run an executable"
    (let%map_open file = anon ("file" %: string) in
     fun () ->
       let exe = Stdlib.Filename.temp_file "syl" ".exe" in
       compile_to_exe file exe;
       let status = Core_unix.system exe in
       Core_unix.unlink exe;
       match status with
       | Ok () -> ()
       | Error (`Exit_non_zero exit_code) -> print_s [%message "Program failed" (exit_code : int)]
       | Error (`Signal signal) -> print_s [%message "Program killed" (signal : Signal.t)])
;;

let error_json ~(loc : Syl.Lex.Location.t) reason =
  eprintf "{\"line\":%d,\"column\":%d,\"reason\":\"%s\"}\n" loc.line loc.column reason;
  exit 1
;;

let read_input file =
  match file with
  | Some file -> In_channel.read_all file
  | None -> In_channel.input_all In_channel.stdin
;;

let format =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"format a Syl source file"
    (let%map_open file = anon (maybe ("file" %: string))
     and inplace = flag "-i" no_arg ~doc:"format file in place" in
     fun () ->
       let contents = read_input file in
       match Syl.Parse.parse contents with
       | Ok cst ->
         let output = Syl_fmt.to_string cst in
         if inplace
         then (
           match file with
           | Some file -> Out_channel.write_all file ~data:output
           | None -> eprintf "error: -i requires a file argument\n")
         else print_string output
       | Error { loc; reason; _ } ->
         error_json ~loc (Sexp.to_string [%sexp (reason : Syl.Parse.Error.t)]))
;;

let check =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"check a Syl source file for errors"
    (let%map_open file = anon (maybe ("file" %: string)) in
     fun () ->
       let contents = read_input file in
       let open Syl in
       match to_tst contents with
       | Ok _ -> ()
       | Parse_error { loc; reason; _ } ->
         error_json ~loc (Sexp.to_string [%sexp (reason : Parse.Error.t)])
       | Type_error { loc; reason; _ } ->
         error_json ~loc (Sexp.to_string [%sexp (reason : Typecheck.Error.t)]))
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"Syl Compiler"
       [ "dump", dump; "build", build; "run", run; "format", format; "check", check ])
;;
