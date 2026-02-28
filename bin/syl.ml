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
    ~summary:"print the concrete IR"
    (let%map_open file = anon ("file" %: string) in
     fun () ->
       let contents = In_channel.read_all file in
       unwrap (Syl.to_cst contents) ~f:(fun cst -> print_s [%message (cst : Syl.Cst.Program.t)]))
;;

let dump_tst =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"print the typed IR"
    (let%map_open file = anon ("file" %: string) in
     fun () ->
       let contents = In_channel.read_all file in
       unwrap (Syl.to_tst contents) ~f:(fun tst -> print_s [%message (tst : Syl.Tst.Program.t)]))
;;

let dump_sst =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"print the simplified IR"
    (let%map_open file = anon ("file" %: string) in
     fun () ->
       let contents = In_channel.read_all file in
       unwrap (Syl.to_sst contents) ~f:(fun ir -> print_s [%message (ir : Syl.Sst.Program.t)]))
;;

let dump_lst =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"print the linearized IR"
    (let%map_open file = anon ("file" %: string) in
     fun () ->
       let contents = In_channel.read_all file in
       unwrap (Syl.to_lst contents) ~f:(fun ir -> print_s [%message (ir : Syl.Lst.Program.t)]))
;;

let dump_c =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"print the C output"
    (let%map_open file = anon ("file" %: string) in
     fun () ->
       let contents = In_channel.read_all file in
       unwrap (Syl.to_c contents) ~f:print_endline)
;;

let dump =
  Command.group
    ~summary:"print an IR"
    [ "cst", dump_cst; "tst", dump_tst; "sst", dump_sst; "lst", dump_lst; "c", dump_c ]
;;

let default_output file = Filename.basename (Filename.chop_extension file) ^ ".exe"

let compile_to_exe file output =
  let contents = In_channel.read_all file in
  unwrap (Syl.to_c contents) ~f:(fun c_code ->
    let c_file = Stdlib.Filename.temp_file "syl" ".cpp" in
    Out_channel.write_all c_file ~data:c_code;
    let cmd = sprintf "clang++ -g -o %s %s" (Filename.quote output) (Filename.quote c_file) in
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

let () =
  Command_unix.run
    (Command.group ~summary:"Syl Compiler" [ "dump", dump; "build", build; "run", run ])
;;
