open! Core
open! Core_unix

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
    ~summary:"print the concrete syntax tree"
    (let%map_open file = anon ("file" %: string) in
     fun () ->
       let contents = In_channel.read_all file in
       unwrap (Syl.to_cst contents) ~f:(fun cst -> print_s [%message (cst : Syl.Cst.Program.t)]))
;;

let dump_tst =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"print the concrete syntax tree"
    (let%map_open file = anon ("file" %: string) in
     fun () ->
       let contents = In_channel.read_all file in
       unwrap (Syl.to_tst contents) ~f:(fun tst -> print_s [%message (tst : Syl.Tst.Program.t)]))
;;

let dump_ir =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"print the concrete syntax tree"
    (let%map_open file = anon ("file" %: string) in
     fun () ->
       let contents = In_channel.read_all file in
       unwrap (Syl.to_ir contents) ~f:(fun ir -> print_s [%message (ir : Syl.Ir.Program.t)]))
;;

let dump_c =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"print the concrete syntax tree"
    (let%map_open file = anon ("file" %: string) in
     fun () ->
       let contents = In_channel.read_all file in
       unwrap (Syl.to_c contents) ~f:print_endline)
;;

let dump =
  Command.group
    ~summary:"print an IR"
    [ "cst", dump_cst; "tst", dump_tst; "ir", dump_ir; "c", dump_c ]
;;

let default_output file = Filename.basename (Filename.chop_extension file) ^ ".exe"

let compile_to_exe file output =
  let contents = In_channel.read_all file in
  let ok = ref false in
  unwrap (Syl.to_c contents) ~f:(fun c_code ->
    let c_file = Stdlib.Filename.temp_file "syl" ".c" in
    Out_channel.write_all c_file ~data:c_code;
    let cmd = sprintf "clang -o %s %s -O2" (Filename.quote output) (Filename.quote c_file) in
    (match Core_unix.system cmd with
     | Ok () -> ok := true
     | Error (`Exit_non_zero _) -> eprintf "clang failed\n"
     | Error (`Signal _) -> eprintf "clang killed by signal\n");
    Core_unix.unlink c_file);
  !ok
;;

let build =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"build an executable"
    (let%map_open file = anon ("file" %: string)
     and output = flag "-o" (optional string) ~doc:"FILE output executable path" in
     fun () ->
       let output = Option.value output ~default:(default_output file) in
       let (_ : bool) = compile_to_exe file output in
       ())
;;

let run =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"build and run an executable"
    (let%map_open file = anon ("file" %: string) in
     fun () ->
       let exe = Stdlib.Filename.temp_file "syl" ".exe" in
       if compile_to_exe file exe
       then (
         let status = Core_unix.system exe in
         Core_unix.unlink exe;
         match status with
         | Ok () -> ()
         | Error (`Exit_non_zero n) -> Core_unix.exit_immediately n
         | Error (`Signal _) -> eprintf "program killed by signal\n"))
;;

let () =
  Command_unix.run
    (Command.group ~summary:"Syl Compiler" [ "dump", dump; "build", build; "run", run ])
;;
