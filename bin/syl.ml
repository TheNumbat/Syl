open! Core

let unwrap ~f result =
  let open Syl in
  match result with
  | Result.Ok x -> f x
  | Result.Parse_error err -> print_s [%message (err : Parse.Error.t Loc.t)]
  | Result.Type_error err -> print_s [%message (err : Typecheck.Error.t Loc.t)]
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

let default_output file = Filename.basename (Filename.chop_extension file) ^ ".exe"

let compile_to_exe file output =
  let contents = In_channel.read_all file in
  unwrap (Syl.to_cpp contents) ~f:(fun c_code ->
    let c_file = Stdlib.Filename.temp_file "syl" ".cpp" in
    Out_channel.write_all c_file ~data:c_code;
    let cmd = sprintf "clang++ -O2 -o %s %s" (Filename.quote output) (Filename.quote c_file) in
    (match Core_unix.system cmd with
     | Ok () -> ()
     | Error (`Exit_non_zero exit_code) ->
       print_s [%message "Clang failed" (exit_code : int)];
       Core_unix.unlink c_file;
       Stdlib.exit 1
     | Error (`Signal signal) ->
       print_s [%message "Clang killed" (signal : Signal.t)];
       Core_unix.unlink c_file;
       Stdlib.exit 1);
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
       | Error (`Exit_non_zero exit_code) ->
         print_s [%message "Program failed" (exit_code : int)];
         Stdlib.exit (if exit_code >= 1 && exit_code <= 255 then exit_code else 1)
       | Error (`Signal signal) ->
         print_s [%message "Program killed" (signal : Signal.t)];
         Stdlib.exit 1)
;;

let json_escape =
  String.concat_map ~f:(fun c ->
    match c with
    | '"' -> "\\\""
    | '\\' -> "\\\\"
    | '\n' -> "\\n"
    | '\r' -> "\\r"
    | '\t' -> "\\t"
    | c when Char.to_int c < 0x20 -> sprintf "\\u%04x" (Char.to_int c)
    | c -> String.of_char c)
;;

let error_json ~(loc : Syl.Lex.Location.t) reason =
  eprintf "{\"line\":%d,\"column\":%d,\"reason\":\"%s\"}\n" loc.line loc.column (json_escape reason);
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

module Asm = struct
  exception Tool_error of string

  let is_symbol_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.' | '$' -> true
    | c -> Char.to_int c >= 128
  ;;

  let map_symbols line ~f =
    let length = String.length line in
    let rec quoted_end i =
      if i >= length
      then length
      else (
        match line.[i] with
        | '\\' -> quoted_end (i + 2)
        | '"' -> i + 1
        | _ -> quoted_end (i + 1))
    in
    let rec symbol_end i =
      if i < length && is_symbol_char line.[i] then symbol_end (i + 1) else i
    in
    let buf = Buffer.create length in
    let rec loop i =
      if i < length
      then
        if Char.equal line.[i] '"'
        then (
          let stop = Int.min length (quoted_end (i + 1)) in
          let token = String.sub line ~pos:i ~len:(stop - i) in
          let symbol = String.sub token ~pos:1 ~len:(Int.max 0 (String.length token - 2)) in
          let replacement = f symbol in
          Buffer.add_string buf (if String.equal symbol replacement then token else replacement);
          loop stop)
        else if is_symbol_char line.[i]
        then (
          let stop = symbol_end (i + 1) in
          Buffer.add_string buf (f (String.sub line ~pos:i ~len:(stop - i)));
          loop stop)
        else (
          Buffer.add_char buf line.[i];
          loop (i + 1))
    in
    loop 0;
    Buffer.contents buf
  ;;

  let strip_comment line ~comment =
    let rec loop i quoted =
      if i >= String.length line
      then line
      else if quoted && Char.equal line.[i] '\\'
      then loop (i + 2) quoted
      else if Char.equal line.[i] '"'
      then loop (i + 1) (not quoted)
      else if (not quoted) && String.is_substring_at line ~pos:i ~substring:comment
      then String.prefix line i
      else loop (i + 1) quoted
    in
    loop 0 false |> String.strip
  ;;

  let data_directives =
    String.Set.of_list
      [ ".ascii"
      ; ".asciz"
      ; ".string"
      ; ".byte"
      ; ".short"
      ; ".value"
      ; ".word"
      ; ".long"
      ; ".int"
      ; ".quad"
      ; ".octa"
      ; ".hword"
      ; ".xword"
      ; ".2byte"
      ; ".4byte"
      ; ".8byte"
      ; ".float"
      ; ".single"
      ; ".double"
      ; ".zero"
      ; ".space"
      ; ".skip"
      ; ".fill"
      ; ".comm"
      ; ".lcomm"
      ; ".set"
      ; ".equ"
      ; ".equiv"
      ; ".uleb128"
      ; ".sleb128"
      ]
  ;;

  let first_word line = String.take_while line ~f:(fun c -> not (Char.is_whitespace c))
  let is_label line = String.is_suffix line ~suffix:":"

  let label_name line =
    let name = String.drop_suffix line 1 in
    if String.is_prefix name ~prefix:"\"" && String.is_suffix name ~suffix:"\""
    then String.sub name ~pos:1 ~len:(String.length name - 2)
    else name
  ;;

  let is_local_label label =
    String.is_prefix label ~prefix:".L"
    || String.is_prefix label ~prefix:"L"
    || String.is_prefix label ~prefix:"l_"
  ;;

  let is_string_data line =
    List.mem [ ".ascii"; ".asciz"; ".string" ] (first_word line) ~equal:String.equal
  ;;

  let clean ?(comment = "#") ?(demangle = Fn.id) assembly =
    let skip_section = ref false in
    let lines =
      String.split_lines assembly
      |> List.filter_map ~f:(fun line ->
        let line = strip_comment line ~comment in
        let directive = first_word line in
        if String.equal directive ".section"
        then
          skip_section
          := List.exists [ ".note"; ".debug"; ".comment"; ".llvm"; ".eh_frame" ] ~f:(fun part ->
               String.is_substring line ~substring:part)
        else if List.mem [ ".text"; ".data"; ".bss"; ".rodata" ] directive ~equal:String.equal
        then skip_section := false;
        if !skip_section || String.is_empty line
        then None
        else if
          is_label line
          || (not (String.is_prefix line ~prefix:"."))
          || Set.mem data_directives directive
          || String.equal directive ".cfi_endproc"
          || String.equal directive ".size"
        then Some line
        else None)
    in
    let symbols = Hash_set.create (module String) in
    List.iter lines ~f:(fun line ->
      if not (is_string_data line)
      then
        ignore
          (map_symbols line ~f:(fun symbol ->
             if String.is_prefix symbol ~prefix:"_Z" || String.is_prefix symbol ~prefix:"__Z"
             then Hash_set.add symbols symbol;
             symbol)
           : string));
    let symbols = Hash_set.to_list symbols |> List.sort ~compare:String.compare in
    let names = String.Map.of_alist_exn (List.zip_exn symbols (demangle symbols)) in
    let skip_function = ref false in
    let lines =
      List.filter lines ~f:(fun line ->
        if String.equal line ".cfi_endproc" || String.equal (first_word line) ".size"
        then (
          skip_function := false;
          false)
        else (
          if is_label line && not (is_local_label (label_name line))
          then (
            let symbol = label_name line in
            let name = Map.find names symbol |> Option.value ~default:symbol in
            skip_function
            := String.is_prefix name ~prefix:"syl_std_" || String.is_prefix name ~prefix:"_syl_std_");
          not !skip_function))
    in
    (* Collect references after hiding stdlib bodies, so their private data
       disappears while constants also used by the program remain visible. *)
    let references = Hash_set.create (module String) in
    List.iter lines ~f:(fun line ->
      if not (is_label line || is_string_data line)
      then
        ignore
          (map_symbols line ~f:(fun symbol ->
             Hash_set.add references symbol;
             symbol)
           : string));
    let lines = Array.of_list lines in
    let skip_data = ref false in
    let live_labels = ref false in
    let buf = Buffer.create (String.length assembly) in
    Array.iteri lines ~f:(fun i line ->
      let label = is_label line in
      let name = if label then label_name line else "" in
      if label
      then (
        (* Consecutive labels can name the same data. Keep its payload if any
           of those names is referenced. *)
        if i = 0 || not (is_label lines.(i - 1)) then live_labels := false;
        live_labels := !live_labels || (not (is_local_label name)) || Hash_set.mem references name;
        skip_data
        := (not !live_labels)
           && i + 1 < Array.length lines
           && Set.mem data_directives (first_word lines.(i + 1)))
      else if not (Set.mem data_directives (first_word line))
      then skip_data := false;
      if
        (not !skip_data)
        && ((not label) || (not (is_local_label name)) || Hash_set.mem references name)
      then (
        let line =
          if is_string_data line
          then line
          else
            map_symbols line ~f:(fun symbol ->
              Map.find names symbol |> Option.value ~default:symbol)
        in
        if label
        then (
          if (not (is_local_label name)) && Buffer.length buf > 0 then Buffer.add_char buf '\n';
          Buffer.add_string buf line)
        else (
          Buffer.add_string buf "        ";
          let mnemonic = first_word line in
          Buffer.add_string buf mnemonic;
          let operands = String.drop_prefix line (String.length mnemonic) |> String.strip in
          if not (String.is_empty operands)
          then (
            Buffer.add_string buf (String.make (Int.max 1 (8 - String.length mnemonic)) ' ');
            Buffer.add_string buf operands));
        Buffer.add_char buf '\n'));
    Buffer.contents buf
  ;;

  let capture prog args =
    let command = String.concat ~sep:" " (List.map (prog :: args) ~f:Filename.quote) in
    let channel = Core_unix.open_process_in command in
    let status = ref (Ok ()) in
    let output =
      Exn.protect
        ~f:(fun () -> In_channel.input_all channel)
        ~finally:(fun () -> status := Core_unix.close_process_in channel)
    in
    (match !status with
     | Ok () -> ()
     | Error (`Exit_non_zero code) ->
       raise (Tool_error (sprintf "%s failed with exit code %d" prog code))
     | Error (`Signal signal) ->
       raise (Tool_error (sprintf "%s was killed by %s" prog (Signal.to_string signal))));
    output
  ;;

  let demangle symbols =
    (* c++filt's stdin scanner does not recognize UTF-8 identifiers. Pass whole
       symbols as arguments instead, in batches to stay below command-size limits. *)
    List.chunks_of symbols ~length:100
    |> List.concat_map ~f:(fun symbols ->
      let arguments =
        List.map symbols ~f:(fun symbol ->
          if String.is_prefix symbol ~prefix:"__Z" then String.drop_prefix symbol 1 else symbol)
      in
      capture "c++filt" ("-n" :: arguments) |> String.split_lines)
  ;;

  let of_cpp ~opt_level ~raw cpp =
    let target = capture "clang++" [ "-dumpmachine" ] |> String.strip in
    let arch = String.lsplit2 target ~on:'-' |> Option.value_map ~f:fst ~default:target in
    let x86 = List.mem [ "x86_64"; "i386"; "i486"; "i586"; "i686" ] arch ~equal:String.equal in
    let comment =
      if String.is_prefix arch ~prefix:"aarch64" || String.is_prefix arch ~prefix:"arm64"
      then "//"
      else if String.is_prefix arch ~prefix:"arm" || String.is_prefix arch ~prefix:"thumb"
      then "@"
      else "#"
    in
    let cpp_file = Stdlib.Filename.temp_file "syl" ".cpp" in
    Exn.protect
      ~f:(fun () ->
        Out_channel.write_all cpp_file ~data:cpp;
        let assembly =
          capture
            "clang++"
            ([ "-O" ^ opt_level; "-S"; "-fno-verbose-asm" ]
             @ (if x86 then [ "-masm=intel" ] else [])
             @ [ "-o"; "-"; cpp_file ])
        in
        if raw then assembly else clean ~comment ~demangle assembly)
      ~finally:(fun () -> Core_unix.unlink cpp_file)
  ;;
end

let dump_asm =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"print disassembly"
    (let%map_open file = anon (maybe ("file" %: string))
     and opt_level =
       flag
         "-O"
         (optional_with_default
            "2"
            (Command.Arg_type.of_alist_exn
               (List.map [ "0"; "1"; "2"; "3"; "s"; "z" ] ~f:(fun level -> level, level))))
         ~doc:"LEVEL Clang optimization level (0, 1, 2, 3, s, z; default 2)"
     and raw = flag "-raw" no_arg ~doc:"print unfiltered Clang assembly" in
     fun () ->
       let contents = read_input file in
       match Syl.to_cpp contents with
       | Parse_error { loc; reason; _ } ->
         error_json ~loc (Sexp.to_string [%sexp (reason : Syl.Parse.Error.t)])
       | Type_error { loc; reason; _ } ->
         error_json ~loc (Sexp.to_string [%sexp (reason : Syl.Typecheck.Error.t)])
       | Ok cpp ->
         (try print_string (Asm.of_cpp ~opt_level ~raw cpp) with
          | Asm.Tool_error message ->
            eprintf "error: %s\n" message;
            exit 1))
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
    ; "asm", dump_asm
    ; "all", dump_all
    ]
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"Syl Compiler"
       [ "dump", dump; "build", build; "run", run; "format", format; "check", check ])
;;
