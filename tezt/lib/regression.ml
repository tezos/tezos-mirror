(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Base

let capture_output : out_channel option ref = ref None

(* Capture a string into a regression output. *)
let capture line =
  match !capture_output with
  | None -> ()
  | Some channel ->
      output_string channel line ;
      output_string channel "\n"

let hooks =
  Process.
    {
      on_spawn =
        (fun command arguments ->
          let message = Log.quote_shell_command command arguments in
          capture "" ;
          capture message);
      on_log = capture;
    }

(* Run [f] and capture the output of ran processes into the [output_file]. *)
let run_and_capture_output ~output_file (f : unit -> 'a Lwt.t) =
  let rec create_parent filename =
    let parent = Filename.dirname filename in
    if String.length parent < String.length filename then (
      create_parent parent ;
      if not (Sys.file_exists parent) then Unix.mkdir parent 0o755)
  in
  create_parent output_file ;
  let channel = open_out output_file in
  capture_output := Option.some channel ;
  Lwt.finalize f (fun () ->
      capture_output := None ;
      close_out channel ;
      unit)

(* Log regression output diff, with colors if enabled. *)
let log_regression_diff diff =
  List.iter
    (fun line ->
      if String.length line = 0 then Log.log ~level:Error ""
      else
        let color =
          match line.[0] with
          | '+' -> Log.Color.FG.green
          | '-' -> Log.Color.FG.red
          | '@' -> Log.Color.FG.cyan
          | _ -> Log.Color.reset
        in
        Log.log ~level:Error ~color "%s" line)
    (String.split_on_char '\n' diff)

(* Map from output directories to output files.
   Output directories are directories that are supposed to only contain output files.
   Subdirectories of output directories do not appear as keys in this map.
   In the map, output files can be in subdirectories (i.e. they can contain '/'). *)
let output_dirs_and_files : String_set.t String_map.t ref = ref String_map.empty

let register ~__FILE__ ~title ~tags ~output_file:_ f =
  let tags = "regression" :: tags in
  let output_dir = Filename.dirname __FILE__ // "expected" in
  let relative_output_file =
    let sanitized_title =
      (* We exclude ':' because of Windows. *)
      let sanitize_char = function
        | ( 'a' .. 'z'
          | 'A' .. 'Z'
          | '0' .. '9'
          | '_' | '-' | '.' | ' ' | '(' | ')' ) as x ->
            x
        | _ -> '-'
      in
      let full = String.map sanitize_char title in
      let max_length = 80 in
      if String.length full > max_length then String.sub full 0 max_length
      else full
    in
    Filename.basename __FILE__ // (sanitized_title ^ ".out")
  in
  let old_relative_output_files =
    String_map.find_opt output_dir !output_dirs_and_files
    |> Option.value ~default:String_set.empty
  in
  let stored_full_output_file = output_dir // relative_output_file in
  if String_set.mem relative_output_file old_relative_output_files then
    invalid_arg
      (sf
         "the output of test %S would be stored in %S, which is already used \
          by another test"
         title
         stored_full_output_file) ;
  output_dirs_and_files :=
    String_map.add
      output_dir
      (String_set.add relative_output_file old_relative_output_files)
      !output_dirs_and_files ;
  Test.register ~__FILE__ ~title ~tags (fun () ->
      (* when the stored output doesn't already exists, must reset regressions *)
      if
        not
          (Sys.file_exists stored_full_output_file
          || Cli.options.reset_regressions)
      then
        Test.fail
          "Regression output file not found: %s. To generate it, use: \
           --reset-regressions --title %s"
          (Log.quote_shell stored_full_output_file)
          (Log.quote_shell title) ;
      let capture_f ~full_output_file =
        run_and_capture_output ~output_file:full_output_file f
      in
      if Cli.options.reset_regressions then
        capture_f ~full_output_file:stored_full_output_file
      else
        (* store the current run into a temp file *)
        let temp_full_output_file = Temp.file relative_output_file in
        let* () = capture_f ~full_output_file:temp_full_output_file in
        (* compare the captured output with the stored output *)
        let diff_process =
          Process.spawn
            ~log_status_on_exit:false
            ~log_output:false
            "diff"
            [
              "--unified=0";
              "--label";
              "stored";
              "--label";
              "actual";
              stored_full_output_file;
              temp_full_output_file;
            ]
        in
        let* status = Process.wait diff_process in
        match status with
        | WEXITED 0 -> unit
        | _ ->
            let stream = Lwt_io.read_lines (Process.stdout diff_process) in
            let buffer = Buffer.create 1024 in
            let* () =
              Lwt_stream.iter
                (fun line ->
                  Buffer.add_string buffer line ;
                  Buffer.add_string buffer "\n")
                stream
            in
            let diff = Buffer.contents buffer in
            Buffer.reset buffer ;
            log_regression_diff diff ;
            Test.fail
              "Regression output file contains differences: %s. To accept the \
               differences, use: --reset-regressions --title %s"
              (Log.quote_shell stored_full_output_file)
              (Log.quote_shell title))

let check_unknown_output_files output_dir relative_output_files =
  let full_output_files =
    String_set.map
      (fun relative_output_file -> output_dir // relative_output_file)
      relative_output_files
  in
  let found_unknown = ref false in
  let mode = Cli.options.on_unknown_regression_files_mode in
  let log_unused = match mode with Fail -> Log.error | _ -> Log.warn in
  let rec browse path =
    let handle_file filename =
      let full = path // filename in
      match Sys.is_directory full with
      | exception Sys_error _ ->
          (* If we can't browse, ignore. *)
          ()
      | true -> browse full
      | false ->
          if not (String_set.mem full full_output_files) then
            if mode = Delete then
              try
                Sys.remove full ;
                Log.report "Deleted file: %s" full
              with Sys_error message ->
                Log.warn "Failed to delete file: %s" message
            else (
              log_unused "%s is not used by any test and can be deleted." full ;
              found_unknown := true)
    in
    Array.iter handle_file (Sys.readdir path) ;
    (* Check whether directory is empty now that we may have deleted files. *)
    match Sys.readdir path with
    | [||] ->
        if mode = Delete then
          try
            Sys.rmdir path ;
            Log.report "Deleted directory: %s" path
          with Sys_error message ->
            Log.warn "Failed to delete directory: %s" message
        else (
          log_unused "%s is empty and can be deleted." path ;
          found_unknown := true)
    | _ -> ()
  in
  browse output_dir ;
  if !found_unknown then (
    Log.warn
      "Use --on-unknown-regression-files delete to delete those files and/or \
       directories." ;
    if mode = Fail then exit 1) ;
  if mode = Delete then exit 0

let () =
  (* We cannot run [check_unknown_output_files] before [Cli.init],
     and we cannot run it from the [Test] module because it would create
     a circular dependency. *)
  Test.before_test_run @@ fun () ->
  if Cli.options.on_unknown_regression_files_mode <> Ignore then
    String_map.iter check_unknown_output_files !output_dirs_and_files
