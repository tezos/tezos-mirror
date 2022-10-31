(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2020-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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

let capture_output : (string -> unit) option ref = ref None

(* Capture a string into a regression output. *)
let capture line =
  match !capture_output with
  | None -> ()
  | Some output ->
      output line ;
      output "\n"

let hooks : Process_hooks.t =
  {
    on_spawn =
      (fun command arguments ->
        let message = Log.quote_shell_command command arguments in
        capture "" ;
        capture message);
    on_log = capture;
  }

let run_and_capture_output ~capture (f : unit -> 'a Lwt.t) =
  capture_output := Some capture ;
  Lwt.finalize f @@ fun () ->
  capture_output := None ;
  unit

(* Run [f] and capture the output of ran processes into the [output_file]. *)
let run_and_capture_output_to_file ~output_file (f : unit -> 'a Lwt.t) =
  let rec create_parent filename =
    let parent = Filename.dirname filename in
    if String.length parent < String.length filename then (
      create_parent parent ;
      if not (Sys.file_exists parent) then
        try Unix.mkdir parent 0o755
        with Unix.Unix_error (EEXIST, _, _) ->
          (* Can happen with [-j] in particular. *)
          ())
  in
  create_parent output_file ;
  let channel = open_out output_file in
  capture_output := Some (output_string channel) ;
  Lwt.finalize f @@ fun () ->
  capture_output := None ;
  close_out channel ;
  unit

(* Map from output directories to output files.
   Output directories are directories that are supposed to only contain output files.
   Subdirectories of output directories do not appear as keys in this map.
   In the map, output files can be in subdirectories (i.e. they can contain '/'). *)
let output_dirs_and_files : String_set.t String_map.t ref = ref String_map.empty

let register ~__FILE__ ~title ~tags ?file f =
  let tags = "regression" :: tags in
  let output_dir = project_root // Filename.dirname __FILE__ // "expected" in
  let relative_output_file =
    let file =
      match file with
      | Some file -> file
      | None ->
          (* Sanitize title. We exclude ':' because of Windows. *)
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
    Filename.basename __FILE__ // (file ^ ".out")
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
  Test.register ~__FILE__ ~title ~tags @@ fun () ->
  (* when the stored output doesn't already exists, must reset regressions *)
  if
    not
      (Sys.file_exists stored_full_output_file || Cli.options.reset_regressions)
  then
    Test.fail
      "Regression output file not found: %s. To generate it, use: \
       --reset-regressions --title %s"
      (Log.quote_shell stored_full_output_file)
      (Log.quote_shell title) ;
  if Cli.options.reset_regressions then
    run_and_capture_output_to_file ~output_file:stored_full_output_file f
  else
    let* after =
      let buffer = Buffer.create 512 in
      let* () = run_and_capture_output ~capture:(Buffer.add_string buffer) f in
      Buffer.contents buffer |> String.split_on_char '\n' |> Array.of_list
      |> return
    in
    let before =
      read_file stored_full_output_file
      |> String.split_on_char '\n' |> Array.of_list
    in
    let diff =
      Diff.arrays
        ~equal:String.equal
        ~before:stored_full_output_file
        ~after:"captured"
        before
        after
    in
    if diff.different then (
      Diff.log (Diff.reduce_context diff) ;
      Test.fail
        "Regression output file contains differences: %s. To accept the \
         differences, use: --reset-regressions --title %s"
        (Log.quote_shell stored_full_output_file)
        (Log.quote_shell title)) ;
    unit

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
    let try_to_read_dir () =
      try Sys.readdir path
      with Sys_error _ ->
        (* Mostly happens if [path] does not exist or is not a
           directory, in which case we have nothing to browse.
           Could also happen because of permissions or other system issues,
           but since this is just a check to help developers, we
           don't want to bother them if this happens. *)
        [||]
    in
    Array.iter handle_file (try_to_read_dir ()) ;
    (* Check whether directory is empty now that we may have deleted files. *)
    match Sys.readdir path with
    | exception Sys_error _ -> ()
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
  !found_unknown

let () =
  (* We cannot run [check_unknown_output_files] before [Cli.init],
     and we cannot run it from the [Test] module because it would create
     a circular dependency. *)
  Test.before_test_run @@ fun () ->
  let check_all_unknown_output_files () =
    String_map.fold
      (fun output_dir relative_output_files found_unknown ->
        check_unknown_output_files output_dir relative_output_files
        || found_unknown)
      !output_dirs_and_files
      false
  in
  let warn_unknown_output_files () =
    Log.warn
      "Use --on-unknown-regression-files delete to delete those files and/or \
       directories."
  in
  match Cli.options.on_unknown_regression_files_mode with
  | Ignore -> ()
  | Warn ->
      let found_unknown = check_all_unknown_output_files () in
      if found_unknown then warn_unknown_output_files ()
  | Fail ->
      let found_unknown = check_all_unknown_output_files () in
      if found_unknown then (
        warn_unknown_output_files () ;
        exit 1)
  | Delete ->
      let _ = check_all_unknown_output_files () in
      (* Unknown output files are deleted inside [check_unknown_output_files]
         so we do nothing here. *)
      exit 0
