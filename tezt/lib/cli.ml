(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type log_level = Quiet | Error | Warn | Report | Info | Debug

type temporary_file_mode = Delete | Delete_if_successful | Keep

type options = {
  color : bool;
  log_level : log_level;
  log_file : string option;
  commands : bool;
  temporary_file_mode : temporary_file_mode;
  keep_going : bool;
  files_to_run : string list;
  tests_to_run : string list;
  tags_to_run : string list;
  tags_not_to_run : string list;
  list : bool;
}

let options =
  let color =
    ref (Unix.isatty Unix.stdout && Sys.getenv_opt "TERM" <> Some "dumb")
  in
  let log_level = ref Report in
  let log_file = ref None in
  let commands = ref false in
  let temporary_file_mode = ref Delete in
  let keep_going = ref false in
  let files_to_run = ref [] in
  let tests_to_run = ref [] in
  let tags_to_run = ref [] in
  let tags_not_to_run = ref [] in
  let list = ref false in
  let set_log_level = function
    | "quiet" ->
        log_level := Quiet
    | "error" ->
        log_level := Error
    | "warn" ->
        log_level := Warn
    | "report" ->
        log_level := Report
    | "info" ->
        log_level := Info
    | "debug" ->
        log_level := Debug
    | level ->
        raise (Arg.Bad (Printf.sprintf "invalid log level: %S" level))
  in
  let spec =
    Arg.align
      [ ("--color", Arg.Set color, " Use colors in output.");
        ("--no-color", Arg.Clear color, " Do not use colors in output.");
        ( "--log-level",
          Arg.String set_log_level,
          "<LEVEL> Set log level to LEVEL. Possible LEVELs are: quiet, error, \
           warn, report, info, debug. Default is report." );
        ( "--log-file",
          Arg.String (fun f -> log_file := Some f),
          "<FILE> Also log to FILE (in verbose mode: --log-level only applies \
           to stdout)." );
        ( "--verbose",
          Arg.Unit (fun () -> log_level := Debug),
          " Same as --log-level debug." );
        ("-v", Arg.Unit (fun () -> log_level := Debug), " Same as --verbose.");
        ( "--quiet",
          Arg.Unit (fun () -> log_level := Quiet),
          " Same as --log-level quiet." );
        ("-q", Arg.Unit (fun () -> log_level := Quiet), " Same as --quiet.");
        ( "--info",
          Arg.Unit (fun () -> log_level := Info),
          " Same as --log-level info." );
        ("-i", Arg.Unit (fun () -> log_level := Info), " Same as --info.");
        ( "--commands",
          Arg.Unit (fun () -> commands := true),
          " Output commands which are run, in a way that is easily \
           copy-pasted for manual reproductibility." );
        ("-c", Arg.Unit (fun () -> commands := true), " Same as --commands.");
        ( "--delete-temp",
          Arg.Unit (fun () -> temporary_file_mode := Delete),
          " Delete temporary files and directories that were created (this is \
           the default)." );
        ( "--delete-temp-if-success",
          Arg.Unit (fun () -> temporary_file_mode := Delete_if_successful),
          " Delete temporary files and directories, except if the test failed."
        );
        ( "--keep-temp",
          Arg.Unit (fun () -> temporary_file_mode := Keep),
          " Do not delete temporary files and directories that were created."
        );
        ( "--keep-going",
          Arg.Set keep_going,
          " If a test fails, continue with the remaining tests instead of \
           stopping. Aborting manually with Ctrl+C still stops everything." );
        ("-k", Arg.Set keep_going, " Same as --keep-going.");
        ("--list", Arg.Set list, " List tests instead of running them.");
        ("-l", Arg.Set list, " Same as --list.");
        ( "--file",
          Arg.String (fun file -> files_to_run := file :: !files_to_run),
          "<FILE> Only run tests implemented in source file FILE. You can \
           specify several --file options, all of them will be run. \
           Specifying tags (see TAGS) or --test may still reduce the set of \
           tests to run." );
        ( "-f",
          Arg.String (fun file -> files_to_run := file :: !files_to_run),
          "<TITLE> Same as --file." );
        ( "--test",
          Arg.String (fun title -> tests_to_run := title :: !tests_to_run),
          "<TITLE> Only run tests which are exactly entitled TITLE. You can \
           specify several --test options, all of them will be run. \
           Specifying tags (see TAGS) or --file may still reduce the set of \
           tests to run." );
        ( "-t",
          Arg.String (fun title -> tests_to_run := title :: !tests_to_run),
          "<TITLE> Same as --test." ) ]
  in
  let usage =
    (* This was formatted by ocamlformat. Sorry for all the slashes. *)
    "Usage: " ^ Sys.argv.(0)
    ^ " [OPTION..] [TAG..]\n\n\
       TAGS\n\n\
      \  You can specify tags or negated tags on the command line.\n\
      \  Only tests which match all tags and no negated tags will be run.\n\
      \  To negate a tag, prefix it with a slash: /\n\n\
      \  For instance:\n\n\
      \    " ^ Sys.argv.(0)
    ^ " node bootstrap /rpc\n\n\
      \  will run all tests tagged with at least tags 'node' and 'bootstrap', \
       but not\n\
      \  tests tagged with tag 'rpc'.\n\n\
      \  You can get the list of tests and their tags with --list.\n\n\
       OPTIONS\n"
  in
  let add_tag tag =
    if tag = "" || tag.[0] <> '/' then tags_to_run := tag :: !tags_to_run
    else
      tags_not_to_run :=
        String.sub tag 1 (String.length tag - 1) :: !tags_not_to_run
  in
  Arg.parse spec add_tag usage ;
  {
    color = !color;
    log_level = !log_level;
    log_file = !log_file;
    commands = !commands;
    temporary_file_mode = !temporary_file_mode;
    keep_going = !keep_going;
    files_to_run = !files_to_run;
    tests_to_run = !tests_to_run;
    tags_to_run = !tags_to_run;
    tags_not_to_run = !tags_not_to_run;
    list = !list;
  }
