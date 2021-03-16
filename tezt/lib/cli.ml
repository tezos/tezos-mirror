(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type log_level = Quiet | Error | Warn | Report | Info | Debug

type temporary_file_mode = Delete | Delete_if_successful | Keep

type options = {
  color : bool;
  log_level : log_level;
  log_file : string option;
  log_buffer_size : int;
  commands : bool;
  temporary_file_mode : temporary_file_mode;
  keep_going : bool;
  files_to_run : string list;
  tests_to_run : string list;
  tags_to_run : string list;
  tags_not_to_run : string list;
  list : [`Ascii_art | `Tsv] option;
  global_timeout : float option;
  test_timeout : float option;
  reset_regressions : bool;
  loop : bool;
  time : bool;
  starting_port : int;
}

let options =
  let color =
    ref (Unix.isatty Unix.stdout && Sys.getenv_opt "TERM" <> Some "dumb")
  in
  let log_level = ref Report in
  let log_file = ref None in
  let log_buffer_size = ref 50 in
  let commands = ref false in
  let temporary_file_mode = ref Delete in
  let keep_going = ref false in
  let files_to_run = ref [] in
  let tests_to_run = ref [] in
  let tags_to_run = ref [] in
  let tags_not_to_run = ref [] in
  let list = ref None in
  let global_timeout = ref None in
  let test_timeout = ref None in
  let reset_regressions = ref false in
  let loop = ref false in
  let time = ref false in
  let starting_port = ref 16384 in
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
        ( "--log-buffer-size",
          Arg.Set_int log_buffer_size,
          "<COUNT> Before logging an error on stdout, also log the last COUNT \
           messages that have been ignored because of the log level since the \
           last message that was not ignored. Default is 50." );
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
        ( "--list",
          Arg.Unit (fun () -> list := Some `Ascii_art),
          " List tests instead of running them." );
        ("-l", Arg.Unit (fun () -> list := Some `Ascii_art), " Same as --list.");
        ( "--list-tsv",
          Arg.Unit (fun () -> list := Some `Tsv),
          " List tests instead of running them but one-per-line, as \
           tab-separated-values." );
        ( "--file",
          Arg.String (fun file -> files_to_run := file :: !files_to_run),
          "<FILE> Only run tests implemented in source file FILE (see \
           SELECTING TESTS)." );
        ( "-f",
          Arg.String (fun file -> files_to_run := file :: !files_to_run),
          "<FILE> Same as --file." );
        ( "--test",
          Arg.String (fun title -> tests_to_run := title :: !tests_to_run),
          "<TITLE> Only run tests which are exactly entitled TITLE (see \
           SELECTING TESTS)." );
        ( "-t",
          Arg.String (fun title -> tests_to_run := title :: !tests_to_run),
          "<TITLE> Same as --test." );
        ( "--global-timeout",
          Arg.Float (fun delay -> global_timeout := Some delay),
          "<SECONDS> Fail if the set of tests takes more than SECONDS to run."
        );
        ( "--test-timeout",
          Arg.Float (fun delay -> test_timeout := Some delay),
          "<SECONDS> Fail if a test takes, on its own, more than SECONDS to \
           run." );
        ( "--reset-regressions",
          Arg.Set reset_regressions,
          " Remove regression test outputs if they exist, and regenerate them."
        );
        ( "--loop",
          Arg.Set loop,
          " Restart from the beginning once all tests are done. All tests are \
           repeated until one of them fails or if you interrupt with Ctrl+C. \
           This is useful to reproduce non-deterministic failures. When used \
           in conjunction with --keep-going, tests are repeated even if they \
           fail, until you interrupt them with Ctrl+C." );
        ( "--time",
          Arg.Set time,
          " Print a summary of the time taken by each test. Ignored if a test \
           failed." );
        ( "--starting-port",
          Arg.Set_int starting_port,
          " If tests need to open ports, they may start from this number." ) ]
  in
  let usage =
    (* This was formatted by ocamlformat. Sorry for all the slashes. *)
    "Usage: " ^ Sys.argv.(0)
    ^ " [OPTION..] [TAG..]\n\n\
       SELECTING TESTS\n\n\
      \  You can specify multiple tags, negated tags, titles and filenames on \
       the command line. Only tests which match all the following conditions \
       will be run:\n\
      \  - the test must have all tags and none of the negated tags;\n\
      \  - the test must have one of the specified titles;\n\
      \  - the test must be implemented in one of the specified files.\n\n\
      \  The tags of a test are given by the ~tags argument of Test.run. To \
       negate a tag, prefix it with a slash: /\n\n\
      \  The title of a test is given by the ~title argument of Test.run. It \
       is what is printed after [SUCCESS] (or [FAILURE] or [ABORTED]) in the \
       reports. Use --test to select a test by its title on the command-line.\n\n\
      \  The file in which a test is implemented is specified by the \
       ~__FILE__ argument of Test.run. In other words, it is the name of the \
       file in which the test is defined, without directories. Use --file to \
       select a test by its filename on the command-line.\n\n\
      \  For instance:\n\n\
      \    " ^ Sys.argv.(0)
    ^ " node bake /rpc --file bootstrap.ml --file sync.ml\n\n\
      \  will run all tests defined in either bootstrap.ml or sync.ml, which \
       have at least tags 'node' and 'bake', but which do not have the 'rpc' \
       tag.\n\n\
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
    log_buffer_size = !log_buffer_size;
    commands = !commands;
    temporary_file_mode = !temporary_file_mode;
    keep_going = !keep_going;
    files_to_run = !files_to_run;
    tests_to_run = !tests_to_run;
    tags_to_run = !tags_to_run;
    tags_not_to_run = !tags_not_to_run;
    list = !list;
    global_timeout = !global_timeout;
    test_timeout = !test_timeout;
    reset_regressions = !reset_regressions;
    loop = !loop;
    time = !time;
    starting_port = !starting_port;
  }
