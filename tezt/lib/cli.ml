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

open Base

type log_level = Quiet | Error | Warn | Report | Info | Debug

type temporary_file_mode = Delete | Delete_if_successful | Keep

type loop_mode = Infinite | Count of int

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
  tests_not_to_run : string list;
  tags_to_run : string list;
  tags_not_to_run : string list;
  list : [`Ascii_art | `Tsv] option;
  global_timeout : float option;
  test_timeout : float option;
  reset_regressions : bool;
  loop_mode : loop_mode;
  time : bool;
  starting_port : int;
  record : string option;
  from_records : string list;
  job : (int * int) option;
  job_count : int;
  suggest_jobs : bool;
  junit : string option;
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
  let tests_not_to_run = ref [] in
  let tags_to_run = ref [] in
  let tags_not_to_run = ref [] in
  let list = ref None in
  let global_timeout = ref None in
  let test_timeout = ref None in
  let reset_regressions = ref false in
  let loop_mode = ref (Count 1) in
  let time = ref false in
  let starting_port = ref 16384 in
  let record = ref None in
  let from_records = ref [] in
  let job = ref None in
  let job_count = ref 3 in
  let suggest_jobs = ref false in
  let junit = ref None in
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
  let set_job_count value =
    if value < 1 then raise (Arg.Bad "--job-count must be positive") ;
    job_count := value
  in
  let set_loop_count value =
    if value < 0 then raise (Arg.Bad "--loop-count must be positive or null") ;
    loop_mode := Count value
  in
  let set_job value =
    match value =~** rex "^([0-9]+)/([0-9]+)$" with
    | None ->
        raise
          (Arg.Bad
             "--job must be of the form: X/Y where X and Y are positive \
              integers")
    | Some (index, count) ->
        let int s =
          match int_of_string_opt s with
          | None ->
              raise (Arg.Bad ("value too large: " ^ s))
          | Some i ->
              i
        in
        let index = int index in
        let count = int count in
        if index < 1 then raise (Arg.Bad "--job index must be at least 1")
        else if count < 1 then raise (Arg.Bad "--job count must be at least 1")
        else if index > count then
          raise (Arg.Bad "--job index cannot be greater than job count")
        else job := Some (index, count)
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
        ( "--not-test",
          Arg.String
            (fun title -> tests_not_to_run := title :: !tests_not_to_run),
          "<TITLE> Only run tests which are not exactly entitled TITLE (see \
           SELECTING TESTS)." );
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
          Arg.Unit (fun () -> loop_mode := Infinite),
          " Restart from the beginning once all tests are done. All tests are \
           repeated until one of them fails or if you interrupt with Ctrl+C. \
           This is useful to reproduce non-deterministic failures. When used \
           in conjunction with --keep-going, tests are repeated even if they \
           fail, until you interrupt them with Ctrl+C." );
        ( "--loop-count",
          Arg.Int set_loop_count,
          "<COUNT> Same as --loop, but stop after all tests have been run \
           COUNT times. A value of 0 means tests are not run. The default \
           behavior corresponds to --loop-count 1. If you specify both --loop \
           and --loop-count, only the last one is taken into account." );
        ( "--time",
          Arg.Set time,
          " Print a summary of the total time taken by each test. Ignored if \
           a test failed. Includes the time read from records: to display a \
           record, you can use --time --loop-count 0 --from-record <FILE>." );
        ( "--starting-port",
          Arg.Set_int starting_port,
          " If tests need to open ports, they may start from this number." );
        ( "--record",
          Arg.String (fun file -> record := Some file),
          "<FILE> Record test results to FILE. This file can then be used \
           with --from-record. If you use --loop or --loop-count, times are \
           averaged for each test." );
        ( "--from-record",
          Arg.String (fun file -> from_records := file :: !from_records),
          "<FILE> Start from a file recorded with --record. Can be specified \
           multiple times. When using --time, test durations include tests \
           found in record files. When using --record, the new record which \
           is output does NOT include the input records. When using --junit, \
           reports do NOT include input records." );
        ( "--job",
          Arg.String set_job,
          "<INDEX>/<COUNT> COUNT must be at least 1 and INDEX must be between \
           1 and COUNT. Use --from-record to feed duration data from past \
           runs. Split the set of selected tests (see SELECTING TESTS) into \
           COUNT subsets of roughly the same total duration. Execute only one \
           of these subsets, specified by INDEX. Tests for which no time data \
           is available are given a default duration of 1 second. You can use \
           --list to see what tests are in a subset without actually running \
           the tests. A typical use is to run tests in parallel on different \
           machines. For instance, have one machine run with --job 1/3, one \
           with --job 2/3 and one with --job 3/3. Be sure to provide exactly \
           the same records with --from-record, in the same order, and to \
           select exactly the same set of tests (same tags, same --file and \
           same --test) for all machines, otherwise some tests may not be run \
           at all." );
        ( "--job-count",
          Arg.Int set_job_count,
          "<COUNT> Set the number of target jobs for --suggest-jobs (default \
           is 3)." );
        ("-j", Arg.Int set_job_count, "<COUNT> Same as --job-count.");
        ( "--suggest-jobs",
          Set suggest_jobs,
          " Read test results records specified with --from-records and \
           suggest a partition of the tests that would result in --job-count \
           sets of roughly the same total duration. Output each job as a list \
           of flags that can be passed to Tezt, followed by a shell comment \
           that denotes the expected duration of the job. A similar result \
           can be obtained with --list --job, except that the last job \
           suggested by --suggest-jobs uses --not-test to express \"all tests \
           that are not already in other jobs\", meaning that the last job \
           acts as a catch-all for unknown tests." );
        ( "--junit",
          Arg.String (fun path -> junit := Some path),
          "<FILE> Store test results in FILE using JUnit XML format. Time \
           information for each test is the sum of all runs of this test for \
           the current session." ) ]
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
       reports. Use --test (respectively --not-test) to select (respectively \
       unselect) a test by its title on the command-line.\n\n\
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
    tests_not_to_run = !tests_not_to_run;
    tags_to_run = !tags_to_run;
    tags_not_to_run = !tags_not_to_run;
    list = !list;
    global_timeout = !global_timeout;
    test_timeout = !test_timeout;
    reset_regressions = !reset_regressions;
    loop_mode = !loop_mode;
    time = !time;
    starting_port = !starting_port;
    record = !record;
    from_records = List.rev !from_records;
    job = !job;
    job_count = !job_count;
    suggest_jobs = !suggest_jobs;
    junit = !junit;
  }
