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
  mutable color : bool;
  mutable log_level : log_level;
  mutable log_file : out_channel option;
  mutable log_buffer_size : int;
  mutable commands : bool;
  mutable temporary_file_mode : temporary_file_mode;
  mutable keep_going : bool;
  mutable files_to_run : string list;
  mutable tests_to_run : string list;
  mutable tests_not_to_run : string list;
  mutable tags_to_run : string list;
  mutable tags_not_to_run : string list;
  mutable list : [`Ascii_art | `Tsv] option;
  mutable global_timeout : float option;
  mutable test_timeout : float option;
  mutable retry : int;
  mutable regression_dir : string;
  mutable reset_regressions : bool;
  mutable delete_unknown_regression_files : bool;
  mutable loop_mode : loop_mode;
  mutable time : bool;
  mutable starting_port : int;
  mutable record : string option;
  mutable from_records : string list;
  mutable job : (int * int) option;
  mutable job_count : int;
  mutable suggest_jobs : bool;
  mutable junit : string option;
  mutable skip : int;
  mutable only : int option;
  mutable test_args : string String_map.t;
}

let options =
  {
    color = Unix.isatty Unix.stdout && Sys.getenv_opt "TERM" <> Some "dumb";
    log_level = Report;
    log_file = None;
    log_buffer_size = 50;
    commands = false;
    temporary_file_mode = Delete;
    keep_going = false;
    files_to_run = [];
    tests_to_run = [];
    tests_not_to_run = [];
    tags_to_run = [];
    tags_not_to_run = [];
    list = None;
    global_timeout = None;
    test_timeout = None;
    retry = 0;
    regression_dir = "tezt/_regressions";
    reset_regressions = false;
    delete_unknown_regression_files = false;
    loop_mode = Count 1;
    time = false;
    starting_port = 16384;
    record = None;
    from_records = [];
    job = None;
    job_count = 1;
    suggest_jobs = false;
    junit = None;
    skip = 0;
    only = None;
    test_args = String_map.empty;
  }

let () = at_exit @@ fun () -> Option.iter close_out options.log_file

let init ?args () =
  let set_log_level = function
    | "quiet" -> options.log_level <- Quiet
    | "error" -> options.log_level <- Error
    | "warn" -> options.log_level <- Warn
    | "report" -> options.log_level <- Report
    | "info" -> options.log_level <- Info
    | "debug" -> options.log_level <- Debug
    | level -> raise (Arg.Bad (Printf.sprintf "invalid log level: %S" level))
  in
  let set_log_file filename =
    Option.iter close_out options.log_file ;
    (* The channel we open here will be closed either:
       - by the next call to [set_log_file], if there are several [--log-file]
         arguments or if [Cli.init] is called several times;
       - at exit. *)
    options.log_file <- Some (open_out filename)
  in
  let set_job_count value =
    if value < 1 then raise (Arg.Bad "--job-count must be positive") ;
    options.job_count <- value
  in
  let set_loop_count value =
    if value < 0 then raise (Arg.Bad "--loop-count must be positive or null") ;
    options.loop_mode <- Count value
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
          | None -> raise (Arg.Bad ("value too large: " ^ s))
          | Some i -> i
        in
        let index = int index in
        let count = int count in
        if index < 1 then raise (Arg.Bad "--job index must be at least 1")
        else if count < 1 then raise (Arg.Bad "--job count must be at least 1")
        else if index > count then
          raise (Arg.Bad "--job index cannot be greater than job count")
        else options.job <- Some (index, count)
  in
  let add_from_record path =
    if not (Sys.is_directory path) then
      options.from_records <- path :: options.from_records
    else
      let records =
        Sys.readdir path |> Array.to_list
        |> List.filter (fun name -> Filename.extension name = ".json")
        |> List.map (fun name -> path // name)
      in
      options.from_records <- records @ options.from_records
  in
  let set_skip value =
    if value < 0 then raise (Arg.Bad "--skip must be non-negative") ;
    options.skip <- value
  in
  let set_only value =
    if value <= 0 then raise (Arg.Bad "--only must be at least one") ;
    options.only <- Some value
  in
  let add_test_arg value =
    let len = String.length value in
    let rec find_equal i =
      if i >= len then None
      else if value.[i] = '=' then Some i
      else find_equal (i + 1)
    in
    let (parameter, value) =
      match find_equal 0 with
      | None -> (value, "true")
      | Some i -> (String.sub value 0 i, String.sub value (i + 1) (len - i - 1))
    in
    options.test_args <- String_map.add parameter value options.test_args
  in
  let spec =
    Arg.align
      [
        ( "--color",
          Arg.Unit (fun () -> options.color <- true),
          " Use colors in output." );
        ( "--no-color",
          Arg.Unit (fun () -> options.color <- false),
          " Do not use colors in output." );
        ( "--log-level",
          Arg.String set_log_level,
          "<LEVEL> Set log level to LEVEL. Possible LEVELs are: quiet, error, \
           warn, report, info, debug. Default is report." );
        ( "--log-file",
          Arg.String set_log_file,
          "<FILE> Also log to FILE (in verbose mode: --log-level only applies \
           to stdout)." );
        ( "--log-buffer-size",
          Arg.Int (fun x -> options.log_buffer_size <- x),
          "<COUNT> Before logging an error on stdout, also log the last COUNT \
           messages that have been ignored because of the log level since the \
           last message that was not ignored. Default is 50." );
        ( "--verbose",
          Arg.Unit (fun () -> options.log_level <- Debug),
          " Same as --log-level debug." );
        ( "-v",
          Arg.Unit (fun () -> options.log_level <- Debug),
          " Same as --verbose." );
        ( "--quiet",
          Arg.Unit (fun () -> options.log_level <- Quiet),
          " Same as --log-level quiet." );
        ( "-q",
          Arg.Unit (fun () -> options.log_level <- Quiet),
          " Same as --quiet." );
        ( "--info",
          Arg.Unit (fun () -> options.log_level <- Info),
          " Same as --log-level info." );
        ( "-i",
          Arg.Unit (fun () -> options.log_level <- Info),
          " Same as --info." );
        ( "--commands",
          Arg.Unit (fun () -> options.commands <- true),
          " Output commands which are run, in a way that is easily copy-pasted \
           for manual reproductibility." );
        ( "-c",
          Arg.Unit (fun () -> options.commands <- true),
          " Same as --commands." );
        ( "--delete-temp",
          Arg.Unit (fun () -> options.temporary_file_mode <- Delete),
          " Delete temporary files and directories that were created (this is \
           the default)." );
        ( "--delete-temp-if-success",
          Arg.Unit
            (fun () -> options.temporary_file_mode <- Delete_if_successful),
          " Delete temporary files and directories, except if the test failed."
        );
        ( "--keep-temp",
          Arg.Unit (fun () -> options.temporary_file_mode <- Keep),
          " Do not delete temporary files and directories that were created." );
        ( "--keep-going",
          Arg.Unit (fun () -> options.keep_going <- true),
          " If a test fails, continue with the remaining tests instead of \
           stopping. Aborting manually with Ctrl+C still stops everything." );
        ( "-k",
          Arg.Unit (fun () -> options.keep_going <- true),
          " Same as --keep-going." );
        ( "--list",
          Arg.Unit (fun () -> options.list <- Some `Ascii_art),
          " List tests instead of running them." );
        ( "-l",
          Arg.Unit (fun () -> options.list <- Some `Ascii_art),
          " Same as --list." );
        ( "--list-tsv",
          Arg.Unit (fun () -> options.list <- Some `Tsv),
          " List tests instead of running them but one-per-line, as \
           tab-separated-values." );
        ( "--file",
          Arg.String
            (fun file -> options.files_to_run <- file :: options.files_to_run),
          "<FILE> Only run tests implemented in source file FILE (see \
           SELECTING TESTS)." );
        ( "-f",
          Arg.String
            (fun file -> options.files_to_run <- file :: options.files_to_run),
          "<FILE> Same as --file." );
        ( "--test",
          Arg.String
            (fun title -> options.tests_to_run <- title :: options.tests_to_run),
          "<TITLE> Only run tests which are exactly entitled TITLE (see \
           SELECTING TESTS)." );
        ( "-t",
          Arg.String
            (fun title -> options.tests_to_run <- title :: options.tests_to_run),
          "<TITLE> Same as --test." );
        ( "--not-test",
          Arg.String
            (fun title ->
              options.tests_not_to_run <- title :: options.tests_not_to_run),
          "<TITLE> Only run tests which are not exactly entitled TITLE (see \
           SELECTING TESTS)." );
        ( "--global-timeout",
          Arg.Float (fun delay -> options.global_timeout <- Some delay),
          "<SECONDS> Fail if the set of tests takes more than SECONDS to run."
        );
        ( "--test-timeout",
          Arg.Float (fun delay -> options.test_timeout <- Some delay),
          "<SECONDS> Fail if a test takes, on its own, more than SECONDS to \
           run." );
        ( "--retry",
          Arg.Int (fun retry -> options.retry <- retry),
          "<COUNT> Retry each failing test up to COUNT times. If one retry is \
           successful, the test is considered successful." );
        ( "--regression-dir",
          Arg.String (fun dir -> options.regression_dir <- dir),
          "<PATH> Where to store the output of regression tests. Default is: \
           tezt/_regressions" );
        ( "--reset-regressions",
          Arg.Unit (fun () -> options.reset_regressions <- true),
          " Remove regression test outputs if they exist, and regenerate them."
        );
        ( "--delete-unknown-regression-files",
          Arg.Unit (fun () -> options.delete_unknown_regression_files <- true),
          " Delete regression test outputs that are not declared by any test. \
           To check which files would be deleted, run without this option \
           first. If there are files that would be deleted by this flag, a \
           warning is emitted for each of them." );
        ( "--loop",
          Arg.Unit (fun () -> options.loop_mode <- Infinite),
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
          Arg.Unit (fun () -> options.time <- true),
          " Print a summary of the total time taken by each test. Ignored if a \
           test failed. Includes the time read from records: to display a \
           record, you can use --time --loop-count 0 --from-record <FILE>." );
        ( "--starting-port",
          Arg.Int (fun x -> options.starting_port <- x),
          " If tests need to open ports, they may start from this number." );
        ( "--record",
          Arg.String (fun file -> options.record <- Some file),
          "<FILE> Record test results to FILE. This file can then be used with \
           --from-record. If you use --loop or --loop-count, times are \
           averaged for each test." );
        ( "--from-record",
          Arg.String add_from_record,
          "<FILE> Start from a file recorded with --record. Can be specified \
           multiple times. If <FILE> is a directory, this is equivalent to \
           specifying --from-record for all files in this directory that have \
           the .json extension. When using --time, test durations include \
           tests found in record files. When using --record, the new record \
           which is output does NOT include the input records. When using \
           --junit, reports do NOT include input records." );
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
          "<COUNT> Run COUNT tests in parallel, in separate processes.\n\
           With --suggest-jobs, set the number of target jobs for \
           --suggest-jobs instead (default is 1)." );
        ("-j", Arg.Int set_job_count, "<COUNT> Same as --job-count.");
        ( "--suggest-jobs",
          Arg.Unit (fun () -> options.suggest_jobs <- true),
          " Read test results records specified with --from-records and \
           suggest a partition of the tests that would result in --job-count \
           sets of roughly the same total duration. Output each job as a list \
           of flags that can be passed to Tezt, followed by a shell comment \
           that denotes the expected duration of the job. A similar result can \
           be obtained with --list --job, except that the last job suggested \
           by --suggest-jobs uses --not-test to express \"all tests that are \
           not already in other jobs\", meaning that the last job acts as a \
           catch-all for unknown tests." );
        ( "--junit",
          Arg.String (fun path -> options.junit <- Some path),
          "<FILE> Store test results in FILE using JUnit XML format. Time \
           information for each test is the sum of all runs of this test for \
           the current session. Test result (success or failure) is the result \
           for the last run of the test." );
        ( "--skip",
          Arg.Int set_skip,
          "<COUNT> Skip the first COUNT tests. This filter is applied after  \
           --job and before --only." );
        ( "--only",
          Arg.Int set_only,
          "<COUNT> Only run the first COUNT tests. This filter is applied \
           after --job and --skip." );
        ( "--test-arg",
          Arg.String add_test_arg,
          "<PARAMETER>=<VALUE> Pass a generic argument to tests. Tests can get \
           this argument with Cli.get. --test-arg <PARAMETER> is a short-hand \
           for: --test-arg <PARAMETER>=true" );
        ( "-a",
          Arg.String add_test_arg,
          "<PARAMETER>=<VALUE> Same as --test-arg." );
      ]
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
      \  The file in which a test is implemented is specified by the ~__FILE__ \
       argument of Test.run. In other words, it is the name of the file in \
       which the test is defined, without directories. Use --file to select a \
       test by its filename on the command-line.\n\n\
      \  For instance:\n\n\
      \    " ^ Sys.argv.(0)
    ^ " node bake /rpc --file bootstrap.ml --file sync.ml\n\n\
      \  will run all tests defined in either bootstrap.ml or sync.ml, which \
       have at least tags 'node' and 'bake', but which do not have the 'rpc' \
       tag.\n\n\
       OPTIONS\n"
  in
  let add_tag tag =
    if tag = "" || tag.[0] <> '/' then
      options.tags_to_run <- tag :: options.tags_to_run
    else
      options.tags_not_to_run <-
        String.sub tag 1 (String.length tag - 1) :: options.tags_not_to_run
  in
  let argv =
    let executable_name =
      if Array.length Sys.argv > 0 then Sys.argv.(0) else Sys.executable_name
    in
    match args with
    | None -> Sys.argv
    | Some x -> Array.of_list (executable_name :: x)
  in
  try Arg.parse_argv argv spec add_tag usage with
  | Arg.Bad msg ->
      Printf.eprintf "%s" msg ;
      exit 2
  | Arg.Help msg ->
      Printf.printf "%s" msg ;
      exit 0

let () = init ()

let get ?default parse parameter =
  match String_map.find_opt parameter options.test_args with
  | Some value -> (
      match parse value with
      | None -> failwith (sf "invalid value for -a %s: %s" parameter value)
      | Some value -> value)
  | None -> (
      match default with
      | None ->
          failwith
            (sf
               "missing test argument %s, please specify it with: -a %s=<VALUE>"
               parameter
               parameter)
      | Some default -> default)

let get_bool ?default parameter = get ?default bool_of_string_opt parameter

let get_int ?default parameter = get ?default int_of_string_opt parameter

let get_float ?default parameter = get ?default float_of_string_opt parameter

let get_string ?default parameter = get ?default Option.some parameter
