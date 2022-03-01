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

(** Command-line interface. *)

(** Log levels for standard output.

    The list below is sorted from the most quiet level to the most verbose level.

    - Absolutely no log has log level [Quiet].
      In other words, setting log level [Quiet] inhibits all logs.

    - [Error] logs are about errors which imply that the current test failed.
      This includes messages given to [Test.fail] and uncaught exceptions.

    - [Warn] logs are about errors that do not cause the current test to fail.
      This includes failure to clean up temporary files, for instance.

    - [Report] logs are informational messages that report the result of the current test.
      They tell the user whether the test was successful or not.
      They may also include information about how to re-run the test.

    - [Info] logs are informational messages that summarize what the test is doing.
      They tell the user that a particular milestone was reached.
      In tests, it is a good idea to log [Info] messages at significant checkpoints.

    - [Debug] logs give more details about exactly what is happening.
      They include external process outputs, exit codes, and signals which are sent.

    Additionally, some flags such as [--commands] and [--list] cause some information
    to be printed unconditionally, even with [--quiet]. Such kind of output is not
    considered to be log messages. *)
type log_level = Quiet | Error | Warn | Report | Info | Debug

(** What to do with temporary files after the test is finished. *)
type temporary_file_mode = Delete | Delete_if_successful | Keep

(** How many times to loop. *)
type loop_mode = Infinite | Count of int

(** Command-line options.

    [log_file] is [Some channel] where [channel] is open on [filename]
    if [--log-file filename] was specified on the command line.
    [channel] is automatically replaced by another channel if {!init}
    is called again with [--log-file]. [channel] is automatically closed
    either when replaced by another one or at exit. *)
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
}

(** Values for command-line options. *)
val options : options

(** Read command-line options to initialize [options].

    By default arguments are read from [Sys.argv], but you can specify [args]
    to override this behavior. Note that [args] must not contain the executable
    name ([Sys.argv.(0)]), only actual arguments.

    If you do not call [init], [options] will contain only default values.

    [init] exits the program on failure to parse the arguments (with code 2) or
    when either [-help] or [--help] is present (with code 0).

    Warning: if [--log-file] is specified, the file is truncated.
    So if you call [init] several times with the same [--log-file] argument,
    all logs between the calls to [init] are lost in this file. *)
val init : ?args:string list -> unit -> unit
