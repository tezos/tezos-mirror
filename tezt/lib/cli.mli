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

(** Command-line options. *)
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
}

(** Values for command-line options. *)
val options : options
