(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(** External processes launched by tests. *)

(** A process which was {!spawn}ed. *)
type t

(** Process can have some hooks attached when {!spawn}ed. *)
type hooks = {
  on_log : string -> unit;
      (** A hook that is called with every line that is being logged. *)
  on_spawn : string -> string list -> unit;
      (** A hook that is called whenever a process is being spawned. The first
          parameter is the command and the second are its arguments. *)
}

(** Create a process which starts running in the background.

    Usage: [spawn command arguments]

    If [log_status_on_exit] is [true] (which is the default), log the exit code
    when the process terminates.

    If [log_output] is [true] (which is the default), log the [stdout] and
    [stderr] output.

    Parameter [name] specifies the prefix (in brackets) that is added to
    each logged output line. Parameter [color] specifies the color of those
    output lines.

    Parameter [hooks] allows to attach some hooks to the process.

    Note that this function can only be called if [Background.register] is
    allowed (which is the case inside functions given to [Test.register]).

    Parameter [runner] specifies the runner on which to run the process.
    If unspecified, the process runs locally. Otherwise, it runs on the given
    runner using SSH.

    Example: [spawn "git" [ "log"; "-p" ]] *)
val spawn :
  ?runner:Runner.t ->
  ?log_status_on_exit:bool ->
  ?log_output:bool ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?env:string Base.String_map.t ->
  ?hooks:hooks ->
  string ->
  string list ->
  t

(** Same as {!spawn}, but with a channel to send data to the process [stdin]. *)
val spawn_with_stdin :
  ?runner:Runner.t ->
  ?log_status_on_exit:bool ->
  ?log_output:bool ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?env:string Base.String_map.t ->
  ?hooks:hooks ->
  string ->
  string list ->
  t * Lwt_io.output_channel

(** Send SIGTERM to a process. *)
val terminate : t -> unit

(** Send SIGKILL to a process. *)
val kill : t -> unit

(** Wait until a process terminates and return its status. *)
val wait : t -> Unix.process_status Lwt.t

(** Wait until a process terminates and check its status.

    If [not expect_failure] and exit code is not 0,
    or if [expect_failure] and exit code is 0,
    or if the process was killed, fail the test. *)
val check : ?expect_failure:bool -> t -> unit Lwt.t

(** Wait until a process terminates and check its status.

    If [exit_code] is different than [t] exit code,
    or if [msg] does not match the stderr output, fail the test.

    If [exit_code] is not specified, any non-zero code is accepted.
    If no [msg] is given, the stderr is ignored.*)
val check_error : ?exit_code:int -> ?msg:Base.rex -> t -> unit Lwt.t

(** Wait until a process terminates and read its standard output.

    Fail the test if the process failed, unless [expect_failure],
    in which case fail if the process succeeds. *)
val check_and_read_stdout : ?expect_failure:bool -> t -> string Lwt.t

(** Wait until a process terminates and read its standard error.

    Fail the test if the process failed, unless [expect_failure],
    in which case fail if the process succeeds. *)
val check_and_read_stderr : ?expect_failure:bool -> t -> string Lwt.t

(** Wait until a process terminates and read both its standard output
    and its standard error.

    Fail the test if the process failed, unless [expect_failure],
    in which case fail if the process succeeds. *)
val check_and_read_both : ?expect_failure:bool -> t -> (string * string) Lwt.t

(** Spawn a process, wait for it to terminate, and check its status. *)
val run :
  ?log_status_on_exit:bool ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?env:string Base.String_map.t ->
  ?expect_failure:bool ->
  string ->
  string list ->
  unit Lwt.t

(** Terminate all live processes created using {!spawn} and wait for them to terminate. *)
val clean_up : unit -> unit Lwt.t

(** Channel from which you can read the standard output of a process. *)
val stdout : t -> Lwt_io.input_channel

(** Channel from which you can read the standard error output of a process. *)
val stderr : t -> Lwt_io.input_channel

(** Get the name which was given to {!spawn}. *)
val name : t -> string

(** Get the PID of the given process. *)
val pid : t -> int

(** Spawn a process such as [run] and return its standard output.

    Fail the test if the process failed, unless [expect_failure],
    in which case fail if the process succeeds. *)
val run_and_read_stdout :
  ?log_status_on_exit:bool ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?env:string Base.String_map.t ->
  ?expect_failure:bool ->
  string ->
  string list ->
  string Lwt.t

(** Spawn a process such as [run] and return its standard error.

    Fail the test if the process failed, unless [expect_failure],
    in which case fail if the process succeeds. *)
val run_and_read_stderr :
  ?log_status_on_exit:bool ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?env:string Base.String_map.t ->
  ?expect_failure:bool ->
  string ->
  string list ->
  string Lwt.t

(** [program_path p] returns [Some path] if the shell command [command -v p]
    succeeds and prints [path]. Returns [None] otherwise. *)
val program_path : string -> string option Lwt.t
