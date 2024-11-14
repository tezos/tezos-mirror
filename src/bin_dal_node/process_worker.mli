(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** A unix worker process, that includes a tiny ipc protocol using unix pipes.
    A message in this protocol involves sending the size, followed by the
    payload. *)
type t

(** Error in inter process communication (IPC) between the main and forked
   process. *)
type error += Process_worker_ipc_error of string

(** [pid pw] returns the pid of the process worker [pw] *)
val pid : t -> int

(** [output_channel pw] accesses the output channel of the process_worker [pw] *)
val output_channel : t -> Lwt_io.output_channel

(** [input_channel pw] accesses the input channel of the process_worker [pw] *)
val input_channel : t -> Lwt_io.input_channel

(** [read_message inchan] reads the message [msg] from the Lwt_io.input_channel
    [inchan]. The promise is fulfilled when the entire message, as sent by the
    [write_message] is received. It returns `End_of_file if the channel was
    closed, or Error if an unexpected error is raised. *)
val read_message :
  Lwt_io.input_channel ->
  ([`Message of bytes | `End_of_file], error trace) result Lwt.t

(** [write_message outchan msg] writes the message [msg] to the
    Lwt_io.output_channel
    The size of the message [msg] is implicitly sent, ensuring the whole message
    to be sent. The promise is fulfilled and returns `Write_ok is the whole message
    was correctly written on the output channel, or `End_of_file if the channel
    was closed. Returns Error for unexpected errors *)
val write_message :
  Lwt_io.output_channel ->
  bytes ->
  ([`Write_ok | `End_of_file], error trace) result Lwt.t

(** [run f arg] will run the function [f] : input_channel output_channel arg,
    in an forked process. This [run] function forks, open some pipes ends
    between the parent and the child. The [f] function is expected to read and
    write on the provided unix pipes. The returned value is a process manager
    containing the other pipe ends, on which read_message and write_message can
    be used.
    As of now, only one reader and one writer are supported. There is not yet
    a locking mechanism and lwt channels are publicly accessible.
    On parent termination, a sigterm signal is sent to the child process,
    triggering a Lwt.cancel of the [f] function.
    *)
val run :
  (Lwt_io.input_channel -> Lwt_io.output_channel -> 'a -> unit tzresult Lwt.t) ->
  'a ->
  t tzresult Lwt.t
