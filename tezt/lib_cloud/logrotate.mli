(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** [write_config name ?pidfile ?max_rotations target_file agent] will write a
    logrotate configuration file for the rotation of [target_file].
    [name] is used to identify a logrotate task and is expected to be the
    executable name generating the logs
    [pidfile] must be a readable file containing the pid of the process writing into the
    log to rotate. If [pidfile] is not provided or none, logrotate will use
    pkill to send sighup to all processes named [name], use with caution.
    The process whose pid is written in [pidfile] or named [name] if [pidfile]
    is none has to reopen its log file on SIGHUP to prevent data loss or race
    condition. The configuration allows for [target_file] to have
    [max_rotations] on the [agent], until the oldest files are deleted. *)
val write_config :
  name:string ->
  ?pidfile:string ->
  ?max_rotations:int ->
  target_file:string ->
  Agent.t ->
  unit Lwt.t

(** [run name agent] will run the logrotate [name] task on the [agent] *)
val run : name:string -> Agent.t -> unit Lwt.t
