(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** [write_config name target_file max_rotations agent] will write a logrotate
    configuration file for the rotation of [target_file], [pidfile] must be
    a readable file containing the pid of the process writing into the log to
    rotate. The process have to reopen its log file on SIGHUP.
    The configuration allows for [target_file] to have [max_rotations] on the
    [agent], until the oldest files are deleted.
    [name] is used to identify a logrotate task and is expected to be the
    executable name generating the logs (only used as identifier at this point) *)
val write_config :
  name:string ->
  pidfile:string ->
  target_file:string ->
  max_rotations:int ->
  Agent.t ->
  unit Lwt.t

(** [run name agent] will run the logrotate [name] task on the [agent] *)
val run : name:string -> Agent.t -> unit Lwt.t
