(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** [write_config name target_file max_rotations agent] will write a logrotate
    configuration file for the rotation of [target_file], will a maximum rotation
    of [max_rotations] on the [agent]. *)
val write_config :
  name:string ->
  target_file:string ->
  max_rotations:int ->
  Agent.t ->
  unit Lwt.t

(** [run name agent] will run the logrotate [name] task on the [agent] *)
val run : name:string -> Agent.t -> unit Lwt.t
