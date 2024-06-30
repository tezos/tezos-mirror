(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* A job for deploying a docker regisry based on `TEZT_CLOUD` variable. *)
val deploy_docker_registry : unit -> unit Lwt.t

(* A job for building and pushing docker images on the registry. *)
val docker_build : push:bool -> unit -> unit Lwt.t

(** [register ~tags] register a set of jobs that can be used for setting
   requirements related to cloud scenarios. Some tags can be given for all the
   registered jobs. *)
val register : tags:string list -> unit
