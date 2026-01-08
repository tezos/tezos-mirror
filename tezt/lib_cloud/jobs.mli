(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* A job for deploying a docker regisry based on `TEZT_CLOUD` variable. *)
val deploy_docker_registry : unit -> unit Lwt.t

(* A job for building and pushing docker images on the registry. *)
val docker_build :
  ?docker_image:Agent.Configuration.docker_image ->
  ?args:(string * string) list ->
  push:bool ->
  ssh_public_key:string ->
  unit ->
  unit Lwt.t

(* The docker containers are killed and restarted from scratch. *)
val clean_up_vms : unit -> unit Lwt.t
