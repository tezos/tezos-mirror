(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Docker_registry : sig
  val init : unit -> unit Lwt.t

  val deploy : tezt_cloud:string -> project_id:string -> unit Lwt.t

  val get_docker_registry : tezt_cloud:string -> string Lwt.t

  val get_hostname : tezt_cloud:string -> string Lwt.t

  val get_zone : tezt_cloud:string -> string Lwt.t
end

module VM : sig
  module Workspace : sig
    val init : tezt_cloud:string -> string list -> unit Lwt.t

    val list : tezt_cloud:string -> string list Lwt.t

    val select : string -> unit Lwt.t

    val destroy : tezt_cloud:string -> unit Lwt.t
  end

  val init : unit -> unit Lwt.t

  val deploy :
    auto_approve:bool ->
    max_run_duration:int option ->
    machine_type:string ->
    disk_type:string option ->
    disk_size_gb:int option ->
    base_port:int ->
    ports_per_vm:int ->
    number_of_vms:int ->
    docker_image:string ->
    os:Types.Os.t ->
    prometheus_port:int ->
    unit Lwt.t

  val points : unit -> string list Lwt.t

  val zone : unit -> string Lwt.t

  val destroy : string list -> project_id:string -> unit Lwt.t
end
