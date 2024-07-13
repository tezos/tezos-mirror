(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Docker_registry : sig
  val init : unit -> unit Lwt.t

  val deploy : unit -> unit Lwt.t

  val get_docker_registry : unit -> string Lwt.t

  val get_hostname : unit -> string Lwt.t
end

module VM : sig
  module Workspace : sig
    val init : string list -> unit Lwt.t

    val get : unit -> string list Lwt.t

    val select : string -> unit Lwt.t

    val destroy : unit -> unit Lwt.t
  end

  val init : unit -> unit Lwt.t

  val deploy :
    max_run_duration:int option ->
    machine_type:string ->
    base_port:int ->
    ports_per_vm:int ->
    number_of_vms:int ->
    docker_image:string ->
    unit Lwt.t

  val points : unit -> string list Lwt.t

  val zone : unit -> string Lwt.t

  val destroy : string list -> unit Lwt.t
end

module State_bucket : sig
  val init : unit -> unit Lwt.t

  val deploy : unit -> unit Lwt.t
end
