(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* This module aims to encapsulate static values from the CLI and several
   functions that are used by the library. *)

type docker_image = Gcp of {alias : string} | Octez_latest_release

val tezt_cloud : string

val mode : [`Localhost | `Cloud]

val prometheus : bool

val prometheus_port : int

val prometheus_export : bool

val prometheus_snapshot_filename : string option

val prometheus_scrape_interval : int

val grafana : bool

val website : bool

val website_port : int

val destroy : bool

val monitoring : bool

val keep_alive : bool

val ssh_private_key_filename : string

val ssh_public_key_filename : string

val dockerfile_alias : string

val dockerfile : string

val docker_registry : string

val docker_image : docker_image

val vms : int option

val vm_base_port : int

val ports_per_vm : int

val machine_type : string

val max_run_duration : int

val no_max_run_duration : bool

val init : unit -> unit Lwt.t

val project_id : unit -> string Lwt.t

val registry_uri : unit -> string Lwt.t

val uri_of_docker_image : docker_image -> string Lwt.t

val zone : unit -> string Lwt.t

val wait_process :
  ?sleep:int ->
  is_ready:(string -> bool) ->
  run:(unit -> Process.t) ->
  unit ->
  string Lwt.t

val run_command :
  ?cmd_wrapper:Gcloud.cmd_wrapper -> string -> string list -> Process.t
