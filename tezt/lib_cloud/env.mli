(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* This module aims to encapsulate static values from the CLI and several
   functions that are used by the library. *)

(** Equivalent to [Cli.tezt_cloud], but if not present, checks if `TEZT_CLOUD`
    is provided. *)
val tezt_cloud : string

(** [ssh_private_key_filename ?home ()] returns the private key path associated to
    the [?home] path and the [tezt_cloud] argument. *)
val ssh_private_key_filename : ?home:string -> unit -> string

(** [ssh_public_key_filename ?home ()] returns the public key path associated to
        the path returned by [ssh_private_key_filename ?home ()]. *)
val ssh_public_key_filename : ?home:string -> unit -> string

(**
  - [`Localhost]: Agents and the orchestrator are run on the host machine.

  - [`Cloud]: The orchestrator is on the host machine. The agents are run onto
    the cloud.

  - [`Orchestrator]: The orchestrator is run on a VM. This mode is used by the
    proxy from the orchestrator point of view.

  - [`Host]: This mode is run by the host machine that initializes the
    orchestrator running on a VM.
*)
val mode : [`Localhost | `Cloud | `Orchestrator | `Host]

(** Equivalent to [Cli.prometheus]. *)
val prometheus : bool

(** Equivalent to [Cli.prometheus_export]. *)
val prometheus_export : bool

(** Equivalent to [Cli.prometheus_port]. *)
val prometheus_port : int

(** Equivalent to [Cli.prometheus_export_path]. *)
val prometheus_export_path : string option

(** Equivalent to [Cli.prometheus_snapshots]. *)
val prometheus_snapshots : (string * int option) list

(** Equivalent to [Cli.prometheus_scrape_interval]. *)
val prometheus_scrape_interval : int

(** Equivalent to [Cli.grafana]. *)
val grafana : bool

(** Equivalent to [Cli.grafana_legacy_source]. *)
val grafana_legacy_source : bool

(** Equivalent to [Cli.website]. *)
val website : bool

(** Equivalent to [Cli.website_port]. *)
val website_port : int

(** Equivalent to [Cli.destroy]. *)
val destroy : bool

(** Equivalent to [Cli.monitoring]. *)
val monitoring : bool

(** Equivalent to [Cli.keep_alive]. *)
val keep_alive : bool

(** Equivalent to [Cli.vms]. *)
val vms : int option

(** Equivalent to [Cli.vm_base_port]. *)
val vm_base_port : int

(** Equivalent to [Cli.ports_per_vm]. *)
val ports_per_vm : int

(** Equivalent to [Cli.machine_type]. *)
val machine_type : string

(** Equivalent to [Cli.max_run_duration]. *)
val max_run_duration : int

(** Equivalent to [Cli.no_max_run_duration]. *)
val no_max_run_duration : bool

(** Equivalent to [Cli.open_telemetry]. *)
val open_telemetry : bool

(** Equivalent to [Cli.alert_handlers]. *)
val alert_handlers : string list

(** If [Cli.dockerfile_alias] is provided, use it, otherwise default to [tezt_cloud]. *)
val dockerfile_alias : string

(** Docker path associated to [dockerfile_alias]. *)
val dockerfile : string

(** Docker registry path associated to [tezt_cloud]. *)
val docker_registry : string

(** Equivalent to [Cli.macosx]. *)
val macosx : bool

(** Equivalent to [Cli.check_file_consistency]. *)
val check_file_consistency : bool

(** Equivalent to [Cli.docker_host_network]. *)
val docker_host_network : bool

(** Equivalent to [Cli.push_docker]. *)
val push_docker : bool

(** Equivalent to [Cli.auto_approve]. *)
val auto_approve : bool

(** Equivalent to [Gcloud.project_id]. *)
val project_id : unit -> string Lwt.t

(** Equivalent to [Cli.faketime]. *)
val faketime : string option

(** Equivalent to [Cli.binaries_path]. *)
val binaries_path : string

(** [init ()] initialises and deploys a Docker registry using Terraform, only when the
    [mode] is either [`Host] or [`Cloud]. *)
val init : unit -> unit Lwt.t

(** [zone ()] retrieves the zone of the VM where the Docker registry is deployed, using
    a cache value. *)
val zone : unit -> string Lwt.t

(** [registry_uri ()] constructs the URI for the Docker registry. *)
val registry_uri : unit -> string Lwt.t

(** [wait_process ?sleep ~is_ready ~run ()] recursively waits for [~run] process to be ready.
    When the process is successful, but no [~is_ready], it loops after [?sleep] seconds. If
    it is ready, it returns the read output. If it fails, this gets logged. *)
val wait_process :
  ?sleep:int ->
  is_ready:(string -> bool) ->
  run:(unit -> Process.t) ->
  unit ->
  string Lwt.t

(** [run_command ?cmd_wrapper cmd args] can wrap the command given by [cmd] and [args] with
    a [Gcloud] wrapper, depending on the value of [?cmd_wrapper]. *)
val run_command :
  ?cmd_wrapper:Gcloud.cmd_wrapper -> string -> string list -> Process.t

(** [dns_domains ()] returns a list of fully qualified domain names (FQDNs) based on current
    configuration (given by [Cli.dns_domains]) and [mode] of operation. *)
val dns_domains : unit -> string list Lwt.t

(** [process_monitoring] enable the monitoring of process through prometheus-process-exporter
    needs to enable prometheus too *)
val process_monitoring : bool
