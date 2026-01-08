(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

include module type of Tezt.Cli

(** The name and encoding of the scenario specific options if mentionned in the config file. *)
val scenario_specific : (string * Data_encoding.Json.t) option

(** When [localhost] is [true], the cloud scenario should be run locally. This
    can be used to test a scenario before deploying it. *)
val localhost : bool

(** When [monitoring] is [true], VMs are monitored with netdata. *)
val monitoring : bool

(** [destroy] is [true] for destroying temporary resources run by a scenario.
    Otherwise, this has to be done manually. *)
val destroy : bool

(** When [keep_alive] is [true], the user must press <enter> to terminate the
  scenario. This can be used for debugging purpose or for inspecting the state
  of VMs at the end of a scenario. *)
val keep_alive : bool

(** [project_id] enables to specify a project id that should be used. This
    option should be set when the scenario should be run for a different
    project id than the default one. *)
val project_id : string option

(** [vms] enables to set the number of vms that will be spawned. *)
val vms : int option

(** First port opened on the VM. Defaults to [30_000]. *)
val vm_base_port : int

(** Number of consecutive ports opened on the VM from the base port. Defaults to [50]. *)
val ports_per_vm : int

(** When [proxy] is [true], the scenario is run via a VM instead of the host
  machine. *)
val proxy : bool

(** OS used by the VM. *)
val os : Types.Os.t

(** Flag which indicates whether Grafana should be run or not. *)
val grafana : bool

(** Flag which indicates whether Grafana should use legacy data source name or not.
    Only used when importing prometheus snapshot. *)
val grafana_legacy_source : bool

(** The alert handlers to be registered by the alert manager. *)
val alert_handlers : string list

(** When [prometheus] is [true] a Prometheus instance is run locally and
    metrics of the experiments are exported to prometheus. At the end of the
    test, the database is snapshotted so that it can be imported later on. *)
val prometheus : bool

(** If [true], exports a Promtheus snapshot at the end of the scenario. *)
val prometheus_export : bool

(** Specify the port of the Prometheus instance on the host machine. *)
val prometheus_port : int

(** Specify the name of the prometheus snapshot. *)
val prometheus_export_path : string option

(** Specify the name of the prometheus snapshots to import. *)
val prometheus_snapshots : (string * int option) list

(** Specify the scraping interval of Prometheus. *)
val prometheus_scrape_interval : int

(** Enable monitoring of individual processes through prometheus-process-exporter *)
val process_monitoring : bool

(** When [website] is [true] (default) a website is up for summarizing various
    information related to the experiment. *)
val website : bool

(** Enable to specify a machine type. The string must be a machine description
    compliant with GCP (ex: "n1-standard-2"). See the different type of machines
    supported here:
    https://cloud.google.com/compute/docs/general-purpose-machines#c3d_series.

    Default is: "n1-standard-2".
    *)
val machine_type : string

(** Enable to specify a disk type for GCP machines. The string must be a machine
    description compliant with GCP (ex: "pdd-ssd" or "hyperdisk-balanced").
*)
val disk_type : string option

(** Enable to specify the size of disks for GCP machines, in GB. *)
val disk_size_gb : int option

(** Specify the dockerfile alias to use. If not specified, the image
    name will be given by the value of the variable `TEZT_CLOUD`. *)
val dockerfile_alias : string option

(** [website_port] determines the port at which the website is set. Default is
  [8080]. *)
val website_port : int

(** Maximum running time of a VM. *)
val max_run_duration : int

(** Deactivate the max run duration parameter. *)
val no_max_run_duration : bool

(** This flag can overwrite the `TEZT_CLOUD` environment variable. *)
val tezt_cloud : string option

(** DNS entries to use for the proxy container. Used by gcloud command in user
    authorized zones*)
val dns_domains : string list

(** No DNS entry will be added for the proxy container. *)
val no_dns : bool

(** Activate the Open Telemetry collector. *)
val open_telemetry : bool

(** To use if run from Mac OS/X. *)
val macosx : bool

(** Check for file consistency. *)
val check_file_consistency : bool

(** Use docker host network in localhost. *)
val docker_host_network : bool

(** Specify if the docker container should be pushed.
    Only considered for remote mode. *)
val push_docker : bool

(** Auto approve the deployment plan. *)
val auto_approve : bool

(** Value used to set the [FAKETIME] environment variable on VMs. *)
val faketime : string option

(** Where to find binaries path by default in the docker image. *)
val binaries_path : string

(** How many log rotation until we remove older logs
    Defaults to 300
    Use 0 to disable log-rotation *)
val log_rotation : int

(* Daily log path retrieval if set. *)
val retrieve_daily_logs : bool

(* Ppx profiling traces retrieval if set. *)
val retrieve_ppx_profiling_traces : bool

(** Boundaries used for setting a random network latency on docker containers. *)
val tc_delay : (float * float) option

(** Boundaries used for setting a random network jitter on docker containers. *)
val tc_jitter : (float * float) option

(* Artifact path retrieval if set. *)
val artifacts_dir : string option

(* Retrieves teztale artifacts if the artifact-dir is set. *)
val teztale_artifacts : bool

(** The hostname of the host accessed by ssh on which to deploy *)
val ssh_host : string option

(** The ssh private key to use for the initial deployment *)
val ssh_private_key : string option

(** Slack channel id to send notifications on *)
val slack_channel_id : string option

(** Slack authentication token to allow publication *)
val slack_bot_token : string option

module Types : sig
  val dir_typ : name:string -> cli_parameter:string -> string option Clap.typ
end

(** [to_json_config ~scenario_name ?scenario_config ()] generates the full
    configuration file encoded in JSON, each option being retrieved from the CLI
    or default parameters. If [scenario_config] is not given, it encodes the
    one from the CLI. *)
val to_json_config :
  ?scenario_config:string * Data_encoding.Json.t -> unit -> Data_encoding.Json.t
