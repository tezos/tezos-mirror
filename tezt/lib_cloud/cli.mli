(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

include module type of Tezt.Cli

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

(** The hostname of the host accessed by ssh on which to deploy *)
val ssh_host : string option
