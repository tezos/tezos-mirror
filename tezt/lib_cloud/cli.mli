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

(** When [monitoring] is [true], VMs are monitored with netdata.  *)
val monitoring : bool

(** [destroy] is [true] for destroying temporary resources run by a scenario.
    Otherwise, this has to be done manually. *)
val destroy : bool

(** [project_id] enables to specify a project id that should be used. This
    option should be set when the scenario should be run for a different
    project id than the default one. *)
val project_id : string option

(** When [keep_alive] is [true], the user must press <enter> to terminate the
  scenario. This can be used for debugging purpose or for inspecting the state
  of VMs at the end of a scenario. *)
val keep_alive : bool

(** [vms] enables to set the number of vms that will be spawned. *)
val vms : int option

(** First port opened on the VM. *)
val vm_base_port : int

(** Number of consecutive ports opened on the VM from the base port. *)
val ports_per_vm : int

(** When [website] is [true] (default) a website is up for summarizing various
    information related to the experiment. *)
val website : bool

(** [website_port] determines the port at which the website is set. Default is
  [8080]. *)
val website_port : int

(** When [prometheus] is [true] a Prometheus instance is run locally and
    metrics of the experiments are exported to prometheus. At the end of the
    test, the database is snapshotted so that it can be imported later on. *)
val prometheus : bool

(** If [true], exports a Promtheus snapshot at the end of the scenario. *)
val prometheus_export : bool

(** Specify the name of the prometheus snapshot. *)
val prometheus_snapshot_filename : string option

(** Specify the port of the Prometheus instance on the host machine. *)
val prometheus_port : int

(** Specify the scraping interval of Prometheus. *)
val prometheus_scrape_interval : int

val grafana : bool

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

(** Maximum running time of a VM. *)
val max_run_duration : int

(** Deactivate the max run duration parameter. *)
val no_max_run_duration : bool

(** When [proxy] is [true], the scenario is run via a VM instead of the host
  machine. *)
val proxy : bool

val tezt_cloud : string option
