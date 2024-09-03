(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Agent : sig
  type t

  val configuration : Agent.t -> Configuration.t
end

module Configuration : sig
  type docker_image = Gcp of {alias : string} | Octez_latest_release

  type t = private {
    machine_type : string;
    docker_image : docker_image;
    max_run_duration : int option;
    binaries_path : string;
    os : string;
  }

  (** [make ?machine_type ()] is a smart-constructor to make a VM
      configuration. 

    Default value for [max_run_duration] is [7200].  
        
    Default value for [machine_type] is [n1-standard-2]. 

    Default value for [docker_image] is [Custom {tezt_cloud}] where [tezt_cloud]
    is the value provided by the environement variable [$TEZT_CLOUD].
    *)
  val make :
    ?os:string ->
    ?binaries_path:string ->
    ?max_run_duration:int ->
    ?machine_type:string ->
    ?docker_image:docker_image ->
    unit ->
    t
end

module Cloud : sig
  type t

  (** A wrapper around [Test.register] that can be used to register new tests
      using VMs provided as a map indexed by name. Each VM is abstracted via
      the [Agent] module. *)
  val register :
    ?proxy_files:string list ->
    ?vms:Configuration.t list ->
    __FILE__:string ->
    title:string ->
    tags:string list ->
    ?seed:Test.seed ->
    (t -> unit Lwt.t) ->
    unit

  (** [push_metric t ?labels ~name v] pushes the value [v] for [metric] on
        Prometheus. [labels] can be used to categorised the metric (each set of
        label define a single curve). *)
  val push_metric :
    t -> ?labels:(string * string) list -> name:string -> float -> unit

  (** [agents t] returns the list of agents deployed. *)
  val agents : t -> Agent.t list

  (** [set_agent_name t agent name] sets the name of the agent [agent] to
      [name]. *)
  val set_agent_name : t -> Agent.t -> string -> unit Lwt.t

  type target = {agent : Agent.t; port : int; app_name : string}

  (** [add_prometheus_source ?metric_path ~job_name targets] allows to add a new
      source of metrics that Prometheus can scrap. By default [metric_path] is
      [/metrics]. [job_name] is just the name to give for the job that will
      scrap the metrics. It must be unique. A target enables to define a list of
      points to scrap. Each point can have a name defined by [app_name]. *)
  val add_prometheus_source :
    t -> ?metric_path:string -> job_name:string -> target list -> unit Lwt.t

  val add_service : t -> string * int -> unit Lwt.t
end

(** [register ~tags] register a set of jobs that can be used for setting
   requirements related to cloud scenarios. Some tags can be given for all the
   registered jobs. *)
val register : tags:string list -> unit
