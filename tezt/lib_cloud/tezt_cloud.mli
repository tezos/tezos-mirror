(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Cloud : sig
  type t

  type vm_configuration = {machine_type : string}

  val default_vm_configuration : vm_configuration

  (** A wrapper around [Test.register] that can be used to register new tests
      using VMs provided as a map indexed by name. Each VM is abstracted via
      the [Agent] module. *)
  val register :
    ?docker_push:bool ->
    ?vms:vm_configuration list ->
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

  val get_configuration : t -> Agent.t -> vm_configuration
end

(** [register ~tags] register a set of jobs that can be used for setting
   requirements related to cloud scenarios. Some tags can be given for all the
   registered jobs. *)
val register : tags:string list -> unit
