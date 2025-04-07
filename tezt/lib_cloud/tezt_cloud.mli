(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Agent = Agent
module Types = Types

module Alert : sig
  (* A receiver of an alert. *)
  type receiver

  (* A slack receiver can be configured via a webhook. *)
  val slack_receiver :
    ?channel:string -> name:string -> api_url:string -> unit -> receiver

  (* This is a dummy receiver. *)
  val null_receiver : receiver

  (* A route explains when an alert should be issued to the receiver. *)
  type route

  (** [route ?group_wait ?group_interval ?repeat_interval receiver]
      creates a fresh route whose receiver is [receiver] and where:

    - [group_wait] defines how long to wait before sending a
    notification about new alerts. If omitted, inherit the
    [group_wait] of the default route.

    - [group_interval] defines how long to wait before sending
    notification about new alerts for a group. If omitted, inherit the
    [group_interval] of the default route.

    - [repeat_interval] defines the minimum time interval between
    sending two notifications about the same alert. If omitted,
    inherit the [repeat_interval] of the default route.  The
    [repeat_interval] value should be a multiple of
    [group_interval]. *)
  val route :
    ?group_wait:string ->
    ?group_interval:string ->
    ?repeat_interval:string ->
    receiver ->
    route

  (** Severity of an alert. *)
  type severity = Critical | Warning | Info

  (** Type of an alert. *)
  type t

  (** [make ?route ?for_ ?description ?summary ?severity ~name ~expr]
      defines a new Prometheus alert with name [name] and promQL
      [expr]. Optionally a severity, summary and description of the
      alert can be defined.

      If [route] is provided, the alert can be routed to a receiver
      (Slack, webhook, ...).
*)
  val make :
    ?route:route ->
    ?for_:string ->
    ?description:string ->
    ?summary:string ->
    ?severity:severity ->
    ?group_name:string ->
    ?interval:string ->
    name:string ->
    expr:string ->
    unit ->
    t
end

module Cloud : sig
  type t

  (** A wrapper around [Test.register] that can be used to register new tests
      using VMs provided as a map indexed by name. Each VM is abstracted via
      the [Agent] module.

      [proxy_files] should contains [file] that are needed by the
      scenario to run (only used for proxy mode).

      [proxy_args] should contains CLI arguments necessary for the
      proxy mode. This can be used for example when an argument is
      provided via an environment variable instead of a command-line
      argument.
 *)
  val register :
    ?proxy_files:string list ->
    ?proxy_args:string list ->
    ?vms:Agent.Configuration.t list ->
    __FILE__:string ->
    title:string ->
    tags:string list ->
    ?seed:Test.seed ->
    ?alerts:Alert.t list ->
    (t -> unit Lwt.t) ->
    unit

  (** [push_metric t ?help ?typ ?labels ~name v] pushes the value [v]
      for [metric] on Prometheus. [labels] can be used to categorise
      the metric (each set of label define a single curve). [typ] can
      be used to provide the type of the metric. [help] can be used to
      provide some naive documentation about the metrics. *)
  val push_metric :
    t ->
    ?help:string ->
    ?typ:[`Counter | `Gauge] ->
    ?labels:(string * string) list ->
    name:string ->
    float ->
    unit

  (** [agents t] returns the list of agents deployed. *)
  val agents : t -> Agent.t list

  type target = {agent : Agent.t; port : int; app_name : string}

  (** [add_prometheus_source t ?metrics_path ~name targets] allows to add a new
      source of metrics that Prometheus can scrap. By default [metric_path] is
      [/metrics]. [job_name] is just the name to give for the job that will
      scrap the metrics. It must be unique. A target enables to define a list of
      points to scrap. Each point can have a name defined by [app_name]. *)
  val add_prometheus_source :
    t -> ?metrics_path:string -> name:string -> target list -> unit Lwt.t

  val add_service : t -> name:string -> url:string -> unit Lwt.t

  val open_telemetry_endpoint : t -> string option

  (** [register_binary t ?agents ?group name] register a binary for individual
    process monitoring via prometheus process exporter.
    [group] will allow to put process in process groups, is currently not used.
    defaults to "tezt-cloud" if not specified
    [name] is the filename of the executable to monitor.
    [agents] when specified, is the list of agents on which to enable monitoring
    when not specified, all container agents will run a prometheus process
    exporter *)
  val register_binary :
    t ->
    ?agents:Agent.t list ->
    ?group:string ->
    name:string ->
    unit ->
    unit Lwt.t
end

(** [register ~tags] register a set of jobs that can be used for setting
   requirements related to cloud scenarios. Some tags can be given for all the
   registered jobs. *)
val register : tags:string list -> unit
