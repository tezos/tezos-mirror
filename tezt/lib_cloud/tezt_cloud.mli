(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Path = Path
module Agent = Agent
module Types = Types
module Ssh = Ssh

module Chronos : sig
  (** A scheduler task. *)
  type task

  (** [task ~name ~tm ~action ?randomized_delay ()] returns a new task
    named [name] performing [action] according to the time [tm].

    The [tm] time format follows the standard cron syntax with five
    space-separated fields: minute, hour, day of month, month, and day
    of week. Each field can be either a specific number within its
    valid range (minute: 0-59, hour: 0-23, day: 1-31, month: 1-12, day
    of week: 0-6 where 0 is Sunday) or an asterisk '*' to indicate
    "any value".

    Relies on UTC (Coordinated Universal Time), also known as GMT for
    time. Paris operates on Central European Time (CET), which is
    UTC+1 during standard time (winter months). During daylight saving
    time (summer months), Paris switches to Central European Summer
    Time (CEST), which is UTC+2.

    For example, "30 2 * * 1" means "2:30 AM every Monday (GMT)".

    Currently, the implementation only supports single values or
    asterisks - ranges, lists and step values are not yet supported.

    [?randomized_delay] delays the timer by a randomly selected amount
    of time between 0 and the specified value. Defaults to 0,
    indicating that no randomized delay shall be applied. Each timer
    unit will determine this delay randomly before each iteration.
    This setting is useful to stretch dispatching of similarly
    configured timer events over a certain time interval, to prevent
    them from firing all at the same time, possibly resulting in
    resource congestion. Similar to to what is proposed here:
    https://www.freedesktop.org/software/systemd/man/latest/systemd.timer.html#RandomizedDelaySec=
*)
  val task :
    name:string ->
    tm:string ->
    action:(unit -> unit Lwt.t) ->
    ?randomized_delay:int ->
    unit ->
    task
end

module Alert : sig
  (* A receiver of an alert. *)
  type receiver

  (* A slack receiver can be configured via a webhook. *)
  val slack_webhook_receiver :
    ?channel:string ->
    name:string ->
    api_url:string ->
    ?title:string ->
    ?text:string ->
    unit ->
    receiver

  (* A slack receiver can be configured via a bot token. *)
  val slack_bottoken_receiver :
    name:string ->
    channel:string ->
    bot_token:string ->
    ?title:string ->
    ?text:string ->
    unit ->
    receiver

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

  (* Set the [FAKETIME] environment variable so that all the ssh sessions have it
   defined. *)
  val set_faketime : Agent.t -> string -> unit Lwt.t

  (** A wrapper around [Test.register] that can be used to register new tests
      using VMs provided as a map indexed by name. Each VM is abstracted via
      the [Agent] module.

      [proxy_files] should contains [file] that are needed by the
      scenario to run (only used for proxy mode).

      [proxy_args] should contains CLI arguments necessary for the
      proxy mode. This can be used for example when an argument is
      provided via an environment variable instead of a command-line
      argument.

      [tasks] represent Chronos tasks that will be registered in a
      Chronos.t process. If [tasks] is empty, no Chronos process will
      be started. *)
  val register :
    ?proxy_files:string list ->
    ?proxy_args:string list ->
    ?vms:(unit -> Agent.Configuration.t list Lwt.t) ->
    ?dockerbuild_args:(string * string) list ->
    __FILE__:string ->
    title:string ->
    tags:string list ->
    ?seed:Test.seed ->
    ?alerts:Alert.t list ->
    ?tasks:Chronos.task list ->
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

  val add_alert : t -> alert:Alert.t -> unit Lwt.t

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

  (** [service_register: name executable on_alive_callback on_shutdowan agent]
      register a service, ie, a long running background process, that we want to
      monitor for launch and crash.
      [name] is a unique name to identify the service.
      [on_alive_callback] is a callback whose argument is a boolean which
      represent the service started if true, or the service was shutdown if
      false. This callback is called regularly, and expects to be update some
      metrics.
      [on_shutdown] is a list of callbacks that will be called as soon as the
      shutdown of a service will be triggered.
      TODO: change arguments executable and pid to a abstraction for tezt Daemon.t
            and merge register_binary functionality into register_service *)
  val service_register :
    name:string ->
    executable:string ->
    ?on_alive_callback:(alive:bool -> unit) ->
    on_shutdown:(unit -> unit Lwt.t) list ->
    Agent.t ->
    unit

  (** [notify_service_start: name] notify the start of a service *)
  val notify_service_start : name:string -> pid:int -> unit

  (** [notify_service_stop: name] notify the normal termination of a service *)
  val notify_service_stop : name:string -> unit

  (** Register a chronos task *)
  val register_chronos_task : t -> Chronos.task -> unit

  (** Returns the [Types.notifier] *)
  val notifier : t -> Types.notifier
end

module Prometheus : sig
  (** [get_query_endpoint ~query] returns the endpoint corresponding
      to the prometheus server, extended with the [query] provided.
      Returns [None] if the prometheus service is not running.
      Useful to perform GET requests. *)
  val get_query_endpoint : query:string -> Uri.t option
end

(** Expose the elements of [Cli] which are useful for the scenario. *)
module Tezt_cloud_cli : sig
  module Types : module type of Cli.Types

  val prometheus : bool

  val scenario_specific_json : (string * Data_encoding.Json.t) option

  (** Equivalent to [Cli.retrieve_daily_logs] *)
  val retrieve_daily_logs : bool

  (** Equivalent to [Cli.retrieve_ppx_profiling_traces] *)
  val retrieve_ppx_profiling_traces : bool

  val artifacts_dir : string option

  val binaries_path : string

  val machine_type : string

  val disk_type : string option

  val teztale_artifacts : bool

  val faketime : string option

  val to_json_config :
    ?scenario_config:string * Data_encoding.Json.t ->
    unit ->
    Data_encoding.Json.t

  (** Equivalent to [Cli.localhost] *)
  val localhost : bool

  (** Equivalent to [Cli.proxy] *)
  val proxy : bool

  (** Equivalent to [Cli.dns_domains] *)
  val dns_domains : string list
end

module Artifact_helpers : sig
  (** [local_path path] ensures that the path (as a list of subdirectories) is a
      valid directory, creates it otherwise, then returns the path. If the first
      directory starts with `/`, it will start from the root of the
      filesystem. *)
  val local_path : string list -> string

  (** [prepate_artifacts ?scenario_config ()] ensures the
      `Tezt_cloud_cli.artifacts_dir` is a valid dir (and creates it if needed),
      and push the full configuration in it. *)
  val prepare_artifacts :
    ?scenario_config:string * Data_encoding.json -> unit -> unit
end

(** [register ~tags] register a set of jobs that can be used for setting
   requirements related to cloud scenarios. Some tags can be given for all the
   registered jobs. *)
val register : tags:string list -> unit

module Gcloud : sig
  module DNS : sig
    (** [add_subdomain ~zone ~name ~value] adds a dns entry for the domain name
    [~name], associated to the value [~value]. The value being an ip *)
    val add_subdomain : zone:string -> name:string -> value:string -> unit Lwt.t
  end
end
