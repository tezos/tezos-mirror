(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type t

(** [register ?proxy_files ?vms ~__FILE__ ~title ~tags ?seed f] is a
    wrapper around [Test.register]. It enables to run a test that can
    use machines deployed onto the cloud. *)
val register :
  ?proxy_files:string list ->
  ?proxy_args:string list ->
  ?vms:(unit -> Agent.Configuration.t list Lwt.t) ->
  ?dockerbuild_args:(string * string) list ->
  __FILE__:string ->
  title:string ->
  tags:string list ->
  ?seed:Test.seed ->
  ?alerts:Alert_manager.alert list ->
  ?tasks:Chronos.task list ->
  (t -> unit Lwt.t) ->
  unit

(* Set the [FAKETIME] environment variable so that all the ssh sessions have it
   defined. *)
val set_faketime : Agent.t -> string -> unit Lwt.t

(** Dynamically add an alert *)
val add_alert : t -> alert:Alert_manager.alert -> unit Lwt.t

val agents : t -> Agent.t list

val push_metric :
  t ->
  ?help:string ->
  ?typ:[`Counter | `Gauge] ->
  ?labels:(string * string) list ->
  name:string ->
  float ->
  unit

val write_website : t -> unit Lwt.t

type target = {agent : Agent.t; port : int; app_name : string}

val add_prometheus_source :
  t -> ?metrics_path:string -> name:string -> target list -> unit Lwt.t

val add_service : t -> name:string -> url:string -> unit Lwt.t

val open_telemetry_endpoint : t -> string option

val register_binary :
  t ->
  ?agents:Agent.t list ->
  ?group:string ->
  name:string ->
  unit ->
  unit Lwt.t

val service_register :
  name:string ->
  executable:string ->
  ?on_alive_callback:(alive:bool -> unit) ->
  on_shutdown:(unit -> unit Lwt.t) list ->
  Agent.t ->
  unit

val notify_service_start : name:string -> pid:int -> unit

val notify_service_stop : name:string -> unit

val register_chronos_task : t -> Chronos.task -> unit

val notifier : t -> Types.notifier
