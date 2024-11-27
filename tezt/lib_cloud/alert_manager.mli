(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Prometheus alert. *)
type alert

(** [alert name ~expr ~severity ?for_ ?description ?summary] creates
    an alert with:
   - [name]: alert identifier.
   - [expr]: promQL expression that triggers the alert.
   - [severity]: alert level (either [`Critical], [`Warning], [`Info]
     or [`None]).
   - [for_]: optional duration before firing.
   - [description]: pptional detailed explanation.
   - [summary]: optional brief overview. *)
val alert :
  expr:string ->
  severity:[`Critical | `Info | `None | `Warning] ->
  ?for_:string ->
  ?description:string ->
  ?summary:string ->
  string ->
  alert

(** [name_of_alert alert] returns the name associated with the
    [alert]. *)
val name_of_alert : alert -> string

(** Jingoo alert template. *)
val alert_template : alert -> Jingoo.Jg_types.tvalue

(** Configuration type for Alert manager. *)
type config

(** [add_global k v config] binds key [k] to value [v] in the
    configuration globals of [config]. Globals is a map of key-value
    pairs that defines global settings like webhook URLs and
    authentication tokens that apply across all alerts and
    receivers. *)
val add_global : k:string -> v:string -> config -> config

(** [add_globals kvs config] is [add_global] for a list of key-value
    pairs.*)
val add_globals : (string * string) list -> config -> config

(** A [receiver] defines how to send notifications (like to Slack,
    email, etc.) when an alert is triggered, including the destination
    and message format. *)
type receiver

(** [add_receiver receiver config] extends the [config] with the
    provided [receiver]. *)
val add_receiver : receiver -> config -> config

(** [slack_receiver ~name ~channel ~text config] creates a [receiver]
    named [name], sending notifications on the slack [channel],
    with the given [title] and [text] metadata. *)
val slack_receiver :
  name:string -> channel:string -> title:string -> text:string -> receiver

(** A group defines when and how alerts are processed, including
    timing parameters (group_wait, group_interval, repeat_interval),
    which alerts to match (alert_names), and where to send them
    (receiver_name). *)
type group

(** [group name receiver ?group_wait ?group_interval
    ?repeat_interval] creates a fresh group named [named]
    whose receiver is [receiver] and where:
    - [group_wait] defines how long to wait before sending a
    notification about new alerts. If omitted, child routes inherit
    the [group_wait] of the parent route.
    - [group_interval] defines how long to wait before sending
    notification about new alerts for a group. If omitted, child
    routes inherit the [group_interval] of the parent route.
    - [repeat_interval] defines the minimum time interval between
    sending two notifications about the same alert. If omitted, child
    routes inherit the [repeat_interval] of the parent route. The
    [repeat_interval] value should be a multiple of [group_interval].
 *)
val group :
  ?group_wait:string ->
  ?group_interval:string ->
  ?repeat_interval:string ->
  string ->
  receiver ->
  group

(** [add_group group config] extends the configuration [config]
    with the provided [group]. *)
val add_group : group -> config -> config

(** [with_group group f config] updates the [group] in [config]
    by applying [f]. The group must be part of the config.*)
val with_group : group -> (group -> group) -> config -> config

(** [add_group_alert alert group] updates the given [group] to match
    against [alert].*)
val add_group_alert : alert -> group -> group

(** [add_group_alerts alerts group] is [add_group_alert] for a list of
    alerts.*)
val add_group_alerts : alert list -> group -> group

(** Alert handler.  *)
module Handler : sig
  (** Alert handler type where:
      - [name] is the handler name.
      - [setup] defines how the alter manager configuration should be updated when this
        handler is activated.  *)
  type t = {name : string; setup : config -> config}

  (** [make_handler ~name ~setup] returns an handler whose name is
      [name] and setup function is [setup]. *)
  val make : name:string -> setup:(config -> config) -> t
end

(** A collection is a registery of alerts and handlers. *)
module Collection : sig
  (** The collection type. *)
  type t

  (** [init ()] returns a fresh, empty collection.  *)
  val init : unit -> t

  (** [register_alert alert t] registers the given [alert] in the
      collection [t]. *)
  val register_alert : alert -> t -> unit

  (** [register_handler handler t] registers the given [handler] in the
      collection [t]. *)
  val register_handler : Handler.t -> t -> unit

  (** [alerts t] returns the alerts registered in collection [t]. *)
  val alerts : t -> alert list

  (** [handlers t] returns the handlers registered in collection [t]. *)
  val handlers : t -> Handler.t list
end

(** The alert manager service. *)
type t

(** [run collection alert_handlers] starts a new docker container in
    the background, using the latest Prometheus Alert Manager image.
    Activates the given [alert_handlers]. Note that each alert handler
    name must be registered in [collection]. *)
val run : Collection.t -> string list -> t Lwt.t

(** [shutdown t] shutdowns the alert manager service. *)
val shutdown : unit -> unit Lwt.t
