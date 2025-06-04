(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Defines the receiver of an alert (like Slack, email, etc.) when an
    alert is triggered, including the destination and message
    format. *)
type receiver

(** [slack_webhook_receiver ?channel ~name ~api_url] creates a [receiver] named
    [name], sending notifications on the URL defined by [api_url]. [channel] is
    a placeholder. *)
val slack_webhook_receiver :
  ?channel:string -> name:string -> api_url:string -> unit -> receiver

(** [slack_bottoken_receiver ~name ~channel ~bot_token] creates a
    [receiver] named [name], sending notifications on channel [channel] using
    [bot_token] authentication: https://api.slack.com/concepts/token-types *)
val slack_bottoken_receiver :
  name:string -> channel:string -> bot_token:string -> receiver

val null_receiver : receiver

(** A route defines when and how alerts are processed, including
    timing parameters (group_wait, group_interval, repeat_interval),
    which alerts to match (alert_names), and where to send them
    (receiver_name). *)
type route

(** [route name receiver ?group_wait ?group_interval
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
val route :
  ?group_wait:string ->
  ?group_interval:string ->
  ?repeat_interval:string ->
  receiver ->
  route

type alert = private {alert : Prometheus.alert; route : route option}

val alert : ?route:route -> Prometheus.alert -> alert

(** The alert manager service. *)
type t

(** Add an alert (and reload the service) *)
val add_alert : t -> alert:alert -> unit Lwt.t

(** [run alerts] starts a new docker container in
    the background, using the latest Prometheus Alert Manager image.
    Activates the given [alerts]. *)
val run : ?default_receiver:receiver -> alert list -> t option Lwt.t

(** [shutdown t] shutdowns the alert manager service. *)
val shutdown : unit -> unit Lwt.t
