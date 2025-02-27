(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type t

(** Get the docker container name *)
val get_name : t -> string

(** Get the port used on localhost used for port [9090] of the docker machine *)
val get_port : t -> int

(** [get_query_endpoint ~query] returns the endpoint corresponding
    to the prometheus server, extended with the [query] provided.
    Returns [None] if the prometheus service is not running.
    Useful to perform GET requests.

    It assumes the prometheus instance is running on [localhost] and
    uses [Env.port]. As a consequence, the returned [Uri.t] is valid
    for the tezt-cloud orchestrator but may be not suited for other
    services.
*)
val get_query_endpoint : query:string -> Uri.t option

type target = {address : string; port : int; app_name : string}

type alert

type severity = Critical | Warning | Info

(** [make_alert name ?for_ ?description ?summary ?severity ~name ~expr ()]
    creates an alert with:
   - [name]: alert identifier.
   - [expr]: promQL expression that triggers the alert.
   - [severity]: alert level.
   - [for_]: optional duration before firing.
   - [description]: optional detailed explanation.
   - [summary]: optional brief overview. *)
val make_alert :
  ?for_:string ->
  ?description:string ->
  ?summary:string ->
  ?severity:severity ->
  ?group_name:string ->
  ?interval:string ->
  name:string ->
  expr:string ->
  unit ->
  alert

(** [start agents] starts the prometheus server. *)
val start : alerts:alert list -> Agent.t List.t -> t Lwt.t

(** [shutdown prometheus] shutdowns the prometheus server. *)
val shutdown : t -> unit Lwt.t

(** [export_snapshot prometheus] exports a prometheus snapshot. *)
val export_snapshot : t -> unit Lwt.t

(** [run_with_snapshot port snapshot_path] allows to run the prometheus with the given
    snapshot. The snapshot must have been exported by running a tezt-cloud
    scenario beforehand. *)
val run_with_snapshot : int -> string -> t Lwt.t

(** [reload prometheus] reloads the prometheus configuration. This can be used
  to take into account a change such a different agent name or a new source. *)
val reload : t -> unit Lwt.t

(** [add_job prometheus ?metrics_path ~name targets] adds a new job for
    fetching new metrics from given targets. Automatically calls [reload] so
    that the source is taken account just after calling this function.  *)
val add_job :
  t -> ?metrics_path:string -> name:string -> target list -> unit Lwt.t

type record

type rule

val make_record : name:string -> query:string -> record

val rule_of_alert : alert -> rule

val rule_of_record : record -> rule

val name_of_alert : alert -> string

val register_rules :
  ?group_name:string -> ?interval:string -> rule list -> t -> unit Lwt.t
