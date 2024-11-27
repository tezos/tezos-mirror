(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type t

type target = {address : string; port : int; app_name : string}

(** [start agents] starts the prometheus server. *)
val start : Agent.t List.t -> t Lwt.t

(** [shutdown prometheus] shutdowns the prometheus server. *)
val shutdown : t -> unit Lwt.t

(** [export_snapshot prometheus] exports a prometheus snapshot. *)
val export_snapshot : t -> unit Lwt.t

(** [run_wtih_snapshot ()] allows to run the prometheus with the given
    snapshot. The snapshot must have been exported by running a tezt-cloud
    scenario beforehand. *)
val run_with_snapshot : unit -> t Lwt.t

(** [reload prometheus] reloads the prometheus configuration. This can be used
  to take into account a change such a different agent name or a new source. *)
val reload : t -> unit Lwt.t

(** [add_job prometheus ?metrics_path ~name targets] adds a new job for
    fetching new metrics from given targets. Automatically calls [reload] so
    that the source is taken account just after calling this function.  *)
val add_job :
  t -> ?metrics_path:string -> name:string -> target list -> unit Lwt.t

(** [add_alert alert prometheus] adds a new [alert] in the Prometheus
    configuration. Similarly to [add_job], it implies a call to
    [reload] so that the alert is taken into account just after
    calling this function. *)
val add_alert : Alert_manager.alert -> t -> unit Lwt.t

(** [add_alerts alerts prometheus] adds new [alerts] in the Prometheus
    configuration. Similarly to [add_job], it implies a call to
    [reload] so that the alerts are taken into account just after
    calling this function. *)
val add_alerts : Alert_manager.alert list -> t -> unit Lwt.t
