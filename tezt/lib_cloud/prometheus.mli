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

(** [add_source prometheuse ?metric_path ~job_name targets] add a new job for
    fetching new metrics from given targets. Automatically calls [reload] so
    that the source is taken account just after calling this function.  *)
val add_source :
  t -> ?metric_path:string -> job_name:string -> target list -> unit Lwt.t
