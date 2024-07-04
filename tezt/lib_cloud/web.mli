(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type t

(** [start ~agents env] starts a webpage with experimentations information
  if [Cli.website] is [true]. *)
val start : agents:Agent.t List.t -> t Lwt.t

(** [shutdown website] shutdowns the website. *)
val shutdown : t -> unit Lwt.t

(** [push_metric ?labels ~name value] push a metric into the page
  [/metrics.txt]. This page can be scrapped by Prometheus if activated. *)
val push_metric :
  t -> ?labels:(string * string) list -> name:string -> float -> unit

val write : t -> agents:Agent.t List.t -> unit Lwt.t
