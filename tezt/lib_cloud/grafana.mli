(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

include module type of Tezt_tezos_tezt_performance_regression.Grafana

type t

val run : ?sources:string list -> unit -> t Lwt.t

val shutdown : t -> unit Lwt.t

val dashboards_filepaths : unit -> string list Lwt.t
