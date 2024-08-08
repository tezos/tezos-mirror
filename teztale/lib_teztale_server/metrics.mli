(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Metrics : sig
  val durations : string -> Prometheus.Summary.t
end

val sql : string -> (unit -> 'a Lwt.t) -> 'a Lwt.t
