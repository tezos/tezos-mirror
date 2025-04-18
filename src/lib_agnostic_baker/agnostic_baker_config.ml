(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* Agnostic Baker Daemon configuration *)

include Tezos_client_base_unix.Daemon_config

(* All logging must be centralised in a single place. *)
let default_daily_logs_path = Parameters.default_daily_logs_path

let advertise_log_levels = Some true
