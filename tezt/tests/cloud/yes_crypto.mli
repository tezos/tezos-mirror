(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** This module provides utilities for enabling the "yes-crypto" testing mode,
    which bypasses certain cryptographic confirmations in Octez nodes.
*)

open Scenarios_cli

(** [should_enable_yes_crypto config] returns a flag
    indicating whether "yes-crypto" mode should be enabled (for [Scatter] and [Map])
    or disabled (for [Disabled]). *)
val should_enable_yes_crypto : network_simulation_config -> bool

(** [may_set_yes_crypto_env config] is similar to [should_enable_yes_crypto],
    but also returns the environment variable that needs to be set to enable
    the yes-crypto mechanism. *)
val may_set_yes_crypto_env :
  network_simulation_config -> string String_map.t option * bool
