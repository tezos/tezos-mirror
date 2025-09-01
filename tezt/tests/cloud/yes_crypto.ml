(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

let yes_crypto_env =
  String_map.singleton Tezos_crypto.Helpers.yes_crypto_environment_variable "y"

let should_enable_yes_crypto = function
  | Network_simulation.Scatter _ | Map _ -> true
  | Disabled -> false

let may_set_yes_crypto_env = function
  | Network_simulation.Scatter _ | Map _ -> (Some yes_crypto_env, true)
  | Disabled -> (None, false)
