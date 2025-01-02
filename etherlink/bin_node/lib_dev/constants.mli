(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

val rollup_address :
  Configuration.supported_network -> Tezos_crypto.Hashed.Smart_rollup_address.t

val chain_id : Configuration.supported_network -> Ethereum_types.quantity

val latest_snapshot_url : Configuration.supported_network -> string
