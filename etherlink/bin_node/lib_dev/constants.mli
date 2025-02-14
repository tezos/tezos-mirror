(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Etherlink networks supported by this EVM node. *)
val supported_networks : Configuration.supported_network list

(** Name of Etherlink network. *)
val network_name : Configuration.supported_network -> string

val rollup_address :
  Configuration.supported_network -> Tezos_crypto.Hashed.Smart_rollup_address.t

val latest_snapshot_url : Configuration.supported_network -> string
