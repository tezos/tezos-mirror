(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** Etherlink networks supported by this EVM node. *)
val supported_networks : Configuration.supported_network list

(** Name of Etherlink network. *)
val network_name : Configuration.supported_network -> string

val rollup_address :
  Configuration.supported_network -> Tezos_crypto.Hashed.Smart_rollup_address.t

val network_of_address :
  Tezos_crypto.Hashed.Smart_rollup_address.t ->
  Configuration.supported_network option

type kernel =
  | Bifrost
  | Calypso
  | Calypso2
  | Dionysus
  | DionysusR1
  | Ebisu
  | Farfadet

val kernel_from_string : string -> kernel option

val root_hash_from_kernel : kernel -> Hex.t
