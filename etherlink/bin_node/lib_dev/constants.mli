(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025-2026 Functori <contact@functori.com>                   *)
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
  | Mainnet_beta
  | Mainnet_gamma
  | Bifrost
  | Calypso
  | Calypso2
  | Dionysus
  | DionysusR1
  | Ebisu
  | Farfadet
  | FarfadetR1
  | FarfadetR2
  | Latest

val kernel_from_string : string -> kernel option

(** [kernel_is_older a ~than:b] returns [true] if kernel [a] is strictly
    older than kernel [b]. *)
val kernel_is_older : kernel -> than:kernel -> bool

(** [kernel_is_newer a ~than:b] returns [true] if kernel [a] is strictly
    newer than kernel [b]. *)
val kernel_is_newer : kernel -> than:kernel -> bool

(** Returns the root hash of a released kernel, or [None] for [Latest]
    whose root hash changes over time. *)
val root_hash_from_released_kernel : kernel -> Hex.t option
