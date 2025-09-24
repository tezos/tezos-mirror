(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

type error +=
  | Invalid_snapshot_file of string
  | Invalid_snapshot_provider of string
  | Data_dir_populated of string
  | History_mode_mismatch of
      Configuration.history_mode * Configuration.history_mode
  | Incorrect_rollup of Address.t * Address.t
  | Outdated_snapshot of Z.t * Z.t

(** [interpolate_snapshot_file block_number rollup_address history_mode path]
    replaces placeholders in the given snapshot [path] with the provided values.
    The supported placeholders are:
    - [%l] for the block number
    - [%r] for the rollup address (short form)
    - [%R] for the rollup address (long form)
    - [%h] for the history mode

    @param block_number The block number to use for interpolation.
    @param rollup_address The rollup address to use for interpolation.
    @param history_mode The history mode to use for interpolation.
    @param path The path to the snapshot file, possibly containing placeholders.
*)
val interpolate_snapshot_file :
  Ethereum_types.quantity ->
  Address.t ->
  Configuration.history_mode ->
  string ->
  string tzresult

(** [interpolate_snapshot_provider ?rollup_address ?network history_mode
    provider] replaces placeholders in the given snapshot [provider] with the
    provided values.
    The supported placeholders are:
    - [%n] for the network name
    - [%r] for the rollup address (short form)
    - [%R] for the rollup address (long form)
    - [%h] for the history mode

    @param rollup_address The rollup address to use for interpolation.
    @param network The network to use for interpolation.
    @param history_mode The history mode to use for interpolation.
    @param provider The snapshot provider string, possibly containing
      placeholders.
*)
val interpolate_snapshot_provider :
  ?rollup_address:Address.t ->
  ?network:Configuration.supported_network ->
  Configuration.history_mode ->
  string ->
  string tzresult
