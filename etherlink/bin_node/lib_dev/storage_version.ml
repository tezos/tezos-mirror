(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* This file reflects the storage version logic found at:
   * `etherlink/kernel_latest/kernel/src/migration.rs`
   It centralizes the EVM node's gates so it becomes easy to identify breaking
   changes. *)

let simulation_v0 ~storage_version = storage_version < 12

let simulation_v2 ~storage_version = storage_version > 12

let populate_delayed_inbox_disabled ~storage_version = storage_version < 15

let kernel_has_txs_in_storage ~storage_version = storage_version < 17

let gas_limit_validation_enabled ~storage_version = storage_version < 34

let is_prague_enabled ~storage_version = storage_version >= 37

let legacy_storage_compatible ~storage_version = storage_version < 41

let sub_block_latency_entrypoints_disabled ~storage_version =
  storage_version < 42
