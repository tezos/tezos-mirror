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

let tezosx_tezos_blocks ~storage_version = storage_version >= 50

let sequencer_key_storage_migrated_to_world_state ~storage_version =
  storage_version >= 51

let ipc_paths_moved_to_base ~storage_version = storage_version >= 52

let tezosx_single_tx ~storage_version = storage_version >= 53

let governance_config_moved_to_base ~storage_version = storage_version >= 54

let michelson_runtime_paths_moved_to_world_state ~storage_version =
  storage_version >= 57

let evm_config_moved_to_world_state ~storage_version = storage_version >= 58

let evm_accounts_isolated ~storage_version = storage_version >= 59

let simulation_trace_ipc_moved_to_base ~storage_version = storage_version >= 60

(* Version gate: Michelson blocks moved from [/tez/world_state/tez_blocks] to
   the world-state root [/tez/world_state] at storage version 62; the V62
   kernel migration moves the existing block subtrees to the new root. A
   pre-V62 kernel still writes the legacy root, so the node reads blocks there
   while such a kernel is running (e.g. the Previewnet V60 kernel, before the
   upgrade migrates the blocks). *)
let michelson_blocks_at_world_state_root ~storage_version =
  storage_version >= 62
