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

(** [storage_version_of_kernel kernel] is the storage version that [kernel]
    bakes. For [Latest], it is [Int.max_int]: above every version, so it
    passes every gate. *)
let storage_version_of_kernel : Constants.kernel -> int = function
  (* Each value is the [STORAGE_VERSION] of the kernel source that its
     comment names: a tree vendored under [etherlink/], or a commit and a
     path in it. *)
  | Mainnet_beta -> 11 (* b9f6c91:etherlink/kernel_evm/kernel/src/storage.rs *)
  | Mainnet_gamma -> 12 (* 4f4457e:etherlink/kernel_evm/kernel/src/storage.rs *)
  | Bifrost -> 22 (* etherlink/kernel_bifrost/kernel/src/storage.rs *)
  | Calypso -> 26 (* etherlink/kernel_calypso/kernel/src/storage.rs *)
  | Calypso2 -> 26 (* etherlink/kernel_calypso2/kernel/src/storage.rs *)
  | Dionysus -> 33 (* etherlink/kernel_dionysus/kernel/src/storage.rs *)
  | DionysusR1 -> 36 (* etherlink/kernel_dionysus_r1/kernel/src/storage.rs *)
  | Ebisu -> 38 (* etherlink/kernel_ebisu/kernel/src/storage.rs *)
  | Farfadet -> 44 (* etherlink/kernel_farfadet/kernel/src/storage.rs *)
  | FarfadetR1 -> 45 (* etherlink/kernel_farfadet_r1/kernel/src/storage.rs *)
  | FarfadetR2 -> 45 (* etherlink/kernel_farfadet_r2_su/kernel/src/storage.rs *)
  | FarfadetR3 -> 46 (* etherlink/kernel_farfadet_r3_su/kernel/src/storage.rs *)
  | FarfadetR4 -> 47 (* etherlink/kernel_farfadet_r4_su/kernel/src/storage.rs *)
  | FarfadetR5 -> 47 (* etherlink/kernel_farfadet_r5_su/kernel/src/storage.rs *)
  | FarfadetR6 -> 47 (* etherlink/kernel_farfadet_r6_su/kernel/src/storage.rs *)
  | Previewnet02 ->
      54 (* 017753c8:etherlink/kernel_latest/kernel/src/storage.rs *)
  | Previewnet04 ->
      56 (* 7e580654:etherlink/kernel_latest/kernel/src/storage.rs *)
  | Previewnet05 ->
      57 (* 3038e37e:etherlink/kernel_latest/kernel/src/storage.rs *)
  | Previewnet06 ->
      60 (* ae3d7318:etherlink/kernel_latest/kernel/src/storage.rs *)
  | Ganesha -> 65 (* 6d47b6a1:etherlink/kernel_latest/kernel/src/storage.rs *)
  | GaneshaR1 -> 65 (* da2977ec:etherlink/kernel_latest/kernel/src/storage.rs *)
  | GaneshaR2 -> 65 (* 54a7b092:etherlink/kernel_latest/kernel/src/storage.rs *)
  | Latest -> Int.max_int

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

let michelson_runtime_paths_moved_to_world_state_version = 57

let michelson_runtime_paths_moved_to_world_state ~storage_version =
  storage_version >= michelson_runtime_paths_moved_to_world_state_version

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

(* Version gates: the node serves part of the Michelson runtime through
   kernel entrypoints, and a kernel that predates one has no such WASM
   export — the call would leave the IPC result path empty and surface as
   an opaque failure. [tezosx_michelson_entrypoints] exists from V51 on
   (added under V50, so V51 is the first version every kernel carrying it
   holds), [tezosx_run_code] from V64 on. *)
let tezosx_michelson_entrypoints ~storage_version = storage_version >= 51

let tezosx_run_code ~storage_version = storage_version >= 64

let michelson_runtime_target_sunrise_level_moved_to_base_version = 66

let michelson_runtime_target_sunrise_level_moved_to_base ~storage_version =
  storage_version
  >= michelson_runtime_target_sunrise_level_moved_to_base_version
