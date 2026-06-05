(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2023-2025 Functori <contact@functori.com>                   *)
(* Copyright (c) 2023 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

type path = string

let reboot_counter = "/readonly/kernel/env/reboot_counter"

let world_state = "/world_state"

module BASE = struct
  let root = "/base"

  let make s = root ^ s
end

module EVM = struct
  let root = "/evm"

  let make s = root ^ s

  module World_state = struct
    let make s = root ^ world_state ^ s
  end

  module Eth_accounts = struct
    let accounts_path = "/eth_accounts"

    let make s = root ^ accounts_path ^ s
  end
end

module TEZ = struct
  let root = "/tez"

  module World_state = struct
    let make s = root ^ world_state ^ s
  end

  module Tez_accounts = struct
    let accounts_path = "/tez_accounts"

    let make s = root ^ accounts_path ^ s
  end
end

let evm_node_flag_legacy = "/__evm_node"

let evm_node_flag_base = BASE.make evm_node_flag_legacy

let evm_node_flag ~storage_version =
  if Storage_version.ipc_paths_moved_to_base ~storage_version then
    evm_node_flag_base
  else evm_node_flag_legacy

module Single_tx = struct
  let input_tx_base = BASE.make "/instant_confirmation/input_tx"

  let input_tx_legacy = EVM.World_state.make "/single_tx/input_tx"

  let input_tx ~storage_version =
    if Storage_version.ipc_paths_moved_to_base ~storage_version then
      input_tx_base
    else input_tx_legacy
end

module Tezosx_simulation = struct
  let path = BASE.make "/__simulation"

  let input = path ^ "/input"

  let result = path ^ "/result"
end

module Tezosx_entrypoints = struct
  let path = BASE.make "/tezosx_entrypoints"

  let input = path ^ "/input"

  let result = path ^ "/result"
end

let delayed_input ~storage_version =
  if Storage_version.ipc_paths_moved_to_base ~storage_version then
    BASE.make "/__delayed_input"
  else "/__delayed_input"

module Assemble_block = struct
  let input_base = BASE.make "/instant_confirmation/assemble_block/input"

  let input_legacy = EVM.World_state.make "/assemble_block/input"

  let input ~storage_version =
    if Storage_version.ipc_paths_moved_to_base ~storage_version then input_base
    else input_legacy
end

let etherlink_root = EVM.World_state.make ""

let etherlink_safe_root = "/tmp" ^ etherlink_root

let michelson_contracts_index = TEZ.Tez_accounts.make "/contracts/index"

(** [/tez/tez_accounts/big_map] — root of the Michelson big_map subtree
    that the kernel ([etherlink/kernel_latest/tezos_execution/src/context.rs])
    writes under the Tezlink context root. *)
let tezos_big_map_root = TEZ.Tez_accounts.make "/big_map"

(** [/kernel/boot.wasm] — path of the kernel's WASM blob, the entry
    point [Pvm.Kernel] runs. Writable by the EVM node only for
    debugger / replay overrides. *)
let kernel_boot_wasm = "/kernel/boot.wasm"

let contract_hex contract =
  let raw_key =
    Data_encoding.Binary.to_bytes_exn Tezos_types.Contract.encoding contract
  in
  let (`Hex s) = Hex.of_bytes raw_key in
  s

let michelson_contract_dir contract =
  michelson_contracts_index ^ "/" ^ contract_hex contract

let michelson_contract_storage contract =
  michelson_contract_dir contract ^ "/data/storage"

let michelson_contract_code contract =
  michelson_contract_dir contract ^ "/data/code"

let michelson_contract_balance contract =
  michelson_contract_dir contract ^ "/balance"

let tezos_big_map_dir id =
  tezos_big_map_root ^ "/"
  ^ Z.to_string (Tezlink_imports.Imported_context.Big_map.Id.unparse_to_z id)

let tezos_big_map_value id key_hash =
  let raw_hash =
    Tezlink_imports.Imported_protocol.Script_expr_hash.to_bytes key_hash
  in
  let (`Hex key_hex) = Hex.of_bytes raw_hash in
  tezos_big_map_dir id ^ "/" ^ key_hex

let tezos_big_map_key_type id = tezos_big_map_dir id ^ "/key_type"

let tezos_big_map_value_type id = tezos_big_map_dir id ^ "/value_type"

let tezos_big_map_total_bytes id = tezos_big_map_dir id ^ "/total_bytes"

let michelson_ledger_root = TEZ.Tez_accounts.make "/tezosx"

(** TezosX: Tezos blocks live in the Michelson world-state keyspace. *)
let tezosx_tezos_blocks_root = TEZ.World_state.make "/tez_blocks"

let block_root_of_chain_family (type f) (chain_family : f L2_types.chain_family)
    =
  match chain_family with
  | L2_types.EVM -> etherlink_root
  (* TezosX and standalone Tezlink both store Michelson block data at
     /tez/world_state/tez_blocks (= tezosx_tezos_blocks_root). This is
     the root the kernel passes to [block_storage::store_current] for
     Michelson blocks, see [MichelsonChainConfig::finalize_and_store]
     and [BlockInProgress::finalize_and_store]. *)
  | L2_types.Michelson -> tezosx_tezos_blocks_root

let chain_id ~storage_version =
  if Storage_version.evm_config_moved_to_world_state ~storage_version then
    EVM.World_state.make "/chain_id"
  else EVM.make "/chain_id"

let michelson_runtime_chain_id = TEZ.World_state.make "/chain_id"

let minimum_base_fee_per_gas =
  EVM.World_state.make "/fees/minimum_base_fee_per_gas"

let backlog = EVM.World_state.make "/fees/backlog"

let da_fee_per_byte = EVM.World_state.make "/fees/da_fee_per_byte"

let michelson_to_evm_gas_multiplier =
  EVM.World_state.make "/fees/michelson_to_evm_gas_multiplier"

let kernel_version ~storage_version =
  if Storage_version.governance_config_moved_to_base ~storage_version then
    BASE.make "/kernel_version"
  else EVM.make "/kernel_version"

let kernel_verbosity ~storage_version =
  if Storage_version.governance_config_moved_to_base ~storage_version then
    BASE.make "/logging_verbosity"
  else EVM.make "/logging_verbosity"

let storage_version_base = BASE.make "/storage_version"

let storage_version_legacy = EVM.make "/storage_version"

let kernel_root_hash ~storage_version =
  if Storage_version.governance_config_moved_to_base ~storage_version then
    BASE.make "/kernel_root_hash"
  else EVM.make "/kernel_root_hash"

let kernel_upgrade ~storage_version =
  if Storage_version.governance_config_moved_to_base ~storage_version then
    BASE.make "/kernel_upgrade"
  else EVM.make "/kernel_upgrade"

let sequencer_upgrade ~storage_version =
  if
    Storage_version.sequencer_key_storage_migrated_to_world_state
      ~storage_version
  then EVM.World_state.make "/sequencer_upgrade"
  else EVM.make "/sequencer_upgrade"

let delayed_inbox ~storage_version =
  if Storage_version.governance_config_moved_to_base ~storage_version then
    BASE.make "/delayed-inbox"
  else EVM.make "/delayed-inbox"

let sequencer_pool_address ~storage_version =
  if Storage_version.evm_config_moved_to_world_state ~storage_version then
    EVM.World_state.make "/sequencer_pool_address"
  else EVM.make "/sequencer_pool_address"

let sequencer_key_legacy = EVM.make "/sequencer"

let sequencer_key_world_state = EVM.World_state.make "/sequencer"

let sequencer_key ~storage_version =
  if
    Storage_version.sequencer_key_storage_migrated_to_world_state
      ~storage_version
  then sequencer_key_world_state
  else sequencer_key_legacy

let maximum_gas_per_transaction ~storage_version =
  if Storage_version.evm_config_moved_to_world_state ~storage_version then
    EVM.World_state.make "/maximum_gas_per_transaction"
  else EVM.make "/maximum_gas_per_transaction"

let michelson_runtime_sunrise_level ~storage_version =
  if
    Storage_version.michelson_runtime_paths_moved_to_world_state
      ~storage_version
  then TEZ.World_state.make "/michelson_runtime/sunrise_level"
  else EVM.make "/michelson_runtime/sunrise_level"

let michelson_runtime_target_sunrise_level ~storage_version =
  if
    Storage_version.michelson_runtime_paths_moved_to_world_state
      ~storage_version
  then TEZ.World_state.make "/michelson_runtime/target_sunrise_level"
  else EVM.make "/michelson_runtime/target_sunrise_level"

let maximum_allowed_ticks ~storage_version =
  if Storage_version.governance_config_moved_to_base ~storage_version then
    BASE.make "/maximum_allowed_ticks"
  else EVM.make "/maximum_allowed_ticks"

module Accounts = struct
  let accounts_root ~storage_version =
    if Storage_version.evm_accounts_isolated ~storage_version then
      EVM.Eth_accounts.make ""
    else EVM.World_state.make "/eth_accounts"

  let info_path = "/info"

  let balance_path = "/balance"

  let nonce_path = "/nonce"

  let code_path = "/code"

  let code_hash = "/code.hash"

  let storage_path = "/storage"

  let account ~storage_version (Address (Hex s)) =
    accounts_root ~storage_version ^ "/" ^ s

  let info ~storage_version address =
    account ~storage_version address ^ info_path

  let balance ~storage_version address =
    account ~storage_version address ^ balance_path

  let nonce ~storage_version address =
    account ~storage_version address ^ nonce_path

  let code ~storage_version address =
    account ~storage_version address ^ code_path

  let code_hash ~storage_version address =
    account ~storage_version address ^ code_hash

  type error += Invalid_address of string | Invalid_key of string

  let () =
    register_error_kind
      `Permanent
      ~id:"durable_storage_invalid_address"
      ~title:"Invalid address"
      ~description:"Tried to access the account storage of an invalid address."
      ~pp:(fun ppf path ->
        Format.fprintf ppf "No account storage for invalid address %s" path)
      Data_encoding.(obj1 (req "durable_storage_invalid_address" string))
      (function Invalid_address path -> Some path | _ -> None)
      (fun path -> Invalid_address path) ;
    register_error_kind
      `Permanent
      ~id:"durable_storage_invalid_key"
      ~title:"Invalid storage key"
      ~description:"Tried to access an invalid key in an account storage."
      ~pp:(fun ppf path ->
        Format.fprintf ppf "%s is not a valid storage key" path)
      Data_encoding.(obj1 (req "durable_storage_invalid_key" string))
      (function Invalid_key path -> Some path | _ -> None)
      (fun path -> Invalid_key path)

  (* Validated wrappers: their values are only constructed through
     [fixed_address] / [fixed_index], which check the length of the
     underlying hex strings. The representations are abstract via
     [.mli] so callers cannot bypass the validators. *)
  type fixed_address = path

  type fixed_index = path

  let fixed_address ~storage_version (Address (Hex s)) =
    let open Result_syntax in
    if String.length s = 40 then
      return (accounts_root ~storage_version ^ "/" ^ s)
    else tzfail (Invalid_address s)

  let fixed_index s =
    let open Result_syntax in
    if String.length s = 64 then return s else tzfail (Invalid_key s)

  let storage addr idx = String.concat "" [addr; storage_path; "/"; idx]

  let storage_dir addr = addr ^ storage_path
end

module Code = struct
  let codes ~storage_version =
    if Storage_version.evm_accounts_isolated ~storage_version then
      EVM.Eth_accounts.make "/eth_codes"
    else EVM.World_state.make "/eth_codes"

  let code_storage ~storage_version (Hash (Hex hash)) =
    codes ~storage_version ^ "/" ^ hash

  let code_path = "/code"

  let code ~storage_version code_hash =
    code_storage ~storage_version code_hash ^ code_path
end

module Blueprint = struct
  let blueprints_root ~storage_version =
    if Storage_version.governance_config_moved_to_base ~storage_version then
      BASE.make "/blueprints"
    else EVM.make "/blueprints"

  let current_generation ~storage_version =
    blueprints_root ~storage_version ^ "/generation"

  let blueprint ~storage_version blueprint_number =
    blueprints_root ~storage_version ^ "/" ^ Z.to_string blueprint_number

  let chunk ~storage_version ~blueprint_number ~chunk_index =
    blueprint ~storage_version blueprint_number
    ^ "/" ^ string_of_int chunk_index

  let nb_chunks ~storage_version ~blueprint_number =
    blueprint ~storage_version blueprint_number ^ "/nb_chunks"

  let generation ~storage_version ~blueprint_number =
    blueprint ~storage_version blueprint_number ^ "/generation"
end

module Block = struct
  type number = Current | Nth of Z.t

  let by_hash ~root (Block_hash (Hex hash)) = root ^ "/blocks" ^ "/" ^ hash

  let current_block_parent ~root = root ^ "/blocks/current"

  let current_block ~root = current_block_parent ~root ^ "/block"

  let current_number ~root = current_block_parent ~root ^ "/number"

  let current_hash ~root = current_block_parent ~root ^ "/hash"

  let current_receipts ~root =
    current_block_parent ~root ^ "/transactions_receipts"

  let current_transactions_objects ~root =
    current_block_parent ~root ^ "/transactions_objects"
end

module BlockHeader = struct
  let current ~storage_version =
    if Storage_version.governance_config_moved_to_base ~storage_version then
      "/base/current_block_header"
    else "/evm/current_block_header"
end

module Indexes = struct
  (* Phase 6 (V58) reorganized the block-index path from
     [<world_state>/indexes/blocks/{N}] to
     [<world_state>/blocks/indexes/{N}]. This applies to every root
     (EVM and Michelson alike): block_storage.rs uses the new layout
     unconditionally for all chains. *)

  let legacy_blocks ~root = root ^ "/indexes/blocks"

  let new_blocks ~root = root ^ "/blocks/indexes"

  let blocks ~storage_version ~root =
    if Storage_version.evm_config_moved_to_world_state ~storage_version then
      new_blocks ~root
    else legacy_blocks ~root

  let number_to_string = function
    | Block.Current -> "current"
    | Nth i -> Z.to_string i

  let block_by_number ~storage_version ~root number =
    blocks ~storage_version ~root ^ "/" ^ number_to_string number
end

module Transaction_receipt = struct
  let receipts = EVM.World_state.make "/transactions_receipts"

  let receipt (Hash (Hex tx_hash)) = receipts ^ "/" ^ tx_hash
end

module Transaction_object = struct
  let objects = EVM.World_state.make "/transactions_objects"

  let object_ (Hash (Hex tx_hash)) = objects ^ "/" ^ tx_hash
end

module Delayed_transaction = struct
  let hashes ~storage_version =
    if Storage_version.governance_config_moved_to_base ~storage_version then
      BASE.make "/delayed-inbox"
    else EVM.make "/delayed-inbox"

  let transaction ~storage_version (Hash (Hex tx_hash)) =
    hashes ~storage_version ^ "/" ^ tx_hash ^ "/data"
end

module Evm_events = struct
  let events ~storage_version =
    if Storage_version.governance_config_moved_to_base ~storage_version then
      BASE.make "/rollup_events"
    else EVM.make "/events"

  let length ~storage_version = events ~storage_version ^ "/" ^ "length"

  let nth_event ~storage_version i =
    events ~storage_version ^ "/" ^ string_of_int i
end

module Http_trace = struct
  (* Durable storage paths used by the per-transaction HTTP trace replay
     RPCs.

     The [enabled_flag] key sits at [/base/__http_trace_enabled] —
     alongside the other node-driven control keys ([/base/__evm_node],
     [/base/__simulation/...], [/base/__delayed_input]). The EVM node
     writes it via [alter_evm_state] before the replay, and the kernel
     reads it exactly once at the top of [block::produce] before any
     [SafeStorage] wrapping, so nothing inside the apply chain has to
     round-trip through [SafeStorage] to see it.

     [root ~storage_version] is where the kernel persists the per-tx
     traces. On V60+ kernels the subtree is dedicated under [/base/]
     and the kernel promotes [/tmp/base/__http_trace] through
     [SafeStorage::promote_http_trace] at block finalisation; on older
     kernels it lived under [/evm/world_state/] because all writes went
     through the [SafeStorage]-wrapped host, requiring it to sit under a
     promoted subtree to be readable from the post-replay state. *)
  let root ~storage_version =
    if Storage_version.simulation_trace_ipc_moved_to_base ~storage_version then
      BASE.make "/__http_trace/traces"
    else EVM.World_state.make "/__http_trace/traces"

  let enabled_flag = BASE.make "/__http_trace_enabled"

  let for_tx ~storage_version ~transaction_hash =
    root ~storage_version ^ "/" ^ transaction_hash
end

module Trace = struct
  let root ~storage_version =
    if Storage_version.simulation_trace_ipc_moved_to_base ~storage_version then
      BASE.make "/trace"
    else EVM.make "/trace"

  let root_indexed_by_hash ~storage_version ~transaction_hash =
    match transaction_hash with
    | Some transaction_hash -> root ~storage_version ^ "/" ^ transaction_hash
    | None -> root ~storage_version

  let input ~storage_version = root ~storage_version ^ "/input"

  let output_gas ~storage_version ~transaction_hash =
    root_indexed_by_hash ~storage_version ~transaction_hash ^ "/gas"

  let output_failed ~storage_version ~transaction_hash =
    root_indexed_by_hash ~storage_version ~transaction_hash ^ "/failed"

  let output_return_value ~storage_version ~transaction_hash =
    root_indexed_by_hash ~storage_version ~transaction_hash ^ "/return_value"

  let opcodes_root ~storage_version ~transaction_hash =
    root_indexed_by_hash ~storage_version ~transaction_hash ^ "/struct_logs"

  let logs_length ~storage_version ~transaction_hash =
    opcodes_root ~storage_version ~transaction_hash ^ "/length"

  let opcode ~storage_version ~transaction_hash i =
    opcodes_root ~storage_version ~transaction_hash ^ "/" ^ string_of_int i

  let call_trace_root ~storage_version ~transaction_hash =
    root_indexed_by_hash ~storage_version ~transaction_hash ^ "/call_trace"

  let call_trace_length ~storage_version ~transaction_hash =
    call_trace_root ~storage_version ~transaction_hash ^ "/length"

  let call_trace ~storage_version ~transaction_hash i =
    call_trace_root ~storage_version ~transaction_hash ^ "/" ^ string_of_int i
end

(* Simulation insight paths. Each is the path the kernel writes during a
   [simulate_and_read*] host call; the EVM node requests it back through
   [Durable_storage_key], so simulator.ml needs the path components, not
   the path string. *)
let evm_simulation_result ~storage_version =
  if Storage_version.simulation_trace_ipc_moved_to_base ~storage_version then
    BASE.make "/evm_simulation_result"
  else EVM.make "/simulation_result"

let evm_simulation_http_traces ~storage_version =
  if Storage_version.simulation_trace_ipc_moved_to_base ~storage_version then
    BASE.make "/simulation_http_traces"
  else EVM.make "/simulation_http_traces"

let tez_simulation_result ~storage_version =
  if Storage_version.simulation_trace_ipc_moved_to_base ~storage_version then
    BASE.make "/tez_simulation_result"
  else TEZ.World_state.make "/simulation_result"

(* Split a [/foo/bar/baz] path into the [["foo"; "bar"; "baz"]] component
   list expected by [Simulation.Encodings.Durable_storage_key]. *)
let to_components path =
  String.split_on_char '/' path |> List.filter (fun s -> s <> "")

module Chain_configuration = struct
  open L2_types

  let root ~storage_version chain_id =
    let prefix =
      if Storage_version.governance_config_moved_to_base ~storage_version then
        BASE.make "/chain_configurations/"
      else EVM.make "/chain_configurations/"
    in
    prefix ^ Chain_id.to_string chain_id

  let minimum_base_fee_per_gas ~storage_version chain_id =
    root ~storage_version chain_id ^ "/minimum_base_fee_per_gas"

  let da_fee_per_byte ~storage_version chain_id =
    root ~storage_version chain_id ^ "/da_fee_per_byte"

  let maximum_gas_per_transaction ~storage_version chain_id =
    root ~storage_version chain_id ^ "/maximum_gas_per_transaction"

  let chain_family ~storage_version chain_id =
    root ~storage_version chain_id ^ "/chain_family"

  let world_state ~storage_version chain_id =
    root ~storage_version chain_id ^ "/world_state"
end

module Feature_flags = struct
  let root = BASE.make "/feature_flags"

  let multichain = root ^ "/enable_multichain"

  let tezos_runtime = root ^ "/enable_tezos_runtime"
end
