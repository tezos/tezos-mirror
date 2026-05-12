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

module BASE = struct
  let root = "/base"

  let make s = root ^ s
end

module EVM = struct
  let root = "/evm"

  let make s = root ^ s
end

module TEZ = struct
  module World_state = struct
    let root = "/tez/world_state"

    let make s = root ^ s
  end
end

let evm_node_flag_legacy = "/__evm_node"

let evm_node_flag_base = BASE.make evm_node_flag_legacy

let evm_node_flag ~storage_version =
  if Storage_version.ipc_paths_moved_to_base ~storage_version then
    evm_node_flag_base
  else evm_node_flag_legacy

module World_state = struct
  let root = "/world_state"

  let make s = EVM.make (root ^ s)
end

module Single_tx = struct
  let input_tx_base = BASE.make "/instant_confirmation/input_tx"

  let input_tx_legacy = World_state.make "/single_tx/input_tx"

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

  let input_legacy = World_state.make "/assemble_block/input"

  let input ~storage_version =
    if Storage_version.ipc_paths_moved_to_base ~storage_version then input_base
    else input_legacy
end

let etherlink_root = World_state.make ""

let etherlink_safe_root = "/tmp" ^ World_state.make ""

let michelson_contracts_index = "/tez/tez_accounts/contracts/index"

let michelson_ledger_root = "/tez/tez_accounts/tezosx"

let tez_world_state_root = TEZ.World_state.root

let tez_world_state_safe_root = "/tmp" ^ TEZ.World_state.root

(** TezosX: Tezos blocks live in the Michelson world-state keyspace. *)
let tezosx_tezos_blocks_root = TEZ.World_state.make "/tez_blocks"

let root_of_chain_family (type f) (chain_family : f L2_types.chain_family) =
  match chain_family with
  | L2_types.EVM -> etherlink_root
  (* TezosX and standalone Tezlink both store Michelson block data at
     /tez/world_state/tez_blocks (= tezosx_tezos_blocks_root). This is
     the root the kernel passes to [block_storage::store_current] for
     Michelson blocks, see [MichelsonChainConfig::finalize_and_store]
     and [BlockInProgress::finalize_and_store]. *)
  | L2_types.Michelson -> tezosx_tezos_blocks_root

let chain_id = EVM.make "/chain_id"

let michelson_runtime_chain_id = TEZ.World_state.make "/chain_id"

let minimum_base_fee_per_gas = World_state.make "/fees/minimum_base_fee_per_gas"

let backlog = World_state.make "/fees/backlog"

let da_fee_per_byte = World_state.make "/fees/da_fee_per_byte"

let michelson_to_evm_gas_multiplier =
  World_state.make "/fees/michelson_to_evm_gas_multiplier"

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
  then World_state.make "/sequencer_upgrade"
  else EVM.make "/sequencer_upgrade"

let delayed_inbox ~storage_version =
  if Storage_version.governance_config_moved_to_base ~storage_version then
    BASE.make "/delayed-inbox"
  else EVM.make "/delayed-inbox"

let sequencer_pool_address = EVM.make "/sequencer_pool_address"

let sequencer_key_legacy = EVM.make "/sequencer"

let sequencer_key_world_state = World_state.make "/sequencer"

let sequencer_key ~storage_version =
  if
    Storage_version.sequencer_key_storage_migrated_to_world_state
      ~storage_version
  then sequencer_key_world_state
  else sequencer_key_legacy

let maximum_gas_per_transaction = EVM.make "/maximum_gas_per_transaction"

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
  let accounts_path = World_state.make "/eth_accounts"

  let info_path = "/info"

  let balance_path = "/balance"

  let nonce_path = "/nonce"

  let code_path = "/code"

  let code_hash = "/code.hash"

  let storage_path = "/storage"

  let account (Address (Hex s)) = accounts_path ^ "/" ^ s

  let info address = account address ^ info_path

  let balance address = account address ^ balance_path

  let nonce address = account address ^ nonce_path

  let code address = account address ^ code_path

  let code_hash address = account address ^ code_hash

  let storage address index = account address ^ storage_path ^ "/" ^ index

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

  let account_e (Address (Hex s)) =
    let open Result_syntax in
    if String.length s = 40 then return (accounts_path ^ "/" ^ s)
    else tzfail (Invalid_address s)

  let concat_e address path =
    let open Result_syntax in
    let* address_path = account_e address in
    return (address_path ^ path)

  let storage_e address index =
    if String.length index = 64 then
      concat_e address (storage_path ^ "/" ^ index)
    else Result_syntax.tzfail (Invalid_key index)

  let storage_dir_e address = concat_e address storage_path
end

module Code = struct
  let codes = World_state.make "/eth_codes"

  let code_storage (Hash (Hex hash)) = codes ^ "/" ^ hash

  let code = "/code"

  let code code_hash = code_storage code_hash ^ code
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
  let indexes ~root = root ^ "/indexes"

  let blocks = "/blocks"

  let blocks ~root = indexes ~root ^ blocks

  let number_to_string = function
    | Block.Current -> "current"
    | Nth i -> Z.to_string i

  let block_by_number ~root number =
    blocks ~root ^ "/" ^ number_to_string number
end

module Transaction_receipt = struct
  let receipts = World_state.make "/transactions_receipts"

  let receipt (Hash (Hex tx_hash)) = receipts ^ "/" ^ tx_hash
end

module Transaction_object = struct
  let objects = World_state.make "/transactions_objects"

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

     [root] is where the kernel persists the per-transaction traces: it
     *does* sit under [/evm/world_state/] because those writes happen
     through the [SafeStorage]-wrapped host, so they need to live under a
     promoted subtree to be readable from the post-replay state. *)
  let root = World_state.make "/__http_trace/traces"

  let enabled_flag = BASE.make "/__http_trace_enabled"

  let for_tx ~transaction_hash = root ^ "/" ^ transaction_hash
end

module Trace = struct
  let root = EVM.make "/trace"

  let root_indexed_by_hash ~transaction_hash =
    match transaction_hash with
    | Some transaction_hash -> root ^ "/" ^ transaction_hash
    | None -> root

  let input = root ^ "/input"

  let output_gas ~transaction_hash =
    root_indexed_by_hash ~transaction_hash ^ "/gas"

  let output_failed ~transaction_hash =
    root_indexed_by_hash ~transaction_hash ^ "/failed"

  let output_return_value ~transaction_hash =
    root_indexed_by_hash ~transaction_hash ^ "/return_value"

  let opcodes_root ~transaction_hash =
    root_indexed_by_hash ~transaction_hash ^ "/struct_logs"

  let logs_length ~transaction_hash = opcodes_root ~transaction_hash ^ "/length"

  let opcode ~transaction_hash i =
    opcodes_root ~transaction_hash ^ "/" ^ string_of_int i

  let call_trace_root ~transaction_hash =
    root_indexed_by_hash ~transaction_hash ^ "/call_trace"

  let call_trace_length ~transaction_hash =
    call_trace_root ~transaction_hash ^ "/length"

  let call_trace ~transaction_hash i =
    call_trace_root ~transaction_hash ^ "/" ^ string_of_int i
end

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
