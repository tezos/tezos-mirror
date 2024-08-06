(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2023-2024 Functori <contact@functori.com>                   *)
(* Copyright (c) 2023 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

type path = string

let evm_node_flag = "/__evm_node"

module EVM = struct
  let root = "/evm"

  let make s = root ^ s
end

module World_state = struct
  let root = "/world_state"

  let make s = EVM.make (root ^ s)
end

let chain_id = EVM.make "/chain_id"

let base_fee_per_gas = World_state.make "/fees/base_fee_per_gas"

let da_fee_per_byte = World_state.make "/fees/da_fee_per_byte"

let kernel_version = EVM.make "/kernel_version"

let storage_version = EVM.make "/storage_version"

let kernel_root_hash = EVM.make "/kernel_root_hash"

let kernel_upgrade = EVM.make "/kernel_upgrade"

let sequencer_upgrade = EVM.make "/sequencer_upgrade"

let delayed_inbox = EVM.make "/delayed-inbox"

let sequencer_pool_address = EVM.make "/sequencer_pool_address"

let sequencer_key = EVM.make "/sequencer"

let maximum_gas_per_transaction = EVM.make "/maximum_gas_per_transaction"

module Accounts = struct
  let accounts = World_state.make "/eth_accounts"

  let balance = "/balance"

  let nonce = "/nonce"

  let code = "/code"

  let storage = "/storage"

  let account (Address (Hex s)) = accounts ^ "/" ^ s

  let balance address = account address ^ balance

  let nonce address = account address ^ nonce

  let code address = account address ^ code

  let storage address index = account address ^ storage ^ "/" ^ index
end

module Block = struct
  type number = Current | Nth of Z.t

  let blocks = World_state.make "/blocks"

  let number = "/number"

  let by_hash (Block_hash (Hex hash)) = blocks ^ "/" ^ hash

  let current_number = blocks ^ "/current" ^ number

  let current_hash = blocks ^ "/current/hash"
end

module Indexes = struct
  let indexes = World_state.make "/indexes"

  let blocks = "/blocks"

  let blocks = indexes ^ blocks

  let number_to_string = function
    | Block.Current -> "current"
    | Nth i -> Z.to_string i

  let block_by_number number = blocks ^ "/" ^ number_to_string number
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
  let hashes = EVM.make "/delayed-inbox"

  let transaction (Hash (Hex tx_hash)) = hashes ^ "/" ^ tx_hash ^ "/data"
end

module Evm_events = struct
  let events = EVM.make "/events"

  let length = events ^ "/" ^ "length"

  let nth_event i = events ^ "/" ^ string_of_int i
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
