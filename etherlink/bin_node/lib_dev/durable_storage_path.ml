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
  let accounts_path = World_state.make "/eth_accounts"

  let balance_path = "/balance"

  let nonce_path = "/nonce"

  let code_path = "/code"

  let code_hash = "/code.hash"

  let storage_path = "/storage"

  let account (Address (Hex s)) = accounts_path ^ "/" ^ s

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

  let balance_e address = concat_e address balance_path

  let nonce_e address = concat_e address nonce_path

  let code_e address = concat_e address code_path

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
