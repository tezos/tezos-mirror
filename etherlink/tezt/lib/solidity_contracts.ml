(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023-2024 TriliTech <contact@trili.tech>                    *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2023-2024 Functori <contact@functori.com>                   *)
(*                                                                           *)
(*****************************************************************************)

open Helpers

type contract = {label : string; abi : string; bin : string}

(** The info for the "storage.sol" contract.
    See [etherlink/tezt/tests/evm_kernel_inputs/storage.*] *)
let simple_storage =
  {
    label = "simpleStorage";
    abi = kernel_inputs_path ^ "/storage.abi";
    bin = kernel_inputs_path ^ "/storage.bin";
  }

(** The info for the "erc20tok.sol" contract.
    See [etherlink/tezt/tests/evm_kernel_inputs/erc20tok.*] *)
let erc20 =
  {
    label = "erc20tok";
    abi = kernel_inputs_path ^ "/erc20tok.abi";
    bin = kernel_inputs_path ^ "/erc20tok.bin";
  }

(** The info for the "loop.sol" contract.
    See [etherlink/tezt/tests/evm_kernel_inputs/loop.*] *)
let loop =
  {
    label = "loop";
    abi = kernel_inputs_path ^ "/loop.abi";
    bin = kernel_inputs_path ^ "/loop.bin";
  }

(** The info for the "mapping_storage.sol" contract.
    See [etherlink/tezt/tests/evm_kernel_inputs/mapping_storage*] *)
let mapping_storage =
  {
    label = "mappingStorage";
    abi = kernel_inputs_path ^ "/mapping_storage_abi.json";
    bin = kernel_inputs_path ^ "/mapping_storage.bin";
  }

(** The info for the "storage.sol" contract, compiled for Shanghai.
    See [etherlink/tezt/tests/evm_kernel_inputs/shanghai_storage.*] *)
let shanghai_storage =
  {
    label = "shanghai";
    abi = kernel_inputs_path ^ "/shanghai_storage.abi";
    bin = kernel_inputs_path ^ "/shanghai_storage.bin";
  }

(** The info for the Callee contract.
    See [src\kernel_evm\solidity_examples\caller_callee.sol] *)
let callee =
  {
    label = "callee";
    abi = kernel_inputs_path ^ "/callee.abi";
    bin = kernel_inputs_path ^ "/callee.bin";
  }

(** The info for the Caller contract.
    See [src\kernel_evm\solidity_examples\caller_callee.sol] *)
let caller =
  {
    label = "caller";
    abi = kernel_inputs_path ^ "/caller.abi";
    bin = kernel_inputs_path ^ "/caller.bin";
  }

(** The info for the "events.sol" contract.
    See [etherlink/kernel_evm/solidity_examples/events.sol] *)
let events =
  {
    label = "events";
    abi = kernel_inputs_path ^ "/events.abi";
    bin = kernel_inputs_path ^ "/events.bin";
  }

(** The info for the "nested_create.sol" contract.
    See [etherlink/kernel_evm/solidity_examples/nested_create.sol] *)
let nested_create =
  {
    label = "nested_create";
    abi = kernel_inputs_path ^ "/nested_create.abi";
    bin = kernel_inputs_path ^ "/nested_create.bin";
  }

(** The info for the "revert.sol" contract.
    See [etherlink/kernel_evm/solidity_examples/revert.sol] *)
let revert =
  {
    label = "revert";
    abi = kernel_inputs_path ^ "/revert.abi";
    bin = kernel_inputs_path ^ "/revert.bin";
  }

(** The info for the "create2.sol" contract.
    See [etherlink/kernel_evm/solidity_examples/create2.sol] *)
let create2 =
  {
    label = "create2";
    abi = kernel_inputs_path ^ "/create2.abi";
    bin = kernel_inputs_path ^ "/create2.bin";
  }

(** The info for the "oog_call.sol" contract.
    See [etherlink/kernel_evm/solidity_examples/oog_call.sol] *)
let oog_call =
  {
    label = "oog_call";
    abi = kernel_inputs_path ^ "/oog_call.abi";
    bin = kernel_inputs_path ^ "/oog_call.bin";
  }

(** The info for the "ether_wallet.sol" contract.
    See [etherlink/kernel_evm/solidity_examples/ether_wallet.sol] *)
let ether_wallet =
  {
    label = "ether_wallet";
    abi = kernel_inputs_path ^ "/ether_wallet.abi";
    bin = kernel_inputs_path ^ "/ether_wallet.bin";
  }

(** The info for the "block_hash_gen.sol" contract.
    See [etherlink/kernel_evm/solidity_examples/block_hash_gen.sol] *)
let block_hash_gen =
  {
    label = "block_hash_gen";
    abi = kernel_inputs_path ^ "/block_hash_gen.abi";
    bin = kernel_inputs_path ^ "/block_hash_gen.bin";
  }

(** The info for the "block_hash_gen.sol" contract.
    See [etherlink/kernel_evm/solidity_examples/blockhash.sol] *)
let blockhash =
  {
    label = "blockhash";
    abi = kernel_inputs_path ^ "/blockhash.abi";
    bin = kernel_inputs_path ^ "/blockhash.bin";
  }

(** The info for the "timestamp.sol" contract.
    See [etherlink/kernel_evm/solidity_examples/timestamp.sol] *)
let timestamp =
  {
    label = "timestamp";
    abi = kernel_inputs_path ^ "/timestamp.abi";
    bin = kernel_inputs_path ^ "/timestamp.bin";
  }

(** The info for the "call_selfdestruct.sol" contract.
    See [etherlink/kernel_evm/solidity_examples/call_selfdestruct.sol] *)
let call_selfdestruct =
  {
    label = "call_selfdestruct";
    abi = kernel_inputs_path ^ "/call_selfdestruct.abi";
    bin = kernel_inputs_path ^ "/call_selfdestruct.bin";
  }

(** The info for the "recursive.sol" contract.
    See [etherlink/kernel_evm/solidity_examples/recursive.sol] *)
let recursive =
  {
    label = "recursive";
    abi = kernel_inputs_path ^ "/recursive.abi";
    bin = kernel_inputs_path ^ "/recursive.bin";
  }

(** The info for the "error.sol" contract.
    See [etherlink/kernel_evm/solidity_examples/error.sol] *)
let error =
  {
    label = "error";
    abi = kernel_inputs_path ^ "/error.abi";
    bin = kernel_inputs_path ^ "/error.bin";
  }

(** The info for the "block_hash_gen.sol" contract.
    See [etherlink/kernel_evm/solidity_examples/spam_withdrawal.sol] *)
let spam_withdrawal =
  {
    label = "spam_withdrawal";
    abi = kernel_inputs_path ^ "/spam_withdrawal.abi";
    bin = kernel_inputs_path ^ "/spam_withdrawal.bin";
  }

(** The info for the "gas_limit.sol" contract.
    See [etherlink/kernel_evm/solidity_examples/gas_limit.sol] *)
let gas_limit_contract =
  {
    label = "gas_limit_contract";
    abi = kernel_inputs_path ^ "/gas_limit.abi";
    bin = kernel_inputs_path ^ "/gas_limit.bin";
  }

(** The info for the "gas_limit.sol" contract.
    See [etherlink/kernel_evm/solidity_examples/counter.sol] *)
let counter =
  {
    label = "counter";
    abi = kernel_inputs_path ^ "/counter.abi";
    bin = kernel_inputs_path ^ "/counter.bin";
  }
