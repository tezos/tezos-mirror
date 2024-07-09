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

let solidity_contracts_path = "etherlink/kernel_evm/solidity_examples"

module JSON = Tezt.JSON

let generate_json_string ~label ~contract ~path ~evm_version =
  let contents = Tezt.Base.read_file path in
  Printf.sprintf
    {|
{
  "language": "Solidity",
  "sources": {
    "%s": {
      "content": %S
    }
  },
  "settings": {
    "evmVersion": "%s",
    "optimizer": {
      "enabled": true,
      "runs": 200
    },
    "outputSelection": {
      "*": {
        "%s": [
          "abi",
          "evm.bytecode.object"
        ]
      }
    }
  }
}
    |}
    label
    contents
    evm_version
    contract

let compile_contract ~source ~label ~contract ~evm_version =
  (* Construct the JSON input for solc *)
  let input_json =
    generate_json_string ~label ~contract ~path:source ~evm_version
  in
  let command = "npx" in
  let args = ["--yes"; "solc"; "--standard-json"] in

  (* Spawn the process with the JSON input as stdin *)
  let* process, stdin_channel =
    Lwt.return (Tezt.Process.spawn_with_stdin command args)
  in

  (* Write the JSON input to the stdin of the process *)
  let* () = Lwt_io.write stdin_channel input_json in
  let* () = Lwt_io.close stdin_channel in

  (* Read the stdout of the process *)
  let* result = Tezt.Process.check_and_read_stdout process in

  (* If using solcjs, then some extra logs might appear
     These logs begin with whitespaces and are followed by >>>
     We define a regular expression to match these lines starting
     and then delete them *)
  let re = Re.(compile (seq [bos; rep space; str ">>>"])) in
  let lines = String.split_on_char '\n' result in
  let lines = List.filter (fun line -> not (Re.execp re line)) lines in
  let result = String.concat "\n" lines in

  let json = JSON.parse ~origin:"solc" result in

  let abi = JSON.(json |-> "contracts" |-> label |-> contract |-> "abi") in
  let bin =
    JSON.(
      json |-> "contracts" |-> label |-> contract |-> "evm" |-> "bytecode"
      |-> "object" |> as_string)
  in

  let abi_file = Tezt.Temp.file (label ^ ".abi") in
  let bin_file = Tezt.Temp.file (label ^ ".bin") in
  JSON.encode_to_file abi_file abi ;
  Tezt.Base.write_file bin_file ~contents:bin ;
  return {label; abi = abi_file; bin = bin_file}

(** The info for the "storage.sol" contract.
    See [etherlink/tezt/tests/evm_kernel_inputs/storage.*] *)
let simple_storage =
  {
    label = "simpleStorage";
    abi = kernel_inputs_path ^ "/storage.abi";
    bin = kernel_inputs_path ^ "/storage.bin";
  }

(** The info for the "erc20tok.sol" contract. *)
let erc20 () =
  compile_contract
    ~source:(solidity_contracts_path ^ "/erc20tok.sol")
    ~label:"erc20tok"
    ~contract:"ERC20"
    ~evm_version:"london"

(** The info for the "loop.sol" contract.
    See [etherlink/tezt/tests/evm_kernel_inputs/loop.*] *)
let loop =
  {
    label = "loop";
    abi = kernel_inputs_path ^ "/loop.abi";
    bin = kernel_inputs_path ^ "/loop.bin";
  }

(** The info for the "mapping_storage.sol" contract. *)
let mapping_storage () =
  compile_contract
    ~source:(solidity_contracts_path ^ "/mapping_storage.sol")
    ~label:"mapping_storage"
    ~contract:"MappingStorage"
    ~evm_version:"london"

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

(** The info for the "events.sol" contract. *)
let events () =
  compile_contract
    ~source:(solidity_contracts_path ^ "/events.sol")
    ~label:"events"
    ~contract:"Events"
    ~evm_version:"london"

(** The info for the "nested_create.sol" contract. *)
let nested_create () =
  compile_contract
    ~source:(solidity_contracts_path ^ "/nested_create.sol")
    ~label:"nested_create"
    ~contract:"Creator"
    ~evm_version:"london"

(** The info for the "revert.sol" contract. *)
let revert () =
  compile_contract
    ~source:(solidity_contracts_path ^ "/revert.sol")
    ~label:"revert"
    ~contract:"Revert"
    ~evm_version:"london"

(** The info for the "create2.sol" contract. *)
let create2 () =
  compile_contract
    ~source:(solidity_contracts_path ^ "/create2.sol")
    ~label:"create2"
    ~contract:"Create2"
    ~evm_version:"london"

(** The info for the "oog_call.sol" contract. *)
let oog_call () =
  compile_contract
    ~source:(solidity_contracts_path ^ "/oog_call.sol")
    ~label:"oog_call"
    ~contract:"OOGCall"
    ~evm_version:"london"

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

(** The info for the "coinbase.sol" contract.
    See [etherlink/kernel_evm/solidity_examples/coinbase.sol] *)
let coinbase =
  {
    label = "coinbase";
    abi = kernel_inputs_path ^ "/coinbase.abi";
    bin = kernel_inputs_path ^ "/coinbase.bin";
  }

(** The info for the "block_constants.sol" contract.
    See [etherlink/kernel_evm/solidity_examples/block_constants.sol] *)
let block_constants =
  {
    label = "blocks_constants";
    abi = kernel_inputs_path ^ "/block_constants.abi";
    bin = kernel_inputs_path ^ "/block_constants.bin";
  }

(** The info for the "call_withdrawal.sol" contract.
    See [etherlink/kernel_evm/solidity_examples/call_withdrawal.sol] *)
let call_withdrawal =
  {
    label = "call_withdrawal";
    abi = kernel_inputs_path ^ "/call_withdrawal.abi";
    bin = kernel_inputs_path ^ "/call_withdrawal.bin";
  }

(** The info for the "callcode_withdrawal.sol" contract.
    See [etherlink/kernel_evm/solidity_examples/callcode_withdrawal.sol] *)
let callcode_withdrawal =
  {
    label = "callcode_withdrawal";
    abi = kernel_inputs_path ^ "/callcode_withdrawal.abi";
    bin = kernel_inputs_path ^ "/callcode_withdrawal.bin";
  }
