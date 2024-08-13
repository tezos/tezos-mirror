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

(** The info for the "storage.sol" contract. *)
let simple_storage () =
  compile_contract
    ~source:(solidity_contracts_path ^ "/storage.sol")
    ~label:"simpleStorage"
    ~contract:"SimpleStorage"
    ~evm_version:"london"

(** The info for the "erc20tok.sol" contract. *)
let erc20 () =
  compile_contract
    ~source:(solidity_contracts_path ^ "/erc20tok.sol")
    ~label:"erc20tok"
    ~contract:"ERC20"
    ~evm_version:"london"

(** The info for the "loop.sol" contract. *)
let loop () =
  compile_contract
    ~source:(solidity_contracts_path ^ "/loop.sol")
    ~label:"loop"
    ~contract:"Loop"
    ~evm_version:"london"

(** The info for the "mapping_storage.sol" contract. *)
let mapping_storage () =
  compile_contract
    ~source:(solidity_contracts_path ^ "/mapping_storage.sol")
    ~label:"mapping_storage"
    ~contract:"MappingStorage"
    ~evm_version:"london"

(** The info for the "storage.sol" contract, compiled for Shanghai. *)
let shanghai_storage () =
  compile_contract
    ~source:(solidity_contracts_path ^ "/storage.sol")
    ~label:"shanghai_storage"
    ~contract:"SimpleStorage"
    ~evm_version:"shanghai"

(** The info for the Callee contract. *)
let callee () =
  compile_contract
    ~source:(solidity_contracts_path ^ "/caller_callee.sol")
    ~label:"callee"
    ~contract:"Callee"
    ~evm_version:"london"

(** The info for the Caller contract. *)
let caller () =
  compile_contract
    ~source:(solidity_contracts_path ^ "/caller_callee.sol")
    ~label:"caller"
    ~contract:"Caller"
    ~evm_version:"london"

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

(** The info for the "ether_wallet.sol" contract. *)
let ether_wallet () =
  compile_contract
    ~source:(solidity_contracts_path ^ "/ether_wallet.sol")
    ~label:"ether_wallet"
    ~contract:"SharedEtherWallet"
    ~evm_version:"london"

(** The info for the "block_hash_gen.sol" contract. *)
let block_hash_gen () =
  compile_contract
    ~source:(solidity_contracts_path ^ "/block_hash_gen.sol")
    ~label:"block_hash_gen"
    ~contract:"BlockHashGen"
    ~evm_version:"london"

(** The info for the "block_hash_gen.sol" contract. *)
let blockhash () =
  compile_contract
    ~source:(solidity_contracts_path ^ "/blockhash.sol")
    ~label:"blockhash"
    ~contract:"Blockhash"
    ~evm_version:"london"

(** The info for the "timestamp.sol" contract. *)
let timestamp () =
  compile_contract
    ~source:(solidity_contracts_path ^ "/timestamp.sol")
    ~label:"timestamp"
    ~contract:"Timestamp"
    ~evm_version:"london"

(** The info for the "call_selfdestruct.sol" contract. *)
let call_selfdestruct () =
  compile_contract
    ~source:(solidity_contracts_path ^ "/call_selfdestruct.sol")
    ~label:"call_selfdestruct"
    ~contract:"C1"
    ~evm_version:"london"

(** The info for the "recursive.sol" contract. *)
let recursive () =
  compile_contract
    ~source:(solidity_contracts_path ^ "/recursive.sol")
    ~label:"recursive"
    ~contract:"Recursive"
    ~evm_version:"london"

(** The info for the "error.sol" contract. *)
let error () =
  compile_contract
    ~source:(solidity_contracts_path ^ "/error.sol")
    ~label:"error"
    ~contract:"Error"
    ~evm_version:"london"

(** The info for the "block_hash_gen.sol" contract. *)
let spam_withdrawal () =
  compile_contract
    ~source:(solidity_contracts_path ^ "/spam_withdrawal.sol")
    ~label:"spam_withdrawal"
    ~contract:"SpamWithdrawals"
    ~evm_version:"london"

(** The info for the "gas_limit.sol" contract. *)
let gas_limit_contract () =
  compile_contract
    ~source:(solidity_contracts_path ^ "/gas_limit.sol")
    ~label:"gas_limit"
    ~contract:"Gas_limit"
    ~evm_version:"london"

(** The info for the "gas_limit.sol" contract. *)
let counter () =
  compile_contract
    ~source:(solidity_contracts_path ^ "/counter.sol")
    ~label:"counter"
    ~contract:"TestCounter"
    ~evm_version:"london"

(** The info for the "coinbase.sol" contract. *)
let coinbase () =
  compile_contract
    ~source:(solidity_contracts_path ^ "/coinbase.sol")
    ~label:"coinbase"
    ~contract:"Coinbase"
    ~evm_version:"london"

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

let gas_left () =
  compile_contract
    ~source:(solidity_contracts_path ^ "/gas_left.sol")
    ~label:"gas_left"
    ~contract:"GasLeft"
    ~evm_version:"shanghai"

let call_types () =
  compile_contract
    ~source:(solidity_contracts_path ^ "/call_types.sol")
    ~label:"call_types"
    ~contract:"TestCallTypes"
    ~evm_version:"shanghai"

let simple_logger () =
  compile_contract
    ~source:(solidity_contracts_path ^ "/simple_logger.sol")
    ~label:"simple_logger"
    ~contract:"SimpleLogger"
    ~evm_version:"shanghai"

let call_tracer_depth () =
  compile_contract
    ~source:(solidity_contracts_path ^ "/call_tracer_depth.sol")
    ~label:"call_tracer_depth"
    ~contract:"TestDepthCall"
    ~evm_version:"shanghai"

let call_tracer_revert () =
  compile_contract
    ~source:(solidity_contracts_path ^ "/call_tracer_revert.sol")
    ~label:"call_tracer_revert"
    ~contract:"ErrorContract"
    ~evm_version:"shanghai"
