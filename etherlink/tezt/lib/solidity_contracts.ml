(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023-2024 TriliTech <contact@trili.tech>                    *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2023-2026 Functori <contact@functori.com>                   *)
(*                                                                           *)
(*****************************************************************************)

open Test_helpers

type contract = {
  label : string;
  abi : string;
  bin : string;
  deployed_bin : string;
}

let solidity_contracts_path = "etherlink/kernel_latest/solidity_examples"

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
          "evm.bytecode.object",
          "evm.deployedBytecode.object"
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

let compile_contract ~source ~label ~contract evm_version =
  (* Construct the JSON input for solc *)
  let evm_version = Evm_version.to_solidity_evm_version evm_version in
  let input_json =
    generate_json_string ~label ~contract ~path:source ~evm_version
  in
  let command, args =
    if Option.is_some (Sys.getenv_opt "TEZT_NO_NPX") then
      ("/usr/local/lib/node_modules/solc/solc.js", ["--standard-json"])
    else ("npx", ["--yes"; "solc"; "--standard-json"])
  in

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
  let deployed_bin =
    JSON.(
      json |-> "contracts" |-> label |-> contract |-> "evm"
      |-> "deployedBytecode" |-> "object" |> as_string)
  in

  let abi_file = Tezt.Temp.file (label ^ ".abi") in
  let bin_file = Tezt.Temp.file (label ^ ".bin") in
  let deployed_file = Tezt.Temp.file (label ^ ".deployed.bin") in

  JSON.encode_to_file abi_file abi ;
  Tezt.Base.write_file bin_file ~contents:bin ;
  Tezt.Base.write_file deployed_file ~contents:deployed_bin ;
  return {label; abi = abi_file; bin = bin_file; deployed_bin = deployed_file}

(** The info for the "storage.sol" contract. *)
let simple_storage =
  compile_contract
    ~source:(solidity_contracts_path ^ "/storage.sol")
    ~label:"simpleStorage"
    ~contract:"SimpleStorage"

(* The info for the "even_block_gas_consumer.sol" contract. *)
let even_block_gas_consumer =
  compile_contract
    ~source:(solidity_contracts_path ^ "/even_block_gas_consumer.sol")
    ~label:"even_block_gas_consumer"
    ~contract:"EvenBlockGasConsumer"

(** The info for the "erc20tok.sol" contract. *)
let erc20 =
  compile_contract
    ~source:(solidity_contracts_path ^ "/erc20tok.sol")
    ~label:"erc20tok"
    ~contract:"ERC20"

(** The info for the "loop.sol" contract. *)
let loop =
  compile_contract
    ~source:(solidity_contracts_path ^ "/loop.sol")
    ~label:"loop"
    ~contract:"Loop"

(** The info for the "mapping_storage.sol" contract. *)
let mapping_storage =
  compile_contract
    ~source:(solidity_contracts_path ^ "/mapping_storage.sol")
    ~label:"mapping_storage"
    ~contract:"MappingStorage"

(** The info for the "storage.sol" contract, compiled for Shanghai. *)
let shanghai_storage =
  compile_contract
    ~source:(solidity_contracts_path ^ "/storage.sol")
    ~label:"shanghai_storage"
    ~contract:"SimpleStorage"

(** The info for the Callee contract. *)
let callee =
  compile_contract
    ~source:(solidity_contracts_path ^ "/caller_callee.sol")
    ~label:"callee"
    ~contract:"Callee"

(** The info for the Caller contract. *)
let caller =
  compile_contract
    ~source:(solidity_contracts_path ^ "/caller_callee.sol")
    ~label:"caller"
    ~contract:"Caller"

(** The info for the "events.sol" contract. *)
let events =
  compile_contract
    ~source:(solidity_contracts_path ^ "/events.sol")
    ~label:"events"
    ~contract:"Events"

(** The info for the "nested_create.sol" contract. *)
let nested_create =
  compile_contract
    ~source:(solidity_contracts_path ^ "/nested_create.sol")
    ~label:"nested_create"
    ~contract:"Creator"

(** The info for the "revert.sol" contract. *)
let revert =
  compile_contract
    ~source:(solidity_contracts_path ^ "/revert.sol")
    ~label:"revert"
    ~contract:"Revert"

(** The info for the "create2.sol" contract. *)
let create2 =
  compile_contract
    ~source:(solidity_contracts_path ^ "/create2.sol")
    ~label:"create2"
    ~contract:"Create2"

(** The info for the "oog_call.sol" contract. *)
let oog_call =
  compile_contract
    ~source:(solidity_contracts_path ^ "/oog_call.sol")
    ~label:"oog_call"
    ~contract:"OOGCall"

(** The info for the "ether_wallet.sol" contract. *)
let ether_wallet =
  compile_contract
    ~source:(solidity_contracts_path ^ "/ether_wallet.sol")
    ~label:"ether_wallet"
    ~contract:"SharedEtherWallet"

(** The info for the "block_hash_gen.sol" contract. *)
let block_hash_gen =
  compile_contract
    ~source:(solidity_contracts_path ^ "/block_hash_gen.sol")
    ~label:"block_hash_gen"
    ~contract:"BlockHashGen"

(** The info for the "block_hash_gen.sol" contract. *)
let blockhash =
  compile_contract
    ~source:(solidity_contracts_path ^ "/blockhash.sol")
    ~label:"blockhash"
    ~contract:"Blockhash"

(** The info for the "timestamp.sol" contract. *)
let timestamp =
  compile_contract
    ~source:(solidity_contracts_path ^ "/timestamp.sol")
    ~label:"timestamp"
    ~contract:"Timestamp"

(** The info for the "call_selfdestruct.sol" contract. *)
let call_selfdestruct =
  compile_contract
    ~source:(solidity_contracts_path ^ "/call_selfdestruct.sol")
    ~label:"call_selfdestruct"
    ~contract:"C1"

(** The info for the "call_selfdestruct_behavior.sol" contract. *)
let call_selfdestruct_behavior =
  compile_contract
    ~source:(solidity_contracts_path ^ "/call_selfdestruct_behavior.sol")
    ~label:"call_selfdestruct"
    ~contract:"Parent"

(** The info for the "mcopy.sol" contract. *)
let mcopy =
  compile_contract
    ~source:(solidity_contracts_path ^ "/mcopy.sol")
    ~label:"mcopy"
    ~contract:"MCOPY"

(** The info for the "transient_storage_multiplier.sol" contract. *)
let transient_storage_multiplier =
  compile_contract
    ~source:(solidity_contracts_path ^ "/transient_storage_multiplier.sol")
    ~label:"transient_storage_multiplier"
    ~contract:"TransientStorageMultiplier"

(** The info for the "recursive.sol" contract. *)
let recursive =
  compile_contract
    ~source:(solidity_contracts_path ^ "/recursive.sol")
    ~label:"recursive"
    ~contract:"Recursive"

(** The info for the "error.sol" contract. *)
let error =
  compile_contract
    ~source:(solidity_contracts_path ^ "/error.sol")
    ~label:"error"
    ~contract:"Error"

(** The info for the "spam_withdrawal.sol" contract. *)
let spam_withdrawal =
  compile_contract
    ~source:(solidity_contracts_path ^ "/spam_withdrawal.sol")
    ~label:"spam_withdrawal"
    ~contract:"SpamWithdrawals"

(** The info for the "gas_limit.sol" contract. *)
let gas_limit_contract =
  compile_contract
    ~source:(solidity_contracts_path ^ "/gas_limit.sol")
    ~label:"gas_limit"
    ~contract:"Gas_limit"

(** The info for the "gas_limit.sol" contract. *)
let counter =
  compile_contract
    ~source:(solidity_contracts_path ^ "/counter.sol")
    ~label:"counter"
    ~contract:"TestCounter"

let dummy_proxy =
  compile_contract
    ~source:(solidity_contracts_path ^ "/dummy_proxy.sol")
    ~label:"dummyproxy"
    ~contract:"DummyProxy"

(** The info for the "coinbase.sol" contract. *)
let coinbase =
  compile_contract
    ~source:(solidity_contracts_path ^ "/coinbase.sol")
    ~label:"coinbase"
    ~contract:"Coinbase"

(** The info for the "block_constants.sol" contract.
    See [etherlink/kernel_latest/solidity_examples/block_constants.sol] *)
let block_constants =
  compile_contract
    ~source:(solidity_contracts_path ^ "/block_constants.sol")
    ~label:"block_constants"
    ~contract:"BlockConstants"

(** The info for the "call_withdrawal.sol" contract.
    See [etherlink/kernel_latest/solidity_examples/call_withdrawal.sol] *)
let call_withdrawal =
  compile_contract
    ~source:(solidity_contracts_path ^ "/call_withdrawal.sol")
    ~label:"call_withdrawal"
    ~contract:"CallPrecompile"

(** The info for the "callcode_withdrawal.sol" contract.
    See [etherlink/kernel_latest/solidity_examples/callcode_withdrawal.sol]
    Deprecated Callcode withdrawal contract can't be compiled with the current solc version *)
let callcode_withdrawal =
  {
    label = "callcode_withdrawal";
    abi = kernel_inputs_path ^ "/callcode_withdrawal.abi";
    bin = kernel_inputs_path ^ "/callcode_withdrawal.bin";
    (* This field is unused as the deployment check is done directly within the called
       contract. *)
    deployed_bin = "";
  }

let gas_left =
  compile_contract
    ~source:(solidity_contracts_path ^ "/gas_left.sol")
    ~label:"gas_left"
    ~contract:"GasLeft"

let call_types =
  compile_contract
    ~source:(solidity_contracts_path ^ "/call_types.sol")
    ~label:"call_types"
    ~contract:"TestCallTypes"

let simple_logger =
  compile_contract
    ~source:(solidity_contracts_path ^ "/simple_logger.sol")
    ~label:"simple_logger"
    ~contract:"SimpleLogger"

let call_tracer_depth =
  compile_contract
    ~source:(solidity_contracts_path ^ "/call_tracer_depth.sol")
    ~label:"call_tracer_depth"
    ~contract:"TestDepthCall"

let call_tracer_revert =
  compile_contract
    ~source:(solidity_contracts_path ^ "/call_tracer_revert.sol")
    ~label:"call_tracer_revert"
    ~contract:"ErrorContract"

let call_revert =
  compile_contract
    ~source:(solidity_contracts_path ^ "/call_revert.sol")
    ~label:"call_revert"
    ~contract:"CallRevert"

let precompiles =
  compile_contract
    ~source:(solidity_contracts_path ^ "/precompiles.sol")
    ~label:"precompiles"
    ~contract:"PrecompileCaller"

let state_override_tester =
  compile_contract
    ~source:(solidity_contracts_path ^ "/state_override_tester.sol")
    ~label:"state_override_tester"
    ~contract:"StateOverrideTester"

let state_override_tester_readable =
  compile_contract
    ~source:(solidity_contracts_path ^ "/state_override_tester_readable.sol")
    ~label:"state_override_tester_readable"
    ~contract:"StateOverrideTester"

let batcher =
  compile_contract
    ~source:(solidity_contracts_path ^ "/batcher.sol")
    ~label:"batcher"
    ~contract:"Batcher"

let reentrancy_test =
  compile_contract
    ~source:"etherlink/kernel_latest/revm/contracts/tests/reentrancy_tester.sol"
    ~label:"reentrancy_tester"
    ~contract:"ReentrancyTester"

let slot_filler =
  compile_contract
    ~source:(solidity_contracts_path ^ "/slot_filler.sol")
    ~label:"slot_filler"
    ~contract:"SlotFiller"

let call_fast_withdrawal =
  compile_contract
    ~source:(solidity_contracts_path ^ "/call_fast_withdrawal.sol")
    ~label:"call_fast_withdrawal"
    ~contract:"CallPrecompile"

let delegatecall_delegator =
  compile_contract
    ~source:(solidity_contracts_path ^ "/delegatecall.sol")
    ~label:"delegatecall_delegator"
    ~contract:"Delegator"

let delegatecall_delegated =
  compile_contract
    ~source:(solidity_contracts_path ^ "/delegatecall.sol")
    ~label:"delegatecall_delegated"
    ~contract:"Delegated"

let etherlink_fa_proxy_mock =
  compile_contract
    ~source:(solidity_contracts_path ^ "/etherlink_fa_proxy_mock.sol")
    ~label:"proxy"
    ~contract:"Proxy"

let eip2930_storage_access =
  compile_contract
    ~source:(solidity_contracts_path ^ "/eip2930_storage_access.sol")
    ~label:"storageaccess"
    ~contract:"StorageAccess"

let eip7702 =
  compile_contract
    ~source:(solidity_contracts_path ^ "/eip7702.sol")
    ~label:"eip7702contract"
    ~contract:"EIP7702Contract"

let eip7702_fallback =
  compile_contract
    ~source:(solidity_contracts_path ^ "/eip7702_fallback.sol")
    ~label:"eip7702fallbackcontract"
    ~contract:"EIP7702FallbackContract"

let gateway_catch_revert =
  compile_contract
    ~source:(solidity_contracts_path ^ "/gateway_catch_revert.sol")
    ~label:"gateway_catch_revert"
    ~contract:"GatewayCatchRevert"

let nested_delegatecalls_A =
  compile_contract
    ~source:(solidity_contracts_path ^ "/nested_delegatecalls.sol")
    ~label:"nested_delegatecalls_A"
    ~contract:"A"

let nested_delegatecalls_B =
  compile_contract
    ~source:(solidity_contracts_path ^ "/nested_delegatecalls.sol")
    ~label:"nested_delegatecalls_B"
    ~contract:"B"

let nested_delegatecalls_C =
  compile_contract
    ~source:(solidity_contracts_path ^ "/nested_delegatecalls.sol")
    ~label:"nested_delegatecalls_C"
    ~contract:"C"

let nested_delegatecalls_D =
  compile_contract
    ~source:(solidity_contracts_path ^ "/nested_delegatecalls.sol")
    ~label:"nested_delegatecalls_D"
    ~contract:"D"

module Precompile = struct
  let xtz_bridge = "0xff00000000000000000000000000000000000001"

  let fa_bridge = "0xff00000000000000000000000000000000000002"

  let sequencer_key_change = "0xff00000000000000000000000000000000000006"
end
