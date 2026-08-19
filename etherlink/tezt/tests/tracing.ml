(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Trilitech <contact@trili.tech>                         *)
(* Copyright (c) 2024-2026 Functori <contact@functori.com>                   *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Smart Optimistic Rollups: Etherlink Sequencer
   Requirement:  make -f etherlink.mk build
                 npm install eth-cli solc@0.8.31
                 # Install cast or foundry (see: https://book.getfoundry.sh/getting-started/installation)
                 curl -L https://foundry.paradigm.xyz | bash
                 foundryup
                 make fa-bridge-watchtower
                 ./scripts/install_dal_trusted_setup.sh
                 # Install websocat (see: https://github.com/vi/websocat?tab=readme-ov-file#installation)
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file tracing.ml
   Subject:      Transaction and block tracing (structLogger and the trace RPCs).
*)

open Rpc.Syntax
open Transaction
open Test_helpers
open Setup
module Protocol = Test_helpers.Protocol_no_direct_register

let test_replay_rpc =
  register_all
    ~__FILE__
    ~tags:["evm"; "rpc"; "replay"]
    ~title:"Sequencer can replay a block"
    ~time_between_blocks:Nothing
  @@ fun {sc_rollup_node; sequencer; client; _} _protocol ->
  (* Transfer funds to a random address. *)
  let address = "0xB7A97043983f24991398E5a82f63F4C58a417185" in
  let* transaction_hash =
    send_transaction_to_sequencer
      (Eth_cli.transaction_send
         ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
         ~to_public_key:address
         ~value:(Wei.of_eth_int 10)
         ~endpoint:(Evm_node.endpoint sequencer))
      sequencer
  in
  let*@! {Transaction.blockNumber; _} =
    Rpc.get_transaction_by_hash ~transaction_hash sequencer
  in
  (* Block few levels to ensure we are replaying on an old block. *)
  let* () = repeat 2 (fun () -> next_evm_level ~evm_node:sequencer) in
  let* () = bake_until_sync ~sequencer ~sc_rollup_node ~client () in
  let blockNumber =
    match blockNumber with
    | Some blockNumber -> blockNumber
    | None -> Test.fail "Missing block number"
  in
  let*@ original_block =
    Rpc.get_block_by_number
      ~full_tx_objects:false
      ~block:(Int32.to_string blockNumber)
      sequencer
  in
  let*@ replayed_block =
    Rpc.replay_block (Int32.to_int blockNumber) sequencer
  in
  (* Checks the block hash is the same. If so, we can assume they have the same
     state hash and transactions. *)
  Check.(
    (original_block.hash = replayed_block.hash)
      string
      ~error_msg:"Replayed block hash is %R, but the original block has %L") ;
  unit

let test_trace_transaction =
  let check_trace_result ~sequencer:trace_result ~rpc:trace_result_rpc =
    match (trace_result, trace_result_rpc) with
    | Ok t, Ok t' -> assert (JSON.equal t t')
    | Error _, _ ->
        Test.fail "Trace transaction shouldn't have failed (sequencer)"
    | _, Error _ -> Test.fail "Trace transaction shouldn't have failed (rpc)"
  in
  register_all
    ~__FILE__
    ~kernels:Kernel.etherlink_all
    ~tags:["evm"; "rpc"; "run"; "trace"]
    ~title:"Sequencer can run debug_traceTransaction"
    ~time_between_blocks:Nothing
  @@ fun {sequencer; _} _protocol ->
  (* Start a RPC node, as we also want to test that the sequencer and its RPC
     node return the same thing. *)
  let* rpc_node = run_new_rpc_endpoint sequencer in
  (* Transfer funds to a random address. *)
  let address = "0xB7A97043983f24991398E5a82f63F4C58a417185" in
  let* transaction_hash =
    send_transaction_to_sequencer
      (Eth_cli.transaction_send
         ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
         ~to_public_key:address
         ~value:(Wei.of_eth_int 10)
         ~endpoint:(Evm_node.endpoint sequencer))
      sequencer
  in
  let* _ = produce_block sequencer in
  (* Check tracing without options works *)
  let* trace_result = Rpc.trace_transaction ~transaction_hash sequencer in
  let* trace_result_rpc = Rpc.trace_transaction ~transaction_hash rpc_node in
  check_trace_result ~sequencer:trace_result ~rpc:trace_result_rpc ;
  (* Check tracing with a tracer without config *)
  let* trace_result =
    Rpc.trace_transaction ~transaction_hash ~tracer:"structLogger" sequencer
  in
  (match trace_result with
  | Ok _ -> ()
  | Error _ -> Test.fail "Trace transaction shouldn't have failed") ;
  (* Check tracing with a tracer and a config *)
  let* trace_result =
    Rpc.trace_transaction
      ~transaction_hash
      ~tracer:"structLogger"
      ~tracer_config:[("enableMemory", `Bool true)]
      sequencer
  in
  let* trace_result_rpc =
    Rpc.trace_transaction
      ~transaction_hash
      ~tracer:"structLogger"
      ~tracer_config:[("enableMemory", `Bool true)]
      rpc_node
  in
  check_trace_result ~sequencer:trace_result ~rpc:trace_result_rpc ;
  (* Check tracing without a tracer and a config *)
  let* trace_result =
    Rpc.trace_transaction
      ~transaction_hash
      ~tracer_config:
        [
          ("enableMemory", `Bool true);
          ("enableReturnData", `Bool true);
          ("disableStorage", `Bool true);
          ("disableStack", `Bool true);
        ]
      sequencer
  in
  let* trace_result_rpc =
    Rpc.trace_transaction
      ~transaction_hash
      ~tracer_config:
        [
          ("enableMemory", `Bool true);
          ("enableReturnData", `Bool true);
          ("disableStorage", `Bool true);
          ("disableStack", `Bool true);
        ]
      rpc_node
  in
  check_trace_result ~sequencer:trace_result ~rpc:trace_result_rpc ;
  unit

let test_trace_transaction_on_invalid_transaction =
  register_all
    ~__FILE__
    ~kernels:Kernel.etherlink_all
    ~tags:["evm"; "rpc"; "trace"; "fail"]
    ~title:"debug_traceTransaction fails on invalid transactions"
    ~time_between_blocks:Nothing
  @@ fun {sequencer; _} _protocol ->
  let* _ = produce_block sequencer in
  (* Check tracing without options works *)
  let* trace_result =
    Rpc.trace_transaction
      ~transaction_hash:("0x" ^ String.make 64 'f')
      sequencer
  in
  (match trace_result with
  | Ok _ -> Test.fail "Trace transaction should have failed"
  | Error {message; _} ->
      Check.(
        (message =~ rex "not found")
          ~error_msg:"traceTransaction failed with the wrong error")) ;
  unit

(* Checks that each opcode log are either all empty or non empty, considering
   the configuration. *)
let check_struct_logs expect_null log =
  let check_field field =
    if expect_null then
      Check.(
        (JSON.is_null log = false)
          bool
          ~error_msg:
            (Format.sprintf
               "Field %s was expected to be null, but got %%L instead"
               field))
    else
      Check.(
        (JSON.is_null log = false)
          bool
          ~error_msg:
            (Format.sprintf "Field %s wasn't expected to be null" field))
  in
  check_field "memory" ;
  check_field "storage" ;
  check_field "memSize" ;
  check_field "stack" ;
  check_field "returnData"

let check_trace expect_null expected_returned_value receipt trace =
  let failed = JSON.(trace |-> "failed" |> as_bool) in
  let gas_used = JSON.(trace |-> "gas" |> as_int64) in
  let returned_value =
    JSON.(trace |-> "returnValue" |> as_string |> Durable_storage_path.no_0x)
  in
  let logs = JSON.(trace |-> "structLogs" |> as_list) in
  Check.(
    (failed <> receipt.Transaction.status)
      bool
      ~error_msg:"The trace has a different status than in the receipt") ;
  Check.(
    (gas_used = receipt.gasUsed)
      int64
      ~error_msg:"Trace reported %L gas used, but the trace reported %R") ;

  (* Whether we don't expect a value and we get "0x", or we expect a value and
     it is encoded into a H256. *)
  (match expected_returned_value with
  | Some value ->
      let expected_value = hex_256_of_int value in
      Check.(
        (returned_value = expected_value)
          string
          ~error_msg:
            "The transaction returned the value %L, but %R was expected")
  | None ->
      Check.(
        (returned_value = "")
          string
          ~error_msg:"The transaction shouldn't return a value, but returned %L")) ;

  (* Checks the logs are consistent with the configuration (its an all in or
     all out). *)
  Check.((logs <> []) (list json) ~error_msg:"Logs shouldn't be empty") ;
  List.iter
    (check_struct_logs expect_null)
    JSON.(trace |-> "structLogs" |> as_list) ;
  unit

let test_trace_transaction_call =
  register_all
    ~__FILE__
    ~kernels:Kernel.etherlink_all
    ~tags:["evm"; "rpc"; "trace"; "call"]
    ~title:"Sequencer can run debug_traceTransaction and return a valid log"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  (* Transfer funds to a random address. *)
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* simple_storage_resolved =
    Solidity_contracts.simple_storage evm_version
  in

  (* deploy contract *)
  let* () =
    Eth_cli.add_abi
      ~label:simple_storage_resolved.label
      ~abi:simple_storage_resolved.abi
      ()
  in
  let* contract_address, _tx_deployment =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:sender.Eth_account.private_key
         ~endpoint
         ~abi:simple_storage_resolved.label
         ~bin:simple_storage_resolved.bin)
      sequencer
  in
  let* _ = produce_block sequencer in
  (* We will first trace with every options enabled, and check that we have the
     logs as complete as possible. We call the function `set` from the contract,
     which isn't expected to fail and doesn't return any result. *)
  let value_in_storage = 10 in
  let* transaction_hash =
    send_transaction_to_sequencer
      (Eth_cli.contract_send
         ~source_private_key:sender.private_key
         ~endpoint
         ~abi_label:simple_storage_resolved.label
         ~address:contract_address
         ~method_call:(Format.sprintf "set(%d)" value_in_storage))
      sequencer
  in
  let* _ = produce_block sequencer in
  (* We will use the receipt to check that the results from the trace are
     consistent with the result from the transaction once applied in the
     block. *)
  let*@ transaction_receipt =
    Rpc.get_transaction_receipt ~tx_hash:transaction_hash sequencer
  in
  let transaction_receipt = Option.get transaction_receipt in
  let* trace_result =
    Rpc.trace_transaction
      ~transaction_hash
      ~tracer_config:
        [("enableMemory", `Bool true); ("enableReturnData", `Bool true)]
      sequencer
  in
  let* () =
    match trace_result with
    | Ok trace -> check_trace false None transaction_receipt trace
    | Error _ -> Test.fail "Trace transaction shouldn't have failed"
  in
  (* The second test will disable every tracing options and call `get`, which
     returns a value, so that we can check that it returns the correct value in
     the end. *)
  let* transaction_hash =
    send_transaction_to_sequencer
      (Eth_cli.contract_send
         ~source_private_key:sender.private_key
         ~endpoint
         ~abi_label:simple_storage_resolved.label
         ~address:contract_address
         ~method_call:"get()")
      sequencer
  in
  let* _ = produce_block sequencer in
  let*@ transaction_receipt =
    Rpc.get_transaction_receipt ~tx_hash:transaction_hash sequencer
  in
  let transaction_receipt = Option.get transaction_receipt in
  let* trace_result =
    Rpc.trace_transaction
      ~transaction_hash
      ~tracer_config:
        [("disableStack", `Bool true); ("disableStorage", `Bool true)]
      sequencer
  in
  match trace_result with
  | Ok trace ->
      check_trace true (Some value_in_storage) transaction_receipt trace
  | Error _ -> Test.fail "Trace transaction shouldn't have failed"

let test_trace_call =
  register_all
    ~__FILE__
    ~kernels:Kernel.etherlink_all
    ~tags:["evm"; "rpc"; "trace"; "call"]
    ~title:"Sequencer can run debug_traceCall and return a valid log"
    ~da_fee:Wei.zero
  @@ fun {sequencer; evm_version; _} _protocol ->
  (* Start a RPC node as well, since we will want to check it returns the
     same result as the sequencer *)
  let* rpc_node = run_new_rpc_endpoint sequencer in
  (* Transfer funds to a random address. *)
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* simple_storage_resolved =
    Solidity_contracts.simple_storage evm_version
  in

  (* deploy contract *)
  let* () =
    Eth_cli.add_abi
      ~label:simple_storage_resolved.label
      ~abi:simple_storage_resolved.abi
      ()
  in
  let* contract_address, _tx_deployment =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:sender.Eth_account.private_key
         ~endpoint
         ~abi:simple_storage_resolved.label
         ~bin:simple_storage_resolved.bin)
      sequencer
  in
  let* () = repeat 2 (fun () -> next_evm_level ~evm_node:sequencer) in

  let value_in_storage = 10 in
  let* _ =
    send_transaction_to_sequencer
      (Eth_cli.contract_send
         ~source_private_key:sender.private_key
         ~endpoint
         ~abi_label:simple_storage_resolved.label
         ~address:contract_address
         ~method_call:(Format.sprintf "set(%d)" value_in_storage))
      sequencer
  in
  let* () = repeat 2 (fun () -> next_evm_level ~evm_node:sequencer) in

  let* abi_string =
    Eth_cli.encode_method
      ~abi_label:simple_storage_resolved.label
      ~method_:"get()"
      ()
  in

  let*@ trace =
    Rpc.trace_call
      ~block:Latest
      ~to_:contract_address
      ~data:abi_string
      ~tracer_config:
        [("enableMemory", `Bool true); ("enableReturnData", `Bool true)]
      sequencer
  in
  let*@ trace_rpc =
    Rpc.trace_call
      ~block:Latest
      ~to_:contract_address
      ~data:abi_string
      ~tracer_config:
        [("enableMemory", `Bool true); ("enableReturnData", `Bool true)]
      rpc_node
  in

  assert (JSON.equal trace trace_rpc) ;

  let logs = JSON.(trace |-> "structLogs" |> as_list) in
  let returned_value =
    JSON.(trace |-> "returnValue" |> as_string |> Durable_storage_path.no_0x)
  in
  Check.((logs <> []) (list json) ~error_msg:"Logs shouldn't be empty") ;
  Check.(
    (returned_value
   = "000000000000000000000000000000000000000000000000000000000000000a")
      string
      ~error_msg:"Expect return value to be %R, got %L") ;
  List.iter (check_struct_logs false) logs ;

  let*@ trace_failed_transaction =
    Rpc.trace_call
      ~block:(Number 0)
      ~to_:contract_address
      ~data:abi_string
      ~tracer_config:
        [("enableMemory", `Bool true); ("enableReturnData", `Bool true)]
      sequencer
  in
  Check.(
    JSON.(
      trace_failed_transaction |-> "returnValue" |> as_string
      |> Durable_storage_path.no_0x = "")
      string
      ~error_msg:"Expect return value to be empty, got %L") ;

  unit

let test_trace_empty_block =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "trace"; "block"; "empty"]
    ~title:"debug_traceBlockByNumber succeeds on empty block"
    ~kernels:Kernel.etherlink_all
  @@ fun {client; sc_rollup_node; sequencer; _} _protocol ->
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
  let*@ trace_result = Rpc.trace_block ~block:Rpc.Latest sequencer in
  Check.(
    (trace_result = [])
      (list json)
      ~error_msg:"Wrong trace, expected %R but got %L") ;
  unit

let test_trace_block_struct_logger =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "trace"; "block"; "empty"; "struct_logger"]
    ~title:"debug_traceBlockByNumber not implemented for struct logger"
    ~kernels:Kernel.etherlink_all
  @@ fun {client; sc_rollup_node; sequencer; _} _protocol ->
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
  let* trace_result =
    Rpc.trace_block ~tracer:"structLogger" ~block:Rpc.Latest sequencer
  in
  match trace_result with
  | Ok _ -> Test.fail "should have failed, structLogger not implemented"
  | Error {data; code; _} ->
      Check.((code = -32002) int ~error_msg:"Expected %R but got %L") ;
      Check.(
        (data = Some "structLogger")
          (option string)
          ~error_msg:"Error should be the name of the tracer") ;
      unit

let test_trace_block =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "trace"; "block"]
    ~title:"debug_traceBlockByNumber succeeds on non empty block"
    ~kernels:Kernel.etherlink_all
  @@ fun {client; sc_rollup_node; sequencer; evm_version; _} _protocol ->
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
  let endpoint = Evm_node.endpoint sequencer in
  let sender_0 = Eth_account.bootstrap_accounts.(0) in
  let sender_1 = Eth_account.bootstrap_accounts.(1) in
  let* call_types = Solidity_contracts.call_types evm_version in
  let* () = Eth_cli.add_abi ~label:call_types.label ~abi:call_types.abi () in
  let* address, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:sender_0.Eth_account.private_key
         ~endpoint
         ~abi:call_types.label
         ~bin:call_types.bin)
      sequencer
  in
  let* raw_tx_0 =
    Cast.craft_tx
      ~chain_id:1337
      ~source_private_key:sender_0.private_key
      ~nonce:1
      ~gas_price:1_000_000_000
      ~gas:300_000
      ~value:Wei.zero
      ~address
      ~signature:"testProduceOpcodes()"
      ()
  in
  let* raw_tx_1 =
    Cast.craft_tx
      ~chain_id:1337
      ~source_private_key:sender_1.private_key
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:300_000
      ~value:Wei.one
      ~address:sender_0.address
      ()
  in
  let*@ transaction_hash_0 =
    Rpc.send_raw_transaction ~raw_tx:raw_tx_0 sequencer
  in
  let*@ transaction_hash_1 =
    Rpc.send_raw_transaction ~raw_tx:raw_tx_1 sequencer
  in
  let*@ size = produce_block sequencer in
  Check.((size = 2) int)
    ~error_msg:"Expected 2 transactions in the block, got %L" ;
  let*@ trace_result = Rpc.trace_block ~block:Rpc.Latest sequencer in
  Check.(
    (List.length trace_result = 2)
      int
      ~error_msg:"Wrong nb of traces, expected %R but got %L") ;
  let t0 = List.nth trace_result 0 in
  let t1 = List.nth trace_result 1 in
  (* first tx is a contract call with internal calls *)
  Check.(
    JSON.(t0 |-> "txHash" |> as_string = transaction_hash_0)
      string
      ~error_msg:"Wrong hash, expected %R but got %L") ;
  Check.(
    JSON.(t0 |-> "result" |-> "calls" |> as_list <> [])
      (list json)
      ~error_msg:"Should have a list of subcalls") ;
  (* second tx is a simple transfer *)
  Check.(
    JSON.(t1 |-> "txHash" |> as_string = transaction_hash_1)
      string
      ~error_msg:"Wrong hash, expected %R but got %L") ;
  Check.(
    JSON.(t1 |-> "result" |-> "calls" |> as_list = [])
      (list json)
      ~error_msg:"Should not have a list of subcalls") ;
  unit

let test_trace_block_txs_same_caller =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "trace"; "block"]
    ~title:
      "debug_traceBlockByNumber succeeds on block with transactions using same \
       caller"
    ~kernels:Kernel.etherlink_all
  @@ fun {client; sc_rollup_node; sequencer; evm_version; _} _protocol ->
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
  let endpoint = Evm_node.endpoint sequencer in
  let sender_0 = Eth_account.bootstrap_accounts.(0) in
  let* call_types = Solidity_contracts.call_types evm_version in
  let* () = Eth_cli.add_abi ~label:call_types.label ~abi:call_types.abi () in
  let* address, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:sender_0.Eth_account.private_key
         ~endpoint
         ~abi:call_types.label
         ~bin:call_types.bin)
      sequencer
  in
  let* raw_tx_0 =
    Cast.craft_tx
      ~chain_id:1337
      ~source_private_key:sender_0.private_key
      ~nonce:1
      ~gas_price:1_000_000_000
      ~gas:300_000
      ~value:Wei.zero
      ~address
      ~signature:"testProduceOpcodes()"
      ()
  in
  let* raw_tx_1 =
    Cast.craft_tx
      ~chain_id:1337
      ~source_private_key:sender_0.private_key
      ~nonce:2
      ~gas_price:1_000_000_000
      ~gas:300_000
      ~value:Wei.one
      ~address:sender_0.address
      ()
  in
  let*@ transaction_hash_0 =
    Rpc.send_raw_transaction ~raw_tx:raw_tx_0 sequencer
  in
  let*@ transaction_hash_1 =
    Rpc.send_raw_transaction ~raw_tx:raw_tx_1 sequencer
  in
  let*@ size = produce_block sequencer in
  Check.((size = 2) int)
    ~error_msg:"Expected 2 transactions in the block, got %L" ;
  let*@ trace_result = Rpc.trace_block ~block:Rpc.Latest sequencer in
  Check.(
    (List.length trace_result = 2)
      int
      ~error_msg:"Wrong nb of traces, expected %R but got %L") ;
  let t0 = List.nth trace_result 0 in
  let t1 = List.nth trace_result 1 in
  (* first tx is a contract call with internal calls *)
  Check.(
    JSON.(t0 |-> "txHash" |> as_string = transaction_hash_0)
      string
      ~error_msg:"Wrong hash, expected %R but got %L") ;
  Check.(
    JSON.(t0 |-> "result" |-> "calls" |> as_list <> [])
      (list json)
      ~error_msg:"Should have a list of subcalls") ;
  (* second tx is a simple transfer *)
  Check.(
    JSON.(t1 |-> "txHash" |> as_string = transaction_hash_1)
      string
      ~error_msg:"Wrong hash, expected %R but got %L") ;
  Check.(
    JSON.(t1 |-> "result" |-> "calls" |> as_list = [])
      (list json)
      ~error_msg:"Should not have a list of subcalls") ;
  unit

let protocols = [Protocol.Alpha]

let () =
  test_replay_rpc protocols ;
  test_trace_transaction protocols ;
  test_trace_transaction_on_invalid_transaction protocols ;
  test_trace_transaction_call protocols ;
  test_trace_call protocols ;
  test_trace_empty_block protocols ;
  test_trace_block protocols ;
  test_trace_block_txs_same_caller protocols ;
  test_trace_block_struct_logger protocols
