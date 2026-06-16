(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Rpc.Syntax
open Test_helpers

let send_and_get_hash sequencer =
  let sender = Eth_account.bootstrap_accounts.(0) in
  let receiver = Eth_account.bootstrap_accounts.(1) in
  send_transaction_to_sequencer
    (Eth_cli.transaction_send
       ~source_private_key:sender.private_key
       ~to_public_key:receiver.address
       ~value:(Wei.of_eth_int 1)
       ~endpoint:(Evm_node.endpoint sequencer))
    sequencer

let test_trace_block_calltracer () =
  register_sandbox
    ~__FILE__
    ~tags:["evm"; "trace"; "cli"; "calltracer"]
    ~title:"Trace block CLI command outputs same result as RPC"
  @@ fun sequencer ->
  let* transaction_hash = send_and_get_hash sequencer in
  let*@! transaction_object =
    Rpc.get_transaction_by_hash ~transaction_hash sequencer
  in
  let block_number =
    match transaction_object.blockNumber with
    | Some n -> Int32.to_int n
    | None -> Test.fail "Transaction has no block number"
  in
  let*@ expected = Rpc.trace_block ~block:(Number block_number) sequencer in
  let*! cli_result = Evm_node.trace_block sequencer block_number in
  Check.((List.length cli_result = List.length expected) int)
    ~error_msg:"Expected %R trace results but got %L" ;
  List.iter2
    (fun cli_item rpc_item ->
      Check.is_true
        (JSON.equal cli_item rpc_item)
        ~error_msg:"CLI trace output differs from RPC trace output")
    cli_result
    expected ;
  unit

let test_trace_transaction_calltracer () =
  register_sandbox
    ~__FILE__
    ~tags:["evm"; "trace"; "cli"; "calltracer"; "transaction"]
    ~title:
      "Trace transaction CLI command outputs same result as RPC (callTracer)"
  @@ fun sequencer ->
  let* transaction_hash = send_and_get_hash sequencer in
  let*@ expected =
    Rpc.trace_transaction ~transaction_hash ~tracer:"callTracer" sequencer
  in
  let*! cli_result = Evm_node.trace_transaction sequencer transaction_hash in
  Check.is_true
    (JSON.equal cli_result expected)
    ~error_msg:"CLI trace output differs from RPC trace output" ;
  unit

let test_trace_transaction_structlogger () =
  register_sandbox
    ~__FILE__
    ~tags:["evm"; "trace"; "cli"; "structlogger"; "transaction"]
    ~title:
      "Trace transaction CLI command outputs same result as RPC (structLogger)"
  @@ fun sequencer ->
  let* transaction_hash = send_and_get_hash sequencer in
  let*@ expected =
    Rpc.trace_transaction ~transaction_hash ~tracer:"structLogger" sequencer
  in
  let*! cli_result =
    Evm_node.trace_transaction ~tracer:"structLogger" sequencer transaction_hash
  in
  Check.is_true
    (JSON.equal cli_result expected)
    ~error_msg:"CLI trace output differs from RPC trace output" ;
  unit

let () =
  test_trace_block_calltracer () ;
  test_trace_transaction_calltracer () ;
  test_trace_transaction_structlogger ()
