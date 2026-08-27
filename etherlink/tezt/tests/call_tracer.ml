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
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file call_tracer.ml
   Subject:      The callTracer variant of debug_traceTransaction.
*)

open Rpc.Syntax
open Transaction
open Delayed_inbox
open Test_helpers
open Setup
module Protocol = Test_helpers.Protocol_no_direct_register

let test_trace_transaction_call_trace =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call_trace"]
    ~title:"Sequencer can run debug_traceTransaction with calltracer"
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
  let*@ trace_result =
    Rpc.trace_transaction
      ~tracer:"callTracer"
      ~transaction_hash
      ~tracer_config:[("withLog", `Bool true); ("onlyTopCall", `Bool true)]
      sequencer
  in
  let*@! {Transaction.input; gas; _} =
    Rpc.get_transaction_by_hash ~transaction_hash sequencer
  in
  Check.(
    (JSON.(trace_result |-> "calls" |> as_list) = [])
      (list json)
      ~error_msg:"Wrong calls, expected %R but got %L") ;
  Check.(
    (JSON.(trace_result |-> "type" |> as_string) = "CALL")
      string
      ~error_msg:"Wrong type, expected %R but got %L") ;
  Check.(
    (JSON.(trace_result |-> "from" |> as_string) = transaction_receipt.from)
      string
      ~error_msg:"Wrong from, expected %R but got %L") ;
  Check.(
    (JSON.(trace_result |-> "to" |> as_string)
    = Option.get transaction_receipt.to_)
      string
      ~error_msg:"Wrong to, expected %R but got %L") ;
  Check.(
    (JSON.(trace_result |-> "value" |> as_string) = "0x00")
      string
      ~error_msg:"Wrong value, expected %R but got %L") ;
  Check.(
    (JSON.(trace_result |-> "gasUsed" |> as_string)
    = Format.sprintf "0x%02x" (Int64.to_int transaction_receipt.gasUsed))
      string
      ~error_msg:"Wrong gasUsed, expected %R but got %L") ;
  Check.(
    (JSON.(trace_result |-> "output" |> as_string) = "0x")
      string
      ~error_msg:"Wrong output, expected %R but got %L") ;
  Check.(
    (JSON.(trace_result |-> "input" |> as_string)
    = Option.value ~default:"" input)
      string
      ~error_msg:"Wrong input, expected %R but got %L") ;
  Check.(
    (JSON.(trace_result |-> "gas" |> as_string)
    = Format.sprintf "0x%02x" @@ Int64.to_int gas)
      string
      ~error_msg:"Wrong gas, expected %R but got %L") ;
  unit

let test_trace_transaction_calltracer_failed_create =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call_trace"; "fail"]
    ~title:
      "debug_traceTransaction with calltracer returns an error when contract \
       creation reverts but is included"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* call_types = Solidity_contracts.call_types evm_version in
  let* () = Eth_cli.add_abi ~label:call_types.label ~abi:call_types.abi () in
  let* _contract_address, create_tx =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:sender.Eth_account.private_key
         ~endpoint
         ~abi:call_types.label
         ~bin:call_types.bin)
      sequencer
  in
  let* _ = produce_block sequencer in
  let*@! {Transaction.gas; _} =
    Rpc.get_transaction_by_hash ~transaction_hash:create_tx sequencer
  in
  let gas = Int64.to_int gas in
  let not_enough_gas = gas - (gas / 10) in
  let data = read_file call_types.bin in
  let* raw_tx =
    Cast.craft_deploy_tx
      ~chain_id:1337
      ~gas_price:1_000_000_000
      ~nonce:1
      ~source_private_key:sender.Eth_account.private_key
      ~gas:not_enough_gas
      ~data
      ()
  in
  let*@ tx_hash = Rpc.send_raw_transaction ~raw_tx sequencer in
  let*@ size = produce_block sequencer in
  Check.((size = 1) int)
    ~error_msg:"Expected %R transactions in the block, got %L" ;
  let*@ trace_result =
    Rpc.trace_transaction
      ~tracer:"callTracer"
      ~transaction_hash:tx_hash
      sequencer
  in
  Check.(
    (JSON.(trace_result |-> "error" |> as_string) = "OutOfGas")
      string
      ~error_msg:"Trace should report an error. Expected %R but got %L") ;
  unit

let test_trace_delegate_call =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call_trace"; "delegate_call"]
    ~title:"calltracer correctly display delegate call infos"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* delegated = Solidity_contracts.delegatecall_delegated evm_version in
  let* delegator = Solidity_contracts.delegatecall_delegator evm_version in
  let* () = Eth_cli.add_abi ~label:delegated.label ~abi:delegated.abi () in
  let* () = Eth_cli.add_abi ~label:delegator.label ~abi:delegator.abi () in
  let* delegated_address, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:sender.Eth_account.private_key
         ~endpoint
         ~abi:delegated.label
         ~bin:delegated.bin)
      sequencer
  in
  let* _ = produce_block sequencer in
  let* delegator_address, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:sender.Eth_account.private_key
         ~endpoint
         ~abi:delegator.label
         ~bin:delegator.bin)
      sequencer
  in
  let* _ = produce_block sequencer in
  let* tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.contract_send
         ~source_private_key:sender.private_key
         ~endpoint
         ~abi_label:delegator.label
         ~address:delegator_address
         ~method_call:(Format.sprintf "setVars(\"%s\",%d)" delegated_address 42))
      sequencer
  in
  let* _ = produce_block sequencer in
  let*@ top_call =
    Rpc.trace_transaction
      ~tracer:"callTracer"
      ~transaction_hash:tx_hash
      ~tracer_config:[("withLog", `Bool false); ("onlyTopCall", `Bool false)]
      sequencer
  in
  Check.(
    JSON.(
      top_call |-> "to" |> as_string = String.lowercase_ascii delegator_address)
      string
      ~error_msg:"Top call should have been traced as coming from %R but was %L") ;
  Check.(
    JSON.(top_call |-> "calls" |> as_list <> [])
      (list json)
      ~error_msg:"Top call should have some subcalls") ;
  let delegatecall = JSON.(top_call |-> "calls" |> as_list |> List.hd) in
  Check.(
    JSON.(
      delegatecall |-> "from" |> as_string
      = String.lowercase_ascii delegator_address)
      string
      ~error_msg:"Delegate call should be traced as coming from %R but was %L") ;
  Check.(
    JSON.(
      delegatecall |-> "to" |> as_string
      = String.lowercase_ascii delegated_address)
      string
      ~error_msg:
        "Delegate call should be traced as going to delegated contract %R but \
         was %L") ;
  unit

let test_trace_transaction_calltracer_all_types =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call_trace"; "types"]
    ~title:
      "debug_traceTransaction with calltracer can produce all the call types"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* call_types = Solidity_contracts.call_types evm_version in
  let* () = Eth_cli.add_abi ~label:call_types.label ~abi:call_types.abi () in
  let* contract_address, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:sender.Eth_account.private_key
         ~endpoint
         ~abi:call_types.label
         ~bin:call_types.bin)
      sequencer
  in
  let* _ = produce_block sequencer in
  let* tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.contract_send
         ~source_private_key:sender.private_key
         ~endpoint
         ~abi_label:call_types.label
         ~address:contract_address
         ~method_call:"testProduceOpcodes()")
      sequencer
  in
  let* _ = produce_block sequencer in
  let*@ trace_result =
    Rpc.trace_transaction
      ~tracer:"callTracer"
      ~transaction_hash:tx_hash
      ~tracer_config:[("withLog", `Bool false); ("onlyTopCall", `Bool false)]
      sequencer
  in
  let call_list = JSON.(trace_result |-> "calls" |> as_list) in
  (* Initial call is of type CALL *)
  Check.(
    (JSON.(trace_result |-> "type" |> as_string) = "CALL")
      string
      ~error_msg:"Wrong type, expected %R but got %L") ;
  (* The call list should be of size 6, we produce the following opcodes:
     CREATE / CREATE2 / CALL / DELEGATECALL / STATICCALL / CALLCODE *)
  Check.(
    (List.length call_list = 6)
      int
      ~error_msg:"Wrong call list size, expected %R but got %L") ;
  List.iteri
    (fun position call ->
      match position with
      | 0 ->
          Check.(
            (JSON.(call |-> "type" |> as_string) = "CREATE")
              string
              ~error_msg:"Wrong type, expected %R but got %L")
      | 1 ->
          Check.(
            (JSON.(call |-> "type" |> as_string) = "CREATE2")
              string
              ~error_msg:"Wrong type, expected %R but got %L")
      | 2 ->
          Check.(
            (JSON.(call |-> "type" |> as_string) = "CALL")
              string
              ~error_msg:"Wrong type, expected %R but got %L")
      | 3 ->
          Check.(
            (JSON.(call |-> "type" |> as_string) = "DELEGATECALL")
              string
              ~error_msg:"Wrong type, expected %R but got %L")
      | 4 ->
          Check.(
            (JSON.(call |-> "type" |> as_string) = "STATICCALL")
              string
              ~error_msg:"Wrong type, expected %R but got %L")
      | 5 ->
          Check.(
            (JSON.(call |-> "type" |> as_string) = "CALLCODE")
              string
              ~error_msg:"Wrong type, expected %R but got %L")
      | _ -> failwith "Impossible case, call list's size should be 6")
    call_list ;
  unit

let test_trace_transaction_calltracer_create_to_field =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call_trace"; "create"; "to"]
    ~title:
      "debug_traceTransaction with calltracer includes to field for \
       CREATE/CREATE2"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* call_types = Solidity_contracts.call_types evm_version in
  let* () = Eth_cli.add_abi ~label:call_types.label ~abi:call_types.abi () in
  let* contract_address, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:sender.Eth_account.private_key
         ~endpoint
         ~abi:call_types.label
         ~bin:call_types.bin)
      sequencer
  in
  let*@ _ = produce_block sequencer in
  let* tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.contract_send
         ~source_private_key:sender.private_key
         ~endpoint
         ~abi_label:call_types.label
         ~address:contract_address
         ~method_call:"testProduceOpcodes()")
      sequencer
  in
  let*@ _ = produce_block sequencer in
  let*@ trace_result =
    Rpc.trace_transaction
      ~tracer:"callTracer"
      ~transaction_hash:tx_hash
      ~tracer_config:[("withLog", `Bool false); ("onlyTopCall", `Bool false)]
      sequencer
  in
  let call_list = JSON.(trace_result |-> "calls" |> as_list) in
  (* CREATE is at position 0, CREATE2 is at position 1 *)
  List.iteri
    (fun position call ->
      match position with
      | 0 ->
          (* CREATE should have a non-null "to" field with a valid address *)
          Check.(
            (JSON.(call |-> "type" |> as_string) = "CREATE")
              string
              ~error_msg:"Wrong type, expected %R but got %L") ;
          Check.(
            (JSON.is_null JSON.(call |-> "to") = false)
              bool
              ~error_msg:
                "CREATE trace should have a 'to' field with the created \
                 contract address") ;
          let to_addr = JSON.(call |-> "to" |> as_string) in
          Check.(
            (String.length to_addr = 42)
              int
              ~error_msg:
                "CREATE 'to' address should be 42 chars (0x + 40 hex), got \
                 length %L")
      | 1 ->
          (* CREATE2 should have a non-null "to" field with a valid address *)
          Check.(
            (JSON.(call |-> "type" |> as_string) = "CREATE2")
              string
              ~error_msg:"Wrong type, expected %R but got %L") ;
          Check.(
            (JSON.is_null JSON.(call |-> "to") = false)
              bool
              ~error_msg:
                "CREATE2 trace should have a 'to' field with the created \
                 contract address") ;
          let to_addr = JSON.(call |-> "to" |> as_string) in
          Check.(
            (String.length to_addr = 42)
              int
              ~error_msg:
                "CREATE2 'to' address should be 42 chars (0x + 40 hex), got \
                 length %L")
      | _ -> ())
    call_list ;
  unit

let test_trace_transaction_call_tracer_with_logs =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call_trace"; "with_logs"]
    ~title:"debug_traceTransaction with calltracer can produce a call with logs"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  (* [LoggerA.run] emits a log, calls [LoggerB] (which emits its own log and
     returns), then emits another log. We check that the callTracer attributes
     each log to the frame that actually emitted it: the two [LoggerA] logs to
     the top-level frame, and the single [LoggerB] log to the nested call. *)
  let* logger_nested = Solidity_contracts.logger_nested evm_version in
  let* () =
    Eth_cli.add_abi ~label:logger_nested.label ~abi:logger_nested.abi ()
  in
  let* contract_address, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:sender.Eth_account.private_key
         ~endpoint
         ~abi:logger_nested.label
         ~bin:logger_nested.bin)
      sequencer
  in
  let* _ = produce_block sequencer in
  (* Three distinct values so each log is unambiguously identifiable: the first
     and third are emitted by [LoggerA], the second (nested) by [LoggerB]. *)
  let value_a1 = 251197 in
  let value_b = 424242 in
  let value_a2 = 999001 in
  let* tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.contract_send
         ~source_private_key:sender.private_key
         ~endpoint
         ~abi_label:logger_nested.label
         ~address:contract_address
         ~method_call:(Format.sprintf "run(%d,%d,%d)" value_a1 value_b value_a2))
      sequencer
  in
  let* _ = produce_block sequencer in
  let*@ trace_result =
    Rpc.trace_transaction
      ~tracer:"callTracer"
      ~transaction_hash:tx_hash
      ~tracer_config:[("withLog", `Bool true); ("onlyTopCall", `Bool false)]
      sequencer
  in
  (* The emitted value is carried in the second topic; the first topic is the
     event selector, which is irrelevant here. Log addresses are compared
     against the frame's own ["to"] field, so we don't depend on address
     casing. *)
  let log_topic_value log =
    let topics = JSON.(log |-> "topics" |> as_list) in
    JSON.(List.nth topics 1 |> as_string)
  in
  let log_address log = JSON.(log |-> "address" |> as_string) in
  let expected_topic value = add_0x @@ hex_256_of_int value in
  (* Top-level frame: [LoggerA]. *)
  let addr_a = JSON.(trace_result |-> "to" |> as_string) in
  let logs_a = JSON.(trace_result |-> "logs" |> as_list) in
  Check.(
    (List.length logs_a = 2)
      int
      ~error_msg:"Wrong number of logs on the LoggerA frame, expected %R got %L") ;
  (* Nested frame: the single call to [LoggerB]. *)
  let calls = JSON.(trace_result |-> "calls" |> as_list) in
  Check.(
    (List.length calls = 1)
      int
      ~error_msg:"Wrong number of nested calls, expected %R but got %L") ;
  let call_b = List.hd calls in
  let addr_b = JSON.(call_b |-> "to" |> as_string) in
  let logs_b = JSON.(call_b |-> "logs" |> as_list) in
  Check.(
    (List.length logs_b = 1)
      int
      ~error_msg:"Wrong number of logs on the LoggerB frame, expected %R got %L") ;
  Check.(
    (String.lowercase_ascii addr_a <> String.lowercase_ascii addr_b)
      string
      ~error_msg:"LoggerA and LoggerB should live at distinct addresses") ;
  (* Attribution: both LoggerA logs belong to the top frame, in emission
     order (value_a1 then value_a2), each with LoggerA's address. *)
  let log_a1 = List.nth logs_a 0 and log_a2 = List.nth logs_a 1 in
  Check.(
    (log_topic_value log_a1 = expected_topic value_a1)
      string
      ~error_msg:"Wrong first LoggerA log value, expected %R but got %L") ;
  Check.(
    (log_topic_value log_a2 = expected_topic value_a2)
      string
      ~error_msg:"Wrong second LoggerA log value, expected %R but got %L") ;
  List.iter
    (fun log ->
      Check.(
        (String.lowercase_ascii (log_address log)
        = String.lowercase_ascii addr_a)
          string
          ~error_msg:"LoggerA log misattributed, expected address %R but got %L"))
    logs_a ;
  (* Attribution: the single LoggerB log belongs to the nested frame, with
     LoggerB's address. *)
  let log_b = List.hd logs_b in
  Check.(
    (log_topic_value log_b = expected_topic value_b)
      string
      ~error_msg:"Wrong LoggerB log value, expected %R but got %L") ;
  Check.(
    (String.lowercase_ascii (log_address log_b) = String.lowercase_ascii addr_b)
      string
      ~error_msg:"LoggerB log misattributed, expected address %R but got %L") ;
  unit

let test_trace_transaction_call_trace_certain_depth =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call_trace"; "depth"]
    ~title:"debug_traceTransaction with calltracer to see difficult depth"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* call_tracer_depth = Solidity_contracts.call_tracer_depth evm_version in
  let* () =
    Eth_cli.add_abi ~label:call_tracer_depth.label ~abi:call_tracer_depth.abi ()
  in
  let* contract_address, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:sender.Eth_account.private_key
         ~endpoint
         ~abi:call_tracer_depth.label
         ~bin:call_tracer_depth.bin)
      sequencer
  in
  let* _ = produce_block sequencer in
  let* tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.contract_send
         ~source_private_key:sender.private_key
         ~endpoint
         ~abi_label:call_tracer_depth.label
         ~address:contract_address
         ~method_call:"startDepth()")
      sequencer
  in
  let* _ = produce_block sequencer in
  let*@ trace_result =
    Rpc.trace_transaction
      ~tracer:"callTracer"
      ~transaction_hash:tx_hash
      ~tracer_config:[("withLog", `Bool false); ("onlyTopCall", `Bool false)]
      sequencer
  in
  let check_and_get_subcalls opcode size json_value =
    Check.(
      (JSON.(json_value |-> "type" |> as_string) = opcode)
        string
        ~error_msg:"Wrong type, expected %R but got %L") ;

    let call_list = JSON.(json_value |-> "calls" |> as_list) in
    Check.(
      (List.length call_list = size)
        int
        ~error_msg:"Wrong call list size, expected %R but got %L") ;
    return call_list
  in
  (* The transaction's intrinsic gas is accounted only on the top-level
     frame, so every frame's gasUsed covers its subcalls'. Regression: the
     tracer used to inflate every nested frame by the intrinsic gas,
     breaking this invariant. *)
  let rec check_gas_used_covers_subcalls trace =
    let gas_used trace =
      JSON.(trace |-> "gasUsed" |> as_string) |> int_of_string
    in
    let subcalls = JSON.(trace |-> "calls" |> as_list) in
    let subcalls_gas_used =
      List.fold_left (fun acc subcall -> acc + gas_used subcall) 0 subcalls
    in
    Check.(
      (gas_used trace >= subcalls_gas_used)
        int
        ~error_msg:"Frame's gasUsed %L should cover its subcalls' sum %R") ;
    List.iter check_gas_used_covers_subcalls subcalls
  in
  check_gas_used_covers_subcalls trace_result ;
  (* The call list should be of size 4, 3 create and one call in depth *)
  let* depth_call_list = check_and_get_subcalls "CALL" 4 trace_result in
  (* Check that in a CREATE we can make a CALL *)
  let first_contract = List.nth depth_call_list 1 in
  let* create_call_list = check_and_get_subcalls "CREATE" 1 first_contract in
  let call_in_create = List.hd create_call_list in
  let* _ = check_and_get_subcalls "CALL" 0 call_in_create in
  (* Now check the last CALL of the first list that makes multiple calls
     in depth *)
  let depth_0 = List.nth depth_call_list 3 in
  let* depth_1 = check_and_get_subcalls "CALL" 1 depth_0 in
  let* depth_2 = check_and_get_subcalls "CALL" 2 (List.hd depth_1) in
  let depth_2_1, depth_2_2 = (List.nth depth_2 0, List.nth depth_2 0) in
  (* first branch *)
  let* depth_3_1 = check_and_get_subcalls "CALL" 1 depth_2_1 in
  let* _ = check_and_get_subcalls "CALL" 0 (List.hd depth_3_1) in
  (* second branch *)
  let* depth_3_2 = check_and_get_subcalls "CALL" 1 depth_2_2 in
  let* _ = check_and_get_subcalls "CALL" 0 (List.hd depth_3_2) in
  unit

let test_trace_transaction_call_revert =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call_trace"; "revert"; "propagate"]
    ~title:
      "debug_traceTransaction with calltracer to see how propagated revert is \
       handled"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* call_revert = Solidity_contracts.call_revert evm_version in
  let* () = Eth_cli.add_abi ~label:call_revert.label ~abi:call_revert.abi () in
  let* contract_address, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:sender.Eth_account.private_key
         ~endpoint
         ~abi:call_revert.label
         ~bin:call_revert.bin)
      sequencer
  in
  let* _ = produce_block sequencer in
  let* txn =
    Cast.craft_tx
      ~chain_id:1337
      ~source_private_key:sender.private_key
      ~nonce:1
      ~gas_price:1_000_000_000
      ~gas:30_000
      ~value:Wei.zero
      ~address:contract_address
      ~signature:"callerFunction()"
      ()
  in
  let*@ txn_hash = Rpc.send_raw_transaction ~raw_tx:txn sequencer in
  let* _ = produce_block sequencer in
  let*@ trace_result =
    Rpc.trace_transaction
      ~tracer:"callTracer"
      ~transaction_hash:txn_hash
      ~tracer_config:[("withLog", `Bool false); ("onlyTopCall", `Bool false)]
      sequencer
  in
  Check.(
    (JSON.(trace_result |-> "error" |> as_string) = "execution reverted")
      string
      ~error_msg:"Unexpected error got %L but %R was expected") ;
  Check.(
    (JSON.(trace_result |-> "revertReason" |> as_string)
    = "This function reverts")
      string
      ~error_msg:"Unexpected revert reason got %L but %R was expected") ;
  unit

let test_trace_transaction_call_trace_revert =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call_trace"; "revert"]
    ~title:"debug_traceTransaction with calltracer to see how revert is handled"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* call_tracer_revert = Solidity_contracts.call_tracer_revert evm_version in
  let* () =
    Eth_cli.add_abi
      ~label:call_tracer_revert.label
      ~abi:call_tracer_revert.abi
      ()
  in
  let* contract_address, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:sender.Eth_account.private_key
         ~endpoint
         ~abi:call_tracer_revert.label
         ~bin:call_tracer_revert.bin)
      sequencer
  in
  let* _ = produce_block sequencer in
  let* tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.contract_send
         ~source_private_key:sender.private_key
         ~endpoint
         ~abi_label:call_tracer_revert.label
         ~address:contract_address
         ~method_call:"startTest()")
      sequencer
  in
  let* _ = produce_block sequencer in
  let*@ trace_result =
    Rpc.trace_transaction
      ~tracer:"callTracer"
      ~transaction_hash:tx_hash
      ~tracer_config:[("withLog", `Bool false); ("onlyTopCall", `Bool false)]
      sequencer
  in
  let check_error_and_revert_reason ~error ?revert_reason call =
    Check.(
      (JSON.(call |-> "type" |> as_string) = "CALL")
        string
        ~error_msg:"Wrong type, expected %R but got %L") ;
    let error_call = JSON.(call |-> "error" |> as_string) in
    Check.(
      (error_call = error)
        string
        ~error_msg:"Wrong error for call, expected %R but got %L") ;
    match revert_reason with
    | None -> ()
    | Some revert_reason ->
        let revert_reason_call = JSON.(call |-> "revertReason" |> as_string) in
        Check.(
          (revert_reason_call = revert_reason)
            string
            ~error_msg:"Wrong revert_reason for call, expected %R but got %L")
  in
  let call_list = JSON.(trace_result |-> "calls" |> as_list) in
  (* Initial call is of type CALL *)
  Check.(
    (JSON.(trace_result |-> "type" |> as_string) = "CALL")
      string
      ~error_msg:"Wrong type, expected %R but got %L") ;
  (* The call list should be of size 5, 4 call and one create *)
  Check.(
    (List.length call_list = 5)
      int
      ~error_msg:"Wrong call list size, expected %R but got %L") ;
  let call = List.nth call_list 0 in
  check_error_and_revert_reason
    ~error:"execution reverted"
    ~revert_reason:"Expected message for explicit revert"
    call ;
  (* Skip item 1 in call_list, it's a create *)
  let call = List.nth call_list 2 in
  let call = List.hd JSON.(call |-> "calls" |> as_list) in
  check_error_and_revert_reason ~error:"ReentrancySentryOOG" call ;
  let call = List.nth call_list 3 in
  check_error_and_revert_reason ~error:"InvalidFEOpcode" call ;
  let call = List.nth call_list 4 in
  check_error_and_revert_reason
    ~error:"execution reverted"
    ~revert_reason:"Expected message for explicit assert false"
    call ;
  unit

(* A child emits a log then reverts; the parent swallows the revert so the
   outer tx succeeds. The reverted child keeps its frame and [error] but
   loses its logs (the kernel keeps them; the RPC rendering drops them), so
   the trace's logs equal [eth_getLogs]. *)
let test_trace_transaction_call_tracer_reverted_logs_dropped =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call_trace"; "with_logs"; "revert"]
    ~title:
      "debug_traceTransaction callTracer drops logs of a reverted sub-frame"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* swallower =
    Solidity_contracts.call_tracer_revert_logs_swallower evm_version
  in
  let* () = Eth_cli.add_abi ~label:swallower.label ~abi:swallower.abi () in
  let* contract_address, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:sender.Eth_account.private_key
         ~endpoint
         ~abi:swallower.label
         ~bin:swallower.bin)
      sequencer
  in
  let* _ = produce_block sequencer in
  let value = 777 in
  let* tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.contract_send
         ~source_private_key:sender.private_key
         ~endpoint
         ~abi_label:swallower.label
         ~address:contract_address
         ~method_call:(Format.sprintf "run(%d)" value))
      sequencer
  in
  let* _ = produce_block sequencer in
  let*@ trace_result =
    Rpc.trace_transaction
      ~tracer:"callTracer"
      ~transaction_hash:tx_hash
      ~tracer_config:[("withLog", `Bool true)]
      sequencer
  in
  (* The parent frame carries exactly its own Ok log. *)
  let topic_value log =
    JSON.(log |-> "topics" |> as_list |> fun l -> List.nth l 1 |> as_string)
  in
  let root_logs = JSON.(trace_result |-> "logs" |> as_list) in
  Check.(
    (List.length root_logs = 1)
      int
      ~error_msg:"Wrong root logs size, expected %R but got %L") ;
  let ok_log = List.hd root_logs in
  Check.(
    (topic_value ok_log = add_0x @@ hex_256_of_int value)
      string
      ~error_msg:"Wrong Ok topic, expected %R but got %L") ;
  let calls = JSON.(trace_result |-> "calls" |> as_list) in
  Check.(
    (List.length calls = 1)
      int
      ~error_msg:"Wrong number of child calls, expected %R but got %L") ;
  let child = List.hd calls in
  Check.(
    (JSON.(child |-> "error" |> as_string) = "execution reverted")
      string
      ~error_msg:"Wrong error for the reverted child, expected %R but got %L") ;
  Check.(
    (JSON.(child |-> "revertReason" |> as_string) = "reverting on purpose")
      string
      ~error_msg:"Wrong revert reason, expected %R but got %L") ;
  (* geth omits the reverted frame's [logs] field, it does not empty it. *)
  Check.(
    (trace_frame_has_logs_field child = false)
      bool
      ~error_msg:"Reverted child's logs field must be absent") ;
  Check.(
    (List.length JSON.(child |-> "calls" |> as_list) = 0)
      int
      ~error_msg:"The reverted child must have no children, got %L") ;
  let*@! Transaction.{blockNumber; _} =
    Rpc.get_transaction_receipt ~tx_hash sequencer
  in
  let block_number = Int32.to_int blockNumber in
  let*@ chain_logs =
    Rpc.get_logs
      ~from_block:(Number block_number)
      ~to_block:(Number block_number)
      sequencer
  in
  Check.(
    (List.length chain_logs = 1)
      int
      ~error_msg:"eth_getLogs must carry only the surviving Ok log, got %L") ;
  let chain_bodies =
    List.map
      (fun l ->
        let a, t, d = Transaction.extract_log_body l in
        ( String.lowercase_ascii a,
          List.map String.lowercase_ascii t,
          String.lowercase_ascii d ))
      chain_logs
  in
  (* Sorted multisets: the trace carries no execution order. *)
  Check.(
    (List.sort compare [trace_log_body ok_log] = List.sort compare chain_bodies)
      (list (tuple3 string (list string) string))
      ~error_msg:
        "Trace log union must equal eth_getLogs on the revert path, expected \
         %R but got %L") ;
  unit

(* The test checks that each call is traced separately, not causing an error
   having multiple top calls. *)
let test_trace_transaction_calltracer_multiple_txs =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call_trace"; "multi_txs"]
    ~title:
      "debug_traceTransaction handles blocks containing several transactions"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
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
      ~value:Wei.zero
      ~address
      ~signature:"testProduceOpcodes()"
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
  let*@ _ =
    Rpc.trace_transaction
      ~tracer:"callTracer"
      ~transaction_hash:transaction_hash_0
      ~tracer_config:[("withLog", `Bool false); ("onlyTopCall", `Bool false)]
      sequencer
  in
  let*@ _ =
    Rpc.trace_transaction
      ~tracer:"callTracer"
      ~transaction_hash:transaction_hash_1
      ~tracer_config:[("withLog", `Bool false); ("onlyTopCall", `Bool false)]
      sequencer
  in
  (* At this point, the two RPCs succeeded otherwise we'd get an error about having
     multiple top calls. *)
  unit

let test_trace_transaction_calltracer_on_simple_transfer =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call_trace"; "simple_transfer"]
    ~title:"debug_traceTransaction can trace a simple transfer"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let receiver = Eth_account.bootstrap_accounts.(1) in
  let* transaction_hash =
    send_transaction_to_sequencer
      (Eth_cli.transaction_send
         ~source_private_key:sender.private_key
         ~to_public_key:receiver.address
         ~value:(Wei.of_eth_int 10)
         ~endpoint)
      sequencer
  in
  let* _ = produce_block sequencer in
  let*@ _ =
    Rpc.trace_transaction
      ~tracer:"callTracer"
      ~transaction_hash
      ~tracer_config:[("withLog", `Bool false); ("onlyTopCall", `Bool false)]
      sequencer
  in
  unit

let test_trace_transaction_calltracer_precompiles =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call_trace"; "precompiles"]
    ~title:"debug_traceTransaction with calltracer can trace precompiles"
    ~da_fee:Wei.zero
    ~maximum_allowed_ticks:100_000_000_000_000L
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* precompiles = Solidity_contracts.precompiles evm_version in
  let* () = Eth_cli.add_abi ~label:precompiles.label ~abi:precompiles.abi () in
  let* contract_address, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:sender.Eth_account.private_key
         ~endpoint
         ~abi:precompiles.label
         ~bin:precompiles.bin)
      sequencer
  in
  let* _ = produce_block sequencer in
  let* transaction_hash =
    send_transaction_to_sequencer
      (Eth_cli.contract_send
         ~source_private_key:sender.private_key
         ~endpoint
         ~abi_label:precompiles.label
         ~address:contract_address
         ~method_call:"callPrecompiles()")
      sequencer
  in
  let* _ = produce_block sequencer in
  let*@ trace_result =
    Rpc.trace_transaction
      ~tracer:"callTracer"
      ~transaction_hash
      ~tracer_config:[("withLog", `Bool false); ("onlyTopCall", `Bool false)]
      sequencer
  in
  let call_list = JSON.(trace_result |-> "calls" |> as_list) in
  Check.(
    (JSON.(trace_result |-> "type" |> as_string) = "CALL")
      string
      ~error_msg:"Wrong type, expected %R but got %L") ;
  (* All precompiles from 0x00..01 to 0x00..09. *)
  Check.(
    (List.length call_list = 9)
      int
      ~error_msg:"Wrong call list size, expected %R but got %L") ;
  List.iteri
    (fun position call ->
      let precompile_number = position + 1 in
      (* Each precompile is reached through a Solidity `staticcall` from
         the caller contract, so every frame is a STATICCALL originating
         from [contract_address] and targeting the precompile address
         0x00..0{precompile_number}. *)
      Check.(
        (JSON.(call |-> "type" |> as_string) = "STATICCALL")
          string
          ~error_msg:
            (Format.sprintf
               "Wrong call type for precompile %d, expected %%R but got %%L"
               precompile_number)) ;
      Check.(
        (JSON.(call |-> "from" |> as_string)
        = String.lowercase_ascii contract_address)
          string
          ~error_msg:
            (Format.sprintf
               "Wrong caller for precompile %d, expected %%R but got %%L"
               precompile_number)) ;
      Check.(
        (JSON.(call |-> "to" |> as_string)
        = Format.sprintf "0x%040x" precompile_number)
          string
          ~error_msg:
            (Format.sprintf
               "Wrong precompiled contract for frame %d, expected %%R but got \
                %%L"
               precompile_number)) ;
      if precompile_number = 4 then (
        Check.(
          (JSON.(call |-> "output" |> as_string) = "0x46726565")
            string
            ~error_msg:
              "Wrong identity precompile output, expected %R but got %L") ;
        Check.(
          (JSON.(call |-> "gasUsed" |> as_string) <> "0x0")
            string
            ~error_msg:
              "Identity precompile frame should record a non-zero gasUsed, got \
               %L")))
    call_list ;
  unit

let test_trace_transaction_calltracer_deposit =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~time_between_blocks:Nothing
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "rpc"; "trace"; "call_trace"; "deposit"]
    ~title:"debug_traceTransaction with calltracer can trace deposits"
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
  let endpoint = Evm_node.endpoint sequencer in
  let amount = Tez.of_int 16 in
  let depositor = Constant.bootstrap5 in
  let receiver =
    Eth_account.
      {
        address = "0x1074Fd1EC02cbeaa5A90450505cF3B48D834f3EB";
        private_key =
          "0xb7c548b5442f5b28236f0dcd619f65aaaafd952240908adcf9642d8e616587ee";
      }
  in
  let* receiver_balance_prev =
    Eth_cli.balance ~account:receiver.address ~endpoint ()
  in
  let deposit_info =
    {receiver = EthereumAddr receiver.address; chain_id = None}
  in
  let* () =
    send_deposit_to_delayed_inbox
      ~amount
      ~bridge:l1_contracts.bridge
      ~depositor
      ~deposit_info
      ~sc_rollup_node
      ~sc_rollup_address
      client
  in
  let* () =
    wait_for_delayed_inbox_add_tx_and_injected
      ~sequencer
      ~sc_rollup_node
      ~client
  in
  let*@ block = Rpc.get_block_by_number ~block:"latest" sequencer in
  let transaction_hash =
    match block.transactions with
    | Hash txs -> List.hd txs
    | Full _ | Empty ->
        failwith "Block should contain at least one simple transaction"
  in
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in
  let* receiver_balance_next =
    Eth_cli.balance ~account:receiver.address ~endpoint ()
  in
  Check.((receiver_balance_next > receiver_balance_prev) Wei.typ)
    ~error_msg:"Expected a bigger balance" ;
  (* The following tracing should succeed. *)
  let*@ _ =
    Rpc.trace_transaction
      ~tracer:"callTracer"
      ~transaction_hash
      ~tracer_config:[("withLog", `Bool false); ("onlyTopCall", `Bool false)]
      sequencer
  in
  unit

let test_trace_transaction_calltracer_on_nested_delegatecalls =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call_trace"; "nested_delegatecalls"]
    ~title:
      "debug_traceTransaction with calltracer can trace nested delegatecalls \
       with appropriate sender addresses for each call"
    ~da_fee:Wei.zero
    ~maximum_allowed_ticks:100_000_000_000_000L
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let source_private_key, address_eoa =
    Eth_account.
      ( bootstrap_accounts.(0).private_key,
        String.lowercase_ascii bootstrap_accounts.(0).address )
  in
  let init_contract ~contract =
    let open Solidity_contracts in
    let* contract = contract evm_version in
    let* () = Eth_cli.add_abi ~label:contract.label ~abi:contract.abi () in
    let* address, _ =
      send_transaction_to_sequencer
        (Eth_cli.deploy
           ~source_private_key
           ~endpoint
           ~abi:contract.label
           ~bin:contract.bin)
        sequencer
    in
    return (String.lowercase_ascii address, contract.label)
  in
  let* address_A, label_A =
    init_contract ~contract:Solidity_contracts.nested_delegatecalls_A
  in
  let* address_B, _ =
    init_contract ~contract:Solidity_contracts.nested_delegatecalls_B
  in
  let* address_C, _ =
    init_contract ~contract:Solidity_contracts.nested_delegatecalls_C
  in
  let* address_D, _ =
    init_contract ~contract:Solidity_contracts.nested_delegatecalls_D
  in
  let* _ = produce_block sequencer in
  let* transaction_hash =
    send_transaction_to_sequencer
      (Eth_cli.contract_send
         ~source_private_key
         ~endpoint
         ~abi_label:label_A
         ~address:address_A
         ~method_call:
           (Format.sprintf
              "callB(\"%s\",\"%s\",\"%s\")"
              address_B
              address_C
              address_D))
      sequencer
  in
  let* _ = produce_block sequencer in
  let*@ trace_result =
    Rpc.trace_transaction
      ~tracer:"callTracer"
      ~transaction_hash
      ~tracer_config:[("withLog", `Bool true); ("onlyTopCall", `Bool false)]
      sequencer
  in
  assert (JSON.(trace_result |-> "type" |> as_string) = "CALL") ;
  assert (JSON.(trace_result |-> "from" |> as_string) = address_eoa) ;
  assert (JSON.(trace_result |-> "to" |> as_string) = address_A) ;
  let call_list = JSON.(trace_result |-> "calls" |> as_list) in
  let delegatecall_1 = List.hd call_list in
  assert (JSON.(delegatecall_1 |-> "type" |> as_string) = "DELEGATECALL") ;
  assert (JSON.(delegatecall_1 |-> "from" |> as_string) = address_A) ;
  assert (JSON.(delegatecall_1 |-> "to" |> as_string) = address_B) ;
  let delegatecall_1_call_list = JSON.(delegatecall_1 |-> "calls" |> as_list) in
  let delegatecall_2 = List.hd delegatecall_1_call_list in
  assert (JSON.(delegatecall_2 |-> "type" |> as_string) = "CALL") ;
  assert (JSON.(delegatecall_2 |-> "from" |> as_string) = address_A) ;
  assert (JSON.(delegatecall_2 |-> "to" |> as_string) = address_C) ;
  let delegatecall_2_call_list = JSON.(delegatecall_2 |-> "calls" |> as_list) in
  let delegatecall_3 = List.hd delegatecall_2_call_list in
  assert (JSON.(delegatecall_3 |-> "type" |> as_string) = "CALL") ;
  assert (JSON.(delegatecall_3 |-> "from" |> as_string) = address_C) ;
  assert (JSON.(delegatecall_3 |-> "to" |> as_string) = address_D) ;
  unit

let protocols = [Protocol.Alpha]

let () =
  test_trace_transaction_call_trace protocols ;
  test_trace_delegate_call protocols ;
  test_trace_transaction_calltracer_all_types protocols ;
  test_trace_transaction_calltracer_create_to_field protocols ;
  test_trace_transaction_call_tracer_with_logs protocols ;
  test_trace_transaction_call_revert protocols ;
  test_trace_transaction_call_trace_certain_depth protocols ;
  test_trace_transaction_call_trace_revert protocols ;
  test_trace_transaction_call_tracer_reverted_logs_dropped protocols ;
  test_trace_transaction_calltracer_multiple_txs protocols ;
  test_trace_transaction_calltracer_on_simple_transfer protocols ;
  test_trace_transaction_calltracer_precompiles protocols ;
  test_trace_transaction_calltracer_deposit protocols ;
  test_trace_transaction_calltracer_on_nested_delegatecalls [Alpha] ;
  test_trace_transaction_calltracer_failed_create protocols
