(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Trilitech <contact@trili.tech>                         *)
(* Copyright (c) 2024-2025 Functori <contact@functori.com>                   *)
(*                                                                           *)
(*****************************************************************************)

open Rpc.Syntax
open Transaction
open Test_helpers
open Setup

type sandbox_test = {sandbox : Evm_node.t; observer : Evm_node.t}

let register_sandbox_with_observer ?tx_queue_tx_per_addr_limit ~title
    ?set_account_code ?da_fee_per_byte ?minimum_base_fee_per_gas ~tags
    ?patch_config ?websockets ?(sequencer_keys = [Constant.bootstrap1]) body =
  Test.register
    ~__FILE__
    ~title
    ~tags
    ~uses_admin_client:false
    ~uses_client:false
    ~uses_node:false
    ~uses:
      [
        Constant.octez_evm_node;
        Constant.WASM.evm_kernel;
        Constant.smart_rollup_installer;
      ]
  @@ fun () ->
  let* sandbox =
    init_sequencer_sandbox
      ?tx_queue_tx_per_addr_limit
      ?set_account_code
      ?da_fee_per_byte
      ?minimum_base_fee_per_gas
      ?patch_config
      ?websockets
      ~sequencer_keys
      ()
  in
  let* observer = Setup.run_new_observer_node ~sc_rollup_node:None sandbox in
  body {sandbox; observer}

(* To be removed when this experimental feature is stable *)
let setup_experimental_feature sandbox observer =
  let patch_config =
    Evm_node.patch_config_with_experimental_feature
      ~preconfirmation_stream_enabled:true
      ()
  in
  let* () = Evm_node.terminate sandbox in
  let* () = Evm_node.terminate observer in
  let* () = Evm_node.Config_file.update sandbox patch_config in
  let* () = Evm_node.Config_file.update observer patch_config in
  let* () = Evm_node.run sandbox in
  let* _ = Evm_node.wait_for_ready sandbox in
  let* rpc_node = run_new_rpc_endpoint sandbox in
  let* () =
    Evm_node.run
      ~extra_arguments:["--evm-node-endpoint"; Evm_node.endpoint rpc_node]
      observer
  in
  let* _ = Evm_node.wait_for_ready observer in
  let* _res = produce_block sandbox in
  let* _ = Evm_node.wait_for_blueprint_applied observer 1 in
  unit

let get_receipt_result ?(timeout = 0) sender_pk nonce_counter gas_price
    receiver_addr sandbox rpc_observer_node observer block =
  let* raw_tx =
    Cast.craft_tx
      ~source_private_key:sender_pk
      ~chain_id:1337
      ~nonce:!nonce_counter
      ~gas_price:(Int32.to_int gas_price)
      ~gas:23_300
      ~value:(Wei.of_eth_int 10)
      ~address:receiver_addr
      ()
  in
  let receipt_promise =
    Rpc.eth_send_raw_transaction_sync ~raw_tx ~block rpc_observer_node ~timeout
  in
  let* _ = Evm_node.wait_for_tx_queue_injecting_transaction observer in
  Check.((Lwt.is_sleeping receipt_promise = true) bool)
    ~error_msg:"eth_sendRawTransactionSync should wait for inclusion" ;
  if block = Pending || timeout > 0 then
    let* receipt = receipt_promise in
    return receipt
  else
    let* _ = Evm_node.wait_for_inclusion observer in
    let*@ _ = produce_block sandbox in
    let* receipt = receipt_promise in
    return receipt

let test_observer_receives_preconfirmations () =
  register_sandbox_with_observer
    ~tags:["evm"; "delayed_transaction"]
    ~title:"Observer receives preconfirmations"
  @@ fun {sandbox; observer; _} ->
  let* () = setup_experimental_feature sandbox observer in
  (*
    1. Observer receives the transaction
    2. Transaction is added to the transaction queue
    3. Transaction is forwarded to the sequencer
    4. Sequencer validates the transaction
    5. Sequencer streams the result back to the observer
    6. Sequencer produces block in order to access the receipt
    7. Hashes are the same
  *)
  let add = Evm_node.wait_for_tx_queue_add_transaction sandbox in
  let next_ts = Evm_node.wait_for_next_block_timestamp observer in
  let preconf = Evm_node.wait_for_inclusion observer in
  let p =
    Eth_cli.transaction_send
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~to_public_key:Eth_account.bootstrap_accounts.(1).address
      ~value:(Wei.of_eth_int 10)
      ~endpoint:(Evm_node.endpoint observer)
      ()
  in
  let* _ = add in
  let* _ = next_ts in
  let* streamed_txn_hash = preconf in
  let* _res = produce_block sandbox in
  let* txn_hash = p in
  Check.(
    (txn_hash = streamed_txn_hash)
      string
      ~error_msg:
        "txn hash received through the preconfirmation stream does not match \
         expected value") ;
  unit

let test_eth_send_raw_transaction_sync_rpc () =
  register_sandbox_with_observer
    ~tags:["evm"; "delayed_transaction"]
    ~title:"eth_sendRawTransactionSync waits for receipt"
  @@ fun {sandbox; observer; _} ->
  let* () = setup_experimental_feature sandbox observer in
  let* gas_price = Rpc.get_gas_price sandbox in
  let nonce_counter = ref 0 in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let receiver = Eth_account.bootstrap_accounts.(1) in
  let* rpc_observer_node = run_new_rpc_endpoint observer in
  let*@ receipt =
    get_receipt_result
      sender.private_key
      nonce_counter
      gas_price
      receiver.address
      sandbox
      rpc_observer_node
      observer
      Latest
  in
  Check.(
    (receipt.status = true)
      bool
      ~error_msg:
        "Transaction should have been included and executed successfully") ;
  incr nonce_counter ;
  let*@ preconfirmed_receipt =
    get_receipt_result
      sender.private_key
      nonce_counter
      gas_price
      receiver.address
      sandbox
      rpc_observer_node
      observer
      Latest
  in
  Check.(
    (preconfirmed_receipt.status = true)
      bool
      ~error_msg:"Transaction should have been executed successfully") ;
  unit

let test_multiple_transactions_comparison () =
  register_sandbox_with_observer
    ~tags:["evm"; "delayed_transaction"]
    ~title:"eth_sendRawTransactionSync with multiple transactions comparison"
  @@ fun {sandbox; observer} ->
  let* () = setup_experimental_feature sandbox observer in
  let* rpc_observer_node = run_new_rpc_endpoint observer in
  let* sandbox_preconfirm =
    init_sequencer_sandbox ~sequencer_keys:[Constant.bootstrap1] ()
  in
  let* observer_preconfirm =
    Setup.run_new_observer_node ~sc_rollup_node:None sandbox_preconfirm
  in
  let* () = setup_experimental_feature sandbox_preconfirm observer_preconfirm in
  let* rpc_observer_node_preconfirm =
    run_new_rpc_endpoint observer_preconfirm
  in
  let* gas_price = Rpc.get_gas_price sandbox in
  let nonce_counter = ref 0 in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let receiver = Eth_account.bootstrap_accounts.(1) in

  let rec send_n_transactions n =
    if n <= 0 then unit
    else
      let*@ receipt =
        get_receipt_result
          sender.private_key
          nonce_counter
          gas_price
          receiver.address
          sandbox
          rpc_observer_node
          observer
          Latest
      in
      Check.(
        (receipt.status = true)
          bool
          ~error_msg:
            "Transaction should have been included and executed successfully") ;
      let*@ preconfirmed_receipt =
        get_receipt_result
          sender.private_key
          nonce_counter
          gas_price
          receiver.address
          sandbox_preconfirm
          rpc_observer_node_preconfirm
          observer_preconfirm
          Pending
      in
      Check.(
        (preconfirmed_receipt.status = true)
          bool
          ~error_msg:"Transaction should have been executed successfully") ;

      Check.(
        (receipt.transactionHash = preconfirmed_receipt.transactionHash)
          string
          ~error_msg:
            "Transaction hashes should match between standard and \
             preconfirmation nodes") ;
      Check.(
        (receipt.gasUsed = preconfirmed_receipt.gasUsed)
          int64
          ~error_msg:
            "Gas used should match between standard and preconfirmation nodes") ;
      incr nonce_counter ;
      let* () = send_n_transactions (n - 1) in
      unit
  in
  let* () = send_n_transactions 10 in
  unit

let test_eth_send_raw_transaction_sync_rpc_multiple_transactions_per_block () =
  register_sandbox_with_observer
    ~tags:["evm"; "delayed_transaction"]
    ~title:"eth_sendRawTransactionSync with multiple transactions per block"
    ~tx_queue_tx_per_addr_limit:20
  @@ fun {sandbox; observer} ->
  let* () = setup_experimental_feature sandbox observer in
  let* gas_price = Rpc.get_gas_price sandbox in
  let nonce_counter = ref 0 in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let receiver = Eth_account.bootstrap_accounts.(1) in
  let* rpc_observer_node = run_new_rpc_endpoint observer in
  let rec scenario nb_block =
    if nb_block <= 0 then unit
    else
      let rec send_n_transactions_per_block n =
        if n <= 0 then unit
        else
          let*@ receipt =
            get_receipt_result
              sender.private_key
              nonce_counter
              gas_price
              receiver.address
              sandbox
              rpc_observer_node
              observer
              Pending
          in
          Check.(
            (receipt.status = true)
              bool
              ~error_msg:
                "Transaction should have been included and executed \
                 successfully") ;
          incr nonce_counter ;
          let* () = send_n_transactions_per_block (n - 1) in
          unit
      in
      let* () = send_n_transactions_per_block 5 in
      let*@ last_receipt =
        get_receipt_result
          sender.private_key
          nonce_counter
          gas_price
          receiver.address
          sandbox
          rpc_observer_node
          observer
          Latest
      in
      incr nonce_counter ;
      let*@ block = Rpc.get_block_by_number ~block:"latest" sandbox in
      Check.(
        (last_receipt.blockNumber = block.number)
          int32
          ~error_msg:
            "Last transaction's block number should match the latest block \
             number") ;
      Check.(
        (last_receipt.transactionIndex = Int32.of_int 5)
          int32
          ~error_msg:
            "Last transaction's index should be 5, as there were 5 \
             transactions in the block") ;
      Check.(
        (last_receipt.cumulativeGasUsed = block.gasUsed)
          int64
          ~error_msg:
            "Last transaction's cumulative gas used should match the block's \
             gas used") ;
      scenario (nb_block - 1)
  in
  let* () = scenario 3 in
  unit

let test_eth_send_raw_transaction_sync_rpc_error_propagated () =
  register_sandbox_with_observer
    ~tags:["evm"; "delayed_transaction"]
    ~title:"eth_sendRawTransactionSync error propagation"
  @@ fun {sandbox; observer} ->
  let* () = setup_experimental_feature sandbox observer in
  let* gas_price = Rpc.get_gas_price sandbox in
  let nonce = 1 in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let receiver = Eth_account.bootstrap_accounts.(1) in
  let* rpc_observer_node = run_new_rpc_endpoint observer in
  let* raw_tx =
    Cast.craft_tx
      ~source_private_key:sender.private_key
      ~chain_id:1337
      ~nonce
      ~gas_price:(Int32.to_int gas_price)
      ~gas:21_000
      ~value:(Wei.of_eth_int 10_000_000)
      ~address:receiver.address
      ()
  in
  let* result =
    Rpc.eth_send_raw_transaction_sync
      ~raw_tx
      ~block:Pending
      rpc_observer_node
      ~timeout:0
  in
  match result with
  | Ok _ ->
      Test.fail
        "eth_sendRawTransactionSync should have failed due to insufficient \
         funds, but got a receipt"
  | Error rpc_err ->
      Check.(
        (rpc_err.message = "Transaction nonce is not the expected nonce.")
          string
          ~error_msg:
            "eth_sendRawTransactionSync error message does not match expected \
             value") ;
      unit

let test_eth_send_raw_transaction_sync_rpc_timeouts () =
  register_sandbox_with_observer
    ~tags:["evm"; "delayed_transaction"]
    ~title:"eth_sendRawTransactionSync timeouts"
  @@ fun {sandbox; observer} ->
  let* () = setup_experimental_feature sandbox observer in
  let* gas_price = Rpc.get_gas_price sandbox in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let receiver = Eth_account.bootstrap_accounts.(1) in
  let* rpc_observer_node = run_new_rpc_endpoint observer in
  let* receipt =
    get_receipt_result
      ~timeout:5000
      sender.private_key
      (ref 0)
      gas_price
      receiver.address
      sandbox
      rpc_observer_node
      observer
      Latest
  in
  match receipt with
  | Ok _ ->
      Test.fail
        "eth_sendRawTransactionSync should have timed out, but got a receipt"
  | Error _ -> unit

let () =
  test_observer_receives_preconfirmations () ;
  test_eth_send_raw_transaction_sync_rpc () ;
  test_eth_send_raw_transaction_sync_rpc_timeouts () ;
  test_eth_send_raw_transaction_sync_rpc_error_propagated () ;
  test_multiple_transactions_comparison () ;
  test_eth_send_raw_transaction_sync_rpc_multiple_transactions_per_block ()
