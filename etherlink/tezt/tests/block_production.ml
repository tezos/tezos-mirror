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
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file block_production.ml
   Subject:      Block production policy: timestamps, size and tick limits.
*)

open Sc_rollup_helpers
open Rpc.Syntax
open Transaction
open Delayed_inbox
open Test_helpers
open Setup
module Protocol = Test_helpers.Protocol_no_direct_register

let test_rpc_produceBlock =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "produce_block"]
    ~title:"RPC method produceBlock"
  @@ fun {sequencer; _} _protocol ->
  let*@ start_block_number = Rpc.block_number sequencer in
  let*@ _ = produce_block sequencer in
  let*@ new_block_number = Rpc.block_number sequencer in
  Check.((Int32.succ start_block_number = new_block_number) int32)
    ~error_msg:"Expected new block number to be %L, but got: %R" ;
  unit

let test_sequencer_is_reimbursed =
  let tbb = 1. in
  (* We use an arbitrary address for the pool address, the goal is just to
     verify its balance increases. *)
  let sequencer_pool_address = "0xb7a97043983f24991398e5a82f63f4c58a417185" in
  register_all
    ~__FILE__
    ~da_fee:(Wei.of_string "1_000_000_000_000")
    ~time_between_blocks:(Time_between_blocks tbb)
    ~sequencer_pool_address
    ~tags:["evm"; "sequencer"; "transaction"; "reimbursed"]
    ~title:"Sequencer is reimbursed for DA fees"
  @@ fun {sequencer = sequencer_node; _} _protocol ->
  let* balance =
    Eth_cli.balance
      ~account:sequencer_pool_address
      ~endpoint:Evm_node.(endpoint sequencer_node)
      ()
  in

  Check.((Wei.zero = balance) Wei.typ)
    ~error_msg:"Balance of the sequencer address pool should be null" ;

  (* Ensure the sequencer has produced the block. *)
  let* () =
    Evm_node.wait_for_blueprint_applied ~timeout:10.0 sequencer_node 1
  in

  let* txn =
    Eth_cli.transaction_send
      ~source_private_key:Eth_account.bootstrap_accounts.(1).private_key
      ~to_public_key:Eth_account.bootstrap_accounts.(2).address
      ~value:Wei.one
      ~endpoint:(Evm_node.endpoint sequencer_node)
      ()
  in

  let* receipt =
    Eth_cli.get_receipt ~endpoint:(Evm_node.endpoint sequencer_node) ~tx:txn ()
  in

  match receipt with
  | Some receipt when receipt.status ->
      let* balance =
        Eth_cli.balance
          ~account:sequencer_pool_address
          ~endpoint:Evm_node.(endpoint sequencer_node)
          ()
      in

      Check.((Wei.zero < balance) Wei.typ)
        ~error_msg:"Balance of the sequencer address pool should not be null" ;
      unit
  | Some _ ->
      Test.fail
        "transaction receipt received from the sequencer, but transaction \
         failed"
  | None ->
      Test.fail
        "Missing receipt in the sequencer node for transaction successfully \
         injected in the observer"

let test_no_automatic_block_production =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "block"]
    ~title:"No automatic block production"
  @@ fun {sequencer; _} _protocol ->
  let*@ before_head = Rpc.get_block_by_number ~block:"latest" sequencer in
  let transfer =
    let* tx_hash =
      Eth_cli.transaction_send
        ~source_private_key:Eth_account.(bootstrap_accounts.(0).private_key)
        ~to_public_key:Eth_account.(bootstrap_accounts.(0).address)
        ~value:(Wei.of_eth_int 1)
        ~endpoint:(Evm_node.endpoint sequencer)
        ()
    in
    return (Some tx_hash)
  in
  let timeout =
    let* () = Lwt_unix.sleep 15. in
    return None
  in
  let* tx_hash = Lwt.pick [transfer; timeout] in

  let*@ after_head = Rpc.get_block_by_number ~block:"latest" sequencer in
  (* As the time between blocks is "none", the sequencer should not produce a block
     even if we send a transaction. *)
  Check.((before_head.number = after_head.number) int32)
    ~error_msg:"No block production expected" ;
  (* The transaction hash is not returned as no receipt is produced, and eth-cli
     awaits for the receipt. *)
  Check.is_true
    (Option.is_none tx_hash)
    ~error_msg:"No transaction hash expected" ;
  unit

let test_non_increasing_timestamp =
  register_all
    ~__FILE__
    ~genesis_timestamp:Client.(At (Time.of_notation_exn "2020-01-01T00:00:00Z"))
    ~kernels:[Kernel.Latest]
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "block"; "timestamp"]
    ~title:"Non increasing timestamp are forbidden"
  @@ fun {sequencer; _} _protocol ->
  let*@ (_ : int) = produce_block ~timestamp:"2020-01-01T00:00:05Z" sequencer in
  (* This produce block will fail as the timestamp is before the previous block. *)
  let* () =
    match Evm_node.mode sequencer with
    | Sequencer _ ->
        let*@? _err =
          Rpc.produce_block ~timestamp:"2020-01-01T00:00:00Z" sequencer
        in
        unit
    | _ -> assert false (* impossible case as it's a sequencer. *)
  in
  (* However the same timestamp is accepted. *)
  let*@ (_ : int) = produce_block ~timestamp:"2020-01-01T00:00:05Z" sequencer in
  unit

let test_timestamp_from_the_future =
  register_all
    ~__FILE__
    ~max_blueprint_lookahead_in_seconds:300L
    ~genesis_timestamp:Client.(At (Time.of_notation_exn "2020-01-01T00:00:00Z"))
    ~kernels:[Kernel.Latest]
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "block"; "timestamp"]
    ~title:"Timestamp from the future are refused"
    ~use_dal:ci_enabled_dal_registration
  @@ fun {sequencer; sc_rollup_node; client; enable_dal; _} _protocol ->
  (* In this test the time between blocks is 1 second. *)

  (* Producing a block 4:25 minutes after the L1 timestamp will be accepted. We
     do not check precisely 4:59 minutes to avoid flakiness w.r.t to blueprint
     inclusion. *)
  let* current_l1_timestamp = l1_timestamp client in
  let accepted_timestamp =
    Tezos_base.Time.Protocol.(add current_l1_timestamp 265L |> to_notation)
  in
  (* The sequencer will accept it anyway, but we need to check that the rollup
     node accepts it. *)
  let*@ (_ : int) = produce_block ~timestamp:accepted_timestamp sequencer in
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

  (* Producing a block 5:30 minutes after the L1 timestamp will be accepted by
     the sequencer and not the rollup node. *)
  let* current_l1_timestamp = l1_timestamp client in
  let refused_timestamp =
    Tezos_base.Time.Protocol.(add current_l1_timestamp 330L |> to_notation)
  in
  let*@ (_ : int) = produce_block ~timestamp:refused_timestamp sequencer in
  (* We wait more in case of DAL because 5 blocks are not enough to
     send the blueprint through the DAL. *)
  let number_of_blocks_to_wait = if enable_dal then 20 else 5 in
  let* _ =
    repeat number_of_blocks_to_wait (fun () ->
        let* _l1_lvl = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in

  let*@ rollup_head = rollup_level sc_rollup_node in
  let*@ sequencer_head = Rpc.block_number sequencer in

  Check.((sequencer_head = Int32.succ rollup_head) int32)
    ~error_msg:"The rollup was supposed to refuse the block" ;

  let kernel_log =
    let path = Sc_rollup_node.data_dir sc_rollup_node // "kernel.log" in
    read_file path
  in

  Check.(
    kernel_log
    =~ rex
         "Deleting invalid blueprint at path /base/blueprints/2, error: \
          TimestampFromFuture")
    ~error_msg:"The blueprint should have been refused by TimestampFromFuture" ;

  unit

(** Test that the kernel can handle more than 100 withdrawals per level,
    which is currently the limit of outbox messages in the L1. *)
let test_outbox_size_limit_resilience ~slow =
  let commitment_period = 5 and challenge_window = 5 in
  let slow_str = if slow then "slow" else "fast" in
  register_all
    ~__FILE__
    ~sequencer:Constant.bootstrap1
    ~time_between_blocks:Nothing
    ~tags:
      (["evm"; "withdraw"; "outbox"; "spam"] @ if slow then [Tag.slow] else [])
    ~title:(sf "Outbox size limit resilience (%s)" slow_str)
    ~commitment_period
    ~challenge_window
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        evm_version;
        _;
      }
      _protocol
    ->
  let endpoint = Evm_node.endpoint sequencer in

  (* Make a tez deposit *)
  let amount = Tez.of_int 1000 in
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
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in
  let* receiver_balance_next =
    Eth_cli.balance ~account:receiver.address ~endpoint ()
  in
  Check.((receiver_balance_next > receiver_balance_prev) Wei.typ)
    ~error_msg:"Expected a bigger balance" ;

  (* Deploy and top up batch withdrawal contract *)
  let* spammer_resolved = Solidity_contracts.spam_withdrawal evm_version in
  let* () =
    Eth_cli.add_abi ~label:spammer_resolved.label ~abi:spammer_resolved.abi ()
  in
  let* spammer_contract, _tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:receiver.private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi:spammer_resolved.abi
         ~bin:spammer_resolved.bin)
      sequencer
  in
  let* _ =
    send_transaction_to_sequencer
      (Eth_cli.contract_send
         ~source_private_key:receiver.private_key
         ~endpoint
         ~abi_label:spammer_resolved.label
         ~address:spammer_contract
         ~method_call:"giveFunds()"
         ~value:(Wei.of_eth_int 990))
      sequencer
  in

  (* Create and topup many accounts (to overcome single account per block limitation) *)
  let* wallets = Cast.gen_wallets ~number:150 () in
  let addresses =
    List.map (fun (w : Cast.wallet) -> sf "\"%s\"" w.address) wallets
  in
  let* _ =
    send_transaction_to_sequencer
      (Eth_cli.contract_send
         ~source_private_key:receiver.private_key
         ~endpoint
         ~abi_label:spammer_resolved.label
         ~address:spammer_contract
         ~method_call:(sf "batchTopUp([%s])" (String.concat "," addresses))
         ~value:(Wei.of_eth_int 0))
      sequencer
  in

  (* Create 150 withdrawals *)
  let* withdrawal_level = Client.level client in
  let withdraw_tx ~(wallet : Cast.wallet) =
    Cast.craft_tx
      ~source_private_key:wallet.private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:30_000_000
      ~value:(Wei.of_eth_int 0)
      ~address:spammer_contract
      ~signature:"doWithdrawals(uint256)"
      ~arguments:["1"]
  in
  let* withdraw_txs =
    List.map (fun w -> withdraw_tx ~wallet:w ()) wallets |> Lwt.all
  in
  let* _ = batch_n_transactions ~evm_node:sequencer withdraw_txs in
  let* _ = produce_block sequencer in

  (* At this point the outbox queue must contain 150 messages *)
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

  (* 100 messages flushed at this point *)
  let* _ = produce_block sequencer in
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

  (* +50 messages flushed at this point *)
  if slow then (
    (* Execute the first 100 withdrawals *)
    let* actual_withdrawal_level =
      find_and_execute_withdrawal
        ~withdrawal_level
        ~commitment_period
        ~challenge_window
        ~sc_rollup_node
        ~sc_rollup_address
        ~client
        ()
    in
    let* balance =
      Client.get_balance_for
        ~account:"tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j"
        client
    in
    Check.((balance = Tez.of_int 100) Tez.typ)
      ~error_msg:"Expected balance of %R, got %L" ;
    (* Execute the next 50 withdrawals *)
    let* _ =
      find_and_execute_withdrawal
        ~withdrawal_level:(actual_withdrawal_level + 1)
        ~commitment_period
        ~challenge_window
        ~sc_rollup_node
        ~sc_rollup_address
        ~client
        ()
    in
    let* balance =
      Client.get_balance_for
        ~account:"tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j"
        client
    in
    Check.((balance = Tez.of_int 150) Tez.typ)
      ~error_msg:"Expected balance of %R, got %L" ;
    unit)
  else unit

let test_stage_one_reboot =
  register_all
    ~__FILE__
    ~sequencer:Constant.bootstrap1
    ~time_between_blocks:Nothing
    ~maximum_allowed_ticks:9_000_000_000L
    ~tags:["evm"; "sequencer"; "reboot"; Tag.slow]
    ~title:
      "Checks the stage one reboots when reading too much chunks in a single \
       L1 level"
  @@ fun {sc_rollup_node; client; sc_rollup_address; _} _protocol ->
  let* chunks =
    Lwt_list.map_s (fun i ->
        Evm_node.chunk_data
          ~rollup_address:sc_rollup_address
          ~sequencer_key:Constant.bootstrap1.alias
          ~client
          ~number:i
          [])
    @@ List.init 400 Fun.id
  in
  let chunks = List.flatten chunks in
  let send_chunks chunks src =
    let messages =
      `A (List.map (fun c -> `String c) chunks)
      |> JSON.annotate ~origin:"send_message"
      |> JSON.encode
    in
    Client.Sc_rollup.send_message
      ?wait:None
      ~msg:("hex:" ^ messages)
      ~src
      client
  in
  let rec split_chunks acc chunks =
    match chunks with
    | [] -> acc
    | _ ->
        let messages, rem = Tezos_stdlib.TzList.split_n 100 chunks in
        split_chunks (messages :: acc) rem
  in
  let splitted_messages = split_chunks [] chunks in
  let* () =
    Lwt_list.iteri_s
      (fun i messages -> send_chunks messages Account.Bootstrap.keys.(i).alias)
      splitted_messages
  in
  let* total_tick_number_before_expected_reboots =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_total_ticks ()
  in
  let* _ = Rollup.next_rollup_node_level ~client ~sc_rollup_node in
  let* total_tick_number_with_expected_reboots =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_total_ticks ()
  in
  let ticks_after_expected_reboot =
    total_tick_number_with_expected_reboots
    - total_tick_number_before_expected_reboots
  in

  (* The PVM takes 11G ticks for collecting inputs, 11G for a kernel_run. As such,
     an L1 level is at least 22G ticks. *)
  let min_ticks_per_l1_level = ticks_per_snapshot * 2 in
  (* If the inbox is not empty, the kernel enforces a reboot after reading it,
     to give the maximum ticks available for the first block production. *)
  let min_ticks_when_inbox_is_not_empty =
    min_ticks_per_l1_level + ticks_per_snapshot
  in
  Check.((ticks_after_expected_reboot > min_ticks_when_inbox_is_not_empty) int)
    ~error_msg:
      "The number of ticks spent during the period should be higher than %R, \
       but got %L, which implies there have been no reboot, contrary to what \
       was expected." ;
  unit

let test_blueprint_is_limited_in_size =
  register_all
    ~__FILE__
    ~sequencer:Constant.bootstrap1
    ~time_between_blocks:Nothing
    ~max_number_of_chunks:2
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
    ~eth_bootstrap_accounts:Eth_account.lots_of_address
    ~tags:["evm"; "sequencer"; "blueprint"; "limit"]
    ~title:
      "Checks the sequencer doesn't produce blueprint bigger than the given \
       maximum number of chunks"
    ~use_dal:ci_enabled_dal_registration
      (* IC set to false because transaction dropping mechanism from IC workflow invalidates it:
      Remaining transactions are not included in the last block *)
    ~instant_confirmations:false
  @@ fun {sequencer; _} _protocol ->
  let txs = read_tx_from_file () |> List.map (fun (tx, _hash) -> tx) in
  let* requests, hashes = batch_n_transactions ~evm_node:sequencer txs in
  (* Each transaction is about 114 bytes, hence 100 * 114 = 11400 bytes, which
     will fit in two blueprints of two chunks each. *)
  let* () = next_evm_level ~evm_node:sequencer in
  let* () = next_evm_level ~evm_node:sequencer in
  let first_hash = List.hd hashes in
  let* level_of_first_transaction =
    let*@ receipt = Rpc.get_transaction_receipt ~tx_hash:first_hash sequencer in
    match receipt with
    | None -> Test.fail "Delayed transaction hasn't be included"
    | Some receipt -> return receipt.blockNumber
  in
  let*@ block_with_first_transaction =
    Rpc.get_block_by_number
      ~block:(Int32.to_string level_of_first_transaction)
      sequencer
  in
  (* The block containing the first transaction of the batch cannot contain the
     100 transactions of the batch, as it doesn't fit in two chunks. *)
  let block_size_of_first_transaction =
    match block_with_first_transaction.Block.transactions with
    | Block.Empty -> Test.fail "Expected a non empty block"
    | Block.Full _ ->
        Test.fail "Block is supposed to contain only transaction hashes"
    | Block.Hash hashes ->
        Check.((List.length hashes < List.length requests) int)
          ~error_msg:"Expected less than %R transactions in the block, got %L" ;
        List.length hashes
  in

  let* () = next_evm_level ~evm_node:sequencer in
  (* It's not clear the first transaction of the batch is applied in the first
     blueprint or the second, as it depends how the tx_pool sorts the
     transactions (by caller address). We need to check that either the previous
     block or the next block contains transactions, which puts in evidence that
     the batch has been split into two consecutive blueprints.
  *)
  let check_block_size block_number =
    let*@ block =
      Rpc.get_block_by_number ~block:(Int32.to_string block_number) sequencer
    in
    match block.Block.transactions with
    | Block.Empty -> return 0
    | Block.Full _ ->
        Test.fail "Block is supposed to contain only transaction hashes"
    | Block.Hash hashes -> return (List.length hashes)
  in
  let* next_block_size =
    check_block_size (Int32.succ level_of_first_transaction)
  in
  let* previous_block_size =
    check_block_size (Int32.pred level_of_first_transaction)
  in
  if next_block_size = 0 && previous_block_size = 0 then
    Test.fail
      "The sequencer didn't apply the 100 transactions in two consecutive \
       blueprints" ;
  Check.(
    (block_size_of_first_transaction + previous_block_size + next_block_size
    = List.length hashes)
      int
      ~error_msg:
        "Not all the transactions have been injected, only %L, while %R was \
         expected.") ;
  unit

let test_regression_block_hash_gen =
  (* This test is created because of bug in blockConstant in simulation,
     which caused the simulation to return a wrong estimate of gas limit,
     leading to failed contract deployment for block_hash_gen.
     This test checks regression for the fix *)
  let timestamp = "2020-01-01T00:00:05Z" in
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~tags:["evm"; "l2_call"; "block_hash"; "timestamp"]
    ~title:"Random generation based on block hash and timestamp"
    ~genesis_timestamp:Client.(At (Time.of_notation_exn timestamp))
  @@ fun {sequencer; evm_version; _} _protocol ->
  let* () =
    repeat 3 (fun _ ->
        let*@ _ = produce_block sequencer ~timestamp in
        unit)
  in
  let* {abi; bin; _} = Solidity_contracts.block_hash_gen evm_version in
  let* _contract, _tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi
         ~bin)
      sequencer
  in
  unit

let test_filling_max_slots_cant_lead_to_out_of_memory =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "slots"; "out_of_memory"]
    ~title:
      "Calling a contract that fills the maximum amount of slots leads to \
       OutOfGas not an out of memory error"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* slot_filler = Solidity_contracts.slot_filler evm_version in
  let* () = Eth_cli.add_abi ~label:slot_filler.label ~abi:slot_filler.abi () in
  let* contract_address, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:sender.Eth_account.private_key
         ~endpoint
         ~abi:slot_filler.label
         ~bin:slot_filler.bin)
      sequencer
  in
  let* _ = produce_block sequencer in
  let* failure =
    Eth_cli.contract_send
      ~source_private_key:sender.private_key
      ~endpoint
      ~abi_label:slot_filler.label
      ~expect_failure:true
      ~address:contract_address
      ~method_call:"fillSlots()"
      ()
  in
  let contains s sub =
    let re = Str.regexp_string sub in
    try
      ignore (Str.search_forward re s 0) ;
      true
    with Not_found -> false
  in
  let error_out_of_gas = "OutOfGas(Basic)" in
  if contains failure error_out_of_gas then unit
  else Test.fail "Test should fail with error: %s" error_out_of_gas

let test_block_producer_validation () =
  register_sandbox_with_observer
  (* Disable preconfirmations because transaction are rejected before the block production when IC are enabled *)
    ~patch_config:
      (Evm_node.patch_config_with_experimental_feature
         ~preconfirmation_stream_enabled:false
         ())
    ~__FILE__
    ~tags:["observer"; "tx_queue"; "validation"]
    ~title:"Test part of the validation is done when producing blocks."
  @@ fun {sandbox; observer} ->
  let send_and_wait_sequencer_receive ~raw_tx =
    let wait_sequencer_see_tx =
      Evm_node.wait_for_tx_queue_add_transaction sandbox
    in
    let* hash =
      let*@ hash = Rpc.send_raw_transaction ~raw_tx observer in
      return hash
    and* _ = wait_sequencer_see_tx in
    return hash
  in

  (* helper to craft a tx with given nonce. *)
  let gas_price = 1_000_000_000 in
  let gas = 23_000 in
  let gas_cost = Wei.of_eth_int (1 * 23) in
  let*@ balance_b0 =
    Rpc.get_balance ~address:Eth_account.bootstrap_accounts.(0).address observer
  in
  let*@ balance_b1 =
    Rpc.get_balance ~address:Eth_account.bootstrap_accounts.(1).address observer
  in

  let* raw_tx_empty_account_bO =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price
      ~gas
      ~value:Wei.(balance_b0 - gas_cost)
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  in
  let* raw_tx_empty_account_b1 =
    let*@ _ =
      Rpc.get_balance
        ~address:Eth_account.bootstrap_accounts.(1).address
        observer
    in
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(1).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price
      ~gas
      ~value:Wei.(balance_b0 + balance_b1 - (gas_cost * Z.of_int 2))
      ~address:Eth_account.bootstrap_accounts.(2).address
      ()
  in
  let* raw_tx_invalid_value =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:1
      ~gas_price
      ~gas
      ~value:(Wei.of_eth_int 100)
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  in
  let* raw_tx_invalid_nonce =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(1).private_key
      ~chain_id:1337
      ~nonce:10
      ~gas_price
      ~gas
      ~value:Wei.one
      ~address:Eth_account.bootstrap_accounts.(2).address
      ()
  in
  let* _hash =
    (* tx included *)
    send_and_wait_sequencer_receive ~raw_tx:raw_tx_empty_account_bO
  in
  let* invalid_balance_hash1 =
    send_and_wait_sequencer_receive ~raw_tx:raw_tx_invalid_value
  in
  let* _hash =
    (* The transaction is included because the balance of b1 is
       covered by the preceding transaction,
       `raw_tx_empty_account_b0`. *)
    send_and_wait_sequencer_receive ~raw_tx:raw_tx_empty_account_b1
  in
  let* invalid_nonce_hash2 =
    send_and_wait_sequencer_receive ~raw_tx:raw_tx_invalid_nonce
  in
  let* txs =
    let*@ txs = produce_block sandbox in
    return txs
  and* reason1 =
    Evm_node.wait_for_block_producer_rejected_transaction
      ~hash:invalid_balance_hash1
      sandbox
  and* reason2 =
    Evm_node.wait_for_block_producer_rejected_transaction
      ~hash:invalid_nonce_hash2
      sandbox
  in
  Check.(reason1 =~ rex "Not enough funds")
    ~error_msg:"transaction rejected for invalid reason, found %L, expected %R" ;
  Check.(reason2 =~ rex "Transaction nonce is not the expected nonce")
    ~error_msg:"transaction rejected for invalid reason, found %L, expected %R" ;
  Check.((txs = 2) int ~error_msg:"block has %L, but expected %R") ;
  unit

let test_next_block_info_ic_reset () =
  register_sandbox_with_observer
    ~fail_on_divergence:false
    ~patch_config:
      (Evm_node.patch_config_with_experimental_feature
         ~preconfirmation_stream_enabled:true
         ())
    ~__FILE__
    ~genesis_timestamp
    ~tags:["evm"; "observer"; "next_block_info"; "ic_reset"]
    ~title:
      "Observer re-enters Executing on duplicate next_block_info, drops old tx \
       and executes new one"
  @@ fun {sandbox; observer} ->
  (* Step 1: produce an initial block so both nodes are synced and the
     observer's preconfirmation stream callbacks are activated. *)
  let*@ _ = produce_block ~timestamp:"2020-01-01T00:00:01Z" sandbox in
  let* () = Evm_node.wait_for_blueprint_applied observer 1 in

  (* Step 2: submit a transaction so the observer enters IC Executing
     state via the preconfirmation stream's next_block_info. *)
  let next_block_info_p = Evm_node.wait_for_next_block_info observer in
  let* raw_tx =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:21_000
      ~value:(Wei.of_eth_int 1)
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  in
  let*@ _hash = Rpc.send_raw_transaction ~raw_tx sandbox in
  let* _ = next_block_info_p in

  (* Step 3: restart the sequencer without producing a block. The observer
     reconnects while its IC state is still Executing. After restart, the
     block_producer is in Awaiting_first_timestamp so we re-enable
     preconfirmations by proposing a timestamp. *)
  let* () = Evm_node.terminate sandbox in
  let* () = Evm_node.run sandbox in
  let* () = Evm_node.wait_for_ready sandbox in
  let*@ () =
    Rpc.propose_next_block_timestamp ~timestamp:"2020-01-01T00:00:02Z" sandbox
  in
  let* () = Evm_node.wait_for_connection_acquired observer in

  (* Step 4: re-submit a transaction. The sequencer sends a new
     next_block_info which the observer receives while still in Executing
     state from step 2. This triggers ic_reset (discarding the first tx's
     partial IC work), then IC re-enters Executing with the new block info.
     The re-submitted transaction should be executed via IC
     (single_tx_execution_done fires). *)
  let ic_reset_p = Evm_node.wait_for_ic_reset observer in
  let single_tx_done_p = Evm_node.wait_for_single_tx_execution_done observer in
  let* raw_tx =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:21_000
      ~value:(Wei.of_eth_int 1)
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  in
  let*@ _hash = Rpc.send_raw_transaction ~raw_tx sandbox in
  let* () = ic_reset_p in
  let* _hash = single_tx_done_p in
  let*@ _ = produce_block ~timestamp:"2020-01-01T00:00:02Z" sandbox in
  let* () = Evm_node.wait_for_blueprint_applied observer 2 in
  unit

let protocols = [Protocol.Alpha]

let () =
  test_rpc_produceBlock protocols ;
  test_sequencer_is_reimbursed protocols ;
  test_no_automatic_block_production protocols ;
  test_non_increasing_timestamp protocols ;
  test_timestamp_from_the_future protocols ;
  test_stage_one_reboot protocols ;
  test_blueprint_is_limited_in_size protocols ;
  test_regression_block_hash_gen protocols ;
  test_outbox_size_limit_resilience ~slow:true protocols ;
  test_outbox_size_limit_resilience ~slow:false protocols ;
  test_filling_max_slots_cant_lead_to_out_of_memory protocols ;
  test_block_producer_validation () ;
  test_next_block_info_ic_reset ()
