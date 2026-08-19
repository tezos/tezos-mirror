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
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file storage.ml
   Subject:      Node storage: SQLite compression, durable storage, block-param RPCs.
*)

open Rpc.Syntax
open Transaction
open Test_helpers
open Setup
module Protocol = Test_helpers.Protocol_no_direct_register

let test_get_balance_block_param =
  register_all
    ~__FILE__
    ~tags:["evm"; "sequencer"; "rpc"; "get_balance"; "block_param"]
    ~title:"RPC method getBalance uses block parameter"
    ~time_between_blocks:Nothing
  @@ fun {sequencer; sc_rollup_node; client; _} _protocol ->
  (* Transfer funds to a random address. *)
  let address = "0xB7A97043983f24991398E5a82f63F4C58a417185" in
  let* _tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.transaction_send
         ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
         ~to_public_key:address
         ~value:(Wei.of_eth_int 10)
         ~endpoint:(Evm_node.endpoint sequencer))
      sequencer
  in
  (* Check the balance on genesis block and latest block. *)
  let*@ balance_genesis =
    Rpc.get_balance ~address ~block:(Number 0) sequencer
  in
  let*@ balance_now = Rpc.get_balance ~address ~block:Latest sequencer in
  Check.((balance_genesis = Wei.of_eth_int 0) Wei.typ)
    ~error_msg:(sf "%s should have no funds at genesis, but got %%L" address) ;
  Check.((balance_now = Wei.of_eth_int 10) Wei.typ)
    ~error_msg:(sf "Balance of %s expected to be %%R but got %%L" address) ;
  (* Now we will create another observer initialized from the rollup node, in
     order to test the block parameter "earliest". "Earliest" on the current
     sequencer will be block [0] which is not really robust to test. *)
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in
  let* _ =
    repeat 2 (fun _ ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  let observer_partial_history =
    Evm_node.create
      ~node_setup:
        (Evm_node.make_setup
           ~name:"partial_history"
           ~initial_kernel:"evm_kernel.wasm"
           ~preimages_dir:"/tmp"
           ())
      ~mode:
        (Observer
           {
             rollup_node_endpoint = Some (Sc_rollup_node.endpoint sc_rollup_node);
             evm_node_endpoint = Evm_node.endpoint sequencer;
           })
      ()
  in
  let* () =
    Process.check @@ Evm_node.spawn_init_config observer_partial_history
  in
  let* () =
    Evm_node.init_from_rollup_node_data_dir
      observer_partial_history
      sc_rollup_node
  in
  let* () = Evm_node.run observer_partial_history in
  (* Transfer funds again to the address. *)
  let*@ level = Rpc.block_number sequencer in
  let* _tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.transaction_send
         ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
         ~to_public_key:address
         ~value:(Wei.of_eth_int 10)
         ~endpoint:(Evm_node.endpoint sequencer))
      sequencer
  in
  let* () =
    Evm_node.wait_for_blueprint_applied
      observer_partial_history
      (Int32.to_int level + 1)
  in
  (* Observer does not know block 0. *)
  let*@? (_error : Rpc.error) =
    Rpc.get_balance ~address ~block:(Number 0) observer_partial_history
  in
  let*@ balance_earliest =
    Rpc.get_balance ~address ~block:Earliest observer_partial_history
  in
  let*@ balance_now =
    Rpc.get_balance ~address ~block:Latest observer_partial_history
  in
  Check.((balance_earliest = Wei.of_eth_int 10) Wei.typ)
    ~error_msg:(sf "%s expected to have a balance of %%R but got %%L" address) ;
  Check.((balance_now = Wei.of_eth_int 20) Wei.typ)
    ~error_msg:(sf "%s expected to have a balance of %%R but got %%L" address) ;
  unit

let test_get_block_by_number_block_param =
  register_all
    ~__FILE__
    ~tags:["evm"; "sequencer"; "rpc"; "get_block_by_number"; "block_param"]
    ~title:"RPC method getBlockByNumber uses block parameter"
    ~time_between_blocks:Nothing
  @@ fun {sequencer; observer; sc_rollup_node; client; _} _protocols ->
  let observer_offset = 3l in
  let* () =
    repeat Int32.(to_int observer_offset) @@ fun () ->
    next_evm_level ~evm_node:sequencer
  in
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in
  let* _ =
    repeat 2 (fun _ ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  let observer_partial_history =
    Evm_node.create
      ~node_setup:
        (Evm_node.make_setup
           ~name:"partial_history"
           ?initial_kernel:(Evm_node.initial_kernel observer)
           ~preimages_dir:"/tmp"
           ())
      ~mode:
        (Observer
           {
             rollup_node_endpoint = Some (Sc_rollup_node.endpoint sc_rollup_node);
             evm_node_endpoint = Evm_node.endpoint sequencer;
           })
      ()
  in
  let* () =
    Process.check @@ Evm_node.spawn_init_config observer_partial_history
  in
  let* () =
    Evm_node.init_from_rollup_node_data_dir
      observer_partial_history
      sc_rollup_node
  in
  let* () = Evm_node.run observer_partial_history in
  let* () = repeat 2 @@ fun () -> next_evm_level ~evm_node:sequencer in

  let*@ earliest_block_sequencer =
    Rpc.get_block_by_number ~block:"earliest" sequencer
  in
  let*@ earliest_block_observer =
    Rpc.get_block_by_number ~block:"earliest" observer
  in
  let*@ earliest_block_observer_partial =
    Rpc.get_block_by_number ~block:"earliest" observer_partial_history
  in

  Check.(
    ((earliest_block_sequencer.number = 0l) int32)
      ~error_msg:"Earliest block of sequencer is %L instead of %R") ;

  Check.(
    ((earliest_block_observer.number = 0l) int32)
      ~error_msg:"Earliest block of observer is %L instead of %R") ;

  Check.(
    ((earliest_block_observer_partial.number = observer_offset) int32)
      ~error_msg:"Earliest block of observer started late is %L instead of %R") ;

  unit

let test_extended_block_param =
  register_all
    ~__FILE__
    ~tags:["evm"; "sequencer"; "rpc"; "block_param"; "counter"]
    ~title:"Supports extended block parameter"
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocols ->
  (*
     In this test we will deploy a counter contract, increments its counter
     at multiple consecutives blocks, and check the counter using block
     parameter.
     As the counter contract has a view, we can test the block parameter
     both for read only RPCs such as [eth_getStorageAt] and simulation
     such as [eth_call].
  *)
  let* counter_resolved = Solidity_contracts.counter evm_version in
  let* () =
    Eth_cli.add_abi ~label:counter_resolved.label ~abi:counter_resolved.abi ()
  in
  (* Deploy the contract. *)
  let* contract, _tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi:counter_resolved.abi
         ~bin:counter_resolved.bin)
      sequencer
  in
  (* Takes the level where it was originated, the counter value will be 0. *)
  let*@ level = Rpc.block_number sequencer in
  let level = Int32.to_int level in
  (* Increments the counter multiple times. *)
  let* () =
    repeat 2 (fun _ ->
        let* _ =
          send_transaction_to_sequencer
            (Eth_cli.contract_send
               ~source_private_key:
                 Eth_account.bootstrap_accounts.(0).private_key
               ~endpoint:(Evm_node.endpoint sequencer)
               ~abi_label:counter_resolved.label
               ~address:contract
               ~method_call:"incrementCounter()")
            sequencer
        in
        unit)
  in

  let*@ block0 =
    Rpc.get_block_by_number ~block:(string_of_int level) sequencer
  in
  let*@ block1 =
    Rpc.get_block_by_number ~block:(string_of_int (level + 1)) sequencer
  in
  let*@ block2 =
    Rpc.get_block_by_number ~block:(string_of_int (level + 2)) sequencer
  in

  let check_counter_value ~(block : Block.t) expected_value =
    let counter_value ~block =
      let*@ s =
        Rpc.get_storage_at ~address:contract ~pos:"0x0" ~block sequencer
      in
      return (int_of_string s)
    in
    let* counter_via_number =
      counter_value
        ~block:
          (Block_number
             {number = Int32.to_int block.number; require_canonical = false})
    in
    let* counter_via_hash =
      counter_value
        ~block:(Block_hash {hash = block.hash; require_canonical = false})
    in
    let*@ counter_via_call =
      Rpc.(
        call
          ~block:(Number (Int32.to_int block.number))
          ~to_:contract
          ~data:"a87d942c"
          sequencer)
    in
    let counter_via_call = int_of_string counter_via_call in
    Check.((counter_via_number = expected_value) int)
      ~error_msg:"Expected counter to be %R but got %L, using {blockNumber}" ;
    Check.((counter_via_hash = expected_value) int)
      ~error_msg:"Expected counter to be %R but got %L, using {blockHash}" ;
    Check.((counter_via_call = expected_value) int)
      ~error_msg:"Expected counter to be %R but got %L, using eth_call" ;
    unit
  in

  let* () = check_counter_value ~block:block0 0 in
  let* () = check_counter_value ~block:block1 1 in
  let* () = check_counter_value ~block:block2 2 in

  unit

let test_rpc_getLogs_with_earliest_fail =
  register_all
    ~__FILE__
    ~tags:["evm"; "rpc"; "get_logs"; "earliest"]
    ~title:"RPC method getLogs with earliest block"
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
  @@ fun {sequencer; evm_version; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* erc20_resolved = Solidity_contracts.erc20 evm_version in
  let* () =
    Eth_cli.add_abi ~label:erc20_resolved.label ~abi:erc20_resolved.abi ()
  in
  let* contract, _tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi:erc20_resolved.abi
         ~bin:erc20_resolved.bin)
      sequencer
  in
  let call_mint n =
    Eth_cli.contract_send
      ~source_private_key:sender.private_key
      ~endpoint
      ~abi_label:erc20_resolved.label
      ~address:contract
      ~method_call:(Printf.sprintf "mint(%d)" n)
  in
  let nb_generated_logs = 5 in
  let* _ =
    repeat nb_generated_logs (fun _ ->
        let* _ =
          wait_for_application
            ~produce_block:(fun _ -> produce_block sequencer)
            (call_mint 42)
        in
        unit)
  in
  let*@ get_logs_using_number = Rpc.get_logs ~from_block:(Number 0) sequencer in
  let*@ get_logs_using_earliest = Rpc.get_logs ~from_block:Earliest sequencer in
  Check.(
    (List.length get_logs_using_number = nb_generated_logs)
      int
      ~error_msg:"Expected %R logs, got %L") ;
  Check.(
    (List.length get_logs_using_earliest = List.length get_logs_using_number)
      int
      ~error_msg:"Expected %R logs using Earliest as from_block, got %L") ;
  (* Check that when from > to we get an empty list *)
  let*@ empty_logs =
    Rpc.get_logs ~from_block:Latest ~to_block:Earliest sequencer
  in
  Check.((List.length empty_logs = 0) int) ~error_msg:"Expected %R logs, got %L" ;
  unit

let test_apply_from_full_history_mode =
  register_all
    ~__FILE__
    ~genesis_timestamp
    ~time_between_blocks:Nothing
    ~tags:["evm"; "observer"]
    ~title:"Can apply blueprints from full mode"
  @@ fun {sequencer; observer; _} _protocol ->
  (* Stop the observer *)
  let* () = Evm_node.terminate observer in

  (* Restart sequencer in full mode *)
  let* () = Evm_node.terminate sequencer in
  let*! () = Evm_node.switch_history_mode sequencer (Full 3) in
  let wait_full =
    Evm_node.wait_for_start_history_mode ~history_mode:"full:3" sequencer
  in
  let* () = Evm_node.run sequencer and* _ = wait_full in

  (* Trigger gc twice with 9 blocks and confirm it *)
  let wait_gc = Evm_node.wait_for_gc_finished sequencer in
  let* _ =
    fold 9 () (fun i _ ->
        let* _ = Rpc.produce_block ~timestamp:(get_timestamp i) sequencer in
        Evm_node.wait_for_blueprint_injected sequencer i)
  and* _ = wait_gc in

  Log.info "first block in sequencer is accessible" ;
  let*@ _ = Rpc.get_block_by_number ~block:"1" sequencer in

  (* Start the observer and check it applied block 1  *)
  let* () = Evm_node.run observer in
  let* _ = Evm_node.wait_for_blueprint_applied observer 1 in

  Log.info "first block in observer after the catchup is accessible" ;
  let*@ _ = Rpc.get_block_by_number ~block:"1" observer in
  unit

(* Test that everyone agrees on what the storage version of the rollup
   is. *)
let test_durable_storage_consistency =
  register_all
    ~__FILE__
    ~tags:["durable_storage"; "consistency"]
    ~time_between_blocks:Nothing
    ~title:"Everyone agrees on L1 level"
  @@ fun {sequencer; observer; sc_rollup_node; kernel; _} _protocol ->
  let key = Durable_storage_path.storage_version kernel in
  let* val_from_rollup_node_opt =
    Sc_rollup_node.RPC.call sc_rollup_node ~rpc_hooks:Tezos_regression.rpc_hooks
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind:"wasm_2_0_0"
         ~operation:Sc_rollup_rpc.Value
         ~key
         ()
  in
  let val_from_rollup_node =
    match val_from_rollup_node_opt with
    | None ->
        Test.fail
          ~__LOC__
          "No value found in durable storage from rollup node at path %S"
          key
    | Some v -> v
  in
  let () =
    Log.info
      "Value at path %S according to rollup node: %S"
      key
      val_from_rollup_node
  in

  (* Rpc.state_value calls a private RPC so we can only test the EVM
     nodes for which a private RPC server exists and
     [Evm_node.endpoint ~private_:true] succeeds not the
     RPC EVM nodes. *)
  let evm_nodes_with_private_servers = [sequencer; observer] in

  let* () =
    Lwt_list.iter_s
      (fun evm_node ->
        let*@ val_from_evm_node_opt = Rpc.state_value evm_node key in
        match val_from_evm_node_opt with
        | Some v when v = val_from_rollup_node -> unit
        | Some val_from_evm_node ->
            let () =
              Log.info
                "Value at path %S according to EVM node %s: %S (value \
                 according to rollup node: %S)"
                key
                (Evm_node.name evm_node)
                val_from_evm_node
                val_from_rollup_node
            in
            unit
        | None ->
            Test.fail
              ~__LOC__
              "No value found in durable storage from EVM node %s at path %S"
              (Evm_node.name evm_node)
              key)
      evm_nodes_with_private_servers
  in
  unit

let test_evm_events_cleanup () =
  Test_helpers.register_sandbox
    ~__FILE__
    ~title:
      "The EVM node does not accumulate unnecessary EVM events in its durable \
       storage"
    ~tags:["events"]
  @@ fun sandbox ->
  let*@ _ = Rpc.produce_block sandbox in
  let*@ rpc_result = Rpc.state_subkeys sandbox "/base/rollup_events" in
  match rpc_result with
  | None | Some [] -> unit
  | Some l ->
      Test.fail
        "Expected an empty result, got %a instead"
        Format.(
          pp_print_list
            ~pp_sep:(fun fmt () -> pp_print_string fmt ", ")
            pp_print_string)
        l

let patch_config_sqlite_compression value =
  JSON.update
    "experimental_features"
    (JSON.put
       ("sqlite_compression", JSON.annotate ~origin:"sqlite_compression" value))

let compression_check_rpcs sequencer ~tx_hash =
  let*@! receipt = Rpc.get_transaction_receipt ~tx_hash sequencer in
  Check.((receipt.transactionHash = tx_hash) string)
    ~error_msg:"eth_getTransactionReceipt hash mismatch: %L vs %R" ;
  let*@ block_obj =
    Rpc.get_block_by_number
      ~block:(Int32.to_string receipt.blockNumber)
      ~full_tx_objects:true
      sequencer
  in
  Check.((block_obj.number = receipt.blockNumber) int32)
    ~error_msg:"eth_getBlockByNumber: expected %R, got %L" ;
  let* block_receipts =
    Evm_node.(
      jsonrpc
        sequencer
        {
          method_ = "eth_getBlockReceipts";
          parameters = `A [`String (Format.sprintf "%#lx" block_obj.number)];
        })
  in
  Check.is_true
    ~__LOC__
    (JSON.(block_receipts |-> "result" |> as_list) <> [])
    ~error_msg:"eth_getBlockReceipts should return non-empty receipts" ;
  unit

(* keccak256("LogValue(uint256)") — topic 0 emitted by [SimpleLogger] for the
   single indexed [value] argument. Matches the contract at
   [etherlink/kernel_latest/solidity_examples/simple_logger.sol]. *)
let log_value_event_topic =
  let h =
    Tezos_crypto.Hacl.Hash.Keccak_256.digest
      (Bytes.of_string "LogValue(uint256)")
  in
  "0x" ^ Hex.show (Hex.of_bytes h)

let hex_256_of_int n = Printf.sprintf "0x%064x" n

(* Deploys [SimpleLogger] from [bootstrap_accounts.(0)] and registers its
   ABI with eth-cli. Returns the deployed address and the ABI label, both
   needed by [send_compression_test_logvalue] below. The deployment is
   itself a transaction whose receipt-fields/object-fields BLOBs are
   wide enough to be a representative compression workload. *)
let setup_simple_logger sequencer =
  let* simple_logger = Solidity_contracts.simple_logger Evm_version.Cancun in
  let* () =
    Eth_cli.add_abi ~label:simple_logger.label ~abi:simple_logger.abi ()
  in
  let* address, _deploy_tx =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi:simple_logger.label
         ~bin:simple_logger.bin)
      sequencer
  in
  return (address, simple_logger.label)

(* Calls [SimpleLogger.logValue(value)] from [bootstrap_accounts.(0)]. The
   resulting receipt has one [LogValue(uint256)] log entry with [value] as
   its second indexed topic — both the receipt and the bloom filter are
   non-trivial, so the [transactions.{receipt,object}_fields] BLOBs are
   meaningfully compressible. *)
let send_compression_test_logvalue ~logger:(address, abi_label) ~value sequencer
    =
  send_transaction_to_sequencer
    (Eth_cli.contract_send
       ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
       ~endpoint:(Evm_node.endpoint sequencer)
       ~abi_label
       ~address
       ~method_call:(Format.sprintf "logValue(%d)" value))
    sequencer

(* Plain ETH transfer: receipt has no logs and an all-zero bloom. Used in
   the [eth_getLogs] test alongside [send_compression_test_logvalue] so
   the [transactions] table contains both log-bearing and zero-bloom rows
   under the bloom prefilter. *)
let send_compression_test_transfer sequencer =
  send_transaction_to_sequencer
    (Eth_cli.transaction_send
       ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
       ~to_public_key:"0xB7A97043983f24991398E5a82f63F4C58a417185"
       ~value:(Wei.of_eth_int 10)
       ~endpoint:(Evm_node.endpoint sequencer))
    sequencer

(* Read the first [n] bytes of [column] for the row at [rowid] in
   [table], returned as an upper-case hex string (no [0x] prefix). Used
   to peek at storage-class-and-compression metadata of EVM-store rows
   without going through the node's RPCs. *)
let read_first_bytes_hex ~data_dir ~table ~column ~rowid ~n =
  let db = Filename.concat data_dir "store.sqlite" in
  let sql =
    Format.sprintf
      "SELECT hex(substr(%s, 1, %d)) FROM %s WHERE rowid = %d"
      column
      n
      table
      rowid
  in
  let* output = Process.run_and_read_stdout "sqlite3" [db; sql] in
  Lwt.return (String.trim output)

let test_sqlite_compression_migration () =
  Test.register
    ~__FILE__
    ~title:"SQLite compression migration end-to-end"
    ~tags:["evm"; "sequencer"; "sqlite_compression"]
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
  Log.info "Starting sequencer without compression" ;
  let* sequencer = init_sequencer_sandbox () in
  let*@ _ = produce_block sequencer in
  Log.info "Deploy SimpleLogger so subsequent transactions emit real logs" ;
  let* logger = setup_simple_logger sequencer in
  Log.info "Sending transaction on uncompressed database" ;
  let* tx_hash_before =
    send_compression_test_logvalue ~logger ~value:0 sequencer
  in
  let* () = compression_check_rpcs sequencer ~tx_hash:tx_hash_before in

  Log.info "Stop, enable compression, run 'compress store' command" ;
  let* () = Evm_node.terminate sequencer in
  let* () =
    Evm_node.Config_file.update
      sequencer
      (patch_config_sqlite_compression (`Bool true))
  in
  let* () = Evm_node.compress_store sequencer in

  Log.info "Start node — pre-migration data still readable after the wrap" ;
  let* () = Evm_node.run sequencer in
  let* () = compression_check_rpcs sequencer ~tx_hash:tx_hash_before in
  Log.info "New transaction is written through zstd_compress" ;
  let* tx_hash_after =
    send_compression_test_logvalue ~logger ~value:1 sequencer
  in
  let* () = compression_check_rpcs sequencer ~tx_hash:tx_hash_after in

  Log.info
    "Stop, run 'compress store' again — fully-migrated DB should be a no-op" ;
  let* () = Evm_node.terminate sequencer in
  let* () = Evm_node.compress_store sequencer in

  Log.info "Both pre- and post-migration rows survive a clean restart" ;
  let* () = Evm_node.run sequencer in
  let* () = compression_check_rpcs sequencer ~tx_hash:tx_hash_before in
  let* () = compression_check_rpcs sequencer ~tx_hash:tx_hash_after in
  unit

let test_sqlite_compression_migration_resume () =
  Test.register
    ~__FILE__
    ~title:"SQLite compression migration resumes after crash"
    ~tags:["evm"; "sequencer"; "sqlite_compression"]
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
  Log.info "Pre-populate uncompressed DB with several blocks/txs" ;
  let* sequencer = init_sequencer_sandbox () in
  let* logger = setup_simple_logger sequencer in
  let* first_tx_hash =
    send_compression_test_logvalue ~logger ~value:0 sequencer
  in
  let* () =
    Lwt_list.iter_s
      (fun i ->
        let* _ =
          send_compression_test_logvalue ~logger ~value:(i + 1) sequencer
        in
        unit)
      (List.init 5 Fun.id)
  in
  let* () = Evm_node.terminate sequencer in

  Log.info "Enable compression with batch_size=1 to maximise progress events" ;
  let* () =
    Evm_node.Config_file.update
      sequencer
      (patch_config_sqlite_compression (`O [("batch_size", `Float 1.)]))
  in

  Log.info
    "Spawn 'compress store', kill it after a short delay to simulate a crash" ;
  let proc = Evm_node.spawn_compress_store sequencer in
  let* () = Lwt_unix.sleep 0.5 in
  Process.kill proc ;
  let* _status = Process.wait proc in

  Log.info "Run 'compress store' again — should resume and complete" ;
  let* () = Evm_node.compress_store sequencer in

  Log.info "Verify a pre-crash transaction is still readable" ;
  let* () = Evm_node.run sequencer in
  let* () = compression_check_rpcs sequencer ~tx_hash:first_tx_hash in
  unit

let test_sqlite_compression_disable_on_partial_db () =
  Test.register
    ~__FILE__
    ~title:"SQLite compression disabled on a partially-compressed DB"
    ~tags:["evm"; "sequencer"; "sqlite_compression"]
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
  Log.info "Start with compression enabled" ;
  let* sequencer =
    init_sequencer_sandbox
      ~patch_config:(patch_config_sqlite_compression (`Bool true))
      ()
  in
  let* logger = setup_simple_logger sequencer in
  let* tx_hash_compressed =
    send_compression_test_logvalue ~logger ~value:0 sequencer
  in

  Log.info "Stop, disable compression, restart" ;
  let* () = Evm_node.terminate sequencer in
  let* () =
    Evm_node.Config_file.update
      sequencer
      (patch_config_sqlite_compression (`Bool false))
  in
  let* () = Evm_node.run sequencer in
  (* Produce one empty block first: this settles the post-restart chain
     state and lets the tx pool catch up before we submit a transaction
     through eth-cli (which would otherwise race the produceBlock call). *)
  let*@ _ = produce_block sequencer in
  let* tx_hash_uncompressed =
    send_compression_test_logvalue ~logger ~value:1 sequencer
  in

  Log.info "Inspect on-disk receipt_fields for log-bearing rows" ;
  let* () = Evm_node.terminate sequencer in
  let data_dir = Evm_node.data_dir sequencer in
  let zstd_magic = "28B52FFD" in
  (* Row 1 is the [SimpleLogger] deployment from [setup_simple_logger];
     row 2 is the compressed [logValue] write before the flag flip;
     row 3 is the uncompressed [logValue] write after the flag flip. *)
  let* prefix_compressed =
    read_first_bytes_hex
      ~data_dir
      ~table:"transactions"
      ~column:"receipt_fields"
      ~rowid:2
      ~n:4
  in
  Check.((prefix_compressed = zstd_magic) string)
    ~error_msg:
      "row 2 (compressed write) should start with the zstd magic %R, got %L" ;
  let* prefix_uncompressed =
    read_first_bytes_hex
      ~data_dir
      ~table:"transactions"
      ~column:"receipt_fields"
      ~rowid:3
      ~n:4
  in
  Check.((prefix_uncompressed <> zstd_magic) string)
    ~error_msg:
      "row 3 (compression disabled) should not start with the zstd magic, got \
       %L" ;

  Log.info "Restart still disabled — both rows must read fine" ;
  let* () = Evm_node.run sequencer in
  let* () = compression_check_rpcs sequencer ~tx_hash:tx_hash_compressed in
  let* () = compression_check_rpcs sequencer ~tx_hash:tx_hash_uncompressed in
  unit

(* Exercises the SQL composition
   [receipt_contains_bloom_filter(zstd_decompress(...), ?)] from
   [Evm_store.Q.Transactions.receipts_of_block_range]: an [eth_getLogs]
   query goes through both extensions in order, so a wrong wiring (e.g.
   bloom filter applied to compressed bytes, or decompression bypassed
   for compressed rows) would either return zero results, return wrong
   results, or crash when the bloom UDF reads zstd-framed bytes as a
   bitmap. *)
let test_sqlite_compression_eth_get_logs () =
  Test.register
    ~__FILE__
    ~title:
      "SQLite compression: eth_getLogs with bloom filter on compressed receipts"
    ~tags:["evm"; "sequencer"; "sqlite_compression"; "bloom"; "eth_get_logs"]
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
  Log.info "Start sequencer with compression enabled from the start" ;
  let* sequencer =
    init_sequencer_sandbox
      ~patch_config:(patch_config_sqlite_compression (`Bool true))
      ()
  in
  let* ((logger_address, _) as logger) = setup_simple_logger sequencer in

  Log.info
    "Emit %d distinct LogValue events, interleaved with plain transfers"
    10 ;
  let log_values = List.init 10 Fun.id in
  let* () =
    Lwt_list.iter_s
      (fun i ->
        let* _ = send_compression_test_logvalue ~logger ~value:i sequencer in
        let* _ = send_compression_test_transfer sequencer in
        unit)
      log_values
  in

  Log.info "eth_getLogs with topic filter for value=5 returns exactly one log" ;
  let target_value = 5 in
  let*@ matched_logs =
    Rpc.get_logs
      ~from_block:(Number 0)
      ~address:(Single logger_address)
      ~topics:[[log_value_event_topic]; [hex_256_of_int target_value]]
      sequencer
  in
  (match matched_logs with
  | [log] ->
      let topics = log.Transaction.topics in
      Check.((List.length topics = 2) int ~__LOC__)
        ~error_msg:"expected 2 topics on the matched log, got %L" ;
      Check.((List.nth topics 0 = log_value_event_topic) string ~__LOC__)
        ~error_msg:"expected event-signature topic %R, got %L" ;
      Check.((List.nth topics 1 = hex_256_of_int target_value) string ~__LOC__)
        ~error_msg:"expected indexed value %R, got %L"
  | _ ->
      Test.fail
        "eth_getLogs with topic [LogValue, value=%d] should return exactly one \
         log, got %d"
        target_value
        (List.length matched_logs)) ;

  Log.info "eth_getLogs with a topic that no transaction emitted returns []" ;
  let absent_value = 999 in
  let*@ no_logs =
    Rpc.get_logs
      ~from_block:(Number 0)
      ~address:(Single logger_address)
      ~topics:[[log_value_event_topic]; [hex_256_of_int absent_value]]
      sequencer
  in
  Check.((List.length no_logs = 0) int ~__LOC__)
    ~error_msg:"eth_getLogs for an unemitted topic should be empty, got %L" ;

  Log.info "eth_getLogs across all topics returns one log per logValue call" ;
  let*@ all_logs =
    Rpc.get_logs
      ~from_block:(Number 0)
      ~address:(Single logger_address)
      ~topics:[[log_value_event_topic]]
      sequencer
  in
  Check.((List.length all_logs = List.length log_values) int ~__LOC__)
    ~error_msg:
      "eth_getLogs across the whole address+topic-0 filter should return %R \
       logs (one per logValue call), got %L" ;
  unit

let protocols = [Protocol.Alpha]

let () =
  test_get_balance_block_param protocols ;
  test_get_block_by_number_block_param protocols ;
  test_extended_block_param protocols ;
  test_rpc_getLogs_with_earliest_fail protocols ;
  test_apply_from_full_history_mode protocols ;
  test_durable_storage_consistency [Alpha] ;
  test_evm_events_cleanup () ;
  test_sqlite_compression_migration () ;
  test_sqlite_compression_migration_resume () ;
  test_sqlite_compression_disable_on_partial_db () ;
  test_sqlite_compression_eth_get_logs ()
