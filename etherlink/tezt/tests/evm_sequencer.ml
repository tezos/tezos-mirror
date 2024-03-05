(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Smart Optimistic Rollups: Etherlink Sequencer
   Requirement:  make -f kernels.mk build
                 npm install eth-cli
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file evm_sequencer.ml
*)

open Sc_rollup_helpers
open Rpc.Syntax
open Contract_path

module Sequencer_rpc = struct
  let get_blueprint sequencer number =
    Runnable.run
    @@ Curl.get
         ~args:["--fail"]
         (Evm_node.endpoint sequencer
         ^ "/evm/blueprint/" ^ Int64.to_string number)

  let get_smart_rollup_address sequencer =
    let* res =
      Runnable.run
      @@ Curl.get
           ~args:["--fail"]
           (Evm_node.endpoint sequencer ^ "/evm/smart_rollup_address")
    in
    return (JSON.as_string res)
end

let uses _protocol =
  [
    Constant.octez_smart_rollup_node;
    Constant.octez_evm_node;
    Constant.smart_rollup_installer;
    Constant.WASM.evm_kernel;
  ]

open Helpers

let base_fee_for_hardcoded_tx = Wei.to_wei_z @@ Z.of_int 21000

type l1_contracts = {
  delayed_transaction_bridge : string;
  exchanger : string;
  bridge : string;
  admin : string;
  sequencer_admin : string;
}

type sequencer_setup = {
  node : Node.t;
  client : Client.t;
  sc_rollup_address : string;
  sc_rollup_node : Sc_rollup_node.t;
  sequencer : Evm_node.t;
  proxy : Evm_node.t;
  l1_contracts : l1_contracts;
}

let setup_l1_contracts ?(dictator = Constant.bootstrap2) client =
  (* Originates the delayed transaction bridge. *)
  let* delayed_transaction_bridge =
    Client.originate_contract
      ~alias:"evm-seq-delayed-bridge"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.public_key_hash
      ~prg:(delayed_path ())
      ~burn_cap:Tez.one
      client
  in
  let* () = Client.bake_for_and_wait ~keys:[] client in
  (* Originates the exchanger. *)
  let* exchanger =
    Client.originate_contract
      ~alias:"exchanger"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.public_key_hash
      ~init:"Unit"
      ~prg:(exchanger_path ())
      ~burn_cap:Tez.one
      client
  in
  let* () = Client.bake_for_and_wait ~keys:[] client in
  (* Originates the bridge. *)
  let* bridge =
    Client.originate_contract
      ~alias:"evm-bridge"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.public_key_hash
      ~init:(sf "Pair %S None" exchanger)
      ~prg:(bridge_path ())
      ~burn_cap:Tez.one
      client
  in
  let* () = Client.bake_for_and_wait ~keys:[] client in
  (* Originates the administrator contract. *)
  let* admin =
    Client.originate_contract
      ~alias:"evm-admin"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.public_key_hash
      ~init:(sf "%S" dictator.Account.public_key_hash)
      ~prg:(admin_path ())
      ~burn_cap:Tez.one
      client
  in
  let* () = Client.bake_for_and_wait ~keys:[] client in
  (* Originates the administrator contract. *)
  let* sequencer_admin =
    Client.originate_contract
      ~alias:"evm-sequencer-admin"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.public_key_hash
      ~init:(sf "%S" dictator.Account.public_key_hash)
      ~prg:(admin_path ())
      ~burn_cap:Tez.one
      client
  in
  let* () = Client.bake_for_and_wait ~keys:[] client in
  return {delayed_transaction_bridge; exchanger; bridge; admin; sequencer_admin}

let setup_sequencer ?(devmode = true) ?config ?genesis_timestamp
    ?time_between_blocks ?max_blueprints_lag ?max_blueprints_catchup
    ?catchup_cooldown ?delayed_inbox_timeout ?delayed_inbox_min_levels
    ?(bootstrap_accounts = Eth_account.bootstrap_accounts)
    ?(sequencer = Constant.bootstrap1) ?(kernel = Constant.WASM.evm_kernel)
    ?minimum_base_fee_per_gas ?preimages_dir protocol =
  let* node, client = setup_l1 ?timestamp:genesis_timestamp protocol in
  let* l1_contracts = setup_l1_contracts client in
  let sc_rollup_node =
    Sc_rollup_node.create
      ~default_operator:Constant.bootstrap1.public_key_hash
      Batcher
      node
      ~base_dir:(Client.base_dir client)
  in
  let preimages_dir =
    Option.value
      ~default:(Sc_rollup_node.data_dir sc_rollup_node // "wasm_2_0_0")
      preimages_dir
  in
  let base_config =
    Configuration.make_config
      ~bootstrap_accounts
      ~sequencer:sequencer.public_key
      ~delayed_bridge:l1_contracts.delayed_transaction_bridge
      ~ticketer:l1_contracts.exchanger
      ~administrator:l1_contracts.admin
      ~sequencer_administrator:l1_contracts.sequencer_admin
      ?minimum_base_fee_per_gas
      ?delayed_inbox_timeout
      ?delayed_inbox_min_levels
      ()
  in
  let config =
    match (config, base_config) with
    | Some (`Config config), Some (`Config base) ->
        Some (`Config (base @ config))
    | Some (`Path path), Some (`Config base) -> Some (`Both (base, path))
    | None, _ -> base_config
    | Some (`Config config), None -> Some (`Config config)
    | Some (`Path path), None -> Some (`Path path)
  in
  let* {output; _} = prepare_installer_kernel ~preimages_dir ?config kernel in
  let* sc_rollup_address =
    originate_sc_rollup
      ~keys:[]
      ~kind:"wasm_2_0_0"
      ~boot_sector:("file:" ^ output)
      ~parameters_ty:Helpers.evm_type
      client
  in
  let* () =
    Sc_rollup_node.run sc_rollup_node sc_rollup_address [Log_kernel_debug]
  in
  let private_rpc_port = Port.fresh () in
  let mode =
    Evm_node.Sequencer
      {
        initial_kernel = output;
        preimage_dir = preimages_dir;
        private_rpc_port;
        time_between_blocks;
        sequencer = sequencer.alias;
        genesis_timestamp;
        max_blueprints_lag;
        max_blueprints_catchup;
        catchup_cooldown;
        devmode;
        wallet_dir = Some (Client.base_dir client);
      }
  in
  let* sequencer =
    Evm_node.init ~mode (Sc_rollup_node.endpoint sc_rollup_node)
  in
  let* proxy =
    Evm_node.init
      ~mode:(Proxy {devmode})
      (Sc_rollup_node.endpoint sc_rollup_node)
  in
  return
    {
      node;
      client;
      sequencer;
      proxy;
      l1_contracts;
      sc_rollup_address;
      sc_rollup_node;
    }

let send_raw_transaction_to_delayed_inbox ?(amount = Tez.one) ?expect_failure
    ~sc_rollup_node ~node ~client ~l1_contracts ~sc_rollup_address raw_tx =
  let expected_hash =
    `Hex raw_tx |> Hex.to_bytes |> Tezos_crypto.Hacl.Hash.Keccak_256.digest
    |> Hex.of_bytes |> Hex.show
  in
  let* () =
    Client.transfer
      ~arg:(sf "Pair %S 0x%s" sc_rollup_address raw_tx)
      ~amount
      ~giver:Constant.bootstrap2.public_key_hash
      ~receiver:l1_contracts.delayed_transaction_bridge
      ~burn_cap:Tez.one
      ?expect_failure
      client
  in
  let* () = Client.bake_for_and_wait ~keys:[] client in
  let* _ = next_rollup_node_level ~sc_rollup_node ~node ~client in
  Lwt.return expected_hash

let send_deposit_to_delayed_inbox ~amount ~l1_contracts ~depositor ~receiver
    ~sc_rollup_node ~sc_rollup_address ~node client =
  let* () =
    Client.transfer
      ~entrypoint:"deposit"
      ~arg:(sf "Pair %S %s" sc_rollup_address receiver)
      ~amount
      ~giver:depositor.Account.public_key_hash
      ~receiver:l1_contracts.bridge
      ~burn_cap:Tez.one
      client
  in
  let* _ = next_rollup_node_level ~sc_rollup_node ~node ~client in
  unit

let test_remove_sequencer =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "sequencer"; "admin"]
    ~title:"Remove sequencer via sequencer admin contract"
    ~uses
  @@ fun protocol ->
  let* {
         sequencer;
         proxy;
         sc_rollup_node;
         node;
         client;
         sc_rollup_address;
         l1_contracts;
         _;
       } =
    setup_sequencer ~time_between_blocks:Nothing protocol
  in
  (* Produce blocks to show that both the sequencer and proxy are not
     progressing. *)
  let* _ =
    repeat 5 (fun () ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~node ~client in
        unit)
  in
  (* Both are at genesis *)
  let*@ sequencer_head = Rpc.block_number sequencer in
  let*@ proxy_head = Rpc.block_number proxy in
  Check.((sequencer_head = 0l) int32)
    ~error_msg:"Sequencer should be at genesis" ;
  Check.((sequencer_head = proxy_head) int32)
    ~error_msg:"Sequencer and proxy should have the same block number" ;
  (* Remove the sequencer via the sequencer-admin contract. *)
  let* () =
    Client.transfer
      ~amount:Tez.zero
      ~giver:Constant.bootstrap2.public_key_hash
      ~receiver:l1_contracts.sequencer_admin
      ~arg:(sf "Pair %S 0x" sc_rollup_address)
      ~burn_cap:Tez.one
      client
  in
  let* _ = next_rollup_node_level ~sc_rollup_node ~node ~client in
  (* Produce L1 blocks to show that only the proxy is progressing *)
  let* _ =
    repeat 5 (fun () ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~node ~client in
        unit)
  in
  (* Sequencer is at genesis, proxy is at [advance]. *)
  let*@ sequencer_head = Rpc.block_number sequencer in
  let*@ proxy_head = Rpc.block_number proxy in
  Check.((sequencer_head = 0l) int32)
    ~error_msg:"Sequencer should still be at genesis" ;
  Check.((proxy_head > 0l) int32) ~error_msg:"Proxy should have advanced" ;

  unit

let test_persistent_state =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "sequencer"]
    ~title:"Sequencer state is persistent across runs"
    ~uses
  @@ fun protocol ->
  let* {sequencer; _} = setup_sequencer protocol in
  (* Force the sequencer to produce a block. *)
  let* _ = Rpc.produce_block sequencer in
  (* Ask for the current block. *)
  let*@ block_number = Rpc.block_number sequencer in
  Check.is_true
    ~__LOC__
    (block_number > 0l)
    ~error_msg:"The sequencer should have produced a block" ;
  (* Terminate the sequencer. *)
  let* () = Evm_node.terminate sequencer in
  (* Restart it. *)
  let* () = Evm_node.run sequencer in
  (* Assert the block number is at least [block_number]. Asserting
     that the block number is exactly the same as {!block_number} can
     be flaky if a block is produced between the restart and the
     RPC. *)
  let*@ new_block_number = Rpc.block_number sequencer in
  Check.is_true
    ~__LOC__
    (new_block_number >= block_number)
    ~error_msg:"The sequencer should have produced a block" ;
  unit

let test_publish_blueprints =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "sequencer"; "data"]
    ~title:"Sequencer publishes the blueprints to L1"
    ~uses
  @@ fun protocol ->
  let* {sequencer; proxy; node; client; sc_rollup_node; _} =
    setup_sequencer ~time_between_blocks:Nothing protocol
  in
  let* _ =
    repeat 5 (fun () ->
        let* _ = Rpc.produce_block sequencer in
        unit)
  in

  let* () = Evm_node.wait_for_blueprint_injected ~timeout:5. sequencer 5 in

  (* Ask for the current block. *)
  let*@ sequencer_head = Rpc.get_block_by_number ~block:"latest" sequencer in

  (* At this point, the evm node should called the batcher endpoint to publish
     all the blueprints. Stopping the node is then not a problem. *)
  let* () =
    repeat 10 (fun () ->
        let* _ = next_rollup_node_level ~node ~client ~sc_rollup_node in
        unit)
  in

  (* We have unfortunately noticed that the test can be flaky. Sometimes,
     the following RPC is done before the proxy being initialised, even though
     we wait for it. The source of flakiness is unknown but happens very rarely,
     we put a small sleep to make the least flaky possible. *)
  let* () = Lwt_unix.sleep 2. in
  let*@ rollup_head = Rpc.get_block_by_number ~block:"latest" proxy in
  Check.((sequencer_head.hash = rollup_head.hash) string)
    ~error_msg:"Expected the same head on the rollup node and the sequencer" ;
  unit

let test_resilient_to_rollup_node_disconnect =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "sequencer"; "data"]
    ~title:"Sequencer is resilient to rollup node disconnection"
    ~uses
  @@ fun protocol ->
  (* The objective of this test is to show that the sequencer can deal with
     rollup node outage. The logic of the sequencer at the moment is to
     wait for its advance on the rollup node to be more than [max_blueprints_lag]
     before sending at most [max_blueprints_catchup] blueprints. The sequencer
     waits for [catchup_cooldown] L1 blocks before checking if it needs to push
     new blueprints again. This scenario checks this logic. *)
  let max_blueprints_lag = 10 in
  let max_blueprints_catchup = max_blueprints_lag - 3 in
  let catchup_cooldown = 10 in
  let first_batch_blueprints_count = 5 in
  let ensure_rollup_node_publish = 5 in

  let* {sequencer; proxy; sc_rollup_node; sc_rollup_address; node; client; _} =
    setup_sequencer
      ~max_blueprints_lag
      ~max_blueprints_catchup
      ~catchup_cooldown
      ~time_between_blocks:Nothing
      protocol
  in

  (* Produce blueprints *)
  let* _ =
    repeat first_batch_blueprints_count (fun () ->
        let* _ = Rpc.produce_block sequencer in
        unit)
  in
  let* () =
    Evm_node.wait_for_blueprint_injected
      ~timeout:(float_of_int first_batch_blueprints_count)
      sequencer
      first_batch_blueprints_count
  in

  (* Produce some L1 blocks so that the rollup node publishes the blueprints. *)
  let* _ =
    repeat ensure_rollup_node_publish (fun () ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~node ~client in
        unit)
  in

  (* Check sequencer and rollup consistency *)
  let* () =
    check_head_consistency
      ~error_msg:"The head should be the same before the outage"
      ~left:sequencer
      ~right:proxy
      ()
  in

  (* Kill the rollup node *)
  let* () = Sc_rollup_node.kill sc_rollup_node in

  (* The sequencer node should keep producing blocks, enough so that
     it cannot catchup in one go. *)
  let* _ =
    repeat (2 * max_blueprints_lag) (fun () ->
        let* _ = Rpc.produce_block sequencer in
        unit)
  in

  let* () =
    Evm_node.wait_for_blueprint_applied
      sequencer
      ~timeout:5.
      (first_batch_blueprints_count + (2 * max_blueprints_lag))
  in

  (* Kill the sequencer node, restart the rollup node, restart the sequencer to
     reestablish the connection *)
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup_address [] in
  let* () = Sc_rollup_node.wait_for_ready sc_rollup_node in

  let* () = Evm_node.terminate sequencer in
  let* () = Evm_node.run sequencer in
  let* () = Evm_node.wait_for_ready sequencer in

  (* Produce enough blocks in advance to ensure the sequencer node will catch
     up at the end. *)
  let* _ =
    repeat max_blueprints_lag (fun () ->
        let* _ = Rpc.produce_block sequencer in
        unit)
  in

  let* () =
    Evm_node.wait_for_blueprint_applied
      sequencer
      ~timeout:5.
      (first_batch_blueprints_count + (2 * max_blueprints_catchup) + 1)
  in

  (* Give some time for the sequencer node to inject the first round of
     blueprints *)
  let* _ =
    repeat ensure_rollup_node_publish (fun () ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~node ~client in
        unit)
  in

  let*@ rollup_node_head = Rpc.get_block_by_number ~block:"latest" proxy in
  Check.(
    (rollup_node_head.number
    = Int32.(of_int (first_batch_blueprints_count + max_blueprints_catchup)))
      int32)
    ~error_msg:
      "The rollup node should have received the first round of lost blueprints" ;

  (* Go through several cooldown periods to let the sequencer sends the rest of
     the blueprints. *)
  let* _ =
    repeat (2 * catchup_cooldown) (fun () ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~node ~client in
        unit)
  in

  (* Check the consistency again *)
  let* () =
    check_head_consistency
      ~error_msg:"The head should be the same after the outage"
      ~left:sequencer
      ~right:proxy
      ()
  in

  unit

let test_can_fetch_blueprint =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "sequencer"; "data"]
    ~title:"Sequencer can provide blueprints on demand"
    ~uses
  @@ fun protocol ->
  let* {sequencer; _} = setup_sequencer ~time_between_blocks:Nothing protocol in
  let number_of_blocks = 5 in
  let* _ =
    repeat number_of_blocks (fun () ->
        let* _ = Rpc.produce_block sequencer in
        unit)
  in

  let* () = Evm_node.wait_for_blueprint_injected ~timeout:5. sequencer 5 in

  let* blueprints =
    fold number_of_blocks [] (fun i acc ->
        let* blueprint =
          Sequencer_rpc.get_blueprint sequencer Int64.(of_int @@ (i + 1))
        in
        return (blueprint :: acc))
  in

  (* Test for uniqueness  *)
  let blueprints_uniq =
    List.sort_uniq
      (fun b1 b2 -> String.compare (JSON.encode b1) (JSON.encode b2))
      blueprints
  in
  if List.length blueprints = List.length blueprints_uniq then unit
  else
    Test.fail
      ~__LOC__
      "At least two blueprints from a different level are equal."

let test_can_fetch_smart_rollup_address =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "sequencer"; "rpc"]
    ~title:"Sequencer can return the smart rollup address on demand"
    ~uses
  @@ fun protocol ->
  let* {sequencer; sc_rollup_address; _} =
    setup_sequencer ~time_between_blocks:Nothing protocol
  in
  let* claimed_address = Sequencer_rpc.get_smart_rollup_address sequencer in

  Check.((sc_rollup_address = claimed_address) string)
    ~error_msg:"Returned address is not the expected one" ;

  unit

let test_send_transaction_to_delayed_inbox =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "sequencer"; "delayed_inbox"]
    ~title:"Send a transaction to the delayed inbox"
    ~uses
  @@ fun protocol ->
  (* Start the evm node *)
  let* {client; node; l1_contracts; sc_rollup_address; sc_rollup_node; _} =
    setup_sequencer protocol
  in
  let raw_transfer =
    "f86d80843b9aca00825b0494b53dc01974176e5dff2298c5a94343c2585e3c54880de0b6b3a764000080820a96a07a3109107c6bd1d555ce70d6253056bc18996d4aff4d4ea43ff175353f49b2e3a05f9ec9764dc4a3c3ab444debe2c3384070de9014d44732162bb33ee04da187ef"
  in
  let send ~amount ?expect_failure () =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      ~node
      ~amount
      ?expect_failure
      raw_transfer
  in
  (* Test that paying less than 1XTZ is not allowed. *)
  let* _hash =
    send ~amount:(Tez.parse_floating "0.9") ~expect_failure:true ()
  in
  (* Test the correct case where the user burns 1XTZ to send the transaction. *)
  let* hash = send ~amount:Tez.one ~expect_failure:false () in
  (* Assert that the expected transaction hash is found in the delayed inbox
     durable storage path. *)
  let* delayed_transactions_hashes =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind:"wasm_2_0_0"
         ~operation:Sc_rollup_rpc.Subkeys
         ~key:"/evm/delayed-inbox"
         ()
  in
  Check.(list_mem string hash delayed_transactions_hashes)
    ~error_msg:"hash %L should be present in the delayed inbox %R" ;
  (* Test that paying more than 1XTZ is allowed. *)
  let* _hash =
    send ~amount:(Tez.parse_floating "1.1") ~expect_failure:false ()
  in
  unit

let test_send_deposit_to_delayed_inbox =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "deposit"]
    ~title:"Send a deposit to the delayed inbox"
    ~uses
  @@ fun protocol ->
  let* {client; node; l1_contracts; sc_rollup_address; sc_rollup_node; _} =
    setup_sequencer protocol
  in
  let amount = Tez.of_int 16 in
  let depositor = Constant.bootstrap5 in
  let receiver =
    Eth_account.
      {
        address = "0x1074Fd1EC02cbeaa5A90450505cF3B48D834f3EB";
        private_key =
          "0xb7c548b5442f5b28236f0dcd619f65aaaafd952240908adcf9642d8e616587ee";
        public_key =
          "0466ed90f9a86c0908746475fbe0a40c72237de22d89076302e22c2a8da259b4aba5c7ee1f3dc3fd0b240645462620ae62b6fe8fe5b3464c3b1b4ae6c06c97b7b6";
      }
  in
  let* () =
    send_deposit_to_delayed_inbox
      ~amount
      ~l1_contracts
      ~depositor
      ~receiver:receiver.address
      ~sc_rollup_node
      ~sc_rollup_address
      ~node
      client
  in
  let* delayed_transactions_hashes =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind:"wasm_2_0_0"
         ~operation:Sc_rollup_rpc.Subkeys
         ~key:"/evm/delayed-inbox"
         ()
  in
  Check.(
    list_mem
      string
      "a07feb67aff94089c8d944f5f8ffb5acc37306da9102fc310264e90999a42eb1"
      delayed_transactions_hashes)
    ~error_msg:"the deposit is not present in the delayed inbox" ;
  unit

let test_rpc_produceBlock =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "sequencer"; "produce_block"]
    ~title:"RPC method produceBlock"
    ~uses
  @@ fun protocol ->
  (* Set a large [time_between_blocks] to make sure the block production is
     triggered by the RPC call. *)
  let* {sequencer; _} = setup_sequencer ~time_between_blocks:Nothing protocol in
  let*@ start_block_number = Rpc.block_number sequencer in
  let* _ = Rpc.produce_block sequencer in
  let*@ new_block_number = Rpc.block_number sequencer in
  Check.((Int32.succ start_block_number = new_block_number) int32)
    ~error_msg:"Expected new block number to be %L, but got: %R" ;
  unit

let wait_for_event ?(levels = 10) event_watcher ~sequencer ~sc_rollup_node ~node
    ~client ~error_msg =
  let event_value = ref None in
  let _ =
    let* return_value = event_watcher in
    event_value := Some return_value ;
    unit
  in
  let rec rollup_node_loop n =
    if n = 0 then Test.fail error_msg
    else
      let* _ = next_rollup_node_level ~sc_rollup_node ~node ~client in
      let* _ = Rpc.produce_block sequencer in
      match !event_value with
      | Some value -> return value
      | None -> rollup_node_loop (n - 1)
  in
  Lwt.pick [rollup_node_loop levels]

let wait_for_delayed_inbox_add_tx_and_injected ~sequencer ~sc_rollup_node ~node
    ~client =
  let event_watcher =
    let added =
      Evm_node.wait_for sequencer "delayed_inbox_add_transaction.v0"
      @@ fun json ->
      let hash = JSON.(json |-> "hash" |> as_string) in
      Some hash
    in
    let injected =
      Evm_node.wait_for sequencer "tx_pool_transaction_injected.v0"
      @@ fun json ->
      let hash = JSON.(json |> as_string) in
      Some hash
    in
    let* added_hash, injected_hash = Lwt.both added injected in
    Check.((added_hash = injected_hash) string)
      ~error_msg:"Injected hash %R is not the expected one %L" ;
    Lwt.return_unit
  in
  wait_for_event
    event_watcher
    ~sequencer
    ~sc_rollup_node
    ~node
    ~client
    ~error_msg:
      "Timed out while waiting for transaction to be added to the delayed \
       inbox and injected"

let wait_for_delayed_inbox_fetch ~sequencer ~sc_rollup_node ~node ~client =
  let event_watcher =
    Evm_node.wait_for sequencer "delayed_inbox_fetch_succeeded.v0"
    @@ fun json ->
    let nb = JSON.(json |-> "nb" |> as_int) in
    Some nb
  in
  wait_for_event
    event_watcher
    ~sequencer
    ~sc_rollup_node
    ~node
    ~client
    ~error_msg:"Timed out while waiting for delayed inbox to be fetched"

let wait_until_delayed_inbox_is_empty ~sequencer ~sc_rollup_node ~node ~client =
  let levels = 10 in
  let rec go n =
    if n = 0 then
      Test.fail "Timed out waiting for the delayed inbox to be empty"
    else
      let* nb =
        wait_for_delayed_inbox_fetch ~sequencer ~sc_rollup_node ~node ~client
      in
      if nb = 0 then Lwt.return_unit else go (n - 1)
  in
  go levels

let test_delayed_transfer_is_included =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "inclusion"]
    ~title:"Delayed transaction is included"
    ~uses
  @@ fun protocol ->
  (* Start the evm node *)
  let* {
         client;
         node;
         l1_contracts;
         sc_rollup_address;
         sc_rollup_node;
         sequencer;
         _;
       } =
    setup_sequencer protocol
  in
  let endpoint = Evm_node.endpoint sequencer in
  (* This is a transfer from Eth_account.bootstrap_accounts.(0) to
     Eth_account.bootstrap_accounts.(1). *)
  let raw_transfer =
    "f86d80843b9aca00825b0494b53dc01974176e5dff2298c5a94343c2585e3c54880de0b6b3a764000080820a96a07a3109107c6bd1d555ce70d6253056bc18996d4aff4d4ea43ff175353f49b2e3a05f9ec9764dc4a3c3ab444debe2c3384070de9014d44732162bb33ee04da187ef"
  in
  let sender = Eth_account.bootstrap_accounts.(0).address in
  let receiver = Eth_account.bootstrap_accounts.(1).address in
  let* sender_balance_prev = Eth_cli.balance ~account:sender ~endpoint in
  let* receiver_balance_prev = Eth_cli.balance ~account:receiver ~endpoint in
  let* _hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      ~node
      raw_transfer
  in
  let* () =
    wait_for_delayed_inbox_add_tx_and_injected
      ~sequencer
      ~sc_rollup_node
      ~node
      ~client
  in
  let* () =
    wait_until_delayed_inbox_is_empty ~sequencer ~sc_rollup_node ~node ~client
  in
  let* sender_balance_next = Eth_cli.balance ~account:sender ~endpoint in
  let* receiver_balance_next = Eth_cli.balance ~account:receiver ~endpoint in
  Check.((sender_balance_prev <> sender_balance_next) Wei.typ)
    ~error_msg:"Balance should be updated" ;
  Check.((receiver_balance_prev <> receiver_balance_next) Wei.typ)
    ~error_msg:"Balance should be updated" ;
  Check.((sender_balance_prev > sender_balance_next) Wei.typ)
    ~error_msg:"Expected a smaller balance" ;
  Check.((receiver_balance_next > receiver_balance_prev) Wei.typ)
    ~error_msg:"Expected a bigger balance" ;
  unit

let test_delayed_deposit_is_included =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "inclusion"; "deposit"]
    ~title:"Delayed deposit is included"
    ~uses
  @@ fun protocol ->
  (* Start the evm node *)
  let* {
         client;
         node;
         l1_contracts;
         sc_rollup_address;
         sc_rollup_node;
         sequencer;
         _;
       } =
    setup_sequencer protocol
  in
  let endpoint = Evm_node.endpoint sequencer in

  let amount = Tez.of_int 16 in
  let depositor = Constant.bootstrap5 in
  let receiver =
    Eth_account.
      {
        address = "0x1074Fd1EC02cbeaa5A90450505cF3B48D834f3EB";
        private_key =
          "0xb7c548b5442f5b28236f0dcd619f65aaaafd952240908adcf9642d8e616587ee";
        public_key =
          "0466ed90f9a86c0908746475fbe0a40c72237de22d89076302e22c2a8da259b4aba5c7ee1f3dc3fd0b240645462620ae62b6fe8fe5b3464c3b1b4ae6c06c97b7b6";
      }
  in
  let* receiver_balance_prev =
    Eth_cli.balance ~account:receiver.address ~endpoint
  in
  let* () =
    send_deposit_to_delayed_inbox
      ~amount
      ~l1_contracts
      ~depositor
      ~receiver:receiver.address
      ~sc_rollup_node
      ~sc_rollup_address
      ~node
      client
  in
  let* () =
    wait_for_delayed_inbox_add_tx_and_injected
      ~sequencer
      ~sc_rollup_node
      ~node
      ~client
  in
  let* () =
    wait_until_delayed_inbox_is_empty ~sequencer ~sc_rollup_node ~node ~client
  in
  let* receiver_balance_next =
    Eth_cli.balance ~account:receiver.address ~endpoint
  in
  Check.((receiver_balance_next > receiver_balance_prev) Wei.typ)
    ~error_msg:"Expected a bigger balance" ;
  unit

(** test to initialise a sequencer data dir based on a rollup node
        data dir *)
let test_init_from_rollup_node_data_dir =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "rollup_node"; "init"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
      ])
    ~title:"Init evm node sequencer data dir from a rollup node data dir"
  @@ fun protocol ->
  let* {sc_rollup_node; sequencer; proxy; client; node; _} =
    setup_sequencer ~time_between_blocks:Nothing protocol
  in
  (* a sequencer is needed to produce an initial block *)
  let* () =
    repeat 5 (fun () ->
        let* _ = Rpc.produce_block sequencer in
        unit)
  in
  let* () =
    repeat 5 (fun () ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~node ~client in
        unit)
  in
  let* () = Evm_node.terminate sequencer in
  let evm_node' =
    Evm_node.create
      ~mode:(Evm_node.mode sequencer)
      (Sc_rollup_node.endpoint sc_rollup_node)
  in
  let* () =
    Evm_node.init_from_rollup_node_data_dir
      ~devmode:true
      evm_node'
      sc_rollup_node
  in
  let* () = Evm_node.run evm_node' in

  let* () = check_head_consistency ~left:evm_node' ~right:proxy () in

  let* _ = Rpc.produce_block evm_node' in
  let* () =
    repeat 5 (fun () ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~node ~client in
        unit)
  in

  let* () = check_head_consistency ~left:evm_node' ~right:proxy () in

  unit

let test_observer_applies_blueprint =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "observer"]
    ~title:"Can start an Observer node"
    ~uses
  @@ fun protocol ->
  (* Start the evm node *)
  let tbb = 1. in
  let* {sequencer = sequencer_node; sc_rollup_node; _} =
    setup_sequencer ~time_between_blocks:(Time_between_blocks tbb) protocol
  in
  let preimage_dir = Sc_rollup_node.data_dir sc_rollup_node // "wasm_2_0_0" in
  let observer_node =
    Evm_node.create
      ~mode:
        (Observer
           {
             initial_kernel = Evm_node.initial_kernel sequencer_node;
             preimage_dir;
           })
      (Evm_node.endpoint sequencer_node)
  in
  let* () = Evm_node.run observer_node in
  let* () = Evm_node.wait_for_ready observer_node in
  let levels_to_wait = 3 in
  let timeout = tbb *. float_of_int levels_to_wait *. 2. in

  let* _ =
    Lwt.both
      (Evm_node.wait_for_blueprint_applied
         ~timeout
         observer_node
         levels_to_wait)
      (Evm_node.wait_for_blueprint_applied
         ~timeout
         sequencer_node
         levels_to_wait)
  in

  let* () =
    check_head_consistency ~left:sequencer_node ~right:observer_node ()
  in

  unit

let test_observer_forwards_transaction =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "observer"; "transaction"]
    ~title:"Observer forwards transaction"
    ~uses
  @@ fun protocol ->
  (* Start the evm node *)
  let tbb = 1. in
  let* {sequencer = sequencer_node; sc_rollup_node; _} =
    setup_sequencer ~time_between_blocks:(Time_between_blocks tbb) protocol
  in
  let preimage_dir = Sc_rollup_node.data_dir sc_rollup_node // "wasm_2_0_0" in
  let* observer_node =
    Evm_node.init
      ~mode:
        (Observer
           {
             initial_kernel = Evm_node.initial_kernel sequencer_node;
             preimage_dir;
           })
      (Evm_node.endpoint sequencer_node)
  in

  (* Ensure the observer node has a correctly initialized local state. *)
  let* () = Evm_node.wait_for_blueprint_applied ~timeout:3.0 observer_node 1 in

  let* txn =
    Eth_cli.transaction_send
      ~source_private_key:Eth_account.bootstrap_accounts.(1).private_key
      ~to_public_key:Eth_account.bootstrap_accounts.(2).address
      ~value:Wei.one
      ~endpoint:(Evm_node.endpoint observer_node)
      ()
  in

  let* receipt =
    Eth_cli.get_receipt ~endpoint:(Evm_node.endpoint sequencer_node) ~tx:txn
  in

  match receipt with
  | Some receipt when receipt.status -> unit
  | Some _ ->
      Test.fail
        "transaction receipt received from the sequenecer, but transaction \
         failed"
  | None ->
      Test.fail
        "Missing receipt in the sequencer node for transaction successfully \
         injected in the observer"

(** This tests the situation where the kernel has an upgrade and the
    sequencer upgrade by following the event of the kernel. *)
let test_upgrade_kernel_auto_sync =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "sequencer"; "upgrade"; "auto"; "sync"]
    ~title:"Rollup-node kernel upgrade is applied to the sequencer state."
    ~uses:(fun protocol -> Constant.WASM.debug_kernel :: uses protocol)
  @@ fun protocol ->
  (* Add a delay between first block and activation timestamp. *)
  let genesis_timestamp =
    Client.(At (Time.of_notation_exn "2020-01-01T00:00:00Z"))
  in
  let activation_timestamp = "2020-01-01T00:00:10Z" in

  let* {
         sc_rollup_node;
         l1_contracts;
         sc_rollup_address;
         client;
         sequencer;
         proxy;
         node;
         _;
       } =
    setup_sequencer ~genesis_timestamp ~time_between_blocks:Nothing protocol
  in
  (* Sends the upgrade to L1, but not to the sequencer. *)
  let* () =
    upgrade
      ~sc_rollup_node
      ~sc_rollup_address
      ~admin:Constant.bootstrap2.public_key_hash
      ~admin_contract:l1_contracts.admin
      ~client
      ~upgrade_to:Constant.WASM.debug_kernel
      ~activation_timestamp
  in

  (* Per the activation timestamp, the state will remain synchronised until
     the kernel is upgraded. *)
  let* _ =
    repeat 2 (fun () ->
        let* _ =
          Rpc.produce_block ~timestamp:"2020-01-01T00:00:05Z" sequencer
        in
        unit)
  in
  let* () =
    repeat 4 (fun () ->
        let* _ = next_rollup_node_level ~node ~client ~sc_rollup_node in
        unit)
  in

  let* () =
    check_head_consistency
      ~left:sequencer
      ~right:proxy
      ~error_msg:"The head should be the same before the upgrade"
      ()
  in

  (* Produce a block after activation timestamp, both the rollup
     node and the sequencer will upgrade to debug kernel and
     therefore not produce the block. *)
  let* _ =
    repeat 2 (fun () ->
        let* _ =
          Rpc.produce_block ~timestamp:"2020-01-01T00:00:15Z" sequencer
        in
        unit)
  in
  let* () =
    repeat 4 (fun () ->
        let* _ = next_rollup_node_level ~node ~client ~sc_rollup_node in
        unit)
  in

  let* () =
    check_head_consistency
      ~left:sequencer
      ~right:proxy
      ~error_msg:"The head should be the same after the upgrade"
      ()
  in

  unit

let test_delayed_transfer_timeout =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "timeout"]
    ~title:"Delayed transaction timeout"
    ~uses
  @@ fun protocol ->
  (* Start the evm node *)
  let* {
         client;
         node;
         l1_contracts;
         sc_rollup_address;
         sc_rollup_node;
         sequencer;
         proxy;
       } =
    setup_sequencer
      ~delayed_inbox_timeout:3
      ~delayed_inbox_min_levels:1
      protocol
  in
  (* Kill the sequencer *)
  let* () = Evm_node.terminate sequencer in
  let endpoint = Evm_node.endpoint proxy in
  let* _ = next_rollup_node_level ~sc_rollup_node ~node ~client in
  let sender = Eth_account.bootstrap_accounts.(0).address in
  let _ = Rpc.block_number proxy in
  let receiver = Eth_account.bootstrap_accounts.(1).address in
  let* sender_balance_prev = Eth_cli.balance ~account:sender ~endpoint in
  let* receiver_balance_prev = Eth_cli.balance ~account:receiver ~endpoint in
  (* This is a transfer from Eth_account.bootstrap_accounts.(0) to
     Eth_account.bootstrap_accounts.(1). *)
  let raw_transfer =
    "f86d80843b9aca00825b0494b53dc01974176e5dff2298c5a94343c2585e3c54880de0b6b3a764000080820a96a07a3109107c6bd1d555ce70d6253056bc18996d4aff4d4ea43ff175353f49b2e3a05f9ec9764dc4a3c3ab444debe2c3384070de9014d44732162bb33ee04da187ef"
  in
  let* _hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      ~node
      raw_transfer
  in
  (* Bake a few blocks, should be enough for the tx to time out and be
     forced *)
  let* _ =
    repeat 5 (fun () ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~node ~client in
        unit)
  in
  let* sender_balance_next = Eth_cli.balance ~account:sender ~endpoint in
  let* receiver_balance_next = Eth_cli.balance ~account:receiver ~endpoint in
  Check.((sender_balance_prev <> sender_balance_next) Wei.typ)
    ~error_msg:"Balance should be updated" ;
  Check.((receiver_balance_prev <> receiver_balance_next) Wei.typ)
    ~error_msg:"Balance should be updated" ;
  Check.((sender_balance_prev > sender_balance_next) Wei.typ)
    ~error_msg:"Expected a smaller balance" ;
  Check.((receiver_balance_next > receiver_balance_prev) Wei.typ)
    ~error_msg:"Expected a bigger balance" ;
  unit

let test_delayed_transfer_timeout_fails_l1_levels =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "timeout"; "min_levels"]
    ~title:"Delayed transaction timeout considers l1 level"
    ~uses
  @@ fun protocol ->
  let* {
         client;
         node;
         l1_contracts;
         sc_rollup_address;
         sc_rollup_node;
         sequencer;
         proxy;
       } =
    setup_sequencer
      ~delayed_inbox_timeout:3
      ~delayed_inbox_min_levels:20
      protocol
  in
  (* Kill the sequencer *)
  let* () = Evm_node.terminate sequencer in
  let endpoint = Evm_node.endpoint proxy in
  let* _ = next_rollup_node_level ~sc_rollup_node ~node ~client in
  let sender = Eth_account.bootstrap_accounts.(0).address in
  let _ = Rpc.block_number proxy in
  let receiver = Eth_account.bootstrap_accounts.(1).address in
  let* sender_balance_prev = Eth_cli.balance ~account:sender ~endpoint in
  let* receiver_balance_prev = Eth_cli.balance ~account:receiver ~endpoint in
  (* This is a transfer from Eth_account.bootstrap_accounts.(0) to
     Eth_account.bootstrap_accounts.(1). *)
  let raw_transfer =
    "f86d80843b9aca00825b0494b53dc01974176e5dff2298c5a94343c2585e3c54880de0b6b3a764000080820a96a07a3109107c6bd1d555ce70d6253056bc18996d4aff4d4ea43ff175353f49b2e3a05f9ec9764dc4a3c3ab444debe2c3384070de9014d44732162bb33ee04da187ef"
  in
  let* _hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      ~node
      raw_transfer
  in
  (* Bake a few blocks, should be enough for the tx to time out in terms
     of wall time, but not in terms of L1 levels.
     Note that this test is almost the same as the one where the tx
     times out, only difference being the value of [delayed_inbox_min_levels].
  *)
  let* _ =
    repeat 5 (fun () ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~node ~client in
        unit)
  in
  let* sender_balance_next = Eth_cli.balance ~account:sender ~endpoint in
  let* receiver_balance_next = Eth_cli.balance ~account:receiver ~endpoint in
  Check.((sender_balance_prev = sender_balance_next) Wei.typ)
    ~error_msg:"Balance should be the same" ;
  Check.((receiver_balance_prev = receiver_balance_next) Wei.typ)
    ~error_msg:"Balance should be same" ;
  Check.((sender_balance_prev = sender_balance_next) Wei.typ)
    ~error_msg:"Expected equal balance" ;
  Check.((receiver_balance_next = receiver_balance_prev) Wei.typ)
    ~error_msg:"Expected equal balance" ;
  (* Wait until it's forced *)
  let* _ =
    repeat 15 (fun () ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~node ~client in
        unit)
  in
  let* sender_balance_next = Eth_cli.balance ~account:sender ~endpoint in
  let* receiver_balance_next = Eth_cli.balance ~account:receiver ~endpoint in
  Check.((sender_balance_prev <> sender_balance_next) Wei.typ)
    ~error_msg:"Balance should be updated" ;
  Check.((receiver_balance_prev <> receiver_balance_next) Wei.typ)
    ~error_msg:"Balance should be updated" ;
  Check.((sender_balance_prev > sender_balance_next) Wei.typ)
    ~error_msg:"Expected a smaller balance" ;
  Check.((receiver_balance_next > receiver_balance_prev) Wei.typ)
    ~error_msg:"Expected a bigger balance" ;
  unit

(** This tests the situation where force kernel upgrade happens too soon. *)
let test_force_kernel_upgrade_too_early =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "sequencer"; "upgrade"; "force"]
    ~title:"Force kernel upgrade fail too early"
    ~uses:(fun protocol -> Constant.WASM.ghostnet_evm_kernel :: uses protocol)
  @@ fun protocol ->
  (* Add a delay between first block and activation timestamp. *)
  let genesis_timestamp =
    Client.(At (Time.of_notation_exn "2020-01-10T00:00:00Z"))
  in
  let* {
         sc_rollup_node;
         l1_contracts;
         sc_rollup_address;
         client;
         sequencer;
         node;
         _;
       } =
    setup_sequencer ~genesis_timestamp ~time_between_blocks:Nothing protocol
  in
  (* Wait for the sequencer to publish its genesis block. *)
  let* () =
    repeat 3 (fun () ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~node ~client in
        unit)
  in
  let* proxy =
    Evm_node.init
      ~mode:(Proxy {devmode = true})
      (Sc_rollup_node.endpoint sc_rollup_node)
  in

  (* Assert the kernel version is the same at start up. *)
  let*@ sequencer_kernelVersion = Rpc.tez_kernelVersion sequencer in
  let*@ proxy_kernelVersion = Rpc.tez_kernelVersion proxy in
  Check.((sequencer_kernelVersion = proxy_kernelVersion) string)
    ~error_msg:"Kernel versions should be the same at start up" ;

  (* Activation timestamp is 1 day after the genesis. Therefore, it cannot
     be forced now. *)
  let activation_timestamp = "2020-01-11T00:00:00Z" in
  (* Sends the upgrade to L1 and sequencer. *)
  let* () =
    upgrade
      ~sc_rollup_node
      ~sc_rollup_address
      ~admin:Constant.bootstrap2.public_key_hash
      ~admin_contract:l1_contracts.admin
      ~client
      ~upgrade_to:Constant.WASM.ghostnet_evm_kernel
      ~activation_timestamp
  in

  (* Now we try force the kernel upgrade via an external message. *)
  let* () =
    force_kernel_upgrade ~sc_rollup_address ~sc_rollup_node ~node ~client
  in

  (* Assert the kernel version are still the same. *)
  let*@ sequencer_kernelVersion = Rpc.tez_kernelVersion sequencer in
  let*@ new_proxy_kernelVersion = Rpc.tez_kernelVersion proxy in
  Check.((sequencer_kernelVersion = new_proxy_kernelVersion) string)
    ~error_msg:"The force kernel ugprade should have failed" ;
  unit

(** This tests the situation where the kernel does not produce blocks but
    still can be forced to upgrade via an external message. *)
let test_force_kernel_upgrade =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "sequencer"; "upgrade"; "force"]
    ~title:"Force kernel upgrade"
    ~uses:(fun protocol -> Constant.WASM.ghostnet_evm_kernel :: uses protocol)
  @@ fun protocol ->
  (* Add a delay between first block and activation timestamp. *)
  let genesis_timestamp =
    Client.(At (Time.of_notation_exn "2020-01-10T00:00:00Z"))
  in
  let* {
         sc_rollup_node;
         l1_contracts;
         sc_rollup_address;
         client;
         sequencer;
         node;
         _;
       } =
    setup_sequencer ~genesis_timestamp ~time_between_blocks:Nothing protocol
  in
  (* Wait for the sequencer to publish its genesis block. *)
  let* () =
    repeat 3 (fun () ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~node ~client in
        unit)
  in
  let* proxy =
    Evm_node.init
      ~mode:(Proxy {devmode = true})
      (Sc_rollup_node.endpoint sc_rollup_node)
  in

  (* Assert the kernel version is the same at start up. *)
  let*@ sequencer_kernelVersion = Rpc.tez_kernelVersion sequencer in
  let*@ proxy_kernelVersion = Rpc.tez_kernelVersion proxy in
  Check.((sequencer_kernelVersion = proxy_kernelVersion) string)
    ~error_msg:"Kernel versions should be the same at start up" ;

  (* Activation timestamp is 1 day before the genesis. Therefore, it can
     be forced immediatly. *)
  let activation_timestamp = "2020-01-09T00:00:00Z" in
  (* Sends the upgrade to L1 and sequencer. *)
  let* () =
    upgrade
      ~sc_rollup_node
      ~sc_rollup_address
      ~admin:Constant.bootstrap2.public_key_hash
      ~admin_contract:l1_contracts.admin
      ~client
      ~upgrade_to:Constant.WASM.ghostnet_evm_kernel
      ~activation_timestamp
  in

  (* We bake a few blocks. As the sequencer is not producing anything, the
     kernel will not upgrade. *)
  let* () =
    repeat 5 (fun () ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~node ~client in
        unit)
  in
  (* Assert the kernel version is the same, it proves the upgrade did not
      happen. *)
  let*@ sequencer_kernelVersion = Rpc.tez_kernelVersion sequencer in
  let*@ proxy_kernelVersion = Rpc.tez_kernelVersion proxy in
  Check.((sequencer_kernelVersion = proxy_kernelVersion) string)
    ~error_msg:"Kernel versions should be the same even after the message" ;

  (* Now we force the kernel upgrade via an external message. They will
     become unsynchronised. *)
  let* () =
    force_kernel_upgrade ~sc_rollup_address ~sc_rollup_node ~node ~client
  in

  (* Assert the kernel version are now different, it shows that only the rollup
     node upgraded. *)
  let*@ sequencer_kernelVersion = Rpc.tez_kernelVersion sequencer in
  let*@ new_proxy_kernelVersion = Rpc.tez_kernelVersion proxy in
  Check.((sequencer_kernelVersion <> new_proxy_kernelVersion) string)
    ~error_msg:"Kernel versions should be different after forced upgrade" ;
  Check.((sequencer_kernelVersion = proxy_kernelVersion) string)
    ~error_msg:"Sequencer should be on the previous version" ;
  unit

let test_external_transaction_to_delayed_inbox_fails =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "external"]
    ~title:"Sending an external transaction to the delayed inbox fails"
    ~uses
  @@ fun protocol ->
  (* Start the evm node *)
  let* {client; node; sequencer; proxy; sc_rollup_node; _} =
    setup_sequencer
      protocol
      ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
      ~time_between_blocks:Nothing
      ~config:(`Path (kernel_inputs_path ^ "/100-inputs-for-proxy-config.yaml"))
  in
  let* () = Evm_node.wait_for_blueprint_injected ~timeout:5. sequencer 0 in
  (* Bake a couple more levels for the blueprint to be final *)
  let* _ = next_rollup_node_level ~sc_rollup_node ~node ~client in
  let* _ = next_rollup_node_level ~sc_rollup_node ~node ~client in
  let raw_tx, _ = read_tx_from_file () |> List.hd in
  let*@ tx_hash = Rpc.send_raw_transaction ~raw_tx proxy in
  (* Bake enough levels to make sure the transaction would be processed
     if added *)
  let* () =
    repeat 10 (fun () ->
        let* _ = Rpc.produce_block sequencer in
        let* _ = next_rollup_node_level ~node ~client ~sc_rollup_node in
        unit)
  in
  (* Response should be none *)
  let*@ response = Rpc.get_transaction_receipt ~tx_hash proxy in
  assert (Option.is_none response) ;
  let*@ response = Rpc.get_transaction_receipt ~tx_hash sequencer in
  assert (Option.is_none response) ;
  unit

let test_delayed_inbox_flushing =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "timeout"]
    ~title:"Delayed inbox flushing"
    ~uses
  @@ fun protocol ->
  (* Setup with a short wall time timeout but a significant lower bound of
     L1 levels needed for timeout.
     The idea is to send 2 transactions to the delayed inbox, having one
     time out and check that the second is also forced.
     We set [delayed_inbox_min_levels] to a value that is large enough
     to give us time to send the second one while the first one is not
     timed out yet.
  *)
  let* {
         client;
         node;
         l1_contracts;
         sc_rollup_address;
         sc_rollup_node;
         sequencer;
         proxy;
       } =
    setup_sequencer
      ~delayed_inbox_timeout:1
      ~delayed_inbox_min_levels:20
      protocol
  in
  (* Kill the sequencer *)
  let* () = Evm_node.terminate sequencer in
  let endpoint = Evm_node.endpoint proxy in
  let* _ = next_rollup_node_level ~sc_rollup_node ~node ~client in
  let sender = Eth_account.bootstrap_accounts.(0).address in
  let _ = Rpc.block_number proxy in
  let receiver = Eth_account.bootstrap_accounts.(1).address in
  let* sender_balance_prev = Eth_cli.balance ~account:sender ~endpoint in
  let* receiver_balance_prev = Eth_cli.balance ~account:receiver ~endpoint in
  (* Send the first transaction, this one is dummy (from [100-inputs-for-proxy])
     as we only use it for the timeout. *)
  let tx1 =
    "f863808252088252089400000000000000000000000000000000000000000a80820a95a0aedf43a765be7e57167a732fb460bec1c73f29bc8c2f7e753b652918ea19cd8da04062403d1ddcdf9d80dc69cab4509400a21604f0ec42f82289597f8476792648"
  in
  let* _hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      ~node
      tx1
  in
  (* Bake a few blocks but not enough for the first tx to be forced! *)
  let* _ =
    repeat 10 (fun () ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~node ~client in
        unit)
  in
  (* Send the second transaction, a transfer from
     Eth_account.bootstrap_accounts.(0) to Eth_account.bootstrap_accounts.(1).
  *)
  let tx2 =
    "f86d80843b9aca00825b0494b53dc01974176e5dff2298c5a94343c2585e3c54880de0b6b3a764000080820a96a07a3109107c6bd1d555ce70d6253056bc18996d4aff4d4ea43ff175353f49b2e3a05f9ec9764dc4a3c3ab444debe2c3384070de9014d44732162bb33ee04da187ef"
  in
  let* _hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      ~node
      tx2
  in
  (* Bake a few more blocks to make sure the first tx times out, but not
     the second one. However, the latter should also be included. *)
  let* _ =
    repeat 10 (fun () ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~node ~client in
        unit)
  in
  let* sender_balance_next = Eth_cli.balance ~account:sender ~endpoint in
  let* receiver_balance_next = Eth_cli.balance ~account:receiver ~endpoint in
  Check.((sender_balance_prev <> sender_balance_next) Wei.typ)
    ~error_msg:"Balance should be updated" ;
  Check.((receiver_balance_prev <> receiver_balance_next) Wei.typ)
    ~error_msg:"Balance should be updated" ;
  Check.((sender_balance_prev > sender_balance_next) Wei.typ)
    ~error_msg:"Expected a smaller balance" ;
  Check.((receiver_balance_next > receiver_balance_prev) Wei.typ)
    ~error_msg:"Expected a bigger balance" ;
  unit

let test_no_automatic_block_production =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "sequencer"; "block"]
    ~title:"No automatic block production"
    ~uses
  @@ fun protocol ->
  let* {sequencer; _} = setup_sequencer protocol ~time_between_blocks:Nothing in
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

let test_migration_from_ghostnet =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "sequencer"; "upgrade"; "migration"; "ghostnet"]
    ~title:"Sequencer can upgrade from ghostnet"
    ~uses:(fun protocol -> Constant.WASM.ghostnet_evm_kernel :: uses protocol)
  @@ fun protocol ->
  (* Creates a sequencer using prod version and ghostnet kernel. *)
  let* {
         sequencer;
         node;
         client;
         sc_rollup_node;
         sc_rollup_address;
         l1_contracts;
         proxy;
         _;
       } =
    setup_sequencer
      protocol
      ~time_between_blocks:Nothing
      ~kernel:Constant.WASM.ghostnet_evm_kernel
      ~devmode:false
      ~max_blueprints_lag:0
  in
  let* _ = next_rollup_node_level ~sc_rollup_node ~node ~client in
  let check_kernel_version ~evm_node ~equal expected =
    let*@ kernel_version = Rpc.tez_kernelVersion evm_node in
    if equal then
      Check.((kernel_version = expected) string)
        ~error_msg:"Expected kernelVersion to be %R, got %L"
    else
      Check.((kernel_version <> expected) string)
        ~error_msg:"Expected kernelVersion to be different than %R" ;
    return kernel_version
  in
  (* Check kernelVersion. *)
  let* _kernel_version =
    check_kernel_version
      ~evm_node:sequencer
      ~equal:true
      Constant.WASM.ghostnet_evm_commit
  in
  let* _kernel_version =
    check_kernel_version
      ~evm_node:proxy
      ~equal:true
      Constant.WASM.ghostnet_evm_commit
  in

  (* Produces a few blocks. *)
  let* _ =
    repeat 2 (fun () ->
        let* _ = Rpc.produce_block sequencer in
        unit)
  in
  let* () =
    repeat 4 (fun () ->
        let* _ = next_rollup_node_level ~node ~client ~sc_rollup_node in
        unit)
  in
  (* Check the consistency. *)
  let* () = check_head_consistency ~left:proxy ~right:sequencer () in
  (* Sends upgrade to current version. *)
  let* () =
    upgrade
      ~sc_rollup_node
      ~sc_rollup_address
      ~admin:Constant.bootstrap2.public_key_hash
      ~admin_contract:l1_contracts.admin
      ~client
      ~upgrade_to:Constant.WASM.evm_kernel
      ~activation_timestamp:"0"
  in
  (* Bakes 2 blocks for the event follower to see the upgrade. *)
  let* _ =
    repeat 2 (fun () ->
        let* _ = next_rollup_node_level ~node ~client ~sc_rollup_node in
        unit)
  in
  (* Produce a block to trigger the upgrade. *)
  let* _ = Rpc.produce_block sequencer in
  let* _ =
    repeat 4 (fun () ->
        let* _ = next_rollup_node_level ~node ~client ~sc_rollup_node in
        unit)
  in
  (* Check that the prod sequencer has updated. *)
  let* new_kernel_version =
    check_kernel_version
      ~evm_node:sequencer
      ~equal:false
      Constant.WASM.ghostnet_evm_commit
  in
  (* Runs sequencer and proxy with --devmode. *)
  let* () = Evm_node.terminate proxy in
  let* () = Evm_node.terminate sequencer in
  (* Manually put `--devmode` to use the same command line. *)
  let* () = Evm_node.run ~extra_arguments:["--devmode"] proxy in
  let* () = Evm_node.run ~extra_arguments:["--devmode"] sequencer in
  (* Check that new sequencer and proxy are on a new version. *)
  let* _kernel_version =
    check_kernel_version ~evm_node:sequencer ~equal:true new_kernel_version
  in
  let* _kernel_version =
    check_kernel_version ~evm_node:proxy ~equal:true new_kernel_version
  in
  (* Check the consistency. *)
  let* () = check_head_consistency ~left:proxy ~right:sequencer () in
  (* Produces a few blocks. *)
  let* _ =
    repeat 2 (fun () ->
        let* _ = Rpc.produce_block sequencer in
        unit)
  in
  let* () =
    repeat 4 (fun () ->
        let* _ = next_rollup_node_level ~node ~client ~sc_rollup_node in
        unit)
  in
  (* Final consistency check. *)
  check_head_consistency ~left:sequencer ~right:proxy ()

(** This tests the situation where the kernel has an upgrade and the
    sequencer upgrade by following the event of the kernel. *)
let test_sequencer_upgrade =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "sequencer"; "sequencer_upgrade"; "auto"; "sync"]
    ~title:
      "Rollup-node sequencer upgrade is applied to the sequencer local state."
    ~uses
  @@ fun protocol ->
  let* {
         sc_rollup_node;
         l1_contracts;
         sc_rollup_address;
         client;
         sequencer;
         proxy;
         node;
         _;
       } =
    setup_sequencer
      ~sequencer:Constant.bootstrap1
      ~time_between_blocks:Nothing
      protocol
  in
  (* produce an initial block *)
  let* _ = Rpc.produce_block sequencer in
  let* () =
    (* make sure rollup node saw it *)
    repeat 4 (fun () ->
        let* _ = next_rollup_node_level ~node ~client ~sc_rollup_node in
        unit)
  in
  let* () =
    check_head_consistency
      ~left:proxy
      ~right:sequencer
      ~error_msg:"The head should be the same before the upgrade"
      ()
  in
  let*@ previous_proxy_head = Rpc.get_block_by_number ~block:"latest" proxy in
  (* Sends the upgrade to L1. *)
  Log.info "Sending the sequencer upgrade to the L1 contract" ;
  let new_sequencer_key = Constant.bootstrap2.alias in
  let* () =
    Evm_node.wait_for_evm_event sequencer ~event_kind:"sequencer_upgrade"
  and* () =
    let* () =
      sequencer_upgrade
        ~sc_rollup_address
        ~sequencer_admin:Constant.bootstrap2.alias
        ~sequencer_admin_contract:l1_contracts.sequencer_admin
        ~pool_address:Eth_account.bootstrap_accounts.(0).address
        ~client
        ~upgrade_to:new_sequencer_key
        ~activation_timestamp:"0"
    in
    (* 2 block so the sequencer sees the event from the rollup
       node. *)
    repeat 2 (fun () ->
        let* _ = next_rollup_node_level ~node ~client ~sc_rollup_node in
        unit)
  in
  let* () =
    check_head_consistency
      ~left:proxy
      ~right:sequencer
      ~error_msg:"The head should be the same after the upgrade"
      ()
  in
  let nb_block = 10l in
  let* () =
    repeat (Int32.to_int nb_block) (fun () ->
        let* _ = Rpc.produce_block sequencer in
        unit)
  in
  let* () =
    repeat 5 (fun () ->
        let* _ = next_rollup_node_level ~node ~client ~sc_rollup_node in
        unit)
  in
  let*@ proxy_head = Rpc.get_block_by_number ~block:"latest" proxy in
  Check.((previous_proxy_head.hash = proxy_head.hash) string)
    ~error_msg:
      "The proxy should not have progessed because no block have been produced \
       by the current sequencer." ;
  (* Check that even the evm-node sequencer itself refuses the blocks as they do
     not respect the sequencer's signature. *)
  let* () =
    check_head_consistency
      ~left:proxy
      ~right:sequencer
      ~error_msg:
        "The head should be the same after the sequencer tried to produce \
         blocks, they are are disregarded."
      ()
  in
  Log.info
    "Stopping current sequencer and starting a new one with new sequencer key" ;
  let* () = Evm_node.terminate sequencer in
  let new_sequencer =
    let mode =
      match Evm_node.mode sequencer with
      | Sequencer config ->
          Evm_node.Sequencer {config with sequencer = new_sequencer_key}
      | _ -> Test.fail "impossible case, it's a sequencer"
    in
    Evm_node.create
      ~name:"new-sequencer"
      ~mode
      (Sc_rollup_node.endpoint sc_rollup_node)
  in

  let* () =
    Evm_node.init_from_rollup_node_data_dir
      ~devmode:true
      new_sequencer
      sc_rollup_node
  in
  let* () = Evm_node.run new_sequencer in
  let* () =
    repeat (Int32.to_int nb_block) (fun () ->
        let* _ = Rpc.produce_block new_sequencer in
        unit)
  in
  let* () =
    repeat 5 (fun () ->
        let* _ = next_rollup_node_level ~node ~client ~sc_rollup_node in
        unit)
  in
  let previous_proxy_head = proxy_head in
  let* () =
    check_head_consistency
      ~left:proxy
      ~right:new_sequencer
      ~error_msg:
        "The head should be the same after blocks produced by the new sequencer"
      ()
  in
  let*@ proxy_head = Rpc.get_block_by_number ~block:"latest" proxy in
  Check.(
    (Int32.add previous_proxy_head.number nb_block = proxy_head.number) int32)
    ~error_msg:
      "The block number should have incremented (previous: %L, current: %R)" ;
  unit

let protocols = Protocol.all

let () =
  test_remove_sequencer protocols ;
  test_persistent_state protocols ;
  test_publish_blueprints protocols ;
  test_resilient_to_rollup_node_disconnect protocols ;
  test_can_fetch_smart_rollup_address protocols ;
  test_can_fetch_blueprint protocols ;
  test_send_transaction_to_delayed_inbox protocols ;
  test_send_deposit_to_delayed_inbox protocols ;
  test_rpc_produceBlock protocols ;
  test_delayed_transfer_is_included protocols ;
  test_delayed_deposit_is_included protocols ;
  test_init_from_rollup_node_data_dir protocols ;
  test_observer_applies_blueprint protocols ;
  test_observer_forwards_transaction protocols ;
  test_upgrade_kernel_auto_sync protocols ;
  test_force_kernel_upgrade protocols ;
  test_force_kernel_upgrade_too_early protocols ;
  test_external_transaction_to_delayed_inbox_fails protocols ;
  test_delayed_transfer_timeout protocols ;
  test_delayed_transfer_timeout_fails_l1_levels protocols ;
  test_delayed_inbox_flushing protocols ;
  test_no_automatic_block_production protocols ;
  test_migration_from_ghostnet protocols ;
  test_sequencer_upgrade protocols
