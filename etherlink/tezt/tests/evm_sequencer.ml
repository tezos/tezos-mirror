(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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
  let get_blueprint evm_node number =
    Runnable.run
    @@ Curl.get
         ~args:["--fail"]
         (Evm_node.endpoint evm_node ^ "/evm/blueprint/"
        ^ Int64.to_string number)

  let get_smart_rollup_address evm_node =
    let* res =
      Runnable.run
      @@ Curl.get
           ~args:["--fail"]
           (Evm_node.endpoint evm_node ^ "/evm/smart_rollup_address")
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

type l1_contracts = {
  delayed_transaction_bridge : string;
  exchanger : string;
  bridge : string;
}

type sequencer_setup = {
  node : Node.t;
  client : Client.t;
  sc_rollup_address : string;
  sc_rollup_node : Sc_rollup_node.t;
  evm_node : Evm_node.t;
  l1_contracts : l1_contracts;
}

let setup_l1_contracts client =
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
  return {delayed_transaction_bridge; exchanger; bridge}

let setup_sequencer ?time_between_blocks
    ?(bootstrap_accounts = Eth_account.bootstrap_accounts)
    ?(sequencer = Constant.bootstrap1) protocol =
  let* node, client = setup_l1 protocol in
  let* l1_contracts = setup_l1_contracts client in
  let sc_rollup_node =
    Sc_rollup_node.create
      ~default_operator:Constant.bootstrap1.public_key_hash
      Batcher
      node
      ~base_dir:(Client.base_dir client)
  in
  let preimages_dir = Sc_rollup_node.data_dir sc_rollup_node // "wasm_2_0_0" in
  let config =
    Configuration.make_config
      ~bootstrap_accounts
      ~sequencer:sequencer.public_key
      ~delayed_bridge:l1_contracts.delayed_transaction_bridge
      ~ticketer:l1_contracts.exchanger
      ()
  in
  let* {output; _} =
    prepare_installer_kernel ~preimages_dir ?config Constant.WASM.evm_kernel
  in
  let* sc_rollup_address =
    originate_sc_rollup
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
    let sequencer =
      match sequencer.secret_key with
      | Unencrypted sk -> sk
      | Encrypted _ -> Test.fail "Provide an unencrypted key for the sequencer"
    in
    Evm_node.Sequencer
      {
        initial_kernel = output;
        preimage_dir = preimages_dir;
        private_rpc_port;
        time_between_blocks;
        sequencer;
        genesis_timestamp = None;
      }
  in
  let* evm_node =
    Evm_node.init ~mode (Sc_rollup_node.endpoint sc_rollup_node)
  in
  return
    {node; client; evm_node; l1_contracts; sc_rollup_address; sc_rollup_node}

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

let test_persistent_state =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "sequencer"]
    ~title:"Sequencer state is persistent across runs"
    ~uses
  @@ fun protocol ->
  let* {evm_node; _} = setup_sequencer protocol in
  (* Force the sequencer to produce a block. *)
  let* _ = Rpc.produce_block evm_node in
  (* Ask for the current block. *)
  let*@ block_number = Rpc.block_number evm_node in
  Check.is_true
    ~__LOC__
    (block_number > 0l)
    ~error_msg:"The sequencer should have produced a block" ;
  (* Terminate the sequencer. *)
  let* () = Evm_node.terminate evm_node in
  (* Restart it. *)
  let* () = Evm_node.run evm_node in
  (* Assert the block number is at least [block_number]. Asserting
     that the block number is exactly the same as {!block_number} can
     be flaky if a block is produced between the restart and the
     RPC. *)
  let*@ new_block_number = Rpc.block_number evm_node in
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
  let* {evm_node; node; client; sc_rollup_node; _} =
    setup_sequencer ~time_between_blocks:Nothing protocol
  in
  let* _ =
    repeat 5 (fun () ->
        let* _ = Rpc.produce_block evm_node in
        unit)
  in

  let* () = Evm_node.wait_for_blueprint_injected ~timeout:5. evm_node 5 in

  (* Ask for the current block. *)
  let*@ sequencer_head = Rpc.get_block_by_number ~block:"latest" evm_node in

  (* At this point, the evm node should called the batcher endpoint to publish
     all the blueprints. Stopping the node is then not a problem. *)
  let* () =
    repeat 10 (fun () ->
        let* _ = next_rollup_node_level ~node ~client ~sc_rollup_node in
        unit)
  in

  (* Open an EVM node in proxy mode to fetch the rollup node storage. *)
  let proxy_mode = Evm_node.Proxy {devmode = true} in
  let* proxy_evm =
    Evm_node.init ~mode:proxy_mode (Sc_rollup_node.endpoint sc_rollup_node)
  in
  (* We have unfortunately noticed that the test can be flaky. Sometimes,
     the following RPC is done before the proxy being initialised, even though
     we wait for it. The source of flakiness is unknown but happens very rarely,
     we put a small sleep to make the least flaky possible. *)
  let* () = Lwt_unix.sleep 2. in
  let*@ rollup_head = Rpc.get_block_by_number ~block:"latest" proxy_evm in
  Check.((sequencer_head.hash = rollup_head.hash) (option string))
    ~error_msg:"Expected the same head on the rollup node and the sequencer" ;
  unit

let test_resilient_to_rollup_node_disconnect =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "sequencer"; "data"]
    ~title:"Sequencer is resilient to rollup node disconnection"
    ~uses
  @@ fun protocol ->
  let* {evm_node; sc_rollup_node; _} =
    setup_sequencer ~time_between_blocks:Nothing protocol
  in
  let* _ =
    repeat 5 (fun () ->
        let* _ = Rpc.produce_block evm_node in
        unit)
  in

  let* () = Evm_node.wait_for_blueprint_injected ~timeout:5. evm_node 5 in

  (* Kill the rollup node *)
  let* () = Sc_rollup_node.kill sc_rollup_node in

  (* The sequencer node should keep producing blocks. *)
  let* _ =
    repeat 5 (fun () ->
        let* _ = Rpc.produce_block evm_node in
        unit)
  in

  let* () = Evm_node.wait_for_blueprint_produced evm_node ~timeout:5. 10 in

  unit

let test_can_fetch_blueprint =
  Protocol.register_test
    ~__FILE__
    ~tags:["evm"; "sequencer"; "data"]
    ~title:"Sequencer can provide blueprints on demand"
    ~uses
  @@ fun protocol ->
  let* {evm_node; _} = setup_sequencer ~time_between_blocks:Nothing protocol in
  let number_of_blocks = 5 in
  let* _ =
    repeat number_of_blocks (fun () ->
        let* _ = Rpc.produce_block evm_node in
        unit)
  in

  let* () = Evm_node.wait_for_blueprint_injected ~timeout:5. evm_node 5 in

  let* blueprints =
    fold number_of_blocks [] (fun i acc ->
        let* blueprint =
          Sequencer_rpc.get_blueprint evm_node Int64.(of_int @@ (i + 1))
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
  let* {evm_node; sc_rollup_address; _} =
    setup_sequencer ~time_between_blocks:Nothing protocol
  in
  let* claimed_address = Sequencer_rpc.get_smart_rollup_address evm_node in

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
  let* {evm_node; _} = setup_sequencer ~time_between_blocks:Nothing protocol in
  let*@ start_block_number = Rpc.block_number evm_node in
  let* _ = Rpc.produce_block evm_node in
  let*@ new_block_number = Rpc.block_number evm_node in
  Check.((Int32.succ start_block_number = new_block_number) int32)
    ~error_msg:"Expected new block number to be %L, but got: %R" ;
  unit

let wait_for_event ?(levels = 10) event_watcher ~evm_node ~sc_rollup_node ~node
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
      let* _ = Rpc.produce_block evm_node in
      match !event_value with
      | Some value -> return value
      | None -> rollup_node_loop (n - 1)
  in
  Lwt.pick [rollup_node_loop levels]

let wait_for_delayed_inbox_add_tx_and_injected ~evm_node ~sc_rollup_node ~node
    ~client =
  let event_watcher =
    let added =
      Evm_node.wait_for evm_node "evm_node_dev_delayed_inbox_add_transaction.v0"
      @@ fun json ->
      let hash = JSON.(json |-> "hash" |> as_string) in
      Some hash
    in
    let injected =
      Evm_node.wait_for evm_node "evm_node_dev_tx_pool_transaction_injected.v0"
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
    ~evm_node
    ~sc_rollup_node
    ~node
    ~client
    ~error_msg:
      "Timed out while waiting for transaction to be added to the delayed \
       inbox and injected"

let wait_for_delayed_inbox_fetch ~evm_node ~sc_rollup_node ~node ~client =
  let event_watcher =
    Evm_node.wait_for evm_node "evm_node_dev_delayed_inbox_fetch_succeeded.v0"
    @@ fun json ->
    let nb = JSON.(json |-> "nb" |> as_int) in
    Some nb
  in
  wait_for_event
    event_watcher
    ~evm_node
    ~sc_rollup_node
    ~node
    ~client
    ~error_msg:"Timed out while waiting for delayed inbox to be fetched"

let wait_until_delayed_inbox_is_empty ~evm_node ~sc_rollup_node ~node ~client =
  let levels = 10 in
  let rec go n =
    if n = 0 then
      Test.fail "Timed out waiting for the delayed inbox to be empty"
    else
      let* nb =
        wait_for_delayed_inbox_fetch ~evm_node ~sc_rollup_node ~node ~client
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
  let* {client; node; l1_contracts; sc_rollup_address; sc_rollup_node; evm_node}
      =
    setup_sequencer protocol
  in
  let endpoint = Evm_node.endpoint evm_node in
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
      ~evm_node
      ~sc_rollup_node
      ~node
      ~client
  in
  let* () =
    wait_until_delayed_inbox_is_empty ~evm_node ~sc_rollup_node ~node ~client
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
  let* {client; node; l1_contracts; sc_rollup_address; sc_rollup_node; evm_node}
      =
    setup_sequencer protocol
  in
  let endpoint = Evm_node.endpoint evm_node in

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
      ~evm_node
      ~sc_rollup_node
      ~node
      ~client
  in
  let* () =
    wait_until_delayed_inbox_is_empty ~evm_node ~sc_rollup_node ~node ~client
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
  let* {sc_rollup_node; evm_node; client; _} =
    setup_sequencer ~time_between_blocks:Nothing protocol
  in
  (* a sequencer is needed to produce an initial block *)
  let* () =
    repeat 5 (fun () ->
        let* _l2_lvl = Rpc.produce_block evm_node in
        let* _lvl = Client.bake_for_and_wait client in
        let* _lvl = Sc_rollup_node.wait_sync ~timeout:30. sc_rollup_node in
        unit)
  in
  let* () = Evm_node.terminate evm_node in
  let* proxy_node =
    Evm_node.init
      ~mode:(Proxy {devmode = false})
      (Sc_rollup_node.endpoint sc_rollup_node)
  in
  let evm_node' =
    Evm_node.create
      ~mode:(Evm_node.mode evm_node)
      (Sc_rollup_node.endpoint sc_rollup_node)
  in
  let* () = Evm_node.init_from_rollup_node_data_dir evm_node' sc_rollup_node in
  let* () = Evm_node.run evm_node' in
  let*@ rollup_node_head = Rpc.get_block_by_number ~block:"latest" proxy_node in
  let*@ sequencer_head = Rpc.get_block_by_number ~block:"latest" evm_node' in
  Check.((sequencer_head.number = rollup_node_head.number) int32)
    ~error_msg:"block number is not equal (sequencer: %L; rollup: %R)" ;
  let* _l2_lvl = Rpc.produce_block evm_node in
  let* _lvl = Client.bake_for_and_wait client in
  let* _lvl = Sc_rollup_node.wait_sync ~timeout:30. sc_rollup_node in
  let*@ rollup_node_head = Rpc.get_block_by_number ~block:"latest" proxy_node in
  let*@ sequencer_head = Rpc.get_block_by_number ~block:"latest" evm_node' in
  Check.((sequencer_head.number = rollup_node_head.number) int32)
    ~error_msg:"block number is not equal (sequencer: %L; rollup: %R)" ;
  unit

let () =
  test_persistent_state [Alpha] ;
  test_publish_blueprints [Alpha] ;
  test_resilient_to_rollup_node_disconnect [Alpha] ;
  test_can_fetch_smart_rollup_address [Alpha] ;
  test_can_fetch_blueprint [Alpha] ;
  test_send_transaction_to_delayed_inbox [Alpha] ;
  test_send_deposit_to_delayed_inbox [Alpha] ;
  test_rpc_produceBlock [Alpha] ;
  test_delayed_transfer_is_included [Alpha] ;
  test_delayed_deposit_is_included [Alpha] ;
  test_init_from_rollup_node_data_dir [Alpha]
