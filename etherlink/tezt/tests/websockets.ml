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
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file websockets.ml
   Subject:      The websocket RPC and subscription interface.
*)

open Rpc.Syntax
open Transaction
open Test_helpers
open Setup
module Protocol = Test_helpers.Protocol_no_direct_register

let test_websocket_rpcs =
  register_all
    ~__FILE__
    ~tags:["evm"; "rpc"; "websocket"]
    ~title:"RPC methods over websocket"
    ~time_between_blocks:Nothing
    ~eth_bootstrap_accounts:
      ((Array.to_list Eth_account.bootstrap_accounts
       |> List.map (fun a -> a.Eth_account.address))
      @ Eth_account.lots_of_address)
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
    ~websockets:true
  @@ fun {sequencer; _} _protocol ->
  Log.info "Opening a websocket connection with the node" ;
  let* websocket = Evm_node.open_websocket sequencer in
  Log.info "getBalance" ;
  let*@ balance =
    Rpc.get_balance
      ~websocket
      ~address:Eth_account.bootstrap_accounts.(0).address
      sequencer
  in
  Check.((balance = default_bootstrap_account_balance) Wei.typ)
    ~error_msg:
      (sf
         "Expected balance of %s should be %%R, but got %%L"
         Eth_account.bootstrap_accounts.(0).address) ;
  Log.info "getBlockByNumber" ;
  let*@ block = Rpc.get_block_by_number ~websocket ~block:"0" sequencer in
  Check.((block.number = 0l) int32)
    ~error_msg:"Unexpected block number, should be %%R, but got %%L" ;
  Log.info "getBlockByHash" ;
  let* block' =
    let* block =
      Evm_node.(
        jsonrpc
          ~websocket
          sequencer
          {
            method_ = "eth_getBlockByHash";
            parameters = `A [`String block.hash; `Bool false];
          })
    in
    return @@ (block |> Evm_node.extract_result |> Block.of_json)
  in
  assert (block = block') ;
  Log.info "blockNumber" ;
  let* () =
    repeat 2 (fun () ->
        let*@ _ = produce_block sequencer in
        unit)
  in
  let*@ block_number = Rpc.block_number ~websocket sequencer in
  Check.((block_number = 2l) int32)
    ~error_msg:"Expected a block number of %R, but got %L" ;
  Log.info "getTransactionCount" ;
  let*@ transaction_count =
    Rpc.get_transaction_count
      ~websocket
      ~address:Eth_account.bootstrap_accounts.(0).address
      sequencer
  in
  Check.((transaction_count = 0L) int64)
    ~error_msg:"Expected a nonce of %R, but got %L" ;
  Log.info "netVersion" ;
  let*@ net_version = Rpc.net_version ~websocket sequencer in
  Check.((net_version = "1337") string)
    ~error_msg:"Expected net_version is %R, but got %L" ;
  Log.info "coinbase" ;
  let*@ coinbase = Rpc.coinbase ~websocket sequencer in
  Check.((coinbase = "0x0000000000000000000000000000000000000000") string)
    ~error_msg:"eth_coinbase returned %L, expected %R" ;
  unit

let test_websocket_subscription_rpcs_cant_be_called_via_http_requests =
  register_all
    ~__FILE__
    ~tags:["evm"; "rpc"; "websocket"; "http"]
    ~title:
      "Check that subscriptions rpcs can't be called via regular http requests"
    ~time_between_blocks:Nothing
    ~eth_bootstrap_accounts:
      ((Array.to_list Eth_account.bootstrap_accounts
       |> List.map (fun a -> a.Eth_account.address))
      @ Eth_account.lots_of_address)
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
    ~websockets:true
  @@ fun {sequencer; _} _protocol ->
  let* () =
    Lwt.catch
      (fun () ->
        let* _ = Rpc.subscribe ~kind:NewHeads sequencer in
        failwith "eth_subscribe shouldn't be callable via http requests")
      (fun _ -> unit)
  in
  Lwt.catch
    (fun () ->
      let* _ = Rpc.unsubscribe ~id:"0x0" sequencer in
      failwith "eth_unsubscribe shouldn't be callable via http requests")
    (fun _ -> unit)

let produce_block_and_wait_for_sync ~sequencer evm_node =
  let*@ head = Rpc.block_number sequencer in
  let level = Int32.(succ head |> to_int) in
  if Evm_node.can_apply_blueprint evm_node then
    let wait_for =
      Evm_node.wait_for_blueprint_applied evm_node Int32.(succ head |> to_int)
    in
    let*@ _ = produce_block sequencer in
    let* () = wait_for in
    return level
  else
    let*@ _ = produce_block sequencer in
    return level

let check_unsubscription ~websocket ~id ~sequencer evm_node =
  let* sub_status = Rpc.unsubscribe ~websocket ~id evm_node in
  Check.((sub_status = true) bool)
    ~error_msg:"Unsubscription should be successful" ;
  (* After unsubbing to the event, we shouldn't receive data anymore. *)
  let* _ = produce_block_and_wait_for_sync ~sequencer evm_node in
  let* result =
    Lwt.pick
      [
        (let* never_return = Websocket.recv websocket in
         return (Error never_return));
        (let* () = Lwt_unix.sleep 2. in
         return (Ok ()));
      ]
  in
  (match result with
  | Ok () -> ()
  | Error _ -> failwith "The websocket shouldn't have received any new data.") ;
  (* If we try to unsubscribe from this event again, it should return false as
     we were already unsubbed. *)
  let* sub_status = Rpc.unsubscribe ~websocket ~id evm_node in
  Check.((sub_status = false) bool)
    ~error_msg:"Unsubscribing from the same event twice should return false" ;
  unit

let test_websocket_newHeads_event =
  register_all
    ~__FILE__
    ~tags:["evm"; "rpc"; "websocket"; "new_heads"]
    ~title:"Check that websocket event `newHeads` is behaving correctly"
    ~time_between_blocks:Nothing
    ~eth_bootstrap_accounts:
      ((Array.to_list Eth_account.bootstrap_accounts
       |> List.map (fun a -> a.Eth_account.address))
      @ Eth_account.lots_of_address)
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
    ~websockets:true
  @@ fun {sequencer; observer; _} _protocol ->
  let scenario evm_node nb_websockets =
    let* websockets =
      Lwt_list.map_p
        (fun () ->
          let* websocket = Evm_node.open_websocket evm_node in
          let* id = Rpc.subscribe ~websocket ~kind:NewHeads evm_node in
          return (websocket, id))
        (List.init nb_websockets (fun _ -> ()))
    in
    let check_block_number () =
      (* We always wait for the observer to be synced, to know that the
         blueprint was fully propagated. *)
      let* level = produce_block_and_wait_for_sync ~sequencer observer in
      Lwt_list.iter_s
        (fun (websocket, _id) ->
          let* block = Websocket.recv ~timeout:10. websocket in
          let Block.{number; _} =
            JSON.(block |-> "params" |-> "result" |> Block.of_json)
          in
          Check.((number = Int32.of_int level) int32)
            ~error_msg:"Received block level was %L, expected %R" ;
          unit)
        websockets
    in
    let* () = repeat 2 check_block_number in
    Lwt_list.iter_p
      (fun (websocket, id) ->
        check_unsubscription ~websocket ~id ~sequencer evm_node)
      websockets
  in
  let* rpc_node = run_new_rpc_endpoint sequencer in
  let* () = scenario sequencer 3 in
  let* () = scenario observer 3 in
  let* () = scenario rpc_node 3 in
  unit

(* This test is flaky because Dream may not correctly detect websocket
   connections closed by the client. *)
let test_websocket_cleanup =
  register_all
    ~__FILE__
    ~tags:["evm"; "rpc"; "websocket"; "cleanup"]
    ~title:"Check that websocket subscriptions are cleaned up on close"
    ~time_between_blocks:Nothing
    ~eth_bootstrap_accounts:
      ((Array.to_list Eth_account.bootstrap_accounts
       |> List.map (fun a -> a.Eth_account.address))
      @ Eth_account.lots_of_address)
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
    ~rpc_server:Resto (* Websockets disconnection is flaky in Dream *)
    ~websockets:true
  @@ fun {sequencer; _} _protocol ->
  let* websocket = Evm_node.open_websocket sequencer in
  let* id1 = Rpc.subscribe ~websocket ~kind:NewHeads sequencer in
  let* id2 = Rpc.subscribe ~websocket ~kind:NewHeads sequencer in
  let*@ _ = produce_block sequencer in
  let*@ _ = produce_block sequencer in
  Log.info "Closing websocket" ;
  let* () = Websocket.close websocket in
  let* () = Lwt_unix.sleep 1. in
  Log.info "New websocket connection" ;
  let* websocket = Evm_node.open_websocket sequencer in
  let* sub_status = Rpc.unsubscribe ~websocket ~id:id2 sequencer in
  Check.((sub_status = false) bool)
    ~error_msg:"Subscription should have been cleaned up from node" ;
  let* sub_status = Rpc.unsubscribe ~websocket ~id:id1 sequencer in
  Check.((sub_status = false) bool)
    ~error_msg:"All subscriptions should have been cleaned up from node" ;
  unit

let test_websocket_max_message_length () =
  let max_message_length = 512 in
  let patch_config json =
    json
    |> Evm_node.patch_config_with_experimental_feature
         ~rpc_server:Resto (* The limit is not implemented for Dream *)
         ()
    |> Evm_node.patch_config_websockets_if_enabled ~max_message_length
  in
  register_sandbox
    ~__FILE__
    ~tags:["evm"; "rpc"; "websocket"; "max"]
    ~title:"Websocket server does not accept messages larger than maximum"
    ~patch_config
    ~websockets:true
  @@ fun sequencer ->
  let* websocket = Evm_node.open_websocket sequencer in
  let shutdown =
    Lwt.pick
      [
        ( Evm_node.wait_for sequencer "websocket_shutdown.v0" @@ fun json ->
          Some JSON.(json |-> "reason" |> as_string) );
        (let* () = Lwt_unix.sleep 5. in
         Test.fail ~__LOC__ "Websocket worker did not close.");
      ]
  in
  let* () =
    Websocket.send_raw websocket (String.make (max_message_length + 1) '\000')
  in
  let* reason = shutdown in
  Log.info "Websocket shutdown reason: %S." reason ;
  Check.(reason =~ rex "too big")
    ~error_msg:"Expected reason to match %R, got %L" ;
  Lwt.catch
    (fun () ->
      let* _ = Websocket.recv ~timeout:10. websocket in
      Test.fail ~__LOC__ "Connection was not closed.")
    (function
      | Websocket.Connection_closed ->
          Log.info "Connection was closed on client end." ;
          unit
      | e -> Lwt.reraise e)

let test_websocket_rate_limit strategy =
  let max_messages = 5 in
  let interval = 3 in
  let strategy_str =
    match strategy with
    | `Wait -> "wait"
    | `Error -> "error"
    | `Close -> "close"
  in
  let patch_config json =
    json
    |> Evm_node.patch_config_with_experimental_feature
         ~rpc_server:Resto (* The limit is not implemented for Dream *)
         ()
    |> Evm_node.patch_config_websockets_if_enabled
         ~monitor_heartbeat:false (* To not count ping/pong frames *)
         ~rate_limit:
           (`O
              [
                ("max_messages", `Float (float_of_int max_messages));
                ("interval", `Float (float_of_int interval));
                ("strategy", `String strategy_str);
              ])
  in
  register_sandbox
    ~__FILE__
    ~tags:["evm"; "rpc"; "websocket"; "rate_limit"; strategy_str]
    ~title:(sf "Websocket server limits rate of messages (%s)" strategy_str)
    ~patch_config
    ~websockets:true
  @@ fun sequencer ->
  let* websocket = Evm_node.open_websocket sequencer in
  let*@ _ = produce_block sequencer in
  Log.info "Send messages below limit" ;
  let send_requests n =
    List.init n Fun.id
    |> Lwt_list.map_s (fun _ ->
           Lwt.catch
             (fun () ->
               Rpc.block_number ~websocket sequencer
               |> Lwt_result.map_error (fun e -> `Rpc_error e))
             (fun e -> Lwt.return_error (`Exn e)))
  in
  let* responses = send_requests max_messages in
  List.iteri
    (fun i -> function
      | Ok _ -> ()
      | Error (`Rpc_error ({code; message; _} : Rpc.error)) ->
          Test.fail
            ~__LOC__
            "Websocket message %i resulted in error %d: %s"
            i
            code
            message
      | Error (`Exn e) -> raise e)
    responses ;
  Log.info "Wait for rate limit interval" ;
  let* () = Lwt_unix.sleep (float_of_int interval) in
  let ws_server_shutdown =
    Evm_node.wait_for sequencer "websocket_shutdown.v0" @@ fun json ->
    Some JSON.(json |-> "reason" |> as_string)
  in
  let t1 = Unix.gettimeofday () in
  Log.info "Sending messages above limit" ;
  let* responses = send_requests (max_messages + 1) in
  let t2 = Unix.gettimeofday () in
  let handle_time = t2 -. t1 in
  let errors =
    List.filter_map (function Ok _ -> None | Error e -> Some e) responses
  in
  match (strategy, errors) with
  | `Error, [] ->
      Test.fail ~__LOC__ "Websocket server should have rate limited a message"
  | `Error, _ :: _ :: _ ->
      Test.fail
        ~__LOC__
        "Websocket server should have rate limited only one message"
  | `Error, [`Rpc_error {code; message; _}] ->
      Check.((code = -32005) int)
        ~error_msg:"Rate limit error should have code %R but was %L" ;
      Check.(message =~ rex "Rate limited by messages on websocket")
        ~error_msg:"Expected message to match %R, got %L" ;
      unit
  | `Wait, _ :: _ ->
      Test.fail
        ~__LOC__
        "Websocket server should have blocked in rate limit instead of error"
  | `Wait, [] ->
      Check.((handle_time > float_of_int interval) float)
        ~error_msg:"Rate limit blocked for %L but should block for %R" ;
      unit
  | `Close, _ ->
      let* reason =
        Lwt.pick
          [
            ws_server_shutdown;
            (let* () = Lwt_unix.sleep 5. in
             Test.fail
               ~__LOC__
               "Websocket worker should have closed on rate limit.");
          ]
      in
      Check.(reason =~ rex "Rate limited by messages on websocket")
        ~error_msg:"Expected close reason to match %R, got %L" ;
      unit
  | _, [`Exn e] -> raise e

let test_websocket_frames_rate_limit () =
  let max_frames = 100 in
  let interval = 3 in
  let patch_config json =
    json
    |> Evm_node.patch_config_with_experimental_feature
         ~rpc_server:Resto (* The limit is not implemented for Dream *)
         ()
    |> Evm_node.patch_config_websockets_if_enabled
         ~monitor_heartbeat:false (* To not count ping/pong frames *)
         ~rate_limit:
           (`O
              [
                ("max_frames", `Float (float_of_int max_frames));
                ("interval", `Float (float_of_int interval));
              ])
  in

  register_sandbox
    ~__FILE__
    ~tags:["evm"; "rpc"; "websocket"; "rate_limit"; "frames"]
    ~title:"Websocket server limits rate of frames"
    ~patch_config
    ~websockets:true
  @@ fun sequencer ->
  let* websocket = Evm_node.open_websocket sequencer in
  let*@ _ = produce_block sequencer in
  let send_frames n =
    List.init n Fun.id
    |> Lwt_list.iter_p (fun _ -> Websocket.send_raw websocket "")
  in
  let ws_server_shutdown =
    Evm_node.wait_for sequencer "websocket_shutdown.v0" @@ fun json ->
    Some JSON.(json |-> "reason" |> as_string)
  in
  Log.info "Send frames below limit" ;
  let* () = send_frames max_frames in
  let* () =
    Lwt.choose
      [
        (let* reason = ws_server_shutdown in
         Test.fail ~__LOC__ "Websocket connection closed because %s." reason);
        Lwt_unix.sleep 1.;
      ]
  in
  Log.info "Wait for rate limit interval" ;
  let* () = Lwt_unix.sleep (float_of_int interval) in
  let* () =
    Lwt.catch
      (fun () -> send_frames (max_frames + 1))
      (function Websocket.Connection_closed -> unit | e -> raise e)
  in
  let* reason =
    Lwt.pick
      [
        ws_server_shutdown;
        (let* () = Lwt_unix.sleep 5. in
         Test.fail ~__LOC__ "Websocket worker did not close.");
      ]
  in
  Log.info "Websocket shutdown reason: %S." reason ;
  Check.(reason =~ rex "Rate limited by frames on websocket")
    ~error_msg:"Expected close reason to match %R, got %L" ;
  unit

let test_websocket_heartbeat_monitoring () =
  let patch_config json =
    json
    |> Evm_node.patch_config_with_experimental_feature
         ~rpc_server:Resto (* Monitoring is not implemented for Dream *)
         ()
    |> Evm_node.patch_config_websockets_if_enabled ~monitor_heartbeat:true
  in
  register_sandbox
    ~__FILE__
    ~tags:["evm"; "rpc"; "websocket"; "monitoring"]
    ~title:"Websocket server closes connection when client unresponsive"
    ~patch_config
    ~websockets:true
  @@ fun sequencer ->
  let ws_connection_established =
    Evm_node.wait_for sequencer "websocket_starting.v0" @@ fun _json -> Some ()
  in

  let* websocket = Evm_node.open_websocket sequencer in
  let shutdown =
    Lwt.pick
      [
        ( Evm_node.wait_for sequencer "websocket_shutdown.v0" @@ fun json ->
          Some JSON.(json |-> "reason" |> as_string) );
        (let* () = Lwt_unix.sleep 5. in
         Test.fail ~__LOC__ "Websocket worker did not close.");
      ]
  in
  Log.info "Wait for websocket client to be connected" ;
  let* () = ws_connection_established in
  Log.info "Pausing websocket client" ;
  Websocket.pause websocket ;
  let* reason = shutdown in
  Log.info "Websocket shutdown reason: %S." reason ;
  Check.(reason =~ rex "Timeout")
    ~error_msg:"Expected reason to match %R, got %L" ;
  unit

let test_websocket_newPendingTransactions_event =
  register_all
    ~__FILE__
    ~tags:["evm"; "rpc"; "websocket"; "new_pending_transactions"]
    ~title:
      "Check that websocket event `newPendingTransactions` is behaving \
       correctly"
    ~time_between_blocks:Nothing
    ~eth_bootstrap_accounts:
      ((Array.to_list Eth_account.bootstrap_accounts
       |> List.map (fun a -> a.Eth_account.address))
      @ Eth_account.lots_of_address)
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
    ~websockets:true
  @@ fun {sequencer; sc_rollup_node; _} _protocol ->
  let* observer =
    run_new_observer_node
      ~finalized_view:false
      ~sc_rollup_node:(Some sc_rollup_node)
      ~websockets:true
      sequencer
  in
  let* rpc_node = run_new_rpc_endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let scenario evm_node transaction_hash =
    let* websocket = Evm_node.open_websocket evm_node in
    let* id = Rpc.subscribe ~websocket ~kind:NewPendingTransactions evm_node in
    (* In observer mode, the transaction will never be mined which will cause
       the following lines of code to be blocking hence the [Background.register]. *)
    Background.register
      (Lwt.catch
         (fun () ->
           let _ =
             Eth_cli.transaction_send
               ~source_private_key:sender.private_key
               ~to_public_key:sender.address
               ~value:(Wei.of_eth_int 10)
               ~endpoint:(Evm_node.endpoint evm_node)
               ()
           in
           unit)
         (fun exn ->
           Log.debug "TX send failed because %s" (Printexc.to_string exn) ;
           unit)) ;
    let* tx = Websocket.recv ~timeout:10. websocket in
    let tx_hash = JSON.(tx |-> "params" |-> "result" |> as_string) in
    Check.((transaction_hash = tx_hash) string)
      ~error_msg:"Received tx_hash was %R, expected %L" ;
    check_unsubscription ~websocket ~id ~sequencer evm_node
  in
  Log.info "Scenario with sequencer" ;
  let* () =
    scenario
      sequencer
      "0x1b5678a27af55582f2bd6fa07223ff59ee93e16c228e40a948d09ee593560d36"
  in
  Log.info "Scenario with observer" ;
  let* () =
    scenario
      observer
      "0xf5e6dcb59cbf260cfe04d89d07a0f270c11e489a6de4df319916c7ddb19f3a34"
  in
  let* _ = produce_block_and_wait_for_sync ~sequencer observer in
  Log.info "Scenario with RPC node" ;
  let* () =
    scenario
      rpc_node
      "0x10318840345abfa7d61f34ce6b09061176f375062802646f98dc2e88f0639bdf"
  in
  unit

let test_websocket_logs_event =
  register_all
    ~__FILE__
    ~tags:["evm"; "rpc"; "websocket"; "logs"]
    ~title:"Check that websocket event `logs` is behaving correctly"
    ~time_between_blocks:Nothing
    ~eth_bootstrap_accounts:
      ((Array.to_list Eth_account.bootstrap_accounts
       |> List.map (fun a -> a.Eth_account.address))
      @ Eth_account.lots_of_address)
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
    ~websockets:true
  @@ fun {sequencer; observer; evm_version; _} _protocol ->
  let scenario evm_node =
    let* websocket = Evm_node.open_websocket evm_node in
    let endpoint = Evm_node.endpoint evm_node in
    let sender = Eth_account.bootstrap_accounts.(0) in
    let* erc20 = Solidity_contracts.erc20 evm_version in
    let* () = Eth_cli.add_abi ~label:erc20.label ~abi:erc20.abi () in
    let* address, _tx =
      send_transaction_to_sequencer
        (Eth_cli.deploy
           ~source_private_key:sender.private_key
           ~endpoint
           ~abi:erc20.label
           ~bin:erc20.bin)
        sequencer
    in
    let address = String.lowercase_ascii address in
    let transfer_event_topic =
      let h =
        Tezos_crypto.Hacl.Hash.Keccak_256.digest
          (Bytes.of_string "Transfer(address,address,uint256)")
      in
      "0x" ^ Hex.show (Hex.of_bytes h)
    in
    let zero_address = "0x" ^ String.make 64 '0' in
    let value = 42 in
    let* id_all_logs =
      Rpc.subscribe
        ~websocket
        ~kind:(Logs (Some {address = Some (Single address); topics = None}))
        evm_node
    in
    let* _tx =
      send_transaction_to_sequencer
        (Eth_cli.contract_send
           ~source_private_key:sender.private_key
           ~endpoint
           ~abi_label:erc20.label
           ~address
           ~method_call:(Printf.sprintf "mint(%d)" value))
        sequencer
    in
    let* json = Websocket.recv ~timeout:10. websocket in
    let logs =
      JSON.(json |-> "params" |-> "result" |> Transaction.logs_of_json)
    in
    Check.((address = logs.address) string)
      ~error_msg:"Received logs address was %R, expected %L" ;
    Check.(
      ("0x000000000000000000000000000000000000000000000000000000000000002a"
     = logs.data)
        string)
      ~error_msg:"Received logs data was %R, expected %L" ;
    let* sub_status = Rpc.unsubscribe ~websocket ~id:id_all_logs evm_node in
    Check.((sub_status = true) bool)
      ~error_msg:"Unsubscription from logs should be successful" ;
    let* _id_specific_topics =
      Rpc.subscribe
        ~websocket
        ~kind:
          (Logs
             (Some
                {
                  address = None;
                  topics =
                    Some
                      [
                        transfer_event_topic;
                        hex_256_of_address sender;
                        zero_address;
                      ];
                }))
        evm_node
    in
    let* _tx =
      send_transaction_to_sequencer
        (Eth_cli.contract_send
           ~expect_failure:false
           ~source_private_key:sender.private_key
           ~endpoint
           ~abi_label:erc20.label
           ~address
           ~method_call:(Printf.sprintf "burn(%d)" value))
        sequencer
    in
    let* json = Websocket.recv ~timeout:10. websocket in
    let sender_burn_logs =
      JSON.(json |-> "params" |-> "result" |> Transaction.logs_of_json)
    in
    Check.((address = logs.address) string)
      ~error_msg:"Received logs address was %R, expected %L" ;
    Check.(("0x" ^ hex_256_of_int value = logs.data) string)
      ~error_msg:"Received logs data was %R, expected %L" ;
    Check.(
      (sender_burn_logs.topics
      = [transfer_event_topic; hex_256_of_address sender; zero_address])
        (list string))
      ~error_msg:"Expected topics %R, got %L" ;
    unit
  in
  let* rpc_node = run_new_rpc_endpoint sequencer in
  let* () = scenario sequencer in
  let* () = scenario observer in
  let* () = scenario rpc_node in
  unit

let test_websocket_tez_newIncludedTransactions_event =
  register_test
    ~__FILE__
    ~time_between_blocks:Nothing
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
    ~eth_bootstrap_accounts:
      ((Array.to_list Eth_account.bootstrap_accounts
       |> List.map (fun a -> a.Eth_account.address))
      @ Eth_account.lots_of_address)
    ~tags:["evm"; "rpc"; "websocket"; "tez_new_included_transactions"]
    ~title:
      "Check that websocket event `newIncludedTransactions` is behaving \
       correctly"
    ~kernel:Latest
    ~websockets:true
    ~enable_dal:false
  @@ fun {sequencer; observer; _} _protocol ->
  (*
    To avoid updating all the tooling.
    This experimental feature will be deprecated in the short future.
  *)
  let patch_config =
    Evm_node.patch_config_with_experimental_feature
      ~preconfirmation_stream_enabled:true
      ()
  in
  let* () = Evm_node.terminate sequencer in
  let* () = Evm_node.terminate observer in
  let* () = Evm_node.Config_file.update sequencer patch_config in
  let* () = Evm_node.Config_file.update observer patch_config in
  let p = Evm_node.wait_for_start_history_mode observer in
  let* () = Evm_node.run sequencer in
  let* () = Evm_node.run observer in
  let* _ = p in
  let* _res = produce_block sequencer in
  let* rpc_node = run_new_rpc_endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let scenario evm_node transaction_hash =
    let* websocket = Evm_node.open_websocket evm_node in
    let* id = Rpc.subscribe ~websocket ~kind:NewIncludedTransactions evm_node in
    (* In observer mode, the transaction will never be mined which will cause
       the following lines of code to be blocking hence the [Background.register]. *)
    Background.register
      (Lwt.catch
         (fun () ->
           let _ =
             Eth_cli.transaction_send
               ~source_private_key:sender.private_key
               ~to_public_key:sender.address
               ~value:(Wei.of_eth_int 10)
               ~endpoint:(Evm_node.endpoint evm_node)
               ()
           in
           unit)
         (fun exn ->
           Log.debug "TX send failed because %s" (Printexc.to_string exn) ;
           unit)) ;
    let* tx = Websocket.recv ~timeout:10. websocket in
    let tx =
      JSON.(tx |-> "params" |-> "result" |> transaction_object_of_json)
    in
    Check.((transaction_hash = tx.hash) string)
      ~error_msg:"Received tx_hash was %R, expected %L" ;
    check_unsubscription ~websocket ~id ~sequencer evm_node
  in
  Log.info "Scenario with sequencer" ;
  let* () =
    scenario
      sequencer
      "0x1b5678a27af55582f2bd6fa07223ff59ee93e16c228e40a948d09ee593560d36"
  in
  Log.info "Scenario with observer" ;
  let* () =
    scenario
      observer
      "0xf5e6dcb59cbf260cfe04d89d07a0f270c11e489a6de4df319916c7ddb19f3a34"
  in
  let* _ = produce_block_and_wait_for_sync ~sequencer observer in
  Log.info "Scenario with RPC node" ;
  let* () =
    scenario
      rpc_node
      "0x10318840345abfa7d61f34ce6b09061176f375062802646f98dc2e88f0639bdf"
  in
  unit

let test_websocket_tez_newPreconfirmedReceipts_event =
  register_test
    ~__FILE__
    ~time_between_blocks:Nothing
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
    ~eth_bootstrap_accounts:
      ((Array.to_list Eth_account.bootstrap_accounts
       |> List.map (fun a -> a.Eth_account.address))
      @ Eth_account.lots_of_address)
    ~tags:["evm"; "rpc"; "websocket"; "tez_new_preconfirmed_receipts"]
    ~title:
      "Check that websocket event `newPreconfirmedReceipts` is behaving \
       correctly"
    ~kernel:Latest
    ~websockets:true
    ~enable_dal:false
  @@ fun {sequencer; observer; _} _protocol ->
  (*
    To avoid updating all the tooling.
    This experimental feature will be deprecated in the short future.
  *)
  let patch_config =
    Evm_node.patch_config_with_experimental_feature
      ~preconfirmation_stream_enabled:true
      ()
  in
  let* () = Evm_node.terminate sequencer in
  let* () = Evm_node.terminate observer in
  let* () = Evm_node.Config_file.update sequencer patch_config in
  let* () = Evm_node.Config_file.update observer patch_config in
  let p = Evm_node.wait_for_start_history_mode observer in
  let* () = Evm_node.run sequencer in
  let* () = Evm_node.run observer in
  let* _ = p in
  let* _res = produce_block sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let scenario evm_node transaction_hash =
    let* websocket = Evm_node.open_websocket evm_node in
    let* id = Rpc.subscribe ~websocket ~kind:NewPreconfirmedReceipts evm_node in
    (* In observer mode, the transaction will never be mined which will cause
       the following lines of code to be blocking hence the [Background.register]. *)
    Background.register
      (Lwt.catch
         (fun () ->
           let* _ =
             Eth_cli.transaction_send
               ~source_private_key:sender.private_key
               ~to_public_key:sender.address
               ~value:(Wei.of_eth_int 10)
               ~endpoint:(Evm_node.endpoint evm_node)
               ()
           in
           unit)
         (fun exn ->
           Log.debug "TX send failed because %s" (Printexc.to_string exn) ;
           unit)) ;
    let* receipt = Websocket.recv ~timeout:10. websocket in
    let receipt =
      JSON.(
        receipt |-> "params" |-> "result"
        |> Transaction.transaction_receipt_of_json)
    in
    Check.((transaction_hash = receipt.transactionHash) string)
      ~error_msg:"Received tx_hash was %R, expected %L" ;
    check_unsubscription ~websocket ~id ~sequencer evm_node
  in
  Log.info "Scenario with observer" ;
  let* () =
    scenario
      observer
      "0x1b5678a27af55582f2bd6fa07223ff59ee93e16c228e40a948d09ee593560d36"
  in
  let* _ = produce_block_and_wait_for_sync ~sequencer observer in
  unit

let bake_l1_until_finalized ~sc_rollup_node ~sequencer ~client ~level =
  let l1_finality_lag = 2 in
  let bake () =
    let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
    unit
  in
  let* current = Client.level client in
  let* () = repeat (max 0 (level - current + l1_finality_lag)) bake
  and* _ =
    Evm_node.wait_for_processed_l1_level ~timeout:300. ~level sequencer
  in
  unit

let drain_l1_l2_levels ?(silence = 3.) websocket =
  Lwt_stream.from @@ fun () ->
  Lwt.catch
    (fun () ->
      let* json = Websocket.recv ~timeout:silence websocket in
      let level =
        JSON.(json |-> "params" |-> "result" |-> "l1Level" |> as_int)
      in
      return (Some level))
    (function Websocket.Timeout _ -> return None | exn -> Lwt.reraise exn)

let test_websocket_tez_l1L2Levels_min_level =
  register_test
    ~__FILE__
    ~time_between_blocks:Nothing
    ~kernel:Latest
    ~enable_dal:false
    ~websockets:true
    ~tags:["evm"; "rpc"; "websocket"; "tez_l1_l2_levels"]
    ~title:
      "Check that websocket event `tez_l1L2Levels` terminates on a minimal \
       fromL1Level"
  @@ fun {sequencer; sc_rollup_node; client; _} _protocol ->
  let first_l1_level = 5 in
  let last_l1_level = 10 in
  let* () =
    bake_l1_until_finalized
      ~sc_rollup_node
      ~sequencer
      ~client
      ~level:last_l1_level
  in
  let* websocket = Evm_node.open_websocket sequencer in
  let* _id =
    Evm_node.resolve_or_timeout
      ~timeout:120.
      sequencer
      ~name:"tez_l1L2Levels subscription"
      (Rpc.subscribe
         ~websocket
         ~kind:(L1L2Levels (Some (Int32.to_int Int32.min_int)))
         sequencer)
  in
  let* levels = Lwt_stream.to_list (drain_l1_l2_levels websocket) in
  let expected_levels =
    List.init (last_l1_level - first_l1_level + 1) (fun i -> first_l1_level + i)
  in
  Check.((levels = expected_levels) (list int))
    ~error_msg:"Replayed L1 levels %L, expected %R" ;
  unit

let protocols = [Protocol.Alpha]

let () =
  test_websocket_rpcs [Protocol.Alpha] ;
  test_websocket_subscription_rpcs_cant_be_called_via_http_requests
    [Protocol.Alpha] ;
  test_websocket_newHeads_event [Protocol.Alpha] ;
  test_websocket_cleanup [Protocol.Alpha] ;
  test_websocket_max_message_length () ;
  test_websocket_rate_limit `Close ;
  test_websocket_rate_limit `Error ;
  test_websocket_rate_limit `Wait ;
  test_websocket_frames_rate_limit () ;
  test_websocket_heartbeat_monitoring () ;
  test_websocket_newPendingTransactions_event [Protocol.Alpha] ;
  test_websocket_logs_event [Protocol.Alpha] ;
  test_websocket_tez_newIncludedTransactions_event [Protocol.Alpha] ;
  test_websocket_tez_newPreconfirmedReceipts_event [Protocol.Alpha] ;
  test_websocket_tez_l1L2Levels_min_level [Protocol.Alpha]
