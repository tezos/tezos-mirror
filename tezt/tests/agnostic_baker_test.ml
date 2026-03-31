(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Agnostic baker (octez-agnostic-baker)
   Invocation:   dune exec tezt/tests/main.exe -- --file agnostic_baker_test.ml
   Subject:      Ensure that the agnostic baker behaves as expected

   Tests
   -----
   - each migration test starts from an [Active] protocol (which has a baker binary)
   and migrates to the next protocol
   - each migration test can either use remote signing or not
   - each migration test can either be a resilience or normal test; the former testing
   that killing the baker during migration results in a graceful restart
   - there is also a start and stop test
*)

let team = Tag.layer1

let wait_for_active_protocol_waiting agnostic_baker =
  Agnostic_baker.wait_for
    agnostic_baker
    "waiting_for_active_protocol.v0"
    (fun _json -> Some ())

let wait_for_become_old_baker agnostic_baker =
  Agnostic_baker.wait_for agnostic_baker "become_old_agent.v0" (fun _json ->
      Some ())

let wait_for_become_old_accuser accuser =
  Lwt.pick
    [
      (let* () = Lwt_unix.sleep 60. in
       Test.fail
         "[wait_for_become_old_baker] has waited for 60 seconds, exiting");
      Accuser.wait_for accuser "become_old_agent.v0" (fun _json -> Some ());
    ]

let wait_for_stopping_baker agnostic_baker =
  Agnostic_baker.wait_for agnostic_baker "stopping_agent.v0" (fun _json ->
      Some ())

let wait_for_stopping_accuser accuser =
  Accuser.wait_for accuser "stopping_agent.v0" (fun _json -> Some ())

let remote_sign client =
  let* () = Client.forget_all_keys client in
  let keys = Constant.activator :: (Account.Bootstrap.keys |> Array.to_list) in
  let* signer = Signer.init ~keys () in
  (* tell the baker to ask the signer for the bootstrap keys *)
  let uri = Signer.uri signer in
  Lwt_list.iter_s
    (fun account ->
      let Account.{alias; public_key_hash; _} = account in
      Client.import_signer_key client ~alias ~public_key_hash ~signer:uri)
    keys

(* Performs a protocol migration thanks to a UAU and the agnostic baker. *)
let perform_protocol_migration ?node_name ?client_name ?parameter_file
    ?(use_remote_signer = false) ?(extra_run_arguments = []) ?on_new_baker_ready
    ~blocks_per_cycle ~migration_level ~migrate_from ~migrate_to
    ~baked_blocks_after_migration () =
  assert (migration_level >= blocks_per_cycle) ;
  Log.info "Node starting" ;
  let* client, node =
    Protocol_migration.user_migratable_node_init
      ?node_name
      ?client_name
      ~migration_level
      ~migrate_to
      ()
  in
  let* () = if use_remote_signer then remote_sign client else unit in
  Log.info "Node %s initialized" (Node.name node) ;
  let baker = Agnostic_baker.create node client in
  let accuser = Accuser.create node in
  let wait_for_active_protocol_waiting =
    wait_for_active_protocol_waiting baker
  in
  Log.info "Starting agnostic baker" ;
  let* () = Agnostic_baker.run ~extra_arguments:extra_run_arguments baker in
  Log.info "Starting agnostic accuser" ;
  let* () = Accuser.run accuser in

  let* () = wait_for_active_protocol_waiting in
  let* () = Agnostic_baker.wait_for_ready baker in
  let* () =
    Client.activate_protocol
      ~protocol:migrate_from
      client
      ?parameter_file
      ~timestamp:Client.Now
  in
  Log.info "Protocol %s activated" (Protocol.hash migrate_from) ;
  Log.info "Baking at least %d blocks to trigger migration" migration_level ;
  let* _level = Node.wait_for_level node migration_level in
  let wait_for_become_old_baker = wait_for_become_old_baker baker in
  let wait_for_become_old_accuser = wait_for_become_old_accuser accuser in
  Log.info
    "Check that the baking process for %s is not killed"
    (Protocol.tag migrate_from) ;
  let* () = wait_for_become_old_baker in
  let* () = wait_for_become_old_accuser in
  let* () =
    match on_new_baker_ready with
    | None -> Lwt.return_unit
    | Some f -> f ~node ~client ~baker
  in
  (* Ensure that the block before migration is consistent *)
  Log.info "Checking migration block consistency" ;
  let* () =
    Protocol_migration.block_check
      ~expected_block_type:`Migration
      client
      ~migrate_from
      ~migrate_to
      ~level:migration_level
  in
  let* _level = Node.wait_for_level node (migration_level + 1) in
  (* Ensure that we migrated *)
  Log.info "Checking migration block consistency" ;
  let* () =
    Protocol_migration.block_check
      ~expected_block_type:`Non_migration
      client
      ~migrate_from
      ~migrate_to
      ~level:(migration_level + 1)
  in
  Log.info
    "Check that baker for protocol %s is killed after %d levels"
    (Protocol.tag migrate_from)
    Agnostic_baker.extra_levels_for_old_baker ;
  let wait_for_stopping_baker = wait_for_stopping_baker baker in
  let wait_for_stopping_accuser = wait_for_stopping_accuser accuser in
  let* _level =
    Node.wait_for_level
      node
      (migration_level + Agnostic_baker.extra_levels_for_old_baker)
  in
  let* () = wait_for_stopping_baker and* () = wait_for_stopping_accuser in
  (* Test that we can still bake after migration *)
  let* _level = Node.wait_for_level node baked_blocks_after_migration in
  let* () = Agnostic_baker.terminate baker
  and* () = Accuser.terminate accuser in
  unit

let migrate ~migrate_from ~migrate_to ~use_remote_signer =
  let remote_signer_text, remote_signer =
    if use_remote_signer then
      (" using HTTP remote signer", [Constant.octez_signer])
    else ("", [])
  in
  let parameters = JSON.parse_file (Protocol.parameter_file migrate_to) in
  Test.register
    ~__FILE__
    ~title:
      (Format.asprintf
         "Protocol migration from protocol %s to protocol %s with agnostic \
          baker%s"
         (Protocol.tag migrate_from)
         (Protocol.tag migrate_to)
         remote_signer_text)
    ~tags:
      [
        "protocol";
        "migration";
        "agnostic";
        "baker";
        Protocol.tag migrate_from;
        Protocol.tag migrate_to;
      ]
    ~uses:
      ([Constant.octez_agnostic_baker; Constant.octez_accuser] @ remote_signer)
  @@ fun () ->
  let blocks_per_cycle = JSON.(get "blocks_per_cycle" parameters |> as_int) in
  let consensus_rights_delay =
    JSON.(get "consensus_rights_delay" parameters |> as_int)
  in
  (* Migration level is set arbitrarily *)
  let migration_level = blocks_per_cycle in
  let* () =
    perform_protocol_migration
      ~use_remote_signer
      ~blocks_per_cycle
      ~migration_level
      ~migrate_from
      ~migrate_to
      ~baked_blocks_after_migration:
        (2 * consensus_rights_delay * blocks_per_cycle)
      ()
  in
  unit

let reconnect_after_migration ~migrate_from ~migrate_to =
  let parameters = JSON.parse_file (Protocol.parameter_file migrate_to) in
  Test.register
    ~__FILE__
    ~title:
      (Format.asprintf
         "Protocol migration reconnect from %s to %s"
         (Protocol.tag migrate_from)
         (Protocol.tag migrate_to))
    ~tags:
      [
        "protocol";
        "migration";
        "agnostic";
        "baker";
        "reconnect";
        Protocol.tag migrate_from;
        Protocol.tag migrate_to;
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/8171
           Reconnection during a protocol migration kills the baker. *)
        Tag.ci_disabled;
      ]
    ~uses:[Constant.octez_agnostic_baker]
  @@ fun () ->
  let blocks_per_cycle = JSON.(get "blocks_per_cycle" parameters |> as_int) in
  let consensus_rights_delay =
    JSON.(get "consensus_rights_delay" parameters |> as_int)
  in
  let baked_blocks_after_migration =
    2 * consensus_rights_delay * blocks_per_cycle
  in
  let migration_level = blocks_per_cycle in

  Log.info "Node starting" ;
  let* client, node =
    Protocol_migration.user_migratable_node_init ~migration_level ~migrate_to ()
  in
  Log.info "Node %s initialized" (Node.name node) ;
  let baker = Agnostic_baker.create node client in

  let wait_for_active_protocol_waiting =
    wait_for_active_protocol_waiting baker
  in
  let wait_for_ready () = Agnostic_baker.wait_for_ready baker in
  let wait_for_period_status () =
    Agnostic_baker.wait_for baker "period_status.v0" (fun _ -> Some ())
  in
  let wait_for_become_old_baker = wait_for_become_old_baker baker in
  let wait_for_stopping_baker = wait_for_stopping_baker baker in

  let timeout message seconds =
    let* () = Lwt_unix.sleep seconds in
    Test.fail message
  in

  Log.info "Starting agnostic baker" ;
  let* () = Agnostic_baker.run ~extra_arguments:["--keep-alive"] baker in
  let* () = wait_for_active_protocol_waiting in
  let* () = wait_for_ready () in
  let* () =
    Client.activate_protocol ~protocol:migrate_from client ~timestamp:Client.Now
  in
  Log.info "Protocol %s activated" (Protocol.hash migrate_from) ;
  Log.info "Baking at least %d blocks to trigger migration" migration_level ;
  let* _level = Node.wait_for_level node migration_level in
  Log.info
    "Check that the baking process for %s is not killed"
    (Protocol.tag migrate_from) ;
  let* () = wait_for_become_old_baker in

  (* Prepare watchers for the restart before it happens. *)
  let wait_ready_after_restart = wait_for_ready () in
  let wait_period_status_after_restart = wait_for_period_status () in

  Log.info "Restarting node to check baker reconnection" ;
  let* () = Node.terminate node in
  let* () = Lwt_unix.sleep 1. in
  let* () = Node.run node [] in
  let* () = Node.wait_for_ready node in
  let* () =
    Lwt.pick
      [
        (let* () = wait_ready_after_restart in
         Log.info "Agnostic baker reported ready after node restart" ;
         Lwt.return_unit);
        timeout "Baker did not report ready after node restart" 60.;
      ]
  in
  let* () =
    Lwt.pick
      [
        (let* () = wait_period_status_after_restart in
         Log.info "Agnostic baker emitted period_status after node restart" ;
         Lwt.return_unit);
        timeout "Baker did not emit period_status after node restart" 120.;
      ]
  in

  Log.info
    "Check that baker for protocol %s is killed after %d levels"
    (Protocol.tag migrate_from)
    Agnostic_baker.extra_levels_for_old_baker ;
  let kill_level =
    migration_level + Agnostic_baker.extra_levels_for_old_baker
  in
  let* _level = Node.wait_for_level node kill_level in
  let* () = wait_for_stopping_baker in

  let* _level = Node.wait_for_level node baked_blocks_after_migration in
  let* () = Agnostic_baker.terminate baker in
  unit

let test_start_and_stop () =
  Test.register
    ~__FILE__
    ~title:"Agnostic baker starts and stops"
    ~tags:[team; "sandbox"; "agnostic"; "baker"; "init"]
    ~uses:[Constant.octez_agnostic_baker]
  @@ fun () ->
  let* node, client = Client.init_with_node `Client () in
  let* baker = Agnostic_baker.init node client in
  let* () = Agnostic_baker.wait_for_ready baker in
  let* () = Agnostic_baker.terminate baker in
  unit

let test_man () =
  Regression.register
    ~__FILE__
    ~title:"Agnostic baker man"
    ~tags:[team; "sandbox"; "agnostic"; "baker"; "man"]
    ~uses:[Constant.octez_agnostic_baker]
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun () ->
  let hooks = Tezos_regression.hooks in
  let* () = Process.run ~hooks (Uses.path Constant.octez_agnostic_baker) [] in
  let* () =
    Process.run ~hooks (Uses.path Constant.octez_agnostic_baker) ["--help"]
  in
  let* () =
    Process.run ~hooks (Uses.path Constant.octez_agnostic_baker) ["man"]
  in
  unit

let test_keep_alive =
  Protocol.register_test
    ~__FILE__
    ~title:"Agnostic baker --keep-alive"
    ~tags:[team; "sandbox"; "agnostic"; "baker"; "keep_alive"]
    ~uses:(fun _ -> [Constant.octez_agnostic_baker])
  @@ fun protocol ->
  let* node, client = Client.init_with_protocol ~protocol `Client () in
  (* Provide a delegate with no voting power to avoid baking blocks with the baker
     and the client simultaneously. *)
  let baker =
    Agnostic_baker.create
      ~delegates:[Constant.activator.public_key_hash]
      ~node_version_check_bypass:true
      node
      client
  in
  let* () = Node.terminate node in
  (* Start the baker with no node running and no [--keep-alive], it crashes. *)
  let process = Agnostic_baker.spawn_run baker in
  let* () = Process.check_error ~msg:(rex "Cannot connect to node") process in
  (* Start the baker with no node running and [--keep-alive], it'll wait. *)
  let wait_for_retry_on_disconnection () =
    Agnostic_baker.wait_for baker "retry_on_disconnection.v0" (fun _json ->
        Some ())
  in
  let* () = Agnostic_baker.run ~extra_arguments:["--keep-alive"] baker
  and* () = wait_for_retry_on_disconnection () in
  (* Start the node. *)
  let f_wait_for_chain_id () =
    (* This is an event emitted by the baker lib. *)
    Agnostic_baker.wait_for baker "node_chain_id.v0" (fun _json -> Some ())
  in
  let wait_for_chain_id = f_wait_for_chain_id () in
  let* () = Node.run node []
  and* () = Node.wait_for_ready node
  and* () = wait_for_chain_id in
  (* Bake a block, the baker is connected so it'll see it. *)
  let f_wait_baker_proposal () =
    (* This is an event emitted by the baker lib. *)
    Agnostic_baker.wait_for baker "new_valid_proposal.v0" (fun _json -> Some ())
  in
  let f_wait_period_status () =
    (* This is an event emitted by the agnostic baker. *)
    Agnostic_baker.wait_for baker "period_status.v0" (fun _json -> Some ())
  in
  let wait_baker_proposal = f_wait_baker_proposal () in
  let wait_period_status = f_wait_period_status () in
  let* () = Client.bake_for_and_wait client
  and* () = wait_baker_proposal
  and* () = wait_period_status in
  (* Kill the node now that they are connected, the baker will stay alive. *)
  let* () = Node.terminate node and* () = wait_for_retry_on_disconnection () in
  (* Redo the procedure, restart the node and wait for the block events. *)
  let wait_for_chain_id = f_wait_for_chain_id () in
  let* () = Node.run node []
  and* () = Node.wait_for_ready node
  and* () = wait_for_chain_id in

  let wait_baker_proposal = f_wait_baker_proposal () in
  let wait_period_status = f_wait_period_status () in
  let* () = Client.bake_for_and_wait client
  and* () = wait_baker_proposal
  and* () = wait_period_status in
  unit

let test_cli =
  Protocol.register_test
    ~__FILE__
    ~title:"Agnostic baker CLI"
    ~tags:[team; "sandbox"; "agnostic"; "baker"; "cli"]
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
  @@ fun protocol ->
  let* node, client = Client.init_with_protocol `Client ~protocol () in
  let baker = Agnostic_baker.create node client in
  let check_process_error expected_err p =
    let* err = Process.check_and_read_stderr ~expect_failure:true p in
    Check.(err =~ rex expected_err) ~error_msg:"Expected error %R but got %L" ;
    unit
  in

  let arguments =
    [
      "--endpoint";
      Node.rpc_endpoint node;
      "--base-dir";
      Client.base_dir client;
      "run";
      "with";
      "local";
      "node";
      Node.data_dir node;
    ]
  in
  let p = Agnostic_baker.spawn_raw ~arguments baker in
  let* () = check_process_error ".*Missing liquidity baking toggle vote.*" p in

  let arguments = arguments @ ["--liquidity-baking-toggle-vote"; "pass"] in
  let p = Agnostic_baker.spawn_raw ~arguments baker in
  let* () = check_process_error "Please connect a running DAL node using" p in

  let arguments_with_bad_extra_node =
    arguments @ ["--without-dal"; "--extra-node"; "file:///etc/passwd"]
  in
  let p =
    Agnostic_baker.spawn_raw ~arguments:arguments_with_bad_extra_node baker
  in
  let* () =
    check_process_error "only http and https endpoints are supported" p
  in

  let arguments = arguments @ ["--without-dal"] in
  let* () = Agnostic_baker.wait_for_ready baker
  and* () = Agnostic_baker.raw ~arguments baker in
  unit

let test_multi_node_connection_recovery =
  Protocol.register_test
    ~__FILE__
    ~title:"Agnostic baker multi-node connection recovery"
    ~tags:[team; "sandbox"; "agnostic"; "baker"; "multi_node"]
    ~supports:Protocol.(From_protocol (number Alpha))
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
  @@ fun protocol ->
  Log.info "Setting up two nodes connected via P2P" ;
  (* Setup: 2 nodes sharing the same chain via P2P *)
  let* node1 = Node.init [Synchronisation_threshold 0; Private_mode] in
  let node1_uri =
    Node.as_rpc_endpoint node1 |> Endpoint.to_uri |> Uri.to_string
  in
  let* node2 = Node.init [Synchronisation_threshold 0; Private_mode] in
  let node2_uri =
    Node.as_rpc_endpoint node2 |> Endpoint.to_uri |> Uri.to_string
  in

  let* client1 = Client.init ~endpoint:(Node node1) () in
  let* client2 = Client.init ~endpoint:(Node node2) () in

  (* Set up P2P trust relationships (required for Private_mode) *)
  let* () = Client.Admin.trust_address client1 ~peer:node2 in
  let* () = Client.Admin.trust_address client2 ~peer:node1 in

  (* Connect the nodes *)
  let* () = Client.Admin.connect_address client1 ~peer:node2 in

  (* Activate protocol on node1 (will propagate to node2) *)
  let* () = Client.activate_protocol ~protocol client1 in

  (* Wait for both nodes to sync to level 1 *)
  let* _ = Node.wait_for_level node1 1 in
  let* _ = Node.wait_for_level node2 1 in
  Log.info "✓ Two nodes set up, connected via P2P, and synced" ;

  Log.info "Creating baker connected to both nodes using --extra-node" ;
  (* Use activator (no voting power) to avoid conflict with manual baking *)
  let baker =
    Agnostic_baker.create
      ~delegates:[Constant.activator.public_key_hash]
      ~extra_nodes:[Node.as_rpc_endpoint node2]
      ~keep_alive:true
      ~remote_mode:true
      node1
      client1
  in

  (* Start baker and wait for ready *)
  Log.info
    "Starting baker with --extra-node %s"
    (Endpoint.as_string (Node.as_rpc_endpoint node2)) ;
  let* () = Agnostic_baker.run ~event_level:`Debug baker in
  let* () = Agnostic_baker.wait_for_ready baker in
  Log.info "✓ Baker is ready and monitoring both nodes" ;

  (* Helper: wait for daemon events *)
  let wait_for_node_connection_lost uri =
    Agnostic_baker.wait_for
      baker
      "agnostic_daemon_node_connection_lost.v0"
      (fun json ->
        let uri' = JSON.as_string json in
        if uri = uri' then Some () else None)
  in

  let wait_for_node_connection_restored () =
    Agnostic_baker.wait_for
      baker
      "agnostic_daemon_node_connection_restored.v0"
      (fun json ->
        let uri = JSON.as_string json in
        Log.info "Detected node_connection_restored event for: %s" uri ;
        Some uri)
  in

  let wait_for_monitoring_head_from_node level uri =
    Agnostic_baker.wait_for
      baker
      "agnostic_daemon_monitoring_head_from_node.v0"
      (fun json ->
        let level' = JSON.(json |-> "level" |> as_int) in
        let uri' = JSON.(json |-> "uri" |> as_string) in
        if level = level' && uri = uri' then Some () else None)
  in

  (* Bake a few initial blocks to ensure the baker is operational *)
  Log.info "Baking initial blocks to verify daemon is monitoring" ;
  let wait_monitor1 = wait_for_monitoring_head_from_node 2 node1_uri in
  let wait_monitor2 = wait_for_monitoring_head_from_node 2 node2_uri in

  let* () = Client.bake_for_and_wait ~keys:[] client1
  and* () = wait_monitor1
  and* () = wait_monitor2 in

  Log.info "✓ Daemon saw head at level 2 from %s" node1_uri ;
  Log.info "✓ Daemon saw head at level 2 from %s" node2_uri ;

  (* Test 1: Terminate node2 - should trigger node_connection_lost event *)
  Log.info "Terminating node2 to test connection loss detection" ;
  (* Set up the event watcher BEFORE terminating the node *)
  let wait_connection_lost = wait_for_node_connection_lost node2_uri in
  let* () = Node.terminate node2 and* () = wait_connection_lost in

  Log.info "✓ node_connection_lost event received for: %s" node2_uri ;

  (* Verify baker continues working with node1 only *)
  Log.info "Baking a block to verify baker still works with node1" ;
  let* current_level = Node.get_level node1 in
  let wait_monitor =
    wait_for_monitoring_head_from_node (current_level + 1) node1_uri
  in
  let* () = Client.bake_for_and_wait ~keys:[] client1 and* _ = wait_monitor in
  Log.info
    "✓ Baker continues operating with node1 (monitoring from %s)"
    node1_uri ;

  (* Test 2: Restart node2 - should trigger node_connection_restored event *)
  Log.info "Restarting node2 to test connection restoration" ;
  (* Set up the event watcher BEFORE restarting the node *)
  let wait_connection_restored = wait_for_node_connection_restored () in
  let* () = Node.run node2 []
  and* _ = wait_connection_restored
  and* () = Node.wait_for_ready node2 in

  (* Re-establish P2P trust (needed after restart in Private_mode) *)
  let* () = Client.Admin.trust_address client2 ~peer:node1 in
  let* () = Client.Admin.connect_address client1 ~peer:node2 in
  Log.info "Node2 restarted and P2P connected" ;

  (* Wait for node2 to sync *)
  let* node1_level = Node.get_level node1 in
  let* _ = Node.wait_for_level node2 node1_level in
  Log.info "Node2 synced to level %d" node1_level ;

  (* Verify both nodes are working again by baking more blocks *)
  Log.info "Baking blocks to verify both nodes are operational" ;
  let expected_level = node1_level + 1 in
  let wait_monitor1 =
    wait_for_monitoring_head_from_node expected_level node1_uri
  in
  let wait_monitor2 =
    wait_for_monitoring_head_from_node expected_level node2_uri
  in

  let* () = wait_monitor1
  and* () = wait_monitor2
  and* () = Client.bake_for_and_wait ~keys:[] client1 in
  Log.info "✓ Daemon saw head at level %d from %s" expected_level node1_uri ;
  Log.info "✓ Daemon saw head at level %d from %s" expected_level node2_uri ;

  (* Test completed successfully *)
  Log.info "✓ Multi-node connection recovery test completed successfully" ;
  unit

let test_multi_node_dal_worker_connection_recovery =
  Protocol.register_test
    ~__FILE__
    ~title:"Baker multi-node DAL worker connection recovery"
    ~tags:[team; "sandbox"; "agnostic"; "baker"; "multi_node"; "dal"]
    ~supports:Protocol.(From_protocol (number Alpha))
    ~uses:(fun _protocol ->
      [Constant.octez_agnostic_baker; Constant.octez_dal_node])
  @@ fun protocol ->
  Log.info "Setting up three nodes connected via P2P" ;
  (* Setup: 3 nodes sharing the same chain via P2P *)
  (* node1 and node2 are for the baker, node3 is dedicated for the DAL node *)
  (* Use Archive mode to ensure DAL node has enough history *)
  let* node1 =
    Node.init [Synchronisation_threshold 0; Private_mode; History_mode Archive]
  in
  let* node2 =
    Node.init [Synchronisation_threshold 0; Private_mode; History_mode Archive]
  in
  let* node3 =
    Node.init [Synchronisation_threshold 0; Private_mode; History_mode Archive]
  in

  let* client1 = Client.init ~endpoint:(Node node1) () in
  let* client2 = Client.init ~endpoint:(Node node2) () in
  let* client3 = Client.init ~endpoint:(Node node3) () in

  (* Set up P2P trust relationships (required for Private_mode) *)
  let* () = Client.Admin.trust_address client1 ~peer:node2 in
  let* () = Client.Admin.trust_address client1 ~peer:node3 in
  let* () = Client.Admin.trust_address client2 ~peer:node1 in
  let* () = Client.Admin.trust_address client2 ~peer:node3 in
  let* () = Client.Admin.trust_address client3 ~peer:node1 in
  let* () = Client.Admin.trust_address client3 ~peer:node2 in

  (* Connect the nodes *)
  let* () = Client.Admin.connect_address client1 ~peer:node2 in
  let* () = Client.Admin.connect_address client1 ~peer:node3 in

  (* Activate protocol with DAL enabled *)
  let dal_parameters =
    [
      (["dal_parametric"; "feature_enable"], `Bool true);
      (["dal_parametric"; "number_of_slots"], `Int 8);
      (["dal_parametric"; "attestation_lag"], `Int 4);
      ( ["dal_parametric"; "attestation_lags"],
        `A [`Float 2.; `Float 3.; `Float 4.] );
      (["dal_parametric"; "attestation_threshold"], `Int 50);
    ]
  in
  let* parameter_file =
    Protocol.write_parameter_file
      ~base:(Either.Right (protocol, None))
      dal_parameters
  in
  let* () =
    Client.activate_protocol ~protocol ~parameter_file client1 ~timestamp:Now
  in

  (* Wait for all nodes to sync to level 1 *)
  let* _ = Node.wait_for_level node1 1 in
  let* _ = Node.wait_for_level node2 1 in
  let* _ = Node.wait_for_level node3 1 in
  Log.info "✓ Three nodes set up, connected via P2P, and synced" ;

  (* Start DAL node with attester profiles for delegates, connected to node3 *)
  Log.info "Starting DAL node connected to node3" ;
  let dal_node = Dal_node.create ~node:node3 () in
  let* () =
    Dal_node.init_config
      ~attester_profiles:
        [
          Account.Bootstrap.keys.(0).public_key_hash;
          Account.Bootstrap.keys.(1).public_key_hash;
        ]
      dal_node
  in
  let* () = Dal_node.run ~wait_ready:true dal_node in
  Log.info "✓ DAL node running" ;

  Log.info "Creating baker connected to both nodes using --extra-node" ;
  (* Use two bootstrap accounts as delegates to have actual DAL committee membership *)
  let baker =
    Agnostic_baker.create
      ~delegates:[Account.Bootstrap.keys.(0).public_key_hash]
      ~extra_nodes:[Node.as_rpc_endpoint node2]
      ~dal_node_rpc_endpoint:(Dal_node.as_rpc_endpoint dal_node)
      ~keep_alive:true
      ~remote_mode:true
      node1
      client1
  in

  (* Start baker and wait for ready *)
  Log.info
    "Starting baker with --extra-node %s and DAL node endpoint %s"
    (Endpoint.as_string (Node.as_rpc_endpoint node2))
    (Endpoint.as_string (Dal_node.as_rpc_endpoint dal_node)) ;
  let* () = Agnostic_baker.run ~event_level:`Info baker in
  let* () = Agnostic_baker.wait_for_ready baker in
  Log.info "✓ Baker is ready and monitoring both nodes" ;

  (* Wait for DAL workers to start for both automatons *)
  Log.info "Waiting for DAL workers to start for both automatons" ;
  let node1_endpoint = Endpoint.as_string (Node.as_rpc_endpoint node1) in
  let node2_endpoint = Endpoint.as_string (Node.as_rpc_endpoint node2) in
  let wait_for_dal_worker_started expected_automaton =
    Agnostic_baker.wait_for baker "dal_worker_started.v0" (fun json ->
        let automaton_name = JSON.(json |-> "automaton_name" |> as_string) in
        if automaton_name = expected_automaton then Some () else None)
  in
  let* () = wait_for_dal_worker_started node1_endpoint
  and* () = wait_for_dal_worker_started node2_endpoint in
  Log.info
    "✓ DAL workers started for both automatons: %s and %s"
    node1_endpoint
    node2_endpoint ;

  (* Phase 1: Both nodes active - wait for 2 no_attestable_dal_slots_for_levels events *)
  Log.info
    "Phase 1: Both nodes active - waiting for DAL worker query events from \
     both automatons" ;
  let wait_for_dal_event_from expected_automaton =
    Agnostic_baker.wait_for
      baker
      "no_attestable_dal_slots_for_levels.v0"
      (fun json ->
        let automaton_name = JSON.(json |-> "automaton_name" |> as_string) in
        if automaton_name = expected_automaton then Some () else None)
  in
  (* Wait for one event from each automaton *)
  let* () = wait_for_dal_event_from node1_endpoint
  and* () = wait_for_dal_event_from node2_endpoint in
  Log.info
    "✓ Received no_attestable_dal_slots_for_levels events from both \
     automatons: %s and %s"
    node1_endpoint
    node2_endpoint ;

  (* Phase 2: Terminate node1 - should only see 1 event from node2's automaton *)
  Log.info
    "Phase 2: Terminating node1 - expecting only 1 DAL worker query event from \
     node2's automaton" ;
  let* () = Node.terminate node1 in
  (* Wait for just one DAL event from the remaining automaton *)
  let* () = wait_for_dal_event_from node2_endpoint
  and* () =
    Lwt.pick
      [
        (let* () = wait_for_dal_event_from node1_endpoint in
         Test.fail "We shouldn't see events from 1st automaton as it's down");
        Lwt_unix.sleep 10.;
      ]
  in
  Log.info
    "✓ Received 1 no_attestable_dal_slots_for_levels event from automaton: %s"
    node2_endpoint ;

  (* Phase 3: Restart node1 - should see 2 events again *)
  Log.info
    "Phase 3: Restarting node1 - expecting 2 DAL attestation events from both \
     automatons" ;
  let* () = Node.run node1 [] and* () = Node.wait_for_ready node1 in

  (* Re-establish P2P trust (needed after restart in Private_mode) *)
  let* () = Client.Admin.trust_address client1 ~peer:node2 in
  let* () = Client.Admin.trust_address client1 ~peer:node3 in
  let* () = Client.Admin.connect_address client2 ~peer:node1 in
  Log.info "Node1 restarted and P2P connected" ;

  (* Wait for node1 to sync *)
  let* node2_level = Node.get_level node2 in
  let* _ = Node.wait_for_level node1 node2_level in
  Log.info "Node1 synced to level %d" node2_level ;

  (* Multi node baker takes 20 seconds before retrying the connection *)
  let wait_for_dal_worker_started =
    wait_for_dal_worker_started node1_endpoint
  in

  (* Wait for DAL worker to restart for node1 automaton *)
  Log.info "Waiting for DAL worker to restart for node1 automaton" ;
  let* () = wait_for_dal_worker_started in
  Log.info "✓ DAL worker restarted for automaton: %s" node1_endpoint ;

  (* Verify both automatons emit DAL events again *)
  let* () = wait_for_dal_event_from node1_endpoint
  and* () = wait_for_dal_event_from node2_endpoint in
  Log.info
    "✓ Received no_attestable_dal_slots_for_levels events from both \
     automatons: %s and %s (both operational again)"
    node1_endpoint
    node2_endpoint ;

  (* Test completed successfully *)
  Log.info
    "✓ Multi-node DAL worker connection recovery test completed: verified \
     per-automaton DAL worker architecture" ;
  unit

let test_multi_node_nonce_connection_recovery =
  Protocol.register_test
    ~__FILE__
    ~title:"Baker multi-node nonce worker connection recovery"
    ~tags:[team; "sandbox"; "agnostic"; "baker"; "multi_node"; "nonce"]
    ~supports:Protocol.(From_protocol (number Alpha))
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
  @@ fun protocol ->
  Log.info "Setting up two nodes connected via P2P" ;
  (* Setup: 2 nodes sharing the same chain via P2P *)
  let* node1 = Node.init [Synchronisation_threshold 0; Private_mode] in
  let node1_uri =
    Node.as_rpc_endpoint node1 |> Endpoint.to_uri |> Uri.to_string
  in
  let* node2 = Node.init [Synchronisation_threshold 0; Private_mode] in
  let node2_uri =
    Node.as_rpc_endpoint node2 |> Endpoint.to_uri |> Uri.to_string
  in

  let* client1 = Client.init ~endpoint:(Node node1) () in
  let* client2 = Client.init ~endpoint:(Node node2) () in

  (* Set up P2P trust relationships (required for Private_mode) *)
  let* () = Client.Admin.trust_address client1 ~peer:node2 in
  let* () = Client.Admin.trust_address client2 ~peer:node1 in

  (* Connect the nodes *)
  let* () = Client.Admin.connect_address client1 ~peer:node2 in

  (* Activate protocol on node1 (will propagate to node2) *)
  let* () = Client.activate_protocol ~protocol client1 in

  (* Wait for both nodes to sync to level 1 *)
  let* _ = Node.wait_for_level node1 1 in
  let* _ = Node.wait_for_level node2 1 in
  Log.info "✓ Two nodes set up, connected via P2P, and synced" ;

  Log.info "Creating baker connected to both nodes using --extra-node" ;
  (* Use activator (no voting power) to avoid conflict with manual baking *)
  let baker =
    Agnostic_baker.create
      ~delegates:[Constant.activator.public_key_hash]
      ~extra_nodes:[Node.as_rpc_endpoint node2]
      ~keep_alive:true
      ~remote_mode:true
      node1
      client1
  in

  (* Start baker and wait for ready *)
  Log.info
    "Starting baker with --extra-node %s"
    (Endpoint.as_string (Node.as_rpc_endpoint node2)) ;
  let* () = Agnostic_baker.run ~event_level:`Debug baker in
  let* () = Agnostic_baker.wait_for_ready baker in
  Log.info "✓ Baker is ready and monitoring both nodes" ;

  let wait_for_revelation_worker_proposal level uri =
    Agnostic_baker.wait_for
      baker
      "revelation_worker_new_proposal.v0"
      (fun json ->
        let level' = JSON.(json |-> "level" |> as_int) in
        let uri' = JSON.(json |-> "uri" |> as_string) in
        if level = level' && uri = uri' then Some (level, uri) else None)
  in

  (* Bake a few initial blocks to ensure the baker is operational *)
  Log.info "Baking initial blocks to verify nonce worker is monitoring" ;
  let wait_proposal1 = wait_for_revelation_worker_proposal 2 node1_uri in
  let wait_proposal2 = wait_for_revelation_worker_proposal 2 node2_uri in

  let* () = Client.bake_for_and_wait ~keys:[] client1
  and* level1, uri1 = wait_proposal1
  and* level2, uri2 = wait_proposal2 in

  Log.info "✓ Nonce worker saw proposal at level %d from %s" level1 uri1 ;
  Log.info "✓ Nonce worker saw proposal at level %d from %s" level2 uri2 ;

  (* Test 1: Terminate node2 *)
  let* () = Node.terminate node2 in

  (* Verify baker continues working with node1 only *)
  Log.info "Baking a block to verify baker still works with node1" ;
  let* current_level = Node.get_level node1 in
  let wait_proposal =
    wait_for_revelation_worker_proposal (current_level + 1) node1_uri
  in
  let* () = Client.bake_for_and_wait ~keys:[] client1 and* _ = wait_proposal in
  Log.info "✓ Baker continues operating with node1 (proposal from %s)" node1_uri ;

  (* Test 2: Restart node2 *)
  Log.info "Restarting node2 to test connection restoration" ;
  let* () = Node.run node2 [] and* () = Node.wait_for_ready node2 in

  (* Re-establish P2P trust (needed after restart in Private_mode) *)
  let* () = Client.Admin.trust_address client2 ~peer:node1 in
  let* () = Client.Admin.connect_address client1 ~peer:node2 in
  Log.info "Node2 restarted and P2P connected" ;

  (* Wait for node2 to sync *)
  let* node1_level = Node.get_level node1 in
  let* _ = Node.wait_for_level node2 node1_level in
  Log.info "Node2 synced to level %d" node1_level ;

  (* Verify both nodes are working again by baking more blocks *)
  Log.info "Baking blocks to verify both nodes are operational" ;
  let expected_level = node1_level + 1 in
  let wait_proposal1 =
    wait_for_revelation_worker_proposal expected_level node1_uri
  in
  let wait_proposal2 =
    wait_for_revelation_worker_proposal expected_level node2_uri
  in

  let* () = Lwt_unix.sleep 5. in

  let* level1, uri1 = wait_proposal1
  and* level2, uri2 = wait_proposal2
  and* () = Client.bake_for_and_wait ~keys:[] client1 in
  Log.info "✓ Nonce worker saw proposal at level %d from %s" level1 uri1 ;
  Log.info "✓ Nonce worker saw proposal at level %d from %s" level2 uri2 ;

  (* Test completed successfully *)
  Log.info
    "✓ Multi-node nonce worker connection recovery test completed successfully" ;
  unit

let test_multi_node_nonce_revelation_injection =
  Protocol.register_test
    ~__FILE__
    ~title:"Baker multi-node nonce revelation injection"
    ~tags:
      [
        team; "sandbox"; "agnostic"; "baker"; "multi_node"; "nonce"; "revelation";
      ]
    ~supports:Protocol.(From_protocol (number Alpha))
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
  @@ fun protocol ->
  Log.info "Setting up two nodes connected via P2P" ;
  (* Setup: 2 nodes sharing the same chain via P2P *)
  let* node1, client1 =
    Client.init_with_protocol
      `Client
      ~protocol
      ~timestamp:Now
      ~nodes_args:[Synchronisation_threshold 0; Connections 1]
      ()
  in
  let* node2, client2 =
    Client.init_with_node
      ~nodes_args:[Synchronisation_threshold 0; Connections 1]
      `Client
      ()
  in

  (* Set up P2P trust relationships (required for Private_mode) *)
  let* () = Client.Admin.trust_address client1 ~peer:node2 in
  let* () = Client.Admin.trust_address client2 ~peer:node1 in

  (* Connect the nodes *)
  let* () = Client.Admin.connect_address client1 ~peer:node2 in

  (* Wait for both nodes to sync to level 1 *)
  let* _ = Node.wait_for_level node1 1 in
  let* _ = Node.wait_for_level node2 1 in
  Log.info "✓ Two nodes set up, connected via P2P, and synced" ;

  Log.info "Creating baker connected to both nodes using --extra-node" ;
  (* Use bootstrap1 so the baker actually bakes blocks with nonces *)
  let node1_uri =
    Node.as_rpc_endpoint node1 |> Endpoint.to_uri |> Uri.to_string
  in
  let node2_uri =
    Node.as_rpc_endpoint node2 |> Endpoint.to_uri |> Uri.to_string
  in
  let baker =
    Agnostic_baker.create
      ~delegates:[] (* It'll use all the bakers. *)
      ~extra_nodes:[Node.as_rpc_endpoint node2]
      ~keep_alive:true
      ~remote_mode:true
      node1
      client1
  in

  (* Helper to check nodes in revealing_nonce event *)
  let wait_for_revealing_nonce ?expected_node () =
    Agnostic_baker.wait_for ~timeout:50. baker "revealing_nonce.v0"
    @@ fun json ->
    let level = JSON.(json |-> "level" |> as_int) in
    let ophash = JSON.(json |-> "ophash" |> as_string) in
    let node = JSON.(json |-> "node" |> as_string) in
    Log.info
      "Detected revealing_nonce: level %d, ophash %s from %s"
      level
      ophash
      node ;
    match expected_node with
    | Some expected_node when expected_node <> node -> None
    | _ -> Some ()
  in

  (* Start baker and wait for ready *)
  Log.info
    "Starting baker with --extra-node %s"
    (Endpoint.as_string (Node.as_rpc_endpoint node2)) ;
  let* () = Agnostic_baker.run baker in
  let* () = Agnostic_baker.wait_for_ready baker in
  Log.info "✓ Baker is ready and monitoring both nodes" ;

  (* Phase 1: Both nodes active - expect injection from any node *)
  let* () = wait_for_revealing_nonce () in

  (* Phase 2: Terminate node1, expect injection to node2 *)
  let* () = Node.terminate node1 in
  let* () = wait_for_revealing_nonce ~expected_node:node2_uri () in

  (* Phase 3: Restart node1, expect injection *)
  let node1_start_waiter =
    Agnostic_baker.wait_for_supervisor_automaton_start
      ~endpoint:(Node.as_rpc_endpoint node1)
      baker
  in

  let* () = Node.run node1 [Synchronisation_threshold 0; Connections 1] in
  let* () = Node.wait_for_ready node1 in
  let* () = Client.Admin.trust_address client2 ~peer:node1 in
  let* () = Client.Admin.trust_address client1 ~peer:node2 in
  let* () = Client.Admin.connect_address client2 ~peer:node1 in
  let* () = node1_start_waiter in
  let* () = wait_for_revealing_nonce () in

  (* Phase 2: Terminate node2, expect injection to node1 *)
  let* () = Node.terminate node2 in
  let* () = wait_for_revealing_nonce ~expected_node:node1_uri () in

  Log.info "✓ Multi-node nonce revelation injection test completed successfully" ;
  unit

let register ~protocols =
  test_keep_alive protocols ;
  test_cli protocols ;
  test_multi_node_connection_recovery protocols ;
  test_multi_node_dal_worker_connection_recovery protocols ;
  test_multi_node_nonce_connection_recovery protocols ;
  test_multi_node_nonce_revelation_injection protocols

let register_migration ~migrate_from ~migrate_to =
  migrate ~migrate_from ~migrate_to ~use_remote_signer:false ;
  reconnect_after_migration ~migrate_from ~migrate_to ;
  migrate ~migrate_from ~migrate_to ~use_remote_signer:true

let register_protocol_independent () =
  test_start_and_stop () ;
  test_man ()
