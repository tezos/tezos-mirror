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
        "flaky";
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
  let wait_for_cannot_connect =
    Agnostic_baker.wait_for baker "cannot_connect.v0" (fun _json -> Some ())
  in
  let* () = Agnostic_baker.run ~extra_arguments:["--keep-alive"] baker
  and* () = wait_for_cannot_connect in
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
  let* () = Node.terminate node and* () = wait_for_cannot_connect in
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

  let arguments = arguments @ ["--without-dal"] in
  let* () = Agnostic_baker.wait_for_ready baker
  and* () = Agnostic_baker.raw ~arguments baker in
  unit

let register ~protocols =
  test_keep_alive protocols ;
  test_cli protocols

let register_migration ~migrate_from ~migrate_to =
  migrate ~migrate_from ~migrate_to ~use_remote_signer:false ;
  reconnect_after_migration ~migrate_from ~migrate_to ;
  migrate ~migrate_from ~migrate_to ~use_remote_signer:true

let register_protocol_independent () =
  test_start_and_stop () ;
  test_man ()
