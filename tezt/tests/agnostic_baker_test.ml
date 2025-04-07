(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Agnostic baker (octez-experimental-agnostic-baker)
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

let wait_for_baker_process agnostic_baker protocol_short_hash =
  Agnostic_baker.wait_for
    agnostic_baker
    (Format.sprintf "baker_%s_started.v0" protocol_short_hash)
    (fun json -> JSON.(json |> as_int_opt))

(* Performs a protocol migration thanks to a UAU and the agnostic
   baker.
   The [resilience_test] flag aims to test the resilience of the
   agnostic baker thanks to a kill of the baker daemon during the
   test, making sure it is restarted as expected. *)
let perform_protocol_migration ?(resilience_test = false) ?node_name
    ?client_name ?parameter_file ?(use_remote_signer = false) ~blocks_per_cycle
    ~migration_level ~migrate_from ~migrate_to ~baked_blocks_after_migration ()
    =
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
  let* () =
    if use_remote_signer then
      let* () = Client.forget_all_keys client in
      let keys =
        Constant.activator :: (Account.Bootstrap.keys |> Array.to_list)
      in
      let* signer = Signer.init ~keys () in
      (* tell the baker to ask the signer for the bootstrap keys *)
      let uri = Signer.uri signer in
      Lwt_list.iter_s
        (fun account ->
          let Account.{alias; public_key_hash; _} = account in
          Client.import_signer_key client ~alias ~public_key_hash ~signer:uri)
        keys
    else unit
  in
  Log.info "Node %s initialized" (Node.name node) ;
  let baker = Agnostic_baker.create node client in
  let wait_for_active_protocol_waiting =
    wait_for_active_protocol_waiting baker
  in
  Log.info "Starting agnostic baker" ;
  let* () = Agnostic_baker.run baker in
  let* () = wait_for_active_protocol_waiting in
  let* () = Agnostic_baker.wait_for_ready baker in
  let wait_pre_migration_baker_pid =
    if resilience_test then
      let wait =
        wait_for_baker_process baker (Protocol.short_hash migrate_from)
      in
      wait
    else Lwt.return (-1)
  in
  let* () =
    Client.activate_protocol
      ~protocol:migrate_from
      client
      ?parameter_file
      ~timestamp:Client.Now
  in
  Log.info "Protocol %s activated" (Protocol.hash migrate_from) ;
  Log.info "Baking at least %d blocks to trigger migration" migration_level ;
  let* () =
    if resilience_test then (
      (* If resilience test is activated, we kill the pre migration
         baker process in the middle of the migration period. The
         baker is expected to be restarted automatically so that the
         test is expected to continue smoothly. *)
      let* pid = wait_pre_migration_baker_pid in
      let* _level = Node.wait_for_level node (migration_level / 2) in
      let wait_new_baker_pid =
        wait_for_baker_process baker (Protocol.short_hash migrate_from)
      in
      Log.info "Kill the pre-migration baker (pid %d) with SIGTERM signal" pid ;
      Unix.kill pid Sys.sigterm ;
      let* new_pid = wait_new_baker_pid in
      Log.info
        "New pre-migration baker process is taking over (pid: %d)"
        new_pid ;
      unit)
    else unit
  in
  (* Wait one block before the new protocol activation to register the
     post migration baker's pid waiter. *)
  let* _level = Node.wait_for_level node (migration_level - 1) in
  let wait_post_migration_baker_pid =
    if resilience_test then
      let wait =
        wait_for_baker_process baker (Protocol.short_hash migrate_to)
      in
      wait
    else Lwt.return (-1)
  in
  let* _level = Node.wait_for_level node migration_level in
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
  (* Test that we can still bake after migration *)
  let* () =
    if resilience_test then (
      (* If resilience test is activated, we kill the post migration
         baker process close to the end of the post migration
         period. The baker is expected to be restarted automatically
         so that the test is expected to continue smoothly. *)
      let* pid = wait_post_migration_baker_pid in
      let* _level =
        Node.wait_for_level
          node
          (baked_blocks_after_migration - blocks_per_cycle)
      in
      let wait_new_baker_pid =
        wait_for_baker_process baker (Protocol.short_hash migrate_to)
      in
      Log.info "Kill the post-migration baker (pid %d) with SIGTERM signal" pid ;
      Unix.kill pid Sys.sigterm ;
      let* new_pid = wait_new_baker_pid in
      Log.info
        "New post-migration baker process is taking over (pid: %d)"
        new_pid ;
      unit)
    else unit
  in
  let* _level = Node.wait_for_level node baked_blocks_after_migration in
  let* () = Agnostic_baker.terminate baker in
  unit

let raw_migrate ~migrate_from ~migrate_to ~resilience_test ~use_remote_signer =
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
          baker%s %s"
         (Protocol.tag migrate_from)
         (Protocol.tag migrate_to)
         remote_signer_text
         (if resilience_test then "and resilience test" else ""))
    ~tags:
      [
        "protocol";
        "migration";
        "agnostic";
        "baker";
        Protocol.tag migrate_from;
        Protocol.tag migrate_to;
      ]
    ~uses:([Constant.octez_experimental_agnostic_baker] @ remote_signer)
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

let migrate = raw_migrate ~resilience_test:false

let migrate_with_resilience_test = raw_migrate ~resilience_test:true

let register ~migrate_from ~migrate_to =
  (* We want to migrate only from Active protocols *)
  if Agnostic_baker.protocol_status migrate_from = Active then (
    migrate ~migrate_from ~migrate_to ~use_remote_signer:false ;
    migrate ~migrate_from ~migrate_to ~use_remote_signer:true ;
    migrate_with_resilience_test
      ~migrate_from
      ~migrate_to
      ~use_remote_signer:false ;
    migrate_with_resilience_test
      ~migrate_from
      ~migrate_to
      ~use_remote_signer:true)
  else ()

let register_protocol_independent () =
  Test.register
    ~__FILE__
    ~title:"Agnostic baker starts and stops"
    ~tags:[team; "sandbox"; "agnostic"; "baker"; "init"]
    ~uses:[Constant.octez_experimental_agnostic_baker]
  @@ fun () ->
  let* node, client = Client.init_with_node `Client () in
  let* baker = Agnostic_baker.init node client in
  let* () = Agnostic_baker.wait_for_ready baker in
  let* () = Agnostic_baker.terminate baker in
  unit
