(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Agnostic baker (octez-agnostic-baker)
   Invocation:   dune exec tezt/manual_tests/main.exe -- --file agnostic_baking.ml
   Subject:      Ensure that the agnostic baker behaves as expected
*)

(** Boilerplate code to create a user-migratable node. Used in the
    tests below. **)
let user_migratable_node_init ?node_name ?client_name ?(more_node_args = [])
    ~migration_level ~migrate_to () =
  let* node =
    Node.init
      ?name:node_name
      ~patch_config:
        (Node.Config_file.set_sandbox_network_with_user_activated_upgrades
           [(migration_level, migrate_to)])
      ([Node.Synchronisation_threshold 0; Private_mode] @ more_node_args)
  in
  let* client = Client.(init ?name:client_name ~endpoint:(Node node) ()) in
  Lwt.return (client, node)

(** [block_check ?level ~expected_block_type ~migrate_to ~migrate_from client]
    is generic check that a block of type [expected_block_type] contains
    (protocol) metatadata conforming to its type at [level]. **)
let block_check ?level ~expected_block_type ~migrate_to ~migrate_from client =
  let block =
    match level with Some level -> Some (string_of_int level) | None -> None
  in
  let* metadata =
    Client.RPC.call client @@ RPC.get_chain_block_metadata ?block ()
  in
  let protocol = metadata.protocol in
  let next_protocol = metadata.next_protocol in
  (match expected_block_type with
  | `Migration ->
      Check.(
        (next_protocol = Protocol.hash migrate_to)
          string
          ~error_msg:"expected next protocol to be %R, got %L") ;
      Check.(
        (protocol = Protocol.hash migrate_from)
          string
          ~error_msg:"expected (from) protocol to be %R, got %L")
  | `Non_migration ->
      Check.(
        (next_protocol = protocol)
          string
          ~error_msg:"expected a non migration block ")) ;
  Lwt.return_unit

let wait_for_active_protocol_waiting agnostic_baker =
  Agnostic_baker.wait_for
    agnostic_baker
    "waiting_for_active_protocol.v0"
    (fun _json -> Some ())

(* Performs a protocol migration thanks to a UAU and the agnostic
   baker. *)
let perform_protocol_migration ?node_name ?client_name ?parameter_file
    ~blocks_per_cycle ~migration_level ~migrate_from ~migrate_to
    ~baked_blocks_after_migration () =
  assert (migration_level >= blocks_per_cycle) ;
  Log.info "Node starting" ;
  let* client, node =
    user_migratable_node_init
      ?node_name
      ?client_name
      ~migration_level
      ~migrate_to
      ()
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
  (* Ensure that the block before migration is consistent *)
  Log.info "Checking migration block consistency" ;
  let* () =
    block_check
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
    block_check
      ~expected_block_type:`Non_migration
      client
      ~migrate_from
      ~migrate_to
      ~level:(migration_level + 1)
  in
  (* Test that we can still bake after migration *)
  let* _level = Node.wait_for_level node baked_blocks_after_migration in
  let* () = Agnostic_baker.terminate baker in
  Lwt.return_unit

let migrate ~migrate_from ~migrate_to =
  let parameters = JSON.parse_file (Protocol.parameter_file migrate_to) in
  let blocks_per_cycle = JSON.(get "blocks_per_cycle" parameters |> as_int) in
  let consensus_rights_delay =
    JSON.(get "consensus_rights_delay" parameters |> as_int)
  in
  (* Migration level is set arbitrarily *)
  let migration_level = blocks_per_cycle in
  Test.register
    ~__FILE__
    ~title:
      (Format.sprintf
         "Protocol migration (from %s to %s) with agnostic baker"
         (Protocol.tag migrate_from)
         (Protocol.tag migrate_to))
    ~tags:["protocol"; "migration"; "agnostic"; "baker"]
    ~uses:[Constant.octez_agnostic_baker]
  @@ fun () ->
  let* () =
    perform_protocol_migration
      ~blocks_per_cycle
      ~migration_level
      ~migrate_from
      ~migrate_to
      ~baked_blocks_after_migration:
        (2 * consensus_rights_delay * blocks_per_cycle)
      ()
  in
  unit

let start_stop =
  Test.register
    ~__FILE__
    ~title:"Agnostic baker starts and stops"
    ~tags:["sandbox"; "agnostic"; "baker"; "init"]
    ~uses:[Constant.octez_agnostic_baker]
  @@ fun () ->
  let* node, client = Client.init_with_node `Client () in
  let* baker = Agnostic_baker.init node client in
  let* () = Agnostic_baker.wait_for_ready baker in
  let* () = Agnostic_baker.terminate baker in
  unit

let register ~migrate_from ~migrate_to =
  start_stop ;
  migrate ~migrate_from ~migrate_to
