(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Protocol
   Invocation:   dune exec tezt/tests/main.exe -- --file weeklynet.ml
   Subject:      Test weeklynet activation with/without migration to Alpha.
*)

open Protocol_migration

let team = Tag.layer1

(** Tests that weeklynet parameters are consistent when migrating to Alpha
    - This test starts from the predecessor of protocol alpha (supposedly the
      last snapshotted protocol) and will migrate to alpha.
    - It starts by using
      "tezt/tests/weeklynet_configs/last_snapshotted_protocol.json" parameters
      that should reflect those used for weeklynet.
    - It will then wait for 4 levels to migrate to alpha and check migration.
    - Parameters are checked after migration to ensure they are still
      consistent.

    In case of errors:
    - Errors during initial protocol activation imply inconsistency between the
      last snapshotted protocol and the configuration file.
    - Errors during migration imply that the stitching might not be consistent
      with the current parameters.

    When snapshotting a protocol, this test may fail because stitching to alpha
    is not yet implemented. You should temporarily patch this test to migrate
    from Alpha-2 to Alpha-1 and make sure to reactivate the migration from
    Alpha-1 to Alpha in the first MR introducing stitching to new alpha.
*)
let test_weeklynet_migration_parameters =
  let migrate_from =
    match Protocol.previous_protocol Protocol.Alpha with
    | None -> Test.fail "No previous protocol for Alpha"
    | Some p -> p
  in
  let base_path = "tezt" // "tests" // "weeklynet_configs" in
  let last_snapshotted_protocol_parameters_path =
    Uses.make
      ~tag:"last_snapshotted_protocol_json"
      ~path:(base_path // "last_snapshotted_protocol.json")
      ()
  in

  let migration_level = 4 in

  let perform_migration ~migrate_to ~parameter_file =
    let keys =
      List.map (fun b -> b.Account.alias) (Array.to_list Account.Bootstrap.keys)
    in
    Log.info "Node starting" ;
    let* client, node =
      user_migratable_node_init ~migration_level ~migrate_to ()
    in
    Log.info "Node %s initialized" (Node.name node) ;
    let activate_protocol =
      Client.spawn_activate_protocol
        ~protocol:migrate_from
        client
        ~parameter_file
    in
    let* activation_result = Process.wait activate_protocol in
    let () =
      match activation_result with
      | Unix.WEXITED 0 -> ()
      | _ ->
          Test.fail
            "Failed to activate protocol %s, inconsistent parameter file in %s"
            (Protocol.hash migrate_from)
            (Uses.path last_snapshotted_protocol_parameters_path)
    in
    Log.info "Protocol %s activated" (Protocol.hash migrate_from) ;
    (* Bake until migration *)
    let* () =
      repeat (migration_level - 2) (fun () ->
          Client.bake_for_and_wait ~keys client)
    in

    (* Migrate *)
    let bake_migration = Client.spawn_bake_for ~keys client in
    let* migration_result = Process.wait bake_migration in
    let () =
      match migration_result with
      | Unix.WEXITED n when n = 0 -> ()
      | _ ->
          Test.fail
            "Failed to migrate to protocol %s"
            (Protocol.hash migrate_to)
    in
    Log.info "Checking migration block consistency" ;
    let* () =
      block_check
        ~expected_block_type:`Migration
        client
        ~migrate_from
        ~migrate_to
        ~level:migration_level
    in
    let* () = Client.bake_for_and_wait ~keys client in

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
    let* () = Client.bake_for_and_wait ~keys client in
    return (client, node)
  in

  Protocol.register_regression_test
    ~__FILE__
    ~title:"weeklynet regression test"
    ~tags:
      [team; "protocol"; "migration"; "sandbox"; "parameter_file"; "weeklynet"]
    ~uses:(fun _protocol -> [last_snapshotted_protocol_parameters_path])
    ~supports:Protocol.(From_protocol (number Alpha))
  @@ fun migrate_to ->
  let* client, __POS_OF__node =
    perform_migration
      ~parameter_file:(Uses.path last_snapshotted_protocol_parameters_path)
      ~migrate_to
  in

  let* _parametric =
    Client.RPC.call ~hooks:Tezos_regression.hooks client
    @@ RPC.get_chain_block_context_constants_parametric ()
  in

  unit

(** Test that the activation of the alpha protocol is successful using weeklynet
    parameters. *)
let test_weeklynet_alpha_activation () =
  let base_path = "tezt" // "tests" // "weeklynet_configs" in
  let alpha_parameters_path =
    Uses.make ~tag:"alpha_json" ~path:(base_path // "alpha.json") ()
  in
  Test.register
    ~__FILE__
    ~title:"activate alpha from weeklynet parameters"
    ~uses:[alpha_parameters_path]
    ~tags:
      [team; "protocol"; "activation"; "sandbox"; "parameter_file"; "weeklynet"]
  @@ fun () ->
  Log.info "Node starting" ;
  let* node = Node.init [Node.Synchronisation_threshold 0; Private_mode] in
  let* client = Client.(init ~endpoint:(Node node) ()) in

  Log.info "Node %s initialized" (Node.name node) ;
  let activate_protocol =
    Client.spawn_activate_protocol
      ~protocol:Protocol.Alpha
      client
      ~parameter_file:(Uses.path alpha_parameters_path)
  in
  let* activation_result = Process.wait activate_protocol in
  let () =
    match activation_result with
    | Unix.WEXITED 0 -> ()
    | _ ->
        Test.fail
          "Failed to activate protocol %s, inconsistent parameter file in %s"
          (Protocol.hash Alpha)
          (Uses.path alpha_parameters_path)
  in
  Log.info "Protocol %s activated" (Protocol.hash Alpha) ;
  Lwt.return_unit

let register () =
  test_weeklynet_migration_parameters Protocol.all ;
  test_weeklynet_alpha_activation ()
