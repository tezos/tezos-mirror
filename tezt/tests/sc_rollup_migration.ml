(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2021-2023 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2022-2023 TriliTech <contact@trili.tech>                    *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Smart Optimistic Rollups Migration tests
   Invocation:   dune exec tezt/tests/main.exe -- --file sc_rollup_migration.ml
*)

open Sc_rollup_helpers

let test_l1_migration_scenario ?parameters_ty ?(src = Constant.bootstrap1.alias)
    ?variant ?(tags = []) ?(timeout = 10) ?(commitment_period = 10) ~kind
    ~migrate_from ~migrate_to ~scenario_prior ~scenario_after ~description () =
  let tags =
    Protocol.tag migrate_from :: Protocol.tag migrate_to :: kind :: "migration"
    :: tags
  in
  Test.register
    ~__FILE__
    ~tags
    ~title:
      (sf
         "%s->%s: %s"
         (Protocol.name migrate_from)
         (Protocol.name migrate_to)
         (Sc_rollup_helpers.format_title_scenario
            kind
            {variant; tags; description}))
  @@ fun () ->
  let* tezos_node, tezos_client =
    Sc_rollup_helpers.setup_l1
      ~commitment_period
      ~challenge_window:10
      ~timeout
      migrate_from
  in
  let* sc_rollup =
    Sc_rollup_helpers.originate_sc_rollup ?parameters_ty ~kind ~src tezos_client
  in
  let* prior_res = scenario_prior tezos_client ~sc_rollup in
  let* current_level = Node.get_level tezos_node in
  let migration_level = current_level + 1 in
  let* () = Node.terminate tezos_node in
  let patch_config =
    Node.Config_file.set_sandbox_network_with_user_activated_upgrades
      [(migration_level, migrate_to)]
  in
  let nodes_args =
    Node.[Synchronisation_threshold 0; History_mode Archive; No_bootstrap_peers]
  in
  let* () = Node.run ~patch_config tezos_node nodes_args in
  let* () = Node.wait_for_ready tezos_node in
  let* () =
    repeat migration_level (fun () -> Client.bake_for_and_wait tezos_client)
  in
  let* () = scenario_after tezos_client ~sc_rollup prior_res in
  unit

(** Test that it is still possible to send message after a migration
(internal and external). *)
let test_migration_inbox ~kind ~migrate_from ~migrate_to =
  let tags = ["internal"; "external"; "message"; "inbox"]
  and description = "testing to send inbox operation post migration."
  and scenario_prior tezos_client ~sc_rollup =
    let* minter_address =
      Sc_rollup_helpers.originate_forward_smart_contract
        tezos_client
        migrate_from
    in
    let* () = Sc_rollup_helpers.send_messages 2 tezos_client in
    let* () =
      Client.transfer
        ~amount:Tez.(of_int 100)
        ~burn_cap:Tez.(of_int 100)
        ~storage_limit:100000
        ~giver:Constant.bootstrap1.alias
        ~receiver:minter_address
        ~arg:
          (Format.sprintf
             "Pair 0x%s %S "
             (Sc_rollup_helpers.hex_encode "pred_message")
             sc_rollup)
        tezos_client
    in
    return minter_address
  and scenario_after tezos_client ~sc_rollup minter_address =
    let* () = Sc_rollup_helpers.send_messages 2 tezos_client in
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/5286
       Check messages are correctly added in the inbox. *)
    Client.transfer
      ~amount:Tez.(of_int 100)
      ~burn_cap:Tez.(of_int 100)
      ~storage_limit:100000
      ~giver:Constant.bootstrap1.alias
      ~receiver:minter_address
      ~arg:
        (Format.sprintf
           "Pair 0x%s %S "
           (Sc_rollup_helpers.hex_encode "next_message")
           sc_rollup)
      tezos_client
  in
  test_l1_migration_scenario
    ~parameters_ty:"bytes"
    ~kind
    ~migrate_from
    ~migrate_to
    ~scenario_prior
    ~scenario_after
    ~tags
    ~description
    ()

(** Test to continue to send messages after a migration. *)
let test_migration_ticket_inbox ~kind ~migrate_from ~migrate_to =
  let tags = ["internal"; "inbox"; "ticket"]
  and description =
    "testing to send internal message with ticket post migration."
  and scenario_prior tezos_client ~sc_rollup =
    (* Originate forwarder contract to send internal messages to rollup *)
    let* alias, minter_address =
      Client.originate_contract_at
        ~amount:Tez.zero
        ~src:Constant.bootstrap1.alias
        ~init:"Unit"
        ~burn_cap:Tez.(of_int 1)
        tezos_client
        ["mini_scenarios"; "sc_rollup_mint_and_forward"]
        migrate_from
    in
    let* () = Client.bake_for_and_wait tezos_client in
    Log.info
      "The minter-forwarder %s (%s) contract was successfully originated"
      alias
      minter_address ;
    let* () =
      Client.transfer
        ~amount:Tez.(of_int 100)
        ~burn_cap:Tez.(of_int 100)
        ~storage_limit:100000
        ~giver:Constant.bootstrap1.alias
        ~receiver:minter_address
        ~arg:
          (Format.sprintf
             "Pair (Pair 0x%s 10) %S "
             (Sc_rollup_helpers.hex_encode "pred_message")
             sc_rollup)
        tezos_client
    in
    return minter_address
  and scenario_after tezos_client ~sc_rollup minter_address =
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/5286
       Check messages are correctly added in the inbox. *)
    Client.transfer
      ~amount:Tez.(of_int 100)
      ~burn_cap:Tez.(of_int 100)
      ~storage_limit:100000
      ~giver:Constant.bootstrap1.alias
      ~receiver:minter_address
      ~arg:
        (Format.sprintf
           "Pair (Pair 0x%s 10) %S "
           (Sc_rollup_helpers.hex_encode "pred_message")
           sc_rollup)
      tezos_client
  in
  test_l1_migration_scenario
    ~parameters_ty:"ticket bytes"
    ~kind
    ~migrate_from
    ~migrate_to
    ~scenario_prior
    ~scenario_after
    ~tags
    ~description
    ()

(* Test to cement a commitment pre-migration. *)
let test_migration_cement ~kind ~migrate_from ~migrate_to =
  let tags = ["commitment"; "cement"]
  and description =
    "Test to cement a commitment made pre migration then publish a new \
     commitment."
  and scenario_prior tezos_client ~sc_rollup =
    Sc_rollup_helpers.bake_period_then_publish_commitment
      ~sc_rollup
      ~src:Constant.bootstrap1.public_key_hash
      tezos_client
  and scenario_after tezos_client ~sc_rollup
      ((commitment : Sc_rollup_rpc.commitment), hash) =
    let* {commitment_period_in_blocks = commitment_period; _} =
      Sc_rollup_helpers.get_sc_rollup_constants tezos_client
    in
    let last_inbox_level = commitment.inbox_level in
    let* current_level = Client.level tezos_client in
    let missing_blocks_to_commit =
      last_inbox_level + commitment_period - current_level + 1
    in
    let* () =
      repeat missing_blocks_to_commit (fun () ->
          Client.bake_for_and_wait tezos_client)
    in
    let* _commitment, _next_hash =
      Sc_rollup_helpers.forge_and_publish_commitment
        ~number_of_ticks:1
        ~inbox_level:(last_inbox_level + commitment_period)
        ~predecessor:hash
        ~sc_rollup
        ~src:Constant.bootstrap1.public_key_hash
        tezos_client
    in
    (* no need to bake more to have the correct level for the
       cementation because commitment_period = challenge_period and we
       baked to be able to published a commit. *)
    Sc_rollup_helpers.cement_commitment migrate_to ~sc_rollup ~hash tezos_client
  in
  test_l1_migration_scenario
    ~kind
    ~migrate_from
    ~migrate_to
    ~scenario_prior
    ~scenario_after
    ~tags
    ~description
    ()

(** Test to recover stakes when the commitment was cemented
    pre-migration. *)
let test_migration_recover ~kind ~migrate_from ~migrate_to =
  let tags = ["commitment"; "cement"; "recover"]
  and description = "Test recover bond with cementation made pre-migration."
  and scenario_prior tezos_client ~sc_rollup =
    let* commitment, hash =
      Sc_rollup_helpers.bake_period_then_publish_commitment
        ~sc_rollup
        ~src:Constant.bootstrap1.public_key_hash
        tezos_client
    in
    let* {challenge_window_in_blocks = challenge_window; _} =
      Sc_rollup_helpers.get_sc_rollup_constants tezos_client
    in
    let* current_level = Client.level tezos_client in
    let missing_blocks_to_cement =
      commitment.inbox_level + challenge_window - current_level + 2
    in
    let* () =
      repeat missing_blocks_to_cement (fun () ->
          Client.bake_for_and_wait tezos_client)
    in
    Sc_rollup_helpers.cement_commitment
      migrate_from
      ~sc_rollup
      ~hash
      tezos_client
  and scenario_after tezos_client ~sc_rollup () =
    let*! () =
      let recover_bond_fee = 1_000_000 in
      Client.Sc_rollup.submit_recover_bond
        ~hooks:Sc_rollup_helpers.hooks
        ~rollup:sc_rollup
        ~src:Constant.bootstrap2.alias
        ~fee:(Tez.of_mutez_int recover_bond_fee)
        ~staker:Constant.bootstrap1.alias
        tezos_client
    in
    Client.bake_for_and_wait tezos_client
  in
  test_l1_migration_scenario
    ~kind
    ~migrate_from
    ~migrate_to
    ~scenario_prior
    ~scenario_after
    ~tags
    ~description
    ()

(* Test to refute a commitment pre-migration. *)
let test_migration_refute ~kind ~migrate_from ~migrate_to =
  let tags = ["refutation"]
  and description = "Refuting a pre-migration commitment."
  and scenario_prior tezos_client ~sc_rollup =
    let* commitment1, hash1 =
      Sc_rollup_helpers.bake_period_then_publish_commitment
        ~sc_rollup
        ~number_of_ticks:1
        ~src:Constant.bootstrap1.public_key_hash
        tezos_client
    in
    let* _commitment2, hash2 =
      Sc_rollup_helpers.forge_and_publish_commitment
        ~inbox_level:commitment1.inbox_level
        ~predecessor:commitment1.predecessor
        ~sc_rollup
        ~number_of_ticks:2
        ~src:Constant.bootstrap2.public_key_hash
        tezos_client
    in
    return (hash1, hash2)
  and scenario_after tezos_client ~sc_rollup
      (player_commitment_hash, opponent_commitment_hash) =
    let* {timeout_period_in_blocks = timeout_period; _} =
      Sc_rollup_helpers.get_sc_rollup_constants tezos_client
    in
    let* () =
      Sc_rollup_helpers.start_refute
        tezos_client
        ~sc_rollup
        ~source:Constant.bootstrap1
        ~opponent:Constant.bootstrap2.public_key_hash
        ~player_commitment_hash
        ~opponent_commitment_hash
    in
    let* Sc_rollup_rpc.{compressed_state = state_hash; _} =
      Sc_rollup_helpers.genesis_commitment ~sc_rollup tezos_client
    in
    let* () =
      Sc_rollup_helpers.move_refute_with_unique_state_hash
        ~number_of_sections_in_dissection:3
        tezos_client
        ~source:Constant.bootstrap1
        ~opponent:Constant.bootstrap2.public_key_hash
        ~sc_rollup
        ~state_hash
    in
    let* () =
      repeat (timeout_period + 1) (fun () ->
          Client.bake_for_and_wait tezos_client)
    in
    let* () =
      Sc_rollup_helpers.timeout
        ~sc_rollup
        ~staker1:Constant.bootstrap1.public_key_hash
        ~staker2:Constant.bootstrap2.public_key_hash
        tezos_client
    in
    unit
  in
  test_l1_migration_scenario
    ~kind
    ~migrate_from
    ~migrate_to
    ~scenario_prior
    ~scenario_after
    ~tags
    ~description
    ()

(* Test to refute a commitment pre-migration. *)
let test_cont_refute_pre_migration ~kind ~migrate_from ~migrate_to =
  let tags = ["refutation"]
  and description =
    "Refuting a commitment pre-migration when the game started pre-migration."
  and scenario_prior tezos_client ~sc_rollup =
    let* commitment1, player_commitment_hash =
      Sc_rollup_helpers.bake_period_then_publish_commitment
        ~sc_rollup
        ~number_of_ticks:1
        ~src:Constant.bootstrap1.public_key_hash
        tezos_client
    in
    let* _commitment2, opponent_commitment_hash =
      Sc_rollup_helpers.forge_and_publish_commitment
        ~inbox_level:commitment1.inbox_level
        ~predecessor:commitment1.predecessor
        ~sc_rollup
        ~number_of_ticks:2
        ~src:Constant.bootstrap2.public_key_hash
        tezos_client
    in
    let* () =
      Sc_rollup_helpers.start_refute
        tezos_client
        ~sc_rollup
        ~source:Constant.bootstrap1
        ~opponent:Constant.bootstrap2.public_key_hash
        ~player_commitment_hash
        ~opponent_commitment_hash
    in
    unit
  and scenario_after tezos_client ~sc_rollup () =
    let* {timeout_period_in_blocks = timeout_period; _} =
      Sc_rollup_helpers.get_sc_rollup_constants tezos_client
    in
    let* Sc_rollup_rpc.{compressed_state = state_hash; _} =
      Sc_rollup_helpers.genesis_commitment ~sc_rollup tezos_client
    in
    let* () =
      Sc_rollup_helpers.move_refute_with_unique_state_hash
        ~number_of_sections_in_dissection:3
        tezos_client
        ~source:Constant.bootstrap1
        ~opponent:Constant.bootstrap2.public_key_hash
        ~sc_rollup
        ~state_hash
    in
    let* () =
      repeat (timeout_period + 1) (fun () ->
          Client.bake_for_and_wait tezos_client)
    in
    let* () =
      Sc_rollup_helpers.timeout
        ~sc_rollup
        ~staker1:Constant.bootstrap1.public_key_hash
        ~staker2:Constant.bootstrap2.public_key_hash
        tezos_client
    in
    unit
  in
  test_l1_migration_scenario
    ~kind
    ~migrate_from
    ~migrate_to
    ~scenario_prior
    ~scenario_after
    ~tags
    ~description
    ()

let test_l2_migration_scenario ?parameters_ty ?(mode = Sc_rollup_node.Operator)
    ?(operator = Constant.bootstrap1.alias) ?boot_sector ?commitment_period
    ?challenge_window ?timeout ?variant ?(tags = []) ~kind ~migrate_from
    ~migrate_to ~scenario_prior ~scenario_after ~description () =
  let tags =
    Protocol.tag migrate_from :: Protocol.tag migrate_to :: kind :: "l2"
    :: "migration" :: tags
  in
  Test.register
    ~__FILE__
    ~tags
    ~uses:
      [Constant.octez_smart_rollup_node; Protocol.sc_rollup_client migrate_from]
    ~title:
      (sf
         "%s->%s: %s"
         (Protocol.name migrate_from)
         (Protocol.name migrate_to)
         (format_title_scenario kind {variant; tags; description}))
  @@ fun () ->
  let* tezos_node, tezos_client =
    setup_l1 ?commitment_period ?challenge_window ?timeout migrate_from
  in
  let* rollup_node, rollup_client, sc_rollup =
    setup_rollup
      ~protocol:migrate_from
      ?parameters_ty
      ~kind
      ~mode
      ?boot_sector
      ~operator
      tezos_node
      tezos_client
  in
  let* () = Sc_rollup_node.run rollup_node sc_rollup [] in
  let* prior_res =
    scenario_prior
      ~sc_rollup
      ~rollup_node
      ~rollup_client
      tezos_node
      tezos_client
  in
  let* current_level = Node.get_level tezos_node in
  let migration_level = current_level + 1 in
  let patch_config =
    Node.Config_file.set_sandbox_network_with_user_activated_upgrades
      [(migration_level, migrate_to)]
  in
  Log.info
    "Migrating L1 from %s to %s"
    (Protocol.name migrate_from)
    (Protocol.name migrate_to) ;
  let* () = Node.terminate tezos_node in
  let nodes_args =
    Node.[Synchronisation_threshold 0; History_mode Archive; No_bootstrap_peers]
  in
  let* () = Node.run ~patch_config tezos_node nodes_args in
  let* () = Node.wait_for_ready tezos_node in
  let* () = Client.bake_for_and_wait tezos_client in
  scenario_after ~sc_rollup ~rollup_node tezos_node tezos_client prior_res

let test_rollup_node_simple_migration ~kind ~migrate_from ~migrate_to =
  let tags = ["store"] in
  let description = "node can read data after store migration" in
  let commitment_period = 5 in
  let challenge_window = 5 in
  let scenario_prior ~sc_rollup:_ ~rollup_node ~rollup_client:_ _tezos_node
      tezos_client =
    let* () = Sc_rollup_helpers.send_messages commitment_period tezos_client in
    let* _ = Sc_rollup_node.wait_sync rollup_node ~timeout:10. in
    unit
  in
  let scenario_after ~sc_rollup ~rollup_node tezos_node tezos_client () =
    let* migration_level = Node.get_level tezos_node in
    let* () =
      Sc_rollup_helpers.send_messages (commitment_period + 3) tezos_client
    in
    let* _ = Sc_rollup_node.wait_sync rollup_node ~timeout:10. in
    let* _l2_block =
      Sc_rollup_node.RPC.call rollup_node
      @@ Sc_rollup_rpc.get_global_block
           ~block:(string_of_int (migration_level - 1))
           ()
    in
    let* l2_head =
      Sc_rollup_node.RPC.call rollup_node @@ Sc_rollup_rpc.get_global_block ()
    in
    let last_l2_commitment =
      JSON.(l2_head |-> "previous_commitment_hash" |> as_string)
    in
    Log.info
      "Checking that last commitment in the new protocol was published on L1" ;
    let* _commitment_on_l1 =
      Client.RPC.call tezos_client
      @@ RPC.get_chain_block_context_smart_rollups_smart_rollup_commitment
           ~sc_rollup
           ~hash:last_l2_commitment
           ()
    in
    unit
  in
  test_l2_migration_scenario
    ~tags
    ~kind
    ~commitment_period
    ~challenge_window
    ~migrate_from
    ~migrate_to
    ~scenario_prior
    ~scenario_after
    ~description
    ()

let test_rollup_node_catchup_migration ~kind ~migrate_from ~migrate_to =
  let tags = ["catchup"] in
  let description = "node can catch up on protocol migration" in
  let commitment_period = 10 in
  let challenge_window = 10 in
  let scenario_prior ~sc_rollup:_ ~rollup_node ~rollup_client:_ _tezos_node
      tezos_client =
    let* () = Sc_rollup_helpers.send_messages 1 tezos_client in
    let* _ = Sc_rollup_node.wait_sync rollup_node ~timeout:10. in
    Log.info "Stopping rollup node before protocol migration." ;
    let* () = Sc_rollup_node.terminate rollup_node in
    Log.info "Sending more messages on L1." ;
    Sc_rollup_helpers.send_messages (commitment_period - 1) tezos_client
  in
  let scenario_after ~sc_rollup ~rollup_node tezos_node tezos_client () =
    let* migration_level = Node.get_level tezos_node in
    let* () = Sc_rollup_helpers.send_messages 1 tezos_client in
    Log.info "Restarting rollup node after migration." ;
    let* () = Sc_rollup_node.run rollup_node sc_rollup [] in
    Log.info "Waiting for rollup node to catch up." ;
    let* _ = Sc_rollup_node.wait_sync rollup_node ~timeout:100. in
    Log.info "Rollup node has caught up!" ;
    let* _l2_block =
      Sc_rollup_node.RPC.call rollup_node
      @@ Sc_rollup_rpc.get_global_block
           ~block:(string_of_int (migration_level - 1))
           ()
    in
    let* _l2_block =
      Sc_rollup_node.RPC.call rollup_node @@ Sc_rollup_rpc.get_global_block ()
    in
    unit
  in
  test_l2_migration_scenario
    ~tags
    ~kind
    ~commitment_period
    ~challenge_window
    ~migrate_from
    ~migrate_to
    ~scenario_prior
    ~scenario_after
    ~description
    ()

let l1_level_event level tezos_node _rollup_node =
  let* _ = Node.wait_for_level tezos_node level in
  unit

let published_commitment_event ~inbox_level _tezos_node rollup_node =
  Sc_rollup_node.wait_for
    rollup_node
    "smart_rollup_node_commitment_lpc_updated.v0"
  @@ fun json ->
  let level = JSON.(json |-> "level" |> as_int) in
  if level >= inbox_level then Some () else None

let commitment_computed_event ~inbox_level _tezos_node rollup_node =
  Sc_rollup_node.wait_for rollup_node "smart_rollup_node_commitment_compute.v0"
  @@ fun json ->
  let level = JSON.as_int json in
  if level >= inbox_level then Some () else None

let test_migration_removes_dead_games ~kind ~migrate_from ~migrate_to =
  let tags = ["refutation"; "clean"] in
  let description = "dead games are cleaned during migration" in
  let scenario_prior tezos_client ~sc_rollup =
    (* 3 stakers are in conflict: A, B and C.

       A vs B, A vs C and B vs C.
       A wins against B and C, slashing them. The outcome of B vs C then
       is a draw.
    *)
    let* commitment1, hash1 =
      Sc_rollup_helpers.bake_period_then_publish_commitment
        ~sc_rollup
        ~number_of_ticks:1
        ~src:Constant.bootstrap1.public_key_hash
        tezos_client
    in
    let* _commitment2, hash2 =
      Sc_rollup_helpers.forge_and_publish_commitment
        ~inbox_level:commitment1.inbox_level
        ~predecessor:commitment1.predecessor
        ~sc_rollup
        ~number_of_ticks:2
        ~src:Constant.bootstrap2.public_key_hash
        tezos_client
    in
    let* _commitment3, hash3 =
      Sc_rollup_helpers.forge_and_publish_commitment
        ~inbox_level:commitment1.inbox_level
        ~predecessor:commitment1.predecessor
        ~sc_rollup
        ~number_of_ticks:3
        ~src:Constant.bootstrap3.public_key_hash
        tezos_client
    in
    let* () =
      Sc_rollup_helpers.start_refute
        tezos_client
        ~sc_rollup
        ~source:Constant.bootstrap2
        ~opponent:Constant.bootstrap1.public_key_hash
        ~player_commitment_hash:hash2
        ~opponent_commitment_hash:hash1
    in
    let* () =
      Sc_rollup_helpers.start_refute
        tezos_client
        ~sc_rollup
        ~source:Constant.bootstrap3
        ~opponent:Constant.bootstrap1.public_key_hash
        ~player_commitment_hash:hash3
        ~opponent_commitment_hash:hash1
    in
    let* () =
      Sc_rollup_helpers.start_refute
        tezos_client
        ~sc_rollup
        ~source:Constant.bootstrap2
        ~opponent:Constant.bootstrap3.public_key_hash
        ~player_commitment_hash:hash2
        ~opponent_commitment_hash:hash3
    in

    let* {timeout_period_in_blocks = timeout_period; _} =
      Sc_rollup_helpers.get_sc_rollup_constants tezos_client
    in
    let* () =
      repeat (timeout_period + 1) (fun () ->
          Client.bake_for_and_wait tezos_client)
    in
    let* () =
      Sc_rollup_helpers.timeout
        ~sc_rollup
        ~staker1:Constant.bootstrap1.public_key_hash
        ~staker2:Constant.bootstrap2.public_key_hash
        tezos_client
    in
    let* () =
      Sc_rollup_helpers.timeout
        ~sc_rollup
        ~staker1:Constant.bootstrap1.public_key_hash
        ~staker2:Constant.bootstrap3.public_key_hash
        tezos_client
    in
    return (Constant.bootstrap2, Constant.bootstrap3)
  in
  let scenario_after tezos_client ~sc_rollup (playerB, playerC) =
    let* opponents =
      Client.RPC.call tezos_client
      @@ RPC.get_chain_block_context_raw_json
           ~path:
             [
               "smart_rollup";
               "index";
               sc_rollup;
               "game";
               playerB.Account.public_key_hash;
               "opponents";
             ]
           ()
    in
    let opponents = JSON.as_list opponents |> List.map JSON.as_string in
    let expected_opponents =
      if Protocol.number migrate_to > 017 then []
      else [playerC.Account.public_key_hash]
    in
    Check.((expected_opponents = opponents) (list string))
      ~error_msg:
        (sf
           "After migration %s should have %%L as opponents, but found %%R"
           playerB.Account.public_key_hash) ;
    unit
  in
  test_l1_migration_scenario
    ~kind
    ~migrate_from
    ~migrate_to
    ~scenario_prior
    ~scenario_after
    ~tags
    ~description
    ~timeout:1
    ~commitment_period:1
    ()

(** Same as {!test_l2_migration_scenario} but the migration is done when an
    event is detected. *)
let test_l2_migration_scenario_event ?parameters_ty
    ?(mode = Sc_rollup_node.Operator) ?(operator = Constant.bootstrap1.alias)
    ?boot_sector ?commitment_period ?challenge_window ?rollup_node_name ?timeout
    ?variant ?(tags = []) ~kind ~migrate_from ~migrate_to ~migration_on_event
    ~description scenario =
  let tags =
    Protocol.tag migrate_from :: Protocol.tag migrate_to :: kind :: "l2"
    :: "migration" :: tags
  in
  Test.register
    ~__FILE__
    ~tags
    ~uses:
      [Constant.octez_smart_rollup_node; Protocol.sc_rollup_client migrate_from]
    ~title:
      (sf
         "%s->%s: %s"
         (Protocol.name migrate_from)
         (Protocol.name migrate_to)
         (format_title_scenario kind {variant; tags; description}))
  @@ fun () ->
  let* tezos_node, tezos_client =
    Sc_rollup_helpers.setup_l1
      ?commitment_period
      ?challenge_window
      ?timeout
      migrate_from
  in
  let* rollup_node, rollup_client, sc_rollup =
    setup_rollup
      ~protocol:migrate_from
      ?parameters_ty
      ~kind
      ~mode
      ?boot_sector
      ~operator
      ?rollup_node_name
      tezos_node
      tezos_client
  in
  let (_migration : unit Lwt.t) =
    let* () = migration_on_event tezos_node rollup_node in
    let migration_level = Node.get_last_seen_level tezos_node + 1 in
    let patch_config =
      Node.Config_file.set_sandbox_network_with_user_activated_upgrades
        [(migration_level, migrate_to)]
    in
    Log.info
      "Migrating L1 from %s to %s"
      (Protocol.name migrate_from)
      (Protocol.name migrate_to) ;
    let* () = Node.terminate tezos_node in
    let nodes_args =
      Node.
        [Synchronisation_threshold 0; History_mode Archive; No_bootstrap_peers]
    in
    let* () = Node.run ~patch_config tezos_node nodes_args in
    let* () = Node.wait_for_ready tezos_node in
    Client.bake_for_and_wait tezos_client
  in
  scenario
    migrate_from
    rollup_node
    rollup_client
    sc_rollup
    tezos_node
    tezos_client

let test_refutation_migration_scenario ?(flaky = false) ?commitment_period
    ?challenge_window ~variant ~mode ~kind scenario ~migrate_from ~migrate_to
    ~migration_on_event =
  let tags =
    (if flaky then [Tag.flaky] else [])
    @ ["refutation"]
    @ if mode = Sc_rollup_node.Accuser then ["accuser"] else []
  in

  let variant = variant ^ if mode = Accuser then "+accuser" else "" in
  test_l2_migration_scenario_event
    ?commitment_period
    ~kind
    ~mode
    ~timeout:60
    ?challenge_window
    ~rollup_node_name:"honest"
    ~tags
    ~variant
    ~description:"refutation games over migrations"
    ~migrate_from
    ~migrate_to
    ~migration_on_event
    (Sc_rollup_helpers.test_refutation_scenario_aux ~mode ~kind scenario)

let test_refutation_migration ~migrate_from ~migrate_to =
  let challenge_window = 10 in
  let commitment_period = 10 in
  let tests =
    [
      ( "inbox_proof_1",
        3,
        true,
        refutation_scenario_parameters
          ~loser_modes:["3 4 0"]
          (inputs_for 10)
          ~final_level:80
          ~priority:`Priority_loser );
      ( "pvm_proof_2",
        7,
        false,
        refutation_scenario_parameters
          ~loser_modes:["7 7 22_000_002_000"]
          (inputs_for 10)
          ~final_level:80
          ~priority:`Priority_loser );
    ]
  in
  List.iter
    (fun (variant, fault_level, flaky_scenario, inputs) ->
      List.iter
        (fun (migration_variant, migration_on_event, flaky_variant) ->
          let variant = String.concat "_" [variant; migration_variant] in
          test_refutation_migration_scenario
            ~flaky:(flaky_scenario && flaky_variant)
            ~kind:"wasm_2_0_0"
              (* The tests for refutations over migrations are only ran for wasm
                 as the arith PVMs do not have the same semantic in all
                 protocols. *)
            ~mode:Operator
            ~challenge_window
            ~commitment_period
            ~variant
            inputs
            ~migrate_from
            ~migrate_to
            ~migration_on_event)
        [
          ("ongoing", injecting_refute_event, false);
          ("at_inbox", l1_level_event fault_level, true);
          ( "at_commitment",
            commitment_computed_event ~inbox_level:fault_level,
            false );
          ( "at_published_commitment",
            published_commitment_event ~inbox_level:fault_level,
            false );
        ])
    tests

let register_migration ~kind ~migrate_from ~migrate_to =
  test_migration_inbox ~kind ~migrate_from ~migrate_to ;
  test_migration_ticket_inbox ~kind ~migrate_from ~migrate_to ;
  test_migration_cement ~kind ~migrate_from ~migrate_to ;
  test_migration_recover ~kind ~migrate_from ~migrate_to ;
  test_migration_refute ~kind ~migrate_from ~migrate_to ;
  test_cont_refute_pre_migration ~kind ~migrate_from ~migrate_to ;
  test_rollup_node_simple_migration ~kind ~migrate_from ~migrate_to ;
  test_rollup_node_catchup_migration ~kind ~migrate_from ~migrate_to ;
  test_migration_removes_dead_games ~kind ~migrate_from ~migrate_to

let register_migration_only_wasm ~migrate_from ~migrate_to =
  test_refutation_migration ~migrate_from ~migrate_to

let register ~migrate_from ~migrate_to =
  register_migration ~kind:"arith" ~migrate_from ~migrate_to ;
  register_migration ~kind:"wasm_2_0_0" ~migrate_from ~migrate_to ;
  register_migration_only_wasm ~migrate_from ~migrate_to
