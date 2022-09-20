(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component: All
   Invocation: make test-tezt
   Subject: This file is the entrypoint of all Tezt tests. It dispatches to
            other files.
*)

let protocols = Protocol.[Jakarta; Kathmandu; Alpha]

let migrate_to = Protocol.Alpha

(* This module runs the tests implemented in all other modules of this directory.
   Each module defines tests which are thematically related,
   as functions to be called here. *)

let register_protocol_independent_tests () =
  (* Tests that are protocol-independent.
     They do not take a protocol as a parameter and thus need to be registered only once. *)
  Bootstrap.register_protocol_independent () ;
  Cli_tezos.register_protocol_independent () ;
  Client_keys.register_protocol_independent () ;
  Injection.register_protocol_independent () ;
  Light.register_protocol_independent () ;
  Mockup.register_protocol_independent () ;
  P2p.register_protocol_independent () ;
  Proxy.register_protocol_independent () ;
  Config.register () ;
  Demo_counter.register ()

let register_protocol_migration_tests () =
  (* Tests related to protocol migration. *)
  let migrate_from = Option.get @@ Protocol.previous_protocol migrate_to in
  Mockup.register_constant_migration ~migrate_from ~migrate_to ;
  Protocol_migration.register ~migrate_from ~migrate_to ;
  Protocol_table_update.register ~migrate_from ~migrate_to ;
  User_activated_upgrade.register ~migrate_from ~migrate_to ;
  (* TODO #3380
     Alpha cannot stitch from Jakarta yet, but when it can, we can
     add a voting test from Jakarta to Alpha. *)
  (* Voting.register
       ~from_protocol:migrate_from
       ~to_protocol:(Known migrate_to)
       ~loser_protocols:[migrate_to] ;
     Voting.register
       ~from_protocol:migrate_from
       ~to_protocol:Injected_test
       ~loser_protocols:[migrate_to; migrate_from] ; *)
  Voting.register
    ~from_protocol:migrate_to
    ~to_protocol:Injected_test
    ~loser_protocols:[migrate_from] ;
  Voting.register
    ~from_protocol:migrate_to
    ~to_protocol:Demo
    ~loser_protocols:[migrate_from] ;
  Iticket_migration.register ~migrate_from ~migrate_to

let register_protocol_agnostic_tests () =
  (* Tests that are relatively protocol-agnostic.
     We can run them on all protocols, or only one if the CI would be too slow. *)
  Baker_test.register ~protocols:[Alpha] ;
  Baking.register ~protocols ;
  Baking.register_operations_pool ~protocols:[Jakarta; Kathmandu; Alpha] ;
  Basic.register ~protocols:[Alpha] ;
  Big_map_all.register ~protocols:[Alpha] ;
  Bootstrap.register ~protocols:[Alpha] ;
  Cache_cache.register protocols ;
  Client_config.register ~protocols:[Alpha] ;
  Client_commands.register ~protocols ;
  Client_run_view.register ~protocols ;
  Contract_hash_fun.register ~protocols ;
  Dal.register ~protocols:[Alpha] ;
  Deposits_limit.register ~protocols ;
  Double_bake.register ~protocols:[Alpha] ;
  Encoding.register ~protocols ;
  Forge.register ~protocols:[Alpha] ;
  Global_constants.register ~protocols:[Alpha] ;
  Hash_data.register ~protocols:[Alpha] ;
  Large_metadata.register ~protocols:[Alpha] ;
  Light.register ~protocols:[Alpha] ;
  Liquidity_baking_per_block_votes.register ~protocols ;
  Manager_operations.register ~protocols ;
  Mockup.register ~protocols ;
  Mockup.register_global_constants ~protocols:[Alpha] ;
  Monitor_operations.register ~protocols:[Alpha] ;
  Multinode_snapshot.register ~protocols:[Alpha] ;
  Consensus_key.register ~protocols:[Alpha] ;
  Node_event_level.register ~protocols:[Alpha] ;
  Normalize.register ~protocols:[Alpha] ;
  Precheck.register ~protocols ;
  Prevalidator.register ~protocols ;
  Protocol_limits.register ~protocols:[Alpha] ;
  Proxy.register ~protocols ;
  Proxy_server_test.register ~protocols:[Alpha] ;
  P2p.register ~protocols:[Alpha] ;
  Reject_malformed_micheline.register ~protocols:[Alpha] ;
  Replace_by_fees.register ~protocols ;
  Rpc_config_logging.register ~protocols:[Alpha] ;
  RPC_test.register protocols ;
  Run_operation_RPC.register ~protocols ;
  Run_script.register ~protocols:[Alpha] ;
  Runtime_script_failure.register ~protocols ;
  Sapling.register ~protocols:[Alpha] ;
  Signer_test.register ~protocols:[Alpha] ;
  Stresstest_command.register ~protocols:[Alpha] ;
  Synchronisation_heuristic.register ~protocols:[Alpha] ;
  Tenderbake.register ~protocols:[Alpha] ;
  Test_contract_bls12_381.register ~protocols:[Alpha] ;
  Ticket_updates_in_receipt.register ~protocols:[Alpha] ;
  Timelock.register ~protocols ;
  Tx_rollup.register ~protocols ;
  Tx_rollup_l2_node.register ~protocols ;
  Views.register [Alpha] ;
  Retro.register ~protocols

let register_K_plus_tests () =
  (* Relies on a feature only available since K.
     Move these to [register_protocol_agnostic_tests] once K is the smallest
     protocol. *)
  let protocols = Protocol.[Kathmandu; Alpha] in
  Events.register ~protocols:[Alpha] ;
  Ghostnet_dictator_migration.register ~protocols ;
  Increase_paid_storage.register ~protocols ;
  Operation_validation.register ~protocols ;
  Sc_rollup.register ~protocols:[Alpha] ;
  Testnet_dictator.register ~protocols ;
  Vdf_test.register ~protocols

let () =
  register_protocol_independent_tests () ;
  register_protocol_migration_tests () ;
  register_protocol_agnostic_tests () ;
  register_K_plus_tests () ;
  (* Test.run () should be the last statement, don't register afterwards! *)
  Test.run ()
