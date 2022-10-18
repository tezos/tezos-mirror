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

let migrate_to = Protocol.Alpha

let alpha_can_stitch_from_its_predecessor = false

(* This module runs the tests implemented in all other modules of this directory.
   Each module defines tests which are thematically related,
   as functions to be called here. *)

(* Tests that are protocol-independent.
   They do not take a protocol as a parameter and thus need to be registered only once. *)
let register_protocol_independent_tests () =
  Bootstrap.register_protocol_independent () ;
  Cli_tezos.register_protocol_independent () ;
  Client_keys.register_protocol_independent () ;
  Config.register () ;
  Demo_counter.register () ;
  Injection.register_protocol_independent () ;
  Light.register_protocol_independent () ;
  Mockup.register_protocol_independent () ;
  P2p.register_protocol_independent () ;
  Proxy.register_protocol_independent ()

(* Tests related to protocol migration. *)
let register_protocol_migration_tests () =
  let migrate_from = Option.get @@ Protocol.previous_protocol migrate_to in
  Mockup.register_constant_migration ~migrate_from ~migrate_to ;
  Protocol_migration.register ~migrate_from ~migrate_to ;
  Protocol_table_update.register ~migrate_from ~migrate_to ;
  User_activated_upgrade.register ~migrate_from ~migrate_to ;
  (if alpha_can_stitch_from_its_predecessor then
   Protocol.previous_protocol Alpha
   |> Option.iter @@ fun from_protocol ->
      Voting.register
        ~from_protocol
        ~to_protocol:(Known Alpha)
        ~loser_protocols:[]) ;
  Voting.register
    ~from_protocol:migrate_to
    ~to_protocol:Injected_test
    ~loser_protocols:[migrate_from] ;
  Voting.register
    ~from_protocol:migrate_to
    ~to_protocol:Demo
    ~loser_protocols:[migrate_from]

(* Tests related to one-off protocol migrations used in the past. *)
let register_older_protocol_migration_tests () =
  Iticket_migration.register
    ~migrate_from:Protocol.Kathmandu
    ~migrate_to:Protocol.Lima

(* Register tests that use [Protocol.register_test] and for which we rely on
   [?supports] to decide which protocols the tests should run on.
   As a consequence, all those tests should be registered with [Protocol.all]
   as the list of protocols.

   In the future, we could remove the [Protocol.t list] arguments from
   [Protocol.register_test]. It would use [Protocol.all] directly instead.
   Then we could remove the [~protocols] argument from all register functions. *)
let register_protocol_tests_that_use_supports_correctly () =
  let protocols = Protocol.all in
  Bad_indentation.register ~protocols ;
  Baker_test.register ~protocols ;
  Baking.register ~protocols ;
  Baking.register_operations_pool ~protocols ;
  Basic.register ~protocols ;
  Big_map_all.register ~protocols ;
  Big_map_arity.register ~protocols ;
  Bootstrap.register ~protocols ;
  Cache_cache.register protocols ;
  Client_commands.register ~protocols ;
  Client_config.register ~protocols ;
  Client_run_view.register ~protocols ;
  Contract_hash_fun.register ~protocols ;
  Create_contract.register ~protocols ;
  Deposits_limit.register ~protocols ;
  Double_bake.register ~protocols ;
  Encoding.register ~protocols ;
  Events.register ~protocols ;
  Forge.register ~protocols ;
  Ghostnet_dictator_migration.register ~protocols ;
  Global_constants.register ~protocols ;
  Large_metadata.register ~protocols ;
  Light.register ~protocols ;
  Liquidity_baking_per_block_votes.register ~protocols ;
  Manager_operations.register ~protocols ;
  Mockup.register ~protocols ;
  Mockup.register_global_constants ~protocols ;
  Monitor_operations.register ~protocols ;
  Multinode_snapshot.register ~protocols ;
  Node_event_level.register ~protocols ;
  Normalize.register ~protocols ;
  Operation_validation.register ~protocols ;
  P2p.register ~protocols ;
  Precheck.register ~protocols ;
  Prevalidator.register ~protocols ;
  Protocol_limits.register ~protocols ;
  Proxy.register ~protocols ;
  Proxy_server_test.register ~protocols ;
  RPC_test.register protocols ;
  Reject_malformed_micheline.register ~protocols ;
  Replace_by_fees.register ~protocols ;
  Retro.register ~protocols ;
  Rpc_config_logging.register ~protocols ;
  Run_operation_RPC.register ~protocols ;
  Run_script.register ~protocols ;
  Runtime_script_failure.register ~protocols ;
  Sapling.register ~protocols ;
  Self_address_transfer.register ~protocols ;
  Signer_test.register ~protocols ;
  Stresstest_command.register ~protocols ;
  Synchronisation_heuristic.register ~protocols ;
  Tenderbake.register ~protocols ;
  Testnet_dictator.register ~protocols ;
  Tickets.register ~protocols ;
  Timelock.register ~protocols ;
  Tx_rollup.register ~protocols ;
  Tx_rollup_l2_node.register ~protocols ;
  Tzip4_view.register ~protocols ;
  Used_paid_storage_spaces.register ~protocols ;
  Vdf_test.register ~protocols

(* Regression tests are not easy to maintain for multiple protocols because one needs
   to update and maintain all the expected output files. Some of them, such as
   those in [create_contract.ml] and [deposits_limit.ml], already support all protocols.
   Some do not. Those that do not are declared here. *)
let register_protocol_specific_because_regression_tests () =
  Consensus_key.register ~protocols:[Alpha] ;
  Dal.register ~protocols:[Alpha] ;
  Hash_data.register ~protocols:[Alpha] ;
  Increase_paid_storage.register ~protocols:[Kathmandu; Alpha] ;
  Sc_rollup.register ~protocols:[Alpha] ;
  Test_contract_bls12_381.register ~protocols:[Alpha] ;
  Ticket_receipt_and_rpc.register ~protocols:[Alpha] ;
  Views.register [Alpha]

let () =
  register_protocol_independent_tests () ;
  register_protocol_migration_tests () ;
  register_older_protocol_migration_tests () ;
  register_protocol_tests_that_use_supports_correctly () ;
  register_protocol_specific_because_regression_tests () ;
  (* Test.run () should be the last statement, don't register afterwards! *)
  Test.run ()
