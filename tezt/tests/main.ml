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

let protocols = [Protocol.Alpha; Protocol.Jakarta; Protocol.Ithaca]

let migrate_from = Protocol.Jakarta

let migrate_to = Protocol.Alpha

(* This module runs the tests implemented in all other modules of this directory.
   Each module defines tests which are thematically related,
   as functions to be called here. *)
let () =
  (* Tests that are relatively protocol-agnostic.
     We can run them on all protocols, or only one if the CI would be too slow. *)
  Baker_test.register ~protocols:[Alpha] ;
  Signer_test.register ~protocols:[Alpha] ;
  Basic.register ~protocols:[Alpha] ;
  Client_config.register ~protocols:[Alpha] ;
  Global_constants.register ~protocols:[Alpha] ;
  Bootstrap.register ~protocols:[Alpha] ;
  Hash_data.register ~protocols:[Alpha] ;
  Synchronisation_heuristic.register ~protocols:[Alpha] ;
  Normalize.register ~protocols:[Alpha] ;
  Double_bake.register ~protocols:[Alpha] ;
  Light.register ~protocols:[Alpha] ;
  Mockup.register ~protocols:[Ithaca; Jakarta; Alpha] ;
  Mockup.register_constant_migration ~migrate_from ~migrate_to ;
  Mockup.register_global_constants ~protocols:[Alpha] ;
  Node_event_level.register ~protocols:[Alpha] ;
  Proxy.register ~protocols ;
  Proxy_server_test.register ~protocols:[Alpha] ;
  P2p.register ~protocols:[Alpha] ;
  Protocol_limits.register ~protocols:[Alpha] ;
  Protocol_migration.register ~migrate_from ~migrate_to ;
  User_activated_upgrade.register ~migrate_from ~migrate_to ;
  Rpc_config_logging.register ~protocols:[Alpha] ;
  Protocol_table_update.register ~migrate_from ~migrate_to ;
  Cache_cache.register [Ithaca; Jakarta; Alpha] ;
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/1823
     the "Baking" test does not have a documentation.
     I don't know if it is about baking accounts (and thus it is not a protocol-agnostic
     test since it requires Alpha) or about baking (which would make it possible to run
     on previous protocols, if not for a problem that was introduced in
     Client.bake_for which causes the default key to be a baking account key). *)
  Baking.register ~protocols:[Ithaca; Jakarta; Alpha] ;
  Prevalidator.register ~protocols:[Ithaca; Jakarta; Alpha] ;
  Monitor_operations.register ~protocols:[Alpha] ;
  Stresstest_command.register ~protocols:[Alpha] ;
  (* Adding a new protocol would require adding samples at ./tezt/tests/encoding_samples directory*)
  Encoding.register ~protocols ;
  Precheck.register ~protocols:[Ithaca; Jakarta; Alpha] ;
  Tenderbake.register ~protocols:[Alpha] ;
  Forge.register ~protocols:[Alpha] ;
  (* Tests that are protocol-independent.
     They do not take a protocol as a parameter and thus need to be registered only once. *)
  Light.register_protocol_independent () ;
  P2p.register_protocol_independent () ;
  Proxy.register_protocol_independent () ;
  Mockup.register_protocol_independent () ;
  Bootstrap.register_protocol_independent () ;
  Cli_tezos.register_protocol_independent () ;
  Client_keys.register_protocol_independent () ;
  (* Tests that are heavily protocol-dependent.
     Those modules define different tests for different protocols in their [register]. *)
  RPC_test.register [Ithaca; Jakarta; Alpha] ;
  Demo_counter.register () ;
  (* Alpha cannot stitch from Jakarta yet, but when it can, we can
     add a voting test from Jakarta to Alpha. *)
  Voting.register
    ~from_protocol:Ithaca
    ~to_protocol:(Known Jakarta)
    ~loser_protocols:[Alpha] ;
  Voting.register
    ~from_protocol:Ithaca
    ~to_protocol:Injected_test
    ~loser_protocols:[Alpha; Ithaca] ;
  Voting.register
    ~from_protocol:Alpha
    ~to_protocol:Injected_test
    ~loser_protocols:[Jakarta] ;
  Voting.register
    ~from_protocol:Alpha
    ~to_protocol:Demo
    ~loser_protocols:[Jakarta] ;
  (* This file tests an RPC added in protocol G *)
  Big_map_all.register () ;
  Reject_malformed_micheline.register ~protocols:[Alpha] ;
  Tx_rollup.register ~protocols:[Alpha] ;
  Tx_rollup_node.register ~protocols:[Alpha] ;
  Manager_operations.register ~protocols ;
  Replace_by_fees.register ~protocols:[Ithaca; Jakarta; Alpha] ;
  Sc_rollup.register ~protocols:[Alpha] ;
  Views.register [Alpha] ;
  Runtime_script_failure.register ~protocols ;
  Deposits_limit.register ~protocols:[Ithaca; Jakarta; Alpha] ;
  Large_metadata.register ~protocols:[Alpha] ;
  (* Relies on a feature only available since J. *)
  Run_script.register ~protocols:[Alpha] ;
  Sapling.register ~protocols:[Alpha] ;
  (* Test.run () should be the last statement, don't register afterwards! *)
  Test.run ()
