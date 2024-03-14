(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Adaptive Issuance, Slashing
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/integration/main.exe \
                   -- --file test_scenario_slashing_stakers.ml
    Subject:      Test slashing scenarios in the protocol.
*)

open Adaptive_issuance_helpers
open State_account
open Tez_helpers.Ez_tez
open Scenario
open Scenario_constants

let test_simple_slash =
  let any_slash delegate =
    Tag "double baking" --> double_bake delegate
    |+ Tag "double attesting"
       --> double_attest ~other_bakers:("bootstrap2", "bootstrap3") delegate
    |+ Tag "double preattesting"
       --> double_preattest ~other_bakers:("bootstrap2", "bootstrap3") delegate
  in
  init_constants ()
  --> set S.Adaptive_issuance.autostaking_enable false
  --> activate_ai `Zero_threshold
  --> branch_flag S.Adaptive_issuance.ns_enable
  --> begin_test ["delegate"; "bootstrap1"; "bootstrap2"; "bootstrap3"]
  --> (Tag "No AI" --> next_cycle
      |+ Tag "Yes AI" --> next_block --> wait_ai_activation)
  --> any_slash "delegate"
  --> snapshot_balances "before slash" ["delegate"]
  --> ((Tag "denounce same cycle"
        --> make_denunciations ()
            (* delegate can be forbidden in this case, so we set another baker *)
        --> exclude_bakers ["delegate"]
       |+ Tag "denounce next cycle" --> next_cycle --> make_denunciations ()
          (* delegate can be forbidden in this case, so we set another baker *)
          --> exclude_bakers ["delegate"])
       --> (Empty
           |+ Tag "another slash" --> any_slash "bootstrap1"
              --> make_denunciations ()
              (* bootstrap1 can be forbidden in this case, so we set another baker *)
              --> exclude_bakers ["delegate"; "bootstrap1"])
       --> check_snapshot_balances "before slash"
       --> exec_unit (check_pending_slashings ~loc:__LOC__)
       --> next_cycle
       --> assert_failure
             (exec_unit (fun (_block, state) ->
                  if State_ai_flags.Delayed_slashing.enabled state then
                    failwith "ns_enable = true: slash not applied yet"
                  else Lwt_result_syntax.return_unit)
             --> check_snapshot_balances "before slash")
       --> exec_unit (check_pending_slashings ~loc:__LOC__)
       --> next_cycle
      |+ Tag "denounce too late" --> next_cycle --> next_cycle
         --> assert_failure
               ~expected_error:(fun (_block, state) ->
                 let ds = state.State.double_signings in
                 let ds = match ds with [a] -> a | _ -> assert false in
                 let level =
                   Protocol.Alpha_context.Raw_level.Internal_for_tests.from_repr
                     ds.misbehaviour.level
                 in
                 let last_cycle =
                   Cycle.add
                     (Block.current_cycle_of_level
                        ~blocks_per_cycle:state.State.constants.blocks_per_cycle
                        ~current_level:
                          (Protocol.Raw_level_repr.to_int32
                             ds.misbehaviour.level))
                     (Protocol.Constants_repr.max_slashing_period - 1)
                 in
                 let (kind : Protocol.Alpha_context.Misbehaviour.kind) =
                   (* This conversion would not be needed if
                      Misbehaviour_repr.kind were moved to a
                      separate file that doesn't have under/over
                      Alpha_context versions. *)
                   match ds.misbehaviour.kind with
                   | Double_baking -> Double_baking
                   | Double_attesting -> Double_attesting
                   | Double_preattesting -> Double_preattesting
                 in
                 [
                   Environment.Ecoproto_error
                     (Protocol.Validate_errors.Anonymous.Outdated_denunciation
                        {kind; level; last_cycle});
                 ])
               (make_denunciations ())
         --> check_snapshot_balances "before slash")
