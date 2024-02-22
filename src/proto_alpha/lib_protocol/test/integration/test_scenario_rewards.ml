(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Adaptive Issuance, Rewards
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/integration/main.exe \
                   -- --file test_scenario_rewards.ml
    Subject:      Test rewards issuance in the protocol.
*)

open Adaptive_issuance_helpers
open State_account
open Tez_helpers.Ez_tez
open Scenario_dsl
open Scenario_base
open Scenario_op
open Test_scenario_base

let test_wait_with_rewards =
  let constants =
    init_constants ~reward_per_block:1_000_000_000L ~autostaking_enable:false ()
  in
  let set_edge pct =
    let params =
      {
        limit_of_staking_over_baking = Q.one;
        edge_of_baking_over_staking = Q.of_float pct;
      }
    in
    set_delegate_params "delegate" params
  in
  begin_test ~activate_ai:true ~constants ["delegate"; "faucet"]
  --> set_baker "faucet"
  --> (Tag "edge = 0" --> set_edge 0.
      |+ Tag "edge = 0.24" --> set_edge 0.24
      |+ Tag "edge = 0.11..." --> set_edge 0.1111111111
      |+ Tag "edge = 1" --> set_edge 1.)
  --> add_account_with_funds
        "staker1"
        "faucet"
        (Amount (Tez.of_mutez 2_000_000_000L))
  --> add_account_with_funds
        "staker2"
        "faucet"
        (Amount (Tez.of_mutez 2_000_000_000L))
  --> add_account_with_funds
        "staker3"
        "faucet"
        (Amount (Tez.of_mutez 2_000_000_000L))
  --> set_delegate "staker1" (Some "delegate")
  --> set_delegate "staker2" (Some "delegate")
  --> set_delegate "staker3" (Some "delegate")
  --> set_baker "delegate"
  --> (Tag "block step" --> wait_n_blocks 200
      |+ Tag "cycle step" --> wait_n_cycles 20
      |+ Tag "wait AI activation" --> next_block --> wait_ai_activation
         --> (Tag "no staker" --> Empty
             |+ Tag "one staker"
                --> stake "staker1" (Amount (Tez.of_mutez 450_000_111L))
             |+ Tag "two stakers"
                --> stake "staker1" (Amount (Tez.of_mutez 444_000_111L))
                --> stake "staker2" (Amount (Tez.of_mutez 333_001_987L))
                --> set_baker "delegate"
             |+ Tag "three stakers"
                --> stake "staker1" (Amount (Tez.of_mutez 444_000_111L))
                --> stake "staker2" (Amount (Tez.of_mutez 333_001_987L))
                --> stake "staker3" (Amount (Tez.of_mutez 123_456_788L)))
         --> (Tag "block step" --> wait_n_blocks 100
             |+ Tag "cycle step" --> wait_n_cycles 10))
  --> Tag "staker 1 unstakes half..." --> unstake "staker1" Half
  --> (Tag "block step" --> wait_n_blocks 100
      |+ Tag "cycle step" --> wait_n_cycles 10)

let test_ai_curve_activation_time =
  let constants =
    init_constants
      ~reward_per_block:1_000_000_000L
      ~deactivate_dynamic:true
      ~autostaking_enable:false
      ()
  in
  let pc = constants.consensus_rights_delay in
  begin_test ~activate_ai:true ~burn_rewards:true ~constants [""]
  --> next_block --> save_current_rate (* before AI rate *)
  --> wait_ai_activation
  (* Rate remains unchanged right after AI activation, we must wait [pc + 1] cycles *)
  --> check_rate_evolution Q.equal
  --> wait_n_cycles pc
  --> check_rate_evolution Q.equal
  --> next_cycle
  (* The new rate should be active now. With the chosen constants, it should be lower.
     We go from 1000tz per day to (at most) 5% of 4_000_000tz per year *)
  --> check_rate_evolution Q.gt

let test_static =
  let constants =
    init_constants
      ~reward_per_block:1_000_000_000L
      ~deactivate_dynamic:true
      ~autostaking_enable:false
      ()
  in
  let rate_var_lag = constants.consensus_rights_delay in
  let init_params =
    {limit_of_staking_over_baking = Q.one; edge_of_baking_over_staking = Q.one}
  in
  let delta = Amount (Tez.of_mutez 20_000_000_000L) in
  let cycle_stake =
    save_current_rate --> stake "delegate" delta --> next_cycle
    --> check_rate_evolution Q.gt
  in
  let cycle_unstake =
    save_current_rate --> unstake "delegate" delta --> next_cycle
    --> check_rate_evolution Q.lt
  in
  let cycle_stable =
    save_current_rate --> next_cycle --> check_rate_evolution Q.equal
  in
  begin_test ~activate_ai:true ~burn_rewards:true ~constants ["delegate"]
  --> set_delegate_params "delegate" init_params
  --> save_current_rate --> wait_ai_activation
  (* We stake about 50% of the total supply *)
  --> stake "delegate" (Amount (Tez.of_mutez 1_800_000_000_000L))
  --> stake "__bootstrap__" (Amount (Tez.of_mutez 1_800_000_000_000L))
  --> (Tag "increase stake, decrease rate" --> next_cycle
       --> loop rate_var_lag (stake "delegate" delta --> next_cycle)
       --> loop 10 cycle_stake
      |+ Tag "decrease stake, increase rate" --> next_cycle
         --> loop rate_var_lag (unstake "delegate" delta --> next_cycle)
         --> loop 10 cycle_unstake
      |+ Tag "stable stake, stable rate" --> next_cycle
         --> wait_n_cycles rate_var_lag --> loop 10 cycle_stable
      |+ Tag "test timing" --> wait_n_cycles rate_var_lag
         --> check_rate_evolution Q.equal
         --> next_cycle --> check_rate_evolution Q.gt --> save_current_rate
         --> (Tag "increase stake" --> stake "delegate" delta
              --> wait_n_cycles rate_var_lag
              --> check_rate_evolution Q.equal
              --> next_cycle --> check_rate_evolution Q.gt
             |+ Tag "decrease stake" --> unstake "delegate" delta
                --> wait_n_cycles rate_var_lag
                --> check_rate_evolution Q.equal
                --> next_cycle --> check_rate_evolution Q.lt))

let tests =
  tests_of_scenarios
  @@ [
       ("Test wait with rewards", test_wait_with_rewards);
       ("Test ai curve activation time", test_ai_curve_activation_time);
       (* ("Test static rate", test_static); *)
     ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("protocol rewards", tests)]
  |> Lwt_main.run
