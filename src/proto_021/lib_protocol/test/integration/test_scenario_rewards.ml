(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Adaptive Issuance, Rewards
    Invocation:   dune exec src/proto_beta/lib_protocol/test/integration/main.exe \
                   -- --file test_scenario_rewards.ml
    Subject:      Test rewards issuance in the protocol.
*)

open Adaptive_issuance_helpers
open State_account
open Tez_helpers.Ez_tez
open Scenario

(** Test reward distribution without AI and without autostaking.
    [State_account.add_*_rewards] ensures the rewards are distributed
    correctly, and it is checked at the end of every block.
*)
let test_wait_rewards_no_ai_no_auto =
  (* Prime number to always trigger roundings *)
  init_constants ~reward_per_block:1_000_000_007L ()
  --> set S.Adaptive_issuance.autostaking_enable false
  --> activate_ai `No
  --> begin_test ["delegate1"; "delegate2"; "delegate3"]
  --> wait_n_cycles 20

(** Test reward distribution without AI and with autostaking.
    We expect autostaking to keep the ratio total/frozen equal to
    [limit_of_delegation_over_baking + 1], rounding towards frozen.
*)
let test_wait_rewards_no_ai_yes_auto =
  let open Lwt_result_syntax in
  let check_balanced_balance src_name =
    exec_unit (fun (_block, state) ->
        let src_balance, src_total =
          balance_and_total_balance_of_account src_name state.State.account_map
        in
        let rat = state.constants.limit_of_delegation_over_baking + 1 in
        let expected_frozen =
          Tez_helpers.(mul_q src_total Q.(1 // rat) |> of_q ~round:`Up)
        in
        let* () =
          Assert.equal_tez
            ~loc:__LOC__
            expected_frozen
            (Tez_helpers.of_q ~round:`Down src_balance.staked_b)
        in
        return_unit)
  in
  let check_all_balanced_balances = unfold check_balanced_balance in
  let all_delegates = ["delegate1"; "delegate2"; "delegate3"] in
  init_constants ~reward_per_block:1_000_000_007L ()
  --> set S.Adaptive_issuance.autostaking_enable true
  --> activate_ai `No --> begin_test all_delegates
  --> loop
        20
        (exec bake_until_dawn_of_next_cycle
        --> check_all_balanced_balances all_delegates
        --> next_block)

(** Tests reward distribution under AI:
    - with and without stakers (sometimes overstaking);
    - with different values of edge. *)
let test_wait_rewards_with_ai =
  let set_edge pct =
    let params =
      {
        limit_of_staking_over_baking = Q.one;
        edge_of_baking_over_staking = Q.of_float pct;
      }
    in
    set_delegate_params "delegate" params
  in
  init_constants ~reward_per_block:1_000_000_007L ()
  --> activate_ai `Force
  --> begin_test ["delegate"; "faucet"]
  --> (Tag "edge = 0" --> set_edge 0.
      |+ Tag "edge = 0.24" --> set_edge 0.24
      |+ Tag "edge = 0.11... repeating" --> set_edge 0.1111111111
      |+ Tag "edge = 1" --> set_edge 1.)
  --> wait_delegate_parameters_activation
  --> (Tag "no staker" --> Empty
      |+ Tag "one staker"
         --> add_account_with_funds
               "staker1"
               ~funder:"faucet"
               (Amount (Tez.of_mutez 2_000_000_000L))
         --> set_delegate "staker1" (Some "delegate")
         --> stake "staker1" (Amount (Tez.of_mutez 444_000_111L))
         --> (Empty
             |+ Tag "two stakers"
                --> add_account_with_funds
                      "staker2"
                      ~funder:"faucet"
                      (Amount (Tez.of_mutez 2_000_000_000L))
                --> set_delegate "staker2" (Some "delegate")
                --> stake "staker2" (Amount (Tez.of_mutez 333_001_987L))
                --> (Empty
                    |+ Tag "three stakers! ha ha ha"
                       (* This staker overstakes *)
                       --> add_account_with_funds
                             "staker3"
                             ~funder:"faucet"
                             (Amount (Tez.of_mutez 1_800_000_000_000L))
                       --> set_delegate "staker3" (Some "delegate")
                       --> stake
                             "staker3"
                             (Amount (Tez.of_mutez 1_799_123_456_788L))
                       --> exec_unit (fun (_, state) ->
                               let src = State.find_account "delegate" state in
                               let self_frozen =
                                 src.frozen_deposits.self_current
                               in
                               let staked =
                                 Frozen_tez.total_co_current_q
                                   src.frozen_deposits.co_current
                               in
                               Assert.is_true
                                 ~loc:__LOC__
                                 Q.(
                                   Tez.mul_q
                                     self_frozen
                                     src.parameters.limit_of_staking_over_baking
                                   < staked)))))
  --> set_baker "delegate" --> wait_n_cycles 20

(** Tests reward distribution under AI for one baker and one staker,
    and different arbitrary events:
    staking, unstaking, finalizing, slashing *)
let test_wait_rewards_with_ai_staker_variation =
  let set_edge pct =
    let params =
      {
        limit_of_staking_over_baking = Q.one;
        edge_of_baking_over_staking = Q.of_float pct;
      }
    in
    set_delegate_params "delegate" params
  in
  init_constants ~reward_per_block:1_000_000_007L ()
  --> activate_ai `Force
  --> begin_test ["delegate"; "faucet"]
  --> set_edge 0.24 --> wait_delegate_parameters_activation
  --> add_account_with_funds
        "staker"
        ~funder:"faucet"
        (Amount (Tez.of_mutez 20_000_000_000L))
  --> set_delegate "staker" (Some "delegate")
  --> stake "staker" (Amount (Tez.of_mutez 12_444_000_111L))
  --> set_baker "delegate"
  (* Regular rewards *)
  --> wait_n_cycles 7
  (* Staker unstakes some *)
  --> unstake "staker" Half
  --> wait_n_cycles 4
  (* Staker restakes some *)
  --> stake "staker" Half
  (* Reactivate another baker for allowing it to bake later *)
  --> set_delegate "faucet" (Some "faucet")
  --> wait_n_cycles 4
  (* Add unstake requests in the mix *)
  --> unstake "staker" Half
  --> next_cycle
  (* Double bake for the delegate *)
  --> set_baker "faucet"
  --> double_bake "delegate" --> make_denunciations ()
  (* Wait for the delegate to not be forbidden anymore *)
  --> wait_n_cycles 10
  (* Reactivate it, make it bake, and see everything is as before *)
  --> set_delegate "delegate" (Some "delegate")
  --> wait_n_cycles 4 --> set_baker "delegate" --> wait_n_cycles 10

(** Tests reward distribution under AI for one baker and two stakers,
    and the baker changes its limit parameter while being overstaked.
    We expect the rewards for the stakers to change accordingly with the limit.
*)
let test_overstake_different_limits =
  let set_limit l =
    let params =
      {
        limit_of_staking_over_baking = Q.of_float l;
        edge_of_baking_over_staking = Q.zero;
      }
    in
    set_delegate_params "delegate" params
  in
  init_constants ~reward_per_block:1_000_000_007L ()
  --> activate_ai `Force
  --> begin_test ["delegate"; "faucet"]
  --> set_baker "faucet"
  --> unstake "delegate" (Amount (Tez.of_mutez 190_000_000_000L))
  --> check_balance_field "delegate" `Staked (Tez.of_mutez 10_000_000_000L)
  --> set_baker "delegate"
  (* same rights to have same block distribution *)
  --> unstake "faucet" (Amount (Tez.of_mutez 190_000_000_000L))
  --> unstake "__bootstrap__" (Amount (Tez.of_mutez 190_000_000_000L))
  --> set_limit 5. --> wait_delegate_parameters_activation
  --> add_account_with_funds
        "staker1"
        ~funder:"faucet"
        (Amount (Tez.of_mutez 400_000_000_000L))
  --> set_delegate "staker1" (Some "delegate")
  --> add_account_with_funds
        "staker2"
        ~funder:"faucet"
        (Amount (Tez.of_mutez 400_000_000_000L))
  --> set_delegate "staker2" (Some "delegate")
  (* Always overstaked *)
  --> stake "staker1" (Amount (Tez.of_mutez 111_000_000_000L))
  --> stake "staker2" (Amount (Tez.of_mutez 222_000_000_000L))
  --> (Tag "limit = 0" --> set_limit 0.
      |+ Tag "limit = 0.24" --> set_limit 0.24
      |+ Tag "limit = 1" --> set_limit 1.
      |+ Tag "limit >= 5" --> set_limit 6.)
  (* Before activation: testing global limit (5) *)
  --> wait_delegate_parameters_activation
  --> wait_n_cycles 6
  --> (Tag "limit = 0" --> set_limit 0.
      |+ Tag "limit = 0.24" --> set_limit 0.24
      |+ Tag "limit = 1" --> set_limit 1.
      |+ Tag "limit >= 5" --> set_limit 6.)
  --> wait_delegate_parameters_activation --> wait_n_cycles 6

(** Tests that the activation time for AI is as expected:
    The expected delay is [consensus_rights_delay] + 1 cycles after activation. *)
let test_ai_curve_activation_time =
  let consensus_rights_delay (_, state) =
    state.State.constants.consensus_rights_delay
  in
  init_constants ~reward_per_block:1_000_000_000L ~deactivate_dynamic:true ()
  --> set S.Adaptive_issuance.autostaking_enable false
  --> activate_ai `Zero_threshold
  --> begin_test ~burn_rewards:true [""]
  --> next_block --> save_current_rate (* before AI rate *)
  --> wait_ai_activation
  (* Rate remains unchanged right after AI activation, we must wait [pc + 1] cycles *)
  --> check_rate_evolution Q.equal
  --> wait_n_cycles_f consensus_rights_delay
  --> check_rate_evolution Q.equal
  --> next_cycle
  (* The new rate should be active now. With the chosen constants, it should be lower.
     We go from 1000tz per day to (at most) 5% of 4_000_000tz per year *)
  --> check_rate_evolution Q.gt

(** Integration test for Adaptive Issuance.
    Tests that the curve is decreasing wrt stake ratio. *)
let test_static_decreasing =
  let rate_var_lag = Default_parameters.constants_test.consensus_rights_delay in
  let delta = Amount (Tez.of_mutez 20_000_000_000L) in
  let q_almost_equal x y =
    let rat = Q.div x y in
    (* ~ inverse square root of total supply *)
    let epsilon = Q.(1 // 100_000) in
    Q.(rat >= one - epsilon && rat <= one + epsilon)
  in
  let cycle_stake =
    save_current_rate --> stake "delegate" delta --> next_cycle
    --> check_rate_evolution Q.gt
  in
  let cycle_unstake =
    save_current_rate --> unstake "delegate" delta --> next_cycle
    --> check_rate_evolution Q.lt
  in
  let cycle_stable =
    save_current_rate --> next_cycle --> check_rate_evolution q_almost_equal
  in
  init_constants ~reward_per_block:1L ~deactivate_dynamic:true ()
  (* Set rate bounds that should not be reached *)
  --> set S.Adaptive_issuance.autostaking_enable false
  --> set
        S.Adaptive_issuance.Adaptive_rewards_params.issuance_ratio_final_min
        Q.(1 // 100_000)
  --> set
        S.Adaptive_issuance.Adaptive_rewards_params.issuance_ratio_initial_min
        Q.(1 // 100_000)
  --> set
        S.Adaptive_issuance.Adaptive_rewards_params.issuance_ratio_final_max
        Q.one
  --> set
        S.Adaptive_issuance.Adaptive_rewards_params.issuance_ratio_initial_max
        Q.one
  --> activate_ai `Zero_threshold
  --> begin_test ~burn_rewards:true ["delegate"]
  --> next_block --> wait_ai_activation
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
         --> wait_n_cycles rate_var_lag --> loop 10 cycle_stable)

(** Integration test for Adaptive Issuance.
    Tests that the curve is updated for stake movement only after
    [consensus_rights_delay] cycles. *)
let test_static_timing =
  let consensus_rights_delay (_block, state) =
    state.State.constants.consensus_rights_delay
  in
  let delta = Amount (Tez.of_mutez 20_000_000_000L) in
  let q_almost_equal x y =
    let rat = Q.div x y in
    (* ~ inverse square root of total supply *)
    let epsilon = Q.(1 // 100_000) in
    Q.(rat >= one - epsilon && rat <= one + epsilon)
  in
  init_constants ~reward_per_block:1L ~deactivate_dynamic:true ()
  (* Set rate bounds that should not be reached *)
  --> set S.Adaptive_issuance.autostaking_enable false
  --> set
        S.Adaptive_issuance.Adaptive_rewards_params.issuance_ratio_final_min
        Q.(1 // 100_000)
  --> set
        S.Adaptive_issuance.Adaptive_rewards_params.issuance_ratio_initial_min
        Q.(1 // 100_000)
  --> set
        S.Adaptive_issuance.Adaptive_rewards_params.issuance_ratio_final_max
        Q.one
  --> set
        S.Adaptive_issuance.Adaptive_rewards_params.issuance_ratio_initial_max
        Q.one
  --> activate_ai `Force
  --> begin_test ~burn_rewards:true ["delegate"]
  (* We stake about 50% of the total supply *)
  --> stake "delegate" (Amount (Tez.of_mutez 1_800_000_000_000L))
  --> stake "__bootstrap__" (Amount (Tez.of_mutez 1_800_000_000_000L))
  --> wait_n_cycles_f (fun x -> consensus_rights_delay x + 1)
  --> save_current_rate
  --> (Tag "increase stake" --> stake "delegate" delta
       --> wait_n_cycles_f consensus_rights_delay
       --> check_rate_evolution q_almost_equal
       --> next_cycle --> check_rate_evolution Q.gt
      |+ Tag "decrease stake" --> unstake "delegate" delta
         --> wait_n_cycles_f consensus_rights_delay
         --> check_rate_evolution q_almost_equal
         --> next_cycle --> check_rate_evolution Q.lt)

let tests =
  tests_of_scenarios
  @@ [
       ("Test wait rewards no AI no autostake", test_wait_rewards_no_ai_no_auto);
       ( "Test wait rewards no AI yes autostake",
         test_wait_rewards_no_ai_yes_auto );
       ("Test wait rewards with AI, stakers and edge", test_wait_rewards_with_ai);
       ( "Test wait rewards with AI and stake variation events",
         test_wait_rewards_with_ai_staker_variation );
       ("Test ai curve activation time", test_ai_curve_activation_time);
       ( "Test static rate decreasing with stake ratio increasing",
         test_static_decreasing );
       ( "Test static rate updated after consensus_rights_delay",
         test_static_timing );
       ( "Test limit parameter with overstake and rewards",
         test_overstake_different_limits );
     ]

let () =
  register_tests ~__FILE__ ~tags:["protocol"; "scenario"; "rewards"] tests
