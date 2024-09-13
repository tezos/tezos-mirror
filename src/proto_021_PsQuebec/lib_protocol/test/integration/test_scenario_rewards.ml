(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Adaptive Issuance, Rewards
    Invocation:   dune exec src/proto_021_PsquebeC/lib_protocol/test/integration/main.exe \
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

let begin_test_with_rewards_checks ~init_limit =
  let set_limit name l =
    let params =
      {
        limit_of_staking_over_baking = Q.of_float l;
        edge_of_baking_over_staking = Q.of_float 0.5;
      }
    in
    set_delegate_params name params
  in
  init_constants ~reward_per_block:1_000_000_000L ()
  --> activate_ai `Force
  --> begin_test ["delegate"; "faucet"]
  --> set_baker "delegate"
  --> add_account_with_funds
        "staker"
        ~funder:"faucet"
        (Amount (Tez.of_mutez 400_000_000_000L))
  --> set_limit "delegate" init_limit
  --> set_limit "faucet" 4. --> wait_delegate_parameters_activation
  --> set_delegate "staker" (Some "delegate")
  --> snapshot_balances "before staking" ["staker"]
  --> stake "staker" (Amount (Tez.of_mutez 111_000_000_000L))
  --> check_snapshot_balances
        ~f:
          (Test_scenario_autostaking.assert_balance_evolution
             ~loc:__LOC__
             ~for_accounts:["staker"]
             ~part:`staked
             Q.gt)
        "before staking"

let check_finalize_unstake_no_change staker =
  snapshot_balances "before finalize" [staker]
  --> finalize_unstake staker
  --> check_snapshot_balances "before finalize"

let test_rewards_with_limit_change =
  let set_limit name l =
    let params =
      {
        limit_of_staking_over_baking = Q.of_float l;
        edge_of_baking_over_staking = Q.of_float 0.5;
      }
    in
    set_delegate_params name params
  in
  begin_test_with_rewards_checks ~init_limit:5.
  --> set_limit "delegate" 0.
  --> wait_cycle_until `right_before_delegate_parameters_activation
  --> snapshot_balances "old limit" ["staker"]
  --> next_cycle
  (* The last cycle when the staker gets rewards *)
  --> check_snapshot_balances
        ~f:
          (Test_scenario_autostaking.assert_balance_evolution
             ~loc:__LOC__
             ~for_accounts:["staker"]
             ~part:`staked
             Q.gt)
        "old limit"
  --> snapshot_balances "limit = 0" ["staker"]
  --> next_cycle
  (* The staker does not get rewards *)
  --> check_snapshot_balances "limit = 0"
  --> Test_scenario_stake.fail_to_stake_refuse_external_staking
        ~loc:__LOC__
        "staker"
        ~amount:(Amount (Tez.of_mutez 222_000_000_000L))
  --> (Tag "unstake" --> unstake "staker" All
       --> check_finalize_unstake_no_change "staker"
       --> wait_n_cycles_f Test_scenario_stake.unstake_wait
       --> Test_scenario_stake.finalize "staker"
      |+ Tag "change delegate"
         --> set_delegate "staker" (Some "faucet")
         --> check_balance_field "staker" `Staked Tez.zero
         --> check_finalize_unstake_no_change "staker"
         --> Test_scenario_stake
             .fail_to_stake_with_unfinalizable_unstake_requests
               ~loc:__LOC__
               "staker"
               ~amount:(Amount (Tez.of_mutez 10_000_000_000L))
         --> wait_n_cycles_f Test_scenario_stake.unstake_wait
         --> assert_failure_in_check_balance_field
               ~loc:__LOC__
               "staker"
               `Unstaked_finalizable
               Tez.zero
         --> (Tag "finalize" --> finalize_unstake "staker"
             |+ Tag "don't finalize, stake"
                --> stake "staker" (Amount (Tez.of_mutez 10_000_000_000L)))
         --> check_balance_field "staker" `Unstaked_finalizable Tez.zero)

let test_stake_with_unfinalizable_unstake_requests =
  begin_test_with_rewards_checks ~init_limit:5.
  --> set_delegate "staker" (Some "faucet")
  --> check_balance_field "staker" `Staked Tez.zero
  --> check_finalize_unstake_no_change "staker"
  --> Test_scenario_stake.fail_to_stake_with_unfinalizable_unstake_requests
        ~loc:__LOC__
        "staker"
        ~amount:(Amount (Tez.of_mutez 10_000_000_000L))
  --> next_cycle
  (* Staker can stake with the old delegate with which he has
     unfinalizable unstake requests *)
  --> set_delegate "staker" (Some "delegate")
  --> stake "staker" (Amount (Tez.of_mutez 50_000_000_000L))

let check_overstaked_status ~loc ~expected delegate =
  let open Lwt_result_syntax in
  exec_unit (fun (block, state) ->
      let dlgt = State.find_account delegate state in
      let* info = Context.Delegate.info (B block) dlgt.pkh in
      let* staked_balance =
        Context.Contract.staked_balance (B block) dlgt.contract
      in
      let staked_balance = Option.value ~default:Tez.zero staked_balance in
      let overstaked_limit =
        Tez.(
          of_q
            ~round:`Up
            (mul_q staked_balance dlgt.parameters.limit_of_staking_over_baking))
      in
      let is_overstaked = Tez.(overstaked_limit < info.total_delegated_stake) in
      Log.debug "total_delegated_stake = %a" Tez.pp info.total_delegated_stake ;
      Log.debug
        "limit_of_staking_over_baking = %a"
        Q.pp_print
        dlgt.parameters.limit_of_staking_over_baking ;
      Log.debug "staked_balance = %a" Tez.pp staked_balance ;
      Log.debug
        "The diff is %a"
        Tez.pp
        (if is_overstaked then
           Tez.(info.total_delegated_stake -! overstaked_limit)
         else Tez.(overstaked_limit -! info.total_delegated_stake)) ;
      Assert.equal_bool ~loc is_overstaked expected)

let test_overstake =
  let set_edge pct =
    let params =
      {
        limit_of_staking_over_baking = Q.one;
        edge_of_baking_over_staking = Q.of_float pct;
      }
    in
    set_delegate_params "delegate" params
  in
  let check_rewards ~loc ~snapshot_name name (diff : Q.t) =
    let gt_diff (diff : Q.t) new_b old_b = Q.equal new_b (Q.add old_b diff) in
    check_snapshot_balances
      ~f:
        (Test_scenario_autostaking.assert_balance_evolution
           ~loc
           ~for_accounts:[name]
           ~part:`staked
           (gt_diff diff))
      snapshot_name
  in
  let next_block_with_check_rewards ~loc ~staker_diff ~delegate_diff =
    snapshot_balances "1 block" ["staker"]
    --> snapshot_balances "1 block-d" ["delegate"]
    --> next_block
    --> check_rewards ~loc ~snapshot_name:"1 block" "staker" staker_diff
    --> check_rewards ~loc ~snapshot_name:"1 block-d" "delegate" delegate_diff
  in
  begin_test_with_rewards_checks ~init_limit:3.
  --> next_block_with_check_rewards
        ~loc:__LOC__
        ~staker_diff:(Q.of_int32 21_315l)
        ~delegate_diff:(Q.of_int32 98_803l)
  --> next_block_with_check_rewards
        ~loc:__LOC__
        ~staker_diff:(Q.of_int32 21_315l)
        ~delegate_diff:(Q.of_int32 98_803l)
  --> add_account_with_funds
        "staker2"
        ~funder:"faucet"
        (Amount (Tez.of_mutez 600_000_000_000L))
  --> set_delegate "staker2" (Some "delegate")
  --> stake "staker2" (Amount (Tez.of_mutez 222_000_000_000L))
  --> next_block_with_check_rewards
        ~loc:__LOC__
        ~staker_diff:
          (Q.make (Z.of_int64 69_188_150_000_000L) (Z.of_int64 5_549_995_737L))
          (* = 12466.3429088 *)
        ~delegate_diff:(Q.of_int32 82_719l)
  --> check_overstaked_status ~loc:__LOC__ ~expected:false "delegate"
  --> stake "staker2" (Amount (Tez.of_mutez 300_000_000_000L))
  --> check_overstaked_status ~loc:__LOC__ ~expected:true "delegate"
  --> next_block_with_check_rewards
        ~loc:__LOC__
        ~staker_diff:
          (Q.make
             (Z.of_int64 833_314_000_000_000L)
             (Z.of_int64 105_499_888_531L))
          (* = 7898.71925 *)
        ~delegate_diff:(Q.of_int32 75_074l)
  --> set_edge 0.8 --> wait_delegate_parameters_activation
  --> set_delegate "staker2" (Some "faucet")
  --> check_overstaked_status ~loc:__LOC__ ~expected:false "delegate"
  --> next_block_with_check_rewards
        ~loc:__LOC__
        ~staker_diff:(Q.of_int32 26_486l)
        ~delegate_diff:(Q.of_int32 346_663l)
  --> wait_n_cycles_f Test_scenario_stake.unstake_wait
  --> next_block_with_check_rewards
        ~loc:__LOC__
        ~staker_diff:(Q.of_int32 12_740l)
        ~delegate_diff:(Q.of_int32 166_764l)

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
       ("Test rewards with limit change", test_rewards_with_limit_change);
       ( "Test stake with unfinalizable unstake requests",
         test_stake_with_unfinalizable_unstake_requests );
       ("Test overstake", test_overstake);
     ]

let () =
  register_tests ~__FILE__ ~tags:["protocol"; "scenario"; "rewards"] tests
