(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Adaptive Issuance, Staking
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/integration/main.exe \
                   -- --file test_scenario_stake.ml
    Subject:      Test staking operations in the protocol.
*)

open Adaptive_issuance_helpers
open State_account
open Tez_helpers.Ez_tez
open Scenario
open Test_scenario_base

let fs = Format.asprintf

let stake_init =
  stake "staker" Half
  --> (Tag "no wait after stake" --> Empty
      |+ Tag "wait after stake" --> wait_n_cycles 2)

let wait_for_unfreeze_and_check wait =
  snapshot_balances "wait snap" ["staker"]
  --> wait_n_cycles (wait - 1)
  (* Balance didn't change yet, but will change next cycle *)
  --> check_snapshot_balances "wait snap"
  --> next_cycle
  --> assert_failure (check_snapshot_balances "wait snap")

let finalize staker =
  assert_failure (check_balance_field staker `Unstaked_finalizable Tez.zero)
  --> finalize_unstake staker
  --> check_balance_field staker `Unstaked_finalizable Tez.zero

let simple_roundtrip =
  stake_init
  --> (Tag "full unstake" --> unstake "staker" All
      |+ Tag "half unstake" --> unstake "staker" Half)
  --> wait_for_unfreeze_and_check default_unstake_wait
  --> finalize "staker" --> next_cycle

let double_roundtrip =
  stake_init --> unstake "staker" Half
  --> (Tag "half then full unstake" --> wait_n_cycles 2 --> unstake "staker" All
      |+ Tag "half then half unstake" --> wait_n_cycles 2
         --> unstake "staker" Half)
  --> wait_for_unfreeze_and_check (default_unstake_wait - 2)
  --> wait_for_unfreeze_and_check 2
  --> finalize "staker" --> next_cycle

let shorter_roundtrip_for_baker =
  let constants = init_constants ~autostaking_enable:false () in
  let amount = Amount (Tez.of_mutez 333_000_000_000L) in
  let consensus_rights_delay = constants.consensus_rights_delay in
  start_with ~constants --> activate_ai true --> begin_test ["delegate"]
  --> next_block --> wait_ai_activation
  --> stake "delegate" (Amount (Tez.of_mutez 1_800_000_000_000L))
  --> next_cycle
  --> snapshot_balances "init" ["delegate"]
  --> unstake "delegate" amount
  --> List.fold_left
        (fun acc i -> acc |+ Tag (fs "wait %i cycles" i) --> wait_n_cycles i)
        (Tag "wait 0 cycles" --> Empty)
        (Stdlib.List.init (consensus_rights_delay + 1) (fun i -> i + 1))
  --> stake "delegate" amount
  --> check_snapshot_balances "init"

let status_quo_rountrip =
  let full_amount = Tez.of_mutez 10_000_000L in
  let amount_1 = Tez.of_mutez 2_999_999L in
  let amount_2 = Tez.of_mutez 7_000_001L in
  snapshot_balances "init" ["staker"]
  --> stake "staker" (Amount full_amount)
  --> next_cycle
  --> (Tag "1 unstake" --> unstake "staker" (Amount full_amount)
      |+ Tag "2 unstakes"
         --> unstake "staker" (Amount amount_1)
         --> next_cycle
         --> unstake "staker" (Amount amount_2))
  --> wait_n_cycles default_unstake_wait
  --> finalize "staker"
  --> check_snapshot_balances "init"

let scenario_finalize =
  no_tag --> stake "staker" Half --> next_cycle --> unstake "staker" Half
  --> wait_n_cycles (default_unstake_wait + 2)
  --> assert_failure
        (check_balance_field "staker" `Unstaked_finalizable Tez.zero)
  --> (Tag "finalize with finalize" --> finalize_unstake "staker"
      |+ Tag "finalize with stake" --> stake "staker" (Amount Tez.one_mutez)
      |+ Tag "finalize with unstake" --> unstake "staker" (Amount Tez.one_mutez)
      )
  --> check_balance_field "staker" `Unstaked_finalizable Tez.zero

(* Finalize does not go through when unstake does nothing *)
(* Todo: there might be other cases... like changing delegates *)
let scenario_not_finalize =
  no_tag --> stake "staker" Half --> next_cycle --> unstake "staker" All
  --> wait_n_cycles (default_unstake_wait + 2)
  --> assert_failure
        (check_balance_field "staker" `Unstaked_finalizable Tez.zero)
  --> snapshot_balances "not finalize" ["staker"]
  --> (Tag "no finalize with unstake if staked = 0"
      --> unstake "staker" (Amount Tez.one_mutez))
  --> assert_failure
        (check_balance_field "staker" `Unstaked_finalizable Tez.zero)
  --> check_snapshot_balances "not finalize"

(* TODO: there's probably more... *)
let scenario_forbidden_operations =
  let open Lwt_result_syntax in
  let fail_if_staker_is_self_delegate staker =
    exec (fun ((_, state) as input) ->
        if State.(is_self_delegate staker state) then
          failwith "_self_delegate_exit_"
        else return input)
  in
  no_tag
  (* Staking everything works for self delegates, but not for delegated accounts *)
  --> assert_failure
        (fail_if_staker_is_self_delegate "staker" --> stake "staker" All)
  (* stake is always forbidden when amount is zero *)
  --> assert_failure (stake "staker" Nothing)
  (* One cannot stake more that one has *)
  --> assert_failure (stake "staker" Max_tez)
  (* unstake is actually authorized for amount 0, but does nothing (doesn't even finalize if possible) *)
  --> unstake "staker" Nothing

let full_balance_in_finalizable =
  add_account_with_funds "dummy" "staker" (Amount (Tez.of_mutez 10_000_000L))
  --> stake "staker" All_but_one --> next_cycle --> unstake "staker" All
  --> wait_n_cycles (default_unstake_wait + 2)
  (* At this point, almost all the balance (but one mutez) of the stake is in finalizable *)
  (* Staking is possible, but not transfer *)
  --> assert_failure
        (transfer "staker" "dummy" (Amount (Tez.of_mutez 10_000_000L)))
  --> stake "staker" (Amount (Tez.of_mutez 10_000_000L))
  (* After the stake, transfer is possible again because the funds were finalized *)
  --> transfer "staker" "dummy" (Amount (Tez.of_mutez 10_000_000L))

(* Stress test: what happens if someone were to stake and unstake every cycle? *)
let odd_behavior =
  let one_cycle =
    no_tag --> stake "staker" Half --> unstake "staker" Half --> next_cycle
  in
  loop 20 one_cycle

let change_delegate =
  let constants = init_constants ~autostaking_enable:false () in
  let init_params =
    {limit_of_staking_over_baking = Q.one; edge_of_baking_over_staking = Q.one}
  in
  start_with ~constants --> activate_ai true
  --> begin_test ["delegate1"; "delegate2"]
  --> set_delegate_params "delegate1" init_params
  --> set_delegate_params "delegate2" init_params
  --> add_account_with_funds
        "staker"
        "delegate1"
        (Amount (Tez.of_mutez 2_000_000_000_000L))
  --> set_delegate "staker" (Some "delegate1")
  --> wait_ai_activation --> next_cycle --> stake "staker" Half --> next_cycle
  --> set_delegate "staker" (Some "delegate2")
  --> next_cycle
  --> assert_failure (stake "staker" Half)
  --> wait_n_cycles (default_unstake_wait + 1)
  --> stake "staker" Half

let unset_delegate =
  let constants = init_constants ~autostaking_enable:false () in
  let init_params =
    {limit_of_staking_over_baking = Q.one; edge_of_baking_over_staking = Q.one}
  in
  start_with ~constants --> activate_ai true --> begin_test ["delegate"]
  --> set_delegate_params "delegate" init_params
  --> add_account_with_funds
        "staker"
        "delegate"
        (Amount (Tez.of_mutez 2_000_000_000_000L))
  --> add_account_with_funds
        "dummy"
        "delegate"
        (Amount (Tez.of_mutez 2_000_000L))
  --> set_delegate "staker" (Some "delegate")
  --> wait_ai_activation --> next_cycle --> stake "staker" Half
  --> unstake "staker" All --> next_cycle --> set_delegate "staker" None
  --> next_cycle
  --> transfer "staker" "dummy" All
  (* staker has an empty liquid balance, but still has unstaked frozen tokens,
     so it doesn't get deactivated *)
  --> wait_n_cycles (default_unstake_wait + 1)
  --> finalize_unstake "staker"

let forbid_costaking =
  let default_constants =
    ("default protocol constants", init_constants ~autostaking_enable:false ())
  in
  let small_delegate_parameter_constants =
    ( "small delegate parameters delay",
      init_constants
        ~delegate_parameters_activation_delay:0
        ~autostaking_enable:false
        () )
  in
  let large_delegate_parameter_constants =
    ( "large delegate parameters delay",
      init_constants
        ~delegate_parameters_activation_delay:10
        ~autostaking_enable:false
        () )
  in
  let init_params =
    {limit_of_staking_over_baking = Q.one; edge_of_baking_over_staking = Q.one}
  in
  let no_costake_params =
    {limit_of_staking_over_baking = Q.zero; edge_of_baking_over_staking = Q.one}
  in
  let amount = Amount (Tez.of_mutez 1_000_000L) in
  (* init *)
  start_with_list
    ~constants:
      [
        default_constants;
        small_delegate_parameter_constants;
        large_delegate_parameter_constants;
      ]
  --> activate_ai true --> begin_test ["delegate"]
  --> set_delegate_params "delegate" init_params
  --> add_account_with_funds
        "staker"
        "delegate"
        (Amount (Tez.of_mutez 2_000_000_000_000L))
  --> set_delegate "staker" (Some "delegate")
  --> wait_cycle (`And (`AI_activation, `delegate_parameters_activation))
  --> next_cycle
  (* try stake in normal conditions *)
  --> stake "staker" amount
  (* Change delegate parameters to forbid staking *)
  --> set_delegate_params "delegate" no_costake_params
  (* The changes are not immediate *)
  --> stake "staker" amount
  (* The parameters change is applied exactly
     [delegate_parameters_activation_delay] after the request *)
  --> wait_delegate_parameters_activation
  (* Not yet... *)
  --> stake "staker" amount
  --> next_cycle
  (* External staking is now forbidden *)
  --> assert_failure (stake "staker" amount)
  (* Can still self-stake *)
  --> stake "delegate" amount
  (* Can still unstake *)
  --> unstake "staker" Half
  --> wait_n_cycles (default_unstake_wait + 1)
  --> finalize_unstake "staker"
  (* Can authorize stake again *)
  --> set_delegate_params "delegate" init_params
  --> wait_delegate_parameters_activation
  (* Not yet... *)
  --> assert_failure (stake "staker" amount)
  --> next_cycle
  (* Now possible *)
  --> stake "staker" amount

let tests =
  tests_of_scenarios
  @@ [
       ("Test simple roundtrip", init_scenario () --> simple_roundtrip);
       ("Test double roundtrip", init_scenario () --> double_roundtrip);
       ("Test preserved balance", init_scenario () --> status_quo_rountrip);
       ("Test finalize", init_scenario () --> scenario_finalize);
       ("Test no finalize", init_scenario () --> scenario_not_finalize);
       ( "Test forbidden operations",
         init_scenario () --> scenario_forbidden_operations );
       ( "Test full balance in finalizable",
         init_scenario () --> full_balance_in_finalizable );
       ("Test stake unstake every cycle", init_scenario () --> odd_behavior);
       ("Test change delegate", change_delegate);
       ("Test unset delegate", unset_delegate);
       ("Test forbid costake", forbid_costaking);
       ("Test stake from unstake", shorter_roundtrip_for_baker);
     ]

let () = register_tests ~__FILE__ ~tags:["protocol"; "scenario"; "stake"] tests
