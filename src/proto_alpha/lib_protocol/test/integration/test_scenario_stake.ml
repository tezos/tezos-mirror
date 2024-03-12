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

let fs = Format.asprintf

(** Initializes of scenarios with 2 cases:
     - staker = delegate
     - staker != delegate
    Any scenario that begins with this will be duplicated.

    Also, ensures that AI is activated (sets EMA threshold to zero,
    enables activation vote, and waits for AI activation). *)
let init_staker_delegate_or_external =
  let init_params =
    {limit_of_staking_over_baking = Q.one; edge_of_baking_over_staking = Q.one}
  in
  let begin_test ~self_stake =
    let name = if self_stake then "staker" else "delegate" in
    init_constants ()
    --> set S.Adaptive_issuance.autostaking_enable false
    --> Scenario_begin.activate_ai `Force
    --> begin_test [name]
    --> set_delegate_params name init_params
    --> set_baker "__bootstrap__"
  in
  Tag "AI activated"
  --> (Tag "self stake" --> begin_test ~self_stake:true
      |+ Tag "external stake"
         --> begin_test ~self_stake:false
         --> add_account_with_funds
               "staker"
               ~funder:"delegate"
               (Amount (Tez.of_mutez 2_000_000_000_000L))
         --> set_delegate "staker" (Some "delegate"))
  --> wait_delegate_parameters_activation --> next_cycle

let stake_init =
  stake "staker" Half
  --> (Tag "no wait after stake" --> Empty
      |+ Tag "wait after stake" --> wait_n_cycles 2)

let ( -- ) a b x = a x - b

let ( ++ ) a b x = a x + b

let wait_for_unfreeze_and_check wait =
  snapshot_balances "wait snap" ["staker"]
  --> wait_n_cycles_f (wait -- 1)
  (* Balance didn't change yet, but will change next cycle *)
  --> check_snapshot_balances "wait snap"
  --> next_cycle
  --> assert_failure (check_snapshot_balances "wait snap")

let unstake_wait (_, state) =
  let crd = state.State.constants.consensus_rights_delay in
  let msp = Protocol.Constants_repr.max_slashing_period in
  crd + msp

let finalize staker =
  assert_failure (check_balance_field staker `Unstaked_finalizable Tez.zero)
  --> finalize_unstake staker
  --> check_balance_field staker `Unstaked_finalizable Tez.zero

(* Simple stake - unstake - finalize roundtrip.

   - Note that the test framework automatically checks, whenever a
   block is baked, that the staker's fully detailed balance (liquid,
   bonds, staked, unstaked frozen, unstaked finalizable, and costaking
   values) is the same as predicted in the simulated state.

   - Moreover, we explicitly check that after the unstake operation,
   the staker's balance doesn't change until the last cycle of the
   unfreeze delay (which is [consensus_rights_delay +
   max_slashing_period = 2 + 2]). *)
let simple_roundtrip =
  init_staker_delegate_or_external --> stake_init
  --> (Tag "full unstake" --> unstake "staker" All
      |+ Tag "half unstake" --> unstake "staker" Half)
  --> wait_for_unfreeze_and_check unstake_wait
  --> finalize "staker" --> next_cycle

(* Same as above, except with two separate unstake operations. *)
let double_roundtrip =
  init_staker_delegate_or_external --> stake_init --> unstake "staker" Half
  --> (Tag "half then full unstake" --> wait_n_cycles 2 --> unstake "staker" All
      |+ Tag "half then half unstake" --> wait_n_cycles 2
         --> unstake "staker" Half)
  --> wait_for_unfreeze_and_check (unstake_wait -- 2)
  --> wait_for_unfreeze_and_check (Fun.const 2)
  --> finalize "staker" --> next_cycle

(* Test that a baker can stake from unstaked frozen funds. *)
let shorter_roundtrip_for_baker =
  let amount = Amount (Tez.of_mutez 333_000_000_000L) in
  let consensus_rights_delay =
    Default_parameters.constants_test.consensus_rights_delay
  in
  init_constants ()
  --> set S.Adaptive_issuance.autostaking_enable false
  --> activate_ai `Force --> begin_test ["delegate"]
  --> stake "delegate" (Amount (Tez.of_mutez 1_800_000_000_000L))
  --> next_cycle
  --> snapshot_balances "init" ["delegate"]
  --> unstake "delegate" amount
  --> (* Wait [n] cycles where [0 <= n <= consensus_rights_delay + 1]. *)
  List.fold_left
    (fun acc i -> acc |+ Tag (fs "wait %i cycles" i) --> wait_n_cycles i)
    (Tag "wait 0 cycles" --> Empty)
    (Stdlib.List.init (consensus_rights_delay + 1) (fun i -> i + 1))
  --> stake "delegate" amount
  --> check_snapshot_balances "init"

(* Roundtrip where the unstaked amount matches the initially staked
   amount (either from one unstake operation of this amount, or two
   unstake operations summing up to it). This lets us explicitly check
   that the detailed balance of the staker at the end is identical to
   its balance before the stake operation. *)
let status_quo_rountrip =
  let full_amount = Tez.of_mutez 10_000_000L in
  let amount_1 = Tez.of_mutez 2_999_999L in
  let amount_2 = Tez.of_mutez 7_000_001L in
  init_staker_delegate_or_external
  --> snapshot_balances "init" ["staker"]
  --> stake "staker" (Amount full_amount)
  --> next_cycle
  --> (Tag "1 unstake" --> unstake "staker" (Amount full_amount)
      |+ Tag "2 unstakes"
         --> unstake "staker" (Amount amount_1)
         --> next_cycle
         --> unstake "staker" (Amount amount_2))
  --> wait_n_cycles_f unstake_wait
  --> finalize "staker"
  --> check_snapshot_balances "init"

(* Test three different ways to finalize unstake requests:
   - finalize_unstake operation
   - stake operation (of 1 mutez)
   - unstake operation (of 1 mutez)

   Check that the finalizable unstaked balance is non-zero before, and
   becomes zero after the finalization. *)
let scenario_finalize =
  init_staker_delegate_or_external --> stake "staker" Half --> next_cycle
  --> unstake "staker" Half
  --> wait_n_cycles_f (unstake_wait ++ 2)
  --> assert_failure
        (check_balance_field "staker" `Unstaked_finalizable Tez.zero)
  --> (Tag "finalize with finalize" --> finalize_unstake "staker"
      |+ Tag "finalize with stake" --> stake "staker" (Amount Tez.one_mutez)
      |+ Tag "finalize with unstake" --> unstake "staker" (Amount Tez.one_mutez)
      )
  --> check_balance_field "staker" `Unstaked_finalizable Tez.zero

(* Test that an unstake operation doesn't cause finalization when
   there are zero staked funds (so the unstake operation doesn't do
   anything). *)
(* Todo: there might be other cases... like changing delegates *)
let scenario_not_finalize =
  init_staker_delegate_or_external --> stake "staker" Half --> next_cycle
  --> unstake "staker" All
  --> wait_n_cycles_f (unstake_wait ++ 2)
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
  init_staker_delegate_or_external
  --> (* Staking everything works for self delegates, but not for delegated accounts *)
  assert_failure
    (fail_if_staker_is_self_delegate "staker" --> stake "staker" All)
  (* stake is always forbidden when amount is zero *)
  --> assert_failure (stake "staker" Nothing)
  (* One cannot stake more that one has *)
  --> assert_failure (stake "staker" Max_tez)
  (* unstake is actually authorized for amount 0, but does nothing (doesn't even finalize if possible) *)
  --> unstake "staker" Nothing

let full_balance_in_finalizable =
  init_staker_delegate_or_external
  --> add_account_with_funds
        "dummy"
        ~funder:"staker"
        (Amount (Tez.of_mutez 10_000_000L))
  --> stake "staker" All_but_one --> next_cycle --> unstake "staker" All
  --> wait_n_cycles_f (unstake_wait ++ 2)
  (* At this point, almost all the balance (but one mutez) of the stake is in finalizable *)
  (* Staking is possible, but not transfer *)
  --> assert_failure
        (transfer "staker" "dummy" (Amount (Tez.of_mutez 10_000_000L)))
  --> stake "staker" (Amount (Tez.of_mutez 10_000_000L))
  (* After the stake, transfer is possible again because the funds were finalized *)
  --> transfer "staker" "dummy" (Amount (Tez.of_mutez 10_000_000L))

(* Stress test: what happens if someone were to stake and unstake every cycle? *)
let odd_behavior =
  init_staker_delegate_or_external
  -->
  let one_cycle =
    no_tag --> stake "staker" Half --> unstake "staker" Half --> next_cycle
  in
  loop 20 one_cycle

let change_delegate =
  let init_params =
    {limit_of_staking_over_baking = Q.one; edge_of_baking_over_staking = Q.one}
  in
  init_constants ()
  --> set S.Adaptive_issuance.autostaking_enable false
  --> activate_ai `Force
  --> begin_test ["delegate1"; "delegate2"]
  --> set_delegate_params "delegate1" init_params
  --> set_delegate_params "delegate2" init_params
  --> add_account_with_funds
        "staker"
        ~funder:"delegate1"
        (Amount (Tez.of_mutez 2_000_000_000_000L))
  --> set_delegate "staker" (Some "delegate1")
  --> wait_delegate_parameters_activation --> next_cycle --> stake "staker" Half
  --> next_cycle
  --> set_delegate "staker" (Some "delegate2")
  --> next_cycle
  --> assert_failure (stake "staker" Half)
  --> wait_n_cycles_f (unstake_wait ++ 1)
  --> stake "staker" Half

let unset_delegate =
  let init_params =
    {limit_of_staking_over_baking = Q.one; edge_of_baking_over_staking = Q.one}
  in
  init_constants ()
  --> set S.Adaptive_issuance.autostaking_enable false
  --> activate_ai `Force --> begin_test ["delegate"]
  --> set_delegate_params "delegate" init_params
  --> add_account_with_funds
        "staker"
        ~funder:"delegate"
        (Amount (Tez.of_mutez 2_000_000_000_000L))
  --> add_account_with_funds
        "dummy"
        ~funder:"delegate"
        (Amount (Tez.of_mutez 2_000_000L))
  --> set_delegate "staker" (Some "delegate")
  --> wait_delegate_parameters_activation --> next_cycle --> stake "staker" Half
  --> unstake "staker" All --> next_cycle --> set_delegate "staker" None
  --> next_cycle
  --> transfer "staker" "dummy" All
  (* staker has an empty liquid balance, but still has unstaked frozen tokens,
     so it doesn't get deactivated *)
  --> wait_n_cycles_f (unstake_wait ++ 1)
  --> finalize_unstake "staker"

(* Test that external stakers cannot stake when a delegate sets the
   limit of staking over baking to zero, then can stake again when the
   limit is set back to one. Changes take effect only after
   [delegate_parameters_activation_delay + 1] cycles. *)
let forbid_costaking =
  let init_params =
    {limit_of_staking_over_baking = Q.one; edge_of_baking_over_staking = Q.one}
  in
  let no_costake_params =
    {limit_of_staking_over_baking = Q.zero; edge_of_baking_over_staking = Q.one}
  in
  let amount = Amount (Tez.of_mutez 1_000_000L) in
  (* init constants *)
  (Tag "default protocol constants" --> init_constants ()
  |+ Tag "small delegate parameters delay"
     --> init_constants ~delegate_parameters_activation_delay:0 ()
  |+ Tag "large delegate parameters delay"
     --> init_constants ~delegate_parameters_activation_delay:10 ())
  (* Set flags *)
  --> set S.Adaptive_issuance.autostaking_enable false
  --> activate_ai `Zero_threshold
  (* Start scenario *)
  --> begin_test ["delegate"]
  --> set_delegate_params "delegate" init_params
  --> add_account_with_funds
        "staker"
        ~funder:"delegate"
        (Amount (Tez.of_mutez 2_000_000_000_000L))
  --> set_delegate "staker" (Some "delegate")
  --> wait_cycle_until (`And (`AI_activation, `delegate_parameters_activation))
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
  --> wait_n_cycles_f (unstake_wait ++ 1)
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
       ("Test simple roundtrip", simple_roundtrip);
       ("Test double roundtrip", double_roundtrip);
       ("Test preserved balance", status_quo_rountrip);
       ("Test finalize", scenario_finalize);
       ("Test no finalize", scenario_not_finalize);
       ("Test forbidden operations", scenario_forbidden_operations);
       ("Test full balance in finalizable", full_balance_in_finalizable);
       ("Test stake unstake every cycle", odd_behavior);
       ("Test change delegate", change_delegate);
       ("Test unset delegate", unset_delegate);
       ("Test forbid costake", forbid_costaking);
       ("Test stake from unstake", shorter_roundtrip_for_baker);
     ]

let () = register_tests ~__FILE__ ~tags:["protocol"; "scenario"; "stake"] tests
