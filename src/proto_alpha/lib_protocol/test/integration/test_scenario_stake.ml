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

let fail_to_stake_refuse_external_staking ~loc staker ~amount =
  assert_failure
    ~expected_error:(fun _ errs ->
      Error_helpers.check_error_constructor_name
        ~loc
        ~expected:
          Protocol.Apply.Staking_to_delegate_that_refuses_external_staking
        errs)
    (stake staker amount)

let fail_to_stake_with_unfinalizable_unstake_requests ~loc staker ~amount =
  assert_failure
    ~expected_error:(fun _ errs ->
      Error_helpers.check_error_constructor_name
        ~loc
        ~expected:
          Protocol.Staking
          .Cannot_stake_with_unfinalizable_unstake_requests_to_another_delegate
        errs)
    (stake staker amount)

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
    --> Scenario_begin.activate_ai `Force
    --> begin_test [name]
    --> set_delegate_params name init_params
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
  --> wait_delegate_parameters_activation

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
  --> assert_failure_in_check_snapshot_balances ~loc:__LOC__ "wait snap"

let unstake_wait (_, state) =
  let crd = state.State.constants.consensus_rights_delay in
  let msp = Protocol.Constants_repr.max_slashing_period in
  crd + msp

let finalize staker =
  assert_failure_in_check_balance_field
    ~loc:__LOC__
    staker
    `Unstaked_finalizable
    Tez.zero
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

(* Test that a baker can stake from unstaked frozen funds.
   The most recent unstakes are prioritized when staking. *)
let shorter_roundtrip_for_baker =
  let unstake_amount = Amount (Tez.of_mutez 222_000_000_000L) in
  let consensus_rights_delay =
    Default_parameters.constants_mainnet.consensus_rights_delay
    (* mainnet value, = 2 *)
  in
  let init_params =
    {limit_of_staking_over_baking = Q.one; edge_of_baking_over_staking = Q.one}
  in
  init_constants ()
  --> set S.consensus_rights_delay consensus_rights_delay
  --> activate_ai `Force
  --> begin_test ["delegate"; "faucet"]
  --> stake "delegate" (Amount (Tez.of_mutez 1_800_000_000_000L))
  --> set_delegate_params "delegate" init_params
  --> add_account_with_funds
        "staker1"
        ~funder:"faucet"
        (Amount (Tez.of_mutez 200_000_000_000L))
  --> add_account_with_funds
        "staker2"
        ~funder:"faucet"
        (Amount (Tez.of_mutez 200_000_000_000L))
  --> wait_delegate_parameters_activation
  --> set_delegate "staker1" (Some "delegate")
  --> set_delegate "staker2" (Some "delegate")
  --> stake "staker1" Half --> stake "staker2" Half
  -->
  (* From now on, staker1 unstakes every cycle to fill all the containers, but
     this shouldn't change anything for the baker *)
  let next_cycle = unstake "staker1" Half --> next_cycle in
  next_cycle
  (* We unstake to have an amount in the last container for ufd *)
  --> unstake "delegate" unstake_amount
  --> next_cycle
  (* We unstake either one, two or three cycles later *)
  --> (Tag "unstake cycle (current-2)"
       --> unstake "delegate" unstake_amount
       --> next_cycle --> next_cycle
      |+ Tag "unstake cycle (current-1)" --> next_cycle
         --> unstake "delegate" unstake_amount
         --> next_cycle
      |+ Tag "unstake cycle (current)" --> next_cycle --> next_cycle
         --> unstake "delegate" unstake_amount)
  (* Nothing is finalizable yet. If nothing else happens, next cycle the
     first unstake request will become finalizable. *)
  --> check_balance_field "delegate" `Unstaked_finalizable Tez.zero
  --> (Tag "stake from unstake one container"
       --> (Tag "one stake"
            --> stake "delegate" (Amount (Tez.of_mutez 111_000_000_000L))
           |+ Tag "two stakes"
              --> stake "delegate" (Amount (Tez.of_mutez 100_000_000_000L))
              --> stake "delegate" (Amount (Tez.of_mutez 11_000_000_000L)))
       --> check_balance_field
             "delegate"
             `Unstaked_frozen_total
             (Tez.of_mutez 333_000_000_000L)
       (* We only removed unstake from the most recent unstake: we should
          expect the first unstake request to finalize with its full amount on the next cycle *)
       --> next_cycle
       --> check_balance_field
             "delegate"
             `Unstaked_finalizable
             (Tez.of_mutez 222_000_000_000L)
       --> check_balance_field
             "delegate"
             `Unstaked_frozen_total
             (Tez.of_mutez 111_000_000_000L)
      |+ Tag "stake from unstake two containers"
         --> stake "delegate" (Amount (Tez.of_mutez 333_000_000_000L))
         --> check_balance_field
               "delegate"
               `Unstaked_frozen_total
               (Tez.of_mutez 111_000_000_000L)
         (* We should have removed all the unstake from the most recent container.
            The rest will be finalizable next cycle. *)
         --> next_cycle
         --> check_balance_field
               "delegate"
               `Unstaked_finalizable
               (Tez.of_mutez 111_000_000_000L)
         --> check_balance_field "delegate" `Unstaked_frozen_total Tez.zero
      |+ Tag "stake from all unstaked + liquid"
         --> stake "delegate" (Amount (Tez.of_mutez 555_000_000_000L))
         (* Nothing remains unstaked *)
         --> check_balance_field "delegate" `Unstaked_frozen_total Tez.zero
         --> check_balance_field "delegate" `Unstaked_finalizable Tez.zero)

(* Test three different ways to finalize unstake requests:
   - finalize_unstake operation
   - stake operation (of 1 mutez)
   - unstake operation (of 1 mutez)

   Check that the finalizable unstaked balance is non-zero before, and
   becomes zero after the finalization. *)
let scenario_finalize =
  init_staker_delegate_or_external --> stake "staker" Half --> next_cycle
  --> unstake "staker" Half
  --> wait_n_cycles_f unstake_wait
  --> (Tag "minimal wait after unstake" --> Empty
      |+ Tag "wait longer after unstake" --> wait_n_cycles 2)
  --> assert_failure_in_check_balance_field
        ~loc:__LOC__
        "staker"
        `Unstaked_finalizable
        Tez.zero
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
  --> assert_failure_in_check_balance_field
        ~loc:__LOC__
        "staker"
        `Unstaked_finalizable
        Tez.zero
  --> snapshot_balances "not finalize" ["staker"]
  --> (Tag "no finalize with unstake if staked = 0"
      --> unstake "staker" (Amount Tez.one_mutez))
  --> assert_failure_in_check_balance_field
        ~loc:__LOC__
        "staker"
        `Unstaked_finalizable
        Tez.zero
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
    ~expected_error:(fun (_, state) errs ->
      if State.(is_self_delegate "staker" state) then
        Error_helpers.expect_failwith
          ~loc:__LOC__
          ~str:(Str.regexp_string "_self_delegate_exit_")
          errs
      else
        let staker = State.find_account "staker" state in
        Error_helpers.expect_empty_implicit_delegated_contract
          ~loc:__LOC__
          ~contract:staker.contract
          errs)
    (fail_if_staker_is_self_delegate "staker" --> stake "staker" All)
  (* stake is always forbidden when amount is zero *)
  --> assert_failure
        ~expected_error:(fun (_, state) errs ->
          let staker = State.find_account "staker" state in
          Error_helpers.expect_empty_transaction
            ~loc:__LOC__
            ~contract:staker.contract
            errs)
        (stake "staker" Nothing)
  (* One cannot stake more that one has *)
  --> assert_failure
        ~expected_error:(fun _ errs ->
          Error_helpers.expect_balance_too_low ~loc:__LOC__ errs)
        (stake "staker" Max_tez)
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
        ~expected_error:(fun _ errs ->
          Error_helpers.expect_balance_too_low ~loc:__LOC__ errs)
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

(* Test changing delegate to self delegation while having staked funds. *)
let change_delegate_to_self =
  let init_params =
    {limit_of_staking_over_baking = Q.one; edge_of_baking_over_staking = Q.one}
  in
  init_constants () --> activate_ai `Force --> begin_test ["delegate"]
  --> set_delegate_params "delegate" init_params
  --> add_account_with_funds
        "staker"
        ~funder:"delegate"
        (Amount (Tez.of_mutez 2_000_000_000_000L))
  --> set_delegate "staker" (Some "delegate")
  --> wait_delegate_parameters_activation --> stake "staker" Half --> next_cycle
  --> set_delegate "staker" (Some "staker")
  (* Can't stake: "A contract tries to stake to its delegate while
     having unstake requests to a previous delegate that cannot be
     finalized yet. Try again in a later cycle (no more than
     consensus_rights_delay + max_slashing_period)." *)
  --> fail_to_stake_with_unfinalizable_unstake_requests
        ~loc:__LOC__
        "staker"
        ~amount:Half
  --> unstake "staker" Max_tez
  --> wait_n_cycles_f (unstake_wait -- 1) (* Still can't stake. *)
  --> check_balance_field "staker" `Unstaked_finalizable Tez.zero
  --> fail_to_stake_with_unfinalizable_unstake_requests
        ~loc:__LOC__
        "staker"
        ~amount:Half
  --> next_cycle
  (* The unstake request from changing delegates is now finalizable. *)
  --> assert_failure_in_check_balance_field
        ~loc:__LOC__
        "staker"
        `Unstaked_finalizable
        Tez.zero
  --> assert_success
        (* Can directly stake again, which automatically finalizes,
           even though the finalizable unstaked request is about a
           previous delegate. *)
        (stake "staker" Half
        --> check_balance_field "staker" `Unstaked_finalizable Tez.zero)
  --> (Tag "finalize"
       --> (* Explicitly finalize, so that we can check that the balances
              are identical to the beginning. This proves that changing
              delegates has indeed unstaked all staked funds. *)
       finalize "staker"
       --> check_snapshot_balances "init"
       --> check_balance_field "staker" `Unstaked_finalizable Tez.zero
      |+ Tag "don't finalize" --> Empty)
  --> stake "staker" Half --> unstake "staker" Half --> stake "staker" Half

(* Test changing delegates while having staked funds. *)
let change_delegate =
  let init_params =
    {limit_of_staking_over_baking = Q.one; edge_of_baking_over_staking = Q.one}
  in
  init_constants () --> activate_ai `Force
  --> begin_test ["delegate1"; "delegate2"]
  --> set_delegate_params "delegate1" init_params
  --> set_delegate_params "delegate2" init_params
  --> add_account_with_funds
        "staker"
        ~funder:"delegate1"
        (Amount (Tez.of_mutez 2_000_000_000_000L))
  --> snapshot_balances "init" ["staker"]
  --> set_delegate "staker" (Some "delegate1")
  --> wait_delegate_parameters_activation --> stake "staker" Half
  --> snapshot_balances "after_stake" ["staker"]
  (* Changing delegates. This also unstakes all staked funds. *)
  --> set_delegate "staker" (Some "delegate2")
  (* Can't stake: "A contract tries to stake to its delegate while
     having unstake requests to a previous delegate that cannot be
     finalized yet. Try again in a later cycle (no more than
     consensus_rights_delay + max_slashing_period)." *)
  --> fail_to_stake_with_unfinalizable_unstake_requests
        ~loc:__LOC__
        "staker"
        ~amount:Half
  --> unstake "staker" Max_tez
  --> wait_n_cycles_f (unstake_wait -- 1) (* Still can't stake. *)
  --> check_balance_field "staker" `Unstaked_finalizable Tez.zero
  --> fail_to_stake_with_unfinalizable_unstake_requests
        ~loc:__LOC__
        "staker"
        ~amount:Half
  --> next_cycle
  (* The unstake request from changing delegates is now finalizable. *)
  --> assert_failure_in_check_balance_field
        ~loc:__LOC__
        "staker"
        `Unstaked_finalizable
        Tez.zero
  --> assert_success
        (* Can directly stake again, which automatically finalizes,
           even though the finalizable unstaked request is about a
           previous delegate. *)
        (stake "staker" Half
        --> check_balance_field "staker" `Unstaked_finalizable Tez.zero)
  --> (Tag "finalize"
       --> (* Explicitly finalize, so that we can check that the balances
              are identical to the beginning. This proves that changing
              delegates has indeed unstaked all staked funds. *)
       finalize "staker"
       --> check_snapshot_balances "init"
       --> check_balance_field "staker" `Unstaked_finalizable Tez.zero
       --> (* Staking again is also possible. *) stake "staker" Half
       --> check_snapshot_balances "after_stake"
      |+ Tag "don't finalize" --> stake "staker" Half)
  --> (Tag "finally, unstake" --> unstake "staker" Half
      |+ Tag "finally, change delegate one last time"
         --> set_delegate "staker" (Some "delegate1")
      |+ Tag "finally, unset delegate" --> set_delegate "staker" None)

let unset_delegate =
  let init_params =
    {limit_of_staking_over_baking = Q.one; edge_of_baking_over_staking = Q.one}
  in
  init_constants () --> activate_ai `Force --> begin_test ["delegate"]
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
  --> wait_delegate_parameters_activation --> stake "staker" Half
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
  (* try stake in normal conditions *)
  --> stake "staker" amount
  (* Change delegate parameters to forbid staking *)
  --> set_delegate_params "delegate" no_costake_params
  (* The changes are not immediate *)
  --> stake "staker" amount
  (* The parameters change is applied after at least
     [delegate_parameters_activation_delay] full cycles have passed,
     that is, exactly [delegate_parameters_activation_delay + 1]
     cycles after the request. *)
  --> wait_cycle_until `right_before_delegate_parameters_activation
  (* Not yet... *)
  --> stake "staker" amount
  --> next_cycle
  (* External staking is now forbidden *)
  --> fail_to_stake_refuse_external_staking ~loc:__LOC__ "staker" ~amount
  (* Can still self-stake *)
  --> stake "delegate" amount
  (* Can still unstake *)
  --> unstake "staker" Half
  --> wait_n_cycles_f (unstake_wait ++ 1)
  --> finalize_unstake "staker"
  (* Can authorize stake again *)
  --> set_delegate_params "delegate" init_params
  --> wait_cycle_until `right_before_delegate_parameters_activation
  (* Not yet... *)
  --> fail_to_stake_refuse_external_staking ~loc:__LOC__ "staker" ~amount
  --> next_cycle
  (* Now possible *)
  --> stake "staker" amount

(* Check that a delegate can be deactivated under AI by unstaking everything, even with stakers.
   Check that such a delegate can reactivate later, and still have their stakers *)
let test_deactivation =
  let init_params =
    {limit_of_staking_over_baking = Q.one; edge_of_baking_over_staking = Q.one}
  in
  let check_deactivated_status ~loc ~expected delegate =
    let open Lwt_result_syntax in
    exec_unit (fun (block, state) ->
        let dlgt = State.find_account delegate state in
        let* deactivated = Context.Delegate.deactivated (B block) dlgt.pkh in
        Assert.equal_bool ~loc deactivated expected)
  in
  let check_is_deactivated = check_deactivated_status ~expected:true in
  let check_is_not_deactivated = check_deactivated_status ~expected:false in
  init_constants () --> activate_ai `Force
  --> begin_test ["delegate"; "faucet"]
  --> stake "delegate" (Amount (Tez.of_mutez 1_800_000_000_000L))
  --> set_delegate_params "delegate" init_params
  --> add_account_with_funds
        "staker1"
        ~funder:"faucet"
        (Amount (Tez.of_mutez 200_000_000_000L))
  --> add_account_with_funds
        "staker2"
        ~funder:"faucet"
        (Amount (Tez.of_mutez 200_000_000_000L))
  --> wait_delegate_parameters_activation
  --> set_delegate "staker1" (Some "delegate")
  --> set_delegate "staker2" (Some "delegate")
  --> stake "staker1" Half --> stake "staker2" Half --> next_cycle
  (* The delegate unstakes all, starting the deactivation process *)
  --> unstake "delegate" All
  (* "delegate" can still bake, but not for long... *)
  --> assert_success ~loc:__LOC__ (next_block_with_baker "delegate")
  --> wait_n_cycles_f (fun (_, state) ->
          state.State.constants.consensus_rights_delay)
  (* After consensus_rights_delay, the delegate still has rights... *)
  --> assert_success ~loc:__LOC__ (next_block_with_baker "delegate")
  --> next_cycle
  (* ...But not in the following cycle *)
  --> assert_failure
        ~expected_error:(fun (_, state) errs ->
          let delegate = State.find_account "delegate" state in
          Error_helpers.expect_no_slots_found_for
            ~loc:__LOC__
            ~pkh:delegate.pkh
            errs)
        (next_block_with_baker "delegate")
  (* The stakers still have stake, and can still stake/unstake *)
  --> check_balance_field "staker1" `Staked (Tez.of_mutez 100_000_000_000L)
  --> check_balance_field "staker2" `Staked (Tez.of_mutez 100_000_000_000L)
  --> assert_success ~loc:__LOC__ (stake "staker1" Half)
  --> assert_success
        ~loc:__LOC__
        (unstake "staker2" Half --> wait_n_cycles 5
       --> finalize_unstake "staker2")
  (* We wait until the delegate is completely deactivated *)
  --> check_is_not_deactivated ~loc:__LOC__ "delegate"
  (* We already waited for [consensus_rights_delay] + 1 cycles since 0 stake,
     we must wait for [tolerated_inactivity_period - 1] more. *)
  --> wait_n_cycles_f (fun (_, state) ->
          state.State.constants.tolerated_inactivity_period - 1)
  --> check_is_not_deactivated ~loc:__LOC__ "delegate"
  --> next_cycle
  --> check_is_deactivated ~loc:__LOC__ "delegate"
  --> next_cycle
  (* The stakers still have stake, and can still stake/unstake *)
  --> check_balance_field "staker1" `Staked (Tez.of_mutez 100_000_000_000L)
  --> check_balance_field "staker2" `Staked (Tez.of_mutez 100_000_000_000L)
  --> assert_success ~loc:__LOC__ (stake "staker1" Half)
  --> assert_success
        ~loc:__LOC__
        (unstake "staker2" Half --> wait_n_cycles 5
       --> finalize_unstake "staker2")
  --> next_cycle
  (* We now reactivate the delegate *)
  --> set_delegate "delegate" (Some "delegate")
  --> stake "delegate" (Amount (Tez.of_mutez 2_000_000_000_000L))
  (* It cannot bake right away *)
  --> assert_failure
        ~expected_error:(fun (_, state) errs ->
          let delegate = State.find_account "delegate" state in
          Error_helpers.expect_no_slots_found_for
            ~loc:__LOC__
            ~pkh:delegate.pkh
            errs)
        (next_block_with_baker "delegate")
  --> wait_n_cycles_f (fun (_, state) ->
          state.State.constants.consensus_rights_delay)
  (* After consensus_rights_delay, the delegate still has no rights... *)
  --> assert_failure
        ~expected_error:(fun (_, state) errs ->
          let delegate = State.find_account "delegate" state in
          Error_helpers.expect_no_slots_found_for
            ~loc:__LOC__
            ~pkh:delegate.pkh
            errs)
        (next_block_with_baker "delegate")
  --> next_cycle
  (* ...But has enough to bake in the following cycle *)
  --> assert_success ~loc:__LOC__ (next_block_with_baker "delegate")
  --> exec_unit (fun (_, state) ->
          let dlgt = State.find_account "delegate" state in
          let total = Frozen_tez.total_current dlgt.frozen_deposits in
          Assert.equal_tez ~loc:__LOC__ total (Tez.of_mutez 2_200_000_000_000L))

let change_delegate_and_wait_then_stake ~loc ~staker ~delegate ~amount ~wait =
  let check_is_last_level_of_cycle ~loc =
    exec_unit (fun (block, _state) ->
        Assert.equal_bool ~loc (Block.last_block_of_cycle block) true)
  in
  let amount = Amount (Tez.of_mutez amount) in
  set_delegate staker (Some delegate)
  --> fail_to_stake_with_unfinalizable_unstake_requests ~loc staker ~amount
  --> wait_n_cycles_f (unstake_wait -- 1)
  --> exec bake_until_next_cycle_end_but_one
  --> fail_to_stake_with_unfinalizable_unstake_requests ~loc staker ~amount
  --> next_block --> stake staker amount --> wait_n_cycles wait
  (* Create unstake request for cycle N *)
  --> unstake staker Half
  (* Bake until right before the end of the cycle *)
  --> exec bake_until_next_cycle_end_but_one
  (* Modify unstake request for cycle N, [unstake] also bakes a block,
     so we are now at the end of the cycle. *)
  --> unstake staker Half
  --> check_is_last_level_of_cycle ~loc:__LOC__
  --> stake staker amount
  (* Create unstake request for cycle N + 1 *)
  --> unstake staker Half

let test_change_delegates =
  let set_limit name l =
    let params =
      {
        limit_of_staking_over_baking = Q.of_float l;
        edge_of_baking_over_staking = Q.zero;
      }
    in
    set_delegate_params name params
  in
  init_constants () --> activate_ai `Force
  --> begin_test ["delegate1"; "delegate2"; "delegate3"; "faucet"]
  --> add_account_with_funds
        "staker"
        ~funder:"faucet"
        (Amount (Tez.of_mutez 400_000_000_000L))
  --> set_limit "delegate1" 5. --> set_limit "delegate2" 4.
  --> set_limit "delegate3" 3. --> wait_delegate_parameters_activation
  --> set_delegate "staker" (Some "delegate1")
  --> stake "staker" (Amount (Tez.of_mutez 10_000_000_000L))
  --> wait_n_cycles 2
  --> change_delegate_and_wait_then_stake
        ~loc:__LOC__
        ~staker:"staker"
        ~delegate:"delegate2"
        ~amount:10_000_000_000L
        ~wait:3
  --> change_delegate_and_wait_then_stake
        ~loc:__LOC__
        ~staker:"staker"
        ~delegate:"delegate3"
        ~amount:10_000_000_000L
        ~wait:4
  --> change_delegate_and_wait_then_stake
        ~loc:__LOC__
        ~staker:"staker"
        ~delegate:"delegate1"
        ~amount:15_000_000_000L
        ~wait:1
  --> change_delegate_and_wait_then_stake
        ~loc:__LOC__
        ~staker:"staker"
        ~delegate:"staker"
        ~amount:10_000_000_000L
        ~wait:2
  --> wait_n_cycles 2

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
       ("Test change delegate to self", change_delegate_to_self);
       ("Test unset delegate", unset_delegate);
       ("Test forbid costake", forbid_costaking);
       ("Test stake from unstake", shorter_roundtrip_for_baker);
       ("Test deactivation under AI", test_deactivation);
       ("Test change delegates", test_change_delegates);
     ]

let () = register_tests ~__FILE__ ~tags:["protocol"; "scenario"; "stake"] tests
