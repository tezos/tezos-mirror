(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Protocol, Consensus, Deactivation
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/integration/main.exe \
                   -- --file test_scenario_deactivation.ml
    Subject:      Test deactivation in the protocol.
*)

open State_account
open Tez_helpers.Ez_tez
open Scenario

let check_rights_aux ~loc ~expect_rights src_name =
  exec_unit @@ fun (block, state) ->
  let src = State.find_account src_name state in
  let current_cycle = Block.current_cycle block in
  let rights =
    CycleMap.find current_cycle src.frozen_rights
    |> Option.value ~default:Tez.zero
  in
  if expect_rights then Assert.not_equal_tez ~loc Tez.zero rights
  else Assert.equal_tez ~loc Tez.zero rights

let check_no_rights = check_rights_aux ~expect_rights:false

let check_has_rights = check_rights_aux ~expect_rights:true

let check_cannot_bake_next_block ~loc src_name =
  assert_failure
    ~expected_error:(fun (_, state) errs ->
      let src = State.find_account src_name state in
      Error_helpers.expect_no_slots_found_for ~loc ~pkh:src.pkh errs)
    (next_block_with_baker src_name)

let check_can_bake_next_block ~loc src_name =
  assert_success ~loc (next_block_with_baker src_name)

let check_grace_period ~loc src_name =
  let open Lwt_result_syntax in
  exec_unit @@ fun (block, state) ->
  let src = State.find_account src_name state in
  let last_seen_activity = Stdlib.Option.get src.last_seen_activity in
  let grace =
    Cycle.add
      last_seen_activity
      state.State.constants.tolerated_inactivity_period
  in
  let* rpc_grace = Context.Delegate.grace_period (B block) src.pkh in
  Assert.equal
    ( = )
    "Grace period is not correct: expected vs RPC"
    ~loc
    Cycle.pp
    grace
    rpc_grace

(** Test that a delegate gets deactivated after a set period of time
    if it is not baking.

    Test that the frozen funds stay frozen, and the delegate can still
    issue staking operations without reactivating. *)
let test_simple_scenario =
  init_constants ()
  --> begin_test ["delegate"; "baker"]
  --> check_balance_field "delegate" `Staked (Tez.of_mutez 200_000_000_000L)
  --> set_baker "baker"
  --> wait_n_cycles_f (fun (_, state) ->
          state.State.constants.consensus_rights_delay
          + state.State.constants.tolerated_inactivity_period)
  --> check_balance_field "delegate" `Staked (Tez.of_mutez 200_000_000_000L)
  --> check_is_active ~loc:__LOC__ "delegate"
  --> next_cycle
  --> check_is_not_active ~loc:__LOC__ "delegate"
  --> check_balance_field "delegate" `Staked (Tez.of_mutez 200_000_000_000L)
  --> check_balance_field "delegate" `Unstaked_frozen_total Tez.zero
  --> unstake "delegate" All
  --> wait_n_cycles_f Test_scenario_stake.unstake_wait
  --> finalize_unstake "delegate"
  --> check_balance_field "delegate" `Unstaked_finalizable Tez.zero
  --> check_balance_field "delegate" `Staked Tez.zero
  --> check_balance_field "delegate" `Liquid (Tez.of_mutez 4_000_000_000_000L)
  --> check_is_not_active ~loc:__LOC__ "delegate"
  --> stake "delegate" Half --> next_cycle
  --> check_is_not_active ~loc:__LOC__ "delegate"

(** Test that a delegate can be deactivated by setting its frozen funds to 0.
    Test that a delegate can be activated while having no rights.
    Test that a delegate can be deactivated while having rights, and test that it can
    still bake while deactivated, hence reactivating *)
let test_baking_deactivation =
  init_constants ()
  --> begin_test ["delegate"; "baker"]
  --> unstake "delegate" All
  --> wait_n_cycles_f (fun (_, state) ->
          state.State.constants.consensus_rights_delay
          + state.State.constants.tolerated_inactivity_period)
  --> check_is_active ~loc:__LOC__ "delegate"
  --> next_cycle
  --> check_is_not_active ~loc:__LOC__ "delegate"
  (* Reactivate and wait for rights *)
  --> stake "delegate" Half
  --> (Tag "Reactivate in next block" --> Empty
      |+ Tag "Reactivate in next cycle" --> exec bake_until_dawn_of_next_cycle
      |+ Tag "Reactivate in last block"
         --> exec bake_until_next_cycle_end_but_one)
  --> set_delegate "delegate" (Some "delegate")
  (* No rights yet, but active *)
  --> check_cannot_bake_next_block ~loc:__LOC__ "delegate"
  --> check_is_active ~loc:__LOC__ "delegate"
  --> wait_n_cycles_f (fun (_, state) ->
          state.State.constants.consensus_rights_delay + 1)
  --> check_is_active ~loc:__LOC__ "delegate"
  --> next_block_with_baker "delegate"
  (* Get deactivated by doing nothing *)
  --> set_baker "baker"
  --> wait_n_cycles_f (fun (_, state) ->
          state.State.constants.tolerated_inactivity_period)
  --> check_is_active ~loc:__LOC__ "delegate"
  --> next_cycle
  --> check_is_not_active ~loc:__LOC__ "delegate"
  (* The delegate still has enough rights to bake... *)
  --> check_has_rights ~loc:__LOC__ "delegate"
  --> next_block_with_baker "delegate"
  --> check_is_active ~loc:__LOC__ "delegate"

let test_deactivation_timing =
  let staked_balance_no_change =
    check_balance_field "delegate" `Staked (Tez.of_mutez 200_000_000_000L)
  in
  init_constants ()
  --> begin_test ["delegate"; "baker"]
  --> staked_balance_no_change
  --> (Tag "Delegate is never active" --> set_baker "baker"
       --> wait_n_cycles_f (fun (_, state) ->
               state.State.constants.consensus_rights_delay
               + state.State.constants.tolerated_inactivity_period)
      |+ Tag "Delegate is active for a few cycles" --> set_baker "delegate"
         --> wait_n_cycles_f (fun (_, state) ->
                 state.State.constants.consensus_rights_delay + 1)
         --> set_baker "baker"
         --> wait_n_cycles_f (fun (_, state) ->
                 state.State.constants.tolerated_inactivity_period))
  --> exec bake_until_next_cycle_end_but_one
  --> check_is_active ~loc:__LOC__ "delegate"
  --> staked_balance_no_change --> next_block
  (* delegate is deactivated at the end of the cycle but it has rights *)
  --> check_is_not_active ~loc:__LOC__ "delegate"
  --> check_has_rights ~loc:__LOC__ "delegate"
  --> staked_balance_no_change
  (* delegate is removed from the committee after
     [consensus_rights_delay] cycles *)
  --> wait_n_cycles_f (fun (_, state) ->
          state.State.constants.consensus_rights_delay)
  --> check_is_not_active ~loc:__LOC__ "delegate"
  --> check_has_rights ~loc:__LOC__ "delegate"
  --> check_can_bake_next_block ~loc:__LOC__ "delegate"
  --> next_cycle
  --> check_cannot_bake_next_block ~loc:__LOC__ "delegate"
  (* delegate is still deactivated but it also has no rights *)
  --> check_is_not_active ~loc:__LOC__ "delegate"
  --> check_no_rights ~loc:__LOC__ "delegate"
  --> staked_balance_no_change
  (* reactivate and wait for rights *)
  --> (Tag "Reactivate in next block" --> Empty
      |+ Tag "Reactivate in next cycle" --> exec bake_until_dawn_of_next_cycle
      |+ Tag "Reactivate in last block"
         --> exec bake_until_next_cycle_end_but_one)
  --> set_delegate "delegate" (Some "delegate")
  (* delegate has no rights yet, but it is active *)
  --> check_is_active ~loc:__LOC__ "delegate"
  --> check_no_rights ~loc:__LOC__ "delegate"
  --> staked_balance_no_change
  --> check_cannot_bake_next_block ~loc:__LOC__ "delegate"
  --> wait_n_cycles_f (fun (_, state) ->
          state.State.constants.consensus_rights_delay + 1)
  (* delegate has rights and it is active *)
  --> check_is_active ~loc:__LOC__ "delegate"
  --> check_has_rights ~loc:__LOC__ "delegate"
  --> staked_balance_no_change
  --> next_block_with_baker "delegate"

let tests =
  tests_of_scenarios
    [
      ("Test simple deactivation scenario", test_simple_scenario);
      ( "Test deactivation and reactivation scenarios with baking",
        test_baking_deactivation );
      ("Test deactivation timing", test_deactivation_timing);
    ]

let () =
  register_tests ~__FILE__ ~tags:["protocol"; "scenario"; "deactivation"] tests
