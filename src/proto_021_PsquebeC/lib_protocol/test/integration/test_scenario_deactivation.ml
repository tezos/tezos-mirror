(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Protocol, Consensus, Deactivation
    Invocation:   dune exec src/proto_021_PsquebeC/lib_protocol/test/integration/main.exe \
                   -- --file test_scenario_deactivation.ml
    Subject:      Test deactivation in the protocol.
*)

open State_account
open Tez_helpers.Ez_tez
open Scenario

let check_is_active ~loc src_name =
  let open Lwt_result_syntax in
  exec_unit @@ fun (block, state) ->
  let src = State.find_account src_name state in
  let* b = Context.Delegate.deactivated (B block) src.pkh in
  Assert.is_true ~loc (not b)

let check_is_not_active ~loc src_name =
  let open Lwt_result_syntax in
  exec_unit @@ fun (block, state) ->
  let src = State.find_account src_name state in
  let* b = Context.Delegate.deactivated (B block) src.pkh in
  Assert.is_true ~loc b

let check_grace_period ~loc src_name =
  let open Lwt_result_syntax in
  exec_unit @@ fun (block, state) ->
  let src = State.find_account src_name state in
  let grace =
    Cycle.add
      src.last_active_cycle
      (state.State.constants.consensus_rights_delay + 1)
  in
  let* rpc_grace = Context.Delegate.grace_period (B block) src.pkh in
  Assert.equal
    ( = )
    "Grace period is not correct: expected vs RPC"
    ~loc
    Cycle.pp
    grace
    rpc_grace

(** Test that a delegate gets deactivated after a set period of time if it is not baking.
    Test that with autostaking, the frozen funds are completely unstaked, which get
    finalizable (but not finalized) after a set period of time.
    Test that these finalizable funds can indeed be finalized. *)
let test_simple_scenario_with_autostaking =
  init_constants ()
  --> set S.Adaptive_issuance.autostaking_enable true
  --> activate_ai `No
  --> begin_test ["delegate"; "baker"]
  --> check_grace_period ~loc:__LOC__ "baker"
  --> set_baker "baker"
  --> wait_n_cycles_f (fun (_, state) ->
          (2 * state.State.constants.consensus_rights_delay) + 1)
  --> check_balance_field "delegate" `Staked (Tez.of_mutez 200_000_000_000L)
  --> check_is_active ~loc:__LOC__ "delegate"
  --> next_cycle
  --> check_is_not_active ~loc:__LOC__ "delegate"
  --> check_balance_field "delegate" `Staked Tez.zero
  --> check_grace_period ~loc:__LOC__ "baker"
  --> next_block
  --> check_grace_period ~loc:__LOC__ "baker"
  --> check_balance_field
        "delegate"
        `Unstaked_frozen_total
        (Tez.of_mutez 200_000_000_000L)
  --> wait_n_cycles_f Test_scenario_stake.unstake_wait
  --> check_balance_field "delegate" `Unstaked_frozen_total Tez.zero
  --> check_grace_period ~loc:__LOC__ "delegate"
  --> check_grace_period ~loc:__LOC__ "baker"
  --> check_balance_field
        "delegate"
        `Unstaked_finalizable
        (Tez.of_mutez 200_000_000_000L)
  --> (Tag "Reactivate"
       --> set_delegate "delegate" (Some "delegate")
       --> check_is_active ~loc:__LOC__ "delegate"
       --> next_cycle
       --> check_balance_field "delegate" `Unstaked_finalizable Tez.zero
       --> check_balance_field
             "delegate"
             `Staked
             (Tez.of_mutez 200_000_000_000L)
      |+ Tag "manual finalize unstake"
         --> finalize_unstake "delegate"
         --> check_balance_field "delegate" `Unstaked_finalizable Tez.zero
         --> check_balance_field "delegate" `Staked Tez.zero
         --> check_balance_field
               "delegate"
               `Liquid
               (Tez.of_mutez 4_000_000_000_000L))

(** Test that a delegate gets deactivated after a set period of time if it is not baking.
    Test that with AI, the frozen funds stay frozen, and the delegate can still issue AI
    operations without reactivating. *)
let test_simple_scenario_with_ai =
  init_constants () --> activate_ai `Force
  --> begin_test ["delegate"; "baker"]
  --> check_balance_field "delegate" `Staked (Tez.of_mutez 200_000_000_000L)
  --> set_baker "baker"
  --> wait_n_cycles_f (fun (_, state) ->
          (2 * state.State.constants.consensus_rights_delay) + 1)
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
  init_constants () --> activate_ai `Force
  --> begin_test ["delegate"; "baker"]
  --> unstake "delegate" All
  --> wait_n_cycles_f (fun (_, state) ->
          (2 * state.State.constants.consensus_rights_delay) + 1)
  --> check_is_active ~loc:__LOC__ "delegate"
  --> next_cycle
  --> check_is_not_active ~loc:__LOC__ "delegate"
  (* Reactivate and wait for rights *)
  --> stake "delegate" Half
  --> set_delegate "delegate" (Some "delegate")
  (* No rights yet, but active *)
  --> assert_failure
        ~expected_error:(fun (_, state) errs ->
          let delegate = State.find_account "delegate" state in
          Error_helpers.expect_no_slots_found_for
            ~loc:__LOC__
            ~pkh:delegate.pkh
            errs)
        (next_block_with_baker "delegate")
  --> check_is_active ~loc:__LOC__ "delegate"
  --> wait_n_cycles_f (fun (_, state) ->
          state.State.constants.consensus_rights_delay + 1)
  --> check_is_active ~loc:__LOC__ "delegate"
  --> next_block_with_baker "delegate"
  (* Get deactivated by doing nothing *)
  --> set_baker "baker"
  --> wait_n_cycles_f (fun (_, state) ->
          state.State.constants.consensus_rights_delay + 1)
  --> check_is_active ~loc:__LOC__ "delegate"
  --> next_cycle
  --> check_is_not_active ~loc:__LOC__ "delegate"
  (* The delegate still has enough rights to bake... *)
  --> exec_unit (fun (block, state) ->
          let dlgt = State.find_account "delegate" state in
          let current_cycle = Block.current_cycle block in
          let rights =
            CycleMap.find current_cycle dlgt.frozen_rights
            |> Option.value ~default:Tez.zero
          in
          Assert.not_equal_tez ~loc:__LOC__ Tez.zero rights)
  --> next_block_with_baker "delegate"
  --> check_is_active ~loc:__LOC__ "delegate"

let tests =
  tests_of_scenarios
    [
      ( "Test simple deactivation scenario with autostaking",
        test_simple_scenario_with_autostaking );
      ("Test simple deactivation scenario with ai", test_simple_scenario_with_ai);
      ( "Test deactivation and reactivation scenarios with baking",
        test_baking_deactivation );
    ]

let () =
  register_tests ~__FILE__ ~tags:["protocol"; "scenario"; "deactivation"] tests
