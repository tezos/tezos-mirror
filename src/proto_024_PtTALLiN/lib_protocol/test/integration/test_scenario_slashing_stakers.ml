(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Adaptive Issuance, Slashing with Stakers
    Invocation:   dune exec src/proto_024_PtTALLiN/lib_protocol/test/integration/main.exe \
                   -- --file test_scenario_slashing_stakers.ml
    Subject:      Test slashing scenarios in the protocol with stakers.
*)

open Adaptive_issuance_helpers
open State_account
open Tez_helpers.Ez_tez
open Scenario

let fs = Format.asprintf

let slashed_staker_1 = "staker1"

let slashed_staker_2 = "staker2"

let never_slashed_staker = "staker3"

let first_slashed_delegate = "delegate"

let second_slashed_delegate = "bootstrap1"

let never_slashed_delegate1 = "bootstrap2"

let never_slashed_delegate2 = "bootstrap3"

(** Setup starting state for test with
   - 4 delegates
   - potentialy three stakers (2 delegating to first_slashed_delegate and 1 to
     never_slashed_delegate1 respectively)
   - alternative parameters for first_slashed_delegate and
     never_slashed_delegate1
*)
let init_with_stakers () =
  let init_params l e =
    {limit_of_staking_over_baking = l; edge_of_baking_over_staking = e}
  in
  (* Same edge for both delegates to avoid test branches explosion *)
  let set_delegate_params dlgt1 dlgt2 =
    (Tag "edge 1"
     --> set_delegate_params dlgt1 (init_params Q.one Q.one)
     --> set_delegate_params dlgt2 (init_params Q.one Q.one)
    |+ Tag "edge 1/3"
       --> set_delegate_params dlgt1 (init_params Q.one Q.(1 // 3))
       --> set_delegate_params dlgt2 (init_params Q.one Q.(1 // 3)))
    --> wait_n_cycles 4
  in
  let add_staker name delegate amount staked_amount =
    add_account_with_funds name ~funder:delegate (Amount (Tez.of_mutez amount))
    --> set_delegate name (Some delegate)
    --> stake name staked_amount
  in
  let add_stakers =
    Tag "with stakers"
    --> add_staker
          slashed_staker_1
          first_slashed_delegate
          1_000_000_000_000L
          Half
    --> add_staker slashed_staker_2 first_slashed_delegate 3_333_333L Half
    --> add_staker
          never_slashed_staker
          never_slashed_delegate1
          1_000_000_000L
          Half
    |+ Empty
  in
  init_constants ()
  --> begin_test
        ~force_attest_all:true
        [
          first_slashed_delegate;
          second_slashed_delegate;
          never_slashed_delegate1;
          never_slashed_delegate2;
        ]
  --> set_delegate_params first_slashed_delegate never_slashed_delegate1
  --> add_stakers

(** Starts with four delegates, misbhehaves with one, denounce it, and
  potentially do it again with another delegate.
  Alternative scenarios include:
  - all three misbehaviors
  - delegates with/without stakers to observe sharing of slashing and rewards, 
  - denunciations be made in the same cycle, in the next or too late
  - several staking parameters
 *)
let test_simple_slash =
  let any_slash delegate =
    Tag "double baking" --> double_bake delegate
    |+ Tag "double attesting"
       --> double_attest
             ~other_bakers:(never_slashed_delegate1, never_slashed_delegate2)
             delegate
    |+ Tag "double preattesting"
       --> double_preattest
             ~other_bakers:(never_slashed_delegate1, never_slashed_delegate2)
             delegate
  in
  init_with_stakers ()
  --> any_slash first_slashed_delegate
  --> log "make denunciations"
  --> snapshot_balances "before slash" [first_slashed_delegate]
  --> (Tag "denouncer with maybe staker" --> set_baker never_slashed_delegate1
       (* ensure denunciation is included by never_slashed_delegate1 *)
      |+ Tag "denouncer without staker" --> set_baker never_slashed_delegate2)
  --> ((Tag "denounce same cycle" --> make_denunciations () --> next_cycle
        (* delegate can be forbidden in this case, so we exclude it from list of potential bakers *)
        --> exclude_bakers [first_slashed_delegate]
       |+ Tag "denounce next cycle" --> next_cycle --> make_denunciations ()
          (* delegate can be forbidden in this case, so we set another baker *)
          --> exclude_bakers [first_slashed_delegate])
       --> next_block
       -->
       (* only exclude "delegate" *)
       exclude_bakers [first_slashed_delegate]
       --> (Empty
           |+ Tag "another slash"
              --> any_slash second_slashed_delegate
              --> make_denunciations ()
              (* bootstrap1 can be forbidden in this case, so we exclude it from list of potential bakers *)
              --> exclude_bakers
                    [first_slashed_delegate; second_slashed_delegate])
       (* Whether the denunciation was in the same cycle or the next
          cycle, we are in the next cycle after the misbehavior, so
          the slashing has not been applied yet. *)
       --> check_snapshot_balances "before slash"
       --> exec_unit (check_pending_slashings ~loc:__LOC__)
       --> next_cycle
       (* The slashing has been applied. *)
       --> check_snapshot_balances
             ~f:
               (assert_balance_evolution
                  ~loc:__LOC__
                  ~for_accounts:[first_slashed_delegate]
                  ~part:`staked
                  Q.lt)
             "before slash"
       --> exec_unit (check_pending_slashings ~loc:__LOC__)
       --> next_cycle
      |+ Tag "denounce too late" --> next_cycle --> next_cycle
         --> assert_failure
               ~expected_error:(fun (_block, state) errs ->
                 Error_helpers.expect_outdated_denunciation_state
                   ~loc:__LOC__
                   ~state
                   errs)
               (make_denunciations ())
         --> check_snapshot_balances "before slash")

let tests = tests_of_scenarios @@ [("Test simple slashing", test_simple_slash)]

let () =
  register_tests
    ~__FILE__
    ~tags:["protocol"; "scenario"; "slashing"; "stakers"]
    tests
