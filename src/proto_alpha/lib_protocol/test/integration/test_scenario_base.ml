(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Scenario, State
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/integration/main.exe \
                   -- --file test_scenario_base.ml
    Subject:      Test basic functionality of the scenario framework.
*)

open Adaptive_issuance_helpers
open State_account
open Tez_helpers.Ez_tez
open Scenario

let test_expected_error =
  assert_failure
    ~expected_error:(fun _ -> [Exn (Failure "")])
    (exec (fun _ -> failwith ""))
  --> assert_failure
        ~expected_error:(fun _ -> [Unexpected_error])
        (assert_failure
           ~expected_error:(fun _ ->
             [Inconsistent_number_of_bootstrap_accounts])
           (exec (fun _ -> failwith "")))

(** Initialization of scenarios with 3 cases:
     - AI activated, staker = delegate
     - AI activated, staker != delegate
     - AI not activated (and staker = delegate)
    Any scenario that begins with this will be triplicated.
 *)
let init_scenario ?(force_ai = true) ?reward_per_block () =
  let init_params =
    {limit_of_staking_over_baking = Q.one; edge_of_baking_over_staking = Q.one}
  in
  let begin_test ~activate_ai ~self_stake =
    let name = if self_stake then "staker" else "delegate" in
    init_constants ?reward_per_block ()
    --> set S.Adaptive_issuance.autostaking_enable false
    --> Scenario_begin.activate_ai (if activate_ai then `Force else `No)
    --> begin_test [name]
    --> set_delegate_params name init_params
    --> set_baker "__bootstrap__"
  in
  let ai_activated =
    Tag "AI activated"
    --> (Tag "self stake" --> begin_test ~activate_ai:true ~self_stake:true
        |+ Tag "external stake"
           --> begin_test ~activate_ai:true ~self_stake:false
           --> add_account_with_funds
                 "staker"
                 ~funder:"delegate"
                 (Amount (Tez.of_mutez 2_000_000_000_000L))
           --> set_delegate "staker" (Some "delegate"))
    --> wait_delegate_parameters_activation --> next_cycle
  in

  let ai_deactivated =
    Tag "AI deactivated, self stake"
    --> begin_test ~activate_ai:false ~self_stake:true
  in
  (if force_ai then ai_activated else ai_activated |+ ai_deactivated)
  --> next_block

let tests =
  tests_of_scenarios
  @@ [
       ("Test expected error in assert failure", test_expected_error);
       ("Test init", init_scenario () --> Action (fun _ -> return_unit));
     ]

let () = register_tests ~__FILE__ ~tags:["protocol"; "scenario"; "base"] tests
