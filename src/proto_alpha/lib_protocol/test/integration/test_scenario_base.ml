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

let tests =
  tests_of_scenarios
  @@ [("Test expected error in assert failure", test_expected_error)]

let () = register_tests ~__FILE__ ~tags:["protocol"; "scenario"; "base"] tests
