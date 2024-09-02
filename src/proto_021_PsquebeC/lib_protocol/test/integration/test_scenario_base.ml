(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Scenario, State
    Invocation:   dune exec src/proto_021_PsquebeC/lib_protocol/test/integration/main.exe \
                   -- --file test_scenario_base.ml
    Subject:      Test basic functionality of the scenario framework.
*)

open Scenario

let test_expected_error =
  assert_failure
    ~expected_error:(fun _ errs ->
      Error_helpers.expect_failwith
        ~loc:__LOC__
        ~str:(Str.regexp_string "")
        errs)
    (exec (fun _ -> failwith ""))
  --> assert_failure
        ~expected_error:(fun _ errs ->
          Error_helpers.expect_failwith
            ~str:(Str.regexp ".*expected a specific error.*")
            ~loc:__LOC__
            errs)
        (assert_failure
           ~expected_error:(fun _ errs ->
             Error_helpers.check_error_constructor_name
               ~loc:__LOC__
               ~expected:
                 Protocol.Apply
                 .Staking_to_delegate_that_refuses_external_staking
               errs)
           (exec (fun _ -> failwith "")))

let tests =
  tests_of_scenarios
  @@ [("Test expected error in assert failure", test_expected_error)]

let () = register_tests ~__FILE__ ~tags:["protocol"; "scenario"; "base"] tests
