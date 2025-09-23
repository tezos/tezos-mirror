(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Protocol ((pre)attestations)
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/integration/consensus/main.exe \
                  -- --file test_all_bakers_attest.ml

    Subject:      Test all bakers attest feature and activation.
*)

open Scenario

let tests = tests_of_scenarios @@ []

let () =
  register_tests
    ~__FILE__
    ~tags:["protocol"; "scenario"; "consensus"; "all_bakers_attest"]
    tests
