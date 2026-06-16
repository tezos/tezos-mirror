(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let tests =
  [
    ( "Bindings",
      [
        ("Generate key pair", `Quick, Test_bindings.test_generate_key_pair);
        ("Sign message", `Quick, Test_bindings.test_sign);
        ("Verify signature", `Quick, Test_bindings.test_verify);
        ( "Verify invalid signature",
          `Quick,
          Test_bindings.test_verify_invalid_signature );
        ( "Validate buffer lengths",
          `Quick,
          Test_bindings.test_invalid_buffer_lengths );
      ] );
  ]

let () = Alcotest_lwt.run ~__FILE__ "ML-DSA-44" tests |> Lwt_main.run
