(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023-2025 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2024-2025 TriliTech <contact@trili.tech>                    *)
(*                                                                           *)
(*****************************************************************************)

let tests =
  [
    ( "Backend",
      [
        ( "PVM advances the expected number of steps",
          `Quick,
          Test_backend.test_advance_dummy_kernel );
        ( "Proofs produced via the OCaml API match those produced by the \
           sandboxed RISC-V PVM",
          `Quick,
          Test_backend.test_jstz_proof_regression );
        ("Proofs are immutable", `Quick, Test_backend.test_proof_immutability);
      ] );
    ( "Storage",
      [
        ("Simple test", `Quick, Test_storage.test_simple);
        ("Simple state test", `Quick, Test_storage.test_state_simple);
        Tztest.tztest "Export snapshot" `Quick Test_storage.test_export_snapshot;
      ] );
    ( "Rust Bindings",
      [
        ("Status type", `Quick, Test_rust_bindings.test_status);
        ("Input type", `Quick, Test_rust_bindings.test_input);
        ("Input Request type", `Quick, Test_rust_bindings.test_input_request);
        ("Output Info type", `Quick, Test_rust_bindings.test_output_info);
        ("Output type", `Quick, Test_rust_bindings.test_output);
      ] );
  ]

let () = Alcotest_lwt.run ~__FILE__ "RISC-V PVM" tests |> Lwt_main.run
