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
        ( "Jstz: Proofs produced via the OCaml API match those produced by the \
           sandboxed RISC-V PVM",
          `Quick,
          Test_backend.test_proof_regression Utils.Jstz );
        ( "Jstz: Proofs are immutable",
          `Quick,
          Test_backend.test_proof_immutability Utils.Jstz );
        ( "Etherlink: Proofs produced via the OCaml API match those produced \
           by the sandboxed RISC-V PVM",
          `Quick,
          Test_backend.test_proof_regression Utils.Etherlink );
        ( "Etherlink: Proofs are immutable",
          `Quick,
          Test_backend.test_proof_immutability Utils.Etherlink );
        ( "Echo: Output proof serialisation roundtrip",
          `Quick,
          Test_outbox.test_output_proof_serialisation_round_trip );
        ( "Echo: Produce and verify output proofs",
          `Quick,
          Test_outbox.test_produce_output_proofs );
        ( "Echo: Produce proof fails for invalid message index",
          `Quick,
          Test_outbox.test_produce_proof_invalid_message_index );
        ( "Echo: Produce proof fails for invalid outbox level",
          `Quick,
          Test_outbox.test_produce_proof_invalid_outbox_level );
        ( "Echo: Produce proof fails on empty outbox",
          `Quick,
          Test_outbox.test_produce_proof_empty_outbox );
        ( "Echo: Deserialise invalid bytes fails",
          `Quick,
          Test_outbox.test_deserialise_invalid_bytes );
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
        ("Output list type", `Quick, Test_rust_bindings.test_output_list);
      ] );
  ]

let () = Alcotest_lwt.run ~__FILE__ "RISC-V PVM" tests |> Lwt_main.run
