(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023-2025 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

let tests =
  [
    ( "Backend",
      [
        ( "PVM advances the expected number of steps",
          `Quick,
          Test_backend.test_advance_dummy_kernel );
      ] );
    ( "Storage",
      [
        ("Simple test", `Quick, Test_storage.test_simple);
        ("Simple state test", `Quick, Test_storage.test_state_simple);
        Tztest.tztest "Export snapshot" `Quick Test_storage.test_export_snapshot;
      ] );
  ]

let () = Alcotest_lwt.run ~__FILE__ "RISC-V PVM" tests |> Lwt_main.run
