(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Lib_scoru_wasm Wasm_pvm_config
    Invocation:   dune exec src/lib_scoru_wasm/test/main.exe -- \
                  --file test_wasm_pvm_config.ml
    Subject:      Cover the typed feature-config module.
*)

open Tezos_scoru_wasm

let test_of_signals_empty () =
  let open Lwt_result_syntax in
  assert (
    Wasm_pvm_config.equal (Wasm_pvm_config.of_signals []) Wasm_pvm_config.empty) ;
  return_unit

let test_of_signals_drops_unknown () =
  let open Lwt_result_syntax in
  let config =
    Wasm_pvm_config.of_signals [("any_signal", 1l); ("another", 42l)]
  in
  assert (Wasm_pvm_config.equal config Wasm_pvm_config.empty) ;
  return_unit

let tests =
  [
    Tztest.tztest "of_signals on empty list" `Quick test_of_signals_empty;
    Tztest.tztest
      "of_signals drops unknown signal names"
      `Quick
      test_of_signals_drops_unknown;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ "test lib scoru wasm" [("Wasm_pvm_config", tests)]
  |> Lwt_main.run
