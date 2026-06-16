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

let test_nds_host_functions_signal_roundtrip () =
  let open Lwt_result_syntax in
  let name = Wasm_pvm_config.signal_name_of_feature Nds_host_functions in
  assert (Wasm_pvm_config.feature_of_signal_name name = Some Nds_host_functions) ;
  return_unit

let test_nds_host_functions_activation () =
  let open Lwt_result_syntax in
  let activation = 10l in
  let config =
    Wasm_pvm_config.of_signals
      [(Wasm_pvm_config.signal_name_of_feature Nds_host_functions, activation)]
  in
  assert (
    Wasm_pvm_config.activation_level config Nds_host_functions = Some activation) ;
  (* Not yet active at or before the activation level. *)
  assert (
    not
      (Wasm_pvm_config.is_enabled
         config
         Nds_host_functions
         ~current_level:activation)) ;
  (* Active at strictly higher levels. *)
  assert (
    Wasm_pvm_config.is_enabled
      config
      Nds_host_functions
      ~current_level:(Int32.succ activation)) ;
  return_unit

let tests =
  [
    Tztest.tztest "of_signals on empty list" `Quick test_of_signals_empty;
    Tztest.tztest
      "of_signals drops unknown signal names"
      `Quick
      test_of_signals_drops_unknown;
    Tztest.tztest
      "nds_host_functions signal name roundtrips"
      `Quick
      test_nds_host_functions_signal_roundtrip;
    Tztest.tztest
      "nds_host_functions activation gating"
      `Quick
      test_nds_host_functions_activation;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ "test lib scoru wasm" [("Wasm_pvm_config", tests)]
  |> Lwt_main.run
