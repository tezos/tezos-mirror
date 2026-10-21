(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Bin_evm_node
    Invocation:   cd etherlink/bin_node/test/ ;
                  dune exec ./test_compute_alias_codec.exe
    Subject:      The `tezosx_compute_alias` IPC codec
                  ([Tezosx.Compute_alias]).

                  The pinned vectors are the ones the kernel pins in
                  [evm_node_entrypoint.rs] (`compute_alias_tests`), so a
                  drift on either side fails a unit test instead of
                  surfacing as a decode error at runtime. *)

open Evm_node_lib_dev.Tezosx

let hex bytes = Hex.show (Hex.of_bytes bytes)

let unhex s = Hex.to_bytes_exn (`Hex s)

let tz1 = "tz1aygVpocoeEMU89vXk92sMHqPYFNZVgvrh"

let tz1_alias = "0x04e9ba988ba87bbd7b75e5aa813c52ca3c31e9ae"

(* Target runtime tag (`01` = Ethereum), then the address as a dynamic
   string. *)
let input_hex =
  "0100000024747a3161796756706f636f65454d55383976586b3932734d48715059464e5a5667767268"

(* Variant tag, then a dynamic string. *)
let success_result_hex =
  "000000002a307830346539626139383862613837626264376237356535616138313363353263613363333165396165"

let error_result_hex = "010000000962616420696e707574"

let test_input_wire_format () =
  Check.(
    (hex (Compute_alias.encode_input ~target:`Ethereum ~native_address:tz1)
    = input_hex)
      string
      ~error_msg:"expected input %R, got %L")

let test_success_result () =
  match Compute_alias.decode_result (unhex success_result_hex) with
  | Ok alias ->
      Check.((alias = tz1_alias) string ~error_msg:"expected alias %R, got %L")
  | Error err -> Test.fail "expected an alias, got %a" pp_print_trace err

let test_error_result () =
  match Compute_alias.decode_result (unhex error_result_hex) with
  | Error err ->
      let msg = Format.asprintf "%a" pp_print_trace err in
      Check.(
        (msg =~ rex "bad input")
          ~error_msg:"expected the kernel error message, got %L")
  | Ok alias -> Test.fail "expected an error, got the alias %s" alias

let test_unknown_result_tag () =
  match Compute_alias.decode_result (unhex "07") with
  | Error _ -> ()
  | Ok _ -> Test.fail "an unknown tag must not decode"

let tests =
  [
    ( "compute_alias_codec",
      [
        ("input wire format", `Quick, test_input_wire_format);
        ("success result", `Quick, test_success_result);
        ("error result", `Quick, test_error_result);
        ("unknown result tag", `Quick, test_unknown_result_tag);
      ] );
  ]

let () = Alcotest.run ~__FILE__ "compute_alias_codec" tests

let () = Test.run ()
