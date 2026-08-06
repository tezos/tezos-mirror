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
                  dune exec ./test_run_code_codec.exe
    Subject:      The `tezosx_run_code` IPC codec
                  ([Tezos_backend.Run_code]).

                  The pinned vectors are the ones the kernel pins in
                  [evm_node_entrypoint.rs] (`run_code_codec` tests).
                  Both sides produce the same literal from their own
                  definition, so a drift on either fails a unit test
                  instead of surfacing as a decode error at runtime. *)

open Evm_node_lib_dev.Tezos_backend
module Tezos_types = Evm_node_lib_dev_tezlink.Tezos_types
module Tezlink_imports = Evm_node_lib_dev_tezlink.Tezlink_imports
module Micheline = Tezos_micheline.Micheline

let hex bytes = Hex.show (Hex.of_bytes bytes)

let unhex s = Hex.to_bytes_exn (`Hex s)

(* Field by field: dynamic bytes (4-byte length prefix) for script /
   storage / input, dynamic string for the entrypoint, 4 raw bytes of
   chain id, 22 bytes of contract (`01` + KT1 hash + padding), int64
   amount, then the six options (`00` absent, `ff` + payload present). *)
let full_input_hex =
  "000000010200000001030000000200070000000764656661756c74f3d48b2a011f2d825fdd9da219235510335e558520235f4f54000000000000000007ff0183e44ba40860b4042688621bd56fbed9fa12e9c500ff002422090f872dfd3a39471bb23f180e6dfed030f3ff0000000000000063ff00000000000004d2ff0000000000000005ff0000002a"

let bare_input_hex =
  "000000010200000001030000000200070000000764656661756c74f3d48b2a011f2d825fdd9da219235510335e558520235f4f54000000000000000007000000000000"

(* Variant tag, then the payload: dynamic bytes for the storage, dynamic
   string for an error message. *)
let success_result_hex = "0000000002000c"

let execution_error_result_hex = "0100000010696c6c2d747970656420736372697074"

let host_error_result_hex = "020000001273746f7261676520756e7265616461626c65"

let kt1 = "KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye"

let kt1_sender = "KT1Lc9a9E7vqt6XYtkUbrErDGLQ55HztXV5N"

let tz1 = "tz1Nw5nr152qddEjKT2dKBH8XcBMDAg72iLw"

let contract b58 =
  WithExceptions.Result.get_ok
    ~loc:__LOC__
    (Tezos_types.Contract.of_b58check b58)

let payer = Signature.V3.Public_key_hash.of_b58check_exn tz1

let chain_id = Chain_id.of_bytes_exn (Bytes.of_string "\xf3\xd4\x8b\x2a")

let full_input =
  ( ( Bytes.of_string "\x02",
      Bytes.of_string "\x03",
      (* Micheline `int 7`. *)
      Bytes.of_string "\x00\x07",
      "default",
      chain_id,
      contract kt1,
      7L,
      Some (contract kt1_sender),
      Some payer,
      Some 99L ),
    (Some 1234L, Some 5L, Some 42l) )

let bare_input =
  let (script, storage, input, entrypoint, chain_id, self, amount, _, _, _), _ =
    full_input
  in
  ( ( script,
      storage,
      input,
      entrypoint,
      chain_id,
      self,
      amount,
      None,
      None,
      None ),
    (None, None, None) )

let check_hex ~__LOC__ ~name actual expected =
  Check.(
    (actual = expected)
      string
      ~__LOC__
      ~error_msg:(name ^ ": expected %R, got %L"))

(* The node's production direction: it writes the input the kernel
   reads. *)
let test_input_wire_format () =
  List.iter
    (fun (name, input, expected) ->
      check_hex
        ~__LOC__
        ~name
        (hex (Data_encoding.Binary.to_bytes_exn Run_code.input_encoding input))
        expected)
    [
      ("full input", full_input, full_input_hex);
      ("bare input", bare_input, bare_input_hex);
    ]

(* [input_encoding] alone does not pin which argument [encode_input]
   puts in which slot: three distinct Micheline values, decoded back out
   of the wire, must land in the field they were named for. *)
let test_encode_input_field_placement () =
  let int_expr n = Micheline.strip_locations (Micheline.Int (0, Z.of_int n)) in
  let expr_hex n =
    hex
      (Data_encoding.Binary.to_bytes_exn
         Tezlink_imports.Imported_context.Script.expr_encoding
         (int_expr n))
  in
  let bytes =
    WithExceptions.Result.get_ok
      ~loc:__LOC__
      (Run_code.encode_input
         ~script:(int_expr 1)
         ~storage:(int_expr 2)
         ~input:(int_expr 3)
         ~entrypoint:"default"
         ~chain_id
         ~self:(contract kt1)
         ~amount:0L
         ~sender:None
         ~payer:None
         ~balance:None
         ~gas:None
         ~now:None
         ~level:None)
  in
  let (script, storage, input, _, _, _, _, _, _, _), _ =
    Data_encoding.Binary.of_bytes_exn Run_code.input_encoding bytes
  in
  check_hex ~__LOC__ ~name:"script" (hex script) (expr_hex 1) ;
  check_hex ~__LOC__ ~name:"storage" (hex storage) (expr_hex 2) ;
  check_hex ~__LOC__ ~name:"input" (hex input) (expr_hex 3)

(* The node's other production direction: it reads the result the kernel
   writes, and each tag must reach the error kind that classifies it. *)
let test_success_result () =
  match Run_code.decode_result (unhex success_result_hex) with
  | Ok expr -> (
      match Micheline.root expr with
      | Micheline.Int (_, z) ->
          Check.(
            (Z.to_string z = "12")
              string
              ~__LOC__
              ~error_msg:"expected the storage to decode to %R, got %L")
      | _ -> Test.fail "expected the storage to decode to an integer")
  | Error err -> Test.fail "expected a success, got %a" pp_print_trace err

let test_execution_error_result () =
  match Run_code.decode_result (unhex execution_error_result_hex) with
  | Error [Run_script_view_failed msg] ->
      Check.(
        (msg = "ill-typed script")
          string
          ~__LOC__
          ~error_msg:"expected the message %R, got %L")
  | Error err ->
      Test.fail "expected Run_script_view_failed, got %a" pp_print_trace err
  | Ok _ -> Test.fail "an execution error must not decode as a success"

let test_host_error_result () =
  match Run_code.decode_result (unhex host_error_result_hex) with
  | Error [Run_script_view_host_error msg] ->
      Check.(
        (msg = "storage unreadable")
          string
          ~__LOC__
          ~error_msg:"expected the message %R, got %L")
  | Error err ->
      Test.fail "expected Run_script_view_host_error, got %a" pp_print_trace err
  | Ok _ -> Test.fail "a host error must not decode as a success"

(* A tag this node does not know is a decode failure, not a script
   rejection: a future kernel variant must not be reported as a
   permanent failure of the view. *)
let test_unknown_result_tag () =
  match Run_code.decode_result (unhex "03") with
  | Error [Run_script_view_decode_error _] -> ()
  | Error err ->
      Test.fail
        "expected Run_script_view_decode_error, got %a"
        pp_print_trace
        err
  | Ok _ -> Test.fail "an unknown tag must not decode"

let tests =
  [
    ( "run_code_codec",
      [
        ("input wire format", `Quick, test_input_wire_format);
        ( "encode_input field placement",
          `Quick,
          test_encode_input_field_placement );
        ("success result", `Quick, test_success_result);
        ("execution error result", `Quick, test_execution_error_result);
        ("host error result", `Quick, test_host_error_result);
        ("unknown result tag", `Quick, test_unknown_result_tag);
      ] );
  ]

let () = Alcotest.run ~__FILE__ "run_code_codec" tests

let () = Test.run ()
