(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Bin_evm_node
    Invocation:   dune exec etherlink/bin_node/test/test_compact_receipt_encoding.exe
    Subject:      Verify that the legacy on-disk encoding for transaction
                  receipts (used when [experimental_features.compact_receipt_encoding]
                  is [false]) produces bytes that decode through the legacy
                  decoder used by older node releases, while the compact
                  decoder still accepts both formats. This is the rollback-
                  safety contract of the feature flag.
*)

open Evm_node_lib_dev_encoding
open Ethereum_types

(* A non-zero bloom built by setting one byte to 0xff. *)
let some_bloom = Hex (String.make 511 '0' ^ "f")

let zero_bloom = Transaction_info.zero_logs_bloom

let some_address = Address (Hex (String.make 40 '1'))

let some_hash = Hash (Hex (String.make 64 '2'))

let some_block_hash = Block_hash (Hex (String.make 64 '3'))

let some_log : transaction_log =
  {
    address = some_address;
    topics = [some_hash];
    data = Hex "deadbeef";
    blockNumber = Some (Qty (Z.of_int 42));
    transactionHash = Some some_hash;
    transactionIndex = Some (Qty Z.zero);
    blockHash = Some some_block_hash;
    logIndex = Some (Qty Z.zero);
    removed = Some false;
  }

let make_receipt ~logs_bloom ~logs : Transaction_info.receipt_fields =
  {
    cumulative_gas_used = Qty (Z.of_int 21000);
    effective_gas_price = Qty (Z.of_int 1_000_000_000);
    gas_used = Qty (Z.of_int 21000);
    logs;
    logs_bloom;
    type_ = Qty Z.zero;
    status = Qty Z.one;
    contract_address = None;
  }

let hex_typ : hex Check.typ = Check.(convert hex_to_string string)

let receipt_typ : Transaction_info.receipt_fields Check.typ =
  Check.equalable
    (fun fmt rf ->
      Format.fprintf
        fmt
        "{cumulative_gas_used = %a; gas_used = %a; logs = %d; bloom = %s; \
         status = %a}"
        Z.pp_print
        (let (Qty x) = rf.Transaction_info.cumulative_gas_used in
         x)
        Z.pp_print
        (let (Qty x) = rf.gas_used in
         x)
        (List.length rf.logs)
        (let (Hex h) = rf.logs_bloom in
         h)
        Z.pp_print
        (let (Qty x) = rf.status in
         x))
    ( = )

let encode encoding value = Data_encoding.Binary.to_string_exn encoding value

let decode encoding bytes = Data_encoding.Binary.of_string encoding bytes

let decode_exn encoding bytes =
  match decode encoding bytes with
  | Ok v -> v
  | Error err ->
      Test.fail "Decoder failure: %a" Data_encoding.Binary.pp_read_error err

(* Round-trip a receipt through [encoding] and verify the decoded value
   equals the original. *)
let check_round_trip ~name encoding receipt =
  let bytes = encode encoding receipt in
  let decoded = decode_exn encoding bytes in
  Check.((decoded = receipt) receipt_typ)
    ~error_msg:(name ^ ": round-trip mismatch (got %L, expected %R)")

(* The rollback contract: a value written by [legacy_receipt_fields_encoding]
   must decode through the *same* legacy encoding (which is what older node
   binaries use) and *also* through the new compact-aware decoder. *)
let test_legacy_round_trip () =
  let receipt = make_receipt ~logs_bloom:zero_bloom ~logs:[some_log] in
  check_round_trip
    ~name:"legacy/legacy (zero bloom)"
    Transaction_info.legacy_receipt_fields_encoding
    receipt ;
  let receipt = make_receipt ~logs_bloom:some_bloom ~logs:[some_log] in
  check_round_trip
    ~name:"legacy/legacy (non-zero bloom)"
    Transaction_info.legacy_receipt_fields_encoding
    receipt ;
  let receipt = make_receipt ~logs_bloom:zero_bloom ~logs:[] in
  check_round_trip
    ~name:"legacy/legacy (no logs)"
    Transaction_info.legacy_receipt_fields_encoding
    receipt

let test_compact_round_trip () =
  let receipt = make_receipt ~logs_bloom:zero_bloom ~logs:[some_log] in
  check_round_trip
    ~name:"compact/compact (zero bloom)"
    Transaction_info.receipt_fields_encoding
    receipt ;
  let receipt = make_receipt ~logs_bloom:some_bloom ~logs:[some_log] in
  check_round_trip
    ~name:"compact/compact (non-zero bloom)"
    Transaction_info.receipt_fields_encoding
    receipt

(* The forward-compatibility contract: the new node (compact-aware decoder)
   must read rows produced by an old node (legacy encoder). *)
let test_legacy_decodes_through_compact () =
  let receipts =
    [
      make_receipt ~logs_bloom:zero_bloom ~logs:[some_log];
      make_receipt ~logs_bloom:some_bloom ~logs:[some_log];
      make_receipt ~logs_bloom:zero_bloom ~logs:[];
    ]
  in
  List.iter
    (fun receipt ->
      let legacy_bytes =
        encode Transaction_info.legacy_receipt_fields_encoding receipt
      in
      let decoded =
        decode_exn Transaction_info.receipt_fields_encoding legacy_bytes
      in
      Check.((decoded = receipt) receipt_typ)
        ~error_msg:
          "Legacy bytes should decode through the compact-aware decoder (got \
           %L, expected %R)")
    receipts

(* Wire-format invariant: for an all-zero bloom, the legacy encoding embeds
   the full 514-byte hex string while the compact encoding embeds the empty
   string. The legacy serialization is therefore strictly larger — guarding
   against accidentally collapsing the two encodings into one. *)
let test_wire_format_diverges_on_zero_bloom () =
  let receipt = make_receipt ~logs_bloom:zero_bloom ~logs:[] in
  let compact_bytes = encode Transaction_info.receipt_fields_encoding receipt in
  let legacy_bytes =
    encode Transaction_info.legacy_receipt_fields_encoding receipt
  in
  Check.(
    (String.length legacy_bytes > String.length compact_bytes)
      int
      ~error_msg:
        ("Legacy zero-bloom bytes should be larger than compact (legacy="
        ^ string_of_int (String.length legacy_bytes)
        ^ " bytes, compact="
        ^ string_of_int (String.length compact_bytes)
        ^ ")")) ;
  (* The size delta is exactly the 514-byte hex string ("0x" + 512 hex
     digits) minus the empty-string length-prefixed payload. *)
  Check.(
    (String.length legacy_bytes - String.length compact_bytes = 514)
      int
      ~error_msg:
        "Expected legacy/compact size delta of 514 bytes for all-zero bloom \
         (got %L)")

(* Wire-format invariant: for a non-zero bloom, the legacy and compact
   encodings produce identical bytes. The compact encoder only diverges from
   legacy when the bloom is all-zero. *)
let test_wire_format_matches_on_nonzero_bloom () =
  let receipt = make_receipt ~logs_bloom:some_bloom ~logs:[some_log] in
  let compact_bytes = encode Transaction_info.receipt_fields_encoding receipt in
  let legacy_bytes =
    encode Transaction_info.legacy_receipt_fields_encoding receipt
  in
  Check.(
    (compact_bytes = legacy_bytes)
      string
      ~error_msg:
        "Compact and legacy encodings should agree on non-zero-bloom receipts \
         (compact %L, legacy %R)")

(* The other direction of the rollback contract: a value written by the
   compact encoder against an all-zero bloom does NOT survive the legacy
   decoder. The legacy decoder happens to accept the empty bloom field as
   [Hex ""] (since [hex_of_string ""] returns the empty hex value rather than
   raising), but that value is corrupt: it is *not* the all-zero bloom the
   writer intended. Older node binaries that rely on the bloom being the
   well-formed 512-character hex string would mishandle these rows — which is
   exactly why operators must keep [compact_receipt_encoding = false] until
   they no longer want to roll back. *)
let test_compact_zero_bloom_corrupts_under_legacy_decoder () =
  let receipt = make_receipt ~logs_bloom:zero_bloom ~logs:[] in
  let compact_bytes = encode Transaction_info.receipt_fields_encoding receipt in
  let decoded =
    decode_exn Transaction_info.legacy_receipt_fields_encoding compact_bytes
  in
  Check.(
    (decoded.logs_bloom = Hex "")
      hex_typ
      ~error_msg:
        "Expected legacy decoder to materialize the empty bloom as %R but got \
         %L") ;
  Check.(
    (decoded.logs_bloom <> receipt.logs_bloom)
      hex_typ
      ~error_msg:
        "Legacy decoder should NOT reconstruct the original all-zero bloom \
         from compact bytes — getting %L from a row that was originally %R \
         demonstrates the rollback hazard the flag protects against")

let tests =
  [
    ( "Round-trip",
      [
        ("legacy", `Quick, test_legacy_round_trip);
        ("compact", `Quick, test_compact_round_trip);
      ] );
    ( "Cross-format",
      [
        ( "legacy bytes decode through compact decoder",
          `Quick,
          test_legacy_decodes_through_compact );
        ( "compact zero-bloom bytes corrupt under legacy decoder",
          `Quick,
          test_compact_zero_bloom_corrupts_under_legacy_decoder );
      ] );
    ( "Wire format",
      [
        ( "diverges on zero bloom",
          `Quick,
          test_wire_format_diverges_on_zero_bloom );
        ( "matches on non-zero bloom",
          `Quick,
          test_wire_format_matches_on_nonzero_bloom );
      ] );
  ]

let () = Alcotest.run ~__FILE__ "Compact receipt encoding" tests

let () = Test.run ()
