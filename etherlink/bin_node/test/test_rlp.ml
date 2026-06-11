(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Bin_evm_node
    Invocation:   cd etherlink/bin_node/test/ ; dune exec ./test_rlp.exe
    Subject:      Tests for the RLP encoder/decoder
*)

(* TODO: #7449
   These tests need to be refactored in tezt, Alcote*t has been deprecated *)

open Evm_node_lib_dev_encoding
open Rlp

let decode_value s =
  match String.remove_prefix ~prefix:"#" s with
  | Some bigint -> Value (Z.of_string bigint |> encode_z)
  | None -> Value (Bytes.of_string s)

let rec decode_rlp_in_vector = function
  | `A l ->
      let items = List.map decode_rlp_in_vector l in
      List items
  | `String s -> decode_value s
  | `Float f -> Value (encode_int (Int.of_float f))
  | _ -> Test.fail "Illformed JSON test vector"

let decode_maybe_invalid_rlp = function
  | `String "INVALID" -> None
  | json -> Some (decode_rlp_in_vector json)

let decode_out_in_vector out =
  let (Hex stripped) = Ethereum_types.hex_of_string out in
  match Hex.to_bytes (`Hex stripped) with
  | Some b -> b
  | None -> Test.fail "Illformed JSON test vector"

let read_test_vector filename =
  let file = read_file filename in
  let json = Data_encoding.Json.from_string file in
  let read_test = function
    | testname, `O [("in", rlp); ("out", `String bytes)] ->
        (testname, decode_maybe_invalid_rlp rlp, decode_out_in_vector bytes)
    | _ -> Test.fail "Illformed JSON test vector"
  in
  match json with
  | Ok (`O fields) -> List.map read_test fields
  | _ -> Test.fail "Illformed JSON test vector"

let examples_from_specification =
  let bs = Bytes.of_string in
  let value s = Value (bs s) in
  [
    (value "dog", bs "\x83dog");
    (List [value "cat"; value "dog"], bs "\xc8\x83cat\x83dog");
    (value "", bs "\x80");
    (* Check that 0 is encoded as the empty bytes. *)
    (Value (encode_int 0), bs "\x80");
    (List [], bs "\xc0");
    (value "\x00", bs "\x00");
    (value "\x0f", bs "\x0f");
    (value "\x04\x00", bs "\x82\x04\x00");
    ( List [List []; List [List []]; List [List []; List [List []]]],
      bs "\xc7\xc0\xc1\xc0\xc3\xc0\xc1\xc0" );
    ( value "Lorem ipsum dolor sit amet, consectetur adipisicing elit",
      bs "\xb8\x38Lorem ipsum dolor sit amet, consectetur adipisicing elit" );
  ]

let rlp_typ = Check.equalable pp ( = )

let bytes_typ = Check.(convert Hex.of_bytes (equalable Hex.pp Stdlib.( = )))

let check testname rlp bytes =
  let error_msg = Format.sprintf "Test `%s`: expected %%R, got %%L." testname in
  let check_encode rlp expected =
    match rlp with
    | Some rlp ->
        let encoded = encode rlp in
        Check.((encoded = expected) bytes_typ ~error_msg)
    | None -> ()
  in
  let check_decode bytes expected =
    let decoded =
      try decode bytes |> Option.of_result
      with e ->
        let bt = Printexc.get_backtrace () in
        Format.printf "Test: %s, Backtrace:\n%s\n%!" testname bt ;
        raise e
    in
    match expected with
    | Some expected ->
        Check.((decoded = Some expected) (option rlp_typ) ~error_msg)
    | None ->
        if decoded <> None then
          Test.fail
            "Test '%s': decoding is expected to fail, but got %a"
            testname
            (Format.pp_print_option
               ~none:(fun ppf () -> Format.fprintf ppf "None")
               pp)
            decoded
  in
  check_encode rlp bytes ;
  check_decode bytes rlp

let test_examples () =
  List.iter
    (fun (rlp, bytes) -> check "specification" (Some rlp) bytes)
    examples_from_specification

let test_canonical_valid_vectors () =
  let valid_tests = read_test_vector "rlptest.json" in
  List.iter (fun (testname, rlp, bytes) -> check testname rlp bytes) valid_tests

let test_canonical_invalid_vectors () =
  let valid_tests = read_test_vector "invalidRLPTest.json" in
  List.iter (fun (testname, rlp, bytes) -> check testname rlp bytes) valid_tests

let test_truncated_input () =
  (* A list header claiming more bytes than available must return an error,
     not raise an exception. Regression test for a crash caused by
     decode_int reading past the end of the buffer. *)
  let truncated_cases =
    [
      (* f8 5e = list of 94 bytes, but only 1 byte follows *)
      ("truncated list length", Bytes.of_string "\xf8\x5e\x01");
      (* b8 38 = string of 56 bytes, but nothing follows *)
      ("truncated string length", Bytes.of_string "\xb8\x38");
      (* f9 02 8f = list of 655 bytes, but only 3 bytes follow *)
      ("truncated long list", Bytes.of_string "\xf9\x02\x8f\xc0\xc0\xc0");
      (* Single byte claiming long length *)
      ("truncated length field", Bytes.of_string "\xf9\x01");
    ]
  in
  List.iter
    (fun (name, input) ->
      let result =
        try decode input |> Option.of_result
        with exn ->
          Test.fail
            "Test '%s': decode raised %s instead of returning an error"
            name
            (Printexc.to_string exn)
      in
      if result <> None then
        Test.fail
          "Test '%s': truncated input should fail to decode, but got %a"
          name
          (Format.pp_print_option
             ~none:(fun ppf () -> Format.fprintf ppf "None")
             pp)
          result)
    truncated_cases

(* Compatibility contract of the [/info] account record codec between
   node releases and kernels before/after the origin-classification tag
   (L2-1570): both the legacy 3-field RLP shape and the 4-field shape
   carrying the tag are accepted, and the tag is round-tripped without
   ever being invented. Lives in this executable rather than its own:
   every test binary of this directory links the full EVM node with
   [-linkall], and one more link step is enough to OOM the CI job. *)
module Account_info_codec = struct
  module Account_info = Evm_node_lib_dev.Durable_storage.EVM_account_info

  let some_balance = Ethereum_types.Qty (Z.of_int64 1_000_000_000_000L)

  let some_nonce = Ethereum_types.Qty (Z.of_int 42)

  let some_code_hash = Ethereum_types.Hash (Hex (String.make 64 'a'))

  let make_info ?origin () : Account_info.t =
    {
      balance = some_balance;
      nonce = some_nonce;
      code_hash = some_code_hash;
      origin;
    }

  let info_typ : Account_info.t Check.typ =
    Check.equalable
      (fun fmt
           {Account_info.balance = Qty b; nonce = Qty n; code_hash; origin}
         ->
        Format.fprintf
          fmt
          "{balance = %a; nonce = %a; code_hash = %s; origin = %s}"
          Z.pp_print
          b
          Z.pp_print
          n
          (let (Ethereum_types.Hash (Hex h)) = code_hash in
           h)
          (match origin with
          | None -> "None"
          | Some Account_info.Unclassified -> "Unclassified"
          | Some Account_info.Native -> "Native"
          | Some (Account_info.Alias payload) ->
              Format.sprintf "Alias %s" (Bytes.to_string payload)))
      ( = )

  let decode_exn bytes =
    match Account_info.decode_opt bytes with
    | Some info -> info
    | None -> Test.fail "decode_opt rejected a valid account info record"

  let check_round_trip ~name info =
    let decoded = decode_exn (Account_info.encode info) in
    Check.((decoded = info) info_typ)
      ~error_msg:(name ^ ": round-trip mismatch (got %L, expected %R)")

  (* Legacy 3-field records (written by kernels without the origin
     classification — the current Etherlink mainnet format) must decode
     with [origin = None] and re-encode identically: the node never
     invents the fourth field. *)
  let test_legacy_round_trip () =
    let info = make_info () in
    check_round_trip ~name:"legacy 3-field" info ;
    let legacy_bytes = Account_info.encode info in
    let reencoded = Account_info.encode (decode_exn legacy_bytes) in
    Check.((Bytes.to_string reencoded = Bytes.to_string legacy_bytes) string)
      ~error_msg:"re-encoding a legacy record changed its bytes (%L vs %R)"

  (* A well-formed [AliasInfo] payload, byte-identical to what the
     kernel's [AliasInfo::bin_write] emits for {Tezos, "tz1abcdef"}:
     RuntimeId tag 0 (Tezos), a 4-byte big-endian length (9), then the
     address bytes. Pins the cross-language wire format the node now
     validates. *)
  let valid_alias_payload = Bytes.of_string "\000\000\000\000\009tz1abcdef"

  (* 4-field records round-trip for every classification the kernel
     emits, alias payload included. *)
  let test_origin_round_trip () =
    List.iter
      (fun origin -> check_round_trip ~name:"classified" (make_info ~origin ()))
      Account_info.[Unclassified; Native; Alias valid_alias_payload]

  (* A record with trailing fields beyond the origin is not silently
     accepted. *)
  let test_reject_extra_fields () =
    let open Rlp in
    let bytes =
      Rlp.encode
        (List
           [
             Value (Ethereum_types.encode_u256_le some_balance);
             Value (Ethereum_types.encode_u64_le some_nonce);
             Value (Ethereum_types.encode_hash some_code_hash);
             Value (Bytes.make 1 '\001');
             Value (Bytes.make 1 '\001');
           ])
    in
    match Account_info.decode_opt bytes with
    | None -> ()
    | Some _ -> Test.fail "decode_opt accepted a 5-field record"

  (* The origin field starts with a one-byte tag among the known
     values, and only [Alias] carries a payload, like in the kernel
     codec: a record the kernel would reject must not be silently
     "repaired" by a node round-trip. *)
  let test_reject_malformed_origin () =
    List.iter
      (fun origin ->
        let open Rlp in
        let bytes =
          Rlp.encode
            (List
               [
                 Value (Ethereum_types.encode_u256_le some_balance);
                 Value (Ethereum_types.encode_u64_le some_nonce);
                 Value (Ethereum_types.encode_hash some_code_hash);
                 Value origin;
               ])
        in
        match Account_info.decode_opt bytes with
        | None -> ()
        | Some _ ->
            Test.fail
              "decode_opt accepted a malformed origin field (%d bytes)"
              (Bytes.length origin))
      [
        (* empty field *)
        Bytes.empty;
        (* unknown tag *)
        Bytes.make 1 '\003';
        (* Unclassified and Native must not carry a payload *)
        Bytes.of_string "\000payload";
        Bytes.of_string "\001payload";
        (* Alias must carry a payload *)
        Bytes.make 1 '\002';
        (* Alias with a malformed [AliasInfo]: bad runtime tag (3) *)
        Bytes.of_string "\002\003\000\000\000\000";
        (* Alias with a length prefix longer than the payload *)
        Bytes.of_string "\002\000\000\000\000\009ab";
        (* Alias with trailing bytes after a well-formed [AliasInfo] *)
        Bytes.of_string "\002\000\000\000\000\001a\255";
      ]
end

let tests =
  [
    ( "RLP",
      [
        ("test specification", `Quick, test_examples);
        ("test canonical valid vectors", `Quick, test_canonical_valid_vectors);
        ( "test canonical invalid vectors",
          `Quick,
          test_canonical_invalid_vectors );
        ("test truncated input", `Quick, test_truncated_input);
      ] );
    ( "Account info codec",
      [
        ( "legacy 3-field round-trip",
          `Quick,
          Account_info_codec.test_legacy_round_trip );
        ( "classified 4-field round-trip",
          `Quick,
          Account_info_codec.test_origin_round_trip );
        ( "reject extra fields",
          `Quick,
          Account_info_codec.test_reject_extra_fields );
        ( "reject malformed origin",
          `Quick,
          Account_info_codec.test_reject_malformed_origin );
      ] );
  ]

let () = Alcotest.run ~__FILE__ "Test RLP encoding" tests

let () = Test.run ()
