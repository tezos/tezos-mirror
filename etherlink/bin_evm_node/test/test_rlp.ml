(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    Invocation:   dune exec etherlink/bin_evm_node/test/main.exe -- --file test_rlp.ml
    Subject:      Tests for the RLP encoder/decoder
*)

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
  let file = read_file Filename.(concat (dirname __FILE__) filename) in
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

let tests =
  [
    ( "RLP",
      [
        ("test specification", `Quick, test_examples);
        ("test canonical valid vectors", `Quick, test_canonical_valid_vectors);
        ( "test canonical invalid vectors",
          `Quick,
          test_canonical_invalid_vectors );
      ] );
  ]

let () = Alcotest.run ~__FILE__ "Test RLP encoding" tests
