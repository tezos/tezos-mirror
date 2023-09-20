(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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
    Component:  Lib_dac Dac_plugin
    Invocation: dune exec src/lib_dac/test/main.exe -- --file test_dac_plugin.ml
    Subject:    Tests for the Dac_plugin.raw_hash.
*)

(* On all the following tests, the raw_hash used is coming from
   tezt/tests/dac_example_payloads/preimage.json *)
let test_encode_decode_json () =
  let raw_hash =
    Dac_plugin.raw_hash_of_bytes
      (Bytes.of_string
         "00dce42551fb786c51b29b723f4abba3ea04eb3d239a9a59ec5a5434e151e105e4")
  in
  let encoded_raw_hash =
    Data_encoding.Json.construct Dac_plugin.raw_hash_encoding raw_hash
  in
  let decoded_raw_hash =
    Data_encoding.Json.destruct Dac_plugin.raw_hash_encoding encoded_raw_hash
  in
  assert (raw_hash = decoded_raw_hash)

let test_encode_decode_binary () =
  let raw_hash =
    Dac_plugin.raw_hash_of_bytes
      (Bytes.of_string
         "00dce42551fb786c51b29b723f4abba3ea04eb3d239a9a59ec5a5434e151e105e4")
  in
  let encoded_raw_hash =
    Data_encoding.Binary.to_string_exn Dac_plugin.raw_hash_encoding raw_hash
  in
  let decoded_raw_hash =
    Data_encoding.Binary.of_string Dac_plugin.raw_hash_encoding encoded_raw_hash
  in
  match decoded_raw_hash with
  | Error _ -> assert false
  | Ok decoded_raw_hash -> assert (raw_hash = decoded_raw_hash)

let tests =
  [
    Alcotest.test_case
      "Encode and decode to JSON leads to the same Dac_plugin.raw_hash"
      `Quick
      test_encode_decode_json;
    Alcotest.test_case
      "Encode and decode to binary leads to the same Dac_plugin.raw_hash"
      `Quick
      test_encode_decode_binary;
  ]

let () = Alcotest.run ~__FILE__ "lib_dac" [("Dac_plugin.ml", tests)]
