(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Trili Tech, <contact@trili.tech>                       *)
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
    Component:  Lib_dac_node Dac_hash
    Invocation: dune exec src/proto_alpha/lib_dac_plugin/test/main.exe \
                  -- --file test_dac_plugin_registration.ml
    Subject:    Tests for the interoperability between Dac hash
                and given protocol hash
*)

module Protocol_reveal_hash = Protocol.Sc_rollup_reveal_hash

let dac_plugin = Stdlib.Option.get (Dac_plugin.get Protocol.hash)

module P = (val dac_plugin)

(* Hash copied from
   https://gitlab.com/tezos/tezos/-/blob/master/tezt/tests/dac.ml#L331 *)
let reveal_hash =
  Stdlib.Option.get
  @@ Protocol_reveal_hash.of_hex
       "00a3703854279d2f377d689163d1ec911a840d84b56c4c6f6cafdf0610394df7c6"

let assert_equal_bytes ~loc msg =
  Assert.equal ~loc Bytes.equal msg String.pp_bytes_hex

let test_dac_hash_bin_encoding_roundtrip_with_reveal_hash () =
  let open Lwt_result_syntax in
  let to_bytes e a =
    Stdlib.Result.get_ok @@ Data_encoding.Binary.to_bytes e a
  in
  let from_bytes e a =
    Stdlib.Result.get_ok @@ Data_encoding.Binary.of_bytes e a
  in
  let reveal_hash_bytes = to_bytes Protocol_reveal_hash.encoding reveal_hash in
  let dac_hash = from_bytes P.encoding reveal_hash_bytes in
  let dac_hash_bytes = to_bytes P.encoding dac_hash in
  let reveal_hash_decoded =
    from_bytes Protocol_reveal_hash.encoding dac_hash_bytes
  in
  let* () =
    assert_equal_bytes
      ~loc:__LOC__
      "Encoded bytes are not equal"
      reveal_hash_bytes
      dac_hash_bytes
  in
  Assert.equal
    ~loc:__LOC__
    Protocol_reveal_hash.equal
    "Roundtrip hash is not equal"
    Protocol_reveal_hash.pp
    reveal_hash
    reveal_hash_decoded

let test_dac_hash_hex_roundtrip_with_reveal_hash () =
  let reveal_hash_hex = Protocol_reveal_hash.to_hex reveal_hash in
  let dac_hash = Stdlib.Option.get @@ P.of_hex reveal_hash_hex in
  let dac_hash_hex = P.to_hex dac_hash in
  Assert.equal_string ~loc:__LOC__ reveal_hash_hex dac_hash_hex

let test_dac_hash_hash_bytes_with_reveal_hash () =
  let payload = Bytes.of_string "Hello world" in
  let dac_hash = P.hash_bytes ~scheme:Blake2B [payload] in
  let dac_hash =
    Stdlib.Result.get_ok @@ Data_encoding.Binary.to_bytes P.encoding dac_hash
  in
  let reveal_hash = Protocol_reveal_hash.hash_bytes ~scheme:Blake2B [payload] in
  let reveal_hash =
    Stdlib.Result.get_ok
    @@ Data_encoding.Binary.to_bytes Protocol_reveal_hash.encoding reveal_hash
  in
  assert_equal_bytes
    ~loc:__LOC__
    "Encoded bytes are not equal"
    reveal_hash
    dac_hash

let test_dac_hash_hash_string_with_reveal_hash () =
  let payload = "Hello world" in
  let dac_hash = P.hash_string ~scheme:Blake2B [payload] in
  let dac_hash =
    Stdlib.Result.get_ok @@ Data_encoding.Binary.to_bytes P.encoding dac_hash
  in
  let reveal_hash =
    Protocol_reveal_hash.hash_string ~scheme:Blake2B [payload]
  in
  let reveal_hash =
    Stdlib.Result.get_ok
    @@ Data_encoding.Binary.to_bytes Protocol_reveal_hash.encoding reveal_hash
  in
  assert_equal_bytes
    ~loc:__LOC__
    "Encoded bytes are not equal"
    reveal_hash
    dac_hash

let test_json_encoding_is_hexified () =
  let payload = Bytes.of_string "Hello world" in
  let dac_hash = P.hash_bytes ~scheme:Blake2B [payload] in
  let dac_hash_json = Data_encoding.Json.construct P.encoding dac_hash in
  let dac_hash_json_string = Data_encoding.Json.to_string dac_hash_json in
  let dac_hash_hex_string = P.to_hex dac_hash in
  Assert.equal_string ~loc:__LOC__ dac_hash_hex_string dac_hash_json_string

let tests =
  [
    Tztest.tztest
      "Binary encoding roundtrip test between Dac hash and reveal hash"
      `Quick
      test_dac_hash_bin_encoding_roundtrip_with_reveal_hash;
    Tztest.tztest
      "Hex encoding roundtrip test between Dac hash and reveal hash"
      `Quick
      test_dac_hash_hex_roundtrip_with_reveal_hash;
    Tztest.tztest
      "Hash bytes should be equal between Dac hash and reveal hash"
      `Quick
      test_dac_hash_hash_bytes_with_reveal_hash;
    Tztest.tztest
      "Hash string should be equal between Dac hash and reveal hash"
      `Quick
      test_dac_hash_hash_string_with_reveal_hash;
    Tztest.tztest
      "Json encoded hash string should be a hex string"
      `Quick
      test_dac_hash_hash_string_with_reveal_hash;
  ]

let () =
  Alcotest_lwt.run
    ~__FILE__
    Protocol.name
    [Test_helpers.Unit_test.spec "Dac_plugin_registration.ml" tests]
  |> Lwt_main.run
