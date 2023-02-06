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
    Invocation: dune exec src/proto_alpha/lib_dac/test/main.exe \
                  -- test "^\[Unit\] Dac_plugin_registration.ml$"   
    Subject:    Tests for the interoperability between Dac hash
                and given protocol hash
*)

module Protocol_reveal_hash = Protocol.Sc_rollup_reveal_hash

(* Hash copied from
   https://gitlab.com/tezos/tezos/-/blob/master/tezt/tests/dac.ml#L331 *)
let reveal_hash =
  Stdlib.Option.get
  @@ Protocol_reveal_hash.of_hex
       "00a3703854279d2f377d689163d1ec911a840d84b56c4c6f6cafdf0610394df7c6"

let assert_equal_bytes ~loc msg =
  Assert.equal ~loc Bytes.equal msg String.pp_bytes_hex

let make_plugin_and_save_hash (proto : (module Dac_plugin.T) option ref)
    make_plugin : (bytes -> Dac_plugin.Dac_hash.t) -> (module Dac_plugin.T) =
 fun f ->
  let dac_plugin = make_plugin f in
  proto := Option.some dac_plugin ;
  dac_plugin

let test_dac_hash_bin_encoding_roundtrips_with_reveal_hash () =
  let open Lwt_result_syntax in
  let dac_plugin = Stdlib.Option.get (Dac_plugin.get Protocol.hash) in
  let module Plugin = (val dac_plugin) in
  let to_bytes e a =
    Stdlib.Result.get_ok @@ Data_encoding.Binary.to_bytes e a
  in
  let from_bytes e a =
    Stdlib.Result.get_ok @@ Data_encoding.Binary.of_bytes e a
  in
  let reveal_hash_bytes = to_bytes Protocol_reveal_hash.encoding reveal_hash in
  let dac_hash = from_bytes Plugin.Dac_hash.encoding reveal_hash_bytes in
  let dac_hash_bytes = to_bytes Plugin.Dac_hash.encoding dac_hash in
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

let tests =
  [
    Tztest.tztest
      "Binary encoding roundtrip test between Dac hash and reveal hash"
      `Quick
      test_dac_hash_bin_encoding_roundtrips_with_reveal_hash;
  ]
