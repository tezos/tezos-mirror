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
    Invocation: dune exec src/lib_dac_node/test/main.exe \
                  -- test "^\[Unit\] Dac_hash.ml$"   
    Subject:    Tests for the interoperability between Dac hash
                and given protocol hash
*)

open Tezos_crypto

module Mock_Reveal_Hash = struct
  module Blake2B = struct
    include
      Blake2B.Make
        (Base58)
        (struct
          let name = "Smart_rollup_reveal_data_blake2b_hash"

          let title = "A smart rollup reveal hash"

          let b58check_prefix =
            "\230\206\128\200\196" (* "scrrh1(56)" decoded from Base58. *)

          let size = Some 32
        end)

    let () = Base58.check_encoded_prefix b58check_encoding "scrrh1" 56
  end

  type t = Blake2B of Blake2B.t

  let encoding =
    let open Data_encoding in
    union
      ~tag_size:`Uint8
      [
        case
          ~title:"Reveal_data_hash_v0"
          (Tag 0)
          Blake2B.encoding
          (fun (Blake2B s) -> Some s)
          (fun s -> Blake2B s);
      ]

  let to_hex hash =
    let (`Hex hash) =
      Hex.of_string @@ Data_encoding.Binary.to_string_exn encoding hash
    in
    hash

  let of_hex hex =
    let open Option_syntax in
    let* hash = Hex.to_bytes (`Hex hex) in
    Data_encoding.Binary.of_bytes_opt encoding hash

  let equal (Blake2B a) (Blake2B b) = Blake2B.equal a b
end

(* Hash copied from
   https://gitlab.com/tezos/tezos/-/blob/master/tezt/tests/dac.ml#L331 *)
let reveal_hash =
  Stdlib.Option.get
  @@ Mock_Reveal_Hash.of_hex
       "00a3703854279d2f377d689163d1ec911a840d84b56c4c6f6cafdf0610394df7c6"

module Reveal_hash_mapper = Dac_hash.Make (Mock_Reveal_Hash)

let test_dac_hash_bin_encoding_roundtrips_with_reveal_hash () =
  let to_bytes e a =
    Stdlib.Result.get_ok @@ Data_encoding.Binary.to_bytes e a
  in
  let from_bytes e a =
    Stdlib.Result.get_ok @@ Data_encoding.Binary.of_bytes e a
  in
  let reveal_hash_bytes = to_bytes Mock_Reveal_Hash.encoding reveal_hash in
  let dac_hash = from_bytes Reveal_hash_mapper.encoding reveal_hash_bytes in
  let dac_hash_bytes = to_bytes Reveal_hash_mapper.encoding dac_hash in
  let reveal_hash_decoded =
    from_bytes Mock_Reveal_Hash.encoding dac_hash_bytes
  in
  Assert.equal ~loc:__LOC__ reveal_hash_bytes dac_hash_bytes ;
  Assert.equal ~loc:__LOC__ reveal_hash reveal_hash_decoded ;
  Lwt.return @@ Ok ()

let test_dac_conversion_roundtrips_with_reveal_hash () =
  let dac_hash = Reveal_hash_mapper.of_reveal_hash reveal_hash in
  let decoded_reveal_hash = Reveal_hash_mapper.to_reveal_hash dac_hash in
  Assert.equal ~loc:__LOC__ reveal_hash decoded_reveal_hash ;
  Lwt.return @@ Ok ()

let tests =
  [
    Tztest.tztest
      "Binary encoding roundtrip test between Dac hash and reveal hash"
      `Quick
      test_dac_hash_bin_encoding_roundtrips_with_reveal_hash;
    Tztest.tztest
      "Conversion roundrip test between Dac hash and reveal hash"
      `Quick
      test_dac_conversion_roundtrips_with_reveal_hash;
  ]
