(*****************************************************************************)
(*                                                                           *)
(* Copyright (c) 2021 Danny Willems <be.danny.willems@gmail.com>             *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
    Component:    lib_mec
    Invocation:   dune exec src/lib_mec/test/main.exe \
                  -- --file test_digestif.ml
    Subject:      Test lib mec
*)

module Digestif = Mec.Digestif

module Blake2b = Digestif.Make_BLAKE2B (struct
  let digest_size = 64
end)

let test_blake2b_feed_twice_and_concat_is_the_same () =
  let vectors =
    [
      ( Bytes.init 16 (fun _ -> char_of_int (Random.int 256)),
        Bytes.init (Random.int 1000) (fun _ -> char_of_int (Random.int 256)),
        Bytes.init (Random.int 1000) (fun _ -> char_of_int (Random.int 256)) );
    ]
  in
  List.iter
    (fun (personalisation, a, b) ->
      let ab = Bytes.concat Bytes.empty [a; b] in
      let ctx_1 = Blake2b.init ~personalisation () in
      let ctx_1 = Blake2b.feed_bytes ctx_1 a in
      let ctx_1 = Blake2b.feed_bytes ctx_1 b in
      let res_1 = Blake2b.to_hex (Blake2b.get ctx_1) in
      let ctx_2 = Blake2b.init ~personalisation () in
      let ctx_2 = Blake2b.feed_bytes ctx_2 ab in
      let res_2 = Blake2b.to_hex (Blake2b.get ctx_2) in
      assert (String.equal res_1 res_2))
    vectors

let test_vectors_blake2b () =
  let vectors =
    [
      ( Bytes.of_string "Zcash_RedJubjubH",
        Hex.to_string
          (`Hex
            "1de4252d8a94cd363901802d944a267a27aabd81b5ee9205810a14e9b353c04f466f6f20626172"),
        "7f1c630bd9ce5daac6d42506b860032d08e185896f5bf33082548644afb89ae4a34926112f066ac88819004263cde207423c74e158d53ed6e23c92f208a2ef1b"
      );
      ( Bytes.of_string "Zcash_RedJubjubH",
        Hex.to_string
          (`Hex
            "e733065e8fa3f5172cf389ba48990e0ae50af8650e34abdf44ac4e2a9cb4f454c533fef1b062737796cc00285ecda1d9b4adc1d9de324b359d2c011b9ed91dcca4616518b47dd47477d2a36614265044466f6f20626172"),
        "7e7cd9dad1aa4890333558296a48038ac6f4b0730c238693867e3cc3711d5a471917c71cd695c8718ecaa8a69747f8454e35dda0c4a699ce2d1f166644acf80b"
      );
      ( Bytes.make 16 '\000',
        Hex.to_string (`Hex ""),
        "786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419d25e1031afee585313896444934eb04b903a685b1448b755d56f701afe9be2ce"
      );
      ( Bytes.make 16 '\000',
        Hex.to_string (`Hex "00"),
        "2fa3f686df876995167e7c2e5d74c4c7b6e48f8068fe0e44208344d480f7904c36963e44115fe3eb2a3ac8694c28bcb4f5a0f3276f2e79487d8219057a506e4b"
      );
    ]
  in
  List.iter
    (fun (personalisation, input, expected_output) ->
      let ctx = Blake2b.init ~personalisation () in
      let ctx = Blake2b.feed_string ctx input in
      let output = Blake2b.to_hex (Blake2b.get ctx) in
      let output_bytes = Bytes.of_string output in
      let expected_output_bytes = Bytes.of_string expected_output in
      if not (Bytes.compare output_bytes expected_output_bytes = 0) then
        Alcotest.failf
          "Fail on input %s with personalisation %s. Expected output is %s but \
           the computed output is %s"
          input
          (Bytes.to_string personalisation)
          (Hex.show (`Hex expected_output))
          output)
    vectors

let () =
  Alcotest.run
    ~__FILE__
    "Digestif fork"
    [
      ( "Blake2b test vectors",
        [
          Alcotest.test_case
            "Test vector with personalisation"
            `Quick
            test_vectors_blake2b;
        ] );
      ( "Blake2b properties",
        [
          Alcotest.test_case
            "Feed twice and finalize gives the same result than feeding with \
             the concatenation and finalize"
            `Quick
            (Mec.Curve.Utils.PBT.repeat
               100
               test_blake2b_feed_twice_and_concat_is_the_same);
        ] );
    ]
