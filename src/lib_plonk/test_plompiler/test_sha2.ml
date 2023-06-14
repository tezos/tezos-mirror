(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
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

open Plompiler
open Plonk_test
module CS = Plonk.Circuit
open Helpers

module Internal_sha256 : Test =
functor
  (L : LIB)
  ->
  struct
    open L
    open L.Bytes

    open Utils (L)

    module H = Plompiler__Gadget_sha2.MAKE (Plompiler__Sha2_variants.Sha256) (L)

    let bytes_of_hex = Plompiler.Utils.bytes_of_hex

    let test_ch a b c z () =
      let* a = input ~kind:`Public a in
      let* b = input b in
      let* c = input c in
      let* z = input z in
      let* z' = H.ch a b c in
      assert_equal z z'

    let tests_ch =
      let i0 = input_bytes @@ bytes_of_hex "11" in
      let i1 = input_bytes @@ bytes_of_hex "13" in
      let i2 = input_bytes @@ bytes_of_hex "2A" in
      let o = input_bytes @@ bytes_of_hex "3B" in
      [
        test ~valid:true ~name:"SHA256.test_ch" @@ test_ch i0 i1 i2 o;
        test ~valid:false ~name:"SHA256.test_ch" @@ test_ch i0 i1 i2 i2;
      ]

    let test_maj a b c z () =
      let* a = input ~kind:`Public a in
      let* b = input b in
      let* c = input c in
      let* z = input z in
      let* z' = H.maj a b c in
      assert_equal z z'

    let tests_maj =
      let i0 = input_bytes @@ bytes_of_hex "11" in
      let i1 = input_bytes @@ bytes_of_hex "13" in
      let i2 = input_bytes @@ bytes_of_hex "2A" in
      let o = input_bytes @@ bytes_of_hex "13" in
      [
        test ~valid:true ~name:"SHA256.test_maj" @@ test_maj i0 i1 i2 o;
        test ~valid:false ~name:"SHA256.test_maj" @@ test_maj i0 i1 i2 i2;
      ]

    let test_sigma0 a z () =
      let* a = input ~kind:`Public a in
      let* z = input z in
      let* z' = H.sigma_0 a in
      assert_equal z z'

    let tests_sigma0 =
      let i = input_bytes @@ bytes_of_hex "0000002A" in
      let o = input_bytes @@ bytes_of_hex "540A8005" in
      [
        test ~valid:true ~name:"SHA256.test_sigma0" @@ test_sigma0 i o;
        test ~valid:false ~name:"SHA256.test_sigma0" @@ test_sigma0 i i;
      ]

    let test_sigma1 a z () =
      let* a = input ~kind:`Public a in
      let* z = input z in
      let* z' = H.sigma_1 a in
      assert_equal z z'

    let tests_sigma1 =
      let i = input_bytes @@ bytes_of_hex "0000002A" in
      let o = input_bytes @@ bytes_of_hex "00104000" in
      [
        test ~valid:true ~name:"SHA256.test_sigma1" @@ test_sigma1 i o;
        test ~valid:false ~name:"SHA256.test_sigma1" @@ test_sigma1 i i;
      ]

    let test_sum0 a z () =
      let* a = input ~kind:`Public a in
      let* z = input z in
      let* z' = H.sum_0 a in
      assert_equal z z'

    let tests_sum0 =
      let i = input_bytes @@ bytes_of_hex "00000001" in
      let o = input_bytes @@ bytes_of_hex "40080400" in
      [
        test ~valid:true ~name:"SHA256.test_sum0" @@ test_sum0 i o;
        test ~valid:false ~name:"SHA256.test_sum0" @@ test_sum0 i i;
      ]

    let test_sum1 a z () =
      let* a = input ~kind:`Public a in
      let* z = input z in
      let* z' = H.sum_1 a in
      assert_equal z z'

    let tests_sum1 =
      let i = input_bytes @@ bytes_of_hex "00000001" in
      let o = input_bytes @@ bytes_of_hex "04200080" in
      [
        test ~valid:true ~name:"SHA256.test_sum1" @@ test_sum1 i o;
        test ~valid:false ~name:"SHA256.test_sum1" @@ test_sum1 i i;
      ]

    let test_padding i o () =
      let* i = input ~kind:`Public i in
      let* o = input o in
      let* o' = H.padding i in
      assert_equal o o'

    (* Example from the spec https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.180-4.pdf
       section 5.1.1 *)
    let tests_padding =
      let abc = Stdlib.Bytes.of_string "abc" in
      let padding =
        Stdlib.Bytes.(
          concat
            empty
            (of_string "\128" :: List.init 52 (fun _ -> of_string "\000")))
      in
      let length_bites =
        let res = Stdlib.Bytes.create 8 in
        Stdlib.Bytes.set_int64_be res 0 24L ;
        res
      in
      let output_bytes =
        Stdlib.Bytes.(concat empty [abc; padding; length_bites])
      in
      assert (Stdlib.Bytes.length output_bytes = 512 / 8) ;
      let i = input_bytes abc in
      let o = input_bytes output_bytes in
      [test ~valid:true ~name:"SHA256.test_padding" @@ test_padding i o]

    let test_initial_hash i o () =
      let* o = input o in
      let* ih = H.initial_hash in
      assert_equal o ih.(i)

    let tests_initial_hash =
      let o = input_bytes @@ bytes_of_hex "6A09E667" in
      [
        test ~valid:true ~name:"SHA256.test_initial_hash"
        @@ test_initial_hash 0 o;
      ]

    let tests =
      tests_ch @ tests_maj @ tests_sigma0 @ tests_sigma1 @ tests_sum0
      @ tests_sum1 @ tests_padding @ tests_initial_hash
  end

module External_sha256 : Test =
functor
  (L : LIB)
  ->
  struct
    open L
    open Bytes

    open Utils (L)

    module H256 = Plompiler.Gadget.Sha256 (L)

    let bytes_of_hex = Plompiler.Utils.bytes_of_hex

    let test_digest_sha256 i o () =
      let* i = input ~kind:`Public i in
      let* o = input o in
      let* o' = H256.digest i in
      assert_equal o o'

    let tests_digest_sha256 =
      List.map
        (fun (i, o) ->
          let name = "SHA256.test_digest" ^ i in
          let i = input_bytes @@ Stdlib.Bytes.of_string i in
          let o = input_bytes @@ bytes_of_hex (String.concat "" o) in
          test ~valid:true ~name ~flamegraph:false @@ test_digest_sha256 i o)
        [
          ( "abc",
            [
              "BA7816BF";
              "8F01CFEA";
              "414140DE";
              "5DAE2223";
              "B00361A3";
              "96177A9C";
              "B410FF61";
              "F20015AD";
            ] );
          ( "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq",
            [
              "248D6A61";
              "D20638B8";
              "E5C02693";
              "0C3E6039";
              "A33CE459";
              "64FF2167";
              "F6ECEDD4";
              "19DB06C1";
            ] );
        ]

    let tests = tests_digest_sha256
  end

module External_sha512 : Test =
functor
  (L : LIB)
  ->
  struct
    open L
    open Bytes

    open Utils (L)

    module H512 = Plompiler.Gadget.Sha512 (L)

    let bytes_of_hex = Plompiler.Utils.bytes_of_hex

    let test_digest_sha512 i o () =
      let* i = input ~kind:`Public i in
      let* o = input o in
      let* o' = H512.digest i in
      assert_equal o o'

    let tests_digest_sha512 =
      List.map
        (fun (i, o) ->
          let name = "SHA512.test_digest" ^ i in
          let i = input_bytes @@ Stdlib.Bytes.of_string i in
          let o = input_bytes @@ bytes_of_hex (String.concat "" o) in
          test ~valid:true ~name ~flamegraph:false @@ test_digest_sha512 i o)
        [
          ( "abc",
            [
              "DDAF35A1";
              "93617ABA";
              "CC417349";
              "AE204131";
              "12E6FA4E";
              "89A97EA2";
              "0A9EEEE6";
              "4B55D39A";
              "2192992A";
              "274FC1A8";
              "36BA3C23";
              "A3FEEBBD";
              "454D4423";
              "643CE80E";
              "2A9AC94F";
              "A54CA49F";
            ] );
          ( "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu",
            [
              "8E959B75";
              "DAE313DA";
              "8CF4F728";
              "14FC143F";
              "8F7779C6";
              "EB9F7FA1";
              "7299AEAD";
              "B6889018";
              "501D289E";
              "4900F7E4";
              "331B99DE";
              "C4B5433A";
              "C7D329EE";
              "B6DD2654";
              "5E96E55B";
              "874BE909";
            ] );
        ]

    let tests = tests_digest_sha512
  end

let tests =
  let sha256 =
    [
      ("Internal_sha256", (module Internal_sha256 : Test));
      ("External_sha256", (module External_sha256 : Test));
    ]
  in
  let sha512 = [("External_sha512", (module External_sha512 : Test))] in
  (* This test uses plonk and it is marked quick so that it
     is always run by the CI *)
  List.map
    (fun (name, m) -> Alcotest.test_case name `Slow (to_test m))
    (sha256 @ sha512)
  @ List.map
      (fun (name, m) ->
        Alcotest.test_case
          (name ^ " plonk")
          `Slow
          (to_test ~plonk:(module Plonk.Main_protocol) m))
      sha256
