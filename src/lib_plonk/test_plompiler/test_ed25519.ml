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
open Helpers

(** Test Vectors from RFC 8032, Section 7.1
   https://www.rfc-editor.org/rfc/rfc8032.txt *)
let test_vectors =
  (* sk, pk, msg, sign *)
  [
    ( "c5aa8df43f9f837bedb7442f31dcb7b166d38535076f094b85ce3a2e0b4458f7",
      "fc51cd8e6218a1a38da47ed00230f0580816ed13ba3303ac5deb911548908025",
      "af82",
      "6291d657deec24024827e69c3abe01a30ce548a284743a445e3680d7db5ac3ac18ff9b538d16f290ae67f760984dc6594a7c15e9716ed28dc027beceea1ec40a"
    );
  ]

module P = struct
  module Ed25519 = Plompiler.Ed25519
  module P = Ed25519.P
  module Curve = Ed25519.Curve

  let bytes_of_hex = Plompiler.Utils.bytes_of_hex

  let random_bytes len =
    Bytes.init len (fun _i -> Char.chr @@ (Random.bits () mod 255))

  let test_point_of_compressed_bytes b p_expected () =
    let p = P.point_of_compressed_bytes_exn b in
    assert (Curve.eq p p_expected)

  let tests_point_of_compressed_bytes () =
    List.iter
      (fun (x, expected) ->
        let x = bytes_of_hex x in
        let expected = bytes_of_hex expected |> Curve.of_bytes_exn in
        test_point_of_compressed_bytes x expected ())
      [
        ( "fc51cd8e6218a1a38da47ed00230f0580816ed13ba3303ac5deb911548908025",
          "02bdcd8654ffa945b9e9e334176f23189885cf8db4d1653f83689ddca23a2161fc51cd8e6218a1a38da47ed00230f0580816ed13ba3303ac5deb911548908025"
        );
        ( "6291d657deec24024827e69c3abe01a30ce548a284743a445e3680d7db5ac3ac",
          "4f445bba8b44933201eda162798f8f091a79b65f2c16dbf9666ef59fd00ea5396291d657deec24024827e69c3abe01a30ce548a284743a445e3680d7db5ac32c"
        );
      ]

  let test_vanilla_ed25519 sk pk_expected msg sign_expected =
    let P.{r = sign_r_expected; s = sign_s_expected} = sign_expected in
    let pk = P.neuterize sk in
    assert (Curve.eq pk pk_expected) ;

    let signature = P.sign sk msg in
    assert (Curve.eq signature.r sign_r_expected) ;
    assert (List.for_all2 Bool.equal signature.s sign_s_expected) ;

    assert (P.verify msg pk signature) ;
    Bytes.set msg 0 '\x00' ;
    assert (not @@ P.verify msg pk signature)

  let test_random () =
    let sk = random_bytes 32 in
    let msg = random_bytes 64 in
    let pk =
      Hacl_star.Hacl.Ed25519.secret_to_public ~sk
      |> P.point_of_compressed_bytes_exn
    in
    let signature =
      let sign_rs = Hacl_star.Hacl.Ed25519.sign ~sk ~msg in
      let sign_r = Bytes.sub sign_rs 0 32 |> P.point_of_compressed_bytes_exn in
      let sign_s = Bytes.sub sign_rs 32 32 |> P.scalar_of_bytes_exn in
      P.{r = sign_r; s = sign_s}
    in
    test_vanilla_ed25519 sk pk msg signature

  let tests_ed25519 () =
    List.iter
      (fun (sk, pk, msg, sign) ->
        let sk = bytes_of_hex sk in
        let msg = bytes_of_hex msg in
        let pk = bytes_of_hex pk |> P.point_of_compressed_bytes_exn in
        let signature =
          let sign = bytes_of_hex sign in
          let sign_r = Bytes.sub sign 0 32 |> P.point_of_compressed_bytes_exn in
          let sign_s = Bytes.sub sign 32 32 |> P.scalar_of_bytes_exn in
          P.{r = sign_r; s = sign_s}
        in
        test_vanilla_ed25519 sk pk msg signature)
      test_vectors

  let test () =
    tests_point_of_compressed_bytes () ;
    tests_ed25519 () ;
    test_random ()
end

module Ed25519 (L : LIB) = struct
  open L

  open Utils (L)

  module Ed25519 = Plompiler.Ed25519
  module P = Ed25519.P
  module V = Ed25519.V (L)

  let bytes_of_hex = Plompiler.Utils.bytes_of_hex

  let test_verify_circuit pk msg signature () =
    let msg = Bytes.input_bytes ~le:true msg in
    with_label ~label:"Ed25519.test_verify"
    @@ let* pk = input ~kind:`Public @@ V.Encoding.pk_encoding.input pk in
       let* msg = input msg in
       let* signature =
         input @@ V.Encoding.signature_encoding.input signature
       in
       let signature = V.Encoding.signature_encoding.decode signature in
       with_label ~label:"Ed25519.test_with_bool_check"
       @@ with_bool_check (V.verify msg pk signature)

  let tests_ed25519_verify =
    List.map
      (fun (_sk, pk, msg, sign) ->
        let msg = bytes_of_hex msg in
        let pk = bytes_of_hex pk |> P.point_of_compressed_bytes_exn in
        let sign = bytes_of_hex sign in
        let sign_r =
          Stdlib.Bytes.sub sign 0 32 |> P.point_of_compressed_bytes_exn
        in
        let sign_s = Stdlib.Bytes.sub sign 32 32 |> P.scalar_of_bytes_exn in
        test
          ~valid:true
          ~name:"Ed25519.test_verify_circuit"
          ~flamegraph:false
          (test_verify_circuit pk msg {r = sign_r; s = sign_s}))
      test_vectors

  let tests = tests_ed25519_verify
end

let tests =
  [
    Alcotest.test_case "P" `Quick P.test;
    Alcotest.test_case "Ed25519" `Slow (to_test (module Ed25519 : Test));
    (*     Alcotest.test_case *)
    (*       "Ed25519 plonk" *)
    (*       `Slow *)
    (*       (to_test ~plonk:(module Plonk.Main_protocol) (module Ed25519 : Test)); *)
  ]
