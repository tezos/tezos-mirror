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
                  -- --file test_poseidon252.ml
    Subject:      Test lib mec
*)

open Mec.Hash

module Scalar = Ff.MakeFp (struct
  let prime_order =
    Z.of_string
      "52435875175126190479447740508185965837690552500527637822603658699938581184513"
end)

module Poseidon = Poseidon252.Make (Scalar)

let test_perm_is_consistent () =
  let x = Array.make Poseidon252.Constant.width (Scalar.of_string "17") in
  let y = Array.make Poseidon252.Constant.width (Scalar.of_string "17") in
  let z = Array.make Poseidon252.Constant.width (Scalar.of_string "19") in

  let state_x = Poseidon.Strategy.init x in
  let state_y = Poseidon.Strategy.init y in
  let state_z = Poseidon.Strategy.init z in

  Poseidon.Strategy.apply_perm state_x ;
  Poseidon.Strategy.apply_perm state_y ;
  Poseidon.Strategy.apply_perm state_z ;

  let res_x = Poseidon.Strategy.get state_x in
  let res_y = Poseidon.Strategy.get state_y in
  let res_z = Poseidon.Strategy.get state_z in
  assert (Array.for_all2 Scalar.eq res_x res_y) ;
  assert (not @@ Array.for_all2 Scalar.eq res_x res_z)

let test_vectors_hades252 () =
  let vectors =
    [
      ( Array.make Poseidon252.Constant.width (Scalar.of_string "17"),
        [|
          "20b31534ae4b071c49f0bfaf757c60eeedbc8afd8de7e778c1b870e45b5a334a";
          "84460293173542e4fa384e65596b8bd34f3394c3a424470c0963c57c1208f104";
          "33106ccdafa51903ae0d6c0c1adcf1aa568dd164cc7490ce1c66b64c58865a4c";
          "4454fbb8dbe02de35e5521a4c5b7f0e6dc7968b0f983040336cc17d3792c2c43";
          "914fb19a465a71043e27b88b75603f75a4e664dd87ce27f74c47faf65b4e0f5e";
        |] );
      ( Array.make Poseidon252.Constant.width (Scalar.of_string "19"),
        [|
          "2f26f38f20a624eb7ddc58a28f94a868824a320a64a05c7b028be716c3d47938";
          "577a6555ceb8acfcec1024f76a647a63bef97ef490fa875d5d8d640e9c477973";
          "d3c9f03664b22c12a49a428cd13bf60c397105ae18039208598f00270b71472f";
          "968c4eeb53cb2888a565bf27bc7eb23c648c05f595b1a39fbe11a7aaaba57c4a";
          "e6ddc232b1895b132931211f1052df5a9945ef7c62011a45c5509490cf8cb001";
        |] );
    ]
  in
  List.iter
    (fun (input, expected_output) ->
      let s = Poseidon.Strategy.init input in
      Poseidon.Strategy.apply_perm s ;
      let res = Poseidon.Strategy.get s in
      let expected_output =
        Array.map
          (fun s -> Scalar.of_bytes_exn (Hex.to_bytes (`Hex s)))
          expected_output
      in
      if not (Array.for_all2 Scalar.eq res expected_output) then
        let res =
          String.concat
            "; "
            (Array.to_list @@ Array.map (fun s -> Scalar.to_string s) res)
        in
        let expected_output =
          String.concat
            "; "
            (Array.to_list
            @@ Array.map (fun s -> Scalar.to_string s) expected_output)
        in
        Alcotest.failf
          "Computed result: [%s]. Expected result: [%s]\n"
          res
          expected_output)
    vectors

let test_vectors_poseidon252 () =
  (* See https://github.com/dusk-network/Poseidon252/blob/91bab8cac8fb50bc6f9dc8b039165380600bb5f2/src/sponge/sponge.rs#L302. Only using the 10 first elements as it is the only ones used *)
  let open Poseidon in
  let test_inputs =
    [|
      "bb67ed265bf1db490ded2e1ede55c0d14c55521509dc73f9c354e98ab76c9625";
      "7e74220084d75e10c89e9435d47bb5b8075991b2e29be3b84421dac3b1ee6007";
      "5ce5481a4d78cca03498f72761da1b9f1d2aa8fb300be39f0e4fe2534f9d4308";
      "b1e710e3c4a8c35154b0ce4e4f4af6f498ebd79f8e7cdf3150372c7501be250b";
      "33c9e2025f86b5d82149f1ab8e20a168fc3d99d09b48cbce0286db8752cc3306";
      "e98206bfdce791e4e5144079b997d4fc25006194b35655f0e48490b26e24ea35";
      "86d2a95cc552de8d5bb20bd4a407fee5ffdc314e93dfe6b2dc792bc71fd8cc2d";
      "4edd8307ce28a8c70963d20a7bc28df1e1720bbbc93878a18bd07fad7d51fa15";
      "eabc7a296704a68aa01f95adc85f6dd758b175745336d8fc795a17984024b21e";
      "cfc108673c93df305e31c283b9c767b7097ae4e174a223e0c24b15a67b701a3a";
    |]
  in
  let test_inputs =
    Array.map (fun s -> Scalar.of_bytes_exn (Hex.to_bytes (`Hex s))) test_inputs
  in
  let inner points expected_res =
    let ctxt = Hash.init () in
    let ctxt = Hash.digest ctxt points in
    let v = Hash.get ctxt in
    let exp_res = Scalar.of_bytes_exn (Hex.to_bytes (`Hex expected_res)) in
    if not (Scalar.eq v exp_res) then
      Alcotest.failf
        "Expected result %s, but computed %s"
        Hex.(show (of_bytes (Scalar.to_bytes exp_res)))
        Hex.(show (of_bytes (Scalar.to_bytes v)))
  in
  inner [||] "00c78302fb0a2213d756fc08cb382d02adb4fd22d132fa14413f6a60e32d9054" ;
  inner
    (Array.sub test_inputs 0 3)
    "e36f4ea9b858d5c85b02770823c7c5d8253c28787d17f283ca348b906dca8528" ;
  inner
    (Array.sub test_inputs 0 4)
    "75ea3265c80d07e608c1f363ea0b4394ff1fa1cbf50b43b14c880a5755f7f755" ;

  inner
    (Array.sub test_inputs 0 5)
    "533106a0980eff5b01f5ce63a6b0dd87328b318ac6aa600fc28b9a2ab9f88842" ;

  inner
    (Array.sub test_inputs 0 6)
    "1a815864684fff47c4d279ee4c31ad964c9dc232734e08188554fa27d33e6731" ;

  inner
    (Array.sub test_inputs 0 8)
    "a8b936d057df818048e634254719d13970df22926c51e5190c916fcf13dfa25a" ;

  inner
    (Array.sub test_inputs 0 10)
    "982934231a0410c86f9ed1daa46863a5ddae6d250670d27cb21d10739088e30b"

let () =
  Alcotest.run
    ~__FILE__
    "Poseidon252"
    [
      ( "Properties",
        [Alcotest.test_case "Perm is consistent" `Quick test_perm_is_consistent]
      );
      ( "Test vectors for Hades252",
        [
          Alcotest.test_case
            "Test vectors from dusk-network/hades252"
            `Quick
            test_vectors_hades252;
        ] );
      ( "Test vectors for Poseidon252",
        [
          Alcotest.test_case
            "Test vectors from dusk-network/poseidon252"
            `Quick
            test_vectors_poseidon252;
        ] );
    ]
