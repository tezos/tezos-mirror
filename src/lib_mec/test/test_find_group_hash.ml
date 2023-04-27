(*****************************************************************************)
(*                                                                           *)
(* Copyright (c) 2021 Danny Willems <be.danny.willems@gmail.com>             *)
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

open Mec.Protocol.Sapling
open Mec.Curve

let test_vectors () =
  let v =
    [
      (* Spending key:
         https://github.com/zcash/librustzcash/blob/da431a0eb207f69c9b0631d7d02136d819e1bfd9/zcash_primitives/src/constants.rs#L344
      *)
      ( "Zcash_G_",
        Bytes.empty,
        ( "0x0926d4f32059c712d418a7ff26753b6ad5b9a7d3ef8e282747bf46920a95a753",
          "0x57a1019e6de9b67553bb37d0c21cfd056d65674dcedbddbc305632adaaf2b530"
        ) );
      (* note commitment:
         https://github.com/zcash/librustzcash/blob/da431a0eb207f69c9b0631d7d02136d819e1bfd9/zcash_primitives/src/constants.rs#L312
      *)
      ( "Zcash_PH",
        Bytes.of_string "r",
        ( "0x26eb9f8a9ec72a8ca1409aa1f33bec2cf0919d06ffb1ecdaa5143b34a8e36462",
          "0x114b7501ad104c57949d77476e262c9596b78beafa9cc44cd4fc6365796c77ac"
        ) );
      (* value commitment value generator:
         https://github.com/zcash/librustzcash/blob/da431a0eb207f69c9b0631d7d02136d819e1bfd9/zcash_primitives/src/constants.rs#L328
      *)
      ( "Zcash_cv",
        Bytes.of_string "v",
        ( "0x273f910d9ecc1615d8618ed1d15fef4e9472c89ac043042d36183b2cb4d7ef51",
          "0x466a7e3a82f67ab1d32294fd89774ad6bc3332d0fa1ccd18a77a81f50667c8d7"
        ) );
      (* value commitment randomness generator:
         https://github.com/zcash/librustzcash/blob/da431a0eb207f69c9b0631d7d02136d819e1bfd9/zcash_primitives/src/constants.rs#L336
      *)
      ( "Zcash_cv",
        Bytes.of_string "r",
        ( "0x6800f4fa0f001cfc7ff6826ad58004b4d1d8da41af03744e3bce3b7793664337",
          "0x6d81d3a9cb45dedbe6fb2a6e1e22ab50ad46f1b0473b803b3caefab9380b6a8b"
        ) );
    ]
  in
  List.iter
    (fun (personalisation, bytes, (u, v)) ->
      let u = Jubjub.AffineEdwards.Base.of_string u in
      let v = Jubjub.AffineEdwards.Base.of_string v in
      let expected_p = Jubjub.AffineEdwards.from_coordinates_exn ~u ~v in
      let p =
        GroupHash.find_group_hash bytes (Bytes.of_string personalisation)
      in
      assert (Jubjub.AffineEdwards.eq p expected_p))
    v

let () =
  Alcotest.run
    ~__FILE__
    "Find group hash"
    [
      ( "Zcash test vectors",
        [Alcotest.test_case "Test vectors" `Quick test_vectors] );
    ]
