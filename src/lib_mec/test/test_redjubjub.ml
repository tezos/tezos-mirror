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

open Mec.Curve
open Mec.Signature

let rec repeat n f =
  if n <= 0 then
    let f () = () in
    f
  else (
    f () ;
    repeat (n - 1) f)

(*
  Generated using find_group_hash with the personalisation "Zcash_G_". Used by
  Zcash for the nullifiers
*)
let u, v =
  ( "0x0926d4f32059c712d418a7ff26753b6ad5b9a7d3ef8e282747bf46920a95a753",
    "0x57a1019e6de9b67553bb37d0c21cfd056d65674dcedbddbc305632adaaf2b530" )

let generator =
  Jubjub.AffineEdwards.from_coordinates_exn
    ~u:(Jubjub.AffineEdwards.Base.of_string u)
    ~v:(Jubjub.AffineEdwards.Base.of_string v)

module RedJubjub = RedJubjub.Make (struct
  let generator = generator
end)

let test_sign_and_verify_random_message () =
  let sk = Jubjub.AffineEdwards.Scalar.random () in
  let vk = Jubjub.AffineEdwards.(mul generator sk) in
  let message =
    Bytes.init (Random.int 100) (fun _ -> char_of_int (Random.int 256))
  in
  let signature = RedJubjub.sign sk message in
  assert (RedJubjub.verify vk message signature)

let test_sign_and_verify_wrong_message () =
  (* Sign a message and use the signature with a different message *)
  let sk = Jubjub.AffineEdwards.Scalar.random () in
  let vk = Jubjub.AffineEdwards.(mul generator sk) in
  let correct_message = Bytes.of_string "Correct message" in
  let incorrect_message = Bytes.of_string "Incorrect message" in
  let signature = RedJubjub.sign sk correct_message in
  assert (not (RedJubjub.verify vk incorrect_message signature))

let test_sign_and_verify_wrong_vk_because_of_different_sk () =
  (* Sign a message with a sk and attempt to verify with an invalid vk. The
     verifying key is computed using a different secret key, but the same generator
     than the scheme
  *)
  let sk = Jubjub.AffineEdwards.Scalar.random () in
  let vk =
    Jubjub.AffineEdwards.(mul generator (Jubjub.AffineEdwards.Scalar.random ()))
  in
  let message = Bytes.of_string "Message" in
  let signature = RedJubjub.sign sk message in
  assert (not (RedJubjub.verify vk message signature))

let test_sign_and_verify_wrong_vk_because_of_different_generator () =
  (* Sign a message with a sk and attempt to verify with an invalid vk. The vk
     is generated using the correct secret key but using a different generator than
     the one used in the scheme.
  *)
  let sk = Jubjub.AffineEdwards.Scalar.random () in
  let vk = Jubjub.AffineEdwards.(mul one sk) in
  let message = Bytes.of_string "Message" in
  let signature = RedJubjub.sign sk message in
  assert (not (RedJubjub.verify vk message signature))

let test_vectors () =
  (*
    Picked from https://github.com/zcash/librustzcash/blob/da431a0eb207f69c9b0631d7d02136d819e1bfd9/zcash_primitives/src/sapling/redjubjub.rs#L214
    The randomness can be printed using `println!("{:?}", t)` at line https://github.com/zcash/librustzcash/blob/da431a0eb207f69c9b0631d7d02136d819e1bfd9/zcash_primitives/src/sapling/redjubjub.rs#L78.
    The expected signature and the secret key can be printed using
    ```
    println!("SK: {:?}", sk_bytes);
    println!("SIG: {:?}", sig_bytes);
    ```
    at line https://github.com/zcash/librustzcash/blob/da431a0eb207f69c9b0631d7d02136d819e1bfd9/zcash_primitives/src/sapling/redjubjub.rs#L308.
    The first tuple comes from the 10th iteration (i = 9), and the second tuple comes
    from the first iteration (i = 0). Simply add a variable name in the for loop at line 296, and print for i = 0 and i = 9. Ignore the second part with [sk/vk/sig]_2.
  *)
  let vectors =
    [
      ( "Foo bar",
        "e705f4fd254312adc9c28d34433f1077111233aa6d6e3554b70135ae1a423403",
        "e733065e8fa3f5172cf389ba48990e0ae50af8650e34abdf44ac4e2a9cb4f454c533fef1b062737796cc00285ecda1d9b4adc1d9de324b359d2c011b9ed91dcca4616518b47dd47477d2a36614265044",
        ( "1de4252d8a94cd363901802d944a267a27aabd81b5ee9205810a14e9b353c04f",
          "689a85c808017250ad3187fe2316917378ae7a171433cb0a21dbf8af18e88d09" )
      );
      ( "Foo bar",
        "7a202b25feb063be0410b9df8cc8f9a949f7de6b1cbe08527d4a5f64638afe0a",
        "0adf4bd75fac61fd430021e782921106c5ae2b8f2be8d87f8bdc149089c91b1a891194c80021b570195b7346df85e08253d490ea2055f433d46b83ef86c5cb69a0b6950567be5994f3ba70608ea4a057",
        ( "18391a03cd8bcb8b9fbc1165cfed61fd3f4d953f9e213380f1ef9c12b1e12834",
          "99d380bbbc9d99c8ec109ce5fc12595fedc2a193751d7e69b87974fa661ae108" )
      );
    ]
  in
  List.iter
    (fun (msg, sk, randomness, (rbar, sbar)) ->
      let msg = Bytes.of_string msg in
      let sk = Hex.to_bytes (`Hex sk) in
      let sk = Jubjub.AffineEdwards.Scalar.of_bytes_exn sk in
      (* reconstruct expected signature *)
      let rbar = Hex.to_bytes (`Hex rbar) in
      let sbar = Hex.to_bytes (`Hex sbar) in
      let expected_signature = Bytes.concat Bytes.empty [rbar; sbar] in
      let randomness = Hex.to_bytes (`Hex randomness) in
      (* Compute the signature given by the library *)
      let signature = RedJubjub.sign_deterministic randomness sk msg in
      let signature_bytes = RedJubjub.signature_to_bytes signature in
      (* Verify the computed signature is the same than the Rust reference
         implementation
      *)
      if not (Bytes.equal signature_bytes expected_signature) then
        Alcotest.failf
          "Expected signature is %s, computed %s"
          Hex.(show (of_bytes expected_signature))
          Hex.(show (of_bytes signature_bytes)) ;
      (* Let's verify at the same time *)
      assert (
        RedJubjub.verify (Jubjub.AffineEdwards.mul generator sk) msg signature))
    vectors

let () =
  let open Alcotest in
  run
    ~__FILE__
    "RedJubjub"
    [
      ( "Signature scheme properties",
        [
          Alcotest.test_case
            "Sign and verify random message"
            `Quick
            (repeat 100 test_sign_and_verify_random_message);
          Alcotest.test_case
            "Sign and verify wrong message"
            `Quick
            (repeat 100 test_sign_and_verify_wrong_message);
          Alcotest.test_case
            "sign and verify with an invalid verifying key (different sk)"
            `Quick
            (repeat 100 test_sign_and_verify_wrong_vk_because_of_different_sk);
          Alcotest.test_case
            "sign and verify with an invalid verifying key (different \
             generator)"
            `Quick
            (repeat
               100
               test_sign_and_verify_wrong_vk_because_of_different_generator);
        ] );
      ( "Sign on test vectors",
        [
          Alcotest.test_case
            "Sign on test vectors from librustzcash/zcash_primitives"
            `Quick
            test_vectors;
        ] );
    ]
