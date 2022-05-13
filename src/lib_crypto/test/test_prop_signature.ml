(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    Component:    Crypto
    Invocation:   dune build @src/lib_crypto/runtest
    Subject:      Property-tests over the interface S.SIGNATURE and its
                  instantiations Ed25519 and Secp256k1.
*)
open Lib_test.Qcheck2_helpers

open QCheck2

module Signature_Properties (Desc : sig
  val name : string
end)
(X : S.SIGNATURE) =
struct
  (** Tests that a signature of [s] by a generated key and [X.sign] is
      accepted by [X.check] with the same key.  *)
  let test_prop_sign_check (s : string) =
    let _, pk, sk = X.generate_key () in
    let data = Bytes.of_string s in
    let signed = X.sign sk data in
    X.check pk signed data

  let test_prop_sign_check =
    Test.make
      ~name:(Desc.name ^ "_sign_check")
      ~print:Print.string
      Gen.string
      test_prop_sign_check

  let tests = [test_prop_sign_check]
end

module Aggregate_Signature_Properties (Desc : sig
  val name : string
end)
(X : S.AGGREGATE_SIGNATURE) =
struct
  (** Tests that signatures of [s] obtained using [X.sign] are accepted by
      [X.check] when using the corresponding key. It then tests that the
      aggregation of all these signatures obtained using
      [X.aggregate_signature_opt] is accepted by [X.aggregate_check]. *)
  let test_prop_sign_check ((seed1, msg1), (seed2, msg2), (seed3, msg3)) =
    let _, pk1, sk1 = X.generate_key ~seed:seed1 () in
    let _, pk2, sk2 = X.generate_key ~seed:seed2 () in
    let _, pk3, sk3 = X.generate_key ~seed:seed3 () in
    let signed1 = X.sign sk1 msg1 in
    let signed2 = X.sign sk2 msg2 in
    let signed3 = X.sign sk3 msg3 in
    let is_valid_aggregated_sign =
      X.aggregate_signature_opt [signed1; signed2; signed3] |> function
      | None -> false
      | Some s -> X.aggregate_check [(pk1, msg1); (pk2, msg2); (pk3, msg3)] s
    in
    X.check pk1 signed1 msg1 && X.check pk2 signed2 msg2
    && X.check pk3 signed3 msg3 && is_valid_aggregated_sign

  let test_prop_sign_check =
    let gen =
      let open Gen in
      let+ seed1 = string_size (pure 32) >|= Bytes.of_string
      and+ seed2 = string_size (pure 32) >|= Bytes.of_string
      and+ seed3 = string_size (pure 32) >|= Bytes.of_string
      and+ msg1 = string >|= Bytes.of_string
      and+ msg2 = string >|= Bytes.of_string
      and+ msg3 = string >|= Bytes.of_string in
      ((seed1, msg1), (seed2, msg2), (seed3, msg3))
    in
    Test.make
      ~name:("Aggregate_signature_" ^ Desc.name ^ "_sign_check")
      ~print:
        Print.(
          triple
            (pair Bytes.unsafe_to_string Bytes.unsafe_to_string)
            (pair Bytes.unsafe_to_string Bytes.unsafe_to_string)
            (pair Bytes.unsafe_to_string Bytes.unsafe_to_string))
      gen
      test_prop_sign_check

  let tests = [test_prop_sign_check]
end

(** Test: instantiate Signature_Properties over Signature
    with algo in generate key respectively set to
    Ed25519, Secp256k1, P256. *)
let () =
  let module Bls_Props =
    Aggregate_Signature_Properties
      (struct
        let name = "Bls12_381"
      end)
      (Bls)
  in
  let f (algo, name) =
    let module X = struct
      include Signature

      let generate_key ?seed () = generate_key ~algo ?seed ()
    end in
    let module XProps =
      Signature_Properties
        (struct
          let name = "Signature_" ^ name
        end)
        (X)
    in
    (name, qcheck_wrap XProps.tests)
  in

  [("bls12_381", qcheck_wrap Bls_Props.tests)]
  @ List.map f [(Ed25519, "Ed25519"); (Secp256k1, "Secp256k1"); (P256, "P256")]
  |> Alcotest.run "tezos-crypto-prop-signature"
