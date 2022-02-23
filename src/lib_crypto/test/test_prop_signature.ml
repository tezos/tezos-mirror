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
    let (_, pk, sk) = X.generate_key () in
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

module Ed25519_Props =
  Signature_Properties
    (struct
      let name = "Ed25519"
    end)
    (Ed25519)

module P256_Props =
  Signature_Properties
    (struct
      let name = "P256"
    end)
    (P256)

module Secp256k1_Props =
  Signature_Properties
    (struct
      let name = "Secp256k1"
    end)
    (Secp256k1)

(** Test: instantiate Signature_Properties over Signature
    with algo in generate key respectively set to
    Ed25519, Secp256k1, P256. *)
let () =
  let open Signature in
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
  List.map f [(Ed25519, "Ed25519"); (Secp256k1, "Secp256k1"); (P256, "P256")]
  |> Alcotest.run "tezos-crypto-prop-signature"
