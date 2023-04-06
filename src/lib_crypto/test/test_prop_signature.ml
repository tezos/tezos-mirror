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
    Invocation:   dune exec src/lib_crypto/test/main.exe
    Subject:      Property-tests over the interface S.SIGNATURE and its
                  instantiations.
*)
open Qcheck2_helpers

open QCheck2

module type SIGNATURE = sig
  include Intfs.SIGNATURE

  val watermark_of_bytes : bytes -> watermark
end

let gen_watermark =
  let open Gen in
  Gen.char >|= Bytes.make 1

module Signature_Properties (Desc : sig
  val name : string
end)
(X : SIGNATURE) =
struct
  (** Tests that a signature of [s], with optional [watermark], by a generated
      key and [X.sign] is accepted by [X.check] with the same key.  *)
  let test_prop_sign_check (s, watermark) =
    let _, pk, sk = X.generate_key () in
    let data = Bytes.of_string s in
    let watermark = Option.map X.watermark_of_bytes watermark in
    let signed = X.sign ?watermark sk data in
    X.check ?watermark pk signed data

  let gen =
    let open Gen in
    let+ msg = string and+ wm1 = gen_watermark |> option in
    (msg, wm1)

  let test_prop_sign_check =
    Test.make
      ~name:(Desc.name ^ "_sign_check")
      ~print:Print.(pair string (option Bytes.unsafe_to_string))
      gen
      test_prop_sign_check

  let tests = [test_prop_sign_check]
end

module type AGGREGATE_SIGNATURE = sig
  include Intfs.AGGREGATE_SIGNATURE

  val watermark_of_bytes : bytes -> watermark
end

module Aggregate_Signature_Properties (Desc : sig
  val name : string
end)
(X : AGGREGATE_SIGNATURE) =
struct
  (** Tests that signatures of [msg1], [msg2], [msg3], (with optional
      corresponding watermarks) obtained using [X.sign] are accepted by
      [X.check] when using the corresponding key. It then tests that the
      aggregation of all these signatures obtained using
      [X.aggregate_signature_opt] is accepted by [X.aggregate_check]. *)
  let test_prop_sign_check
      ( (seed1, msg1, watermark1),
        (seed2, msg2, watermark2),
        (seed3, msg3, watermark3) ) =
    let _, pk1, sk1 = X.generate_key ~seed:seed1 () in
    let _, pk2, sk2 = X.generate_key ~seed:seed2 () in
    let _, pk3, sk3 = X.generate_key ~seed:seed3 () in
    let watermark1 = Option.map X.watermark_of_bytes watermark1 in
    let watermark2 = Option.map X.watermark_of_bytes watermark2 in
    let watermark3 = Option.map X.watermark_of_bytes watermark3 in
    let signed1 = X.sign ?watermark:watermark1 sk1 msg1 in
    let signed2 = X.sign ?watermark:watermark2 sk2 msg2 in
    let signed3 = X.sign ?watermark:watermark3 sk3 msg3 in
    let is_valid_aggregated_sign =
      X.aggregate_signature_opt [signed1; signed2; signed3] |> function
      | None -> false
      | Some s ->
          X.aggregate_check
            [
              (pk1, watermark1, msg1);
              (pk2, watermark2, msg2);
              (pk3, watermark3, msg3);
            ]
            s
    in
    X.check ?watermark:watermark1 pk1 signed1 msg1
    && X.check ?watermark:watermark2 pk2 signed2 msg2
    && X.check ?watermark:watermark3 pk3 signed3 msg3
    && is_valid_aggregated_sign

  let test_prop_sign_check =
    let gen =
      let open Gen in
      let+ seed1 = string_size (pure 32) >|= Bytes.of_string
      and+ seed2 = string_size (pure 32) >|= Bytes.of_string
      and+ seed3 = string_size (pure 32) >|= Bytes.of_string
      and+ msg1 = string >|= Bytes.of_string
      and+ msg2 = string >|= Bytes.of_string
      and+ msg3 = string >|= Bytes.of_string
      and+ wm1 = gen_watermark |> option
      and+ wm2 = gen_watermark |> option
      and+ wm3 = gen_watermark |> option in
      ((seed1, msg1, wm1), (seed2, msg2, wm2), (seed3, msg3, wm3))
    in
    let print_param =
      Print.(
        triple
          Bytes.unsafe_to_string
          Bytes.unsafe_to_string
          (option Bytes.unsafe_to_string))
    in
    Test.make
      ~name:("Aggregate_signature_" ^ Desc.name ^ "_sign_check")
      ~print:(Print.triple print_param print_param print_param)
      gen
      test_prop_sign_check

  let tests = [test_prop_sign_check]
end

(** Test: instantiate Signature_Properties over Signature
    with algo in generate key respectively set to
    Ed25519, Secp256k1, P256, Bls. *)
let () =
  let module Bls_Props =
    Aggregate_Signature_Properties
      (struct
        let name = "Bls12_381"
      end)
      (struct
        include Signature.Bls

        let watermark_of_bytes b = b
      end)
  in
  let f (algo, name) =
    let module X = struct
      include Signature

      let generate_key ?seed () = generate_key ~algo ?seed ()

      let watermark_of_bytes b = Custom b
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
  @ List.map
      f
      [
        (Ed25519, "Ed25519");
        (Secp256k1, "Secp256k1");
        (P256, "P256");
        (Bls, "Bls");
      ]
  |> Alcotest.run ~__FILE__ "tezos-crypto-prop-signature"
