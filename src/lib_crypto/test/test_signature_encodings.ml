(*****************************************************************************)
(*                                                                           *)
(* Copyright (c) 2020-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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
    Dependencies: src/lib_crypto/test/roundtrips.ml
    Subject:      Checking Base58 key encodings
*)

module type B58CHECK = sig
  type t

  val pp : Format.formatter -> t -> unit

  include Intfs.B58_DATA with type t := t
end

module Make_tests
    (P : Intfs.SIGNATURE) (T : sig
      val name : string

      val pkh_vectors : (string * string) list

      val key_encoding_vectors : (string * string * string * string) list
    end) =
struct
  let test_b58check_roundtrip :
      type t. (module B58CHECK with type t = t) -> string -> t -> unit =
   fun m msg input ->
    let module M = (val m) in
    let testable =
      Alcotest.testable M.pp (fun a b -> M.to_b58check a = M.to_b58check b)
    in
    Roundtrips.test_rt_opt
      ("b58check." ^ msg)
      testable
      M.to_b58check
      M.of_b58check_opt
      input

  (** Check the Base58 roundtrip (encoding then decoding) for public key hashes,
      public keys, and secret keys. *)
  let test_b58check_roundtrips () =
    let pubkey_hash, pubkey, seckey = P.generate_key () in
    test_b58check_roundtrip (module P.Public_key_hash) "pubkey_hash" pubkey_hash ;
    test_b58check_roundtrip (module P.Public_key) "pubkey" pubkey ;
    test_b58check_roundtrip (module P.Secret_key) "seckey" seckey

  let test_b58check_invalid input =
    Roundtrips.test_decode_opt_fail
      "b58check"
      (Alcotest.testable P.Public_key_hash.pp P.Public_key_hash.( = ))
      P.Public_key_hash.of_b58check_opt
      input

  (** Test with invalid values, mostly random bytes and wrong
      sizes, which are not accepted as Base58 encodings for keys. *)
  let test_b58check_invalids () =
    List.iter
      test_b58check_invalid
      [
        "ThisIsGarbageNotACheck";
        "\x00";
        String.make 1000 '\x00';
        String.make 2048 'a';
        String.init 2048 (fun _ -> Char.chr (Random.int 256));
        "";
      ]

  let of_hex hex = Cstruct.(to_bytes (of_hex hex))

  (** Check the Base58 encodings of public key hashes on test vectors. *)
  let test_pkh_encodings () =
    let test_encoded_pkh (input, test) =
      let encoding = P.Public_key_hash.b58check_encoding in
      let input = of_hex input in
      let pkh = P.Public_key_hash.of_bytes_exn input in
      let encoded = Base58.simple_encode encoding pkh in
      assert (String.equal encoded test)
    in
    List.iter test_encoded_pkh T.pkh_vectors

  (** Generate a key pair using the seed from a test vector, then check that
      the Base58 encodings for the public key hash, the public key, and
      the secret key match the test vector. *)
  let test_key_encodings () =
    let test_encoded_key (seed, pkh_b58, pk_b58, sk_b58) =
      let seed = of_hex seed in
      let pkh_test, pk_test, sk_test = P.generate_key ~seed () in
      let pkh_test =
        Base58.simple_encode P.Public_key_hash.b58check_encoding pkh_test
      in
      let pk_test =
        Base58.simple_encode P.Public_key.b58check_encoding pk_test
      in
      let sk_test =
        Base58.simple_encode P.Secret_key.b58check_encoding sk_test
      in
      assert (String.equal pkh_test pkh_b58) ;
      assert (String.equal pk_test pk_b58) ;
      assert (String.equal sk_test sk_b58)
    in
    List.iter test_encoded_key T.key_encoding_vectors

  let tests =
    [
      ( T.name ^ "-encodings",
        [
          ("b58check.roundtrip", `Quick, test_b58check_roundtrips);
          ("b58check.invalid", `Slow, test_b58check_invalids);
          ("b58 pkh encodings", `Slow, test_pkh_encodings);
          ("b58 key encodings", `Slow, test_key_encodings);
        ] );
    ]
end

module Test_Ed25519 =
  Make_tests
    (Signature.Ed25519)
    (struct
      let name = "ed25519"

      let pkh_vectors = Key_encoding_vectors.ed25519_pkhs

      let key_encoding_vectors = Key_encoding_vectors.ed25519_key_encodings
    end)

module Test_Secp256k1 =
  Make_tests
    (Signature.Secp256k1)
    (struct
      let name = "secp256k1"

      let pkh_vectors = Key_encoding_vectors.secp256k1_pkhs

      let key_encoding_vectors = Key_encoding_vectors.secp256k1_key_encodings
    end)

module Test_P256 =
  Make_tests
    (Signature.P256)
    (struct
      let name = "p256"

      let pkh_vectors = Key_encoding_vectors.p256_pkhs

      let key_encoding_vectors = Key_encoding_vectors.p256_key_encodings
    end)

module Test_Bls =
  Make_tests
    (Signature.Bls)
    (struct
      let name = "bls12_381"

      let pkh_vectors = Key_encoding_vectors.bls12_381_pkhs

      let key_encoding_vectors = Key_encoding_vectors.bls12_381_key_encodings
    end)

let tests =
  Test_Ed25519.tests @ Test_Secp256k1.tests @ Test_P256.tests @ Test_Bls.tests
