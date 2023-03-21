(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
    Subject:      Roundtrips for functions built on the HACL* NaCl API.
*)

let sk, pk, pkh = Crypto_box.random_keypair ()

let zero_nonce = Crypto_box.zero_nonce

let chkey = Crypto_box.precompute sk pk

(** The test defines a proof-of-work target, generates a proof-of-work
    for that target, and then verifies it the proof of work is accepted
    by [Crypto_box.check_proof_of_work].
*)
let test_check_pow () =
  let target = Crypto_box.make_pow_target 2. in
  let pow =
    Crypto_box.For_testing_only.generate_proof_of_work_n_attempts
      ~max:1000
      pk
      target
  in
  Alcotest.(check bool)
    "check_pow"
    (Crypto_box.check_proof_of_work pk pow target)
    true

(** Checks the neuterize function, i.e. the [pk] corresponds to the
    generated public key from secret key [sk].
*)
let test_neuterize sk pk () =
  Alcotest.check
    (Alcotest.testable Crypto_box.pp_pk Crypto_box.equal)
    "neuterize"
    (Crypto_box.neuterize sk)
    pk

(** Checks that the [pkh] corresponds to the generated hash of the
    public key [pk].
*)
let test_hash pk pkh () =
  Alcotest.check
    (Alcotest.testable
       Crypto_box.Public_key_hash.pp
       Crypto_box.Public_key_hash.equal)
    "test_hash"
    (Crypto_box.hash pk)
    pkh

(** Encrypts then decrypts the message [msg] with authentication
    (with side-effects, in-place changes).
*)
let test_fast_box_noalloc msg () =
  let buf = Bytes.copy msg in
  let tag = Bytes.make Crypto_box.tag_length '\x00' in
  (* encryption / decryption *)
  Crypto_box.fast_box_noalloc chkey zero_nonce tag buf ;
  Alcotest.(
    check
      bool
      "fast_box_open"
      true
      (Crypto_box.fast_box_open_noalloc chkey zero_nonce tag buf)) ;
  Alcotest.(check bytes "test_fast_box_noalloc" buf msg)

(** Encrypts then decrypts the message [msg] with authentication.
    Returns a new buffer for ciphertext.
*)
let test_fast_box msg () =
  let cmsg = Crypto_box.fast_box chkey zero_nonce msg in
  match Crypto_box.fast_box_open chkey zero_nonce cmsg with
  | Some decrypted_msg ->
      Alcotest.(check bytes "test_fast_box" msg decrypted_msg)
  | None -> Alcotest.fail "Box: Decryption error"

let tests =
  [
    ( "crypto_box",
      [
        ("Neuterize Secret roundtrip", `Quick, test_neuterize sk pk);
        ("Public Key Hash roundtrip", `Quick, test_hash pk pkh);
        ( "HACL* box (noalloc)",
          `Quick,
          test_fast_box_noalloc (Bytes.of_string "test") );
        ("HACL* box", `Quick, test_fast_box (Bytes.of_string "test"));
        ("Check PoW", `Slow, test_check_pow);
      ] );
  ]
