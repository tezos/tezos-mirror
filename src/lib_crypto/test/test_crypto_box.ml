(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

let check_bytes =
  Alcotest.testable (fun fmt x -> Hex.pp fmt (Hex.of_bytes x)) Bytes.equal

let (sk, pk, pkh) = Crypto_box.random_keypair ()

let zero_nonce = Crypto_box.zero_nonce

let chkey = Crypto_box.precompute sk pk

let test_check_pow () =
  let target = Crypto_box.make_target 2. in
  let pow = Crypto_box.generate_proof_of_work pk target in
  Alcotest.(check bool)
    "check_pow"
    (Crypto_box.check_proof_of_work pk pow target)
    true

let test_neuterize sk pk () =
  Alcotest.check
    (Alcotest.testable Crypto_box.pp_pk Crypto_box.equal)
    "neuterize"
    (Crypto_box.neuterize sk)
    pk

let test_hash pk pkh () =
  Alcotest.check
    (Alcotest.testable
       Crypto_box.Public_key_hash.pp
       Crypto_box.Public_key_hash.equal)
    "test_hash"
    (Crypto_box.hash pk)
    pkh

let test_fast_box_noalloc msg () =
  let buf = Bytes.copy msg in
  let tag = Bytes.make Crypto_box.tag_length '\x00' in
  (* encryption / decryption *)
  Crypto_box.fast_box_noalloc chkey zero_nonce tag buf ;
  assert (Crypto_box.fast_box_open_noalloc chkey zero_nonce tag buf) ;
  Alcotest.check check_bytes "test_fast_box_noalloc" buf msg

let test_fast_box msg () =
  let cmsg = Crypto_box.fast_box chkey zero_nonce msg in
  match Crypto_box.fast_box_open chkey zero_nonce cmsg with
  | Some decrypted_msg ->
      Alcotest.check check_bytes "test_fast_box" msg decrypted_msg
  | None ->
      Alcotest.fail "Box: Decryption error"

let tests =
  [ ("Neuterize Secret roundtrip", `Quick, test_neuterize sk pk);
    ("Public Key Hash roundtrip", `Quick, test_hash pk pkh);
    ("Check PoW", `Slow, test_check_pow);
    ( "HACL* box (noalloc)",
      `Quick,
      test_fast_box_noalloc (Bytes.of_string "test") );
    ("HACL* box", `Quick, test_fast_box (Bytes.of_string "test")) ]

let () = Alcotest.run "tezos-crypto" [("crypto_box", tests)]
