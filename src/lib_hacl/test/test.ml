(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

(** Testing
    -------
    Component:    Crypto
    Invocation:   dune exec src/lib_hacl/test/main.exe
    Subject:      Tests cryptobox primitives
*)

open Tezos_hacl

let init_bytes len = Bytes.make len '\x00'

let check msg b = Alcotest.(check bool) msg true b

(* this from hacl-star test suite *)
type 'a ed25519_test = {
  name : string;
  sk : 'a;
  pk : 'a;
  msg : 'a;
  expected_sig : 'a;
}

let ed25519_tests =
  [
    {
      name = "Test 1";
      sk =
        Bytes.of_string
          "\x9d\x61\xb1\x9d\xef\xfd\x5a\x60\xba\x84\x4a\xf4\x92\xec\x2c\xc4\x44\x49\xc5\x69\x7b\x32\x69\x19\x70\x3b\xac\x03\x1c\xae\x7f\x60";
      pk =
        Bytes.of_string
          "\xd7\x5a\x98\x01\x82\xb1\x0a\xb7\xd5\x4b\xfe\xd3\xc9\x64\x07\x3a\x0e\xe1\x72\xf3\xda\xa6\x23\x25\xaf\x02\x1a\x68\xf7\x07\x51\x1a";
      msg = Bytes.of_string "";
      expected_sig =
        Bytes.of_string
          "\xe5\x56\x43\x00\xc3\x60\xac\x72\x90\x86\xe2\xcc\x80\x6e\x82\x8a\x84\x87\x7f\x1e\xb8\xe5\xd9\x74\xd8\x73\xe0\x65\x22\x49\x01\x55\x5f\xb8\x82\x15\x90\xa3\x3b\xac\xc6\x1e\x39\x70\x1c\xf9\xb4\x6b\xd2\x5b\xf5\xf0\x59\x5b\xbe\x24\x65\x51\x41\x43\x8e\x7a\x10\x0b";
    };
  ]

type 'a curve25519_test = {name : string; sk : 'a; pk : 'a}

let curve25519_tests =
  [
    {
      name = "Test 1";
      sk =
        Bytes.of_string
          "\xe5\x21\x0f\x12\x78\x68\x11\xd3\xf4\xb7\x95\x9d\x05\x38\xae\x2c\x31\xdb\xe7\x10\x6f\xc0\x3c\x3e\xfc\x4c\xd5\x49\xc7\x15\xa4\x93";
      pk =
        Bytes.of_string
          "\x52\x19\x01\x0a\x3d\xd8\x97\x9f\xd8\x4a\x5b\xb9\x7f\x53\x6d\x78\xe7\x77\x5f\xe0\x36\x7b\x11\x20\x77\x71\x12\xd0\x0d\xc5\x45\x5c";
    };
  ]

type 'a blake2_test = {name : string; plaintext : 'a; key : 'a; expected : 'a}

let blake2b_tests =
  [
    {
      name = "Test 1";
      plaintext =
        Bytes.of_string
          "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f\x20\x21\x22\x23\x24\x25\x26\x27\x28\x29\x2a\x2b";
      key =
        Bytes.of_string
          "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f\x20\x21\x22\x23\x24\x25\x26\x27\x28\x29\x2a\x2b\x2c\x2d\x2e\x2f\x30\x31\x32\x33\x34\x35\x36\x37\x38\x39\x3a\x3b\x3c\x3d\x3e\x3f";
      expected =
        Bytes.of_string
          "\xc8\xf6\x8e\x69\x6e\xd2\x82\x42\xbf\x99\x7f\x5b\x3b\x34\x95\x95\x08\xe4\x2d\x61\x38\x10\xf1\xe2\xa4\x35\xc9\x6e\xd2\xff\x56\x0c\x70\x22\xf3\x61\xa9\x23\x4b\x98\x37\xfe\xee\x90\xbf\x47\x92\x2e\xe0\xfd\x5f\x8d\xdf\x82\x37\x18\xd8\x6d\x1e\x16\xc6\x09\x00\x71";
    };
  ]

type 'a hash_test = {name : string; plaintext : 'a; expected : 'a}

let sha2_256_tests =
  [
    {
      name = "SHA2_256 Test 1";
      plaintext = Bytes.of_string "\x61\x62\x63";
      expected =
        Bytes.of_string
          "\xba\x78\x16\xbf\x8f\x01\xcf\xea\x41\x41\x40\xde\x5d\xae\x22\x23\xb0\x03\x61\xa3\x96\x17\x7a\x9c\xb4\x10\xff\x61\xf2\x00\x15\xad";
    };
  ]

let sha2_512_tests =
  [
    {
      name = "SHA2_512 Test 1";
      plaintext = Bytes.of_string "\x61\x62\x63";
      expected =
        Bytes.of_string
          "\xdd\xaf\x35\xa1\x93\x61\x7a\xba\xcc\x41\x73\x49\xae\x20\x41\x31\x12\xe6\xfa\x4e\x89\xa9\x7e\xa2\x0a\x9e\xee\xe6\x4b\x55\xd3\x9a\x21\x92\x99\x2a\x27\x4f\xc1\xa8\x36\xba\x3c\x23\xa3\xfe\xeb\xbd\x45\x4d\x44\x23\x64\x3c\xe8\x0e\x2a\x9a\xc9\x4f\xa5\x4c\xa4\x9f";
    };
  ]

let sha3_256_tests =
  [
    {
      name = "SHA3_256 Test 1";
      plaintext = Bytes.of_string "\x61\x62\x63";
      expected =
        Bytes.of_string
          "\x3a\x98\x5d\xa7\x4f\xe2\x25\xb2\x04\x5c\x17\x2d\x6b\xd3\x90\xbd\x85\x5f\x08\x6e\x3e\x9d\x52\x5b\x46\xbf\xe2\x45\x11\x43\x15\x32";
    };
  ]

let sha3_512_tests =
  [
    {
      name = "SHA3_512 Test 1";
      plaintext = Bytes.of_string "\x61\x62\x63";
      expected =
        Bytes.of_string
          "\xb7\x51\x85\x0b\x1a\x57\x16\x8a\x56\x93\xcd\x92\x4b\x6b\x09\x6e\x08\xf6\x21\x82\x74\x44\xf7\x0d\x88\x4f\x5d\x02\x40\xd2\x71\x2e\x10\xe1\x16\xe9\x19\x2a\xf3\xc9\x1a\x7e\xc5\x76\x47\xe3\x93\x40\x57\x34\x0b\x4c\xf4\x08\xd5\xa5\x65\x92\xf8\x27\x4e\xec\x53\xf0";
    };
  ]

type 'a hmac_test = {name : string; key : 'a; data : 'a; expected : 'a}

let hmac_sha256_tests =
  [
    {
      name = "SHA2_256 Test 1";
      key = Bytes.of_string "\x4a\x65\x66\x65";
      data =
        Bytes.of_string
          "\x77\x68\x61\x74\x20\x64\x6f\x20\x79\x61\x20\x77\x61\x6e\x74\x20\x66\x6f\x72\x20\x6e\x6f\x74\x68\x69\x6e\x67\x3f";
      expected =
        Bytes.of_string
          "\x5b\xdc\xc1\x46\xbf\x60\x75\x4e\x6a\x04\x24\x26\x08\x95\x75\xc7\x5a\x00\x3f\x08\x9d\x27\x39\x83\x9d\xec\x58\xb9\x64\xec\x38\x43";
    };
  ]

let hmac_sha512_tests =
  [
    {
      name = "SHA2_512 Test 1";
      key = Bytes.of_string "\x4a\x65\x66\x65";
      data =
        Bytes.of_string
          "\x77\x68\x61\x74\x20\x64\x6f\x20\x79\x61\x20\x77\x61\x6e\x74\x20\x66\x6f\x72\x20\x6e\x6f\x74\x68\x69\x6e\x67\x3f";
      expected =
        Bytes.of_string
          "\x16\x4b\x7a\x7b\xfc\xf8\x19\xe2\xe3\x95\xfb\xe7\x3b\x56\xe0\xa3\x87\xbd\x64\x22\x2e\x83\x1f\xd6\x10\x27\x0c\xd7\xea\x25\x05\x54\x97\x58\xbf\x75\xc0\x5a\x99\x4a\x6d\x03\x4f\x65\xf8\xf0\xe6\xfd\xca\xea\xb1\xa3\x4d\x4a\x6b\x4b\x63\x6e\x07\x0a\x38\xbc\xe7\x37";
    };
  ]

type 'a secretbox_test = {
  name : string;
  key : 'a;
  n : 'a;
  pt : 'a;
  expected_ct : 'a;
}

let secretbox_tests =
  [
    {
      name = "Test 1";
      key =
        Bytes.of_string
          "\x82\x2b\xca\x3c\x7e\x05\xfd\xe0\xdc\x20\x45\x19\x73\x0b\x35\xf8\x12\x16\xa9\xc9\xf1\xdf\x95\x25\xe2\xa9\x00\xec\x89\x71\x8f\x57";
      n =
        Bytes.of_string
          "\x72\xb9\x02\x08\xd2\x80\x0e\x36\xad\x16\xc7\x30\x94\x1a\x03\x8d\x7c\x3a\xd9\xd8\x70\x30\xd3\x29";
      pt = Bytes.of_string "";
      expected_ct =
        Bytes.of_string
          "\x79\xb3\x45\x51\xed\x22\x4f\xa1\x7c\xb6\x46\x0c\xcb\x90\xa0\xd9";
    };
    {
      name = "Test 2";
      key =
        Bytes.of_string
          "\x49\xd1\x3a\x96\x6a\x76\x1a\x6a\xcf\xfe\xd8\x92\x3b\x5e\xe5\x0c\x29\x71\xba\x7e\x12\xc0\x1f\xd9\x9e\x0d\x70\xde\x91\x32\xf6\xd7";
      n =
        Bytes.of_string
          "\x66\x1d\x42\xc4\x8f\x71\xb3\x1c\x30\xd5\xc2\x65\xb1\x68\x51\x1a\xfd\x58\xb8\x70\xbf\x35\x4f\x4f";
      pt =
        Bytes.of_string
          "\xa9\x4d\x36\x5d\x0f\x3a\x0a\x50\x4c\x8c\x12\x76\x25\x31";
      expected_ct =
        Bytes.of_string
          "\x00\x21\xf4\x17\x24\xbe\x3a\xc9\xa4\x6b\xd5\x6c\x19\x64\x4d\x32\x0e\xb9\xcb\x66\x30\x3b\x98\xed\xa2\x9e\x22\x81\xed\x5e";
    };
  ]

type 'a box_test = {
  name : string;
  pk : 'a;
  sk : 'a;
  n : 'a;
  pt : 'a;
  expected_ct : 'a;
}

let box_tests =
  [
    {
      name = "Test 1";
      pk =
        Bytes.of_string
          "\xfe\x38\x04\x02\x70\x14\xcf\x0c\x89\x68\x11\xd5\x23\x40\x32\xe5\xeb\x6c\xa1\x78\x6f\x64\x8c\x64\x86\xf3\xfa\xdd\x26\x4d\x17\x41";
      sk =
        Bytes.of_string
          "\xd5\x7d\xff\xb8\x10\xeb\x32\xff\xaa\x87\x93\x24\x46\xa0\xc6\xe3\x6e\xb9\x54\xa7\x37\xe9\xcc\x3a\xc0\xd9\x80\x34\x41\xe2\xbe\xac";
      n = init_bytes 24;
      pt = Bytes.of_string "\x17\x8c\xb3\x11\xd2\x0f\x0a";
      expected_ct =
        Bytes.of_string
          "\x06\x2f\x0c\x61\x1b\x5a\xd3\x2d\xf8\xd4\x2f\xea\x32\x6e\xb9\xc5\xb9\x2a\xda\x4d\x98\xea\x08";
    };
    {
      name = "Test 2";
      pk =
        Bytes.of_string
          "\xc7\x99\xe1\xb6\xa1\x0c\x60\x9e\x44\x9b\xa3\x48\x38\xec\xc7\x94\x04\x98\x9c\x69\xac\xb3\x63\xd9\x52\x7f\x78\x66\xe5\x7b\xa6\x4a";
      sk =
        Bytes.of_string
          "\x07\xbb\x38\xf5\xb2\xdb\x98\xae\xe6\x02\x1b\x5e\xb1\xe8\x08\xe3\xe4\x67\x70\x20\xa2\x60\x9f\xa7\xd0\x89\xb5\x23\xc1\x35\xf5\xdf";
      n = init_bytes 24;
      pt = Bytes.of_string "\x93\xc6\x9d\x5b\xce\xea\xd5\x24\x03\x4e\x5c\x50";
      expected_ct =
        Bytes.of_string
          "\x77\x87\x8b\x2b\xce\x5a\xbf\x2f\xac\x5a\x14\xec\xd9\x58\x09\x68\xa6\x97\x6a\x8a\xf3\x41\x15\xce\x02\x3c\x95\x0e";
    };
  ]

type 'a p256_test = {
  name : string;
  sk : 'a;
  pk : 'a;
  msg : 'a;
  k : 'a;
  expected_sig : 'a;
}

let p256_tests =
  [
    {
      name = "Test 1";
      msg =
        Bytes.of_string
          "\x1c\xcb\xe9\x1c\x07\x5f\xc7\xf4\xf0\x33\xbf\xa2\x48\xdb\x8f\xcc\xd3\x56\x5d\xe9\x4b\xbf\xb1\x2f\x3c\x59\xff\x46\xc2\x71\xbf\x83";
      sk =
        Bytes.of_string
          "\x51\x9b\x42\x3d\x71\x5f\x8b\x58\x1f\x4f\xa8\xee\x59\xf4\x77\x1a\x5b\x44\xc8\x13\x0b\x4e\x3e\xac\xca\x54\xa5\x6d\xda\x72\xb4\x64";
      pk =
        Bytes.of_string
          "\x04\x1c\xcb\xe9\x1c\x07\x5f\xc7\xf4\xf0\x33\xbf\xa2\x48\xdb\x8f\xcc\xd3\x56\x5d\xe9\x4b\xbf\xb1\x2f\x3c\x59\xff\x46\xc2\x71\xbf\x83\xce\x40\x14\xc6\x88\x11\xf9\xa2\x1a\x1f\xdb\x2c\x0e\x61\x13\xe0\x6d\xb7\xca\x93\xb7\x40\x4e\x78\xdc\x7c\xcd\x5c\xa8\x9a\x4c\xa9";
      k =
        Bytes.of_string
          "\x94\xa1\xbb\xb1\x4b\x90\x6a\x61\xa2\x80\xf2\x45\xf9\xe9\x3c\x7f\x3b\x4a\x62\x47\x82\x4f\x5d\x33\xb9\x67\x07\x87\x64\x2a\x68\xde";
      expected_sig =
        Bytes.of_string
          "\xf3\xac\x80\x61\xb5\x14\x79\x5b\x88\x43\xe3\xd6\x62\x95\x27\xed\x2a\xfd\x6b\x1f\x6a\x55\x5a\x7a\xca\xbb\x5e\x6f\x79\xc8\xc2\xac\xcf\xa7\x40\xfe\xc7\x67\x96\xd2\xe3\x92\x16\xbe\x7e\xbf\x58\x0e\xa3\xc0\xef\x4b\xb0\x0a\xb2\xe7\xe4\x20\x84\x34\xf4\x5f\x8c\x9c";
    };
  ]

let test_ed25519 (v : Bytes.t ed25519_test) : unit =
  let pk1, sk1 = Hacl.Ed25519.keypair () in
  let pk2, _ = Hacl.Ed25519.keypair () in
  check "[Ed25519.keypair]" (pk1 <> pk2) ;
  check "[Ed25519.neuterize] 1/2" (pk1 = Hacl.Ed25519.neuterize sk1) ;
  let pk = Stdlib.Option.get @@ Hacl.Ed25519.pk_of_bytes v.pk in
  let sk = Stdlib.Option.get @@ Hacl.Ed25519.sk_of_bytes v.sk in
  check "[Ed25519.neuterize] 2/2" (pk = Hacl.Ed25519.neuterize sk) ;
  let signature = Hacl.Ed25519.sign ~sk ~msg:v.msg in
  check "[Ed25519.sign]" (Bytes.compare signature v.expected_sig = 0) ;
  check "[Ed25519.verify] 1/2" (Hacl.Ed25519.verify ~pk ~msg:v.msg ~signature) ;
  let random_pk =
    Stdlib.Option.get @@ Hacl.Ed25519.pk_of_bytes (Hacl.Rand.gen 32)
  in
  check
    "[Ed25519.verify] 2/2"
    (not (Hacl.Ed25519.verify ~pk:random_pk ~msg:v.msg ~signature))

let test_p256 (v : Bytes.t p256_test) : unit =
  let pk1, sk1 = Hacl.P256.keypair () in
  let pk2, _ = Hacl.P256.keypair () in
  check "[P256.keypair]" (pk1 <> pk2) ;
  check "[Hacl.P256.neuterize]" (pk1 = Hacl.P256.neuterize sk1) ;
  let pk = Stdlib.Option.get @@ Hacl.P256.pk_of_bytes v.pk in
  let pk_unsafe =
    Stdlib.Option.get @@ Hacl.P256.pk_of_bytes_without_validation v.pk
  in
  check "[P256.pk_of_bytes_without_validation]" (Hacl.P256.equal pk pk_unsafe) ;
  let sk = Stdlib.Option.get @@ Hacl.P256.sk_of_bytes v.sk in
  check "[P256.neuterize] 1/2" (pk = Hacl.P256.neuterize sk) ;
  let invalid_pk_uncompressed = Bytes.copy v.pk in
  Bytes.set_int8 invalid_pk_uncompressed 0 7 ;
  assert (
    Option.is_none
      (Hacl.P256.pk_of_bytes_without_validation invalid_pk_uncompressed)) ;
  let invalid_pk_compressed = Bytes.sub invalid_pk_uncompressed 0 33 in
  assert (
    Option.is_none
      (Hacl.P256.pk_of_bytes_without_validation invalid_pk_compressed)) ;
  let invalid_sk = Bytes.make Hacl.P256.sk_size '\x00' in
  check "[P256.invalid_sk]" (Option.is_none (Hacl.P256.sk_of_bytes invalid_sk)) ;
  let invalid_pk = Bytes.make Hacl.P256.pk_size '\x00' in
  check
    "[P256.neuterize] 2/2"
    (Option.is_none (Hacl.P256.pk_of_bytes invalid_pk)) ;
  let sk_bytes = Hacl.P256.to_bytes sk in
  check "[P256.to_bytes] 1/3" (sk_bytes = v.sk) ;
  (* for to_bytes pk we cannot directly compare the output buffer with the input
     buffer because of_bytes takes a pk in either compressed or uncompressed form
     v.pk is in uncompressed form but to_bytes only outputs the pk in compressed
     form; thus we only compare the relevant bytes *)
  let pk_bytes = Hacl.P256.to_bytes pk in
  check "[P256.to_bytes] 2/3" (Bytes.get_int8 pk_bytes 0 = 3) ;
  check "[P256.to_bytes] 3/3" (Bytes.sub pk_bytes 1 32 = Bytes.sub v.pk 1 32) ;
  let pk_bytes_blit = Bytes.create 33 in
  Hacl.P256.blit_to_bytes pk pk_bytes_blit ;
  check "[P256.blit_to_bytes]" (pk_bytes_blit = pk_bytes) ;
  check
    "[P256.verify] 1/2"
    (Hacl.P256.verify ~pk ~msg:v.msg ~signature:v.expected_sig) ;
  check
    "[P256.verify] 2/2"
    (not (Hacl.P256.verify ~pk ~msg:invalid_pk ~signature:v.expected_sig))

let test_curve25519 (v : Bytes.t curve25519_test) : unit =
  let sk = Hacl.Box.unsafe_sk_of_bytes v.sk in
  let pk = Hacl.Box.neuterize sk in
  check "[Curve25519]" (Hacl.Box.unsafe_to_bytes pk = v.pk)

let test_blake2b (v : Bytes.t blake2_test) : unit =
  let digest =
    Hacl.Blake2b.direct ~key:v.key v.plaintext (Bytes.length v.expected)
  in
  match digest with Hash d -> check "[Blake2b.blake2b]" (d = v.expected)

let test_sha2_256 (v : Bytes.t hash_test) : unit =
  let digest = Hacl.Hash.SHA256.digest v.plaintext in
  check "[SHA2_256.hash]" (digest = v.expected)

let test_sha2_512 (v : Bytes.t hash_test) : unit =
  let digest = Hacl.Hash.SHA512.digest v.plaintext in
  check "[SHA2_512.hash]" (digest = v.expected)

let test_sha3_256 (v : Bytes.t hash_test) : unit =
  let digest = Hacl.Hash.SHA3_256.digest v.plaintext in
  check "[SHA3_256.hash]" (digest = v.expected)

let test_sha3_512 (v : Bytes.t hash_test) : unit =
  let digest = Hacl.Hash.SHA3_512.digest v.plaintext in
  check "[SHA3_512.hash]" (digest = v.expected)

let test_keccak () : unit =
  (* from lib_crypto/test/hacl.ml *)
  let msg = Bytes.of_string "Longtemps, je me suis couche de bonne heure" in
  let expected =
    Bytes.of_string
      "\x9f\x3a\xfe\x7d\x35\xd9\xbb\xc4\xef\xd9\x82\x52\x35\x7e\x73\xe8\x5c\xe1\x23\x4a\x48\x60\x3a\x06\x3b\xb7\x07\x91\x74\xaa\xfa\x68"
  in
  let digest = Hacl.Hash.Keccak_256.digest msg in
  check "[SHA3.keccak]" (digest = expected)

let test_hmac_sha256 (v : Bytes.t hmac_test) : unit =
  let tag = Hacl.Hash.SHA256.HMAC.digest ~key:v.key ~msg:v.data in
  check "[HMAC_SHA2_256.digest] " (tag = v.expected)

let test_hmac_sha512 (v : Bytes.t hmac_test) : unit =
  let tag = Hacl.Hash.SHA512.HMAC.digest ~key:v.key ~msg:v.data in
  check "[HMAC_SHA2_512.digest]" (tag = v.expected)

let test_secretbox (v : Bytes.t secretbox_test) : unit =
  let key = Hacl.Secretbox.unsafe_of_bytes v.key in
  let ct = init_bytes (Bytes.length v.pt + 16) in
  Hacl.Secretbox.secretbox ~key ~nonce:v.n ~msg:v.pt ~cmsg:ct ;
  check ("[NaCl.Noalloc.Easy.secretbox] " ^ v.name) (ct = v.expected_ct) ;
  let pt = init_bytes (Bytes.length v.pt) in
  let b =
    Hacl.Secretbox.secretbox_open ~key ~nonce:v.n ~msg:pt ~cmsg:v.expected_ct
  in
  check ("[NaCl.Noalloc.Easy.secretbox_open] 1/2 " ^ v.name) (b && pt = v.pt) ;
  check
    ("[NaCl.Noalloc.Easy.secretbox_open] 2/2 " ^ v.name)
    (not
       (Hacl.Secretbox.secretbox_open
          ~key:(Hacl.Secretbox.unsafe_of_bytes (Bytes.make 32 '\x00'))
          ~nonce:v.n
          ~msg:pt
          ~cmsg:v.expected_ct))

let test_box (v : Bytes.t box_test) : unit =
  let k =
    Hacl.Box.dh
      (Hacl.Box.unsafe_pk_of_bytes v.pk)
      (Hacl.Box.unsafe_sk_of_bytes v.sk)
  in
  Alcotest.check_raises
    "[Hacl.Box.unsafe_sk_of_bytes 32]"
    (Failure "Error computing box_beforenm")
    (fun () ->
      ignore
        (Hacl.Box.dh
           (Hacl.Box.unsafe_pk_of_bytes (Bytes.make 32 '\x00'))
           (Hacl.Box.unsafe_sk_of_bytes (Bytes.make 32 '\x00')))) ;
  let cmsg = init_bytes (Bytes.length v.pt + 16) in
  Hacl.Box.box ~k ~nonce:v.n ~msg:v.pt ~cmsg ;
  check ("[NaCl.Noalloc.Easy.box_afternm] " ^ v.name) (cmsg = v.expected_ct) ;
  let msg = init_bytes (Bytes.length v.pt) in
  check
    ("[NaCl.Noalloc.Easy.box_open_afternm] 1/3 " ^ v.name)
    (Hacl.Box.box_open ~k ~nonce:v.n ~msg ~cmsg:v.expected_ct) ;
  check ("[NaCl.Noalloc.Easy.box_open_afternm] 2/3 " ^ v.name) (msg = v.pt) ;
  check
    ("[NaCl.Noalloc.Easy.box_open_afternm] 3/3 " ^ v.name)
    (not
       (Hacl.Box.box_open ~k ~nonce:(Hacl.Rand.gen 24) ~msg ~cmsg:v.expected_ct)) ;
  let buf = Bytes.copy v.pt in
  let tag = init_bytes 16 in
  Hacl.Box.box_noalloc ~k ~nonce:v.n ~tag ~buf ;
  let combined_ct =
    Bytes.of_string @@ Bytes.to_string tag ^ Bytes.to_string buf
  in
  check
    ("[NaCl.Noalloc.Detached.box_afternm] " ^ v.name)
    (combined_ct = v.expected_ct) ;
  check
    ("[NaCl.Noalloc.Detached.box_open_afternm] 1/2" ^ v.name)
    (Hacl.Box.box_open_noalloc ~k ~nonce:v.n ~tag ~buf) ;
  check "" (buf = v.pt) ;
  check
    ("[NaCl.Noalloc.Detached.box_open_afternm] 2/2" ^ v.name)
    (not (Hacl.Box.box_open_noalloc ~k ~nonce:(Hacl.Rand.gen 24) ~tag ~buf))

let tests =
  [
    ("Testing Ed25519", `Quick, fun () -> List.iter test_ed25519 ed25519_tests);
    ("Testing P256", `Quick, fun () -> List.iter test_p256 p256_tests);
    ( "Testing Curve25519",
      `Quick,
      fun () -> List.iter test_curve25519 curve25519_tests );
    ( "Testing SHA2_256",
      `Quick,
      fun () -> List.iter test_sha2_256 sha2_256_tests );
    ( "Testing SHA2_512",
      `Quick,
      fun () -> List.iter test_sha2_512 sha2_512_tests );
    ( "Testing SHA3_256",
      `Quick,
      fun () -> List.iter test_sha3_256 sha3_256_tests );
    ( "Testing SHA3_512",
      `Quick,
      fun () -> List.iter test_sha3_512 sha3_512_tests );
    ("Testing Keccak", `Quick, fun () -> test_keccak ());
    ( "Testing HMAC_SHA2_256",
      `Quick,
      fun () -> List.iter test_hmac_sha256 hmac_sha256_tests );
    ( "Testing HMAC_SHA2_512",
      `Quick,
      fun () -> List.iter test_hmac_sha512 hmac_sha512_tests );
    ("Testing Blake2b", `Quick, fun () -> List.iter test_blake2b blake2b_tests);
    ( "Testing NaCl.Secretbox",
      `Quick,
      fun () -> List.iter test_secretbox secretbox_tests );
    ("Testing NaCl.Box", `Quick, fun () -> List.iter test_box box_tests);
  ]

let () = Alcotest.run ~__FILE__ "hacl-test" [("tests", tests)]
