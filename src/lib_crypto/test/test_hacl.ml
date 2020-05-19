(* The MIT License (MIT)
 *
 *   Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>
 *
 *   Permission is hereby granted, free of charge, to any person obtaining a copy
 *   of this software and associated documentation files (the "Software"), to deal
 *   in the Software without restriction, including without limitation the rights
 *   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *   copies of the Software, and to permit persons to whom the Software is
 *   furnished to do so, subject to the following conditions:
 *
 *   The above copyright notice and this permission notice shall be included in all
 *   copies or substantial portions of the Software.
 *
 *   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 *   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 *   SOFTWARE. *)

open Hacl

let hex s = Hex.to_bytes (`Hex s)

let check_bytes =
  Alcotest.testable (fun fmt x -> Hex.pp fmt (Hex.of_bytes x)) Bytes.equal

type vector = {
  data_in : Bytes.t list;
  data_key : Bytes.t option;
  data_out : Bytes.t;
}

let vectors =
  [ {
      data_in =
        [ hex
            "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f"
        ];
      data_key = None;
      data_out =
        hex
          "1c077e279de6548523502b6df800ffdab5e2c3e9442eb838f58c295f3b147cef9d701c41c321283f00c71affa0619310399126295b78dd4d1a74572ef9ed5135";
    };
    {
      data_in =
        [ hex "000102030405060708090a0b0c0d0e0f101112131415";
          hex "161718";
          hex
            "191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f"
        ];
      data_key = None;
      data_out =
        hex
          "1c077e279de6548523502b6df800ffdab5e2c3e9442eb838f58c295f3b147cef9d701c41c321283f00c71affa0619310399126295b78dd4d1a74572ef9ed5135";
    };
    {
      data_in =
        [ hex
            "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9fa0a1a2a3a4a5a6a7a8a9aaabacadaeafb0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedfe0e1e2e3e4e5e6e7e8e9eaebecedeeeff0f1f2f3"
        ];
      data_key =
        Some
          (hex
             "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f");
      data_out =
        hex
          "b39614268fdd8781515e2cfebf89b4d5402bab10c226e6344e6b9ae000fb0d6c79cb2f3ec80e80eaeb1980d2f8698916bd2e9f747236655116649cd3ca23a837";
    } ]

let msg = Bytes.of_string "Longtemps, je me suis couche de bonne heure"

let msglen = Bytes.length msg

let of_hex hex = Cstruct.(to_bytes (of_hex hex))

let randmsg =
  of_hex
    "12c0c5a283401a81163dfd645e57ef6ff58b2f877c4e2d4add10345ec80bef3ffc720060c82e4288a20eccf99d64f18223edb30069fa76de9fe9ae8f875f3a3f75f91dd625652632869766839075e88afc852918da3445bca6d428a4f55d98366065fc70e0306fc6c84ec9e8d1325cc63ba09d5803383d0be40bd7ace7e7551615e4267f94630a0ad62cf798b4a7648390547a3616f42d8b8e58d7223f3c07826670209601be0ef2ea60e662c34b21113680141bead22e8b31015d7fe1a6617101036f03050d8b6854989bdfc13efaa6b2e1960c291f91da346911b1d46f20242bb1eb16f4104f9d684ed0dfca8e13e46b47ba9c39513f5e0746dd828f43da416e10341f3b169691ee823a53500f1ef00c6a52c3f4ecb42f68e1894785d4d192079cc8e53be8bb4ca1e000553504d6132e95490a4b477baaddca598f8947b20fbf732ac608830fb4b11c3cd1e19257e8cb00a22a8fc54ad6e47960086cd5ed24451c1f2ac2cda4514e6e1118ffabd74e7aae3514f3e5d40443ed94bdbbf7af5fa737d2da3b19cac58ca24539313a545164c20c4fae74d01fcb535d4414885ee50cdbb5ff1fcd465fc0c0a0c0f0ebc62687569bd5d36774a6a9c8d9e05b33ac30f13fdd7906aebd27dfd2ee19616a6f3694f2539b89b9ce6d73396816202700f50617f26a7134a6819fe808775bff75df240102fb0352f67eb97e022f66d40403"

let randmsg_len = Bytes.length randmsg

let sha256 () =
  let open Hash.SHA256 in
  let resp =
    of_hex "d8d219deae87c5e5fffbc0a0a38986de266427b2e08be9f9d22a07b91099dbfe"
  in
  let randresp =
    of_hex "9f043732d7117fa402d24e7343108976524b097390b0b160df42b0fa5bc6425c"
  in
  let st = init () in
  Printf.printf "Init done\n" ;
  update st msg ;
  print_endline "Update done." ;
  let d = finish st in
  Printf.printf "Digest size %d\n" (Bytes.length d) ;
  print_endline "Finish done." ;
  Alcotest.(check check_bytes "sha256" resp d) ;
  let d = digest msg in
  print_endline "Direct hash done." ;
  Alcotest.(check check_bytes "sha256" resp d) ;
  let st = init () in
  Printf.printf "Init done\n" ;
  update st randmsg ;
  print_endline "Update done." ;
  let d = finish st in
  Printf.printf "Digest size %d\n" (Bytes.length d) ;
  print_endline "Finish done." ;
  Alcotest.(check check_bytes "sha256" randresp d) ;
  let d = digest randmsg in
  print_endline "Direct hash done." ;
  Alcotest.(check check_bytes "sha256" randresp d)

let sha256_seq () =
  let open Hash.SHA256 in
  let bothresp =
    of_hex "ddabe6c4552e944d927bd0b03dd5ab95ecdfa5a135b6c3b60416dbde57b38416"
  in
  let st = init () in
  Printf.printf "Init done\n" ;
  update st msg ;
  update st randmsg ;
  print_endline "Update done." ;
  let d = finish st in
  Printf.printf "Digest size %d\n" (Bytes.length d) ;
  print_endline "Finish done." ;
  Alcotest.(check check_bytes "sha256_seq" bothresp d)

let sha512 () =
  let resp =
    of_hex
      "b4686c597b48c05f9d79f933611343e00985967c16f81a301269c75065ed05067dc547b1b36ec1822cc19a78df691e30fdb739ffb6b2a0ea6533ff20a2202e51"
  in
  let digest = Hash.SHA512.digest msg in
  Alcotest.(check check_bytes "sha512" resp digest)

let hmac_sha256 () =
  let key = Bytes.unsafe_of_string "key" in
  let msg =
    Bytes.unsafe_of_string "The quick brown fox jumps over the lazy dog"
  in
  let resp =
    of_hex "f7bc83f430538424b13298e6aa6fb143ef4d59a14946175997479dbc2d1a3cd8"
  in
  let digest = Hash.SHA256.HMAC.digest ~key ~msg in
  Alcotest.(check check_bytes "hmac_sha256" resp digest)

let hmac_sha512 () =
  let vectors =
    [ ( Bytes.unsafe_of_string "key",
        Bytes.unsafe_of_string "The quick brown fox jumps over the lazy dog",
        of_hex
          "b42af09057bac1e2d41708e48a902e09b5ff7f12ab428a4fe86653c73dd248fb82f948a549f7b791a5b41915ee4d1ec3935357e4e2317250d0372afa2ebeeb3a"
      );
      ( Bytes.empty,
        Bytes.empty,
        of_hex
          "b936cee86c9f87aa5d3c6f2e84cb5a4239a5fe50480a6ec66b70ab5b1f4ac6730c6c515421b327ec1d69402e53dfb49ad7381eb067b338fd7b0cb22247225d47"
      ) ]
  in
  List.iter
    (fun (key, msg, resp) ->
      let digest = Hash.SHA512.HMAC.digest ~key ~msg in
      Alcotest.(check check_bytes "hmac_sha512" resp digest))
    vectors

let hash =
  [ ("hmac_sha256", `Quick, hmac_sha256);
    ("hmac_sha512", `Quick, hmac_sha512);
    ("sha256", `Quick, sha256);
    ("sha256_seq", `Quick, sha256_seq);
    ("sha512", `Quick, sha512) ]

let test_blake2b_direct {data_in; data_key; data_out} =
  let (Blake2b.Hash h) =
    Blake2b.direct
      ?key:data_key
      (Bytes.concat Bytes.empty data_in)
      (Bytes.length data_out)
  in
  assert (Bytes.(equal data_out h))

let blake2b_tests =
  List.mapi
    (fun i v -> (string_of_int i, `Quick, fun () -> test_blake2b_direct v))
    vectors

let secretbox () =
  let open Secretbox in
  let key = genkey () in
  let nonce = Nonce.gen () in
  let cmsg = Bytes.create (msglen + 16) in
  secretbox ~key ~nonce ~msg ~cmsg ;
  let decrypted_msg = Bytes.create msglen in
  assert (secretbox_open ~key ~nonce ~cmsg ~msg:decrypted_msg) ;
  Alcotest.check check_bytes "secretbox_decrypt" msg decrypted_msg

let secretbox = [("secretbox", `Quick, secretbox)]

let box () =
  let open Box in
  let (pk, sk) = keypair () in
  let k = dh pk sk in
  let nonce = Nonce.gen () in
  let msg_orig = msg in
  let cmsg = Bytes.create (msglen + tagbytes) in
  box ~k ~nonce ~msg:msg_orig ~cmsg ;
  let decrypted_msg = Bytes.create msglen in
  assert (box_open ~k ~nonce ~cmsg ~msg:decrypted_msg) ;
  Alcotest.check check_bytes "box" msg_orig decrypted_msg

let box = [("box", `Quick, box)]

let keypair () =
  let seed = Hacl.Rand.gen 32 in
  let sk = Sign.unsafe_sk_of_bytes seed in
  let pk = Sign.neuterize sk in
  let sk' = Sign.unsafe_sk_of_bytes seed in
  let pk' = Sign.neuterize sk' in
  Alcotest.(check bool "Sign.of_seed" true (Sign.equal pk pk')) ;
  Alcotest.(check bool "Sign.of_seed" true (Sign.equal sk sk')) ;
  let pk_bytes = Sign.unsafe_to_bytes pk in
  let pk_bytes_length = Bytes.length pk_bytes in
  Alcotest.(check int "Sign.to_bytes" Sign.pkbytes pk_bytes_length)

let sign () =
  let (pk, sk) = Sign.keypair () in
  let signature = Bytes.create Sign.size in
  Sign.sign ~sk ~msg ~signature ;
  assert (Sign.verify ~pk ~msg ~signature)

let public () =
  let (pk, sk) = Sign.keypair () in
  let pk' = Sign.unsafe_to_bytes pk in
  let ppk = Sign.(unsafe_to_bytes (neuterize pk)) in
  let psk = Sign.(unsafe_to_bytes (neuterize sk)) in
  Alcotest.check check_bytes "public" pk' ppk ;
  Alcotest.check check_bytes "public" pk' psk

let sign =
  [ ("keypair", `Quick, keypair);
    ("sign", `Quick, sign);
    ("public", `Quick, public) ]

let () =
  Alcotest.run
    "hacl"
    [ ("hash", hash);
      ("blake2b", blake2b_tests);
      ("secretbox", secretbox);
      ("box", box);
      ("sign", sign) ]
