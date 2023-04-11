(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>           *)
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
    Subject:      Checking all of the HACL* primitives used in lib_crypto:
                  hashing, HMAC, NaCl, Ed25519, and P-256.
*)
open Hacl

let hex s = Hex.to_bytes_exn (`Hex s)

type vector = {
  data_in : Bytes.t list;
  data_key : Bytes.t option;
  data_out : Bytes.t;
}

let vectors =
  [
    {
      data_in =
        [
          hex
            "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f";
        ];
      data_key = None;
      data_out =
        hex
          "1c077e279de6548523502b6df800ffdab5e2c3e9442eb838f58c295f3b147cef9d701c41c321283f00c71affa0619310399126295b78dd4d1a74572ef9ed5135";
    };
    {
      data_in =
        [
          hex "000102030405060708090a0b0c0d0e0f101112131415";
          hex "161718";
          hex
            "191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f";
        ];
      data_key = None;
      data_out =
        hex
          "1c077e279de6548523502b6df800ffdab5e2c3e9442eb838f58c295f3b147cef9d701c41c321283f00c71affa0619310399126295b78dd4d1a74572ef9ed5135";
    };
    {
      data_in =
        [
          hex
            "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9fa0a1a2a3a4a5a6a7a8a9aaabacadaeafb0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedfe0e1e2e3e4e5e6e7e8e9eaebecedeeeff0f1f2f3";
        ];
      data_key =
        Some
          (hex
             "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f");
      data_out =
        hex
          "b39614268fdd8781515e2cfebf89b4d5402bab10c226e6344e6b9ae000fb0d6c79cb2f3ec80e80eaeb1980d2f8698916bd2e9f747236655116649cd3ca23a837";
    };
  ]

let msg = Bytes.of_string "Longtemps, je me suis couche de bonne heure"

let msglen = Bytes.length msg

let of_hex hex = Cstruct.(to_bytes (of_hex hex))

let randmsg =
  of_hex
    "12c0c5a283401a81163dfd645e57ef6ff58b2f877c4e2d4add10345ec80bef3ffc720060c82e4288a20eccf99d64f18223edb30069fa76de9fe9ae8f875f3a3f75f91dd625652632869766839075e88afc852918da3445bca6d428a4f55d98366065fc70e0306fc6c84ec9e8d1325cc63ba09d5803383d0be40bd7ace7e7551615e4267f94630a0ad62cf798b4a7648390547a3616f42d8b8e58d7223f3c07826670209601be0ef2ea60e662c34b21113680141bead22e8b31015d7fe1a6617101036f03050d8b6854989bdfc13efaa6b2e1960c291f91da346911b1d46f20242bb1eb16f4104f9d684ed0dfca8e13e46b47ba9c39513f5e0746dd828f43da416e10341f3b169691ee823a53500f1ef00c6a52c3f4ecb42f68e1894785d4d192079cc8e53be8bb4ca1e000553504d6132e95490a4b477baaddca598f8947b20fbf732ac608830fb4b11c3cd1e19257e8cb00a22a8fc54ad6e47960086cd5ed24451c1f2ac2cda4514e6e1118ffabd74e7aae3514f3e5d40443ed94bdbbf7af5fa737d2da3b19cac58ca24539313a545164c20c4fae74d01fcb535d4414885ee50cdbb5ff1fcd465fc0c0a0c0f0ebc62687569bd5d36774a6a9c8d9e05b33ac30f13fdd7906aebd27dfd2ee19616a6f3694f2539b89b9ce6d73396816202700f50617f26a7134a6819fe808775bff75df240102fb0352f67eb97e022f66d40403"

(** Checks that the SHA256-digest from [msg] corresponds to [expected_value],
    whether using [digest] or with [init]/[update]. Same for [randmsg].
*)

let sha256_expected_value =
  of_hex "d8d219deae87c5e5fffbc0a0a38986de266427b2e08be9f9d22a07b91099dbfe"

and sha256_rand_expected_value =
  of_hex "9f043732d7117fa402d24e7343108976524b097390b0b160df42b0fa5bc6425c"

let test_sha256 () =
  let open Hash.SHA256 in
  let run (value, msg) =
    let d = digest msg in
    Alcotest.(check bytes "sha256" value d)
  in
  List.iter
    run
    [(sha256_expected_value, msg); (sha256_rand_expected_value, randmsg)]

(** Checks SHA256 incremental hashing. *)
let test_sha256_seq () =
  let open Hash.SHA256 in
  let one_update () =
    let run (value, msg) =
      let st = init () in
      update st msg ;
      let d = finish st in
      Alcotest.(check bytes "sha256" value d)
    in
    List.iter
      run
      [(sha256_expected_value, msg); (sha256_rand_expected_value, randmsg)]
  in
  let two_updates () =
    let bothresp =
      of_hex "ddabe6c4552e944d927bd0b03dd5ab95ecdfa5a135b6c3b60416dbde57b38416"
    in
    let st = init () in
    update st msg ;
    update st randmsg ;
    let d = finish st in
    Alcotest.(check bytes "sha256_seq" bothresp d)
  in
  one_update () ;
  two_updates ()

(** Checks SHA512 hash function. *)
let test_sha512 () =
  let resp =
    of_hex
      "b4686c597b48c05f9d79f933611343e00985967c16f81a301269c75065ed05067dc547b1b36ec1822cc19a78df691e30fdb739ffb6b2a0ea6533ff20a2202e51"
  in
  let digest = Hash.SHA512.digest msg in
  Alcotest.(check bytes "sha512" resp digest)

(** Checks HMAC-SHA256 with a fixed message and key. *)
let test_hmac_sha256 () =
  let key = Bytes.of_string "key" in
  let msg = Bytes.of_string "The quick brown fox jumps over the lazy dog" in
  let resp =
    of_hex "f7bc83f430538424b13298e6aa6fb143ef4d59a14946175997479dbc2d1a3cd8"
  in
  let digest = Hash.SHA256.HMAC.digest ~key ~msg in
  Alcotest.(check bytes "hmac_sha256" resp digest)

(** Checks HMAC-SHA512. *)
let test_hmac_sha512 () =
  let vectors =
    [
      ( Bytes.of_string "key",
        Bytes.of_string "The quick brown fox jumps over the lazy dog",
        of_hex
          "b42af09057bac1e2d41708e48a902e09b5ff7f12ab428a4fe86653c73dd248fb82f948a549f7b791a5b41915ee4d1ec3935357e4e2317250d0372afa2ebeeb3a"
      );
      ( Bytes.empty,
        Bytes.empty,
        of_hex
          "b936cee86c9f87aa5d3c6f2e84cb5a4239a5fe50480a6ec66b70ab5b1f4ac6730c6c515421b327ec1d69402e53dfb49ad7381eb067b338fd7b0cb22247225d47"
      );
    ]
  in
  List.iter
    (fun (key, msg, resp) ->
      let digest = Hash.SHA512.HMAC.digest ~key ~msg in
      Alcotest.(check bytes "hmac_sha512" resp digest))
    vectors

(** Checks SHA3-256 hash function. *)
let test_sha3_256 () =
  let resp =
    of_hex "c0bc8a6fc1d24c6d8aba95294d86159807aacb95eff450367e807c76fdf98037"
  in
  let digest = Hash.SHA3_256.digest msg in
  Alcotest.(check bytes "sha3_256" resp digest)

(** Checks SHA3-512 hash function. *)
let test_sha3_512 () =
  let resp =
    of_hex
      "5d64d3ef8598612744af86fb24d9ad792e0064544a97c149ff8aaedf35c2717d105a2ae191aa11e08c525c28433687c044a8e81271ab9a668ba531091823dfe7"
  in
  let digest = Hash.SHA3_512.digest msg in
  Alcotest.(check bytes "sha3_512" resp digest)

(** Checks Keccak-256 hash function. *)
let test_keccak_256 () =
  let resp =
    of_hex "9f3afe7d35d9bbc4efd98252357e73e85ce1234a48603a063bb7079174aafa68"
  in
  let digest = Hash.Keccak_256.digest msg in
  Alcotest.(check bytes "keccak_256" resp digest)

let is_js_of_ocaml_backend =
  match Sys.backend_type with
  | Other "js_of_ocaml" -> true
  | Native | Bytecode | Other _ -> false

let hash =
  [
    ("hmac_sha256", `Quick, test_hmac_sha256);
    ("hmac_sha512", `Quick, test_hmac_sha512);
    ("sha256", `Quick, test_sha256);
    ("sha512", `Quick, test_sha512);
    ("sha3_256", `Quick, test_sha3_256);
    ("sha3_512", `Quick, test_sha3_512);
    ("keccak_256", `Quick, test_keccak_256);
  ]
  @
  (* Hacl wasm does not support incremental hashing yet *)
  if is_js_of_ocaml_backend then []
  else [("sha256_seq", `Quick, test_sha256_seq)]

(** Compares a Blake2b hash from [data_in] with [key] to the expected
   output [data_out].
*)
let test_blake2b_direct {data_in; data_key; data_out} =
  let (Blake2b.Hash h) =
    Blake2b.direct
      ?key:data_key
      (Bytes.concat Bytes.empty data_in)
      (Bytes.length data_out)
  in
  Alcotest.(check bytes "blake2b_direct" data_out h)

(** Tests Blake2b using [vectors]. *)
let blake2b_tests =
  List.mapi
    (fun i v -> (string_of_int i, `Quick, fun () -> test_blake2b_direct v))
    vectors

(** Encrypts then decrypts a message using the Secretbox module. *)
let test_secretbox () =
  let open Secretbox in
  let key = genkey () in
  let nonce = Nonce.gen () in
  let cmsg = Bytes.create (msglen + 16) in
  secretbox ~key ~nonce ~msg ~cmsg ;
  let decrypted_msg = Bytes.create msglen in
  Alcotest.(
    check
      bool
      "secretbox_open"
      true
      (secretbox_open ~key ~nonce ~cmsg ~msg:decrypted_msg)) ;
  Alcotest.(check bytes "secretbox_decrypt" msg decrypted_msg)

let secretbox = [("secretbox", `Quick, test_secretbox)]

(** Using Box's precomputation inferface. Encrypts and decrypts
    [msg_orig], ensuring the result is the same.
*)
let test_box () =
  let open Box in
  let pk, sk = keypair () in
  let k = dh pk sk in
  let nonce = Nonce.gen () in
  let msg_orig = msg in
  let cmsg = Bytes.create (msglen + tagbytes) in
  box ~k ~nonce ~msg:msg_orig ~cmsg ;
  let decrypted_msg = Bytes.create msglen in
  Alcotest.(
    check bool "box_open" true (box_open ~k ~nonce ~cmsg ~msg:decrypted_msg)) ;
  Alcotest.(check bytes "box" msg_orig decrypted_msg)

let box = [("box", `Quick, test_box)]

open Ed25519

(** Checks that neuterize is deterministic. With the same seed,
    generates two keypairs. Checks that they are identical. The public
    key has the expected length.
*)
let test_keypair_ed25519 () =
  let seed = Hacl.Rand.gen 32 in
  match (sk_of_bytes seed, sk_of_bytes seed) with
  | Some sk, Some sk' ->
      let pk = neuterize sk in
      let pk' = neuterize sk' in
      Alcotest.(check bool "of_seed" true (Ed25519.equal pk pk')) ;
      Alcotest.(check bool "of_seed" true (Ed25519.equal sk sk')) ;
      let pk_bytes = to_bytes pk in
      let pk_bytes_length = Bytes.length pk_bytes in
      Alcotest.(check int "to_bytes" pk_size pk_bytes_length)
  | _ -> Alcotest.fail "keypair_ed25519"

(** Signs the message [msg] with [Sign.sign] and then verifies that it
    is accepted by [Sign.verify].
*)
let test_sign_ed25519 () =
  let pk, sk = keypair () in
  let signature = sign ~sk ~msg in
  Alcotest.(check bool "verify" true (verify ~pk ~msg ~signature))

(** Checks the neuterize function for public key generation. *)
let test_public_ed25519 () =
  let pk, sk = keypair () in
  let pk' = to_bytes pk in
  let ppk = to_bytes (neuterize pk) in
  let psk = to_bytes (neuterize sk) in
  Alcotest.(check bytes "public" pk' ppk) ;
  Alcotest.(check bytes "public" pk' psk)

(** Checks that Ed25519 makes the expected choices in the scope allowed by
    RFC-8032. See [Vectors_ed25519] for more details. *)
let test_speccheck_ed25519 () =
  let open Vectors_ed25519 in
  List.iter
    (fun case ->
      match Ed25519.pk_of_bytes (hex case.pub_key) with
      | Some pk ->
          let msg = hex case.message in
          let signature = hex case.signature in
          Alcotest.(
            check
              bool
              "ed25519-speccheck"
              (Hacl.Ed25519.verify ~pk ~msg ~signature)
              case.expected)
      | None -> Alcotest.fail "pk_of_bytes")
    cases

let ed25519 =
  [
    ("keypair", `Quick, test_keypair_ed25519);
    ("sign", `Quick, test_sign_ed25519);
    ("public", `Quick, test_public_ed25519);
    ("ed25519-speccheck", `Quick, test_speccheck_ed25519);
  ]

open P256

let check_p256_bytes_secret =
  Alcotest.testable
    (fun fmt bytes ->
      Format.fprintf fmt "%S" (Bytes.to_string (P256.to_bytes bytes)))
    P256.equal

let check_p256_bytes_public =
  Alcotest.testable
    (fun fmt bytes ->
      Format.fprintf fmt "%S" (Bytes.to_string (P256.to_bytes bytes)))
    P256.equal

let nb_iterations = 10

let test_export_p256 () =
  let pk, sk = keypair () in
  let sk_bytes = to_bytes sk in
  let pk_bytes = to_bytes pk in
  Alcotest.(check int __LOC__ sk_size (Bytes.length sk_bytes)) ;
  Alcotest.(check int __LOC__ pk_size (Bytes.length pk_bytes)) ;
  match (sk_of_bytes sk_bytes, pk_of_bytes pk_bytes) with
  | Some sk', Some pk' ->
      let pk'' = neuterize pk' in
      Alcotest.(check check_p256_bytes_secret "sk'" sk sk') ;
      Alcotest.(check check_p256_bytes_public "pk'" pk pk') ;
      Alcotest.(check check_p256_bytes_public "pk''" pk pk'') ;
      Alcotest.(check check_p256_bytes_public "pk" pk' pk')
  | _ -> Alcotest.fail "export_p256"

let test_export_p256 () =
  for _i = 0 to nb_iterations - 1 do
    test_export_p256 ()
  done

let test_write_key_p256 () =
  let pk, sk = keypair () in
  let sk_bytes = to_bytes sk in
  let pk_bytes = to_bytes pk in
  let sk_buf = Bytes.create sk_size in
  let pk_buf = Bytes.create pk_size in
  blit_to_bytes sk sk_buf ;
  blit_to_bytes pk pk_buf ;
  Alcotest.(check bytes "write_key_p256 sk" sk_bytes sk_buf) ;
  Alcotest.(check bytes "write_key_p256 pk" pk_bytes pk_buf)

let test_write_key_pos_p256 () =
  let pos = 42 in
  let pk, sk = keypair () in
  let sk_bytes = to_bytes sk in
  let pk_bytes = to_bytes pk in
  let sk_buf = Bytes.create (sk_size + pos) in
  let pk_buf = Bytes.create (pk_size + pos) in
  blit_to_bytes ~pos sk sk_buf ;
  blit_to_bytes ~pos pk pk_buf ;
  Alcotest.(
    check bytes "write_key_pos_p256 sk" sk_bytes (Bytes.sub sk_buf pos sk_size)) ;
  Alcotest.(
    check bytes "write_key_pos_p256 pk" pk_bytes (Bytes.sub pk_buf pos pk_size))

let test_write_key_with_ledger () =
  (* This test simulates  the code in Ledger_commands.public_key_returning_instruction *)
  let pk, _ = keypair () in
  let pk_bytes = to_bytes pk in
  let buf = Bytes.create (pk_size + 1) in
  match pk_of_bytes pk_bytes with
  | None -> Stdlib.failwith "Impossible to read P256 public key from Ledger"
  | Some pk ->
      TzEndian.set_int8 buf 0 2 ;
      blit_to_bytes pk ~pos:1 buf ;
      Alcotest.(
        check bytes "write_key_with_ledger" (Bytes.sub buf 1 pk_size) pk_bytes)

let test_write_key_p256 () =
  for _i = 0 to nb_iterations - 1 do
    test_write_key_p256 () ;
    test_write_key_pos_p256 () ;
    test_write_key_with_ledger ()
  done

let test_keypair_p256 () =
  let pk, sk = keypair () in
  let pk' = neuterize sk in
  Alcotest.(check bytes "keccak_256" (P256.to_bytes pk) (P256.to_bytes pk'))

let test_keypair_p256 () =
  for _i = 0 to nb_iterations - 1 do
    test_keypair_p256 ()
  done

let test_sign_p256 () =
  let pk, sk = keypair () in
  let signature = sign ~sk ~msg in
  Alcotest.(check bool "sign_p256" true (verify ~pk ~msg ~signature))

let test_sign_p256 () =
  for _i = 0 to nb_iterations - 1 do
    test_sign_p256 ()
  done

let test_vectors_p256 () =
  let msgs = List.map of_hex Vectors_p256.msgs in
  let keys =
    List.map
      (fun (sk, pk) ->
        match (sk_of_bytes (of_hex sk), pk_of_bytes (of_hex pk)) with
        | Some sk, Some pk -> (sk, pk)
        | _ -> Alcotest.fail "invalid key")
      Vectors_p256.keys
  in
  let expected_sigs =
    List.map
      (fun block ->
        List.map
          (fun (r, s) ->
            let r = of_hex r in
            let s = of_hex s in
            Bytes.cat r s)
          block)
      Vectors_p256.sigs
  in
  List.iter2_e
    ~when_different_lengths:()
    (fun (sk, pk) sigs ->
      List.iter2
        ~when_different_lengths:()
        (fun msg s ->
          Alcotest.(
            check bool "vectors_p256" true (verify ~pk ~msg ~signature:s)) ;
          let signature = sign ~sk ~msg in
          Alcotest.(check bool "vectors_p256" true (verify ~pk ~msg ~signature)))
        msgs
        sigs)
    keys
    expected_sigs
  |> function
  | Ok () -> ()
  | Error () -> failwith "unequal number of keys, messages, and signatures"

let p256 =
  [
    ("export", `Quick, test_export_p256);
    ("write_key", `Quick, test_write_key_p256);
    ("keypair", `Quick, test_keypair_p256);
    ("sign", `Quick, test_sign_p256);
    ("test_vectors", `Quick, test_vectors_p256);
  ]

let tests =
  [
    ("hash", hash);
    ("blake2b", blake2b_tests);
    ("secretbox", secretbox);
    ("box", box);
    ("ed25519", ed25519);
    ("p256", p256);
  ]

let () = Alcotest.run ~__FILE__ "tezos-crypto" tests
