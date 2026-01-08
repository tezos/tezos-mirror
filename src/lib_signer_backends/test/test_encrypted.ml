(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Testing
    -------
    Component:    Remote-signature Backends
    Invocation:   dune exec src/lib_signer_backends/test/main.exe \
                  -- --file test_encrypted.ml
    Subject:      On secret keys and URIs.
*)

open Error_monad

let loops = 10

let passwords =
  List.map
    Bytes.of_string
    [
      "ahThie5H";
      "aVah7eid";
      "Hihohh1n";
      "mui0Hoox";
      "Piu7pual";
      "paik6aiW";
      "caeS5me5";
      "boh5dauL";
      "zaiK1Oht";
      "Oogh4hah";
      "kiY5ohlo";
      "booth0Ei";
      "xa2Aidao";
      "aju6oXu4";
      "gooruGh9";
      "ahy4Daih";
      "chosh0Wu";
      "Cheij6za";
      "quee9ooL";
      "Sohs9are";
      "Pae3gay7";
      "Naif5iel";
      " eir6Aed1";
      "aa6Aesai";
      "";
    ]

let nb_passwds = List.length passwords

let fake_ctx () : Client_context.io_wallet =
  object
    val mutable i = 0

    val mutable distributed = false

    inherit Client_context.simple_printer (fun _ _ -> Lwt.return_unit)

    method prompt : type a. (a, string tzresult) Client_context.lwt_format -> a
        =
      Format.kasprintf (fun _ -> Lwt.return_ok "")

    method prompt_password : type a.
        (a, Bytes.t tzresult) Client_context.lwt_format -> a =
      Format.kasprintf (fun _ ->
          (* return Bigstring.empty *)
          match distributed with
          | false ->
              distributed <- true ;
              Lwt.return_ok
                (WithExceptions.Option.get ~loc:__LOC__ @@ List.nth passwords 0)
          | true ->
              i <- (if i = nb_passwds - 1 then 0 else succ i) ;
              distributed <- false ;
              Lwt.return_ok
                (WithExceptions.Option.get ~loc:__LOC__ @@ List.nth passwords i))

    method multiple_password_retries = true

    method get_base_dir = ""

    method load :
        'a.
        string ->
        default:'a ->
        'a Data_encoding.t ->
        'a Tezos_base.TzPervasives.tzresult Lwt.t =
      fun _ ~default _ -> Lwt.return_ok default

    method load_passwords = None

    method read_file _ = Lwt.return_ok ""

    method with_lock : 'a. (unit -> 'a Lwt.t) -> 'a Lwt.t = fun f -> f ()

    method write :
        'a.
        string ->
        'a ->
        'a Data_encoding.t ->
        unit Tezos_base.TzPervasives.tzresult Lwt.t =
      fun _ _ _ -> Lwt.return_ok ()

    method last_modification_time : string -> float option tzresult Lwt.t =
      fun _ -> Lwt_result_syntax.return_none
  end

let make_sk_uris ?(scheme = "encrypted") =
  List.map_e (fun path -> Client_keys.make_sk_uri (Uri.make ~scheme ~path ()))

let ed25519_sks =
  [
    "edsk3kMNLNdzLPbbABASDLARft8JRZ3Wpwibn8SMAb4KmuWSMJmAFd";
    "edsk3Kqr8VHRx9kmR8Pj5qRGcmvQH34cForiMaMz1Ahhq5DkZp7FxJ";
    "edsk2mBu4w9sMGhryvvXK53dXgpcNdZWi8pJQ1QL2rAgRPrE5y12az";
  ]

let ed25519_sks_encrypted =
  make_sk_uris
    [
      "edesk1oGXxunJ5FTGpQ6o1xdop8VGKdT36Fj7LwWF9HLjzEqaCC4V6tdRVN1jaeJTfCHS8bYf7U2YhMK2yW6jSUy";
      "edesk1s4xEifbUdUkghHHimbNUuyQ4eidDVJdc8JtPRUon758hBqZNZsQxadUDFRSRiUdLoFqBG35HAiLKafyczw";
      "edesk1zY5jEs4QXrF9tXxFq1mfW9PkatdRxCKQ2Q598y5LLz65nQj4eWxefYFp8YLerya1haRdGe5NWckHDb5ApM";
    ]

let secp256k1_sks =
  [
    "spsk24attf9uuQ7PUKFHxTm6E3TMqB6SPkFiMbXPBur7JNrvupW2xg";
    "spsk2H32XfWL7MkW58r76q6Yu5tJg77YGgVyjwq7EvLUHhn4JmAtEG";
    "spsk3KQ56REAUGc6Gn87xCRnWyPwR2Un667vegQVuU16ZcgNyLCooh";
  ]

let secp256k1_sks_encrypted =
  make_sk_uris
    [
      "spesk2CXQHDbzrcNatRzmg83Dto6kX6BWwpP2zGs4Zks9LDsXzaX6mAYRj5ZrrdgyZQap4DS9YRRLNSpaVC2TSsk";
      "spesk1upiFp23osWSUTgHcx8DCVpTrMr9xtdqVQkQDWj5sFG7vqcWLDaNv9AKKcF27Nb266YfuAGF2hEbcyAxHmK";
      "spesk1w7d68hzTWJusk5Xn5oz8EgDXbotDW9BXb5ksFjr8Jd94Kxnu5yKAhgRszojhMUoJ1EEt5BtPpGpkgCjELq";
    ]

let p256_sks =
  [
    "p2sk2YQcwF5h7qgRztocEMrfizUwZaM41f4v7zWneiig2Y5AxajqYC";
    "p2sk2XiSoQC9tvejVBDJyvkbHUq2kvcQHdJJ2wM8rii228DkjKV2b5";
    "p2sk3ZsfsEaxDNn74orv91Ruu35fomzF373aT9ForA4fDo54c47o6H";
  ]

let p256_sks_encrypted =
  make_sk_uris
    [
      "p2esk2JMFpR9yaSpgsaKQYLqFnv16t4gowJ4cgjj7D7iMfoaJz2vZuH7Tdi11MrX6FC2yhfs2nvy5VRxAvzH1STE";
      "p2esk1nfobVL73mY5Y18W8Ltb3Vm6Nf5Th7trN3yA3ucyyP4AH93XfyRatkh9AxxaDtnju1EtArykjroEQHDT97k";
      "p2esk2Ge1jrVak7NhxksimzaQjRCTLx5vxUZ4Akgq3spGQLx6N41h6aKXeEYDgxN5eztnPwD6QiCHCfVAKXLPNm8";
    ]

let bls12_381_sks =
  [
    "BLsk1o1S8oAG76r3o3aJr1gbVhNdDnAMsdy1yMfwHLWwUHfAeyEXHV";
    "BLsk1ootrVQmMBgcnXDKdyBR2M5tXwZh2TRVcinBJwMhCYVDRm3K1N";
    "BLsk1rgztT2EBdQ2vtyXkTzgwmEjabvZri3c7tLHQtDmgH695mcWUt";
  ]

let bls12_381_sks_encrypted ~scheme =
  make_sk_uris
    ~scheme
    [
      "BLesk1ExnCaFxVcGFvKFQrPs2AADo2KpukB6bhA8SLASRzZ58uqvSNUNyzdNdya5NPgE1BAFwcN3wtyFv76r1GJ9";
      "BLesk1c92TTyYAbkt5Aa2g2puGZHy1M9hQVX7um7PYpxfsjbaaiYsqR2ahArH53WGSvbvzUBizgPipMyfmh8bCs5";
      "BLesk1yLinYLYYxq947JSbwt4s94xZb6azZz8AD8myiU67mQpKA2bLxYwq8QhYTrsCdXTtLSLyYnVL5EQAuubYjJ";
    ]

let sk_testable =
  Alcotest.testable
    Tezos_crypto.Signature.Secret_key.pp
    Tezos_crypto.Signature.Secret_key.equal

let test_vectors () =
  let open Encrypted in
  List.iter_es
    (fun (sks, encrypted_sks) ->
      let open Lwt_result_syntax in
      let ctx = fake_ctx () in
      let sks =
        List.map Tezos_crypto.Signature.Secret_key.of_b58check_exn sks
      in
      let*? l = encrypted_sks in
      let* decs = List.map_es (decrypt ctx) l in
      assert (List.equal Tezos_crypto.Signature.Secret_key.equal decs sks) ;
      return_unit)
    [
      (ed25519_sks, ed25519_sks_encrypted);
      (secp256k1_sks, secp256k1_sks_encrypted);
      (p256_sks, p256_sks_encrypted);
      (bls12_381_sks, bls12_381_sks_encrypted ~scheme:"encrypted");
      (bls12_381_sks, bls12_381_sks_encrypted ~scheme:"aggregate_encrypted");
    ]

let test_random algo =
  let open Encrypted in
  let ctx = fake_ctx () in
  let decrypt_ctx = (ctx :> Client_context.io_wallet) in
  let rec inner i =
    let open Lwt_result_syntax in
    if i >= loops then return_unit
    else
      let _, _, sk = Tezos_crypto.Signature.generate_key ~algo () in
      let* sk_uri =
        Tezos_signer_backends.Encrypted.prompt_twice_and_encrypt ctx sk
      in
      let* decrypted_sk = decrypt decrypt_ctx sk_uri in
      Alcotest.check sk_testable "test_encrypt: decrypt" sk decrypted_sk ;
      inner (succ i)
  in
  inner 0

(** For each of the algorithms [[Ed25519; Secp256k1; P256; Bls]], creates a
    dummy context. It randomly generates a Base58-encoded secret key,
    then encrypts it into a URI and decrypts it. It it asserted that
    the secret key is preserved after Base58-decoding comparison. This
    process is repeated 10 times.
*)
let test_random _switch () =
  let open Lwt_syntax in
  let* r =
    List.iter_es
      test_random
      Tezos_crypto.Signature.[Ed25519; Secp256k1; P256; Bls]
  in
  match r with
  | Ok _ -> Lwt.return_unit
  | Error _ -> Lwt.fail_with "test_random"

(** For each of the algorithms [[Ed25519; Secp256k1; P256; Bls]], creates a
    dummy context, uses it to decrypt a list of secret key URIs
    [...__sks_encrypted]. It is asserted that the decrypted keys shall
    match the list [..._sks].
*)
let test_vectors _switch () =
  let open Lwt_syntax in
  let* r = test_vectors () in
  match r with
  | Ok _ -> Lwt.return_unit
  | Error _ -> Lwt.fail_with "test_vectors"

let tests =
  [
    Alcotest_lwt.test_case "random_roundtrip" `Quick test_random;
    Alcotest_lwt.test_case "vectors_decrypt" `Quick test_vectors;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ "tezos-signer-backends" [("encrypted", tests)]
  |> Lwt_main.run
