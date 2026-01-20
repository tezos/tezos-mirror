(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Bindings = Octez_libcrux_ml_dsa

let ml_dsa_44_signing_key_size = 2560

let ml_dsa_44_verification_key_size = 1312

let ml_dsa_44_signature_size = 2420

let key_generation_randomness_size = 32

let signing_randomness_size = 32

let generate_random_bytes size =
  Bytes.init size (fun _ -> char_of_int (Random.int 256))

let key_pair_equivalence name signing_key verification_key signing_key_bytes
    verification_key_bytes =
  Alcotest.(check bytes)
    (Printf.sprintf "signing key: %s equivalent to generate_key_pair" name)
    (Bindings.octez_libcrux_ml_dsa_44_signing_key_to_bytes signing_key)
    signing_key_bytes ;
  Alcotest.(check bytes)
    (Printf.sprintf "verification key: %s equivalent to generate_key_pair" name)
    (Bindings.octez_libcrux_ml_dsa_44_verification_key_to_bytes
       verification_key)
    verification_key_bytes

let test_generate_key_pair () =
  let seed = generate_random_bytes key_generation_randomness_size in

  let key_pair =
    match Bindings.octez_libcrux_ml_dsa_44_generate_key_pair seed with
    | Ok key_pair -> key_pair
    | Error err -> Alcotest.failf "generate_key_pair: generation failed: %s" err
  in

  (* Extract signing and verification keys from key pair *)
  let signing_key =
    Bindings.octez_libcrux_ml_dsa_44_key_pair_get_signing_key key_pair
  in
  let verification_key =
    Bindings.octez_libcrux_ml_dsa_44_key_pair_get_verification_key key_pair
  in

  (* Convert keys to bytes and verify sizes *)
  let signing_key_bytes =
    Bindings.octez_libcrux_ml_dsa_44_signing_key_to_bytes signing_key
  in
  let verification_key_bytes =
    Bindings.octez_libcrux_ml_dsa_44_verification_key_to_bytes verification_key
  in
  Alcotest.(check int)
    "signing key size"
    ml_dsa_44_signing_key_size
    (Bytes.length signing_key_bytes) ;
  Alcotest.(check int)
    "verification key size"
    ml_dsa_44_verification_key_size
    (Bytes.length verification_key_bytes) ;

  (* Check the keys produced from the same seed by `generate_keys` *)
  (match Bindings.octez_libcrux_ml_dsa_44_generate_keys seed with
  | Error err -> Alcotest.failf "generate_keys: generation failed: %s" err
  | Ok (signing_key, verification_key) ->
      key_pair_equivalence
        "generate_keys"
        signing_key
        verification_key
        signing_key_bytes
        verification_key_bytes) ;

  (* Check the keys produced from the same seed by `generate_keys_opt` *)
  (match Bindings.octez_libcrux_ml_dsa_44_generate_keys_opt seed with
  | None -> Alcotest.failf "generate_keys_opt: generation failed"
  | Some (signing_key, verification_key) ->
      key_pair_equivalence
        "generate_keys_opt"
        signing_key
        verification_key
        signing_key_bytes
        verification_key_bytes) ;

  (* Initialise keys from bytes *)
  match
    ( Bindings.octez_libcrux_ml_dsa_44_signing_key_from_bytes signing_key_bytes,
      Bindings.octez_libcrux_ml_dsa_44_verification_key_from_bytes
        verification_key_bytes )
  with
  | Ok _signing_key, Ok _verification_key -> Lwt.return_unit
  | Error err, _ ->
      Alcotest.failf "Initialising signing key from bytes failed: %s" err
  | _, Error err ->
      Alcotest.failf "Initialising verification key from bytes failed: %s" err

let test_sign () =
  let seed = generate_random_bytes key_generation_randomness_size in

  let key_pair =
    match Bindings.octez_libcrux_ml_dsa_44_generate_key_pair seed with
    | Ok key_pair -> key_pair
    | Error err -> Alcotest.failf "Key pair generation failed: %s" err
  in

  let signing_key =
    Bindings.octez_libcrux_ml_dsa_44_key_pair_get_signing_key key_pair
  in

  let message = generate_random_bytes 16 in
  let randomness = generate_random_bytes signing_randomness_size in

  match
    ( Bindings.octez_libcrux_ml_dsa_44_sign
        signing_key
        message
        Bytes.empty
        randomness,
      Bindings.octez_libcrux_ml_dsa_44_sign_opt signing_key message randomness
    )
  with
  | Error err, _ -> Alcotest.failf "sign failed: %s" err
  | _, None -> Alcotest.failf "sign_opt failed"
  | Ok signature1, Some signature2 -> (
      let signature_bytes =
        Bindings.octez_libcrux_ml_dsa_44_signature_to_bytes signature1
      in

      Alcotest.(check bytes)
        "sign is equivalent to sign_opt"
        signature_bytes
        (Bindings.octez_libcrux_ml_dsa_44_signature_to_bytes signature2) ;

      Alcotest.(check int)
        "signature size"
        ml_dsa_44_signature_size
        (Bytes.length signature_bytes) ;

      match
        Bindings.octez_libcrux_ml_dsa_44_signature_from_bytes signature_bytes
      with
      | Ok _signature -> Lwt.return_unit
      | Error err ->
          Alcotest.failf "Initialising signature from bytes failed: %s" err)

let test_verify () =
  let seed = generate_random_bytes key_generation_randomness_size in

  let key_pair =
    match Bindings.octez_libcrux_ml_dsa_44_generate_key_pair seed with
    | Ok key_pair -> key_pair
    | Error err -> Alcotest.failf "Key pair generation failed: %s" err
  in

  let signing_key =
    Bindings.octez_libcrux_ml_dsa_44_key_pair_get_signing_key key_pair
  in
  let verification_key =
    Bindings.octez_libcrux_ml_dsa_44_key_pair_get_verification_key key_pair
  in

  let message = generate_random_bytes 16 in
  let randomness = generate_random_bytes signing_randomness_size in
  let context = Bytes.empty in

  match
    Bindings.octez_libcrux_ml_dsa_44_sign signing_key message context randomness
  with
  | Error err -> Alcotest.failf "Signing failed: %s" err
  | Ok signature -> (
      Alcotest.(check bool)
        "verify_opt"
        true
        (Bindings.octez_libcrux_ml_dsa_44_verify_b
           verification_key
           message
           signature) ;
      match
        Bindings.octez_libcrux_ml_dsa_44_verify
          verification_key
          message
          context
          signature
      with
      | Ok () -> Lwt.return_unit
      | Error err -> Alcotest.failf "Verification failed: %s" err)

let test_verify_invalid_signature () =
  let seed = generate_random_bytes key_generation_randomness_size in

  let key_pair =
    match Bindings.octez_libcrux_ml_dsa_44_generate_key_pair seed with
    | Ok key_pair -> key_pair
    | Error err -> Alcotest.failf "Key pair generation failed: %s" err
  in

  let signing_key =
    Bindings.octez_libcrux_ml_dsa_44_key_pair_get_signing_key key_pair
  in
  let verification_key =
    Bindings.octez_libcrux_ml_dsa_44_key_pair_get_verification_key key_pair
  in

  (* Sign two different messages *)
  let message1 = generate_random_bytes 16 in
  let message2 = generate_random_bytes 16 in
  let randomness1 = generate_random_bytes signing_randomness_size in
  let randomness2 = generate_random_bytes signing_randomness_size in
  let context = Bytes.empty in

  match
    ( Bindings.octez_libcrux_ml_dsa_44_sign
        signing_key
        message1
        context
        randomness1,
      Bindings.octez_libcrux_ml_dsa_44_sign
        signing_key
        message2
        context
        randomness2 )
  with
  | Error err, _ -> Alcotest.failf "Signing message1 failed: %s" err
  | _, Error err -> Alcotest.failf "Signing message2 failed: %s" err
  | Ok _signature1, Ok signature2 -> (
      (* Try to verify message1 with signature2 *)
      match
        Bindings.octez_libcrux_ml_dsa_44_verify
          verification_key
          message1
          context
          signature2
      with
      | Ok () ->
          Alcotest.failf "Verification should have failed with wrong signature"
      | Error _ -> Lwt.return_unit)

(* Test that buffer sizes are validated for the following functions and buffers:
   - generate_key_pair: randomness
   - generate_keys(_opt): randomness
   - sign: randomness, context
   - sign_opt: randomness
   - verify: context
   - verification_key_from_bytes(_opt): bytes
   - signing_key_from_bytes(_opt): bytes
   - signature_from_bytes(_opt): bytes *)
let test_invalid_buffer_lengths () =
  let max_context_size = 255 in

  let invalid_seed =
    generate_random_bytes (key_generation_randomness_size + 1)
  in

  (* Test `generate_key_pair` with invalid randomness size *)
  if
    Bindings.octez_libcrux_ml_dsa_44_generate_key_pair invalid_seed
    |> Result.is_ok
  then
    Alcotest.failf
      "generate_key_pair should have failed (invalid randomness size)" ;

  (* Test `generate_keys` with invalid randomness size *)
  if Bindings.octez_libcrux_ml_dsa_44_generate_keys invalid_seed |> Result.is_ok
  then
    Alcotest.failf "generate_keys should have failed (invalid randomness size)" ;

  (* Test `generate_keys_opt` with invalid randomness size *)
  if
    Bindings.octez_libcrux_ml_dsa_44_generate_keys_opt invalid_seed
    |> Option.is_some
  then
    Alcotest.failf "generate_keys should have failed (invalid randomness size)" ;

  let seed = generate_random_bytes key_generation_randomness_size in
  let key_pair =
    match Bindings.octez_libcrux_ml_dsa_44_generate_key_pair seed with
    | Ok key_pair -> key_pair
    | Error err -> Alcotest.failf "Key pair generation failed: %s" err
  in
  let signing_key =
    Bindings.octez_libcrux_ml_dsa_44_key_pair_get_signing_key key_pair
  in
  let verification_key =
    Bindings.octez_libcrux_ml_dsa_44_key_pair_get_verification_key key_pair
  in

  (* Test `sign` with invalid randomness size *)
  let message = generate_random_bytes 16 in
  let invalid_randomness =
    generate_random_bytes (signing_randomness_size + 1)
  in
  if
    Bindings.octez_libcrux_ml_dsa_44_sign
      signing_key
      message
      Bytes.empty
      invalid_randomness
    |> Result.is_ok
  then Alcotest.failf "sign should have failed (invalid randomness size)" ;

  (* Test `sign` with invalid context size *)
  let valid_randomness = generate_random_bytes signing_randomness_size in
  let invalid_context = generate_random_bytes (max_context_size + 1) in
  if
    Bindings.octez_libcrux_ml_dsa_44_sign
      signing_key
      message
      invalid_context
      valid_randomness
    |> Result.is_ok
  then Alcotest.failf "sign should have failed (invalid context size)" ;

  (* Test `sign_opt` with invalid randomness size *)
  let message = generate_random_bytes 16 in
  let invalid_randomness =
    generate_random_bytes (signing_randomness_size + 1)
  in
  if
    Bindings.octez_libcrux_ml_dsa_44_sign_opt
      signing_key
      message
      invalid_randomness
    |> Option.is_some
  then Alcotest.failf "sign should have failed (invalid randomness size)" ;

  (* Test `verify` with invalid context size *)
  let signature =
    match
      Bindings.octez_libcrux_ml_dsa_44_sign
        signing_key
        message
        Bytes.empty
        valid_randomness
    with
    | Ok signature -> signature
    | Error err -> Alcotest.failf "Signing for test setup failed: %s" err
  in
  let invalid_context = generate_random_bytes (max_context_size + 1) in
  if
    Bindings.octez_libcrux_ml_dsa_44_verify
      verification_key
      message
      invalid_context
      signature
    |> Result.is_ok
  then Alcotest.failf "verify should have failed (invalid context size)" ;

  let invalid_verification_key_bytes =
    generate_random_bytes (ml_dsa_44_verification_key_size + 1)
  in

  (* Test `verification_key_from_bytes` with invalid input buffer size *)
  if
    Bindings.octez_libcrux_ml_dsa_44_verification_key_from_bytes
      invalid_verification_key_bytes
    |> Result.is_ok
  then
    Alcotest.failf
      "verification_key_from_bytes_opt should have failed (invalid input size)" ;

  (* Test `verification_key_from_bytes_opt` with invalid input buffer size *)
  if
    Bindings.octez_libcrux_ml_dsa_44_verification_key_from_bytes_opt
      invalid_verification_key_bytes
    |> Option.is_some
  then
    Alcotest.failf
      "verification_key_from_bytes_opt should have failed (invalid input size)" ;

  let invalid_signing_key_bytes =
    generate_random_bytes (ml_dsa_44_signing_key_size + 1)
  in

  (* Test `signing_key_from_bytes` with invalid input buffer size *)
  if
    Bindings.octez_libcrux_ml_dsa_44_signing_key_from_bytes
      invalid_signing_key_bytes
    |> Result.is_ok
  then
    Alcotest.failf
      "signing_key_from_bytes should have failed (invalid input size)" ;

  (* Test `signing_key_from_bytes_opt` with invalid input buffer size *)
  if
    Bindings.octez_libcrux_ml_dsa_44_signing_key_from_bytes_opt
      invalid_signing_key_bytes
    |> Option.is_some
  then
    Alcotest.failf
      "signing_key_from_bytes_opt should have failed (invalid input size)" ;

  let invalid_signature_bytes =
    generate_random_bytes (ml_dsa_44_signature_size + 1)
  in

  (* Test `signature_from_bytes` with invalid input buffer size *)
  if
    Bindings.octez_libcrux_ml_dsa_44_signature_from_bytes
      invalid_signature_bytes
    |> Result.is_ok
  then
    Alcotest.failf
      "signature_from_bytes should have failed (invalid input size)" ;

  (* Test `signature_from_bytes_opt` with invalid input buffer size *)
  if
    Bindings.octez_libcrux_ml_dsa_44_signature_from_bytes_opt
      invalid_signature_bytes
    |> Option.is_some
  then
    Alcotest.failf
      "signature_from_bytes_opt should have failed (invalid input size)" ;

  Lwt.return_unit
