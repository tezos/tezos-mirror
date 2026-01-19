(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type signing_key = Octez_libcrux_ml_dsa.signing_key

type verification_key = Octez_libcrux_ml_dsa.verification_key

type signature = Octez_libcrux_ml_dsa.signature

open Octez_libcrux_ml_dsa

let signing_key_size = 2560

let verification_key_size = 1312

let signature_size = 2420

let key_gen_randomness_size = 32

let signing_randomness_size = 32

let generate_key_pair ~seed =
  octez_libcrux_ml_dsa_44_generate_key_pair seed
  |> Result.map (fun key_pair ->
         ( octez_libcrux_ml_dsa_44_key_pair_get_signing_key key_pair,
           octez_libcrux_ml_dsa_44_key_pair_get_verification_key key_pair ))

let generate_key_pair_opt ~seed = Result.to_option @@ generate_key_pair ~seed

let sign ~randomness signing_key message =
  octez_libcrux_ml_dsa_44_sign
    signing_key
    message
    Bytes.empty (* Empty context *)
    randomness

let sign_opt ~randomness signing_key message =
  Result.to_option @@ sign ~randomness signing_key message

let verify verification_key message signature =
  octez_libcrux_ml_dsa_44_verify
    verification_key
    message
    Bytes.empty (* Empty context *)
    signature

let verify_b verification_key message signature =
  Result.is_ok @@ verify verification_key message signature

let verification_key_to_bytes verification_key =
  octez_libcrux_ml_dsa_44_verification_key_to_bytes verification_key

let verification_key_from_bytes bytes =
  octez_libcrux_ml_dsa_44_verification_key_from_bytes bytes

let verification_key_from_bytes_opt bytes =
  Result.to_option @@ verification_key_from_bytes bytes

let signing_key_to_bytes signing_key =
  octez_libcrux_ml_dsa_44_signing_key_to_bytes signing_key

let signing_key_from_bytes bytes =
  octez_libcrux_ml_dsa_44_signing_key_from_bytes bytes

let signing_key_from_bytes_opt bytes =
  Result.to_option @@ signing_key_from_bytes bytes

let signature_to_bytes signature =
  octez_libcrux_ml_dsa_44_signature_to_bytes signature

let signature_from_bytes bytes =
  octez_libcrux_ml_dsa_44_signature_from_bytes bytes

let signature_from_bytes_opt bytes =
  Result.to_option @@ signature_from_bytes bytes
