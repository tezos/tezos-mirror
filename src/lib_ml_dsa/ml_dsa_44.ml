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

let generate_key_pair_opt ~seed = octez_libcrux_ml_dsa_44_generate_keys_opt seed

let generate_key_pair ~seed = octez_libcrux_ml_dsa_44_generate_keys seed

let sign_opt ~randomness signing_key message =
  octez_libcrux_ml_dsa_44_sign_opt signing_key message randomness

let sign ~randomness signing_key message =
  octez_libcrux_ml_dsa_44_sign
    signing_key
    message
    Bytes.empty (* Empty context *)
    randomness

let verify_b verification_key message signature =
  octez_libcrux_ml_dsa_44_verify_b verification_key message signature

let verify verification_key message signature =
  octez_libcrux_ml_dsa_44_verify
    verification_key
    message
    Bytes.empty (* Empty context *)
    signature

let verification_key_to_bytes verification_key =
  octez_libcrux_ml_dsa_44_verification_key_to_bytes verification_key

let verification_key_from_bytes bytes =
  octez_libcrux_ml_dsa_44_verification_key_from_bytes bytes

let verification_key_from_bytes_opt bytes =
  octez_libcrux_ml_dsa_44_verification_key_from_bytes_opt bytes

let signing_key_to_bytes signing_key =
  octez_libcrux_ml_dsa_44_signing_key_to_bytes signing_key

let signing_key_from_bytes bytes =
  octez_libcrux_ml_dsa_44_signing_key_from_bytes bytes

let signing_key_from_bytes_opt bytes =
  octez_libcrux_ml_dsa_44_signing_key_from_bytes_opt bytes

let signature_to_bytes signature =
  octez_libcrux_ml_dsa_44_signature_to_bytes signature

let signature_from_bytes bytes =
  octez_libcrux_ml_dsa_44_signature_from_bytes bytes

let signature_from_bytes_opt bytes =
  octez_libcrux_ml_dsa_44_signature_from_bytes_opt bytes
