(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type key_pair = Octez_libcrux_ml_dsa.key_pair

type signing_key = Octez_libcrux_ml_dsa.signing_key

type verification_key = Octez_libcrux_ml_dsa.verification_key

let generate_key_pair =
  Octez_libcrux_ml_dsa.octez_libcrux_ml_dsa_44_generate_key_pair

let key_pair_get_signing_key =
  Octez_libcrux_ml_dsa.octez_libcrux_ml_dsa_44_key_pair_get_signing_key

let key_pair_get_verification_key =
  Octez_libcrux_ml_dsa.octez_libcrux_ml_dsa_44_key_pair_get_verification_key

type signature = Octez_libcrux_ml_dsa.signature

let sign signing_key message context randomness =
  Octez_libcrux_ml_dsa.octez_libcrux_ml_dsa_44_sign
    signing_key
    message
    context
    randomness

let verify verification_key message context signature =
  Octez_libcrux_ml_dsa.octez_libcrux_ml_dsa_44_verify
    verification_key
    message
    context
    signature

let verification_key_to_bytes verification_key =
  Octez_libcrux_ml_dsa.octez_libcrux_ml_dsa_44_verification_key_to_bytes
    verification_key

let verification_key_from_bytes bytes =
  Octez_libcrux_ml_dsa.octez_libcrux_ml_dsa_44_verification_key_from_bytes bytes

let signing_key_to_bytes signing_key =
  Octez_libcrux_ml_dsa.octez_libcrux_ml_dsa_44_signing_key_to_bytes signing_key

let signing_key_from_bytes bytes =
  Octez_libcrux_ml_dsa.octez_libcrux_ml_dsa_44_signing_key_from_bytes bytes

let signature_to_bytes signature =
  Octez_libcrux_ml_dsa.octez_libcrux_ml_dsa_44_signature_to_bytes signature

let signature_from_bytes bytes =
  Octez_libcrux_ml_dsa.octez_libcrux_ml_dsa_44_signature_from_bytes bytes
