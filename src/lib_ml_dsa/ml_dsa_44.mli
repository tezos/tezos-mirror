(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type key_pair

type signing_key

type verification_key

type signature

val generate_key_pair : bytes -> (key_pair, string) result

val key_pair_get_signing_key : key_pair -> signing_key

val key_pair_get_verification_key : key_pair -> verification_key

val sign : signing_key -> bytes -> bytes -> bytes -> (signature, string) result

val verify :
  verification_key -> bytes -> bytes -> signature -> (unit, string) result

val verification_key_to_bytes : verification_key -> bytes

val verification_key_from_bytes : bytes -> (verification_key, string) result

val signing_key_to_bytes : signing_key -> bytes

val signing_key_from_bytes : bytes -> (signing_key, string) result

val signature_to_bytes : signature -> bytes

val signature_from_bytes : bytes -> (signature, string) result
