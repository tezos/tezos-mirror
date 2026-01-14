(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type signing_key

val signing_key_size : int

val signing_key_to_bytes : signing_key -> bytes

val signing_key_from_bytes : bytes -> (signing_key, string) result

type verification_key

val verification_key_size : int

val verification_key_to_bytes : verification_key -> bytes

val verification_key_from_bytes : bytes -> (verification_key, string) result

type signature

val signature_size : int

val signature_to_bytes : signature -> bytes

val signature_from_bytes : bytes -> (signature, string) result

val key_gen_randomness_size : int

val generate_key_pair :
  seed:bytes -> (signing_key * verification_key, string) result

val signing_randomness_size : int

val sign :
  randomness:bytes -> signing_key -> bytes -> (signature, string) result

val verify : verification_key -> bytes -> signature -> (unit, string) result
