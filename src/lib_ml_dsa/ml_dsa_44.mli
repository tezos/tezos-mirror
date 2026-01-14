(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Type of the signing key. *)
type signing_key

(** The size of a [signing_key]. *)
val signing_key_size : int

(** [signing_key_to_bytes signing_key] serialises the signing key into bytes
    representation. *)
val signing_key_to_bytes : signing_key -> bytes

(** [signing_key_from_bytes bytes] attempts to deserialize [bytes] into a
    signing key.

    @return an error string "Invalid signing key size:.." if the
    [bytes] is not [signing_key_size] long. *)
val signing_key_from_bytes : bytes -> (signing_key, string) result

(** Type of the verification key. *)
type verification_key

(** The size of a [verification_key]. *)
val verification_key_size : int

(** [verification_key_to_bytes verification_key] serialises the verification key
    into bytes representation. *)
val verification_key_to_bytes : verification_key -> bytes

(** [verification_key_from_bytes bytes] attempts to deserialize [bytes] into a
    verification key.

    @return an error string "Invalid verification key size:.." if the
    [bytes] is not [verification_key_size] long. *)
val verification_key_from_bytes : bytes -> (verification_key, string) result

(** Type of the signature. *)
type signature

(** The size of a [signature]. *)
val signature_size : int

(** [signature_to_bytes signature] serialises the signature
    into bytes representation. *)
val signature_to_bytes : signature -> bytes

(** [signature_from_bytes bytes] attempts to deserialize [bytes] into a
    signature.

    @return an error string "Invalid signature size:.." if the [bytes] is not
    [signature_size] long. *)
val signature_from_bytes : bytes -> (signature, string) result

(** The size of a randomness value for key generation. *)
val key_gen_randomness_size : int

(** [generate_key_pair ~seed] generates a new pair of [signing_key] and
    [verification_key] based on the given [seed].

    @return an error string "Invalid key generation randomness size:.." if the
    [seed] is not [key_gen_randomness_size] long. *)
val generate_key_pair :
  seed:bytes -> (signing_key * verification_key, string) result

(** The size of a randomness value for signing. *)
val signing_randomness_size : int

(** [sign ~randomness signing_key message] signs the given [message] with the
    [signing_key].

    @return an error string "Invalid signing randomness size:.." if the
    [randomness] is not [signing_randomness_size] long.

    @return an error string "Signing failed: .." if the signing fails. *)
val sign :
  randomness:bytes -> signing_key -> bytes -> (signature, string) result

(** [verify verification_key message signature] checks if the given
    [signature] is a valid signature of a [message] under a
    [verification_key].

    @return an error string "Verification failed: .." if the verification
      fails. *)
val verify : verification_key -> bytes -> signature -> (unit, string) result
