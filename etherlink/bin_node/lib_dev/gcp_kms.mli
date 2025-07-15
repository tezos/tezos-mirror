(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** A handler to a key held by a GCP KMS allowing to sign arbitrary payload. *)
type t

(** The hash algorithm to be used to compute the digest sent to GCP. *)
type hash_algorithm = Blake2B | Keccak256

(** [from_gcp_key config key] creates a new handler for the given [key].

    Are currently supported [EC_SIGN_P256_SHA256] and
    [EC_SIGN_SECP256K1_SHA256] keys. Returns an error when providing details
    about a key of an unsupported algorithm. *)
val from_gcp_key :
  Configuration.gcp_kms -> Configuration.gcp_key -> t tzresult Lwt.t

(** Give back the key identifier *)
val gcp_key : t -> Configuration.gcp_key

(** [public_key kms] returns the public key of the KMS cryptographic material
    encoded to be compatible with the Tezos blockchain. *)
val public_key : t -> Signature.Public_key.t

(** [sign kms algorithm payload] computes the signature of [payload] using the
    requested [algorithm] for computing the digest.

    To be noted that while GCP officially only support Sha256 digests, it will
    happily sign any 32-byte input which is why we can use it to sign
    blueprints (Blake2B) and Ethereum transactions (Keccak256). However, it
    means that arbitrary third-party tools recomputing the hash themselves will
    likely assume they need to use SHA256 to verify the signature of the
    original payload. *)
val sign : t -> hash_algorithm -> bytes -> Signature.t tzresult Lwt.t

(** [ethereum_address_opt kms] returns the Ethereum address of the key managed
    by [kms], if said key is compatible ([EC_SIGN_SECP256K1_SHA256]). Returns
    [None] otherwise. *)
val ethereum_address_opt : t -> Ethereum_types.address option
