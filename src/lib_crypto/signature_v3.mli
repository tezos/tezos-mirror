(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type public_key_hash =
  | Ed25519 of Ed25519.Public_key_hash.t
  | Secp256k1 of Secp256k1.Public_key_hash.t
  | P256 of P256.Public_key_hash.t
  | Bls of Bls.Public_key_hash.t
  | Mldsa44 of Mldsa44.Public_key_hash.t

type public_key =
  | Ed25519 of Ed25519.Public_key.t
  | Secp256k1 of Secp256k1.Public_key.t
  | P256 of P256.Public_key.t
  | Bls of Bls.Public_key.t
  | Mldsa44 of Mldsa44.Public_key.t

type secret_key =
  | Ed25519 of Ed25519.Secret_key.t
  | Secp256k1 of Secp256k1.Secret_key.t
  | P256 of P256.Secret_key.t
  | Bls of Bls.Secret_key.t
  | Mldsa44 of Mldsa44.Secret_key.t

type watermark = Signature_v0.watermark =
  | Block_header of Chain_id.t
  | Endorsement of Chain_id.t
  | Generic_operation
  | Custom of Bytes.t

val bytes_of_watermark : watermark -> Bytes.t

val pp_watermark : Format.formatter -> watermark -> unit

type signature =
  | Ed25519 of Ed25519.t
  | Secp256k1 of Secp256k1.t
  | P256 of P256.t
  | Bls of Bls.t
  | Mldsa44 of Mldsa44.t
  | Unknown of Bytes.t

(** A signature prefix holds data only for signature that are more than 64 bytes
    long.  *)
type prefix = Bls_prefix of Bytes.t

include
  S.SPLIT_SIGNATURE
    with type Public_key_hash.t = public_key_hash
     and type Public_key.t = public_key
     and type Secret_key.t = secret_key
     and type watermark := watermark
     and type prefix := prefix
     and type t = signature

module Public_key_hash : sig
  include module type of Public_key_hash

  val is_bls : t -> bool
end

(** [append sk buf] is the concatenation of [buf] and the
    serialization of the signature of [buf] signed by [sk]. *)
val append : ?watermark:watermark -> secret_key -> Bytes.t -> Bytes.t

(** [concat buf t] is the concatenation of [buf] and the serialization
    of [t]. *)
val concat : Bytes.t -> t -> Bytes.t

include S.RAW_DATA with type t := t

(** The size of the signature in bytes. Can be [64] for Ed25519, Secp256k1 and
    P256 signatures, [96] for BLS signatures and [2420] for ML-DSA-44
    signatures. *)
val size : t -> int

(** [of_secp256k1 s] returns a wrapped version of the Secp256k1 signature [s] in
    {!t}. *)
val of_secp256k1 : Secp256k1.t -> t

(** [of_ed25519 s] returns a wrapped version of the Ed25519 signature [s] in
    {!t}. *)
val of_ed25519 : Ed25519.t -> t

(** [of_p256 s] returns a wrapped version of the P256 signature [s] in {!t}. *)
val of_p256 : P256.t -> t

(** [of_bls s] returns a wrapped version of the BLS signature [s] in {!t}. *)
val of_bls : Bls.t -> t

(** [of_mldsa44 s] returns a wrapped version of the Mldsa44 signature [s] in
    {!t}. *)
val of_mldsa44 : Mldsa44.t -> t

(** The type of signing algorithms. *)
type algo = Ed25519 | Secp256k1 | P256 | Bls | Mldsa44

(** The list of signing algorithm supported, i.e. all constructors of type
    {!algo}. *)
val algos : algo list

(** [generate_key ~algo ~seed ()] generates a key pair for the signing algorithm
    [algo] from the random seed [seed]. *)
val generate_key :
  ?algo:algo ->
  ?seed:Bytes.t ->
  unit ->
  public_key_hash * public_key * secret_key

(** This module provides conversion functions for values (of keys and
    signatures) of the module {!Signature_V0}. Note that these functions are
    total because [Signature_v1] supports more signature kinds than
    {!Signature_v0}. *)
module Of_V0 : sig
  (** Convert a public key hash from V0 to V2. *)
  val public_key_hash : Signature_v0.Public_key_hash.t -> Public_key_hash.t

  (** Convert a public key from V0 to V2. *)
  val public_key : Signature_v0.Public_key.t -> Public_key.t

  (** Convert a secret key from V0 to V2. *)
  val secret_key : Signature_v0.Secret_key.t -> Secret_key.t

  (** Convert a signature from V0 to V2. *)
  val signature : Signature_v0.t -> t
end

(** This module provides conversion functions for values (of keys and
    signatures) of the module {!Signature_V1}. Note that these functions are
    total because [Signature_v1] supports more signature kinds than
    {!Signature_v0}. *)
module Of_V1 : sig
  (** Convert a public key hash from V1 to V2. *)
  val public_key_hash : Signature_v1.Public_key_hash.t -> Public_key_hash.t

  (** Convert a public key from V1 to V2. *)
  val public_key : Signature_v1.Public_key.t -> Public_key.t

  (** Convert a secret key from V1 to V2. *)
  val secret_key : Signature_v1.Secret_key.t -> Secret_key.t

  (** Convert a signature from V1 to V2. *)
  val signature : Signature_v1.t -> t
end

(** This module provides conversion functions for values (of keys and
    signatures) of the module {!Signature_V2}. Note that these functions are
    total because [Signature_v1] supports more signature kinds than
    {!Signature_v0}. *)
module Of_V2 : sig
  (** Convert a public key hash from V2 to V3. *)
  val public_key_hash : Signature_v2.Public_key_hash.t -> Public_key_hash.t

  (** Convert a public key from V2 to V3. *)
  val public_key : Signature_v2.Public_key.t -> Public_key.t

  (** Convert a secret key from V2 to V3. *)
  val secret_key : Signature_v2.Secret_key.t -> Secret_key.t

  (** Convert a signature from V2 to V3. *)
  val signature : Signature_v2.t -> t
end
