(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

type public_key_hash =
  | Ed25519 of Ed25519.Public_key_hash.t
  | Secp256k1 of Secp256k1.Public_key_hash.t
  | P256 of P256.Public_key_hash.t
  | Bls of Bls.Public_key_hash.t

type public_key =
  | Ed25519 of Ed25519.Public_key.t
  | Secp256k1 of Secp256k1.Public_key.t
  | P256 of P256.Public_key.t
  | Bls of Bls.Public_key.t

type secret_key =
  | Ed25519 of Ed25519.Secret_key.t
  | Secp256k1 of Secp256k1.Secret_key.t
  | P256 of P256.Secret_key.t
  | Bls of Bls.Secret_key.t

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

(** [append sk buf] is the concatenation of [buf] and the
    serialization of the signature of [buf] signed by [sk]. *)
val append : ?watermark:watermark -> secret_key -> Bytes.t -> Bytes.t

(** [concat buf t] is the concatenation of [buf] and the serialization
    of [t]. *)
val concat : Bytes.t -> t -> Bytes.t

include S.RAW_DATA with type t := t

(** The size of the signature in bytes. Can be [64] for Ed25519, Secp256k1 and
    P256 signatures or [96] for BLS signatures.  *)
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

(** The type of signing algorithms. *)
type algo = Ed25519 | Secp256k1 | P256 | Bls

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
  (** Convert a public key hash from V0 to V1. *)
  val public_key_hash : Signature_v0.Public_key_hash.t -> Public_key_hash.t

  (** Convert a public key from V0 to V1. *)
  val public_key : Signature_v0.Public_key.t -> Public_key.t

  (** Convert a secret key from V0 to V1. *)
  val secret_key : Signature_v0.Secret_key.t -> Secret_key.t

  (** Convert a signature from V0 to V1. *)
  val signature : Signature_v0.t -> t
end
