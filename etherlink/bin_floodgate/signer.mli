(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** An opaque signing mechanism *)
type t

(** An unprotected Secp256k1 secret key *)
type secret_key

(** [secret_key_from_hex h] returns the secret key decoded from [h], or
    an error. *)
val secret_key_from_hex : Hex.t -> secret_key tzresult

(** [from_secret_key sk] creates a new signer which will use [sk] to sign
    payloads. *)
val from_secret_key : secret_key -> t

(** [from_gcp config key] creates a new signer delegating signing to a GCP KMS
    holding the key. *)
val from_gcp_key :
  Configuration.gcp_kms -> Configuration.gcp_key -> t tzresult Lwt.t

(** [fresh ()] creates a new signer with fresh cryptographic material. *)
val fresh : unit -> t

(** [to_address signer] returns the Ethereum address owned by [signer] *)
val to_address : t -> Ethereum_types.address

(** [sign signer payload] uses [signer] to compute the signature of the
    Keccak256 hash of [payload] *)
val sign : t -> string -> Efunc_core.Types.signature tzresult Lwt.t
