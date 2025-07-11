(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t

type hash_algorithm = Blake2B | Keccak256

val from_gcp_key :
  Configuration.gcp_kms -> Configuration.gcp_key -> t tzresult Lwt.t

val gcp_key : t -> Configuration.gcp_key

val public_key : t -> Signature.Public_key.t

val sign : t -> hash_algorithm -> bytes -> Signature.t tzresult Lwt.t

(** [ethereum_address_opt kms] returns the Ethereum address of the key managed
    by [kms], if said key is compatible ([EC_SIGN_SECP256K1_SHA256]). Returns
    [None] otherwise. *)
val ethereum_address_opt : t -> Ethereum_types.address option
