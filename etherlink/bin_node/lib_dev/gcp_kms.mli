(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t

type hash_algorithm = Blake2B

val from_gcp_key :
  Configuration.gcp_kms -> Configuration.gcp_key -> t tzresult Lwt.t

val gcp_key : t -> Configuration.gcp_key

val public_key : t -> Signature.Public_key.t

val sign : t -> hash_algorithm -> bytes -> Signature.t tzresult Lwt.t
