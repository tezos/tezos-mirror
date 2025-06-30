(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t

val from_gcp_key : Configuration.gcp_key -> t Lwt.t

val gcp_key : t -> Configuration.gcp_key

val public_key : t -> Signature.Public_key.t tzresult Lwt.t

val sign : t -> bytes -> Signature.t tzresult Lwt.t
