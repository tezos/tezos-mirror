(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t

val wallet : #Client_context.wallet -> Client_keys.sk_uri -> t

val sign : t -> bytes -> Signature.t tzresult Lwt.t
