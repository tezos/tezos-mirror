(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t = Wallet : #Client_context.wallet * Client_keys.sk_uri -> t

let wallet cctxt sk_uri = Wallet (cctxt, sk_uri)

let sign = function Wallet (cctxt, sk) -> Client_keys.sign cctxt sk
