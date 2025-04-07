(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type error += Not_a_blueprint

val transaction_hashes :
  Blueprint_types.with_events -> Ethereum_types.hash list tzresult
