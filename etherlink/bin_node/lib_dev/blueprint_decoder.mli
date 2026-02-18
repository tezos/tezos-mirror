(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [transactions payload] parses [payload] to extract the injected
    transactions along with their hash (except for delayed transactions, as the
    raw transaction is not contained in the blueprint in this case). *)
val transactions :
  Blueprint_types.payload ->
  (Ethereum_types.hash * Broadcast.common_transaction option) list tzresult
