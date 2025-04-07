(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [send_raw_transaction ~keep_alive ~base raw_txn] uses the
    [eth_sendRawTransaction] RPC method to inject [raw_txn] to the endpoint
    [base]. It will retry as many time as necessary if [keep_alive] is set to
    true, or return an error otherwise. *)
val send_raw_transaction :
  keep_alive:bool ->
  base:Uri.t ->
  raw_tx:string ->
  (Ethereum_types.hash, string) result tzresult Lwt.t

val inject_transaction :
  keep_alive:bool ->
  base:Uri.t ->
  tx_object:Ethereum_types.legacy_transaction_object ->
  raw_tx:string ->
  (Ethereum_types.hash, string) result tzresult Lwt.t
