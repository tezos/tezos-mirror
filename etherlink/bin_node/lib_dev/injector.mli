(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

val call_singleton_request :
  keep_alive:bool ->
  timeout:float ->
  base:Uri.t ->
  (module Rpc_encodings.METHOD
     with type input = 'input
      and type output = 'output) ->
  Rpc_encodings.JSONRPC.request ->
  ('output, string) result tzresult Lwt.t

(** [send_raw_transaction ~keep_alive ~base raw_txn] uses the
    [eth_sendRawTransaction] RPC method to inject [raw_txn] to the endpoint
    [base]. It will retry as many time as necessary if [keep_alive] is set to
    true, or return an error otherwise. *)
val send_raw_transaction :
  keep_alive:bool ->
  timeout:float ->
  base:Uri.t ->
  raw_tx:string ->
  (Ethereum_types.hash, string) result tzresult Lwt.t

(** [send_raw_transaction_sync ~keep_alive ~base raw_txn block_parameter] uses the
    [eth_sendRawTransaction] RPC method to inject [raw_txn] to the endpoint
    [base], and waits for its confirmation. It will retry as many time as
    necessary if [keep_alive] is set to true, or return an error otherwise. *)
val send_raw_transaction_sync :
  keep_alive:bool ->
  timeout:float ->
  base:Uri.t ->
  raw_tx:Ethereum_types.hex ->
  internal_timeout:int64 ->
  block_parameter:Ethereum_types.Block_parameter.t ->
  (Transaction_receipt.t, string) result tzresult Lwt.t

val inject_transaction :
  keep_alive:bool ->
  timeout:float ->
  base:Uri.t ->
  tx_object:Transaction_object.t ->
  raw_tx:string ->
  wait_confirmation:bool ->
  (Ethereum_types.hash, string) result tzresult Lwt.t

val inject_tezlink_operation :
  keep_alive:bool ->
  timeout:float ->
  base:Uri.t ->
  op:Tezos_types.Operation.t ->
  raw_op:bytes ->
  (Ethereum_types.hash, string) result tzresult Lwt.t

val get_transaction_count :
  keep_alive:bool ->
  timeout:float ->
  base:Uri.t ->
  Ethereum_types.address ->
  Ethereum_types.Block_parameter.extended ->
  (Ethereum_types.quantity, string) result tzresult Lwt.t

val get_transaction_by_hash :
  keep_alive:bool ->
  timeout:float ->
  base:Uri.t ->
  Ethereum_types.hash ->
  (Transaction_object.t option, string) result tzresult Lwt.t
