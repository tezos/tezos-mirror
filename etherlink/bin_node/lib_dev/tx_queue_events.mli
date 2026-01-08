(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** [is_ready ()] advertises that the [Tx_queue] is ready to receive
    transactions. *)
val is_ready : unit -> unit Lwt.t

(** [shutdown ()] advertises that the [Tx_queue] is shutting down. *)
val shutdown : unit -> unit Lwt.t

(** [cleared ()] advertises that the [Tx_queue] finished clearing. *)
val cleared : unit -> unit Lwt.t

(** [injecting_transactions nb] advertises [nb] transactions are about to be
    injected to the relay endpoint with a batch of [eth_sendRawTransaction]. *)
val injecting_transactions : int -> unit Lwt.t

(** [injecting_transactions_failed err] advertized that the node was not able
    to inject some transactions and returned [err] instead. *)
val injecting_transactions_failed : tztrace -> unit Lwt.t

(** [add_transaction tx_hash] Advertises [tx_hash] was added to the tx
    queue. *)
val add_transaction : Ethereum_types.hash -> unit Lwt.t

(** [transaction_dropped tx_hash] Advertises [tx_hash] was dropped to the tx
    queue. *)
val transaction_dropped : Ethereum_types.hash -> unit Lwt.t

(** [transaction_confirmed tx_hash] Advertises [tx_hash] was confirmed
    to the tx queue. *)
val transaction_confirmed : Ethereum_types.hash -> unit Lwt.t

(** [rpc_error error] advertises an RPC produced the error [error]. *)
val rpc_error : Rpc_encodings.JSONRPC.error -> unit Lwt.t

(** [missing_tx_object hash] Advertises that it fails to find the
    tx_object associated to [hash]. *)
val missing_tx_object : Ethereum_types.hash -> unit Lwt.t

(** [callback_error__dont_wait__use_with_care error] advertises an RPC
    produced the error [error] without waiting. *)
val callback_error__dont_wait__use_with_care : tztrace -> unit

(** [callback_error error] advertises an RPC produced the error [error]. *)
val callback_error : tztrace -> unit Lwt.t
