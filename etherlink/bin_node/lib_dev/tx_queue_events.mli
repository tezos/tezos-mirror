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

(** [injecting_transactions nb] advertises [nb] transactions are about to be
    injected to the relay endpoint with a batch of [eth_sendRawTransaction]. *)
val injecting_transactions : int -> unit Lwt.t

(** [rpc_error error] advertises an RPC produced the error [error]. *)
val rpc_error : Rpc_encodings.JSONRPC.error -> unit Lwt.t
