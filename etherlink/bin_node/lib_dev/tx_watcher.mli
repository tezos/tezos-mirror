(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** Watcher that gets notified each time a transaction is added to the
    pending state. *)

val create_stream :
  unit ->
  ( Transaction_object.t,
    Transaction_receipt.t )
  Ethereum_types.Subscription.output
  Lwt_stream.t
  * Lwt_watcher.stopper

val notify : Ethereum_types.hash -> unit
