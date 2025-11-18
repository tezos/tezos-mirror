(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** Watcher that gets notified each time a transaction is added to the
    pending state. *)
let txs_watcher :
    ( Transaction_object.t,
      Transaction_receipt.t )
    Ethereum_types.Subscription.output
    Lwt_watcher.input =
  Lwt_watcher.create_input ()

let create_stream () = Lwt_watcher.create_stream txs_watcher

let notify tx_hash =
  Lwt_watcher.notify
    txs_watcher
    (Ethereum_types.Subscription.NewPendingTransactions tx_hash)
