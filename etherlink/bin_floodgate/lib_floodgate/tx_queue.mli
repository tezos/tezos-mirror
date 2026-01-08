(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** The Tx_queue is a worker allowing to batch raw transactions in a single
    [eth_sendRawTransaction] at a regular interval. It provides a non-blocking
    interface based on the use of callbacks. *)

(** A [callback] is called by the [Tx_queue] at various stage of a submitted
    transaction life.

    The next tick after its insertion in the queue, a transaction is submitted
    to the relay node within a batch of [eth_sendRawTransaction] requests.

    {ul
      {li Depending on the result of the RPC, its [callback] is called with
          either [`Accepted hash] (where [hash] is the hash of the raw
          transaction) or [`Refused]).}
      {li As soon as the transaction appears in a blueprint, its callback is
          called with [`Confirmed]. If this does not happen before 2s, the
          [callback] is called with [`Dropped].}} *)
type callback =
  [`Accepted of Ethereum_types.hash | `Confirmed | `Dropped | `Refused] ->
  unit Lwt.t

(** A [request] submitted to the [Tx_queue] consists in a payload (that is, a
    raw transaction) and a {!callback} that will be used to advertise the
    transaction life cycle. *)
type request = {payload : Ethereum_types.hex; callback : callback}

(** [start ~relay_endpoint ~max_transaction_batch_length ()] starts
    the worker, meaning it is possible to call {!inject}, {!confirm}
    and {!beacon}. *)
val start :
  relay_endpoint:Uri.t ->
  max_transaction_batch_length:int option ->
  ?inclusion_timeout:float ->
  unit ->
  unit tzresult Lwt.t

(** [inject ?callback raw_txn] pushes the raw transaction [raw_txn] to the
    worker queue.

    {b Note:} The promise will be sleeping until at least {!start} is called. *)
val inject : ?callback:callback -> Ethereum_types.hex -> unit Lwt.t

(** [confirm hash] is to be called by an external component to advertise a
    transaction has been included in a blueprint. *)
val confirm : Ethereum_types.hash -> unit tzresult Lwt.t

(** [beacon ~tick_interval] is a never fulfilled promise which triggers a tick
    in the [Tx_queue] every [tick_interval] seconds. *)
val beacon : tick_interval:float -> unit tzresult Lwt.t

(** [transfer ?callback ~infos sender receiver value] inject a transaction
    moving [value] native tokens from [sender] to [receiver]. The nonce and
    balance of [receiver] (see {!Account.t}). Additionally, a [callback] can be
    provided, and will be called according to the transaction’s lifecycle (see
    {!callback}). *)
val transfer :
  ?callback:callback ->
  ?to_:Efunc_core.Eth.address ->
  ?value:Ethereum_types.NonceMap.key ->
  ?nonce:Ethereum_types.NonceMap.key ->
  ?data:Efunc_core.Private.b ->
  gas_limit:Ethereum_types.NonceMap.key ->
  infos:Network_info.t ->
  from:Account.t ->
  unit ->
  unit Lwt.t

(** Stops the worker for the tx queue. *)
val shutdown : unit -> unit Lwt.t

module Misc : sig
  (** [send_raw_transaction ~relay_endpoint hex_raw_txn] sends [hex_raw_txn] to
      [relay_endpoint] using the [eth_sendRawTransaction] method, without going
      through the [Tx_queue]. *)
  val send_raw_transaction :
    relay_endpoint:Uri.t ->
    Ethereum_types.hex ->
    Ethereum_types.hash tzresult Lwt.t
end
