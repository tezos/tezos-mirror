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

(** A [callback] is called by the [Tx_queue] at various stages of a
    submitted transaction's life.

    The next tick after its insertion in the queue, a transaction is submitted
    to the relay node within a batch of [eth_sendRawTransaction] requests.

    {ul
      {li Depending on the result of the RPC, its [callback] is called with
          either [`Accepted] or [`Refused]).}
      {li As soon as the transaction appears in a blueprint, its callback is
          called with [`Confirmed]. If this does not happen before 2s, the
          [callback] is called with [`Dropped].}} *)
type callback = [`Accepted | `Confirmed | `Dropped | `Refused] -> unit Lwt.t

(** Inject transactions with either RPCs or on a websocket connection. *)
type endpoint = Rpc of Uri.t | Websocket of Websocket_client.t

(** [start ~config ~max_transaction_batch_length ()] starts the
    worker, meaning it is possible to call {!inject}, {!confirm} and
    {!beacon}. *)
val start :
  config:Configuration.tx_queue ->
  keep_alive:bool ->
  unit ->
  unit tzresult Lwt.t

(** [shutdown ()] stops the tx queue, waiting for the ongoing request
    to be processed. *)
val shutdown : unit -> unit tzresult Lwt.t

(** [clear ()] removes the tx queue data but keeps the allocated space *)
val clear : unit -> unit tzresult Lwt.t

(** [inject ?callback ~next_nonce tx_object raw_txn] pushes the
    transaction [raw_txn] to the worker queue.

    The [tx_object] is stored until the transaction is confirmed or
    dropped, so the transaction can be retrieved with [find].

    [next_nonce] is the next nonce expected by the kernel for the
    address [tx_object.from]. [next_nonce] must always be increasing
    for any given [tx_object.from], else if the tx_queue already
    contains a transaction for [tx_object.from] it will raise an error
    for that request. The increasing order of [next_nonce] is enforced
    by the kernel execution where transaction must be executed in
    order.

    Any transaction added in the tx_queue via {!inject} must be a
    valid transaction. In particular the nonce of the transaction must
    be valid, i.e. it must be greater or equal to [next_nonce]. This
    is validated by {!Validate.is_tx_valid}.

    {b Note:} The promise will be sleeping until at least {!start} is called. *)
val inject :
  ?callback:callback ->
  next_nonce:Ethereum_types.quantity ->
  Ethereum_types.legacy_transaction_object ->
  Ethereum_types.hex ->
  (unit, string) result tzresult Lwt.t

(** [confirm hash] is to be called by an external component to advertise a
    transaction has been included in a blueprint. *)
val confirm : Ethereum_types.hash -> unit tzresult Lwt.t

(** Trigger a tick in the [Tx_queue]. *)
val tick : evm_node_endpoint:endpoint -> unit tzresult Lwt.t

(** [beacon ~evm_node_endpoint ~tick_interval] is a never fulfilled
    promise which triggers a tick in the [Tx_queue] every
    [tick_interval] seconds. *)
val beacon :
  evm_node_endpoint:endpoint -> tick_interval:float -> unit tzresult Lwt.t

(** [find hash] returns the transaction associated with that hash if
    it's found in the tx_queue. *)
val find :
  Ethereum_types.hash ->
  Ethereum_types.legacy_transaction_object option tzresult Lwt.t

(** [nonce ~next_nonce address] returns the first gap in the tx queue
    for [address], or [next_nonce] if no transaction for [address] are
    found. *)
val nonce :
  next_nonce:Ethereum_types.quantity ->
  Ethereum_types.address ->
  Ethereum_types.quantity tzresult Lwt.t

(** [lock_transactions] locks the transactions in the queue, new
    transactions can be added but nothing can be retrieved with
    {!pop_transactions}. *)
val lock_transactions : unit -> unit tzresult Lwt.t

(** [unlock_transactions] unlocks the transactions if it was locked by
    {!lock_transactions}. *)
val unlock_transactions : unit -> unit tzresult Lwt.t

(** [is_locked] checks if the queue is locked. *)
val is_locked : unit -> bool tzresult Lwt.t

(** [content ()] returns the queued and pending transactions of the
    tx_queue mapped into a tx_pool to mimic
    {!Tx_pool.get_tx_pool_content}. Semantics of pending and queued
    are not equal to {!Tx_pool.get_tx_pool_content} *)
val content : unit -> Ethereum_types.txpool tzresult Lwt.t

(** [pop_transactions ~validate_tx ~initial_validation_state] pops as
    many transactions as possible from the queue, validating them with
    [validate_tx]. If [validate_tx] returns [`Keep validation_state]
    then the evaluated transaction is popped, else if it returns
    [`Drop], it's considered invalid and it's callback is called with
    [`Refused]. If [validate_tx] returns [`Stop] then the caller has
    enough transactions.

    If the tx_queue is locked (c.f. {!lock_transactions} then returns
    the empty list. *)
val pop_transactions :
  validate_tx:
    ('a ->
    string ->
    Ethereum_types.legacy_transaction_object ->
    [`Keep of 'a | `Drop | `Stop] tzresult Lwt.t) ->
  initial_validation_state:'a ->
  (string * Ethereum_types.legacy_transaction_object) list tzresult Lwt.t

(** [confirm_transactions ~clear_pending_queue_after ~confirmed_txs]
    confirms [confirmed_txs] hash. If [drop_unconfirmed] then any
    other pending transactions in the tx_queue are dropped. *)
val confirm_transactions :
  clear_pending_queue_after:bool ->
  confirmed_txs:Ethereum_types.hash Seq.t ->
  unit tzresult Lwt.t

(** wrapper of the Tx_queue to be compatible with the Tx_container
    signature for the services. *)
module Tx_container : Services_backend_sig.Tx_container

(**/*)

module Internal_for_tests : sig
  module Nonce_bitset : sig
    type t = {next_nonce : Z.t; bitset : Tezos_base.Bitset.t}

    val create : next_nonce:Z.t -> t

    val offset : nonce1:Z.t -> nonce2:Z.t -> int tzresult

    val add : t -> nonce:Z.t -> t tzresult

    val remove : t -> nonce:Z.t -> t tzresult

    val shift : t -> nonce:Z.t -> t tzresult

    val is_empty : t -> bool

    val next_gap : t -> Z.t

    val shift_then_next_gap : t -> shift_nonce:Z.t -> Z.t tzresult
  end

  module Address_nonce : sig
    type t

    val empty : start_size:int -> t

    val add : t -> addr:string -> next_nonce:Z.t -> nonce:Z.t -> unit tzresult

    val find : t -> addr:string -> Nonce_bitset.t option

    val confirm_nonce : t -> addr:string -> nonce:Z.t -> unit tzresult

    val remove : t -> addr:string -> nonce:Z.t -> unit tzresult

    val next_gap : t -> addr:string -> next_nonce:Z.t -> Z.t tzresult
  end
end
