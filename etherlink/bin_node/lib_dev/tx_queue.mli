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

(** [nonce ~next_nonce address] must returns the next gap nonce
    available. *)
val nonce :
  next_nonce:Ethereum_types.quantity ->
  string ->
  Ethereum_types.quantity tzresult Lwt.t

(** [add ~next_nonce tx_object raw_tx] returns the next gap nonce
    available based on the pending transaction of the tx_queue.
    [next_nonce] is the next expected nonce found in the backend. *)
val add :
  ?callback:Services_backend_sig.callback ->
  next_nonce:Ethereum_types.quantity ->
  Tx_queue_types.transaction_object_t ->
  raw_tx:Ethereum_types.hex ->
  (Ethereum_types.hash, string) result tzresult Lwt.t

(** [find hash] returns the transaction_object found in tx
    container. *)
val find :
  Ethereum_types.hash ->
  Tx_queue_types.transaction_object_t option tzresult Lwt.t

(** [content ()] returns all the transactions found in tx
    container. *)
val content : unit -> Transaction_object.txqueue_content tzresult Lwt.t

(** [shutdown ()] stops the tx container, waiting for the ongoing request
    to be processed. *)
val shutdown : unit -> unit tzresult Lwt.t

(** [clear ()] removes the container data but keeps the allocated space *)
val clear : unit -> unit tzresult Lwt.t

(** Trigger a tick in the [Tx_queue]. *)
val tx_queue_tick :
  evm_node_endpoint:Services_backend_sig.endpoint -> unit tzresult Lwt.t

(** [tx_queue_beacon ~evm_node_endpoint ~tick_interval] is a never fulfilled
    promise which triggers a tick in the [Tx_queue] every
    [tick_interval] seconds. *)
val tx_queue_beacon :
  evm_node_endpoint:Services_backend_sig.endpoint ->
  tick_interval:float ->
  unit tzresult Lwt.t

(** The Tx_queue has a table of pending transactions. There are two
    ways for transactions to be removed from this table; either they
    are confirmed because they have been seen in a block or they are
    dropped.

    [confirm_transactions ~clear_pending_queue_after ~confirmed_txs]
    confirms [confirmed_txs] hash. If [clear_pending_queue_after]
    then any other pending transactions in the tx_queue are
    dropped. *)
val confirm_transactions :
  clear_pending_queue_after:bool ->
  confirmed_txs:Ethereum_types.hash Seq.t ->
  unit tzresult Lwt.t

(** The Tx_queue has a table of pending transactions. There are two
    ways for transactions to be removed from this table; either they
    are confirmed because they have been seen in a block or they are
    dropped.

    [dropped_transaction ~dropped_tx] drops [dropped_tx] hash. *)
val dropped_transaction :
  dropped_tx:Ethereum_types.hash -> reason:string -> unit tzresult Lwt.t

(** [add_pending_callback hash ~callback] registers [callback] for the
    transaction associated with [hash].
    If the transaction is not found, [callback] is called
    immediately with [`Missing]. Otherwise, [callback] will be
    called when the transaction is either confirmed or dropped. *)
val add_pending_callback :
  Ethereum_types.hash ->
  callback:
    [`Confirmed | `Dropped | `Missing] Services_backend_sig.variant_callback ->
  unit tzresult Lwt.t

(** The Tx_pool pops transactions until the sum of the sizes of the
    popped transactions reaches maximum_cumulative_size; it ignores
    the [validate_tx] and [initial_validation_state] arguments, The
    Tx_queue however ignores [maximum_cumulative_size] and instead
    uses [validate_tx] to pop valid transactions until either `Drop
    or `Stop is returned. *)
val pop_transactions :
  maximum_cumulative_size:int ->
  validate_tx:
    ('a ->
    string ->
    Tx_queue_types.transaction_object_t ->
    [`Keep of 'a | `Drop of string | `Stop] tzresult Lwt.t) ->
  initial_validation_state:'a ->
  (string * Tx_queue_types.transaction_object_t) list tzresult Lwt.t

(** [size_info] returns the size of the tx container. *)
val size_info : unit -> Metrics.Tx_pool.size_info tzresult Lwt.t

val start :
  config:Configuration.tx_queue ->
  keep_alive:bool ->
  timeout:float ->
  start_injector_worker:bool ->
  unit ->
  unit tzresult Lwt.t

module Internal_for_tests : sig
  module Address_nonce : sig
    type t

    val empty : start_size:int -> t

    val add :
      t ->
      addr:string ->
      next_nonce:Z.t ->
      nonce:'a ->
      add:(Nonce_bitset.t -> 'a -> Nonce_bitset.t tzresult) ->
      unit tzresult

    val find : t -> addr:string -> Nonce_bitset.t option

    val confirm_nonce :
      t -> addr:string -> nonce:'a -> next:('a -> Z.t) -> unit tzresult

    val remove :
      t ->
      addr:string ->
      nonce:'a ->
      rm:(Nonce_bitset.t -> 'a -> Nonce_bitset.t tzresult) ->
      unit tzresult

    val next_gap : t -> addr:string -> next_nonce:Z.t -> Z.t tzresult
  end
end
