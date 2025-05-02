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

(** [start ~config ~max_transaction_batch_length ()] starts the
    worker, meaning it is possible to call {!inject}, {!confirm} and
    {!beacon}. *)
val start :
  config:Configuration.tx_queue ->
  keep_alive:bool ->
  unit ->
  unit tzresult Lwt.t

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
