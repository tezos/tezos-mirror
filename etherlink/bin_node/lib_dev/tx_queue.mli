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

(** [tx_container ~chain_family] is a pair [(start, container)] where
    [start ~config ~max_transaction_batch_length ()] starts the
    worker, meaning it is possible to call {!inject}, {!confirm} and
    {!beacon} and [container] is a wrapper of the Tx_queue to be
    compatible with the Tx_container signature for the services. *)
val tx_container :
  chain_family:'f L2_types.chain_family ->
  (config:Configuration.tx_queue ->
  keep_alive:bool ->
  timeout:float ->
  unit ->
  unit tzresult Lwt.t)
  * 'f Services_backend_sig.tx_container

(**/*)

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
