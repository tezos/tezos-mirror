(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type transaction_object_t =
  | Evm of Transaction_object.t
  | Michelson of Tezos_types.Operation.t

(** Wraps the transaction payload with the chain family so that code
    can behave differently on each type. *)
type payload_t =
  | Evm_payload of Ethereum_types.hex
  | Michelson_payload of Ethereum_types.hex

val payload_raw : payload_t -> Ethereum_types.hex

val payload_method : payload_t -> string

val tag_payload : transaction_object_t -> Ethereum_types.hex -> payload_t

type preconfirmed_transactions_result = {
  accepted : Ethereum_types.hash list;
      (** Transactions that were validated by the
          preconfirm_transactions request. *)
  refused : Ethereum_types.hash list;
      (** Transactions that were invalidated by the preconfirm_transactions request. *)
  dropped : Ethereum_types.hash list;
      (** Transactions that were dropped by the
          preconfirm_transactions request when there is already too
          many transaction. *)
}

type t = transaction_object_t

type address

type nonce

val address_encoding : address Data_encoding.t

val hash_of_tx_object : t -> Ethereum_types.hash

val address_to_string : address -> string

val address_of_string : string -> address

val from_address_of_tx_object : t -> address

val bitset_add_nonce : Nonce_bitset.t -> nonce -> Nonce_bitset.t tzresult

val bitset_remove_nonce : Nonce_bitset.t -> nonce -> Nonce_bitset.t tzresult

val next_nonce : nonce -> Z.t

(* Used only for the [Content] request. *)
val nonce_to_z_opt : nonce -> Z.t option

val nonce_of_tx_object : t -> nonce

val to_transaction_object_t : t -> transaction_object_t

module AddressMap : Map.S with type key = address

val forward_batch_method : t -> string

val make_txpool :
  pending:t Ethereum_types.NonceMap.t AddressMap.t ->
  queued:t Ethereum_types.NonceMap.t AddressMap.t ->
  Transaction_object.txqueue_content

type tezlink_batch_nonces = {first : Z.t; length : int}

type shared_nonce =
  | Evm_nonce of Ethereum_types.quantity
  | Michelson_nonce of tezlink_batch_nonces

type preconfirm_transactions =
  transactions:(string * transaction_object_t) list ->
  preconfirmed_transactions_result tzresult Lwt.t

type error += IC_disabled

type endpoint =
  | Rpc of Uri.t
  | Websocket of Websocket_client.t
  | Block_producer of preconfirm_transactions

type callback_status = [`Accepted | `Confirmed | `Dropped | `Refused]

type 'a variant_callback = 'a -> unit Lwt.t

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
type callback = callback_status variant_callback
