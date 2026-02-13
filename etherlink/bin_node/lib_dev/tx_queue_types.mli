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

module type L2_transaction = sig
  type t

  type address

  type nonce

  val address_encoding : address Data_encoding.t

  val hash_of_tx_object : t -> Ethereum_types.hash

  val address_to_string : address -> string

  val from_address_of_tx_object : t -> address

  val bitset_add_nonce : Nonce_bitset.t -> nonce -> Nonce_bitset.t tzresult

  val bitset_remove_nonce : Nonce_bitset.t -> nonce -> Nonce_bitset.t tzresult

  val next_nonce : nonce -> Z.t

  (* Used only for the [Content] request. *)
  val nonce_to_z_opt : nonce -> Z.t option

  val nonce_of_tx_object : t -> nonce

  val to_transaction_object_t : t -> transaction_object_t

  module AddressMap : Map.S with type key = address

  module Forward_batch :
    Rpc_encodings.METHOD
      with type input = Ethereum_types.hex
       and type output = Ethereum_types.hash

  val make_txpool :
    pending:t Ethereum_types.NonceMap.t AddressMap.t ->
    queued:t Ethereum_types.NonceMap.t AddressMap.t ->
    Transaction_object.txqueue_content
end

module Eth_transaction_object :
  L2_transaction
    with type t = Transaction_object.t
     and type address = Ethereum_types.address
     and type nonce = Ethereum_types.quantity
     and module AddressMap = Ethereum_types.AddressMap

type tezlink_batch_nonces = {first : Z.t; length : int}

module Tezlink_operation :
  L2_transaction
    with type t = Tezos_types.Operation.t
     and type nonce = tezlink_batch_nonces
