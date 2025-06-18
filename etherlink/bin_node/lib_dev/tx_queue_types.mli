(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module type L2_transaction = sig
  type t

  type legacy

  type address

  val address_encoding : address Data_encoding.t

  val hash_of_tx_object : legacy -> Ethereum_types.hash

  val address_to_string : address -> string

  val from_address_of_tx_object : legacy -> address

  val nonce_of_tx_object : legacy -> Ethereum_types.quantity

  val transaction_object_from_legacy : legacy -> t

  module AddressMap : Map.S with type key = address

  val make_txpool :
    pending:legacy Ethereum_types.NonceMap.t AddressMap.t ->
    queued:legacy Ethereum_types.NonceMap.t AddressMap.t ->
    Ethereum_types.txpool
end

module Eth_transaction_object :
  L2_transaction
    with type t = Transaction_object.t
     and type legacy = Ethereum_types.legacy_transaction_object
     and type address = Ethereum_types.address
     and module AddressMap = Ethereum_types.AddressMap
