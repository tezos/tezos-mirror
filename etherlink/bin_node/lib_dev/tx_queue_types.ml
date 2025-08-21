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
     and module AddressMap = Ethereum_types.AddressMap = struct
  open Ethereum_types

  type t = Transaction_object.t

  type legacy = legacy_transaction_object

  type nonrec address = address

  let address_encoding = address_encoding

  let hash_of_tx_object (tx_object : legacy_transaction_object) = tx_object.hash

  let address_to_string (Address (Hex s)) = s

  let from_address_of_tx_object (tx_object : legacy_transaction_object) =
    tx_object.from

  let nonce_of_tx_object (tx_object : legacy_transaction_object) =
    tx_object.nonce

  let transaction_object_from_legacy =
    Transaction_object.from_store_transaction_object

  module AddressMap = AddressMap

  let make_txpool ~pending ~queued = {pending; queued}
end

module Tezlink_operation :
  L2_transaction
    with type t = Tezos_types.Operation.t
     and type legacy = Tezos_types.Operation.t = struct
  open Ethereum_types

  type t = Tezos_types.Operation.t

  type legacy = Tezos_types.Operation.t

  type address = Signature.V1.public_key_hash

  let address_encoding = Signature.V1.Public_key_hash.encoding

  let hash_of_tx_object = Tezos_types.Operation.hash_operation

  let address_to_string = Signature.V1.Public_key_hash.to_string

  let from_address_of_tx_object (op : Tezos_types.Operation.t) = op.source

  let nonce_of_tx_object (op : Tezos_types.Operation.t) =
    Ethereum_types.Qty op.counter

  let transaction_object_from_legacy op = op

  module AddressMap = Map.Make (Signature.V1.Public_key_hash)

  let make_txpool ~pending:_ ~queued:_ =
    {
      pending = Ethereum_types.AddressMap.empty;
      queued = Ethereum_types.AddressMap.empty;
    }
end
