(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type transaction_object_t =
  | Evm of Transaction_object.t
  | Michelson of Tezos_types.Operation.t

type payload_t =
  | Evm_payload of Ethereum_types.hex
  | Michelson_payload of Ethereum_types.hex

let payload_raw = function Evm_payload hex | Michelson_payload hex -> hex

let payload_method = function
  | Evm_payload _ -> Rpc_encodings.Send_raw_transaction.method_
  | Michelson_payload _ -> Rpc_encodings.Send_raw_tezlink_operation.method_

let tag_payload tx_object payload =
  match tx_object with
  | Evm _ -> Evm_payload payload
  | Michelson _ -> Michelson_payload payload

type preconfirmed_transactions_result = {
  accepted : Ethereum_types.hash list;
  refused : Ethereum_types.hash list;
  dropped : Ethereum_types.hash list;
}

type tezlink_batch_nonces = {first : Z.t; length : int}

type shared_nonce =
  | Evm_nonce of Ethereum_types.quantity
  | Michelson_nonce of tezlink_batch_nonces

module L2_transaction = struct
  type t = transaction_object_t

  type address = string

  type nonce = shared_nonce

  let address_encoding = Data_encoding.string

  let hash_of_tx_object = function
    | Evm tx_object -> Transaction_object.hash tx_object
    | Michelson operation -> Tezos_types.Operation.hash_operation operation

  let address_to_string addr = addr

  let address_of_string addr = addr

  let from_address_of_tx_object = function
    | Evm tx_object ->
        let (Ethereum_types.Address (Ethereum_types.Hex sender)) =
          Transaction_object.sender tx_object
        in
        sender
    | Michelson operation ->
        Signature.V2.Public_key_hash.to_string operation.source

  let bitset_add_nonce bitset = function
    | Evm_nonce (Ethereum_types.Qty nonce) -> Nonce_bitset.add bitset ~nonce
    | Michelson_nonce {first; length} ->
        Nonce_bitset.add_many bitset ~nonce:first ~length

  let bitset_remove_nonce bitset = function
    | Evm_nonce (Ethereum_types.Qty nonce) -> Nonce_bitset.remove bitset ~nonce
    | Michelson_nonce {first; length} ->
        Nonce_bitset.remove_many bitset ~nonce:first ~length

  let next_nonce = function
    | Evm_nonce (Ethereum_types.Qty nonce) -> Z.succ nonce
    | Michelson_nonce {first; length} -> Z.(add first (of_int length))

  let nonce_to_z_opt = function
    | Evm_nonce (Ethereum_types.Qty nonce) -> Some nonce
    | Michelson_nonce _nonce -> None

  let nonce_of_tx_object = function
    | Evm tx_object -> Evm_nonce (Transaction_object.nonce tx_object)
    | Michelson operation ->
        Michelson_nonce
          {first = operation.first_counter; length = operation.length}

  let to_transaction_object_t tx_object = tx_object

  module AddressMap = Map.Make (String)

  let forward_batch_method = function
    | Evm _ -> Rpc_encodings.Send_raw_transaction.method_
    | Michelson _ -> Rpc_encodings.Send_raw_tezlink_operation.method_

  let add_txpool_entry tx_nonce tx_object content =
    let sender = Transaction_object.sender tx_object in
    let nonce_map =
      Ethereum_types.AddressMap.find_opt sender content
      |> Option.value ~default:Ethereum_types.NonceMap.empty
    in
    let nonce_map = Ethereum_types.NonceMap.add tx_nonce tx_object nonce_map in
    Ethereum_types.AddressMap.add sender nonce_map content

  let fold_txpool tx_map =
    AddressMap.fold
      (fun _address nonce_map content ->
        Ethereum_types.NonceMap.fold
          (fun tx_nonce tx_object content ->
            match tx_object with
            | Evm tx_object -> add_txpool_entry tx_nonce tx_object content
            | Michelson _operation -> content)
          nonce_map
          content)
      tx_map
      Ethereum_types.AddressMap.empty

  let make_txpool ~pending ~queued : Transaction_object.txqueue_content =
    {pending = fold_txpool pending; queued = fold_txpool queued}
end
