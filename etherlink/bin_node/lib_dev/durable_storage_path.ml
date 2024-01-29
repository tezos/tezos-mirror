(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(* Copyright (c) 2023 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

type path = string

module EVM = struct
  let root = "/evm"

  let make s = root ^ s
end

let chain_id = EVM.make "/chain_id"

let base_fee_per_gas = EVM.make "/base_fee_per_gas"

let kernel_version = EVM.make "/kernel_version"

let kernel_upgrade = EVM.make "/kernel_upgrade"

module Accounts = struct
  let accounts = EVM.make "/eth_accounts"

  let balance = "/balance"

  let nonce = "/nonce"

  let code = "/code"

  let storage = "/storage"

  let account (Address (Hex s)) = accounts ^ "/" ^ s

  let balance address = account address ^ balance

  let nonce address = account address ^ nonce

  let code address = account address ^ code

  let storage address index = account address ^ storage ^ "/" ^ index
end

module Block = struct
  type number = Current | Nth of Z.t

  let blocks = EVM.make "/blocks"

  let number = "/number"

  let by_hash (Block_hash (Hex hash)) = blocks ^ "/" ^ hash

  let current_number = blocks ^ "/current" ^ number
end

module Indexes = struct
  let indexes = EVM.make "/indexes"

  let blocks = "/blocks"

  let blocks = indexes ^ blocks

  let number_to_string = function
    | Block.Current -> "current"
    | Nth i -> Z.to_string i

  let block_by_number number = blocks ^ "/" ^ number_to_string number
end

module Transaction_receipt = struct
  let receipts = EVM.make "/transactions_receipts"

  let receipt (Hash (Hex tx_hash)) = receipts ^ "/" ^ tx_hash
end

module Transaction_object = struct
  let objects = EVM.make "/transactions_objects"

  let object_ (Hash (Hex tx_hash)) = objects ^ "/" ^ tx_hash
end

module Delayed_transaction = struct
  let hashes = EVM.make "/delayed-inbox"

  let transaction (Hash (Hex tx_hash)) = hashes ^ "/" ^ tx_hash ^ "/data"
end
