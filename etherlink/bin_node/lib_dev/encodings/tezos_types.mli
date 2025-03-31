(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type block = {
  number : Ethereum_types.quantity;
  hash : Ethereum_types.block_hash;
  timestamp : Ethereum_types.quantity;
  parent_hash : Ethereum_types.block_hash;
}

val decode_block_hash : bytes -> Ethereum_types.block_hash

val genesis_parent_hash : Ethereum_types.block_hash

val block_from_binary : bytes -> block
