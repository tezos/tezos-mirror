(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(* WIP: Minimal Block Tezos (should be equal to shell_block header) *)
type block = {
  number : Ethereum_types.quantity;
  hash : Ethereum_types.block_hash;
  timestamp : Ethereum_types.quantity;
  parent_hash : Ethereum_types.block_hash;
}

let decode_block_hash = Ethereum_types.decode_block_hash

let genesis_parent_hash =
  (* This Hex comes from this b58 hash 'BLockGenesisGenesisGenesisGenesisGenesis1db77eJNeJ9' *)
  (* That is the ghostnet genesis hash according to 'devtools/get_contracts/config.ml' *)
  Ethereum_types.Block_hash
    (Hex "8fcf233671b6a04fcf679d2a381c2544ea6c1ea29ba6157776ed8423e7c02934")

(* This function may be replaced in the future by an already existing function *)
(* When Tezos block will be complete *)
let block_from_binary bytes =
  if Bytes.length bytes = 44 then (
    let number = Bytes.make 4 '\000' in
    Bytes.blit bytes 0 number 0 4 ;
    let number = Ethereum_types.Qty (Helpers.decode_z_be number) in
    let previous_hash = Bytes.make 32 '\000' in
    Bytes.blit bytes 4 previous_hash 0 32 ;
    let parent = Ethereum_types.decode_block_hash previous_hash in
    let timestamp = Bytes.make 8 '\000' in
    Bytes.blit bytes 36 timestamp 0 8 ;
    let timestamp = Ethereum_types.Qty (Helpers.decode_z_le timestamp) in
    let block_hash = Block_hash.hash_bytes [bytes] in
    let hash =
      Ethereum_types.decode_block_hash (Block_hash.to_bytes block_hash)
    in
    {number; hash; timestamp; parent_hash = parent})
  else raise (Invalid_argument "Expected a string of length 44")
