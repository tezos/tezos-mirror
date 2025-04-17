(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2024-2025 Functori <contact@functori.com>                   *)
(*                                                                           *)
(*****************************************************************************)

type chain_id = Chain_id of Z.t [@@ocaml.unboxed]

module Chain_id = struct
  let of_string_exn s = Chain_id (Z.of_string s)

  let to_string (Chain_id s) = Z.to_string s

  let encoding =
    Data_encoding.conv
      (fun (Chain_id c) -> Helpers.z_to_hexa c)
      of_string_exn
      Data_encoding.string

  let decode_le bytes = Chain_id (Helpers.decode_z_le bytes)

  let decode_be bytes = Chain_id (Helpers.decode_z_be bytes)

  let compare (Chain_id c1) (Chain_id c2) = Z.compare c1 c2

  let pp fmt (Chain_id cid) =
    Format.fprintf fmt "Chain_id (%s)" (Z.to_string cid)
end

type chain_family = EVM | Michelson

module Chain_family = struct
  let to_string = function EVM -> "EVM" | Michelson -> "Michelson"

  let of_string_exn s =
    match String.lowercase_ascii s with
    | "evm" -> EVM
    | "michelson" -> Michelson
    | _ -> invalid_arg "Chain_family.of_string"

  let encoding =
    Data_encoding.string_enum [("EVM", EVM); ("Michelson", Michelson)]

  let pp fmt cf = Format.fprintf fmt "%s" (to_string cf)
end

module Tezos_block = struct
  type t = {
    level : int32;
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
      let level = Bytes.get_int32_be bytes 0 in
      let previous_hash = Bytes.make 32 '\000' in
      Bytes.blit bytes 4 previous_hash 0 32 ;
      let parent = Ethereum_types.decode_block_hash previous_hash in
      let timestamp = Bytes.make 8 '\000' in
      Bytes.blit bytes 36 timestamp 0 8 ;
      let timestamp = Ethereum_types.Qty (Helpers.decode_z_be timestamp) in
      let block_hash = Block_hash.hash_bytes [bytes] in
      let hash =
        Ethereum_types.decode_block_hash (Block_hash.to_bytes block_hash)
      in
      {level; hash; timestamp; parent_hash = parent})
    else raise (Invalid_argument "Expected a string of length 44")

  let encode_block (block : t) : (string, string) result =
    let (Ethereum_types.Qty timestamp) = block.timestamp in

    let z_to_64_be z =
      let bytes = Bytes.make 64 '\000' in
      Bytes.set_int64_be bytes 0 (Z.to_int64 z) ;
      bytes
    in

    let parent_bytes = Ethereum_types.encode_block_hash block.parent_hash in
    let timestamp_bytes = z_to_64_be timestamp in

    let encoded_block = Bytes.create 44 in
    Bytes.set_int32_be encoded_block 0 block.level ;
    Bytes.blit parent_bytes 0 encoded_block 4 32 ;
    Bytes.blit timestamp_bytes 0 encoded_block 36 8 ;

    Ok (Bytes.to_string encoded_block)

  let decode_block (b : string) : (t, string) result =
    let b = Bytes.of_string b in
    try Ok (block_from_binary b) with e -> Error (Printexc.to_string e)
end

type 'a block = Eth of 'a Ethereum_types.block | Tez of Tezos_block.t

let block_hash block =
  match block with Eth block -> block.hash | Tez block -> block.hash

let block_number block =
  match block with
  | Eth block -> block.number
  | Tez block -> Qty (Z.of_int32 block.level)

let block_number_of_transactions block =
  match block with
  | Eth block ->
      let number_of_transactions =
        match block.transactions with
        | TxHash l -> List.length l
        | TxFull l -> List.length l
      in
      number_of_transactions
  | Tez _ -> 0

let block_parent block =
  match block with Eth block -> block.parent | Tez block -> block.parent_hash

let decode_block_hash ~chain_family bytes =
  match chain_family with
  | EVM -> Ethereum_types.decode_block_hash bytes
  | Michelson -> Tezos_block.decode_block_hash bytes

let genesis_parent_hash ~chain_family =
  match chain_family with
  | EVM -> Ethereum_types.genesis_parent_hash
  | Michelson -> Tezos_block.genesis_parent_hash

let block_from_bytes ~chain_family bytes =
  match chain_family with
  | EVM ->
      let eth_block = Ethereum_types.block_from_rlp bytes in
      Eth eth_block
  | Michelson ->
      let tez_block = Tezos_block.block_from_binary bytes in
      Tez tez_block
