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

type evm_chain_family = Evm_chain_family

type michelson_chain_family = Michelson_chain_family

type _ chain_family =
  | EVM : evm_chain_family chain_family
  | Michelson : michelson_chain_family chain_family

type ex_chain_family = Ex_chain_family : _ chain_family -> ex_chain_family

module Chain_family = struct
  let to_string (type f) : f chain_family -> string = function
    | EVM -> "EVM"
    | Michelson -> "Michelson"

  let of_string_exn s =
    match String.lowercase_ascii s with
    | "evm" -> Ex_chain_family EVM
    | "michelson" -> Ex_chain_family Michelson
    | _ -> invalid_arg "Chain_family.of_string"

  let encoding =
    Data_encoding.string_enum
      [("EVM", Ex_chain_family EVM); ("Michelson", Ex_chain_family Michelson)]

  let pp fmt cf = Format.fprintf fmt "%s" (to_string cf)
end

module Tezos_block = struct
  type t = {
    hash : Ethereum_types.block_hash;
    level : int32;
    timestamp : Time.Protocol.t;
    parent_hash : Ethereum_types.block_hash;
        (* Deserialization of operation and receipts is delayed to
           avoid introducing a dependency from this lib_encodings to
           the protocol. *)
    operations : bytes;
  }

  let decode_block_hash = Ethereum_types.decode_block_hash

  let genesis_parent_hash =
    (* This Hex comes from this b58 hash 'BLockGenesisGenesisGenesisGenesisGenesis1db77eJNeJ9' *)
    (* That is the ghostnet genesis hash according to 'devtools/get_contracts/config.ml' *)
    Ethereum_types.Block_hash
      (Hex "8fcf233671b6a04fcf679d2a381c2544ea6c1ea29ba6157776ed8423e7c02934")

  let block_encoding : t Data_encoding.t =
    let open Data_encoding in
    let timestamp_encoding = Time.Protocol.encoding in
    let block_hash_encoding =
      let open Ethereum_types in
      conv
        (fun (Block_hash (Hex s)) -> Hex.to_bytes_exn (`Hex s))
        (fun b ->
          let (`Hex s) = Hex.of_bytes b in
          Block_hash (Hex s))
        (Fixed.bytes 32)
    in
    def "tezlink_block"
    @@ conv
         (fun {hash; level; parent_hash; timestamp; operations} ->
           (hash, level, parent_hash, timestamp, operations))
         (fun (hash, level, parent_hash, timestamp, operations) ->
           {hash; level; parent_hash; timestamp; operations})
         (obj5
            (req "hash" block_hash_encoding)
            (req "level" int32)
            (req "parent_hash" block_hash_encoding)
            (req "timestamp" timestamp_encoding)
            (req "operations" bytes))

  let () = Data_encoding.Registration.register block_encoding

  (* This function may be replaced in the future by an already existing function *)
  (* When Tezos block will be complete *)
  let block_from_binary bytes =
    Data_encoding.Binary.of_bytes_exn block_encoding bytes

  let encode_block (block : t) : (string, string) result =
    Ok (Data_encoding.Binary.to_string_exn block_encoding block)

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
  | Tez block -> Ethereum_types.Qty (Z.of_int32 block.level)

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

let decode_block_hash (type f) ~(chain_family : f chain_family) bytes =
  match chain_family with
  | EVM -> Ethereum_types.decode_block_hash bytes
  | Michelson -> Tezos_block.decode_block_hash bytes

let genesis_parent_hash (type f) ~(chain_family : f chain_family) =
  match chain_family with
  | EVM -> Ethereum_types.genesis_parent_hash
  | Michelson -> Tezos_block.genesis_parent_hash

let block_from_bytes (type f) ~(chain_family : f chain_family) bytes =
  match chain_family with
  | EVM ->
      let eth_block = Ethereum_types.block_from_rlp bytes in
      Eth eth_block
  | Michelson ->
      let tez_block = Tezos_block.block_from_binary bytes in
      Tez tez_block
