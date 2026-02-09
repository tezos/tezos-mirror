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
  module Version = struct
    type t = V0

    let from_bytes (bytes : bytes) : t =
      match Rlp.decode_int bytes with
      | Ok 0 -> V0
      | Ok _ -> raise (Invalid_argument "Expected a valid version")
      | Error _ ->
          (* TODO: Instead of raising an exception, return the Result *)
          raise
            (Invalid_argument "Unexpected version read for a L2 Tezos block")

    let to_bytes (version : t) : bytes =
      let version = match version with V0 -> 0 in
      Rlp.encode_int version
  end

  let decode_block_hash = Ethereum_types.decode_block_hash

  let genesis_parent_hash =
    (* This Hex comes from this b58 hash 'BLockGenesisGenesisGenesisGenesisGenesis1db77eJNeJ9' *)
    (* That is the ghostnet genesis hash according to 'devtools/get_contracts/config.ml' *)
    Ethereum_types.Block_hash
      (Hex "8fcf233671b6a04fcf679d2a381c2544ea6c1ea29ba6157776ed8423e7c02934")

  module V0 = struct
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

    let from_rlp (block_rlp : Rlp.item list) : t =
      let open Rlp in
      match block_rlp with
      | [
       Value hash;
       Value level;
       Value previous_hash;
       Value timestamp;
       Value operations;
      ] ->
          let level = Bytes.get_int32_le level 0 in
          let hash = decode_block_hash hash in
          let parent_hash = decode_block_hash previous_hash in
          let timestamp = Ethereum_types.timestamp_of_bytes timestamp in
          {hash; level; timestamp; parent_hash; operations}
      | _ ->
          raise
            (Invalid_argument
               "Expected a RLP list of 6 elements (including the version field)")
  end

  module Latest = V0

  module Legacy = struct
    type t = V0.t = {
      hash : Ethereum_types.block_hash;
      level : int32;
      timestamp : Time.Protocol.t;
      parent_hash : Ethereum_types.block_hash;
          (* Deserialization of operation and receipts is delayed to
           avoid introducing a dependency from this lib_encodings to
           the protocol. *)
      operations : bytes;
    }

    let encoding : t Data_encoding.t =
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
      def "legacy_tezlink_block"
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

    let () = Data_encoding.Registration.register encoding
  end

  include Latest

  let block_from_kernel bytes =
    match Rlp.decode bytes with
    | Ok (Rlp.List (Value version :: block_rlp)) -> (
        let version = Version.from_bytes version in
        match version with V0 -> V0.from_rlp block_rlp)
    | _ ->
        (* The octez-evm-node needs to be retro compatible with legacy Data_encoding *)
        Data_encoding.Binary.of_bytes_exn Legacy.encoding bytes

  (* Latest version of the block. It is used for block RLP encoding *)
  let latest_version = Version.V0

  (* Serialize a block using the latest version of the block RLP format. *)
  let block_to_rlp {hash; level; timestamp; parent_hash; operations} =
    let open Rlp in
    let level = Helpers.encode_i32_le level in
    let version = Version.to_bytes latest_version in
    let hash = Ethereum_types.encode_block_hash hash in
    let previous_hash = Ethereum_types.encode_block_hash parent_hash in
    let timestamp = Ethereum_types.timestamp_to_bytes timestamp in
    let item =
      List
        [
          Value version;
          Value hash;
          Value level;
          Value previous_hash;
          Value timestamp;
          Value operations;
        ]
    in
    encode item

  let encode_block_for_store (block : t) : (string, string) result =
    Ok (Bytes.to_string (block_to_rlp block))

  let decode_block_for_store (block : string) : (t, string) result =
    try Ok (block_from_kernel (Bytes.of_string block))
    with exn -> Error (Printexc.to_string exn)

  module Internal_for_test = struct
    module Legacy = struct
      include Legacy

      let encode_block_for_store (block : t) : (string, string) result =
        Result.map_error
          (Format.asprintf
             "Not a valid block: %a"
             Data_encoding.Binary.pp_write_error)
          (Data_encoding.Binary.to_string encoding block)
    end
  end
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
      let tez_block = Tezos_block.block_from_kernel bytes in
      Tez tez_block
