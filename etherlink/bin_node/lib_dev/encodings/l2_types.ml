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

type 'a block = Eth of 'a Ethereum_types.block

let decode_block_hash ~chain_family bytes =
  match chain_family with
  | EVM -> Ethereum_types.decode_block_hash bytes
  | Michelson -> raise (Invalid_argument "TODO tezos")

let genesis_parent_hash ~chain_family =
  match chain_family with
  | EVM -> Ethereum_types.genesis_parent_hash
  | Michelson -> raise (Invalid_argument "TODO tezos")

let block_from_bytes ~chain_family bytes =
  match chain_family with
  | EVM ->
      let eth_block = Ethereum_types.block_from_rlp bytes in
      Eth eth_block
  | Michelson -> raise (Invalid_argument "TODO tezos")
