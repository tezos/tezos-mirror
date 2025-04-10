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

type address = Signature.V1.public_key_hash

val address_of_b58check : string -> address tzresult

val address_of_b58exn : string -> address

val address_to_hex_exn : address -> string

module Tez : sig
  include module type of Tezos_protocol_021_PsQuebec.Protocol.Alpha_context.Tez

  val of_string_exn : string -> t
end
