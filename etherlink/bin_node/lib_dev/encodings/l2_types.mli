(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2024-2025 Functori <contact@functori.com>                   *)
(*                                                                           *)
(*****************************************************************************)

type chain_id = Chain_id of Z.t [@@unboxed]

module Chain_id : sig
  val encoding : chain_id Data_encoding.t

  (** [of_string_exn hex] transforms a string to a chain id.
  It raises an exception if the string is not an number
  (base 10, hexa, binary, octal ...) *)
  val of_string_exn : string -> chain_id

  val to_string : chain_id -> string

  val decode_le : bytes -> chain_id

  val decode_be : bytes -> chain_id

  val compare : chain_id -> chain_id -> int

  val pp : Format.formatter -> chain_id -> unit
end

type chain_family = EVM | Michelson

module Chain_family : sig
  val encoding : chain_family Data_encoding.t

  (** [of_string_exn s] returns the chain family corresponding to the string [s].
      The comparison is case-insensitive, so ["Evm"], ["evm"], ["EVM"], etc. are all valid.
      @raise Invalid_argument if [s] does not correspond to a recognized chain family.
  *)
  val of_string_exn : string -> chain_family

  val to_string : chain_family -> string

  val pp : Format.formatter -> chain_family -> unit
end

module Tezos_block : sig
  type t = {
    level : int32;
    hash : Ethereum_types.block_hash;
    timestamp : Time.Protocol.t;
    parent_hash : Ethereum_types.block_hash;
  }

  val decode_block_hash : bytes -> Ethereum_types.block_hash

  val genesis_parent_hash : Ethereum_types.block_hash

  val block_from_binary : bytes -> t

  val encode_block : t -> (string, string) result

  val decode_block : string -> (t, string) result
end

type 'a block = Eth of 'a Ethereum_types.block | Tez of Tezos_block.t

val block_hash : 'a block -> Ethereum_types.block_hash

val block_number : 'a block -> Ethereum_types.quantity

val block_number_of_transactions : 'a block -> int

val block_parent : 'a block -> Ethereum_types.block_hash

val decode_block_hash :
  chain_family:chain_family -> bytes -> Ethereum_types.block_hash

val genesis_parent_hash : chain_family:chain_family -> Ethereum_types.block_hash

val block_from_bytes :
  chain_family:chain_family ->
  bytes ->
  Ethereum_types.legacy_transaction_object block
