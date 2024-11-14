(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Ethereum data, as Hex-encoded strings *)
type hex = Hex of string [@@unboxed]

val hex_encoding : hex Data_encoding.t

(** version of [hex_encoding] that do not add `0x` on encoded values. *)
val hex_encoding_no0x : hex Data_encoding.t

(** Strips the [0x] prefix of a string. *)
val hex_of_string : string -> hex

(** Encodes a string into hexa. see {!Hex.of_string}
    E.g. [hex_encode_string "Mirage OS"] is [Hex "4d69726167654f53"]*)
val hex_encode_string : string -> hex

(** [hex_to_bytes hex] transforms the [hex] to binary format. *)
val hex_to_bytes : hex -> string

(** [hex_to_real_bytes hex] transforms the [hex] to bytes. *)
val hex_to_real_bytes : hex -> bytes

(** [hex_of_utf8] encodes a string to its utf8 representation in
    hexadecimal. *)
val hex_of_utf8 : string -> hex

(** [hex_of_bytes] transforms the [bytes] to hexadecimal. *)
val hex_of_bytes : bytes -> hex

(** Ethereum block hash (32 bytes) *)
type block_hash = Block_hash of hex [@@unboxed]

val block_hash_encoding : block_hash Data_encoding.t

val pp_block_hash : Format.formatter -> block_hash -> unit

val decode_block_hash : bytes -> block_hash

val genesis_parent_hash : block_hash

val block_hash_to_bytes : block_hash -> string

val block_hash_of_string : string -> block_hash

(** Ethereum generic quantity, always encoded in hexadecimal. *)
type quantity = Qty of Z.t [@@unboxed]

module Qty : sig
  val pred : quantity -> quantity

  val next : quantity -> quantity

  val to_z : quantity -> Z.t

  val zero : quantity

  val ( = ) : quantity -> quantity -> bool
end

val quantity_encoding : quantity Data_encoding.t

val pp_quantity : Format.formatter -> quantity -> unit

val quantity_of_z : Z.t -> quantity

val decode_number_le : bytes -> quantity

val decode_number_be : bytes -> quantity

val encode_u256_le : quantity -> bytes

val encode_u64_le : quantity -> bytes

(** [u16_to_bytes n] Translate an int in a binary string of two bytes
    (little endian).  Ints greater than 2 bytes are truncated. *)
val u16_to_bytes : int -> string

(** Ethereum hash, that would encoded with a 0x prefix. *)
type hash = Hash of hex [@@unboxed]

val hash_encoding : hash Data_encoding.t

val pp_hash : Format.formatter -> hash -> unit

(** Transaction hash size is 32 bytes. *)
val transaction_hash_size : int

val decode_hash : bytes -> hash

(** [hash_of_string s] takes a string [s] representing a hash in
    hexadecimal format, e.g. [0xFFFFFFF]. Strips the prefix and keeps
    the hash value, e.g. [FFFFFFF]. *)
val hash_of_string : string -> hash

(** [hash_to_string h] constructs a valid hash encoded in hexadecimal
    format, e.g. [0xFFFFFFF]. *)
val hash_to_string : hash -> string

(** [hash_to_bytes hash] transforms the [hash] to binary format. *)
val hash_to_bytes : hash -> string

(** Ethereum address (20 bytes) *)
type address = Address of hex [@@unboxed]

val address_encoding : address Data_encoding.t

val decode_address : bytes -> address

type transaction_object = {
  blockHash : block_hash option;
  blockNumber : quantity option;
  from : address;
  gas : quantity;
  gasPrice : quantity;
  hash : hash;
  input : hash;
  nonce : quantity;
  to_ : address option;
  transactionIndex : quantity option;
  value : quantity;
  v : quantity;
  r : hash;
  s : hash;
}

val transaction_object_encoding : transaction_object Data_encoding.t

val transaction_object_from_rlp_item :
  block_hash option -> Rlp.item -> transaction_object

val transaction_object_from_rlp :
  block_hash option -> bytes -> transaction_object

type block_transactions =
  | TxHash of hash list
  | TxFull of transaction_object list

(** Ethereum block hash representation from RPCs. *)
type block = {
  number : quantity;
  hash : block_hash;
  parent : block_hash;
  nonce : hex;
  sha3Uncles : hash;
  logsBloom : hex;
  transactionRoot : hash;
  stateRoot : hash;
  receiptRoot : hash;
  miner : hex;
  difficulty : quantity;
  totalDifficulty : quantity;
  extraData : hex;
  size : quantity;
  gasLimit : quantity;
  gasUsed : quantity;
  timestamp : quantity;
  transactions : block_transactions;
  uncles : hash list;
  (* baseFeePerGas and prevRandao are set optionnal because old blocks didn't have
     them*)
  baseFeePerGas : quantity option;
  prevRandao : block_hash option;
}

val block_encoding : block Data_encoding.t

type transaction_log = {
  address : address;
  topics : hash list;
  data : hex;
  blockNumber : quantity option;
  transactionHash : hash option;
  transactionIndex : quantity option;
  blockHash : block_hash option;
  logIndex : quantity option;
  removed : bool option;
}

type call = {
  from : address option;
  to_ : address option;
  gas : quantity option;
  gasPrice : quantity option;
  value : quantity option;
  data : hash option;
}

val call_encoding : call Data_encoding.t

module NonceMap : sig
  include Map.S with type key = Z.t

  val associative_array_encoding : 'a Data_encoding.t -> 'a t Data_encoding.t
end

module AddressMap : sig
  include Map.S with type key = address

  val associative_array_encoding : 'a Data_encoding.t -> 'a t Data_encoding.t
end

type txpool = {
  pending : transaction_object NonceMap.t AddressMap.t;
  queued : transaction_object NonceMap.t AddressMap.t;
}

val txpool_encoding : txpool Data_encoding.t

module StorageMap : sig
  include Map.S with type key = hex

  val associative_array_encoding : 'a Data_encoding.t -> 'a t Data_encoding.t
end

type state_account_override = {
  balance : quantity option;
  nonce : quantity option;
  code : hex option;
  state_diff : hex StorageMap.t;
  state : hex StorageMap.t option;
}

type state_override = state_account_override AddressMap.t

val state_override_encoding : state_override Data_encoding.t

val state_override_empty : state_override

val block_from_rlp : bytes -> block

module Block_parameter : sig
  (** Ethereum block params in RPCs. *)
  type t = Number of quantity | Earliest | Latest | Pending | Finalized

  val encoding : t Data_encoding.t

  (** Extended block parameter defined in https://eips.ethereum.org/EIPS/eip-1898. *)
  type extended =
    | Block_parameter of t
    | Block_hash of {hash : block_hash; require_canonical : bool}

  val extended_encoding : extended Data_encoding.t

  val pp_extended : Format.formatter -> extended -> unit
end

module Address : sig
  type t = address

  val compare : t -> t -> int

  val to_string : t -> string
end

(** [timestamp_to_bytes timestamp] transforms the timestamp to bytes
    compatible with the kernel. *)
val timestamp_to_bytes : Time.Protocol.t -> bytes

(** See bool encoding for RLP: https://docs.rs/ethereum-rlp/latest/src/rlp/impls.rs.html#36-44 *)
val bool_to_rlp_bytes : bool -> Rlp.item

val hash_raw_tx : string -> hash

val timestamp_of_bytes : bytes -> Time.Protocol.t

val encode_address : address -> bytes

val transaction_log_encoding : transaction_log Data_encoding.t

val transaction_log_body_from_rlp :
  Rlp.item -> address * hash list * hex * quantity

val decode_hex : bytes -> hex

module From_rlp : sig
  val decode_address : Rlp.item -> address tzresult

  val decode_string : Rlp.item -> string tzresult

  val decode_int : Rlp.item -> int tzresult

  val decode_z : Rlp.item -> Z.t tzresult

  val decode_hex : Rlp.item -> hex tzresult
end
