(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2024-2025 Functori <contact@functori.com>                   *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

type t = {
  transactionHash : hash;
  transactionIndex : quantity;
  blockHash : block_hash;
  blockNumber : quantity;
  from : address;
  to_ : address option;
  cumulativeGasUsed : quantity;
  effectiveGasPrice : quantity;
  gasUsed : quantity;
  logs : transaction_log list;
  logsBloom : hex;
  type_ : quantity;
  status : quantity;
  contractAddress : address option;
}

val of_rlp_item : block_hash -> Rlp.item -> t

val of_rlp_bytes : block_hash -> bytes -> t

val decode_last_from_list : block_hash -> bytes -> t

(** [decode_nth_from_list ~index block_hash bytes] decodes the receipt at
    position [index] in the RLP list of receipts [bytes], if any.
    Returns [None] if the index is out of bounds, and an error if [bytes]
    is not an RLP list or if the receipt at [index] is malformed. *)
val decode_nth_from_list : index:int -> block_hash -> bytes -> t option tzresult

val encoding : t Data_encoding.t
