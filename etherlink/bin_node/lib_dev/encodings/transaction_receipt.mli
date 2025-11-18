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

val encoding : t Data_encoding.t
