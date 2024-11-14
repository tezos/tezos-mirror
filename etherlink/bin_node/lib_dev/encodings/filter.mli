(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

(** Event filter, see
    https://ethereum.org/en/developers/docs/apis/json-rpc/#eth_getlogs *)
type topic = One of hash | Or of hash list

type filter_address = Single of address | Vec of address list

type changes =
  | Block_filter of block_hash
  | Pending_transaction_filter of hash
  | Log of transaction_log

val changes_encoding : changes Data_encoding.t

type t = {
  from_block : Block_parameter.t option;
  to_block : Block_parameter.t option;
  address : filter_address option;
  topics : topic option list option;
  block_hash : block_hash option;
}

val encoding : t Data_encoding.t
