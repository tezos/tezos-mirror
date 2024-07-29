(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

type topic = One of hash | Or of hash list

let topic_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"one"
        (Tag 0)
        hash_encoding
        (function One hash -> Some hash | _ -> None)
        (fun hash -> One hash);
      case
        ~title:"or"
        (Tag 1)
        (list hash_encoding)
        (function Or l -> Some l | _ -> None)
        (fun l -> Or l);
    ]

type filter_address = Single of address | Vec of address list

let filter_address_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"single"
        (Tag 0)
        address_encoding
        (function Single address -> Some address | _ -> None)
        (fun address -> Single address);
      case
        ~title:"vec"
        (Tag 1)
        (list address_encoding)
        (function Vec l -> Some l | _ -> None)
        (fun l -> Vec l);
    ]

type changes =
  | Block_filter of block_hash
  | Pending_transaction_filter of hash
  | Log of transaction_log

let changes_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"block"
        (Tag 0)
        block_hash_encoding
        (function Block_filter hash -> Some hash | _ -> None)
        (fun hash -> Block_filter hash);
      case
        ~title:"pending_transaction"
        (Tag 1)
        hash_encoding
        (function Pending_transaction_filter hash -> Some hash | _ -> None)
        (fun hash -> Pending_transaction_filter hash);
      case
        ~title:"log"
        (Tag 2)
        transaction_log_encoding
        (function Log f -> Some f | _ -> None)
        (fun f -> Log f);
    ]

type t = {
  from_block : Block_parameter.t option;
  to_block : Block_parameter.t option;
  address : filter_address option;
  topics : topic option list option;
  block_hash : block_hash option;
}

let encoding =
  let open Data_encoding in
  conv
    (fun {from_block; to_block; address; topics; block_hash} ->
      (from_block, to_block, address, topics, block_hash))
    (fun (from_block, to_block, address, topics, block_hash) ->
      {from_block; to_block; address; topics; block_hash})
    (obj5
       (opt "fromBlock" Block_parameter.encoding)
       (opt "toBlock" Block_parameter.encoding)
       (opt "address" filter_address_encoding)
       (opt "topics" (list @@ option topic_encoding))
       (opt "blockHash" block_hash_encoding))
