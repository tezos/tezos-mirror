(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

type receipt_fields = {
  cumulative_gas_used : quantity;
  effective_gas_price : quantity;
  gas_used : quantity;
  logs : transaction_log list;
  logs_bloom : hex;
  type_ : quantity;
  status : quantity;
  contract_address : address option;
}

type object_fields = {
  gas : quantity;
  gas_price : quantity;
  input : hex;
  nonce : quantity;
  value : quantity;
  v : quantity;
  r : quantity;
  s : quantity;
}

type t = {
  block_hash : block_hash;
  block_number : quantity;
  index : quantity;
  hash : hash;
  from : address;
  to_ : address option;
  receipt_fields : receipt_fields;
  object_fields : object_fields;
}

let of_receipt_and_object
    ({
       transactionHash = hash;
       transactionIndex = index;
       blockHash = block_hash;
       blockNumber = block_number;
       from;
       to_;
       cumulativeGasUsed = cumulative_gas_used;
       effectiveGasPrice = effective_gas_price;
       gasUsed = gas_used;
       logs;
       logsBloom = logs_bloom;
       type_;
       status;
       contractAddress = contract_address;
     } :
      Transaction_receipt.t)
    ({
       blockHash = _;
       blockNumber = _;
       from = _;
       gas;
       gasPrice = gas_price;
       hash = _;
       input;
       nonce;
       to_ = _;
       transactionIndex = _;
       value;
       v;
       r;
       s;
     } :
      legacy_transaction_object) : t =
  let receipt_fields =
    {
      cumulative_gas_used;
      effective_gas_price;
      gas_used;
      logs;
      logs_bloom;
      type_;
      status;
      contract_address;
    }
  in
  let object_fields = {gas; gas_price; input; nonce; value; v; r; s} in
  {
    block_hash;
    block_number;
    index;
    from;
    hash;
    to_;
    receipt_fields;
    object_fields;
  }

let zero_logs_bloom = Hex (String.make 512 '0')

let is_zero_logs_bloom (Hex h) =
  let (Hex h') = zero_logs_bloom in
  String.equal h h'

(* Compact encoding for logs_bloom: an all-zero bloom (no logs) is stored as
   an empty string instead of the full 514-byte ASCII hex representation.
   This reduce storage for the all simple transfers transactions.

   WARNING: the SQLite extension in ../sqlite_receipt_bloom depends on the
   database storing receipts with this encoding to work. If it is ever changed,
   the SQLite extension will also need to be adapted. *)
let compact_bloom_encoding =
  Data_encoding.conv
    (fun (Hex h as bloom) -> if is_zero_logs_bloom bloom then "" else "0x" ^ h)
    (fun s -> if s = "" then zero_logs_bloom else hex_of_string s)
    Data_encoding.string

let receipt_fields_encoding =
  let open Data_encoding in
  conv
    (fun {
           cumulative_gas_used;
           effective_gas_price;
           gas_used;
           logs;
           logs_bloom;
           type_;
           status;
           contract_address;
         }
       ->
      ( cumulative_gas_used,
        effective_gas_price,
        gas_used,
        logs,
        logs_bloom,
        type_,
        status,
        contract_address ))
    (fun ( cumulative_gas_used,
           effective_gas_price,
           gas_used,
           logs,
           logs_bloom,
           type_,
           status,
           contract_address )
       ->
      {
        cumulative_gas_used;
        effective_gas_price;
        gas_used;
        logs;
        logs_bloom;
        type_;
        status;
        contract_address;
      })
    (obj8
       (req "cumulative_gas_used" quantity_encoding)
       (req "effective_gas_price" quantity_encoding)
       (req "gas_used" quantity_encoding)
       (req "logs" (list transaction_log_encoding))
       (req "logs_bloom" compact_bloom_encoding)
       (req "type_" quantity_encoding)
       (req "status" quantity_encoding)
       (req "contract_address" (option address_encoding)))

let object_fields_encoding =
  let open Data_encoding in
  conv
    (fun {gas; gas_price; input; nonce; value; v; r; s} ->
      (gas, gas_price, input, nonce, value, v, r, s))
    (fun (gas, gas_price, input, nonce, value, v, r, s) ->
      {gas; gas_price; input; nonce; value; v; r; s})
    (obj8
       (req "gas" quantity_encoding)
       (req "gas_price" quantity_encoding)
       (req "input" hex_encoding)
       (req "nonce" quantity_encoding)
       (req "value" quantity_encoding)
       (req "v" quantity_encoding)
       (req "r" quantity_encoding)
       (req "s" quantity_encoding))
