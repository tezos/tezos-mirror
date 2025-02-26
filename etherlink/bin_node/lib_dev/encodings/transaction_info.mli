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
  r : hex;
  s : hex;
}

(** This is the merge of a {!Transaction_receipt.t} and
    {!Ethereum_types.transaction_object}, the goal is to minimize the size
    on disk to store these information. *)
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

(** [of_receipt_and_object receipt object_] merges [receipt] and [object_]
    into a {!t}. *)
val of_receipt_and_object :
  Transaction_receipt.t -> legacy_transaction_object -> t

val receipt_fields_encoding : receipt_fields Data_encoding.t

val object_fields_encoding : object_fields Data_encoding.t
