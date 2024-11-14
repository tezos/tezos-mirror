(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
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

let of_rlp_bytes block_hash bytes =
  match Rlp.decode bytes with
  | Ok
      (Rlp.List
        [
          Value hash;
          Value index;
          Value block_number;
          Value from;
          Value to_;
          Value cumulative_gas_used;
          Value effective_gas_price;
          Value gas_used;
          Value contract_address;
          List logs;
          Value bloom;
          Value type_;
          Value status;
        ]) ->
      let hash = decode_hash hash in
      let index = decode_number_be index in
      let block_number = decode_number_le block_number in
      let from = decode_address from in
      let to_ = if to_ = Bytes.empty then None else Some (decode_address to_) in
      let cumulative_gas_used = decode_number_le cumulative_gas_used in
      let effective_gas_price = decode_number_le effective_gas_price in
      let gas_used = decode_number_le gas_used in
      let contract_address =
        if contract_address = Bytes.empty then None
        else Some (decode_address contract_address)
      in
      let logs_body = List.map transaction_log_body_from_rlp logs in
      let logs_objects =
        List.map
          (fun (address, topics, data, logIndex) ->
            {
              address;
              topics;
              data;
              blockHash = Some block_hash;
              blockNumber = Some block_number;
              transactionHash = Some hash;
              transactionIndex = Some index;
              logIndex = Some logIndex;
              removed = Some false;
            })
          logs_body
      in
      let bloom = decode_hex bloom in
      let type_ = decode_number_le type_ in
      let status = decode_number_le status in
      {
        transactionHash = hash;
        transactionIndex = index;
        blockHash = block_hash;
        blockNumber = block_number;
        from;
        to_;
        cumulativeGasUsed = cumulative_gas_used;
        effectiveGasPrice = effective_gas_price;
        gasUsed = gas_used;
        logs = logs_objects;
        logsBloom = bloom;
        type_;
        status;
        contractAddress = contract_address;
      }
  | _ ->
      raise
        (Invalid_argument
           "Expected a RlpList of 13 elements in transaction receipt")

let encoding =
  let open Data_encoding in
  conv
    (fun {
           transactionHash;
           transactionIndex;
           blockHash;
           blockNumber;
           from;
           to_;
           cumulativeGasUsed;
           effectiveGasPrice;
           gasUsed;
           logs;
           logsBloom;
           type_;
           status;
           contractAddress;
         } ->
      ( ( transactionHash,
          transactionIndex,
          blockHash,
          blockNumber,
          from,
          to_,
          cumulativeGasUsed,
          effectiveGasPrice,
          gasUsed,
          logs ),
        (logsBloom, type_, status, contractAddress) ))
    (fun ( ( transactionHash,
             transactionIndex,
             blockHash,
             blockNumber,
             from,
             to_,
             cumulativeGasUsed,
             effectiveGasPrice,
             gasUsed,
             logs ),
           (logsBloom, type_, status, contractAddress) ) ->
      {
        transactionHash;
        transactionIndex;
        blockHash;
        blockNumber;
        from;
        to_;
        cumulativeGasUsed;
        effectiveGasPrice;
        gasUsed;
        logs;
        logsBloom;
        type_;
        status;
        contractAddress;
      })
    (merge_objs
       (obj10
          (req "transactionHash" hash_encoding)
          (req "transactionIndex" quantity_encoding)
          (req "blockHash" block_hash_encoding)
          (req "blockNumber" quantity_encoding)
          (req "from" address_encoding)
          (req "to" (option address_encoding))
          (req "cumulativeGasUsed" quantity_encoding)
          (req "effectiveGasPrice" quantity_encoding)
          (req "gasUsed" quantity_encoding)
          (req "logs" (list transaction_log_encoding)))
       (obj4
          (req "logsBloom" hex_encoding)
          (req "type" quantity_encoding)
          (req "status" quantity_encoding)
          (req "contractAddress" (option address_encoding))))
