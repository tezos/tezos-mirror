(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

type access = {address : address; storage_keys : hex list}

let access_encoding =
  let open Data_encoding in
  conv
    (fun {address; storage_keys} -> (address, storage_keys))
    (fun (address, storage_keys) -> {address; storage_keys})
    (obj2
       (req "address" address_encoding)
       (req "storageKeys" (list hex_encoding)))

module EIP_2930 = struct
  type t = {
    chain_id : quantity;
    block_hash : block_hash option;
    block_number : quantity option;
    from : address;
    gas : quantity;
    gas_price : quantity;
    hash : hash;
    input : hex;
    nonce : quantity;
    to_ : address option;
    transaction_index : quantity option;
    value : quantity;
    access_list : access list;
    v : quantity;
    r : hex;
    s : hex;
  }

  let encoding =
    let open Data_encoding in
    conv
      (function
        | {
            chain_id;
            block_hash;
            block_number;
            from;
            gas;
            gas_price;
            hash;
            input;
            nonce;
            to_;
            transaction_index;
            value;
            access_list;
            v;
            r;
            s;
          } ->
            ( ( chain_id,
                block_hash,
                block_number,
                from,
                gas,
                gas_price,
                hash,
                input,
                nonce,
                to_ ),
              (transaction_index, value, access_list, v, r, s) ))
      (fun ( ( chain_id,
               block_hash,
               block_number,
               from,
               gas,
               gas_price,
               hash,
               input,
               nonce,
               to_ ),
             (transaction_index, value, access_list, v, r, s) ) ->
        {
          chain_id;
          block_hash;
          block_number;
          from;
          gas;
          gas_price;
          hash;
          input;
          nonce;
          to_;
          transaction_index;
          value;
          access_list;
          v;
          r;
          s;
        })
      (merge_objs
         (obj10
            (req "chainId" quantity_encoding)
            (req "blockHash" (option block_hash_encoding))
            (req "blockNumber" (option quantity_encoding))
            (req "from" address_encoding)
            (req "gas" quantity_encoding)
            (req "gasPrice" quantity_encoding)
            (req "hash" hash_encoding)
            (req "input" hex_encoding)
            (req "nonce" quantity_encoding)
            (req "to" (option address_encoding)))
         (obj6
            (req "transactionIndex" (option quantity_encoding))
            (req "value" quantity_encoding)
            (req "accessList" (list access_encoding))
            (req "v" quantity_encoding)
            (req "r" hex_encoding)
            (req "s" hex_encoding)))
end

module EIP_1559 = struct
  type t = {
    chain_id : quantity;
    hash : hash;
    nonce : quantity;
    block_hash : block_hash option;
    block_number : quantity option;
    transaction_index : quantity option;
    from : address;
    to_ : address option;
    value : quantity;
    gas : quantity;
    max_fee_per_gas : quantity;
    max_priority_fee_per_gas : quantity;
    access_list : access list;
    input : hex;
    v : quantity;
    r : hex;
    s : hex;
  }

  let encoding =
    let open Data_encoding in
    conv
      (fun {
             chain_id;
             hash;
             nonce;
             block_hash;
             block_number;
             transaction_index;
             from;
             to_;
             value;
             gas;
             max_fee_per_gas;
             max_priority_fee_per_gas;
             access_list;
             input;
             v;
             r;
             s;
           } ->
        ( ( chain_id,
            hash,
            nonce,
            block_hash,
            block_number,
            transaction_index,
            from,
            to_,
            value,
            gas ),
          ( max_fee_per_gas,
            max_priority_fee_per_gas,
            access_list,
            input,
            v,
            r,
            s ) ))
      (fun ( ( chain_id,
               hash,
               nonce,
               block_hash,
               block_number,
               transaction_index,
               from,
               to_,
               value,
               gas ),
             ( max_fee_per_gas,
               max_priority_fee_per_gas,
               access_list,
               input,
               v,
               r,
               s ) ) ->
        {
          chain_id;
          hash;
          nonce;
          block_hash;
          block_number;
          transaction_index;
          from;
          to_;
          value;
          gas;
          max_fee_per_gas;
          max_priority_fee_per_gas;
          access_list;
          input;
          v;
          r;
          s;
        })
      (merge_objs
         (obj10
            (req "chainId" quantity_encoding)
            (req "hash" hash_encoding)
            (req "nonce" quantity_encoding)
            (req "blockHash" (option block_hash_encoding))
            (req "blockNumber" (option quantity_encoding))
            (req "transactionIndex" (option quantity_encoding))
            (req "from" address_encoding)
            (req "to" (option address_encoding))
            (req "value" quantity_encoding)
            (req "gas" quantity_encoding))
         (obj7
            (req "maxFeePerGas" quantity_encoding)
            (req "maxPriorityFeePerGas" quantity_encoding)
            (req "accessList" (list access_encoding))
            (req "input" hex_encoding)
            (req "v" quantity_encoding)
            (req "r" hex_encoding)
            (req "s" hex_encoding)))
end

type t =
  | Kernel of legacy_transaction_object
      (** Transaction object read from the durable storage. *)
  | Legacy of legacy_transaction_object
  | EIP_2930 of EIP_2930.t
  | EIP_1559 of EIP_1559.t

let from_store_transaction_object (obj : legacy_transaction_object) = Kernel obj

let block_from_legacy block =
  {
    block with
    transactions =
      (match block.transactions with
      | TxHash hashes -> TxHash hashes
      | TxFull l -> TxFull (List.map from_store_transaction_object l));
  }

let decode_access_list =
  let open Result_syntax in
  Rlp.decode_list (function
      | List [Value address; storage_keys] ->
          let address = Ethereum_types.decode_address address in
          let* storage_keys =
            Rlp.decode_list
              (function
                | Value x -> Ok (Ethereum_types.decode_hex x)
                | _ -> error_with "Invalid storage key")
              storage_keys
          in
          Ok {address; storage_keys}
      | _ -> error_with "failed to decode access list from raw transaction")

let reconstruct_from_eip_2930_transaction (obj : legacy_transaction_object)
    raw_txn =
  let open Result_syntax in
  match Rlp.decode (Bytes.unsafe_of_string raw_txn) with
  | Ok
      (List
        [
          Value chain_id;
          Value _nonce;
          Value _gas_price;
          Value _gas_limit;
          Value _to_;
          Value _value;
          Value _data;
          access_list;
          Value _v;
          Value _r;
          Value _s;
        ]) ->
      let chain_id = decode_number_be chain_id in
      let+ access_list = decode_access_list access_list in
      EIP_2930
        {
          chain_id;
          hash = obj.hash;
          nonce = obj.nonce;
          block_hash = obj.blockHash;
          block_number = obj.blockNumber;
          transaction_index = obj.transactionIndex;
          from = obj.from;
          to_ = obj.to_;
          value = obj.value;
          gas = obj.gas;
          gas_price = obj.gasPrice;
          access_list;
          input = obj.input;
          v = obj.v;
          r = obj.r;
          s = obj.s;
        }
  | _ -> error_with "failed to decode EIP-2930 transaction"

let reconstruct_from_eip_1559_transaction (obj : legacy_transaction_object)
    raw_txn =
  let open Result_syntax in
  match Rlp.decode (Bytes.unsafe_of_string raw_txn) with
  | Ok
      (List
        [
          Value chain_id;
          _nonce;
          Value max_priority_fee_per_gas;
          Value max_fee_per_gas;
          _gas_limit;
          _to_;
          _value;
          _input;
          access_list;
          _v;
          _r;
          _s;
        ]) ->
      let chain_id = decode_number_be chain_id in
      let max_fee_per_gas = decode_number_be max_fee_per_gas in
      let max_priority_fee_per_gas =
        decode_number_be max_priority_fee_per_gas
      in
      let+ access_list = decode_access_list access_list in

      EIP_1559
        {
          chain_id;
          hash = obj.hash;
          nonce = obj.nonce;
          block_hash = obj.blockHash;
          block_number = obj.blockNumber;
          transaction_index = obj.transactionIndex;
          from = obj.from;
          to_ = obj.to_;
          value = obj.value;
          gas = obj.gas;
          max_fee_per_gas;
          max_priority_fee_per_gas;
          access_list;
          input = obj.input;
          v = obj.v;
          r = obj.r;
          s = obj.s;
        }
  | _ ->
      (* This should not happen, but we cannot decode the transaction *)
      error_with "Invalid EIP-1559 transaction"

let reconstruct_from_raw_transaction (obj : legacy_transaction_object) raw_txn =
  match String.get raw_txn 0 with
  | '\x02' ->
      (* EIP 1559 *)
      reconstruct_from_eip_1559_transaction
        obj
        (String.sub raw_txn 1 (String.length raw_txn - 1))
  | '\x01' ->
      (* EIP 2930 *)
      reconstruct_from_eip_2930_transaction
        obj
        (String.sub raw_txn 1 (String.length raw_txn - 1))
  | _ -> Ok (Legacy obj)

let reconstruct_from_transactions_list transactions
    (obj : legacy_transaction_object) =
  let open Result_syntax in
  match List.assoc ~equal:( = ) obj.hash transactions with
  | None ->
      (* This should not happen *)
      error_with
        "Cannot reconstruct %a: cannot find the raw transaction in the \
         blueprint"
        Ethereum_types.pp_hash
        obj.hash
  | Some None ->
      (* It is a delayed transaction, there is nothing we can do except
         returning the potentially incorrect transaction object *)
      return (from_store_transaction_object obj)
  | Some (Some raw_txn) ->
      (* We have the original transaction, let's try to reconstruct the
         transaction object *)
      let* res = reconstruct_from_raw_transaction obj raw_txn in
      return res

let reconstruct payload (obj : legacy_transaction_object) =
  let open Result_syntax in
  let* transactions = Blueprint_decoder.transactions payload in
  reconstruct_from_transactions_list transactions obj

let reconstruct_block payload block =
  let open Result_syntax in
  let* transactions = Blueprint_decoder.transactions payload in
  match block.transactions with
  | TxHash h -> return {block with transactions = TxHash h}
  | TxFull l ->
      let* l = List.map_e (reconstruct_from_transactions_list transactions) l in
      return {block with transactions = TxFull l}

let rereconstruct payload = function
  | Kernel obj -> reconstruct payload obj
  | other -> Ok other

let rereconstruct_block payload block =
  let open Result_syntax in
  let* transactions = Blueprint_decoder.transactions payload in
  match block.transactions with
  | TxHash h -> return {block with transactions = TxHash h}
  | TxFull l ->
      let* l =
        List.map_e
          (function
            | Kernel obj -> reconstruct_from_transactions_list transactions obj
            | other -> Ok other)
          l
      in
      return {block with transactions = TxFull l}

let hash = function
  | Kernel obj -> obj.hash
  | Legacy obj -> obj.hash
  | EIP_2930 obj -> obj.hash
  | EIP_1559 obj -> obj.hash

let block_number = function
  | Kernel obj -> obj.blockNumber
  | Legacy obj -> obj.blockNumber
  | EIP_2930 obj -> obj.block_number
  | EIP_1559 obj -> obj.block_number

let input = function
  | Kernel obj -> obj.input
  | Legacy obj -> obj.input
  | EIP_2930 obj -> obj.input
  | EIP_1559 obj -> obj.input

let to_ = function
  | Kernel obj -> obj.to_
  | Legacy obj -> obj.to_
  | EIP_2930 obj -> obj.to_
  | EIP_1559 obj -> obj.to_

let encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"legacy"
        (Tag 0)
        Ethereum_types.legacy_transaction_object_encoding
        (function Legacy o -> Some o | _ -> None)
        (fun o -> Legacy o);
      case
        ~title:"eip-2930"
        (Tag 3)
        (merge_objs (obj1 (req "type" (constant "0x1"))) EIP_2930.encoding)
        (function EIP_2930 o -> Some ((), o) | _ -> None)
        (fun ((), o) -> EIP_2930 o);
      case
        ~title:"eip-1559"
        (Tag 4)
        (merge_objs (obj1 (req "type" (constant "0x2"))) EIP_1559.encoding)
        (function EIP_1559 o -> Some ((), o) | _ -> None)
        (fun ((), o) -> EIP_1559 o);
      case
        ~title:"kernel"
        (Tag 255)
        Ethereum_types.legacy_transaction_object_encoding
        (function Kernel o -> Some o | _ -> None)
        (fun o -> Kernel o);
      (* The following are incorrect but kept for backward compatibility. See
         https://ethereum.org/en/developers/docs/apis/json-rpc/#quantities-encoding
         and
         https://ethereum.org/en/developers/docs/transactions/#typed-transaction-envelope. *)
      case
        ~title:"eip-2930-backward-compat"
        (Tag 1)
        (merge_objs (obj1 (req "type" (constant "0x01"))) EIP_2930.encoding)
        (fun _ -> None)
        (fun ((), o) -> EIP_2930 o);
      case
        ~title:"eip-1559-backward-compat"
        (Tag 2)
        (merge_objs (obj1 (req "type" (constant "0x02"))) EIP_1559.encoding)
        (fun _ -> None)
        (fun ((), o) -> EIP_1559 o);
    ]
