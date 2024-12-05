(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Functori, <contact@functori.com>             *)
(*                                                                           *)
(*****************************************************************************)

(* This file is sourced from the efunc library, licensed under the MIT License:
   https://gitlab.com/functori/dev/efunc. *)

include Private
open Json_encoding

type bz = Z.t

let bz_enc =
  union
    [
      case string (fun z -> Some ("0x" ^ Z.format "%x" z)) Z.of_string;
      case int53 (fun _ -> None) Z.of_int64;
    ]

type bint = int

let bint_enc =
  union
    [
      case string (fun i -> Some (Format.sprintf "0x%x" i)) int_of_string;
      case int53 (fun _ -> None) Int64.to_int;
    ]

type bint64 = int64

let bint64_enc =
  union
    [
      case string (fun i -> Some (Format.sprintf "0x%Lx" i)) Int64.of_string;
      case int53 (fun _ -> None) (fun i -> i);
    ]

type rope = Rope.t

type rlp = S of rope | L of rlp list

type 'a input = {meth : string; [@key "method"] params : 'a}

type syncing_info = {
  starting_block : bint;
  current_block : bint;
  highest_block : bint;
}

type level_tag = ([`latest | `earliest | `pending][@enum])

type level_id = [level_tag | `level of bint]

type block_id = [level_id | `hash of b]

type transaction_count =
  [ `address of address * block_id
  | (block_id[@encoding Json_encoding.tup1 block_id_enc]) ]

type signature = {v : bint; r : b; s : b}

type fixed_type =
  [ `int of (int[@wrap "int"])
  | `uint of (int[@wrap "uint"])
  | `bool
  | `address
  | `fixed of (int * int[@wrap "fixed"])
  | `ufixed of (int * int[@wrap "ufixed"])
  | `fbytes of (int[@wrap "bytes"]) ]

type evm_type =
  [ fixed_type
  | `bytes
  | `string
  | `array of (evm_type[@wrap "array"])
  | `farray of (evm_type * int[@wrap "array"])
  | `tuple of evm_type list ]

type evm_value =
  [ `int of (Z.t[@encoding Json_encoding.(conv Z.to_string Z.of_string string)])
  | `bool of bool
  | `address of (Private.address[@wrap "address"])
  | `bytes of Private.b
  | `string of string
  | `array of evm_value list ]

type data_input =
  | B of b
  | O of {name : string; types : evm_type list; values : evm_value list}

type access_list = {al_address : address; al_storage_keys : b list}

type create_access_list_output = {
  calo_access_list : access_list list;
  calo_error : string option;
  calo_gas_used : bint;
}

type 'a transaction_input = {
  ti_max_priority_fee : bz; [@key "maxPriorityFeePerGas"] [@dft Z.minus_one]
  ti_max_fee : bz; [@key "maxFeePerGas"] [@dft Z.minus_one]
  ti_value : bz; [@dft Z.zero]
  ti_data : 'a option;
  ti_chain_id : bint; [@dft -1]
  ti_nonce : bint; [@dft -1]
  ti_gas_limit : bint; [@dft -1]
  ti_access_list : access_list list; [@dft []] [@camel]
  ti_signature : signature option;
  ti_to : address option;
  ti_max_fee_per_blob_gas : bz option;
  ti_blob_versioned_hashes : bz list; [@dft []]
  ti_blobs : b list; [@dft []]
}

let ti ?(chain_id = -1) ?(nonce = -1) ?(gas_limit = -1)
    ?(max_priority_fee = Z.minus_one) ?(max_fee = Z.minus_one) ?(value = Z.zero)
    ?data ?(access_list = []) ?signature ?max_blob_fee
    ?(blob_versioned_hashes = []) ?(blobs = []) ti_to =
  {
    ti_max_priority_fee = max_priority_fee;
    ti_max_fee = max_fee;
    ti_value = value;
    ti_data = data;
    ti_chain_id = chain_id;
    ti_nonce = nonce;
    ti_gas_limit = gas_limit;
    ti_access_list = access_list;
    ti_signature = signature;
    ti_to;
    ti_max_fee_per_blob_gas = max_blob_fee;
    ti_blob_versioned_hashes = blob_versioned_hashes;
    ti_blobs = blobs;
  }

type 'a block = {
  number : bint;
  hash : b option;
  parent_hash : b;
  nonce : b option;
  sha3_uncles : b;
  logs_bloom : b option; [@req]
  transactions_root : b;
  state_root : b;
  receipts_root : b;
  miner : address option;
  difficulty : bz;
  total_difficulty : bz option;
  extra_data : b;
  size : bint;
  gas_limit : bint;
  gas_used : bint;
  timestamp : bint64;
  transactions : 'a list;
  uncles : b list;
  base_fee : bz; [@dft Z.zero] [@key "baseFeePerGas"]
}

type transaction = {
  from : address;
  dst : address option; [@key "to"]
  gas : bint;
  gas_price : bz;
  value : bz;
  block_hash : b option;
  block_number : bint option;
  tx_hash : b; [@key "hash"]
  input : b;
  tx_nonce : bint; [@key "nonce"]
  transaction_index : bint option;
  signature : signature; [@merge]
  max_fee_per_gas : bz option;
  max_priority_fee_per_gas : bz option;
  typ : bint option; [@key "type"]
  access_list : access_list list; [@dft []]
  chain_id : bint option;
  y_parity : string option;
  max_fee_per_blob_gas : bz option;
  blob_versioned_hashes : bz list; [@dft []]
}

type transaction_id =
  [ `hash of (b[@encoding Json_encoding.tup1 b_enc])
  | `block of b * bint
  | `level of level_id * bint ]

type 'a log = {
  l_removed : bool;
  l_log_index : bint option;
  l_transaction_index : bint option;
  l_transaction_hash : b option;
  l_block_hash : b option;
  l_block_number : bint option;
  l_address : address;
  l_data : 'a;
  l_topics : 'a list; [@dft []]
}

type transaction_receipt = {
  r_transaction_hash : b;
  r_transaction_index : bint;
  r_block_hash : b;
  r_block_number : bint;
  r_from : address;
  r_dst : address option; [@key "to"] [@req]
  (* Address of the receiver or null in a contract creation transaction. *)
  r_cumulative_gas_used : bint;
  (* The sum of gas used by this transaction and all preceding transactions
     in the same block. *)
  r_gas_used : bint;
  (* The amount of gas used for this specific transaction alone. *)
  r_contract_address : address option; [@req]
  r_logs : b log list;
  r_logs_bloom : b;
  r_root : b option;
  (* The post-transaction state root. Only specified for transactions included before
     the Byzantium upgrade. *)
  r_status : bint;
  (* Either 1 (success) or 0 (failure). Only specified for transactions included
     after the Byzantium upgrade. *)
  r_effective_gas_price : bint;
      (* The actual value per gas deducted from the senders account.
         Before EIP-1559, this is equal to the transaction's gas price.
         After, it is equal to
         baseFeePerGas + min(maxFeePerGas - baseFeePerGas, maxPriorityFeePerGas). *)
}

type uncle_id = [`block of b * bint | `level of level_id * bint]

type topic = T of b option | Or of b list

type filter = {
  from_block : level_id option;
  to_block : level_id option;
  address : address list; [@dft []]
  topics : topic list; [@dft []]
}

let filter ?from ?to_ ?(addresses = []) topics =
  {from_block = from; to_block = to_; address = addresses; topics}

type none =
  (unit
  [@encoding Json_encoding.(conv (fun () -> [||]) (fun _ -> ()) (array unit))])

type get_filter = [`changes of bz | `all of bz]

type event = {
  e_name : string;
  e_types : (evm_type * bool) list;
  e_values : evm_value list option list;
  e_anonymous : bool;
}

type 'a filter_result = [`hash of b | `log of 'a log]

type compiler = ([`solidity | `lll | `serpent][@enum])

type _ meth =
  | Client_version : string meth
  | Sha3 : b -> b meth
  | Version : string meth
  | Listening : bool meth
  | Peer_count : bint meth
  | Protocol : string meth
  | Coinbase : b meth
  | Mining : bool meth
  | Hashrate : bint meth
  | Gas_price : bz meth
  | Max_priority_fee : bz meth
  | Chain_id : bint meth
  | Accounts : address list meth
  | Level : bint meth
  | Balance : address * level_id -> bz meth
  | Storage : address * bz * level_id -> b meth
  | Transaction_count : transaction_count -> bint meth
  | Uncle_count : block_id -> bint meth
  | Code : address * level_id -> b meth
  | Sign : address * b -> b meth
  | Sign_transaction : b transaction_input -> b meth
  | Send_transaction : b transaction_input -> b meth
  | Send_raw_transaction : b -> b meth
  | Call :
      b transaction_input * address option * bint option * level_id
      -> b meth
  | Create_access_list :
      b transaction_input * address option * bint option * level_id
      -> create_access_list_output meth
  | Estimate_gas : b transaction_input * address option * level_id -> bint meth
  | GetBlockReceipts : block_id -> transaction_receipt list meth
  | Block : block_id -> b block meth
  | Block_exp : block_id -> transaction block meth
  | Transaction : transaction_id -> transaction meth
  | Receipt : b -> transaction_receipt meth
  | Uncle : uncle_id -> b block meth
  | Compilers : compiler list meth
  | Compile : compiler * string -> Json_repr.ezjsonm meth
  | Remove_filter : bz -> bool meth
  | Get_filter : get_filter -> b filter_result list meth
  | Logs : filter -> b log list meth
  | Work : (b * b * b) meth
  | Submit_work : b * b * b -> bool meth
  | Submit_hashrate : b * b -> bool meth
