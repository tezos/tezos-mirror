(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
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

(** Encodings for the JSON-RPC standard. See
    https://www.jsonrpc.org/specification.
*)
module JSONRPC = struct
  let version = "2.0"

  (** Ids in the JSON-RPC specification can be either a string, a number or NULL
      (which is represented by the option type). *)
  type id_repr = Id_string of string | Id_float of float

  let id_repr_encoding =
    let open Data_encoding in
    union
      [
        case
          ~title:"id-string"
          (Tag 0)
          string
          (function Id_string s -> Some s | _ -> None)
          (fun s -> Id_string s);
        case
          ~title:"id-int"
          (Tag 1)
          float
          (function Id_float i -> Some i | _ -> None)
          (fun i -> Id_float i);
      ]

  type id = id_repr option

  type request = {
    method_ : string;
    parameters : Data_encoding.json option;
    id : id;
  }

  let request_encoding =
    Data_encoding.(
      conv
        (fun {parameters; id; method_; _} -> ((), method_, parameters, id))
        (fun ((), method_, parameters, id) -> {method_; parameters; id})
        (obj4
           (req "jsonrpc" (constant version))
           (req "method" string)
           (opt "params" Data_encoding.json)
           (opt "id" id_repr_encoding)))

  type 'data error = {code : int; message : string; data : 'data option}

  let error_encoding data_encoding =
    Data_encoding.(
      conv
        (fun {code; message; data} -> (code, message, data))
        (fun (code, message, data) -> {code; message; data})
        (obj3
           (req "code" int31)
           (req "message" string)
           (opt "data" data_encoding)))

  type value = (Data_encoding.json, Data_encoding.json error) result

  type response = {value : value; id : id}

  let response_encoding =
    Data_encoding.(
      conv
        (fun {value; id} ->
          let result, error =
            match value with Ok r -> (Some r, None) | Error e -> (None, Some e)
          in
          ((), result, error, id))
        (fun ((), result, error, id) ->
          let value =
            match (result, error) with
            | Some r, None -> Ok r
            | None, Some e -> Error e
            | _ -> assert false
            (* Impossible case according to the JSON-RPC standard: result XOR
               error. *)
          in
          {value; id})
        (obj4
           (req "jsonrpc" (constant version))
           (opt "result" Data_encoding.json)
           (opt "error" (error_encoding Data_encoding.json))
           (req "id" (option id_repr_encoding))))
end

module Error = struct
  type t = unit

  let encoding = Data_encoding.unit
end

type 'result rpc_result = ('result, Error.t JSONRPC.error) result

type ('input, 'output) method_ = ..

module type METHOD = sig
  val method_ : string

  type input

  type output

  val input_encoding : input Data_encoding.t

  val output_encoding : output Data_encoding.t

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Kernel_version = struct
  type input = unit

  type output = string

  let input_encoding = Data_encoding.unit

  let output_encoding = Data_encoding.string

  let method_ = "tez_kernelVersion"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Network_id = struct
  type input = unit

  type output = string

  let input_encoding = Data_encoding.unit

  let output_encoding = Data_encoding.string

  let method_ = "net_version"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Chain_id = struct
  type input = unit

  type output = Ethereum_types.quantity

  let input_encoding = Data_encoding.unit

  let output_encoding = Ethereum_types.quantity_encoding

  let method_ = "eth_chainId"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Accounts = struct
  type input = unit

  type output = Ethereum_types.address list

  let input_encoding = Data_encoding.unit

  let output_encoding = Data_encoding.list Ethereum_types.address_encoding

  let method_ = "eth_accounts"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Get_balance = struct
  open Ethereum_types

  type input = address * block_param

  type output = quantity

  let input_encoding = Data_encoding.tup2 address_encoding block_param_encoding

  let output_encoding = quantity_encoding

  let method_ = "eth_getBalance"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Get_storage_at = struct
  open Ethereum_types

  type input = address * quantity * block_param

  type output = hex

  let input_encoding =
    Data_encoding.tup3 address_encoding quantity_encoding block_param_encoding

  let output_encoding = hex_encoding

  let method_ = "eth_getStorageAt"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Block_number = struct
  open Ethereum_types

  type input = unit

  type output = block_height

  let input_encoding = Data_encoding.unit

  let output_encoding = block_height_encoding

  let method_ = "eth_blockNumber"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Get_block_by_number = struct
  open Ethereum_types

  type input = block_param * bool

  type output = block

  let input_encoding =
    Data_encoding.tup2 block_param_encoding Data_encoding.bool

  let output_encoding = block_encoding

  let method_ = "eth_getBlockByNumber"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Get_block_by_hash = struct
  open Ethereum_types

  type input = block_hash * bool

  type output = block

  let input_encoding = Data_encoding.tup2 block_hash_encoding Data_encoding.bool

  let output_encoding = block_encoding

  let method_ = "eth_getBlockByHash"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Get_code = struct
  open Ethereum_types

  type input = address * block_param

  type output = hex

  let input_encoding = Data_encoding.tup2 address_encoding block_param_encoding

  let output_encoding = hex_encoding

  let method_ = "eth_getCode"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Gas_price = struct
  open Ethereum_types

  type input = unit

  type output = quantity

  let input_encoding = Data_encoding.unit

  let output_encoding = quantity_encoding

  let method_ = "eth_gasPrice"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Get_transaction_count = struct
  open Ethereum_types

  type input = address * block_param

  type output = quantity

  let input_encoding = Data_encoding.tup2 address_encoding block_param_encoding

  let output_encoding = quantity_encoding

  let method_ = "eth_getTransactionCount"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Get_block_transaction_count_by_hash = struct
  open Ethereum_types

  type input = block_hash

  type output = quantity

  let input_encoding = Data_encoding.tup1 block_hash_encoding

  let output_encoding = quantity_encoding

  let method_ = "eth_getBlockTransactionCountByHash"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Get_block_transaction_count_by_number = struct
  open Ethereum_types

  type input = block_param

  type output = quantity

  let input_encoding = Data_encoding.tup1 block_param_encoding

  let output_encoding = quantity_encoding

  let method_ = "eth_getBlockTransactionCountByNumber"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Get_uncle_count_by_block_hash = struct
  open Ethereum_types

  type input = block_hash

  type output = quantity

  let input_encoding = Data_encoding.tup1 block_hash_encoding

  let output_encoding = quantity_encoding

  let method_ = "eth_getUncleCountByBlockHash"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Get_uncle_count_by_block_number = struct
  open Ethereum_types

  type input = block_param

  type output = quantity

  let input_encoding = Data_encoding.tup1 block_param_encoding

  let output_encoding = quantity_encoding

  let method_ = "eth_getUncleCountByBlockNumber"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Get_transaction_receipt = struct
  open Ethereum_types

  type input = hash

  type output = transaction_receipt option

  let input_encoding = Data_encoding.tup1 hash_encoding

  let output_encoding = Data_encoding.option transaction_receipt_encoding

  let method_ = "eth_getTransactionReceipt"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Get_transaction_by_hash = struct
  open Ethereum_types

  type input = hash

  type output = transaction_object option

  let input_encoding = Data_encoding.tup1 hash_encoding

  let output_encoding = Data_encoding.option transaction_object_encoding

  let method_ = "eth_getTransactionByHash"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Get_transaction_by_block_hash_and_index = struct
  open Ethereum_types

  type input = block_hash * quantity

  type output = transaction_object option

  let input_encoding = Data_encoding.tup2 block_hash_encoding quantity_encoding

  let output_encoding = Data_encoding.option transaction_object_encoding

  let method_ = "eth_getTransactionByBlockHashAndIndex"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Get_transaction_by_block_number_and_index = struct
  open Ethereum_types

  type input = block_param * quantity

  type output = transaction_object option

  let input_encoding = Data_encoding.tup2 block_param_encoding quantity_encoding

  let output_encoding = Data_encoding.option transaction_object_encoding

  let method_ = "eth_getTransactionByBlockNumberAndIndex"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Get_uncle_by_block_hash_and_index = struct
  open Ethereum_types

  type input = block_hash * quantity

  type output = block option

  let input_encoding = Data_encoding.tup2 block_hash_encoding quantity_encoding

  let output_encoding = Data_encoding.option block_encoding

  let method_ = "eth_getUncleByBlockHashAndIndex"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Get_uncle_by_block_number_and_index = struct
  open Ethereum_types

  type input = block_param * quantity

  type output = block option

  let input_encoding = Data_encoding.tup2 block_param_encoding quantity_encoding

  let output_encoding = Data_encoding.option block_encoding

  let method_ = "eth_getUncleByBlockNumberAndIndex"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Send_raw_transaction = struct
  open Ethereum_types

  type input = hex

  type output = hash

  let input_encoding = Data_encoding.tup1 hex_encoding

  let output_encoding = hash_encoding

  let method_ = "eth_sendRawTransaction"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Send_transaction = struct
  open Ethereum_types

  type input = transaction

  type output = hash

  let input_encoding = transaction_encoding

  let output_encoding = hash_encoding

  let method_ = "eth_sendTransaction"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Eth_call = struct
  open Ethereum_types

  type input = call * block_param

  type output = hash

  let input_encoding = Data_encoding.tup2 call_encoding block_param_encoding

  let output_encoding = hash_encoding

  let method_ = "eth_call"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Get_estimate_gas = struct
  open Ethereum_types

  type input = call * block_param

  type output = quantity

  let input_encoding =
    let open Data_encoding in
    union
      [
        case
          ~title:"full_parameters"
          (Tag 0)
          (tup2 call_encoding block_param_encoding)
          (fun (call, block_param) -> Some (call, block_param))
          (fun (call, block_param) -> (call, block_param));
        (* eth-cli doesn't put the block parameter. *)
        case
          ~title:"only_call_parameter"
          (Tag 1)
          (tup1 call_encoding)
          (fun (call, _) -> Some call)
          (fun call -> (call, Latest));
      ]

  let output_encoding = quantity_encoding

  let method_ = "eth_estimateGas"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Txpool_content = struct
  open Ethereum_types

  type input = unit

  type output = txpool

  let input_encoding = Data_encoding.unit

  let output_encoding = txpool_encoding

  let method_ = "txpool_content"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Web3_clientVersion = struct
  type input = unit

  type output = string

  let input_encoding = Data_encoding.unit

  let output_encoding = Data_encoding.string

  let method_ = "web3_clientVersion"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Web3_sha3 = struct
  open Ethereum_types

  type input = hex

  type output = hash

  let input_encoding = Data_encoding.tup1 hex_encoding

  let output_encoding = hash_encoding

  let method_ = "web3_sha3"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Get_logs = struct
  open Ethereum_types

  type input = filter

  type output = filter_changes list

  let input_encoding = Data_encoding.tup1 filter_encoding

  let output_encoding = Data_encoding.list filter_changes_encoding

  let method_ = "eth_getLogs"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Produce_block = struct
  type input = Time.Protocol.t

  type output = Ethereum_types.quantity

  let input_encoding = Time.Protocol.encoding

  let output_encoding = Ethereum_types.quantity_encoding

  let method_ = "produceBlock"

  type ('input, 'output) method_ += Method : (input, output) method_
end

type map_result =
  | Method :
      ('input, 'output) method_
      * (module METHOD with type input = 'input and type output = 'output)
      -> map_result
  | Unsupported
  | Unknown

let supported_methods : (module METHOD) list =
  [
    (module Kernel_version);
    (module Network_id);
    (module Chain_id);
    (module Accounts);
    (module Get_balance);
    (module Get_storage_at);
    (module Block_number);
    (module Get_block_by_number);
    (module Get_block_by_hash);
    (module Get_code);
    (module Gas_price);
    (module Get_transaction_count);
    (module Get_block_transaction_count_by_hash);
    (module Get_block_transaction_count_by_number);
    (module Get_logs);
    (module Get_uncle_count_by_block_hash);
    (module Get_uncle_count_by_block_number);
    (module Get_transaction_receipt);
    (module Get_transaction_by_hash);
    (module Get_transaction_by_block_hash_and_index);
    (module Get_transaction_by_block_number_and_index);
    (module Get_uncle_by_block_hash_and_index);
    (module Get_uncle_by_block_number_and_index);
    (module Send_transaction);
    (module Send_raw_transaction);
    (module Eth_call);
    (module Get_estimate_gas);
    (module Txpool_content);
    (module Web3_clientVersion);
    (module Web3_sha3);
    (module Produce_block);
  ]

let unsupported_methods : string list =
  [
    "net_listening";
    "net_peerCount";
    "eth_protocolVersion";
    "eth_syncing";
    "eth_coinbase";
    "eth_mining";
    "eth_hashrate";
    "eth_accounts";
    "eth_sign";
    "eth_signTransaction";
    "eth_sendTransaction";
  ]

let map_method_name method_name =
  match
    List.find
      (fun (module M : METHOD) -> M.method_ = method_name)
      supported_methods
  with
  | Some (module M) -> Method (M.Method, (module M))
  | None ->
      if List.mem ~equal:( = ) method_name unsupported_methods then Unsupported
      else Unknown
