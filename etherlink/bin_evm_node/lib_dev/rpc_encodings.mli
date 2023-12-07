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
module JSONRPC : sig
  (** Constant being `2.0`. *)
  val version : string

  (** Ids in the JSON-RPC specification can be either a string, a number or NULL
      (which is represented by the option type). Note that MetaMask uses ids
      that only fit in 64 bits, which is not supported by Data_encoding. *)
  type id_repr = Id_string of string | Id_float of float

  val id_repr_encoding : id_repr Data_encoding.t

  type id = id_repr option

  (** JSON-RPC Request object:
  {@js[
    { "jsonrpc" : "2.0",
    "method": <string>,
    "params": <array | object>, //optional
    "id": <string | number | NULL> //optional
    }
    ]}
  *)
  type request = {
    method_ : string;
    parameters : Data_encoding.json option;  (** `params` is optional. *)
    id : id;  (** `id` is optional. *)
  }

  val request_encoding : request Data_encoding.t

  (** JSON-RPC Error representation.
  {@js[
      { "code" : <number>,
        "message": <string>,
        "data": <any value>
      }
    ]}
  *)
  type 'data error = {code : int; message : string; data : 'data option}

  val error_encoding : 'a Data_encoding.t -> 'a error Data_encoding.t

  type value = (Data_encoding.json, Data_encoding.json error) result

  (** JSON-RPC Response object:
  {@js[
      { "jsonrpc": "2.0",
        "result": <any>,
        "error": <error object>,
        "id": <id>
      }
    ]}

      Note that `result` and `error` cannot appear at the same time, hence the
      choice of using the result type as representation. *)
  type response = {value : value; id : id}

  val response_encoding : response Data_encoding.t
end

(* Errors returned by the RPC server, to be embedded as data to the JSON-RPC
   error object. *)
module Error : sig
  type t = unit

  val encoding : unit Data_encoding.t
end

type 'result rpc_result = ('result, Error.t JSONRPC.error) result

type ('input, 'output) method_ = ..

(** API of an Ethereum method. *)
module type METHOD = sig
  (** Method name in the specification. *)
  val method_ : string

  (** Type of expected input, if any. *)
  type input

  (** Type of the value returned by the RPC. *)
  type output

  val input_encoding : input Data_encoding.t

  val output_encoding : output Data_encoding.t

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Kernel_version : METHOD with type input = unit and type output = string

module Network_id : METHOD with type input = unit and type output = string

module Chain_id :
  METHOD with type input = unit and type output = Ethereum_types.quantity

module Accounts :
  METHOD with type input = unit and type output = Ethereum_types.address list

module Get_balance :
  METHOD
    with type input = Ethereum_types.address * Ethereum_types.block_param
     and type output = Ethereum_types.quantity

module Get_storage_at :
  METHOD
    with type input =
      Ethereum_types.address
      * Ethereum_types.quantity
      * Ethereum_types.block_param
     and type output = Ethereum_types.hex

module Block_number :
  METHOD with type input = unit and type output = Ethereum_types.block_height

module Get_block_by_number :
  METHOD
    with type input = Ethereum_types.block_param * bool
     and type output = Ethereum_types.block

module Get_block_by_hash :
  METHOD
    with type input = Ethereum_types.block_hash * bool
     and type output = Ethereum_types.block

module Get_code :
  METHOD
    with type input = Ethereum_types.address * Ethereum_types.block_param
     and type output = Ethereum_types.hex

module Gas_price :
  METHOD with type input = unit and type output = Ethereum_types.quantity

module Get_transaction_count :
  METHOD
    with type input = Ethereum_types.address * Ethereum_types.block_param
     and type output = Ethereum_types.quantity

module Get_block_transaction_count_by_hash :
  METHOD
    with type input = Ethereum_types.block_hash
     and type output = Ethereum_types.quantity

module Get_block_transaction_count_by_number :
  METHOD
    with type input = Ethereum_types.block_param
     and type output = Ethereum_types.quantity

module Get_uncle_count_by_block_hash :
  METHOD
    with type input = Ethereum_types.block_hash
     and type output = Ethereum_types.quantity

module Get_uncle_count_by_block_number :
  METHOD
    with type input = Ethereum_types.block_param
     and type output = Ethereum_types.quantity

module Get_transaction_receipt :
  METHOD
    with type input = Ethereum_types.hash
     and type output = Ethereum_types.transaction_receipt option

module Get_transaction_by_hash :
  METHOD
    with type input = Ethereum_types.hash
     and type output = Ethereum_types.transaction_object option

module Get_transaction_by_block_hash_and_index :
  METHOD
    with type input = Ethereum_types.block_hash * Ethereum_types.quantity
     and type output = Ethereum_types.transaction_object option

module Get_transaction_by_block_number_and_index :
  METHOD
    with type input = Ethereum_types.block_param * Ethereum_types.quantity
     and type output = Ethereum_types.transaction_object option

module Get_uncle_by_block_hash_and_index :
  METHOD
    with type input = Ethereum_types.block_hash * Ethereum_types.quantity
     and type output = Ethereum_types.block option

module Get_uncle_by_block_number_and_index :
  METHOD
    with type input = Ethereum_types.block_param * Ethereum_types.quantity
     and type output = Ethereum_types.block option

module Send_raw_transaction :
  METHOD
    with type input = Ethereum_types.hex
     and type output = Ethereum_types.hash

module Send_transaction :
  METHOD
    with type input = Ethereum_types.transaction
     and type output = Ethereum_types.hash

module Eth_call :
  METHOD
    with type input = Ethereum_types.call * Ethereum_types.block_param
     and type output = Ethereum_types.hash

module Get_estimate_gas :
  METHOD
    with type input = Ethereum_types.call * Ethereum_types.block_param
     and type output = Ethereum_types.quantity

module Txpool_content :
  METHOD with type input = unit and type output = Ethereum_types.txpool

module Web3_clientVersion :
  METHOD with type input = unit and type output = string

module Web3_sha3 :
  METHOD
    with type input = Ethereum_types.hex
     and type output = Ethereum_types.hash

module Get_logs :
  METHOD
    with type input = Ethereum_types.filter
     and type output = Ethereum_types.filter_changes list

type map_result =
  | Method :
      ('input, 'output) method_
      * (module METHOD with type input = 'input and type output = 'output)
      -> map_result
  | Unsupported
  | Unknown

val map_method_name : string -> map_result
