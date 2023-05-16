(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

  type 'params request = {
    method_ : string;
    parameters : 'params option;
    id : id;
  }

  let request_encoding method_ parameters_encoding =
    Data_encoding.(
      conv
        (fun {parameters; id; _} -> ((), (), parameters, id))
        (fun ((), (), parameters, id) -> {method_; parameters; id})
        (obj4
           (req "jsonrpc" (constant version))
           (req "method" (constant method_))
           (opt "params" parameters_encoding)
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

  type ('result, 'data_error) response = {
    value : ('result, 'data_error error) result;
    id : id;
  }

  let response_encoding result_encoding error_data_encoding =
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
           (opt "result" result_encoding)
           (opt "error" (error_encoding error_data_encoding))
           (req "id" (option id_repr_encoding))))
end

type input = ..

type output = ..

module Error = struct
  type t = unit

  let encoding = Data_encoding.unit
end

type 'result rpc_result = ('result, Error.t JSONRPC.error) result

module type METHOD_DEF = sig
  val method_ : string

  type input

  type output

  val input_encoding : input Data_encoding.t

  val output_encoding : output Data_encoding.t
end

module type METHOD = sig
  type m_input

  type m_output

  (* The parameters MAY be omitted. See JSONRPC Specification. *)
  type input += Input of m_input option

  type output += Output of m_output rpc_result

  val method_ : string

  val request_encoding : m_input JSONRPC.request Data_encoding.t

  val request : m_input option -> JSONRPC.id -> m_input JSONRPC.request

  val response_encoding : (m_output, Error.t) JSONRPC.response Data_encoding.t

  val response :
    (m_output, Error.t JSONRPC.error) result ->
    JSONRPC.id ->
    (m_output, Error.t) JSONRPC.response

  val response_ok :
    m_output -> JSONRPC.id -> (m_output, Error.t) JSONRPC.response
end

module MethodMaker (M : METHOD_DEF) :
  METHOD with type m_input = M.input and type m_output = M.output = struct
  type m_input = M.input

  type m_output = M.output

  type input += Input of m_input option

  type output += Output of m_output rpc_result

  let method_ = M.method_

  let request_encoding = JSONRPC.request_encoding M.method_ M.input_encoding

  let request parameters id = JSONRPC.{method_; parameters; id}

  let response_encoding =
    JSONRPC.response_encoding M.output_encoding Error.encoding

  let response value id = JSONRPC.{value; id}

  let response_ok result id = response (Ok result) id
end

module Network_id = MethodMaker (struct
  type input = unit

  type output = string

  let input_encoding = Data_encoding.unit

  let output_encoding = Data_encoding.string

  let method_ = "net_version"
end)

module Chain_id = MethodMaker (struct
  type input = unit

  type output = Ethereum_types.quantity

  let input_encoding = Data_encoding.unit

  let output_encoding = Ethereum_types.quantity_encoding

  let method_ = "eth_chainId"
end)

module Accounts = MethodMaker (struct
  type input = unit

  type output = Ethereum_types.address list

  let input_encoding = Data_encoding.unit

  let output_encoding = Data_encoding.list Ethereum_types.address_encoding

  let method_ = "eth_accounts"
end)

module Get_balance = MethodMaker (struct
  open Ethereum_types

  type input = address * block_param

  type output = quantity

  let input_encoding = Data_encoding.tup2 address_encoding block_param_encoding

  let output_encoding = quantity_encoding

  let method_ = "eth_getBalance"
end)

module Block_number = MethodMaker (struct
  open Ethereum_types

  type input = unit

  type output = block_height

  let input_encoding = Data_encoding.unit

  let output_encoding = block_height_encoding

  let method_ = "eth_blockNumber"
end)

module Get_block_by_number = MethodMaker (struct
  open Ethereum_types

  type input = block_param * bool

  type output = block

  let input_encoding =
    Data_encoding.tup2 block_param_encoding Data_encoding.bool

  let output_encoding = block_encoding

  let method_ = "eth_getBlockByNumber"
end)

module Get_block_by_hash = MethodMaker (struct
  open Ethereum_types

  type input = hash * bool

  type output = block

  let input_encoding = Data_encoding.tup2 hash_encoding Data_encoding.bool

  let output_encoding = block_encoding

  let method_ = "eth_getBlockByHash"
end)

module Get_code = MethodMaker (struct
  open Ethereum_types

  type input = address * block_param

  type output = hash

  let input_encoding = Data_encoding.tup2 address_encoding block_param_encoding

  let output_encoding = hash_encoding

  let method_ = "eth_getCode"
end)

module Gas_price = MethodMaker (struct
  open Ethereum_types

  type input = unit

  type output = quantity

  let input_encoding = Data_encoding.unit

  let output_encoding = quantity_encoding

  let method_ = "eth_gasPrice"
end)

module Get_transaction_count = MethodMaker (struct
  open Ethereum_types

  type input = address * block_param

  type output = quantity

  let input_encoding = Data_encoding.tup2 address_encoding block_param_encoding

  let output_encoding = quantity_encoding

  let method_ = "eth_getTransactionCount"
end)

module Get_transaction_receipt = MethodMaker (struct
  open Ethereum_types

  type input = hash

  type output = transaction_receipt option

  let input_encoding = Data_encoding.tup1 hash_encoding

  let output_encoding = Data_encoding.option transaction_receipt_encoding

  let method_ = "eth_getTransactionReceipt"
end)

module Get_transaction_by_hash = MethodMaker (struct
  open Ethereum_types

  type input = hash

  type output = transaction_object option

  let input_encoding = Data_encoding.tup1 hash_encoding

  let output_encoding = Data_encoding.option transaction_object_encoding

  let method_ = "eth_getTransactionByHash"
end)

module Send_raw_transaction = MethodMaker (struct
  open Ethereum_types

  type input = hash

  type output = hash

  let input_encoding = Data_encoding.tup1 hash_encoding

  let output_encoding = hash_encoding

  let method_ = "eth_sendRawTransaction"
end)

module Send_transaction = MethodMaker (struct
  open Ethereum_types

  type input = transaction

  type output = hash

  let input_encoding = transaction_encoding

  let output_encoding = hash_encoding

  let method_ = "eth_sendTransaction"
end)

module Eth_call = MethodMaker (struct
  open Ethereum_types

  type input = call * block_param

  type output = hash

  let input_encoding = Data_encoding.tup2 call_encoding block_param_encoding

  let output_encoding = hash_encoding

  let method_ = "eth_call"
end)

module Get_estimate_gas = MethodMaker (struct
  open Ethereum_types

  type input = call

  type output = quantity

  let input_encoding = Data_encoding.tup1 call_encoding

  let output_encoding = quantity_encoding

  let method_ = "eth_estimateGas"
end)

module Txpool_content = MethodMaker (struct
  open Ethereum_types

  type input = unit

  type output = txpool

  let input_encoding = Data_encoding.unit

  let output_encoding = txpool_encoding

  let method_ = "txpool_content"
end)

module Web3_clientVersion = MethodMaker (struct
  type input = unit

  type output = string

  let input_encoding = Data_encoding.unit

  let output_encoding = Data_encoding.string

  let method_ = "web3_clientVersion"
end)

let methods : (module METHOD) list =
  [
    (module Network_id);
    (module Chain_id);
    (module Accounts);
    (module Get_balance);
    (module Block_number);
    (module Get_block_by_number);
    (module Get_block_by_hash);
    (module Get_code);
    (module Gas_price);
    (module Get_transaction_count);
    (module Get_transaction_receipt);
    (module Get_transaction_by_hash);
    (module Send_transaction);
    (module Send_raw_transaction);
    (module Eth_call);
    (module Get_estimate_gas);
    (module Txpool_content);
    (module Web3_clientVersion);
  ]

module Input = struct
  type t = input

  let case_maker tag_id (module M : METHOD) =
    let open Data_encoding in
    case
      ~title:M.method_
      (Tag tag_id)
      M.request_encoding
      (function M.Input input, id -> Some (M.request input id) | _ -> None)
      (fun {parameters; id; _} -> (M.Input parameters, id))

  let encoding =
    let open Data_encoding in
    union @@ List.mapi case_maker methods
end

module Output = struct
  type nonrec 'a result = ('a, error JSONRPC.error) result

  let case_maker tag_id (module M : METHOD) =
    let open Data_encoding in
    case
      ~title:M.method_
      (Tag tag_id)
      M.response_encoding
      (function
        | M.Output accounts, id -> Some JSONRPC.{value = accounts; id}
        | _ -> None)
      (fun {value = req; id} -> (M.Output req, id))

  let encoding =
    let open Data_encoding in
    union @@ List.mapi case_maker methods
end
