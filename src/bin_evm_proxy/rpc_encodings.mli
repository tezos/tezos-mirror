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
      { "jsonrpc" : "2.0",
        "method": <string>,
        "params": <array | object>, //optional
        "id": <string | number | NULL> //optional
      }
  *)
  type 'params request = {
    method_ : string;
    parameters : 'params option;  (** `params` is optional. *)
    id : id;  (** `id` is optional. *)
  }

  val request_encoding :
    string -> 'a Data_encoding.t -> 'a request Data_encoding.t

  (** JSON-RPC Error representation.
      { "code" : <number>,
        "message": <string>,
        "data": <any value>
      }
  *)
  type 'data error = {code : int; message : string; data : 'data option}

  val error_encoding : 'a Data_encoding.t -> 'a error Data_encoding.t

  (** JSON-RPC Response object:
      { "jsonrpc": "2.0",
        "result": <any>,
        "error": <error object>,
        "id": <id>
      }

      Note that `result` and `error` cannot appear at the same time, hence the
      choice of using the result type as representation. *)
  type ('result, 'data_error) response = {
    value : ('result, 'data_error error) result;
    id : id;
  }

  val response_encoding :
    'a Data_encoding.t ->
    'b Data_encoding.t ->
    ('a, 'b) response Data_encoding.t
end

(* Errors returned by the RPC server, to be embedded as data to the JSON-RPC
   error object. *)
module Error : sig
  type t = unit

  val encoding : unit Data_encoding.t
end

(** Extensible variant representing the possible input requests extended by the
    application of the method generator. *)
type input = ..

(** Extensible variant representing the possible outputs extended by the
    application of the method generator. *)
type output = ..

type 'result rpc_result = ('result, Error.t JSONRPC.error) result

(** API of an Ethereum method. *)
module type METHOD_DEF = sig
  (** Method name in the specification. *)
  val method_ : string

  (** Type of expected input, if any. *)
  type input

  (** Type of the value returned by the RPC. *)
  type output

  val input_encoding : input Data_encoding.t

  val output_encoding : output Data_encoding.t
end

(** Interface of a generated method. *)
module type METHOD = sig
  (** Input type of the method, if any. *)
  type m_input

  (** Output type of the method. *)
  type m_output

  (** Variant representing the method's request. *)
  type input += Input of m_input option

  (** Variant representing the method's response. *)
  type output += Output of m_output rpc_result

  (** See {METHOD_DEF.method_}. *)
  val method_ : string

  val request_encoding : m_input JSONRPC.request Data_encoding.t

  (** [request input] builds a request object of the current method. *)
  val request : m_input option -> JSONRPC.id -> m_input JSONRPC.request

  val response_encoding : (m_output, Error.t) JSONRPC.response Data_encoding.t

  (** [response output] returns a response object for the method. *)
  val response :
    (m_output, Error.t JSONRPC.error) result ->
    JSONRPC.id ->
    (m_output, Error.t) JSONRPC.response

  (** [response_ok output] is a shortcut for [reponse (Ok output)]. *)
  val response_ok :
    m_output -> JSONRPC.id -> (m_output, Error.t) JSONRPC.response
end

(** Builds a full Method module out of a method description. *)
module MethodMaker : functor (M : METHOD_DEF) ->
  METHOD with type m_input = M.input and type m_output = M.output

(** [methods] is the list of currently defined methods. *)
val methods : (module METHOD) list

(** [Input] defines the input encoding matching the defined methods in
    {methods}. *)
module Input : sig
  type t = input

  val encoding : (input * JSONRPC.id) Data_encoding.t
end

(** [Output] defines the output encoding matching the defined methods in
    {methods}. *)
module Output : sig
  type nonrec 'a result = ('a, error JSONRPC.error) result

  val encoding : (output * JSONRPC.id) Data_encoding.t
end

module Network_id : METHOD with type m_input = unit and type m_output = string

module Chain_id :
  METHOD with type m_input = unit and type m_output = Ethereum_types.quantity

module Accounts :
  METHOD
    with type m_input = unit
     and type m_output = Ethereum_types.address list

module Get_balance :
  METHOD
    with type m_input = Ethereum_types.address * Ethereum_types.block_param
     and type m_output = Ethereum_types.quantity

module Block_number :
  METHOD
    with type m_input = unit
     and type m_output = Ethereum_types.block_height

module Get_block_by_number :
  METHOD
    with type m_input = Ethereum_types.block_param * bool
     and type m_output = Ethereum_types.block

module Get_block_by_hash :
  METHOD
    with type m_input = Ethereum_types.hash * bool
     and type m_output = Ethereum_types.block

module Get_code :
  METHOD
    with type m_input = Ethereum_types.address * Ethereum_types.block_param
     and type m_output = Ethereum_types.hash

module Gas_price :
  METHOD with type m_input = unit and type m_output = Ethereum_types.quantity

module Get_transaction_count :
  METHOD
    with type m_input = Ethereum_types.address * Ethereum_types.block_param
     and type m_output = Ethereum_types.quantity

module Get_transaction_receipt :
  METHOD
    with type m_input = Ethereum_types.hash
     and type m_output = Ethereum_types.transaction_receipt option

module Get_transaction_by_hash :
  METHOD
    with type m_input = Ethereum_types.hash
     and type m_output = Ethereum_types.transaction_object option

module Send_raw_transaction :
  METHOD
    with type m_input = Ethereum_types.hash
     and type m_output = Ethereum_types.hash

module Send_transaction :
  METHOD
    with type m_input = Ethereum_types.transaction
     and type m_output = Ethereum_types.hash

module Eth_call :
  METHOD
    with type m_input = Ethereum_types.call * Ethereum_types.block_param
     and type m_output = Ethereum_types.hash

module Get_estimate_gas :
  METHOD
    with type m_input = Ethereum_types.call
     and type m_output = Ethereum_types.quantity
