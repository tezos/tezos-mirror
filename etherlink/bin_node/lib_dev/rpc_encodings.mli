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

  val random_id : ?seed:Random.State.t -> unit -> id_repr

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
  type error = {code : int; message : string; data : Data_encoding.json option}

  val error_encoding : error Data_encoding.t

  type value = (Data_encoding.json, error) result

  type return_value = Direct of value | Lazy of value Lwt.t

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

  type return_response = {return_value : return_value; id : id}

  val response_encoding : response Data_encoding.t
end

(* This isn't the JSON-RPC request that is sent to request websocket events but
   the generic outputed data that is sent through the websocket periodically once
   the appropriate websocket request was sent (see module [Subscribe]). *)
module Subscription : sig
  val version : string

  val method_ : string

  type result = {
    result : Data_encoding.json;
    subscription : Ethereum_types.Subscription.id;
  }

  val result_encoding : result Data_encoding.t

  type notification = {params : result}

  val notification_encoding : notification Data_encoding.t
end

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

module Sequencer :
  METHOD
    with type input = Ethereum_types.Block_parameter.extended
     and type output = Signature.Public_key.t

module Kernel_version : METHOD with type input = unit and type output = string

module Kernel_root_hash :
  METHOD with type input = unit and type output = string option

module Network_id : METHOD with type input = unit and type output = string

module Chain_id :
  METHOD with type input = unit and type output = L2_types.chain_id

module Chain_family :
  METHOD
    with type input = L2_types.chain_id
     and type output = L2_types.ex_chain_family

module Accounts :
  METHOD with type input = unit and type output = Ethereum_types.address list

module Get_balance :
  METHOD
    with type input =
      Ethereum_types.address * Ethereum_types.Block_parameter.extended
     and type output = Ethereum_types.quantity

module Get_storage_at :
  METHOD
    with type input =
      Ethereum_types.address
      * Ethereum_types.quantity
      * Ethereum_types.Block_parameter.extended
     and type output = Ethereum_types.hex

module Generic_block_number :
  METHOD with type input = unit and type output = Ethereum_types.quantity

module Block_number :
  METHOD with type input = unit and type output = Ethereum_types.quantity

module Get_block_by_number :
  METHOD
    with type input = Ethereum_types.Block_parameter.t * bool
     and type output = Transaction_object.t Ethereum_types.block

module Get_block_by_hash :
  METHOD
    with type input = Ethereum_types.block_hash * bool
     and type output = Transaction_object.t Ethereum_types.block

module Get_block_receipts :
  METHOD
    with type input = Ethereum_types.Block_parameter.t
     and type output = Transaction_receipt.t list

module Get_code :
  METHOD
    with type input =
      Ethereum_types.address * Ethereum_types.Block_parameter.extended
     and type output = Ethereum_types.hex

module Gas_price :
  METHOD with type input = unit and type output = Ethereum_types.quantity

module Get_transaction_count :
  METHOD
    with type input =
      Ethereum_types.address * Ethereum_types.Block_parameter.extended
     and type output = Ethereum_types.quantity

module Get_block_transaction_count_by_hash :
  METHOD
    with type input = Ethereum_types.block_hash
     and type output = Ethereum_types.quantity

module Get_block_transaction_count_by_number :
  METHOD
    with type input = Ethereum_types.Block_parameter.t
     and type output = Ethereum_types.quantity

module Get_uncle_count_by_block_hash :
  METHOD
    with type input = Ethereum_types.block_hash
     and type output = Ethereum_types.quantity

module Get_uncle_count_by_block_number :
  METHOD
    with type input = Ethereum_types.Block_parameter.t
     and type output = Ethereum_types.quantity

module Get_transaction_receipt :
  METHOD
    with type input = Ethereum_types.hash
     and type output = Transaction_receipt.t option

type gas_info = {
  execution_gas : Ethereum_types.quantity;
  inclusion_gas : Ethereum_types.quantity;
}

module Get_transaction_gas_info :
  METHOD with type input = Ethereum_types.hash and type output = gas_info option

module Get_transaction_by_hash :
  METHOD
    with type input = Ethereum_types.hash
     and type output = Transaction_object.t option

module Get_transaction_by_block_hash_and_index :
  METHOD
    with type input = Ethereum_types.block_hash * Ethereum_types.quantity
     and type output = Transaction_object.t option

module Get_transaction_by_block_number_and_index :
  METHOD
    with type input = Ethereum_types.Block_parameter.t * Ethereum_types.quantity
     and type output = Transaction_object.t option

module Get_uncle_by_block_hash_and_index :
  METHOD
    with type input = Ethereum_types.block_hash * Ethereum_types.quantity
     and type output = Transaction_object.t Ethereum_types.block option

module Get_uncle_by_block_number_and_index :
  METHOD
    with type input = Ethereum_types.Block_parameter.t * Ethereum_types.quantity
     and type output = Transaction_object.t Ethereum_types.block option

module Send_raw_transaction :
  METHOD
    with type input = Ethereum_types.hex
     and type output = Ethereum_types.hash

module Send_raw_tezlink_operation :
  METHOD
    with type input = Ethereum_types.hex
     and type output = Ethereum_types.hash

module Send_raw_transaction_sync :
  METHOD
    with type input =
      Ethereum_types.hex * int64 * Ethereum_types.Block_parameter.t
     and type output = Transaction_receipt.t

module Eth_call :
  METHOD
    with type input =
      Ethereum_types.call
      * Ethereum_types.Block_parameter.extended
      * Ethereum_types.state_override
     and type output = Ethereum_types.hash

module Get_estimate_gas :
  METHOD
    with type input = Eth_call.input
     and type output = Ethereum_types.quantity

module Txpool_content :
  METHOD
    with type input = unit
     and type output = Transaction_object.txqueue_content

module Web3_clientVersion :
  METHOD with type input = unit and type output = string

module Web3_sha3 :
  METHOD
    with type input = Ethereum_types.hex
     and type output = Ethereum_types.hash

module Get_logs :
  METHOD
    with type input = Ethereum_types.Filter.t
     and type output =
      Ethereum_types.transaction_log Ethereum_types.pre_encoded list

type produce_block_input = {
  timestamp : Time.Protocol.t option;
  with_delayed_transactions : bool;
}

module Produce_block :
  METHOD
    with type input = produce_block_input
     and type output = Ethereum_types.quantity

module Propose_next_block_timestamp :
  METHOD with type input = Time.Protocol.t and type output = unit

module Produce_proposal :
  METHOD with type input = Time.Protocol.t and type output = unit

module Execute_single_transaction :
  METHOD with type input = Ethereum_types.hex and type output = unit

module Inject_transaction :
  METHOD
    with type input = Transaction_object.t * string * bool
     and type output = Ethereum_types.hash

module Wait_transaction_confirmation :
  METHOD with type input = Ethereum_types.hash and type output = unit

module Inject_tezlink_operation :
  METHOD
    with type input = Tezos_types.Operation.t * bytes
     and type output = Ethereum_types.hash

module Durable_state_value :
  METHOD
    with type input =
      Durable_storage_path.path * Ethereum_types.Block_parameter.extended
     and type output = Bytes.t option

module Durable_state_subkeys :
  METHOD
    with type input =
      Durable_storage_path.path * Ethereum_types.Block_parameter.extended
     and type output = string list

module Eth_max_priority_fee_per_gas :
  METHOD with type input = unit and type output = Ethereum_types.quantity

module Replay_block :
  METHOD
    with type input = Ethereum_types.quantity
     and type output =
      Ethereum_types.legacy_transaction_object Ethereum_types.block

module Lock_block_production :
  METHOD with type input = unit and type output = unit

module Unlock_block_production :
  METHOD with type input = unit and type output = unit

module Trace_transaction :
  METHOD
    with type input = Tracer_types.input
     and type output = Tracer_types.output

module Trace_call :
  METHOD
    with type input = Tracer_types.call_input
     and type output = Tracer_types.output

module Trace_block :
  METHOD
    with type input = Tracer_types.block_input
     and type output = Tracer_types.block_output

module Eth_fee_history :
  METHOD
    with type input =
      Ethereum_types.quantity * Ethereum_types.Block_parameter.t * Float.t list
     and type output = Fee_history.t

module Coinbase :
  METHOD with type input = unit and type output = Ethereum_types.address

module Subscribe :
  METHOD
    with type input = Ethereum_types.Subscription.kind
     and type output = Ethereum_types.Subscription.id

module Unsubscribe :
  METHOD with type input = Ethereum_types.Subscription.id and type output = bool

type l1_block_l2_levels = {
  start_l2_level : Ethereum_types.quantity;
  end_l2_level : Ethereum_types.quantity;
}

module Get_finalized_blocks_of_l1_level :
  METHOD with type input = int32 and type output = l1_block_l2_levels

type map_result =
  | Method :
      ('input, 'output) method_
      * (module METHOD with type input = 'input and type output = 'output)
      -> map_result
  | Unsupported
  | Unknown
  | Disabled

val map_method_name :
  rpc_server_family:_ Rpc_types.rpc_server_family ->
  restrict:Configuration.restricted_rpcs ->
  string ->
  map_result

type websocket_subscription = {
  id : Ethereum_types.Subscription.id;
  stream : Subscription.notification Lwt_stream.t;
  stopper : unit -> bool tzresult Lwt.t;
}

type websocket_response = {
  response : JSONRPC.response;
  subscription : websocket_subscription option;
}

type websocket_handler = JSONRPC.request -> websocket_response Lwt.t

module Tezosx : sig
  module Get_tezos_ethereum_address :
    METHOD
      with type input = Signature.V2.public_key_hash
       and type output = Ethereum_types.address

  module Get_ethereum_tezos_address :
    METHOD
      with type input = Ethereum_types.address
       and type output = Tezos_types.Contract.t
end
