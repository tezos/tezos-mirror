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

  let random_id =
    let state = Random.get_state () in
    fun ?seed () ->
      let seed = Option.value seed ~default:state in
      let uuid = Uuidm.v4_gen seed () in
      Id_string Uuidm.(to_string ~upper:false uuid)

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
           (dft "id" (option id_repr_encoding) None)))

  type error = {code : int; message : string; data : Data_encoding.json option}

  let error_encoding =
    Data_encoding.(
      conv
        (fun {code; message; data} -> (code, message, data))
        (fun (code, message, data) -> {code; message; data})
        (obj3
           (req "code" int31)
           (req "message" string)
           (opt "data" Data_encoding.json)))

  type value = (Data_encoding.json, error) result

  type return_value = Direct of value | Lazy of value Lwt.t

  type response = {value : value; id : id}

  type return_response = {return_value : return_value; id : id}

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
           (opt "error" error_encoding)
           (req "id" (option id_repr_encoding))))
end

module Subscription = struct
  let version = JSONRPC.version

  let method_ = "eth_subscription"

  type result = {
    result : Data_encoding.json;
    subscription : Ethereum_types.Subscription.id;
  }

  let result_encoding =
    Data_encoding.(
      conv
        (fun {result; subscription} -> (result, subscription))
        (fun (result, subscription) -> {result; subscription})
        (obj2
           (req "result" Data_encoding.json)
           (req "subscription" Ethereum_types.Subscription.id_encoding)))

  type notification = {params : result}

  let notification_encoding =
    Data_encoding.(
      conv
        (fun {params} -> ((), (), params))
        (fun ((), (), params) -> {params})
        (obj3
           (req "jsonrpc" (constant version))
           (req "method" (constant method_))
           (req "params" result_encoding)))
end

type ('input, 'output) method_ = ..

module type METHOD = sig
  val method_ : string

  type input

  type output

  val input_encoding : input Data_encoding.t

  val output_encoding : output Data_encoding.t

  type ('input, 'output) method_ += Method : (input, output) method_
end

(* A simple equality check for two (module METHOD) values by comparing the method_ field. *)
let equal_method (module M1 : METHOD) (module M2 : METHOD) =
  M1.method_ = M2.method_

let get_diff (list1 : (module METHOD) list) (list2 : (module METHOD) list) :
    (module METHOD) list =
  List.filter
    (fun m1 -> not (List.exists (fun m2 -> equal_method m1 m2) list2))
    list1

let encoding_with_optional_extended_block_param encoding =
  Evm_node_lib_dev_encoding.Helpers.encoding_with_optional_last_param
    encoding
    Ethereum_types.Block_parameter.extended_encoding
    Ethereum_types.Block_parameter.(Block_parameter Latest)

module Kernel_version = struct
  type input = unit

  type output = string

  let input_encoding = Data_encoding.unit

  let output_encoding = Data_encoding.string

  let method_ = "tez_kernelVersion"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Kernel_root_hash = struct
  type input = unit

  type output = string option

  let input_encoding = Data_encoding.unit

  let output_encoding = Data_encoding.(option (string' Hex))

  let method_ = "tez_kernelRootHash"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Sequencer = struct
  open Ethereum_types

  type input = Block_parameter.extended

  type output = Signature.Public_key.t

  let input_encoding = Block_parameter.extended_encoding

  let output_encoding = Signature.Public_key.encoding

  let method_ = "tez_sequencer"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Generic_block_number = struct
  open Ethereum_types

  type input = unit

  type output = quantity

  let input_encoding = Data_encoding.unit

  let output_encoding = quantity_encoding

  let method_ = "tez_blockNumber"

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

  type output = L2_types.chain_id

  let input_encoding = Data_encoding.unit

  let output_encoding = L2_types.Chain_id.encoding

  let method_ = "eth_chainId"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Chain_family = struct
  type input = L2_types.chain_id

  type output = L2_types.ex_chain_family

  let input_encoding = Data_encoding.tup1 L2_types.Chain_id.encoding

  let output_encoding = L2_types.Chain_family.encoding

  let method_ = "tez_chainFamily"

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

  type input = address * Block_parameter.extended

  type output = quantity

  let input_encoding =
    encoding_with_optional_extended_block_param address_encoding

  let output_encoding = quantity_encoding

  let method_ = "eth_getBalance"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Get_storage_at = struct
  open Ethereum_types

  type input = address * quantity * Block_parameter.extended

  type output = hex

  let input_encoding : input Data_encoding.t =
    let open Data_encoding in
    conv
      (fun (address, quantity, block_param) ->
        ((address, quantity), block_param))
      (fun ((address, quantity), block_param) ->
        (address, quantity, block_param))
      (encoding_with_optional_extended_block_param
         (tup2 address_encoding quantity_encoding))

  let output_encoding = hex_encoding

  let method_ = "eth_getStorageAt"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Block_number = struct
  open Ethereum_types

  type input = unit

  type output = quantity

  let input_encoding = Data_encoding.unit

  let output_encoding = quantity_encoding

  let method_ = "eth_blockNumber"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Get_block_by_number = struct
  open Ethereum_types

  type input = Block_parameter.t * bool

  type output = Transaction_object.t block

  let input_encoding =
    Data_encoding.tup2 Block_parameter.encoding Data_encoding.bool

  let output_encoding = block_encoding Transaction_object.encoding

  let method_ = "eth_getBlockByNumber"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Get_block_by_hash = struct
  open Ethereum_types

  type input = block_hash * bool

  type output = Transaction_object.t block

  let input_encoding = Data_encoding.tup2 block_hash_encoding Data_encoding.bool

  let output_encoding = block_encoding Transaction_object.encoding

  let method_ = "eth_getBlockByHash"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Get_block_receipts = struct
  open Ethereum_types

  type input = Block_parameter.t

  type output = Transaction_receipt.t list

  let input_encoding = Data_encoding.tup1 Block_parameter.encoding

  let output_encoding = Data_encoding.list Transaction_receipt.encoding

  let method_ = "eth_getBlockReceipts"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Get_code = struct
  open Ethereum_types

  type input = address * Block_parameter.extended

  type output = hex

  let input_encoding =
    encoding_with_optional_extended_block_param address_encoding

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

  type input = address * Block_parameter.extended

  type output = quantity

  let input_encoding =
    encoding_with_optional_extended_block_param address_encoding

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

  type input = Block_parameter.t

  type output = quantity

  let input_encoding = Data_encoding.tup1 Block_parameter.encoding

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

  type input = Block_parameter.t

  type output = quantity

  let input_encoding = Data_encoding.tup1 Block_parameter.encoding

  let output_encoding = quantity_encoding

  let method_ = "eth_getUncleCountByBlockNumber"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Get_transaction_receipt = struct
  open Ethereum_types

  type input = hash

  type output = Transaction_receipt.t option

  let input_encoding = Data_encoding.tup1 hash_encoding

  let output_encoding = Data_encoding.option Transaction_receipt.encoding

  let method_ = "eth_getTransactionReceipt"

  type ('input, 'output) method_ += Method : (input, output) method_
end

type gas_info = {
  execution_gas : Ethereum_types.quantity;
  inclusion_gas : Ethereum_types.quantity;
}

let gas_info_encoding =
  Data_encoding.(
    conv
      (fun {execution_gas; inclusion_gas} -> (execution_gas, inclusion_gas))
      (fun (execution_gas, inclusion_gas) -> {execution_gas; inclusion_gas})
    @@ obj2
         (req "execution_gas" Ethereum_types.quantity_encoding)
         (req "inclusion_gas" Ethereum_types.quantity_encoding))

module Get_transaction_gas_info = struct
  open Ethereum_types

  type input = hash

  type output = gas_info option

  let input_encoding = Data_encoding.tup1 hash_encoding

  let output_encoding = Data_encoding.option gas_info_encoding

  let method_ = "tez_getTransactionGasInfo"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Get_transaction_by_hash = struct
  open Ethereum_types

  type input = hash

  type output = Transaction_object.t option

  let input_encoding = Data_encoding.tup1 hash_encoding

  let output_encoding = Data_encoding.option Transaction_object.encoding

  let method_ = "eth_getTransactionByHash"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Get_transaction_by_block_hash_and_index = struct
  open Ethereum_types

  type input = block_hash * quantity

  type output = Transaction_object.t option

  let input_encoding = Data_encoding.tup2 block_hash_encoding quantity_encoding

  let output_encoding = Data_encoding.option Transaction_object.encoding

  let method_ = "eth_getTransactionByBlockHashAndIndex"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Get_transaction_by_block_number_and_index = struct
  open Ethereum_types

  type input = Block_parameter.t * quantity

  type output = Transaction_object.t option

  let input_encoding =
    Data_encoding.tup2 Block_parameter.encoding quantity_encoding

  let output_encoding = Data_encoding.option Transaction_object.encoding

  let method_ = "eth_getTransactionByBlockNumberAndIndex"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Get_uncle_by_block_hash_and_index = struct
  open Ethereum_types

  type input = block_hash * quantity

  type output = Transaction_object.t block option

  let input_encoding = Data_encoding.tup2 block_hash_encoding quantity_encoding

  let output_encoding =
    Data_encoding.option (block_encoding Transaction_object.encoding)

  let method_ = "eth_getUncleByBlockHashAndIndex"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Get_uncle_by_block_number_and_index = struct
  open Ethereum_types

  type input = Block_parameter.t * quantity

  type output = Transaction_object.t block option

  let input_encoding =
    Data_encoding.tup2 Block_parameter.encoding quantity_encoding

  let output_encoding =
    Data_encoding.option (block_encoding Transaction_object.encoding)

  let method_ = "eth_getUncleByBlockNumberAndIndex"

  type ('input, 'output) method_ += Method : (input, output) method_
end

(* Shared types and encodings between eth_sendRawTransaction and tez_sendRawTezlinkOperation and 
   all future RPCs used to relay transactions when the Tx_queue gets a Tick. *)
module Send_raw_common = struct
  open Ethereum_types

  type input = hex

  type output = hash

  let input_encoding = Data_encoding.tup1 hex_encoding

  let output_encoding = hash_encoding
end

module Send_raw_transaction = struct
  include Send_raw_common

  let method_ = "eth_sendRawTransaction"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Send_raw_tezlink_operation = struct
  include Send_raw_common

  let method_ = "tez_sendRawTezlinkOperation"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Send_raw_transaction_sync = struct
  open Ethereum_types

  type input = hex * int64 * Block_parameter.t

  type output = Transaction_receipt.t

  let default_timeout = 0L

  let default_block_param = Block_parameter.Latest

  let input_encoding =
    let open Data_encoding in
    union
      [
        case
          ~title:"Only raw transaction"
          (Tag 0)
          (tup1 hex_encoding)
          (fun (_, _, _) -> None)
          (fun c -> (c, default_timeout, default_block_param));
        case
          ~title:"Raw transaction and timeout"
          (Tag 1)
          (tup2 hex_encoding Data_encoding.int64)
          (fun (_, _, _) -> None)
          (fun (c, b) -> (c, b, default_block_param));
        case
          ~title:"Raw transaction and block parameter"
          (Tag 2)
          (tup2 hex_encoding Block_parameter.encoding)
          (fun (_, _, _) -> None)
          (fun (c, s) -> (c, default_timeout, s));
        case
          ~title:"Raw transaction, timeout and block parameter"
          (Tag 3)
          (tup3 hex_encoding Data_encoding.int64 Block_parameter.encoding)
          (fun (c, b, s) -> Some (c, b, s))
          (fun (c, b, s) -> (c, b, s));
      ]

  let output_encoding = Transaction_receipt.encoding

  let method_ = "eth_sendRawTransactionSync"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Eth_call = struct
  open Ethereum_types

  type input = call * Block_parameter.extended * state_override

  type output = hash

  let default_block = Ethereum_types.Block_parameter.(Block_parameter Latest)

  let default_state_override = AddressMap.empty

  let input_encoding =
    let open Data_encoding in
    union
      [
        case
          ~title:"Only call"
          (Tag 0)
          (tup1 call_encoding)
          (fun (c, _, _) -> Some c)
          (fun c -> (c, default_block, default_state_override));
        case
          ~title:"Call and block param"
          (Tag 1)
          (tup2 call_encoding Block_parameter.extended_encoding)
          (fun (c, b, _) -> Some (c, b))
          (fun (c, b) -> (c, b, default_state_override));
        case
          ~title:"Call and state override"
          (Tag 2)
          (tup2 call_encoding state_override_encoding)
          (fun (c, _, s) -> Some (c, s))
          (fun (c, s) -> (c, default_block, s));
        case
          ~title:"Call, block param and state override"
          (Tag 3)
          (tup3
             call_encoding
             Block_parameter.extended_encoding
             state_override_encoding)
          (fun (c, b, s) -> Some (c, b, s))
          (fun (c, b, s) -> (c, b, s));
      ]

  let output_encoding = hash_encoding

  let method_ = "eth_call"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Get_estimate_gas = struct
  open Ethereum_types

  type input = Eth_call.input

  type output = quantity

  let input_encoding = Eth_call.input_encoding

  let output_encoding = quantity_encoding

  let method_ = "eth_estimateGas"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Txpool_content = struct
  type input = unit

  type output = Transaction_object.txqueue_content

  let input_encoding = Data_encoding.unit

  let output_encoding = Transaction_object.txqueue_content_encoding

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
  type input = Ethereum_types.Filter.t

  type output = Ethereum_types.transaction_log Ethereum_types.pre_encoded list

  let input_encoding = Data_encoding.tup1 Ethereum_types.Filter.encoding

  let output_encoding =
    Data_encoding.list
      (Ethereum_types.pre_encoded_encoding
         Ethereum_types.transaction_log_encoding)

  let method_ = "eth_getLogs"

  type ('input, 'output) method_ += Method : (input, output) method_
end

type produce_block_input = {
  timestamp : Time.Protocol.t option;
  with_delayed_transactions : bool;
}

module Produce_block = struct
  type input = produce_block_input

  type output = Ethereum_types.quantity

  let input_encoding =
    let open Data_encoding in
    conv
      (fun {timestamp; with_delayed_transactions} ->
        (timestamp, with_delayed_transactions))
      (fun (timestamp, with_delayed_transactions) ->
        {timestamp; with_delayed_transactions})
      (obj2
         (opt "timestamp" Time.Protocol.encoding)
         (dft "with_delayed_transactions" bool true))

  let output_encoding = Ethereum_types.quantity_encoding

  let method_ = "produceBlock"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Propose_next_block_timestamp = struct
  type input = Time.Protocol.t

  type output = unit

  let input_encoding = Time.Protocol.encoding

  let output_encoding = Data_encoding.unit

  let method_ = "proposeNextBlockTimestamp"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Produce_proposal = struct
  type input = Time.Protocol.t

  type output = unit

  let input_encoding = Time.Protocol.encoding

  let output_encoding = Data_encoding.unit

  let method_ = "produceProposal"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Execute_single_transaction = struct
  (** Takes a raw transaction hex string. The server parses it and
      executes it via the IC single transaction execution path. *)
  open Ethereum_types

  type input = hex

  type output = unit

  let input_encoding = Data_encoding.tup1 hex_encoding

  let output_encoding = Data_encoding.unit

  let method_ = "executeSingleTransaction"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Inject_transaction = struct
  open Ethereum_types

  type input = Transaction_object.t * string * bool

  type output = hash

  let input_encoding =
    Data_encoding.(tup3 Transaction_object.encoding string bool)

  let output_encoding = hash_encoding

  let method_ = "injectTransaction"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Wait_transaction_confirmation = struct
  type input = Ethereum_types.hash

  type output = unit

  let input_encoding = Ethereum_types.hash_encoding

  let output_encoding = Data_encoding.unit

  let method_ = "waitTransactionConfirmation"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Inject_tezlink_operation = struct
  type input = Tezos_types.Operation.t * bytes

  type output = Ethereum_types.hash

  let input_encoding = Data_encoding.(tup2 Tezos_types.Operation.encoding bytes)

  let output_encoding = Ethereum_types.hash_encoding

  let method_ = "injectTezlinkOperation"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Durable_state_value = struct
  type input =
    Durable_storage_path.path * Ethereum_types.Block_parameter.extended

  type output = Bytes.t option

  let input_encoding =
    Helpers.encoding_with_optional_last_param
      Data_encoding.string
      Ethereum_types.Block_parameter.extended_encoding
      Ethereum_types.Block_parameter.(Block_parameter Latest)

  let output_encoding = Data_encoding.(option bytes)

  let method_ = "stateValue"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Durable_state_subkeys = struct
  type input =
    Durable_storage_path.path * Ethereum_types.Block_parameter.extended

  type output = string list

  let input_encoding =
    Helpers.encoding_with_optional_last_param
      Data_encoding.string
      Ethereum_types.Block_parameter.extended_encoding
      Ethereum_types.Block_parameter.(Block_parameter Latest)

  let output_encoding = Data_encoding.(list string)

  let method_ = "stateSubkeys"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Eth_max_priority_fee_per_gas = struct
  open Ethereum_types

  type input = unit

  type output = quantity

  let input_encoding = Data_encoding.unit

  let output_encoding = quantity_encoding

  let method_ = "eth_maxPriorityFeePerGas"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Replay_block = struct
  open Ethereum_types

  type input = Ethereum_types.quantity

  type output =
    (* Replay block is a debugging RPC, not used in production. We could
       migrate it to use [Transaction_object.t] instead of
       [legacy_transaction_object], but simply showing what the kernel is
       producing is not harmful. *)
    legacy_transaction_object block

  let input_encoding = quantity_encoding

  let output_encoding = block_encoding legacy_transaction_object_encoding

  let method_ = "tez_replayBlock"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Lock_block_production = struct
  type input = unit

  type output = unit

  let input_encoding = Data_encoding.unit

  let output_encoding = Data_encoding.unit

  let method_ = "lockBlockProduction"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Unlock_block_production = struct
  type input = unit

  type output = unit

  let input_encoding = Data_encoding.unit

  let output_encoding = Data_encoding.unit

  let method_ = "unlockBlockProduction"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Trace_transaction = struct
  type input = Tracer_types.input

  type output = Tracer_types.output

  let input_encoding = Tracer_types.input_encoding

  let output_encoding = Tracer_types.output_encoding

  let method_ = "debug_traceTransaction"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Trace_call = struct
  type input = Tracer_types.call_input

  type output = Tracer_types.output

  let input_encoding = Tracer_types.call_input_encoding

  let output_encoding = Tracer_types.output_encoding

  let method_ = "debug_traceCall"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Trace_block = struct
  type input = Tracer_types.block_input

  type output = Tracer_types.block_output

  let input_encoding = Tracer_types.block_input_encoding

  let output_encoding = Tracer_types.block_output_encoding

  let method_ = "debug_traceBlockByNumber"

  type ('input, 'output) method_ += Method : (input, output) method_
end

type l1_block_l2_levels = {
  start_l2_level : Ethereum_types.quantity;
  end_l2_level : Ethereum_types.quantity;
}

module Get_finalized_blocks_of_l1_level = struct
  type input = int32

  type output = l1_block_l2_levels

  let input_encoding = Data_encoding.int32

  let output_encoding =
    let open Data_encoding in
    conv
      (fun {start_l2_level; end_l2_level} -> (start_l2_level, end_l2_level))
      (fun (start_l2_level, end_l2_level) -> {start_l2_level; end_l2_level})
    @@ obj2
         (req
            "startBlockNumber"
            ~description:
              "Level of finalized L2 block before application of the L1 block"
            Ethereum_types.quantity_encoding)
         (req
            "endBlockkNumber"
            ~description:
              "Level of finalized L2 block after application of the L1 block"
            Ethereum_types.quantity_encoding)

  let method_ = "tez_getFinalizedBlocksOfL1Level"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Eth_fee_history = struct
  open Ethereum_types

  type input = quantity * Block_parameter.t * float list

  type output = Fee_history.t

  let input_encoding =
    Data_encoding.tup3
      quantity_encoding
      Block_parameter.encoding
      (Data_encoding.list Data_encoding.float)

  let output_encoding = Fee_history.encoding

  let method_ = "eth_feeHistory"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Coinbase = struct
  type input = unit

  type output = Ethereum_types.address

  let input_encoding = Data_encoding.unit

  let output_encoding = Ethereum_types.address_encoding

  let method_ = "eth_coinbase"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Subscribe = struct
  open Ethereum_types

  type input = Subscription.kind

  type output = Subscription.id

  let input_encoding = Subscription.kind_encoding

  let output_encoding = Subscription.id_encoding

  let method_ = "eth_subscribe"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Unsubscribe = struct
  open Ethereum_types

  type input = Subscription.id

  type output = bool

  let input_encoding = Subscription.id_input_encoding

  let output_encoding = Data_encoding.bool

  let method_ = "eth_unsubscribe"

  type ('input, 'output) method_ += Method : (input, output) method_
end

module Tezosx = struct
  module Get_tezos_ethereum_address = struct
    open Ethereum_types

    type input = Signature.V2.public_key_hash

    type output = address

    let input_encoding = Signature.V2.Public_key_hash.encoding

    let output_encoding = address_encoding

    let method_ = "tez_getTezosEthereumAddress"

    type ('input, 'output) method_ += Method : (input, output) method_
  end

  module Get_ethereum_tezos_address = struct
    open Ethereum_types

    type input = address

    type output = Tezos_types.Contract.t

    let input_encoding = address_encoding

    let output_encoding = Tezos_types.Contract.encoding

    let method_ = "tez_getEthereumTezosAddress"

    type ('input, 'output) method_ += Method : (input, output) method_
  end
end

type map_result =
  | Method :
      ('input, 'output) method_
      * (module METHOD with type input = 'input and type output = 'output)
      -> map_result
  | Unsupported
  | Unknown
  | Disabled

let evm_supported_methods : (module METHOD) list =
  [
    (* Generic rpcs *)
    (module Generic_block_number);
    (module Kernel_version);
    (module Kernel_root_hash);
    (module Sequencer);
    (module Network_id);
    (module Chain_id);
    (module Chain_family);
    (module Durable_state_value);
    (module Durable_state_subkeys);
    (module Get_finalized_blocks_of_l1_level);
    (* Etherlink rpcs *)
    (module Accounts);
    (module Get_balance);
    (module Get_storage_at);
    (module Block_number);
    (module Get_block_by_number);
    (module Get_block_by_hash);
    (module Get_block_receipts);
    (module Get_code);
    (module Gas_price);
    (module Get_transaction_count);
    (module Get_block_transaction_count_by_hash);
    (module Get_block_transaction_count_by_number);
    (module Get_logs);
    (module Get_uncle_count_by_block_hash);
    (module Get_uncle_count_by_block_number);
    (module Get_transaction_gas_info);
    (module Get_transaction_receipt);
    (module Get_transaction_by_hash);
    (module Get_transaction_by_block_hash_and_index);
    (module Get_transaction_by_block_number_and_index);
    (module Get_uncle_by_block_hash_and_index);
    (module Get_uncle_by_block_number_and_index);
    (module Send_raw_transaction);
    (module Send_raw_transaction_sync);
    (module Eth_call);
    (module Get_estimate_gas);
    (module Txpool_content);
    (module Web3_clientVersion);
    (module Web3_sha3);
    (module Produce_block);
    (module Propose_next_block_timestamp);
    (module Produce_proposal);
    (module Execute_single_transaction);
    (module Inject_transaction);
    (module Wait_transaction_confirmation);
    (module Eth_max_priority_fee_per_gas);
    (module Replay_block);
    (module Lock_block_production);
    (module Unlock_block_production);
    (module Trace_transaction);
    (module Eth_fee_history);
    (module Coinbase);
    (module Trace_call);
    (module Subscribe);
    (module Unsubscribe);
    (module Trace_block);
    (* Tezos X rpcs *)
    (module Tezosx.Get_tezos_ethereum_address);
    (module Tezosx.Get_ethereum_tezos_address);
  ]

let evm_unsupported_methods : string list =
  [
    (* net *)
    "net_listening";
    "net_peerCount";
    (* eth *)
    "eth_protocolVersion";
    "eth_syncing";
    "eth_mining";
    "eth_hashrate";
    "eth_sign";
    "eth_signTransaction";
    "eth_blobBaseFee";
    "eth_getProof";
    "eth_createAccessList";
    "eth_getFilterChanges";
    "eth_getFilterLogs";
    "eth_newBlockFilter";
    "eth_newFilter";
    "eth_newPendingTransactionFilter";
    "eth_uninstallFilter";
    "eth_sendTransaction";
    "eth_subscribe";
    "eth_unsubscribe";
    (* debug *)
    "debug_getBadBlocks";
    "debug_getRawBlock";
    "debug_getRawHeader";
    "debug_getRawReceipts";
    "debug_getRawTransaction";
    (* engine *)
    "engine_exchangeCapabilities";
    "engine_exchangeTransitionConfigurationV1";
    "engine_forkchoiceUpdatedV1";
    "engine_forkchoiceUpdatedV2";
    "engine_forkchoiceUpdatedV3";
    "engine_getPayloadBodiesByHashV1";
    "engine_getPayloadBodiesByRangeV1";
    "engine_getPayloadV1";
    "engine_getPayloadV2";
    "engine_getPayloadV3";
    "engine_getPayloadV4";
    "engine_newPayloadV1";
    "engine_newPayloadV2";
    "engine_newPayloadV3";
    "engine_newPayloadV4";
  ]

let michelson_supported_methods =
  evm_supported_methods @ [(module Send_raw_tezlink_operation)]

let multichain_sequencer_supported_methods : (module METHOD) list =
  [
    (module Generic_block_number);
    (module Send_raw_transaction);
    (module Send_raw_tezlink_operation);
    (module Send_raw_transaction_sync);
    (* Private RPCs *)
    (module Produce_block);
    (module Propose_next_block_timestamp);
    (module Inject_transaction);
    (module Wait_transaction_confirmation);
    (module Inject_tezlink_operation);
    (module Durable_state_value);
    (module Durable_state_subkeys);
    (module Replay_block);
    (module Lock_block_production);
    (module Unlock_block_production);
  ]

let michelson_unsupported_methods = evm_unsupported_methods

let multichain_sequencer_unsupported_methods =
  let diff_methods =
    get_diff evm_supported_methods multichain_sequencer_supported_methods
  in
  let diff_method_names =
    List.map (fun (module M : METHOD) -> M.method_) diff_methods
  in
  evm_unsupported_methods @ diff_method_names

let map_method_name (type f)
    ~(rpc_server_family : f Rpc_types.rpc_server_family) ~restrict method_name =
  let supported_methods, unsupported_methods =
    match rpc_server_family with
    | Rpc_types.Multichain_sequencer_rpc_server ->
        ( multichain_sequencer_supported_methods,
          multichain_sequencer_unsupported_methods )
    | Rpc_types.Single_chain_node_rpc_server EVM ->
        (evm_supported_methods, evm_unsupported_methods)
    | Rpc_types.Single_chain_node_rpc_server Michelson ->
        (michelson_supported_methods, michelson_unsupported_methods)
  in
  let disabled =
    match restrict with
    | Configuration.Pattern {regex; _} -> Re.execp regex method_name
    | Whitelist l -> not (List.mem ~equal:String.equal method_name l)
    | Blacklist l -> List.mem ~equal:String.equal method_name l
    | Unrestricted -> false
  in

  if disabled then Disabled
  else
    match
      List.find
        (fun (module M : METHOD) -> M.method_ = method_name)
        supported_methods
    with
    | Some (module M) -> Method (M.Method, (module M))
    | None ->
        if List.mem ~equal:( = ) method_name unsupported_methods then
          Unsupported
        else Unknown

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
