(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type inconsistent_traces_input = {
  block : Ethereum_types.quantity;
  nb_txs : int;
  nb_traces : int;
}

type error +=
  | Not_supported
  | Transaction_not_found of Ethereum_types.hash
  | Block_not_found of Ethereum_types.quantity
  | Trace_not_found
  | Tracer_not_activated  (** Tracer only activated after a certain level *)
  | Tracer_not_implemented of string
        (** Not all tracer are available for all rpcs *)
  | Inconsistent_traces of inconsistent_traces_input

type tracer_config = {
  enable_return_data : bool;
  enable_memory : bool;
  disable_stack : bool;
  disable_storage : bool;
  with_logs : bool;
  only_top_call : bool;
}

val default_tracer_config : tracer_config

val tracer_config_encoding : tracer_config Data_encoding.t

type tracer_kind = StructLogger | CallTracer

val tracer_kind_encoding : tracer_kind Data_encoding.t

type config = {
  tracer : tracer_kind;
  tracer_config : tracer_config;
  timeout : Time.System.Span.t;
  reexec : int64;
}

val tracer_version_activation : tracer_kind -> int

val default_config : config

val config_encoding : config Data_encoding.t

val config_to_string : config -> string

type input = Ethereum_types.hash * config

type call_input =
  (Ethereum_types.call * Ethereum_types.Block_parameter.extended) * config

type block_input = Ethereum_types.Block_parameter.t * config

val input_encoding : (Ethereum_types.hash * config) Data_encoding.t

val call_input_encoding :
  ((Ethereum_types.call * Ethereum_types.Block_parameter.extended) * config)
  Data_encoding.t

val block_input_encoding : block_input Data_encoding.t

val input_rlp_encoder : ?hash:Ethereum_types.hash -> config -> string

module Opcode : sig
  type t = Char.t

  val opcode_to_string : t -> string

  val encoding : t Data_encoding.t
end

type uint53 = Z.t

val uint53_encoding : uint53 Data_encoding.t

module StructLogger : sig
  type opcode_log = {
    pc : uint53;
    op : Opcode.t;
    gas : uint53;
    gas_cost : uint53;
    memory : Ethereum_types.hex list option;
    mem_size : int32 option;
    stack : Ethereum_types.hex list option;
    return_data : Ethereum_types.hex option;
    storage : (Ethereum_types.hex * Ethereum_types.hex) list option;
    depth : uint53;
    refund : uint53;
    error : string option;
  }

  type output = {
    gas : int64;
    failed : bool;
    return_value : Ethereum_types.hash;
    struct_logs : opcode_log list;
  }

  val output_encoding : output Data_encoding.t

  val output_binary_decoder :
    gas:bytes ->
    failed:bytes ->
    return_value:bytes ->
    struct_logs:bytes list ->
    output tzresult
end

module CallTracer : sig
  type logs = {
    address : Ethereum_types.address;
    topics : Ethereum_types.hex list;
    data : Ethereum_types.hex;
  }

  type output = {
    calls : output list;
    type_ : string;
    from : Ethereum_types.address;
    to_ : Ethereum_types.address option;
    value : uint53;
    gas : uint53 option;
    gas_used : uint53;
    input : Ethereum_types.hex;
    output : Ethereum_types.hex option;
    error : string option;
    revert_reason : string option;
    logs : logs list option;
  }

  val logs_encoding : logs Data_encoding.t

  val output_encoding : output Data_encoding.t

  (** Expects a RLP representation of an output, with an additionnal field
      containing the depth of the call.*)
  val decode_call : bytes -> (output * int) tzresult

  (** [to_string output] contains a Json representation of [output]. Based on
      [Data_encoding.Json] so expect exceptions. *)
  val to_string : output -> string
end

type output =
  | StructLoggerOutput of StructLogger.output
  | CallTracerOutput of CallTracer.output

val output_encoding : output Data_encoding.t

type block_output = (Ethereum_types.hash * output) list

val block_output_encoding : block_output Data_encoding.t
