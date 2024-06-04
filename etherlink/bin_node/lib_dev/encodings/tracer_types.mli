(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type error +=
  | Not_supported
  | Transaction_not_found of Ethereum_types.hash
  | Block_not_found of Ethereum_types.quantity
  | Trace_not_found

type tracer_config = {
  enable_return_data : bool;
  enable_memory : bool;
  disable_stack : bool;
  disable_storage : bool;
}

val default_tracer_config : tracer_config

val tracer_config_encoding : tracer_config Data_encoding.t

type tracer_kind = StructLogger

val tracer_kind_encoding : tracer_kind Data_encoding.t

type config = {
  tracer : tracer_kind;
  tracer_config : tracer_config;
  timeout : Time.System.Span.t;
  reexec : int64;
}

val default_config : config

val config_encoding : config Data_encoding.t

type input = Ethereum_types.hash * config

val input_encoding : (Ethereum_types.hash * config) Data_encoding.t

val input_rlp_encoder : Ethereum_types.hash -> config -> string

module Opcode : sig
  type t = Char.t

  val opcode_to_string : t -> string

  val encoding : t Data_encoding.t
end

type uint53 = Z.t

val uint53_encoding : uint53 Data_encoding.t

type opcode_log = {
  pc : uint53;
  op : Opcode.t;
  gas : uint53;
  gas_cost : uint53;
  memory : Hex.t list option;
  mem_size : int32 option;
  stack : Hex.t list option;
  return_data : Hex.t option;
  storage : (Hex.t * Hex.t) list option;
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