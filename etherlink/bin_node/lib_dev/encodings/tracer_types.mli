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

(* This is a temporary type, it should be filled in a follow up patch. *)
type opcode_log

type output = {
  gas : int64;
  failed : bool;
  return_value : Ethereum_types.hash;
  struct_logs : opcode_log list;
}

val output_encoding : output Data_encoding.t

val output_binary_decoder :
  gas:bytes -> failed:bytes -> return_value:bytes -> output
