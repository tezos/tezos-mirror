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

let default_tracer_config =
  {
    enable_return_data = false;
    enable_memory = false;
    disable_stack = false;
    disable_storage = false;
  }

let tracer_config_encoding =
  let open Data_encoding in
  conv
    (fun {enable_return_data; enable_memory; disable_stack; disable_storage} ->
      ( enable_return_data,
        enable_memory,
        disable_stack,
        disable_storage,
        false,
        5 ))
    (fun ( enable_return_data,
           enable_memory,
           disable_stack,
           disable_storage,
           _,
           _ ) ->
      {enable_return_data; enable_memory; disable_stack; disable_storage})
    (obj6
       (dft "enableReturnData" bool default_tracer_config.enable_return_data)
       (dft "enableMemory" bool default_tracer_config.enable_memory)
       (dft "disableStack" bool default_tracer_config.disable_stack)
       (dft "disableStorage" bool default_tracer_config.disable_storage)
       (dft "debug" bool false)
       (dft "limit" int31 5))

type tracer_kind = StructLogger

(* TODO: (#7212) make it return an error so that we can return an understadable
   error to the user. *)
(* Cannot be made a string_enum due to `"data_encoding.string_enum: cannot have a
   single case, use constant instead"`. *)
let tracer_kind_encoding =
  Data_encoding.(
    conv
      (fun StructLogger -> ())
      (fun () -> StructLogger)
      (constant "structLogger"))

type config = {
  tracer : tracer_kind;
  tracer_config : tracer_config;
  timeout : Time.System.Span.t;
  reexec : int64;
}

(* Default config is derived from the specification of the RPC:
   https://geth.ethereum.org/docs/interacting-with-geth/rpc/ns-debug#debugtracetransaction. *)
let default_config =
  {
    tracer = StructLogger;
    tracer_config = default_tracer_config;
    timeout = Time.System.Span.of_seconds_exn 5.;
    reexec = 128L;
  }

let config_encoding =
  let open Data_encoding in
  conv
    (fun {tracer; tracer_config; timeout; reexec} ->
      (tracer, tracer_config, timeout, reexec))
    (fun (tracer, tracer_config, timeout, reexec) ->
      {tracer; tracer_config; timeout; reexec})
    (obj4
       (dft "tracer" tracer_kind_encoding default_config.tracer)
       (dft "tracerConfig" tracer_config_encoding default_config.tracer_config)
       (dft "timeout" Time.System.Span.encoding default_config.timeout)
       (dft "reexec" int64 default_config.reexec))

type input = Ethereum_types.hash * config

let input_encoding =
  Helpers.encoding_with_optional_second_param
    Ethereum_types.hash_encoding
    config_encoding
    default_config

let input_rlp_encoder hash config =
  let open Rlp in
  (* See bool encoding for RLP: https://docs.rs/ethereum-rlp/latest/src/rlp/impls.rs.html#36-44 *)
  let bool_encoding b =
    if b then Value (Bytes.make 1 '\001') else Value Bytes.empty
  in
  let hash = Value (Ethereum_types.hash_to_bytes hash |> Bytes.of_string) in
  let return_data = bool_encoding config.tracer_config.enable_return_data in
  let memory = bool_encoding config.tracer_config.enable_memory in
  let stack = bool_encoding config.tracer_config.disable_stack in
  let storage = bool_encoding config.tracer_config.disable_storage in
  List [hash; return_data; memory; stack; storage] |> encode |> Bytes.to_string
