(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let read_value state path =
  let open Lwt_result_syntax in
  let*! value = Evm_state.inspect state path in
  match value with
  | None -> tzfail Tracer_types.Trace_not_found
  | Some value -> return value

let read_output state =
  let open Lwt_result_syntax in
  let* gas =
    read_value state Durable_storage_path.Trace_transaction.output_gas
  in
  let* failed =
    read_value state Durable_storage_path.Trace_transaction.output_failed
  in
  let* return_value =
    read_value state Durable_storage_path.Trace_transaction.output_return_value
  in
  return @@ Tracer_types.output_binary_decoder ~gas ~failed ~return_value

let trace_transaction ~block_number ~transaction_hash ~config =
  let open Lwt_result_syntax in
  let input = Tracer_types.input_rlp_encoder transaction_hash config in
  let set_input state =
    let*! state =
      Evm_state.modify
        ~key:Durable_storage_path.Trace_transaction.input
        ~value:input
        state
    in
    return state
  in
  let* apply_result =
    Evm_context.replay
      ~log_file:"trace_transaction"
      ~alter_evm_state:set_input
      block_number
  in
  match apply_result with
  | Apply_failure -> tzfail Tracer_types.Trace_not_found
  | Apply_success {evm_state; _} -> read_output evm_state
