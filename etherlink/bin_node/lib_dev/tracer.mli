(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [trace_transaction ~block_number ~transaction ~config] replays the block
    [block_number] and traces [transaction_hash] in it, with the given
    [config]. *)
val trace_transaction :
  (module Evm_execution.S) ->
  block_number:Ethereum_types.quantity ->
  transaction_hash:Ethereum_types.hash ->
  config:Tracer_types.config ->
  Tracer_types.output tzresult Lwt.t

(** [trace_call ~call ~block ~config] simulates and traces call
    [call] in block [block], with the given [config]. *)
val trace_call :
  (module Evm_execution.S) ->
  call:Ethereum_types.call ->
  block:Ethereum_types.Block_parameter.extended ->
  config:Tracer_types.config ->
  Tracer_types.output tzresult Lwt.t
