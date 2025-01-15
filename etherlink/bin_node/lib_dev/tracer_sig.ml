(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module type Backend = sig
  val trace_transaction :
    (module Evm_execution.S) ->
    block_number:Ethereum_types.quantity ->
    transaction_hash:Ethereum_types.hash ->
    config:Tracer_types.config ->
    Tracer_types.output tzresult Lwt.t

  val trace_call :
    (module Evm_execution.S) ->
    call:Ethereum_types.call ->
    block:Ethereum_types.Block_parameter.extended ->
    config:Tracer_types.config ->
    Tracer_types.output tzresult Lwt.t

  val trace_block :
    (module Evm_execution.S) ->
    (module Block_storage_sig.S) ->
    block_number:Ethereum_types.quantity ->
    config:Tracer_types.config ->
    Tracer_types.block_output tzresult Lwt.t
end

module type S = sig
  (** [trace_transaction hash tracer] replays the block containing the
      transaction [hash], and traces this transaction with the specified
      [tracer]. *)
  val trace_transaction :
    Ethereum_types.hash ->
    Tracer_types.config ->
    Tracer_types.output tzresult Lwt.t

  (** [trace_call call block tracer] simulates a [call] on top of the [block]
      and traces it with the specified [tracer]. *)
  val trace_call :
    Ethereum_types.call ->
    Ethereum_types.Block_parameter.extended ->
    Tracer_types.config ->
    Tracer_types.output tzresult Lwt.t

  (** [trace_block block tracer] traces all the transactions in [block] with
      the specified [tracer]. *)
  val trace_block :
    Ethereum_types.quantity ->
    Tracer_types.config ->
    Tracer_types.block_output tzresult Lwt.t
end

module Make
    (Executor : Evm_execution.S)
    (Storage : Block_storage_sig.S)
    (Tracer : Backend) : S = struct
  let trace_transaction transaction_hash config =
    let open Lwt_result_syntax in
    let* receipt = Storage.transaction_receipt transaction_hash in
    match receipt with
    | None -> tzfail (Tracer_types.Transaction_not_found transaction_hash)
    | Some Transaction_receipt.{blockNumber; _} ->
        Tracer.trace_transaction
          (module Executor)
          ~block_number:blockNumber
          ~transaction_hash
          ~config

  let trace_call call block config =
    Tracer.trace_call (module Executor) ~call ~block ~config

  let trace_block block_number config =
    Tracer.trace_block (module Executor) (module Storage) ~block_number ~config
end
