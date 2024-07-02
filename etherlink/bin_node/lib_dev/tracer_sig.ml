(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module type Backend = sig
  val trace_transaction :
    block_number:Ethereum_types.quantity ->
    transaction_hash:Ethereum_types.hash ->
    config:Tracer_types.config ->
    Tracer_types.output tzresult Lwt.t

  val trace_call :
    call:Ethereum_types.call ->
    block:Ethereum_types.Block_parameter.extended ->
    config:Tracer_types.config ->
    Tracer_types.output tzresult Lwt.t
end

module Make (Storage : sig
  val transaction_receipt :
    Ethereum_types.hash -> Transaction_receipt.t option tzresult Lwt.t
end)
(Tracer : Backend) =
struct
  let trace_transaction transaction_hash config =
    let open Lwt_result_syntax in
    let* receipt = Storage.transaction_receipt transaction_hash in
    match receipt with
    | None -> tzfail (Tracer_types.Transaction_not_found transaction_hash)
    | Some Transaction_receipt.{blockNumber; _} ->
        Tracer.trace_transaction
          ~block_number:blockNumber
          ~transaction_hash
          ~config

  let trace_call call block config = Tracer.trace_call ~call ~block ~config
end
