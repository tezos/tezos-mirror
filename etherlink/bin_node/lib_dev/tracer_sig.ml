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
    unit tzresult Lwt.t
end

module Make (Storage : sig
  val transaction_receipt :
    Ethereum_types.hash ->
    Ethereum_types.transaction_receipt option tzresult Lwt.t
end)
(Tracer : Backend) =
struct
  let trace_transaction transaction_hash config =
    let open Lwt_result_syntax in
    let* receipt = Storage.transaction_receipt transaction_hash in
    match receipt with
    | None -> tzfail (Tracer_types.Transaction_not_found transaction_hash)
    | Some Ethereum_types.{blockNumber; _} ->
        Tracer.trace_transaction
          ~block_number:blockNumber
          ~transaction_hash
          ~config
end
