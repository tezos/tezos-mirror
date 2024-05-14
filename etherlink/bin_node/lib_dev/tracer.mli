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
  block_number:Ethereum_types.quantity ->
  transaction_hash:Ethereum_types.hash ->
  config:Tracer_types.config ->
  unit tzresult Lwt.t
