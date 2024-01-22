(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [execute ?commit ctxt messages] executes [messages] on the local
    EVM state of [ctxt], commits to disk if [commit] is true. Returns
    the modified EVM state even if it's not commited. *)
val execute :
  ?commit:bool ->
  Sequencer_context.t ->
  [< `Input of string] list ->
  (Sequencer_context.t * Sequencer_context.evm_state) tzresult Lwt.t

(** [init ~secret_key ~smart_rollup_address ~rollup_node_endpoint ctxt]
    initializes the local state in [ctxt], produces and publishes the
    genesis block. *)
val init :
  secret_key:Signature.secret_key ->
  smart_rollup_address:string ->
  rollup_node_endpoint:Uri.t ->
  Sequencer_context.t ->
  Sequencer_context.t tzresult Lwt.t

(** [inspect evm_state key] inspects [key] in [evm_state]. *)
val inspect : Sequencer_context.evm_state -> string -> bytes option Lwt.t

(** [execute_and_inspect ctxt ~input] executes [input] in [ctxt] and
    returns the [input.insights_requests]. *)
val execute_and_inspect :
  Sequencer_context.t ->
  input:Simulation.Encodings.simulate_input ->
  bytes option list tzresult Lwt.t

(** [current_block_height evm_state] returns the height of the latest block
    produced by the kernel. *)
val current_block_height :
  Sequencer_context.evm_state -> Ethereum_types.block_height Lwt.t
