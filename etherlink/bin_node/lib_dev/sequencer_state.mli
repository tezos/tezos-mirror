(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t = Irmin_context.PVMState.value

(** [execute ~config evm_state messages] executes [messages] on the local
    [evm_state]. *)
val execute :
  config:Config.config -> t -> [< `Input of string] list -> t tzresult Lwt.t

(** [init ~kernel] initializes the local [evm_state] with [kernel]. *)
val init : kernel:string -> t tzresult Lwt.t

(** [inspect evm_state key] returns the value stored under [key] in
    [evm_state], if any. *)
val inspect : t -> string -> bytes option Lwt.t

(** [execute_and_inspect ~config ~input evm_state] executes [input] within
    [evm_state], and returns [input.insights_requests]. *)
val execute_and_inspect :
  config:Config.config ->
  input:Simulation.Encodings.simulate_input ->
  t ->
  bytes option list tzresult Lwt.t

(** [current_block_height evm_state] returns the height of the latest block
    produced by the kernel. *)
val current_block_height : t -> Ethereum_types.block_height Lwt.t

type error += Cannot_apply_blueprint

val apply_blueprint :
  config:Config.config ->
  t ->
  Blueprint_types.t ->
  (t * Ethereum_types.block_height) tzresult Lwt.t
