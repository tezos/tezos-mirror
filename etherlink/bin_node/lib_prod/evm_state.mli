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

(** [modify ~key ~value evm_state] sets [value] at [key] in the local EVM
    state. *)
val modify : key:string -> value:string -> t -> t Lwt.t

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

(** Same as {!current_block_height} for the block hash. *)
val current_block_hash : t -> Ethereum_types.block_hash tzresult Lwt.t

type apply_result =
  | Apply_success of t * Ethereum_types.block_height * Ethereum_types.block_hash
  | Apply_failure

(** [apply_blueprint ~config state payload] applies the blueprint [payload] on
    top of [evm_state]. If the payload produces a block, the new updated EVM
    state is returned along with the new block’s height. *)
val apply_blueprint :
  config:Config.config ->
  t ->
  Blueprint_types.payload ->
  apply_result tzresult Lwt.t
