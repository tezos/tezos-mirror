(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Api = Octez_riscv_api

type reveals

type write_debug = string -> unit Lwt.t

type state = Api.state

type status = Octez_riscv_api.status

type reveal_data = Octez_riscv_api.reveal_data

type input = Octez_riscv_api.input

type input_request = Octez_riscv_api.input_request

type proof = Octez_riscv_api.proof

module Mutable_state : sig
  type t = Api.mut_state

  val from_imm : state -> t

  val to_imm : t -> state

  val compute_step_many :
    ?reveal_builtins:reveals ->
    ?write_debug:write_debug ->
    ?stop_at_snapshot:bool ->
    max_steps:int64 ->
    t ->
    int64 Lwt.t

  val get_tick : t -> Z.t Lwt.t

  val get_status : t -> status Lwt.t

  val get_message_counter : t -> int64 Lwt.t

  val get_current_level : t -> int32 option Lwt.t

  val state_hash : t -> bytes

  val set_input : t -> input -> unit Lwt.t
end

val compute_step_many :
  ?reveal_builtins:reveals ->
  ?write_debug:write_debug ->
  ?stop_at_snapshot:bool ->
  max_steps:int64 ->
  state ->
  (state * int64) Lwt.t

val compute_step : state -> state Lwt.t

val compute_step_with_debug : ?write_debug:write_debug -> state -> state Lwt.t

val get_tick : state -> Z.t Lwt.t

val get_status : state -> status Lwt.t

val get_message_counter : state -> int64 Lwt.t

val string_of_status : status -> string

val install_boot_sector : state -> string -> state Lwt.t

val get_current_level : state -> int32 option Lwt.t

val state_hash : state -> bytes

val set_input : state -> input -> state Lwt.t

val proof_start_state : proof -> bytes

val proof_stop_state : proof -> bytes

val verify_proof : input option -> proof -> input_request option

val produce_proof : input option -> state -> proof option
