(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024-2025 TriliTech <contact@trili.tech>                    *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Api = Octez_riscv_api

type reveals

type write_debug = string -> unit Lwt.t

type hash = Tezos_crypto.Hashed.Smart_rollup_state_hash.t

type state = Api.state

type status = Api.status

(* TODO RV-615: Improve the `input` type exposed in protocol environment *)
(* Mirrors Api.input but requires manual conversion *)
type input = Inbox_message of int32 * int64 * string | Reveal of string

(* Mirrors Api.input_request but requires manual conversion *)
type input_request =
  | No_input_required
  | Initial
  | First_after of int32 * int64
  | Needs_reveal of string

type proof = Api.proof

type output_proof = Api.output_proof

type output_info = {
  message_index : Z.t;
  outbox_level : Bounded.Non_negative_int32.t;
}

type output = {info : output_info; encoded_message : string}

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

  val state_hash : t -> hash

  val set_input : t -> input -> unit Lwt.t

  val get_reveal_request : t -> string Lwt.t

  val insert_failure : t -> unit Lwt.t

  val install_boot_sector : t -> string -> unit Lwt.t
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

val state_hash : state -> hash

val set_input : state -> input -> state Lwt.t

val proof_start_state : proof -> hash

val proof_stop_state : proof -> hash

val verify_proof : input option -> proof -> input_request option

val produce_proof : input option -> state -> proof option

val serialise_proof : proof -> bytes

val deserialise_proof : bytes -> (proof, string) result

val output_info_of_output_proof : output_proof -> output_info

val state_of_output_proof : output_proof -> hash

val verify_output_proof : output_proof -> output option

val serialise_output_proof : output_proof -> bytes

val deserialise_output_proof : bytes -> (output_proof, string) result

val get_reveal_request : state -> string Lwt.t

val insert_failure : state -> state Lwt.t
