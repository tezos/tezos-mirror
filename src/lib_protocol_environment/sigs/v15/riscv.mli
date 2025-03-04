(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 TriliTech <contact@trilitech.com>                      *)
(*                                                                           *)
(*****************************************************************************)

type state

type proof

type output_info = {
  message_index : Z.t;
  outbox_level : Bounded.Non_negative_int32.t;
}

type output = {info : output_info; encoded_message : string}

type output_proof

type hash = Smart_rollup.State_hash.t

type input = Inbox_message of int32 * int64 * string | Reveal of string

type input_request =
  | No_input_required
  | Initial
  | First_after of int32 * int64
  | Needs_reveal of string

val state_hash : state -> hash

val empty_state : unit -> state

val proof_start_state : proof -> hash

val proof_stop_state : proof -> hash

val proof_to_bytes : proof -> bytes

val bytes_to_proof : bytes -> (proof, string) result

val install_boot_sector : state -> string -> state Lwt.t

val verify_proof : input option -> proof -> input_request option

val output_info_of_output_proof : output_proof -> output_info

val state_of_output_proof : output_proof -> hash

val verify_output_proof : output_proof -> output option

val output_proof_to_bytes : output_proof -> bytes

val bytes_to_output_proof : bytes -> (output_proof, string) result

val get_current_level : state -> int32 option Lwt.t
