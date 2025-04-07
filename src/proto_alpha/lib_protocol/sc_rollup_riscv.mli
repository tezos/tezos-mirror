(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 - 2024 Nomadic Labs <contact@nomadic-labs.com>         *)
(* Copyright (c) 2024 - 2025 TriliTech <contact@trili.tech>                  *)
(*                                                                           *)
(*****************************************************************************)

(* TODO RV-374: Move `Riscv_proto_env_sig` and `Riscv_proto_env` to protocol
   environment *)
module type Riscv_proto_env_sig = sig
  type state

  type proof

  type output_info = {
    outbox_level : Bounded.Non_negative_int32.t;
    message_index : Z.t;
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

  val install_boot_sector : state -> string -> state

  val verify_proof : input option -> proof -> input_request option

  val output_info_of_output_proof : output_proof -> output_info

  val state_of_output_proof : output_proof -> hash

  val verify_output_proof : output_proof -> output option

  val output_proof_to_bytes : output_proof -> bytes

  val bytes_to_output_proof : bytes -> (output_proof, string) result

  val get_current_level : state -> int32 option
end

module Riscv_proto_env : Riscv_proto_env_sig

type error += RISCV_proof_verification_failed

type error += RISCV_proof_production_failed

type state = Riscv_proto_env.state

type proof = Riscv_proto_env.proof

val make_empty_state : unit -> state

module Protocol_implementation :
  Sc_rollup_PVM_sig.PROTO_VERIFICATION
    with type context = unit
     and type state = Riscv_proto_env.state
     and type proof = Riscv_proto_env.proof
