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

  type output

  type output_proof

  type hash = Smart_rollup.State_hash.t

  type input

  type input_request

  val state_hash : state -> hash

  val empty_state : unit -> state

  val proof_start_state : proof -> hash

  val proof_stop_state : proof -> hash

  val proof_to_bytes : proof -> bytes

  val bytes_to_proof : bytes -> (proof, string) result

  val install_boot_sector : state -> string -> state

  val verify_proof : input option -> proof -> input_request option

  val output_of_output_proof : output_proof -> output

  val state_of_output_proof : output_proof -> hash

  val verify_output_proof : output_proof -> output option

  val output_proof_to_bytes : output_proof -> bytes

  val bytes_to_output_proof : bytes -> (output_proof, string) result

  val get_current_level : state -> int32 option
end

module Riscv_proto_env : Riscv_proto_env_sig

type void = |

type minimal_state = {
  payload : string;
  level : Raw_level_repr.t option;
  message_counter : Z.t;
  tick : Z.t;
}

(* This encoding is used in the rollup node when embedding the state into an Irmin context. *)
val minimal_state_encoding : minimal_state Data_encoding.t

val make_empty_state : unit -> minimal_state

module Protocol_implementation :
  Sc_rollup_PVM_sig.PROTO_VERIFICATION
    with type context = unit
     and type state = minimal_state
     and type proof = void
