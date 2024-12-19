(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 - 2024 Nomadic Labs <contact@nomadic-labs.com>         *)
(* Copyright (c) 2024 - 2025 TriliTech, <contact@trili.tech>                 *)
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

module Riscv_proto_env : Riscv_proto_env_sig = struct
  type state = {
    payload : string;
    level : Raw_level_repr.t option;
    message_counter : Z.t;
    tick : Z.t;
  }

  type proof = unit

  type output = unit

  type output_proof = unit

  type hash = Smart_rollup.State_hash.t

  type input = unit

  type input_request = unit

  (* In order to synchronise with the node implementation of the PVM at genesis,
     * we set the state hash to be the initial state hash of the node
     * implementation. *)
  let state_hash _state =
    Smart_rollup.State_hash.of_b58check_exn
      "srs129JscUr3XsPcNFUEiKqVNP38tn8oksbGir1qYXgQs8QD7bcNNd"

  let empty_state () =
    {payload = ""; level = None; message_counter = Z.zero; tick = Z.zero}

  let proof_start_state _proof = assert false

  let proof_stop_state _proof = assert false

  let proof_to_bytes _proof = assert false

  let bytes_to_proof _bytes = assert false

  let install_boot_sector state boot_sector = {state with payload = boot_sector}

  let verify_proof _input _proof = assert false

  let output_of_output_proof _output_proof = assert false

  let state_of_output_proof _output_proof = assert false

  let verify_output_proof _output_proof = assert false

  let output_proof_to_bytes _output_proof = assert false

  let bytes_to_output_proof _bytes = assert false

  let get_current_level _state = assert false
end

open Sc_rollup_repr
module PS = Sc_rollup_PVM_sig

(* [void] definition from [Sc_rollup_origination_machine] *)
type void = |

let void =
  Data_encoding.(
    conv_with_guard
      (function (_ : void) -> .)
      (fun _ -> Error "void has no inhabitant")
      unit)

type minimal_state = {
  payload : string;
  level : Raw_level_repr.t option;
  message_counter : Z.t;
  tick : Z.t;
}

let minimal_state_encoding =
  let open Data_encoding in
  conv
    (fun {payload; level; message_counter; tick} ->
      (payload, level, message_counter, tick))
    (fun (payload, level, message_counter, tick) ->
      {payload; level; message_counter; tick})
  @@ obj4
       (req "payload" (string Hex))
       (req "level" (option Raw_level_repr.encoding))
       (req "message_counter" n)
       (req "tick" n)

let make_empty_state () =
  {payload = ""; level = None; message_counter = Z.zero; tick = Z.zero}

let state_hash _state =
  (* In order to synchronise with the node implementation of the PVM at genesis,
   * we set the state hash to be the initial state hash of the node
   * implementation. *)
  State_hash.of_b58check_exn
    "srs129JscUr3XsPcNFUEiKqVNP38tn8oksbGir1qYXgQs8QD7bcNNd"

module Protocol_implementation :
  Sc_rollup_PVM_sig.PROTO_VERIFICATION
    with type context = unit
     and type state = minimal_state
     and type proof = void = struct
  let pp state =
    Lwt.return @@ fun fmt () -> Format.pp_print_string fmt state.payload

  type state = minimal_state

  type context = unit

  type hash = State_hash.t

  type proof = void

  let proof_encoding = void

  let proof_start_state = function (_ : proof) -> .

  let proof_stop_state = function (_ : proof) -> .

  let state_hash state = Lwt.return (state_hash state)

  let initial_state ~empty = Lwt.return empty

  let install_boot_sector state boot_sector =
    Lwt.return {state with payload = boot_sector}

  let verify_proof ~is_reveal_enabled:_ _input = function (_ : proof) -> .

  type output_proof = void

  let output_proof_encoding = void

  let output_of_output_proof = function (_ : proof) -> .

  let state_of_output_proof = function (_ : proof) -> .

  let verify_output_proof = function (_ : proof) -> .

  let check_dissection ~default_number_of_sections:_ ~start_chunk:_
      ~stop_chunk:_ =
    assert false

  let parse_boot_sector s = Some s

  let get_current_level {level; _} = Lwt.return level
end
