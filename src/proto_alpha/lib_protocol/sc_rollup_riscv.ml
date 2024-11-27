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

type error += RISCV_proof_production_failed

type error += RISCV_proof_verification_failed

type error += RISCV_output_proof_verification_failed

let () =
  let open Data_encoding in
  let msg = "Proof production failed" in
  register_error_kind
    `Permanent
    ~id:"smart_rollup_riscv_proof_production_failed"
    ~title:msg
    ~pp:(fun fmt () -> Format.fprintf fmt "%s" msg)
    ~description:msg
    unit
    (function RISCV_proof_production_failed -> Some () | _ -> None)
    (fun () -> RISCV_proof_production_failed) ;
  let msg = "Proof verification failed" in
  register_error_kind
    `Permanent
    ~id:"smart_rollup_riscv_proof_verification_failed"
    ~title:msg
    ~pp:(fun fmt () -> Format.fprintf fmt "%s" msg)
    ~description:msg
    unit
    (function RISCV_proof_verification_failed -> Some () | _ -> None)
    (fun () -> RISCV_proof_verification_failed) ;
  let msg = "Output proof verification failed" in
  register_error_kind
    `Permanent
    ~id:"smart_rollup_riscv_output_proof_verification_failed"
    ~title:msg
    ~pp:(fun fmt () -> Format.fprintf fmt "%s" msg)
    ~description:msg
    unit
    (function RISCV_output_proof_verification_failed -> Some () | _ -> None)
    (fun () -> RISCV_output_proof_verification_failed)

module PS = Sc_rollup_PVM_sig

type state = Riscv_proto_env.state

type proof = Riscv_proto_env.proof

let make_empty_state = Riscv_proto_env.empty_state

(* TODO: Implement conversion functions after RV-319 will introduce the protocol environment
   modules & types for RISCV *)
let from_riscv_input_request : Riscv_proto_env.input_request -> PS.input_request
    =
 fun _input_request -> assert false

let to_riscv_input : PS.input -> Riscv_proto_env.input =
 fun _input -> assert false

let from_riscv_output : Riscv_proto_env.output -> PS.output =
 fun _output -> assert false

module Protocol_implementation :
  Sc_rollup_PVM_sig.PROTO_VERIFICATION
    with type context = unit
     and type state = Riscv_proto_env.state
     and type proof = Riscv_proto_env.proof = struct
  let parse_boot_sector s = Some s

  let pp _state =
    Lwt.return @@ fun fmt () -> Format.pp_print_string fmt "<Riscv-state>"

  type output_proof = Riscv_proto_env.output_proof

  type state = Riscv_proto_env.state

  type context = unit

  type hash = Smart_rollup.State_hash.t

  type proof = Riscv_proto_env.proof

  let proof_encoding : proof Data_encoding.t =
    let open Data_encoding in
    conv_with_guard
      Riscv_proto_env.proof_to_bytes
      Riscv_proto_env.bytes_to_proof
      (bytes Hex)

  let proof_start_state state = Riscv_proto_env.proof_start_state state

  let proof_stop_state state = Riscv_proto_env.proof_stop_state state

  let state_hash state = Lwt.return @@ Riscv_proto_env.state_hash state

  let initial_state ~empty = Lwt.return empty

  let install_boot_sector state boot_sector =
    Lwt.return @@ Riscv_proto_env.install_boot_sector state boot_sector

  let verify_proof ~is_reveal_enabled:_ input proof =
    let open Lwt_result_syntax in
    match
      Riscv_proto_env.verify_proof (Option.map to_riscv_input input) proof
    with
    | None -> tzfail RISCV_proof_verification_failed
    | Some input_request -> return @@ from_riscv_input_request input_request

  let output_proof_encoding =
    let open Data_encoding in
    conv_with_guard
      Riscv_proto_env.output_proof_to_bytes
      Riscv_proto_env.bytes_to_output_proof
      (bytes Hex)

  let output_of_output_proof output_proof =
    from_riscv_output @@ Riscv_proto_env.output_of_output_proof output_proof

  let state_of_output_proof output_proof =
    Riscv_proto_env.state_of_output_proof output_proof

  let verify_output_proof output_proof =
    let open Lwt_result_syntax in
    match Riscv_proto_env.verify_output_proof output_proof with
    | None -> tzfail RISCV_output_proof_verification_failed
    | Some output -> return @@ from_riscv_output output

  let check_dissection ~default_number_of_sections ~start_chunk ~stop_chunk
      dissection =
    let open Sc_rollup_dissection_chunk_repr in
    let dist = Sc_rollup_tick_repr.distance start_chunk.tick stop_chunk.tick in
    let section_maximum_size = Z.div dist (Z.of_int 2) in
    Sc_rollup_dissection_chunk_repr.(
      default_check
        ~section_maximum_size
        ~check_sections_number:default_check_sections_number
        ~default_number_of_sections
        ~start_chunk
        ~stop_chunk
        dissection)

  let get_current_level state =
    let level = Riscv_proto_env.get_current_level state in
    let level =
      Option.bind level @@ fun level ->
      (* Assuming 1 second / level, this errors after ~65 years i.e. year 2090 *)
      match Raw_level_repr.of_int32 level with
      | Error _ -> None
      | Ok level -> Some level
    in
    Lwt.return level
end
