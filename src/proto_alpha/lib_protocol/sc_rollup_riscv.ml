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

module Riscv_proto_env : Riscv_proto_env_sig = struct
  type state = {
    payload : string;
    level : Raw_level_repr.t option;
    message_counter : Z.t;
    tick : Z.t;
  }

  type proof = unit

  type output_info = {
    outbox_level : Bounded.Non_negative_int32.t;
    message_index : Z.t;
  }

  type output = {info : output_info; encoded_message : string}

  type output_proof = unit

  type hash = Smart_rollup.State_hash.t

  type input = Inbox_message of int32 * int64 * string | Reveal of string

  type input_request =
    | No_input_required
    | Initial
    | First_after of int32 * int64
    | Needs_reveal of string

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

  let output_info_of_output_proof _output_proof = assert false

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

(** If [None] is returned, converting from raw data encoded strings / PVM raw data to Protocol types failed  *)
let from_riscv_input_request (input_request : Riscv_proto_env.input_request) :
    PS.input_request option =
  let open Option_syntax in
  match input_request with
  | Riscv_proto_env.No_input_required -> return PS.No_input_required
  | Riscv_proto_env.Initial -> return PS.Initial
  | Riscv_proto_env.First_after (level, index) ->
      let* raw_level = Option.of_result @@ Raw_level_repr.of_int32 level in
      return @@ PS.First_after (raw_level, Z.of_int64 index)
  | Riscv_proto_env.Needs_reveal raw_string ->
      let* reveal_data =
        Data_encoding.Binary.of_string_opt PS.reveal_encoding raw_string
      in
      return @@ PS.Needs_reveal reveal_data

(** If [None] is returned, converting from raw data encoded strings / protocol raw data to Riscv types failed  *)
let to_riscv_input (input : PS.input) : Riscv_proto_env.input option =
  let open Option_syntax in
  match input with
  | PS.Inbox_message {inbox_level; message_counter; payload} ->
      return
      @@ Riscv_proto_env.Inbox_message
           ( Raw_level_repr.to_int32 inbox_level,
             Z.to_int64 message_counter,
             Sc_rollup_inbox_message_repr.unsafe_to_string payload )
  | PS.Reveal reveal_data ->
      let* raw_reveal_data =
        Data_encoding.Binary.to_string_opt PS.reveal_data_encoding reveal_data
      in
      return @@ Riscv_proto_env.Reveal raw_reveal_data

(** If [None] is returned, converting from raw data encoded strings / PVM raw data to Protocol types failed  *)
let from_riscv_output (output : Riscv_proto_env.output) : PS.output option =
  let open Option_syntax in
  let* message =
    Data_encoding.Binary.of_string_opt
      Sc_rollup_outbox_message_repr.encoding
      output.encoded_message
  in
  let outbox_level =
    Raw_level_repr.of_int32_non_negative output.info.outbox_level
  in
  return
    PS.
      {
        output_info = {message_index = output.info.message_index; outbox_level};
        message;
      }

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
    let input_request =
      let open Option_syntax in
      let* input = Option.map to_riscv_input input in
      let* input_request = Riscv_proto_env.verify_proof input proof in
      from_riscv_input_request input_request
    in
    match input_request with
    | None -> tzfail RISCV_proof_verification_failed
    | Some input_request -> return input_request

  let output_proof_encoding =
    let open Data_encoding in
    conv_with_guard
      Riscv_proto_env.output_proof_to_bytes
      Riscv_proto_env.bytes_to_output_proof
      (bytes Hex)

  let output_info_of_output_proof output_proof : PS.output_info =
    let open Riscv_proto_env in
    let {outbox_level; message_index} =
      output_info_of_output_proof output_proof
    in
    {
      outbox_level = Raw_level_repr.of_int32_non_negative outbox_level;
      message_index;
    }

  let state_of_output_proof output_proof =
    Riscv_proto_env.state_of_output_proof output_proof

  let verify_output_proof output_proof =
    let open Lwt_result_syntax in
    let output =
      let open Option_syntax in
      let* output = Riscv_proto_env.verify_output_proof output_proof in
      from_riscv_output output
    in
    match output with
    | None -> tzfail RISCV_output_proof_verification_failed
    | Some output -> return output

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
