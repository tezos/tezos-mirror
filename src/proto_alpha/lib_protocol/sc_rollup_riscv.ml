(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 - 2024 Nomadic Labs <contact@nomadic-labs.com>         *)
(* Copyright (c) 2024 - 2025 TriliTech, <contact@trili.tech>                 *)
(*                                                                           *)
(*****************************************************************************)

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

type state = Riscv.state

type proof = Riscv.proof

let make_empty_state = Riscv.empty_state

(** If [None] is returned, converting from raw data encoded strings / PVM raw data to Protocol types failed  *)
let of_riscv_input_request (input_request : Riscv.input_request) :
    PS.input_request option =
  let open Option_syntax in
  match input_request with
  | Riscv.No_input_required -> return PS.No_input_required
  | Riscv.Initial -> return PS.Initial
  | Riscv.First_after (level, index) ->
      let+ raw_level = Option.of_result @@ Raw_level_repr.of_int32 level in
      PS.First_after (raw_level, Z.of_int64 index)
  | Riscv.Needs_reveal raw_string ->
      let+ reveal_data =
        Data_encoding.Binary.of_string_opt PS.reveal_encoding raw_string
      in
      PS.Needs_reveal reveal_data

let to_riscv_input (input : PS.input) : Riscv.input =
  match input with
  | PS.Inbox_message {inbox_level; message_counter; payload} ->
      Riscv.Inbox_message
        ( Raw_level_repr.to_int32 inbox_level,
          Z.to_int64 message_counter,
          Sc_rollup_inbox_message_repr.unsafe_to_string payload )
  | PS.Reveal reveal_data ->
      let reveal_data_bytes = PS.reveal_response_to_bytes reveal_data in
      Riscv.Reveal (Bytes.to_string reveal_data_bytes)

(** If [None] is returned, converting from raw data encoded strings / PVM raw data to Protocol types failed  *)
let from_riscv_output (output : Riscv.output) : PS.output option =
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
     and type state = Riscv.state
     and type proof = Riscv.proof = struct
  let parse_boot_sector s = Some s

  let pp _state =
    Lwt.return @@ fun fmt () -> Format.pp_print_string fmt "<Riscv-state>"

  type output_proof = Riscv.output_proof

  type state = Riscv.state

  type context = unit

  type hash = Smart_rollup.State_hash.t

  type proof = Riscv.proof

  let proof_encoding : proof Data_encoding.t =
    let open Data_encoding in
    conv_with_guard Riscv.proof_to_bytes Riscv.bytes_to_proof (bytes Hex)

  let proof_start_state state = Riscv.proof_start_state state

  let proof_stop_state state = Riscv.proof_stop_state state

  let state_hash state = Lwt.return @@ Riscv.state_hash state

  let initial_state ~empty = Lwt.return empty

  let install_boot_sector state boot_sector =
    Riscv.install_boot_sector state boot_sector

  let verify_proof ~is_reveal_enabled:_ input proof =
    let open Lwt_result_syntax in
    let* input_request =
      match Riscv.verify_proof (Option.map to_riscv_input input) proof with
      | Some request -> return request
      | None -> tzfail RISCV_proof_verification_failed
    in
    match of_riscv_input_request input_request with
    | Some request -> return request
    | None -> tzfail RISCV_proof_verification_failed

  let output_proof_encoding =
    let open Data_encoding in
    conv_with_guard
      Riscv.output_proof_to_bytes
      Riscv.bytes_to_output_proof
      (bytes Hex)

  let output_info_of_output_proof output_proof : PS.output_info =
    let open Riscv in
    let {outbox_level; message_index} =
      output_info_of_output_proof output_proof
    in
    {
      outbox_level = Raw_level_repr.of_int32_non_negative outbox_level;
      message_index;
    }

  let state_of_output_proof output_proof =
    Riscv.state_of_output_proof output_proof

  let verify_output_proof output_proof =
    let open Lwt_result_syntax in
    let output =
      let open Option_syntax in
      let* output = Riscv.verify_output_proof output_proof in
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
    let open Lwt_option_syntax in
    let* level = Riscv.get_current_level state in
    (* Assuming 1 second / level, this errors after ~65 years i.e. year 2090 *)
    Lwt.return @@ Option.of_result @@ Raw_level_repr.of_int32 level
end
