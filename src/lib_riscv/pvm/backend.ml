(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024-2025 TriliTech <contact@trili.tech>                    *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Api = Octez_riscv_api

type reveals = unit

type write_debug = string -> unit Lwt.t

type hash = Tezos_crypto.Hashed.Smart_rollup_state_hash.t

type state = Api.state

type status = Api.status

type reveal_data = Api.reveal_data

type input = Api.input

type input_request = Api.input_request

type proof = Api.proof

type output_proof = Api.output_proof

type output_info = {
  message_index : Z.t;
  outbox_level : Bounded.Non_negative_int32.t;
}

type output = {info : output_info; encoded_message : string}

let riscv_hash_to_rollup_state_hash (bytes : bytes) : hash =
  Tezos_crypto.Hashed.Smart_rollup_state_hash.of_bytes_exn bytes

let from_api_output_info : Api.output_info -> output_info =
 fun {message_index; outbox_level} ->
  let outbox_level =
    (* The Rust api guarantees this is a valid unsigned 31 bits integer *)
    match Bounded.Non_negative_int32.of_value outbox_level with
    | None -> assert false
    | Some level -> level
  in
  {message_index = Z.of_int64 message_index; outbox_level}

let from_api_output : Api.output -> output =
 fun {info; encoded_message} ->
  {
    info = from_api_output_info info;
    encoded_message = String.of_bytes encoded_message;
  }

(* The kernel debug logging function (`string -> unit Lwt.t`) passed by the node
 * to [compute_step] and [compute_step_many] cannot be passed directly
 * to the Rust backend, which expects a `u8 -> ()` function and cannot run Lwt
 * computations. We also don't want to call it on every character output by the
 * kernel debug log
 * (TODO: https://linear.app/tezos/issue/RV-154/use-line-buffering-for-kernel-output)
 * We instead pass a closure which accumulates the log in an
 * extensible buffer and, once the Rust function returns, pass the buffer
 * to the kernel debug logging function. *)
let with_hooks printer f =
  let open Lwt_syntax in
  let debug_log = Buffer.create 1024 in
  let res = f (fun c -> Buffer.add_char debug_log (Char.chr c)) in
  let* () = printer (Buffer.contents debug_log) in
  return res

module Mutable_state = struct
  type t = Api.mut_state

  let from_imm = Api.octez_riscv_from_imm

  let to_imm = Api.octez_riscv_to_imm

  let compute_step_many ?reveal_builtins:_ ?write_debug ?stop_at_snapshot:_
      ~max_steps state =
    match write_debug with
    | None -> Lwt.return (Api.octez_riscv_mut_compute_step_many max_steps state)
    | Some printer ->
        with_hooks
          printer
          (Api.octez_riscv_mut_compute_step_many_with_debug max_steps state)

  let get_tick state =
    Lwt.return (Z.of_int64 (Api.octez_riscv_mut_get_tick state))

  let get_status state = Lwt.return (Api.octez_riscv_mut_get_status state)

  let get_message_counter state =
    Lwt.return (Api.octez_riscv_mut_get_message_counter state)

  let get_current_level state = Lwt.return (Api.octez_riscv_mut_get_level state)

  let state_hash state =
    riscv_hash_to_rollup_state_hash @@ Api.octez_riscv_mut_state_hash state

  let set_input state input =
    Lwt.return (Api.octez_riscv_mut_set_input state input)

  let get_reveal_request state =
    Lwt.return (String.of_bytes @@ Api.octez_riscv_mut_get_reveal_request state)
end

let compute_step_many ?reveal_builtins:_ ?write_debug ?stop_at_snapshot:_
    ~max_steps state =
  match write_debug with
  | None -> Lwt.return (Api.octez_riscv_compute_step_many max_steps state)
  | Some printer ->
      with_hooks
        printer
        (Api.octez_riscv_compute_step_many_with_debug max_steps state)

let compute_step state = Lwt.return (Api.octez_riscv_compute_step state)

let compute_step_with_debug ?write_debug state =
  match write_debug with
  | None -> Lwt.return (Api.octez_riscv_compute_step state)
  | Some printer ->
      with_hooks printer (Api.octez_riscv_compute_step_with_debug state)

let get_tick state = Lwt.return (Z.of_int64 (Api.octez_riscv_get_tick state))

let get_status state = Lwt.return (Api.octez_riscv_get_status state)

let get_message_counter state =
  Lwt.return (Api.octez_riscv_get_message_counter state)

let string_of_status status = Api.octez_riscv_string_of_status status

let install_boot_sector state boot_sector =
  Lwt.return
    (Api.octez_riscv_install_boot_sector state (Bytes.of_string boot_sector))

let get_current_level state = Lwt.return (Api.octez_riscv_get_level state)

let state_hash state =
  riscv_hash_to_rollup_state_hash @@ Api.octez_riscv_state_hash state

let set_input state input = Lwt.return (Api.octez_riscv_set_input state input)

let proof_start_state proof =
  riscv_hash_to_rollup_state_hash @@ Api.octez_riscv_proof_start_state proof

let proof_stop_state proof =
  riscv_hash_to_rollup_state_hash @@ Api.octez_riscv_proof_stop_state proof

let verify_proof input proof = Api.octez_riscv_verify_proof input proof

let produce_proof input state = Api.octez_riscv_produce_proof input state

let serialise_proof proof = Api.octez_riscv_serialise_proof proof

let deserialise_proof proof = Api.octez_riscv_deserialise_proof proof

let output_info_of_output_proof output_proof =
  from_api_output_info
  @@ Api.octez_riscv_output_info_of_output_proof output_proof

let state_of_output_proof output_proof =
  riscv_hash_to_rollup_state_hash
  @@ Api.octez_riscv_state_of_output_proof output_proof

let verify_output_proof output_proof =
  let open Option_syntax in
  let+ output = Api.octez_riscv_verify_output_proof output_proof in
  from_api_output output

let serialise_output_proof output_proof =
  Api.octez_riscv_serialise_output_proof output_proof

let deserialise_output_proof bytes =
  Api.octez_riscv_deserialise_output_proof bytes

let get_reveal_request state =
  Lwt.return (String.of_bytes @@ Api.octez_riscv_get_reveal_request state)
