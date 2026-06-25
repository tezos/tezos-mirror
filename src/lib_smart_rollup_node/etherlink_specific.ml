(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let current_level_path = "/evm/world_state/blocks/current/number"

let durable_lookup state key_str =
  let open Lwt_syntax in
  let key = Tezos_scoru_wasm.Durable.key_of_string_exn key_str in
  let* storage = Wasm_2_0_0_utilities.decode_storage state in
  let durable =
    Tezos_scoru_wasm.Wasm_pvm_state.Internal_state.durable_of storage
  in
  let* res_opt = Tezos_scoru_wasm.Durable.find_value durable key in
  match res_opt with
  | None -> return_none
  | Some v ->
      let+ b = Tezos_lazy_containers.Chunked_byte_vector.to_bytes v in
      Some b

let pvm_state_lookup ctxt path =
  let open Lwt_syntax in
  let* state = Context.PVMState.find ctxt in
  match state with
  | None -> return_none
  | Some state -> durable_lookup state path

let current_level ctxt =
  let open Lwt_syntax in
  let+ bytes = pvm_state_lookup ctxt current_level_path in
  Option.map (fun bytes -> Bytes.to_string bytes |> Z.of_bits |> Z.to_int) bytes
