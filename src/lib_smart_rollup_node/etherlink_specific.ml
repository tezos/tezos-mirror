(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let current_level_path = "/evm/world_state/blocks/current/number"

let durable_lookup (module PVM : Pvm_plugin_sig.S) state key_str =
  let open Lwt_syntax in
  let key = Tezos_scoru_wasm.Durable.key_of_string_exn key_str in
  let tree = !(Context_wrapper.Irmin.of_node_pvmstate state) in
  let* durable =
    PVM.Wasm_2_0_0.decode_durable_state
      Tezos_scoru_wasm.Wasm_pvm.durable_storage_encoding
      tree
  in
  let* res_opt = Tezos_scoru_wasm.Durable.find_value durable key in
  match res_opt with
  | None -> return_none
  | Some v ->
      let+ b = Tezos_lazy_containers.Chunked_byte_vector.to_bytes v in
      Some b

let pvm_state_lookup pvm_plugin ctxt path =
  let open Lwt_syntax in
  let* state = Context.PVMState.find ctxt in
  match state with
  | None -> return_none
  | Some state -> durable_lookup pvm_plugin state path

let current_level pvm_plugin ctxt =
  let open Lwt_syntax in
  let+ bytes = pvm_state_lookup pvm_plugin ctxt current_level_path in
  Option.map (fun bytes -> Bytes.to_string bytes |> Z.of_bits |> Z.to_int) bytes
