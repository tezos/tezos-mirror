(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

exception Invalid_block_structure of string

let inspect_durable_and_decode_opt state path decode =
  let open Lwt_result_syntax in
  let* bytes = Durable_storageV2.read_opt (Raw_path path) state in
  match bytes with
  | Some bytes -> return_some (decode bytes)
  | None -> return_none

let inspect_durable_and_decode_default ~default state path decode =
  let open Lwt_result_syntax in
  let* res_opt = inspect_durable_and_decode_opt state path decode in
  match res_opt with Some res -> return res | None -> return default

let inspect_durable_and_decode state path decode =
  let open Lwt_result_syntax in
  let* res_opt = inspect_durable_and_decode_opt state path decode in
  match res_opt with
  | Some res -> return res
  | None -> failwith "No value found under %s" path

let l2_minimum_base_fee_per_gas state chain_id =
  inspect_durable_and_decode
    state
    (Durable_storage_path.Chain_configuration.minimum_base_fee_per_gas chain_id)
    Helpers.decode_z_le

let l2_da_fee_per_byte state chain_id =
  inspect_durable_and_decode
    state
    (Durable_storage_path.Chain_configuration.da_fee_per_byte chain_id)
    Helpers.decode_z_le

let l2_maximum_gas_per_transaction state chain_id =
  inspect_durable_and_decode
    state
    (Durable_storage_path.Chain_configuration.maximum_gas_per_transaction
       chain_id)
    Helpers.decode_z_le

let world_state state chain_id =
  inspect_durable_and_decode
    state
    (Durable_storage_path.Chain_configuration.world_state chain_id)
    Bytes.to_string

let michelson_runtime_sunrise_level state =
  inspect_durable_and_decode_opt
    state
    Durable_storage_path.michelson_runtime_sunrise_level
    (fun bytes -> Ethereum_types.Qty (Bytes.to_string bytes |> Z.of_bits))
