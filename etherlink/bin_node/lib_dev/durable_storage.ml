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

let chain_family state chain_id =
  inspect_durable_and_decode
    state
    (Durable_storage_path.Chain_configuration.chain_family chain_id)
    (fun x -> L2_types.Chain_family.of_string_exn (Bytes.to_string x))

let world_state state chain_id =
  inspect_durable_and_decode
    state
    (Durable_storage_path.Chain_configuration.world_state chain_id)
    Bytes.to_string

let storage_version = Durable_storageV2.storage_version

let sequencer =
 fun ?storage_version state ->
  let open Lwt_result_syntax in
  let* sequencer_path =
    let* storage_version =
      match storage_version with
      | Some storage_version -> return storage_version
      | None -> Durable_storageV2.storage_version state
    in
    return (Durable_storage_path.sequencer_key ~storage_version)
  in
  inspect_durable_and_decode state sequencer_path (fun bytes ->
      Signature.Public_key.of_b58check_exn (String.of_bytes bytes))

let block_number ~root state n =
  let open Lwt_result_syntax in
  match n with
  | Durable_storage_path.Block.Nth i -> return @@ Ethereum_types.Qty i
  | Durable_storage_path.Block.Current -> (
      let* answer =
        Durable_storageV2.read_opt
          (Raw_path (Durable_storage_path.Block.current_number ~root))
          state
      in
      match answer with
      | Some bytes ->
          return (Ethereum_types.Qty (Bytes.to_string bytes |> Z.of_bits))
      | None ->
          raise
          @@ Invalid_block_structure
               "Unexpected [None] value for [current_number]'s [answer]")

let list_runtimes state =
  let open Lwt_result_syntax in
  let check_runtime r =
    let* bytes_opt =
      Durable_storageV2.read_opt (Raw_path (Tezosx.feature_flag r)) state
    in
    if Option.is_some bytes_opt then return @@ Some r else return None
  in
  List.filter_map_ep check_runtime Tezosx.known_runtimes

let michelson_runtime_sunrise_level state =
  inspect_durable_and_decode_opt
    state
    Durable_storage_path.michelson_runtime_sunrise_level
    (fun bytes -> Ethereum_types.Qty (Bytes.to_string bytes |> Z.of_bits))
