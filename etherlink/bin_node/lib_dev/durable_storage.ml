(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

exception Invalid_block_structure of string

module type READER = sig
  type state

  val get_state :
    ?block:Ethereum_types.Block_parameter.extended ->
    unit ->
    state tzresult Lwt.t

  val read : state -> Durable_storage_path.path -> bytes option tzresult Lwt.t

  val subkeys : state -> Durable_storage_path.path -> string list tzresult Lwt.t
end

let inspect_durable_and_decode_opt read path decode =
  let open Lwt_result_syntax in
  let* bytes = read path in
  match bytes with
  | Some bytes -> return_some (decode bytes)
  | None -> return_none

let inspect_durable_and_decode_default ~default read path decode =
  let open Lwt_result_syntax in
  let* res_opt = inspect_durable_and_decode_opt read path decode in
  match res_opt with Some res -> return res | None -> return default

let inspect_durable_and_decode read path decode =
  let open Lwt_result_syntax in
  let* res_opt = inspect_durable_and_decode_opt read path decode in
  match res_opt with
  | Some res -> return res
  | None -> failwith "No value found under %s" path

let chain_id read =
  inspect_durable_and_decode
    read
    Durable_storage_path.chain_id
    L2_types.Chain_id.decode_le

let l2_minimum_base_fee_per_gas read chain_id =
  inspect_durable_and_decode
    read
    (Durable_storage_path.Chain_configuration.minimum_base_fee_per_gas chain_id)
    Helpers.decode_z_le

let l2_da_fee_per_byte read chain_id =
  inspect_durable_and_decode
    read
    (Durable_storage_path.Chain_configuration.da_fee_per_byte chain_id)
    Helpers.decode_z_le

let l2_maximum_gas_per_transaction read chain_id =
  inspect_durable_and_decode
    read
    (Durable_storage_path.Chain_configuration.maximum_gas_per_transaction
       chain_id)
    Helpers.decode_z_le

let chain_family read chain_id =
  inspect_durable_and_decode
    read
    (Durable_storage_path.Chain_configuration.chain_family chain_id)
    (fun x -> L2_types.Chain_family.of_string_exn (Bytes.to_string x))

let world_state read chain_id =
  inspect_durable_and_decode
    read
    (Durable_storage_path.Chain_configuration.world_state chain_id)
    Bytes.to_string

let sequencer read =
  inspect_durable_and_decode
    read
    Durable_storage_path.sequencer_key
    (fun bytes -> Signature.Public_key.of_b58check_exn (String.of_bytes bytes))

let storage_version read =
  inspect_durable_and_decode_default
    ~default:0
    read
    Durable_storage_path.storage_version
    (fun bytes -> Helpers.decode_z_le bytes |> Z.to_int)

let kernel_version read =
  inspect_durable_and_decode
    read
    Durable_storage_path.kernel_version
    Bytes.to_string

let kernel_root_hash read =
  inspect_durable_and_decode_opt
    read
    Durable_storage_path.kernel_root_hash
    Bytes.to_string

let is_multichain_enabled read =
  let open Lwt_result_syntax in
  let* bytes_opt = read Durable_storage_path.Feature_flags.multichain in
  return (Option.is_some bytes_opt)

let block_number ~root read n =
  let open Lwt_result_syntax in
  match n with
  (* This avoids an unecessary service call in case we ask a block's number
     with an already expected/known block number [n]. *)
  | Durable_storage_path.Block.Nth i -> return @@ Ethereum_types.Qty i
  | Durable_storage_path.Block.Current -> (
      let+ answer = read (Durable_storage_path.Block.current_number ~root) in
      match answer with
      | Some bytes -> Ethereum_types.Qty (Bytes.to_string bytes |> Z.of_bits)
      | None ->
          raise
          @@ Invalid_block_structure
               "Unexpected [None] value for [current_number]'s [answer]")

let list_runtimes read =
  let open Lwt_result_syntax in
  let check_runtime r =
    let* bytes_opt = read (Tezosx.feature_flag r) in
    if Option.is_some bytes_opt then return @@ Some r else return None
  in
  List.filter_map_ep check_runtime Tezosx.known_runtimes

module Make (Reader : READER) = struct
  let read_with_state ?block () =
    let open Lwt_result_syntax in
    let* state = Reader.get_state ?block () in
    return (Reader.read state)

  let chain_id () =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    chain_id read

  let is_multichain_enabled () =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    is_multichain_enabled read

  let l2_minimum_base_fee_per_gas chain_id =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    l2_minimum_base_fee_per_gas read chain_id

  let l2_da_fee_per_byte chain_id =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    l2_da_fee_per_byte read chain_id

  let l2_maximum_gas_per_transaction chain_id =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    l2_maximum_gas_per_transaction read chain_id

  let chain_family chain_id =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    chain_family read chain_id

  let world_state chain_id =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    world_state read chain_id

  let storage_version () =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    storage_version read

  let kernel_version () =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    kernel_version read

  let kernel_root_hash () =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    kernel_root_hash read

  let current_block_number ~root =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    block_number ~root read Durable_storage_path.Block.Current

  let list_runtimes () =
    let open Lwt_result_syntax in
    let* read = read_with_state () in
    list_runtimes read
end
