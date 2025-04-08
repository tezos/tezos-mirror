(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_services

let current_level (module Backend : Services_backend_sig.S) chain block
    level_query =
  let open Lwt_result_syntax in
  let constants = Tezlink_constants.mainnet in

  let* offset =
    (* Tezos l1 requires non-negative offset #7845 *)
    if level_query.offset >= 0l then return level_query.offset
    else failwith "The specified level offset should be positive."
  in

  (* TODO: #7831
     take chain into account
     For the moment this implementation only supports the main chain, once
     the rpc support of tezlink is more stable, we can add support for other chains *)
  let* () =
    match chain with `Main -> return_unit | _ -> failwith "Unsupported chain"
  in

  (* TODO: #7831
     take block into account
     For the moment this implementation only supports the head block, once
     the rpc support of tezlink is more stable, we can add support for other blocks *)
  let* () =
    match block with
    | `Head _ -> return_unit
    | _ -> failwith "Unsupported block"
  in

  let* (Qty current_block_number) =
    Backend.block_param_to_block_number (Block_parameter Latest)
  in

  let current_block_number = Z.to_int32 current_block_number in

  let level = Int32.add current_block_number offset in
  return
    {
      level;
      cycle = Int32.div level constants.blocks_per_cycle;
      cycle_position = Int32.rem level constants.blocks_per_cycle;
    }

let constants chain block =
  let open Lwt_result_syntax in
  (* TODO: #7831
     take chain into account
     For the moment this implementation only supports the main chain, once
     the rpc support of tezlink is more stable, we can add support for other chains *)
  let* () =
    match chain with `Main -> return_unit | _ -> failwith "Unsupported chain"
  in

  (* TODO: #7831
     take block into account
     For the moment this implementation only supports the head block, once
     the rpc support of tezlink is more stable, we can add support for other blocks *)
  let* () =
    match block with
    | `Head _ -> return_unit
    | _ -> failwith "Unsupported block"
  in

  let fixed_values =
    Tezlink_constants.
      ( ( proof_of_work_nonce_size,
          nonce_length,
          max_anon_ops_per_block,
          max_operation_data_length,
          max_proposals_per_delegate,
          max_micheline_node_count,
          max_micheline_bytes_limit,
          max_allowed_global_constant_depth,
          cache_layout_size,
          michelson_maximum_type_size ),
        ( max_slashing_period,
          sc_max_wrapped_proof_binary_size,
          sc_rollup_message_size_limit,
          sc_rollup_max_number_of_messages_per_level ) )
  in

  let* fixed =
    match
      Tezos_services.Protocol_types.Constants.values_to_fixed fixed_values
    with
    | Ok fixed -> return fixed
    | Error err ->
        failwith
          "Failed to get fixed constants: %a"
          Error_monad.pp_print_trace
          err
  in
  return
    Tezos_services.Protocol_types.Constants.
      {fixed; parametric = Tezlink_constants.mainnet}

let michelson_services_methods backend =
  {
    current_level = current_level backend;
    version =
      (fun () ->
        (* TODO: #7857 need proper implementation *)
        Lwt_result_syntax.return Tezlink_version.mock);
    protocols = (fun () -> Lwt_result_syntax.return Tezlink_protocols.current);
    balance =
      (fun _ _ _ ->
        Lwt_result_syntax.return @@ Ethereum_types.quantity_of_z Z.one);
    constants;
  }
