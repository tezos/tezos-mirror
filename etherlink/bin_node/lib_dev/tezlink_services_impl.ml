(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_services

let current_level (module Backend : Services_backend_sig.S)
    (constants :
      Imported_protocol.Protocol.Alpha_context.Constants.Parametric.t) chain
    block level_query =
  let open Lwt_result_syntax in
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

let michelson_services_methods backend constants =
  {
    current_level = current_level backend constants;
    version =
      (fun () ->
        (* TODO: #7857 need proper implementation *)
        Lwt_result_syntax.return Tezlink_version.mock);
    protocols = (fun () -> Lwt_result_syntax.return Tezlink_protocols.current);
  }
