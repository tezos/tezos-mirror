(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let read_from_rollup_node path level rollup_node_endpoint =
  let open Rollup_services in
  call_service
    ~base:rollup_node_endpoint
    durable_state_value
    ((), Block_id.Level level)
    {key = path}
    ()

let advertize_blueprints_publisher rollup_node_endpoint finalized_level =
  let open Lwt_result_syntax in
  let* finalized_current_number =
    read_from_rollup_node
      Durable_storage_path.Block.current_number
      finalized_level
      rollup_node_endpoint
  in
  match finalized_current_number with
  | Some bytes ->
      let (Qty evm_block_number) = Ethereum_types.decode_number bytes in
      let* () = Blueprints_publisher.new_l2_head evm_block_number in
      return_unit
  | None -> return_unit

let process_new_block ~rollup_node_endpoint block =
  let open Lwt_result_syntax in
  let finalized_level = Sc_rollup_block.(Int32.(sub block.header.level 2l)) in
  let* () = Evm_events_follower.new_rollup_block finalized_level in
  let* () =
    advertize_blueprints_publisher rollup_node_endpoint finalized_level
  in
  let* () = Tx_pool.pop_and_inject_transactions_lazy () in
  return_unit

let rec process_rollup_node_stream ~stream ~rollup_node_endpoint =
  let open Lwt_result_syntax in
  let*! new_head = Lwt_stream.get stream in
  match new_head with
  | None ->
      let*! () = Rollup_node_follower_events.connection_lost () in
      (* In following commit we we'll try to recover the connection *)
      Lwt_exit.exit_and_raise 1
  | Some block ->
      let*! () =
        Rollup_node_follower_events.new_block
          Sc_rollup_block.(block.header.level)
      in
      let* () = process_new_block ~rollup_node_endpoint block in
      process_rollup_node_stream ~stream ~rollup_node_endpoint

let wait_first_valid_level_then_process ~rollup_node_endpoint ~stream =
  let open Lwt_result_syntax in
  let* first_rollup_node_known_l1_level =
    Rollup_services.oldest_known_l1_level rollup_node_endpoint
  in
  let rec aux () =
    let*! new_head = Lwt_stream.get stream in
    match new_head with
    | None ->
        let*! () = Rollup_node_follower_events.connection_lost () in
        (* In following commit we we'll try to recover the connection *)
        Lwt_exit.exit_and_raise 1
    | Some block ->
        let finalized_level =
          Sc_rollup_block.(Int32.(sub block.header.level 2l))
        in
        if first_rollup_node_known_l1_level <= finalized_level then
          let* () = process_new_block ~rollup_node_endpoint block in
          process_rollup_node_stream ~stream ~rollup_node_endpoint
        else aux ()
  in
  aux ()

let start ~rollup_node_endpoint =
  Lwt.async @@ fun () ->
  let open Lwt_syntax in
  let* () = Rollup_node_follower_events.started () in
  let* stream = Rollup_services.make_streamed_call ~rollup_node_endpoint in
  let* res =
    wait_first_valid_level_then_process ~rollup_node_endpoint ~stream
  in
  match res with
  | Ok () -> return_unit
  | Error errs ->
      Lwt.fail_with (Format.asprintf "%a" Error_monad.pp_print_trace errs)
