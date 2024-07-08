(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

let[@tailrec] rec loop_on_preblocks_stream preblocks_stream ~time_between_blocks
    =
  let open Lwt_result_syntax in
  let*! preblock = Lwt_stream.next preblocks_stream in
  let* () =
    match time_between_blocks with
    | Configuration.Nothing ->
        (* [time_between_blocks = Nothing] is set for tests only.
           Some tests trigger invalid blueprints on purpose so we
           ignore errors during block production here. *)
        let*! _res =
          Threshold_encryption_block_producer.produce_block preblock
        in
        return_unit
    | Time_between_blocks _time_between_blocks ->
        let* _ntx =
          Threshold_encryption_block_producer.produce_block preblock
        in
        return_unit
  in
  let* () = Threshold_encryption_proposals_handler.unlock_next_proposal () in
  let* () =
    match time_between_blocks with
    | Configuration.Nothing -> return_unit
    | Time_between_blocks _time_between_blocks ->
        Threshold_encryption_proposals_handler.submit_next_proposal
        @@ Misc.now ()
  in
  (loop_on_preblocks_stream [@tailcall]) preblocks_stream ~time_between_blocks

let start ~sidecar_endpoint ~time_between_blocks =
  Lwt.async @@ fun () ->
  Misc.unwrap_error_monad @@ fun () ->
  let open Lwt_result_syntax in
  let* preblocks_stream =
    Threshold_encryption_services.monitor_preblocks ~sidecar_endpoint ()
  in
  loop_on_preblocks_stream preblocks_stream ~time_between_blocks
