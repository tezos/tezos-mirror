(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

let rec wait_until_proposal_submitted () =
  let open Lwt_result_syntax in
  let* proposal_submission_outcome =
    Threshold_encryption_proposals_handler.submit_next_proposal @@ Misc.now ()
  in
  (* If no proposal was submitted then either the tx pool was locked,
     not enough time passed before the last proposal and the candidate proposal
     was empty, or the proposal handler is locked. *)
  match proposal_submission_outcome with
  | Threshold_encryption_types.Tx_pool_is_locked | Proposal_is_early ->
      (* The sequencer is not waiting to receive a preblock,
         but the request to send a proposal failed. It will wait a short
         period of time before retrying. *)
      let*! () = Lwt_unix.sleep 0.5 in
      wait_until_proposal_submitted ()
  | Proposal_submitted ->
      (* The preblocks monitor is waiting to deliver a preblock
         received from the dsn node. In this case, there is no need
         to submit a new proposal. *)
      return_unit

let loop_on_preblocks_stream preblocks_stream time_between_blocks =
  let open Lwt_result_syntax in
  let[@tailrec] rec go () =
    let*! preblock = Lwt_stream.next preblocks_stream in
    let*! () =
      Threshold_encryption_preblocks_monitor_events.received_preblock
        Threshold_encryption_types.(preblock.current_blueprint_number)
    in
    let* () =
      match time_between_blocks with
      | Evm_node_config.Configuration.Nothing ->
          (* [time_between_blocks = Nothing] is set for tests only.
             Some tests trigger invalid blueprints on purpose so we
             ignore errors during block production here. *)
          let*! _res =
            Threshold_encryption_block_producer.produce_block preblock
          in
          Threshold_encryption_proposals_handler.unlock_next_proposal ()
      | Time_between_blocks _ ->
          let* _n_tx =
            Threshold_encryption_block_producer.produce_block preblock
          in
          let* () =
            Threshold_encryption_proposals_handler.unlock_next_proposal ()
          in
          wait_until_proposal_submitted ()
    in
    (go [@tailcall]) ()
  in
  go ()

let start ~sidecar_endpoint ~time_between_blocks =
  let open Lwt_result_syntax in
  let*! () = Threshold_encryption_preblocks_monitor_events.started () in
  let* preblocks_stream =
    Threshold_encryption_services.monitor_preblocks ~sidecar_endpoint ()
  in
  (* When [time_between_blocks <> Nothing], we must make sure that we
     send a first proposal to the dsn node, otherwise the latter will not
     produce a preblock and the preblocks monitor will hang waiting for it. *)
  let* () =
    match time_between_blocks with
    | Configuration.Nothing -> return_unit
    | Time_between_blocks _ -> wait_until_proposal_submitted ()
  in
  loop_on_preblocks_stream preblocks_stream time_between_blocks
