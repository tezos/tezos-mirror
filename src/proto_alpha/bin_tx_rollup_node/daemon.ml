(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Protocol.Apply_results
open Tezos_shell_services
open Protocol_client_context
open Protocol.Alpha_context

let messages_to_inbox messages =
  let (rev_contents, cumulated_size) =
    List.fold_left
      (fun (acc, cumulated_size) msg ->
        let (message, size) = Tx_rollup_message.make_batch msg in
        (* TODO/TORU apply message *)
        let message =
          Inbox.
            {
              message;
              result = Discarded [] (* TODO: Placeholder *);
              context_hash =
                Protocol.Tx_rollup_l2_context_hash.zero (* TODO: Placeholder *);
            }
        in
        (message :: acc, cumulated_size + size))
      ([], 0)
      messages
  in
  let contents = List.rev rev_contents in
  Inbox.{contents; cumulated_size}

let compute_messages block_info rollup_id =
  let managed_operation =
    List.nth_opt
      block_info.Alpha_block_services.operations
      State.rollup_operation_index
  in
  let rec get_related_messages :
      type kind. string list -> kind contents_and_result_list -> string list =
   fun acc -> function
    | Single_and_result
        ( Manager_operation
            {
              operation =
                Tx_rollup_submit_batch {tx_rollup; content; burn_limit = _};
              _;
            },
          Manager_operation_result
            {operation_result = Applied (Tx_rollup_submit_batch_result _); _} )
      when Tx_rollup.equal rollup_id tx_rollup ->
        List.rev (content :: acc)
    | Cons_and_result
        ( Manager_operation
            {
              operation =
                Tx_rollup_submit_batch {tx_rollup; content; burn_limit = _};
              _;
            },
          Manager_operation_result
            {operation_result = Applied (Tx_rollup_submit_batch_result _); _},
          xs )
      when Tx_rollup.equal rollup_id tx_rollup ->
        get_related_messages (content :: acc) xs
    | Single_and_result _ -> List.rev acc
    | Cons_and_result (_, _, xs) -> get_related_messages acc xs
  in
  let finalize_receipt operation =
    match Alpha_block_services.(operation.protocol_data, operation.receipt) with
    | ( Operation_data {contents = operation_contents; _},
        Some (Operation_metadata {contents = result_contents}) ) -> (
        match kind_equal_list operation_contents result_contents with
        | Some Eq ->
            let operation_and_result =
              pack_contents_list operation_contents result_contents
            in
            get_related_messages [] operation_and_result
        | None -> [])
    | (_, Some No_operation_metadata) | (_, None) -> []
  in
  match managed_operation with
  | None -> []
  | Some managed_operations ->
      (* We can use [List.concat_map] because we do not expect many batches per
         rollup and per block. *)
      managed_operations |> List.concat_map finalize_receipt

let process_messages_and_inboxes state rollup_genesis block_info rollup_id =
  let open Lwt_result_syntax in
  let current_hash = block_info.Alpha_block_services.hash in
  let predecessor_hash = block_info.header.shell.predecessor in
  let*! has_previous_state =
    State.tezos_block_already_seen state predecessor_hash
  in
  let*? () =
    error_unless
      (has_previous_state || Block_hash.equal rollup_genesis current_hash)
      (Error.Tx_rollup_block_predecessor_not_processed predecessor_hash)
  in
  let messages = compute_messages block_info rollup_id in
  let messages_len = List.length messages in
  let inbox = messages_to_inbox messages in
  let*! () = Event.(emit messages_application) messages_len in
  (* TODO/TORU: Build + save L2 block  *)
  let* () =
    State.save_inbox state L2block.Hash.zero (* TODO/TORU: Placeholder *) inbox
  in
  let*! () =
    Event.(emit inbox_stored)
      ( current_hash,
        List.map (fun m -> m.Inbox.message) inbox.contents,
        inbox.cumulated_size )
  in
  return_unit

let rec process_hash cctxt state current_hash rollup_id =
  let open Lwt_result_syntax in
  let chain = cctxt#chain in
  let block = `Hash (current_hash, 0) in
  let* block_info = Alpha_block_services.info cctxt ~chain ~block () in
  process_block cctxt state block_info rollup_id

and process_block cctxt state block_info rollup_id =
  let open Lwt_result_syntax in
  let current_hash = block_info.hash in
  let predecessor_hash = block_info.header.shell.predecessor in
  let*! was_processed = State.tezos_block_already_seen state current_hash in
  if block_info.header.shell.level < state.State.rollup_origination.block_level
  then
    (* We went back too far because the genesis block is in another branch *)
    fail [Tx_rollup_originated_in_fork]
  else if
    Block_hash.equal state.State.rollup_origination.block_hash current_hash
  then return_unit
  else if was_processed then
    let*! () = Event.(emit block_already_seen) current_hash in
    return_unit
  else
    let*! predecessor_was_processed =
      State.tezos_block_already_seen state predecessor_hash
    in
    let* () =
      if not predecessor_was_processed then
        let*! () = Event.(emit processing_block_predecessor) predecessor_hash in
        process_hash cctxt state predecessor_hash rollup_id
      else return ()
    in
    let*! () = Event.(emit processing_block) (current_hash, predecessor_hash) in
    let* () = process_messages_and_inboxes state block_info rollup_id in
    (* TODO/TORU: Set new L2 block head
       let* () = State.set_new_head state current_hash in *)
    let*! () = Event.(emit new_tezos_head) current_hash in
    let*! () = Event.(emit block_processed) current_hash in
    return_unit

let process_inboxes cctxt state current_hash rollup_id =
  let open Lwt_result_syntax in
  let*! () = Event.(emit new_block) current_hash in
  process_hash cctxt state current_hash rollup_id

let main_exit_callback state data_dir exit_status =
  let open Lwt_syntax in
  let* () = Stores.close data_dir in
  let* () = Context.close state.State.context_index in
  let* () = Event.(emit node_is_shutting_down) exit_status in
  Tezos_base_unix.Internal_event_unix.close ()

let rec connect ~delay cctxt =
  let open Lwt_syntax in
  let* res = Monitor_services.heads cctxt cctxt#chain in
  match res with
  | Ok (stream, stopper) -> Error_monad.return (stream, stopper)
  | Error _ ->
      let* () = Event.(emit cannot_connect) delay in
      let* () = Lwt_unix.sleep delay in
      connect ~delay cctxt

let valid_history_mode = function
  | History_mode.Archive | History_mode.Full _ -> true
  | _ -> false

(* TODO/TORU: https://gitlab.com/tezos/tezos/-/issues/2551
   Clean exit *)
let run ~data_dir cctxt =
  let open Lwt_result_syntax in
  let*! () = Event.(emit starting_node) () in
  let* ({data_dir; rollup_id; rollup_genesis; reconnection_delay; _} as
       configuration) =
    Configuration.load ~data_dir
  in
  let* state = State.init ~data_dir ~context:cctxt ?rollup_genesis rollup_id in
  let* _rpc_server = RPC.start configuration state in
  let _ =
    (* Register cleaner callback *)
    Lwt_exit.register_clean_up_callback
      ~loc:__LOC__
      (main_exit_callback state configuration.data_dir)
  in
  let* (_, _, _, history_mode) = Chain_services.checkpoint cctxt () in
  let* () =
    fail_unless
      (valid_history_mode history_mode)
      (Error.Tx_rollup_invalid_history_mode history_mode)
  in
  let rec loop () =
    let* () =
      Lwt.catch
        (fun () ->
          let* (block_stream, interupt) =
            connect ~delay:reconnection_delay cctxt
          in
          let*! () =
            Lwt_stream.iter_s
              (fun (current_hash, _header) ->
                let*! r = process_inboxes cctxt state current_hash rollup_id in
                match r with
                | Ok state -> Lwt.return ()
                | Error (Tx_rollup_originated_in_fork :: _ as e) ->
                    Format.eprintf "%a@.Exiting.@." pp_print_trace e ;
                    Lwt_exit.exit_and_raise 1
                | Error e ->
                    Format.eprintf "%a@." pp_print_trace e ;
                    let () = interupt () in
                    Lwt.return ())
              block_stream
          in
          let*! () = Event.(emit connection_lost) () in
          loop ())
        fail_with_exn
    in
    Lwt_utils.never_ending ()
  in
  loop ()
