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
  let cumulated_size =
    List.fold_left (fun acc x -> acc + String.length x) 0 messages
  in
  let make_batch msg = fst @@ Tx_rollup_message.make_batch msg in
  let contents = List.map make_batch messages in
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
  let*! has_previous_state = State.block_already_seen state predecessor_hash in
  let*? () =
    error_unless
      (has_previous_state || Block_hash.equal rollup_genesis current_hash)
      (Error.Tx_rollup_block_predecessor_not_processed predecessor_hash)
  in
  let messages = compute_messages block_info rollup_id in
  let messages_len = List.length messages in
  let inbox = messages_to_inbox messages in
  let* () = Event.messages_application messages_len in
  let* () = State.save_inbox state current_hash inbox in
  let* () =
    Event.inbox_stored
      ~block_hash:current_hash
      ~messages:inbox.contents
      ~cumulated_size:inbox.cumulated_size
  in
  return_unit

let rec process_hash cctxt state rollup_genesis current_hash rollup_id =
  let open Lwt_result_syntax in
  let chain = cctxt#chain in
  let block = `Hash (current_hash, 0) in
  let* block_info = Alpha_block_services.info cctxt ~chain ~block () in
  process_block cctxt state rollup_genesis block_info rollup_id

and process_block cctxt state rollup_genesis block_info rollup_id =
  let open Lwt_result_syntax in
  let current_hash = block_info.hash in
  let predecessor_hash = block_info.header.shell.predecessor in
  let*! was_processed = State.block_already_seen state current_hash in
  if was_processed then Event.block_already_seen current_hash
  else
    let*! predecessor_was_processed =
      State.block_already_seen state predecessor_hash
    in
    let* () =
      if
        (not predecessor_was_processed)
        && not (Block_hash.equal rollup_genesis current_hash)
      then
        let* () = Event.processing_block_predecessor predecessor_hash in
        process_hash cctxt state rollup_genesis predecessor_hash rollup_id
      else return ()
    in
    let* () =
      Event.processing_block ~block_hash:current_hash ~predecessor_hash
    in
    let* () =
      process_messages_and_inboxes state rollup_genesis block_info rollup_id
    in
    let* () = State.set_new_head state current_hash in
    let* () = Event.new_tezos_head current_hash in
    Event.block_processed current_hash

let process_inboxes cctxt state rollup_genesis current_hash rollup_id =
  let open Lwt_result_syntax in
  let* () = Event.new_block current_hash in
  let* () = process_hash cctxt state rollup_genesis current_hash rollup_id in
  return_unit

let main_exit_callback data_dir exit_status =
  let open Lwt_syntax in
  let* () = Stores.close data_dir in
  let* () = Event.node_is_shutting_down ~exit_status in
  Tezos_base_unix.Internal_event_unix.close ()

let rec connect ~delay cctxt =
  let open Lwt_syntax in
  let* res = Monitor_services.heads cctxt cctxt#chain in
  match res with
  | Ok (stream, stopper) -> Error_monad.return (stream, stopper)
  | Error _ ->
      let* () = Event.cannot_connect ~delay in
      let* () = Lwt_unix.sleep delay in
      connect ~delay cctxt

let valid_history_mode = function
  | History_mode.Archive | History_mode.Full _ -> true
  | _ -> false

let run ~data_dir cctxt =
  let open Lwt_result_syntax in
  let* () = Event.starting_node () in
  let* ({
          data_dir;
          rpc_addr;
          rpc_port;
          rollup_id;
          rollup_genesis;
          reconnection_delay;
          _;
        } as configuration) =
    Configuration.load ~data_dir
  in
  let* state =
    State.init ~data_dir ~context:cctxt ~rollup:rollup_id ~rollup_genesis
  in
  let* _rpc_server = RPC.start configuration state in
  let _ =
    (* Register cleaner callback *)
    Lwt_exit.register_clean_up_callback
      ~loc:__LOC__
      (main_exit_callback configuration.data_dir)
  in
  let* () = Event.irmin_store_loaded data_dir in
  let* () = Event.node_is_ready ~rpc_addr ~rpc_port in
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
          Lwt_stream.iter_s
            (fun (current_hash, _header) ->
              process_inboxes cctxt state rollup_genesis current_hash rollup_id
              >>= function
              | Ok () -> Lwt.return ()
              | Error _ ->
                  let () = interupt () in
                  Lwt.return ())
            block_stream
          >>= Event.connection_lost >>= loop)
        fail_with_exn
    in
    Lwt_utils.never_ending ()
  in
  loop ()
