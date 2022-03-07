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
open Error

(* TODO/TORU: Move application logic in other module *)

let checkout_context (state : State.t) ctxt_hash =
  let open Lwt_syntax in
  let+ context = Context.checkout state.context_index ctxt_hash in
  Option.to_result ~none:[Tx_rollup_cannot_checkout_context ctxt_hash] context

let interp_messages ctxt messages cumulated_size =
  let open Lwt_syntax in
  let+ (ctxt, _ctxt_hash, rev_contents) =
    List.fold_left_s
      (fun (ctxt, ctxt_hash, acc) message ->
        let+ apply_res = Apply.apply_message ctxt message in
        let (ctxt, ctxt_hash, result) =
          match apply_res with
          | Ok (ctxt, result) ->
              (* The message was successfully interpreted but the status in
                 [result] may indicate that the application failed. The context
                 may have been modified with e.g. updated counters. *)
              (ctxt, Context.hash ctxt, Inbox.Interpreted result)
          | Error err ->
              (* The message was discarded before attempting to interpret it. The
                 context is not modified. For instance if a batch is unparsable,
                 or the BLS signature is incorrect, or a counter is wrong, etc. *)
              (ctxt, ctxt_hash, Inbox.Discarded err)
        in
        let inbox_message = {Inbox.message; result; context_hash = ctxt_hash} in
        (ctxt, ctxt_hash, inbox_message :: acc))
      (ctxt, Context.hash ctxt, [])
      messages
  in
  match rev_contents with
  | [] -> (ctxt, None)
  | _ ->
      let contents = List.rev rev_contents in
      let inbox = Inbox.{contents; cumulated_size} in
      (ctxt, Some inbox)

(* TODO/TORU return proper errors or option *)
let parse_tx_rollup_l2_address :
    Script.node -> Protocol.Tx_rollup_l2_address.Indexable.value tzresult =
  let open Protocol in
  let open Micheline in
  function
  | Bytes (_loc, bytes) (* As unparsed with [Optimized]. *) -> (
      match Tx_rollup_l2_address.of_bytes_opt bytes with
      | Some txa -> ok (Tx_rollup_l2_address.Indexable.value txa)
      | None -> error_with "Not a valid transaction rollup L2 address")
  | String (_loc, str) (* As unparsed with [Readable]. *) -> (
      match Tx_rollup_l2_address.of_b58check_opt str with
      | Some txa -> ok (Tx_rollup_l2_address.Indexable.value txa)
      | None -> error_with "Not a valid transaction rollup L2 address")
  | _expr -> error_with "Not a valid transaction rollup L2 address"

(* TODO/TORU: return proper errors or option *)
(* TODO/TORU: expose uncarbonated parse_tx_rollup_deposit_parameters in protocol *)
let parse_tx_rollup_deposit_parameters :
    Script.expr -> Tx_rollup.deposit_parameters tzresult =
 fun parameters ->
  let open Micheline in
  let open Protocol in
  (* /!\ This pattern matching needs to remain in sync with the
     Script_ir_translator.parse_tx_rollup_deposit_parameters. *)
  match root parameters with
  | Seq
      ( _,
        [
          Prim
            ( _,
              D_Pair,
              [
                Prim
                  ( _,
                    D_Pair,
                    [ticketer; Prim (_, D_Pair, [contents; amount], _)],
                    _ );
                bls;
              ],
              _ );
          ty;
        ] ) ->
      parse_tx_rollup_l2_address bls >>? fun destination ->
      (match amount with
      | Int (_, v) when Compare.Z.(Z.zero < v && v <= Z.of_int64 Int64.max_int)
        ->
          ok @@ Tx_rollup_l2_qty.of_int64_exn (Z.to_int64 v)
      | Int (_, _) -> error_with "Tx_rollup_invalid_ticket_amount"
      | _expr -> error_with "Invalid deposit")
      >|? fun amount -> Tx_rollup.{ticketer; contents; ty; amount; destination}
  | _expr -> error_with "Invalid deposit"

let extract_messages_from_block block_info rollup_id =
  let managed_operation =
    List.nth_opt
      block_info.Alpha_block_services.operations
      State.rollup_operation_index
  in
  let rec get_messages :
      type kind.
      kind manager_operation ->
      kind manager_operation_result ->
      packed_internal_operation_result list ->
      Tx_rollup_message.t list * int ->
      Tx_rollup_message.t list * int =
   fun op result internal_operation_results (messages, cumulated_size) ->
    let message_and_size =
      match (op, result) with
      | ( Tx_rollup_submit_batch {tx_rollup; content; burn_limit = _},
          Applied (Tx_rollup_submit_batch_result _) )
        when Tx_rollup.equal rollup_id tx_rollup ->
          (* Batch message *)
          Some (Tx_rollup_message.make_batch content)
      | ( Transaction
            {amount = _; parameters; destination = Tx_rollup dst; entrypoint},
          Applied
            (Transaction_result
              (Transaction_to_tx_rollup_result {ticket_hash; _})) )
        when Tx_rollup.equal dst rollup_id
             && Entrypoint.(entrypoint = Tx_rollup.deposit_entrypoint) ->
          (* Deposit message *)
          Option.bind (Data_encoding.force_decode parameters)
          @@ fun parameters ->
          parse_tx_rollup_deposit_parameters parameters
          |> Result.to_option
          |> Option.map @@ fun Tx_rollup.{amount; destination; _} ->
             Tx_rollup_message.make_deposit
               destination
               ticket_hash
               amount
      | (_, _) -> None
    in
    let acc =
      match message_and_size with
      | None -> (messages, cumulated_size)
      | Some (msg, size) -> (msg :: messages, cumulated_size + size)
    in
    (* Add messages from internal operations *)
    List.fold_left
      (fun acc (Internal_operation_result ({operation; _}, result)) ->
        get_messages operation result [] acc)
      acc
      internal_operation_results
  in
  let rec get_related_messages :
      type kind.
      Tx_rollup_message.t list * int ->
      kind contents_and_result_list ->
      Tx_rollup_message.t list * int =
   fun acc -> function
    | Single_and_result
        ( Manager_operation {operation; _},
          Manager_operation_result
            {operation_result; internal_operation_results; _} ) ->
        get_messages operation operation_result internal_operation_results acc
    | Single_and_result (_, _) -> acc
    | Cons_and_result
        ( Manager_operation {operation; _},
          Manager_operation_result
            {operation_result; internal_operation_results; _},
          rest ) ->
        let acc =
          get_messages operation operation_result internal_operation_results acc
        in
        get_related_messages acc rest
  in
  let finalize_receipt acc operation =
    match Alpha_block_services.(operation.protocol_data, operation.receipt) with
    | ( Operation_data {contents = operation_contents; _},
        Some (Operation_metadata {contents = result_contents}) ) -> (
        match kind_equal_list operation_contents result_contents with
        | Some Eq ->
            let operation_and_result =
              pack_contents_list operation_contents result_contents
            in
            get_related_messages acc operation_and_result
        | None -> acc)
    | (_, Some No_operation_metadata) | (_, None) -> acc
  in
  match managed_operation with
  | None -> ([], 0)
  | Some managed_operations ->
      let (rev_messages, cumulated_size) =
        List.fold_left finalize_receipt ([], 0) managed_operations
      in
      (List.rev rev_messages, cumulated_size)

let create_genesis_block state tezos_block =
  let open Lwt_result_syntax in
  let ctxt = Context.empty state.State.context_index in
  let*! context_hash = Context.commit ctxt in
  let inbox_hash = Tx_rollup_inbox.hash_inbox [] in
  let header : L2block.header =
    {
      level = Genesis;
      inbox_hash;
      tezos_block;
      predecessor = L2block.genesis_hash;
      context = context_hash;
    }
  in
  let inbox : Inbox.t = {contents = []; cumulated_size = 0} in
  let genesis_block = L2block.{header; inbox} in
  let+ _block_hash = State.save_block state genesis_block in
  (genesis_block, ctxt)

let process_messages_and_inboxes (state : State.t)
    ~(predecessor : L2block.header) ?predecessor_context block_info rollup_id =
  let open Lwt_result_syntax in
  let current_hash = block_info.Alpha_block_services.hash in
  let (messages, cumulated_size) =
    extract_messages_from_block block_info rollup_id
  in
  let*! () = Event.(emit messages_application) (List.length messages) in
  let* predecessor_context =
    match predecessor_context with
    | None -> checkout_context state predecessor.context
    | Some context -> return context
  in
  let*! (context, inbox) =
    interp_messages predecessor_context messages cumulated_size
  in
  match inbox with
  | None ->
      (* No inbox at this block *)
      return (predecessor, predecessor_context)
  | Some inbox ->
      let*! context_hash = Context.commit context in
      let inbox_hash = Inbox.hash_contents inbox.contents in
      let level =
        match predecessor.level with
        | Genesis -> L2block.Rollup_level Tx_rollup_level.root
        | Rollup_level l -> Rollup_level (Tx_rollup_level.succ l)
      in
      let header : L2block.header =
        {
          level;
          inbox_hash;
          tezos_block = current_hash;
          predecessor = L2block.hash_header predecessor;
          context = context_hash;
        }
      in
      let block = L2block.{header; inbox} in
      let* hash = State.save_block state block in
      let*! () =
        Event.(emit rollup_block) (header.level, hash, header.tezos_block)
      in
      return (block.header, context)

let rec process_block cctxt state current_hash rollup_id :
    (L2block.header * Context.context option, tztrace) result Lwt.t =
  let open Lwt_result_syntax in
  if Block_hash.equal state.State.rollup_origination.block_hash current_hash
  then
    (* This is the rollup origination block, create L2 genesis block *)
    let+ (genesis_block, genesis_ctxt) =
      create_genesis_block state current_hash
    in
    (genesis_block.header, Some genesis_ctxt)
  else
    let*! l2_header = State.get_tezos_l2_block state current_hash in
    match l2_header with
    | Some l2_header ->
        (* Already processed *)
        let*! () = Event.(emit block_already_processed) current_hash in
        let* () = State.set_head state l2_header in
        return (l2_header, None)
    | None ->
        let* block_info =
          Alpha_block_services.info
            cctxt
            ~chain:cctxt#chain
            ~block:(`Hash (current_hash, 0))
            ()
        in
        let predecessor_hash = block_info.header.shell.predecessor in
        let* () =
          fail_when
            (block_info.header.shell.level
           < state.State.rollup_origination.block_level)
            Tx_rollup_originated_in_fork
        in
        (* Handle predecessor Tezos block first *)
        let*! () = Event.(emit processing_block_predecessor) predecessor_hash in
        let* (l2_predecessor_header, predecessor_context) =
          process_block cctxt state predecessor_hash rollup_id
        in
        let*! () =
          Event.(emit processing_block) (current_hash, predecessor_hash)
        in
        let* (l2_header, context) =
          process_messages_and_inboxes
            state
            ~predecessor:l2_predecessor_header
            ?predecessor_context
            block_info
            rollup_id
        in
        let* () = State.set_head state l2_header in
        let*! () = Event.(emit new_tezos_head) current_hash in
        let*! () = Event.(emit block_processed) current_hash in
        return (l2_header, Some context)

let process_head cctxt state current_hash rollup_id =
  let open Lwt_result_syntax in
  let*! () = Event.(emit new_block) current_hash in
  process_block cctxt state current_hash rollup_id

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

(* TODO/TORU: https://gitlab.com/tezos/tezos/-/issues/1845
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
                let*! r = process_head cctxt state current_hash rollup_id in
                match r with
                | Ok (_, _) -> Lwt.return ()
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
