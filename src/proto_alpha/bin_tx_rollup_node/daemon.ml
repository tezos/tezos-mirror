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

let parse_tx_rollup_l2_address :
    Script.node -> Protocol.Tx_rollup_l2_address.Indexable.value tzresult =
  let open Protocol in
  let open Micheline in
  function
  | Bytes (loc, bytes) (* As unparsed with [Optimized]. *) -> (
      match Tx_rollup_l2_address.of_bytes_opt bytes with
      | Some txa -> ok (Tx_rollup_l2_address.Indexable.value txa)
      | None -> error (Error.Tx_rollup_invalid_l2_address loc))
  | String (loc, str) (* As unparsed with [Readable]. *) -> (
      match Tx_rollup_l2_address.of_b58check_opt str with
      | Some txa -> ok (Tx_rollup_l2_address.Indexable.value txa)
      | None -> error (Error.Tx_rollup_invalid_l2_address loc))
  | Int (loc, _) | Prim (loc, _, _, _) | Seq (loc, _) ->
      error (Error.Tx_rollup_invalid_l2_address loc)

let parse_tx_rollup_amount_l2_destination_parameters :
    Script.expr ->
    (Protocol.Tx_rollup_l2_qty.t
    * Protocol.Script_typed_ir.tx_rollup_l2_address)
    tzresult =
 fun parameters ->
  let open Micheline in
  let open Protocol in
  (* /!\ This pattern matching needs to remain in sync with the deposit
     parameters. See the transaction to Tx_rollup case in
     Protocol.Apply.Apply.apply_internal_manager_operations *)
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
                    [_ticketer; Prim (_, D_Pair, [_contents; amount], _)],
                    _ );
                bls;
              ],
              _ );
          _ty;
        ] ) ->
      parse_tx_rollup_l2_address bls >>? fun destination ->
      (match amount with
      | Int (_, v) when Compare.Z.(Z.zero < v && v <= Z.of_int64 Int64.max_int)
        ->
          ok @@ Tx_rollup_l2_qty.of_int64_exn (Z.to_int64 v)
      | Int (_, invalid_amount) ->
          error (Error.Tx_rollup_invalid_ticket_amount invalid_amount)
      | _expr -> error Error.Tx_rollup_invalid_deposit)
      >|? fun amount -> (amount, destination)
  | _expr -> error Error.Tx_rollup_invalid_deposit

let extract_messages_from_block block_info rollup_id =
  let managed_operation =
    List.nth_opt
      block_info.Alpha_block_services.operations
      State.rollup_operation_index
  in
  let rec get_messages :
      type kind.
      source:public_key_hash ->
      kind manager_operation ->
      kind manager_operation_result ->
      packed_internal_manager_operation_result list ->
      Tx_rollup_message.t list * int ->
      Tx_rollup_message.t list * int =
   fun ~source op result internal_operation_results (messages, cumulated_size) ->
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
          parse_tx_rollup_amount_l2_destination_parameters parameters
          |> Result.to_option
          |> Option.map @@ fun (amount, destination) ->
             Tx_rollup_message.make_deposit
               source
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
      (fun acc (Internal_manager_operation_result ({operation; _}, result)) ->
        let operation = manager_operation_of_internal_operation operation in
        get_messages ~source operation result [] acc)
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
        ( Manager_operation {operation; source; _},
          Manager_operation_result
            {operation_result; internal_operation_results; _} ) ->
        get_messages
          ~source
          operation
          operation_result
          internal_operation_results
          acc
    | Single_and_result (_, _) -> acc
    | Cons_and_result
        ( Manager_operation {operation; source; _},
          Manager_operation_result
            {operation_result; internal_operation_results; _},
          rest ) ->
        let acc =
          get_messages
            ~source
            operation
            operation_result
            internal_operation_results
            acc
        in
        get_related_messages acc rest
  in
  let finalize_receipt acc operation =
    match Alpha_block_services.(operation.protocol_data, operation.receipt) with
    | ( Operation_data {contents = operation_contents; _},
        Receipt (Operation_metadata {contents = result_contents}) ) -> (
        match kind_equal_list operation_contents result_contents with
        | Some Eq ->
            let operation_and_result =
              pack_contents_list operation_contents result_contents
            in
            ok (get_related_messages acc operation_and_result)
        | None ->
            (* Should not happen *)
            ok acc)
    | (_, Receipt No_operation_metadata) | (_, Empty) | (_, Too_large) ->
        error (Tx_rollup_no_operation_metadata operation.hash)
  in
  match managed_operation with
  | None -> ok ([], 0)
  | Some managed_operations ->
      let open Result_syntax in
      let+ (rev_messages, cumulated_size) =
        List.fold_left_e finalize_receipt ([], 0) managed_operations
      in
      (List.rev rev_messages, cumulated_size)

let create_genesis_block state tezos_block =
  let open Lwt_syntax in
  let* ctxt = Context.init_context state.State.context_index in
  let* genesis_block =
    L2block.genesis_block ctxt state.rollup_info.rollup_id tezos_block
  in
  let+ _block_hash = State.save_block state genesis_block in
  (genesis_block, ctxt)

let process_messages_and_inboxes (state : State.t) ~(predecessor : L2block.t)
    ?predecessor_context block_info rollup_id =
  let open Lwt_result_syntax in
  let current_hash = block_info.Alpha_block_services.hash in
  let*? (messages, cumulated_size) =
    extract_messages_from_block block_info rollup_id
  in
  let*! () = Event.(emit messages_application) (List.length messages) in
  let* predecessor_context =
    match predecessor_context with
    | None -> Context.checkout state.context_index predecessor.header.context
    | Some context -> return context
  in
  let l2_parameters =
    Protocol.Tx_rollup_l2_apply.
      {
        tx_rollup_max_withdrawals_per_batch =
          state.l1_constants.tx_rollup_max_withdrawals_per_batch;
      }
  in
  let* (context, contents) =
    Interpreter.interpret_messages
      predecessor_context
      l2_parameters
      ~rejection_max_proof_size:
        state.l1_constants.tx_rollup_rejection_max_proof_size
      messages
  in
  match contents with
  | None ->
      (* No inbox at this block *)
      return (predecessor, predecessor_context)
  | Some contents ->
      let inbox = Inbox.{contents; cumulated_size} in
      let*! context_hash = Context.commit context in
      let level =
        match predecessor.header.level with
        | Genesis -> L2block.Rollup_level Tx_rollup_level.root
        | Rollup_level l -> Rollup_level (Tx_rollup_level.succ l)
      in
      let header : L2block.header =
        {
          level;
          tezos_block = current_hash;
          predecessor = predecessor.hash;
          context = context_hash;
        }
      in
      let hash = L2block.hash_header header in
      let block = L2block.{hash; header; inbox} in
      let*! () = State.save_block state block in
      let*! () =
        Event.(emit rollup_block) (header.level, hash, header.tezos_block)
      in
      return (block, context)

let rec process_block state current_hash rollup_id :
    (L2block.t * Context.context option, tztrace) result Lwt.t =
  let open Lwt_result_syntax in
  if Block_hash.equal state.State.rollup_info.origination_block current_hash
  then
    (* This is the rollup origination block, create L2 genesis block *)
    let*! (genesis_block, genesis_ctxt) =
      create_genesis_block state current_hash
    in
    return (genesis_block, Some genesis_ctxt)
  else
    let*! l2_block = State.get_tezos_l2_block state current_hash in
    match l2_block with
    | Some l2_block ->
        (* Already processed *)
        let*! () = Event.(emit block_already_processed) current_hash in
        let* context = checkout_context state l2_block.header.context in
        let* _l2_reorg = State.set_head state l2_block context in
        return (l2_block, Some context)
    | None ->
        let* block_info = State.fetch_tezos_block state current_hash in
        let predecessor_hash = block_info.header.shell.predecessor in
        let block_level = block_info.header.shell.level in
        let* () =
          fail_when
            (block_level < state.State.rollup_info.origination_level)
            Tx_rollup_originated_in_fork
        in
        (* Handle predecessor Tezos block first *)
        let*! () = Event.(emit processing_block_predecessor) predecessor_hash in
        let* (l2_predecessor_header, predecessor_context) =
          process_block state predecessor_hash rollup_id
        in
        let*! () =
          Event.(emit processing_block) (current_hash, predecessor_hash)
        in
        let* (l2_block, context) =
          process_messages_and_inboxes
            state
            ~predecessor:l2_predecessor_header
            ?predecessor_context
            block_info
            rollup_id
        in
        let*! () =
          State.save_tezos_block_info
            state
            current_hash
            l2_block.hash
            ~level:block_info.header.shell.level
            ~predecessor:block_info.header.shell.predecessor
        in
        let* _l2_reorg = State.set_head state l2_block context in
        let*! () = Event.(emit new_tezos_head) current_hash in
        let*! () = Event.(emit block_processed) (current_hash, block_level) in
        return (l2_block, Some context)

let maybe_batch_and_inject state =
  match state.State.batcher_state with
  | None -> ()
  | Some batcher_state -> Batcher.async_batch_and_inject batcher_state

let process_head state current_hash rollup_id =
  let open Lwt_result_syntax in
  let*! () = Event.(emit new_block) current_hash in
  let* res = process_block state current_hash rollup_id in
  let* _l1_reorg = State.set_tezos_head state current_hash in
  maybe_batch_and_inject state ;
  (* TODO/TORU: handle new head and reorgs w.r.t. injected operations by the
     rollup node, like commitments and rejections. *)
  return res

let main_exit_callback state exit_status =
  let open Lwt_syntax in
  let* () = Stores.close state.State.stores in
  let* () = Context.close state.State.context_index in
  let* () = Event.(emit node_is_shutting_down) exit_status in
  Tezos_base_unix.Internal_event_unix.close ()

let rec connect ~delay cctxt =
  let open Lwt_syntax in
  let* res = Monitor_services.heads cctxt cctxt#chain in
  match res with
  | Ok (stream, stopper) -> return_ok (stream, stopper)
  | Error _ ->
      let* () = Event.(emit cannot_connect) delay in
      let* () = Lwt_unix.sleep delay in
      connect ~delay cctxt

(* TODO/TORU: https://gitlab.com/tezos/tezos/-/issues/1845
   Clean exit *)
let run configuration cctxt =
  let open Lwt_result_syntax in
  let*! () = Event.(emit starting_node) () in
  let {
    Configuration.data_dir;
    rollup_id;
    rollup_genesis;
    operator;
    signers;
    reconnection_delay;
    l2_blocks_cache_size;
    _;
  } =
    configuration
  in
  let* state =
    State.init
      cctxt
      ~data_dir
      ~l2_blocks_cache_size
      ~operator
      ~signers
      ?rollup_genesis
      rollup_id
  in
  let* _rpc_server = RPC.start configuration state in
  let _ =
    (* Register cleaner callback *)
    Lwt_exit.register_clean_up_callback ~loc:__LOC__ (main_exit_callback state)
  in
  let*! () = Event.(emit node_is_ready) () in
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
                let*! r = process_head state current_hash rollup_id in
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
