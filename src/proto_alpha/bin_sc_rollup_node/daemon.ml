(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

type head_state = {head : Layer1.head; finalized : bool; seen_before : bool}

let emit_head_processing_event
    {head = Head {hash; level}; finalized; seen_before} =
  Daemon_event.head_processing hash level finalized seen_before

let emit_heads_not_processed_event head_states =
  Lwt_list.iter_s
    (fun {head = Head {hash; level}; _} ->
      Daemon_event.not_finalized_head hash level)
    head_states

let categorise_heads (node_ctxt : Node_context.t) old_heads new_heads =
  (* For each head, determine if it has already been seen before and if it has
     been finalized, using the block finality time (for Tenderbake, this
     is 2).
  *)

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/2868
     Handle protocols with non-deterministic finality. *)
  let all_heads = old_heads @ new_heads in
  let number_of_temporary_heads =
    min node_ctxt.block_finality_time (List.length all_heads)
  in

  let number_of_new_heads = List.length new_heads in

  let head_states, _, _ =
    List.fold_right
      (fun head (heads, n, m) ->
        ({head; finalized = n <= 0; seen_before = m <= 0} :: heads, n - 1, m - 1))
      all_heads
      ([], number_of_temporary_heads, number_of_new_heads)
  in
  head_states

module Make (PVM : Pvm.S) = struct
  module Components = Components.Make (PVM)
  open Protocol
  open Alpha_context
  open Apply_results

  (** Process an L1 SCORU operation (for the node's rollup) which is included
      for the first time. {b Note}: this function does not process inboxes for
      the rollup, which is done instead by {!Inbox.process_head}. *)
  let process_included_l1_operation (type kind) (node_ctxt : Node_context.t)
      head ~source:_ (operation : kind manager_operation)
      (result : kind successful_manager_operation_result) =
    let open Lwt_result_syntax in
    match (operation, result) with
    | ( Sc_rollup_publish {commitment; _},
        Sc_rollup_publish_result {published_at_level; _} ) ->
        (* Published commitment --------------------------------------------- *)
        let commitment_hash =
          Sc_rollup.Commitment.hash_uncarbonated commitment
        in
        let*! () =
          Store.Commitments_published_at_level.add
            node_ctxt.store
            commitment_hash
            published_at_level
        in
        return_unit
    | Sc_rollup_cement {commitment; _}, Sc_rollup_cement_result {inbox_level; _}
      ->
        (* Cemented commitment ---------------------------------------------- *)
        let*! () =
          Store.Last_cemented_commitment_level.set node_ctxt.store inbox_level
        in
        let*! () =
          Store.Last_cemented_commitment_hash.set node_ctxt.store commitment
        in
        return_unit
    | ( Sc_rollup_refute _,
        Sc_rollup_refute_result
          {game_status = Ended (Loser {reason; loser}); balance_updates; _} )
      when Node_context.is_operator node_ctxt loser ->
        let*? slashed_amount =
          List.fold_left_e
            (fun slashed -> function
              | ( Receipt.Sc_rollup_refutation_punishments,
                  Receipt.Credited amount,
                  _ ) ->
                  Environment.wrap_tzresult Tez.(slashed +? amount)
              | _ -> Ok slashed)
            Tez.zero
            balance_updates
        in
        tzfail (Sc_rollup_node_errors.Lost_game (loser, reason, slashed_amount))
    | Dal_publish_slot_header {slot_header}, Dal_publish_slot_header_result _ ->
        let {Dal.Slot.Header.id = {index; _}; _} = slot_header in
        (* DAL/FIXME: https://gitlab.com/tezos/tezos/-/issues/3510
           We store slot headers for all slots. In practice,
           it would be convenient to store only information about
           headers of slots to which the rollup node is subscribed to.
           We do not have the information about DAL slots subscribed to
           at this time. *)
        let*! () =
          Store.Dal_slots_headers.add
            node_ctxt.store
            ~primary_key:head
            ~secondary_key:index
            slot_header
        in
        return_unit
    | _, _ ->
        (* Other manager operations *)
        return_unit

  (** Process an L1 SCORU operation (for the node's rollup) which is finalized
      for the first time. *)
  let process_finalized_l1_operation (type kind) _node_ctxt _head ~source:_
      (_operation : kind manager_operation)
      (_result : kind successful_manager_operation_result) =
    return_unit

  let process_l1_operation (type kind) ~finalized node_ctxt head ~source
      (operation : kind manager_operation)
      (result : kind Apply_results.manager_operation_result) =
    let open Lwt_result_syntax in
    let is_for_my_rollup : type kind. kind manager_operation -> bool = function
      | Sc_rollup_add_messages {rollup; _}
      | Sc_rollup_cement {rollup; _}
      | Sc_rollup_publish {rollup; _}
      | Sc_rollup_refute {rollup; _}
      | Sc_rollup_timeout {rollup; _}
      | Sc_rollup_execute_outbox_message {rollup; _}
      | Sc_rollup_recover_bond {sc_rollup = rollup}
      | Sc_rollup_dal_slot_subscribe {rollup; _} ->
          Sc_rollup.Address.(rollup = node_ctxt.Node_context.rollup_address)
      | Dal_publish_slot_header _ -> true
      | Reveal _ | Transaction _ | Origination _ | Delegation _
      | Update_consensus_key _ | Register_global_constant _
      | Set_deposits_limit _ | Increase_paid_storage _ | Tx_rollup_origination
      | Tx_rollup_submit_batch _ | Tx_rollup_commit _ | Tx_rollup_return_bond _
      | Tx_rollup_finalize_commitment _ | Tx_rollup_remove_commitment _
      | Tx_rollup_rejection _ | Tx_rollup_dispatch_tickets _ | Transfer_ticket _
      | Sc_rollup_originate _ | Zk_rollup_origination _ | Zk_rollup_publish _ ->
          false
    in
    if not (is_for_my_rollup operation) then return_unit
    else
      (* Only look at operations that are for the node's rollup *)
      let*! () = Daemon_event.included_operation ~finalized operation result in
      match result with
      | Applied success_result ->
          let process =
            if finalized then process_finalized_l1_operation
            else process_included_l1_operation
          in
          process node_ctxt head ~source operation success_result
      | _ ->
          (* No action for non successful operations  *)
          return_unit

  let process_l1_block_operations ~finalized node_ctxt (Layer1.Head {hash; _}) =
    let open Lwt_result_syntax in
    let* block = Layer1.fetch_tezos_block node_ctxt.Node_context.l1_ctxt hash in
    let apply (type kind) accu ~source (operation : kind manager_operation)
        result =
      let open Lwt_result_syntax in
      let* () = accu in
      process_l1_operation ~finalized node_ctxt hash ~source operation result
    in
    let apply_internal (type kind) accu ~source:_
        (_operation : kind Apply_internal_results.internal_operation)
        (_result : kind Apply_internal_results.internal_operation_result) =
      accu
    in
    let* () =
      Layer1_services.process_manager_operations
        return_unit
        block.operations
        {apply; apply_internal}
    in
    return_unit

  let process_head node_ctxt head_state =
    let open Lwt_result_syntax in
    let {finalized; seen_before; head} = head_state in
    let* () =
      let*! () = emit_head_processing_event head_state in
      (* Avoid processing inbox again if it has been processed before for this head *)
      if seen_before then
        if finalized then process_l1_block_operations ~finalized node_ctxt head
        else return_unit
      else
        let* ctxt = Inbox.process_head node_ctxt head in
        let* () = Dal_slots_tracker.process_head node_ctxt head in
        let* () = process_l1_block_operations ~finalized node_ctxt head in
        (* Avoid storing and publishing commitments if the head is not final *)
        (* Avoid triggering the pvm execution if this has been done before for this head *)
        Components.Interpreter.process_head node_ctxt ctxt head
    in
    let* () =
      when_ finalized @@ fun () ->
      Components.Commitment.process_head node_ctxt head
    in
    (* Publishing a commitment when one is available does not depend on the state of
       the current head. *)
    let* () = Components.Commitment.publish_commitment node_ctxt in
    let* () =
      Components.Commitment.cement_commitment_if_possible node_ctxt head
    in
    let* () =
      (* At each block, there may be some refutation related actions to
         be performed. *)
      when_ finalized @@ fun () ->
      Components.Refutation_game.process head node_ctxt
    in
    when_ finalized (fun () ->
        let*! () = Layer1.mark_processed_head node_ctxt.store head in
        return ())

  let notify_injector {Node_context.l1_ctxt; store; _} chain_event =
    let open Lwt_result_syntax in
    let open Layer1 in
    let hash = chain_event_head_hash chain_event in
    let* head = fetch_tezos_block l1_ctxt hash in
    let* reorg = get_tezos_reorg_for_new_head l1_ctxt store hash in
    let*! () = Injector.new_tezos_head head reorg in
    return_unit

  (* [on_layer_1_chain_event node_ctxt store chain_event] processes a
     list of heads, coming either from a list of [old_heads] persisted in the
     store, or from the current [chain_event]. [old_heads] is a list of heads
     that have not been recognised as finalised by the rollup node. This list
     has been set by the last iteration of [on_layer_1_chain_event] in
     the {!daemonize} function, or it is the empty list if the rollup node is
     executing [on_layer_1_chain_event] for the very first time.
     These are heads included in
     the branch currently tracked by the rollup node, and that
     have only been partially processed, due to the rollup  node not being able
     to establish their finality. The function persists to disk the list of
     heads from the current branch tracked by the rollup node,
     whose finality cannot be established at the time the function is invoked.
     Those heads will be processed again at the next iteration of
     [on_layer_1_chain_event] in the [daemonize] function. If [chain_event] is
     a rollback event, then the list of heads persisted to disk is reset to the
     empty list, as the rollup node started tracking a new branch.
     Because heads that still have not been processed as finalized are
     persisted to disk, this function is robust against interruptions.
  *)
  let on_layer_1_chain_event node_ctxt chain_event =
    let open Lwt_result_syntax in
    let*! old_heads =
      Layer1.get_heads_not_finalized node_ctxt.Node_context.store
    in
    let open Layer1 in
    let* () =
      (* Get information about the last cemented commitment to determine the
         commitment (if any) to publish next. We do this only once per
         chain event to avoid spamming the layer1 node. *)
      Components.Commitment.sync_last_cemented_commitment_hash_with_level
        node_ctxt
    in
    let* () = notify_injector node_ctxt chain_event in
    let* non_final_heads =
      match chain_event with
      | SameBranch {new_head; intermediate_heads} ->
          let new_heads = intermediate_heads @ [new_head] in
          let*! () =
            Daemon_event.processing_heads_iteration old_heads new_heads
          in
          let head_states = categorise_heads node_ctxt old_heads new_heads in
          let* () = List.iter_es (process_head node_ctxt) head_states in
          (* Return new_head to be processed as finalized head if the
             next chain event is of type SameBranch.
          *)
          let non_final_head_states =
            List.filter (fun head_state -> not head_state.finalized) head_states
          in
          let*! () = emit_heads_not_processed_event non_final_head_states in
          let non_final_heads =
            List.map (fun head_state -> head_state.head) non_final_head_states
          in
          return non_final_heads
      | Rollback {new_head = Layer1.Head {level = new_level; _}} ->
          (* The new_head of the rollback event corresponds to a head that
             was previously finalized. Heads in `old_heads` that have a level
             preceding or equal to `new_level` can now be considered final,
             and will be processed as such. `new_level` can now be considered
             as such. Heads in `old_heads` whose level is greater than
             `new_level` can be safely discarded.
          *)
          let final_heads, _non_final_heads =
            List.partition
              (fun head ->
                let (Layer1.Head {level; _}) = head in
                level <= new_level)
              old_heads
          in
          let+ () =
            List.iter_es
              (fun head ->
                process_head
                  node_ctxt
                  {head; finalized = true; seen_before = true})
              final_heads
          in
          []
    in
    let*! () = Layer1.set_heads_not_finalized node_ctxt.store non_final_heads in
    let*! () = Layer1.processed chain_event in
    let*! () = Injector.inject () in
    return_unit

  let is_connection_error trace =
    TzTrace.fold
      (fun yes error ->
        yes
        ||
        match error with
        | Tezos_rpc_http.RPC_client_errors.(
            Request_failed {error = Connection_failed _; _}) ->
            true
        | _ -> false)
      false
      trace

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/2895
     Use Lwt_stream.fold_es once it is exposed. *)
  let daemonize configuration (node_ctxt : Node_context.t) =
    let open Lwt_result_syntax in
    let rec loop (l1_ctxt : Layer1.t) =
      let*! () =
        Lwt_stream.iter_s
          (fun event ->
            let open Lwt_syntax in
            let* res = on_layer_1_chain_event node_ctxt event in
            match res with
            | Ok () -> return_unit
            | Error trace when is_connection_error trace ->
                Format.eprintf
                  "@[<v 2>Connection error:@ %a@]@."
                  pp_print_trace
                  trace ;
                l1_ctxt.stopper () ;
                return_unit
            | Error e ->
                Format.eprintf "%a@.Exiting.@." pp_print_trace e ;
                let* _ = Lwt_exit.exit_and_wait 1 in
                return_unit)
          l1_ctxt.events
      in
      let*! () = Event.connection_lost () in
      let* l1_ctxt =
        Layer1.reconnect configuration node_ctxt.l1_ctxt node_ctxt.store
      in
      loop l1_ctxt
    in
    protect @@ fun () -> Lwt.no_cancel @@ loop node_ctxt.l1_ctxt

  let install_finalizer {Node_context.l1_ctxt; store; _} rpc_server =
    let open Lwt_syntax in
    Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
    let message = l1_ctxt.cctxt#message in
    let* () = message "Stopping L1 monitor@." in
    l1_ctxt.stopper () ;
    let* () = message "Shutting down L1@." in
    let* () = Layer1.shutdown store in
    let* () = message "Shutting down RPC server@." in
    let* () = Components.RPC_server.shutdown rpc_server in
    let* () = message "Closing store@." in
    let* () = Store_utils.close store in
    let* () = Event.shutdown_node exit_status in
    Tezos_base_unix.Internal_event_unix.close ()

  let check_initial_state_hash {Node_context.cctxt; rollup_address; context; _}
      =
    let open Lwt_result_syntax in
    let* l1_reference_initial_state_hash =
      RPC.Sc_rollup.initial_pvm_state_hash
        cctxt
        (cctxt#chain, cctxt#block)
        rollup_address
    in
    let*! s = PVM.initial_state context in
    let*! l2_initial_state_hash = PVM.state_hash s in
    if
      not
        Sc_rollup.State_hash.(
          l1_reference_initial_state_hash = l2_initial_state_hash)
    then
      let*! () =
        Daemon_event.wrong_initial_pvm_state_hash
          l2_initial_state_hash
          l1_reference_initial_state_hash
      in
      Lwt_exit.exit_and_raise 1
    else return_unit

  let run node_ctxt configuration =
    let open Lwt_result_syntax in
    let* () = check_initial_state_hash node_ctxt in
    let start () =
      let* rpc_server = Components.RPC_server.start node_ctxt configuration in
      let (_ : Lwt_exit.clean_up_callback_id) =
        install_finalizer node_ctxt rpc_server
      in
      let*! () = Inbox.start () in
      let*! () = Components.Commitment.start () in
      let signers =
        Configuration.Operator_purpose_map.bindings node_ctxt.operators
        |> List.fold_left
             (fun acc (purpose, operator) ->
               let purposes =
                 match Signature.Public_key_hash.Map.find operator acc with
                 | None -> [purpose]
                 | Some ps -> purpose :: ps
               in
               Signature.Public_key_hash.Map.add operator purposes acc)
             Signature.Public_key_hash.Map.empty
        |> Signature.Public_key_hash.Map.bindings
        |> List.map (fun (operator, purposes) ->
               (operator, `Each_block, purposes))
      in
      let* () =
        Injector.init
          node_ctxt.cctxt
          node_ctxt
          ~data_dir:configuration.data_dir
          ~signers
      in

      let*! () =
        Event.node_is_ready
          ~rpc_addr:configuration.rpc_addr
          ~rpc_port:configuration.rpc_port
      in
      daemonize configuration node_ctxt
    in
    start ()
end

let run ~data_dir (cctxt : Protocol_client_context.full) =
  let open Lwt_result_syntax in
  Random.self_init () (* Initialize random state (for reconnection delays) *) ;
  let*! () = Event.starting_node () in
  let* configuration = Configuration.load ~data_dir in
  let dal_cctxt = Dal_node_client.make_unix_cctxt configuration in
  let open Configuration in
  let* () =
    (* Check that the operators are valid keys. *)
    Operator_purpose_map.iter_es
      (fun _purpose operator ->
        let+ _pkh, _pk, _skh = Client_keys.get_key cctxt operator in
        ())
      configuration.sc_rollup_node_operators
  in
  let*! store =
    Store_utils.load Configuration.(default_storage_dir configuration.data_dir)
  in
  let*! context = Context.load configuration in
  let* l1_ctxt, kind = Layer1.start configuration cctxt store in
  let* node_ctxt =
    Node_context.init
      cctxt
      dal_cctxt
      ~data_dir
      l1_ctxt
      configuration.sc_rollup_address
      kind
      configuration.sc_rollup_node_operators
      configuration.fee_parameters
      ~loser_mode:configuration.loser_mode
      store
      context
  in
  let module Daemon = Make ((val Components.pvm_of_kind node_ctxt.kind)) in
  Daemon.run node_ctxt configuration
