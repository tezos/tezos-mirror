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
  let process_included_l1_operation (type kind) _node_ctxt store ~source:_
      (operation : kind manager_operation)
      (result : kind successful_manager_operation_result) =
    let open Lwt_syntax in
    match (operation, result) with
    | ( Sc_rollup_publish {commitment; _},
        Sc_rollup_publish_result {published_at_level; _} ) ->
        (* Published commitment --------------------------------------------- *)
        let commitment_hash =
          Sc_rollup.Commitment.hash_uncarbonated commitment
        in
        Store.Commitments_published_at_level.add
          store
          commitment_hash
          published_at_level
    | Sc_rollup_cement {commitment; _}, Sc_rollup_cement_result {inbox_level; _}
      ->
        (* Cemented commitment ---------------------------------------------- *)
        let* () = Store.Last_cemented_commitment_level.set store inbox_level in
        Store.Last_cemented_commitment_hash.set store commitment
    | _, _ ->
        (* Other manager operations *)
        return_unit

  (** Process an L1 SCORU operation (for the node's rollup) which is finalized
      for the first time. *)
  let process_finalized_l1_operation (type kind) _node_ctxt _store ~source:_
      (_operation : kind manager_operation)
      (_result : kind successful_manager_operation_result) =
    Lwt.return_unit

  let process_l1_operation (type kind) ~finalized node_ctxt store ~source
      (operation : kind manager_operation)
      (result : kind Apply_results.manager_operation_result) =
    let open Lwt_syntax in
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
      | Reveal _ | Transaction _ | Origination _ | Delegation _
      | Register_global_constant _ | Set_deposits_limit _
      | Increase_paid_storage _ | Tx_rollup_origination
      | Tx_rollup_submit_batch _ | Tx_rollup_commit _ | Tx_rollup_return_bond _
      | Tx_rollup_finalize_commitment _ | Tx_rollup_remove_commitment _
      | Tx_rollup_rejection _ | Tx_rollup_dispatch_tickets _ | Transfer_ticket _
      | Dal_publish_slot_header _ | Sc_rollup_originate _ ->
          false
    in
    if not (is_for_my_rollup operation) then return_unit
    else
      (* Only look at operations that are for the node's rollup *)
      let* () = Daemon_event.included_operation ~finalized operation result in
      match result with
      | Applied success_result ->
          let process =
            if finalized then process_finalized_l1_operation
            else process_included_l1_operation
          in
          process node_ctxt store ~source operation success_result
      | _ ->
          (* No action for non successful operations  *)
          return_unit

  let process_l1_block_operations ~finalized node_ctxt store
      (Layer1.Head {hash; _}) =
    let open Lwt_result_syntax in
    let* block = Layer1.fetch_tezos_block node_ctxt.Node_context.l1_ctxt hash in
    let apply (type kind) accu ~source (operation : kind manager_operation)
        result =
      let open Lwt_syntax in
      let* () = accu in
      process_l1_operation ~finalized node_ctxt store ~source operation result
    in
    let apply_internal (type kind) accu ~source:_
        (_operation : kind Apply_internal_results.internal_operation)
        (_result : kind Apply_internal_results.internal_operation_result) =
      accu
    in
    let*! () =
      Layer1_services.process_manager_operations
        Lwt.return_unit
        block.operations
        {apply; apply_internal}
    in
    return_unit

  let process_head node_ctxt store head_state =
    let open Lwt_result_syntax in
    let {finalized; seen_before; head} = head_state in
    let* () =
      let*! () = emit_head_processing_event head_state in
      (* Avoid processing inbox again if it has been processed before for this head *)
      if seen_before then
        if finalized then
          process_l1_block_operations ~finalized node_ctxt store head
        else return_unit
      else
        let* () = Inbox.process_head node_ctxt store head in
        let* () = process_l1_block_operations ~finalized node_ctxt store head in
        (* Avoid storing and publishing commitments if the head is not final *)
        (* Avoid triggering the pvm execution if this has been done before for this head *)
        let* () = Components.Interpreter.process_head node_ctxt store head in

        (* DAL/FIXME: https://gitlab.com/tezos/tezos/-/issues/3166

           If the rollup is subscribed to at least one slot, then the inbox for
           this block will be downloaded after lag levels have passed and the
           dal slots have been declared available. *)
        Dal_slots_tracker.process_head node_ctxt store head
    in
    let* () =
      when_ finalized @@ fun () ->
      Components.Commitment.process_head node_ctxt store head
    in
    (* Publishing a commitment when one is available does not depend on the state of
       the current head. *)
    let* () = Components.Commitment.publish_commitment node_ctxt store in
    let* () =
      Components.Commitment.cement_commitment_if_possible node_ctxt store head
    in
    let* () =
      (* At each block, there may be some refutation related actions to
         be performed. *)
      when_ finalized @@ fun () ->
      Components.Refutation_game.process head node_ctxt store
    in
    when_ finalized (fun () ->
        let*! () = Layer1.mark_processed_head store head in
        return ())

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
  let on_layer_1_chain_event node_ctxt store chain_event =
    let open Lwt_result_syntax in
    let*! old_heads = Layer1.get_heads_not_finalized store in
    let open Layer1 in
    let* () =
      (* Get information about the last cemented commitment to determine the
         commitment (if any) to publish next. We do this only once per
         chain event to avoid spamming the layer1 node. *)
      Components.Commitment.sync_last_cemented_commitment_hash_with_level
        node_ctxt
        store
    in
    let* non_final_heads =
      match chain_event with
      | SameBranch {new_head; intermediate_heads} ->
          let new_heads = intermediate_heads @ [new_head] in
          let*! () =
            Daemon_event.processing_heads_iteration old_heads new_heads
          in
          let head_states = categorise_heads node_ctxt old_heads new_heads in
          let* () = List.iter_es (process_head node_ctxt store) head_states in
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
                  store
                  {head; finalized = true; seen_before = true})
              final_heads
          in
          []
    in
    let*! () = Layer1.set_heads_not_finalized store non_final_heads in
    let*! () = Layer1.processed chain_event in
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
  let daemonize configuration node_ctxt store =
    let open Lwt_result_syntax in
    let rec loop (l1_ctxt : Layer1.t) =
      let*! () =
        Lwt_stream.iter_s
          (fun event ->
            let open Lwt_syntax in
            let* res = on_layer_1_chain_event node_ctxt store event in
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
      let* l1_ctxt = Layer1.reconnect configuration l1_ctxt store in
      loop l1_ctxt
    in
    protect @@ fun () -> Lwt.no_cancel @@ loop node_ctxt.l1_ctxt

  let install_finalizer store rpc_server (l1_ctxt : Layer1.t) =
    let open Lwt_syntax in
    Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
    l1_ctxt.stopper () ;
    let* () = Lwt_stream.closed l1_ctxt.events in
    let* () = Layer1.shutdown store in
    let* () = Components.RPC_server.shutdown rpc_server in
    let* () = Store.close store in
    let* () = Event.shutdown_node exit_status in
    Tezos_base_unix.Internal_event_unix.close ()

  let check_initial_state_hash {Node_context.cctxt; rollup_address; _} store =
    let open Lwt_result_syntax in
    let* l1_reference_initial_state_hash =
      RPC.Sc_rollup.initial_pvm_state_hash
        cctxt
        (cctxt#chain, cctxt#block)
        rollup_address
    in
    let*! s = PVM.initial_state store in
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

  let run node_ctxt configuration store =
    let open Lwt_result_syntax in
    let* () = check_initial_state_hash node_ctxt store in
    let start () =
      let* rpc_server =
        Components.RPC_server.start node_ctxt store configuration
      in
      let*! () = Inbox.start () in
      let*! () = Components.Commitment.start () in

      let _ = install_finalizer store rpc_server node_ctxt.l1_ctxt in
      let*! () =
        Event.node_is_ready
          ~rpc_addr:configuration.rpc_addr
          ~rpc_port:configuration.rpc_port
      in
      daemonize configuration node_ctxt store
    in
    start ()
end

let run ~data_dir (cctxt : Protocol_client_context.full) =
  let open Lwt_result_syntax in
  Random.self_init () (* Initialize random state (for reconnection delays) *) ;
  let*! () = Event.starting_node () in
  let* configuration = Configuration.load ~data_dir in
  let open Configuration in
  let* () =
    (* Check that the operators are valid keys. *)
    Operator_purpose_map.iter_es
      (fun _purpose operator ->
        let+ _pkh, _pk, _skh = Client_keys.get_key cctxt operator in
        ())
      configuration.sc_rollup_node_operators
  in
  let*! store = Store.load configuration in
  let* l1_ctxt, kind = Layer1.start configuration cctxt store in
  let* node_ctxt =
    Node_context.init
      cctxt
      l1_ctxt
      configuration.sc_rollup_address
      kind
      configuration.sc_rollup_node_operators
      configuration.fee_parameter
      ~loser_mode:configuration.loser_mode
  in
  let module Daemon = Make ((val Components.pvm_of_kind node_ctxt.kind)) in
  Daemon.run node_ctxt configuration store
