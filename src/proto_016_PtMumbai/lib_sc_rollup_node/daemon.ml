(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 TriliTech <contact@trili.tech>                         *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

module Make (PVM : Pvm.S) = struct
  module Components = Components.Make (PVM)
  open Protocol
  open Alpha_context
  open Apply_results

  (** Returns [Some c] if [their_commitment] is refutable where [c] is our
      commitment for the same inbox level. *)
  let is_refutable_commitment node_ctxt
      (their_commitment : Sc_rollup.Commitment.t) their_commitment_hash =
    let open Lwt_result_syntax in
    let* l2_block =
      Node_context.get_l2_block_by_level
        node_ctxt
        (Raw_level.to_int32 their_commitment.inbox_level)
    in
    let* our_commitment_and_hash =
      Option.filter_map_es
        (fun hash ->
          let+ commitment = Node_context.find_commitment node_ctxt hash in
          Option.map (fun c -> (c, hash)) commitment)
        l2_block.header.commitment_hash
    in
    match our_commitment_and_hash with
    | Some (our_commitment, our_commitment_hash)
      when Sc_rollup.Commitment.Hash.(
             their_commitment_hash <> our_commitment_hash
             && their_commitment.predecessor = our_commitment.predecessor) ->
        return our_commitment_and_hash
    | _ -> return_none

  (** Publish a commitment when an accuser node sees a refutable commitment. *)
  let accuser_publish_commitment_when_refutable node_ctxt ~other rollup
      their_commitment their_commitment_hash =
    let open Lwt_result_syntax in
    when_ (Node_context.is_accuser node_ctxt) @@ fun () ->
    (* We are seeing a commitment from someone else. We check if we agree
       with it, otherwise the accuser publishes our commitment in order to
       play the refutation game. *)
    let* refutable =
      is_refutable_commitment node_ctxt their_commitment their_commitment_hash
    in
    match refutable with
    | None -> return_unit
    | Some (our_commitment, our_commitment_hash) ->
        let*! () =
          Refutation_game_event.potential_conflict_detected
            ~our_commitment_hash
            ~their_commitment_hash
            ~level:their_commitment.inbox_level
            ~other
        in
        assert (Sc_rollup.Address.(node_ctxt.rollup_address = rollup)) ;
        Components.Commitment.publish_single_commitment node_ctxt our_commitment

  (** Process an L1 SCORU operation (for the node's rollup) which is included
      for the first time. {b Note}: this function does not process inboxes for
      the rollup, which is done instead by {!Inbox.process_head}. *)
  let process_included_l1_operation (type kind) (node_ctxt : Node_context.rw)
      head ~source (operation : kind manager_operation)
      (result : kind successful_manager_operation_result) =
    let open Lwt_result_syntax in
    match (operation, result) with
    | ( Sc_rollup_publish {commitment; _},
        Sc_rollup_publish_result {published_at_level; _} )
      when Node_context.is_operator node_ctxt source ->
        (* Published commitment --------------------------------------------- *)
        let save_lpc =
          match Reference.get node_ctxt.lpc with
          | None -> true
          | Some lpc -> Raw_level.(commitment.inbox_level >= lpc.inbox_level)
        in
        if save_lpc then Reference.set node_ctxt.lpc (Some commitment) ;
        let commitment_hash =
          Sc_rollup.Commitment.hash_uncarbonated commitment
        in
        let* () =
          Node_context.set_commitment_published_at_level
            node_ctxt
            commitment_hash
            {
              first_published_at_level = published_at_level;
              published_at_level =
                Some (Raw_level.of_int32_exn head.Layer1.level);
            }
        in
        return_unit
    | ( Sc_rollup_publish {commitment = their_commitment; rollup},
        Sc_rollup_publish_result
          {published_at_level; staked_hash = their_commitment_hash; _} ) ->
        (* Commitment published by someone else *)
        (* We first register the publication information *)
        let* known_commitment =
          Node_context.commitment_exists node_ctxt their_commitment_hash
        in
        let* () =
          if not known_commitment then return_unit
          else
            let* republication =
              Node_context.commitment_was_published
                node_ctxt
                ~source:Anyone
                their_commitment_hash
            in
            if republication then return_unit
            else
              let* () =
                Node_context.set_commitment_published_at_level
                  node_ctxt
                  their_commitment_hash
                  {
                    first_published_at_level = published_at_level;
                    published_at_level = None;
                  }
              in
              return_unit
        in
        (* An accuser node will publish its commitment if the other one is
           refutable. *)
        accuser_publish_commitment_when_refutable
          node_ctxt
          ~other:source
          rollup
          their_commitment
          their_commitment_hash
    | Sc_rollup_cement {commitment; _}, Sc_rollup_cement_result {inbox_level; _}
      ->
        (* Cemented commitment ---------------------------------------------- *)
        let* inbox_block =
          Node_context.get_l2_block_by_level
            node_ctxt
            (Raw_level.to_int32 inbox_level)
        in
        let*? () =
          (* We stop the node if we disagree with a cemented commitment *)
          error_unless
            (Option.equal
               Sc_rollup.Commitment.Hash.( = )
               inbox_block.header.commitment_hash
               (Some commitment))
            (Sc_rollup_node_errors.Disagree_with_cemented
               {
                 inbox_level;
                 ours = inbox_block.header.commitment_hash;
                 on_l1 = commitment;
               })
        in
        let lcc = Reference.get node_ctxt.lcc in
        let*! () =
          if Raw_level.(inbox_level > lcc.level) then (
            Reference.set node_ctxt.lcc {commitment; level = inbox_level} ;
            Commitment_event.last_cemented_commitment_updated
              commitment
              inbox_level)
          else Lwt.return_unit
        in
        return_unit
    | ( Sc_rollup_refute _,
        Sc_rollup_refute_result {game_status = Ended end_status; _} )
    | ( Sc_rollup_timeout _,
        Sc_rollup_timeout_result {game_status = Ended end_status; _} ) -> (
        match end_status with
        | Loser {loser; _} when Node_context.is_operator node_ctxt loser ->
            tzfail (Sc_rollup_node_errors.Lost_game end_status)
        | Loser _ ->
            (* Other player lost *)
            return_unit
        | Draw ->
            let stakers =
              match operation with
              | Sc_rollup_refute {opponent; _} -> [source; opponent]
              | Sc_rollup_timeout {stakers = {alice; bob}; _} -> [alice; bob]
              | _ -> assert false
            in
            fail_when
              (List.exists (Node_context.is_operator node_ctxt) stakers)
              (Sc_rollup_node_errors.Lost_game end_status))
    | Dal_publish_slot_header slot_header, Dal_publish_slot_header_result _
      when Node_context.dal_supported node_ctxt ->
        let* () =
          Node_context.save_slot_header
            node_ctxt
            ~published_in_block_hash:head.Layer1.hash
            slot_header.header
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
      | Sc_rollup_add_messages _ -> true
      | Sc_rollup_cement {rollup; _}
      | Sc_rollup_publish {rollup; _}
      | Sc_rollup_refute {rollup; _}
      | Sc_rollup_timeout {rollup; _}
      | Sc_rollup_execute_outbox_message {rollup; _}
      | Sc_rollup_recover_bond {sc_rollup = rollup; staker = _} ->
          Sc_rollup.Address.(rollup = node_ctxt.Node_context.rollup_address)
      | Dal_publish_slot_header _ -> true
      | Reveal _ | Transaction _ | Origination _ | Delegation _
      | Update_consensus_key _ | Register_global_constant _
      | Set_deposits_limit _ | Increase_paid_storage _ | Tx_rollup_origination
      | Tx_rollup_submit_batch _ | Tx_rollup_commit _ | Tx_rollup_return_bond _
      | Tx_rollup_finalize_commitment _ | Tx_rollup_remove_commitment _
      | Tx_rollup_rejection _ | Tx_rollup_dispatch_tickets _ | Transfer_ticket _
      | Sc_rollup_originate _ | Zk_rollup_origination _ | Zk_rollup_publish _
      | Zk_rollup_update _ ->
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

  let process_l1_block_operations ~finalized node_ctxt
      (Layer1.{hash; _} as head) =
    let open Lwt_result_syntax in
    let* block = Layer1.fetch_tezos_block node_ctxt.Node_context.cctxt hash in
    let apply (type kind) accu ~source (operation : kind manager_operation)
        result =
      let open Lwt_result_syntax in
      let* () = accu in
      process_l1_operation ~finalized node_ctxt head ~source operation result
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

  let before_origination (node_ctxt : _ Node_context.t) Layer1.{level; _} =
    let origination_level = Raw_level.to_int32 node_ctxt.genesis_info.level in
    level < origination_level

  let rec processed_finalized_block (node_ctxt : _ Node_context.t)
      Layer1.({hash; level} as block) =
    let open Lwt_result_syntax in
    let* last_finalized = Node_context.get_finalized_head_opt node_ctxt in
    let already_finalized =
      match last_finalized with
      | Some finalized -> level <= Raw_level.to_int32 finalized.header.level
      | None -> false
    in
    unless (already_finalized || before_origination node_ctxt block)
    @@ fun () ->
    let* predecessor = Node_context.get_predecessor_opt node_ctxt block in
    let* () =
      Option.iter_es (processed_finalized_block node_ctxt) predecessor
    in
    let*! () = Daemon_event.head_processing hash level ~finalized:true in
    let* () = process_l1_block_operations ~finalized:true node_ctxt block in
    let* () = Node_context.mark_finalized_head node_ctxt hash in
    return_unit

  let rec process_head (node_ctxt : _ Node_context.t)
      Layer1.({hash; level} as head) =
    let open Lwt_result_syntax in
    let* already_processed = Node_context.is_processed node_ctxt hash in
    unless (already_processed || before_origination node_ctxt head) @@ fun () ->
    let*! () = Daemon_event.head_processing hash level ~finalized:false in
    let* predecessor = Node_context.get_predecessor_opt node_ctxt head in
    match predecessor with
    | None ->
        (* Predecessor not available on the L1, which means the block does not
           exist in the chain. *)
        return_unit
    | Some predecessor ->
        let* () = process_head node_ctxt predecessor in
        let* () = Node_context.save_level node_ctxt head in
        let* inbox_hash, inbox, inbox_witness, messages, ctxt =
          Inbox.process_head node_ctxt ~predecessor head
        in
        let* () =
          when_ (Node_context.dal_supported node_ctxt) @@ fun () ->
          Dal_slots_tracker.process_head node_ctxt head
        in
        let* () = process_l1_block_operations ~finalized:false node_ctxt head in
        (* Avoid storing and publishing commitments if the head is not final. *)
        (* Avoid triggering the pvm execution if this has been done before for
           this head. *)
        let* ctxt, _num_messages, num_ticks, initial_tick =
          Components.Interpreter.process_head
            node_ctxt
            ctxt
            ~predecessor
            head
            (inbox, messages)
        in
        let*! context_hash = Context.commit ctxt in
        let* commitment_hash =
          Components.Commitment.process_head
            node_ctxt
            ~predecessor:predecessor.hash
            head
            ctxt
        in
        let level = Raw_level.of_int32_exn level in
        let* previous_commitment_hash =
          if level = node_ctxt.genesis_info.Sc_rollup.Commitment.level then
            (* Previous commitment for rollup genesis is itself. *)
            return node_ctxt.genesis_info.Sc_rollup.Commitment.commitment_hash
          else
            let+ pred = Node_context.get_l2_block node_ctxt predecessor.hash in
            Sc_rollup_block.most_recent_commitment pred.header
        in
        let header =
          Sc_rollup_block.
            {
              block_hash = hash;
              level;
              predecessor = predecessor.hash;
              commitment_hash;
              previous_commitment_hash;
              context = context_hash;
              inbox_witness;
              inbox_hash;
            }
        in
        let l2_block =
          Sc_rollup_block.{header; content = (); num_ticks; initial_tick}
        in
        let* finalized_block, _ =
          Node_context.nth_predecessor
            node_ctxt
            node_ctxt.block_finality_time
            head
        in
        let* () = processed_finalized_block node_ctxt finalized_block in
        let* () = Node_context.save_l2_head node_ctxt l2_block in
        let*! () =
          Daemon_event.new_head_processed hash (Raw_level.to_int32 level)
        in
        return_unit

  (* [on_layer_1_head node_ctxt head] processes a new head from the L1. It
     also processes any missing blocks that were not processed. Every time a
     head is processed we also process head~2 as finalized (which may recursively
     imply the processing of head~3, etc). *)
  let on_layer_1_head node_ctxt head =
    let open Lwt_result_syntax in
    let* old_head = Node_context.last_processed_head_opt node_ctxt in
    let old_head =
      match old_head with
      | Some h ->
          `Head
            Layer1.
              {
                hash = h.header.block_hash;
                level = Raw_level.to_int32 h.header.level;
              }
      | None ->
          (* if no head has been processed yet, we want to handle all blocks
             since, and including, the rollup origination. *)
          let origination_level =
            Raw_level.to_int32 node_ctxt.genesis_info.level
          in
          `Level (Int32.pred origination_level)
    in
    let*! reorg =
      Node_context.get_tezos_reorg_for_new_head node_ctxt old_head head
    in
    let*? reorg =
      match reorg with
      | Error trace
        when TzTrace.fold
               (fun yes error ->
                 yes
                 ||
                 match error with
                 | Octez_crawler.Layer_1.Cannot_find_predecessor _ -> true
                 | _ -> false)
               false
               trace ->
          (* The reorganization could not be computed entirely because of missing
             info on the Layer 1. We fallback to a recursive process_head. *)
          Ok {Reorg.no_reorg with new_chain = [head]}
      | _ -> reorg
    in
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/3348
       Rollback state information on reorganization, i.e. for
       reorg.old_chain. *)
    let* new_head = Layer1.fetch_tezos_block node_ctxt.cctxt head.Layer1.hash in
    let header =
      Block_header.(
        raw
          {
            shell = new_head.header.shell;
            protocol_data = new_head.header.protocol_data;
          })
    in
    let*! () = Daemon_event.processing_heads_iteration reorg.new_chain in
    let* () = List.iter_es (process_head node_ctxt) reorg.new_chain in
    let* () = Components.Commitment.Publisher.publish_commitments () in
    let* () = Components.Commitment.Publisher.cement_commitments () in
    let*! () = Daemon_event.new_heads_processed reorg.new_chain in
    let* () = Components.Refutation_game.process head node_ctxt in
    let* () = Components.Batcher.batch () in
    let* () = Components.Batcher.new_head head in
    let*! () = Injector.inject ~header () in
    return_unit

  let daemonize (node_ctxt : _ Node_context.t) =
    Layer1.iter_heads node_ctxt.l1_ctxt (on_layer_1_head node_ctxt)

  let install_finalizer node_ctxt rpc_server =
    let open Lwt_syntax in
    Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
    let message = node_ctxt.Node_context.cctxt#message in
    let* () = message "Shutting down RPC server@." in
    let* () = Components.RPC_server.shutdown rpc_server in
    let* () = message "Shutting down Injector@." in
    let* () = Injector.shutdown () in
    let* () = message "Shutting down Batcher@." in
    let* () = Components.Batcher.shutdown () in
    let* () = message "Shutting down Commitment Publisher@." in
    let* () = Components.Commitment.Publisher.shutdown () in
    let* (_ : unit tzresult) = Node_context.close node_ctxt in
    let* () = Event.shutdown_node exit_status in
    Tezos_base_unix.Internal_event_unix.close ()

  let check_initial_state_hash {Node_context.cctxt; rollup_address; _} =
    let open Lwt_result_syntax in
    let* l1_reference_initial_state_hash =
      RPC.Sc_rollup.initial_pvm_state_hash
        cctxt
        (cctxt#chain, cctxt#block)
        rollup_address
    in
    let*! s = PVM.initial_state ~empty:(PVM.State.empty ()) in
    let*! l2_initial_state_hash = PVM.state_hash s in
    fail_unless
      Sc_rollup.State_hash.(
        l1_reference_initial_state_hash = l2_initial_state_hash)
      (Sc_rollup_node_errors.Wrong_initial_pvm_state
         {
           initial_state_hash = l2_initial_state_hash;
           expected_state_hash = l1_reference_initial_state_hash;
         })

  let run node_ctxt configuration =
    let open Lwt_result_syntax in
    let* () = check_initial_state_hash node_ctxt in
    let* rpc_server = Components.RPC_server.start node_ctxt configuration in
    let (_ : Lwt_exit.clean_up_callback_id) =
      install_finalizer node_ctxt rpc_server
    in
    let start () =
      let*! () = Inbox.start () in
      let signers =
        Configuration.Operator_purpose_map.bindings node_ctxt.operators
        |> List.fold_left
             (fun acc (purpose, operator) ->
               let purposes =
                 match
                   Tezos_crypto.Signature.Public_key_hash.Map.find operator acc
                 with
                 | None -> [purpose]
                 | Some ps -> purpose :: ps
               in
               Tezos_crypto.Signature.Public_key_hash.Map.add
                 operator
                 purposes
                 acc)
             Tezos_crypto.Signature.Public_key_hash.Map.empty
        |> Tezos_crypto.Signature.Public_key_hash.Map.bindings
        |> List.map (fun (operator, purposes) ->
               let strategy =
                 match purposes with
                 | [Configuration.Add_messages] -> `Delay_block 0.5
                 | _ -> `Each_block
               in
               (operator, strategy, purposes))
      in
      let* () = Components.Commitment.Publisher.init node_ctxt in
      let* () =
        Injector.init
          node_ctxt.cctxt
          (Node_context.readonly node_ctxt)
          ~data_dir:node_ctxt.data_dir
          ~signers
          ~retention_period:configuration.injector_retention_period
      in
      let* () =
        match
          Configuration.Operator_purpose_map.find
            Add_messages
            node_ctxt.operators
        with
        | None -> return_unit
        | Some signer ->
            Components.Batcher.init configuration.batcher ~signer node_ctxt
      in
      Lwt.dont_wait
        (fun () ->
          let*! r = Metrics.metrics_serve configuration.metrics_addr in
          match r with
          | Ok () -> Lwt.return_unit
          | Error err ->
              Event.(metrics_ended (Format.asprintf "%a" pp_print_trace err)))
        (fun exn -> Event.(metrics_ended_dont_wait (Printexc.to_string exn))) ;

      let*! () =
        Event.node_is_ready
          ~rpc_addr:configuration.rpc_addr
          ~rpc_port:configuration.rpc_port
      in
      daemonize node_ctxt
    in
    Metrics.Info.init_rollup_node_info
      ~id:node_ctxt.rollup_address
      ~mode:configuration.mode
      ~genesis_level:node_ctxt.genesis_info.level
      ~genesis_hash:node_ctxt.genesis_info.commitment_hash
      ~pvm_kind:node_ctxt.kind ;
    protect start ~on_error:(fun e ->
        Format.eprintf "%!%a@.Exiting.@." pp_print_trace e ;
        let*! _ = Lwt_exit.exit_and_wait 1 in
        return_unit)
end

let run ~data_dir (configuration : Configuration.t)
    (cctxt : Protocol_client_context.full) =
  let open Lwt_result_syntax in
  Random.self_init () (* Initialize random state (for reconnection delays) *) ;
  let*! () = Event.starting_node () in
  let open Configuration in
  let* () =
    (* Check that the operators are valid keys. *)
    Operator_purpose_map.iter_es
      (fun _purpose operator ->
        let+ _pkh, _pk, _skh = Client_keys.get_key cctxt operator in
        ())
      configuration.sc_rollup_node_operators
  in
  let* node_ctxt = Node_context.init cctxt ~data_dir Read_write configuration in
  let module Daemon = Make ((val Components.pvm_of_kind node_ctxt.kind)) in
  Daemon.run node_ctxt configuration
