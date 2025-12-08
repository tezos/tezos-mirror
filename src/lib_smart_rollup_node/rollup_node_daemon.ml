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

type state = {
  mutable plugin : (module Protocol_plugin_sig.S);
  rpc_server : Rpc_server.t;
  configuration : Configuration.t;
  node_ctxt : Node_context.rw;
}

let is_before_origination (node_ctxt : _ Node_context.t)
    (header : Layer1.header) =
  let origination_level = node_ctxt.genesis_info.level in
  header.level < origination_level

let previous_context (node_ctxt : _ Node_context.t)
    ~(predecessor : Layer1.header) =
  let open Lwt_result_syntax in
  if is_before_origination node_ctxt predecessor then
    (* This is before we have interpreted the boot sector, so we start
       with an empty context in genesis *)
    return (Context.empty node_ctxt.context)
  else Node_context.checkout_context node_ctxt predecessor.Layer1.hash

let start_workers (plugin : (module Protocol_plugin_sig.S))
    (node_ctxt : _ Node_context.t) =
  let open Lwt_result_syntax in
  let* () = Publisher.init node_ctxt in
  let* () = Batcher.init plugin node_ctxt in
  let* () = Dal_injection_queue.init node_ctxt in
  let* () = Refutation_coordinator.init node_ctxt in
  return_unit

let handle_protocol_migration ~catching_up state (head : Layer1.header) =
  let open Lwt_result_syntax in
  let* head_proto = Node_context.protocol_of_level state.node_ctxt head.level in
  let new_protocol = head_proto.protocol in
  let current_protocol = Reference.get state.node_ctxt.current_protocol in
  when_ Protocol_hash.(new_protocol <> current_protocol.hash) @@ fun () ->
  let*! () =
    Daemon_event.migration
      ~catching_up
      (current_protocol.hash, current_protocol.proto_level)
      (new_protocol, head_proto.proto_level)
  in
  let*? new_plugin = Protocol_plugins.proto_plugin_for_protocol new_protocol in
  let* constants =
    let constants_level =
      Int32.max head.level state.node_ctxt.genesis_info.level
    in
    Protocol_plugins.get_constants_of_protocol
      state.node_ctxt
      ~level:constants_level
      new_protocol
  in
  let new_protocol =
    {
      Node_context.hash = new_protocol;
      proto_level = head_proto.proto_level;
      constants;
    }
  in
  state.plugin <- new_plugin ;
  Reference.set state.node_ctxt.current_protocol new_protocol ;
  Metrics.wrap (fun () ->
      Metrics.Info.set_proto_info new_protocol.hash constants) ;
  let*! () =
    Daemon_event.switched_protocol
      new_protocol.hash
      new_protocol.proto_level
      new_protocol.constants
  in
  return_unit

let maybe_split_context node_ctxt commitment_hash head_level =
  let open Lwt_result_syntax in
  let* history_mode = Node_context.get_history_mode node_ctxt in
  let commit_is_gc_candidate =
    history_mode <> Archive && Option.is_some commitment_hash
  in
  when_ commit_is_gc_candidate @@ fun () ->
  let* last = Node_context.get_last_context_split_level node_ctxt in
  let last = Option.value last ~default:node_ctxt.genesis_info.level in
  if
    Int32.(to_int @@ sub head_level last)
    >= Node_context.splitting_period node_ctxt
  then (
    Context.split node_ctxt.context ;
    Node_context.save_context_split_level node_ctxt head_level)
  else return_unit

(** Register outbox messages if any. *)
let register_outbox_messages (module Plugin : Protocol_plugin_sig.S) node_ctxt
    ctxt level =
  let open Lwt_result_syntax in
  let* pvm_state = Context.PVMState.get ctxt in
  let*! outbox_messages =
    Plugin.Pvm.get_outbox_messages node_ctxt pvm_state ~outbox_level:level
  in
  let indexes = List.map fst outbox_messages in
  Node_context.register_outbox_messages node_ctxt ~outbox_level:level ~indexes

(* Process a L1 that we have never seen and for which we have processed the
   predecessor. *)
let process_unseen_head ({node_ctxt; _} as state) ~catching_up ~predecessor
    ~finalized (head : Layer1.header) =
  let open Lwt_result_syntax in
  let level = head.level in
  Octez_telemetry.Trace.with_tzresult "process_unseen_block" @@ fun scope ->
  Opentelemetry.Scope.add_attrs scope (fun () ->
      [
        ("rollup_node.block_hash", `String (Block_hash.to_b58check head.hash));
        ("rollup_node.block_level", `Int (Int32.to_int level));
      ]) ;
  let* () = Node_context.save_protocol_info node_ctxt head ~predecessor in
  let* () = handle_protocol_migration ~catching_up state head in
  let* ctxt = previous_context node_ctxt ~predecessor in
  let module Plugin = (val state.plugin) in
  let start_timestamp = Time.System.now () in
  let* inbox_hash, inbox, inbox_witness, messages =
    Plugin.Inbox.process_head node_ctxt ~predecessor head
  in
  let fetch_timestamp = Time.System.now () in
  Metrics.wrap (fun () ->
      Metrics.Inbox.set_fetch_time @@ Ptime.diff fetch_timestamp start_timestamp ;
      Metrics.Inbox.set_messages messages ~is_internal:(fun s ->
          String.length s > 0 && s.[0] = '\000')) ;
  let* () =
    when_ (Node_context.dal_supported node_ctxt) @@ fun () ->
    Plugin.Dal_slots_tracker.process_head node_ctxt (Layer1.head_of_header head)
  in
  let* () =
    Plugin.L1_processing.process_l1_block_operations ~catching_up node_ctxt head
  in
  (* Avoid storing and publishing commitments if the head is not final. *)
  (* Avoid triggering the pvm execution if this has been done before for
     this head. *)
  let* _num_messages, num_ticks, initial_tick =
    Interpreter.process_head
      (module Plugin)
      node_ctxt
      ctxt
      ~predecessor:(Layer1.head_of_header predecessor)
      (Layer1.head_of_header head)
      (inbox, messages)
  in
  let*! context_hash = Context.commit ctxt in
  let* commitment_hash =
    Publisher.process_head
      state.plugin
      node_ctxt
      ~predecessor:predecessor.hash
      head
      ctxt
  in
  let* () = maybe_split_context node_ctxt commitment_hash head.level in
  let* previous_commitment_hash =
    if level = node_ctxt.genesis_info.level then
      (* Previous commitment for rollup genesis is itself. *)
      return node_ctxt.genesis_info.commitment_hash
    else
      let+ pred = Node_context.get_l2_block node_ctxt predecessor.hash in
      Sc_rollup_block.most_recent_commitment pred.header
  in
  let* () = register_outbox_messages state.plugin node_ctxt ctxt level in
  let header =
    Sc_rollup_block.
      {
        block_hash = head.hash;
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
  let* () = Node_context.save_l2_block node_ctxt l2_block in
  let* () =
    if finalized then
      Node_context.set_finalized node_ctxt header.block_hash header.level
    else (
      assert (node_ctxt.block_finality_time = 2) ;
      let finalized_level = Int32.pred predecessor.header.level in
      let finalized_hash = predecessor.header.predecessor in
      Node_context.set_finalized node_ctxt finalized_hash finalized_level)
  in
  let end_timestamp = Time.System.now () in
  let total_time = Ptime.diff end_timestamp start_timestamp in
  let process_time = Ptime.diff end_timestamp fetch_timestamp in
  Metrics.wrap (fun () ->
      Metrics.Inbox.set_process_time process_time ;
      Metrics.Inbox.set_total_time total_time) ;
  return l2_block

let rec process_l1_block ({node_ctxt; _} as state) ~catching_up ~finalized
    (head : Layer1.header) =
  let open Lwt_result_syntax in
  if is_before_origination node_ctxt head then return `Nothing
  else
    let* l2_head = Node_context.find_l2_block node_ctxt head.hash in
    match l2_head with
    | Some l2_head ->
        (* Already processed *)
        return (`Already_processed l2_head)
    | None ->
        (* New head *)
        let*! () = Daemon_event.head_processing head.hash head.level in
        let* predecessor = Node_context.get_predecessor_header node_ctxt head in
        let* () =
          update_l2_chain
            state
            ~catching_up:true
            ~recurse_pred:true
            ~finalized
            predecessor
        in
        let* l2_head =
          process_unseen_head state ~catching_up ~predecessor ~finalized head
        in
        return (`New l2_head)

and update_l2_chain ({node_ctxt; _} as state) ~catching_up
    ?(recurse_pred = false) ~finalized (head : Layer1.header) =
  let open Lwt_result_syntax in
  let start_timestamp = Time.System.now () in
  let* () =
    Node_context.save_level
      node_ctxt
      {Layer1.hash = head.hash; level = head.level}
  in
  let* done_ = process_l1_block state ~catching_up ~finalized head in
  match done_ with
  | `Nothing -> return_unit
  | `Already_processed l2_block ->
      Metrics.wrap (fun () ->
          Metrics.Inbox.set_fetch_time Ptime.Span.zero ;
          Metrics.Inbox.set_process_time Ptime.Span.zero ;
          Metrics.Inbox.set_total_time Ptime.Span.zero) ;
      if recurse_pred then
        (* We are calling update_l2_chain recursively to ensure we have handled
           the predecessor. In this case, we don't update the head or notify the
           block. *)
        return_unit
      else
        (* Reorg (reproposal) of a block already handled *)
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/7731
           This just overwrites the outbox messages with the correct ones for
           the level but we need to properly handle reorgs in the storage. *)
        let* ctxt = Node_context.checkout_context node_ctxt head.hash in
        let* () =
          register_outbox_messages state.plugin node_ctxt ctxt head.level
        in
        Node_context.set_l2_head node_ctxt l2_block
  | `New l2_block ->
      let* () = Node_context.set_l2_head node_ctxt l2_block in
      let stop_timestamp = Time.System.now () in
      let process_time = Ptime.diff stop_timestamp start_timestamp in
      let*! () =
        Daemon_event.new_head_processed head.hash head.level process_time
      in
      let* () = Node_context.gc node_ctxt ~level:head.level in
      return_unit

let update_l2_chain state ~catching_up ~finalized head =
  Lwt_lock_file.with_lock
    ~when_locked:`Block
    ~filename:
      (Node_context.processing_lockfile_path ~data_dir:state.node_ctxt.data_dir)
  @@ fun () -> update_l2_chain state ~catching_up ~finalized head

let missing_data_error trace =
  TzTrace.fold
    (fun acc error ->
      match acc with
      | Some _ -> acc
      | None -> (
          match error with
          | Octez_crawler.Layer_1.Cannot_find_predecessor hash -> Some hash
          | _ -> acc))
    None
    trace

let report_missing_data result =
  match result with
  | Ok _ -> result
  | Error trace -> (
      match missing_data_error trace with
      | None -> result
      | Some hash ->
          Error
            (TzTrace.cons
               (Error_monad.error_of_fmt
                  "The L1 node does not have required information before block \
                   %a. Consider using an L1 node in archive mode or with more \
                   history using, e.g., --history-mode full:50"
                  Block_hash.pp
                  hash)
               trace))

let notify_synchronized (node_ctxt : _ Node_context.t) =
  Lwt_condition.broadcast node_ctxt.sync.on_synchronized ()

let notify_synchronization (node_ctxt : _ Node_context.t) head_level =
  let latest_l1_head = Layer1.get_latest_head node_ctxt.l1_ctxt in
  match latest_l1_head with
  | None -> notify_synchronized node_ctxt
  | Some l1_head when head_level = l1_head.level ->
      notify_synchronized node_ctxt
  | Some _ -> ()

(* [on_layer_1_head node_ctxt head] processes a new head from the L1. It
   also processes any missing blocks that were not processed. *)
let on_layer_1_head ({node_ctxt; _} as state) ~finalized (head : Layer1.header)
    =
  let open Lwt_result_syntax in
  Octez_telemetry.Trace.with_tzresult "on_layer1_head" @@ fun scope ->
  Opentelemetry.Scope.add_attrs scope (fun () ->
      [
        ("rollup_node.block_hash", `String (Block_hash.to_b58check head.hash));
        ("rollup_node.block_level", `Int (Int32.to_int head.level));
      ]) ;
  let* old_head = Node_context.last_processed_head_opt node_ctxt in
  let old_head, old_level =
    match old_head with
    | Some h ->
        ( `Head Layer1.{hash = h.header.block_hash; level = h.header.level},
          h.header.level )
    | None ->
        (* if no head has been processed yet, we want to handle all blocks
           since, and including, the rollup origination. *)
        let origination_level = node_ctxt.genesis_info.level in
        let l = Int32.pred origination_level in
        (`Level l, l)
  in
  let missing_blocks = Int32.sub head.level old_level in
  let*! () =
    if missing_blocks > 1l then Daemon_event.catch_up missing_blocks
    else Lwt.return_unit
  in
  let stripped_head = Layer1.head_of_header head in
  let*! reorg =
    Node_context.get_tezos_reorg_for_new_head node_ctxt old_head stripped_head
  in
  let*? reorg = report_missing_data reorg in
  let*! () = Daemon_event.reorg reorg.old_chain in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/3348
     Rollback state information on reorganization, i.e. for
     reorg.old_chain. *)
  let*! () = Daemon_event.processing_heads_iteration reorg.new_chain in
  let get_header Layer1.{hash; level} =
    if Block_hash.equal hash head.hash then return head
    else
      let+ header = Layer1.fetch_tezos_shell_header node_ctxt.l1_ctxt hash in
      {Layer1.hash; level; header}
  in
  let new_chain_prefetching =
    Layer1.make_prefetching_schedule node_ctxt.l1_ctxt reorg.new_chain
  in
  let* () =
    List.iter_es
      (fun (block, to_prefetch) ->
        let module Plugin = (val state.plugin) in
        Octez_telemetry.Trace.with_tzresult "process_block" @@ fun _ ->
        Plugin.Layer1_helpers.prefetch_tezos_blocks
          node_ctxt.l1_ctxt
          to_prefetch ;
        let* header = get_header block in
        let catching_up = block.level < head.level in
        update_l2_chain state ~catching_up ~finalized header)
      new_chain_prefetching
  in
  notify_synchronization node_ctxt head.level ;
  let* () = Publisher.publish_commitments () in
  let* () = Publisher.cement_commitments () in
  let* () = Outbox_execution.publish_executable_messages node_ctxt in
  let*! () = Daemon_event.new_heads_processed reorg.new_chain in
  let* () = Batcher.produce_batches () in
  let* () = Dal_injection_queue.produce_dal_slots ~level:head.level in
  let*! () = Injector.inject ~header:head.header () in
  let*! () = Daemon_event.new_heads_side_process_finished reorg.new_chain in
  Reference.set node_ctxt.degraded false ;
  return_unit

let daemonize state =
  if state.configuration.l1_monitor_finalized || state.node_ctxt.kind = Riscv
  then
    Layer1.iter_finalized_heads
      ~name:"daemon"
      state.node_ctxt.l1_ctxt
      (on_layer_1_head ~finalized:true state)
      ~prefetch:(fun l1_ctxt head ->
        let module Plugin = (val state.plugin) in
        Plugin.Layer1_helpers.prefetch_tezos_blocks l1_ctxt [head])
  else
    Layer1.iter_heads
      ~name:"daemon"
      state.node_ctxt.l1_ctxt
      (on_layer_1_head ~finalized:false state)

let simple_refutation_loop head =
  Refutation_coordinator.process (Layer1.head_of_header head)

let degraded_refutation_loop state (head : Layer1.header) =
  let open Lwt_result_syntax in
  let*! () = Daemon_event.new_head_degraded head.hash head.level in
  let* predecessor = Node_context.get_predecessor_header state.node_ctxt head in
  let* () = Node_context.save_protocol_info state.node_ctxt head ~predecessor in
  let* () = handle_protocol_migration ~catching_up:false state head in
  let* () = Refutation_coordinator.process (Layer1.head_of_header head) in
  let* () = Publisher.publish_commitments () in
  (* Continue to produce batches but ignore any error *)
  let*! (_ : unit tzresult) = Batcher.produce_batches () in
  let*! (_ : unit tzresult) =
    Dal_injection_queue.produce_dal_slots ~level:head.level
  in
  let*! () = Injector.inject () in
  return_unit

let refutation_loop state (head : Layer1.header) =
  if Reference.get state.node_ctxt.degraded then
    degraded_refutation_loop state head
  else simple_refutation_loop head

let rec refutation_daemon ?(restart = false) state =
  let open Lwt_result_syntax in
  let on_error e =
    Format.eprintf "Refutation daemon error: %a@." pp_print_trace e ;
    refutation_daemon ~restart:true state
  in
  let loop ~restart () =
    let*! () =
      if restart then
        let*! () =
          Daemon_event.refutation_loop_retry
            state.node_ctxt.config.loop_retry_delay
        in
        Lwt_unix.sleep state.node_ctxt.config.loop_retry_delay
      else Lwt.return_unit
    in
    Layer1.iter_heads
      ~name:"refutation"
      state.node_ctxt.l1_ctxt
      (refutation_loop state)
  in
  dont_wait (loop ~restart) on_error (fun e -> on_error [Exn e])

let install_finalizer state ~telemetry_cleanup =
  let open Lwt_syntax in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let message = state.node_ctxt.Node_context.cctxt#message in
  let* () = telemetry_cleanup () in
  let* () = message "Shutting down RPC server@." in
  let* () = Rpc_server.shutdown state.rpc_server in
  let* () = message "Shutting down Injector@." in
  let* () = Injector.shutdown () in
  let* () = message "Shutting down Batcher@." in
  let* () = Batcher.shutdown () in
  let* () = message "Shutting down Commitment Publisher@." in
  let* () = Publisher.shutdown () in
  let* () = message "Shutting down Refutation Coordinator@." in
  let* () = Refutation_coordinator.shutdown () in
  let* (_ : unit tzresult) = Node_context_loader.close state.node_ctxt in
  let* () = Event.shutdown_node exit_status in
  Tezos_base_unix.Internal_event_unix.close ()

let maybe_recover_bond ({node_ctxt; configuration; _} as state) =
  let open Lwt_result_syntax in
  (* At the start of the rollup node when in bailout mode check that there is
     an operator who has stake, otherwise stop the node *)
  if Node_context.is_bailout node_ctxt then
    let operator = Node_context.get_operator node_ctxt Purpose.Operating in
    match operator with
    | None ->
        (* this case can't happen because the bailout mode needs an operator to
           start *)
        tzfail Purpose.(Missing_operators (Set.singleton (Purpose Operating)))
    | Some (Single operating_pkh) -> (
        let module Plugin = (val state.plugin) in
        let* last_published_commitment =
          Plugin.Layer1_helpers.get_last_published_commitment
            ~allow_unstake:false
            node_ctxt.cctxt
            configuration.sc_rollup_address
            operating_pkh
        in
        match last_published_commitment with
        | None ->
            (* when the operator is no longer stake on any commitment, then recover bond *)
            Publisher.recover_bond node_ctxt
        | Some _ ->
            (* when the operator is still stake on something *)
            return_unit)
  else return_unit

let check_operator_balance state =
  let open Lwt_result_syntax in
  match Reference.get state.node_ctxt.lpc with
  | Some _ ->
      (* Operator has already published a commitment, so has staked. *)
      return_unit
  | None -> (
      let publisher =
        Purpose.find_operator Operating state.node_ctxt.config.operators
      in
      match publisher with
      | None ->
          (* Not operating the rollup, so no need to stake. *)
          return_unit
      | Some (Single publisher) ->
          let (module Plugin) = state.plugin in
          let* balance =
            Plugin.Layer1_helpers.get_balance_mutez
              state.node_ctxt.cctxt
              publisher
          in
          if balance > 10_000_000_000L (* 10ktez *) then return_unit
          else
            failwith
              "Operator %a only has %f tz but needs more than 10000 in order \
               to stake on the rollup."
              Signature.Public_key_hash.pp
              publisher
              (Int64.to_float balance /. 1_000_000.))

let make_signers_for_injector operators =
  let update map (purpose, operator) =
    match operator with
    | Purpose.Operator (Single operator) | Operator (Multiple [operator]) ->
        Signature.Public_key_hash.Map.update
          operator
          (function
            | None -> Some ([operator], [purpose])
            | Some ([operator], purposes) ->
                Some ([operator], purpose :: purposes)
            | Some (operators, purposes) ->
                invalid_arg
                  (Format.asprintf
                     "operator %a appears to be used in another purpose (%a) \
                      with multiple keys (%a). It can't be reused for another \
                      purpose %a."
                     Signature.Public_key_hash.pp
                     operator
                     (Format.pp_print_list Purpose.pp_ex_purpose)
                     purposes
                     (Format.pp_print_list Signature.Public_key_hash.pp)
                     operators
                     Purpose.pp_ex_purpose
                     purpose))
          map
    | Operator (Multiple operators) ->
        List.fold_left
          (fun map pkh ->
            Signature.Public_key_hash.Map.update
              pkh
              (function
                | None -> Some (operators, [purpose])
                | Some (_operators, purposes) ->
                    invalid_arg
                      (Format.asprintf
                         "operator is already used in another purpose (%a). It \
                          can't be reused for a purpose with multiple keys."
                         (Format.pp_print_list Purpose.pp_ex_purpose)
                         purposes))
              map)
          map
          operators
  in
  List.fold_left
    update
    Signature.Public_key_hash.Map.empty
    (Purpose.operators_bindings operators)
  |> Signature.Public_key_hash.Map.bindings |> List.map snd
  |> List.sort_uniq (fun (operators, _purposes) (operators', _purposes) ->
         List.compare Signature.Public_key_hash.compare operators operators')
  |> List.map (fun (operators, purposes) ->
         let operation_kinds =
           List.flatten @@ List.map Purpose.operation_kind purposes
         in
         let strategy =
           match operation_kinds with
           | [Operation_kind.Add_messages; Publish_dal_commitment]
           | [Publish_dal_commitment; Add_messages] ->
               (* For the batcher we delay half a block to allow more
                  operations to get in. *)
               `Delay_block 0.5
           | _ -> `Each_block
         in
         (operators, strategy, operation_kinds))

let maybe_performance_metrics state =
  let open Lwt_syntax in
  let+ activate =
    if not state.configuration.performance_metrics then return_false
    else Octez_performance_metrics.supports_performance_metrics ()
  in
  if activate then
    let (module Performance) = Lazy.force_val Metrics.performance_metrics in
    let rec collect () =
      let* () = Performance.set_stats ~data_dir:state.node_ctxt.data_dir in
      let* () = Lwt_unix.sleep 10. in
      collect ()
    in
    Metrics.wrap @@ fun () -> Lwt.dont_wait collect ignore

let rec process_daemon ({node_ctxt; _} as state) =
  let open Lwt_result_syntax in
  let fatal_error_exit e =
    Format.eprintf "%!%a@.Exiting.@." pp_print_trace e ;
    let*! _ = Lwt_exit.exit_and_wait 1 in
    return_unit
  in
  let error_to_degraded_mode e =
    let*! () = Daemon_event.error e in
    Reference.set node_ctxt.degraded true ;
    if node_ctxt.config.no_degraded then fatal_error_exit e
    else
      let*! () =
        Daemon_event.main_loop_retry node_ctxt.config.loop_retry_delay
      in
      let*! () = Lwt_unix.sleep node_ctxt.config.loop_retry_delay in
      process_daemon state
  in
  let handle_preimage_not_found e =
    (* When running/initialising a rollup node with missing preimages
       the rollup node enter in a degraded mode where actually there
       isn't much that can be done with a non initialised rollup node,
       hence it should exit after printing the error logs.

       A safe way to do this is to check if there was a processed head.
       If not we can exit safely. If there was a processed head, we
       go deeper and we check if the most recent commitment is actually
       the genesis' one. If that's the case it means we're still on the
       initialisation phase which means we can exit safely as well, if
       not it means there is potential commitment(s) where refutation
       can be played so we enter in degraded mode. *)
    let* head = Node_context.last_processed_head_opt node_ctxt in
    match head with
    | Some head ->
        if
          Commitment.Hash.(
            Sc_rollup_block.most_recent_commitment head.header
            = node_ctxt.genesis_info.commitment_hash)
        then fatal_error_exit e
        else error_to_degraded_mode e
    | None -> fatal_error_exit e
  in
  let loop () = daemonize state in
  protect loop ~on_error:(function
    | ( Rollup_node_errors.(
          ( Lost_game _ | Unparsable_boot_sector _ | Invalid_genesis_state _
          | Operator_not_in_whitelist | Cannot_patch_pvm_of_public_rollup
          | Disagree_with_cemented _ | Disagree_with_commitment _
          | Inconsistent_inbox _ ))
      | Purpose.Missing_operators _ )
      :: _ as e ->
        fatal_error_exit e
    | Rollup_node_errors.Could_not_open_preimage_file _ :: _ as e ->
        handle_preimage_not_found e
    | Rollup_node_errors.Exit_bond_recovered_bailout_mode :: [] ->
        let*! () = Daemon_event.exit_bailout_mode () in
        let*! _ = Lwt_exit.exit_and_wait 0 in
        return_unit
    | e -> error_to_degraded_mode e)

let initialize_metrics ({node_ctxt; configuration; _} as state)
    (current_protocol : Node_context.current_protocol) history_mode =
  let open Lwt_result_syntax in
  (* Do not wrap active_metrics *)
  Metrics.active_metrics configuration ;
  let* () =
    Metrics.wrap_lwt @@ fun () ->
    Metrics.Info.init_rollup_node_info
      configuration
      ~genesis_level:node_ctxt.genesis_info.level
      ~genesis_hash:node_ctxt.genesis_info.commitment_hash
      ~pvm_kind:(Octez_smart_rollup.Kind.to_string node_ctxt.kind)
      ~history_mode ;
    Metrics.Info.set_proto_info current_protocol.hash current_protocol.constants ;
    let* first_available_level = Node_context.first_available_level node_ctxt in
    Metrics.GC.set_oldest_available_level first_available_level ;
    let* head = Node_context.last_processed_head_opt node_ctxt in
    Option.iter
      (fun Sc_rollup_block.{header = {level; _}; _} ->
        Metrics.Inbox.set_head_level level)
      head ;
    return_unit
  in
  let*! () = maybe_performance_metrics state in
  return_unit

let run ({node_ctxt; configuration; plugin; _} as state) =
  let open Lwt_result_syntax in
  let module Plugin = (val state.plugin) in
  let current_protocol = Reference.get node_ctxt.current_protocol in
  let* history_mode = Node_context.get_history_mode node_ctxt in
  let* () = initialize_metrics state current_protocol history_mode in
  let signers = make_signers_for_injector node_ctxt.config.operators in
  let* () =
    unless (signers = []) @@ fun () ->
    Injector.init
      node_ctxt.cctxt
      (Layer1.raw_l1_connection node_ctxt.l1_ctxt)
      {
        cctxt = (node_ctxt.cctxt :> Client_context.full);
        fee_parameters = configuration.fee_parameters;
        minimal_block_delay = current_protocol.constants.minimal_block_delay;
        delay_increment_per_round =
          current_protocol.constants.delay_increment_per_round;
        max_batch_length = configuration.injector.max_batch_length;
      }
      ~data_dir:node_ctxt.data_dir
      ~signers
      ~retention_period:configuration.injector.retention_period
      ~allowed_attempts:configuration.injector.attempts
      ~collect_metrics:(Option.is_some state.configuration.metrics_addr)
  in
  let* () = start_workers plugin node_ctxt in
  Lwt.dont_wait
    (fun () ->
      let*! r = Metrics.metrics_serve configuration.metrics_addr in
      match r with
      | Ok () -> Lwt.return_unit
      | Error err ->
          Event.(metrics_ended (Format.asprintf "%a" pp_print_trace err)))
    (fun exn -> Event.(metrics_ended_dont_wait (Printexc.to_string exn))) ;
  let* whitelist =
    Plugin.Layer1_helpers.find_whitelist
      node_ctxt.cctxt
      configuration.sc_rollup_address
  in
  let*? () =
    match whitelist with
    | Some whitelist ->
        Node_context.check_op_in_whitelist_or_bailout_mode node_ctxt whitelist
    | None -> Result_syntax.return_unit
  in
  let* () = maybe_recover_bond state in
  let*! () =
    Event.node_is_ready
      ~rpc_addr:configuration.rpc_addr
      ~rpc_port:configuration.rpc_port
  in
  (* Run the refutation daemon in the background if needed. *)
  if Refutation_coordinator.start_in_mode configuration.mode then
    refutation_daemon state ;
  (* Run the main rollup node daemon. *)
  process_daemon state

module Internal_for_tests = struct
  (** Same as {!update_l2_chain} but only builds and stores the L2 block
        corresponding to [messages]. It is used by the unit tests to build an L2
        chain. *)
  let process_messages (module Plugin : Protocol_plugin_sig.S)
      (node_ctxt : _ Node_context.t) ~is_first_block ~predecessor head messages
      =
    let open Lwt_result_syntax in
    let* ctxt = previous_context node_ctxt ~predecessor in
    let* () = Node_context.save_level node_ctxt (Layer1.head_of_header head) in
    let* inbox_hash, inbox, inbox_witness, messages =
      Plugin.Inbox.Internal_for_tests.process_messages
        node_ctxt
        ~is_first_block
        ~predecessor
        messages
    in
    let* _num_messages, num_ticks, initial_tick =
      Interpreter.process_head
        (module Plugin)
        node_ctxt
        ctxt
        ~predecessor:(Layer1.head_of_header predecessor)
        (Layer1.head_of_header head)
        (inbox, messages)
    in
    let*! context_hash = Context.commit ctxt in
    let* commitment_hash =
      Publisher.process_head
        (module Plugin)
        node_ctxt
        ~predecessor:predecessor.Layer1.hash
        head
        ctxt
    in
    let level = head.level in
    let* previous_commitment_hash =
      if level = node_ctxt.genesis_info.level then
        (* Previous commitment for rollup genesis is itself. *)
        return node_ctxt.genesis_info.commitment_hash
      else
        let+ pred = Node_context.get_l2_block node_ctxt predecessor.hash in
        Sc_rollup_block.most_recent_commitment pred.header
    in
    let* () = register_outbox_messages (module Plugin) node_ctxt ctxt level in
    let header =
      Sc_rollup_block.
        {
          block_hash = head.hash;
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
    let* () = Node_context.save_l2_block node_ctxt l2_block in
    let* () = Node_context.set_l2_head node_ctxt l2_block in
    return l2_block
end

let plugin_of_first_block cctxt (block : Layer1.header) =
  let open Lwt_result_syntax in
  let* {current_protocol; _} =
    Tezos_shell_services.Shell_services.Blocks.protocols
      cctxt
      ~block:(`Hash (block.hash, 0))
      ()
  in
  let*? plugin = Protocol_plugins.proto_plugin_for_protocol current_protocol in
  return (current_protocol, plugin)

let setup_opentelemetry ~data_dir config =
  Octez_telemetry.Opentelemetry_setup.setup
    ~data_dir
    ~service_namespace:"rollup_node"
    ~service_name:"rollup_node"
    ~version:
      Tezos_version_value.Bin_version.octez_smart_rollup_node_version_string
    config.Configuration.opentelemetry

let run ~data_dir ~irmin_cache_size (configuration : Configuration.t)
    (cctxt : Client_context.full) =
  let open Lwt_result_syntax in
  let* () =
    Tezos_base_unix.Internal_event_unix.enable_default_daily_logs_at
      ~daily_logs_path:Filename.Infix.(data_dir // "daily_logs")
  in
  let* cctxt =
    Layer_1.client_context cctxt ~timeout:configuration.l1_rpc_timeout
  in
  Random.self_init () (* Initialize random state (for reconnection delays) *) ;
  let*! () = Event.starting_node () in
  let open Configuration in
  let* () =
    (* Check that the operators are valid keys. *)
    List.iter_ep
      (fun (_purpose, operator) ->
        let operator_list =
          match operator with
          | Purpose.Operator (Single operator) -> [operator]
          | Operator (Multiple operators) -> operators
        in
        List.iter_es
          (fun operator ->
            let* _alias, _pk, _sk_uri = Client_keys.get_key cctxt operator in
            return_unit)
          operator_list)
      (Purpose.operators_bindings configuration.operators)
  in
  let* telemetry_cleanup = setup_opentelemetry ~data_dir configuration in
  let* l1_ctxt =
    Layer1.start
      ~name:"sc_rollup_node"
      ~reconnection_delay:configuration.reconnection_delay
      ~l1_blocks_cache_size:configuration.l1_blocks_cache_size
      ?prefetch_blocks:configuration.prefetch_blocks
      cctxt
  in
  let*! head = Layer1.wait_first l1_ctxt in
  let* predecessor =
    Layer1.fetch_tezos_shell_header l1_ctxt head.header.predecessor
  in
  let publisher = Purpose.find_operator Operating configuration.operators in

  let* protocol, plugin = plugin_of_first_block cctxt head in
  let module Plugin = (val plugin) in
  let* constants =
    Plugin.Layer1_helpers.retrieve_constants ~block:(`Hash (head.hash, 0)) cctxt
  and* genesis_info =
    Plugin.Layer1_helpers.retrieve_genesis_info
      cctxt
      configuration.sc_rollup_address
  and* lcc =
    Plugin.Layer1_helpers.get_last_cemented_commitment
      cctxt
      configuration.sc_rollup_address
  and* lpc =
    Option.filter_map_es
      (function
        | Purpose.Single operator ->
            Plugin.Layer1_helpers.get_last_published_commitment
              cctxt
              configuration.sc_rollup_address
              operator)
      publisher
  and* kind =
    Plugin.Layer1_helpers.get_kind cctxt configuration.sc_rollup_address
  and* last_whitelist_update =
    Plugin.Layer1_helpers.find_last_whitelist_update
      cctxt
      configuration.sc_rollup_address
  in
  Metrics.wrap (fun () ->
      Metrics.Info.set_lcc_level_l1 lcc.level ;
      Option.iter
        (fun Commitment.{inbox_level = l; _} -> Metrics.Info.set_lpc_level_l1 l)
        lpc) ;
  let current_protocol =
    {
      Node_context.hash = protocol;
      proto_level = predecessor.proto_level;
      constants;
    }
  in
  let* node_ctxt =
    Node_context_loader.init
      cctxt
      ~data_dir
      ~irmin_cache_size
      ~store_access:Read_write
      ~context_access:Read_write
      l1_ctxt
      genesis_info
      ~lcc
      ~lpc
      ?last_whitelist_update
      kind
      current_protocol
      configuration
  in
  let dir = Rpc_directory.directory node_ctxt in
  let* rpc_server =
    Rpc_server.start
      ~rpc_addr:configuration.rpc_addr
      ~rpc_port:configuration.rpc_port
      ~acl:configuration.acl
      ~cors:configuration.cors
      dir
  in
  let state = {node_ctxt; rpc_server; configuration; plugin} in
  let* () = check_operator_balance state in
  let* () = Node_context.save_protocols_from_l1 node_ctxt in
  let (_ : Lwt_exit.clean_up_callback_id) =
    install_finalizer state ~telemetry_cleanup
  in
  run state

module Replay = struct
  let preload_wasmer node_ctxt l2_block =
    let open Lwt_result_syntax in
    match node_ctxt.Node_context.kind with
    | Example_arith | Riscv -> return_unit
    | Wasm_2_0_0 ->
        Format.eprintf
          "Preloading \
           kernel............................................................. \
           %!" ;
        let* () =
          Wasm_2_0_0_utilities.preload_kernel
            node_ctxt
            l2_block.Sc_rollup_block.header
        in
        Format.eprintf "[\x1B[1;32mOK\x1B[0m]@." ;
        return_unit

  let mk_node_ctxt ~data_dir ~profiling cctxt block =
    let open Lwt_result_syntax in
    let* store = Store.init Read_only ~data_dir in
    let opt_get msg = function
      | None -> error_with "Could not find %s" msg
      | Some s -> Ok s
    in
    let* level =
      match block with
      | `Hash h ->
          let* l = Store.L2_blocks.find_level store h in
          let*? l = opt_get "level" l in
          return l
      | `Level l -> return l
    in
    let* metadata = Metadata.read_metadata_file ~dir:data_dir in
    let*? metadata =
      match metadata with
      | None -> error_with "Missing metadata file for snapshot node context"
      | Some m -> Ok m
    in
    let* protocol = Store.Protocols.proto_of_level store level in
    let*? protocol = opt_get "protocol" protocol in
    Format.eprintf "Using protocol %a@." Protocol_hash.pp protocol.protocol ;
    let*? (module Plugin) =
      Protocol_plugins.proto_plugin_for_protocol protocol.protocol
    in
    let*! () = Store.close store in
    let* constants =
      Plugin.Layer1_helpers.retrieve_constants ~block:(`Level level) cctxt
    in
    let*? l1_ctxt =
      Layer1.create
        ~name:"sc_rollup_node_replay"
        ~reconnection_delay:1.
        ~l1_blocks_cache_size:2
        cctxt
    in
    let config_file = Configuration.config_filename ~data_dir None in
    let* configuration =
      Configuration.Cli.create_or_read_config
        ~config_file
        ~rpc_addr:None
        ~rpc_port:None
        ~acl_override:None
        ~metrics_addr:None
        ~disable_performance_metrics:false
        ~loser_mode:None
        ~reconnection_delay:None
        ~dal_node_endpoint:None
        ~pre_images_endpoint:None
        ~injector_retention_period:None
        ~injector_attempts:None
        ~injection_ttl:None
        ~mode:None
        ~sc_rollup_address:None
        ~boot_sector_file:None
        ~operators:[]
        ~index_buffer_size:None
        ~irmin_cache_size:None
        ~log_kernel_debug:true
        ~log_kernel_debug_file:None
        ~unsafe_disable_wasm_kernel_checks:false
        ~no_degraded:true
        ~gc_frequency:None
        ~history_mode:None
        ~allowed_origins:None
        ~allowed_headers:None
        ~apply_unsafe_patches:false
        ~bail_on_disagree:false
        ~profiling
        ~force_etherlink:false
        ~l1_monitor_finalized:None
    in
    let* cleanup = setup_opentelemetry ~data_dir configuration in
    let* node_ctxt =
      Node_context_loader.init
        cctxt
        ~data_dir
        ~irmin_cache_size:10
        ~store_access:Read_only
        ~context_access:Read_only
        l1_ctxt
        metadata.genesis_info
        ~lcc:{commitment = Commitment.Hash.zero; level = 0l}
        ~lpc:None
        metadata.kind
        {
          hash = protocol.protocol;
          proto_level = protocol.proto_level;
          constants;
        }
        configuration
    in
    return (node_ctxt, cleanup)

  let process_time_treshold =
    Ptime.Span.of_float_s 0.5 |> WithExceptions.Option.get ~loc:__LOC__

  let replay_block_aux ?(preload = false) ?(verbose = false) node_ctxt block =
    let open Lwt_result_syntax in
    let* hash, level =
      match block with
      | `Level l ->
          let+ h = Node_context.hash_of_level node_ctxt l in
          (h, l)
      | `Hash h ->
          let+ l = Node_context.level_of_hash node_ctxt h in
          (h, l)
    in
    let* block = Node_context.get_full_l2_block node_ctxt hash in
    let* () = when_ preload @@ fun () -> preload_wasmer node_ctxt block in
    let start_timestamp = Time.System.now () in
    if verbose then
      Format.eprintf
        "@[<v 2>\x1B[1mReplaying block: \x1B[0m@,%a@,@]@."
        Data_encoding.Json.pp
        (Data_encoding.Json.construct Sc_rollup_block.full_encoding block)
    else
      Format.eprintf
        "\x1B[1mReplaying block \x1B[1;36m%ld\x1B[0m (%a)  %!"
        level
        Block_hash.pp
        hash ;
    let* ctxt =
      Node_context.checkout_context node_ctxt block.header.predecessor
    in
    let* (module Plugin) =
      Protocol_plugins.proto_plugin_for_level node_ctxt block.header.level
    in
    let* _num_messages, num_ticks, initial_tick =
      Interpreter.process_head
        (module Plugin)
        node_ctxt
        ctxt
        ~predecessor:
          Layer1.
            {
              level = Int32.pred block.header.level;
              hash = block.header.predecessor;
            }
        Layer1.{level = block.header.level; hash = block.header.block_hash}
        (block.content.inbox, block.content.messages)
    in
    let get_pvm_state ctxt =
      let*! pvm_state = Context.PVMState.find ctxt in
      let*? pvm_state =
        match pvm_state with
        | Some pvm_state -> Ok pvm_state
        | None ->
            error_with
              "PVM state at level %ld is not available"
              block.header.level
      in
      let*! compressed_state = Plugin.Pvm.state_hash node_ctxt.kind pvm_state in
      return compressed_state
    in
    let* commitment =
      Publisher.create_commitment_if_necessary
        (module Plugin)
        node_ctxt
        block.header.level
        ~predecessor:block.header.predecessor
        ctxt
    in
    let commitment_hash = Option.map Commitment.hash commitment in
    let stop_timestamp = Time.System.now () in
    let time = Ptime.diff stop_timestamp start_timestamp in
    let* stored_commitment =
      Option.map_es
        (fun c -> Node_context.get_commitment node_ctxt c)
        block.header.commitment_hash
    in
    let* block_ctxt =
      Node_context.checkout_context node_ctxt block.header.block_hash
    in
    let failed = ref false in
    if num_ticks <> block.num_ticks then (
      Format.eprintf
        "Computed block has %Ld ticks but stored as %Ld ticks@."
        num_ticks
        block.num_ticks ;
      failed := true) ;
    if Z.Compare.(initial_tick <> block.initial_tick) then (
      Format.eprintf
        "Computed block starts with initial tick %a but stored with initial \
         tick %a@."
        Z.pp_print
        initial_tick
        Z.pp_print
        block.initial_tick ;
      failed := true) ;
    let pp_opt pp fmt = function
      | None -> Format.pp_print_string fmt "None"
      | Some a -> pp fmt a
    in
    if
      not
      @@ Option.equal
           Commitment.Hash.equal
           commitment_hash
           block.header.commitment_hash
    then (
      Format.eprintf
        "Computed block has commitment %a but stored with commitment %a@."
        (pp_opt Commitment.pp)
        commitment
        (pp_opt Commitment.pp)
        stored_commitment ;
      failed := true) ;
    let* pvm_state = get_pvm_state ctxt in
    let* stored_pvm_state = get_pvm_state block_ctxt in
    if State_hash.(pvm_state <> stored_pvm_state) then (
      Format.eprintf
        "Computed block has PVM state %a but stored with PVM state %a@."
        State_hash.pp
        pvm_state
        State_hash.pp
        stored_pvm_state ;
      failed := true) ;

    if !failed then failwith "Replayed block differed from the one on disk"
    else
      let block_time =
        (Reference.get node_ctxt.current_protocol).constants.minimal_block_delay
        |> Int64.to_float
      in
      let color_time =
        let time_f = Ptime.Span.to_float_s time in
        if time_f > 2. *. block_time then "\x1B[1;7;31m"
        else if time_f > block_time then "\x1B[1;31m"
        else if time_f > block_time /. 2. then "\x1B[31m"
        else if time_f > block_time /. 5. then "\x1B[33m"
        else "\x1B[0m"
      in
      if not verbose then
        Format.eprintf
          "[\x1B[1;32mOK\x1B[0m] %s %a \x1B[0m@."
          color_time
          Ptime.Span.pp
          time
      else
        Format.eprintf
          "\x1B[32mReplayed block %ld \x1B[1msuccessfully\x1B[0m in \
           %s%a\x1B[0m!@."
          level
          color_time
          Ptime.Span.pp
          time ;
      return_unit

  let replay_block ~data_dir ?profiling cctxt block =
    let open Lwt_result_syntax in
    let* node_ctxt, cleanup = mk_node_ctxt ~profiling ~data_dir cctxt block in
    Lwt.finalize
      (fun () -> replay_block_aux ~preload:true ~verbose:true node_ctxt block)
      cleanup

  let replay_blocks ~data_dir ?profiling cctxt start_level end_level =
    let open Lwt_result_syntax in
    let* node_ctxt, cleanup =
      mk_node_ctxt ~profiling ~data_dir cctxt (`Level start_level)
    in
    Lwt.finalize
      (fun () ->
        let levels =
          Stdlib.List.init
            (Int32.sub end_level start_level |> Int32.to_int |> succ)
            (fun x -> Int32.add (Int32.of_int x) start_level)
        in
        let first = ref true in
        List.iter_es
          (fun l ->
            let preload = !first in
            first := false ;
            replay_block_aux ~preload node_ctxt (`Level l))
          levels)
      cleanup
end
