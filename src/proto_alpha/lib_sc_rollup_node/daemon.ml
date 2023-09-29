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

open Daemon_helpers

let before_origination (node_ctxt : _ Node_context.t) (header : Layer1.header) =
  let origination_level = node_ctxt.genesis_info.level in
  header.level < origination_level

let previous_context (node_ctxt : _ Node_context.t)
    ~(predecessor : Layer1.header) =
  let open Lwt_result_syntax in
  if predecessor.level < node_ctxt.genesis_info.level then
    (* This is before we have interpreted the boot sector, so we start
       with an empty context in genesis *)
    return (Context.empty node_ctxt.context)
  else Node_context.checkout_context node_ctxt predecessor.Layer1.hash

let classify_head (node_ctxt : _ Node_context.t)
    ?(predecessor : Layer1.header option) (head : Layer1.header) =
  let open Lwt_result_syntax in
  if head.header.proto_level = node_ctxt.current_protocol.proto_level then
    (* Same protocol as supported one, ok *)
    return `Supported_proto
  else if head.header.proto_level = node_ctxt.current_protocol.proto_level + 1
  then
    let* predecessor =
      match predecessor with
      | Some p -> return p
      | None -> Node_context.get_predecessor_header node_ctxt head
    in
    if predecessor.header.proto_level = node_ctxt.current_protocol.proto_level
    then
      (* Migration block from supported protocol to the next one, ok *)
      return `Supported_migration
    else return `Unsupported_proto
  else return `Unsupported_proto

let exit_on_other_proto node_ctxt ?predecessor head =
  let open Lwt_result_syntax in
  let* proto_class = classify_head node_ctxt ?predecessor head in
  match proto_class with
  | `Supported_proto | `Supported_migration -> return_unit
  | `Unsupported_proto ->
      let*! () = Event.detected_protocol_migration () in
      let*! _ = Lwt_exit.exit_and_wait 0 in
      return_unit

let exit_after_proto_migration node_ctxt ?predecessor head =
  let open Lwt_result_syntax in
  let* proto_class = classify_head node_ctxt ?predecessor head in
  match proto_class with
  | `Supported_proto -> return_unit
  | `Supported_migration | `Unsupported_proto ->
      let*! () = Event.detected_protocol_migration () in
      let*! _ = Lwt_exit.exit_and_wait 0 in
      return_unit

let rec process_l1_block (daemon_components : (module Daemon_components.S))
    (node_ctxt : _ Node_context.t) ~catching_up (head : Layer1.header) =
  let open Lwt_result_syntax in
  if before_origination node_ctxt head then return `Nothing
  else
    let* l2_head = Node_context.find_l2_block node_ctxt head.hash in
    match l2_head with
    | Some l2_head ->
        (* Already processed *)
        return (`Already_processed l2_head)
    | None -> (
        (* New head *)
        let*! () = Daemon_event.head_processing head.hash head.level in
        let* predecessor =
          Node_context.get_predecessor_header_opt node_ctxt head
        in
        match predecessor with
        | None ->
            (* Predecessor not available on the L1, which means the block does not
               exist in the chain. *)
            return `Nothing
        | Some predecessor ->
            let* () = exit_on_other_proto node_ctxt ~predecessor head in
            let* () =
              update_l2_chain
                daemon_components
                node_ctxt
                ~catching_up:true
                predecessor
            in
            let* ctxt = previous_context node_ctxt ~predecessor in
            let* () =
              Node_context.save_protocol_info node_ctxt head ~predecessor
            in
            let* inbox_hash, inbox, inbox_witness, messages =
              Inbox.process_head node_ctxt ~predecessor head
            in
            let inbox_witness =
              Sc_rollup_proto_types.Merkelized_payload_hashes_hash.to_octez
                inbox_witness
            in
            let inbox_hash =
              Sc_rollup_proto_types.Inbox_hash.to_octez inbox_hash
            in
            let* () =
              when_ (Node_context.dal_supported node_ctxt) @@ fun () ->
              Dal_slots_tracker.process_head
                node_ctxt
                (Layer1.head_of_header head)
            in
            let* () = process_l1_block_operations node_ctxt head in
            (* Avoid storing and publishing commitments if the head is not final. *)
            (* Avoid triggering the pvm execution if this has been done before for
               this head. *)
            let* ctxt, _num_messages, num_ticks, initial_tick =
              Interpreter.process_head
                (module Rollup_node_plugin.Plugin)
                node_ctxt
                ctxt
                ~predecessor
                head
                (inbox, messages)
            in
            let*! context_hash = Context.commit ctxt in
            let* commitment_hash =
              Publisher.process_head
                (module Rollup_node_plugin.Plugin)
                node_ctxt
                ~predecessor:predecessor.hash
                head
                ctxt
            in
            let commitment_hash =
              Option.map
                Sc_rollup_proto_types.Commitment_hash.to_octez
                commitment_hash
            in
            let* () =
              unless (catching_up && Option.is_none commitment_hash)
              @@ fun () -> Inbox.same_as_layer_1 node_ctxt head.hash inbox
            in
            let level = head.level in
            let* previous_commitment_hash =
              if level = node_ctxt.genesis_info.level then
                (* Previous commitment for rollup genesis is itself. *)
                return node_ctxt.genesis_info.commitment_hash
              else
                let+ pred =
                  Node_context.get_l2_block node_ctxt predecessor.hash
                in
                Sc_rollup_block.most_recent_commitment pred.header
            in
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
            let* () =
              Node_context.mark_finalized_level
                node_ctxt
                Int32.(sub head.level (of_int node_ctxt.block_finality_time))
            in
            let* () = Node_context.save_l2_block node_ctxt l2_block in
            return (`New l2_block))

and update_l2_chain daemon_components node_ctxt ~catching_up
    (head : Layer1.header) =
  let open Lwt_result_syntax in
  let start_timestamp = Time.System.now () in
  let* () =
    Node_context.save_level
      node_ctxt
      {Layer1.hash = head.hash; level = head.level}
  in
  let* done_ = process_l1_block daemon_components node_ctxt ~catching_up head in
  match done_ with
  | `Nothing -> return_unit
  | `Already_processed l2_block ->
      let return_l2 = Node_context.set_l2_head node_ctxt l2_block in
      Lwt_watcher.notify node_ctxt.global_block_watcher l2_block ;
      return_l2
  | `New l2_block ->
      let* () = Node_context.set_l2_head node_ctxt l2_block in
      Lwt_watcher.notify node_ctxt.global_block_watcher l2_block ;
      let stop_timestamp = Time.System.now () in
      let process_time = Ptime.diff stop_timestamp start_timestamp in
      Metrics.Inbox.set_process_time process_time ;
      let*! () =
        Daemon_event.new_head_processed head.hash head.level process_time
      in
      return_unit

(* [on_layer_1_head node_ctxt head] processes a new head from the L1. It
   also processes any missing blocks that were not processed. *)
let on_layer_1_head (daemon_components : (module Daemon_components.S)) node_ctxt
    (head : Layer1.header) =
  let open Lwt_result_syntax in
  let (module Components) = daemon_components in
  let* old_head = Node_context.last_processed_head_opt node_ctxt in
  let old_head =
    match old_head with
    | Some h ->
        `Head Layer1.{hash = h.header.block_hash; level = h.header.level}
    | None ->
        (* if no head has been processed yet, we want to handle all blocks
           since, and including, the rollup origination. *)
        let origination_level = node_ctxt.genesis_info.level in
        `Level (Int32.pred origination_level)
  in
  let stripped_head = Layer1.head_of_header head in
  let*! reorg =
    Node_context.get_tezos_reorg_for_new_head node_ctxt old_head stripped_head
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
           info on the Layer 1. We fallback to a recursive process_l1_block. *)
        Ok {Reorg.no_reorg with new_chain = [stripped_head]}
    | _ -> reorg
  in
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
        Layer1_helpers.prefetch_tezos_blocks node_ctxt.l1_ctxt to_prefetch ;
        let* header = get_header block in
        let catching_up = block.level < head.level in
        update_l2_chain daemon_components node_ctxt ~catching_up header)
      new_chain_prefetching
  in
  let* () = Publisher.publish_commitments () in
  let* () = Publisher.cement_commitments () in
  let*! () = Daemon_event.new_heads_processed reorg.new_chain in
  let* () = Refutation_coordinator.process stripped_head in
  let* () = Components.Batcher.new_head stripped_head in
  let*! () = Injector.inject ~header:head.header () in
  let* () = exit_after_proto_migration node_ctxt head in
  return_unit

let daemonize (daemon_components : (module Daemon_components.S))
    (node_ctxt : _ Node_context.t) =
  Layer1.iter_heads
    node_ctxt.l1_ctxt
    (on_layer_1_head daemon_components node_ctxt)

let degraded_refutation_mode (daemon_components : (module Daemon_components.S))
    (node_ctxt : _ Node_context.t) =
  let open Lwt_result_syntax in
  let (module Components) = daemon_components in
  let*! () = Daemon_event.degraded_mode () in
  let message = node_ctxt.Node_context.cctxt#message in
  let*! () = message "Shutting down Batcher@." in
  let*! () = Components.Batcher.shutdown () in
  let*! () = message "Shutting down Commitment Publisher@." in
  let*! () = Publisher.shutdown () in
  Layer1.iter_heads node_ctxt.l1_ctxt @@ fun head ->
  let* () = Refutation_coordinator.process (Layer1.head_of_header head) in
  let*! () = Injector.inject () in
  let* () = exit_on_other_proto node_ctxt head in
  return_unit

let install_finalizer (daemon_components : (module Daemon_components.S))
    node_ctxt rpc_server =
  let open Lwt_syntax in
  let (module Components) = daemon_components in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let message = node_ctxt.Node_context.cctxt#message in
  let* () = message "Shutting down RPC server@." in
  let* () = Components.RPC_server.shutdown rpc_server in
  let* () = message "Shutting down Injector@." in
  let* () = Injector.shutdown () in
  let* () = message "Shutting down Batcher@." in
  let* () = Components.Batcher.shutdown () in
  let* () = message "Shutting down Commitment Publisher@." in
  let* () = Publisher.shutdown () in
  let* () = message "Shutting down Refutation Coordinator@." in
  let* () = Refutation_coordinator.shutdown () in
  let* (_ : unit tzresult) = Node_context.close node_ctxt in
  let* () = Event.shutdown_node exit_status in
  Tezos_base_unix.Internal_event_unix.close ()

let run node_ctxt configuration
    (daemon_components : (module Daemon_components.S)) =
  let open Lwt_result_syntax in
  let (module Components) = daemon_components in
  let* () = check_pvm_initial_state_hash node_ctxt in
  let* rpc_server = RPC_server.start node_ctxt configuration in
  let (_ : Lwt_exit.clean_up_callback_id) =
    install_finalizer daemon_components node_ctxt rpc_server
  in
  let start () =
    let*! () = Inbox.start () in
    let signers =
      Configuration.Operator_purpose_map.bindings
        node_ctxt.config.sc_rollup_node_operators
      |> List.fold_left
           (fun acc (operation_kind, operator) ->
             let operation_kinds =
               Configuration.operation_kinds_of_purpose operation_kind
             in
             let operation_kinds =
               match Signature.Public_key_hash.Map.find operator acc with
               | None -> operation_kinds
               | Some kinds -> operation_kinds @ kinds
             in
             Signature.Public_key_hash.Map.add operator operation_kinds acc)
           Signature.Public_key_hash.Map.empty
      |> Signature.Public_key_hash.Map.bindings
      |> List.map (fun (operator, operation_kinds) ->
             let strategy =
               match operation_kinds with
               | [Configuration.Add_messages] -> `Delay_block 0.5
               | _ -> `Each_block
             in
             (operator, strategy, operation_kinds))
    in
    let* () = Publisher.init node_ctxt in
    let* () = Refutation_coordinator.init node_ctxt in
    let* () =
      unless (signers = []) @@ fun () ->
      Injector.init
        node_ctxt.cctxt
        {
          cctxt = (node_ctxt.cctxt :> Client_context.full);
          fee_parameters = configuration.fee_parameters;
          minimal_block_delay =
            node_ctxt.current_protocol.constants.minimal_block_delay;
          delay_increment_per_round =
            node_ctxt.current_protocol.constants.delay_increment_per_round;
        }
        ~data_dir:node_ctxt.data_dir
        ~signers
        ~retention_period:configuration.injector.retention_period
        ~allowed_attempts:configuration.injector.attempts
    in
    let* () =
      match
        Configuration.Operator_purpose_map.find
          Batching
          node_ctxt.config.sc_rollup_node_operators
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
    daemonize daemon_components node_ctxt
  in
  Metrics.Info.init_rollup_node_info
    ~id:configuration.sc_rollup_address
    ~mode:configuration.mode
    ~genesis_level:node_ctxt.genesis_info.level
    ~pvm_kind:(Octez_smart_rollup.Kind.to_string node_ctxt.kind) ;
  let fatal_error_exit e =
    Format.eprintf "%!%a@.Exiting.@." pp_print_trace e ;
    let*! _ = Lwt_exit.exit_and_wait 1 in
    return_unit
  in
  let error_to_degraded_mode e =
    let*! () = Daemon_event.error e in
    degraded_refutation_mode daemon_components node_ctxt
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
          Octez_smart_rollup.Commitment.Hash.(
            Sc_rollup_block.most_recent_commitment head.header
            = node_ctxt.genesis_info.commitment_hash)
        then fatal_error_exit e
        else error_to_degraded_mode e
    | None -> fatal_error_exit e
  in
  protect start ~on_error:(function
      | Sc_rollup_node_errors.(
          Lost_game _ | Unparsable_boot_sector _ | Invalid_genesis_state _)
        :: _ as e ->
          fatal_error_exit e
      | Sc_rollup_node_errors.Could_not_open_preimage_file _ :: _ as e ->
          handle_preimage_not_found e
      | e -> error_to_degraded_mode e)

module Internal_for_tests = struct
  (** Same as {!update_l2_chain} but only builds and stores the L2 block
        corresponding to [messages]. It is used by the unit tests to build an L2
        chain. *)
  let process_messages (node_ctxt : _ Node_context.t) ~is_first_block
      ~predecessor head messages =
    let open Lwt_result_syntax in
    let* ctxt = previous_context node_ctxt ~predecessor in
    let* () = Node_context.save_level node_ctxt (Layer1.head_of_header head) in
    let* inbox_hash, inbox, inbox_witness, messages =
      Inbox.Internal_for_tests.process_messages
        node_ctxt
        ~is_first_block
        ~predecessor
        head
        messages
    in
    let inbox_witness =
      Sc_rollup_proto_types.Merkelized_payload_hashes_hash.to_octez
        inbox_witness
    in
    let inbox_hash = Sc_rollup_proto_types.Inbox_hash.to_octez inbox_hash in
    let* ctxt, _num_messages, num_ticks, initial_tick =
      Interpreter.process_head
        (module Rollup_node_plugin.Plugin)
        node_ctxt
        ctxt
        ~predecessor
        head
        (inbox, messages)
    in
    let*! context_hash = Context.commit ctxt in
    let* commitment_hash =
      Publisher.process_head
        (module Rollup_node_plugin.Plugin)
        node_ctxt
        ~predecessor:predecessor.Layer1.hash
        head
        ctxt
    in
    let commitment_hash =
      Option.map Sc_rollup_proto_types.Commitment_hash.to_octez commitment_hash
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
    let () = Lwt_watcher.notify node_ctxt.global_block_watcher l2_block in
    return l2_block
end

module Rollup_node_daemon_components : Daemon_components.S = struct
  module Batcher = struct
    include Batcher

    let init c = init (module Rollup_node_plugin.Plugin) c
  end

  module RPC_server = RPC_server
end

let run
    ?(daemon_components : (module Daemon_components.S) =
      (module Rollup_node_daemon_components)) ~data_dir ~irmin_cache_size
    ~index_buffer_size ?log_kernel_debug_file (configuration : Configuration.t)
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
  let*! () = Event.waiting_first_block Protocol.hash in
  let* l1_ctxt =
    Layer1.start
      ~name:"sc_rollup_node"
      ~reconnection_delay:configuration.reconnection_delay
      ~l1_blocks_cache_size:configuration.l1_blocks_cache_size
      ~protocols:[Protocol.hash]
      ?prefetch_blocks:configuration.prefetch_blocks
      cctxt
  in
  let*! head = Layer1.wait_first l1_ctxt in
  let* predecessor =
    Layer1.fetch_tezos_shell_header l1_ctxt head.header.predecessor
  in
  let*! () = Event.received_first_block head.hash Protocol.hash in
  let publisher =
    Configuration.Operator_purpose_map.find
      Operating
      configuration.sc_rollup_node_operators
  in
  let* constants = Layer1_helpers.retrieve_constants cctxt
  and* genesis_info =
    Layer1_helpers.retrieve_genesis_info cctxt configuration.sc_rollup_address
  and* lcc =
    Layer1_helpers.get_last_cemented_commitment
      cctxt
      configuration.sc_rollup_address
  and* lpc =
    Option.filter_map_es
      (Layer1_helpers.get_last_published_commitment
         cctxt
         configuration.sc_rollup_address)
      publisher
  and* kind = Layer1_helpers.get_kind cctxt configuration.sc_rollup_address
  and* last_whitelist_update =
    Layer1_helpers.find_last_whitelist_update
      cctxt
      configuration.sc_rollup_address
  in

  let current_protocol =
    {
      Node_context.hash = Protocol.hash;
      proto_level = predecessor.proto_level;
      constants;
    }
  in
  let* node_ctxt =
    Node_context.init
      cctxt
      ~data_dir
      ~irmin_cache_size
      ~index_buffer_size
      ?log_kernel_debug_file
      ?last_whitelist_update
      Read_write
      l1_ctxt
      genesis_info
      ~lcc
      ~lpc
      kind
      current_protocol
      configuration
  in
  run node_ctxt configuration daemon_components
