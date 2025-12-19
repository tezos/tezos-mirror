(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
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

module Profiler = struct
  include (val Profiler.wrap Dal_profiler.dal_profiler)

  let[@warning "-32"] reset_block_section =
    Dal_profiler.create_reset_block_section Dal_profiler.dal_profiler
end

module Gossipsub_profiler = struct
  include
    (val Tezos_profiler.Profiler.wrap Gossipsub.Profiler.gossipsub_profiler)

  let[@warning "-32"] reset_block_section =
    Gossipsub.Profiler.(create_reset_block_section gossipsub_profiler)
end

let[@warning "-32"] may_start_profiler data_dir =
  match Tezos_profiler_unix.Profiler_instance.selected_backends () with
  | Some backends ->
      List.iter
        (fun Tezos_profiler_unix.Profiler_instance.{instance_maker; _} ->
          let profiler_maker = instance_maker ~directory:data_dir in
          Dal_profiler.init profiler_maker ;
          Gossipsub.Profiler.init profiler_maker)
        backends
  | None -> ()

(* Monitor and process finalized heads. *)
let on_new_finalized_head ctxt cctxt crawler =
  let open Lwt_result_syntax in
  let stream = Crawler.finalized_heads_stream crawler in
  let rec loop () =
    let cryptobox = Node_context.get_cryptobox ctxt in
    let*! next_final_head = Lwt_stream.get stream in
    let launch_time = Unix.gettimeofday () in
    match next_final_head with
    | None -> Lwt.fail_with "L1 crawler lib shut down"
    | Some (finalized_block_hash, finalized_shell_header) ->
        ()
        [@profiler.overwrite
          Profiler.reset_block_section (finalized_block_hash, [])] ;
        ()
        [@profiler.overwrite
          Gossipsub_profiler.reset_block_section (finalized_block_hash, [])] ;
        let* () =
          (Block_handler.new_finalized_head
             ctxt
             cctxt
             crawler
             cryptobox
             finalized_block_hash
             finalized_shell_header
             ~launch_time
           [@profiler.record_s
             {verbosity = Notice; profiler_module = Profiler}
               "new_finalized_head"])
        in
        loop ()
  in
  let*! () = Event.emit_layer1_node_tracking_started () in
  loop ()

let daemonize handlers =
  let open Lwt_result_syntax in
  let* handlers = List.map_es (fun x -> x) handlers in
  let (_ : Lwt_exit.clean_up_callback_id) =
    (* close the stream when an exit signal is received *)
    Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _exit_status ->
        List.iter (fun (_, stopper) -> stopper ()) handlers ;
        Lwt.return_unit)
  in
  (let* _ = all (List.map fst handlers) in
   return_unit)
  |> lwt_map_error (List.fold_left (fun acc errs -> errs @ acc) [])

module IntSet = Set.Make (struct
  type t = int

  let compare = compare
end)

let connect_gossipsub_with_p2p proto_parameters gs_worker transport_layer
    node_store node_ctxt amplificator ~verbose =
  let timing_table_size =
    2 * proto_parameters.Types.attestation_lag
    * proto_parameters.cryptobox_parameters.number_of_shards
    * proto_parameters.number_of_slots
  in
  let shards_timing_table =
    Dal_metrics.Slot_id_bounded_map.create timing_table_size
  in
  let disable_amplification =
    let config = Node_context.get_config node_ctxt in
    config.disable_amplification
  in
  let shards_out_handler shard_store =
    (* Counting potentially emitted message is a good way to count the number of shards validated. *)
    let save_and_notify = Store.Shards.write_all shard_store in
    fun Types.Message.{share; _}
        Types.Message_id.{commitment; shard_index; level; slot_index; _}
      ->
      let open Lwt_result_syntax in
      let slot_id : Types.slot_id = {slot_level = level; slot_index} in
      ((let* () =
          (Seq.return {Cryptobox.share; index = shard_index}
          |> save_and_notify slot_id |> Errors.to_tzresult)
          [@profiler.aggregate_s
            {verbosity = Notice; profiler_module = Profiler} "save_and_notify"]
        in
        let* () =
          Attestable_slots.may_notify_attestable_slot_or_trap node_ctxt ~slot_id
        in
        (* Introduce a new store read at each received shard. Not sure it can be
          a problem, though *)
        let* number_of_already_stored_shards =
          (Store.Shards.count_values
             node_store
             slot_id
           [@profiler.aggregate_s
             {verbosity = Notice; profiler_module = Profiler} "count_values"])
        in
        let update_metric_and_emit_event ?min_shards_to_reconstruct_slot
            ~number_of_expected_shards () =
          let open Lwt_syntax in
          let updated, slot_metrics =
            (Dal_metrics.update_timing_shard_validated
               shards_timing_table
               slot_id
               ?min_shards_to_reconstruct_slot
               ~number_of_already_stored_shards
               ~number_of_expected_shards
             [@profiler.aggregate_f
               {verbosity = Notice; profiler_module = Profiler}
                 "update_timing_shard_validated"])
          in
          let* () =
            if updated then
              Event.emit_validation_of_shard_update
                ~level
                ~slot_index
                ~slot_metrics
            else return_unit
          in
          return slot_metrics
        in
        match
          Profile_manager.get_profiles
          @@ Node_context.get_profile_ctxt node_ctxt
        with
        | Controller profile
          when Controller_profiles.can_publish_on_slot_index slot_index profile
          -> (
            (* If one is an observer or an operator for current slot, then they expect all the shards. *)
            let total_number_of_shards =
              proto_parameters.cryptobox_parameters.number_of_shards
            in
            let min_shards_to_reconstruct_slot =
              let redundancy_factor =
                proto_parameters.cryptobox_parameters.redundancy_factor
              in
              total_number_of_shards / redundancy_factor
            in
            let*! slot_metrics =
              update_metric_and_emit_event
                ~min_shards_to_reconstruct_slot
                ~number_of_expected_shards:total_number_of_shards
                ()
            in
            match amplificator with
            | None ->
                let*! () =
                  if not disable_amplification then
                    Event.emit_amplificator_uninitialized ()
                  else Lwt.return_unit
                in
                return_unit
            | Some amplificator ->
                assert (not disable_amplification) ;
                Amplificator.try_amplification
                  commitment
                  slot_metrics
                  slot_id
                  amplificator
                [@profiler.aggregate_s
                  {verbosity = Notice; profiler_module = Profiler}
                    "try_amplification"])
        | Controller profile when Controller_profiles.has_attester profile ->
            (* If one is not observing the slot but is an attester, then they
               expect a number of shards depending of the committee draw. *)
            let attesters = Controller_profiles.attesters profile in
            let* committee =
              let level =
                Int32.add
                  level
                  (Int32.of_int (proto_parameters.Types.attestation_lag - 1))
              in
              Node_context.fetch_committees node_ctxt ~level
            in
            let number_of_expected_shards =
              Signature.Public_key_hash.Set.fold
                (fun pkh acc ->
                  match Signature.Public_key_hash.Map.find pkh committee with
                  | None -> acc
                  | Some (shard_indices, _) -> acc + List.length shard_indices)
                attesters
                0
            in
            let*! _slot_metrics =
              update_metric_and_emit_event ~number_of_expected_shards ()
            in
            return_unit
        | _ -> return_unit)
      [@profiler.aggregate_s
        {verbosity = Notice; profiler_module = Profiler} "output_shards_handler"])
  in
  let shards_in_handler still_to_receive_indices =
   fun Types.Message_id.{level; slot_index; shard_index; _} from_peer ->
    let open Lwt_result_syntax in
    let*! () =
      Event.emit_reception_of_shard_detailed
        ~level
        ~slot_index
        ~shard_index
        ~sender:from_peer
    in
    let slot_id : Types.slot_id = {slot_level = level; slot_index} in
    let update_metric_and_emit_event if_no_shards_yet =
      let still_to_receive_shards =
        match
          Dal_metrics.Slot_id_bounded_map.find_opt
            still_to_receive_indices
            slot_id
        with
        | None -> if_no_shards_yet ()
        | Some l -> l
      in
      let last_expected_shard =
        if IntSet.is_empty still_to_receive_shards then false
        else
          let new_set = IntSet.remove shard_index still_to_receive_shards in
          Dal_metrics.Slot_id_bounded_map.replace
            still_to_receive_indices
            slot_id
            new_set ;
          IntSet.is_empty new_set
      in
      let updated, slot_metrics =
        (Dal_metrics.update_timing_shard_received
           shards_timing_table
           slot_id
           ~last_expected_shard
         [@profiler.aggregate_f
           {verbosity = Notice; profiler_module = Profiler}
             "update_timing_shard_received"])
      in
      let*! () =
        if updated then
          Event.emit_reception_of_shard_update ~level ~slot_index ~slot_metrics
        else Lwt.return_unit
      in
      return_unit
    in
    match[@profiler.aggregate_s
           {verbosity = Notice; profiler_module = Profiler}
             "input_shards_handler"]
      Profile_manager.get_profiles @@ Node_context.get_profile_ctxt node_ctxt
    with
    | Controller profile
      when Controller_profiles.can_publish_on_slot_index slot_index profile ->
        (* If one is an observer or an operator for current slot, then they
           expect all the shards. *)
        let total_number_of_shards =
          proto_parameters.cryptobox_parameters.number_of_shards
        in
        update_metric_and_emit_event (fun () ->
            IntSet.of_list (0 -- (total_number_of_shards - 1)))
    | Controller profile when Controller_profiles.has_attester profile ->
        (* If one is not observing the slot but is an attester, then they expect
           a number of shards depending of the committee draw. *)
        let attesters = Controller_profiles.attesters profile in
        let* committee =
          let level =
            Int32.add
              level
              (Int32.of_int (proto_parameters.Types.attestation_lag - 1))
          in
          Node_context.fetch_committees node_ctxt ~level
        in
        update_metric_and_emit_event (fun () ->
            Signature.Public_key_hash.Set.fold
              (fun pkh acc ->
                match Signature.Public_key_hash.Map.find pkh committee with
                | None -> acc
                | Some (shard_indices, _) ->
                    IntSet.union acc (IntSet.of_list shard_indices))
              attesters
              IntSet.empty)
    | _ -> return_unit
  in
  let still_to_receive_indices =
    Dal_metrics.Slot_id_bounded_map.create
      (proto_parameters.number_of_slots * proto_parameters.attestation_lag)
  in
  Lwt.catch
    (fun () ->
      Gossipsub.Transport_layer_hooks.activate
        gs_worker
        transport_layer
        ~app_out_callback:(shards_out_handler node_store)
        ~app_in_callback:(shards_in_handler still_to_receive_indices)
        ~verbose)
    (fun exn ->
      let msg =
        "[dal_node] error in Daemon.connect_gossipsub_with_p2p: "
        ^ Printexc.to_string exn
      in
      Format.eprintf "Error: %s@." msg ;
      Lwt_exit.exit_and_raise 1)

let resolve names =
  let open Lwt_result_syntax in
  (* Remove duplicates *)
  let names = List.sort_uniq String.compare names in
  (* Resolve the dns host names *)
  let* points =
    List.concat_map_es
      (fun name ->
        let* points =
          Tezos_base_unix.P2p_resolve.resolve_addr
            ~default_addr:"::"
            ~default_port:(Configuration_file.default.listen_addr |> snd)
            ~warn:false
            name
        in
        let*! () =
          Event.emit_resolved_bootstrap_points
            ~domainname:name
            ~number:(List.length points)
        in
        return points)
      names
  in
  let*! () =
    if points = [] then Event.emit_resolved_bootstrap_no_points ()
    else Event.emit_resolved_bootstrap_points_total ~number:(List.length points)
  in
  return points

let build_profile_context config =
  let open Lwt_result_syntax in
  let base_dir = Configuration_file.store_path config in
  let*! res = Profile_manager.load_profile_ctxt ~base_dir in
  match res with
  | Ok loaded_profile ->
      (* The profiles from the loaded context are prioritized over the
         profiles provided in the config file. *)
      Profile_manager.merge_profiles
        ~lower_prio:config.Configuration_file.profile
        ~higher_prio:(Profile_manager.Profile loaded_profile)
      |> return
  | Error error ->
      let*! () = Event.emit_loading_profiles_failed ~error in
      return config.Configuration_file.profile

(* Registers the attester profile context once we have the protocol plugin. This is supposed
   to be called only once. *)
let update_and_register_profiles ctxt =
  let open Lwt_result_syntax in
  let profile_ctxt = Node_context.get_profile_ctxt ctxt in
  let gs_worker = Node_context.get_gs_worker ctxt in
  let*? proto_parameters =
    Node_context.get_proto_parameters ctxt ~level:`Last_proto
  in
  let profile_ctxt =
    Profile_manager.register_profile
      profile_ctxt
      ~number_of_slots:proto_parameters.number_of_slots
      gs_worker
  in
  let*! () = Node_context.set_profile_ctxt ctxt profile_ctxt in
  return_unit

(* This back-fills the store with slot headers at levels from [from_level] to
   [from_level - attestation_lag]. *)
let backfill_slot_statuses cctxt store (module Plugin : Dal_plugin.T)
    proto_parameters ~from_level =
  let open Lwt_result_syntax in
  let number_of_slots = proto_parameters.Types.number_of_slots in
  List.iter_es
    (fun i ->
      let block_level = Int32.(sub from_level (of_int i)) in
      if block_level > 1l then
        let* slot_headers =
          Plugin.get_published_slot_headers ~block_level cctxt
        in
        let*! () =
          Slot_manager.store_slot_headers
            ~number_of_slots
            ~block_level
            slot_headers
            store
        in
        return_unit
      else return_unit)
    (Stdlib.List.init proto_parameters.attestation_lag Fun.id)

let run ?(disable_shard_validation = false) ~ignore_pkhs ~data_dir ~config_file
    ~configuration_override () =
  let open Lwt_result_syntax in
  let*! () =
    let log_cfg =
      Tezos_base_unix.Logs_simple_config.create_cfg ~advertise_levels:true ()
    in
    let internal_events =
      Tezos_base_unix.Internal_event_unix.make_with_defaults
        ~enable_default_daily_logs_at:Filename.Infix.(data_dir // "daily_logs")
        ~log_cfg
        ()
    in
    Tezos_base_unix.Internal_event_unix.init ~config:internal_events ()
  in
  let* ({
          rpc_addr;
          (* These are not the cryptographic identities of peers, but the points
             (IP addresses + ports) of the nodes we want to connect to at
             startup. *)
          peers = points;
          endpoint;
          profile;
          listen_addr;
          public_addr;
          ignore_l1_config_peers;
          _;
        } as config) =
    let*! result = Configuration_file.load ~config_file in
    match result with
    | Ok configuration -> return (configuration_override configuration)
    | Error _ ->
        let*! () = Event.emit_config_file_not_found ~path:config_file in
        (* Store the default configuration if no configuration were found. *)
        let configuration = configuration_override Configuration_file.default in
        let* () = Configuration_file.save ~config_file configuration in
        return configuration
  in
  let*! () = Event.emit_configuration_loaded () in
  let cctxt = Rpc_context.make endpoint in
  let* network_name = L1_helpers.infer_dal_network_name cctxt in
  let version = Tezos_version_value.Bin_version.octez_version_string in
  let*! () = Event.emit_starting_node ~network_name ~version in
  let* initial_peers_names =
    if ignore_l1_config_peers then return points
    else
      let* dal_config = L1_helpers.fetch_dal_config cctxt in
      return @@ points @ dal_config.bootstrap_peers
  in
  let*! () =
    if initial_peers_names = [] && not ignore_l1_config_peers then
      Event.emit_config_error_no_bootstrap ()
    else Lwt.return_unit
  in
  (* Resolve [initial_peers_names] from DAL node config file and CLI, and
     possibly from the L1 network config, if ignore_l1_config_peers is unset.
     Re-resolve every bootstrap_dns_refresh_delay = 5 minutes. *)
  let* get_initial_points =
    let* current_points = resolve initial_peers_names in
    let initial_points = ref current_points in
    let rec loop () =
      catch_es
        (fun () ->
          let*! () = Lwt_unix.sleep Constants.bootstrap_dns_refresh_delay in
          let* current_points = resolve initial_peers_names in
          initial_points := current_points ;
          loop ())
        ~catch_only:(function Lwt.Canceled -> true | _ -> false)
    in
    let dns_job = loop () in
    let (_ : Lwt_exit.clean_up_callback_id) =
      Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _exit_status ->
          let () = Lwt.cancel dns_job in
          Lwt.return_unit)
    in
    return (fun () -> !initial_points)
  in
  let* p2p_config = Transport_layer_parameters.p2p_config config in
  let p2p_limits = Transport_layer_parameters.p2p_limits in
  (* Get the current L1 head and its DAL plugin and parameters. *)
  let* header, proto_plugins = L1_helpers.wait_for_block_with_plugin cctxt in
  let head_level = header.Block_header.shell.level in
  let*? (module Plugin), proto_parameters =
    Proto_plugins.get_plugin_and_parameters_for_level
      proto_plugins
      ~level:head_level
  in

  (* Set proto number of slots hook. *)
  Value_size_hooks.set_number_of_slots proto_parameters.number_of_slots ;
  let* profile_ctxt =
    let+ profile_ctxt = build_profile_context config in
    Profile_manager.resolve_profile
      profile_ctxt
      ~number_of_slots:proto_parameters.number_of_slots
  in
  let*? () =
    Profile_manager.validate_slot_indexes
      profile_ctxt
      ~number_of_slots:proto_parameters.number_of_slots
  in
  let identity = p2p_config.P2p.identity in
  (* Create and start a GS worker *)
  let gs_worker =
    let rng =
      let seed =
        Random.self_init () ;
        Random.bits ()
      in
      Random.State.make [|seed|]
    in
    let open Worker_parameters in
    let limits =
      if Profile_manager.is_bootstrap_profile profile_ctxt then
        (* Bootstrap nodes should always have a mesh size of zero.
           so all grafts are responded with prunes with PX. See:
           https://github.com/libp2p/specs/blob/f5c5829ef9753ef8b8a15d36725c59f0e9af897e/pubsub/gossipsub/gossipsub-v1.1.md#recommendations-for-network-operators

           Additionally, we set [max_sent_iwant_per_heartbeat = 0]
           so bootstrap nodes do not download any shards via IHave/IWant
           transfers.

           Also, we set [prune_backoff = 10] so that bootstrap nodes send PX
           peers quicker. In particular, we want to avoid the following
           scenario: a peer receives a Prune from the bootstrap node, then
           disconnects for some reason and reconnects within the backoff
           period; with a large backoff, its first Graft will be answered with
           a no PX Prune, and therefore the peer will have to wait for the new
           backoff timeout to be able to obtain PX peers. *)
        {
          limits with
          max_sent_iwant_per_heartbeat = 0;
          degree_low = 0;
          degree_high = 0;
          degree_out = 0;
          degree_optimal = 0;
          degree_score = 0;
          prune_backoff = Ptime.Span.of_int_s 10;
        }
      else limits
    in
    (* Initialize the OpenTelemetry profiler only when identity is available, to
       allow discriminating the different services. *)
    ()
    [@profiler.overwrite
      {driver_ids = [Opentelemetry]}
        (Opentelemetry_profiler.initialize
           ~unique_identifier:(P2p_peer.Id.to_b58check identity.peer_id)
           ?env:config.telemetry_env
           config.service_name)] ;
    let self =
      (* What matters is the identity, the reachable point is more like a placeholder here. *)
      Types.Peer.
        {peer_id = identity.peer_id; maybe_reachable_point = public_addr}
    in
    let gs_worker =
      let batching_interval =
        match config.batching_configuration with
        | Disabled -> None
        | Enabled {time_interval} ->
            let time_in_second = float_of_int time_interval /. 1000. in
            Some (Types.Span.of_float_s time_in_second)
      in
      Gossipsub.Worker.(
        make
          ~initial_points:get_initial_points
          ~events_logging:(Logging.event ~verbose:config.verbose)
          ?batching_interval
          ~self
          rng
          limits
          peer_filter_parameters)
    in
    Gossipsub.Worker.start [] gs_worker ;
    gs_worker
  in
  let points = get_initial_points () in
  (* Create a transport (P2P) layer instance. *)
  let* transport_layer =
    Gossipsub.Transport_layer.create
      ~public_addr
      ~is_bootstrap_peer:(profile = Profile_manager.bootstrap)
      p2p_config
      p2p_limits
      ~network_name
  in
  let (_ : Lwt_exit.clean_up_callback_id) =
    (* This is important to prevent stall connections. *)
    Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _exit_status ->
        Gossipsub.Transport_layer.shutdown transport_layer)
  in
  (* Initialize store *)
  let* store = Store.init config profile_ctxt proto_parameters in
  let* current_chain_id = L1_helpers.fetch_l1_chain_id cctxt in
  let chain_id_store = Store.chain_id store in
  let* stored_chain_id = Store.Chain_id.load chain_id_store in
  let* () =
    match stored_chain_id with
    | None ->
        (* If no chain was stored, then either:
           - the DAL node never ran, hence there are no level stored,
           - the previously running DAL node did not feature the chain id storing.
           Hence, for retrocompatibility, we accept to load stored level if any in this case. *)
        Store.Chain_id.save chain_id_store current_chain_id
    | Some chain when chain = current_chain_id -> return_unit
    | Some stored_chain_id ->
        tzfail @@ Errors.Wrong_chain_id {current_chain_id; stored_chain_id}
  in
  let* last_processed_level =
    let last_processed_level_store = Store.last_processed_level store in
    Store.Last_processed_level.load last_processed_level_store
  in
  let first_seen_level_store = Store.first_seen_level store in
  let* first_seen_level = Store.First_seen_level.load first_seen_level_store in
  (* Check the DAL node's and L1 node's history mode. *)
  let* () =
    History_check.check_history_mode config profile_ctxt proto_parameters
  in
  let* () =
    match first_seen_level with
    | None -> Store.First_seen_level.save first_seen_level_store head_level
    | Some _ -> return_unit
  in
  let* () =
    History_check.check_l1_history_mode
      profile_ctxt
      cctxt
      proto_parameters
      ~head_level
      ~first_seen_level
  in
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5743
     Instead of recomputing these parameters, they could be stored
     (for a given cryptobox). *)
  let* cryptobox, shards_proofs_precomputation =
    Node_context.init_cryptobox config proto_parameters profile_ctxt
  in
  (* Set crypto box share size hook. *)
  Value_size_hooks.set_share_size
    (Cryptobox.Internal_for_tests.encoded_share_size cryptobox) ;
  let*! () =
    if disable_shard_validation then Event.emit_shard_validation_is_disabled ()
    else Lwt.return_unit
  in
  let*! () =
    if not @@ List.is_empty ignore_pkhs then
      Event.emit_ignoring_pkhs ~pkhs:ignore_pkhs
    else Lwt.return_unit
  in
  let ignore_pkhs = Signature.Public_key_hash.Set.of_list ignore_pkhs in
  let ctxt =
    Node_context.init
      config
      ~identity
      profile_ctxt
      cryptobox
      shards_proofs_precomputation
      proto_plugins
      store
      gs_worker
      transport_layer
      cctxt
      ~last_finalized_level:head_level
      ~network_name
      ~disable_shard_validation
      ~ignore_pkhs
      ()
  in
  let* () =
    match Profile_manager.get_profiles profile_ctxt with
    | Controller profile ->
        Node_context.warn_if_attesters_not_delegates ctxt profile
    | _ -> return_unit
  in
  let () =
    match config.batching_configuration with
    | Enabled _ ->
        Gossipsub.Worker.Validate_message_hook.set_batch
          (Message_validation.gossipsub_batch_validation
             ctxt
             cryptobox
             ~head_level
             proto_parameters)
    | Disabled -> ()
  in
  (* Even if the batch validation is activated, one has to register a per message
    validation for the validation of message id. *)
  Gossipsub.Worker.Validate_message_hook.set
    (Message_validation.gossipsub_app_messages_validation
       ctxt
       cryptobox
       ~head_level
       proto_parameters) ;
  let is_prover_profile = Profile_manager.is_prover_profile profile_ctxt in
  (* Initialize amplificator if in prover profile.
     This forks a process and should be kept early to avoid copying opened file
     descriptors. *)
  let* amplificator =
    if is_prover_profile && not config.disable_amplification then
      let* amplificator = Amplificator.make ctxt in
      return_some amplificator
    else return_none
  in
  (* Starts the metrics *after* the amplificator fork, to avoid forked opened
     sockets *)
  let* () =
    match config.metrics_addr with
    | None ->
        let*! () = Event.emit_metrics_server_not_starting () in
        return_unit
    | Some metrics_addr ->
        let*! () = Event.emit_metrics_server_starting ~endpoint:metrics_addr in
        let*! _metrics_server = Metrics.launch metrics_addr in
        return_unit
  in
  (* Start RPC server. We do that before the waiting for the L1 node to be
     bootstrapped so that queries can already be issued. Note that that the node
     will thus already respond to the baker about shards status if queried. *)
  let* rpc_server = RPC_server.(start config ctxt) in
  let _ = RPC_server.install_finalizer rpc_server in
  let*! () = Event.emit_rpc_server_is_ready ~point:rpc_addr in
  (* Wait for the L1 node to be bootstrapped. *)
  Node_context.(set_l1_crawler_status ctxt L1_bootstrapping) ;
  let* () = L1_helpers.wait_for_l1_bootstrapped cctxt in
  let* () =
    match last_processed_level with
    | None -> (* there's nothing to clean up *) return_unit
    | Some last_processed_level ->
        L1_crawler_status.catching_up_or_synced_status
          ~head_level
          ~last_processed_level
        |> Node_context.set_l1_crawler_status ctxt ;
        Store_cleanup.clean_up_store_and_catch_up
          ctxt
          cctxt
          ~last_processed_level
          ~first_seen_level
          ~head_level
          proto_parameters
  in
  (* We reload the last processed level because [clean_up_store] has likely
     modified it. *)
  let* last_notified_level =
    let last_processed_level_store = Store.last_processed_level store in
    Store.Last_processed_level.load last_processed_level_store
  in
  let* crawler =
    let open Constants in
    let*! crawler =
      Crawler.start
        ~name:"dal_node_crawler"
        ~chain:`Main
        ~reconnection_delay:initial_l1_crawler_reconnection_delay
        ~l1_blocks_cache_size:crawler_l1_blocks_cache_size
        ?last_notified_level
        cctxt
    in
    return crawler
  in
  let* () =
    let from_level = Int32.pred head_level in
    (backfill_slot_statuses
       cctxt
       store
       (module Plugin)
       proto_parameters
       ~from_level
     [@profiler.record_s {verbosity = Notice} "backfill_slot_statuses"])
  in
  (* Fetch the committees for the first levels. Note that that committees are
     fetched on a "regular basis" by {!Block_handler.may_update_topics} with a
     delta of [additional_levels + lag] wrt to HEAD~1. We fetch here for a few
     more levels, because the head might have advanced more until
     [may_update_topics] is first called. *)
  let () =
    let additional_levels =
      let minimal_block_delay =
        Int64.to_int proto_parameters.Types.minimal_block_delay
      in
      Constants.time_to_join_new_topics_in_levels ~minimal_block_delay
    in
    List.iter
      (fun i ->
        let level = Int32.(add head_level (of_int i)) in
        let _ = Node_context.fetch_committees ctxt ~level in
        ())
      (Stdlib.List.init
         (additional_levels + proto_parameters.attestation_lag + 2)
         Fun.id)
  in
  (* Activate the p2p instance. *)
  let shards_store = Store.shards store in
  let gs =
    connect_gossipsub_with_p2p
      proto_parameters
      gs_worker
      transport_layer
      shards_store
      ctxt
      amplificator
      ~verbose:config.verbose
  in
  let*! () =
    Gossipsub.Transport_layer.activate ~additional_points:points transport_layer
  in
  let*! () = Event.emit_p2p_server_is_ready ~point:listen_addr in
  (* Start collecting stats related to the Gossipsub worker. *)
  Dal_metrics.collect_gossipsub_metrics gs_worker ;
  (* Register topics with gossipsub worker. *)
  let* () = update_and_register_profiles ctxt in
  (* Start never-ending monitoring daemons *)
  let*! () = Event.emit_node_is_ready () in
  () [@profiler.overwrite may_start_profiler data_dir] ;
  let* _ =
    Lwt.pick
      [Lwt.bind gs return; daemonize [on_new_finalized_head ctxt cctxt crawler]]
  in
  return_unit
