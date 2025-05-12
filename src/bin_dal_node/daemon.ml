(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

let init_cryptobox config proto_parameters profile =
  let open Lwt_result_syntax in
  let prover_srs = Profile_manager.is_prover_profile profile in
  let* () =
    if prover_srs then
      let find_srs_files () = Tezos_base.Dal_srs.find_trusted_setup_files () in
      Cryptobox.init_prover_dal
        ~find_srs_files
        ~fetch_trusted_setup:config.Configuration_file.fetch_trusted_setup
        ()
    else return_unit
  in
  match Cryptobox.make proto_parameters.Types.cryptobox_parameters with
  | Ok cryptobox ->
      if prover_srs then
        match Cryptobox.precompute_shards_proofs cryptobox with
        | Ok precomputation -> return (cryptobox, Some precomputation)
        | Error (`Invalid_degree_strictly_less_than_expected {given; expected})
          ->
            fail
              [
                Errors.Cryptobox_initialisation_failed
                  (Printf.sprintf
                     "Cryptobox.precompute_shards_proofs: SRS size (= %d) \
                      smaller than expected (= %d)"
                     given
                     expected);
              ]
      else return (cryptobox, None)
  | Error (`Fail msg) -> fail [Errors.Cryptobox_initialisation_failed msg]

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
        let* () =
          Block_handler.new_finalized_head
            ctxt
            cctxt
            cryptobox
            finalized_block_hash
            finalized_shell_header
            ~launch_time
        in
        loop ()
  in
  let*! () = Event.emit_layer1_node_tracking_started () in
  loop ()

let daemonize handlers =
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/3605
     Improve concurrent tasks by using workers *)
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

(** [update_timing_shards_received node_ctxt timing slot_id
   ~number_of_already_stored_shards ~number_of_shards] update the shards timing
   table values [timing] associated to slot_id [slot_id] and returns the
   corresponding slot_metrics.
   This function is intended to be called on each received shard, even
   duplicates, but uses the [number_of_already_stored_shards] to only update
   slot_metrics if the count of shards has been incremented.
   This function execution is expected to be relatively fast: no IO is
   involved, an update of Vache map is involved *)
let update_timing_shard_received node_ctxt shards_timing_table slot_id
    ~number_of_already_stored_shards ~number_of_shards =
  let now = Unix.gettimeofday () in
  let open Dal_metrics in
  let timing =
    match
      Dal_metrics.Slot_id_bounded_map.find_opt shards_timing_table slot_id
    with
    | None ->
        (* Note: we expect the entry is None only on the first received shard,
           while lwt might actually process this code after the second or third
           shard. This should be rare and the delta between values is pretty
           minimal *)
        {
          time_first_shard = now;
          duration_enough_shards = None;
          duration_all_shards = None;
        }
    | Some timing ->
        let is_all_shard_received =
          number_of_already_stored_shards = number_of_shards
        in
        if is_all_shard_received then (
          let duration = now -. timing.time_first_shard in
          Dal_metrics.update_amplification_all_shards_received_duration duration ;
          {timing with duration_all_shards = Some duration})
        else
          let cryptobox = Node_context.get_cryptobox node_ctxt in
          let redundancy_factor =
            Cryptobox.(parameters cryptobox).redundancy_factor
          in
          let is_enough_shard_received =
            Option.is_none timing.duration_enough_shards
            && number_of_already_stored_shards
               >= number_of_shards / redundancy_factor
          in
          if is_enough_shard_received then (
            let duration = now -. timing.time_first_shard in
            Dal_metrics.update_amplification_enough_shards_received_duration
              duration ;
            {timing with duration_enough_shards = Some duration})
          else timing
  in
  let () =
    Dal_metrics.Slot_id_bounded_map.replace shards_timing_table slot_id timing
  in
  timing

let connect_gossipsub_with_p2p proto_parameters gs_worker transport_layer
    node_store node_ctxt amplificator ~verbose =
  let open Gossipsub in
  let timing_table_size =
    2 * proto_parameters.Types.attestation_lag
    * proto_parameters.cryptobox_parameters.number_of_shards
    * proto_parameters.number_of_slots
  in
  let shards_timing_table =
    Dal_metrics.Slot_id_bounded_map.create timing_table_size
  in
  let shards_handler shards =
    let save_and_notify = Store.Shards.write_all shards in
    fun Types.Message.{share; _}
        Types.Message_id.{commitment; shard_index; level; slot_index; _} ->
      let open Lwt_result_syntax in
      let slot_id : Types.slot_id = {slot_level = level; slot_index} in
      let* () =
        Seq.return {Cryptobox.share; index = shard_index}
        |> save_and_notify slot_id |> Errors.to_tzresult
      in
      let number_of_shards =
        proto_parameters.cryptobox_parameters.number_of_shards
      in
      (* Introduce a new store read at each received shard. Not sure it can be
         a problem, though *)
      let* number_of_already_stored_shards =
        Store.Shards.count_values node_store slot_id
      in
      let slot_metrics =
        update_timing_shard_received
          node_ctxt
          shards_timing_table
          slot_id
          ~number_of_already_stored_shards
          ~number_of_shards
      in
      match
        Profile_manager.get_profiles @@ Node_context.get_profile_ctxt node_ctxt
      with
      | Controller profile
        when Controller_profiles.is_observed_slot slot_index profile -> (
          match amplificator with
          | None ->
              let*! () = Event.emit_amplificator_uninitialized () in
              return_unit
          | Some amplificator ->
              Amplificator.try_amplification
                commitment
                slot_metrics
                slot_id
                amplificator)
      | _ -> return_unit
  in
  Lwt.dont_wait
    (fun () ->
      Transport_layer_hooks.activate
        gs_worker
        transport_layer
        ~app_messages_callback:(shards_handler node_store)
        ~verbose)
    (fun exn ->
      "[dal_node] error in Daemon.connect_gossipsub_with_p2p: "
      ^ Printexc.to_string exn
      |> Stdlib.failwith)

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

(* This function checks that in case the history mode is Rolling with a custom
   number of blocks, these number of blocks are sufficient. *)
let check_history_mode config profile_ctxt proto_parameters =
  let open Lwt_result_syntax in
  let supports_refutations =
    Profile_manager.supports_refutations profile_ctxt
  in
  let storage_period =
    Profile_manager.get_attested_data_default_store_period
      profile_ctxt
      proto_parameters
  in
  match config.Configuration_file.history_mode with
  | Rolling {blocks = `Some b} ->
      let minimal_levels =
        if supports_refutations then storage_period
        else proto_parameters.attestation_lag
      in
      if b < minimal_levels then
        tzfail @@ Errors.Not_enough_history {stored_levels = b; minimal_levels}
      else return_unit
  | Rolling {blocks = `Auto} | Full -> return_unit

(* We need more levels because [store_skip_list_cells level] needs the plugin
   for [attestation_lag + 1] levels in the past wrt to the target [level]. Note
   {!get_storage_period} refers to published levels (not attested levels). The
   plus one comes from the technical details of {!store_skip_list_cells}. *)
let skip_list_offset proto_parameters =
  proto_parameters.Types.attestation_lag + 1

(* This function determines the storage period taking into account the node's
   [first_seen_level]. Indeed, if the node started for the first time, we do not
   expect it to store skip list cells (even if it supports refutations) for the
   entire required period of 3 months, because it will anyway not have the slot
   pages for this period. Note that this could be further refined by taking into
   account when the corresponding rollup was originated. *)
let get_storage_period profile_ctxt proto_parameters head_level first_seen_level
    =
  let supports_refutations =
    Profile_manager.supports_refutations profile_ctxt
  in
  let default_storage_period =
    Profile_manager.get_attested_data_default_store_period
      profile_ctxt
      proto_parameters
  in
  if supports_refutations then
    (* This deliberately does not take into account the [last_processed_level]. *)
    let online_period =
      match first_seen_level with
      | None -> 0
      | Some first_seen_level ->
          Int32.sub head_level first_seen_level |> Int32.to_int
    in
    (* Even if the node was not online previously, or it was online only for a
       few levels, we still need to store data for the minimal period, defined
       in [get_attested_data_default_store_period]. TODO: refactor to expose
       this value. *)
    let max_period = max (2 * proto_parameters.attestation_lag) online_period in
    min max_period default_storage_period
  else default_storage_period

(* This function checks the L1 node stores enough block data for the DAL node to
   function correctly. *)
let check_l1_history_mode profile_ctxt cctxt proto_parameters head_level
    first_level =
  let open Lwt_result_syntax in
  let* l1_history_mode =
    let* l1_mode, blocks_preservation_cycles_opt =
      Config_services.history_mode cctxt
    in
    (* Note: For the DAL node it does not matter if the L1 node is in Full or
       Rolling mode, because the DAL node is not interested in blocks outside of
       a certain time window. *)
    return
    @@
    match (l1_mode, blocks_preservation_cycles_opt) with
    | Archive, _ -> `L1_archive
    | Full None, None
    | Rolling None, None
    | Full (Some _), None
    | Rolling (Some _), None
    | Full None, Some _
    | Rolling None, Some _ ->
        (* unreachable cases, at least for now *) assert false
    | Full (Some additional_cycles), Some blocks_preservation_cycles
    | Rolling (Some additional_cycles), Some blocks_preservation_cycles ->
        `L1_rolling (additional_cycles.offset + blocks_preservation_cycles)
  in
  let check ~dal_blocks ~l1_cycles =
    let blocks_per_cycle =
      Int32.to_int proto_parameters.Types.blocks_per_cycle
    in
    let dal_cycles = dal_blocks / blocks_per_cycle in
    let minimal_cycles =
      if dal_blocks mod blocks_per_cycle = 0 then dal_cycles else 1 + dal_cycles
    in
    if l1_cycles < minimal_cycles then
      tzfail
      @@ Errors.Not_enough_l1_history
           {stored_cycles = l1_cycles; minimal_cycles}
    else return_unit
  in
  match l1_history_mode with
  | `L1_archive -> return_unit
  | `L1_rolling l1_cycles ->
      (* For the non-"refutation supporting" profiles, we don't currently need
         that many levels in the past, because we don't for instance retrieve the
         protocol parameters for such past levels; though we should. *)
      let dal_blocks =
        get_storage_period profile_ctxt proto_parameters head_level first_level
        +
        if Profile_manager.supports_refutations profile_ctxt then
          skip_list_offset proto_parameters
        else 0
      in
      check ~dal_blocks ~l1_cycles

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

(* This function fetches the protocol plugins for levels in the past for which
   the node may need a plugin, namely for adding skip list cells, or for
   obtaining the protocol parameters.

   Concerning the skip list, getting the plugin is (almost) necessary as skip
   list cells are stored in the storage for a certain period and
   [store_skip_list_cells] needs the L1 context for levels in this period. (It
   would actually not be necessary to go as far in the past, because the
   protocol parameters and the relevant encodings do not change for now, so the
   head plugin could be used). *)
let get_proto_plugins cctxt profile_ctxt ~last_processed_level ~first_seen_level
    head_level proto_parameters =
  let storage_period =
    get_storage_period profile_ctxt proto_parameters head_level first_seen_level
  in
  let first_level =
    Int32.max
      (match last_processed_level with None -> 1l | Some level -> level)
      Int32.(sub head_level (of_int storage_period))
  in
  let first_level =
    if Profile_manager.supports_refutations profile_ctxt then
      Int32.sub first_level (Int32.of_int (skip_list_offset proto_parameters))
    else
      (* The DAL node may need the protocol parameters [attestation_lag] in the
         past wrt to the head level. *)
      Int32.sub first_level (Int32.of_int proto_parameters.attestation_lag)
  in
  let first_level = Int32.(max 1l first_level) in
  Proto_plugins.initial_plugins cctxt ~first_level ~last_level:head_level

(* This function removes old data starting from [last_processed_level -
   storage_period] to [target_level - storage_period], where [storage_period] is
   the period for which the DAL node stores data related to attested slots and
   [target_level] is the level at which we connect the P2P and switch to
   processing blocks in sync with the L1. [target_level] is set to [head_level -
   3]. It also inserts skip list cells if needed in the period [head_level -
   storage_level].

   FIXME: https://gitlab.com/tezos/tezos/-/issues/7429
   We don't call [may_add_plugin], so there is a chance the plugin changes
   and we don't detect it if this code starts running just before the migration
   level, and the head changes meanwhile to be above the migration level.

   TODO: https://gitlab.com/tezos/tezos/-/issues/7779
   Improve the runtime of this function. It may be better to do the clean-up and
   the "catch-up" (that is, updating of the skip list store) separately. *)
let clean_up_store_and_catch_up_for_refutation_support ctxt cctxt
    ~last_processed_level ~first_seen_level head_level proto_parameters =
  let open Lwt_result_syntax in
  let store_skip_list_cells ~level =
    let*? (module Plugin) =
      Node_context.get_plugin_for_level ctxt ~level:(Int32.pred level)
    in
    let* block_info =
      Plugin.block_info cctxt ~block:(`Level level) ~metadata:`Always
    in
    let*? dal_constants =
      Node_context.get_proto_parameters ctxt ~level:(`Level level)
    in
    Block_handler.store_skip_list_cells
      ctxt
      cctxt
      dal_constants
      block_info
      level
      (module Plugin : Dal_plugin.T with type block_info = Plugin.block_info)
  in
  let store = Node_context.get_store ctxt in
  let last_processed_level_store = Store.last_processed_level store in
  (* [target_level] identifies the level wrt to head level at which we want to
     start the P2P and process blocks as usual. It's set to [head_level - 3]
     because the first level the DAL node should process should be a final
     one. *)
  let target_level head_level = Int32.(sub head_level 3l) in
  let first_level_for_skip_list_storage period level =
    (* Note that behind this first level we do not have the plugin. *)
    Int32.(sub level (of_int period))
  in
  let should_store_skip_list_cells ~head_level =
    let profile_ctxt = Node_context.get_profile_ctxt ctxt in
    let period =
      get_storage_period
        profile_ctxt
        proto_parameters
        head_level
        first_seen_level
      + skip_list_offset proto_parameters
    in
    let first_level = first_level_for_skip_list_storage period head_level in
    fun ~level -> level >= first_level
  in
  let rec do_clean_up last_processed_level head_level =
    let last_level = target_level head_level in
    let should_store_skip_list_cells =
      should_store_skip_list_cells ~head_level
    in
    let rec clean_up_at_level level =
      if level > last_level then return_unit
      else
        let*! () =
          Block_handler.remove_old_level_stored_data proto_parameters ctxt level
        in
        let* () =
          if should_store_skip_list_cells ~level then
            store_skip_list_cells ~level
          else return_unit
        in
        let* () =
          Store.Last_processed_level.save last_processed_level_store level
        in
        let*! () =
          if Int32.to_int level mod 1000 = 0 then
            Event.emit_catching_up ~current_level:level
          else Lwt.return_unit
        in
        clean_up_at_level (Int32.succ level)
    in
    (* Clean up from [last_processed_level] to [last_level]. *)
    let* () = clean_up_at_level (Int32.succ last_processed_level) in
    (* As this iteration may be slow, the head level might have advanced in the
       meanwhile. *)
    let* header =
      Shell_services.Blocks.Header.shell_header cctxt ~block:(`Head 0) ()
    in
    let new_head_level = header.Block_header.level in
    if new_head_level > head_level then do_clean_up last_level new_head_level
    else
      let*! () = Event.emit_end_catchup () in
      return_unit
  in
  let start_level = Int32.succ last_processed_level in
  let end_level = target_level head_level in
  let levels_to_clean_up = Int32.(succ @@ sub end_level last_processed_level) in
  if levels_to_clean_up > 0l then
    let*! () =
      Event.emit_start_catchup ~start_level ~end_level ~levels_to_clean_up
    in
    do_clean_up last_processed_level head_level
  else return_unit

let clean_up_store_and_catch_up_for_no_refutation_support ctxt
    ~last_processed_level head_level proto_parameters =
  let open Lwt_result_syntax in
  let profile_ctxt = Node_context.get_profile_ctxt ctxt in
  let storage_period =
    Profile_manager.get_attested_data_default_store_period
      profile_ctxt
      proto_parameters
    |> Int32.of_int
  in
  (* We clean-up *for* (not at) levels between [last_processed_level + 1] and
     [finalized_level - 1], because [last_processed_level] was the last level
     for which there was already a clean-up, and [finalized_level] will be the
     first level to be processed after this restart. However, there is no need
     to clean-up for levels higher than [last_processed_level + storage_period]
     because there is no data corresponding to such levels.

     ("Level *for* cleaning" refers to the level passed to
     [Handler.remove_old_level_stored_data], not to the level at which there is
     data to be wiped.)

     Examples: Say [last_processed_level = 1000] and [storage_period =
     100]. Thus we have data stored for levels 901 to 1000.

     Example 1: Say [finalized_level = 1060]. We clean-up for levels 1001 up to
     1060, that is, we wipe data from level 901 up to level 960.

     Example 2: Say [finalized_level = 3000]. We clean-up for levels 1001 up to
     1100 (so at levels 901 up to 1000). *)
  let finalized_level = Int32.sub head_level 2l in
  let new_last_processed_level = Int32.(max 1l (pred finalized_level)) in
  let last_level_for_cleaning =
    let highest_level_with_data_for_cleaning =
      Int32.add last_processed_level storage_period
    in
    Int32.(min new_last_processed_level highest_level_with_data_for_cleaning)
  in
  let rec cleanup level =
    if level > last_level_for_cleaning then
      let store = Node_context.get_store ctxt in
      let last_processed_level_store = Store.last_processed_level store in
      let* () =
        Store.Last_processed_level.save
          last_processed_level_store
          new_last_processed_level
      in
      let*! () = Event.emit_end_catchup () in
      return_unit
    else
      let*! () =
        Block_handler.remove_old_level_stored_data proto_parameters ctxt level
      in
      cleanup @@ Int32.succ level
  in
  let start_level = Int32.succ last_processed_level in
  let end_level = last_level_for_cleaning in
  let levels_to_clean_up = Int32.(succ @@ sub end_level start_level) in
  if levels_to_clean_up > 0l then
    let*! () =
      Event.emit_start_catchup ~start_level ~end_level ~levels_to_clean_up
    in
    cleanup start_level
  else return_unit

let clean_up_store_and_catch_up ctxt cctxt ~last_processed_level
    ~first_seen_level head_level proto_parameters =
  if Node_context.supports_refutations ctxt then
    clean_up_store_and_catch_up_for_refutation_support
      ctxt
      cctxt
      ~last_processed_level
      ~first_seen_level
      head_level
      proto_parameters
  else
    clean_up_store_and_catch_up_for_no_refutation_support
      ctxt
      ~last_processed_level
      head_level
      proto_parameters

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/3605
   Improve general architecture, handle L1 disconnection etc
*)
let run ~data_dir ~configuration_override =
  let open Lwt_result_syntax in
  let log_cfg = Tezos_base_unix.Logs_simple_config.default_cfg in
  let internal_events =
    Tezos_base_unix.Internal_event_unix.make_with_defaults
      ~enable_default_daily_logs_at:Filename.Infix.(data_dir // "daily_logs")
      ~log_cfg
      ()
  in
  let*! () =
    Tezos_base_unix.Internal_event_unix.init ~config:internal_events ()
  in
  let*! () = Event.emit_starting_node () in
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
    let*! result = Configuration_file.load ~data_dir in
    match result with
    | Ok configuration -> return (configuration_override configuration)
    | Error _ ->
        let*! () = Event.emit_data_dir_not_found ~path:data_dir in
        (* Store the default configuration if no configuration were found. *)
        let configuration = configuration_override Configuration_file.default in
        let* () = Configuration_file.save configuration in
        return configuration
  in
  let*! () = Event.emit_configuration_loaded () in
  let cctxt = Rpc_context.make endpoint in
  let* network_name = L1_helpers.infer_dal_network_name cctxt in
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
  let* header, (module Plugin : Dal_plugin.T) =
    L1_helpers.wait_for_block_with_plugin cctxt
  in
  let head_level = header.Block_header.shell.level in
  let* proto_parameters =
    Plugin.get_constants `Main (`Level head_level) cctxt
  in
  let proto_plugins =
    Proto_plugins.singleton
      ~first_level:head_level
      ~proto_level:header.shell.proto_level
      (module Plugin)
      proto_parameters
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
    let identity = p2p_config.P2p.identity in
    let self =
      (* What matters is the identity, the reachable point is more like a placeholder here. *)
      Types.Peer.
        {peer_id = identity.peer_id; maybe_reachable_point = public_addr}
    in
    let gs_worker =
      Gossipsub.Worker.(
        make
          ~initial_points:get_initial_points
          ~events_logging:(Logging.event ~verbose:config.verbose)
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
  let* store = Store.init config in
  let* last_processed_level =
    let last_processed_level_store = Store.last_processed_level store in
    Store.Last_processed_level.load last_processed_level_store
  in
  let first_seen_level_store = Store.first_seen_level store in
  let* first_seen_level = Store.First_seen_level.load first_seen_level_store in
  (* Check the DAL node's and L1 node's history mode. *)
  let* () = check_history_mode config profile_ctxt proto_parameters in
  let* () =
    match first_seen_level with
    | None -> Store.First_seen_level.save first_seen_level_store head_level
    | Some _ -> return_unit
  in
  let* () =
    check_l1_history_mode
      profile_ctxt
      cctxt
      proto_parameters
      head_level
      first_seen_level
  in
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5743
     Instead of recomputing these parameters, they could be stored
     (for a given cryptobox). *)
  let* cryptobox, shards_proofs_precomputation =
    init_cryptobox config proto_parameters profile_ctxt
  in
  (* Set crypto box share size hook. *)
  Value_size_hooks.set_share_size
    (Cryptobox.Internal_for_tests.encoded_share_size cryptobox) ;
  let ctxt =
    Node_context.init
      config
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
  in
  let* () =
    match Profile_manager.get_profiles profile_ctxt with
    | Controller profile ->
        Node_context.warn_if_attesters_not_delegates ctxt profile
    | _ -> return_unit
  in
  Gossipsub.Worker.Validate_message_hook.set
    (Message_validation.gossipsub_app_messages_validation
       ctxt
       cryptobox
       head_level
       proto_parameters) ;
  let is_prover_profile = Profile_manager.is_prover_profile profile_ctxt in
  (* Initialize amplificator if in prover profile.
     This forks a process and should be kept early to avoid copying opened file
     descriptors. *)
  let* amplificator =
    if is_prover_profile then
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
  let* () = L1_helpers.wait_for_l1_bootstrapped cctxt in
  let* proto_plugins =
    get_proto_plugins
      cctxt
      profile_ctxt
      ~last_processed_level
      ~first_seen_level
      head_level
      proto_parameters
  in
  Node_context.set_proto_plugins ctxt proto_plugins ;
  let* () =
    match last_processed_level with
    | None -> (* there's nothing to clean up *) return_unit
    | Some last_processed_level ->
        clean_up_store_and_catch_up
          ctxt
          cctxt
          ~last_processed_level
          ~first_seen_level
          head_level
          proto_parameters
  in
  let* crawler =
    (* We reload the last processed level because [clean_up_store] has likely
       modified it. *)
    let* last_notified_level =
      let last_processed_level_store = Store.last_processed_level store in
      Store.Last_processed_level.load last_processed_level_store
    in
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
  (* Activate the p2p instance. *)
  let shards_store = Store.shards store in
  connect_gossipsub_with_p2p
    proto_parameters
    gs_worker
    transport_layer
    shards_store
    ctxt
    amplificator
    ~verbose:config.verbose ;
  let*! () =
    Gossipsub.Transport_layer.activate ~additional_points:points transport_layer
  in
  let*! () = Event.emit_p2p_server_is_ready ~point:listen_addr in
  (* Start collecting stats related to the Gossipsub worker. *)
  Dal_metrics.collect_gossipsub_metrics gs_worker ;
  (* Register topics with gossipsub worker. *)
  let* () = update_and_register_profiles ctxt in
  (* Start never-ending monitoring daemons *)
  let version = Tezos_version_value.Bin_version.octez_version_string in
  let*! () = Event.emit_node_is_ready ~network_name ~version in
  let* () = daemonize [on_new_finalized_head ctxt cctxt crawler] in
  return_unit
