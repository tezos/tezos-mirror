(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
(* Copyright (c) 2023-2024 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

type t = {
  config : Configuration_file.t;
  identity : P2p_identity.t;
  network_name : Distributed_db_version.Name.t;
  mutable proto_cryptoboxes : Proto_cryptoboxes.t;
  mutable proto_plugins : Proto_plugins.t;
  mutable ongoing_amplifications : Types.Slot_id.Set.t;
  mutable slots_under_reconstruction :
    (bytes, Errors.other) result Lwt.t Types.Slot_id.Map.t;
  store : Store.t;
  tezos_node_cctxt : Tezos_rpc.Context.generic;
  committee_cache : Committee_cache.t;
  attestation_ops_cache : Attestation_ops_cache.t;
  gs_worker : Gossipsub.Worker.t;
  transport_layer : Gossipsub.Transport_layer.t;
  mutable profile_ctxt : Profile_manager.t;
  mutable last_finalized_level : int32;
  mutable l1_current_level : int32;
  (* the highest finalized level the DAL node is aware of (except at start-up, where
     it is the highest level the node is aware of) *)
  mutable l1_crawler_status : L1_crawler_status.t;
  l1_crawler_status_input : L1_crawler_status.t Lwt_watcher.input;
  disable_shard_validation : bool;
  ignore_pkhs : Signature.Public_key_hash.Set.t;
  mutable last_migration_level : int32;
  mutable attestable_slots_watcher_table : Attestable_slots_watcher_table.t;
}

let init config ~identity ~network_name profile_ctxt proto_cryptoboxes
    proto_plugins store gs_worker transport_layer cctxt ~last_finalized_level
    ~l1_current_level ?(disable_shard_validation = false) ~ignore_pkhs () =
  {
    config;
    identity;
    network_name;
    proto_cryptoboxes;
    proto_plugins;
    ongoing_amplifications = Types.Slot_id.Set.empty;
    slots_under_reconstruction = Types.Slot_id.Map.empty;
    store;
    tezos_node_cctxt = cctxt;
    committee_cache =
      Committee_cache.create ~max_size:Constants.committee_cache_size;
    attestation_ops_cache =
      Attestation_ops_cache.create
        ~max_size:Constants.attestation_ops_cache_size;
    gs_worker;
    transport_layer;
    profile_ctxt;
    last_finalized_level;
    l1_current_level;
    l1_crawler_status = Unknown;
    l1_crawler_status_input = Lwt_watcher.create_input ();
    disable_shard_validation;
    ignore_pkhs;
    last_migration_level = 0l;
    attestable_slots_watcher_table =
      Attestable_slots_watcher_table.create ~initial_size:5;
  }

let get_tezos_node_cctxt ctxt = ctxt.tezos_node_cctxt

let get_identity ctxt = ctxt.identity

let set_l1_crawler_status ctxt status =
  if ctxt.l1_crawler_status <> status then (
    ctxt.l1_crawler_status <- status ;
    Lwt_watcher.notify ctxt.l1_crawler_status_input status)

let get_l1_crawler_status ctxt = ctxt.l1_crawler_status

let get_l1_crawler_status_input ctxt = ctxt.l1_crawler_status_input

let may_reconstruct ~reconstruct slot_id t =
  let open Lwt_result_syntax in
  let p =
    (* If a reconstruction is already ongoing, reuse the
       promise. *)
    match Types.Slot_id.Map.find slot_id t.slots_under_reconstruction with
    | Some promise -> promise
    | None ->
        let promise = reconstruct slot_id in
        t.slots_under_reconstruction <-
          Types.Slot_id.Map.add slot_id promise t.slots_under_reconstruction ;
        promise
  in
  let*! res = p in
  t.slots_under_reconstruction <-
    Types.Slot_id.Map.remove slot_id t.slots_under_reconstruction ;
  Lwt.return res

type error += Unexpected_empty_plugin_table

let () =
  register_error_kind
    `Permanent
    ~id:"dal.node.unexpected_empty_plugin_table"
    ~title:"DAL node: no plugins registered"
    ~description:"DAL node: no plugins registered"
    ~pp:(fun ppf () -> Format.fprintf ppf "No plugin registered.")
    Data_encoding.unit
    (function Unexpected_empty_plugin_table -> Some () | _ -> None)
    (fun () -> Unexpected_empty_plugin_table)

let may_add_plugin_and_cryptobox ctxt cctxt ~block_level ~proto_level =
  let open Lwt_result_syntax in
  let old_proto_plugins = ctxt.proto_plugins in
  match Proto_plugins.current_proto_level old_proto_plugins with
  | Some lvl when lvl = proto_level -> return_false
  | _ -> (
      let* new_proto_plugins =
        Proto_plugins.may_add
          cctxt
          old_proto_plugins
          ~first_level:block_level
          ~proto_level
      in
      ctxt.last_migration_level <- block_level ;
      ctxt.proto_plugins <- new_proto_plugins ;
      let proto_param_opt =
        Proto_plugins.current_proto_parameters new_proto_plugins
      in
      match proto_param_opt with
      | None -> tzfail Unexpected_empty_plugin_table
      | Some params ->
          let* new_cryptoboxes =
            Proto_cryptoboxes.add
              params
              ~level:block_level
              ctxt.proto_cryptoboxes
          in
          Store.Slots.add_file_layout
            (Int32.add block_level 1l)
            params.cryptobox_parameters ;
          let* () =
            match
              Store.Shards_disk.add_file_layout
                (Int32.add block_level 1l)
                params.cryptobox_parameters
            with
            | Ok () -> return_unit
            | Error (`Fail msg) ->
                tzfail (Proto_cryptoboxes.Cannot_register_shard_layout {msg})
          in
          Store.resize_caches ctxt.store params ;
          ctxt.proto_cryptoboxes <- new_cryptoboxes ;
          return_true)

let get_plugin_and_parameters_for_level ctxt ~level =
  Proto_plugins.get_plugin_and_parameters_for_level ctxt.proto_plugins ~level

let get_plugin_for_level ctxt ~level =
  let open Result_syntax in
  let* plugin, _parameters = get_plugin_and_parameters_for_level ctxt ~level in
  return plugin

let get_all_plugins ctxt = Proto_plugins.to_list ctxt.proto_plugins

let set_proto_plugins ctxt proto_plugins = ctxt.proto_plugins <- proto_plugins

let get_proto_parameters ~level ctxt =
  let open Result_syntax in
  let level =
    match level with `Head -> ctxt.l1_current_level | `Level level -> level
  in
  let* _plugin, parameters = get_plugin_and_parameters_for_level ctxt ~level in
  return parameters

let storage_period ctxt proto_parameters =
  match ctxt.config.history_mode with
  | Full -> `Always
  | Rolling {blocks = `Some n} -> `Finite n
  | Rolling {blocks = `Auto} ->
      let n =
        Profile_manager.get_attested_data_default_store_period
          ctxt.profile_ctxt
          proto_parameters
      in
      `Finite n

let level_to_gc ctxt proto_parameters ~current_level =
  let open Lwt_result_syntax in
  match storage_period ctxt proto_parameters with
  | `Always -> return_none
  | `Finite n -> (
      let level = Int32.(sub current_level (of_int n)) in
      if level < 1l then return_none
      else
        let* first_seen_level_opt =
          Store.First_seen_level.load (Store.first_seen_level ctxt.store)
        in
        match first_seen_level_opt with
        | None -> return_none
        | Some first_seen_level ->
            if level < first_seen_level then return_none else return_some level)

let get_profile_ctxt ctxt = ctxt.profile_ctxt

let load_profile_ctxt ctxt =
  let open Lwt_syntax in
  let base_dir = Configuration_file.store_path ctxt.config in
  let* res = Profile_manager.load_profile_ctxt ~base_dir in
  match res with
  | Ok pctxt -> return_some pctxt
  | Error error ->
      let* () = Event.emit_loading_profiles_failed ~error in
      return_none

let set_profile_ctxt ctxt ?(save = true) pctxt =
  let open Lwt_syntax in
  ctxt.profile_ctxt <- pctxt ;
  if save then
    let base_dir = Configuration_file.store_path ctxt.config in
    let* res = Profile_manager.save_profile_ctxt ctxt.profile_ctxt ~base_dir in
    match res with
    | Ok () -> return_unit
    | Error error -> Event.emit_saving_profiles_failed ~error
  else return_unit

let get_config ctxt = ctxt.config

let get_cryptobox_and_precomputations ~level ctxt =
  Proto_cryptoboxes.get_for_level ctxt.proto_cryptoboxes ~level

let set_last_finalized_level ctxt level = ctxt.last_finalized_level <- level

let get_last_finalized_level ctxt = ctxt.last_finalized_level

let set_l1_current_head_level ctxt level = ctxt.l1_current_level <- level

let get_l1_current_head_level ctxt = ctxt.l1_current_level

let get_store ctxt = ctxt.store

let get_gs_worker ctxt = ctxt.gs_worker

let get_attestation_ops_cache ctxt = ctxt.attestation_ops_cache

let get_ongoing_amplifications ctxt = ctxt.ongoing_amplifications

let set_ongoing_amplifications ctxt ongoing_amplifications =
  ctxt.ongoing_amplifications <- ongoing_amplifications

let get_ignore_pkhs ctxt = ctxt.ignore_pkhs

let fetch_committees ctxt ~level =
  let open Lwt_result_syntax in
  let {tezos_node_cctxt = cctxt; committee_cache = cache; _} = ctxt in
  match Committee_cache.find cache ~level with
  | Some committee -> return committee
  | None ->
      let*? (module Plugin) = get_plugin_for_level ctxt ~level in
      let+ committees = Plugin.get_committees cctxt ~level in
      Committee_cache.add cache ~level ~committee:committees ;
      committees

let fetch_assigned_shard_indices ctxt ~level ~pkh =
  let open Lwt_result_syntax in
  let+ committees = fetch_committees ctxt ~level in
  match Signature.Public_key_hash.Map.find pkh committees with
  | None -> []
  | Some (indexes, _) -> indexes

let get_fetched_assigned_shard_indices ctxt ~level ~pkh =
  Option.map
    (fun committees ->
      Signature.Public_key_hash.Map.find_opt pkh committees |> function
      | None -> []
      | Some (dal_committee, _tb_committee) -> dal_committee)
    (Committee_cache.find ctxt.committee_cache ~level)

let version {network_name; _} =
  Types.Version.make ~network_version:(Gossipsub.version ~network_name)

let is_bootstrap_node ctxt =
  get_profile_ctxt ctxt |> Profile_manager.is_bootstrap_profile

let supports_refutations ctxt =
  get_profile_ctxt ctxt |> Profile_manager.supports_refutations

let warn_if_attesters_not_delegates ctxt controller_profiles =
  let open Lwt_result_syntax in
  let pkh_set = Controller_profiles.attesters controller_profiles in
  if Signature.Public_key_hash.Set.is_empty pkh_set then return_unit
  else
    let level = get_last_finalized_level ctxt in
    let cctxt = get_tezos_node_cctxt ctxt in
    let*? (module Plugin) = get_plugin_for_level ctxt ~level in
    Signature.Public_key_hash.Set.iter_es
      (fun pkh ->
        let* is_delegate = Plugin.is_delegate cctxt ~pkh in
        if not is_delegate then
          let*! () = Event.emit_registered_pkh_not_a_delegate ~pkh in
          return_unit
        else return_unit)
      pkh_set

let get_disable_shard_validation ctxt = ctxt.disable_shard_validation

let get_last_migration_level ctxt = ctxt.last_migration_level

let get_attestable_slots_watcher_table ctxt =
  ctxt.attestable_slots_watcher_table

let get_attestation_lag ctxt ~level =
  let open Result_syntax in
  let+ params = get_proto_parameters ctxt ~level:(`Level level) in
  Int32.of_int params.attestation_lag

let get_attestation_lags ctxt ~level =
  let open Result_syntax in
  let+ params = get_proto_parameters ctxt ~level:(`Level level) in
  List.map Int32.of_int params.attestation_lags

let assert_single_lag ctxt =
  let open Result_syntax in
  let+ params = get_proto_parameters ctxt ~level:`Head in
  assert (List.compare_length_with params.attestation_lags 1 = 0)

module P2P = struct
  let connect {transport_layer; _} ?timeout point =
    Gossipsub.Transport_layer.connect transport_layer ?timeout point

  let disconnect_point {transport_layer; _} ?wait point =
    Gossipsub.Transport_layer.disconnect_point transport_layer ?wait point

  let disconnect_peer {transport_layer; _} ?wait peer =
    Gossipsub.Transport_layer.disconnect_peer transport_layer ?wait peer

  let get_points ?connected {transport_layer; _} =
    Gossipsub.Transport_layer.get_points ?connected transport_layer

  let get_points_info ?connected {transport_layer; _} =
    Gossipsub.Transport_layer.get_points_info ?connected transport_layer

  let get_point_info {transport_layer; _} point =
    Gossipsub.Transport_layer.get_point_info transport_layer point

  let get_peers ?connected {transport_layer; _} =
    Gossipsub.Transport_layer.get_peers ?connected transport_layer

  let get_peers_info ?connected {transport_layer; _} =
    Gossipsub.Transport_layer.get_peers_info ?connected transport_layer

  let get_peer_info {transport_layer; _} peer =
    Gossipsub.Transport_layer.get_peer_info transport_layer peer

  let patch_peer {transport_layer; _} peer acl =
    Gossipsub.Transport_layer.patch_peer transport_layer peer acl

  module Gossipsub = struct
    let get_mesh ?slot_index ?delegate {gs_worker; _} =
      let open Gossipsub.Worker in
      let state = state gs_worker in
      let keep Types.Topic.{slot_index = idx; pkh} =
        Option.fold slot_index ~none:true ~some:(Int.equal idx)
        && Option.fold
             delegate
             ~none:true
             ~some:(Signature.Public_key_hash.equal pkh)
      in
      GS.Topic.Map.fold
        (fun topic peers acc ->
          if keep topic then (topic, GS.Peer.Set.elements peers) :: acc else acc)
        state.mesh
        []

    let get_topics {gs_worker; _} =
      let state = Gossipsub.Worker.state gs_worker in
      Gossipsub.Worker.GS.Topic.Map.fold
        (fun topic _peers acc -> topic :: acc)
        state.mesh
        []

    let get_topics_peers ~subscribed ctx =
      let open Gossipsub.Worker in
      let state = state ctx.gs_worker in
      let open GS in
      let topic_to_peers_map =
        Introspection.Connections.peers_per_topic_map state.connections
      in
      let subscribed_topics = state.mesh in
      Topic.Map.fold
        (fun topic peers acc ->
          if (not subscribed) || Topic.Map.mem topic subscribed_topics then
            (topic, Peer.Set.elements peers) :: acc
          else acc)
        topic_to_peers_map
        []

    let get_fanout ctx =
      let open Gossipsub.Worker in
      let state = state ctx.gs_worker in
      let open GS in
      let fanout = Introspection.get_fanout state in
      fanout |> Topic.Map.to_seq
      |> Seq.map (fun (topic, Introspection.{peers; last_published_time}) ->
             (topic, Peer.Set.elements peers, last_published_time))
      |> List.of_seq

    (* TODO: https://gitlab.com/tezos/tezos/-/issues/7462
       We could improve the performance of this function. *)
    let get_slot_indexes_peers ~subscribed ctx =
      let open Gossipsub.Worker in
      let state = state ctx.gs_worker in
      let open GS in
      let topic_to_peers_map =
        Introspection.Connections.peers_per_topic_map state.connections
      in
      let subscribed_topics = state.mesh in
      let module IndexMap = Map.Make (Int) in
      let res_map =
        Gossipsub.Worker.GS.Topic.Map.fold
          (fun topic peers acc ->
            if (not subscribed) || Topic.Map.mem topic subscribed_topics then
              IndexMap.update
                topic.slot_index
                (function
                  | None -> Some peers
                  | Some acc_peers -> Some (Peer.Set.union acc_peers peers))
                acc
            else acc)
          topic_to_peers_map
          IndexMap.empty
      in
      IndexMap.fold
        (fun index peers acc -> (index, Peer.Set.elements peers) :: acc)
        res_map
        []

    (* TODO: https://gitlab.com/tezos/tezos/-/issues/7462
       We could improve the performance of this function. *)
    let get_pkhs_peers ~subscribed ctx =
      let open Gossipsub.Worker in
      let state = state ctx.gs_worker in
      let open GS in
      let topic_to_peers_map =
        Introspection.Connections.peers_per_topic_map state.connections
      in
      let subscribed_topics = state.mesh in
      let module KeyHashMap = Map.Make (Signature.Public_key_hash) in
      let res_map =
        Topic.Map.fold
          (fun topic peers acc ->
            if (not subscribed) || Topic.Map.mem topic subscribed_topics then
              KeyHashMap.update
                topic.pkh
                (function
                  | None -> Some peers
                  | Some acc_peers ->
                      Some (Gossipsub.Worker.GS.Peer.Set.union acc_peers peers))
                acc
            else acc)
          topic_to_peers_map
          KeyHashMap.empty
      in
      KeyHashMap.fold
        (fun pkh peers acc ->
          (pkh, Gossipsub.Worker.GS.Peer.Set.elements peers) :: acc)
        res_map
        []

    let get_connections ?(ignore_bootstrap_topics = false) {gs_worker; _} =
      let state = Gossipsub.Worker.state gs_worker in
      Gossipsub.Worker.GS.Introspection.Connections.fold
        (fun peer {topics; direct; outbound; bootstrap} acc ->
          let topics =
            if bootstrap && ignore_bootstrap_topics then []
            else Gossipsub.Worker.GS.Topic.Set.elements topics
          in
          (peer, Types.Gossipsub.{topics; direct; outbound; bootstrap}) :: acc)
        state.connections
        []

    let get_reconnection_delays {gs_worker; _} =
      Gossipsub.Worker.reconnection_delays gs_worker

    let get_scores {gs_worker; _} =
      let state = Gossipsub.Worker.state gs_worker in
      Gossipsub.Worker.GS.Peer.Map.fold
        (fun peer score acc ->
          let v =
            Gossipsub.Worker.GS.Score.value score
            |> Gossipsub.Worker.GS.Score.Introspection.to_float
          in
          (peer, v) :: acc)
        state.scores
        []

    let get_backoffs {gs_worker; _} =
      let state = Gossipsub.Worker.state gs_worker in
      Gossipsub.Worker.GS.Topic.Map.fold
        (fun topic peer_map acc ->
          (topic, Gossipsub.Worker.GS.Peer.Map.bindings peer_map) :: acc)
        state.backoff
        []

    let get_message_cache {gs_worker; _} =
      let module Cache = Gossipsub.Worker.GS.Introspection.Message_cache in
      let state = Gossipsub.Worker.state gs_worker in
      let map = Cache.Introspection.get_message_ids state.message_cache in
      Cache.Introspection.Map.fold
        (fun tick map acc ->
          let list =
            Cache.Topic.Map.fold
              (fun topic ids acc -> (topic, List.length ids) :: acc)
              map
              []
          in
          (tick, list) :: acc)
        map
        []
  end
end
