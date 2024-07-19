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
  cryptobox : Cryptobox.t;
  shards_proofs_precomputation : Cryptobox.shards_proofs_precomputation option;
  proto_parameters : Dal_plugin.proto_parameters;
  mutable proto_plugins : Proto_plugins.t;
  mutable ongoing_amplifications : Types.Slot_id.Set.t;
  mutable slots_under_reconstruction :
    (bytes, Errors.other) result Lwt.t Types.Slot_id.Map.t;
  store : Store.t;
  tezos_node_cctxt : Tezos_rpc.Context.generic;
  neighbors_cctxts : Dal_node_client.cctxt list;
  committee_cache : Committee_cache.t;
  gs_worker : Gossipsub.Worker.t;
  transport_layer : Gossipsub.Transport_layer.t;
  mutable profile_ctxt : Profile_manager.t;
  metrics_server : Metrics.t;
  crawler : Crawler.t;
  last_processed_level_store : Last_processed_level.t;
}

let init config cryptobox shards_proofs_precomputation proto_parameters
    proto_plugins store gs_worker transport_layer cctxt metrics_server crawler
    last_processed_level_store =
  let neighbors_cctxts =
    List.map
      (fun Configuration_file.{addr; port} ->
        let endpoint =
          Uri.of_string ("http://" ^ addr ^ ":" ^ string_of_int port)
        in
        Dal_node_client.make_unix_cctxt endpoint)
      config.Configuration_file.neighbors
  in
  {
    config;
    cryptobox;
    shards_proofs_precomputation;
    proto_parameters;
    proto_plugins;
    ongoing_amplifications = Types.Slot_id.Set.empty;
    slots_under_reconstruction = Types.Slot_id.Map.empty;
    store;
    tezos_node_cctxt = cctxt;
    neighbors_cctxts;
    committee_cache =
      Committee_cache.create ~max_size:Constants.committee_cache_size;
    gs_worker;
    transport_layer;
    profile_ctxt = Profile_manager.empty;
    metrics_server;
    crawler;
    last_processed_level_store;
  }

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

let may_add_plugin ctxt cctxt ~block_level ~proto_level =
  let open Lwt_result_syntax in
  let* proto_plugins =
    Proto_plugins.may_add
      cctxt
      ctxt.proto_plugins
      ~first_level:block_level
      ~proto_level
  in
  ctxt.proto_plugins <- proto_plugins ;
  return_unit

let get_plugin_for_level ctxt ~level =
  Proto_plugins.get_plugin_for_level ctxt.proto_plugins ~level

let get_all_plugins ctxt = Proto_plugins.to_list ctxt.proto_plugins

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
  match storage_period ctxt proto_parameters with
  | `Always -> None
  | `Finite n ->
      let level = Int32.(sub current_level (of_int n)) in
      if level < 1l then None else Some level

let get_profile_ctxt ctxt = ctxt.profile_ctxt

let load_profile_ctxt ctxt =
  let open Lwt_syntax in
  let base_dir = Configuration_file.store_path ctxt.config in
  let* res = Profile_manager.load_profile_ctxt ~base_dir in
  match res with
  | Ok pctxt -> return_some pctxt
  | Error err ->
      let* () = Event.(emit loading_profiles_failed err) in
      return_none

let set_profile_ctxt ctxt ?(save = true) pctxt =
  let open Lwt_syntax in
  ctxt.profile_ctxt <- pctxt ;
  if save then
    let base_dir = Configuration_file.store_path ctxt.config in
    let* res = Profile_manager.save_profile_ctxt ctxt.profile_ctxt ~base_dir in
    match res with
    | Ok () -> return_unit
    | Error err -> Event.(emit saving_profiles_failed err)
  else return_unit

let get_config ctxt = ctxt.config

let get_cryptobox ctxt = ctxt.cryptobox

let get_proto_parameters ctxt = ctxt.proto_parameters

let get_shards_proofs_precomputation ctxt = ctxt.shards_proofs_precomputation

let get_last_processed_level_store ctxt = ctxt.last_processed_level_store

let get_store ctxt = ctxt.store

let get_gs_worker ctxt = ctxt.gs_worker

let get_tezos_node_cctxt ctxt = ctxt.tezos_node_cctxt

let get_neighbors_cctxts ctxt = ctxt.neighbors_cctxts

let get_ongoing_amplifications ctxt = ctxt.ongoing_amplifications

let set_ongoing_amplifications ctxt ongoing_amplifications =
  ctxt.ongoing_amplifications <- ongoing_amplifications

let fetch_committee ctxt ~level =
  let open Lwt_result_syntax in
  let {tezos_node_cctxt = cctxt; committee_cache = cache; _} = ctxt in
  match Committee_cache.find cache ~level with
  | Some committee -> return committee
  | None ->
      let*? (module Plugin) =
        Proto_plugins.get_plugin_for_level ctxt.proto_plugins ~level
      in
      let+ committee = Plugin.get_committee cctxt ~level in
      Committee_cache.add cache ~level ~committee ;
      committee

let fetch_assigned_shard_indices ctxt ~level ~pkh =
  let open Lwt_result_syntax in
  let+ committee = fetch_committee ctxt ~level in
  match Tezos_crypto.Signature.Public_key_hash.Map.find pkh committee with
  | None -> []
  | Some indexes -> indexes

let get_fetched_assigned_shard_indices ctxt ~level ~pkh =
  Option.map
    (fun committee ->
      Tezos_crypto.Signature.Public_key_hash.Map.find_opt pkh committee
      |> Option.value ~default:[])
    (Committee_cache.find ctxt.committee_cache ~level)

let version {config; _} =
  let network_name = config.Configuration_file.network_name in
  Types.Version.make ~network_version:(Gossipsub.version ~network_name)

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
    let get_topics {gs_worker; _} =
      let state = Gossipsub.Worker.state gs_worker in
      Gossipsub.Worker.GS.Topic.Map.fold
        (fun topic _peers acc -> topic :: acc)
        state.mesh
        []

    let get_topics_peers ~subscribed ctx =
      let state = Gossipsub.Worker.state ctx.gs_worker in
      let topic_to_peers_map =
        Gossipsub.Worker.GS.Introspection.Connections.peers_per_topic_map
          state.connections
      in
      let subscribed_topics = lazy (get_topics ctx) in
      Gossipsub.Worker.GS.Topic.Map.fold
        (fun topic peers acc ->
          if
            (not subscribed)
            || List.mem
                 ~equal:Types.Topic.equal
                 topic
                 (Lazy.force subscribed_topics)
          then (topic, Gossipsub.Worker.GS.Peer.Set.elements peers) :: acc
          else acc)
        topic_to_peers_map
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
