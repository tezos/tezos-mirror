(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

exception Status_already_ready

type ready_ctxt = {
  cryptobox : Cryptobox.t;
  proto_parameters : Dal_plugin.proto_parameters;
  plugin : (module Dal_plugin.T);
  shards_proofs_precomputation : Cryptobox.shards_proofs_precomputation option;
  plugin_proto : int; (* the [proto_level] of the plugin *)
  last_processed_level : int32 option;
}

type status = Ready of ready_ctxt | Starting

type t = {
  mutable status : status;
  config : Configuration_file.t;
  store : Store.node_store;
  tezos_node_cctxt : Tezos_rpc.Context.generic;
  neighbors_cctxts : Dal_node_client.cctxt list;
  committee_cache : Committee_cache.t;
  gs_worker : Gossipsub.Worker.t;
  transport_layer : Gossipsub.Transport_layer.t;
  mutable profile_ctxt : Profile_manager.t;
  metrics_server : Metrics.t;
}

let init config store gs_worker transport_layer cctxt metrics_server =
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
    status = Starting;
    config;
    store;
    tezos_node_cctxt = cctxt;
    neighbors_cctxts;
    committee_cache =
      Committee_cache.create ~max_size:Constants.committee_cache_size;
    gs_worker;
    transport_layer;
    profile_ctxt = Profile_manager.empty;
    metrics_server;
  }

let set_ready ctxt plugin cryptobox shards_proofs_precomputation
    proto_parameters plugin_proto =
  let open Result_syntax in
  match ctxt.status with
  | Starting ->
      let* () =
        Profile_manager.validate_slot_indexes
          ctxt.profile_ctxt
          ~number_of_slots:proto_parameters.Dal_plugin.number_of_slots
      in
      ctxt.status <-
        Ready
          {
            plugin;
            cryptobox;
            proto_parameters;
            shards_proofs_precomputation;
            plugin_proto;
            last_processed_level = None;
          } ;
      return_unit
  | Ready _ -> raise Status_already_ready

let update_plugin_in_ready ctxt plugin proto =
  match ctxt.status with
  | Starting -> ()
  | Ready ready_ctxt ->
      ctxt.status <- Ready {ready_ctxt with plugin; plugin_proto = proto}

type error += Node_not_ready

let () =
  register_error_kind
    `Permanent
    ~id:"dal.node.not.ready"
    ~title:"DAL Node not ready"
    ~description:"DAL node is starting. It's not ready to respond to RPCs."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "DAL node is starting. It's not ready to respond to RPCs.")
    Data_encoding.(unit)
    (function Node_not_ready -> Some () | _ -> None)
    (fun () -> Node_not_ready)

let get_ready ctxt =
  let open Result_syntax in
  match ctxt.status with
  | Ready ctxt -> Ok ctxt
  | Starting -> fail [Node_not_ready]

let update_last_processed_level ctxt ~level =
  let open Result_syntax in
  match ctxt.status with
  | Ready ready_ctxt ->
      ctxt.status <- Ready {ready_ctxt with last_processed_level = Some level} ;
      return_unit
  | Starting -> fail [Node_not_ready]

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

let get_status ctxt = ctxt.status

let get_store ctxt = ctxt.store

let get_gs_worker ctxt = ctxt.gs_worker

let get_tezos_node_cctxt ctxt = ctxt.tezos_node_cctxt

let get_neighbors_cctxts ctxt = ctxt.neighbors_cctxts

let fetch_committee ctxt ~level =
  let open Lwt_result_syntax in
  let {tezos_node_cctxt = cctxt; committee_cache = cache; _} = ctxt in
  match Committee_cache.find cache ~level with
  | Some committee -> return committee
  | None ->
      let*? {plugin = (module Plugin); _} = get_ready ctxt in
      let+ committee = Plugin.get_committee cctxt ~level in
      let committee =
        Tezos_crypto.Signature.Public_key_hash.Map.map
          (fun (start_index, offset) -> Committee_cache.{start_index; offset})
          committee
      in
      Committee_cache.add cache ~level ~committee ;
      committee

let fetch_assigned_shard_indices ctxt ~level ~pkh =
  let open Lwt_result_syntax in
  let+ committee = fetch_committee ctxt ~level in
  match Tezos_crypto.Signature.Public_key_hash.Map.find pkh committee with
  | None -> []
  | Some {start_index; offset} ->
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/4540
         Consider returning some abstract representation of [(s, n)]
         instead of [int list] *)
      Stdlib.List.init offset (fun i -> start_index + i)

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

    let get_connections {gs_worker; _} =
      let state = Gossipsub.Worker.state gs_worker in
      Gossipsub.Worker.GS.Introspection.Connections.fold
        (fun peer connection acc ->
          ( peer,
            Types.Gossipsub.
              {
                topics = Gossipsub.Worker.GS.Topic.Set.elements connection.topics;
                direct = connection.direct;
                outbound = connection.outbound;
              } )
          :: acc)
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
