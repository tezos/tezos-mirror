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

exception Status_already_ready

module LevelMap = Map.Make (struct
  type t = Int32.t

  (* keys are ordered descendingly *)
  let compare a b = compare b a
end)

type proto_plugin = {proto_level : int; plugin : (module Dal_plugin.T)}

type proto_plugins = proto_plugin LevelMap.t

type ready_ctxt = {
  cryptobox : Cryptobox.t;
  proto_parameters : Dal_plugin.proto_parameters;
  proto_plugins : proto_plugins;
  shards_proofs_precomputation : Cryptobox.shards_proofs_precomputation option;
  last_processed_level : int32 option;
  skip_list_cells_store : Skip_list_cells_store.t;
  mutable ongoing_amplifications : Types.Slot_id.Set.t;
}

type status = Ready of ready_ctxt | Starting

type t = {
  mutable status : status;
  config : Configuration_file.t;
  store : Store.t;
  tezos_node_cctxt : Tezos_rpc.Context.generic;
  neighbors_cctxts : Dal_node_client.cctxt list;
  committee_cache : Committee_cache.t;
  gs_worker : Gossipsub.Worker.t;
  transport_layer : Gossipsub.Transport_layer.t;
  mutable profile_ctxt : Profile_manager.t;
  metrics_server : Metrics.t;
  crawler : Crawler.t;
}

let init config store gs_worker transport_layer cctxt metrics_server crawler =
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
    crawler;
  }

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

let get_all_plugins ctxt =
  match ctxt.status with
  | Starting -> []
  | Ready {proto_plugins; _} ->
      LevelMap.bindings proto_plugins
      |> List.map (fun (_block_level, {proto_level = _; plugin}) -> plugin)

type error += No_plugin_for_proto of {proto_hash : Protocol_hash.t}

let () =
  register_error_kind
    `Permanent
    ~id:"dal.node.no_plugin_for_proto"
    ~title:"DAL node: no plugin for protocol"
    ~description:"DAL node: no plugin for the protocol %a"
    ~pp:(fun ppf proto_hash ->
      Format.fprintf
        ppf
        "No plugin for the protocol %a."
        Protocol_hash.pp
        proto_hash)
    Data_encoding.(obj1 (req "proto_hash" Protocol_hash.encoding))
    (function No_plugin_for_proto {proto_hash} -> Some proto_hash | _ -> None)
    (fun proto_hash -> No_plugin_for_proto {proto_hash})

let resolve_plugin proto_hash =
  let open Lwt_result_syntax in
  let plugin_opt = Dal_plugin.get proto_hash in
  match plugin_opt with
  | None ->
      let*! () = Event.(emit no_protocol_plugin proto_hash) in
      fail [No_plugin_for_proto {proto_hash}]
  | Some plugin ->
      let*! () = Event.(emit protocol_plugin_resolved proto_hash) in
      return plugin

(* Loads the plugins for levels between [current_level - 2 - attestation_lag]
   and [current_level], where [current_level] is the level at which the DAL
   node started. Note that if a migration has happened in this interval, there
   will be two plugins to be loaded. Note also that the node does not need the
   plugin for levels smaller than [current_level - 2 - attestation_lag]
   ([current_level - 2] is the level of the first processed block). *)
let load_plugins cctxt ~current_level ~attestation_lag =
  let open Lwt_result_syntax in
  let last_level = current_level in
  let first_level =
    Int32.max 1l (Int32.sub current_level (Int32.of_int (attestation_lag + 2)))
  in
  let block = `Level first_level in
  let* protocols =
    Tezos_shell_services.Chain_services.Blocks.protocols cctxt ~block ()
  in
  let first_proto = protocols.next_protocol in
  let* plugin = resolve_plugin first_proto in
  let* header = Shell_services.Blocks.Header.shell_header cctxt ~block () in
  let proto_level = header.proto_level in
  let proto_plugins =
    LevelMap.add first_level {proto_level; plugin} LevelMap.empty
  in
  let* last_protocols =
    Chain_services.Blocks.protocols cctxt ~block:(`Level last_level) ()
  in
  let last_proto = last_protocols.Chain_services.Blocks.next_protocol in
  let* proto_plugins =
    if Protocol_hash.equal first_proto last_proto then
      (* There's no migration in between, we're done. *)
      return proto_plugins
    else
      (* There was a migration in between; we search the migration level and then
         we add the plugin *)
      let rec find_migration_level level protocols =
        if
          Protocol_hash.equal
            first_proto
            protocols.Chain_services.Blocks.current_protocol
        then return level
        else
          let block = `Level (Int32.pred level) in
          let* protocols = Chain_services.Blocks.protocols cctxt ~block () in
          find_migration_level (Int32.pred level) protocols
      in
      let* migration_level = find_migration_level last_level last_protocols in
      let* plugin = resolve_plugin last_proto in
      let* header =
        Shell_services.Blocks.Header.shell_header
          cctxt
          ~block:(`Level migration_level)
          ()
      in
      let proto_level = header.proto_level in
      LevelMap.add migration_level {proto_level; plugin} proto_plugins |> return
  in
  return proto_plugins

let set_ready ctxt cctxt skip_list_cells_store cryptobox
    shards_proofs_precomputation proto_parameters ~level =
  let open Lwt_result_syntax in
  match ctxt.status with
  | Starting ->
      let*? () =
        Profile_manager.validate_slot_indexes
          ctxt.profile_ctxt
          ~number_of_slots:proto_parameters.Dal_plugin.number_of_slots
      in
      let attestation_lag = proto_parameters.attestation_lag in
      let* proto_plugins =
        load_plugins cctxt ~current_level:level ~attestation_lag
      in
      ctxt.status <-
        Ready
          {
            proto_plugins;
            cryptobox;
            proto_parameters;
            shards_proofs_precomputation;
            last_processed_level = None;
            skip_list_cells_store;
            ongoing_amplifications = Types.Slot_id.Set.empty;
          } ;
      return_unit
  | Ready _ -> raise Status_already_ready

let add_plugin_in_ready ctxt plugin ~proto_level ~block_level =
  match ctxt.status with
  | Starting -> ()
  | Ready ready_ctxt ->
      ctxt.status <-
        Ready
          {
            ready_ctxt with
            proto_plugins =
              LevelMap.add
                block_level
                {proto_level; plugin}
                ready_ctxt.proto_plugins;
          }

let add_plugin ctxt cctxt ~block_level ~proto_level =
  let open Lwt_result_syntax in
  let* protocols =
    Tezos_shell_services.Chain_services.Blocks.protocols
      cctxt
      ~block:(`Level block_level)
      ()
  in
  let proto_hash = protocols.next_protocol in
  let* plugin = resolve_plugin proto_hash in
  add_plugin_in_ready ctxt plugin ~block_level ~proto_level ;
  return_unit

let may_add_plugin ctxt cctxt ~block_level ~proto_level =
  let open Lwt_result_syntax in
  let*? {proto_plugins; _} = get_ready ctxt in
  let plugin_opt = LevelMap.min_binding_opt proto_plugins in
  match plugin_opt with
  | None -> add_plugin ctxt cctxt ~proto_level ~block_level
  | Some (_, {proto_level = prev_proto_level; _})
    when prev_proto_level < proto_level ->
      add_plugin ctxt cctxt ~proto_level ~block_level
  | _ -> return_unit

type error += No_plugin_for_level of {level : int32}

let () =
  register_error_kind
    `Permanent
    ~id:"dal.node.no_plugin"
    ~title:"DAL node: no plugin"
    ~description:"DAL node: no plugin for the given level"
    ~pp:(fun ppf level ->
      Format.fprintf ppf "No plugin for the level %ld." level)
    Data_encoding.(obj1 (req "level" int32))
    (function No_plugin_for_level {level} -> Some level | _ -> None)
    (fun level -> No_plugin_for_level {level})

let get_plugin_for_level ctxt ~level =
  let open Result_syntax in
  let* {proto_plugins; _} = get_ready ctxt in
  let plugins = LevelMap.bindings proto_plugins in
  (* Say that [plugins = [(level_1, plugin_1); ... ; (level_n, plugin_n)]]. We
     have [level_1 > ... > level_n]. We return the plugin [plugin_i] with the
     smallest [i] such that [level_i <= level]. *)
  let rec find = function
    | [] -> fail [No_plugin_for_level {level}]
    | (plugin_first_level, {plugin; proto_level = _}) :: rest ->
        if level >= plugin_first_level then return plugin else find rest
  in
  find plugins

let next_level_to_gc ctxt ~current_level =
  match ctxt.config.history_mode with
  | Full -> Int32.zero
  | Rolling {blocks = `Some n} ->
      Int32.(max zero (sub current_level (of_int n)))
  | Rolling {blocks = `Auto} -> (
      match ctxt.status with
      | Starting -> Int32.zero
      | Ready {proto_parameters; _} ->
          let n =
            Profile_manager.get_default_shard_store_period
              proto_parameters
              ctxt.profile_ctxt
          in
          Int32.(max zero (sub current_level (of_int n))))

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
      let*? (module Plugin) = get_plugin_for_level ctxt ~level in
      let+ committee = Plugin.get_committee cctxt ~level in
      Committee_cache.add cache ~level ~committee ;
      committee

let fetch_assigned_shard_indices ctxt ~level ~pkh =
  let open Lwt_result_syntax in
  let+ committee = fetch_committee ctxt ~level in
  match Tezos_crypto.Signature.Public_key_hash.Map.find pkh committee with
  | None -> []
  | Some indexes -> indexes

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
