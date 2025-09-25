(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Functori,     <contact@functori.com>                   *)
(*                                                                           *)
(*****************************************************************************)

let namespace = "dal"

module Node_metrics = struct
  let subsystem = "node"

  let number_of_reconstructions_started =
    let name = "amplification_reconstructions_started_count" in
    Prometheus.Counter.v
      ~help:"Number of reconstructions started for observer's amplification"
      ~namespace
      ~subsystem
      name

  let number_of_reconstructions_done =
    let name = "amplification_reconstructions_done_count" in
    Prometheus.Counter.v
      ~help:"Number of reconstructions finished for observer's amplification"
      ~namespace
      ~subsystem
      name

  let number_of_reconstructions_aborted =
    let name = "amplification_reconstructions_aborted_count" in
    Prometheus.Counter.v
      ~help:
        "Number of reconstructions aborted for observer's amplification, \
         because observer received all the shards during its random delay"
      ~namespace
      ~subsystem
      name

  let amplification_queue_length =
    let name = "amplification_queue_length" in
    Prometheus.Gauge.v
      ~help:"Length of enqueued reconstruction tasks"
      ~namespace
      ~subsystem
      name

  let amplification_complete_duration =
    let name = "amplification_complete_duration_seconds" in
    Prometheus.DefaultHistogram.v
      ~help:
        "Total duration between the reception of a shard and the publication \
         after a reconstruction"
      ~namespace
      ~subsystem
      name

  let number_of_stored_shards =
    let name = "number_of_stored_shards" in
    Prometheus.Counter.v
      ~help:"Number of stored shards (local injection or remote reception)"
      ~namespace
      ~subsystem
      name

  let verify_shard_time =
    let name = "verify_shard_time" in
    Prometheus.DefaultHistogram.v
      ~help:
        "Time taken to verify a shard in seconds (random sample each 100 \
         shards verification)"
      ~namespace
      ~subsystem
      name

  let slots_waiting_for_attestation =
    let name = "slots_waiting_for_attestation" in
    Prometheus.Gauge.v_label
      ~label_name:"slot_index"
      ~help:
        "The slot at index <i> is waiting for attestation (value is 1) or not \
         (value is 0)"
      ~namespace
      ~subsystem
      name

  let slots_attested =
    let name = "slots_attested" in
    Prometheus.Gauge.v_label
      ~label_name:"slot_index"
      ~help:"The slot at index <i> is attested (value is 1) or not (value is 0)"
      ~namespace
      ~subsystem
      name

  let attested_slots_for_baker_per_level_ratio =
    let name = "attested_slot_for_baker_per_level_ratio" in
    Prometheus.Gauge.v_label
      ~label_name:"attester"
      ~help:
        "Ratio between the number of slots attested by the baker over the \
         total number of attestable slots per level."
      ~namespace
      ~subsystem
      name

  let new_layer1_head =
    let name = "new_layer1_head" in
    Prometheus.Gauge.v
      ~help:"A new layer 1 head with the given level has been received"
      ~namespace
      ~subsystem
      name

  let new_layer1_head_round =
    let name = "new_layer1_head_round" in
    Prometheus.Gauge.v
      ~help:"A new layer 1 head with the given round has been received"
      ~namespace
      ~subsystem
      name

  let layer1_block_finalized =
    let name = "layer1_block_finalized" in
    Prometheus.Gauge.v
      ~help:"The layer 1 block with the given level has been finalized"
      ~namespace
      ~subsystem
      name

  let layer1_block_finalized_round =
    let name = "layer1_block_finalized_round" in
    Prometheus.Gauge.v
      ~help:"The layer 1 block with the given round has been finalized"
      ~namespace
      ~subsystem
      name

  let kvs_shards_opened_files =
    let name = "kvs_shards_opened_files" in
    Prometheus.Gauge.v
      ~help:
        "The size of the table containing opened files by the key-value store \
         for shards"
      ~namespace
      ~subsystem
      name

  let kvs_shards_ongoing_actions =
    let name = "kvs_shards_ongoing_actions" in
    Prometheus.Gauge.v
      ~help:
        "The number of ongoing actions (at most 1 per file) associated with \
         the key-value store for shards"
      ~namespace
      ~subsystem
      name

  let amplification_enough_shards_received_duration =
    let name = "amplification_enough_shards_received_duration_seconds" in
    Prometheus.DefaultHistogram.v
      ~help:
        "Duration between the reception of the first shard and enough shards \
         to reconstruct"
      ~namespace
      ~subsystem
      name

  let amplification_all_shards_received_duration =
    let name = "amplification_all_shards_received_duration_seconds" in
    Prometheus.DefaultHistogram.v
      ~help:
        "Duration between the reception of the first shard and the complete \
         set of shards"
      ~namespace
      ~subsystem
      name

  let amplification_abort_reconstruction_duration =
    let name = "amplification_abort_reconstruction_duration_seconds" in
    Prometheus.DefaultHistogram.v
      ~help:
        "Duration between the reception of a first shard and the abortion of \
         its reconstruction"
      ~namespace
      ~subsystem
      name

  let amplification_start_reconstruction_duration =
    let name = "amplification_start_reconstruction_duration_seconds" in
    Prometheus.DefaultHistogram.v
      ~help:
        "Duration between the reception of the first shard and the beginning \
         of the reconstruction"
      ~namespace
      ~subsystem
      name

  let per_level_processing_time =
    let name = "per_level_processing_time" in
    Prometheus.Gauge.v
      ~help:"Time spent in the new_finalized_head function at each level"
      ~namespace
      ~subsystem
      name
end

module GS = struct
  let subsystem = "gs"

  let metric ~help ~name collector =
    let info =
      {
        Prometheus.MetricInfo.name =
          Prometheus.MetricName.v
            (String.concat "_" [namespace; subsystem; name]);
        help;
        metric_type = Gauge;
        label_names = [];
      }
    in
    let collect () =
      Prometheus.LabelSetMap.singleton
        []
        [Prometheus.Sample_set.sample (collector ())]
    in
    (info, collect)

  let labeled_metric ~help ~name ~label_names collectors =
    let info =
      {
        Prometheus.MetricInfo.name =
          Prometheus.MetricName.v
            (String.concat "_" [namespace; subsystem; name]);
        help;
        metric_type = Gauge;
        label_names = List.map Prometheus.LabelName.v label_names;
      }
    in
    (info, collectors)

  let add_metric (info, collector) =
    Prometheus.CollectorRegistry.(register default) info collector

  module Stats = struct
    let gs_stats = ref (Gossipsub.Worker.Introspection.empty_stats ())

    let input_events_stream_length = ref 0

    let p2p_output_stream_length = ref 0

    let app_output_stream_length = ref 0

    let count_peers_per_topic :
        Prometheus.Sample_set.sample trace Prometheus.LabelSetMap.t ref =
      ref Prometheus.LabelSetMap.empty

    let scores_of_peers :
        Prometheus.Sample_set.sample trace Prometheus.LabelSetMap.t ref =
      ref Prometheus.LabelSetMap.empty

    let topic_as_label Types.Topic.{pkh; slot_index} =
      [
        string_of_int slot_index;
        Format.asprintf "%a" Signature.Public_key_hash.pp pkh;
      ]

    let collect_peers_per_topic_metrics gs_state =
      let module W = Gossipsub.Worker in
      W.GS.Topic.Map.fold
        (fun topic peers accu ->
          Prometheus.LabelSetMap.add
            (topic_as_label topic)
            [
              W.GS.Peer.Set.cardinal peers
              |> float |> Prometheus.Sample_set.sample;
            ]
            accu)
        gs_state.W.GS.Introspection.mesh
        Prometheus.LabelSetMap.empty

    let collect_scores_of_peers_metrics gs_state =
      let module W = Gossipsub.Worker in
      W.GS.Peer.Map.fold
        (fun peer score accu ->
          Prometheus.LabelSetMap.add
            [Format.asprintf "%a" W.GS.Peer.pp peer]
            [
              W.GS.Score.(value score |> Introspection.to_float)
              |> Prometheus.Sample_set.sample;
            ]
            accu)
        gs_state.W.GS.Introspection.scores
        Prometheus.LabelSetMap.empty

    (* This function is called by Prometheus to collect Gossipsub data every X
       seconds, where X is the refresh frequency of the Prometeus server. *)
    let collectors_callback gs_worker =
      let module W = Gossipsub.Worker in
      gs_stats := W.stats gs_worker ;
      input_events_stream_length :=
        W.input_events_stream gs_worker |> W.Stream.length ;
      p2p_output_stream_length :=
        W.p2p_output_stream gs_worker |> W.Stream.length ;
      app_output_stream_length :=
        W.app_output_stream gs_worker |> W.Stream.length ;
      let gs_state = W.state gs_worker in
      (* For [count_peers_per_topic] and [scores_of_peers], we store directly the
         data in the format required by Prometheus to avoid re-folding on the
         data again. *)
      count_peers_per_topic := collect_peers_per_topic_metrics gs_state ;
      scores_of_peers := collect_scores_of_peers_metrics gs_state
  end

  (* Metrics about the stats gathered by the worker *)

  let count_topics =
    metric
      ~name:"count_topics"
      ~help:"The number of topics of the node"
      (fun () -> Int64.to_float !Stats.gs_stats.count_topics)

  let count_connections =
    metric
      ~name:"count_connections"
      ~help:"Count the number connections of the node"
      (fun () -> Int64.to_float !Stats.gs_stats.count_connections)

  let count_bootstrap_connections =
    metric
      ~name:"count_bootstrap_connections"
      ~help:"Count the number of bootstrap connections of the node"
      (fun () -> Int64.to_float !Stats.gs_stats.count_bootstrap_connections)

  let count_received_valid_messages =
    metric
      ~name:"count_received_valid_messages"
      ~help:
        "Count the number of received valid application messages by the node"
      (fun () -> Int64.to_float !Stats.gs_stats.count_recv_valid_app_messages)

  let count_received_invalid_messages =
    metric
      ~name:"count_received_invalid_messages"
      ~help:
        "Count the number of received invalid application messages by the node"
      (fun () -> Int64.to_float !Stats.gs_stats.count_recv_invalid_app_messages)

  let count_received_outdated_messages =
    metric
      ~name:"count_received_outdated_messages"
      ~help:
        "Count the number of received outdated application messages by the node"
      (fun () ->
        Int64.to_float !Stats.gs_stats.count_recv_outdated_app_messages)

  let count_received_unknown_validity_messages =
    metric
      ~name:"count_received_unknown_validity_messages"
      ~help:
        "Count the number of received application messages by the node with \
         unknown validity"
      (fun () ->
        Int64.to_float !Stats.gs_stats.count_recv_unknown_validity_app_messages)

  let count_received_grafts =
    metric
      ~name:"count_received_grafts"
      ~help:"Count the number of received grafts by the node"
      (fun () -> Int64.to_float !Stats.gs_stats.count_recv_grafts)

  let count_received_prunes =
    metric
      ~name:"count_received_prunes"
      ~help:"Count the number of received prunes by the node"
      (fun () -> Int64.to_float !Stats.gs_stats.count_recv_prunes)

  let count_received_ihaves =
    metric
      ~name:"count_received_ihaves"
      ~help:"Count the number of received ihaves by the node"
      (fun () -> Int64.to_float !Stats.gs_stats.count_recv_ihaves)

  let count_received_iwants =
    metric
      ~name:"count_received_iwants"
      ~help:"Count the number of received iwants by the node"
      (fun () -> Int64.to_float !Stats.gs_stats.count_recv_iwants)

  let count_sent_messages =
    metric
      ~name:"count_sent_messages"
      ~help:"Count the number of sent application messages by the node"
      (fun () -> Int64.to_float !Stats.gs_stats.count_sent_app_messages)

  let count_sent_grafts =
    metric
      ~name:"count_sent_grafts"
      ~help:"Count the number of sent grafts by the node"
      (fun () -> Int64.to_float !Stats.gs_stats.count_sent_grafts)

  let count_sent_prunes =
    metric
      ~name:"count_sent_prunes"
      ~help:"Count the number of sent prunes by the node"
      (fun () -> Int64.to_float !Stats.gs_stats.count_sent_prunes)

  let count_sent_ihaves =
    metric
      ~name:"count_sent_ihaves"
      ~help:"Count the number of sent ihaves by the node"
      (fun () -> Int64.to_float !Stats.gs_stats.count_sent_ihaves)

  let count_sent_iwants =
    metric
      ~name:"count_sent_iwants"
      ~help:"Count the number of sent iwants by the node"
      (fun () -> Int64.to_float !Stats.gs_stats.count_sent_iwants)

  (* Metrics about the worker's streams *)

  let input_events_stream_length =
    metric
      ~name:"input_events_stream_length"
      ~help:
        "The number of elements currently in the Gossipsub worker's input \
         events stream"
      (fun () -> float !Stats.input_events_stream_length)

  let p2p_output_stream_length =
    metric
      ~name:"p2p_output_stream_length"
      ~help:
        "The number of elements currently in the Gossipsub worker's P2P output \
         stream"
      (fun () -> float !Stats.p2p_output_stream_length)

  let app_output_stream_length =
    metric
      ~name:"app_output_stream_length"
      ~help:
        "The number of elements currently in the Gossipsub worker's \
         application output stream"
      (fun () -> float !Stats.app_output_stream_length)

  (* Labeled Metrics for the gossipsub automaton's state *)

  let count_peers_per_topic =
    labeled_metric
      ~name:"count_peers_per_topic"
      ~help:"The number of peers the node is connected to per topic in the mesh"
      ~label_names:["slot_index"; "pkh"]
      (fun () -> !Stats.count_peers_per_topic)

  let scores_of_peers =
    labeled_metric
      ~name:"scores_of_peers"
      ~help:"The score of peers connected to the node"
      ~label_names:["peer"]
      (fun () -> !Stats.scores_of_peers)

  let metrics =
    [
      (* Metrics about the stats gathered by the worker *)
      count_topics;
      count_connections;
      count_bootstrap_connections;
      count_received_valid_messages;
      count_received_invalid_messages;
      count_received_unknown_validity_messages;
      count_received_outdated_messages;
      count_received_grafts;
      count_received_prunes;
      count_received_ihaves;
      count_received_iwants;
      count_sent_messages;
      count_sent_grafts;
      count_sent_prunes;
      count_sent_ihaves;
      count_sent_iwants;
      (* Metrics about the worker's streams *)
      input_events_stream_length;
      p2p_output_stream_length;
      app_output_stream_length;
      (* Other metrics about GS automaton's state *)
      count_peers_per_topic;
      scores_of_peers;
    ]

  let () = List.iter add_metric metrics
end

(* Stores metrics about reception and validation of shards *)
type slot_metrics = {
  time_first_shard_received : float;
  duration_all_shards_received : float option;
  duration_first_shard_validated : float option;
  duration_enough_shards_validated : float option;
  duration_all_shards_validated : float option;
}

let slot_metrics_encoding =
  let open Data_encoding in
  conv
    (fun {
           time_first_shard_received;
           duration_all_shards_received;
           duration_first_shard_validated;
           duration_enough_shards_validated;
           duration_all_shards_validated;
         }
       ->
      ( time_first_shard_received,
        duration_all_shards_received,
        duration_first_shard_validated,
        duration_enough_shards_validated,
        duration_all_shards_validated ))
    (fun ( time_first_shard_received,
           duration_all_shards_received,
           duration_first_shard_validated,
           duration_enough_shards_validated,
           duration_all_shards_validated )
       ->
      {
        time_first_shard_received;
        duration_all_shards_received;
        duration_first_shard_validated;
        duration_enough_shards_validated;
        duration_all_shards_validated;
      })
    (obj5
       (req "time_first_shard_received" float)
       (opt "duration_all_shards_received" float)
       (opt "duration_first_shard_validated" float)
       (opt "duration_enough_shards_validated" float)
       (opt "duration_all_shards_validated" float))

let pp_slot_metrics fmt
    {
      time_first_shard_received;
      duration_all_shards_received;
      duration_first_shard_validated;
      duration_enough_shards_validated;
      duration_all_shards_validated;
    } =
  Format.fprintf
    fmt
    "{First shard received at: %f; %s; %s; %s; %s}"
    time_first_shard_received
    (Option.fold
       ~none:"Some shards are still not received"
       ~some:(fun dur ->
         Format.sprintf "All shards were received in %f seconds" dur)
       duration_all_shards_received)
    (Option.fold
       ~none:"First shard is still not validated"
       ~some:(fun dur ->
         Format.sprintf "First shard validation came in %f seconds" dur)
       duration_first_shard_validated)
    (Option.fold
       ~none:"Not enough validated shards for reconstruction"
       ~some:(fun dur ->
         Format.sprintf
           "Enough shards were validated for reconstruction after %f seconds"
           dur)
       duration_enough_shards_validated)
    (Option.fold
       ~none:"Some shards are still not validated"
       ~some:(fun dur ->
         Format.sprintf "All shards were validated in %f seconds" dur)
       duration_all_shards_validated)

let pp_slot_metrics_received fmt
    {time_first_shard_received; duration_all_shards_received; _} =
  Format.fprintf
    fmt
    "{First shard received at: %f; %s}"
    time_first_shard_received
    (Option.fold
       ~none:"Some shards are still not received"
       ~some:(fun dur ->
         Format.sprintf "All shards were received in %f seconds" dur)
       duration_all_shards_received)

(* Bounded map, serving as cache to store shard reception timing values *)
module Slot_id_bounded_map =
  Aches.Vache.Map (Aches.Vache.LRU_Sloppy) (Aches.Vache.Strong) (Types.Slot_id)

let reconstruction_started () =
  Prometheus.Counter.inc_one Node_metrics.number_of_reconstructions_started

let reconstruction_done () =
  Prometheus.Counter.inc_one Node_metrics.number_of_reconstructions_done

let update_amplification_complete_duration duration =
  Prometheus.DefaultHistogram.observe
    Node_metrics.amplification_complete_duration
    duration

let update_amplification_queue_length n =
  Int.to_float n |> Prometheus.Gauge.set Node_metrics.amplification_queue_length

let reconstruction_aborted () =
  Prometheus.Counter.inc_one Node_metrics.number_of_reconstructions_aborted

let shard_stored () =
  Prometheus.Counter.inc_one Node_metrics.number_of_stored_shards

let slot_waiting_for_attestation ~set i =
  let v = float_of_int @@ if set then 1 else 0 in
  Prometheus.Gauge.set
    (Node_metrics.slots_waiting_for_attestation (string_of_int i))
    v

let slot_attested ~set i =
  let v = float_of_int @@ if set then 1 else 0 in
  Prometheus.Gauge.set (Node_metrics.slots_attested (string_of_int i)) v

let attested_slots_for_baker_per_level_ratio ~delegate ratio =
  let attester = Format.asprintf "%a@." Signature.Public_key_hash.pp delegate in
  Prometheus.Gauge.set
    (Node_metrics.attested_slots_for_baker_per_level_ratio attester)
    ratio

let new_layer1_head ~head_level =
  Int32.to_float head_level |> Prometheus.Gauge.set Node_metrics.new_layer1_head

let new_layer1_head_round ~head_round =
  Int32.to_float head_round
  |> Prometheus.Gauge.set Node_metrics.new_layer1_head_round

let layer1_block_finalized ~block_level =
  Int32.to_float block_level
  |> Prometheus.Gauge.set Node_metrics.layer1_block_finalized

let layer1_block_finalized_round ~block_round =
  Int32.to_float block_round
  |> Prometheus.Gauge.set Node_metrics.layer1_block_finalized_round

let update_shards_verification_time f =
  Prometheus.DefaultHistogram.observe Node_metrics.verify_shard_time f

let update_kvs_shards_metrics ~opened_files ~ongoing_actions =
  Prometheus.Gauge.set
    Node_metrics.kvs_shards_ongoing_actions
    (float ongoing_actions) ;
  Prometheus.Gauge.set Node_metrics.kvs_shards_opened_files (float opened_files)

let update_amplification_enough_shards_received_duration duration =
  Prometheus.DefaultHistogram.observe
    Node_metrics.amplification_enough_shards_received_duration
    duration

let update_amplification_all_shards_received_duration duration =
  Prometheus.DefaultHistogram.observe
    Node_metrics.amplification_all_shards_received_duration
    duration

let update_amplification_start_reconstruction_duration duration =
  Prometheus.DefaultHistogram.observe
    Node_metrics.amplification_start_reconstruction_duration
    duration

let update_amplification_abort_reconstruction_duration duration =
  Prometheus.DefaultHistogram.observe
    Node_metrics.amplification_abort_reconstruction_duration
    duration

let per_level_processing_time =
  Prometheus.Gauge.set Node_metrics.per_level_processing_time

let sample_time ~sampling_frequency ~to_sample ~metric_updater =
  if sampling_frequency > 0 && Random.int sampling_frequency <> 0 then
    to_sample ()
  else
    let before = Time.System.now () in
    let res = to_sample () in
    let after = Time.System.now () in
    Ptime.diff after before |> Ptime.Span.to_float_s |> metric_updater ;
    res

let collect_gossipsub_metrics gs_worker =
  Prometheus.CollectorRegistry.(register_pre_collect default) (fun () ->
      GS.Stats.collectors_callback gs_worker)

let update_timing_shard_received shards_timing_table ~last_expected_shard
    slot_id =
  let now = Unix.gettimeofday () in
  let updated, timing =
    match Slot_id_bounded_map.find_opt shards_timing_table slot_id with
    | None ->
        (* Note: we expect the entry is None only on the first received shard,
           while lwt might actually process this code after the second or third
           shard. This should be rare and the delta between values is pretty
           minimal *)
        ( true,
          {
            time_first_shard_received = now;
            duration_all_shards_received = None;
            duration_first_shard_validated = None;
            duration_enough_shards_validated = None;
            duration_all_shards_validated = None;
          } )
    | Some timing ->
        if last_expected_shard then
          let duration = now -. timing.time_first_shard_received in
          (true, {timing with duration_all_shards_received = Some duration})
        else (false, timing)
  in
  let () = Slot_id_bounded_map.replace shards_timing_table slot_id timing in
  (updated, timing)

let update_timing_shard_validated shards_timing_table
    ~number_of_already_stored_shards ~number_of_expected_shards
    ?min_shards_to_reconstruct_slot slot_id =
  let now = Unix.gettimeofday () in
  let updated, timing =
    match Slot_id_bounded_map.find_opt shards_timing_table slot_id with
    | None ->
        (* Note: we expect the entry to never be [None] since shards should be
           received before being validated, while lwt might actually process
           this code after first validation. This should be rare and happen only
           if the delta between values is pretty minimal *)
        ( true,
          {
            time_first_shard_received = now;
            duration_all_shards_received = None;
            duration_first_shard_validated = Some 0.;
            duration_enough_shards_validated = None;
            duration_all_shards_validated = None;
          } )
    | Some timing -> (
        let time_origin = timing.time_first_shard_received in
        let updated, timing =
          match timing.duration_first_shard_validated with
          | None ->
              ( true,
                {
                  timing with
                  duration_first_shard_validated = Some (now -. time_origin);
                } )
          | Some _ -> (false, timing)
        in
        let are_all_shard_validated =
          number_of_already_stored_shards = number_of_expected_shards
        in
        if are_all_shard_validated then (
          let duration = now -. time_origin in
          if Option.is_some min_shards_to_reconstruct_slot then
            update_amplification_all_shards_received_duration duration ;
          (true, {timing with duration_all_shards_validated = Some duration}))
        else
          match min_shards_to_reconstruct_slot with
          | None -> (updated, timing)
          | Some min_shards_to_reconstruct_slot ->
              let is_enough_shard_received =
                Option.is_none timing.duration_enough_shards_validated
                && number_of_already_stored_shards
                   >= min_shards_to_reconstruct_slot
              in
              if is_enough_shard_received then (
                let duration = now -. time_origin in
                update_amplification_enough_shards_received_duration duration ;
                ( true,
                  {timing with duration_enough_shards_validated = Some duration}
                ))
              else (updated, timing))
  in
  let () = Slot_id_bounded_map.replace shards_timing_table slot_id timing in
  (updated, timing)
