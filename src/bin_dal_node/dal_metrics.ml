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
    let name = "slots_waiting_for_attestaion" in
    Prometheus.Gauge.v_label
      ~label_name:"slot_waiting_for_attestaion"
      ~help:
        "The slot at index <i> is waiting for attestation (value is 1) or not \
         (value is 0)"
      ~namespace
      ~subsystem
      name

  let slots_attested =
    let name = "slots_attested" in
    Prometheus.Gauge.v_label
      ~label_name:"slot_attested"
      ~help:"The slot at index <i> is attested (value is 1) or not (value is 0)"
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

  let labeled_metric ~help ~name ~label_name collectors =
    let info =
      {
        Prometheus.MetricInfo.name =
          Prometheus.MetricName.v
            (String.concat "_" [namespace; subsystem; name]);
        help;
        metric_type = Gauge;
        label_names = [Prometheus.LabelName.v label_name];
      }
    in
    let collect () =
      List.fold_left
        (fun acc (label, value) ->
          Prometheus.LabelSetMap.add
            [label]
            [Prometheus.Sample_set.sample value]
            acc)
        Prometheus.LabelSetMap.empty
        (collectors ())
    in
    (info, collect)

  let add_metric (info, collector) =
    Prometheus.CollectorRegistry.(register default) info collector

  module Stats = struct
    let gs_stats = ref (Gossipsub.Worker.Introspection.empty_stats ())

    let input_events_stream_length = ref 0

    let p2p_output_streams_length = ref 0

    let app_output_stream_length = ref 0

    let count_peers_per_topic = ref []

    let topic_as_label Types.Topic.{pkh; slot_index} =
      Format.asprintf
        "topic__pkh-%a__slot_index-%d }"
        Signature.Public_key_hash.pp
        pkh
        slot_index

    let set gs_worker =
      let module W = Gossipsub.Worker in
      gs_stats := W.stats gs_worker ;
      input_events_stream_length :=
        W.input_events_stream gs_worker |> W.Stream.length ;
      p2p_output_streams_length :=
        W.p2p_output_stream gs_worker |> W.Stream.length ;
      app_output_stream_length :=
        W.app_output_stream gs_worker |> W.Stream.length ;
      let gs_state = W.state gs_worker in
      count_peers_per_topic :=
        W.GS.Topic.Map.fold
          (fun topic peers accu ->
            (topic_as_label topic, W.GS.Peer.Set.cardinal peers |> float)
            :: accu)
          gs_state.mesh
          []
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
      ~help:"Count the number of receivedvalid application messages by the node"
      (fun () -> Int64.to_float !Stats.gs_stats.count_recv_invalid_app_messages)

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
      (fun () -> Int64.to_float !Stats.gs_stats.count_recv_prunes)

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

  let p2p_output_streams_length =
    metric
      ~name:"p2p_output_streams_length"
      ~help:
        "The number of elements currently in the Gossipsub worker's P2P output \
         stream"
      (fun () -> float !Stats.p2p_output_streams_length)

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
      ~help:
        "The number of peers the node is connected to per topic in the \
         mesh"
      ~label_name:"count_peers_per_topic"
      (fun () -> !Stats.count_peers_per_topic)

  let metrics =
    [
      (* Metrics about the stats gathered by the worker *)
      count_topics;
      count_connections;
      count_bootstrap_connections;
      count_received_valid_messages;
      count_received_invalid_messages;
      count_received_unknown_validity_messages;
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
      p2p_output_streams_length;
      app_output_stream_length;
      (* Other metrics about GS automaton's state *)
      count_peers_per_topic;
    ]

  let () = List.iter add_metric metrics
end

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

let update_shards_verification_time f =
  Prometheus.DefaultHistogram.observe Node_metrics.verify_shard_time f

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
      GS.Stats.set gs_worker)
