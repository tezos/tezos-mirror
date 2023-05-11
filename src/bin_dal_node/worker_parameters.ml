open Gossipsub_intf
module Span = Gossipsub.Worker.GS.Span

let per_topic_score_limits : (Gossipsub.topic, Span.t) topic_score_limits =
  let open Gossipsub.Worker.Default_parameters.Topic_score in
  Topic_score_limits_single
    {
      time_in_mesh_weight;
      time_in_mesh_cap;
      time_in_mesh_quantum;
      first_message_deliveries_weight;
      first_message_deliveries_cap;
      first_message_deliveries_decay;
      mesh_message_deliveries_weight;
      mesh_message_deliveries_window;
      mesh_message_deliveries_activation;
      mesh_message_deliveries_cap;
      mesh_message_deliveries_threshold;
      mesh_message_deliveries_decay;
      mesh_failure_penalty_weight;
      mesh_failure_penalty_decay;
      invalid_message_deliveries_weight;
      invalid_message_deliveries_decay;
    }

let score_limits =
  let open Gossipsub.Worker.Default_parameters.Score in
  {
    topics = per_topic_score_limits;
    topic_score_cap;
    behaviour_penalty_weight;
    behaviour_penalty_threshold;
    behaviour_penalty_decay;
    app_specific_weight;
    decay_zero;
  }

let limits =
  let open Gossipsub.Worker.Default_parameters.Limits in
  {
    max_recv_ihave_per_heartbeat;
    max_sent_iwant_per_heartbeat;
    max_gossip_retransmission;
    degree_optimal;
    publish_threshold;
    gossip_threshold;
    do_px;
    peers_to_px;
    accept_px_threshold;
    unsubscribe_backoff;
    graft_flood_backoff;
    prune_backoff;
    retain_duration;
    fanout_ttl;
    heartbeat_interval;
    backoff_cleanup_ticks;
    score_cleanup_ticks;
    degree_low;
    degree_high;
    degree_score;
    degree_out;
    degree_lazy;
    gossip_factor;
    history_length;
    history_gossip_length;
    opportunistic_graft_ticks;
    opportunistic_graft_peers;
    opportunistic_graft_threshold;
    seen_history_length;
    score_limits;
  }

let peer_filter_parameters =
  let open Gossipsub.Worker.Default_parameters.Peer_filter in
  {peer_filter}
