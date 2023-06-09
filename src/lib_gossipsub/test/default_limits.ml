(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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
open Gossipsub_intf
module Milliseconds = Test_gossipsub_shared.Milliseconds

(* Most of these limits are the default ones used by the Go implementation. *)
let default_limits ?mesh_message_deliveries_activation ?time_in_mesh_weight
    ?time_in_mesh_quantum ?time_in_mesh_cap ?first_message_deliveries_weight
    ?first_message_deliveries_cap ?first_message_deliveries_decay
    ?mesh_message_deliveries_weight () :
    (string, int, int, Milliseconds.t) limits =
  let per_topic_score_limits =
    {
      time_in_mesh_weight = Option.value ~default:1.0 time_in_mesh_weight;
      time_in_mesh_cap = Option.value ~default:3600.0 time_in_mesh_cap;
      time_in_mesh_quantum =
        Option.value ~default:(Milliseconds.of_float_s 1.0) time_in_mesh_quantum;
      first_message_deliveries_weight =
        Option.value ~default:1.0 first_message_deliveries_weight;
      first_message_deliveries_cap =
        Option.value ~default:2000 first_message_deliveries_cap;
      first_message_deliveries_decay =
        Option.value ~default:0.5 first_message_deliveries_decay;
      mesh_message_deliveries_weight =
        mesh_message_deliveries_weight |> Option.value ~default:~-.1.0;
      mesh_message_deliveries_window = Milliseconds.of_float_s 0.01;
      mesh_message_deliveries_activation =
        mesh_message_deliveries_activation
        |> Option.map (fun t -> Milliseconds.of_int_s t)
        |> Option.value ~default:(Milliseconds.of_int_s 5);
      mesh_message_deliveries_cap = 100;
      mesh_message_deliveries_threshold = 20;
      mesh_message_deliveries_decay = 0.5;
      mesh_failure_penalty_weight = ~-.1.;
      mesh_failure_penalty_decay = 0.5;
      invalid_message_deliveries_weight = ~-.1.;
      invalid_message_deliveries_decay = 0.3;
    }
  in
  let topic_score_limits = Topic_score_limits_single per_topic_score_limits in
  let score_limits : (string, Milliseconds.t) score_limits =
    {
      topics = topic_score_limits;
      topic_score_cap = Some 3600.;
      behaviour_penalty_weight = ~-.10.0;
      behaviour_penalty_threshold = 0.0;
      behaviour_penalty_decay = 0.2;
      app_specific_weight = 10.;
      decay_zero = 0.1;
    }
  in
  {
    max_recv_ihave_per_heartbeat = 10;
    max_sent_iwant_per_heartbeat = 5000;
    max_gossip_retransmission = 3;
    degree_optimal = 6;
    publish_threshold = 0.;
    gossip_threshold = 0.;
    do_px = true;
    peers_to_px = 16;
    accept_px_threshold = 0.;
    unsubscribe_backoff = Milliseconds.Span.of_int_s 10;
    graft_flood_backoff = Milliseconds.Span.of_int_s ~-50;
    prune_backoff = Milliseconds.Span.of_int_s 60;
    retain_duration = Milliseconds.Span.of_int_s 10;
    fanout_ttl = Milliseconds.Span.of_int_s 60;
    heartbeat_interval = Milliseconds.Span.of_int_s 1;
    backoff_cleanup_ticks = 15;
    score_cleanup_ticks = 1;
    degree_low = 5;
    degree_high = 12;
    degree_score = 4;
    degree_out = 2;
    degree_lazy = 6;
    gossip_factor = 0.25;
    history_length = 5;
    history_gossip_length = 3;
    opportunistic_graft_ticks = 60L;
    opportunistic_graft_peers = 2;
    opportunistic_graft_threshold = 1.;
    seen_history_length = 120;
    score_limits;
  }
