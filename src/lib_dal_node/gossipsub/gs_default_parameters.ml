(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Functori,     <contact@functori.com>                   *)
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

(** This module defines the default values for simple Gossip parameters and
    limits. These values are used or re-defined in the DAL node before
    instantiating the Gossipsub worker. *)

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/5559

   Document why we choose these default values for GS parameters.

   Note: The current default value was copied from The Go implementation *)

module Topic_score = struct
  let time_in_mesh_weight = 1.0

  let time_in_mesh_cap = 3600.0

  let time_in_mesh_quantum = Types.Span.of_float_s 1.0

  let first_message_deliveries_weight = 1.0

  let first_message_deliveries_cap = 2000

  let first_message_deliveries_decay = 0.5

  (* This part of the score is deactivated because we cannot predict a
     delivery weight. *)
  let mesh_message_deliveries_weight = 0.

  let mesh_message_deliveries_window = Types.Span.of_float_s 0.01

  let mesh_message_deliveries_activation = Types.Span.of_int_s 5

  let mesh_message_deliveries_cap = 100

  let mesh_message_deliveries_threshold = 20

  let mesh_message_deliveries_decay = 0.5

  (* This part of the score is deactivated because we cannot predict a
     delivery weight. *)
  let mesh_failure_penalty_weight = 0.

  let mesh_failure_penalty_decay = 0.5

  let invalid_message_deliveries_weight = ~-.1.

  let invalid_message_deliveries_decay = 0.3
end

module Score = struct
  let topic_score_cap = Some 3600.

  let behaviour_penalty_weight = ~-.10.0

  let behaviour_penalty_threshold = 0.0

  let behaviour_penalty_decay = 0.2

  let app_specific_weight = 10.

  let decay_zero = 0.1
end

module Limits = struct
  let max_recv_ihave_per_heartbeat = 10

  let max_sent_iwant_per_heartbeat = 5000

  let max_gossip_retransmission = 3

  let degree_optimal = 6

  let publish_threshold = 0.

  let gossip_threshold = 0.

  let do_px = true

  let peers_to_px = 16

  let accept_px_threshold = 0.

  let unsubscribe_backoff = Types.Span.of_int_s 10

  let graft_flood_threshold = Types.Span.of_int_s 10

  let prune_backoff = Types.Span.of_int_s 60

  let retain_duration = Types.Span.of_int_s 10

  let fanout_ttl = Types.Span.of_int_s 60

  let heartbeat_interval = Types.Span.of_int_s 1

  (* The ping should be performed by the heartbeat handler at least
     every 45s, hence every [heartbeat_ping_interval] ticks when
     [heartbeat_interval] = 1s.

     A 45-second ping interval is a reasonable choice as it sits in
     the middle of the commonly recommended 30-60 second range for P2P
     networks.

     This timing provides a good balance between maintaining
     reliable connections and managing network overhead.

     It's frequent enough to detect stale peers before most
     NAT/firewall timeouts (which typically occur after 60-120
     seconds)
  *)
  let heartbeat_ping_interval =
    let heartbeat_interval = Types.Span.to_int_s heartbeat_interval in
    45 / heartbeat_interval

  let backoff_cleanup_ticks = 15

  let score_cleanup_ticks = 1

  let degree_low = 5

  let degree_high = 12

  let degree_score = 4

  let degree_out = 2

  let degree_lazy = 6

  let gossip_factor = 0.25

  let history_length = 5

  let history_gossip_length = 3

  let opportunistic_graft_ticks = 60L

  let opportunistic_graft_peers = 2

  let opportunistic_graft_threshold = 1.

  let seen_history_length = 120
end

module Peer_filter = struct
  let peer_filter _peer _action = true
end
