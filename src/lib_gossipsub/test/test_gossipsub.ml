(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Tezos_gossipsub
open Gossipsub_intf

let per_topic_score_parameters =
  Topic_score_parameters_single
    {
      time_in_mesh_weight = 1.0;
      time_in_mesh_cap = 3600.0;
      time_in_mesh_quantum = 1.0;
    }

let score_parameters =
  {
    topics = per_topic_score_parameters;
    behaviour_penalty_weight = ~-.10.0;
    behaviour_penalty_threshold = 0.0;
  }

(* Most of these limits are the default ones used by the Go implementation. *)
let default_limits =
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
    unsubscribe_backoff = 10;
    graft_flood_backoff = -50;
    prune_backoff = 60;
    retain_duration = 10;
    fanout_ttl = 60;
    heartbeat_interval = 1;
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
    score_parameters;
  }

let parameters = {peer_filter = (fun _peer _action -> true)}

(* This is to use a seed with Tezt. *)
let rng =
  let seed =
    match
      Tezt_core.Cli.get
        ~default:None
        (fun x ->
          try int_of_string x |> Option.some |> Option.some
          with _ -> Option.none)
        "seed"
    with
    | None ->
        Random.self_init () ;
        Random.bits ()
    | Some seed -> seed
  in
  Random.State.make [|seed|]

let () =
  Test_unit.register rng default_limits parameters ;
  Test_integration_worker.register rng default_limits parameters ;
  Test_pbt.register rng default_limits parameters ;
  Tezt.Test.run ()
