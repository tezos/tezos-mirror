(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

let namespace = Tezos_version.Node_version.namespace

let subsystem = "p2p"

let metric ~help ~component ~name collector =
  let info =
    {
      Prometheus.MetricInfo.name =
        Prometheus.MetricName.v
          (String.concat "_" [namespace; subsystem; component; name]);
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

(* we add this function to impose the correct namespace *)
let metric_counter ~component ~help ~name () =
  Prometheus.Counter.v
    ~help
    ~namespace
    ~subsystem
    (String.concat "_" [component; name])

let add_metric (info, collector) =
  Prometheus.CollectorRegistry.(register default) info collector

module Connections = struct
  let component = "connections"

  module Stats = struct
    let active = ref 0.

    let incoming = ref 0.

    let outgoing = ref 0.

    let private_ = ref 0.

    let zero () =
      active := 0. ;
      incoming := 0. ;
      outgoing := 0. ;
      private_ := 0.
  end

  let active_connections =
    metric
      ~help:"Number of active connections"
      ~component
      ~name:"active"
      (fun () -> !Stats.active)

  let incoming_connections =
    metric
      ~help:"Number of incoming connections"
      ~component
      ~name:"incoming"
      (fun () -> !Stats.incoming)

  let outgoing_connections =
    metric
      ~help:"Number of outgoing connections"
      ~component
      ~name:"outgoing"
      (fun () -> !Stats.outgoing)

  let private_connections =
    metric
      ~help:"Number of private connections"
      ~component
      ~name:"private"
      (fun () -> !Stats.private_)

  let metrics =
    [
      active_connections;
      incoming_connections;
      outgoing_connections;
      private_connections;
    ]

  let () = List.iter add_metric metrics

  let collect pool =
    Stats.zero () ;
    P2p_pool.Connection.iter
      (fun _ connection ->
        if P2p_conn.private_node connection then
          Stats.private_ := !Stats.private_ +. 1. ;
        Stats.active := !Stats.active +. 1. ;
        match P2p_conn.info connection with
        | {incoming = true; _} -> Stats.incoming := !Stats.incoming +. 1.
        | {incoming = false; _} -> Stats.outgoing := !Stats.outgoing +. 1.)
      pool
end

module Peers = struct
  let component = "peers"

  module Stats = struct
    let accepted = ref 0.

    let running = ref 0.

    let disconnected = ref 0.

    let zero () =
      accepted := 0. ;
      running := 0. ;
      disconnected := 0.
  end

  let accepted_peers =
    metric
      ~help:"Number of accepted connections"
      ~component
      ~name:"accepted"
      (fun () -> !Stats.accepted)

  let running_peers =
    metric ~help:"Number of running peers" ~component ~name:"running" (fun () ->
        !Stats.running)

  let disconnected_peers =
    metric
      ~help:"Number of disconnected peers"
      ~component
      ~name:"disconnected"
      (fun () -> !Stats.disconnected)

  let metrics = [accepted_peers; running_peers; disconnected_peers]

  let () = List.iter add_metric metrics

  let collect pool =
    Stats.zero () ;
    P2p_pool.Peers.iter_known
      (fun _ info ->
        match info with
        | info when P2p_peer_state.is_accepted info ->
            Stats.accepted := !Stats.accepted +. 1.
        | info when P2p_peer_state.is_disconnected info ->
            Stats.disconnected := !Stats.disconnected +. 1.
        | info when P2p_peer_state.is_running info ->
            Stats.running := !Stats.running +. 1.
        | _ -> ())
      pool
end

module Points = struct
  let component = "points"

  module Stats = struct
    let trusted = ref 0.

    let greylisted = ref 0.

    let accepted = ref 0.

    let running = ref 0.

    let disconnected = ref 0.

    let zero () =
      trusted := 0. ;
      greylisted := 0. ;
      accepted := 0. ;
      running := 0. ;
      disconnected := 0.
  end

  let trusted_points =
    metric
      ~help:"Number of trusted points"
      ~component
      ~name:"trusted"
      (fun () -> !Stats.trusted)

  let greylisted_points =
    metric
      ~help:"Number of greylisted points"
      ~component
      ~name:"greylisted"
      (fun () -> !Stats.greylisted)

  let accepted_points =
    metric
      ~help:"Number of accepted points"
      ~component
      ~name:"accepted"
      (fun () -> !Stats.accepted)

  let running_points =
    metric
      ~help:"Number of running points"
      ~component
      ~name:"running"
      (fun () -> !Stats.running)

  let disconnected_points =
    metric
      ~help:"Number of disconnected points"
      ~component
      ~name:"disconnected"
      (fun () -> !Stats.disconnected)

  let metrics =
    [
      trusted_points;
      greylisted_points;
      accepted_points;
      running_points;
      disconnected_points;
    ]

  let () = List.iter add_metric metrics

  let collect pool =
    Stats.zero () ;
    P2p_pool.Points.iter_known
      (fun _ info ->
        if P2p_point_state.is_accepted info then
          Stats.accepted := !Stats.accepted +. 1.
        else if P2p_point_state.is_disconnected info then
          Stats.disconnected := !Stats.disconnected +. 1.
        else if P2p_point_state.is_running info then
          Stats.running := !Stats.running +. 1. ;
        match P2p_point_state.Info.reconnection_time info with
        | Some _ -> Stats.greylisted := !Stats.greylisted +. 1.
        | _ -> ())
      pool
end

module Messages = struct
  let component = "messages"

  let metric_counter = metric_counter ~component

  let broadcast_message_sent =
    metric_counter
      ~help:"Number of user message sent by broadcasting"
      ~name:"broadcast_message_sent"
      ()

  let user_message_sent =
    metric_counter
      ~help:"Number of user message sent"
      ~name:"user_message_sent"
      ()

  let user_message_received =
    metric_counter
      ~help:"Number of user message received"
      ~name:"user_message_received"
      ()

  let user_message_received_error =
    metric_counter
      ~help:"Number of user message received that resulted in error"
      ~name:"user_message_received_error"
      ()

  let advertise_received =
    metric_counter
      ~help:"Number of advertise received"
      ~name:"advertise_received"
      ()

  let advertise_sent =
    metric_counter ~help:"Number of advertise sent" ~name:"advertise_sent" ()

  let bootstrap_received =
    metric_counter
      ~help:"Number of bootstrap received"
      ~name:"bootstrap_received"
      ()

  let bootstrap_sent =
    metric_counter ~help:"Number of bootstrap sent" ~name:"bootstrap_sent" ()

  let swap_request_sent =
    metric_counter ~help:"Number of swap sent" ~name:"swap_requests_sent" ()

  let swap_request_received =
    metric_counter
      ~help:"Number of swap received"
      ~name:"swap_requests_received"
      ()

  let swap_ack_sent =
    metric_counter ~help:"Number of swap acks sent" ~name:"swap_ack_sent" ()

  let swap_ack_received =
    metric_counter
      ~help:"Number of swap acks received"
      ~name:"swap_ack_received"
      ()
end

module Swap = struct
  let component = "swap"

  let metric_counter = metric_counter ~component

  let ignored = metric_counter ~help:"Number of ignored swap" ~name:"ignored" ()

  let success =
    metric_counter ~help:"Number of successful swap" ~name:"success" ()

  let fail = metric_counter ~help:"Number of failed swap" ~name:"fail" ()
end

let collect pool =
  Prometheus.CollectorRegistry.(register_pre_collect default) (fun () ->
      Connections.collect pool ;
      Peers.collect pool ;
      Points.collect pool)
