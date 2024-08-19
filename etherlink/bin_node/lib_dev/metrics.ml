(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Prometheus

(* The registry and the callback needs to be redefined because
   Prometheus always use the {!CollectRegistry.default}. However, if
   we use the default one, we link the metrics of the octez-node, the
   solution is to create a new registry and implement the callback
   that serves this new registry specifically. *)

let registry = CollectorRegistry.create ()

module Cohttp (Server : Cohttp_lwt.S.Server) = struct
  let callback _conn req _body =
    let open Cohttp in
    let open Lwt_syntax in
    let uri = Request.uri req in
    match (Request.meth req, Uri.path uri) with
    | `GET, "/metrics" ->
        let* data = CollectorRegistry.collect registry in
        let body =
          Fmt.to_to_string Prometheus_app.TextFormat_0_0_4.output data
        in
        let headers =
          Header.init_with "Content-Type" "text/plain; version=0.0.4"
        in
        Server.respond_string ~status:`OK ~headers ~body ()
    | _ -> Server.respond_error ~status:`Bad_request ~body:"Bad request" ()
end

module Metrics_server = Cohttp (Cohttp_lwt_unix.Server)

let namespace = Tezos_version.Octez_node_version.namespace

let subsystem = "evm_node"

module Chain = struct
  type t = {head : Gauge.t; confirmed_head : Gauge.t}

  let init name =
    let head =
      Gauge.v_label
        ~registry
        ~label_name:"head"
        ~help:"Level of the node's head"
        ~namespace
        ~subsystem
        "head"
        name
    in
    let confirmed_head =
      Gauge.v_label
        ~registry
        ~label_name:"confirmed_head"
        ~help:"Confirmed level (smart rollup node's head)"
        ~namespace
        ~subsystem
        "confirmed_head"
        name
    in
    {head; confirmed_head}
end

module Info = struct
  let init ~mode =
    let commit_hash =
      Tezos_version_value.Current_git_info.abbreviated_commit_hash
    in
    let commit_date = Tezos_version_value.Current_git_info.committer_date in
    let metric =
      Gauge.v_labels
        ~registry
        ~help:"Information"
        ~namespace
        ~subsystem
        ~label_names:["commit_hash"; "commit_date"; "mode"]
        "info"
    in
    ignore (Gauge.labels metric [commit_hash; commit_date; mode])
end

module Block = struct
  type t = {time_processed : Counter.t; transactions : Counter.t}

  module Process_time_histogram = Histogram (struct
    let spec = Histogram_spec.of_list [0.1; 0.1; 0.5; 1.; 2.; 5.; 10.]
  end)

  let process_time_histogram =
    Process_time_histogram.v
      ~registry
      ~namespace
      ~subsystem
      ~help:"The time the EVM node spent processing a block"
      "block_process_time_histogram"

  let init name =
    let time_processed =
      Counter.v_label
        ~registry
        ~label_name:"time_processed"
        ~help:"Time to process the blocks"
        ~namespace
        ~subsystem
        "time_processed"
        name
    in
    let transactions =
      Counter.v_label
        ~registry
        ~label_name:"transactions"
        ~help:"Number of transactions in the blocks"
        ~namespace
        ~subsystem
        "transactions"
        name
    in
    {time_processed; transactions}
end

module Rpc = struct
  let metrics =
    Prometheus.Summary.v_labels
      ~registry
      ~label_names:["endpoint"; "method"]
      ~help:"RPC endpoint call counts and sum of execution times."
      ~namespace
      ~subsystem
      "calls"

  let method_ =
    Prometheus.Counter.v_label
      ~registry
      ~label_name:"method"
      ~help:"Method call counts"
      ~namespace
      ~subsystem
      "calls_method"
end

module Tx_pool = struct
  type size_info = {number_of_addresses : int; number_of_transactions : int}

  let register (tx_pool_size_info : unit -> size_info tzresult Lwt.t) =
    CollectorRegistry.register_lwt
      registry
      MetricInfo.
        {
          name =
            MetricName.v (String.concat "_" [namespace; subsystem; "tx_pool"]);
          help = "Metrics about transaction pool content";
          metric_type = Gauge;
          label_names = [LabelName.v "kind"];
        }
      (fun () ->
        let open Lwt_syntax in
        let+ size_info = tx_pool_size_info () in
        let number_of_addresses, number_of_transactions =
          match size_info with
          | Ok {number_of_addresses; number_of_transactions} ->
              ( Int.to_float number_of_addresses,
                Int.to_float number_of_transactions )
          | Error _ -> (0., 0.)
        in
        LabelSetMap.of_list
          [
            ( ["number_of_addresses"],
              [Prometheus.Sample_set.sample number_of_addresses] );
            ( ["number_of_transactions"],
              [Prometheus.Sample_set.sample number_of_transactions] );
          ])
end

type t = {chain : Chain.t; block : Block.t}

let metrics =
  let name = "Etherlink" in
  let chain = Chain.init name in
  let block = Block.init name in
  {chain; block}

let init ~mode ~tx_pool_size_info =
  Info.init ~mode ;
  Tx_pool.register tx_pool_size_info

let set_level ~level = Gauge.set metrics.chain.head (Z.to_float level)

let set_confirmed_level ~level =
  Gauge.set metrics.chain.confirmed_head (Z.to_float level)

let set_block ~time_processed ~transactions =
  let pt = Ptime.Span.to_float_s time_processed in
  Block.(Process_time_histogram.(observe process_time_histogram pt)) ;
  Counter.inc metrics.block.time_processed pt ;
  Counter.inc metrics.block.transactions (Int.to_float transactions)
