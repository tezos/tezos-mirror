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

module Counter = struct
  include Counter

  type t = Counter.t

  let event_assertion_warning =
    Internal_event.Simple.declare_2
      ~section:["evm_node"; "dev"; "metrics"]
      ~name:"counter_inc_assertion_warning"
      ~msg:
        "assertion failed while updating a Counter metric: the increment \
         ({increment} occuring at {label}) is negative; the value will not be \
         updated"
      ~level:Warning
      ("increment", Data_encoding.float)
      ("label", Data_encoding.string)

  let inc t v label =
    if v >= 0.0 then inc t v
    else
      Lwt.dont_wait
        (fun () ->
          Internal_event.Simple.emit event_assertion_warning (v, label))
        (Fun.const ())
end

let registry = CollectorRegistry.create ()

type metrics_body = {body : string; content_type : string}

let get_metrics () =
  let open Lwt_syntax in
  let+ data = CollectorRegistry.collect registry in
  let body = Fmt.to_to_string Prometheus_app.TextFormat_0_0_4.output data in
  {body; content_type = "text/plain; version=0.0.4"}

module Cohttp (Server : Cohttp_lwt.S.Server) = struct
  let callback _conn req _body =
    let open Cohttp in
    let open Lwt_syntax in
    let uri = Request.uri req in
    match (Request.meth req, Uri.path uri) with
    | `GET, "/metrics" ->
        let* {body; content_type} = get_metrics () in
        let headers = Header.init_with "Content-Type" content_type in
        Server.respond_string ~status:`OK ~headers ~body ()
    | _ -> Server.respond_error ~status:`Bad_request ~body:"Bad request" ()
end

module Metrics_server = Cohttp (Cohttp_lwt_unix.Server)

let namespace = Tezos_version.Octez_node_version.namespace

let subsystem = "evm_node"

module Health = struct
  type t = {bootstrapping : Gauge.t; pruning : Gauge.t}

  let init name =
    let bootstrapping =
      Gauge.v_label
        ~registry
        ~label_name:"bootstrapping"
        ~help:"1.0 if the EVM node is catching up with its upstream EVM node"
        ~namespace
        ~subsystem
        "bootstrapping"
        name
    in
    let pruning =
      Gauge.v_label
        ~registry
        ~label_name:"pruning_history"
        ~help:
          "1.0 if the EVM node is currently pruning data past its retention \
           period"
        ~namespace
        ~subsystem
        "pruning_history"
        name
    in
    {bootstrapping; pruning}
end

module Chain = struct
  type t = {head : Gauge.t; confirmed_head : Gauge.t; gas_price : Gauge.t}

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
    let gas_price =
      Gauge.v_label
        ~registry
        ~label_name:"gas_price"
        ~help:"Gas price"
        ~namespace
        ~subsystem
        "gas_price"
        name
    in
    {head; confirmed_head; gas_price}
end

module Info = struct
  let init ~mode ~smart_rollup_address =
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
        ~label_names:
          ["commit_hash"; "commit_date"; "mode"; "smart_rollup_address"]
        "info"
    in
    ignore
      (Gauge.labels
         metric
         [
           commit_hash;
           commit_date;
           mode;
           Tezos_crypto.Hashed.Smart_rollup_address.to_b58check
             smart_rollup_address;
         ])
end

module Block = struct
  type t = {time_processed : Counter.t; transactions : Counter.t}

  module Process_time_histogram = Histogram (struct
    let spec = Histogram_spec.of_list [0.1; 0.5; 1.; 2.; 5.; 10.]
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
    Summary.v_labels
      ~registry
      ~label_names:["endpoint"; "method"]
      ~help:"RPC endpoint call counts and sum of execution times."
      ~namespace
      ~subsystem
      "calls"

  let method_ =
    Counter.v_label
      ~registry
      ~label_name:"method"
      ~help:"Method call counts"
      ~namespace
      ~subsystem
      "calls_method"

  let update_metrics uri meth =
    Summary.(time (labels metrics [uri; meth]) Sys.time)
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
            (["number_of_addresses"], [Sample_set.sample number_of_addresses]);
            ( ["number_of_transactions"],
              [Sample_set.sample number_of_transactions] );
          ])
end

module Simulation = struct
  type t = {
    inconsistent_da_fees : Counter.t;
    confirm_gas_needed : Counter.t;
    time_waiting : Counter.t;
    queue_size : Gauge.t;
  }

  let init name =
    let inconsistent_da_fees =
      Counter.v_label
        ~registry
        ~label_name:"inconsistent_da_fees"
        ~help:"Node DA fees are inconsistent with kernel ones"
        ~namespace
        ~subsystem
        "inconsistent_da_fees"
        name
    in
    let confirm_gas_needed =
      Counter.v_label
        ~registry
        ~label_name:"confirm_gas_needed"
        ~help:"Initially provided gas was not enough, confirmation was needed"
        ~namespace
        ~subsystem
        "confirm_gas_needed"
        name
    in
    let time_waiting =
      Counter.v_label
        ~registry
        ~label_name:"time_waiting"
        ~help:"Time spent by a request waiting for a worker (in picosecond)."
        ~namespace
        ~subsystem
        "time_waiting"
        name
    in
    let queue_size =
      Gauge.v_label
        ~registry
        ~label_name:"queue_size"
        ~help:"Size of the execution queue of simulations"
        ~namespace
        ~subsystem
        "queue_size"
        name
    in
    {inconsistent_da_fees; confirm_gas_needed; time_waiting; queue_size}
end

module Evm = struct
  type t = {host_funs : Counter.t String.Hashtbl.t}

  let init () = {host_funs = String.Hashtbl.create 13}

  let host_counter =
    Counter.v_label
      ~registry
      ~label_name:"host_function"
      ~help:"Number of calls made to host functions"
      ~namespace
      ~subsystem
      "host_function_calls"

  let inc_host_function_call {host_funs} function_name =
    let counter =
      match String.Hashtbl.find_opt host_funs function_name with
      | None ->
          let counter = host_counter function_name in
          String.Hashtbl.replace host_funs function_name counter ;
          counter
      | Some c -> c
    in
    Counter.inc_one counter
end

type t = {
  chain : Chain.t;
  block : Block.t;
  simulation : Simulation.t;
  health : Health.t;
  l1_level : Gauge.t;
  evm : Evm.t;
}

module Blueprint_chunk_sent = struct
  let on_inbox =
    Counter.v
      ~registry
      ~help:"Number of blueprint chunks sent on the shared inbox"
      ~namespace
      ~subsystem
      "blueprint_chunks_sent_on_inbox"

  let on_dal =
    Counter.v
      ~registry
      ~help:"Number of blueprint chunks sent on the DAL"
      ~namespace
      ~subsystem
      "blueprint_chunks_sent_on_dal"
end

module Dal = struct
  let signals_sent =
    Counter.v
      ~registry
      ~help:"Number of DAL import signals sent on the inbox"
      ~namespace
      ~subsystem
      "signals_sent"
end

let metrics =
  let name = "Etherlink" in
  let chain = Chain.init name in
  let block = Block.init name in
  let simulation = Simulation.init name in
  let health = Health.init name in
  let evm = Evm.init () in
  let l1_level =
    Gauge.v_label
      ~registry
      ~label_name:"l1_level"
      ~help:"Last processed L1 block level"
      ~namespace
      ~subsystem
      "l1_level"
      name
  in
  {chain; block; simulation; health; l1_level; evm}

let init ~mode ?tx_pool_size_info ~smart_rollup_address () =
  Info.init ~mode ~smart_rollup_address ;
  Option.iter Tx_pool.register tx_pool_size_info

let set_level ~level = Gauge.set metrics.chain.head (Z.to_float level)

let set_gas_price price = Gauge.set metrics.chain.gas_price (Z.to_float price)

let set_confirmed_level ~level =
  Gauge.set metrics.chain.confirmed_head (Z.to_float level)

let set_l1_level ~level = Gauge.set metrics.l1_level (Int32.to_float level)

let start_bootstrapping () = Gauge.set metrics.health.bootstrapping 1.

let stop_bootstrapping () = Gauge.set metrics.health.bootstrapping 0.

let start_pruning () = Gauge.set metrics.health.pruning 1.

let stop_pruning () = Gauge.set metrics.health.pruning 0.

let inc_time_waiting span =
  let _, time = Ptime.Span.to_d_ps span in
  Counter.inc
    metrics.simulation.time_waiting
    (Int64.to_float time)
    "inc_time_waiting"

let set_simulation_queue_size size =
  Gauge.set metrics.simulation.queue_size (Int.to_float size)

let is_bootstrapping () =
  (* [bootstrapping] is set to 1.0 when bootstrapping, and 0.0 otherwise. To
     [> 0.5] allows to distinguish between the two states. *)
  Gauge.read metrics.health.bootstrapping > 0.5

let set_block ~time_processed ~transactions =
  let pt = Ptime.Span.to_float_s time_processed in
  Block.(Process_time_histogram.(observe process_time_histogram pt)) ;
  Counter.inc metrics.block.time_processed pt "set_block:inc_time_processed" ;
  Counter.inc
    metrics.block.transactions
    (Int.to_float transactions)
    "set_block:inc_transactions"

let record_signals_sent signals =
  Counter.inc
    Dal.signals_sent
    (Int.to_float @@ List.length signals)
    "record_signals_sent"

let inc_confirm_gas_needed () =
  Counter.inc_one metrics.simulation.confirm_gas_needed

let record_blueprint_chunks_sent_on_dal chunks =
  Counter.inc
    Blueprint_chunk_sent.on_dal
    (Float.of_int (Sequencer_blueprint.nb_chunks chunks))
    "record_blueprint_chunks_sent_on_dal"

let record_blueprint_chunks_sent_on_inbox chunks =
  Counter.inc
    Blueprint_chunk_sent.on_inbox
    (Float.of_int (List.length chunks))
    "record_blueprint_chunks_sent_on_inbox"

let inc_rpc_method ~name = Counter.inc_one (Rpc.method_ name)

let () =
  Wasm_runtime_callbacks.Metrics.set_inc_host_function_call
    (Evm.inc_host_function_call metrics.evm)

module Performance_metrics_config = struct
  open Octez_performance_metrics

  let registry = registry

  let subsystem = "evm_node"

  let directories =
    [
      data_dir_element ~metrics_suffix:"store_sqlite" "store.sqlite";
      data_dir_element ~metrics_suffix:"store_irmin" "store";
      data_dir_element ~metrics_suffix:"wasm" "wasm_2_0_0";
      data_dir_element ~metrics_suffix:"logs" "daily_logs";
    ]
end

module type PERFORMANCE = sig
  val set_stats : data_dir:string -> unit
end

let performance_metrics : (module PERFORMANCE) Lazy.t =
  lazy
    (let module M =
       Octez_performance_metrics.Unix.Make (Performance_metrics_config) in
    (module M : PERFORMANCE))

let listing () =
  let open Lwt_syntax in
  let* support = Octez_performance_metrics.supports_performance_metrics () in
  (* If the host provides the necessary utils, we get performance metrics.
     To list them, we need to force the evaluation of the Performance module. *)
  if support then ignore (Lazy.force performance_metrics) ;
  let+ data = CollectorRegistry.(collect registry) in
  let body = Fmt.to_to_string Prometheus_app.TextFormat_0_0_4.output data in
  let metrics =
    String.split_on_char '\n' body
    |> List.filter (String.starts_with ~prefix:"#")
  in
  String.concat "\n" metrics
