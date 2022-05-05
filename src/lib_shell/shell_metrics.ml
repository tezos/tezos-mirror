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

module Mempool = struct
  open Prometheus

  let component = "mempool_pending"

  type mempool_collectors = {
    mutable applied : unit -> float;
    mutable prechecked : unit -> float;
    mutable refused : unit -> float;
    mutable branch_refused : unit -> float;
    mutable branch_delayed : unit -> float;
    mutable outdated : unit -> float;
    mutable unprocessed : unit -> float;
  }

  let mempool_collectors =
    {
      applied = (fun () -> 0.);
      prechecked = (fun () -> 0.);
      refused = (fun () -> 0.);
      branch_refused = (fun () -> 0.);
      branch_delayed = (fun () -> 0.);
      outdated = (fun () -> 0.);
      unprocessed = (fun () -> 0.);
    }

  let set_applied_collector fn = mempool_collectors.applied <- fn

  let set_prechecked_collector fn = mempool_collectors.prechecked <- fn

  let set_refused_collector fn = mempool_collectors.refused <- fn

  let set_branch_delayed_collector fn = mempool_collectors.branch_delayed <- fn

  let set_branch_refused_collector fn = mempool_collectors.branch_refused <- fn

  let set_outdated_collector fn = mempool_collectors.outdated <- fn

  let set_unprocessed_collector fn = mempool_collectors.unprocessed <- fn

  let () =
    let metric ~help ~name collector =
      let info =
        MetricInfo.
          {
            name = MetricName.v (String.concat "_" [namespace; component; name]);
            metric_type = Gauge;
            help;
            label_names = [];
          }
      in
      let collect () =
        LabelSetMap.singleton [] [Sample_set.sample (collector ())]
      in
      (info, collect)
    in
    let applied =
      metric
        ~help:"Mempool pending applied operations count"
        ~name:"applied"
        (fun () -> mempool_collectors.applied ())
    in
    let prechecked =
      metric
        ~help:"Mempool pending prechecked operations count"
        ~name:"prechecked"
        (fun () -> mempool_collectors.prechecked ())
    in
    let refused =
      metric
        ~help:"Mempool pending refused operations count"
        ~name:"refused"
        (fun () -> mempool_collectors.refused ())
    in
    let branch_refused =
      metric
        ~help:"Mempool pending branch refused operations count"
        ~name:"branch_refused"
        (fun () -> mempool_collectors.branch_refused ())
    in
    let branch_delayed =
      metric
        ~help:"Mempool pending branch delayed operations count"
        ~name:"branch_delayed"
        (fun () -> mempool_collectors.branch_delayed ())
    in
    let outdated =
      metric
        ~help:"Mempool pending outdated operations count"
        ~name:"outdated"
        (fun () -> mempool_collectors.outdated ())
    in
    let unprocessed =
      metric
        ~help:"Mempool pending unprocessed operations count"
        ~name:"unprocessed"
        (fun () -> mempool_collectors.unprocessed ())
    in
    let metrics =
      [
        applied;
        prechecked;
        refused;
        branch_refused;
        branch_delayed;
        outdated;
        unprocessed;
      ]
    in
    let add (info, collector) =
      CollectorRegistry.(register default info collector)
    in
    List.iter add metrics
end

module Worker = struct
  type t = {
    last_finished_request_push_timestamp : Prometheus.Gauge.t;
    last_finished_request_treatment_timestamp : Prometheus.Gauge.t;
    last_finished_request_completion_timestamp : Prometheus.Gauge.t;
  }

  let declare ~label_names ~namespace ?subsystem () =
    let last_finished_request_push_timestamp =
      let help =
        "Reception timestamp of the latest request handled by the worker"
      in
      Prometheus.Gauge.v_labels
        ~label_names
        ~help
        ~namespace
        ?subsystem
        "last_finished_request_push_timestamp"
    in
    let last_finished_request_treatment_timestamp =
      let help =
        "Timestamp at which the worker started processing of the latest \
         request it handled"
      in
      Prometheus.Gauge.v_labels
        ~label_names
        ~help
        ~namespace
        ?subsystem
        "last_finished_request_treatment_timestamp"
    in
    let last_finished_request_completion_timestamp =
      let help =
        "Timestamp at which the latest request handled by the worker was \
         completed"
      in
      Prometheus.Gauge.v_labels
        ~label_names
        ~help
        ~namespace
        ?subsystem
        "last_finished_request_completion_timestamp"
    in
    fun labels ->
      {
        last_finished_request_push_timestamp =
          Prometheus.Gauge.labels last_finished_request_push_timestamp labels;
        last_finished_request_treatment_timestamp =
          Prometheus.Gauge.labels
            last_finished_request_treatment_timestamp
            labels;
        last_finished_request_completion_timestamp =
          Prometheus.Gauge.labels
            last_finished_request_completion_timestamp
            labels;
      }

  let update metrics Worker_types.{pushed; treated; completed} =
    Prometheus.Gauge.set
      metrics.last_finished_request_push_timestamp
      (Ptime.to_float_s pushed) ;
    Prometheus.Gauge.set
      metrics.last_finished_request_treatment_timestamp
      (Ptime.to_float_s treated) ;
    Prometheus.Gauge.set
      metrics.last_finished_request_completion_timestamp
      (Ptime.to_float_s completed)
end

module Distributed_db = struct
  type t = {table_length : Prometheus.Gauge.t}

  let init =
    let subsystem = String.concat "_" ["node"; "distributed_db"; "requester"] in
    let labels = ["requester_kind"; "entry_type"] in
    let table_length =
      let help = "Number of entries (to grab) from the network present" in
      Prometheus.Gauge.v_labels
        ~label_names:labels
        ~help
        ~namespace
        ~subsystem
        "table_length"
    in
    fun ~kind ~entry_type ->
      {table_length = Prometheus.Gauge.labels table_length [kind; entry_type]}

  let update metrics ~length =
    Prometheus.Gauge.set metrics.table_length (float_of_int length)
end

module Block_validator = struct
  open Prometheus

  type block_validator_collectors = {
    mutable operations_per_pass : unit -> float list;
  }

  let block_validator_collectors = {operations_per_pass = (fun () -> [])}

  let set_operation_per_pass_collector fn =
    block_validator_collectors.operations_per_pass <- fn

  type t = {
    already_commited_blocks_count : Counter.t;
    outdated_blocks_count : Counter.t;
    validated_blocks_count : Counter.t;
    validation_errors_count : Counter.t;
    preapplied_blocks_count : Counter.t;
    preapplication_errors_count : Counter.t;
    validation_errors_after_precheck_count : Counter.t;
    precheck_failed_count : Counter.t;
    validation_worker_metrics : Worker.t;
  }

  let init name =
    let subsystem = Some (String.concat "_" name) in
    let already_commited_blocks_count =
      let help = "Number of requests to validate a block already handled" in
      Counter.v ~help ~namespace ?subsystem "already_commited_blocks_count"
    in
    let outdated_blocks_count =
      let help =
        "Number of requests to validate a block older than the node's \
         checkpoint"
      in
      Counter.v ~help ~namespace ?subsystem "outdated_blocks_count"
    in
    let validated_blocks_count =
      let help = "Number of requests to validate a valid block" in
      Counter.v ~help ~namespace ?subsystem "validated_blocks_count"
    in
    let validation_errors_count =
      let help = "Number of requests to validate an invalid block" in
      Counter.v ~help ~namespace ?subsystem "validation_errors_count"
    in
    let preapplied_blocks_count =
      let help = "Number of successful application simulations of blocks" in
      Counter.v ~help ~namespace ?subsystem "preapplied_blocks_count"
    in
    let preapplication_errors_count =
      let help = "Number of refused application simulations of blocks" in
      Counter.v ~help ~namespace ?subsystem "preapplication_errors_count"
    in
    let validation_errors_after_precheck_count =
      let help =
        "Number of requests to validate an invalid but precheckable block"
      in
      Counter.v
        ~help
        ~namespace
        ?subsystem
        "validation_errors_after_precheck_count"
    in
    let precheck_failed_count =
      let help =
        "Number of block validation requests where the prechecking of a block \
         failed"
      in
      Counter.v ~help ~namespace ?subsystem "precheck_failed_count"
    in
    let validation_worker_metrics =
      Worker.declare ~label_names:[] ~namespace ?subsystem () []
    in
    let operations_per_pass_metrics () =
      let info =
        MetricInfo.
          {
            name =
              MetricName.v
                (String.concat
                   "_"
                   ([namespace] @ name @ ["operations_per_pass"]));
            metric_type = Gauge;
            help = "Number of operations per pass for the last validated block";
            label_names = List.map LabelName.v ["pass_id"];
          }
      in
      (* This collector aims to associate a label for each operation
         validation pass. The label name is based on the index of the
         list of operations. *)
      let collector () =
        List.fold_left_i
          (fun i map v ->
            LabelSetMap.add [string_of_int i] [Sample_set.sample v] map)
          LabelSetMap.empty
          (block_validator_collectors.operations_per_pass ())
      in
      CollectorRegistry.(register default info collector)
    in
    operations_per_pass_metrics () ;
    {
      already_commited_blocks_count;
      outdated_blocks_count;
      validated_blocks_count;
      validation_errors_count;
      preapplied_blocks_count;
      preapplication_errors_count;
      validation_errors_after_precheck_count;
      precheck_failed_count;
      validation_worker_metrics;
    }
end

module Proto_plugin = struct
  module type PROTOMETRICS = sig
    val hash : Protocol_hash.t

    val update_metrics :
      protocol_metadata:bytes ->
      (cycle:float -> consumed_gas:float -> unit) ->
      unit
  end

  module UndefinedProtoMetrics (P : sig
    val hash : Protocol_hash.t
  end) =
  struct
    let hash = P.hash

    let update_metrics ~protocol_metadata:_ _ = ()
  end

  let proto_metrics_table : (module PROTOMETRICS) Protocol_hash.Table.t =
    Protocol_hash.Table.create 5

  let register_plugin (module ProtoMetrics : PROTOMETRICS) =
    Protocol_hash.Table.replace
      proto_metrics_table
      ProtoMetrics.hash
      (module ProtoMetrics)

  let find_plugin = Protocol_hash.Table.find proto_metrics_table

  let safe_get_prevalidator_proto_metrics hash =
    match find_plugin hash with
    | Some proto_metrics -> Lwt.return proto_metrics
    | None ->
        let module ProtoMetrics = UndefinedProtoMetrics (struct
          let hash = hash
        end) in
        Lwt.return (module ProtoMetrics : PROTOMETRICS)
end

module Chain_validator = struct
  type t = {
    head_level : Prometheus.Gauge.t;
    ignored_head_count : Prometheus.Counter.t;
    branch_switch_count : Prometheus.Counter.t;
    head_increment_count : Prometheus.Counter.t;
    validation_worker_metrics : Worker.t;
    head_cycle : Prometheus.Gauge.t;
    consumed_gas : Prometheus.Gauge.t;
  }

  let init name =
    let subsystem = Some (String.concat "_" name) in
    let label_name = "chain_id" in
    let head_level =
      let help = "Current level of the node's head" in
      Prometheus.Gauge.v_label
        ~label_name
        ~help
        ~namespace
        ?subsystem
        "head_level"
    in
    let ignored_head_count =
      let help =
        "Number of requests where the chain validator ignored a new valid \
         block with a lower fitness than its current head"
      in
      Prometheus.Counter.v_label
        ~label_name
        ~help
        ~namespace
        ?subsystem
        "ignored_head_count"
    in
    let branch_switch_count =
      let help = "Number of times the chain_validator switched branch" in
      Prometheus.Counter.v_label
        ~label_name
        ~help
        ~namespace
        ?subsystem
        "branch_switch_count"
    in
    let head_increment_count =
      let help =
        "Number of times the chain_validator incremented its head for a direct \
         successor"
      in
      Prometheus.Counter.v_label
        ~label_name
        ~help
        ~namespace
        ?subsystem
        "head_increment_count"
    in
    let head_cycle =
      let help = "Current cycle" in
      Prometheus.Gauge.v_label
        ~label_name
        ~help
        ~namespace
        ?subsystem
        "head_cycle"
    in
    let consumed_gas =
      let help = "Consumed Gas" in
      Prometheus.Gauge.v_label
        ~label_name
        ~help
        ~namespace
        ?subsystem
        "consumed_gas"
    in
    let validation_worker_metrics =
      Worker.declare ~label_names:[label_name] ~namespace ?subsystem ()
    in
    fun chain_id ->
      let label = Chain_id.to_short_b58check chain_id in
      {
        head_level = head_level label;
        ignored_head_count = ignored_head_count label;
        branch_switch_count = branch_switch_count label;
        head_increment_count = head_increment_count label;
        validation_worker_metrics = validation_worker_metrics [label];
        head_cycle = head_cycle label;
        consumed_gas = consumed_gas label;
      }

  let update_ref = ref (fun () -> Lwt.return_unit)

  let update_proto update = update_ref := update

  let pre_collect_lock = Lwt_mutex.create ()

  let pre_collect () =
    Lwt.dont_wait
      (fun () ->
        Lwt_mutex.with_lock pre_collect_lock (fun () -> !update_ref ()))
      (fun _ -> ())

  let () =
    Prometheus.CollectorRegistry.(register_pre_collect default) pre_collect
end

module Version = struct
  let metric =
    Prometheus.Gauge.v_labels
      ~help:"Node version"
      ~namespace
      ~label_names:
        [
          "version";
          "chain_name";
          "distributed_db_version";
          "p2p_version";
          "commit_hash";
          "commit_date";
        ]
      "version"

  let version = Tezos_version.Version.to_string Current_git_info.version

  let network_version net =
    let Network_version.{chain_name; distributed_db_version; p2p_version} =
      P2p.announced_version net
    in
    [
      Format.asprintf "%a" Distributed_db_version.Name.pp chain_name;
      Format.asprintf "%a" Distributed_db_version.pp distributed_db_version;
      Format.asprintf "%a" P2p_version.pp p2p_version;
    ]

  let commit_hash = Current_git_info.commit_hash

  let commit_date = Current_git_info.committer_date

  let init net =
    let _ =
      Prometheus.Gauge.labels metric
      @@ [version] @ network_version net @ [commit_hash; commit_date]
    in
    ()
end
