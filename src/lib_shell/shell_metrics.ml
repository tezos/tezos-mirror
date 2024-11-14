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

let namespace = Tezos_version.Octez_node_version.namespace

module Worker = struct
  open Prometheus

  type timestamps = {
    last_finished_request_push_timestamp : Gauge.t;
    last_finished_request_treatment_timestamp : Gauge.t;
    last_finished_request_completion_timestamp : Gauge.t;
  }

  type counters = {
    worker_request_count : Counter.t;
    worker_completion_count : Counter.t;
    worker_error_count : Counter.t;
  }

  let declare_timestamps ~label_names ~namespace ?subsystem () =
    let last_finished_request_push_timestamp =
      let help =
        "Reception timestamp of the latest request handled by the worker"
      in
      Gauge.v_labels
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
      Gauge.v_labels
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
      Gauge.v_labels
        ~label_names
        ~help
        ~namespace
        ?subsystem
        "last_finished_request_completion_timestamp"
    in
    fun labels ->
      {
        last_finished_request_push_timestamp =
          Gauge.labels last_finished_request_push_timestamp labels;
        last_finished_request_treatment_timestamp =
          Gauge.labels last_finished_request_treatment_timestamp labels;
        last_finished_request_completion_timestamp =
          Gauge.labels last_finished_request_completion_timestamp labels;
      }

  let declare_counters ~label_names ~namespace ?subsystem () =
    let worker_request_count =
      let help = "Number of requests received by the block validator worker" in
      Counter.v_labels
        ~label_names
        ~help
        ~namespace
        ?subsystem
        "worker_request_count"
    in
    let worker_completion_count =
      let help = "Number of requests completed the block validator worker" in
      Counter.v_labels
        ~label_names
        ~help
        ~namespace
        ?subsystem
        "worker_completion_count"
    in
    let worker_error_count =
      let help = "Number of errors encountered by the block validator worker" in
      Counter.v_labels
        ~label_names
        ~help
        ~namespace
        ?subsystem
        "worker_error_count"
    in
    fun labels ->
      {
        worker_request_count = Counter.labels worker_request_count labels;
        worker_completion_count = Counter.labels worker_completion_count labels;
        worker_error_count = Counter.labels worker_error_count labels;
      }

  let update_timestamps metrics Worker_types.{pushed; treated; completed} =
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

module Mempool = struct
  open Prometheus

  type t = {worker_counters : Worker.counters}

  let init name =
    let subsystem = Some (String.concat "_" name) in
    let worker_counters =
      Worker.declare_counters ~label_names:[] ~namespace ?subsystem () []
    in
    {worker_counters}

  let component = "mempool_pending"

  type mempool_collectors = {
    mutable validated : unit -> float;
    mutable refused : unit -> float;
    mutable branch_refused : unit -> float;
    mutable branch_delayed : unit -> float;
    mutable outdated : unit -> float;
    mutable unprocessed : unit -> float;
  }

  let mempool_collectors =
    {
      validated = (fun () -> 0.);
      refused = (fun () -> 0.);
      branch_refused = (fun () -> 0.);
      branch_delayed = (fun () -> 0.);
      outdated = (fun () -> 0.);
      unprocessed = (fun () -> 0.);
    }

  let set_validated_collector fn = mempool_collectors.validated <- fn

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
    let validated =
      metric
        ~help:"Mempool pending validated operations count"
        ~name:"validated"
        (fun () -> mempool_collectors.validated ())
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
        validated; refused; branch_refused; branch_delayed; outdated; unprocessed;
      ]
    in
    let add (info, collector) =
      CollectorRegistry.(register default info collector)
    in
    List.iter add metrics
end

module Distributed_db = struct
  type t = {table_length : Prometheus.Gauge.t}

  let subsystem = "distributed_db"

  let init =
    let subsystem = String.concat "_" [subsystem; "requester"] in
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

  module Messages = struct
    type t = {sent : Prometheus.Counter.t; received : Prometheus.Counter.t}

    type t_broadcasted = {
      sent : Prometheus.Counter.t;
      received : Prometheus.Counter.t;
      broadcasted : Prometheus.Counter.t;
    }

    let subsystem = String.concat "_" [subsystem; "message"]

    let msg_action_counter ~name =
      Prometheus.Counter.v_label
        ~help:("Number of " ^ name ^ " messages")
        ~namespace
        ~subsystem
        ~label_name:"action"
        (String.concat "_" [name; "messages"])

    let sent_label, received_label, broadcasted_label =
      ("sent", "received", "broadcasted")

    let sent_received_counter ~name =
      let metric_of_action = msg_action_counter ~name in
      {
        sent = metric_of_action sent_label;
        received = metric_of_action received_label;
      }

    let sent_received_broadcasted_counter ~name =
      let metric_of_action = msg_action_counter ~name in
      {
        sent = metric_of_action sent_label;
        received = metric_of_action received_label;
        broadcasted = metric_of_action broadcasted_label;
      }

    let get_current_branch = sent_received_counter ~name:"get_current_branch"

    let current_branch = sent_received_counter ~name:"current_branch"

    let deactivate = sent_received_counter ~name:"deactivate"

    let get_current_head =
      sent_received_broadcasted_counter ~name:"get_current_head"

    let current_head = sent_received_broadcasted_counter ~name:"current_head"

    let get_block_headers = sent_received_counter ~name:"get_block_headers"

    let block_header = sent_received_counter ~name:"block_header"

    let get_operations = sent_received_counter ~name:"get_operations"

    let operation = sent_received_counter ~name:"operation"

    let get_protocols = sent_received_counter ~name:"get_protocols"

    let protocol = sent_received_counter ~name:"protocol"

    let get_operations_for_blocks =
      sent_received_counter ~name:"get_operations_for_blocks"

    let operations_for_block =
      sent_received_counter ~name:"operations_for_block"

    let get_checkpoint = sent_received_counter ~name:"get_checkpoint"

    let checkpoint = sent_received_counter ~name:"checkpoint"

    let get_protocol_branch = sent_received_counter ~name:"get_protocol_branch"

    let protocol_branch = sent_received_counter ~name:"protocol_branch"

    let get_predecessor_header =
      sent_received_counter ~name:"get_predecessor_header"

    let predecessor_header = sent_received_counter ~name:"predecessor_header"

    let inc_one action_sr action_srb =
      Distributed_db_message.(
        function
        | Get_current_branch _ ->
            Prometheus.Counter.inc_one (action_sr get_current_branch)
        | Current_branch _ ->
            Prometheus.Counter.inc_one (action_sr current_branch)
        | Deactivate _ -> Prometheus.Counter.inc_one (action_sr deactivate)
        | Get_current_head _ ->
            Prometheus.Counter.inc_one (action_srb get_current_head)
        | Current_head _ -> Prometheus.Counter.inc_one (action_srb current_head)
        | Get_block_headers _ ->
            Prometheus.Counter.inc_one (action_sr get_block_headers)
        | Block_header _ -> Prometheus.Counter.inc_one (action_sr block_header)
        | Get_operations _ ->
            Prometheus.Counter.inc_one (action_sr get_operations)
        | Operation _ -> Prometheus.Counter.inc_one (action_sr operation)
        | Get_protocols _ ->
            Prometheus.Counter.inc_one (action_sr get_protocols)
        | Protocol _ -> Prometheus.Counter.inc_one (action_sr protocol)
        | Get_operations_for_blocks _ ->
            Prometheus.Counter.inc_one (action_sr get_operations_for_blocks)
        | Operations_for_block _ ->
            Prometheus.Counter.inc_one (action_sr operations_for_block)
        | Get_checkpoint _ ->
            Prometheus.Counter.inc_one (action_sr get_checkpoint)
        | Checkpoint _ -> Prometheus.Counter.inc_one (action_sr checkpoint)
        | Get_protocol_branch _ ->
            Prometheus.Counter.inc_one (action_sr get_protocol_branch)
        | Protocol_branch _ ->
            Prometheus.Counter.inc_one (action_sr protocol_branch)
        | Get_predecessor_header _ ->
            Prometheus.Counter.inc_one (action_sr get_predecessor_header)
        | Predecessor_header _ ->
            Prometheus.Counter.inc_one (action_sr predecessor_header))

    let on_sent_msg _conn = inc_one (fun x -> x.sent) (fun x -> x.sent)

    let on_received_msg _conn =
      inc_one (fun x -> x.received) (fun x -> x.received)

    let on_broadcasted_msg _conns ?except:_ ?alt msg =
      let same_msg_type x y =
        Distributed_db_message.(
          match (x, y) with
          | Get_current_branch _, Get_current_branch _ -> true
          | Current_branch _, Current_branch _ -> true
          | Deactivate _, Deactivate _ -> true
          | Get_current_head _, Get_current_head _ -> true
          | Current_head _, Current_head _ -> true
          | Get_block_headers _, Get_block_headers _ -> true
          | Block_header _, Block_header _ -> true
          | Get_operations _, Get_operations _ -> true
          | Operation _, Operation _ -> true
          | Get_protocols _, Get_protocols _ -> true
          | Protocol _, Protocol _ -> true
          | Get_operations_for_blocks _, Get_operations_for_blocks _ -> true
          | Operations_for_block _, Operations_for_block _ -> true
          | Get_checkpoint _, Get_checkpoint _ -> true
          | Checkpoint _, Checkpoint _ -> true
          | Get_protocol_branch _, Get_protocol_branch _ -> true
          | Protocol_branch _, Protocol_branch _ -> true
          | Get_predecessor_header _, Get_predecessor_header _ -> true
          | Predecessor_header _, Predecessor_header _ -> true
          | _, _ -> false)
      in
      let inc_one_broadcasted =
        Distributed_db_message.(
          function
          | Get_current_head _ ->
              Prometheus.Counter.inc_one get_current_head.broadcasted
          | Current_head _ ->
              Prometheus.Counter.inc_one current_head.broadcasted
          | _ -> assert false)
      in
      Option.iter
        (fun (_, then_msg) ->
          if not (same_msg_type msg then_msg) then inc_one_broadcasted then_msg)
        alt ;
      inc_one_broadcasted msg
  end
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
    already_known_invalid_blocks_count : Counter.t;
    validated_blocks_count : Counter.t;
    validation_errors_count : Counter.t;
    commit_block_failed_count : Counter.t;
    preapplied_blocks_count : Counter.t;
    preapplication_errors_count : Counter.t;
    application_errors_after_validation_count : Counter.t;
    validation_failed_count : Counter.t;
    worker_timestamps : Worker.timestamps;
    worker_counters : Worker.counters;
  }

  let init name =
    let subsystem = Some (String.concat "_" name) in
    let already_commited_blocks_count =
      let help = "Number of requests to validate a block already handled" in
      Counter.v ~help ~namespace ?subsystem "already_commited_blocks_count"
    in
    let already_known_invalid_blocks_count =
      let help =
        "Number of requests to validate a block already known as invalid"
      in
      Counter.v ~help ~namespace ?subsystem "already_known_invalid_blocks_count"
    in
    let validated_blocks_count =
      let help = "Number of requests to validate a valid block" in
      Counter.v ~help ~namespace ?subsystem "validated_blocks_count"
    in
    let validation_errors_count =
      let help = "Number of requests to validate an invalid block" in
      Counter.v ~help ~namespace ?subsystem "validation_errors_count"
    in
    let commit_block_failed_count =
      let help = "Number of requests that failed to commit a block" in
      Counter.v ~help ~namespace ?subsystem "commit_block_failed_count"
    in
    let preapplied_blocks_count =
      let help = "Number of successful application simulations of blocks" in
      Counter.v ~help ~namespace ?subsystem "preapplied_blocks_count"
    in
    let preapplication_errors_count =
      let help = "Number of refused application simulations of blocks" in
      Counter.v ~help ~namespace ?subsystem "preapplication_errors_count"
    in
    let application_errors_after_validation_count =
      let help =
        "Number of requests to validate an inapplicable but validated block"
      in
      Counter.v
        ~help
        ~namespace
        ?subsystem
        "application_errors_after_validation_count"
    in
    let validation_failed_count =
      let help =
        "Number of block validation requests where the validation of a block \
         failed"
      in
      Counter.v ~help ~namespace ?subsystem "validation_failed_count"
    in
    let worker_timestamps =
      Worker.declare_timestamps ~label_names:[] ~namespace ?subsystem () []
    in
    let worker_counters =
      Worker.declare_counters ~label_names:[] ~namespace ?subsystem () []
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
      already_known_invalid_blocks_count;
      validated_blocks_count;
      validation_errors_count;
      commit_block_failed_count;
      preapplied_blocks_count;
      preapplication_errors_count;
      application_errors_after_validation_count;
      validation_failed_count;
      worker_timestamps;
      worker_counters;
    }
end

module Chain_validator = struct
  open Prometheus

  type t = {
    head_level : Gauge.t;
    ignored_head_count : Counter.t;
    branch_switch_count : Counter.t;
    head_increment_count : Counter.t;
    head_round : Gauge.t;
    head_cycle : Gauge.t;
    consumed_gas : Gauge.t;
    is_bootstrapped : Gauge.t;
    sync_status : Gauge.t;
    worker_timestamps : Worker.timestamps;
    worker_counters : Worker.counters;
  }

  let update_bootstrapped ~metrics b =
    if b then Gauge.set metrics.is_bootstrapped 1.
    else Gauge.set metrics.is_bootstrapped 0.

  let update_sync_status ~metrics event =
    let open Chain_validator_worker_state in
    match event with
    | Not_synchronised -> Gauge.set metrics.sync_status 0.
    | Synchronised {is_chain_stuck = false} -> Gauge.set metrics.sync_status 1.
    | Synchronised {is_chain_stuck = true} -> Gauge.set metrics.sync_status 2.

  let init name =
    let subsystem = Some (String.concat "_" name) in
    let label_name = "chain_id" in
    let head_level =
      let help = "Level of the current node's head" in
      Gauge.v_label ~label_name ~help ~namespace ?subsystem "head_level"
    in
    let ignored_head_count =
      let help =
        "Number of requests where the chain validator ignored a new valid \
         block with a lower fitness than its current head"
      in
      Counter.v_label
        ~label_name
        ~help
        ~namespace
        ?subsystem
        "ignored_head_count"
    in
    let branch_switch_count =
      let help = "Number of times the chain_validator switched branch" in
      Counter.v_label
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
      Counter.v_label
        ~label_name
        ~help
        ~namespace
        ?subsystem
        "head_increment_count"
    in
    let head_cycle =
      let help = "Cycle of the current node's head" in
      Gauge.v_label ~label_name ~help ~namespace ?subsystem "head_cycle"
    in
    let consumed_gas =
      let help = "Gas consumed in the current node's head" in
      Gauge.v_label ~label_name ~help ~namespace ?subsystem "head_consumed_gas"
    in
    let head_round =
      let help = "Round of the current node's head" in
      Gauge.v_label ~label_name ~help ~namespace ?subsystem "head_round"
    in
    let is_bootstrapped =
      let help = "Returns 1 if the node has bootstrapped, 0 otherwise." in
      Gauge.v_label ~label_name ~help ~namespace ?subsystem "is_bootstrapped"
    in
    let sync_status =
      let help =
        "Returns 0 if the node is unsynchronised, 1 if the node is \
         synchronised, 2 if the node is stuck."
      in
      Gauge.v_label
        ~label_name
        ~help
        ~namespace
        ?subsystem
        "synchronisation_status"
    in
    let worker_timestamps =
      Worker.declare_timestamps
        ~label_names:[label_name]
        ~namespace
        ?subsystem
        ()
    in
    let worker_counters =
      Worker.declare_counters ~label_names:[label_name] ~namespace ?subsystem ()
    in
    fun chain_id ->
      let label = Chain_id.to_short_b58check chain_id in
      {
        head_level = head_level label;
        ignored_head_count = ignored_head_count label;
        branch_switch_count = branch_switch_count label;
        head_increment_count = head_increment_count label;
        head_round = head_round label;
        head_cycle = head_cycle label;
        consumed_gas = consumed_gas label;
        is_bootstrapped = is_bootstrapped label;
        sync_status = sync_status label;
        worker_timestamps = worker_timestamps [label];
        worker_counters = worker_counters [label];
      }

  let update_proto_metrics_callback ~metrics ~cycle ~consumed_gas ~round =
    Gauge.set metrics.head_cycle cycle ;
    Gauge.set metrics.consumed_gas consumed_gas ;
    Gauge.set metrics.head_round round

  let update_ref = ref (fun () -> Lwt.return_unit)

  let update_proto update = update_ref := update

  let pre_collect () = !update_ref ()

  let () = CollectorRegistry.(register_pre_collect_lwt default) pre_collect
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

  let network_version net =
    let Network_version.{chain_name; distributed_db_version; p2p_version} =
      P2p.announced_version net
    in
    [
      Format.asprintf "%a" Distributed_db_version.Name.pp chain_name;
      Format.asprintf "%a" Distributed_db_version.pp distributed_db_version;
      Format.asprintf "%a" P2p_version.pp p2p_version;
    ]

  let init ~version
      ~commit_info:({commit_hash; commit_date} : Octez_node_version.commit_info)
      net =
    let _ =
      Prometheus.Gauge.labels metric
      @@ [version] @ network_version net @ [commit_hash; commit_date]
    in
    ()
end

module Peer_validator = struct
  type t = {
    on_no_request : Prometheus.Counter.t;
    new_head_completed : Prometheus.Counter.t;
    new_branch_completed : Prometheus.Counter.t;
    invalid_locator : Prometheus.Counter.t;
    invalid_block : Prometheus.Counter.t;
    system_error : Prometheus.Counter.t;
    unavailable_protocol : Prometheus.Counter.t;
    unknown_ancestor : Prometheus.Counter.t;
    too_short_locator : Prometheus.Counter.t;
    operations_fetching_canceled_new_branch : Prometheus.Counter.t;
    operations_fetching_canceled_new_known_valid_head : Prometheus.Counter.t;
    operations_fetching_canceled_new_unknown_head : Prometheus.Counter.t;
    unknown_error : Prometheus.Counter.t;
    connections : Prometheus.Counter.t;
  }

  let init name =
    let subsystem = String.concat "_" name in
    let on_no_request =
      let help =
        "Number of time we did no hear new messages from a peer since the last \
         timeout."
      in
      Prometheus.Counter.v ~help ~namespace ~subsystem "on_no_request_count"
    in
    let new_head_completed =
      let help =
        "Number of time we successfuly completed a new head request from a \
         peer."
      in
      Prometheus.Counter.v ~help ~namespace ~subsystem "new_head_completed"
    in
    let new_branch_completed =
      let help =
        "Number of time we successfuly completed a new branch request from a \
         peer."
      in
      Prometheus.Counter.v ~help ~namespace ~subsystem "new_branch_completed"
    in
    let invalid_locator =
      let help = "Number of time we received an invalid locator from a peer." in
      Prometheus.Counter.v ~help ~namespace ~subsystem "invalid_locator"
    in
    let invalid_block =
      let help = "Number of time we received an invalid block from a peer." in
      Prometheus.Counter.v ~help ~namespace ~subsystem "invalid_block"
    in
    let system_error =
      let help =
        "Number of time a request trigerred a system error from a peer."
      in
      Prometheus.Counter.v ~help ~namespace ~subsystem "system_error"
    in
    let unavailable_protocol =
      let help =
        "Number of time we received an unknown protocol from a peer."
      in
      Prometheus.Counter.v ~help ~namespace ~subsystem "unavailable_protocol"
    in
    let unknown_ancestor =
      let help =
        "Number of time we received a locator with an unknown ancestor from a \
         peer."
      in
      Prometheus.Counter.v ~help ~namespace ~subsystem "unknown_ancestor"
    in
    let too_short_locator =
      let help =
        "Number of time we received a too short locator from a peer."
      in
      Prometheus.Counter.v ~help ~namespace ~subsystem "too_short_locator"
    in
    let operations_fetching_canceled_new_branch =
      let help =
        "Number of time we canceled the fetching of operations on a new branch \
         request for a peer."
      in
      Prometheus.Counter.v
        ~help
        ~namespace
        ~subsystem
        "operations_fetching_canceled_new_branch"
    in
    let operations_fetching_canceled_new_known_valid_head =
      let help =
        "Number of time we canceled the fetching of operations on a new head \
         request for a peer."
      in
      Prometheus.Counter.v
        ~help
        ~namespace
        ~subsystem
        "operations_fetching_canceled_new_known_valid_head"
    in
    let operations_fetching_canceled_new_unknown_head =
      let help =
        "Number of time we canceled the fetching of operations on a new head \
         request or an unknown head for a peer."
      in
      Prometheus.Counter.v
        ~help
        ~namespace
        ~subsystem
        "operations_fetching_canceled_new_unknown_head"
    in
    let unknown_error =
      let help = "Number of time an unknown error happened for a peer." in
      Prometheus.Counter.v ~help ~namespace ~subsystem "unknown_error"
    in
    let connections =
      let help = "Number of time we connected to a peer." in
      Prometheus.Counter.v ~help ~namespace ~subsystem "connections"
    in
    {
      on_no_request;
      new_head_completed;
      new_branch_completed;
      invalid_locator;
      invalid_block;
      system_error;
      unavailable_protocol;
      unknown_ancestor;
      too_short_locator;
      operations_fetching_canceled_new_branch;
      operations_fetching_canceled_new_known_valid_head;
      operations_fetching_canceled_new_unknown_head;
      unknown_error;
      connections;
    }
end
