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

module Block_validator = struct
  type t = {
    already_commited_blocks_count : Prometheus.Counter.t;
    outdated_blocks_count : Prometheus.Counter.t;
    validated_blocks_count : Prometheus.Counter.t;
    validation_errors_count : Prometheus.Counter.t;
    preapplied_blocks_count : Prometheus.Counter.t;
    preapplication_errors_count : Prometheus.Counter.t;
    validation_errors_after_precheck_count : Prometheus.Counter.t;
    precheck_failed_count : Prometheus.Counter.t;
    validation_worker_metrics : Worker.t;
  }

  let init name =
    let namespace = String.concat "_" name in
    let subsystem = None in
    let already_commited_blocks_count =
      let help = "Number of requests to validate a block already handled" in
      Prometheus.Counter.v
        ~help
        ~namespace
        ?subsystem
        "already_commited_blocks_count"
    in
    let outdated_blocks_count =
      let help =
        "Number of requests to validate a block older than the node's \
         checkpoint"
      in
      Prometheus.Counter.v ~help ~namespace ?subsystem "outdated_blocks_count"
    in
    let validated_blocks_count =
      let help = "Number of requests to validate a valid block" in
      Prometheus.Counter.v ~help ~namespace ?subsystem "validated_blocks_count"
    in
    let validation_errors_count =
      let help = "Number of requests to validate an invalid block" in
      Prometheus.Counter.v ~help ~namespace ?subsystem "validation_errors_count"
    in
    let preapplied_blocks_count =
      let help = "Number of successful application simulations of blocks" in
      Prometheus.Counter.v ~help ~namespace ?subsystem "preapplied_blocks_count"
    in
    let preapplication_errors_count =
      let help = "Number of refused application simulations of blocks" in
      Prometheus.Counter.v
        ~help
        ~namespace
        ?subsystem
        "preapplication_errors_count"
    in
    let validation_errors_after_precheck_count =
      let help =
        "Number of requests to validate an invalid but precheckable block"
      in
      Prometheus.Counter.v
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
      Prometheus.Counter.v ~help ~namespace ?subsystem "precheck_failed_count"
    in
    let validation_worker_metrics =
      Worker.declare ~label_names:[] ~namespace ?subsystem () []
    in
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

module Chain_validator = struct
  type t = {
    head_level : Prometheus.Gauge.t;
    ignored_head_count : Prometheus.Counter.t;
    branch_switch_count : Prometheus.Counter.t;
    head_increment_count : Prometheus.Counter.t;
    validation_worker_metrics : Worker.t;
  }

  let init name =
    let namespace = String.concat "_" name in
    let subsystem = None in
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
      }
end
