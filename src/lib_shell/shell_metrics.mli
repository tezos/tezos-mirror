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

module Worker : sig
  type timestamps

  type counters = {
    worker_request_count : Prometheus.Counter.t;
    worker_completion_count : Prometheus.Counter.t;
    worker_error_count : Prometheus.Counter.t;
  }

  val update_timestamps : timestamps -> Worker_types.request_status -> unit
end

(** Metrics associated to the mempool *)
module Mempool : sig
  type t = {worker_counters : Worker.counters}

  val init : string list -> t

  val set_applied_collector : (unit -> float) -> unit

  val set_prechecked_collector : (unit -> float) -> unit

  val set_refused_collector : (unit -> float) -> unit

  val set_branch_refused_collector : (unit -> float) -> unit

  val set_branch_delayed_collector : (unit -> float) -> unit

  val set_outdated_collector : (unit -> float) -> unit

  val set_unprocessed_collector : (unit -> float) -> unit
end

module Distributed_db : sig
  type t = {table_length : Prometheus.Gauge.t}

  val init : kind:string -> entry_type:string -> t

  val update : t -> length:int -> unit
end

module Block_validator : sig
  type t = {
    already_commited_blocks_count : Prometheus.Counter.t;
    outdated_blocks_count : Prometheus.Counter.t;
    validated_blocks_count : Prometheus.Counter.t;
    validation_errors_count : Prometheus.Counter.t;
    preapplied_blocks_count : Prometheus.Counter.t;
    preapplication_errors_count : Prometheus.Counter.t;
    validation_errors_after_precheck_count : Prometheus.Counter.t;
    precheck_failed_count : Prometheus.Counter.t;
    worker_timestamps : Worker.timestamps;
    worker_counters : Worker.counters;
  }

  val init : string list -> t

  val set_operation_per_pass_collector : (unit -> float list) -> unit
end

module Chain_validator : sig
  type t = {
    head_level : Prometheus.Gauge.t;
    ignored_head_count : Prometheus.Counter.t;
    branch_switch_count : Prometheus.Counter.t;
    head_increment_count : Prometheus.Counter.t;
    head_round : Prometheus.Gauge.t;
    head_cycle : Prometheus.Gauge.t;
    consumed_gas : Prometheus.Gauge.t;
    is_bootstrapped : Prometheus.Gauge.t;
    sync_status : Prometheus.Gauge.t;
    worker_timestamps : Worker.timestamps;
    worker_counters : Worker.counters;
  }

  val update_bootstrapped : metrics:t -> bool -> unit

  val update_sync_status :
    metrics:t -> Chain_validator_worker_state.synchronisation_status -> unit

  val init : string list -> Chain_id.t -> t

  val update_proto_metrics_callback :
    metrics:t -> cycle:float -> consumed_gas:float -> round:float -> unit

  val update_proto : (unit -> unit Lwt.t) -> unit
end

module Version : sig
  val init :
    version:string ->
    commit_info:Node_version.commit_info ->
    ('a, 'b, 'c) P2p.t ->
    unit
end

module Peer_validator : sig
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

  val init : string list -> t
end
