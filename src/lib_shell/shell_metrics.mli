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
  type t

  val update : t -> Worker_types.request_status -> unit
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
    validation_worker_metrics : Worker.t;
  }

  val init : string trace -> t
end

module Chain_validator : sig
  type t = {
    head_level : Prometheus.Gauge.t;
    ignored_head_count : Prometheus.Counter.t;
    branch_switch_count : Prometheus.Counter.t;
    head_increment_count : Prometheus.Counter.t;
    validation_worker_metrics : Worker.t;
  }

  val init : string trace -> Chain_id.t -> t
end

module Version : sig
  val init : ('a, 'b, 'c) P2p.t -> unit
end
