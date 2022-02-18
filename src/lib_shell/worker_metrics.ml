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
      "Timestamp at which the worker started processing of the latest request \
       it handled"
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
        Prometheus.Gauge.labels last_finished_request_treatment_timestamp labels;
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
