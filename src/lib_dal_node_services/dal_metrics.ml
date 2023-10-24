(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Functori,     <contact@functori.com>                   *)
(*                                                                           *)
(*****************************************************************************)

let namespace = "dal"

module Node_metrics = struct
  let subsystem = "node"

  let number_of_stored_shards =
    let name = "number_of_stored_shards" in
    Prometheus.Counter.v
      ~help:"Number of stored shards (local injection or remote reception)"
      ~namespace
      ~subsystem
      name

  let verify_shard_time =
    let name = "verify_shard_time" in
    Prometheus.DefaultHistogram.v
      ~help:
        "Time taken to verify a shard in seconds (random sample each 100 \
         shards verification)"
      ~namespace
      ~subsystem
      name

  let slots_waiting_for_attestation =
    let name = "slots_waiting_for_attestaion" in
    Prometheus.Gauge.v_label
      ~label_name:"slot_waiting_for_attestaion"
      ~help:
        "The slot at index <i> is waiting for attestation (value is 1) or not \
         (value is 0)"
      ~namespace
      ~subsystem
      name

  let slots_attested =
    let name = "slots_attested" in
    Prometheus.Gauge.v_label
      ~label_name:"slot_attested"
      ~help:"The slot at index <i> is attested (value is 1) or not (value is 0)"
      ~namespace
      ~subsystem
      name
end

let shard_stored () =
  Prometheus.Counter.inc_one Node_metrics.number_of_stored_shards

let slot_waiting_for_attestation ~set i =
  let v = float_of_int @@ if set then 1 else 0 in
  Prometheus.Gauge.set
    (Node_metrics.slots_waiting_for_attestation (string_of_int i))
    v

let slot_attested ~set i =
  let v = float_of_int @@ if set then 1 else 0 in
  Prometheus.Gauge.set (Node_metrics.slots_attested (string_of_int i)) v

let update_shards_verification_time f =
  Prometheus.DefaultHistogram.observe Node_metrics.verify_shard_time f

let sample_time ~sampling_frequency ~to_sample ~metric_updater =
  if sampling_frequency > 0 && Random.int sampling_frequency <> 0 then
    to_sample ()
  else
    let before = Time.System.now () in
    let res = to_sample () in
    let after = Time.System.now () in
    Ptime.diff after before |> Ptime.Span.to_float_s |> metric_updater ;
    res
