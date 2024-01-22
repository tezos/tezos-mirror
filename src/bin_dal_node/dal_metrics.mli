(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Functori,     <contact@functori.com>                   *)
(*                                                                           *)
(*****************************************************************************)

(** Update the DAL metrics counter when a shard is stored. *)
val shard_stored : unit -> unit

(** Update the "waiting_for_attestation" status of the given slot index in the
    metrics. The value is set to 1 if [set] is true, and -1 otherwise. *)
val slot_waiting_for_attestation : set:bool -> int -> unit

(** Update the "attestation" status of the given slot index in the metrics. The
    value is set to 1 if [set] is true, and -1 otherwise. *)
val slot_attested : set:bool -> int -> unit

(** Update the seen layer1 heads with the given value. *)
val new_layer1_head : head_level:int32 -> unit

(** Update the finalized layer1 blocks with the given value. *)
val layer1_block_finalized : block_level:int32 -> unit

(** Update the shards verification time with the given value.  *)
val update_shards_verification_time : float -> unit

(** [sample_time ~sampling_frequency ~to_sample ~metric_updater] samples
    execution time of function [to_sample] at frequency
    [sampling_frequency]. Execution time if any is then provided to
    [metric_updater]. *)
val sample_time :
  sampling_frequency:int ->
  to_sample:(unit -> 'a) ->
  metric_updater:(float -> unit) ->
  'a

(** [collect_gossipsub_metrics gs_worker] allows to periodically collect metrics
    from the given GS Worker state. *)
val collect_gossipsub_metrics : Gossipsub.Worker.t -> unit
