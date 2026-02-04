(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Functori,     <contact@functori.com>                   *)
(*                                                                           *)
(*****************************************************************************)

(** Update the DAL metrics counter when a shard is stored. *)
val shard_stored : unit -> unit

(** Update the DAL metrics counter when a reconstruction is started. *)
val reconstruction_started : unit -> unit

(** Update the DAL metrics counter when a reconstruction is done. *)
val reconstruction_done : unit -> unit

(** Update the DAL metrics counter of enqueued reconstruction tasks *)
val update_amplification_queue_length : int -> unit

(** Update a DAL metrics timing when a whole amplification is done. *)
val update_amplification_complete_duration : float -> unit

(** Update the DAL metrics counter when a reconstruction is aborted because
    enough shards have been received during the random delay. *)
val reconstruction_aborted : unit -> unit

(** Update the "waiting_for_attestation" status of the given slot index in the
    metrics. The value is set to 1 if [set] is true, and -1 otherwise. *)
val slot_waiting_for_attestation : set:bool -> int -> unit

(** Update the "attestation" status of the given slot index in the metrics. The
    value is set to 1 if [set] is true, and -1 otherwise. *)
val slot_attested : set:bool -> int -> unit

(** Increment the count of unattested slots. *)
val slot_unattested : int -> unit

(** Update attestation lag histogram when a slot is attested.
    Records the lag value in a histogram for distribution analysis.
    The metric name is [dal_node_attestation_lag]. *)
val slot_attested_with_lag : lag:int -> unit

(** Update the "attestation" ratio for the baker *)
val attested_slots_for_baker_per_level_ratio :
  delegate:Signature.Public_key_hash.t -> float -> unit

(** Update the seen layer1 heads with the given value. *)
val new_layer1_head : head_level:int32 -> unit

(** A new layer1 head with the given round was seen. *)
val new_layer1_head_round : head_round:int32 -> unit

(** Update the finalized layer1 blocks with the given value. *)
val layer1_block_finalized : block_level:int32 -> unit

(** A new layer1 block with the given round was finalized. *)
val layer1_block_finalized_round : block_round:int32 -> unit

(** Update the shards verification time with the given value.  *)
val update_shards_verification_time : float -> unit

(** Update the KVS shards metrics. *)
val update_kvs_shards_metrics : opened_files:int -> ongoing_actions:int -> unit

(** Update the DAL metrics timing value when enough of all the shards are
    received. *)
val update_amplification_enough_shards_received_duration : float -> unit

(** Update the DAL metrics timing value when all the shards are received. *)
val update_amplification_all_shards_received_duration : float -> unit

(** Add a DAL metrics timing value when a reconstruction is started. *)
val update_amplification_start_reconstruction_duration : float -> unit

(** Add a the DAL metrics timing value when a reconstruction is aborted. *)
val update_amplification_abort_reconstruction_duration : float -> unit

val per_level_processing_time : float -> unit

(** [sample_time ~sampling_frequency ~to_sample ~metric_updater] samples
    execution time of function [to_sample] at frequency
    [sampling_frequency]. Execution time if any is then provided to
    [metric_updater]. *)
val sample_time :
  sampling_frequency:int ->
  to_sample:(unit -> 'a) ->
  metric_updater:(float -> unit) ->
  'a

(* Stores metrics about reception and validation of shards for a slot *)
type slot_metrics = {
  time_first_shard_received : float;
  duration_all_shards_received : float option;
  duration_first_shard_validated : float option;
  duration_enough_shards_validated : float option;
  duration_all_shards_validated : float option;
}

val slot_metrics_encoding : slot_metrics Data_encoding.t

val pp_slot_metrics : Format.formatter -> slot_metrics -> unit

(** [pp_slot_metrics_received] only prints the time and duration to receive the
    p2p messages, omitting completely the information about validation time.*)
val pp_slot_metrics_received : Format.formatter -> slot_metrics -> unit

(* Stores the [slot_metric] for the slots *)
module Slot_id_bounded_map : Vache.MAP with type key = Types.Slot_id.t

(** [collect_gossipsub_metrics gs_worker] allows to periodically collect metrics
    from the given GS Worker state. *)
val collect_gossipsub_metrics : Gossipsub.Worker.t -> unit

(** [update_timing_shard_received shards_timing_table  ~last_expected_shards slot_id]
    updates the timing metrics associated with [slot_id] in the [shards_timing_table].

    This function should be called each time a shard is received. It records the
    timestamp of the first shard received for a slot, and updates the duration
    when all shards are received.
    The [last_expected_shard] boolean indicates that this shard is the last
    expected one, meaning that if all shards received until now are valid, then
    the slot is attestable (in attester mode) / we have all the
    [cryptobox_parameters.number_of_shards] shards for the slot (in observer mode).

    This function outputs the up-to-date [slot_metrics] with a [boolean] stating
    if it has been updated or not.

    No I/O is performed; the function only modifies the in-memory timing table.
*)
val update_timing_shard_received :
  slot_metrics Slot_id_bounded_map.t ->
  last_expected_shard:bool ->
  Slot_id_bounded_map.key ->
  bool * slot_metrics

(** [update_timing_shard_validated shards_timing_table  ~number_of_already_stored_shards
    ~number_of_expected_shards ?min_shards_to_reconstruct_slot slot_id]
    updates the timing metrics associated with [slot_id] in the [shards_timing_table].

    This function should be called each time a shard is validated.
    This function is very similar to [update_timing_shard_received].
    There are two main differences:
    - Since this function is called after validation, the shard has been added
      to the store, hence this function uses the [number_of_already_stored_shards]
      and the [number_of_expected_shards] to compute itself if it is the last
      expected shard that just got validated.
    - If the [min_shards_to_reconstruct_slot] argument is passed, it also updates
      the time when this number of shards are validated.

    No I/O is performed; the function only modifies the in-memory timing table.
*)
val update_timing_shard_validated :
  slot_metrics Slot_id_bounded_map.t ->
  number_of_already_stored_shards:int ->
  number_of_expected_shards:int ->
  ?min_shards_to_reconstruct_slot:int ->
  Slot_id_bounded_map.key ->
  bool * slot_metrics
