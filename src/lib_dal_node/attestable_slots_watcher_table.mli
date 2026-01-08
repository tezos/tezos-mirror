(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Types

type watcher

(** Returns the stream currently stored in the watcher. *)
val get_stream : watcher -> Attestable_event.t Lwt_watcher.input

(** Returns the number of subscribers to the stream currently stored in the watcher. *)
val get_num_subscribers : watcher -> shard_index

(** Sets the value for the number of subscribers to the stream in the watcher. *)
val set_num_subscribers : watcher -> shard_index -> unit

(** Table where we lazily create a watcher on first subscription for a pkh. When the last
    subscriber calls [shutdown], the watcher is removed from the table to avoid leaks. *)
type t = watcher Signature.Public_key_hash.Table.t

(** [create t ~initial_size] creates an empty table of watchers with an initial
    bucket size of [~initial_size] (which is an initial estimation). *)
val create : initial_size:int -> t

(** [get_or_init t pkh proto_params] returns (creating if absent) the watcher entry for
    [pkh] in [t].*)
val get_or_init :
  t -> Signature.public_key_hash -> proto_parameters option -> watcher

(** [notify_attestable_slot t pkh ~slot_id] pushes an [Attestable_slot] event for [~slot_id]
    to the stream for [pkh], if present. *)
val notify_attestable_slot :
  t -> Signature.public_key_hash -> slot_id:slot_id -> unit

(** [notify_no_shards_assigned t pkh ~attestation_level] pushes a [No_shards_assigned] event for
    [~attestation_level] to the stream for [pkh], if present. *)
val notify_no_shards_assigned :
  t -> Signature.public_key_hash -> attestation_level:level -> unit

(** [notify_slot_has_trap t pkh ~slot_id] pushed a [Slot_has_trap] event for [~slot_id] to the
    stream for [pkh], if present. *)
val notify_slot_has_trap :
  t -> Signature.public_key_hash -> slot_id:slot_id -> unit

(** [notify_backfill_payload t pkh ~backfill_payload] pushes a [Backfill] event for
      [~backfill_payload] to the stream for [pkh], if present. *)
val notify_backfill_payload :
  t ->
  Signature.public_key_hash ->
  backfill_payload:Attestable_event.backfill_payload ->
  unit

(** [remove t pkh] removes the watcher entry for [pkh] from [t] if present. *)
val remove : t -> Signature.public_key_hash -> unit

(** [elements t] returns the current set of pkhs that have an active monitoring
    subscription in [t]. *)
val elements : t -> Signature.public_key_hash Seq.t
