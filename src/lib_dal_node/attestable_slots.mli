(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** [is_slot_attestable_with_traps shards_store traps_fraction pkh
    assigned_shard_indexes slot_id] checks whether the slot identified by [slot_id]
    is attestable for delegate [pkh] with respect to the traps mechanism.

    The function iterates over the delegate’s [assigned_shard_indexes], reads each
    corresponding stored shard share from [shards_store], and evaluates
    [Trap.share_is_trap] on it using [traps_fraction].  *)
val is_slot_attestable_with_traps :
  Store.Shards.t ->
  Q.t ->
  Signature.public_key_hash ->
  int trace ->
  Types.slot_id ->
  (bool, [> Errors.not_found | Errors.other]) result Lwt.t

(** [subscribe ctxt ~pkh] opens a [Resto_directory.Answer] stream that yields
    [Types.slot_id] values whenever a slot becomes attestable for [~pkh]. The stream
    only emits items produced after subscription. *)
val subscribe :
  Node_context.t ->
  pkh:Signature.public_key_hash ->
  Types.slot_id Resto_directory.Answer.stream

(** Let M = migration level (last block of the old protocol). This function
    attempts to determine whether [~published_level] is included in `[M - lag + 1 .. M]`
    (inclusively), where [lag] is the lag at [~published_level].
    In this case, the corresponding attested levels would fall in the new protocol. *)
val published_just_before_migration :
  Node_context.t -> published_level:int32 -> bool tzresult

(** Let M = migration level (last block of the old protocol). This function
    attempts to determine whether [~attested_level] is included in `[M + 1 .. M + lag]`
    (inclusively), where [lag] is the lag at [~attested_level].
    In this case, the corresponding attested levels would fall in the old protocol. *)
val attested_just_after_migration :
  Node_context.t -> attested_level:int32 -> bool tzresult

(** [may_notify ctxt ~slot_id] checks, for each subscribed [pkh], whether all shards
    assigned to [pkh] at the attestation level corresponding to [~slot_id] are available;
    if so, it emits [~slot_id] to that [pkh]’s stream. *)
val may_notify : Node_context.t -> slot_id:Types.slot_id -> unit tzresult Lwt.t
