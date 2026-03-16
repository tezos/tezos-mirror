(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** Cache for tracking which DAL slots have already been attested by each
    delegate, used to prevent a baker from redundantly attesting the same
    slot at multiple lag positions.

    A slot published at [published_level] is eligible for attestation at
    multiple consensus levels (one per lag in [attestation_lags]). Without
    this cache, a baker would include the same slot in every eligible
    attestation operation.

    The cache is a three-level map:
      [delegate_id -> published_level -> slot_index -> slot_attestation]

    where [slot_attestation] records the earliest on-chain attestation seen
    for that (delegate, published_level, slot_index) triple: the level
    at which it was attested ([attested_level]) and the hash(es) of
    the block(s) that included it ([block_hashes]). *)

open Protocol.Alpha_context
open Baking_state_types

type t

module SlotSet : Set.S with type elt = int

(** [create ~attestation_lags ~number_of_slots] creates a new empty cache. *)
val create : attestation_lags:int list -> number_of_slots:int -> t

(** [set_committee t ~level lookup_fn] stores the committee lookup function
    for the given level. The lookup function maps attestation slots to delegate
    public key hashes and is used when extracting attestations from operations
    without receipts. *)
val set_committee :
  t -> level:int32 -> (Slot.t -> Signature.Public_key_hash.t option) -> unit

(** [filter_attestable_slots t ~delegate_id ~published_level ~attestable_slots
    ~head_level ~head_hash ~predecessor_hash] filters out slots that have
    already been attested on-chain from [~attestable_slots]. *)
val filter_attestable_slots :
  t ->
  delegate_id:Delegate_id.t ->
  published_level:int32 ->
  attestable_slots:SlotSet.t ->
  head_level:int32 ->
  head_hash:Block_hash.t ->
  predecessor_hash:Block_hash.t ->
  SlotSet.t tzresult

(** [update_from_proposal t ~attested_level ~block_hash ~predecessor_hash
    ~grandparent ~operations] extracts DAL attestations from the consensus
    operations of a block and records them in the cache. Uses
    [~predecessor_hash] and [~grandparent] to detect whether existing cache
    entries are on the same chain as the new block or on an abandoned fork. *)
val update_from_proposal :
  t ->
  attested_level:int32 ->
  block_hash:Block_hash.t ->
  predecessor_hash:Block_hash.t ->
  grandparent:Block_hash.t ->
  operations:packed_operation list ->
  unit tzresult

module Internal_for_tests : sig
  (** [update_from_attested_slots] is a simplified [update_from_proposal] for
      tests: same interface except [operations] is replaced by
      [attested_slots] (list of (published_level, slot indices)) and a
      [delegate_id]. Updates the cache for that delegate only (call for
      each delegate as needed). *)
  val update_from_attested_slots :
    t ->
    delegate_id:Baking_state_types.Delegate_id.t ->
    attested_level:int32 ->
    block_hash:Block_hash.t ->
    predecessor_hash:Block_hash.t ->
    grandparent:Block_hash.t ->
    attested_slots:(int32 * int list) list ->
    unit
end
