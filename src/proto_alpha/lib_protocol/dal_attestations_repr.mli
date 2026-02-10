(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Multiple attestations representation for the data-availability layer.

    {1 Overview}

    This module extends {!Dal_attestation_repr} to handle multiple DAL
    attestations at different lags. A lag represents the difference between the
    attested level of a slot and its published level.

    The structure stores a fixed number of attestations, determined by
    [number_of_lags]. Each attestation is indexed by [lag_index] (0-based) and
    is a bitset encoding which slots are attested at a particular level. The
    slots' (published) level is implicit (it is obtained as "attested level"
    minus the lag corresponding to the given lag index), and is not relevant for
    this module.

    {1 Encoding}

    The encoding uses a compact bitset representation that minimizes space
    when attestations are empty:

    {2 Bitset structure}

    The bitset is stored as an integer, with bit positions starting at 0 (LSB).
    The structure is:

    - {b Prefix} ([number_of_lags] bits at positions 0 to [number_of_lags-1]):
    Indicates which attestations are non-empty. Bit [i] corresponds to lag
    index [i]. If bit [i] = 1, the attestation at lag index [i] is non-empty.
    We call this a "prefix" because it logically comes first in the structure,
    even though it occupies the {i lowest} bit positions.

    - {b Data section} (starting at bit position [number_of_lags]): For each
    non-empty attestation (in order from lag index [0] to [number_of_lags-1]),
    exactly [number_of_slots] bits are stored, representing the attested slots.
    Empty attestations are not stored in the data section.

    - {b Empty case}: An empty bitset (value 0) represents the case where all
    attestations are empty.

    {3 Example}

    For [number_of_lags = 4] and [number_of_slots = 32]:
    - Attestation at lag index 0: empty
    - Attestation at lag index 1: slots 1, 5 attested
    - Attestation at lag index 2: empty
    - Attestation at lag index 3: slots 0, 1 attested

    The full bitset in standard binary notation (MSB left, LSB right):

{v
00000000000000000000000000000011 00000000000000000000000000100010 1010
<-------- lag 3 data ----------> <-------- lag 1 data ----------> <-->
         (32 bits)                        (32 bits)               prefix
        slots 0, 1                       slots 1, 5             lags 1, 3
v}

    Bit positions:
    - Prefix: bits 0-3 (rightmost 4 bits), value [0b1010 = 10]
    - Lag 1 data: bits 4-35, with bits 5 and 9 set (slots 1 and 5)
    - Lag 3 data: bits 36-67, with bits 36 and 37 set (slots 0 and 1)
    - Total: 68 bits

    {3 Design considerations}

    An {b alternative encoding} would store the actual length of each non-empty
    attestation (e.g., 5 bits for length, then that many data bits). This would
    save space when attestations have few bits set, at the cost of additional
    complexity. *)

type t = private Bitset.t

(** The size of the encoding is not bounded. However, the size of a DAL
    attestations bitset is checked during validation of an attestation; and
    there is a bound on the size of a generic operation. *)
val encoding : t Data_encoding.t

(** [empty] returns an empty attestation structure where all slots at all lags
    are marked as unavailable. *)
val empty : t

(** [is_empty t] returns [true] if all attestations at all lags are empty. *)
val is_empty : t -> bool

(** [is_empty t] returns [true] if the attestation at [lag_index] is empty. *)
val is_empty_at_lag_index : t -> lag_index:int -> bool

(** [is_attested t ~number_of_slots ~number_of_lags ~lag_index slot_index] returns
    [true] if the attestation at [lag_index] commits that the slot at
    [slot_index] is available. [lag_index] must satisfy [0 <= lag_index <
    number_of_lags], and [slot_index] must satisfy [0 <= slot_index <
    number_of_slots]. *)
val is_attested :
  t ->
  number_of_slots:int ->
  number_of_lags:int ->
  lag_index:int ->
  Dal_slot_index_repr.t ->
  bool

(** [commit t ~number_of_slots ~number_of_lags ~lag_index slot_index] commits into
    the attestation at [lag_index] that the slot [slot_index] is available.
    [lag_index] must satisfy [0 <= lag_index < number_of_lags], and [slot_index]
    must satisfy [0 <= slot_index < number_of_slots]. *)
val commit :
  t ->
  number_of_slots:int ->
  number_of_lags:int ->
  lag_index:int ->
  Dal_slot_index_repr.t ->
  t

(** [occupied_size_in_bits v] returns the size in bits of [v]. *)
val occupied_size_in_bits : t -> int

(** [expected_max_size_in_bits ~number_of_slots ~number_of_lags] returns the
    maximum size (in bits) of a [t] value. *)
val expected_max_size_in_bits : number_of_slots:int -> number_of_lags:int -> int

val weight : t -> int

(** Type alias for use in submodules. *)
type attestation = t

(** Slot availability represents the protocol's attestation result for a block.

    This wraps {!t} but is kept as a separate module to allow for potential
    interface differences between attestations in operations and attestation
    results in block metadata. *)
module Slot_availability : sig
  (** The slot availability type. Currently identical to {!t}. *)
  type t = private Bitset.t

  (** [empty] is the empty slot availability. *)
  val empty : t

  (** [encoding] is the data encoding for slot availability (bitset). *)
  val encoding : t Data_encoding.t

  (** [is_attested t ~number_of_slots ~number_of_lags ~lag_index slot_index]
      returns [true] if the attestation at [lag_index] commits that the slot at
      [slot_index] is available. [lag_index] must satisfy [0 <= lag_index <
      number_of_lags], and [slot_index] must satisfy [0 <= slot_index <
      number_of_slots]. *)
  val is_attested :
    t ->
    number_of_slots:int ->
    number_of_lags:int ->
    lag_index:int ->
    Dal_slot_index_repr.t ->
    bool

  (** [commit t ~number_of_slots ~number_of_lags ~lag_index slot_index] commits
      into the attestation at [lag_index] that the slot [slot_index] is
      available. [lag_index] must satisfy [0 <= lag_index < number_of_lags],
      and [slot_index] must satisfy [0 <= slot_index < number_of_slots]. *)
  val commit :
    t ->
    number_of_slots:int ->
    number_of_lags:int ->
    lag_index:int ->
    Dal_slot_index_repr.t ->
    t

  (** [number_of_attested_slots t ~number_of_lags] returns the number of
      attested slots in the given attestations. *)
  val number_of_attested_slots : t -> number_of_lags:int -> int

  (** [intersection sa attestations ~number_of_slots ~attestation_lags] returns
      the slots attested in both [sa] and [attestations]. *)
  val intersection :
    t -> attestation -> number_of_slots:int -> attestation_lags:int list -> t
end

(** This module is used to record the shard attestations.

    For each attester, a number of shards is associated. For each slot (see
    {!type:t}) we record that that number of shards were deemed available.

    This information will be used at the end of block finalisation to have the
    protocol declaring whether the slot is available. *)
module Accountability : sig
  type attested_slots = t

  (** The data-structure used to record the shards attestations. *)
  type t

  (** The recorded information for one slot: who attested, and the current total
      shard count (the sum of the shards assigned to these attesters). *)
  type slot_attestation_info = {
    attesters : Signature.Public_key_hash.Set.t;
    attested_shards_count : int;
  }

  type attestation_status = {
    total_shards : int;  (** The total number of (attestable) shards. *)
    attested_shards : int;
        (** The total number of shards that have been attested. *)
    attesters : Signature.Public_key_hash.Set.t;
        (** Who attested the shards. *)
    is_proto_attested : bool;
        (** The boolean is set to [true] IFF the [attestation_ratio] is below or
            equal to the threshold defined by the protocol. *)
  }

  (** [init ~number_of_slots ~number_of_lags] initialises a new accountability
      data-structure with [number_of_slots] slots and [number_of_lags] lags, and
      where for every slot and lag, no shard is available. *)
  val init : number_of_slots:int -> number_of_lags:int -> t

  (** [record_number_of_attested_shards t ~number_of_slots ~attestation_lag
      ~lags ~attested_level attested_slots powers] records that, for all slots
      declared available in [attested_slots], a number of shard indices are
      deemed available, this number being given by the [powers] map. This
      function must be called at most once for a given attester; otherwise the
      count will be flawed. *)
  val record_number_of_attested_shards :
    t ->
    number_of_slots:int ->
    attestation_lag:int ->
    lags:int list ->
    delegate:Signature.public_key_hash ->
    attested_level:Raw_level_repr.t ->
    attested_slots ->
    int Raw_level_repr.Map.t ->
    t

  (** [attestation_status t ~threshold ~number_of_shards ~published_level
      ~slot_index] returns the current attestation status of a slot; a slot is
      attested if the number of shards recorded in [t] for the [slot] is above
      the [threshold] with respect to the total number of shards specified by
      [number_of_shards]. *)

  (** [is_threshold_reached ~threshold ~number_of_shards ~attested_shards]
      returns [true] if [attested_shards] is sufficient to meet the attestation
      threshold. The threshold is a percentage (e.g., 50 for 50%). *)
  val is_threshold_reached :
    threshold:int -> number_of_shards:int -> attested_shards:int -> bool

  (** [get_shard_attestations t] returns the internal level map from the accountability
      data structure. The map is indexed by published level, and for each level
      contains a slot map with slot attestation information. *)
  val get_shard_attestations :
    t -> slot_attestation_info Dal_slot_index_repr.Map.t Raw_level_repr.Map.t

  (** The following type is used to persist attestation accountability
      across blocks in a sliding window indexed by absolute published level.

      This is separate from {!type:t} which is used during block application
      to accumulate attestations within a single block.

      Maps published level to slot index to attestation status and it represents
      a sliding window of attestation statuses.

      Invariant: Does not contain entries for published levels equal or smaller
      than [current_level - attestation_lag + 1]. *)
  type history =
    attestation_status Dal_slot_index_repr.Map.t Raw_level_repr.Map.t

  val history_encoding : history Data_encoding.t

  val empty_history : history
end

(** {!type-t}-dependent combination of public keys or signatures. *)
module Dal_dependent_signing : sig
  (** Encodes a {!type-t} into an integer that can be used as a weight
      for the companion key when signing an attestation with DAL. *)
  val weight :
    consensus_pk:Bls.Public_key.t ->
    companion_pk:Bls.Public_key.t ->
    op:Bytes.t ->
    t ->
    Z.t

  (** Efficiently computes [consensus_pk + (weight t) * companion_pk].

      If [subgroup_check] is set, also checks whether the points are
      in the appropriate subgroup.

      Returns [None] if the deserialization of points fails or the
      subgroup check fails -- this cannot happen when the provided
      keys are valid BLS keys. *)
  val aggregate_pk :
    subgroup_check:bool ->
    consensus_pk:Bls.Public_key.t ->
    companion_pk:Bls.Public_key.t ->
    op:Bytes.t ->
    t ->
    Bls.Public_key.t option

  (** Computes the same {!type-t}-dependent combination as
      {!aggregate_pk}, but with signatures instead of public keys. *)
  val aggregate_sig :
    subgroup_check:bool ->
    consensus_pk:Bls.Public_key.t ->
    companion_pk:Bls.Public_key.t ->
    consensus_sig:Bls.t ->
    companion_sig:Bls.t ->
    op:Bytes.t ->
    t ->
    Bls.t option
end

module Internal_for_tests : sig
  (** Builds a {!type-t} from its integer representation, that is, the
      sum of powers of two of the indexes of attested slots.

      Returns an error when the given argument is negative. *)
  val of_z : Z.t -> t tzresult
end
