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

type t

(** The size of the encoding is not bounded. However, the size of a DAL
    attestations bitset is checked during validation of an attestation; and
    there is a bound on the size of a generic operation. *)
val encoding : t Data_encoding.t

(** [empty] returns an empty attestation structure where all slots at all lags
    are marked as unavailable. *)
val empty : t

(** [is_empty t] returns [true] if all attestations at all lags are empty. *)
val is_empty : t -> bool

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

(** Slot availability represents the protocol's attestation result for a block.

    This wraps {!t} but is kept as a separate module to allow for potential
    interface differences between attestations in operations and attestation
    results in block metadata. *)
module Slot_availability : sig
  (** The slot availability type. Currently identical to {!t}. *)
  type t

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
end
