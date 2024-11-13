(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Slot attestation representation for the data-availability layer.

    {1 Overview}

    For the data-availability layer, the layer 1 provides a list of
   slots at every level (see {!Dal_slot_repr}). Slots are not posted
   directly onto L1 blocks. Stakeholders, called attesters in this
   context, can attest on the availability of the data via
   attestation operations.

    The slot is uniformly split into shards. Each attester commits,
   for every slot, on the availability of all shards they are assigned
   to.

    This module encapsulates the representation of this commitment
   that aims to be provided with attestation operations. To avoid
   overloading the network, this representation should be compact.  *)

type t = private Bitset.t

(** The size of the encoding is not bounded. However, the size of a DAL
    attestation bitset is checked during validation of an attestation; and there
    is a bound on the size of a generic operation. *)
val encoding : t Data_encoding.t

(** [empty] returns an empty [slot_attestation] which commits that
   every slot are unavailable. *)
val empty : t

(** [is_attested slot_attestation ~index] returns [true] if the
   [slot_attestation] commits that the slot at [index] is
   available. *)
val is_attested : t -> Dal_slot_index_repr.t -> bool

(** [commit slot_attestation index] commits into [slot_attestation]
   that the slot [index] is available. *)
val commit : t -> Dal_slot_index_repr.t -> t

(** [occupied_size_in_bits slot_attestation] returns the size in bits of an attestation. *)
val occupied_size_in_bits : t -> int

(** [expected_size_in_bits ~max_index] returns the expected size (in
   bits) of an attestation considering the maximum index for a slot is
   [max_index]. *)
val expected_size_in_bits : max_index:Dal_slot_index_repr.t -> int

(** [number_of_attested_slots slot_attestation] returns the number of attested
    slots in an attestation. *)
val number_of_attested_slots : t -> int

(** A shard_index aims to be a positive number. *)
type shard_index = int

module Shard_map : Map.S with type key = shard_index

(** This module is used to record the shard attestations.

   For each attester, a list of shards is associated. For each
   attested slot (see {!type:t}) we record that those shards were
   deemed available.

   This information will be used at the end of block finalisation to
   have the protocol declaring whether the slot is available.  *)
module Accountability : sig
  type attested_slots = t

  (** The data-structure used to record the shards attestations. *)
  type t

  (** DAL/FIXME https://gitlab.com/tezos/tezos/-/issues/3145

     Consider using the [Bounded] module. In particular, change the
     semantics of [is_slot_attested] accordingly. *)

  (** [init ~number_of_slots] initialises a new accountability data-structure
     with [number_of_slots] slots and where for every slot, no shard is
     available. *)
  val init : number_of_slots:int -> t

  (** [record_number_of_attested_shards t slots number] records that, for all
      slots declared available in [slots], the given [number] of shard indices
      are deemed available. This function must be called at most once for a
      given attester; otherwise the count will be flawed. *)
  val record_number_of_attested_shards : t -> attested_slots -> int -> t

  (** [is_slot_attested t ~threshold ~number_of_shards slot] returns [true] if
      the number of shards recorded in [t] for the [slot] is above the
      [threshold] with respect to the total number of shards specified by
      [number_of_shards]. Returns [false] otherwise or if the [index] is out of
      the interval [0; number_of_slots - 1] where [number_of_slots] is the value
      provided to the [init] function. *)
  val is_slot_attested :
    t -> threshold:int -> number_of_shards:int -> Dal_slot_index_repr.t -> bool
end
