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

(** Slot endorsement representation for the data-availability layer.

    {1 Overview}

    For the data-availability layer, the layer 1 provides a list of
   slots at every level (see {!dal_slot_repr}). Slots are not posted
   directly onto L1 blocks. Stakeholders (via endorsements) can commit
   on the availability of the data.

    The slot is uniformly split into shards. Each endorser commits for
   every slot at every level on the availability of all shards they
   are assigned to.

    This module encapsulates the representation of this commitment
   that aims to be provided with endorsement operations. To avoid
   overloading the network, this representation should be compact.  *)

type t

type available_slots = t

val encoding : t Data_encoding.t

(** [empty] returns an empty [slot_endorsement] which commits that
   every slot are unavailable. *)
val empty : t

(** [is_available slot_endorsement ~index] returns [true] if the
   [slot_endorsement] commits that the slot at [index] is
   available. *)
val is_available : t -> Dal_slot_repr.Index.t -> bool

(** [commit slot_endorsement index] commits into [slot_endorsement]
   that the [index] is available. *)
val commit : t -> Dal_slot_repr.Index.t -> t

(** [occupied_size_in_bits slot_endorsement] returns the size in bits of an endorsement. *)
val occupied_size_in_bits : t -> int

(** [expected_size_in_bits ~max_index] returns the expected size (in
   bits) of an endorsement considering the maximum index for a slot is
   [max_index]. *)
val expected_size_in_bits : max_index:Dal_slot_repr.Index.t -> int

(** This module is used to record the various data-availability
   endorsements.

   For each endorser, a list of shards is associated. For each slots
   declared available (see {!type:t}) we record that those shards were
   available.

  This information will be used at the end of block finalisation to
   have the protocol declaring whether the slot is available.  *)
module Accountability : sig
  (** The data-structure used to record the shards-slots availability. *)
  type t

  (** DAL/FIXME https://gitlab.com/tezos/tezos/-/issues/3145

     Consider using the [Bounded] module. In particular, change the
     semantics of [is_slot_available] accordingly. *)

  (** A shard aims to be a positive number. *)
  type shard = int

  (** [init ~length] initialises a new accountability data-structures
     with at most [length] slots and where for every slot, no shard is
     available. *)
  val init : length:int -> t

  (** [record_shards_availability t slots shards] records that for all
     slots declared available in [slots], shard indices in [shards]
     are available. It is the responsibility of the caller to ensure
     the shard indices are positive numbers. A negative shard index is
     ignored. *)
  val record_shards_availability : t -> available_slots -> shard list -> t

  (** [is_slot_available t ~threshold ~number_of_shards slot] returns
     [true] if the number of shards recorded in [t] for the [slot] is
     above the [threshold] with respect to the total number of shards
     specified by [number_of_shards]. Returns [false] otherwise or if
     the [index] is out of the interval [0;length] where [length] is
     the value provided to the [init] function. *)
  val is_slot_available :
    t -> threshold:int -> number_of_shards:int -> Dal_slot_repr.Index.t -> bool
end
