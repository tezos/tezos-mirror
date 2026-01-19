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

(* TODO: https://gitlab.com/tezos/tezos/-/issues/8065
   Module to be removed in protocol 025. *)

(** Slot attestation representation for the data-availability layer.

    {1 Overview}

    For the data-availability layer, the layer 1 provides a list of slots at
    every level (see {!Dal_slot_repr}). Slots are not posted directly onto L1
    blocks. Stakeholders, called attesters in this context, can attest on the
    availability of the data via attestation operations.

    The slot is uniformly split into shards. Each attester commits, for every
    slot, on the availability of all shards they are assigned to.

    This module encapsulates the representation of this commitment that aims to
    be provided with attestation operations. To avoid overloading the network,
    this representation should be compact.  *)

type t = private Bitset.t

(** The size of the encoding is not bounded. However, the size of a DAL
    attestation bitset is checked during validation of an attestation; and there
    is a bound on the size of a generic operation. *)
val encoding : t Data_encoding.t

(** [empty] returns an empty [slot_attestation] which commits that
   every slot are unavailable. *)
val empty : t

(** [is_empty slot_attestation] returns [true] if no slots are attested. *)
val is_empty : t -> bool

(** [is_attested slot_attestation ~index] returns [true] if the
    [slot_attestation] commits that the slot at [index] is available. [index]
    must be valid (in particular, non-negative). *)
val is_attested : t -> Dal_slot_index_repr.t -> bool

(** [commit slot_attestation index] commits into [slot_attestation] that the
    slot [index] is available. [index] must be valid (in particular,
    non-negative). *)
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

val of_attestations : Dal_attestations_repr.t -> t

module Internal_for_tests : sig
  (** Builds a {!type-t} from its integer representation, that is, the
      sum of powers of two of the indexes of attested slots.

      Returns an error when the given argument is negative. *)
  val of_z : Z.t -> t tzresult
end
