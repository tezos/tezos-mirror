(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* This module handle operator profiles for the DAL node *)

(** An operator DAL node can play three different roles:
    - attester for some pkh: checks that the shards assigned to this pkh are published
    - slot producer for some slot index: splits slots into shards and publishes the shards
    - slot observer for some slot index: collects the shards
    corresponding to some slot index, reconstructs slots when enough
    shards are seen, and republishes missing shards.

    A single DAL node can play several of these roles at once. We call a profile the
    set of roles played by a DAL node and represent it as a triple of sets. *)
type t

val encoding : t Data_encoding.t

(** The empty operator; an operator with this profile does nothing *)
val empty : t

val is_empty : t -> bool

(** [has_attester operator] returns true if there is a public key in the
    attester field of [operator] *)
val has_attester : t -> bool

(** [has_producer operator] returns true if there is a slot index in the
    producer field of [operator] *)
val has_producer : t -> bool

(** [has_observer operator] returns true if there is a slot index in the
    observer field of [operator] *)
val has_observer : t -> bool

(** [attester_only operator] returns true if [operator] has an attester role,
    and no producer, not observer roles. *)
val attester_only : t -> bool

(** [producer_slot_out_of_bounds n op] returns the first slot index that for
    producer that is not between 0 and n - 1; it returns [None] if no slot
    index was outside these bounds *)
val producer_slot_out_of_bounds : int -> t -> int option

(** [is_observed_slot i op] returns true if and only if [op] contains an
    observer for slot index [i] *)
val is_observed_slot : int -> t -> bool

(** [can_publish_on_slot_index slot_index op] returns true if and only
    if [op] contains an observer or a producet for slot index
    [slot_index] *)
val can_publish_on_slot_index : int -> t -> bool

(** Returns all slot indexes used in the profile, whether they are for producer
    or observer *)
val get_all_slot_indexes : t -> int list

(** [make ~attesters ~producers ~observers ()] returns an
    operator profile for a node that is an attester for the public keys in
    [attesters], a producer for each slot indexes in [producers], and an
    observer for all slot indexes in [observer]. When a list is empty or not
    specified, no value for the corresponding role will be included in the
    result ; [make ()] returns [empty] *)
val make :
  ?attesters:Signature.Public_key_hash.t list ->
  ?producers:int list ->
  ?observers:int list ->
  unit ->
  t

(** [merge ~on_new_attester op1 op2] returns an operator profile that
    contains both op1 & op2. [on_new_attester] is a function triggered for each
    public key in [op2] that is not already in [op1] *)
val merge :
  ?on_new_attester:(Signature.Public_key_hash.t -> unit) -> t -> t -> t
