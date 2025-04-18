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

(** This module deals with the different profiles of a DAL node in controller
    mode. *)

(** An controller DAL node can play three different roles:
    - attester for some pkh: checks that the shards assigned to this pkh are published
    - operator for some slot index,
    - observer for some slot index.

    Operator and observer DAL nodes can "produce" slots, that is, they split a
    slot into shards and publish the shards. They are therefore also called
    "slot producers".

    Operators and observers also perform amplification: they reconstruct a slot
    when enough shards are seen, and republish the missing shards. They both are
    called "prover" profiles, because they need to generate shard proofs,
    needing the full SRS, while the attester profile only needs the smaller,
    "verifier SRS".

    The difference between operators and observers is that operators can
    participate in refutation games (because they store slot data for the whole
    refutation period), while observers cannot.

    A single DAL node can play several of these roles at once. We call a profile
    the set of roles played by a DAL node and represent it as a triple of
    sets. *)
type t

val encoding : t Data_encoding.t

(** The empty controller; a controller with this profile does nothing *)
val empty : t

val is_empty : t -> bool

(** [has_attester t] returns true if there is a public key in the
    attester field of [t] *)
val has_attester : t -> bool

(** [has_operator t] returns true if there is a slot index in the
    operator field of [t] *)
val has_operator : t -> bool

(** [has_observer t] returns true if there is a slot index in the
    observer field of [t] *)
val has_observer : t -> bool

(** [attester_only t] returns true if [t] has an attester role,
    and no operator, not observer roles. *)
val attester_only : t -> bool

(** [attesters t] returns the set of attesters registered within the
    attester profile, if any. *)
val attesters : t -> Signature.Public_key_hash.Set.t

(** [operator_slot_out_of_bounds n t] returns the first slot index that for
    operator that is not between 0 and n - 1; it returns [None] if no slot index
    was outside these bounds *)
val operator_slot_out_of_bounds : int -> t -> int option

(** [observer_slot_out_of_bounds n t] returns the first slot index that for
    observer that is not between 0 and n - 1; it returns [None] if no slot index
    was outside these bounds *)
val observer_slot_out_of_bounds : int -> t -> int option

(** [is_observed_slot i op] returns true if and only if [op] contains an
    observer for slot index [i] *)
val is_observed_slot : int -> t -> bool

(** [can_publish_on_slot_index slot_index op] returns true if and only
    if [op] contains an observer or an operator for slot index
    [slot_index] *)
val can_publish_on_slot_index : int -> t -> bool

(** Returns all slot indexes used in the profile, whether they are for operator
    or observer *)
val get_all_slot_indexes : t -> int list

(** [make ~attesters ~operators ~observers ()] returns an
    controller mode for a node that is an attester for the public keys in
    [attesters], a operator for each slot indexes in [operators], and an
    observer for all slot indexes in [observer]. When a list is empty or not
    specified, no value for the corresponding role will be included in the
    result ; [make ()] returns [empty] *)
val make :
  ?attesters:Signature.Public_key_hash.t list ->
  ?operators:int list ->
  ?observers:int list ->
  unit ->
  t

(** [merge ~on_new_attester op1 op2] returns a controller mode that contains
    both op1 & op2. [on_new_attester] is a function triggered for each public
    key in [op2] that is not already in [op1] *)
val merge :
  ?on_new_attester:(Signature.Public_key_hash.t -> unit) -> t -> t -> t
