(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

open Shell_operation

(* A priority is attached to each pending operation. *)
type priority = High | Medium | Low of Q.t list

(**
   This type is used for data representing pending operations of the
   prevalidator. Any iterator on this structure will process operations with
   [`High] priority first, followed by [`Medium] and finally [`Low] priority. *)
type 'protocol_data t

module Sized_set :
  Tezos_base.Sized.SizedSet with type set := Operation_hash.Set.t

(** The empty structure of pending operations. *)
val empty : 'protocol_data t

(** [hashes p] returns the set of hashes contained in [p] *)
val hashes : 'protocol_data t -> Operation_hash.Set.t

(** [operations p] returns the Map of bindings [oph -> op] contained in [p] *)
val operations :
  'protocol_data t -> 'protocol_data operation Operation_hash.Map.t

(** [is_empty p] returns [true] if [p] has operations, [false] otherwise. *)
val is_empty : 'protocol_data t -> bool

(** [mem oph p] returns [true] if [oph] is found in [p], [false] otherwise.

    Complexity is O(log(n)), where n is the number of operations (hashes) in the
    structure.
*)
val mem : Operation_hash.t -> 'protocol_data t -> bool

(** [add oph op p prio] records the operation [op] whose hash is [oph] and whose
    priority is [prio] in [p].

    Complexity is O(log(n)), where n is the number of operations (hashes) in the
    structure.

    It is unspecified behaviour to call this function with a hash ([oph]) which
    is already recorded in the data-structure ([p]). It is your responsibility
    as the caller of the function to ensure this.
*)
val add :
  'protocol_data operation -> priority -> 'protocol_data t -> 'protocol_data t

(** [remove oph op p] removes the binding [oph] from [p].

    Complexity is O(log(n)), where n is the number of operations (hashes) in the
    structure.
*)
val remove : Operation_hash.t -> 'protocol_data t -> 'protocol_data t

(** [cardinal p] returns the number of operations (hashes) in [p].

    Complexity is O(n), where n is the number of operations (hashes) in the
    structure.
*)
val cardinal : 'protocol_data t -> int

(** [fold f p acc] applies the function [f] on every binding [oph] |-> [op] of
    priority [prio] in [p]. The [acc] is passed to and (possibly) updated by
    every call to [f].

    We iterate on operations with `High priority first, then on those with `Low
    priority. For operations with the same priority, the iteration order is
    defined [Operation_hash.compare] function (operations with small hashes are
    processed first).
*)
val fold :
  (priority -> Operation_hash.t -> 'protocol_data operation -> 'a -> 'a) ->
  'protocol_data t ->
  'a ->
  'a

(** [iter f p] is similar to [fold] where [acc] is unit *)
val iter :
  (priority -> Operation_hash.t -> 'protocol_data operation -> unit) ->
  'protocol_data t ->
  unit

(** [fold_es f p acc] is the Lwt version of [fold], except that [fold_es]
    returns wihtout iterating over all the elements of the list as soon as a
    value [Error e] is returned by [f] *)
val fold_es :
  (priority ->
  Operation_hash.t ->
  'protocol_data operation ->
  'a ->
  ('a, 'b) result Lwt.t) ->
  'protocol_data t ->
  'a ->
  ('a, 'b) result Lwt.t
