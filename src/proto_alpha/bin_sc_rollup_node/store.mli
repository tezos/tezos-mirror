(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* TODO: https://gitlab.com/tezos/tezos/-/issues/3471
   Use indexed file for append-only instead of Irmin. *)

(* TODO: https://gitlab.com/tezos/tezos/-/issues/3739
   Refactor the store file to have functors in their own
   separate module, and return errors within the Error monad. *)

open Protocol
open Alpha_context

include Store_sigs.Store

type state_info = {
  num_messages : Z.t;
  num_ticks : Z.t;
  initial_tick : Sc_rollup.Tick.t;
}

(** [close store] closes the store. *)
val close : t -> unit Lwt.t

(** [load directory] loads a store from the data persisted in [directory].*)
val load : string -> t Lwt.t

(** Extraneous state information for the PVM *)
module StateInfo :
  Store_sigs.Map
    with type key = Block_hash.t
     and type value = state_info
     and type store = t

module StateHistoryRepr : sig
  type event = {
    tick : Sc_rollup.Tick.t;
    block_hash : Block_hash.t;
    predecessor_hash : Block_hash.t;
    level : Raw_level.t;
  }

  module TickMap : Map.S with type key = Sc_rollup.Tick.t

  type value = event TickMap.t
end

(** [StateHistory] represents storage for the PVM state history: it is an
    extension of [Store_utils.Mutable_value] whose values are lists of bindings
    indexed by PVM tick numbers, and whose value contains information about the
    block that the PVM was processing when generating the tick.
*)
module StateHistory : sig
  include
    Store_sigs.Mutable_value
      with type value = StateHistoryRepr.value
       and type store = t

  val insert : t -> StateHistoryRepr.event -> unit Lwt.t

  val event_of_largest_tick_before :
    t ->
    StateHistoryRepr.TickMap.key ->
    StateHistoryRepr.event option tzresult Lwt.t
end

(** Storage for persisting messages downloaded from the L1 node, indexed by
    [Block_hash.t]. *)
module Messages :
  Store_sigs.Map
    with type key = Block_hash.t
     and type value = Sc_rollup.Inbox_message.t list
     and type store = t

(** Aggregated collection of messages from the L1 inbox *)
module Inboxes :
  Store_sigs.Map
    with type key = Block_hash.t
     and type value = Sc_rollup.Inbox.t
     and type store = t

(** Histories from the rollup node. **)
module Histories :
  Store_sigs.Map
    with type key = Block_hash.t
     and type value = Sc_rollup.Inbox.History.t
     and type store = t

(** Storage containing commitments and corresponding commitment hashes that the
    rollup node has knowledge of. *)
module Commitments :
  Store_sigs.Map
    with type key = Raw_level.t
     and type value = Sc_rollup.Commitment.t * Sc_rollup.Commitment.Hash.t
     and type store = t

(** Storage containing the inbox level of the last commitment produced by the
    rollup node. *)
module Last_stored_commitment_level :
  Store_sigs.Mutable_value with type value = Raw_level.t and type store = t

(** Storage contianing the inbox level of the last commitment published by the
    rollup node. *)
module Last_published_commitment_level :
  Store_sigs.Mutable_value with type value = Raw_level.t and type store = t

(** Storage containing the inbox level of the last commitment cemented for the
    rollup. The commitment has not been necessarily generated by this rollup
    node. *)
module Last_cemented_commitment_level :
  Store_sigs.Mutable_value with type value = Raw_level.t and type store = t

(** torage containing the hash  of the last commitment cemented for the rollup.
    The commitment has not been necessarily generated by this rollup node. *)
module Last_cemented_commitment_hash :
  Store_sigs.Mutable_value
    with type value = Sc_rollup.Commitment.Hash.t
     and type store = t

(** Storage mapping commitment hashes to the level when they were published by
    the rollup node. It only contains hashes of commitments published by this
    rollup node. *)
module Commitments_published_at_level :
  Store_sigs.Map
    with type key = Sc_rollup.Commitment.Hash.t
     and type value = Raw_level.t
     and type store = t

(** Storage containing the a [Block_hash.t]-indexed map whose values are
    the slots to which the rollup was subscribed w.r.t. a block hash. *)
module Dal_slot_subscriptions :
  Store_sigs.Map
    with type key = Block_hash.t
     and type value = Dal.Slot_index.t list
     and type store = t

(** Storage containing the hashes of contexts retrieved from the L1 node. *)
module Contexts :
  Store_sigs.Map
    with type key = Block_hash.t
     and type value = Context.hash
     and type store = t

(** Published slot headers per block hash,
    stored as a list of bindings from [Dal_slot_index.t]
    to [Dal.Slot.t]. The encoding function converts this
    list into a [Dal.Slot_index.t]-indexed map. *)
module Dal_slots_headers :
  Store_sigs.Nested_map
    with type primary_key = Block_hash.t
     and type secondary_key = Dal.Slot_index.t
     and type value = Dal.Slot.Header.t
     and type store = t

module Dal_confirmed_slots_history :
  Store_sigs.Map
    with type key = Block_hash.t
     and type value = Dal.Slots_history.t
     and type store = t

(** Confirmed DAL slots histories cache. See documentation of
    {Dal_slot_repr.Slots_history} for more details. *)
module Dal_confirmed_slots_histories :
  Store_sigs.Map
    with type key = Block_hash.t
     and type value = Dal.Slots_history.History_cache.t
     and type store = t

(** [Dal_slot_pages] is a [Store_utils.Nested_map] used to store the contents
    of dal slots fetched by the rollup node, as a list of pages. The values of
    this storage module have type `string option list`. The value[None] for
    elements of the list is used to denote pages of slots that have not been
    confirmed. A value of the form [Some page_contents] refers to a page of
    a slot that has been confirmed, and whose contents are [page_contents].
*)
module Dal_slot_pages :
  Store_sigs.Nested_map
    with type primary_key = Block_hash.t
     and type secondary_key = Dal.Slot_index.t * Dal.Page.Index.t
     and type value = Dal.Page.content option
     and type store = t
