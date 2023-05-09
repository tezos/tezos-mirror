(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

open Protocol
open Alpha_context
open Indexed_store

module Irmin_store : Store_sigs.Store

module L2_blocks :
  INDEXED_FILE
    with type key := Block_hash.t
     and type value := (unit, unit) Sc_rollup_block.block
     and type header := Sc_rollup_block.header

(** Storage for persisting messages downloaded from the L1 node. *)
module Messages :
  INDEXED_FILE
    with type key := Sc_rollup.Inbox_merkelized_payload_hashes.Hash.t
     and type value := Sc_rollup.Inbox_message.t list
     and type header := bool * Block_hash.t * Timestamp.t * int

(** Aggregated collection of messages from the L1 inbox *)
module Inboxes :
  SIMPLE_INDEXED_FILE
    with type key := Sc_rollup.Inbox.Hash.t
     and type value := Sc_rollup.Inbox.t
     and type header := unit

(** Storage containing commitments and corresponding commitment hashes that the
    rollup node has knowledge of. *)
module Commitments :
  INDEXABLE_STORE
    with type key := Sc_rollup.Commitment.Hash.t
     and type value := Sc_rollup.Commitment.t

(** Storage mapping commitment hashes to the level when they were published by
    the rollup node. It only contains hashes of commitments published by this
    rollup node. *)
module Commitments_published_at_level : sig
  type element = {
    first_published_at_level : Raw_level.t;
        (** The level at which this commitment was first published. *)
    published_at_level : Raw_level.t option;
        (** The level at which we published this commitment. If
            [first_published_at_level <> published_at_level] it means that the
            commitment is republished. *)
  }

  include
    INDEXABLE_STORE
      with type key := Sc_rollup.Commitment.Hash.t
       and type value := element
end

module L2_head : SINGLETON_STORE with type value := Sc_rollup_block.t

module Last_finalized_level : SINGLETON_STORE with type value := int32

module Levels_to_hashes :
  INDEXABLE_STORE with type key := int32 and type value := Block_hash.t

(** Published slot headers per block hash,
    stored as a list of bindings from [Dal_slot_index.t]
    to [Dal.Slot.t]. The encoding function converts this
    list into a [Dal.Slot_index.t]-indexed map. *)
module Dal_slots_headers :
  Store_sigs.Nested_map
    with type primary_key := Block_hash.t
     and type secondary_key := Dal.Slot_index.t
     and type value := Dal.Slot.Header.t
     and type 'a store := 'a Irmin_store.t

module Dal_confirmed_slots_history :
  Store_sigs.Append_only_map
    with type key := Block_hash.t
     and type value := Dal.Slots_history.t
     and type 'a store := 'a Irmin_store.t

(** Confirmed DAL slots histories cache. See documentation of
    {!Dal_slot_repr.Slots_history} for more details. *)
module Dal_confirmed_slots_histories :
  Store_sigs.Append_only_map
    with type key := Block_hash.t
     and type value := Dal.Slots_history.History_cache.t
     and type 'a store := 'a Irmin_store.t

(** [Dal_slots_statuses] is a [Store_utils.Nested_map] used to store the
    attestation status of DAL slots. The values of this storage module have type
    `[`Confirmed | `Unconfirmed]`, depending on whether the content of the slot
    has been attested on L1 or not. If an entry is not present for a
    [(block_hash, slot_index)], this means that the corresponding block is not
    processed yet.
*)
module Dal_slots_statuses :
  Store_sigs.Nested_map
    with type primary_key := Block_hash.t
     and type secondary_key := Dal.Slot_index.t
     and type value := [`Confirmed | `Unconfirmed]
     and type 'a store := 'a Irmin_store.t

type +'a store = {
  l2_blocks : 'a L2_blocks.t;
  messages : 'a Messages.t;
  inboxes : 'a Inboxes.t;
  commitments : 'a Commitments.t;
  commitments_published_at_level : 'a Commitments_published_at_level.t;
  l2_head : 'a L2_head.t;
  last_finalized_level : 'a Last_finalized_level.t;
  levels_to_hashes : 'a Levels_to_hashes.t;
  irmin_store : 'a Irmin_store.t;
}

(** Type of store. The parameter indicates if the store can be written or only
    read. *)
type 'a t = ([< `Read | `Write > `Read] as 'a) store

(** Read/write store {!t}. *)
type rw = Store_sigs.rw t

(** Read only store {!t}. *)
type ro = Store_sigs.ro t

(** [close store] closes the store. *)
val close : _ t -> unit tzresult Lwt.t

(** [load mode ~l2_blocks_cache_size directory] loads a store from the data
    persisted in [directory]. If [mode] is {!Store_sigs.Read_only}, then the
    indexes and irmin store will be opened in readonly mode and only read
    operations will be permitted. This allows to open a store for read access
    that is already opened in {!Store_sigs.Read_write} mode in another
    process. [l2_blocks_cache_size] is the number of L2 blocks the rollup node
    will keep in memory. *)
val load :
  'a Store_sigs.mode ->
  l2_blocks_cache_size:int ->
  string ->
  'a store tzresult Lwt.t

(** [readonly store] returns a read-only version of [store]. *)
val readonly : _ t -> ro

(** [iter_l2_blocks store f] iterates [f] on all L2 blocks reachable from the
    head, from newest to oldest.  *)
val iter_l2_blocks :
  _ t -> (Sc_rollup_block.t -> unit tzresult Lwt.t) -> unit tzresult Lwt.t
