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

(** This version of the store is used for the rollup nodes for protocol Mumbai,
    i.e. = 16.

    This interface is a copy of
    [src/proto_016_PtMumbai/lib_sc_rollup_node/store.mli], which contains the
    layout for the Mumbai rollup node.
*)

open Indexed_store

module Irmin_store : sig
  include module type of Irmin_store.Make (struct
    let name = "Tezos smart rollup node"
  end)

  include Store_sigs.Store with type 'a t := 'a t
end

module L2_blocks :
  INDEXED_FILE
    with type key := Block_hash.t
     and type value := (unit, unit) Sc_rollup_block.block
     and type header := Sc_rollup_block.header

(** Storage for persisting messages downloaded from the L1 node. *)
module Messages :
  INDEXED_FILE
    with type key := Octez_smart_rollup.Merkelized_payload_hashes_hash.t
     and type value := string list
     and type header := Block_hash.t * Time.Protocol.t * int

(** Aggregated collection of messages from the L1 inbox *)
module Inboxes :
  SIMPLE_INDEXED_FILE
    with type key := Octez_smart_rollup.Inbox.Hash.t
     and type value := Octez_smart_rollup.Inbox.V1.t
     and type header := unit

(** Storage containing commitments and corresponding commitment hashes that the
    rollup node has knowledge of. *)
module Commitments :
  INDEXABLE_STORE
    with type key := Octez_smart_rollup.Commitment.Hash.t
     and type value := Octez_smart_rollup.Commitment.t

(** Storage mapping commitment hashes to the level when they were published by
    the rollup node. It only contains hashes of commitments published by this
    rollup node. *)
module Commitments_published_at_level : sig
  type element = {
    first_published_at_level : int32;
        (** The level at which this commitment was first published. *)
    published_at_level : int32 option;
        (** The level at which we published this commitment. If
            [first_published_at_level <> published_at_level] it means that the
            commitment is republished. *)
  }

  include
    INDEXABLE_STORE
      with type key := Octez_smart_rollup.Commitment.Hash.t
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
     and type value := Dal.Slot_header.V1.t
     and type 'a store := 'a Irmin_store.t

(** [Dal_processed_slots] is a [Store_utils.Nested_map] used to store the processing
    status of dal slots content fetched by the rollup node. The values of
    this storage module have type `[`Confirmed | `Unconfirmed]`, depending on
    whether the content of the slot has been confirmed or not. If an entry is
    not present for a [(block_hash, slot_index)], this either means that it's
    not processed yet.
*)
module Dal_processed_slots :
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

include Store_sig.S with type 'a store := 'a store
