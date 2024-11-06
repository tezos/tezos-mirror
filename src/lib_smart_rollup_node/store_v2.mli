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

(** This version of the store is used for the rollup nodes for protocols for and
    after Nairobi, i.e. >= 17. *)

open Indexed_store

include module type of struct
  include Store_v1
end

(** Storage for persisting messages downloaded from the L1 node. *)
module Messages :
  INDEXED_FILE
    with type key := Merkelized_payload_hashes_hash.t
     and type value := string list
     and type header := Block_hash.t

(** Storage for persisting inboxes. *)
module Inboxes :
  SIMPLE_INDEXED_FILE
    with type key := Octez_smart_rollup.Inbox.Hash.t
     and type value := Octez_smart_rollup.Inbox.t
     and type header := unit

(** Storage containing commitments and corresponding commitment hashes that the
    rollup node has knowledge of. *)
module Commitments :
  SIMPLE_INDEXED_FILE
    with type key := Octez_smart_rollup.Commitment.Hash.t
     and type value := Octez_smart_rollup.Commitment.t
     and type header := unit

module Outbox_messages : sig
  type +'a t

  val load : path:string -> 'a Store_sigs.mode -> 'a t tzresult Lwt.t

  val readonly : [> `Read] t -> [`Read] t

  val by_outbox_level :
    [> `Read] t -> outbox_level:int32 -> (int * bool) list tzresult Lwt.t

  val pending :
    [> `Read] t ->
    min_level:int32 ->
    max_level:int32 ->
    (int32 * int list) list tzresult Lwt.t

  val register_new_outbox_messages :
    [> `Read | `Write] t ->
    outbox_level:int32 ->
    indexes:int list ->
    unit tzresult Lwt.t

  val register_missing_outbox_messages :
    [> `Read | `Write] t ->
    outbox_level:int32 ->
    indexes:int list ->
    unit tzresult Lwt.t

  val set_outbox_message_executed :
    [> `Read | `Write] t ->
    outbox_level:int32 ->
    index:int ->
    unit tzresult Lwt.t

  val iter :
    [> `Read] t ->
    (outbox_level:int32 ->
    messages:int list ->
    executed_messages:int list ->
    unit tzresult Lwt.t) ->
    unit tzresult Lwt.t
end

(** Storage containing the last cemented commitment. *)
module Lcc : sig
  type lcc = {commitment : Commitment.Hash.t; level : int32}

  include SINGLETON_STORE with type value := lcc
end

(** Storage containing a single commitment for the last published commitment. *)
module Lpc : SINGLETON_STORE with type value := Octez_smart_rollup.Commitment.t

(** Published slot headers per block hash,
    stored as a list of bindings from [Dal_slot_index.t]
    to [Dal.Slot.t]. The encoding function converts this
    list into a [Dal.Slot_index.t]-indexed map. *)
module Dal_slots_headers :
  Store_sigs.Nested_map
    with type primary_key := Block_hash.t
     and type secondary_key := Dal.Slot_index.t
     and type value := Dal.Slot_header.t
     and type 'a store := 'a Irmin_store.t

module Protocols : sig
  type level = First_known of int32 | Activation_level of int32

  (** Each element of this type represents information we have about a Tezos
      protocol regarding its activation. *)
  type proto_info = {
    level : level;
        (** The level at which we have seen the protocol for the first time,
            either because we saw its activation or because the first block we
            saw (at the origination of the rollup) was from this protocol. *)
    proto_level : int;
        (** The protocol level, i.e. its number in the sequence of protocol
            activations on the chain. *)
    protocol : Protocol_hash.t;  (** The protocol this information concerns. *)
  }

  val proto_info_encoding : proto_info Data_encoding.t

  include SINGLETON_STORE with type value = proto_info list
end

(** Data related to the effects of garbage collection. *)
module Gc_levels : sig
  type levels = {
    last_gc_level : int32;
        (** Records the last level at which GC was called. *)
    first_available_level : int32;
        (** Records the first level for which data is guaranteed to be stored.
        Data for all previous levels might have been removed. *)
  }

  include SINGLETON_STORE with type value = levels
end

(** Level at which context was last split. *)
module Last_context_split : SINGLETON_STORE with type value := int32

type history_mode = Archive | Full

(** History mode of the rollup node. *)
module History_mode : SINGLETON_STORE with type value := history_mode

type +'a store = {
  l2_blocks : 'a L2_blocks.t;
  messages : 'a Messages.t;
  inboxes : 'a Inboxes.t;
  commitments : 'a Commitments.t;
  commitments_published_at_level : 'a Commitments_published_at_level.t;
  outbox_messages : 'a Outbox_messages.t;
  l2_head : 'a L2_head.t;
  last_finalized_level : 'a Last_finalized_level.t;
  lcc : 'a Lcc.t;
  lpc : 'a Lpc.t;
  levels_to_hashes : 'a Levels_to_hashes.t;
  protocols : 'a Protocols.t;
  irmin_store : 'a Irmin_store.t;
  gc_levels : 'a Gc_levels.t;
  successful_gc_levels : 'a Gc_levels.t;
  last_context_split_level : 'a Last_context_split.t;
  history_mode : 'a History_mode.t;
}

include Store_sig.S with type 'a store := 'a store

(** [is_gc_finished t] returns [true] if there is no GC running. *)
val is_gc_finished : 'a t -> bool

(** [cancel_gc t] stops any currently ongoing GC. It returns [true] if a GC was
    canceled. *)
val cancel_gc : 'a t -> bool Lwt.t
