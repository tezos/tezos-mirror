(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Merkelizing inbox for smart-contract rollups.

   {1 Overview}

   The inbox of a smart-contract rollup denotes the incoming messages
   of the rollup. This inbox is the source of truth about what
   operations are being published and have an effect on the rollup
   state. As such, the inbox completely determines the state of the
   rollup. Hence, if two claims disagree about the state of the
   rollup, there are only two possibilities: either these two claims
   correspond to two distinct interpretations of the same inbox ; or,
   these two claims differ on their views about the contents of the
   inbox itself. {!Sc_rollup_PVM_sig} is meant to arbitrate the first
   kind of conflicts while {!Sc_rollup_inbox} focuses on the second
   kind of conflicts.

   {1 Inbox messages}

   A message is a chunk of bytes. Messages are indexed using natural
   numbers and the level they are introduced.

   A message is said to be *consumed* when its processing has been
   cemented, that is, when no refutation about its insertion can
   happen anymore because the commitment that describes the effect of
   this message on the state is cemented. A message is said to be
   *available* (for dispute) if it is not consumed.

   A message processed by the rollup can be consumed or available. A
   message unprocessed by the rollup is always available.

   The number of messages in a commitment period is bounded by
   {!Constants_storage.sc_rollup_max_number_of_messages_per_commitment_period}.
   When an inbox reaches the maximum number of messages in the commitment
   period, the inbox is said to be full and cannot accept more messages.
   This limitation is meant to ensure that Merkle proofs about the inbox
   contents have a bounded size. (See next section.)

   {1 Merkelization of the inbox}

   As for the state of the {!Sc_rollup_PVM_sig}, the layer 1 does not
   have to store the entire inbox but only a compressed form
   (typically a low number of hashes) that witnesses its contents, so
   that the protocol can check the validity of a proof about its contents.
   This saves space in the context of the layer 1 and is sufficient for the
   layer 1 to provide a source of truth about the contents of the
   inbox at the current level.

   {1 A level-indexed chain of inboxes}

   By design, inboxes are logically indexed by Tezos levels. This is
   required to have a simple way to decide if two commitments are in
   conflict. (See {!Sc_rollup_storage}.)

   A commitment included in the block at level L describes the effect
   of the messages of the inboxes with a level between a starting
   level L_0 and a stopping level L_1, both strictly inferior to
   L. The level L_0 must be the inbox level of its parent
   commitment.

   To be valid, a commitment needs to prove that it is reading
   messages from an inbox which is consistent with the inbox at level
   L stored in the layer 1 context. So, it should be possible at any
   time to build a proof that a given inbox is a previous version at
   level L_1 of the inbox found at level L: these are called inclusion
   proofs.

   {1 Clients}

   This module is meant to be used both by the protocol and by the
   rollup node in order to maintain consistent inboxes on both sides.
   These two clients slightly differ on the amount of information they
   store about the inbox.

   On the one hand, to reduce the space consumption of rollups on the
   chain storage, the protocol only stores metadata about the
   inbox. The messages of the current level are kept in memory during
   block validation only (See {!Raw_context.Sc_rollup_in_memory_inbox}).
   By contrast, the messages of the previous levels are not kept in
   the context at all. They can be retrieved from the chain
   history though. However, being absent from the context, they are
   not accessible to the protocol.

   On the other hand, the rollup node must keep a more precise inbox
   to be able to produce Merkle proofs about the content of specific
   messages, at least during the refutation period.

   To cope with the discrepancy of requirements in terms of inbox
   storage while preserving a consistent Merkelization
   between the protocol and the rollup node, this module exposes the
   hashing schemes used to merkelize the inbox as a functor parameterized
   by the exact context where Merkle trees are stored.

*)

module Hash : sig
  include S.HASH

  val of_context_hash : Context_hash.t -> t

  val to_context_hash : t -> Context_hash.t
end

module V1 : sig
  (** The type of the inbox for a smart-contract rollup as stored
    by the protocol in the context. Values that inhabit this type
    only act as fingerprint for inboxes.

    Inbox contents is represented using {!Raw_context.TREE.tree}s.
    (See below.) *)
  type t

  val pp : Format.formatter -> t -> unit

  val equal : t -> t -> bool

  val encoding : t Data_encoding.t

  (** [inbox_level inbox] returns the maximum level of message insertion in
      [inbox] or its initial level. *)
  val inbox_level : t -> Raw_level_repr.t

  (** A [level_proof] contains the root hash of the level tree and its
      corresponding level. *)
  type level_proof

  (** A [history_proof] is a [Skip_list.cell] that stores multiple
    hashes. [Skip_list.content history_proof] gives the hash of the
    level tree for this cell, while [Skip_list.back_pointers
    history_proof] is an array of hashes of earlier [history_proof]s
    in the inbox.

    On the one hand, we think of this type as representing the whole
    Merkle structure of an inbox at a given level---it is the part of
    {!t} above that can actually be used to prove things (it cannot be
    forged by a malicious node because it much match the hash stored by
    the L1).

    On the other hand, we think of this type as representing a single
    proof-step back through the history of the inbox; given a hash that
    appears at some point later in the inbox this type proves that that
    hash points to this particular combination of a level tree and
    further back-pointers.

    In terms of size, this type is a small set of hashes; one for the
    current level tree and `O(log2(ix))` in the back-pointers, where
    [ix] is the index of the cell in the skip list. That is, [ix] is the
    number of non-empty levels between now and the origination level of
    the rollup.
  *)
  type history_proof

  (** A [History.t] is basically a lookup table of {!history_proof}s. We
      need this if we want to produce inbox proofs because it allows us
      to dereference the 'pointer' hashes in any of the
      [history_proof]s. This [deref] function is passed to
      [Skip_list.back_path] or [Skip_list.search] to allow these
      functions to construct valid paths back through the skip list.

      A subtlety of this [history] type is that it is customizable
      depending on how much of the inbox history you actually want to
      remember, using the [capacity] parameter. In the L1 we use this with
      [capacity] set to zero, which makes it immediately forget an old
      level as soon as we move to the next. By contrast, the rollup node
      uses a history that is sufficiently large to be able to take part
      in all potential refutation games occurring during the challenge
      period. *)
  module History :
    Bounded_history_repr.S with type key = Hash.t and type value = history_proof

  val pp_history_proof : Format.formatter -> history_proof -> unit

  val history_proof_encoding : history_proof Data_encoding.t

  val equal_history_proof : history_proof -> history_proof -> bool

  (** [old_levels_messages inbox] returns the skip list of the inbox
    history. How much data there actually is depends on the context---in
    the L1 most of the history is forgotten and just a root hash of the
    skip list is kept. *)
  val old_levels_messages : t -> history_proof

  (** [number_of_messages_during_commitment_period inbox] returns the
    number of messages added in the inbox since the beginning of
    the current commitment period. *)
  val number_of_messages_during_commitment_period : t -> int64
end

(** Versioning, see {!Sc_rollup_data_version_sig.S} for more information. *)
include Sc_rollup_data_version_sig.S with type t = V1.t

include module type of V1 with type t = V1.t

(** This extracts the current {!level_proof} from the inbox. Note: the
    current level hash is stored lazily as [fun () -> ...], and this
    function will call that function. So don't use this if you want to
    preserve the laziness. *)
val current_level_proof : t -> level_proof

type serialized_proof

val serialized_proof_encoding : serialized_proof Data_encoding.t

type inbox = t
