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
   inbox itself. {!Sc_rollup_PVM_sem} is meant to arbitrate the first
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

   The number of available messages is bounded by
   {!Constants_repr.sc_rollup_max_available_messages}. When an inbox
   reaches the maximum number of available messages, the inbox is said
   to be full and cannot accept more messages. This limitation is
   meant to ensure that Merkle proofs about the inbox contents have a
   bounded size. (See next section.)

   {1 Merkelization of the inbox}

   As for the state of the {!Sc_rollup_PVM_sem}, the layer 1 does not
   have to store the entire inbox but only a compressed form
   (typically a low number of hashes) that witnesses its contents, so
   that the protocol can check the validity of a proof about its contents.
   This saves space in the context of the layer 1 and is sufficient for the
   level 1 to provide a source of truth about the contents of the
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
   rollup node in order to maintain consistent inboxes on both side.
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

(** The type of the inbox for a smart-contract rollup as stored
    by the protocol in the context. Values that inhabit this type
    only act as fingerprint for inboxes.

    Inbox contents is represented using {!Raw_context.TREE.tree}s.
    (See below.) *)
type t

val pp : Format.formatter -> t -> unit

val equal : t -> t -> bool

val encoding : t Data_encoding.t

(** [empty level] is an inbox started at some given [level] with no
    message at all. *)
val empty : Sc_rollup_repr.t -> Raw_level_repr.t -> t

(** [level inbox] returns the maximum level of message insertion in
   [inbox] or its initial level. *)
val inbox_level : t -> Raw_level_repr.t

(** [number_of_available_messages inbox] returns the number of
    messages that can be consumed in [inbox]. *)
val number_of_available_messages : t -> Z.t

(** [consume_n_messages n inbox] returns an inbox where [n] messages have
    been consumed, or [None] if there are strictly less than [n] messages
    available in [inbox]. *)
val consume_n_messages : int -> t -> t option tzresult

(** The following operations are subject to cross-validation between
    rollup nodes and the layer 1. *)
module type MerkelizedOperations = sig
  (** The type for the Merkle trees used in this module. *)
  type tree

  (** A merkelized message. *)
  type message = tree

  (** A merkelized sequence of messages. *)
  type messages = tree

  (** The history is a merkelized sequence of [messages], one per
     level. The history is typically used by the rollup node to
     produce inclusion proofs. The protocol only manipulates an empty
     history as it does not remember previous messages and only keeps
     a witness of the latest state of the history. *)
  type history

  val history_encoding : history Data_encoding.t

  val pp_history : Format.formatter -> history -> unit

  (** The beginning of the history is an empty sequence of [messages].
      Fail with {!Invalid_bound_on_history} if [bound] is not strictly
      positive. *)
  val history_at_genesis : bound:int64 -> history

  (** [add_messages history inbox level payloads messages] inserts a list of
     [payloads] as new messages in the [messages] of the current
     [level] of the [inbox]. This function returns the new sequence
     of messages as well as updated [inbox] and [history].

     If the [inbox]'s level is older than [level], the [inbox] is updated
     so that the messages of the levels older than [level] are archived.
     To archive a sequence of [messages] for a given [level], we push
     it at the end of the [history] and update the witness of this
     history in the [inbox]. The [inbox]'s messages for the current
     level is also emptied to insert the [payloads] in a fresh sequence
     of [messages] for [level].

     This function fails if [level] is older than [inbox]'s [level].

     This function fails with {!Max_number_of_available_messages_reached}
     if the inbox is full.

  *)
  val add_messages :
    history ->
    t ->
    Raw_level_repr.t ->
    string list ->
    messages ->
    (messages * history * t) tzresult Lwt.t

  (** [add_messages_no_history inbox level payloads messages] behaves
      a [add_messages] except that it does not remember the inbox
      history. *)
  val add_messages_no_history :
    t ->
    Raw_level_repr.t ->
    string list ->
    message ->
    (message * t, error trace) result Lwt.t

  (** [get_message messages idx] returns [Some message] if the
     sequence of [messages] has a more than [idx] messages and
     [message] is at position [idx] in this sequence.
     Returns [None] otherwise. *)
  val get_message : messages -> Z.t -> message option Lwt.t

  (** [get_message_payload messages idx] returns [Some payload] if the
      sequence of [messages] has a more than [idx] messages,
      [message] is at position [idx] in this sequence, and is defined
      by [payload]. Returns [None] otherwise. *)
  val get_message_payload : messages -> Z.t -> string option Lwt.t

  (** Given a inbox [A] at some level [L] and another inbox [B] at
     some level [L' >= L], an [inclusion_proof] guarantees that [A] is
     an older version of [B].

     To be more precise, an [inclusion_proof] guarantees that the
     previous levels messages of [A] are included in the previous
     levels messages of [B]. The current messages of [A] and [B]
     are not considered.

     The size of this proof is O(log_basis (L' - L)). *)
  type inclusion_proof

  val pp_inclusion_proof : Format.formatter -> inclusion_proof -> unit

  (** [number_of_proof_steps proof] returns the length of [proof]. *)
  val number_of_proof_steps : inclusion_proof -> int

  (** [produce_inclusion_proof history inbox1 inbox2] exploits
     [history] to produce a self-contained proof that [inbox1] is an
     older version of [inbox2]. *)
  val produce_inclusion_proof : history -> t -> t -> inclusion_proof option

  (** [verify_inclusion_proof proof inbox1 inbox2] returns [true] iff
     [proof] is a minimal and valid proof that [inbox1] is included in
     [inbox2]. *)
  val verify_inclusion_proof : inclusion_proof -> t -> t -> bool
end

module type TREE = sig
  type t

  type tree

  type key = string list

  type value = bytes

  val find : tree -> key -> value option Lwt.t

  val find_tree : tree -> key -> tree option Lwt.t

  val add : tree -> key -> value -> tree Lwt.t

  val is_empty : tree -> bool

  val hash : tree -> Context_hash.t
end

(**

   This validation is based on a standardized Merkelization
   scheme. The definition of this scheme is independent from the exact
   data model of the context but it depends on the [Tree] arity and
   internal hashing scheme.

   We provide a functor that takes a {!Context.TREE} module from any
   context, checks that the assumptions made about tree's arity and
   hashing scheme are valid, and returns a standard compliant
   implementation of the {!MerkelizedOperations}.

*)
module MakeHashingScheme (Tree : TREE) :
  MerkelizedOperations with type tree = Tree.tree

include MerkelizedOperations with type tree = Context.tree
