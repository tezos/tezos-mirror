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

type error += Inbox_proof_error of string

type error += Inbox_level_reached_messages_limit

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
   kind of conflicts while {!Sc_rollup_inbox_repr} focuses on the second
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

   The number of messages in an inbox level is bounded by
   {!Constants_repr.sc_rollup_max_number_of_messages_per_level}
   When a level inbox reaches the maximum number of messages in the inbox level,
   the inbox is said to be full and cannot accept more messages at this level.
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
   inbox. The messages' hash of the current level are kept in memory during
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
   functions used to merkelize the inbox with an history (See
   {!History_bounded_repr.t}) as parameters to remember.

*)

module Hash : S.HASH with type t = Smart_rollup.Inbox_hash.t

module Skip_list : Skip_list_repr.S

module V1 : sig
  type level_proof = {
    hash : Sc_rollup_inbox_merkelized_payload_hashes_repr.Hash.t;
    level : Raw_level_repr.t;
  }

  (** A [history_proof] is a [Skip_list.cell] that stores multiple
    hashes. [Skip_list.content history_proof] gives the hash of this cell,
    while [Skip_list.back_pointers history_proof] is an array of hashes of
    earlier [history_proof]s in the inbox.

    On the one hand, we think of this type as representing the whole
    Merkle structure of an inbox at a given level---it is the part of
    {!t} above that can actually be used to prove things (it cannot be
    forged by a malicious node because it much match the hash stored by
    the L1).

    On the other hand, we think of this type as representing a single
    proof-step back through the history of the inbox; given a hash that
    appears at some point later in the inbox this type proves that that
    hash points to this particular combination of a witness and further
    back-pointers.

    In terms of size, this type is a small set of hashes; one for the
    current witness and `O(log2(ix))` in the back-pointers, where [ix]
    is the index of the cell in the skip list. That is, [ix] is the
    number of non-empty levels between now and the origination level of
    the rollup.
  *)
  type history_proof = (level_proof, Hash.t) Skip_list.cell

  (** The type of the inbox for a smart-contract rollup as stored
      by the protocol in the context. Values that inhabit this type
      only act as fingerprint for inboxes and contain:
      - [level] : the inbox level ;
      - [old_levels_messages] : a witness of the inbox history.
  *)
  type t = {level : Raw_level_repr.t; old_levels_messages : history_proof}

  val pp : Format.formatter -> t -> unit

  val equal : t -> t -> bool

  val hash : t -> Hash.t

  val encoding : t Data_encoding.t

  (** [inbox_level inbox] returns the maximum level of message insertion in
      [inbox] or its initial level. *)
  val inbox_level : t -> Raw_level_repr.t

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

  (** [old_levels_messages inbox] returns the latest skip list cell of the inbox
      history that is not up to change (i.e. not the current witness). *)
  val old_levels_messages : t -> history_proof

  (** [current_witness inbox] returns the current witness of the inbox, i.e. the
      merkelized payload hash. *)
  val current_witness :
    t -> Sc_rollup_inbox_merkelized_payload_hashes_repr.Hash.t
end

(** Versioning, see {!Sc_rollup_data_version_sig.S} for more information. *)
include Sc_rollup_data_version_sig.S with type t = V1.t

include
  module type of V1
    with type level_proof = V1.level_proof
     and type history_proof = V1.history_proof
     and type t = V1.t

type serialized_proof

val serialized_proof_encoding : serialized_proof Data_encoding.t

(** [add_all_messages history inbox messages] starts a new inbox level,
    adds all the [messages], then ends the inbox level. It can
    be called even if [payloads] is empty.

    Remembers everything needed in a created [payloads_history] and [history].
    It is meant to be used by the rollup-node to reduce the risk of
    de-synchronisation between the protocol and the node.

    Adds the messages pushed by the protocol and returns a list of messages
    including them. The caller will need to execute this list of messages,
    otherwise, it might miss some internal inputs.

    The expected value of [protocol_migration_message] is either [Some
    Raw_context.protocol_migration_internal_message] (during the first
    block of this protocol) or [None]. *)
val add_all_messages :
  protocol_migration_message:
    Sc_rollup_inbox_message_repr.internal_inbox_message option ->
  predecessor_timestamp:Time.t ->
  predecessor:Block_hash.t ->
  History.t ->
  t ->
  Sc_rollup_inbox_message_repr.t list ->
  (Sc_rollup_inbox_merkelized_payload_hashes_repr.History.t
  * History.t
  * t
  * Sc_rollup_inbox_merkelized_payload_hashes_repr.t
  * Sc_rollup_inbox_message_repr.t list)
  tzresult

(** [add_messages_no_history payloads witness] updates the [witness] by
    inserting the [payloads]. *)
val add_messages_no_history :
  Sc_rollup_inbox_message_repr.serialized list ->
  Sc_rollup_inbox_merkelized_payload_hashes_repr.t ->
  Sc_rollup_inbox_merkelized_payload_hashes_repr.t tzresult

(** Used at the beginning of a refutation game to create the
    snapshot against which proofs in that game must be valid.

    One important note:
    It takes the snapshot of the inbox for the current level. The snapshot
    points to the inbox at the *beginning* of the current block level. This
    prevents to create a mid-level snapshot for a refutation game if new
    messages are added before and/or after in the same block. *)
val take_snapshot : t -> history_proof

(** An inbox proof has three parameters:

    - the [starting_point], of type [Raw_level_repr.t * Z.t], specifying
      a location in the inbox ;

    - the [message], of type [Sc_rollup_PVM_sig.input option] ;

    - and a reference [snapshot] inbox.

    A valid inbox proof implies the following semantics: beginning at
    [starting_point] and reading forward through [snapshot], the first
    message you reach will be [message].

    Usually this is fairly simple because there will actually be a
    message at the location specified by [starting_point]. But in some
    cases [starting_point] is past the last message within a level,
    and then the inbox proof's verification assumes that the next input
    is the SOL of the next level, if not beyond the snapshot.
*)
type proof

val pp_proof : Format.formatter -> proof -> unit

val to_serialized_proof : proof -> serialized_proof

val of_serialized_proof : serialized_proof -> proof option

(** See the docstring for the [proof] type for details of proof semantics.

    [verify_proof starting_point inbox_snapshot proof] will return the third
    parameter of the proof, [message], iff the proof is valid. *)
val verify_proof :
  Raw_level_repr.t * Z.t ->
  history_proof ->
  proof ->
  Sc_rollup_PVM_sig.inbox_message option tzresult

(** [produce_proof ~get_payloads_history ~get_history inbox (level, counter)]
    creates an inbox proof proving the first message after the index [counter]
    at location [level]. This will fail if the [get_payloads_history] given
    doesn't have sufficient data (it needs to be run on an with a full
    history). *)
val produce_proof :
  get_payloads_history:
    (Sc_rollup_inbox_merkelized_payload_hashes_repr.Hash.t ->
    Sc_rollup_inbox_merkelized_payload_hashes_repr.History.t Lwt.t) ->
  get_history:(Hash.t -> history_proof option Lwt.t) ->
  history_proof ->
  Raw_level_repr.t * Z.t ->
  (proof * Sc_rollup_PVM_sig.inbox_message option) tzresult Lwt.t

(** [init_witness_no_history] initializes the witness for a new inbox level
    by adding the first input, i.e. [Start_of_level]. *)
val init_witness_no_history : Sc_rollup_inbox_merkelized_payload_hashes_repr.t

(** [add_info_per_level_no_history] adds the input [Info_per_level]. *)
val add_info_per_level_no_history :
  predecessor_timestamp:Time.t ->
  predecessor:Block_hash.t ->
  Sc_rollup_inbox_merkelized_payload_hashes_repr.t ->
  Sc_rollup_inbox_merkelized_payload_hashes_repr.t

(** [finalize_inbox_level payloads_history history inbox level_witness] updates
    the current inbox's level witness by adding [EOL], and archives the current
    level. *)
val finalize_inbox_level_no_history :
  t -> Sc_rollup_inbox_merkelized_payload_hashes_repr.t -> t

(** [genesis ~protocol_migration_message ~timestamp ~predecessor
    level] initializes the inbox at some given [level] with: [SOL],
    [protocol_migration_message], [Info_per_level {timestamp;
    predecessor}] and [EOL] inside.

    The expected value of [protocol_migration_message] is
    [Raw_context.protocol_migration_internal_message]. *)
val genesis :
  protocol_migration_message:Sc_rollup_inbox_message_repr.serialized ->
  predecessor_timestamp:Time.t ->
  predecessor:Block_hash.t ->
  Raw_level_repr.t ->
  t

module Internal_for_tests : sig
  (** Given a inbox [A] at some level [L] and another inbox [B] at some level
      [L' >= L], an [inclusion_proof] guarantees that [A] is an older version of
      [B].

      To be more precise, an [inclusion_proof] guarantees that the previous
      levels [witness]s of [A] are included in the previous levels [witness]s of
      [B]. The current [witness] of [A] and [B] are not considered.

      The size of this proof is O(log2 (L' - L)). *)
  type inclusion_proof = history_proof list

  val pp_inclusion_proof : Format.formatter -> inclusion_proof -> unit

  (** [produce_inclusion_proof get_history a b] exploits [get_history]
      to produce a self-contained proof that [a] is an older version of [b]. *)
  val produce_inclusion_proof :
    (Hash.t -> history_proof option Lwt.t) ->
    history_proof ->
    Raw_level_repr.t ->
    (inclusion_proof * history_proof) tzresult Lwt.t

  (** [verify_inclusion_proof proof snapshot] returns [A] iff [proof] is a minimal
      and valid proof that [A] is included in [snapshot], fails otherwise. [A] is
      part of the proof. *)
  val verify_inclusion_proof :
    inclusion_proof -> history_proof -> history_proof tzresult

  type payloads_proof = {
    proof : Sc_rollup_inbox_merkelized_payload_hashes_repr.proof;
    payload : Sc_rollup_inbox_message_repr.serialized option;
  }

  val pp_payloads_proof : Format.formatter -> payloads_proof -> unit

  val produce_payloads_proof :
    (Sc_rollup_inbox_merkelized_payload_hashes_repr.Hash.t ->
    Sc_rollup_inbox_merkelized_payload_hashes_repr.History.t Lwt.t) ->
    Sc_rollup_inbox_merkelized_payload_hashes_repr.Hash.t ->
    index:Z.t ->
    payloads_proof tzresult Lwt.t

  val verify_payloads_proof :
    payloads_proof ->
    Sc_rollup_inbox_merkelized_payload_hashes_repr.Hash.t ->
    Z.t ->
    Sc_rollup_inbox_message_repr.serialized option tzresult

  (** Allows to create a dumb {!serialized_proof} from a string, instead of
      serializing a proof with {!to_serialized_proof}. *)
  val serialized_proof_of_string : string -> serialized_proof

  val get_level_of_history_proof : history_proof -> Raw_level_repr.t

  type level_proof = {
    hash : Sc_rollup_inbox_merkelized_payload_hashes_repr.Hash.t;
    level : Raw_level_repr.t;
  }

  val level_proof_of_history_proof : history_proof -> level_proof

  val expose_proof : proof -> inclusion_proof * payloads_proof

  val make_proof : inclusion_proof -> payloads_proof -> proof
end

type inbox = t
