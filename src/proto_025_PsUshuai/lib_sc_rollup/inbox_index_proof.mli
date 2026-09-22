(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

(** Index proofs for the shared inbox skip lists.

    The cells of the per-level skip list of payload hashes are authenticated by
    a hash that omits their index, so an inbox proof can carry relabelled
    indices and still verify. The path can be checked against the canonical
    shape of a skip list, but that needs the index of the cell it starts from,
    and nobody records how many messages a level holds. The missing index is
    re-derived from an {e index proof}: a path from the head cell, whose shape
    determines the head's index without trusting any index field.

    A refutation game player publishes that index proof as external messages
    batched with its refutation move, and the block-validation plugin uses it
    to reject relabelled proofs. *)

open Protocol.Alpha_context
module Hash = Sc_rollup.Inbox_merkelized_payload_hashes.Hash

(** An index proof for the per-level skip list of payload hashes: the cells of
    the path built by {!produce}, head cell first. *)
type t = Sc_rollup.Inbox_merkelized_payload_hashes.t list

(** The outcome of checking the indices disclosed by a proof. *)
type outcome =
  | Valid  (** Every disclosed index is the one the skip list prescribes. *)
  | Inconsistent_indices
      (** Some disclosed index contradicts the skip list's shape, or two cells
          of the proof share a hash without being equal. *)
  | Unconvertible_cells
      (** A cell could not be read back from its own encoding. This is a bug in
          this module rather than a malformed proof: the caller is expected to
          reject the proof it came from. *)

(** [produce history ~head] builds the index proof of [head] from [history],
    which must hold every cell of [head]'s level. Returns [None] if [history]
    is missing a cell of the path, or if the resulting proof does not verify
    against [head]. *)
val produce :
  Sc_rollup.Inbox_merkelized_payload_hashes.History.t ->
  head:Sc_rollup.Inbox_merkelized_payload_hashes.t ->
  t option

(** [verify_payloads_indices ~inclusion ~index_proof] checks the indices
    disclosed by the payload-hashes path [inclusion] of an inbox proof against
    the index [index_proof] derives for their common head cell. [inclusion] and
    [index_proof] must start at the same cell. *)
val verify_payloads_indices :
  inclusion:Sc_rollup.Inbox_merkelized_payload_hashes.t list ->
  index_proof:t ->
  outcome

(** [to_messages index_proof] encodes [index_proof] as external messages, each
    within the protocol's message size limit. [chunk_size] bounds the encoded
    bytes carried by one message; it is only meant to let tests exercise
    multi-message index proofs. *)
val to_messages : ?chunk_size:int -> t -> string list

(** [of_messages messages] reads back the index proofs carried by the messages
    of a single [Sc_rollup_add_messages] operation. Messages that are not
    chunks of an index proof are ignored, and an incomplete or undecodable
    sequence of chunks yields no index proof. *)
val of_messages : string list -> t list
