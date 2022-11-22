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

open Protocol
open Alpha_context

(** {2 Structure of layer 2 blocks} *)

(** A layer 2 block header contains information about the inbox and commitment
    with respect to a layer 1 block, but without the inbox content of
    messages. *)
type header = {
  block_hash : Block_hash.t;  (** Tezos block hash. *)
  level : Raw_level.t;
      (** Level of the block, corresponds to the level of the tezos block. *)
  predecessor : Block_hash.t;  (** Predecessor hash of the Tezos block. *)
  commitment_hash : Sc_rollup.Commitment.Hash.t option;
      (** Hash of this block's commitment if any was computed for it. *)
  previous_commitment_hash : Sc_rollup.Commitment.Hash.t;
      (** Previous commitment hash in the chain. If there is a commitment for this
          block, this field contains the commitment that was previously
          computed. *)
  context : Sc_rollup_context_hash.t;
      (** Hash of the layer 2 context for this block. *)
  inbox_witness : Sc_rollup.Inbox_merkelized_payload_hashes.Hash.t;
      (** Witness for the inbox for this block, i.e. the Merkle hash of payloads
          of messages. *)
  inbox_hash : Sc_rollup.Inbox.Hash.t;  (** Hash of the inbox for this block. *)
}

(** Contents of blocks which include the actual content of the inbox and
    messages.  *)
type content = {
  inbox : Sc_rollup.Inbox.t;  (** Inbox for this block. *)
  messages : Sc_rollup.Inbox_message.t list;
      (** Messages added to the inbox in this block. *)
  commitment : Sc_rollup.Commitment.t option;
      (** Commitment, if any is computed for this block. [header.commitment =
          Some h] iff [commitment = Some c] and [hash c = h]. *)
}

(** Block parameterized by a header and content. The parameters are here to
    allow to split the header and rest of the block.  *)
type ('header, 'content) block = {
  header : 'header;  (** Header of this block. *)
  content : 'content;  (** Content of the block. *)
  initial_tick : Sc_rollup.Tick.t;
      (** Initial tick of the PVM at this block, i.e. before evaluation of the
          messages. *)
  num_ticks : int64;
      (** Number of ticks produced by the evaluation of the messages in this
          block. *)
}

(** The type of layer 2 blocks. This type corresponds to what is stored on disk
    for L2 blocks. The contents is stored in separate tables because it can be
    large to read is is not always necessary. *)
type t = (header, unit) block

(** The type of layer 2 blocks including their content (inbox, messages, commitment). *)
type full = (header, content) block

(** {2 Encodings} *)

val header_encoding : header Data_encoding.t

val header_size : int

val content_encoding : content Data_encoding.t

val block_encoding :
  'header Data_encoding.t ->
  'content Data_encoding.t ->
  ('header, 'content) block Data_encoding.t

val encoding : t Data_encoding.t

val full_encoding : full Data_encoding.t

(** {2 Helper functions} *)

(** [most_recent_commitment header] returns the most recent commitment
    information at the block of with [header]. It is either the commitment for
    this block if there is one or the previous commitment otherwise. *)
val most_recent_commitment : header -> Sc_rollup.Commitment.Hash.t

(** [final_tick block] is the final tick, after evaluation of the messages in
    the [block], i.e. [block.initial_tick + block.num_ticks]. *)
val final_tick : ('a, 'b) block -> Sc_rollup.Tick.t
