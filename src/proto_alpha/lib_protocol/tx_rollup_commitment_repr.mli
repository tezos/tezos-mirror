(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxheadalpha.com>                    *)
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

type error += (* `Branch *) Wrong_commitment_predecessor_level

type error += (* `Temporary *) Missing_commitment_predecessor

type error += (* `Branch *) Wrong_batch_count

type error += (* `Temporary *) Commitment_too_early

type error += (* `Temporary *) Level_already_has_commitment of Raw_level_repr.t

type error += (* `Temporary *) Wrong_inbox_hash

(** A specialized Blake2B implementation for hashing commitments with
    "toc1" as a base58 prefix *)
module Commitment_hash : sig
  val commitment_hash : string

  include S.HASH
end

type batch_commitment = {root : bytes}

val batch_commitment_equal : batch_commitment -> batch_commitment -> bool

(** A commitment describes the interpretation of the messages stored in the
    inbox of a particular [level], on top of a particular layer-2 context.

    It includes one Merkle tree root for each of the [batches]. It has
    a [predecessor], which is the identifier of the commitment for the
    previous inbox. The [predecessor] is used to get the Merkle root
    of the layer-2 context before any inboxes are processed. If
    [predecessor] is [None], the commitment is for the first inbox
    with messages in this rollup, and the initial Merkle root is the
    empty tree. *)
type t = {
  level : Raw_level_repr.t;
  batches : batch_commitment list;
  predecessor : Commitment_hash.t option;
  inbox_hash : Tx_rollup_inbox_repr.hash;
}

include Compare.S with type t := t

val pp : Format.formatter -> t -> unit

val encoding : t Data_encoding.t

val hash : t -> Commitment_hash.t

module Index : Storage_description.INDEX with type t = Commitment_hash.t

module Submitted_commitment : sig
  (** When a commitment is submitted, we store the [committer] and the
      block the commitment was [submitted_at] along with the
      [commitment] itself. *)
  type nonrec t = {
    commitment : t;
    committer : Signature.Public_key_hash.t;
    submitted_at : Raw_level_repr.t;
  }

  val encoding : t Data_encoding.t
end
