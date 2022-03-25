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

(** A specialized Blake2B implementation for hashing commitments with
    "toc1" as a base58 prefix *)
module Hash : sig
  val commitment_hash : string

  include S.HASH
end

module Merkle_hash : S.HASH

module Merkle :
  Merkle_list.T
    with type elt = Tx_rollup_message_result_hash_repr.t
     and type h = Merkle_hash.t

(** A commitment describes the interpretation of the messages stored in the
    inbox of a particular [level], on top of a particular layer-2 context.

    It includes one Merkle tree root for each of the [batches]. It has
    a [predecessor], which is the identifier of the commitment for the
    previous inbox. The [predecessor] is used to get the Merkle root
    of the layer-2 context before any inboxes are processed. If
    [predecessor] is [None], the commitment is for the first inbox
    with messages in this rollup, and the initial Merkle root is the
    empty tree. *)
type 'a template = {
  level : Tx_rollup_level_repr.t;
  messages : 'a;
  predecessor : Hash.t option;
  inbox_merkle_root : Tx_rollup_inbox_repr.Merkle.root;
}

module Compact : sig
  type excerpt = {
    count : int;
    root : Merkle.h;
    last_result_message_hash : Tx_rollup_message_result_hash_repr.t;
  }

  type t = excerpt template

  val pp : Format.formatter -> t -> unit

  val encoding : t Data_encoding.t

  val hash : t -> Hash.t
end

module Full : sig
  type t = Tx_rollup_message_result_hash_repr.t list template

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit

  val compact : t -> Compact.t
end

module Index : Storage_description.INDEX with type t = Hash.t

module Submitted_commitment : sig
  (** When a commitment is submitted, we store the [committer] and the
      block the commitment was [submitted_at] along with the
      [commitment] itself with its hash. *)
  type nonrec t = {
    commitment : Compact.t;
    commitment_hash : Hash.t;
    committer : Signature.Public_key_hash.t;
    submitted_at : Raw_level_repr.t;
    finalized_at : Raw_level_repr.t option;
  }

  val encoding : t Data_encoding.t
end
