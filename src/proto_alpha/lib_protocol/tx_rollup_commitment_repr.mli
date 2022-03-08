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
module Commitment_hash : sig
  val commitment_hash : string

  include S.HASH
end

(** The hash of the result of a layer-2 operation: that is, the hash
    of [(l2_ctxt_hash ^ withdraw_hash)] where [l2_ctxt_hash] is the Merkle
    tree root of the L2 context after any message (ie. deposit or batch),
    and [withdraw_hash] is a [Tx_rollup_withdraw_repr.withdrawals_merkle_root] *)
module Message_result_hash : S.HASH

type message_result = {
  context_hash : Context_hash.t;
  withdrawals_merkle_root : Tx_rollup_withdraw_repr.withdrawals_merkle_root;
}

val message_result_encoding : message_result Data_encoding.t

(** [hash_message_result result] computes the [Message_result_hash.t]
    of the given context hash and withdraw list hash, that is
    [hash(result.context_hash @ result.withdrawals_merkle_root))].  *)
val hash_message_result : message_result -> Message_result_hash.t

val pp_message_result_hash : Format.formatter -> Message_result_hash.t -> unit

(** [empty_l2_context_hash] is the context hash of the layer-2 context
    just after its origination. *)
val empty_l2_context_hash : Context_hash.t

(** [initial_message_result_hash] is equal to

{[
hash_message_result
  {
    context_hash = empty_l2_context_hash;
    withdrawals_merkle_root = Tx_rollup_withdraw_repr.empty_withdrawals_merkle_root;
  }
]} *)
val initial_message_result_hash : Message_result_hash.t

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
  level : Tx_rollup_level_repr.t;
  messages : Message_result_hash.t list;
  predecessor : Commitment_hash.t option;
  inbox_hash : Tx_rollup_inbox_repr.hash;
}

include Compare.S with type t := t

val pp : Format.formatter -> t -> unit

val encoding : t Data_encoding.t

val hash : t -> Commitment_hash.t

(** [check_message_result commitment result message_index] returns true
    if the message result hash of the batch in [commitment] indexed by
    [message_index] corresponds to the hash of [result]. *)
val check_message_result : t -> message_result -> message_index:int -> bool

module Index : Storage_description.INDEX with type t = Commitment_hash.t

module Submitted_commitment : sig
  (** When a commitment is submitted, we store the [committer] and the
      block the commitment was [submitted_at] along with the
      [commitment] itself with its hash. *)
  type nonrec t = {
    commitment : t;
    commitment_hash : Commitment_hash.t;
    committer : Signature.Public_key_hash.t;
    submitted_at : Raw_level_repr.t;
    finalized_at : Raw_level_repr.t option;
  }

  val encoding : t Data_encoding.t
end
