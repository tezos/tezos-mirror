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

type error += (* `Branch *) Commitment_hash_already_submitted

type error += (* `Branch *) Two_commitments_from_one_committer

type error += (* `Branch *) Wrong_commitment_predecessor_level

type error += (* `Temporary *) Missing_commitment_predecessor

type error += (* `Branch *) Wrong_batch_count

type error += (* `Temporary *) Commitment_too_early

(** A specialized Blake2B implementation for hashing commitments with
    "toc1" as a base58 prefix *)
module Commitment_hash : sig
  val commitment_hash : string

  include S.HASH
end

module Commitment : sig
  type batch_commitment = {root : bytes}

  val batch_commitment_equal : batch_commitment -> batch_commitment -> bool

  (* A commitment describes the interpretation of the messages stored in the
     inbox of a particular [level], on top of a particular layer-2 context.

     It includes one Merkle tree root for each of the [batches].  It has
     a [predecessor], which is the identifier of the commitment for the
     previous inbox. The [predecessor] is used to get the Merkle root of the
     layer-2 context before any inboxes are processed. If [predecessor] is
     [None], the commitment is for the first inbox with messages in this rollup,
     and the initial Merkle root is the empty tree. *)
  type t = {
    level : Raw_level_repr.t;
    batches : batch_commitment list;
    predecessor : Commitment_hash.t option;
  }

  val ( = ) : t -> t -> bool

  val pp : Format.formatter -> t -> unit

  val encoding : t Data_encoding.t

  val hash : t -> Commitment_hash.t

  module Index : Storage_description.INDEX with type t = Commitment_hash.t
end

(** A [pending_commitment] is a commitment which has not yet become final.
    The [hash] is redundant and is only stored to reduce computation. We
    track the level that the commitment was submitted at; after the
    finality period, it will become final if it has not been rejected. *)
type pending_commitment = {
  commitment : Commitment.t;
  hash : Commitment_hash.t;
  committer : Signature.Public_key_hash.t;
  submitted_at : Raw_level_repr.t;
}

val pp_pending_commitment : Format.formatter -> pending_commitment -> unit

(** This is the type that we store, ordered in reverse priority order. *)
type t = pending_commitment list

val encoding : t Data_encoding.t

val empty : t

(** [append commitments contract commitment level] appends a new commitment
     to a list of commitments.  It fails if the same account has already
     made a commitment, or if any account has already made this exact
     commitment. *)
val append :
  t ->
  Signature.Public_key_hash.t ->
  Commitment.t ->
  Raw_level_repr.t ->
  t tzresult

(** [commitment_exists commitments t hash] returns true if a commitment
    with this [hash] already exists in this list. *)
val commitment_exists : t -> Commitment_hash.t -> bool

(** [commitment_with_committer_exists commitments t contract] returns
    true if a commitment by this [contract] already exists in this list. *)
val commitment_with_committer_exists : t -> Signature.Public_key_hash.t -> bool
