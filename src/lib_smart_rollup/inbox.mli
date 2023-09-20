(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Smart rollup shared inbox representation, adapted from a subset of
    [src/proto_alpha/lib_protocol/sc_rollup_inbox_repr.mli]. *)

module Hash = Inbox_hash

module Skip_list : Skip_list.S

module V1 : sig
  type level_proof = {
    hash : Tezos_crypto.Hashed.Smart_rollup_merkelized_payload_hashes_hash.t;
    level : int32;
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
  type t = {level : int32; old_levels_messages : history_proof}

  val pp : Format.formatter -> t -> unit

  val equal : t -> t -> bool

  val hash : t -> Hash.t

  val encoding : t Data_encoding.t

  (** [inbox_level inbox] returns the maximum level of message insertion in
      [inbox] or its initial level. *)
  val inbox_level : t -> int32

  val pp_history_proof : Format.formatter -> history_proof -> unit

  val history_proof_encoding : history_proof Data_encoding.t

  val equal_history_proof : history_proof -> history_proof -> bool

  (** [old_levels_messages inbox] returns the latest skip list cell of the inbox
      history that is not up to change (i.e. not the current witness). *)
  val old_levels_messages : t -> history_proof

  (** [current_witness inbox] returns the current witness of the inbox, i.e. the
      merkelized payload hash. *)
  val current_witness :
    t -> Tezos_crypto.Hashed.Smart_rollup_merkelized_payload_hashes_hash.t
end

include Versioned_data.S with type t = V1.t

include
  module type of V1
    with type level_proof = V1.level_proof
     and type history_proof = V1.history_proof
     and type t = V1.t
