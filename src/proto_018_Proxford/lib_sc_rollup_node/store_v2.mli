(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** This version of the store is used for the rollup nodes for protocols for and
    after Nairobi, i.e. >= 17. *)

open Protocol
open Alpha_context
open Indexed_store

include module type of struct
  include Store_v1
end

(** Storage for persisting messages downloaded from the L1 node. *)
module Messages :
  INDEXED_FILE
    with type key := Sc_rollup.Inbox_merkelized_payload_hashes.Hash.t
     and type value := Sc_rollup.Inbox_message.t list
     and type header := Block_hash.t

(** Storage for persisting inboxes. *)
module Inboxes :
  SIMPLE_INDEXED_FILE
    with type key := Sc_rollup.Inbox.Hash.t
     and type value := Sc_rollup.Inbox.t
     and type header := unit

(** Storage containing commitments and corresponding commitment hashes that the
    rollup node has knowledge of. *)
module Commitments :
  SIMPLE_INDEXED_FILE
    with type key := Sc_rollup.Commitment.Hash.t
     and type value := Sc_rollup.Commitment.t
     and type header := unit

module Protocols : sig
  type level = First_known of int32 | Activation_level of int32

  (** Each element of this type represents information we have about a Tezos
      protocol regarding its activation. *)
  type proto_info = {
    level : level;
        (** The level at which we have seen the protocol for the first time,
            either because we saw its activation or because the first block we
            saw (at the origination of the rollup) was from this protocol. *)
    proto_level : int;
        (** The protocol level, i.e. its number in the sequence of protocol
            activations on the chain. *)
    protocol : Protocol_hash.t;  (** The protocol this information concerns. *)
  }

  val proto_info_encoding : proto_info Data_encoding.t

  include SINGLETON_STORE with type value = proto_info list
end

type +'a store = {
  l2_blocks : 'a L2_blocks.t;
  messages : 'a Messages.t;
  inboxes : 'a Inboxes.t;
  commitments : 'a Commitments.t;
  commitments_published_at_level : 'a Commitments_published_at_level.t;
  l2_head : 'a L2_head.t;
  last_finalized_level : 'a Last_finalized_level.t;
  levels_to_hashes : 'a Levels_to_hashes.t;
  protocols : 'a Protocols.t;
  irmin_store : 'a Irmin_store.t;
}

include Store_sig.S with type 'a store := 'a store
