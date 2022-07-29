(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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

(** A non-compact representation of inboxes that represents complete messages
    and not their hashes. *)

open Protocol
open Alpha_context

(** Result of application of an inbox message *)
type message_result =
  | Interpreted of Tx_rollup_l2_apply.Message_result.t
      (** The message was interpreted by the rollup node but may have failed *)
  | Discarded of tztrace
      (** The message was discarded because it could not be interpreted *)

type l2_context_hash = {
  irmin_hash : Tx_rollup_l2_context_hash.t;
      (** The context hash of the commited context, used for checkout *)
  tree_hash : Context_hash.t;
      (** The tree hash is the hash of the underlying tree in the {!Context},
          used to produce proofs *)
}

(** Type of inbox message with the context hash resulting from the application
    of the message *)
type message = {
  message : Tx_rollup_message.t;
  result : message_result;
  l2_context_hash : l2_context_hash;
}

(** The type representing an inbox whose contents are the messages and not the
    hashed messages. *)
type t = message list

(** Encoding for l2 context hashes *)
val l2_context_hash_encoding : l2_context_hash Data_encoding.t

(** Encoding for inbox messages *)
val message_encoding : message Data_encoding.t

(** Encoding for inboxes *)
val encoding : t Data_encoding.t

(** Returns the Merkle root of the (contents of the) inbox. *)
val merkle_root : t -> Tx_rollup_inbox.Merkle.root

(** Returns the protocol inbox from an L2 inbox. The protocol inbox corresponds
    to the structure that is stored on L1, i.e. an inbox with Merklized
    contents. *)
val to_proto : t -> Tx_rollup_inbox.t

(** Return protocol message results for an inbox  *)
val proto_message_results : t -> Tx_rollup_message_result.t list
