(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Types of the abstract data types (ADT) which parameterize the P2p layer.

  Many types used in the P2p layer are parameterized by three type parameters:
    - ['msg]: type of messages exchanged between peers
    - ['peer_meta]: type of the metadata associated with peers (score, etc.)
    - ['conn_meta]: type of the metadata associated with connections

  These types are kept abstract from the P2p layer. It can only operate on
  them via a set of functions packed in a "configuration" record
  passed by the upper layer (see also [P2p] and [P2p.create]).

  This module defines the type of these configuration records. *)

(** Metadata for a peer *)
type 'peer_meta peer_meta_config = {
  peer_meta_encoding : 'peer_meta Data_encoding.t;
  peer_meta_initial : unit -> 'peer_meta;  (** Constructor *)
  score : 'peer_meta -> float;  (** Score of a peer, used for ordering *)
}

(** Metadata for a connection. *)
type 'conn_meta conn_meta_config = {
  conn_meta_encoding : 'conn_meta Data_encoding.t;
  conn_meta_value : unit -> 'conn_meta;  (** Constructor *)
  private_node : 'conn_meta -> bool;
      (** Returns true if peer at the other end of the connection is in private
      mode *)
}

type 'msg app_message_encoding =
  | Encoding : {
      tag : int;
      title : string;
      encoding : 'a Data_encoding.t;
      wrap : 'a -> 'msg;
      unwrap : 'msg -> 'a option;
      max_length : int option;
    }
      -> 'msg app_message_encoding

(** Application-level messages encoding, and version parameters *)
type 'msg message_config = {
  encoding : 'msg app_message_encoding list;  (** Encoding of the messages. *)
  chain_name : Distributed_db_version.Name.t;
      (** Identifier for this P2p protocol when establishing session. *)
  distributed_db_versions : Distributed_db_version.t list;
      (** List of versions supported by this P2p protocol. *)
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/4593
         These last two fields aren't logically related to the `msg type,
         they should be moved somewhere else *)
}
