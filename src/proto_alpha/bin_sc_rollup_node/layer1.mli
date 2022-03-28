(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** This module maintains information about the layer 1 chain.

   This module follows the evolution of the layer 1 chain by
   subscribing to the head monitoring RPC offered by the Tezos node.

   When started, it provides a stream of events that can be used to be
   informed when the head of the layer 1 has changed or when a chain
   reorganization occurred.

*)
type head = Head of {hash : Block_hash.t; level : int32}

val head_encoding : head Data_encoding.t

type chain_event =
  | SameBranch of {new_head : head; intermediate_heads : head list}
      (** The [new_head] follows the current branch. [intermediate_heads]
          have been included since the previous synchronization with Tezos
          node. *)
  | Rollback of {new_head : head}
      (** A chain reorganization occurred since the previous
          synchronization. The rollback set [new_head] to an old block. *)

(** [start configuration cctxt store] returns a stream of [chain_event]
   obtained from the monitoring of the Tezos node set up by the client
   [cctxt]. The layer 1 state is stored in the data directory declared
   in [configuration]. *)
val start :
  Configuration.t ->
  Protocol_client_context.full ->
  Store.t ->
  chain_event Lwt_stream.t tzresult Lwt.t

(** [current_head_hash store] is the current hash of the head of the
   Tezos chain as far as the smart-contract rollup node knows from the
   latest synchronization. Returns [None] if no synchronization has
   ever been made. *)
val current_head_hash : Store.t -> Block_hash.t option Lwt.t

(** [current_level store] is the current level of the Tezos chain as far
   as the smart-contract rollup node knows from the latest
   synchronization. Returns [None] if no synchronization has ever been
   made. *)
val current_level : Store.t -> int32 option Lwt.t

(** [predecessor store head] returns the hash of the head's predecessor block] *)
val predecessor : Store.t -> head -> Block_hash.t Lwt.t

(** [genesis_hash] is the hash of the genesis block of the chain. *)
val genesis_hash : Block_hash.t

(** [processed chain_event] emits a log event to officialize the
    processing of some layer 1 [chain_event]. *)
val processed : chain_event -> unit Lwt.t
