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

type t

val create :
  ?path:string ->
  ?runner:Runner.t ->
  ?data_dir:string ->
  ?addr:string ->
  ?dormant_mode:bool ->
  ?color:Log.Color.t ->
  ?event_pipe:string ->
  ?name:string ->
  rollup_id:string ->
  rollup_genesis:string ->
  ?operator:string ->
  Client.t ->
  Node.t ->
  t

(** Returns the node's endpoint. *)
val endpoint : t -> string

(** Wait until the node is ready.

    More precisely, wait until a [node_is_ready] event occurs.
    If such an event already occurred, return immediately. *)
val wait_for_ready : t -> unit Lwt.t

(** Wait for a given Tezos chain level.

    More precisely, wait until the rollup node have successfully
    validated a block of given [level], received from the Tezos node
    it is connected to.
    If such an event already occurred, return immediately. *)
val wait_for_tezos_level : t -> int -> int Lwt.t

(** Connected to a tezos node.
    Returns the name of the configuration file. *)
val config_init : t -> string -> string -> string Lwt.t

(** [run node] launches the given transaction rollup node. *)
val run : t -> unit Lwt.t

(** See [Daemon.Make.terminate]. *)
val terminate : ?kill:bool -> t -> unit Lwt.t

(** Get the RPC address given as [--rpc-addr] to a node. *)
val rpc_addr : t -> string

module Inbox : sig
  type l2_context_hash = {irmin_hash : string; tree_hash : string}

  type message = {
    message : JSON.t;
    result : JSON.t;
    l2_context_hash : l2_context_hash;
  }

  type t = {contents : message list; cumulated_size : int}
end

(* FIXME/TORU: This is a temporary way of querying the node without
   tx_rollup_client. This aims to be replaced as soon as possible by
   the dedicated client's RPC. *)
module Client : sig
  val get_inbox : tx_node:t -> block:string -> Inbox.t Lwt.t

  val get_balance :
    tx_node:t ->
    block:string ->
    ticket_id:string ->
    tz4_address:string ->
    int Lwt.t

  val get_queue : tx_node:t -> JSON.t Lwt.t

  val get_transaction_in_queue : tx_node:t -> string -> JSON.t Lwt.t

  val get_block : tx_node:t -> block:string -> JSON.t Lwt.t

  val get_merkle_proof :
    tx_node:t -> block:string -> message_pos:string -> JSON.t Lwt.t
end
