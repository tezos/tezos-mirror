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

type mode = Accuser | Batcher | Custom | Maintenance | Observer | Operator

val create :
  ?path:string ->
  ?runner:Runner.t ->
  ?data_dir:string ->
  ?addr:string ->
  ?dormant_mode:bool ->
  ?color:Log.Color.t ->
  ?event_pipe:string ->
  ?name:string ->
  mode ->
  rollup_id:string ->
  rollup_genesis:string ->
  ?operator:string ->
  ?batch_signer:string ->
  ?finalize_commitment_signer:string ->
  ?remove_commitment_signer:string ->
  ?dispatch_withdrawals_signer:string ->
  ?rejection_signer:string ->
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

val change_signers :
  ?operator:string option ->
  ?batch_signer:string option ->
  ?finalize_commitment_signer:string option ->
  ?remove_commitment_signer:string option ->
  ?dispatch_withdrawals_signer:string option ->
  ?rejection_signer:string option ->
  ?mode:mode ->
  t ->
  unit Lwt.t

(** Wait for a custom event to occur.

      Usage: [wait_for_full daemon name filter]

      If an event named [name] occurs, apply [filter] to its
      whole json, which is of the form:
      {[{
        "fd-sink-item.v0": {
          "hostname": "...",
                      "time_stamp": ...,
                      "section": [ ... ],
                      "event": { <name>: ... }
                               }
        }]}
      If [filter] returns [None], continue waiting.
      If [filter] returns [Some x], return [x].

      [where] is used as the [where] field of the [Terminated_before_event] exception
      if the daemon terminates. It should describe the constraint that [filter] applies,
      such as ["field level exists"].

      It is advised to register such event handlers before starting the daemon,
      as if they occur before being registered, they will not trigger your handler.
      For instance, you can define a promise with
      [let x_event = wait_for daemon "x" (fun x -> Some x)]
      and bind it later with [let* x = x_event]. *)
val wait_for_full :
  ?where:string -> t -> string -> (JSON.t -> 'a option) -> 'a Lwt.t

(** Same as [wait_for_full] but ignore metadata from the file descriptor sink.

      More precisely, [filter] is applied to the value of field
      ["fd-sink-item.v0"."event".<name>].

      If the daemon receives a JSON value that does not match the right
      JSON structure, it is not given to [filter] and the event is
      ignored. See [wait_for_full] to know what the JSON value must
      look like. *)
val wait_for : ?where:string -> t -> string -> (JSON.t -> 'a option) -> 'a Lwt.t

(** Write the configuration file for a rollup node.
    Returns the name of the configuration file. *)
val init_config : t -> string Lwt.t

val spawn_init_config : t -> Process.t

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

  val get_ticket : tx_node:t -> block:string -> ticket_id:string -> JSON.t Lwt.t

  val get_ticket_index :
    tx_node:t -> block:string -> ticket_id:string -> int Lwt.t
end
