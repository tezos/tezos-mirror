(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** This module defines functions that emit the events used when the smart
    rollup node is running (see {!Daemon}). *)

open Protocol.Alpha_context

val starting_node : unit -> unit Lwt.t

val node_is_ready : rpc_addr:string -> rpc_port:int -> unit Lwt.t

(** [rollup_exists addr kind] emits the event that the smart rollup
    node is interacting with the rollup at address [addr] and of the given
    [kind]. *)
val rollup_exists : addr:Sc_rollup.t -> kind:Sc_rollup.Kind.t -> unit Lwt.t

(** [shutdown_node exit_status] emits the event that the smart rollup
    node is stopping with exit status [exit_status]. *)
val shutdown_node : int -> unit Lwt.t

(** Emits the event that the connection to the Tezos node has been lost. *)
val connection_lost : unit -> unit Lwt.t

(** [cannot_connect ~count error] emits the event that the rollup node cannot
    connect to the Tezos node because of [error] for the [count]'s time. *)
val cannot_connect : count:int -> tztrace -> unit Lwt.t

(** [wait_reconnect delay] emits the event that the rollup will wait [delay]
    seconds before attempting to reconnect to the Tezos node . *)
val wait_reconnect : float -> unit Lwt.t

(** [starting_metrics_server ~metrics_addr ~metrics_port] emits the event
    that the metrics server for the rollup node is starting. *)
val starting_metrics_server : host:string -> port:int -> unit Lwt.t

(** [metrics_ended error] emits the event that the metrics server
    has ended with a failure. *)
val metrics_ended : string -> unit Lwt.t

(** [metrics_ended error] emits the event that the metrics server
    has ended with a failure.
    (Doesn't wait for event to be emited. *)
val metrics_ended_dont_wait : string -> unit

(** [kernel_debug str] emits the event that the kernel has logged [str]. *)
val kernel_debug : string -> unit Lwt.t

(** [kernel_debug str] emits the event that the kernel has logged [str].
    (Doesn't wait for event to be emitted) *)
val kernel_debug_dont_wait : string -> unit

(** [warn_dal_enabled_no_node ()] emits a warning for when DAL is enabled in the
    protocol but the rollup node has no DAL node. *)
val warn_dal_enabled_no_node : unit -> unit Lwt.t

(** Emit event that the node is waiting for the first block of its protocol. *)
val waiting_first_block : unit -> unit Lwt.t

(** Emit event that the node received the first block of its protocol. *)
val received_first_block : Block_hash.t -> unit Lwt.t

(** [acquiring_lock ()] emits an event to indicate that the node is attempting
    to acquire a lock on the data directory. *)
val acquiring_lock : unit -> unit Lwt.t
