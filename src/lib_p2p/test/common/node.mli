(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* This module creates simple p2p nodes. The others parts of the shell are
   disabled.
   The function [detach_nodes] creates a network of nodes.
   The message [Ping] is the unique none-p2p message used by nodes.
   The function [sync] is used to create a barrier for all the nodes of a
   network. *)

(** Simple p2p message used by the nodes.

    - [Ping] is a none-message used by nodes in simple communication
    tests.
    - [BigPing] is used to add arbitrary long data (similar to
    mempools sent in the actual network) to test bandwidth and chunks
    splitting.*)
type message = Ping | BigPing of Operation_hash.t list

type metadata = Metadata

(** [t] is a simple p2p nodes. *)
type t = {
  iteration : int ref;
  channel : (unit, unit) Process.Channel.t;
  connect_handler : (message, metadata, metadata) P2p_connect_handler.t;
  pool : (message, metadata, metadata) Tezos_p2p.P2p_pool.t;
  watcher : P2p_connection.P2p_event.t Lwt_watcher.input;
  trigger : P2p_trigger.t;
  points : P2p_point.Id.t list;
  trusted_points : P2p_point.Id.t list;
}

(** [sync node] join [node] to a synchronization barrier. *)
val sync : t -> unit tzresult Lwt.t

val default_ipv6_addr : P2p_addr.t

(** [gen_points npoints ~port addr] generated [npoints] points. If
    [port] is not specified, it loops (for at most [max_iterations] )
    and generates points randomly until it finds [npoints] that are
    not currently used. *)
val gen_points :
  ?max_iterations:int -> int -> ?port:int -> P2p_addr.t -> P2p_point.Id.t list

(** [detach_nodes f points] creates a network with one node for each [points].
    [f] is the behavior of each node.
    If the network is alive after [?timeout] seconds, if any is provided, the
    [Timeout] error is returned.
    [?prefix] associates a prefix used for logs to each [nodes].
    [?min_connections], [?max_connections], [?max_incoming_connections],
    [?p2p_versions] and [?msg_config] are used to configure the connect handler
    of each nodes. [?trusted] is used to configure the pool of each nodes. *)
val detach_nodes :
  ?timeout:float ->
  ?prefix:(int -> string) ->
  ?min_connections:(int -> int) ->
  ?max_connections:(int -> int) ->
  ?max_incoming_connections:(int -> int) ->
  ?p2p_versions:(int -> P2p_version.t list) ->
  ?msg_config:(int -> message P2p_params.message_config) ->
  ?trusted:(int -> P2p_point.Id.t list -> P2p_point.Id.t list) ->
  (int -> t -> unit tzresult Lwt.t) ->
  P2p_point.Id.t list ->
  unit tzresult Lwt.t
