(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(** Type of a connection to a peer, parametrized by the type of
    messages exchanged as well as meta-information associated to a
    peer and a connection. It wraps a [P2p_socket.t],
    adding meta-information and data-structures describing a more
    fine-grained logical state of the connection. It also set up
    an answering worker that responds to the messages [P2p_message.t] using
    the callback functions of a [P2p_answerer.t]. *)

type ('msg, 'peer, 'conn) t

val create :
  conn:('msg P2p_message.t, 'conn) P2p_socket.t ->
  point_info:('msg, 'peer, 'conn) t P2p_point_state.Info.t option ->
  peer_info:(('msg, 'peer, 'conn) t, 'peer, 'conn) P2p_peer_state.Info.t ->
  messages:(int * 'msg) Lwt_pipe.Maybe_bounded.t ->
  canceler:Lwt_canceler.t ->
  greylister:(unit -> unit) ->
  callback:'msg P2p_answerer.t ->
  Network_version.t ->
  ('msg, 'peer, 'conn) t

val peer_id : ('msg, 'peer, 'conn) t -> P2p_peer.Id.t

(** [private_node t] returns 'true' if the node associated to this
    connection is in private mode *)
val private_node : ('msg, 'peer, 'conn) t -> bool

(** [trusted_node t] returns 'true' if the node associated to this
    connection is trusted *)
val trusted_node : ('msg, 'peer, 'conn) t -> bool

val info : ('msg, 'peer, 'conn) t -> 'conn P2p_connection.Info.t

val local_metadata : ('msg, 'peer, 'conn) t -> 'conn

val remote_metadata : ('msg, 'peer, 'conn) t -> 'conn

(** [stat t] is a snapshot of current bandwidth usage for [t]. *)
val stat : ('msg, 'peer, 'conn) t -> P2p_stat.t

(** [read t] returns a message popped from [t]'s app message
    queue, or fails with [Connection_closed]. *)
val read : ('msg, 'peer, 'conn) t -> 'msg tzresult Lwt.t

(** [is_readable t] returns when there is at least one message
    ready to be read. *)
val is_readable : ('msg, 'peer, 'conn) t -> unit tzresult Lwt.t

val write_swap_request :
  ('msg, 'peer, 'conn) t -> P2p_point.Id.t -> P2p_peer.Id.t -> bool tzresult

val write_swap_ack :
  ('msg, 'peer, 'conn) t -> P2p_point.Id.t -> P2p_peer.Id.t -> bool tzresult

val write_bootstrap : ('msg, 'peer, 'conn) t -> bool tzresult

(** [write t msg] is [P2p_socket.write t' msg] where [t'] is the internal
    [P2p_socket.t] inside [t]. *)
val write : ('msg, 'peer, 'conn) t -> 'msg -> unit tzresult Lwt.t

(** [write_sync t msg] is [P2p_socket.write_sync t' msg] where [t'] is
    the internal [P2p_socket.t] inside [t]. *)
val write_sync : ('msg, 'peer, 'conn) t -> 'msg -> unit tzresult Lwt.t

(** [write_now t msg] is [P2p_socket.write_now t' msg] where
    [t'] is the internal [P2p_socket.t] inside [t]. *)
val write_now : ('msg, 'peer, 'conn) t -> 'msg -> bool tzresult

val equal_sock : ('msg, 'peer, 'conn) t -> ('msg, 'peer, 'conn) t -> bool

val disconnect : ?wait:bool -> ('msg, 'peer, 'conn) t -> unit Lwt.t

(** Returns the network version that will be used for this connection.
   This network version is the best version compatible with the versions
   supported by ours and the remote peer. *)
val negotiated_version : ('msg, 'peer, 'conn) t -> Network_version.t

(* TODO properly document disconnect/close. Check they are properly used
   and if we really need both. *)

val close : ('msg, 'peer, 'conn) t -> unit Lwt.t

(**/**)

val raw_write_sync : ('msg, 'peer, 'conn) t -> Bytes.t -> unit tzresult Lwt.t

(**/**)
