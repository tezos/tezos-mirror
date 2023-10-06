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

(* [create sock ~point_info ~peer_info msg_pipe canceler greylist callbacks
   disable_peer_discovery version] creates a connection over [sock] with the
   corresponding [point_info], [peer_info] and negotiated [version].

   [msg_pipe] is the pipe where the received messages are read.
   [canceler] is used to initiate or propagate the closure of the connection.
   [greylist] is a callback to greylist the peer id of the connection.
   [callback] is a record that contains the functions to call to handle each
              kind of received messages.
   If [disable_peer_discovery] is true, then the received [Advertise] and
   [Bootstrap] messages are ignored. Requests to send a [Bootstrap] message are
   ignored and requests to send an [Advertise] message fail.
*)
val create :
  conn:('msg P2p_message.t, 'conn) P2p_socket.t ->
  point_info:('msg, 'peer, 'conn) t P2p_point_state.Info.t option ->
  peer_info:(('msg, 'peer, 'conn) t, 'peer, 'conn) P2p_peer_state.Info.t ->
  messages:(int * 'msg) Lwt_pipe.Maybe_bounded.t ->
  canceler:Lwt_canceler.t ->
  greylister:(unit -> unit) ->
  callback:'msg P2p_answerer.t ->
  disable_peer_discovery:bool ->
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

(** [encode t messsage] encodes a message to be used with
    [write_encoded_now]. It is particularly useful to avoid encoding
    several times the same message.*)
val encode :
  ('msg, 'peer, 'conn) t ->
  'msg ->
  'msg P2p_message.t P2p_socket.encoded_message tzresult

(** [write_encoded_now t msg] is [P2p_socket.write_now t' msg] where
    [t'] is the internal [P2p_socket.t] inside [t] and [msg] has been
    pre-encoded using [encode].
    [msg] will be overwritten and should not be used after this
    invocation.
*)
val write_encoded_now :
  ('msg, 'peer, 'conn) t ->
  'msg P2p_message.t P2p_socket.encoded_message ->
  bool tzresult

val equal_sock : ('msg, 'peer, 'conn) t -> ('msg, 'peer, 'conn) t -> bool

(* TODO: https://gitlab.com/tezos/tezos/-/issues/4615
   Properly document disconnect/close. Check they are properly used
   and if we really need both. *)

val disconnect :
  ?wait:bool ->
  reason:P2p_disconnection_reason.t ->
  ('msg, 'peer, 'conn) t ->
  unit Lwt.t

val disconnect_reason :
  ('msg, 'peer, 'conn) t -> P2p_disconnection_reason.t option

val close :
  reason:P2p_disconnection_reason.t -> ('msg, 'peer, 'conn) t -> unit Lwt.t

(** Returns the network version that will be used for this connection.
   This network version is the best version compatible with the versions
   supported by ours and the remote peer. *)
val negotiated_version : ('msg, 'peer, 'conn) t -> Network_version.t

(**/**)

module Internal_for_tests : sig
  val raw_write_sync : ('msg, 'peer, 'conn) t -> Bytes.t -> unit tzresult Lwt.t
end
