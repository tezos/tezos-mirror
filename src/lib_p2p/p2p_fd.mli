(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Type describing an opening failure for the listening socket. *)
type listening_socket_open_failure = {
  reason : Unix.error;  (** The error we are re-raising *)
  address : P2p_addr.t;  (** The interface we are trying to listen to *)
  port : int;  (** The port we are trying to listen to *)
}

(** Type of an error in case of the listening
    socket fails to open. *)
type error += Failed_to_open_listening_socket of listening_socket_open_failure

(** This module defines a type [t] which wraps a file descriptor. Most
    functions simply call the underlying file descriptor function and generate
    logs with prefix "p2p.fd". *)

type t

(** [`Unexpected_error] is a generic case of error that is used by default
    to represent a raising error that cannot be identified as a known error
    case.

    Note: This errror is needed in general to address unexpetected exception
    that could be raised during fd processing. Though, it is not encouraged to
    use it. It is a better practice to catch exceptions and convert them explicitly
    to a dedicated (polymorphic) variant type. *)
type unexpected_error = [`Unexpected_error of exn]

(** [read_write_error] is used by the functions [read] or [write] in case of error.

   [`Connection_closed_by_peer] is returned when the socket returns 0 bytes read
   or written.

   [`Connection_lost] is returned when the connection was closed
   because of a timeout or another problem related to the remote peer.

   [`Connection_locally_closed] is returned when the connection's socket
   has been locally closed before we try to access it. *)
type read_write_error =
  [ `Connection_closed_by_peer
  | `Connection_lost of exn
  | `Connection_locally_closed
  | unexpected_error ]

type connect_error = [`Connection_failed | unexpected_error]

(** [accept_error] is used in case of error while trying to [accept] some connection.

    [`System_error] represents system-wide errors which are related to the state of
    whole process or computer. These are errors that will probably reoccur at
    next [accept] and must be treated accordingly (for example by giving some time to
    the system to recover).

    [`Socket_error ] represents socket-specific errors which are related to the one
    connection that the function attempted to accept. These are usually temporary
    errors related to network delays or remote host responsiveness .
    For most socket-errors you can just log them and call accept again to be
    ready for the next connection. *)
type accept_error =
  [`System_error of exn | `Socket_error of exn | unexpected_error]

(** Pretty printer for read_write_error. *)
val pp_read_write_error : Format.formatter -> read_write_error -> unit

(** [id t] returns a unique, positive, identifier for t.
    Identifiers are generated sequentially at creation time. *)
val id : t -> int

(** [set_point ~point t] sets the point where [fd] is or will be connected to.
   *)
val set_point : point:P2p_point.Id.t -> t -> unit

(** [set_peer_id ~peer_id t] sets the peer id where [fd] is or will be
    connected to. *)
val set_peer_id : peer_id:P2p_peer.Id.t -> t -> unit

(** [close ?reason fd] closes the connection and the underlying fd.
    It is idempotent. *)
val close : ?reason:P2p_disconnection_reason.t -> t -> unit Lwt.t

(** Stores in the fd a reason for which it will be closed in a near future. *)
val add_closing_reason : reason:P2p_disconnection_reason.t -> t -> unit

(** [read fd buf ofs len] reads up to [len] bytes from [fd], and writes them to
    [buf], starting at offset [ofs].
    If the operation leads to a [`Connection_lost] error it is guaranteed that the
    underlying socket has been closed. *)
val read : t -> Bytes.t -> int -> int -> (int, read_write_error) result Lwt.t

(** [write fd buf] writes all bytes from [buf] to [fd].
    If the operation leads to a [`Connection_lost] error it is guaranteed that the
    underlying socket has been closed. *)
val write : t -> Bytes.t -> (unit, read_write_error) result Lwt.t

(** Returns a fresh fd. This call always succeed. *)
val socket : unit -> t Lwt.t

(** [create_listening_socket ?reuse_port ~backlog ?addr port] creates
    a socket that listens on [addr] or [Ipaddr.V6.unspecified] if
    [addr] is not provided and on [port].

   [reuse_port] is used to set Unix socket option [SO_REUSEPORT]. If
   [reuse_port] is not provided this option is set to false.
   [SO_REUSEADDR] is set to true.

   [backlog] set the maximum number of pending connections. *)
val create_listening_socket :
  ?reuse_port:bool ->
  backlog:int ->
  ?addr:Ipaddr.V6.t ->
  int ->
  Lwt_unix.file_descr tzresult Lwt.t

(** [connect fd addr] connects [fd] to [addr]. If there is an error, [fd] is
    closed. *)
val connect : t -> Lwt_unix.sockaddr -> (unit, connect_error) result Lwt.t

(** [accept sock] accepts connections on socket [sock]. *)
val accept :
  Lwt_unix.file_descr -> (t * Lwt_unix.sockaddr, accept_error) result Lwt.t

module Table : Hashtbl.S with type key = t
