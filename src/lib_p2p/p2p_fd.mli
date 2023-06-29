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

(** This module defines a type [t] which wraps a file descriptor. Most
    functions simply call the underlying file descriptor function and generate
    logs with prefix "p2p.fd". *)

type t

(* [close_reason] is used by the functions [read] or [write] in case of error.

   [`Connection_closed_by_peer] is returned when the socket returns 0 bytes read
   or written.

   [`Connection_lost] is returned when the connection was closed
   because of a timeout or another problem related to the remote peer.

   [`Connection_locally_closed] is returned when the connection's socket
   has been locally closed before we try to access it.

   [`Unexpected_error_when_closing (exn1,exn2)] is returned when two unexpected errors
   occurred: a first one [exn1] which triggered an attempt to close the socket, and a
   second one [exn2] whilst closing the socket.

   [`Unexpected_error] is returned in case of an error not caught by the
   previous cases.
*)
type close_reason =
  [ `Connection_closed_by_peer
  | `Connection_locally_closed
  | `Connection_lost of exn
  | `Unexpected_error of exn
  | `Unexpected_error_when_closing of exn * exn ]

(** Type describing an opening failure for the listening socket. *)
type listening_socket_open_failure = {
  reason : Unix.error;  (** The error we are re-raising *)
  address : P2p_addr.t;  (** The interface we are trying to listen to *)
  port : int;  (** The port we are trying to listen to *)
}

(** Type of an error in case of the listening
    socket fails to open. *)
type error += Failed_to_open_listening_socket of listening_socket_open_failure

val pp_close_reason : Format.formatter -> close_reason -> unit

(** [id t] returns a unique, positive, identifier for t. Identifiers
    are generated sequentially at creation time. *)
val id : t -> int

(** [read fd buf ofs len] reads up to [len] bytes from [fd], and writes them to
    [buf], starting at offset [ofs].
    If the operation leads to a [`Connection_lost] error it  is guaranteed that the
    underlying socket has been closed.
*)
val read : t -> Bytes.t -> int -> int -> (int, close_reason) result Lwt.t

(** [close fd] close the connection and the underlying fd.
    It is idempotent.
*)
val close : t -> (unit, [`Unexpected_error of exn]) result Lwt.t

(** [write fd buf] writes all bytes from [buf] to [fd].
    If the operation leads to a [`Connection_lost] error it is guaranteed that the
    underlying socket has been closed.
*)
val write : t -> Bytes.t -> (unit, close_reason) result Lwt.t

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

(** [connect fd addr] connect [fd] to [addr]. *)
val connect :
  t ->
  Lwt_unix.sockaddr ->
  (unit, [`Unexpected_error of exn | `Connection_refused]) result Lwt.t

(** [accept sock] accept connections on socket [sock]

    This function can fail and raise an error.

    [`System_error] : System-wide errors which are related to the state of
    whole process or computer. These are errors that will probably reoccur at
    next call and must be treated accordingly (for example by giving some time to
    the system to recover).


    [`Socket_error ] : Socket-specific errors which are related to the one
    connection that the function attempted to accept. These are usually temporary
    errors related to network delays or remote host responsiveness .
    For most socket-errors you can just log them and call accept again to be
    ready for the next connection.

    [`Unexpected_error] : These are other types of errors that can arise and not
    caught by the previous cases.
*)
val accept :
  Lwt_unix.file_descr ->
  ( t * Lwt_unix.sockaddr,
    [`System_error of exn | `Socket_error of exn | `Unexpected_error of exn] )
  result
  Lwt.t

module Table : Hashtbl.S with type key = t
