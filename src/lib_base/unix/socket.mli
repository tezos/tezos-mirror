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

open Error_monad

type addr =
  | Unix of string
  | Tcp of string * string * Unix.getaddrinfo_option list

(** [connect ?timeout addr] tries connecting to [addr] and returns
    the resulting socket file descriptor on success. When using TCP,
    [Unix.getaddrinfo] is used to resolve the hostname and service
    (port). The different socket addresses returned by
    [Unix.getaddrinfo] are tried sequentially, and the [?timeout]
    argument (default: 5s) governs how long it waits to get a
    connection. If a connection is not obtained in less than
    [?timeout], the connection is canceled and and the next socket
    address (if it exists) is tried. *)
val connect :
  ?timeout:Ptime.Span.t -> addr -> Lwt_unix.file_descr tzresult Lwt.t

val with_connection :
  ?timeout:Ptime.Span.t ->
  addr ->
  (Lwt_unix.file_descr -> 'a tzresult Lwt.t) ->
  'a tzresult Lwt.t

val bind : ?backlog:int -> addr -> Lwt_unix.file_descr list tzresult Lwt.t

val send :
  Lwt_unix.file_descr -> 'a Data_encoding.t -> 'a -> unit tzresult Lwt.t

val recv :
  ?timeout:Ptime.Span.t ->
  Lwt_unix.file_descr ->
  'a Data_encoding.t ->
  'a tzresult Lwt.t

(** [handshake socket magic_bytes] is a function to synchronize two
    separate processes and start a communication.

   The scenario of the handshake is the following:
   - both processes simultaneously send some [magic_bytes],
   - both processes wait and checks the received bytes validity,
   - handshake is finished. *)
val handshake : Lwt_unix.file_descr -> bytes -> unit tzresult Lwt.t

(** [get_temporary_socket_dir ()] returns a temporary path for the
    socket to be spawned. $XDG_RUNTIME_DIR is returned if the
    environment variable is defined. Otherwise, the default temporary
    directory is used. *)
val get_temporary_socket_dir : unit -> string
