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

(** Welcome worker.

    Accept incoming connections and add them to the pool.
*)

(** Type discribing an opening failure for the
   listening socket. *)
type listening_socket_open_failure = {
  reason : Unix.error;  (** The error we are re-raising *)
  address : P2p_addr.t;  (** The interface we are trying to listen to *)
  port : int;  (** The port we are trying to listen to *)
}

(** Type of an error in case where the listening
    socket fails to open. *)
type error += Failed_to_open_listening_socket of listening_socket_open_failure

(** Type of a welcome worker. *)
type t

(** [create ?reuse_port ?addr ~backlog pool port] returns a running welcome worker
    adding connections into [pool] listening on [addr:port]. [backlog]
    is passed to [Lwt_unix.listen].

    [reuse_port] should be used for testing purposes. This option
    sets [SO_REUSEPORT] on the socket, allowing to reuse a port opened 
    elsewhere. *)
val create :
  ?reuse_port:bool ->
  ?addr:P2p_addr.t ->
  backlog:int ->
  ('msg, 'meta, 'meta_conn) P2p_connect_handler.t ->
  P2p_addr.port ->
  t tzresult Lwt.t

(** [activate t] start the worker that will accept connections *)
val activate : t -> unit

(** [shutdown t] returns when [t] has completed shutdown. *)
val shutdown : t -> unit Lwt.t
