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

(** Scheduling of I/O operations over file descriptors.

    This module defines the scheduler type [t], and connection type
    [connection]. A [connection] is a wrapper over a [P2p_fd.t]. R/W
    functions over [connection]s behave like regular R/W over file
    descriptors, but the scheduler ensures of fair allocation of bandwidth
    between them.

    To each connection is associated a read (resp. write) queue where data is
    copied to (resp. read from), at a rate of [max_download_speed /
    num_connections] (resp. [max_upload_speed / num_connections]). *)

(** Type of a connection. *)
type connection

(** Type of an IO scheduler. *)
type t

(** [create ~max_upload_speed ~max_download_speed ~read_queue_size
    ~write_queue_size ()] is an IO scheduler with specified (global)
    max upload (resp. download) speed, and specified read
    (resp. write) queue sizes (in bytes) for connections. *)
val create :
  ?max_upload_speed:int ->
  ?max_download_speed:int ->
  ?read_queue_size:int ->
  ?write_queue_size:int ->
  read_buffer_size:int ->
  unit ->
  t

(** [ma_state sched] returns the state of the moving average
    worker. *)
val ma_state : t -> Moving_average.state

(** [register sched fd] is a [connection] managed by [sched]. *)
val register : t -> P2p_fd.t -> connection

(** [write conn msg] returns [Ok ()] when [msg] has been added to
    [conn]'s write queue, or fail with an error. *)
val write :
  ?canceler:Lwt_canceler.t -> connection -> Bytes.t -> unit tzresult Lwt.t

(** [write_now conn msg] is [true] iff [msg] has been (immediately)
    added to [conn]'s write queue, [false] if it has been dropped. *)
val write_now : connection -> Bytes.t -> bool

(** [set_peer_id ~peer_id conn] sets the peer id where [conn] is connected to.
   *)
val set_peer_id : peer_id:P2p_peer.Id.t -> connection -> unit

(** Returns the [readable] of an abstract [connection] *)
val to_readable : connection -> P2p_buffer_reader.readable

(** [stat conn] is a snapshot of current bandwidth usage for
    [conn]. *)
val stat : connection -> P2p_stat.t

(** [global_stat sched] is a snapshot of [sched]'s bandwidth usage
    (sum of [stat conn] for each [conn] in [sched]). *)
val global_stat : t -> P2p_stat.t

(** [iter_connection sched f] applies [f] on each connection managed
    by [sched]. *)
val iter_connection : t -> (connection -> unit) -> unit

(** Stores a reason for which it will be closed in a near future. *)
val add_closing_reason : reason:P2p_disconnection_reason.t -> connection -> unit

(** [close ?reason conn] set the closing reason and returns after any pending
    data has been sent and the canceler of [conn] has been triggered.

    It does not wait for the canceler callbacks, so there is no guarantee that
    the file descriptor is already closed, but it will eventually be closed.

    If timeout is set, the canceler will be triggered after the timeout, even
    if pending data remains to be sent. *)
val close :
  ?timeout:float ->
  ?reason:P2p_disconnection_reason.t ->
  connection ->
  unit tzresult Lwt.t

(** [shutdown sched] returns after all connections managed by [sched]
    have been closed and [sched]'s inner worker has successfully
    canceled. *)
val shutdown : ?timeout:float -> t -> unit Lwt.t

(** [id connection] returns the identifier of the underlying [P2p_fd.t]
    file descriptor. This uniquely identifies a connection. *)
val id : connection -> int
