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

(** The configurable constants used by the p2p layer as maximum,
    with their encoding and default values. *)

(** Network capacities *)
type t = {
  connection_timeout : Time.System.Span.t;
      (** Maximum time allowed to the establishment of a connection. *)
  authentication_timeout : Time.System.Span.t;
      (** Delay granted to a peer to perform authentication. *)
  greylist_timeout : Time.System.Span.t;
      (** GC delay for the greylists tables. *)
  maintenance_idle_time : Time.System.Span.t option;
      (** How long to wait at most before running a maintenance loop. If None,
          the maintenance is disabled. *)
  min_connections : int;
      (** Strict minimum number of connections (triggers an urgent maintenance) *)
  expected_connections : int;
      (** Targeted number of connections to reach when bootstrapping / maintaining *)
  max_connections : int;
      (** Maximum number of connections (exceeding peers are disconnected) *)
  backlog : int;  (** Argument of [Lwt_unix.accept].*)
  max_incoming_connections : int;
      (** Maximum not-yet-authenticated incoming connections. *)
  max_download_speed : int option;
      (** Hard-limit in the number of bytes received per second. *)
  max_upload_speed : int option;
      (** Hard-limit in the number of bytes sent per second. *)
  read_buffer_size : int;
      (** Size in bytes of the buffer passed to [Lwt_unix.read]. *)
  read_queue_size : int option;
  write_queue_size : int option;
  incoming_app_message_queue_size : int option;
  incoming_message_queue_size : int option;
  outgoing_message_queue_size : int option;
      (** Various bounds for internal queues. *)
  max_known_peer_ids : (int * int) option;
  max_known_points : (int * int) option;
      (** Optional limitation of internal hashtables (max, target) *)
  peer_greylist_size : int;
      (** The number of peer_ids kept in the peer_id greylist. *)
  ip_greylist_size_in_kilobytes : int;
      (** The size of the IP address greylist in kilobytes. *)
  ip_greylist_cleanup_delay : Time.System.Span.t;
      (** The time an IP address is kept in the greylist. *)
  swap_linger : Time.System.Span.t option;
      (** Peer swapping does not occur more than once during a timespan of
          [swap_linger]. If None, the swap mechanism is disabled.  *)
  binary_chunks_size : int option;
      (** Size (in bytes) of binary blocks that are sent to other
      peers. Default value is 64 kB. Max value is 64kB. *)
}

val default : t

val encoding : t Data_encoding.t
