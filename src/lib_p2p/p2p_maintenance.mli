(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** P2P maintenance worker.

    This worker enforces the connection bounds defined on the command-line
    or/and the configuration file.

    The maintenance process is launched:
    . When any of the following future  is resolved
        [P2p_trigger.wait_too_few_connections]
        [P2p_trigger.wait_too_many_connections]
    . When [maintain] is called
    . After [maintenance_idle_time] if none of the other conditions have been
      met.

    If the number of connections is above the limit, the maintainer
    kills existing connections.

    If below the limit, it tries to connect to points available from [P2p_pool].
    If not enough connections can be obtained, it requests new points from
    [P2p_pool] using [P2p_pool.broadcast msg], and wakes up the
    [P2p_discovery] worker. It then waits for new peers or points by waiting
    on futures
      [P2p_trigger.wait_new_peer]
      [P2p_trigger.wait_new_point]
    This is reiterated indefinitely every [require_new_points_time]. *)

type config = {
  maintenance_idle_time : Time.System.Span.t;
      (** How long to wait at most before running a maintenance loop. *)
  private_mode : bool;
      (** If [true], only open outgoing/accept incoming connections
      to/from peers whose addresses are in [trusted_peers], and inform
      these peers that the identity of this node should be revealed to
      the rest of the network. *)
  min_connections : int;  (** Strict minimum number of connections *)
  max_connections : int;  (** Maximum number of connections *)
  expected_connections : int;  (** Targeted number of connections to reach *)
}

(** Type of a maintenance worker. *)
type ('msg, 'meta, 'meta_conn) t

(** [create ?discovery config pool triggers log] returns a maintenance worker,
    with the [discovery] worker if present, for [pool]. *)
val create :
  ?discovery:P2p_discovery.t ->
  config ->
  ('msg, 'meta, 'meta_conn) P2p_pool.t ->
  ('msg, 'meta, 'meta_conn) P2p_connect_handler.t ->
  P2p_trigger.t ->
  log:(P2p_connection.P2p_event.t -> unit) ->
  ('msg, 'meta, 'meta_conn) t

(** [activate t] starts the worker that will maintain connections *)
val activate : ('msg, 'meta, 'meta_conn) t -> unit

(** [maintain t] gives a hint to maintenance worker [t] that
    maintenance is needed and returns whenever [t] has done a
    maintenance cycle. *)
val maintain : ('msg, 'meta, 'meta_conn) t -> unit Lwt.t

(** [shutdown t] is a thread that returns whenever [t] has
    successfully shut down. *)
val shutdown : ('msg, 'meta, 'meta_conn) t -> unit Lwt.t
