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

(** This module maintains tables of points, peers, and connections
    needed by the P2P layer. *)

(** {1 Pool management} *)

(** The type of a pool of connections, parametrized by, resp., the type
    of messages and the meta-informations associated to an identity and
    a connection. *)
type ('msg, 'peer, 'conn) t

type config = {
  identity : P2p_identity.t;  (** Our identity. *)
  trusted_points : P2p_point.Id.t list;
      (** List of hard-coded known peers to bootstrap the network from. *)
  peers_file : string;
      (** The path to the JSON file where the metadata associated to
      peer_ids are loaded / stored. *)
  private_mode : bool;
      (** If [true], only open outgoing/accept incoming connections
      to/from peers whose addresses are in [trusted_peers], and inform
      these peers that the identity of this node should be revealed to
      the rest of the network. *)
  max_known_points : (int * int) option;
      (** Parameters for the garbage collection of known points. If
      None, no garbage collection is performed. Otherwise, the first
      integer of the couple limits the size of the "known points"
      table. When this number is reached, the table is purged off of
      disconnected points, older first, to try to reach the amount of
      connections indicated by the second integer. *)
  max_known_peer_ids : (int * int) option;
      (** Like [max_known_points], but for known peer_ids. *)
}

type 'peer peer_meta_config = {
  peer_meta_encoding : 'peer Data_encoding.t;
  peer_meta_initial : unit -> 'peer;
  score : 'peer -> float;
}

val create :
  config ->
  'peer peer_meta_config ->
  P2p_events.t ->
  log:(P2p_connection.P2p_event.t -> unit) ->
  ('msg, 'peer, 'conn) t Lwt.t

(** [destroy pool] returns when member connections are either
    disconnected or canceled. *)
val destroy : ('msg, 'peer, 'conn) t -> unit Lwt.t

(** [config pool] is the [config] argument passed to [pool] at
    creation. *)
val config : _ t -> config

(** {1 Connections management} *)

(** [active_connections pool] is the number of connections inside
    [pool]. *)
val active_connections : ('msg, 'peer, 'conn) t -> int

val register_point :
  ?trusted:bool ->
  ('msg, 'peer, 'conn) t ->
  P2p_addr.t * int ->
  ('msg, 'peer, 'conn) P2p_conn.t P2p_point_state.Info.t

(** [register_new_point pool point] tries to register [point]
    in pool's internal peer table. *)
val register_new_point :
  ?trusted:bool ->
  ('msg, 'peer, 'conn) t ->
  P2p_point.Id.t ->
  ('msg, 'peer, 'conn) P2p_conn.t P2p_point_state.Info.t option

val register_peer :
  ('msg, 'peer, 'conn) t ->
  P2p_peer.Id.t ->
  (('msg, 'peer, 'conn) P2p_conn.t, 'peer, 'conn) P2p_peer_state.Info.t

module Connection : sig
  val fold :
    ('msg, 'peer, 'conn) t ->
    init:'a ->
    f:(P2p_peer.Id.t -> ('msg, 'peer, 'conn) P2p_conn.t -> 'a -> 'a) ->
    'a

  val list :
    ('msg, 'peer, 'conn) t ->
    (P2p_peer.Id.t * ('msg, 'peer, 'conn) P2p_conn.t) list

  val find_by_point :
    ('msg, 'peer, 'conn) t ->
    P2p_point.Id.t ->
    ('msg, 'peer, 'conn) P2p_conn.t option

  val find_by_peer_id :
    ('msg, 'peer, 'conn) t ->
    P2p_peer.Id.t ->
    ('msg, 'peer, 'conn) P2p_conn.t option

  (** [random_addr ?conn no_private t] returns a random ((addr, port), peer_id) from
      the pool of connections. It ignores connections to private peers if
      [no_private] is set to true. It also ignores connection [conn]. *)
  val random_addr :
    ?different_than:('msg, 'peer, 'conn) P2p_conn.t ->
    no_private:bool ->
    ('msg, 'peer, 'conn) t ->
    ((P2p_addr.t * int) * P2p_peer.Id.t) option

  val propose_swap_request :
    ('msg, 'peer, 'conn) t ->
    ((P2p_addr.t * int) * P2p_peer.Id.t * ('msg, 'peer, 'conn) P2p_conn.t)
    option
end

(** {1 Functions on [Peer_id]} *)

module Peers : sig
  type ('msg, 'peer, 'conn) info =
    (('msg, 'peer, 'conn) P2p_conn.t, 'peer, 'conn) P2p_peer_state.Info.t

  val info :
    ('msg, 'peer, 'conn) t -> P2p_peer.Id.t -> ('msg, 'peer, 'conn) info option

  val get_peer_metadata : ('msg, 'peer, 'conn) t -> P2p_peer.Id.t -> 'peer

  val set_peer_metadata :
    ('msg, 'peer, 'conn) t -> P2p_peer.Id.t -> 'peer -> unit

  val get_score : ('msg, 'peer, 'conn) t -> P2p_peer.Id.t -> float

  val get_trusted : ('msg, 'peer, 'conn) t -> P2p_peer.Id.t -> bool

  val fold_known :
    ('msg, 'peer, 'conn) t ->
    init:'a ->
    f:(P2p_peer.Id.t -> ('msg, 'peer, 'conn) info -> 'a -> 'a) ->
    'a

  val fold_connected :
    ('msg, 'peer, 'conn) t ->
    init:'a ->
    f:(P2p_peer.Id.t -> ('msg, 'peer, 'conn) info -> 'a -> 'a) ->
    'a

  val add_connected :
    ('msg, 'peer, 'conn) t ->
    P2p_peer.Id.t ->
    (('msg, 'peer, 'conn) P2p_conn.t, 'peer, 'conn) P2p_peer_state.Info.t ->
    unit

  val remove_connected : ('msg, 'peer, 'conn) t -> P2p_peer.Id.t -> unit

  val ban : ('msg, 'peer, 'conn) t -> P2p_peer.Id.t -> unit

  val unban : ('msg, 'peer, 'conn) t -> P2p_peer.Id.t -> unit

  val trust : ('msg, 'peer, 'conn) t -> P2p_peer.Id.t -> unit

  val untrust : ('msg, 'peer, 'conn) t -> P2p_peer.Id.t -> unit

  val banned : ('msg, 'peer, 'conn) t -> P2p_peer.Id.t -> bool
end

(** {1 Functions on [Points]} *)

module Points : sig
  type ('msg, 'peer, 'conn) info =
    ('msg, 'peer, 'conn) P2p_conn.t P2p_point_state.Info.t

  val info :
    ('msg, 'peer, 'conn) t ->
    P2p_point.Id.t ->
    ('msg, 'peer, 'conn) info option

  val get_trusted : ('msg, 'peer, 'conn) t -> P2p_point.Id.t -> bool

  val fold_known :
    ('msg, 'peer, 'conn) t ->
    init:'a ->
    f:(P2p_point.Id.t ->
      ('msg, 'peer, 'conn) P2p_conn.t P2p_point_state.Info.t ->
      'a ->
      'a) ->
    'a

  val fold_connected :
    ('msg, 'peer, 'conn) t ->
    init:'a ->
    f:(P2p_point.Id.t -> ('msg, 'peer, 'conn) info -> 'a -> 'a) ->
    'a

  val add_connected :
    ('msg, 'peer, 'conn) t ->
    P2p_point.Id.t ->
    ('msg, 'peer, 'conn) P2p_conn.t P2p_point_state.Info.t ->
    unit

  val remove_connected :
    ('msg, 'peer, 'conn) t -> 'd P2p_point_state.Info.t -> unit

  val ban : ('msg, 'peer, 'conn) t -> P2p_point.Id.t -> unit

  val unban : ('msg, 'peer, 'conn) t -> P2p_point.Id.t -> unit

  val trust : ('msg, 'peer, 'conn) t -> P2p_point.Id.t -> unit

  val untrust : ('msg, 'peer, 'conn) t -> P2p_point.Id.t -> unit

  val banned : ('msg, 'peer, 'conn) t -> P2p_point.Id.t -> bool
end

(** {1 Misc functions} *)

(** [greylist_addr pool addr] adds [addr] to [pool]'s IP greylist. *)
val greylist_addr : ('msg, 'peer, 'conn) t -> P2p_addr.t -> unit

(** [greylist_peer pool peer] adds [peer] to [pool]'s peer greylist
    and [peer]'s address to [pool]'s IP greylist. *)
val greylist_peer : ('msg, 'peer, 'conn) t -> P2p_peer.Id.t -> unit

(** [gc_greylist ~older_than pool] *)
val gc_greylist : older_than:Time.System.t -> ('msg, 'peer, 'conn) t -> unit

(** [acl_clear pool] clears ACL tables. *)
val acl_clear : ('msg, 'peer, 'conn) t -> unit

val list_known_points :
  ignore_private:bool -> ('msg, 'peer, 'coon) t -> P2p_point.Id.t list Lwt.t

val connected_peer_ids :
  ('msg, 'peer, 'conn) t ->
  (('msg, 'peer, 'conn) P2p_conn.t, 'peer, 'conn) P2p_peer_state.Info.t
  P2p_peer.Table.t

(** [score pool peer_meta] returns the score of a peer in the pool
    whose peer_meta is provided *)
val score : ('msg, 'peer, 'conn) t -> 'peer -> float

(** [add_to_id_points t point] adds [point] to the list of points for this
    peer. [point] is removed from the list of known points. *)
val add_to_id_points : ('msg, 'peer, 'conn) t -> P2p_point.Id.t -> unit
