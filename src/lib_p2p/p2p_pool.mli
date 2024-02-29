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

(** This module maintains several pools of points/peers needed by the P2P layer.

    Recall a *point id* (or point) is a couple (addr, port). A *peer id*
    (or peer) is a hash uniquely identifying a peer. An address may host
    several peers.

    Pool functions can trigger two types of events. They can *log*
    [P2p_connection.P2p_event.t] (for the upper layer), and they can
    trigger condition variables defined in [P2p_trigger.t], for inter-modules
    synchronization. *)

(** {1 Pool management} *)

(** The type of a pool of connections, parametrized by, resp., the type
    of messages and the meta-information associated to an identity and
    a connection. *)
type ('msg, 'peer, 'conn) t

type config = {
  identity : P2p_identity.t;  (** Our identity. *)
  trusted_points : (P2p_point.Id.t * P2p_peer.Id.t option) list;
      (** List of hard-coded known peers to bootstrap the network from. *)
  peers_file : string;
      (** The path to the JSON file where the metadata associated to
      peer_ids are loaded / stored. *)
  private_mode : bool;
      (** If [true], only open outgoing/accept incoming connections
      to/from peers whose addresses are in [trusted_peers], and inform
      these peers that the identity of this node should not be revealed to
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
  peer_greylist_size : int;
      (** The number of peer_ids kept in the peer_id greylist. *)
  ip_greylist_size_in_kilobytes : int;
      (** The size of the IP address greylist. *)
  ip_greylist_cleanup_delay : Time.System.Span.t;
      (** The time an IP address is kept in the greylist. *)
}

val create :
  config ->
  'peer P2p_params.peer_meta_config ->
  P2p_trigger.t ->
  log:(P2p_connection.P2p_event.t -> unit) ->
  ('msg, 'peer, 'conn) t Lwt.t

(** [save_peers pool] save all peer currently known by the node on disk *)
val save_peers : ('msg, 'peer, 'conn) t -> unit Lwt.t

(** [tear_down_connections ~reason pool] close all connections, and returns
    when member connections are either disconnected or canceled. *)
val tear_down_connections :
  reason:P2p_disconnection_reason.t -> ('msg, 'peer, 'conn) t -> unit Lwt.t

(** [destroy pool] calls tear_down_connections and save the known peers list on the disk *)
val destroy : ('msg, 'peer, 'conn) t -> unit Lwt.t

(** [config pool] is the [config] argument passed to [pool] at
    creation. *)
val config : _ t -> config

(** {1 Connections management} *)

(** [active_connections pool] is the number of connections inside
    [pool]. *)
val active_connections : ('msg, 'peer, 'conn) t -> int

(** If [point] doesn't belong to the table of known points,
   [register_point ?expected_peer_id t point] creates a
   [P2p_point_state.Info.t], triggers a `New_point` event and signals
   the `new_point` condition. If table capacity is exceeded, a GC of
   the table is triggered.

    If [point] is already known, the [P2p_point_state.Info.t] from the
   table is returned. In either case, the trusted status of the
   returned [P2p_point_state.Info.t] is set to [trusted]. If an
   [expected_peer_id] is given, it will be used during the next
   connection attempt. If the provided peer_id is different the
   connection will be refused. If we are already connected to a peer,
   we check whether the current peer_id is the expected one otherwise
   we disconnect from this point. *)
val register_point :
  ?trusted:bool ->
  ?expected_peer_id:P2p_peer.Id.t ->
  ('msg, 'peer, 'conn) t ->
  P2p_point.Id.t ->
  ('msg, 'peer, 'conn) P2p_conn.t P2p_point_state.Info.t

(** Remove a [point] from the table of known points if it is already known.. *)
val unregister_point : ('msg, 'peer, 'conn) t -> P2p_point.Id.t -> unit

(** [register_new_point pool point] returns [None] if [point] is a point for
    this peer. Otherwise it behaves as [register_point]. *)
val register_new_point :
  ?trusted:bool ->
  ('msg, 'peer, 'conn) t ->
  P2p_point.Id.t ->
  ('msg, 'peer, 'conn) P2p_conn.t P2p_point_state.Info.t option

(** [register_list_of_new_point ?trusted medium source pool point_list]
o    registers all points of the list as new points.
    [medium] and  [source] are for logging purpose. [medium] should indicate
    through which medium the points have been acquired (
    advertisement, Nack, ..)  and [source] is the id of the peer which
    sent the list.
 *)
val register_list_of_new_points :
  ?trusted:bool ->
  medium:string ->
  source:P2p_peer.Id.t ->
  ('msg, 'peer, 'conn) t ->
  P2p_point.Id.t list ->
  unit Lwt.t

(** If [peer] doesn't belong to the table of known peers,
    [register_peer t peer] creates a [P2p_peer.Info.t], triggers a
    `New_peer` event, and signals a `new_peer` condition. If table capacity
    is exceeded, a GC of the table is triggered. If [peer] is already known,
    the [P2p_peer.Info.t] from the table is returned. *)
val register_peer :
  ('msg, 'peer, 'conn) t ->
  P2p_peer.Id.t ->
  (('msg, 'peer, 'conn) P2p_conn.t, 'peer, 'conn) P2p_peer_state.Info.t

module Connection : sig
  (** [fold pool ~init ~f] computes [(f iN cN ... (f i1 c1 init)...)]
      where [id1 ... idN] are the ids of every connected peers and [c1 ... cN] the associated peers info. *)
  val fold :
    ('msg, 'peer, 'conn) t ->
    init:'a ->
    f:(P2p_peer.Id.t -> ('msg, 'peer, 'conn) P2p_conn.t -> 'a -> 'a) ->
    'a

  (** [iter f pool] applies [f] to all connected peers of [pool]. *)
  val iter :
    (P2p_peer.Id.t -> ('msg, 'peer, 'conn) P2p_conn.t -> unit) ->
    ('msg, 'peer, 'conn) t ->
    unit

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

  (** [random_addr ?conn no_private t] returns a random (point_id, peer_id)
      from the pool of connections. It ignores:
      - connections to private peers if [no_private] is set to [true]
      - connection [conn]
      - connections to peers who didn't provide a listening port at
        session-establishment *)
  val random_addr :
    ?different_than:('msg, 'peer, 'conn) P2p_conn.t ->
    no_private:bool ->
    ('msg, 'peer, 'conn) t ->
    (P2p_point.Id.t * P2p_peer.Id.t) option

  (** [propose_swap_request t] returns a triple (point_id, peer_id, conn) where
        conn is a random connection to a non-private peer, and (point_id, peer_id)
        is a random, different, connected peer_id at point_id.  *)
  val propose_swap_request :
    ('msg, 'peer, 'conn) t ->
    (P2p_point.Id.t * P2p_peer.Id.t * ('msg, 'peer, 'conn) P2p_conn.t) option
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

  (** [fold_known pool ~init ~f] computes [(f iN pN ... (f i1 p1 init)...)]
      where [id1 ... idN] are the ids of every known peers and [p1 ... pN] the associated peers info. *)
  val fold_known :
    ('msg, 'peer, 'conn) t ->
    init:'a ->
    f:(P2p_peer.Id.t -> ('msg, 'peer, 'conn) info -> 'a -> 'a) ->
    'a

  (** [iter_known f pool] applies [f] to all known peers of [pool]. *)
  val iter_known :
    (P2p_peer.Id.t -> ('msg, 'peer, 'conn) info -> unit) ->
    ('msg, 'peer, 'conn) t ->
    unit

  (** [fold_connected pool ~init ~f] computes [(f iN pN ... (f i1 p1 init)...)]
      where [id1 ... idN] are the ids of every connected peers and [p1 ... pN] the associated peers info. *)
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

  (** [ban t peer_id] blacklists this peer_id and terminates connection
      (if any). *)
  val ban : ('msg, 'peer, 'conn) t -> P2p_peer.Id.t -> unit Lwt.t

  (** [unban t peer_id] removes this peer_id from the black list. *)
  val unban : ('msg, 'peer, 'conn) t -> P2p_peer.Id.t -> unit

  (** [banned t peer_id] returns [true] if the peer is in the black list. *)
  val banned : ('msg, 'peer, 'conn) t -> P2p_peer.Id.t -> bool

  (** [get_trusted t peer_id] returns [false] if this peer isn't known.
      Otherwise it calls [trusted] for this peer info. *)
  val get_trusted : ('msg, 'peer, 'conn) t -> P2p_peer.Id.t -> bool

  (** [trust t peer_id] sets the peer info for this peer to trusted, and
      [unban] it. The peer is registered first if not known (see
      [register_peer]). *)
  val trust : ('msg, 'peer, 'conn) t -> P2p_peer.Id.t -> unit

  (** [untrust t peer_id] set the peer info for this peer to not trusted.
      Does nothing if this peer isn't known. *)
  val untrust : ('msg, 'peer, 'conn) t -> P2p_peer.Id.t -> unit

  (** [get_greylisted_list t] returns the list of all the greylisted peers *)
  val get_greylisted_list : ('msg, 'peer, 'conn) t -> P2p_peer.Id.t list

  (** [info_of_peer_info t peer] returns the peer info from the peer
      state info. *)
  val info_of_peer_info :
    ('msg, 'peer, 'conn) t ->
    (('msg, 'peer, 'conn) P2p_conn.t, 'peer, 'conn) P2p_peer_state.Info.t ->
    ('peer, 'conn) P2p_peer.Info.t
end

(** {1 Functions on [Points]} *)

module Points : sig
  type ('msg, 'peer, 'conn) info =
    ('msg, 'peer, 'conn) P2p_conn.t P2p_point_state.Info.t

  val info :
    ('msg, 'peer, 'conn) t -> P2p_point.Id.t -> ('msg, 'peer, 'conn) info option

  (** [fold_known pool ~init ~f] computes [(f iN pN ... (f i1 p1 init)...)]
      where [id1 ... idN] are the ids of every known points and [p1 ... pN] the associated points info. *)
  val fold_known :
    ('msg, 'peer, 'conn) t ->
    init:'a ->
    f:
      (P2p_point.Id.t ->
      ('msg, 'peer, 'conn) P2p_conn.t P2p_point_state.Info.t ->
      'a ->
      'a) ->
    'a

  (** [iter_known f pool] applies [f] to all known points of [pool]. *)
  val iter_known :
    (P2p_point.Id.t ->
    ('msg, 'peer, 'conn) P2p_conn.t P2p_point_state.Info.t ->
    unit) ->
    ('msg, 'peer, 'conn) t ->
    unit

  (** [fold_known pool ~init ~f] computes [(f iN pN ... (f i1 p1 init)...)]
      where [id1 ... idN] are the ids of every connected points and [p1 ... pN] the associated points info. *)
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

  (** [ban t point_id] marks the address of this point_id as blacklisted.
      It disconnects and bans all connections to this address. If [ban_peers]
      is false the associated peers are disconnected but not banned.
      The [point_id] port is ignored. *)
  val ban :
    ('msg, 'peer, 'conn) t -> ?ban_peers:bool -> P2p_point.Id.t -> unit Lwt.t

  (* TODO https://gitlab.com/tezos/tezos/-/issues/7028
     this isn't consistent with greylist functions where only an addr is
     provided. *)

  (** [unban t point_id] removes this point address from the black list.
      and unban all associated peers. The [point_id] port is ignored. *)
  val unban : ('msg, 'peer, 'conn) t -> P2p_point.Id.t -> unit

  (** [banned t point_id] returns [true] if the point addr is in the black list.
      This [point_id]'s port is ignored. *)
  val banned : ('msg, 'peer, 'conn) t -> P2p_point.Id.t -> bool

  (** [get_trusted t point_id] returns [false] if this point isn't known.
      Otherwise it calls [trusted] for this peer info. *)
  val get_trusted : ('msg, 'peer, 'conn) t -> P2p_point.Id.t -> bool

  (** [trust t point_id] sets the point info for this point to trusted, and
      [unban] it.  The point is registered first if not known (see [register_point]). *)
  val trust : ('msg, 'peer, 'conn) t -> P2p_point.Id.t -> unit

  (** [untrust t point_id] sets the point info peer info for this point
      to not trusted. Does nothing if point isn't known. *)
  val untrust : ('msg, 'peer, 'conn) t -> P2p_point.Id.t -> unit

  (** [get_greylisted_list t] if [greylisted_list_not_reliable_since t] returns
      [None], returns the list of currently greylisted IPs. *)
  val get_greylisted_list : ('msg, 'peer, 'conn) t -> P2p_addr.t list

  (* The list returned by [get_greylisted_list t] can be overflowed. When it
     has been overflowed, it is no more reliable. The date of the first
     overflow is memorized to indicate that it is no more reliable. This is
     reset by calling [acl_clear t]. *)
  val greylisted_list_not_reliable_since :
    ('msg, 'peer, 'conn) t -> Time.System.t option
end

(** {1 Misc functions} *)

(** [greylist_addr pool addr] adds [addr] to [pool]'s IP greylist. *)
val greylist_addr : ('msg, 'peer, 'conn) t -> P2p_addr.t -> unit

(** [greylist_peer pool peer] adds [peer] to [pool]'s peer greylist
    and [peer]'s address to [pool]'s addr greylist. *)
val greylist_peer : ('msg, 'peer, 'conn) t -> P2p_peer.Id.t -> unit

(** [clear_greylist] removes all addresses from the greylist. *)
val clear_greylist : ('msg, 'peer, 'conn) t -> unit

(** [gc_greylist] removes some addresses from the greylist (the oldest
   have a higher probability to be removed, yet due to the underlying
   probabilistic structure, recent greylistings can be dropped). *)
val gc_greylist : ('msg, 'peer, 'conn) t -> unit

(** [acl_clear pool] clears ACL tables. *)
val acl_clear : ('msg, 'peer, 'conn) t -> unit

(** [list_known_points ~ignore_private ?size t] returns a list of point ids,
    which are not banned, and if [ignore_private] is [true], public.

    It returns at most [size] point ids (default is 50) based on a
    heuristic that selects a mix of 3/5 "good" and 2/5 random points.

    @raise Invalid_argument if [size < 0] *)
val list_known_points :
  ignore_private:bool ->
  ?size:int ->
  ('msg, 'peer, 'coon) t ->
  P2p_point.Id.t list Lwt.t

val connected_peer_ids :
  ('msg, 'peer, 'conn) t ->
  (('msg, 'peer, 'conn) P2p_conn.t, 'peer, 'conn) P2p_peer_state.Info.t
  P2p_peer.Table.t

(** [score t peer_meta] returns the score of [peer_meta]. *)
val score : ('msg, 'peer, 'conn) t -> 'peer -> float

(** [add_to_id_points t point] adds [point] to the list of points for this
    peer. [point] is removed from the list of known points. *)
val add_to_id_points : ('msg, 'peer, 'conn) t -> P2p_point.Id.t -> unit

(** [set_expected_peer_id t point peer] sets [peer] as an expected
   peer_id to [point]. For any future connection with this point,
   there is a check that the peer_id announced is the expected one. As
   a side effect, if there is an active connection with [point] with
   another peer_id, we disconnect from this point and the point is
   greylisted. If the connection is not active but accepted, the point
   is greylisted. *)
val set_expected_peer_id :
  ('msg, 'peer, 'conn) t -> P2p_point.Id.t -> P2p_peer.Id.t -> unit Lwt.t

(**/**)

module Internal_for_tests : sig
  (** [create peer_encoding peer] returns a pool. [peer_encoding] and [peer] are needed as some functions of [P2p_pool]
      return it. *)
  val create : 'peer Data_encoding.t -> 'peer -> ('msg, 'peer, 'conn) t
end
