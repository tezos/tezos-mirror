(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2020 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(** Tezos P2p layer - Dynamic overlay network of authenticated peers.

    The P2P layer implements several mechanisms, notably:
    - It maintains pools of known points (P2P servers), peers (authenticated
      P2P servers), connections,
    - it implements an "administrative" protocol for maintaining the network
      topology,
    - it regulates bandwidth usage between connections,
    - it implements an authentication / session agreement protocol,
    - it can ban or greylist peers or IP addresses who don't behave well,
    - it offers the ability to the upper-layer to send, broadcast, or
      receive messages.

    The protocol sends/receives messages to maintain the network topology,
    and also "generic" application messages that can be sent and received
    by the upper-layer. See [P2p_message].

    The protocol may operate in *private* mode, in which only user-provided
    points (a.k.a. *trusted* ) are used. In particular, points
    advertisements and swap requests messages are ignored.

    The module [P2p_pool] maintains pools of points, peers and
    connections.

    Several workers are used:
    - [P2p_maintenance] tries to regulate the number of connections
    - [P2p_welcome] waits for incoming connections
    - [P2p_discovery] looks for points on the local network via UDP messages
    - A protocol worker implements the messaging protocol

    Points can be trusted. This is relevant in private mode
    (see above), but generally peers shouldn't advertise trusted points.

    Addresses and peers can be *banned* (a.k.a. blacklisted). In
    which case, connections to and from them should be ignored.

    Addresses or peers can be *greylisted*. As for banning, greylisting
    can be enforced via the API, but also dynamically when the peer isn't
    able to authenticate. Eventually greylisted peers are whitelisted again.

    Many types used in the P2p layer are parameterized by three type parameters:
    - ['msg]: type of messages exchanged between peers
    - ['peer_meta]: type of the metadata associated with peers (score, etc.)
    - ['conn_meta]: type of the metadata associated with connections

    The concrete types, and functions operating on them, are defined by the
    calling layer, and passed to [P2p.create]. See module [P2p_params]. *)

(** Network configuration *)
type config = {
  listening_port : P2p_addr.port option;
      (** Tells if incoming connections accepted, specifying the TCP port
      on which the peer can be reached (default: [9732])*)
  listening_addr : P2p_addr.t option;
      (** When incoming connections are accepted, precise on which
      IP address the node listen (default: [[::]]). *)
  advertised_port : P2p_addr.port option;
      (** If incoming connections accepted, specifying the TCP port other peers
      should use to connect to this peer (default: listening_port). Can be used
      when this peer is behind NAT. *)
  discovery_port : P2p_addr.port option;
      (** Tells if local peer discovery is enabled, specifying the TCP port
      on which the peer can be reached (default: [10732]) *)
  discovery_addr : Ipaddr.V4.t option;
      (** When local peer discovery is enabled, precise on which
      IP address messages are broadcast (default: [255.255.255.255]). *)
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
  identity : P2p_identity.t;  (** Cryptographic identity of the peer. *)
  proof_of_work_target : Crypto_box.pow_target;
      (** Expected level of proof of work of peers' identity. *)
  trust_discovered_peers : bool;
      (** If [true], peers discovered on the local network will be trusted. *)
  reconnection_config : P2p_point_state.Info.reconnection_config;
      (** The reconnection delat configuration. *)
}

(** Network capacities *)
type limits = {
  connection_timeout : Time.System.Span.t;
      (** Maximum time allowed to the establishment of a connection. *)
  authentication_timeout : Time.System.Span.t;
      (** Delay granted to a peer to perform authentication. *)
  greylist_timeout : Time.System.Span.t;
      (** GC delay for the greylists tables. *)
  maintenance_idle_time : Time.System.Span.t;
      (** How long to wait at most before running a maintenance loop. *)
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
  swap_linger : Time.System.Span.t;
      (** Peer swapping does not occur more than once during a timespan of
      [swap_linger]. *)
  binary_chunks_size : int option;
      (** Size (in bytes) of binary blocks that are sent to other
      peers. Default value is 64 kB. Max value is 64kB. *)
}

(** Type of a P2P layer instance *)
type ('msg, 'peer_meta, 'conn_meta) t

type ('msg, 'peer_meta, 'conn_meta) net = ('msg, 'peer_meta, 'conn_meta) t

val announced_version : ('msg, 'peer_meta, 'conn_meta) net -> Network_version.t

val pool :
  ('msg, 'peer_meta, 'conn_meta) net ->
  ('msg, 'peer_meta, 'conn_meta) P2p_pool.t option

val connect_handler :
  ('msg, 'peer_meta, 'conn_meta) net ->
  ('msg, 'peer_meta, 'conn_meta) P2p_connect_handler.t option

(** A faked p2p layer, which do not initiate any connection
    nor open any listening socket *)
val faked_network :
  'msg P2p_params.message_config ->
  'peer_meta P2p_params.peer_meta_config ->
  'conn_meta ->
  ('msg, 'peer_meta, 'conn_meta) net

(** Main network initialization function *)
val create :
  config:config ->
  limits:limits ->
  'peer_meta P2p_params.peer_meta_config ->
  'conn_meta P2p_params.conn_meta_config ->
  'msg P2p_params.message_config ->
  ('msg, 'peer_meta, 'conn_meta) net tzresult Lwt.t

val activate : ('msg, 'peer_meta, 'conn_meta) net -> unit

(** Return one's peer_id *)
val peer_id : ('msg, 'peer_meta, 'conn_meta) net -> P2p_peer.Id.t

(** A maintenance operation : try and reach the ideal number of peers *)
val maintain : ('msg, 'peer_meta, 'conn_meta) net -> unit Lwt.t

(** Voluntarily drop some peers and replace them by new buddies *)
val roll : ('msg, 'peer_meta, 'conn_meta) net -> unit Lwt.t

(** Close all connections properly *)
val shutdown : ('msg, 'peer_meta, 'conn_meta) net -> unit Lwt.t

(** A connection to a peer *)
type ('msg, 'peer_meta, 'conn_meta) connection

(** Access the domain of active peers *)
val connections :
  ('msg, 'peer_meta, 'conn_meta) net ->
  ('msg, 'peer_meta, 'conn_meta) connection list

(** Return the active peer with identity [peer_id] *)
val find_connection_by_peer_id :
  ('msg, 'peer_meta, 'conn_meta) net ->
  P2p_peer.Id.t ->
  ('msg, 'peer_meta, 'conn_meta) connection option

(** Return the active peer corresponding to [point] *)
val find_connection_by_point :
  ('msg, 'peer_meta, 'conn_meta) net ->
  P2p_point.Id.t ->
  ('msg, 'peer_meta, 'conn_meta) connection option

(** Access the info of an active peer, if available *)
val connection_info :
  ('msg, 'peer_meta, 'conn_meta) net ->
  ('msg, 'peer_meta, 'conn_meta) connection ->
  'conn_meta P2p_connection.Info.t

val connection_local_metadata :
  ('msg, 'peer_meta, 'conn_meta) net ->
  ('msg, 'peer_meta, 'conn_meta) connection ->
  'conn_meta

val connection_remote_metadata :
  ('msg, 'peer_meta, 'conn_meta) net ->
  ('msg, 'peer_meta, 'conn_meta) connection ->
  'conn_meta

val connection_stat :
  ('msg, 'peer_meta, 'conn_meta) net ->
  ('msg, 'peer_meta, 'conn_meta) connection ->
  P2p_stat.t

(** Returns the network version that will be used for this connection.
   This network version is the best version compatible with the versions
   supported by ours and the remote peer. *)
val negotiated_version :
  ('msg, 'peer_meta, 'conn_meta) net ->
  ('msg, 'peer_meta, 'conn_meta) connection ->
  Network_version.t

(** Cleanly closes a connection. *)
val disconnect :
  ('msg, 'peer_meta, 'conn_meta) net ->
  ?wait:bool ->
  ('msg, 'peer_meta, 'conn_meta) connection ->
  unit Lwt.t

val global_stat : ('msg, 'peer_meta, 'conn_meta) net -> P2p_stat.t

(** Accessors for meta information about a global identifier *)
val get_peer_metadata :
  ('msg, 'peer_meta, 'conn_meta) net -> P2p_peer.Id.t -> 'peer_meta

val set_peer_metadata :
  ('msg, 'peer_meta, 'conn_meta) net -> P2p_peer.Id.t -> 'peer_meta -> unit

(** [connect net ?timeout point] attempts to establish a connection to [point]
   within an optional duration [timeout]. *)
val connect :
  ('msg, 'peer_meta, 'conn_meta) net ->
  ?timeout:Ptime.span ->
  P2p_point.Id.t ->
  ('msg, 'peer_meta, 'conn_meta) connection tzresult Lwt.t

(** Wait for a message from a given connection. *)
val recv :
  ('msg, 'peer_meta, 'conn_meta) net ->
  ('msg, 'peer_meta, 'conn_meta) connection ->
  'msg tzresult Lwt.t

(** Wait for a message from any active connections. *)
val recv_any :
  ('msg, 'peer_meta, 'conn_meta) net ->
  (('msg, 'peer_meta, 'conn_meta) connection * 'msg) Lwt.t

(** [send net peer msg] is a thread that returns when [msg] has been
    successfully enqueued in the send queue. *)
val send :
  ('msg, 'peer_meta, 'conn_meta) net ->
  ('msg, 'peer_meta, 'conn_meta) connection ->
  'msg ->
  unit tzresult Lwt.t

(** [try_send net peer msg] is [true] if [msg] has been added to the
    send queue for [peer], [false] otherwise *)
val try_send :
  ('msg, 'peer_meta, 'conn_meta) net ->
  ('msg, 'peer_meta, 'conn_meta) connection ->
  'msg ->
  bool

(** Send a message to all peers *)
val broadcast : ('msg, 'peer_meta, 'conn_meta) net -> 'msg -> unit

val fold_connections :
  ('msg, 'peer_meta, 'conn_meta) net ->
  init:'a ->
  f:(P2p_peer.Id.t -> ('msg, 'peer_meta, 'conn_meta) connection -> 'a -> 'a) ->
  'a

val iter_connections :
  ('msg, 'peer_meta, 'conn_meta) net ->
  (P2p_peer.Id.t -> ('msg, 'peer_meta, 'conn_meta) connection -> unit) ->
  unit

val on_new_connection :
  ('msg, 'peer_meta, 'conn_meta) net ->
  (P2p_peer.Id.t -> ('msg, 'peer_meta, 'conn_meta) connection -> unit) ->
  unit

val greylist_addr : ('msg, 'peer_meta, 'conn_meta) net -> P2p_addr.t -> unit

val greylist_peer : ('msg, 'peer_meta, 'conn_meta) net -> P2p_peer.Id.t -> unit

val watcher :
  ('msg, 'peer_meta, 'conn_meta) net ->
  P2p_connection.P2p_event.t Lwt_stream.t * Lwt_watcher.stopper
