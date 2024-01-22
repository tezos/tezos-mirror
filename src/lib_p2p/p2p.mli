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
  proof_of_work_target : Tezos_crypto.Crypto_box.pow_target;
      (** Expected level of proof of work of peers' identity. *)
  trust_discovered_peers : bool;
      (** If [true], peers discovered on the local network will be trusted. *)
  reconnection_config : Point_reconnection_config.t;
      (** The reconnection delat configuration. *)
  disable_peer_discovery : bool;
      (** If set to [true], the p2p layer will not participate to the peer
          discovery mechanism. The p2p layer will not be able to find new peers
          to connect with. For more details, refers to field
          [disable_peer_discovery] of {!type:P2p_connect_handler.config}.  *)
}

(** Type of a P2P layer instance *)
type ('msg, 'peer_meta, 'conn_meta) t

type ('msg, 'peer_meta, 'conn_meta) net = ('msg, 'peer_meta, 'conn_meta) t

(** A connection to a peer *)
type ('msg, 'peer_meta, 'conn_meta) connection

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

(** Main network initialization function

    [received_msg_hook] is a function that is called every time a message
    ['msg] is received.
    [sent_msg_hook] is a function that is called every time a message
    ['msg] is sent.
    [broadcasted_msg_hook] is a function that is called every time a message
    ['msg] is broadcasted.
    *)
val create :
  config:config ->
  limits:Tezos_p2p_services.P2p_limits.t ->
  ?received_msg_hook:(('msg, 'peer_meta, 'conn_meta) connection -> 'msg -> unit) ->
  ?sent_msg_hook:(('msg, 'peer_meta, 'conn_meta) connection -> 'msg -> unit) ->
  ?broadcasted_msg_hook:
    (('msg, 'peer_meta, 'conn_meta) connection P2p_peer.Table.t ->
    ?except:(('msg, 'peer_meta, 'conn_meta) connection -> bool) ->
    ?alt:(('msg, 'peer_meta, 'conn_meta) connection -> bool) * 'msg ->
    'msg ->
    unit) ->
  'peer_meta P2p_params.peer_meta_config ->
  'conn_meta P2p_params.conn_meta_config ->
  'msg P2p_params.message_config ->
  ('msg, 'peer_meta, 'conn_meta) net tzresult Lwt.t

val activate : ('msg, 'peer_meta, 'conn_meta) net -> unit

(** Return one's peer_id *)
val peer_id : ('msg, 'peer_meta, 'conn_meta) net -> P2p_peer.Id.t

(** A maintenance operation : try and reach the ideal number of peers *)
val maintain : ('msg, 'peer_meta, 'conn_meta) net -> unit tzresult Lwt.t

(** Voluntarily drop some peers and replace them by new buddies *)
val roll : ('msg, 'peer_meta, 'conn_meta) net -> unit Lwt.t

(** Close all connections properly *)
val shutdown : ('msg, 'peer_meta, 'conn_meta) net -> unit Lwt.t

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
  reason:string ->
  ('msg, 'peer_meta, 'conn_meta) connection ->
  unit Lwt.t

val global_stat : ('msg, 'peer_meta, 'conn_meta) net -> P2p_stat.t

(** Accessors for meta information about a global identifier *)
val get_peer_metadata :
  ('msg, 'peer_meta, 'conn_meta) net -> P2p_peer.Id.t -> 'peer_meta

val set_peer_metadata :
  ('msg, 'peer_meta, 'conn_meta) net -> P2p_peer.Id.t -> 'peer_meta -> unit

(** [connect net ?trusted ?expected_peer_id ?timeout point] attempts to
    establish a connection to [point] within an optional duration [timeout]. The
    optional arguments [trusted] and [expected_peer_id] can be used to specify
    whether the connection should be trusted and to provide the expected remote
    peer's id, respectively. *)
val connect :
  ('msg, 'peer_meta, 'conn_meta) net ->
  ?trusted:bool ->
  ?expected_peer_id:P2p_peer.Id.t ->
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

(** [broadcast net connections ~except ~(alt:if_conn,then_msg) msg] will
    send messages to all [connections] that do not satisfy the
    predicate [except]. [alt] can be used to send an alternative
    message [then_msg] to connections that do satisfy the [if_conn]
    predicate. [?except] and [?alt] can be used to selectively
    transfer messages.

    Broadcasting is best effort and nothing ensures that messages are
    actually received (https://gitlab.com/tezos/tezos/-/issues/4205).
*)
val broadcast :
  ('msg, 'peer_meta, 'conn_meta) net ->
  ('msg, 'peer_meta, 'conn_meta) connection P2p_peer.Table.t ->
  ?except:(('msg, 'peer_meta, 'conn_meta) connection -> bool) ->
  ?alt:(('msg, 'peer_meta, 'conn_meta) connection -> bool) * 'msg ->
  'msg ->
  unit

val fold_connections :
  ('msg, 'peer_meta, 'conn_meta) net ->
  init:'a ->
  f:(P2p_peer.Id.t -> ('msg, 'peer_meta, 'conn_meta) connection -> 'a -> 'a) ->
  'a

val iter_connections :
  ('msg, 'peer_meta, 'conn_meta) net ->
  (P2p_peer.Id.t -> ('msg, 'peer_meta, 'conn_meta) connection -> unit) ->
  unit

(** [on_new_connection p2p f] registers a function [f] that is called
    everytime we were successfully connected to a peer (after having
    initialised a secured channel).

    This function will not be called if we started to connect to a
    peer and the connection was rejected during the initialisation of
    the connection. For example, if the peer is using a different
    network version. *)
val on_new_connection :
  ('msg, 'peer_meta, 'conn_meta) net ->
  (P2p_peer.Id.t -> ('msg, 'peer_meta, 'conn_meta) connection -> unit) ->
  unit

(** [on_disconnection p2p f] registers a function [f] that is called
    everytime we disconnect from a peer after we were successfully
    connected to a peer (this call may happen at any moment during the
    disconnection). The library ensures that anytime [f peer] is
    called, then for any callback registered with [on_new_connection],
    those callbacks would have been called with [peer] in the first
    place (if the callbacks were registered at that time).

    A direct implication of this is that any callback registered with
    this function will not be called if we started to connect to a
    peer and we disconnect from it because the connection was rejected
    somehow.
*)
val on_disconnection :
  ('msg, 'peer_meta, 'conn_meta) net -> (P2p_peer.Id.t -> unit) -> unit

val greylist_addr : ('msg, 'peer_meta, 'conn_meta) net -> P2p_addr.t -> unit

val greylist_peer : ('msg, 'peer_meta, 'conn_meta) net -> P2p_peer.Id.t -> unit

val watcher :
  ('msg, 'peer_meta, 'conn_meta) net ->
  P2p_connection.P2p_event.t Lwt_stream.t * Lwt_watcher.stopper

(**/**)

module Internal_for_tests : sig
  (** [raw_broadcast connections ~except ~(alt:if_conn,then_msg)
      msg] is similarly to broadcast, but for testing purposes:
      - it exposes the internal `connection = P2p_conn.t` type,
      - it does not notify broadcasted hooks,
      - it does not require an argument of type net.
  *)
  val raw_broadcast :
    ('msg, 'peer_meta, 'conn_meta) P2p_conn.t P2p_peer.Table.t ->
    ?except:(('msg, 'peer_meta, 'conn_meta) P2p_conn.t -> bool) ->
    ?alt:(('msg, 'peer_meta, 'conn_meta) connection -> bool) * 'msg ->
    'msg ->
    unit
end
