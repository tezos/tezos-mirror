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

(** This module manages incoming [accept] and outgoing connections [connect].

    [connect] and [accept] try to authenticate the remote point, and agree
    on protocol version. They ultimately returns a [P2p_conn.t] which provides
    the highest-level view of a connection in [lib_p2p].

    TODO: properly document this modules, including side-effects and
          interaction with [P2p_pool].

    Functions of this module can trigger two types of events. They can *log*
    [P2p_connection.P2p_event.t], and they can trigger condition variables
    defined in [P2p_trigger.t]. *)

type ('msg, 'peer, 'conn) t

type config = {
  incoming_app_message_queue_size : int option;
      (** Size of the message queue for user messages (messages returned
      by this module's [read] function. *)
  private_mode : bool;
      (** If [true], only open outgoing/accept incoming connections
      to/from peers whose addresses are in [trusted_peers], and inform
      these peers that the identity of this node should not be revealed to
      the rest of the network. *)
  min_connections : int;
      (** Strict minimum number of connections
      (triggers [LogEvent.too_few_connections]). *)
  max_connections : int;
      (** Max number of connections. If it's reached, [connect] and
      [accept] will fail, i.e. not add more connections
      (also triggers [LogEvent.too_many_connections]). *)
  max_incoming_connections : int;
      (** Max not-yet-authentified incoming connections.
      Above this number, [accept] will start dropping incoming
      connections. *)
  incoming_message_queue_size : int option;
      (** Size of the incoming message queue internal of a peer's Reader
      (See [P2p_connection.accept]). *)
  outgoing_message_queue_size : int option;
      (** Size of the outgoing message queue internal to a peer's Writer
      (See [P2p_connection.accept]). *)
  binary_chunks_size : int option;
      (** Size (in bytes) of binary blocks that are sent to other
      peers. Default value is 64 kB. *)
  identity : P2p_identity.t;  (** Our identity. *)
  connection_timeout : Time.System.Span.t;
      (** Maximum time allowed to the establishment of a connection. *)
  authentication_timeout : Time.System.Span.t;
      (** Maximum time allowed to the establishment of a connection. *)
  greylisting_config : P2p_point_state.Info.greylisting_config;
      (** Delay granted to a peer to perform authentication. *)
  proof_of_work_target : Crypto_box.target;
      (** The greylisting configuration. *)
  listening_port : P2p_addr.port option;
      (** The proof of work target we require from peers. *)
}

(** [create ?p2p_version config pool message_config socket_meta_config
     scheduler triggers log answerer] returns a connection handler.

     [config] is a record of configuration parameters. [triggers] is a record
     of condition variable used to signal some events to other modules.
     [log] is a callback to signal events to the upper layer. [answerer] is
     a parameterized callback that defines how the p2p layer will reply to
     incoming [P2p_message.t]. *)
val create :
  ?p2p_versions:P2p_version.t list ->
  config ->
  ('msg, 'peer, 'conn) P2p_pool.t ->
  'msg P2p_params.message_config ->
  'conn P2p_params.conn_meta_config ->
  P2p_io_scheduler.t ->
  P2p_trigger.t ->
  log:(P2p_connection.P2p_event.t -> unit) ->
  answerer:'msg P2p_answerer.t Lazy.t ->
  ('msg, 'peer, 'conn) t

(** [config t] is the [config] argument passed to [t] at
    creation. *)
val config : _ t -> config

(** [connect ?timeout t point] tries to add a connection to [point]
    in [t] in less than [timeout]. *)
val connect :
  ?timeout:Time.System.Span.t ->
  ('msg, 'peer, 'conn) t ->
  P2p_point.Id.t ->
  ('msg, 'peer, 'conn) P2p_conn.t tzresult Lwt.t

(** [accept t fd point] instructs [t] to start the process of
    accepting a connection from [fd]. [point] is the id of the connecting
    host.

    Incoming connection from banned points, or when maximum number of
    connection is exceeded are refused. The maximum number of connections
    maybe be randomly increased by one. Socket [fd] is closed when the
    connection is refused. *)
val accept : ('msg, 'peer, 'conn) t -> P2p_fd.t -> P2p_point.Id.t -> unit

(** [stat t] is a snapshot of current bandwidth usage for the entire connected
    peers. *)
val stat : ('msg, 'peer, 'conn) t -> P2p_stat.t

(** [on_new_connection t f] installs [f] as a hook for new connections in [t].   *)
val on_new_connection :
  ('msg, 'peer, 'conn) t ->
  (P2p_peer.Id.t -> ('msg, 'peer, 'conn) P2p_conn.t -> unit) ->
  unit

val destroy : ('msg, 'peer, 'conn) t -> unit Lwt.t
