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

(**


*)

type ('msg, 'peer, 'conn) t

type config = {
  incoming_app_message_queue_size : int option;
  private_mode : bool;
  min_connections : int;
  max_connections : int;
  max_incoming_connections : int;
  incoming_message_queue_size : int option;
  outgoing_message_queue_size : int option;
  binary_chunks_size : int option;
  identity : P2p_identity.t;
  connection_timeout : Time.System.Span.t;
  authentication_timeout : Time.System.Span.t;
  greylisting_config : P2p_point_state.Info.greylisting_config;
  proof_of_work_target : Crypto_box.target;
  listening_port : P2p_addr.port option;
}

type 'msg message_config = {
  encoding : 'msg P2p_message.encoding list;
  chain_name : Distributed_db_version.name;
  distributed_db_versions : Distributed_db_version.t list;
}

val create :
  ?p2p_versions:P2p_version.t list ->
  config ->
  ('msg, 'peer, 'conn) P2p_pool.t ->
  'msg message_config ->
  'conn P2p_socket.metadata_config ->
  P2p_io_scheduler.t ->
  P2p_events.t ->
  log:(P2p_connection.P2p_event.t -> unit) ->
  answerer:'msg P2p_answerer.t Lazy.t ->
  ('msg, 'peer, 'conn) t

(** [connect ?timeout t point] tries to add a connection to [point]
    in [t] in less than [timeout] seconds. *)
val connect :
  ?timeout:Time.System.Span.t ->
  ('msg, 'peer, 'conn) t ->
  P2p_point.Id.t ->
  ('msg, 'peer, 'conn) P2p_conn.t tzresult Lwt.t

(** [accept t fd point] instructs [t] to start the process of
    accepting a connection from [fd]. Used by [P2p_welcome]. *)
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
