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

(** Spawn Data-availability-layer (DAL) nodes and control them *)

(** DAL Node state *)
type t

(** Period for the shards to be kept in the storage
    [Full] : never delete
    [Auto] : period depending on the node profile
    [Custom (i)] : keeps the shards during [i] blocks *)
type history_mode = Full | Auto | Custom of int

(** Creates a DAL node *)

val create :
  ?runner:Runner.t ->
  ?path:string ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?data_dir:string ->
  ?event_pipe:string ->
  ?rpc_host:string ->
  ?rpc_port:int ->
  ?listen_addr:string ->
  ?public_addr:string ->
  ?metrics_addr:string ->
  node:Node.t ->
  unit ->
  t

val create_from_endpoint :
  ?runner:Runner.t ->
  ?path:string ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?data_dir:string ->
  ?event_pipe:string ->
  ?rpc_host:string ->
  ?rpc_port:int ->
  ?listen_addr:string ->
  ?public_addr:string ->
  ?metrics_addr:string ->
  l1_node_endpoint:Endpoint.t ->
  unit ->
  t

(** Get the name of an dal node. *)
val name : t -> string

(** Get the RPC host given as [--rpc-addr] to an dal node. *)
val rpc_host : t -> string

(** Get the RPC port given as [--rpc-addr] to an dal node. *)
val rpc_port : t -> int

(** Return the endpoint of the DAL node's RPC server, i.e.,
    http://rpc_host:rpc_port. If [local] is given ([false] by default),
    then [Constant.default_host] is used (it overrides [rpc-addr] or
    the [runner] argument). *)
val rpc_endpoint : ?local:bool -> t -> string

(** Get the node's point pair "address:port" given as [--net-addr] to a dal node. *)
val listen_addr : t -> string

(** Get the node's metrics server point pair "address:port" given as [--metrics-addr] to a dal node. *)
val metrics_addr : t -> string

val metrics_port : t -> int

(** Get the data-dir of an dal node. *)
val data_dir : t -> string

(** [run ?wait_ready ?env ?event_level node] launches the given dal
    node where env is a map of environment variable.

    If [wait_ready] is [true], the promise waits for the dal node to be ready.
    [true] by default.

    [event_level] allows to determine the printed levels. By default,
    it is set to [`Debug] by default.
*)
val run :
  ?wait_ready:bool ->
  ?env:string String_map.t ->
  ?event_level:Daemon.Level.default_level ->
  t ->
  unit Lwt.t

(** Send SIGTERM and wait for the process to terminate.

    Default [timeout] is 30 seconds, after which SIGKILL is sent. *)
val terminate : ?timeout:float -> t -> unit Lwt.t

(** Send SIGKILL and wait for the process to terminate. *)
val kill : t -> unit Lwt.t

(** Send SIGSTOP to a daemon. Do not wait for the process to terminate. *)
val stop : t -> unit Lwt.t

(** Shows in stdout every events sent by the node *)
val log_events : ?max_length:int -> t -> unit

(** See [Daemon.Make.wait_for]. *)
val wait_for : ?where:string -> t -> string -> (JSON.t -> 'a option) -> 'a Lwt.t

(** [is_running_not_ready dal_node] returns true if the given node is
    running but its status is not ready *)
val is_running_not_ready : t -> bool

(** Wait until a node terminates and return its status. If the node is not
    running, make the test fail. *)
val wait : t -> Unix.process_status Lwt.t

(** Run [octez-dal-node config init].

    [expected_pow] allows to change the PoW difficulty. Default value is 0.
*)
val init_config :
  ?expected_pow:float ->
  ?peers:string list ->
  ?attester_profiles:string list ->
  ?producer_profiles:int list ->
  ?observer_profiles:int list ->
  ?bootstrap_profile:bool ->
  ?history_mode:history_mode ->
  t ->
  unit Lwt.t

module Config_file : sig
  (** DAL node configuration files. *)

  (** Read the configuration file ([config.json]) of a DAL node. *)
  val read : t -> JSON.t

  (** Write the configuration file of a DAL node, replacing the existing one. *)
  val write : t -> JSON.t -> unit

  (** Update the configuration file of a DAL node. If the DAL node is already
      running, it needs to be restarted manually.

      Example:
      [Node.Config_file.update node (JSON.put ("expected_pow", "0.0"))] *)
  val update : t -> (JSON.t -> JSON.t) -> unit
end

(** Read the peer id from the node's identity file. *)
val read_identity : t -> string Lwt.t

(** Expose the RPC server address of this node as a foreign endpoint. *)
val as_rpc_endpoint : t -> Endpoint.t

(** Wait for a node to receive a given number of connections.

    [wait_for_connections node n] waits until [node] receives [n]
    ["new_connection.v0"] events. *)
val wait_for_connections : t -> int -> unit Lwt.t

(** Wait until the node is ready.

    More precisely, wait until a [dal_node_is_ready] event occurs. If such an
    event already occurred, return immediately. *)
val wait_for_ready : t -> unit Lwt.t

(** Wait for a node to receive a disconnection for some peer_id.

    [wait_for_disconnection node peer_id] waits until [node] receives a
    ["disconnected.v0"] event from the given peer id. *)
val wait_for_disconnection : t -> peer_id:string -> unit Lwt.t

val runner : t -> Runner.t option

val point_str : t -> string

module Agent : sig
  (* Function below are similar to their counter-part in the main module of this
     file except it takes an agent in parameter. This is to avoid silly mistakes
     when using agents with Tezt cloud.

     In the future, we could decide to merge the two by having an agent
     corresponding to localhost.
  *)

  val create : ?path:string -> ?name:string -> node:Node.t -> Agent.t -> t Lwt.t
end

(** Load and return the current value of the last finalized level processed by
    the crawler and stored in store/last_processed_level KVS file. The function
    returns [None] in case of error (e.g. file not found, file locked, ...). *)
val load_last_finalized_processed_level : t -> int option Lwt.t
