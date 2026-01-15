(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** Tezos P2P node states. *)
type t

val default_net_addr : string

val default_rpc_addr : string

val net_addr : t -> string

val net_port : t -> int

(** Send SIGTERM and wait for the process to terminate.

    Default [timeout] is 30 seconds, after which SIGKILL is sent. *)
val terminate : ?timeout:float -> t -> unit Lwt.t

(** See [Daemon.Make.wait_for]. *)
val wait_for :
  ?timeout:float ->
  ?where:string ->
  t ->
  string ->
  (JSON.t -> 'a option) ->
  'a Lwt.t

(** Wait until the P2P node is ready.

    More precisely, wait until a [p2p_node_is_ready] event occurs.
    If such an event already occurred, return immediately. *)
val wait_for_ready : t -> unit Lwt.t

(** Spawn [octez-p2p-node run].

    The resulting promise is fulfilled as soon as the P2P node has been spawned.
    It continues running in the background.

    [event_level] specifies the verbosity of the file descriptor sink.
    This must be at least [`Notice], which is the level of event
    ["p2p_node_is_ready.v0"], needed for {!wait_for_ready}.
    The default value is [`Info] which is also the default event level
    of the node.
 *)
val run :
  ?env:string String_map.t ->
  ?event_level:Daemon.Level.default_level ->
  t ->
  unit Lwt.t

(** Create a P2P node.

    The standard output and standard error output of the node will
    be logged with prefix [name] and color [color].

    Default [event_pipe] is a temporary file
    whose name is derived from [name]. It will be created
    as a named pipe so that P2P node events can be received.

    Default value for [net_addr] is either [Constant.default_host] if no [runner] is
    provided, or a value allowing the local Tezt program to connect to it
    if it is.

    Default values for [net_port] or [rpc_port] are chosen automatically
    with values starting from 16384 (configurable with `--starting-port`).
    They are used by [config_init]
    and by functions from the [Client] module. They are not used by [run],
    so if you do not call [config_init] or generate the configuration file
    through some other means, your node will not listen.

    If [runner] is specified, the node will be spawned on this
    runner using SSH. *)
val create :
  ?path:string ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?event_pipe:string ->
  ?runner:Runner.t ->
  ?peers:string list ->
  ?ping_interval:float ->
  ?discovery_addr:string ->
  ?net_addr:string ->
  ?net_port:int ->
  ?rpc_host:string ->
  ?rpc_port:int ->
  unit ->
  t

(** Initialize a P2P node.

    This {!create}s a node, runs {!run}, then waits for the P2P node to be ready,
    and finally returns the P2P node. *)
val init :
  ?path:string ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?event_pipe:string ->
  ?event_level:Daemon.Level.default_level ->
  ?runner:Runner.t ->
  ?peers:string list ->
  ?ping_interval:float ->
  ?discovery_addr:string ->
  ?net_addr:string ->
  ?net_port:int ->
  ?rpc_host:string ->
  ?rpc_port:int ->
  unit ->
  t Lwt.t

val send_raw_data : t -> data:string -> unit Lwt.t

val as_rpc_endpoint : ?local:bool -> t -> Endpoint.t
