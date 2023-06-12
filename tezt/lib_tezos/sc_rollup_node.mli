(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Spawn smart-contract rollup nodes and control them. *)

(** This module follows the same conventions as in {!Node}. We write
   [sc node] to refer to [smart-contract rollup node]. *)

(** Smart contract rollup node states. *)
type t

type mode = Batcher | Custom | Maintenance | Observer | Operator | Accuser

(** Returns the associated {!mode}, fails if the mode is not valid. *)
val mode_of_string : string -> mode

(** Create a smart contract rollup node.

    A smart contract rollup node is associated to a tezos node
    passed as argument.

    The standard output and standard error output of the sc node will
    be logged with prefix [name] and color [color].

    Default [data_dir] is a temporary directory
    which is always the same for each [name].

    [dal_node] is a node node from the data availability layer the rollup should
    be connected to.

    Default [event_pipe] is a temporary file
    whose name is derived from [name]. It will be created
    as a named pipe so that sc node events can be received.

    Default values for [net_port] or [rpc_port] are chosen automatically
    with values starting from 17384 (configurable with `--starting-port`).
    They are used by [config_init]
    and by functions from the [Client] module. They are not used by [run],
    so if you do not call [config_init] or generate the configuration file
    through some other means, your sc node will not listen.

*)
val create :
  protocol:Protocol.t ->
  ?runner:Runner.t ->
  ?path:string ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?data_dir:string ->
  base_dir:string ->
  ?event_pipe:string ->
  ?rpc_host:string ->
  ?rpc_port:int ->
  ?operators:(string * string) list ->
  ?default_operator:string ->
  ?dal_node:Dal_node.t ->
  mode ->
  Node.t ->
  t

(** Do not assume we are running the rollup node against a local octez node. *)
val create_with_endpoint :
  ?protocol:Protocol.t ->
  ?runner:Runner.t ->
  ?path:string ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?data_dir:string ->
  base_dir:string ->
  ?event_pipe:string ->
  ?rpc_host:string ->
  ?rpc_port:int ->
  ?operators:(string * string) list ->
  ?default_operator:string ->
  ?dal_node:Dal_node.t ->
  mode ->
  Client.endpoint ->
  t

(** Get the name of an sc node. *)
val name : t -> string

(** Get the RPC host given as [--rpc-addr] to an sc node. *)
val rpc_host : t -> string

(** Get the RPC port given as [--rpc-addr] to an sc node. *)
val rpc_port : t -> int

(** Return the endpoint of the sc node, i.e., http://rpc_host:rpc_port. *)
val endpoint : t -> string

(** Get the data-dir of an sc node. *)
val data_dir : t -> string

(** Get the base-dir of an sc node *)
val base_dir : t -> string

(** Wait until an sc node terminates and check its status.

    If the sc node is not running,
    or if the process returns an exit code which is not [exit_code],
    or if [msg] does not match the stderr output, fail the test.

    If [exit_code] is not specified, any non-zero code is accepted.
    If no [msg] is given, the stderr is ignored.*)
val check_error : ?exit_code:int -> ?msg:Base.rex -> t -> unit Lwt.t

(** [run ?event_level ?event_sections_levels ?loser_mode ?wait_ready node
    rollup_address arguments ] launches the given smart contract rollup node for
    the rollup at [rollup_address] with the given extra arguments. [event_level]
    and [event_sections_levels] allow to select which events we want the node to
    emit (see {!Daemon}). [legacy] (by default [false]) must be set if we want
    to use the legacy [run] command of the node (which requires a config file to
    exist). If [wait_ready] is [false], tezt does not wait for the node to be
    ready. *)
val run :
  ?legacy:bool ->
  ?event_level:Daemon.Level.default_level ->
  ?event_sections_levels:(string * Daemon.Level.level) list ->
  ?loser_mode:string ->
  ?wait_ready:bool ->
  t ->
  string ->
  string list ->
  unit Lwt.t

(** [spawn_run node rollup_address arguments] is a lightweight version of {!run}
    that spawns a process. *)
val spawn_run : t -> string -> string list -> Process.t

(** Wait until a node terminates and return its status. If the node is not
   running, make the test fail. *)
val wait : t -> Unix.process_status Lwt.t

(** Returns [None] if node is already terminated or returns the node process if
    it still running. *)
val process : t -> Process.t option

(** Send SIGTERM and wait for the process to terminate.

    Default [timeout] is 30 seconds, after which SIGKILL is sent. *)
val terminate : ?timeout:float -> t -> unit Lwt.t

(** Send SIGKILL and wait for the process to terminate. *)
val kill : t -> unit Lwt.t

(** Initialize the rollup node configuration file with
    [octez-sc-rollup-node-alpha config init].  Returns the name of the resulting
    configuration file. *)
val config_init :
  t -> ?force:bool -> ?loser_mode:string -> string -> string Lwt.t

(** Initialize the rollup node configuration file with
    [octez-sc-rollup-node-alpha config init] and return the corresponding
    process. *)
val spawn_config_init :
  t -> ?force:bool -> ?loser_mode:string -> string -> Process.t

module Config_file : sig
  (** Sc node configuration files. *)

  (** Returns the configuration file name for a rollup node. *)
  val filename : t -> string

  (** Read the configuration file ([config.json]) of an sc node. *)
  val read : t -> JSON.t

  (** Write the configuration file of an sc node, replacing the existing one. *)
  val write : t -> JSON.t -> unit

  (** Update the configuration file of an sc node. If the sc node is already
     running, it needs to be restarted manually.

      Example: [Node.Config_file.update node (JSON.put ("p2p", new_p2p_config))] *)
  val update : t -> (JSON.t -> JSON.t) -> unit
end

(** Wait until the sc node is ready.

    More precisely, wait until a [node_is_ready] event occurs.
    If such an event already occurred, return immediately. *)
val wait_for_ready : t -> unit Lwt.t

(** Wait until the layer 1 of the sc node is synchronized with some
   given tezos level.

   More precisely, wait until a [new_head] event with a large enough
   level occurs.  If such an event already occurred, return
   immediately.

   If [timeout] is provided, stop waiting if [timeout] seconds have
   passed. *)
val wait_for_level : ?timeout:float -> t -> int -> int Lwt.t

(** Wait until the layer 1 of the sc node is synchronized with its
    underlying l1 node. *)
val wait_sync : t -> timeout:float -> int Lwt.t

(** [wait_for ?where sc_node event_name filter] waits for the SCORU node
    [sc_node] to emit an event named [name] (usually this is the name the event
    is declared with, concatenated with [".v0"]). [wait_for] continues to wait
    until an event which satisfies the [filter] (i.e. for which the function
    returns [Some _]) is produced, in which case the result of the filter is
    returned. [where], if present, should describe the constraint that [filter]
    applies. *)
val wait_for : ?where:string -> t -> string -> (JSON.t -> 'a option) -> 'a Lwt.t

(** Stops the rollup node and restart it, connected to another Tezos Layer 1
    node. *)
val change_node_and_restart :
  ?event_level:Daemon.Level.default_level -> t -> string -> Node.t -> unit Lwt.t
