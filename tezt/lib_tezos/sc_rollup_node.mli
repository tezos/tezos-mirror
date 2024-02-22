(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

type purpose = Operating | Batching | Cementing | Recovering

type operation_kind =
  | Publish
  | Add_messages
  | Cement
  | Timeout
  | Refute
  | Recover
  | Execute_outbox_message

type mode =
  | Batcher
  | Custom of operation_kind list
  | Maintenance
  | Observer
  | Operator
  | Accuser
  | Bailout

type history_mode = Archive | Full

(** Octez smart rollup node command-line arguments. *)
type argument =
  | Data_dir of string
  | Rpc_addr of string
  | Rpc_port of int
  | Log_kernel_debug
  | Log_kernel_debug_file of string
  | Metrics_addr of string
  | Injector_attempts of int
  | Boot_sector_file of string
  | Dac_observer of Dac_node.t
  | Loser_mode of string
  | No_degraded
  | Gc_frequency of int
  | History_mode of history_mode
  | Dal_node of Dal_node.t
  | Mode of mode
  | Rollup of string
  | Pre_images_endpoint of string

type event = {name : string; value : JSON.t; timestamp : float}

(** Returns the associated {!mode}, fails if the mode is not valid. *)
val mode_of_string : string -> mode

(** Returns the string representation of an history mode. *)
val string_of_history_mode : history_mode -> string

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

    [history_mode] is [full] by default to make the rollup runs the
    GC, and the [gc_frequency] is [1] by default to make it runs on
    every occasion during tests.

*)
val create :
  ?runner:Runner.t ->
  ?path:string ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?data_dir:string ->
  base_dir:string ->
  ?event_pipe:string ->
  ?metrics_addr:string ->
  ?metrics_port:int ->
  ?rpc_host:string ->
  ?rpc_port:int ->
  ?operators:(purpose * string) list ->
  ?default_operator:string ->
  ?dal_node:Dal_node.t ->
  ?loser_mode:string ->
  ?allow_degraded:bool ->
  ?gc_frequency:int ->
  ?history_mode:history_mode ->
  ?password_file:string ->
  mode ->
  Node.t ->
  t

(** Do not assume we are running the rollup node against a local octez node. *)
val create_with_endpoint :
  ?runner:Runner.t ->
  ?path:string ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?data_dir:string ->
  base_dir:string ->
  ?event_pipe:string ->
  ?metrics_addr:string ->
  ?metrics_port:int ->
  ?rpc_host:string ->
  ?rpc_port:int ->
  ?operators:(purpose * string) list ->
  ?default_operator:string ->
  ?dal_node:Dal_node.t ->
  ?loser_mode:string ->
  ?allow_degraded:bool ->
  ?gc_frequency:int ->
  ?history_mode:history_mode ->
  ?password_file:string ->
  mode ->
  Client.endpoint ->
  t

(** [write_in_stdin rollup_node str] write str into the stdin of the
    rollup node process. *)
val write_in_stdin : t -> string -> unit Lwt.t

(** Get the name of an sc node. *)
val name : t -> string

(** Get the color of the logs of a smart rollup node. *)
val color : t -> Log.Color.t

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

(** Get the metrics address and port of a node. *)
val metrics : t -> string * int

val string_of_purpose : purpose -> string

(** Wait until an sc node terminates and check its status.

    If the sc node is not running,
    or if the process returns an exit code which is not [exit_code],
    or if [msg] does not match the stderr output, fail the test.

    If [exit_code] is not specified, any non-zero code is accepted.
    If no [msg] is given, the stderr is ignored.*)
val check_error : ?exit_code:int -> ?msg:Base.rex -> t -> unit Lwt.t

(** [run ?event_level ?event_sections_levels ?loser_mode ?allow_degraded
    ?wait_ready node rollup_address arguments ]
    launches the given smart contract rollup node for the rollup at
    [rollup_address] with the given extra arguments. [event_level] and
    [event_sections_levels] allow to select which events we want the node to
    emit (see {!Daemon}). [legacy] (by default [false]) must be set if we want
    to use the legacy [run] command of the node (which requires a config file to
    exist). If [wait_ready] is [false], tezt does not wait for the node to be
    ready. If [restart] is [true], it will stop and restart the node if it is
    already running.  *)
val run :
  ?legacy:bool ->
  ?restart:bool ->
  ?mode:mode ->
  ?event_level:Daemon.Level.default_level ->
  ?event_sections_levels:(string * Daemon.Level.level) list ->
  ?wait_ready:bool ->
  ?password_file:string ->
  t ->
  string ->
  argument list ->
  unit Lwt.t

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
val config_init : ?force:bool -> t -> string -> string Lwt.t

(** Initialize the rollup node configuration file with
    [octez-sc-rollup-node-alpha config init] and return the corresponding
    process. *)
val spawn_config_init : ?force:bool -> t -> string -> Process.t

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

(** Unbounded variant of {!wait_for_sync}. Do not use in a Tezt tests, as it
    has been proven time and again that it is a source of waste of CI time when
    a test is buggy or flaky.

    This variant of {!wait_for_sync} should not be used in sandboxes, as it has
    been witnessed time and time again that these tests are more subject to
    race conditions when setting up rollup infrastructure. On open testnets
    like weeklynet and dailynet, this does not happen because of the large
    block time. *)
val unsafe_wait_sync : ?path_client:string -> ?timeout:float -> t -> int Lwt.t

(** Wait until the layer 1 of the sc node is synchronized with its
    underlying l1 node. *)
val wait_sync : ?path_client:string -> t -> timeout:float -> int Lwt.t

(** [wait_for ?where sc_node event_name filter] waits for the SCORU node
    [sc_node] to emit an event named [name] (usually this is the name the event
    is declared with, concatenated with [".v0"]). [wait_for] continues to wait
    until an event which satisfies the [filter] (i.e. for which the function
    returns [Some _]) is produced, in which case the result of the filter is
    returned. [where], if present, should describe the constraint that [filter]
    applies. *)
val wait_for : ?where:string -> t -> string -> (JSON.t -> 'a option) -> 'a Lwt.t

(** Add a callback to be called whenever the daemon emits an event. *)
val on_event : t -> (event -> unit) -> unit

(** Stops the rollup node and restart it, connected to another Tezos Layer 1
    node. *)
val change_node_and_restart :
  ?event_level:Daemon.Level.default_level -> t -> string -> Node.t -> unit Lwt.t

(** Change the rollup mode. This does not terminate nor restart the
    node. Change will take effect when the node is run/restart. *)
val change_node_mode : t -> mode -> t

(** [dump_durable_storage ~sc_rollup_node ~dump ?string ()] writes to [dump] the current
    state of the WASM PVM from [sc_rollup_node]. *)
val dump_durable_storage :
  sc_rollup_node:t -> dump:string -> ?block:string -> unit -> unit Lwt.t

(** [export_snapshot ?compress_on_the_fly ?compact rollup_node dir] creates a
    snapshot of the rollup node in directory [dir]. *)
val export_snapshot :
  ?compress_on_the_fly:bool ->
  ?compact:bool ->
  t ->
  string ->
  string Runnable.process

(** [import_snapshot ?force rollup_node ~snapshot_file] imports the snapshot
    [snapshot_file] in the rollup node [rollup_node].  *)
val import_snapshot :
  ?force:bool -> t -> snapshot_file:string -> unit Runnable.process

(** Expose the RPC server address of this node as a foreign endpoint. *)
val as_rpc_endpoint : t -> Endpoint.t

module RPC : RPC_core.CALLERS with type uri_provider := t
