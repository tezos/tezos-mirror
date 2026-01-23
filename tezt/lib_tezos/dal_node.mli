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

val disable_shard_validation_environment_variable : string

val ignore_topics_environment_variable : string

val allow_regular_publication_environment_variable : string

type publish_slots_regularly = {
  frequency : int;
  slot_index : int;
  secret_key : Account.secret_key;
}

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
  ?disable_shard_validation:bool ->
  ?disable_amplification:bool ->
  ?ignore_pkhs:string list ->
  ?publish_slots_regularly:publish_slots_regularly ->
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
  ?disable_shard_validation:bool ->
  ?disable_amplification:bool ->
  ?ignore_pkhs:string list ->
  ?publish_slots_regularly:publish_slots_regularly ->
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

(** Get the identity file of a dal node. *)
val identity_file : t -> string

(** [run ?wait_ready ?env ?event_level node] launches the given dal
    node where env is a map of environment variable.

    If [wait_ready] is [true], the promise waits for the dal node to be ready.
    [true] by default.

    [event_level] allows to determine the printed levels. By default,
    it is set to [`Info]. *)
val run :
  ?wait_ready:bool ->
  ?env:string String_map.t ->
  ?event_level:Daemon.Level.default_level ->
  t ->
  unit Lwt.t

(** Return pid of the process if running. *)
val pid : t -> int option

(** Return the path of the daemon. *)
val path : t -> string

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
val wait_for :
  ?timeout:float ->
  ?where:string ->
  t ->
  string ->
  (JSON.t -> 'a option) ->
  'a Lwt.t

(** [is_running_not_ready dal_node] returns true if the given node is
    running but its status is not ready *)
val is_running_not_ready : t -> bool

(** Wait until a DAL node terminates and check its status.

    If the DAL node is not running,
    or if the [Process.check_error] function fails, fail the test. *)
val check_error : ?exit_code:int -> ?msg:Base.rex -> t -> unit Lwt.t

(** Wait until a node terminates and return its status. If the node is not
    running, make the test fail. *)
val wait : t -> Unix.process_status Lwt.t

val spawn_config_init :
  ?expected_pow:float ->
  ?peers:string list ->
  ?attester_profiles:string list ->
  ?operator_profiles:int list ->
  ?observer_profiles:int list ->
  ?bootstrap_profile:bool ->
  ?history_mode:history_mode ->
  ?slots_backup_uris:string list ->
  ?trust_slots_backup_uris:bool ->
  ?batching_time_interval:string ->
  t ->
  Process.t

(** Run [octez-dal-node config init].

    [expected_pow] allows to change the PoW difficulty. Default value is 0.
*)
val init_config :
  ?expected_pow:float ->
  ?peers:string list ->
  ?attester_profiles:string list ->
  ?operator_profiles:int list ->
  ?observer_profiles:int list ->
  ?bootstrap_profile:bool ->
  ?history_mode:history_mode ->
  ?slots_backup_uris:string list ->
  ?trust_slots_backup_uris:bool ->
  ?batching_time_interval:string ->
  t ->
  unit Lwt.t

val spawn_config_update :
  ?expected_pow:float ->
  ?peers:string list ->
  ?attester_profiles:string list ->
  ?operator_profiles:int list ->
  ?observer_profiles:int list ->
  ?bootstrap_profile:bool ->
  ?history_mode:history_mode ->
  ?slots_backup_uris:string list ->
  ?trust_slots_backup_uris:bool ->
  ?batching_time_interval:string ->
  t ->
  Process.t

val update_config :
  ?expected_pow:float ->
  ?peers:string list ->
  ?attester_profiles:string list ->
  ?operator_profiles:int list ->
  ?observer_profiles:int list ->
  ?bootstrap_profile:bool ->
  ?history_mode:history_mode ->
  ?slots_backup_uris:string list ->
  ?trust_slots_backup_uris:bool ->
  ?batching_time_interval:string ->
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

(** Load and return the current value of the last finalized level processed by
    the crawler and stored in store/last_processed_level KVS file. The function
    returns [None] in case of error (e.g. file not found, file locked, ...). *)
val load_last_finalized_processed_level : t -> int option Lwt.t

(** [debug_print_store_schemas ?path ?hooks ()] calls [path debug
    print store schemas] where:
    - [path] is the provided executable path (defaults to
      [Constant.octez_dal_node]),
    - [hooks] are attached to the process (defaults to [None]). *)
val debug_print_store_schemas :
  ?path:string -> ?hooks:Process_hooks.t -> unit -> unit Lwt.t

(** [snapshot_export dal_node ?endpoint ?min_published_level
    ?max_published_level ?slots output_file] exports a snapshot of the DAL
    node's store to [output_file].
    If [endpoint] is provided, it overrides the endpoint in the config file.
    [min_published_level] and [max_published_level] are optional level ranges
    to export, and [slots] the optional list of slots to export. *)
val snapshot_export :
  t ->
  ?endpoint:Endpoint.t ->
  ?min_published_level:int32 ->
  ?max_published_level:int32 ->
  ?slots:int list ->
  string ->
  unit Lwt.t

(** [snapshot_import dal_node ?endpoint ?min_published_level
    ?max_published_level ?slots input_file] imports a snapshot into the DAL
    node's store from [input_file].
    If [endpoint] is provided, it overrides the endpoint in the config file.
    [min_published_level] and [max_published_level] are optional level ranges
    to import, and [slots] the optional list of slots to import. *)
val snapshot_import :
  t ->
  ?no_check:bool ->
  ?endpoint:Endpoint.t ->
  ?min_published_level:int32 ->
  ?max_published_level:int32 ->
  ?slots:int list ->
  string ->
  unit Lwt.t

(** The Proxy module provides functionality to create a proxy server
    that can intercept and mock responses for DAL node requests. *)
module Proxy : sig
  (** A proxy instance. *)
  type proxy

  (** Represents a possible response from a proxy route. *)
  type answer = [`Response of string | `Stream of Cohttp_lwt.Body.t]

  (** A route definition. *)
  type route

  (** Creates a route for the proxy, containing a [path] pattern and a
      [callback].
      The [callback] is provided with the [path] actually matched
      together with a [fetch_answer] callback to retrieve the DAL node
      answer for the given [path]. The [path] could be used when one
      wants to retreive path arguments. The [fetch_answer] callback
      could be used when one only want to modify a field in the honest
      response. *)
  val route :
    path:Re.Str.regexp ->
    callback:
      (path:string ->
      fetch_answer:(unit -> Ezjsonm.t Lwt.t) ->
      fetch_stream:(unit -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t) ->
      answer option Lwt.t) ->
    route

  (** Creates a new proxy instance. *)
  val make :
    name:string ->
    attestation_lag:int ->
    number_of_slots:int ->
    faulty_delegate:string ->
    target_attested_level:int ->
    proxy

  (** Starts running the proxy server. *)
  val run : proxy -> honest_dal_node:t -> faulty_dal_node:t -> unit

  (** Stops the proxy server. *)
  val stop : proxy -> unit
end

(** This module provides a mock HTTP server for selected RPCs.
    It responds to registered routes with mock data and fails all other requests.
    It is similar with the {!Proxy} module. *)
module Mockup : sig
  (** An instance of the mockup server. *)
  type t

  (** Represents a possible response from a mocked-up route. *)
  type answer = [`Response of string | `Stream of string Lwt_stream.t]

  (** A route definition. *)
  type route

  (** Creates a route for the mockup, containing a [path_pattern] pattern and a
      [callback].
      The [callback] is provided with the [path] actually matched and is used to
      retrieve the DAL node answer for the given [path]. *)
  val route :
    path_pattern:string ->
    callback:(path:string -> answer option Lwt.t) ->
    route

  (** Creates a new mockup instance. *)
  val make : name:string -> routes:route list -> t

  (** Starts the mockup server. *)
  val run : t -> port:int -> unit

  (** Stops the mockup server. *)
  val stop : t -> unit
end

(** This module provides a mock server for the RPCs needed by the baker. It is
    based on the {!Mockup} module. *)
module Mockup_for_baker : sig
  type t

  (** [make ~name ~attestation_lag ~attesters ~attestable_slots] creates a new
      instance of the mockup server with name [name].

      It responds to the 'GET /profiles' RPC with an attester profile based
      on the provided [attesters].

      It responds to the 'GET
      /profiles/<attester>/attested_levels/<attested_level>/attestable_slots'
      with the list of attestable slots provided by [attestable_slots ~attester
      ~attested_level]. The parameter [attestation_lag] is needed to compute the
      corresponding [published_level = attested_level - attestation_lag] in the
      RPC's response. *)
  val make :
    name:string ->
    attestation_lag:int ->
    attesters:string list ->
    attestable_slots:(attester:string -> attested_level:int -> bool list) ->
    t

  (** Starts running the mockup server at the given port. *)
  val run : t -> port:int -> unit

  (** Stops the proxy server. *)
  val stop : t -> unit
end
