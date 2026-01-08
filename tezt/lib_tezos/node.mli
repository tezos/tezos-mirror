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

(** Spawn Tezos nodes and control them. *)

(** Convention: in this module, some functions implement node commands;
    those functions are named after those commands.
    For instance, [Node.config_init] corresponds to [octez-node config init],
    and [Node.run] corresponds to [octez-node run].

    The arguments of those functions are also named after the actual arguments.
    For instance, [?network] is named after [--network], to make
    [Node.config_init ~network:"carthagenet"] look as close as possible
    to [octez-node config init --network carthagenet].

    Most options have default values which are not necessarily the default values
    of [octez-node]. Indeed, the latter are tailored for Mainnet, but here we
    use defaults which are tailored for the sandbox. In particular, the default
    value for [?network] is ["sandbox"].
    However, if you specify an option such as [~network] or [~history_mode],
    they are passed to the node unchanged, to reduce surprises.

    These conventions are also followed in the [Client] module. *)

(** History modes for the node. *)

(** The parameter for [Full] And [Rolling] mode is called [additional_cycles].

    For the [Full] (resp. [Rolling]) mode it controls the number of
    contexts (resp. blocks) we preserved behind the [checkpoint] (aka
    the no fork point). Default in sandbox mode is [2] and [5] for
    mainnet parameters (see [preserved_cycles] in the protocol
    parameters). *)
type history_mode = Archive | Full of int option | Rolling of int option

(** Default values for [Full] and [Rolling] history modes *)

(** The default value for the [Full] history mode *)
val default_full : history_mode

(** The default value for the [Rolling] history mode *)
val default_rolling : history_mode

(** Values that can be passed to the node's [--media-type] argument *)
type media_type = Json | Binary | Any

(** Tezos node command-line arguments.

    Not all arguments are available here.
    Some are simply not implemented, and some are handled separately
    because they need special care. The latter are implemented as optional
    labeled arguments (e.g. [?net_port] and [?data_dir]).

    About [RPC_additional_addr], at least one RPC port is always passed by
    [Node.run], which causes the RPC ports from the configuration file to be
    ignored. So these arguments are not written in the config file when using
    [Node.init] and kept in the list of arguments of the persistent state to
    make sure [Node.run] passes them too.

    [Singleprocess] argument does not exist in the configuration file of the
    node. It is only known as a command-line option. [Node.init] will neither
    pass it to [Node.config] nor register it into node's arguments, but only
    use it for [Node.run] function.
*)
type argument =
  | Network of string  (** [--network] *)
  | History_mode of history_mode  (** [--history-mode] *)
  | Expected_pow of int  (** [--expected-pow] *)
  | Singleprocess  (** [--singleprocess] *)
  | Bootstrap_threshold of int  (** [--bootstrap-threshold] (deprecated) *)
  | Synchronisation_threshold of int  (** [--synchronisation-threshold] *)
  | Sync_latency of int  (** [--sync-latency] *)
  | Connections of int  (** [--connections] *)
  | Private_mode  (** [--private-mode] *)
  | Disable_p2p_maintenance  (** [--disable-p2p-maintenance] *)
  | Disable_p2p_swap  (** [--disable-p2p-swap] *)
  | Peer of string  (** [--peer] *)
  | No_bootstrap_peers  (** [--no-bootstrap-peers] *)
  | Media_type of media_type  (** [--media-type] *)
  | Metadata_size_limit of int option  (** --metadata-size-limit *)
  | Metrics_addr of string  (** [--metrics-addr] *)
  | Cors_origin of string  (** [--cors-origin] *)
  | Disable_mempool  (** [--disable-mempool] *)
  | Version  (** [--version] *)
  | RPC_additional_addr of string  (** [--rpc-addr] *)
  | RPC_additional_addr_external of string  (** [--external-rpc-addr] *)
  | Max_active_rpc_connections of int  (** [--max-active-rpc-connections] *)
  | Enable_http_cache_headers  (** [--enable-http-cache-headers] *)
  | Disable_context_pruning  (** [--disable_context-pruning] *)
  | Storage_maintenance_delay of string  (** [--storage-maintenance-delay]*)
  | Force_history_mode_switch  (** [--force-history-mode-switch] *)
  | Allow_yes_crypto  (** [--allow-yes-crypto] *)

(** A TLS configuration for the node: paths to a [.crt] and a [.key] file.

    Passed to [run] like commands through the [--rpc-tls] argument. *)
type tls_config = {certificate_path : string; key_path : string}

(** Tezos node states. *)
type t

(** This placeholder aims to handle the activation of the external RPC
    process for the Tezt nodes. Indeed, the external RPC process is an
    important feature that is not activated by default in the node and
    thus not tested on the CI. The tests on master are thus not
    testing the feature. To test the external RPC process anyway, a
    scheduled pipeline is launched every week, with the feature
    enabled, running all tests.
    When the scheduled pipeline is launched, the TZ_SCHEDULE_KIND
    environment variable is set to "EXTENDED_RPC_TESTS" to turn on the
    external RPC process. Look for the
    [ci/bin/custom_extended_test_pipeline.ml] file for more details. *)
val enable_external_rpc_process : bool

(** This placeholder aims to handle the activation of the
    singleprocess validation for the Tezt nodes. Indeed, the
    singleprocess validation is an alternative way of validating
    blocks that is not activated by default in the node and thus not
    tested by the CI. The tests on master are thus not testing the
    feature. To test the singleprocess validation anyway, a scheduled
    pipeline is launched every week, with the feature enabled, running
    all tests.
    When the scheduled pipeline is launched, the TZ_SCHEDULE_KIND
    environment variable is set to "EXTENDED_VALIDATION_TESTS" to turn
    on the singleprocess validation. Look for the
    [ci/bin/singleprocess_validation_pipeline.ml] file for more
    details. *)
val enable_singleprocess : bool

(** Create a node.

    This function just creates the [t] value, it does not call
    [identity_generate] nor [config_init] nor [run].

    The standard output and standard error output of the node will
    be logged with prefix [name] and color [color].

    Default [data_dir] is a temporary directory
    which is always the same for each [name].

    Default [event_pipe] is a temporary file
    whose name is derived from [name]. It will be created
    as a named pipe so that node events can be received.

    Default value for [net_addr] is either [Constant.default_host] if no [runner] is
    provided, or a value allowing the local Tezt program to connect to it
    if it is.

    Default [rpc_external] is [false]. If [rpc_external] is [true],
    the node will spawn a process for non-blocking RPCs.

    Default values for [net_port] or [rpc_port] are chosen automatically
    with values starting from 16384 (configurable with `--starting-port`).
    They are used by [config_init]
    and by functions from the [Client] module. They are not used by [run],
    so if you do not call [config_init] or generate the configuration file
    through some other means, your node will not listen.

    Default value for [allow_all_rpc] is [true].

    Default value for [max_active_rpc_connections] is [500].

    [local_rpc_server] specify whether or not the RPC server must be
    run locally, if true (default), or on a external process. It is
    not allowed yet to run both server kinds at the same time.

    The argument list is a list of configuration options that the node
    should run with. It is passed to the first run of [octez-node config init].
    It is also passed to all runs of [octez-node run] that occur before
    [octez-node config init]. If [Expected_pow] is given, it is also used as
    the default value for {!identity_generate}.

    If [runner] is specified, the node will be spawned on this
    runner using SSH. *)
val create :
  ?runner:Runner.t ->
  ?path:string ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?data_dir:string ->
  ?event_pipe:string ->
  ?net_addr:string ->
  ?net_port:int ->
  ?advertised_net_port:int ->
  ?metrics_addr:string ->
  ?metrics_port:int ->
  ?rpc_external:bool ->
  ?rpc_host:string ->
  ?rpc_port:int ->
  ?rpc_tls:tls_config ->
  ?allow_all_rpc:bool ->
  ?max_active_rpc_connections:int ->
  argument list ->
  t

(** Add an argument to a node as if it was passed to {!create}.

    The argument is passed to the next run of [octez-node config init].
    It is also passed to all runs of [octez-node run] that occur before
    the next [octez-node config init].

    There are some exceptions, see definition of type [argument]. *)
val add_argument : t -> argument -> unit

(** Add a [--peer] argument to a node.

    Usage: [add_peer node peer]

    Same as [add_argument node (Peer "<HOST>:<PORT>")]
    where [<HOST>] is given by [Runner.address] and [<PORT>] is the P2P port of
    [peer]. *)
val add_peer : t -> t -> unit

(** Returns the list of address of all [Peer <addr>] arguments. *)
val get_peers : t -> string list

(** Add a [--peer] argument to a node.

    Usage: [add_peer node peer]

    Same as [add_argument node (Peer "<HOST>:<PORT>#<ID>")]
    where [<HOST>] is given by [Runner.address], [<PORT>] is the P2P port and
    [<ID>] is the identity of [peer]. *)
val add_peer_with_id : t -> t -> unit Lwt.t

(** Removes the file peers.json that is at the root of data-dir.
    This file contains the list of peers known by the node. *)
val remove_peers_json_file : t -> unit

(** Get the P2P point and id for a node.

    Return ["<ADDRESS>:<PORT>#<ID>"] where [<PORT>] is the P2P
    port and [<ID>] is the identity of the node.

    [<ADDRESS>] is obtained using [Runner.address runner] with [?from]
    being the runner of [from] and [runner] is the runner of the node.
    In other words it is the address where [from] can contact [node]. *)
val point_and_id : ?from:t -> t -> string Lwt.t

(** Get the P2P point of a node.

    Return [(<ADDRESS>,<PORT>)] where [<PORT>] is the P2P
    port and [<ADDRESS>] is the reachable address of the node.

    [<ADDRESS>] is obtained using [Runner.address runner] with [?from]
    being the runner of [from] and [runner] is the runner of the node.
    In other words it is the address where [from] can contact [node]. *)
val point : ?from:t -> t -> string * int

(** Same as [point] but returns a string representation of the point
    ["<ADDRESS>:<PORT>"]. *)
val point_str : ?from:t -> t -> string

(** See [Daemon.Make.name] *)
val name : t -> string

(** Get the network port given as [--net-addr] to a node. *)
val net_port : t -> int

(** Get the network port given as [--advertised-net-port] to a node. *)
val advertised_net_port : t -> int option

val metrics_port : t -> int

(** Get the RPC scheme of a node.

    Returns [https] if node is started with [--rpc-tls], otherwise [http] *)
val rpc_scheme : t -> string

(** Returns [True] if RPCs are handled by a dedicated process. *)
val rpc_external : t -> bool

(** Get the RPC host given as [--rpc-addr] to a node. *)
val rpc_host : t -> string

(** Get the RPC port given as [--rpc-port] to a node. *)
val rpc_port : t -> int

(** Get the node's RPC endpoint URI.

    These are composed of the node's [--rpc-tls], [--rpc-addr] and
    [--rpc-port] arguments. If [local] is given ([false] by default),
    then [Constant.default_host] is used (it overrides [rpc-addr] or
    the [runner] argument). *)
val rpc_endpoint : ?local:bool -> t -> string

(** Get the data-dir of a node. *)
val data_dir : t -> string

(** Get the identity file of a node. *)
val identity_file : t -> string

(** Get the pid of the node, or none if it is not yet running. *)
val pid : t -> int option

(** Get the executable path of the node. *)
val path : t -> string

(** Get the runner associated to a node.

    Return [None] if the node runs on the local machine. *)
val runner : t -> Runner.t option

(** Wait until a node terminates and check its status.

    If the node is not running,
    or if the process returns an exit code which is not [exit_code],
    or if [msg] does not match the stderr output, fail the test.

    If [exit_code] is not specified, any non-zero code is accepted.
    If no [msg] is given, the stderr is ignored.*)
val check_error : ?exit_code:int -> ?msg:Base.rex -> t -> unit Lwt.t

(** Wait until a node terminates and return its status. If the node is not
   running, make the test fail. *)
val wait : t -> Unix.process_status Lwt.t

(** Send SIGTERM and wait for the process to terminate.

    Default [timeout] is 30 seconds, after which SIGKILL is sent. *)
val terminate : ?timeout:float -> t -> unit Lwt.t

(** Send SIGKILL and wait for the process to terminate. *)
val kill : t -> unit Lwt.t

(** {2 Commands} *)

(** Run [octez-node identity generate]. *)
val identity_generate : ?expected_pow:int -> t -> unit Lwt.t

(** Same as [identity_generate], but do not wait for the process to exit. *)
val spawn_identity_generate : ?expected_pow:int -> t -> Process.t

(** Convert an history mode into a string.

    The result is suitable to be passed to the node on the command-line. *)
val show_history_mode : history_mode -> string

(** Run [octez-node config init]. *)
val config_init : t -> argument list -> unit Lwt.t

(** Run [octez-node config update]. *)
val config_update : t -> argument list -> unit Lwt.t

(** Run [octez-node config reset]. *)
val config_reset : t -> argument list -> unit Lwt.t

(** Run [octez-node config show]. Returns the node configuration. *)
val config_show : t -> JSON.t Lwt.t

module Config_file : sig
  (** Node configuration files. *)

  (** Read the configuration file ([config.json]) of a node. *)
  val read : t -> JSON.t Lwt.t

  (** Write the configuration file of a node, replacing the existing one. *)
  val write : t -> JSON.t -> unit Lwt.t

  (** Basic network configuration for ghostnet
      (genesis, genesis_parameters, chain_name and sandboxed_chain_name *)
  val ghostnet_network_config : JSON.u

  (** Basic network configuration for mainnet
      (genesis, chain_name and sandboxed_chain_name *)
  val mainnet_network_config : JSON.u

  (** Update the configuration file of a node. If the node is already
     running, it needs to be restarted manually.

      Example: [Node.Config_file.update node (JSON.put ("p2p", new_p2p_config))] *)
  val update : t -> (JSON.t -> JSON.t) -> unit Lwt.t

  (** Set the network config to a sandbox with the given user
      activated upgrades. *)
  val set_sandbox_network_with_user_activated_upgrades :
    (int * Protocol.t) list -> JSON.t -> JSON.t

  (** Set the network config to a sandbox with the given user
      activated protocol overrides. *)
  val set_sandbox_network_with_user_activated_overrides :
    (string * string) list -> JSON.t -> JSON.t

  (** Updates the network config to add the given dal config. *)
  val set_network_with_dal_config :
    Tezos_crypto_dal.Cryptobox.Config.t -> JSON.t -> JSON.t

  (** Update the network config with the given user
      activated upgrades. *)
  val update_network_with_user_activated_upgrades :
    (int * Protocol.t) list -> JSON.t -> JSON.t

  (** Set the prevalidator configuration in the given configuration. *)
  val set_prevalidator :
    ?operations_request_timeout:float ->
    ?max_refused_operations:int ->
    ?operations_batch_size:int ->
    JSON.t ->
    JSON.t

  (** Set the peer_validator configuration in the given configuration. *)
  val set_peer_validator : ?new_head_request_timeout:float -> JSON.t -> JSON.t

  (** Set the network config to a sandbox chain. *)
  val set_sandbox_network : JSON.t -> JSON.t

  (** Set the network config to a Mainnet network.

      [user_activated_upgrades] can be given to add user-activated upgrades. *)
  val set_mainnet_network :
    ?user_activated_upgrades:(int * Protocol.t) list -> unit -> JSON.t -> JSON.t

  (** Set the network config to a Ghostnet network.

      [user_activated_upgrades] can be given to add user-activated upgrades. *)
  val set_ghostnet_network :
    ?user_activated_upgrades:(int * Protocol.t) list -> unit -> JSON.t -> JSON.t

  (** Set the network config to a Shadownet network.

      [user_activated_upgrades] can be given to add user-activated upgrades. *)
  val set_shadownet_network :
    ?user_activated_upgrades:(int * Protocol.t) list -> unit -> JSON.t -> JSON.t

  (** Set the network config to a sandbox with the same chain_id than Ghostnet.

      [user_activated_upgrades] can be given to add user-activated upgrades. *)
  val set_ghostnet_sandbox_network :
    ?user_activated_upgrades:(int * Protocol.t) list -> unit -> JSON.t -> JSON.t

  (** Set the network config to a Seoulnet network.

      [user_activated_upgrades] can be given to add user-activated upgrades. *)
  val set_seoulnet_network :
    ?user_activated_upgrades:(int * Protocol.t) list -> unit -> JSON.t -> JSON.t

  (** Set the network config to a Tallinnnet network.

      [user_activated_upgrades] can be given to add user-activated upgrades. *)
  val set_tallinnnet_network :
    ?user_activated_upgrades:(int * Protocol.t) list -> unit -> JSON.t -> JSON.t
end

(** Same as [config_init], but do not wait for the process to exit. *)
val spawn_config_init : t -> argument list -> Process.t

(** Same as [config_update], but do not wait for the process to exit. *)
val spawn_config_update : t -> argument list -> Process.t

(** Same as [config_reset], but do not wait for the process to exit. *)
val spawn_config_reset : t -> argument list -> Process.t

(** A snapshot history mode for exports *)
type snapshot_history_mode = Rolling_history | Full_history

(** A snapshot file format for exports *)
type export_format = Tar | Raw

(** Run [octez-node snapshot export]. *)
val snapshot_export :
  ?history_mode:snapshot_history_mode ->
  ?export_level:int ->
  ?export_format:export_format ->
  t ->
  string ->
  unit Lwt.t

(** Same as [snapshot_export], but do not wait for the process to exit. *)
val spawn_snapshot_export :
  ?history_mode:snapshot_history_mode ->
  ?export_level:int ->
  ?export_format:export_format ->
  t ->
  string ->
  Process.t

(** Run [octez-node snapshot info]. *)
val snapshot_info : ?json:bool -> t -> string -> string Lwt.t

(** Same as [snapshot_info], but do not wait for the process to
    exit. *)
val spawn_snapshot_info : ?json:bool -> t -> string -> Process.t

(** Run [octez-node snapshot import]. *)
val snapshot_import :
  ?env:string String_map.t ->
  ?force:bool ->
  ?no_check:bool ->
  ?reconstruct:bool ->
  t ->
  string ->
  unit Lwt.t

(** Same as [snapshot_import], but do not wait for the process to exit. *)
val spawn_snapshot_import :
  ?env:string String_map.t ->
  ?force:bool ->
  ?no_check:bool ->
  ?reconstruct:bool ->
  t ->
  string ->
  Process.t

(** Run [octez-node reconstruct]. *)
val reconstruct : t -> unit Lwt.t

(** Same as [reconstruct], but do not wait for the process to exit. *)
val spawn_reconstruct : t -> Process.t

(** Spawn [octez-node run].

    The resulting promise is fulfilled as soon as the node has been spawned.
    It continues running in the background.

    [event_level] specifies the verbosity of the file descriptor sink.
    This must be at least [`Notice], which is the level of event
    ["node_is_ready.v0"], needed for {!wait_for_ready}.
    The default value is [`Info] which is also the default event level
    of the node.

    [event_sections_levels] specifies the verbosity for events in sections whose
    prefix is in the list. For instance
    [~event_sections_levels:[("prevalidator", `Debug); ("validator.block", `Debug)]]
    will activate the logs at debug level for events whose section starts with
    ["prevalidator"] or ["validator.block"].
    See {!Tezos_stdlib_unix.File_descriptor_sink} and
    {{:https://tezos.gitlab.io/user/logging.html#file-descriptor-sinks}the logging documentation}
    for a more precise semantic.
 *)
val run :
  ?env:string String_map.t ->
  ?patch_config:(JSON.t -> JSON.t) ->
  ?on_terminate:(Unix.process_status -> unit) ->
  ?event_level:Daemon.Level.default_level ->
  ?event_sections_levels:(string * Daemon.Level.level) list ->
  t ->
  argument list ->
  unit Lwt.t

(** Spawn [octez-node replay].

    Same as {!run} but for the [replay] command.
    In particular it also supports events.
    One key difference is that the node will eventually stop.

    Note that the `--network` argument is infered by the `node replay`
    command itself, thanks to the configuration value.

    See {!run} for a description of the arguments. *)
val replay :
  ?on_terminate:(Unix.process_status -> unit) ->
  ?event_level:Daemon.Level.default_level ->
  ?event_sections_levels:(string * Daemon.Level.level) list ->
  ?strict:bool ->
  ?blocks:string list ->
  t ->
  unit Lwt.t

(** {2 Events} *)

exception
  Terminated_before_event of {
    daemon : string;
    event : string;
    where : string option;
  }

(** Wait for synchronisation status changes

More precisely, wait until a [synchronisation_status] event occurs with a status
change listed in [statuses]. *)
val wait_for_synchronisation : statuses:string list -> t -> unit Lwt.t

(** Wait until the node is ready.

    More precisely, wait until a [node_is_ready] event occurs.
    If such an event already occurred, return immediately. *)
val wait_for_ready : t -> unit Lwt.t

(** Wait for a given chain level.

    More precisely, wait until a [head_increment] or [branch_switch]
    (or [store_synchronized_on_head] when the node is running with
    [rpc_external] set to true) with a [level] greater or equal to the
    requested level occurs. If such an event already occurred, return
    immediately. *)
val wait_for_level : t -> int -> int Lwt.t

(** Get the current known level of the node.

    Returns [0] if the node is not running or if no [head_increment] or
    [branch_switch] event was received yet. This makes this function equivalent
    to [wait_for_level node 0] except that it does not actually wait for the
    level to be known.

    Note that, as the node's status is updated only on head increments, this
    value is wrong for instance right after a node restart or snapshot
    import. Therefore it is recommended to use the function {!get_level}
    instead, which does not have this problem. A use case for this function is
    to check for a level increase, when the exact level does not matter, and
    {!get_level}'s promise may not resolve. *)
val get_last_seen_level : t -> int

(** Return a promise that is fulfilled as soon as the node is running and its
    level is known, which is then the value of the promise.

    If the node is not running or if no [head_increment] or [branch_switch]
    event was received yet, then wait until one of these events occur. It is
    equivalent to [wait_for_level node 0], and thus avoids the pitfalls of
    getting a misleading 0 value. *)
val get_level : t -> int Lwt.t

(** Wait for the node to read its identity.

    More precisely, wait until a [read_identity] event occurs.
    If such an event already occurred, return immediately.

    Return the identity. *)
val wait_for_identity : t -> string Lwt.t

(** [wait_for_request ?level ~request node] waits for [request] event
   on the [node]. *)
val wait_for_request :
  request:[< `Flush | `Inject | `Notify | `Arrived] -> t -> unit Lwt.t

(** See [Daemon.Make.wait_for_full]. *)
val wait_for_full :
  ?where:string -> t -> string -> (JSON.t -> 'a option) -> 'a Lwt.t

(** See [Daemon.Make.wait_for]. *)
val wait_for : ?where:string -> t -> string -> (JSON.t -> 'a option) -> 'a Lwt.t

(** Wait for a node to receive a given number of connections.

    [wait_for_connections node n] waits until [node] receives [n]
    ["connection"] Chain validator events. *)
val wait_for_connections : t -> int -> unit Lwt.t

(** Wait for a node to receive a given number of disconnections.

    [wait_for_disconnections node n] waits until [node] receives
    [n] ["disconnection"] Chain validator events. *)
val wait_for_disconnections : t -> int -> unit Lwt.t

(** Waits for the node to switch branches.
    Resolves when the new branch matches [hash] and [level], if provided. *)
val wait_for_branch_switch :
  ?level:int -> ?hash:string -> t -> (int * string) Lwt.t

(** Raw events. *)
type event = {name : string; value : JSON.t; timestamp : float}

(** See [Daemon.Make.on_event]. *)
val on_event : t -> (event -> unit) -> unit

(** See [Daemon.Make.log_events]. *)
val log_events : ?max_length:int -> t -> unit

type observe_memory_consumption = Observe of (unit -> int option Lwt.t)

(** See [Daemon.Make.memory_consumption]. *)
val memory_consumption : t -> observe_memory_consumption Lwt.t

(** {2 High-Level Functions} *)

(** Initialize a node.

    This {!create}s a node, runs {!identity_generate}, {!config_init} and {!run},
    then waits for the node to be ready, and finally returns the node.

    Arguments are passed to {!config_init}. If you specified [Expected_pow],
    it is also passed to {!identity_generate}. Arguments are not passed to {!run}.
    If you do not wish the arguments to be stored in the configuration file
    (which will affect future runs too), do not use [init].

    When [patch_config] is provided, the function is used to patch the
    configuration file generated by {!config_init} before the call to
    {!run}. This argument should only be used to test configuration
    options that cannot be set with command-line {!argument}s.

    To import a snapshot before calling {!run}, specify
    [~snapshot:(file, do_reconstruct)], where file is the path to the snapshot.
    If [reconstruct] is [true], then [--reconstruct] is passed to the import
    command.

    The [env] argument, if specified, allows adding new environment
    variables when executing the [run] command of the octez-node. If
    any environment variable is already defined internally they
    will overwrite the ones provided via [env]. *)
val init :
  ?runner:Runner.t ->
  ?path:string ->
  ?name:string ->
  ?env:string String_map.t ->
  ?color:Log.Color.t ->
  ?data_dir:string ->
  ?event_pipe:string ->
  ?net_addr:string ->
  ?net_port:int ->
  ?advertised_net_port:int ->
  ?metrics_addr:string ->
  ?metrics_port:int ->
  ?rpc_external:bool ->
  ?rpc_host:string ->
  ?rpc_port:int ->
  ?rpc_tls:tls_config ->
  ?event_level:Daemon.Level.default_level ->
  ?event_sections_levels:(string * Daemon.Level.level) list ->
  ?patch_config:(JSON.t -> JSON.t) ->
  ?snapshot:string * bool ->
  argument list ->
  t Lwt.t

(** [send_raw_data node ~data] writes [~data] using an IP socket on the net
    port of [node]. *)
val send_raw_data : t -> data:string -> unit Lwt.t

(** [upgrade_storage node] upgrades the given [node] storage. *)
val upgrade_storage : t -> unit Lwt.t

(** Run [octez-node --version] and return the node's version. *)
val get_version : t -> string Lwt.t

(** Expose the RPC server address of this node as a foreign endpoint.
    See [rpc_endpoint] for a description of the [local] argument. *)
val as_rpc_endpoint : ?local:bool -> t -> Endpoint.t

module RPC : sig
  include RPC_core.CALLERS with type uri_provider := t

  include module type of RPC
end
