(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2024-2025 Functori <contact@functori.com>                   *)
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

(** EVM node server state. *)
type t

(** Same as [Daemon.event] *)
type event = {name : string; value : JSON.t; timestamp : float}

type tez_contract = {address : string; path : string; initial_storage : string}

type l2_setup = {
  l2_chain_id : int;
  l2_chain_family : string;
  world_state_path : string option;
  eth_bootstrap_accounts : string list option;
  tez_bootstrap_accounts : Account.key list option;
  tez_bootstrap_contracts : tez_contract list option;
  sequencer_pool_address : string option;
  minimum_base_fee_per_gas : Wei.t option;
  da_fee_per_byte : Wei.t option;
  maximum_gas_per_transaction : int64 option;
}

val eth_default_bootstrap_accounts : string list

val tez_default_bootstrap_accounts : Account.key list

val default_l2_setup : l2_chain_id:int -> l2_setup

type time_between_blocks =
  | Nothing  (** Does not produce any block if not forced by the private RPC *)
  | Time_between_blocks of float
      (** Interval at which the sequencer creates an empty block by
          default. *)

(** Configuration shared by Sequencer, Sandbox, and Tezlink_sandbox modes. *)
type sequencer_config = {
  time_between_blocks : time_between_blocks option;
      (** See {!time_between_blocks}, if the value is not
          provided, the sequencer uses it default value. *)
  genesis_timestamp : Client.timestamp option;  (** Genesis timestamp *)
  max_number_of_chunks : int option;
  wallet_dir : string option;  (** --wallet-dir: client directory. *)
}

(** EVM node mode.

    Common configuration fields (initial_kernel, preimages_dir, private_rpc_port,
    tx_queue settings) are now passed directly to {!create} and stored in
    persistent state, not in mode variants. *)
type mode =
  | Observer of {
      rollup_node_endpoint : string option;
          (** When None adds `--dont-track-rollup-node` *)
      evm_node_endpoint : string;
    }
  | Sequencer of {
      rollup_node_endpoint : string;
      sequencer_config : sequencer_config;
      sequencer_keys : string list;
          (** Secret keys used to sign the blueprints. *)
      max_blueprints_lag : int option;
      max_blueprints_ahead : int option;
      max_blueprints_catchup : int option;
      catchup_cooldown : int option;
      dal_slots : int list option;
      sequencer_sunset_sec : int option;
    }
  | Sandbox of {
      sequencer_config : sequencer_config;
      network : string option;
      funded_addresses : string list;
      sequencer_keys : string list;
    }
  | Tezlink_sandbox of {
      sequencer_config : sequencer_config;
      funded_addresses : string list;
      verbose : bool;
    }
  | Proxy of string
  | Rpc of mode

type history_mode =
  | Archive
  | Rolling of int  (** Rolling with retention period in days. *)
  | Full of int  (** Full with retention period in days. *)

(** Node configuration shared across modes.
    Groups mode-agnostic parameters to simplify function signatures. *)
type node_setup = {
  path : string;
  name : string;
  runner : Runner.t option;
  history_mode : history_mode option;
  data_dir : string option;
  config_file : string option;
  rpc_addr : string option;
  rpc_port : int;
  restricted_rpcs : string option;
  spawn_rpc : int option;
  websockets : bool;
  initial_kernel : string option;
  preimages_dir : string option;
  private_rpc_port : int option;
  tx_queue_max_lifespan : int option;
  tx_queue_max_size : int option;
  tx_queue_tx_per_addr_limit : int option;
}

(** [make_setup ()] creates a [node_setup] with meaningful defaults:
    path is set to the octez-evm-node binary, name is freshly generated,
    data_dir is set to [Temp.dir name], rpc_port is allocated, and websockets
    is set to false. All parameters can be overridden via optional arguments. *)
val make_setup :
  ?path:string ->
  ?name:string ->
  ?runner:Runner.t ->
  ?history_mode:history_mode ->
  ?data_dir:string ->
  ?config_file:string ->
  ?rpc_addr:string ->
  ?rpc_port:int ->
  ?restricted_rpcs:string ->
  ?spawn_rpc:int ->
  ?websockets:bool ->
  ?initial_kernel:string ->
  ?preimages_dir:string ->
  ?private_rpc_port:int ->
  ?tx_queue_max_lifespan:int ->
  ?tx_queue_max_size:int ->
  ?tx_queue_tx_per_addr_limit:int ->
  unit ->
  node_setup

(** Returns the mode of the EVM node. *)
val mode : t -> mode

val can_apply_blueprint : t -> bool

(** Returns the name of the EVM node. *)
val name : t -> string

(** Returns the data_dir of the EVM node. *)
val data_dir : t -> string

(** Optionally returns the config file used by the EVM node, if not the one by
    default inside the data directory. *)
val config_file : t -> string option

(** Returns the path to the directory storing the preimages used by the
    kernel run by the node. *)
val preimages_dir : t -> string

(** [create ~mode ?node_setup ()] creates an EVM node server.

    The server listens to requests at address [rpc_addr] and the port
    [rpc_port]. [rpc_addr] defaults to [Constant.default_host] and a fresh port
    is chosen if [rpc_port] is not set.

    If [websockets] is true, activates the websocket server.

    Common configuration is provided via [node_setup]. Use {!make_setup}
    with optional arguments to construct it. *)
val create : ?node_setup:node_setup -> mode:mode -> unit -> t

(** [initial_kernel node] returns the path to the kernel used to initialize the
    EVM state. Fails if [node] is a proxy node. *)
val initial_kernel : t -> string option

(** [run ?wait ?extra_arguments evm_node] launches the EVM node server with
    the arguments given during {!create}, additional arguments can be
    passed via [extra_arguments].
    [wait] defaults to true, if it is set to false, the evm node is ran but we
    do not wait for it to be ready. *)
val run :
  ?wait:bool ->
  ?end_test_on_failure:bool ->
  ?extra_arguments:string list ->
  t ->
  unit Lwt.t

(** [wait_for_event ?timeout evm_node ~event f] waits for event [event] until
    [timeout] on node [evm_node].

    The wait is resolved if an event with name [event] is seen and [f] returns
    [Some _].
*)
val wait_for_event :
  ?timeout:float -> t -> event:string -> (JSON.t -> 'a option) -> 'a Lwt.t

(** [wait_for_ready evm_node] waits until [evm_node] is ready. *)
val wait_for_ready : ?timeout:float -> t -> unit Lwt.t

(** [wait_for_blueprint_applied ~timeout evm_node level] waits until
    [evm_node] has applied a blueprint locally for level [level]. *)
val wait_for_blueprint_applied : ?timeout:float -> t -> int -> unit Lwt.t

(** [wait_for_blueprint_invalid_applied] waits for the event
    [blueprint_invalid_applied.v0]. *)
val wait_for_blueprint_invalid_applied : t -> unit Lwt.t

val wait_for_blueprint_finalized : ?timeout:float -> t -> int -> unit Lwt.t

(** [wait_for_predownload_kernel ?timeout evm_node ~root_hash] waits until
    [evm_node] has download a kernel with [root_hash]. *)
val wait_for_predownload_kernel :
  ?timeout:float -> t -> root_hash:string -> unit Lwt.t

val wait_for_predownload_kernel_failed :
  ?timeout:float -> t -> root_hash:string -> unit Lwt.t

(** [wait_for_blueprint_invalid ~timeout evm_node] waits until
    [evm_node] has seen an invalid blueprint. *)
val wait_for_blueprint_invalid : ?timeout:float -> t -> unit Lwt.t

(** [wait_for_blueprint_injected ~timeout evm_node level] waits until
    [evm_node] has injected a blueprint for level [level] to its rollup node. *)
val wait_for_blueprint_injected : ?timeout:float -> t -> int -> unit Lwt.t

(** [wait_for_blueprint_injected_on_dal ~timeout evm_node level] waits until
    [evm_node] has injected a blueprint to the DAL node and returns the L1 level
    of injection and the number of chunks injected. Can be used with
    [wait_for_blueprint_injected] to get the expected level. *)
val wait_for_blueprint_injected_on_dal :
  ?timeout:float -> t -> (int * int) Lwt.t

(** [wait_for_signal_signed ~timeout evm_node] waits until (at least)
    a signal has been signed and returns the smart rollup address for
    which the signal has been signed together with the slot indices
    and the published levels that have been signaled. *)
val wait_for_signal_signed :
  ?timeout:float -> t -> (string * (int * int) list) Lwt.t

val wait_for_pending_upgrade : ?timeout:float -> t -> (string * string) Lwt.t

val wait_for_successful_upgrade : ?timeout:float -> t -> (string * int) Lwt.t

val wait_for_pending_sequencer_upgrade :
  ?timeout:float -> t -> (string * string * string) Lwt.t

val wait_for_spawn_rpc_ready : ?timeout:float -> t -> unit Lwt.t

val wait_for_drift_monitor_ready : ?timeout:float -> t -> unit Lwt.t

val wait_for_import_finished : ?timeout:float -> t -> unit Lwt.t

val wait_for_finished_exporting_snapshot : ?timeout:float -> t -> string Lwt.t

val wait_for_block_producer_locked : ?timeout:float -> t -> unit Lwt.t

val wait_for_block_producer_tx_injected : ?timeout:float -> t -> string Lwt.t

val wait_for_retrying_connect : ?timeout:float -> t -> unit Lwt.t

val wait_for_flush_delayed_inbox :
  ?timeout:float -> ?level:int -> t -> int Lwt.t

val wait_for_rollup_node_follower_connection_acquired :
  ?timeout:float -> t -> unit Lwt.t

val wait_for_rollup_node_follower_disabled : ?timeout:float -> t -> unit Lwt.t

type processed_l1_level = {l1_level : int; finalized_blueprint : int}

(** [wait_for_processed_l1_level ?timeout ?level evm_node] waits until
    a (finalized) layer1 level has been processed and returns the
    layer1 level and it's latest corresponding blueprints. If [level =
    Some level] then it waits until that specific layer1 level is
    processed. It returns the tuple [(layer1_level,
    finalized_blueprint]). *)
val wait_for_processed_l1_level :
  ?timeout:float -> ?level:int -> t -> processed_l1_level Lwt.t

val wait_for_blueprint_catchup : ?timeout:float -> t -> (int * int) Lwt.t

val wait_for_blueprint_injection_failure :
  ?timeout:float -> ?level:int -> t -> unit Lwt.t

val wait_for_next_block_info : ?timeout:float -> t -> string Lwt.t

val wait_for_inclusion : ?timeout:float -> ?hash:string -> t -> string Lwt.t

(** [wait_for_single_tx_execution_done evm_node] waits for the
    [single_tx_execution_done.v0] event to be emitted. Returns the
    transaction hash. *)
val wait_for_single_tx_execution_done : ?timeout:float -> t -> string Lwt.t

module Config_file : sig
  (** Node configuration files. *)

  (** Read the configuration file ([config.json]) of a node. *)
  val read : t -> JSON.t Lwt.t

  (** Write the configuration file of a node, replacing the existing one. *)
  val write : t -> JSON.t -> unit Lwt.t

  (** Update the configuration file of a node. If the node is already
     running, it needs to be restarted manually.

      Example: [Evm_node.Config_file.update evm_node (JSON.put
      ("experimental_features", experimental_feature))] *)
  val update : t -> (JSON.t -> JSON.t) -> unit Lwt.t
end

(** [spawn_init_config ?extra_arguments evm_node] runs "init config"
    with arguments found in the state. *)
val spawn_init_config : ?extra_arguments:string list -> t -> Process.t

(** [spawn_init_config_minimal ?data_dir ?path ?extra_arguments ()] creates a
    minimal config with no cli argument populated as [spawn_init_config].

    Unlike [spawn_init_config], does not require a [Evm_node.t] instance. *)
val spawn_init_config_minimal :
  ?data_dir:string ->
  ?config_file:string ->
  ?path:string ->
  ?extra_arguments:string list ->
  unit ->
  Process.t

type rpc_server = Resto | Dream

(** [patch_config_with_experimental_feature
    ?drop_duplicate_when_injection ?next_wasm_runtime ?rpc_server
    json_config]
    patches a config to add experimental feature. Each optional
    argument adds the corresponding experimental feature. *)
val patch_config_with_experimental_feature :
  ?drop_duplicate_when_injection:bool ->
  ?blueprints_publisher_order_enabled:bool ->
  ?next_wasm_runtime:bool ->
  ?rpc_server:rpc_server ->
  ?spawn_rpc:int ->
  ?periodic_snapshot_path:string ->
  ?l2_chains:l2_setup list ->
  ?preconfirmation_stream_enabled:bool ->
  unit ->
  JSON.t ->
  JSON.t

(** Edit websockets server configuration if websockets server is enabled. *)
val patch_config_websockets_if_enabled :
  ?max_message_length:int ->
  ?monitor_heartbeat:bool ->
  ?rate_limit:Ezjsonm.value ->
  JSON.t ->
  JSON.t

(** Edit garbage collector parameters in the configuration file. *)
val patch_config_gc : ?history_mode:history_mode -> JSON.t -> JSON.t

(** [init ?patch_config ~mode ?node_setup ?end_test_on_failure
    ?extra_arguments ()] creates an EVM node server with {!create},
    init the config with {!spawn_init_config}, patch it with [patch_config],
    then runs it with {!run}. *)
val init :
  ?patch_config:(JSON.t -> JSON.t) ->
  ?node_setup:node_setup ->
  mode:mode ->
  ?end_test_on_failure:bool ->
  ?extra_arguments:string list ->
  unit ->
  t Lwt.t

(** Get the RPC port given as [--rpc-port] to a node. *)
val rpc_port : t -> int

(** Get the private RPC port, if set. *)
val private_rpc_port : t -> int option

(** Get the spawn_rpc value given on creation. *)
val spawn_rpc : t -> int option

(** [spawn_run ?extra_arguments evm_node] same as {!run} but spawns a
    process. *)
val spawn_run : ?extra_arguments:string list -> t -> Process.t

(** Send SIGTERM and wait for the process to terminate.

    Default [timeout] is 30 seconds, after which SIGKILL is sent. *)
val terminate : ?timeout:float -> t -> unit Lwt.t

val resolve_or_timeout :
  ?timeout:float -> t -> name:string -> 'a Lwt.t -> 'a Lwt.t

(** The same exact behavior as {!Sc_rollup_node.wait_for} but for the EVM node. *)
val wait_for :
  ?timeout:float ->
  ?where:string ->
  t ->
  string ->
  (JSON.t -> 'a option) ->
  'a Lwt.t

(** Install a events handler. *)
val on_event : t -> (event -> unit) -> unit

type delayed_transaction_kind = Deposit | Transaction | FaDeposit | Operation

type 'a evm_event_kind =
  | Kernel_upgrade : (string * Client.Time.t) evm_event_kind
  | Sequencer_upgrade : (string * Hex.t * Client.Time.t) evm_event_kind
  | Blueprint_applied : (int * string) evm_event_kind
  | New_delayed_transaction : (delayed_transaction_kind * string) evm_event_kind

(** [wait_for_evm_event evm_node ~event_kind] wait for the event
    [evm_events_new_event.v0] using {!wait_for} where the event kind
    is equal to [event_kind] (e.g. "sequencer_upgrade"). *)
val wait_for_evm_event :
  ?timeout:float ->
  'a evm_event_kind ->
  ?check:(JSON.t -> 'a option) ->
  t ->
  'a Lwt.t

(** [wait_for_diverged evm_node] waits for the event
    [evm_events_follower_diverged.v0] using {!wait_for} and return the
    diverging blueprint level expected hash, and found hash. *)
val wait_for_diverged : t -> (int * string * string) Lwt.t

(** [wait_for_assemble_block_diverged ?timeout evm_node] waits for the event
    [assemble_block_diverged.v0] using {!wait_for} and returns the diverging
    block level. This event is emitted when the observer detects divergence
    during instant confirmation processing. *)
val wait_for_assemble_block_diverged : ?timeout:float -> t -> int Lwt.t

(** [wait_for_reset evm_node] waits for the event [evm_context_reset_at_level]
    using {!wait_for}. It does not wait for a specific level. *)
val wait_for_reset : t -> unit Lwt.t

(** [wait_for_missing_blueprint evm_node] waits for the
    event [evm_events_follower_missing_blueprint.v0] using
    {!wait_for} and return the missing blueprint level and
    block hash. *)
val wait_for_missing_blueprint : t -> (int * string) Lwt.t

(** [wait_for_rollup_node_ahead evm_node] waits for the event
    [evm_events_follower_rollup_node_ahead.v0] using {!wait_for} and returns the
    missing blueprint level. *)
val wait_for_rollup_node_ahead : t -> int Lwt.t

(** [wait_for_tx_queue_add_transaction ?timeout ?hash evm_node] waits for the event
    [tx_queue_add_transaction.v0] using {!wait_for} and returns the transaction
    hash. *)
val wait_for_tx_queue_add_transaction :
  ?timeout:float -> ?hash:string -> t -> string Lwt.t

(** [wait_for_tx_queue_transaction_confirmed ?timeout ?hash evm_node]
    waits for the event [tx_queue_transaction_confirmed.v0] using
    {!wait_for} and returns the transaction hash. If [hash] is
    provided, wait for that hash to be confirmed. *)
val wait_for_tx_queue_transaction_confirmed :
  ?timeout:float -> ?hash:string -> t -> string Lwt.t

(** [wait_for_tx_queue_transaction_dropped ?timeout ?hash evm_node]
    waits for the event [tx_queue_transaction_dropped.v0] using
    {!wait_for} and returns the transaction hash. If [hash] is
    provided, wait for that hash to be confirmed. *)
val wait_for_tx_queue_transaction_dropped :
  ?timeout:float -> ?hash:string -> t -> string Lwt.t

(** [wait_for_tx_queue_injecting_transaction ?timeout evm_node] waits
    for the event [tx_queue_injecting_transaction.v0] using
    {!wait_for} and returns the number of transactions injected. *)
val wait_for_tx_queue_injecting_transaction : ?timeout:float -> t -> int Lwt.t

(** [wait_for_tx_queue_cleared ?timeout evm_node] waits for the [tx_queue_cleared.v0]. *)
val wait_for_tx_queue_cleared : ?timeout:float -> t -> unit Lwt.t

(** [wait_for_block_producer_rejected_transaction ?timeout ?hash
    evm_node] waits for the [block_producer_rejected_transaction.v0]
    and returns the reason for the tx to be rejected. *)
val wait_for_block_producer_rejected_transaction :
  ?timeout:float -> ?hash:string -> t -> string Lwt.t

(** [wait_for_shutdown ?can_terminate evm_node] waits until a node terminates
    and return its status. If the node is not running, make the test fail. If
    [can_terminate] is `true` and the node was already terminated, returns
    `None`. *)
val wait_for_shutdown_event : ?can_terminate:bool -> t -> int option Lwt.t

(** [wait_for_split ?level evm_node] waits until the node terminates
    splitting its irmin context at level [level] if provided. *)
val wait_for_split : ?level:int -> t -> int Lwt.t

(** [wait_for_gc_finished ?gc_level ?head_level evm_node] waits until
    the node terminates garbage collecting its context on head
    [head_level] at gc level [gc_level] if provided. *)
val wait_for_gc_finished :
  ?gc_level:int -> ?head_level:int -> t -> (int * int) Lwt.t

(** [wait_for_start_history_mode ?history_mode evm_node] waits until
    the start history mode event is emitted with [history_mode] if provided. *)
val wait_for_start_history_mode : ?history_mode:string -> t -> string Lwt.t

(** [rpc_endpoint ?local ?private_ evm_node] returns the endpoint to communicate with the
    [evm_node]. If [private_] is true, the endpoint for the private
    RPC server is returned.

    If [local] is given ([false] by default),
    then [Constant.default_host] is used (it overrides [rpc-addr] or
    the [runner] argument).
*)
val rpc_endpoint : ?local:bool -> ?private_:bool -> t -> string

(** [rpc_endpoint ?local ?private_ evm_node] returns the endpoint to communicate with the
    [evm_node] in the {!Endpoint.t} format.

    If [local] is given ([false] by default),
    then [Constant.default_host] is used (it overrides [rpc-addr] or
    the [runner] argument).
*)
val rpc_endpoint_record : ?local:bool -> t -> Endpoint.t

(** A deprecated alias for [rpc_endpoint] where [local] optional parameter is not given. *)
val endpoint : ?private_:bool -> t -> string

(** JSON-RPC request. *)
type request = {method_ : string; parameters : JSON.u}

(** [call_evm_rpc ?private_ evm_node request] sends a JSON-RPC request to
    the [evm_node], for the given [request].
    If [private_] is true, the request is sent to the private RPC
    server. *)
val call_evm_rpc : ?private_:bool -> t -> request -> JSON.t Lwt.t

(** [batch_evm_rpc ?private_ evm_node requests] sends multiple JSON-RPC requests
    to the [evm_node], for the given [requests].
    If [private_] is true, the requests are sent to the private RPC
    server. *)
val batch_evm_rpc : ?private_:bool -> t -> request list -> JSON.t list Lwt.t

(** Open a websocket connection with the EVM node. If [private_] is true, a
    connection is created with the private websocket endpoint of the node. *)
val open_websocket : ?private_:bool -> t -> Websocket.t Lwt.t

(** [call_evm_websocket ws request] sends a JSON-RPC request on the websocket
    connection [ws] and waits for the response. *)
val call_evm_websocket : Websocket.t -> request -> JSON.t Lwt.t

(** [batch_evm_websocket ws requests] sends multiple JSON-RPC requests on the
    websocket connection [ws] without waiting for the responses, then receive
    all responses at the end. *)
val batch_evm_websocket : Websocket.t -> request list -> JSON.t list Lwt.t

(** [jsonrpc] uses the [websocket] to make a JSON-RPC call if provided or falls
    back to using HTTP RPC request on the EVM node otherwise. *)
val jsonrpc :
  ?websocket:Websocket.t -> ?private_:bool -> t -> request -> JSON.t Lwt.t

(** [batch_jsonrpc] uses the [websocket] to make a JSON-RPC calls if provided or
    falls back to using an HTTP RPC batch request on the EVM node otherwise. *)
val batch_jsonrpc :
  ?websocket:Websocket.t ->
  ?private_:bool ->
  t ->
  request list ->
  JSON.t list Lwt.t

(** [extract_result json] expects a JSON-RPC `result` and returns the value. *)
val extract_result : JSON.t -> JSON.t

(** [extract_error_message json] expects a JSON-RPC `error.message` and returns the value. *)
val extract_error_message : JSON.t -> JSON.t

(** [fetch_contract_code evm_node contract] returns the code associated to
    the given contract in the rollup. *)
val fetch_contract_code : t -> string -> string Lwt.t

(** [upgrade_payload ~root_hash ~activation_timestamp] gives the
    upgrade payload to put in a upgrade message, it will upgrade to
    [root_hash] at the first block after [activation_timestamp] (in
    RFC3399 format). *)
val upgrade_payload :
  root_hash:string -> activation_timestamp:string -> string Lwt.t

(** [sequencer_upgrade_payload ?client ~public_key  ~pool_address
    ~activation_timestamp ()] gives the sequencer upgrade payload to
    put in a upgrade message, it will upgrade the sequencer to
    [public_key] at the first l1 block after [activation_timestamp]
    (in RFC3399 format). *)
val sequencer_upgrade_payload :
  ?client:Client.t ->
  public_key:string ->
  pool_address:string ->
  activation_timestamp:string ->
  unit ->
  string Lwt.t

(** [init_from_rollup_node_data_dir ?omit_delayed_tx_events evm_node
    rollup_node] initialises the data dir of the evm node by importing
    the evm state from a rollup node data dir. *)
val init_from_rollup_node_data_dir :
  ?omit_delayed_tx_events:bool -> t -> Sc_rollup_node.t -> unit Lwt.t

(** [transform_dump ~dump_json ~dump_rlp] transforms a JSON list of
    instructions stored in [dump_json] to an RLP list, which is
    stored in [dump_rlp].  *)
val transform_dump : dump_json:string -> dump_rlp:string -> unit Lwt.t

(** [reset evm_node ~l2_level] reset the store of the [evm_node] to
    l2_level. *)
val reset : t -> l2_level:int -> unit Lwt.t

(** [chunk data ~rollup_address ?sequencer_key ?timestamp ?parent_hash
    ?number ?client data] generates the valid inputs for the rollup at
    [rollup_address] from the given [data]. If [sequencer_key] is given, the
    data produced is for the sequencer mode. *)
val chunk_data :
  rollup_address:string ->
  ?sequencer_key:string ->
  ?timestamp:string ->
  ?parent_hash:string ->
  ?number:int ->
  ?client:Client.t ->
  string list ->
  string list Lwt.t

(** [patch_kernel evm_node path] modifies the kernel used by [evm_node] with
    the kernel stored in the file [path]. This will fail if the node is
    running. *)
val patch_kernel : t -> string -> unit Lwt.t

(** [patch_kernel evm_node ~key ~value] modifies the state of the [evm_node]
    by writing [value] at [key]. *)
val patch_state : t -> key:string -> value:string -> unit Lwt.t

(** [execute_single_transaction evm_node ~raw_tx] calls the private
    executeSingleTransaction RPC to execute a single transaction on the
    observer's EVM context. This is used for testing divergence scenarios. *)
val execute_single_transaction : t -> raw_tx:string -> unit Lwt.t

(** [export_snapshot ~desync evm_node] exports a snapshot of the evm node in a
    temporary directory. It returns the path for the produced snapshot file. If
    [desync=true] it uses the desync format. *)
val export_snapshot :
  ?compress_on_the_fly:bool -> desync:bool -> t -> (Process.t, string) runnable

(** [import_snapshot ?force ~desync evm_node ~snapshot_file] imports the
    snapshot [snapshot_file] in the evm node. If [desync=true] it uses the
    desync format.  *)
val import_snapshot :
  ?force:bool ->
  desync:bool ->
  t ->
  snapshot_file:string ->
  (Process.t, unit) runnable

(** Run [snapshot info] command and return output containing information about
    the snapshot file. *)
val snapshot_info :
  desync:bool -> snapshot_file:string -> (Process.t, string) runnable

val wait_termination : t -> unit Lwt.t

(** [make_l2_kernel_installer_config ~output ()] creates the config needed for
    an l2 chain in a multichain kernel *)
val make_l2_kernel_installer_config :
  ?chain_id:int ->
  ?chain_family:string ->
  ?eth_bootstrap_balance:Wei.t ->
  ?tez_bootstrap_balance:Tez.t ->
  ?eth_bootstrap_accounts:string list ->
  ?tez_bootstrap_accounts:Account.key list ->
  ?tez_bootstrap_contracts:string list ->
  ?minimum_base_fee_per_gas:Wei.t ->
  ?da_fee_per_byte:Wei.t ->
  ?sequencer_pool_address:string ->
  ?maximum_gas_per_transaction:int64 ->
  ?set_account_code:(string * string) list ->
  ?world_state_path:string ->
  output:string ->
  unit ->
  (Process.t, unit) runnable

(** [make_kernel_installer_config ~output ()] create the config needed for the
    evm kernel used by the installer *)
val make_kernel_installer_config :
  ?l2_chain_ids:int list ->
  ?max_delayed_inbox_blueprint_length:int ->
  ?mainnet_compat:bool ->
  ?remove_whitelist:bool ->
  ?kernel_root_hash:string ->
  ?chain_id:int ->
  ?eth_bootstrap_balance:Wei.t ->
  ?eth_bootstrap_accounts:string list ->
  ?tez_bootstrap_balance:Tez.t ->
  ?tez_bootstrap_accounts:Account.key list ->
  ?sequencer:string ->
  ?delayed_bridge:string ->
  ?ticketer:string ->
  ?administrator:string ->
  ?sequencer_governance:string ->
  ?kernel_governance:string ->
  ?kernel_security_governance:string ->
  ?minimum_base_fee_per_gas:Wei.t ->
  ?da_fee_per_byte:Wei.t ->
  ?delayed_inbox_timeout:int ->
  ?delayed_inbox_min_levels:int ->
  ?sequencer_pool_address:string ->
  ?maximum_allowed_ticks:int64 ->
  ?maximum_gas_per_transaction:int64 ->
  ?max_blueprint_lookahead_in_seconds:int64 ->
  ?set_account_code:(string * string) list ->
  ?enable_fa_bridge:bool ->
  ?enable_revm:bool ->
  ?enable_dal:bool ->
  ?dal_slots:int list ->
  ?enable_fast_withdrawal:bool ->
  ?enable_fast_fa_withdrawal:bool ->
  ?enable_multichain:bool ->
  ?evm_version:Evm_version.t ->
  ?with_runtimes:Tezosx_runtime.t list ->
  output:string ->
  unit ->
  (Process.t, unit) Runnable.t

val debug_print_store_schemas :
  ?path:string -> ?hooks:Process_hooks.t -> unit -> unit Lwt.t

val man : ?path:string -> ?hooks:Process_hooks.t -> unit -> unit Lwt.t

val describe_config :
  ?path:string -> ?hooks:Process_hooks.t -> unit -> unit Lwt.t

(** A description of the metrics exported by the node. *)
val list_metrics : ?hooks:Process_hooks.t -> unit -> unit Lwt.t

(** A description of the events exported by the node. *)
val list_events :
  ?hooks:Process_hooks.t -> ?level:string -> ?json:bool -> unit -> unit Lwt.t

(** Switch history mode of an EVM node with command switch history. *)
val switch_history_mode : t -> history_mode -> (Process.t, unit) runnable

val switch_sequencer_to_observer : old_sequencer:t -> new_sequencer:t -> t

val daemon_default_colors : Log.Color.t array

val pid : t -> int option
