(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
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

type time_between_blocks =
  | Nothing  (** Does not produce any block if not forced by the private RPC *)
  | Time_between_blocks of float
      (** Interval at which the sequencer creates an empty block by
          default. *)

(** EVM node mode. *)
type mode =
  | Observer of {
      initial_kernel : string;
      preimages_dir : string;
      rollup_node_endpoint : string;
      time_between_blocks : time_between_blocks option;
    }
  | Threshold_encryption_observer of {
      initial_kernel : string;
      preimages_dir : string;
      rollup_node_endpoint : string;
      bundler_node_endpoint : string;
      time_between_blocks : time_between_blocks option;
    }
  | Sequencer of {
      initial_kernel : string;
          (** Path to the initial kernel used by the sequencer. *)
      preimage_dir : string option;
          (** Path to the directory with the associated preimages. *)
      private_rpc_port : int option;  (** Port for private RPC server*)
      time_between_blocks : time_between_blocks option;
          (** See {!time_between_blocks}, if the value is not
              provided, the sequencer uses it default value. *)
      sequencer : string;  (** Secret key used to sign the blueprints. *)
      genesis_timestamp : Client.timestamp option;  (** Genesis timestamp *)
      max_blueprints_lag : int option;
      max_blueprints_ahead : int option;
      max_blueprints_catchup : int option;
      catchup_cooldown : int option;
      max_number_of_chunks : int option;
      wallet_dir : string option;  (** --wallet-dir: client directory. *)
      tx_pool_timeout_limit : int option;
          (** --tx-pool-timeout-limit: transaction timeout inside the pool. *)
      tx_pool_addr_limit : int option;
          (** --tx-pool-addr-limit: maximum address allowed simultaneously inside
              the pool. *)
      tx_pool_tx_per_addr_limit : int option;
          (** --tx-pool-tx-per-addr-limit: maximum transaction per address allowed
              simultaneously inside the pool. *)
    }
  | Threshold_encryption_sequencer of {
      initial_kernel : string;
          (** Path to the initial kernel used by the sequencer. *)
      preimage_dir : string option;
          (** Path to the directory with the associated preimages. *)
      private_rpc_port : int option;  (** Port for private RPC server*)
      time_between_blocks : time_between_blocks option;
          (** See {!time_between_blocks}, if the value is not
              provided, the sequencer uses it default value. *)
      sequencer : string;  (** Secret key used to sign the blueprints. *)
      genesis_timestamp : Client.timestamp option;  (** Genesis timestamp *)
      max_blueprints_lag : int option;
      max_blueprints_ahead : int option;
      max_blueprints_catchup : int option;
      catchup_cooldown : int option;
      max_number_of_chunks : int option;
      wallet_dir : string option;  (** --wallet-dir: client directory. *)
      tx_pool_timeout_limit : int option;
          (** --tx-pool-timeout-limit: transaction timeout inside the pool. *)
      tx_pool_addr_limit : int option;
          (** --tx-pool-addr-limit: maximum address allowed simultaneously inside
              the pool. *)
      tx_pool_tx_per_addr_limit : int option;
          (** --tx-pool-tx-per-addr-limit: maximum transaction per address allowed
              simultaneously inside the pool. *)
      sequencer_sidecar_endpoint : string;
          (** --sequencer-sidecar-endpoint: Uri of the sidecar endpoints to which
              proposals are forwarded, and from where preblocks are fetched. *)
    }
  | Proxy of {
      finalized_view : bool;
          (** Expose the latest final block of the rollup instead of its current head *)
    }

(** Returns the mode of the EVM node. *)
val mode : t -> mode

(** Returns the name of the EVM node. *)
val name : t -> string

(** Returns the data_dir of the EVM node. *)
val data_dir : t -> string

(** [create ?name ?runner ?mode ?data_dir ?rpc_addr ?rpc_port
    rollup_node_endpoint] creates an EVM node server.

    The server listens to requests at address [rpc_addr] and the port
    [rpc_port]. [rpc_addr] defaults to [Constant.default_host] and a fresh port is
    chosen if [rpc_port] is not set.

    The server communicates with a rollup-node and sets its endpoint via
    [rollup_node_endpoint].

    [mode] defaults to [Proxy].
*)
val create :
  ?path:string ->
  ?name:string ->
  ?runner:Runner.t ->
  ?mode:mode ->
  ?data_dir:string ->
  ?rpc_addr:string ->
  ?rpc_port:int ->
  ?restricted_rpcs:string ->
  string ->
  t

(** [initial_kernel node] returns the path to the kernel used to initialize the
    EVM state. Fails if [node] is a proxy node. *)
val initial_kernel : t -> string

(** [run ?wait ?extra_arguments evm_node] launches the EVM node server with
    the arguments given during {!create}, additional arguments can be
    passed via [extra_arguments].
    [wait] defaults to true, if it is set to false, the evm node is ran but we
    do not wait for it to be ready. *)
val run : ?wait:bool -> ?extra_arguments:string list -> t -> unit Lwt.t

(** [wait_for_ready evm_node] waits until [evm_node] is ready. *)
val wait_for_ready : ?timeout:float -> t -> unit Lwt.t

(** [wait_for_blueprint_applied ~timeout evm_node level] waits until
    [evm_node] has applied a blueprint locally for level [level]. *)
val wait_for_blueprint_applied : ?timeout:float -> t -> int -> unit Lwt.t

(** [wait_for_blueprint_invalid ~timeout evm_node] waits until
    [evm_node] has seen an invalid blueprint. *)
val wait_for_blueprint_invalid : ?timeout:float -> t -> unit Lwt.t

(** [wait_for_blueprint_injected ~timeout evm_node level] waits until
    [evm_node] has injected a blueprint for level [level] to its rollup node. *)
val wait_for_blueprint_injected : ?timeout:float -> t -> int -> unit Lwt.t

val wait_for_pending_upgrade : ?timeout:float -> t -> (string * string) Lwt.t

val wait_for_successful_upgrade : ?timeout:float -> t -> (string * int) Lwt.t

val wait_for_block_producer_locked : ?timeout:float -> t -> unit Lwt.t

val wait_for_block_producer_tx_injected : ?timeout:float -> t -> string Lwt.t

val wait_for_invalid_kernel : ?timeout:float -> t -> unit Lwt.t

val wait_for_retrying_connect : ?timeout:float -> t -> unit Lwt.t

val wait_for_rollup_node_follower_connection_acquired :
  ?timeout:float -> t -> unit Lwt.t

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

(** [patch_config_with_experimental_feature ?wal_sqlite_journal_mode
    ?drop_duplicate_when_injection json_config] patch a config to add
    experimental feature. Each optional argument add the correspondent
    experimental feature. *)
val patch_config_with_experimental_feature :
  ?drop_duplicate_when_injection:bool -> JSON.t -> JSON.t

(** [init ?patch_config ?name ?runner ?mode ?data_dir ?rpc_addr
    ?rpc_port rollup_node_endpoint] creates an EVM node server with
    {!create}, init the config with {!spawn_init_config}, patch it
    with [patch_config], then runs it with {!run}. *)
val init :
  ?patch_config:(JSON.t -> JSON.t) ->
  ?name:string ->
  ?runner:Runner.t ->
  ?mode:mode ->
  ?data_dir:string ->
  ?rpc_addr:string ->
  ?rpc_port:int ->
  ?restricted_rpcs:string ->
  string ->
  t Lwt.t

(** Get the RPC port given as [--rpc-port] to a node. *)
val rpc_port : t -> int

(** [spawn_run ?extra_arguments evm_node] same as {!run} but spawns a
    process. *)
val spawn_run : ?extra_arguments:string list -> t -> Process.t

(** Send SIGTERM and wait for the process to terminate.

    Default [timeout] is 30 seconds, after which SIGKILL is sent. *)
val terminate : ?timeout:float -> t -> unit Lwt.t

(** The same exact behavior as {!Sc_rollup_node.wait_for} but for the EVM node. *)
val wait_for : ?where:string -> t -> string -> (JSON.t -> 'a option) -> 'a Lwt.t

type delayed_transaction_kind = Deposit | Transaction | FaDeposit

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

(** [wait_for_missing_blueprint evm_node] waits for the
    event [evm_events_follower_missing_blueprint.v0] using
    {!wait_for} and return the missing blueprint level and
    block hash. *)
val wait_for_missing_blueprint : t -> (int * string) Lwt.t

(** [wait_for_rollup_node_ahead evm_node] waits for the event
    [evm_events_follower_rollup_node_ahead.v0] using {!wait_for} and returns the
    missing blueprint level. *)
val wait_for_rollup_node_ahead : t -> int Lwt.t

(** [wait_for_tx_pool_add_transaction ?timeout evm_node] waits for the event
    [tx_pool_add_transaction.v0] using {!wait_for} and returns the transaction
    hash. *)
val wait_for_tx_pool_add_transaction : ?timeout:float -> t -> string Lwt.t

(** Waits until a node terminates and return its status. If the node is
    not running, make the test fail. *)
val wait_for_shutdown_event : t -> int Lwt.t

(** [rpc_endpoint ?local ?private_ evm_node] returns the endpoint to communicate with the
    [evm_node]. If [private_] is true, the endpoint for the private
    RPC server is returned.

    If [local] is given ([false] by default),
    then [Constant.default_host] is used (it overrides [rpc-addr] or
    the [runner] argument).
*)
val rpc_endpoint : ?local:bool -> ?private_:bool -> t -> string

(** A deprecated alias for [rpc_endpoint] where [local] optional parameter is not given. *)
val endpoint : ?private_:bool -> t -> string

(** JSON-RPC request. *)
type request = {method_ : string; parameters : JSON.u}

(** [call_evm_rpc ?private_ evm_node ~request] sends a JSON-RPC request to
    the [evm_node], for the given [request].
    If [private_] is true, the request is sent to the private RPC
    server. *)
val call_evm_rpc : ?private_:bool -> t -> request -> JSON.t Lwt.t

(** [batch_evm_rpc ?private_ evm_node ~requests] sends multiple JSON-RPC requests
    to the [evm_node], for the given [requests].
    If [private_] is true, the requests are sent to the private RPC
    server. *)
val batch_evm_rpc : ?private_:bool -> t -> request list -> JSON.t Lwt.t

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

(** [init_from_rollup_node_data_dir ?reconstruct evm_node rollup_node]
    initialises the data dir of the evm node by importing the evm
    state from a rollup node data dir. [devmode] is false by default. *)
val init_from_rollup_node_data_dir :
  ?reconstruct:string -> t -> Sc_rollup_node.t -> unit Lwt.t

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

val wait_termination : t -> unit Lwt.t

(** [make_kernel_installer_config ~output ()] create the config needed for the
    evm kernel used by the installer *)
val make_kernel_installer_config :
  ?mainnet_compat:bool ->
  ?remove_whitelist:bool ->
  ?kernel_root_hash:string ->
  ?chain_id:int ->
  ?bootstrap_balance:Wei.t ->
  ?bootstrap_accounts:string list ->
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
  ?enable_fa_bridge:bool ->
  ?enable_dal:bool ->
  ?dal_slots:int list ->
  output:string ->
  unit ->
  (Process.t, unit) Runnable.t

module Agent : sig
  val create :
    ?path:string ->
    ?name:string ->
    ?data_dir:string ->
    ?mode:mode ->
    string ->
    Agent.t ->
    t Lwt.t

  val init :
    ?patch_config:(JSON.t -> JSON.t) ->
    ?name:string ->
    ?mode:mode ->
    ?data_dir:string ->
    string ->
    Agent.t ->
    t Lwt.t

  val rpc_port : t -> int

  val name : t -> string
end
