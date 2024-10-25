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
      preimages_dir : string option;
      private_rpc_port : int option;  (** Port for private RPC server*)
      rollup_node_endpoint : string;
    }
  | Threshold_encryption_observer of {
      initial_kernel : string;
      preimages_dir : string;
      rollup_node_endpoint : string;
      bundler_node_endpoint : string;
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
      dal_slots : int list option;
    }
  | Sandbox of {
      initial_kernel : string;
      preimage_dir : string option;
      private_rpc_port : int option;
      time_between_blocks : time_between_blocks option;
      genesis_timestamp : Client.timestamp option;
      max_number_of_chunks : int option;
      wallet_dir : string option;
      tx_pool_timeout_limit : int option;
      tx_pool_addr_limit : int option;
      tx_pool_tx_per_addr_limit : int option;
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
      dal_slots : int list option;
    }
  | Proxy
  | Rpc of mode

(** Returns the mode of the EVM node. *)
val mode : t -> mode

(** Returns the name of the EVM node. *)
val name : t -> string

(** Returns the data_dir of the EVM node. *)
val data_dir : t -> string

(** Returns the path to the directory storing the preimages used by the
    kernel runned by the node. *)
val preimages_dir : t -> string

val supports_threshold_encryption : t -> bool

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

val wait_for_blueprint_finalized : ?timeout:float -> t -> int -> unit Lwt.t

(** [wait_for_predownload_kernel ?timeout evm_node ~root_hash] waits until
    [evm_node] has download a kernel with [root_hash]. *)
val wait_for_predownload_kernel :
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

val wait_for_block_producer_locked : ?timeout:float -> t -> unit Lwt.t

val wait_for_block_producer_tx_injected : ?timeout:float -> t -> string Lwt.t

val wait_for_retrying_connect : ?timeout:float -> t -> unit Lwt.t

val wait_for_rollup_node_follower_connection_acquired :
  ?timeout:float -> t -> unit Lwt.t

val wait_for_rollup_node_follower_disabled : ?timeout:float -> t -> unit Lwt.t

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

type garbage_collector = {
  split_frequency_in_seconds : int;
  history_to_keep_in_seconds : int;
}

(** [patch_config_with_experimental_feature
    ?node_transaction_validation ?drop_duplicate_when_injection
    ?block_storage_sqlite3 ?next_wasm_runtime json_config] patches a
    config to add experimental feature. Each optional argument add the
    correspondent experimental feature. *)
val patch_config_with_experimental_feature :
  ?drop_duplicate_when_injection:bool ->
  ?node_transaction_validation:bool ->
  ?block_storage_sqlite3:bool ->
  ?next_wasm_runtime:bool ->
  ?garbage_collector:garbage_collector ->
  unit ->
  JSON.t ->
  JSON.t

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

val resolve_or_timeout :
  ?timeout:float -> t -> name:string -> 'a Lwt.t -> 'a Lwt.t

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

(** [wait_for_shutdown ?can_terminate evm_node] waits until a node terminates
    and return its status. If the node is not running, make the test fail. If
    [can_terminate] is `true` and the node was already terminated, returns
    `None`. *)
val wait_for_shutdown_event : ?can_terminate:bool -> t -> int option Lwt.t

(** [wait_for_split ?level evm_node] waits untils the node terminates
    splitting its irmin context at level [level] if provided. *)
val wait_for_split : ?level:int -> t -> int Lwt.t

(** [wait_for_gc_finished ?gc_level ?head_level evm_node] waits untils
    the node terminates garbage collecting its context on head
    [head_level] at gc level [gc_level] if provided. *)
val wait_for_gc_finished :
  ?gc_level:int -> ?head_level:int -> t -> (int * int) Lwt.t

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

(** [reconstruct_from_rollup_node_data_dir ~boot_sector evm_node
    rollup_node] reconstructs the history of the rollup by replaying
    all messages. *)
val reconstruct_from_rollup_node_data_dir :
  boot_sector:string -> t -> Sc_rollup_node.t -> unit Lwt.t

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

(** [export_snapshot evm_node] exports a snapshot of the evm node in a temporary
    directory. It returns the path for the produced snapshot file. *)
val export_snapshot :
  ?compress_on_the_fly:bool -> t -> (Process.t, string) runnable

(** [import_snapshot ?force evm_node ~snapshot_file] imports the snapshot
    [snapshot_file] in the evm node.  *)
val import_snapshot :
  ?force:bool -> t -> snapshot_file:string -> (Process.t, unit) runnable

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
  ?set_account_code:(string * string) list ->
  ?enable_fa_bridge:bool ->
  ?enable_dal:bool ->
  ?dal_slots:int list ->
  output:string ->
  unit ->
  (Process.t, unit) Runnable.t

val debug_print_store_schemas :
  ?path:string -> ?hooks:Process_hooks.t -> unit -> unit Lwt.t

val man : ?path:string -> ?hooks:Process_hooks.t -> unit -> unit Lwt.t

val describe_config :
  ?path:string -> ?hooks:Process_hooks.t -> unit -> unit Lwt.t
