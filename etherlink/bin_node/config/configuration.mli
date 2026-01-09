(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2024 Functori, <contact@functori.com>                       *)
(*                                                                           *)
(*****************************************************************************)

open Evm_node_lib_dev_encoding

(** A list of network officially supported by the EVM node. *)
type supported_network = Mainnet | Testnet | Shadownet

val pp_supported_network : Format.formatter -> supported_network -> unit

type log_filter_config = {
  max_nb_blocks : int;  (** Maximum block range for [get_logs]. *)
  max_nb_logs : int;  (** Maximum number of logs that [get_logs] can return. *)
  chunk_size : int;
      (** Number of blocks that will be filtered in a batch before
      checking if the bound on produced logs has been reached.
      See [get_logs] for more details. *)
}

type time_between_blocks =
  | Nothing  (** Does not produce any block if not forced by the private RPC *)
  | Time_between_blocks of float
      (** Maximum time interval between blocks. If transactions are present
          in the tx pool, blocks will be created as soon as possible. However,
          if there are no transactions to include, a block is produced after
           [time_between_blocks]. *)

type native_execution_policy = Always | Rpcs_only | Never

type kernel_execution_config = {
  preimages : string option;
      (** Path to the preimages directory. if none, it uses a default
          path under the data_dir *)
  preimages_endpoint : Uri.t option;
      (** Endpoint where pre-images can be fetched individually when missing. *)
  native_execution_policy : native_execution_policy;
      (** Policy deciding when to use the native execution for supported
          kernels. *)
}

type blueprints_publisher_config = {
  max_blueprints_lag : int;
      (** The maximum advance (in blueprints) the Sequencer accepts to
          have before trying to send its backlog again. *)
  max_blueprints_ahead : int;
      (** The maximum advance (in blueprints) the Sequencer
          accepts. *)
  max_blueprints_catchup : int;
      (** The maximum number of blueprints the Sequencer resends at
          once. *)
  catchup_cooldown : int;
      (** The maximum number of Layer 1 blocks the Sequencer waits
          after resending its blueprints before trying to catch-up
          again. *)
  dal_slots : int list option;
      (** When set to [Some l], l is the list of DAL slot indices to
          use to publish blueprints, when set to [None] the sequencer
          is not allowed to use the DAL. *)
}

type garbage_collector_parameters = {
  split_frequency_in_seconds : int;
  number_of_chunks : int;
}

type history_mode =
  | Archive  (** Keeps all blocks, operations and states. *)
  | Rolling of garbage_collector_parameters
      (** Keep blocks, operations and states for a period defined by
          {!type-garbage_collector_parameters}. *)
  | Full of garbage_collector_parameters
      (** Keep all blocks and transactions. Keep operations and states for a period defined by
              {!type-garbage_collector_parameters}. *)

(** Compare history modes and ignore [garbage_collector_parameters] *)
val history_mode_partial_eq : history_mode -> history_mode -> bool

(** RPC server implementation. *)
type rpc_server =
  | Resto  (** Resto/Cohttp (default) *)
  | Dream  (** Dream/httpun *)

(** Profiling mode for PVM execution. *)
type profile_mode = Minimal | Flamegraph

(** Parameters for monitoring websocket connection heartbeats. *)
type monitor_websocket_heartbeat = {ping_interval : float; ping_timeout : float}

val chain_id : supported_network -> L2_types.chain_id

type l2_chain = {
  chain_id : L2_types.chain_id;
  chain_family : L2_types.ex_chain_family;
}

type tx_queue = {
  max_size : int;
  max_transaction_batch_length : int option;
  max_lifespan_s : int;
  tx_per_addr_limit : int64;
}

type websocket_rate_limit = {
  max_frames : int;
  max_messages : int option;
  interval : int;
  strategy : [`Wait | `Error | `Close];
}

type websockets_config = {
  max_message_length : int;
  monitor_heartbeat : monitor_websocket_heartbeat option;
  rate_limit : websocket_rate_limit option;
}

(** Configuration settings for experimental features, with no backward
    compatibility guarantees. *)
type experimental_features = {
  drop_duplicate_on_injection : bool;
  blueprints_publisher_order_enabled : bool;
  enable_send_raw_transaction : bool;
  overwrite_simulation_tick_limit : bool;
  rpc_server : rpc_server;
  spawn_rpc : int option;
  l2_chains : l2_chain list option;
  periodic_snapshot_path : string option;
  preconfirmation_stream_enabled : bool;
}

type gcp_key = {
  project : string;
  keyring : string;
  region : string;
  key : string;
  version : int;
}

type sequencer_key = Wallet of Client_keys.sk_uri | Gcp_key of gcp_key

type sequencer = {
  time_between_blocks : time_between_blocks;
      (** See {!type-time_between_blocks}. *)
  max_number_of_chunks : int;
      (** The maximum number of chunks per blueprints. *)
  sequencer : sequencer_key list;  (** The keys used to sign the blueprints. *)
  blueprints_publisher_config : blueprints_publisher_config;
  sunset_sec : int64;
}

type observer = {
  evm_node_endpoint : Uri.t;
  rollup_node_tracking : bool;
  fail_on_divergence : bool;
}

type proxy = {
  finalized_view : bool option;
      (** Provide a view on the latest final state of the rollup, not its
          current HEAD. *)
  evm_node_endpoint : Uri.t option;
      (** If provided, the EVM node will inject transactions to this endpoint
          instead of to its companian rollup node. *)
  ignore_block_param : bool;
}

type fee_history_max_count = Unlimited | Limit of int

type fee_history = {max_count : fee_history_max_count; max_past : int option}

type restricted_rpcs =
  | Unrestricted
  | Pattern of {raw : string; regex : Re.re}
  | Blacklist of string list
  | Whitelist of string list

type limit = Unlimited | Limit of int

type rpc = {
  port : int;
  addr : string;
  cors_origins : string list;
  cors_headers : string list;
  max_active_connections :
    Tezos_rpc_http_server.RPC_server.Max_active_rpc_connections.t;
  batch_limit : limit;
  restricted_rpcs : restricted_rpcs;
}

type db = {
  pool_size : int;  (** Size of the database connection pool *)
  max_conn_reuse_count : int option;
      (** Maximum number of times a connection can be reused *)
}

type gcp_authentication_method = Gcloud_auth | Metadata_server

type gcp_kms = {
  pool_size : int;
  authentication_method : gcp_authentication_method;
  authentication_retries : int;
  authentication_frequency_min : int;
  authentication_retry_backoff_sec : int;
  authentication_timeout_sec : int;
  gcloud_path : string;
}

type performance_profile = Default | Performance

type telemetry_config = {
  config : Octez_telemetry.Opentelemetry_config.t;
  trace_host_functions : bool;
}

type t = {
  data_dir : string;
  public_rpc : rpc;
  private_rpc : rpc option;
  websockets : websockets_config option;
  log_filter : log_filter_config;
  kernel_execution : kernel_execution_config;
  sequencer : sequencer;
  observer : observer option;
  proxy : proxy;
  gcp_kms : gcp_kms;
  keep_alive : bool;
  rollup_node_endpoint : Uri.t;
  rpc_timeout : float;
  verbose : Internal_event.level;
  experimental_features : experimental_features;
  fee_history : fee_history;
  finalized_view : bool;
  history_mode : history_mode option;
  db : db;
  opentelemetry : telemetry_config;
  tx_queue : tx_queue;
  performance_profile : performance_profile;
}

(** [chain_family_from_l2_chains t] returns the chain_family in
  the experimental feature if there's only one chain.

  This function will be removed when multichain is implemented *)
val retrieve_chain_family :
  l2_chains:l2_chain list option -> L2_types.ex_chain_family

val history_mode_encoding : history_mode Data_encoding.t

val pp_history_mode_debug : Format.formatter -> history_mode -> unit

val pp_history_mode_info : Format.formatter -> history_mode -> unit

val native_execution_policy_encoding : native_execution_policy Data_encoding.t

val performance_profile_encoding : performance_profile Data_encoding.t

(** [encoding data_dir] is the encoding of {!t} based on data dir [data_dir].

    If [encoding] is passed, some default values are set according to the
    selected network, for a more straightforward UX. *)
val encoding : ?network:supported_network -> unit -> t Data_encoding.t

(** Encoding for {!type-rpc_server}. *)
val rpc_server_encoding : rpc_server Data_encoding.t

(** [save ~force configuration config_file] writes the [config_file]
    file. If [force] is [true], existing configurations are
    overwritten. *)
val save : force:bool -> t -> string -> unit tzresult Lwt.t

val load_file : ?network:supported_network -> string -> t tzresult Lwt.t

(** [load config_file] loads the configuration stored in
    [config_file] with preimage directory relative to [data_dir] or
    [config.data_dir]. *)
val load : ?network:supported_network -> string -> t tzresult Lwt.t

(** [sequencer_keys config] returns the keys the sequencer should use, or
    fails. *)
val sequencer_keys : t -> sequencer_key list tzresult

val gcp_key_from_string_opt : string -> gcp_key option

(** [observer_config_exn config] returns the observer config of
    [config] or fails *)
val observer_config_exn : t -> observer tzresult

(** [observer_config_dft ()] returns the default observer config
    populated with given value. *)
val observer_config_dft :
  evm_node_endpoint:Uri.t ->
  ?rollup_node_tracking:bool ->
  ?fail_on_divergence:bool ->
  unit ->
  observer

val make_pattern_restricted_rpcs : string -> restricted_rpcs

val string_of_history_mode_debug : history_mode -> string

val string_of_history_mode_info : history_mode -> string

val history_mode_of_string_opt : string -> history_mode option

(** [retention ~days] returns the GC parameters to retain [days] of history
    when provided or the default otherwise. *)
val gc_param_from_retention_period : days:int -> garbage_collector_parameters

val default_history_mode : history_mode

val default_data_dir : string

val get_data_dir : data_dir:string option -> string

val config_filename :
  data_dir:string option -> ?config_file:string -> unit -> string

val preimages_path : t -> string

module Cli : sig
  val create :
    data_dir:string option ->
    ?rpc_addr:string ->
    ?rpc_port:int ->
    ?rpc_batch_limit:limit ->
    ?cors_origins:string list ->
    ?cors_headers:string list ->
    ?enable_websocket:bool ->
    ?tx_queue_max_lifespan:int ->
    ?tx_queue_max_size:int ->
    ?tx_queue_tx_per_addr_limit:int64 ->
    ?keep_alive:bool ->
    ?rollup_node_endpoint:Uri.t ->
    ?dont_track_rollup_node:bool ->
    ?verbose:bool ->
    ?profiling:bool ->
    ?preimages:string ->
    ?preimages_endpoint:Uri.t ->
    ?native_execution_policy:native_execution_policy ->
    ?time_between_blocks:time_between_blocks ->
    ?max_number_of_chunks:int ->
    ?private_rpc_port:int ->
    ?sequencer_keys:sequencer_key list ->
    ?evm_node_endpoint:Uri.t ->
    ?log_filter_max_nb_blocks:int ->
    ?log_filter_max_nb_logs:int ->
    ?log_filter_chunk_size:int ->
    ?max_blueprints_lag:int ->
    ?max_blueprints_ahead:int ->
    ?max_blueprints_catchup:int ->
    ?catchup_cooldown:int ->
    ?restricted_rpcs:restricted_rpcs ->
    ?finalized_view:bool ->
    ?proxy_ignore_block_param:bool ->
    ?dal_slots:int list ->
    ?network:supported_network ->
    ?history_mode:history_mode ->
    ?sunset_sec:int64 ->
    ?rpc_timeout:float ->
    ?fail_on_divergence:bool ->
    unit ->
    t

  val patch_configuration_from_args :
    data_dir:string option ->
    ?rpc_addr:string ->
    ?rpc_port:int ->
    ?rpc_batch_limit:limit ->
    ?cors_origins:string trace ->
    ?cors_headers:string trace ->
    ?enable_websocket:bool ->
    ?tx_queue_max_lifespan:int ->
    ?tx_queue_max_size:int ->
    ?tx_queue_tx_per_addr_limit:int64 ->
    ?keep_alive:bool ->
    ?rollup_node_endpoint:Uri.t ->
    ?dont_track_rollup_node:bool ->
    ?verbose:bool ->
    ?profiling:bool ->
    ?preimages:string ->
    ?preimages_endpoint:Uri.t ->
    ?native_execution_policy:native_execution_policy ->
    ?time_between_blocks:time_between_blocks ->
    ?max_number_of_chunks:int ->
    ?private_rpc_port:int ->
    ?sequencer_keys:sequencer_key list ->
    ?evm_node_endpoint:Uri.t ->
    ?log_filter_max_nb_blocks:int ->
    ?log_filter_max_nb_logs:int ->
    ?log_filter_chunk_size:int ->
    ?max_blueprints_lag:int ->
    ?max_blueprints_ahead:int ->
    ?max_blueprints_catchup:int ->
    ?catchup_cooldown:int ->
    ?restricted_rpcs:restricted_rpcs ->
    ?finalized_view:bool ->
    ?proxy_ignore_block_param:bool ->
    ?history_mode:history_mode ->
    ?dal_slots:int list ->
    ?sunset_sec:int64 ->
    ?rpc_timeout:float ->
    ?fail_on_divergence:bool ->
    t ->
    t

  val create_or_read_config :
    data_dir:string option ->
    ?rpc_addr:string ->
    ?rpc_port:int ->
    ?rpc_batch_limit:limit ->
    ?cors_origins:string list ->
    ?cors_headers:string list ->
    ?enable_websocket:bool ->
    ?tx_queue_max_lifespan:int ->
    ?tx_queue_max_size:int ->
    ?tx_queue_tx_per_addr_limit:int64 ->
    ?keep_alive:bool ->
    ?rollup_node_endpoint:Uri.t ->
    ?dont_track_rollup_node:bool ->
    ?verbose:bool ->
    ?profiling:bool ->
    ?preimages:string ->
    ?preimages_endpoint:Uri.t ->
    ?native_execution_policy:native_execution_policy ->
    ?time_between_blocks:time_between_blocks ->
    ?max_number_of_chunks:int ->
    ?private_rpc_port:int ->
    ?sequencer_keys:sequencer_key list ->
    ?evm_node_endpoint:Uri.t ->
    ?max_blueprints_lag:int ->
    ?max_blueprints_ahead:int ->
    ?max_blueprints_catchup:int ->
    ?catchup_cooldown:int ->
    ?log_filter_max_nb_blocks:int ->
    ?log_filter_max_nb_logs:int ->
    ?log_filter_chunk_size:int ->
    ?restricted_rpcs:restricted_rpcs ->
    ?finalized_view:bool ->
    ?proxy_ignore_block_param:bool ->
    ?dal_slots:int list ->
    ?network:supported_network ->
    ?history_mode:history_mode ->
    ?sunset_sec:int64 ->
    ?rpc_timeout:float ->
    ?fail_on_divergence:bool ->
    string ->
    t tzresult Lwt.t
end

val time_between_blocks_encoding : time_between_blocks Data_encoding.t

val pp_time_between_blocks : Format.formatter -> time_between_blocks -> unit

(** [describe ()] prints the JSON schema of the configuration file to the
    standard output. *)
val describe : unit -> unit

val pp_print_json : Format.formatter -> t -> unit

val observer_evm_node_endpoint : supported_network -> string

val default_gcp_kms : gcp_kms
