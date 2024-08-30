(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2024 Functori, <contact@functori.com>                       *)
(*                                                                           *)
(*****************************************************************************)

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

type kernel_execution_config = {
  preimages : string;  (** Path to the preimages directory. *)
  preimages_endpoint : Uri.t option;
      (** Endpoint where pre-images can be fetched individually when missing. *)
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

(** Configuration settings for experimental features, with no backward
    compatibility guarantees. *)
type experimental_features = {
  drop_duplicate_on_injection : bool;
  enable_send_raw_transaction : bool;
  node_transaction_validation : bool;
}

type sequencer = {
  time_between_blocks : time_between_blocks;
      (** See {!type-time_between_blocks}. *)
  max_number_of_chunks : int;
      (** The maximum number of chunks per blueprints. *)
  sequencer : Client_keys.sk_uri;  (** The key used to sign the blueprints. *)
  blueprints_publisher_config : blueprints_publisher_config;
}

type threshold_encryption_sequencer =
  | Threshold_encryption_sequencer of {
      time_between_blocks : time_between_blocks;
          (** See {!type-time_between_blocks}. *)
      max_number_of_chunks : int;
          (** The maximum number of chunks per blueprints. *)
      sequencer : Client_keys.sk_uri;
          (** The key used to sign the blueprints. *)
      blueprints_publisher_config : blueprints_publisher_config;
      sidecar_endpoint : Uri.t;
          (** Endpoint of the sequencer sidecar this sequencer connects to. *)
    }

type observer = {
  evm_node_endpoint : Uri.t;
  threshold_encryption_bundler_endpoint : Uri.t option;
  rollup_node_tracking : bool;
}

type proxy = {
  finalized_view : bool;
      (** Provide a view on the latest final state of the rollup, not its
          current HEAD. *)
  evm_node_endpoint : Uri.t option;
      (** If provided, the EVM node will inject transactions to this endpoint
          instead of to its companian rollup node. *)
  ignore_block_param : bool;
}

type fee_history = {max_count : int option; max_past : int option}

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

type t = {
  public_rpc : rpc;
  private_rpc : rpc option;
  log_filter : log_filter_config;
  kernel_execution : kernel_execution_config;
  sequencer : sequencer option;
  threshold_encryption_sequencer : threshold_encryption_sequencer option;
  observer : observer option;
  proxy : proxy;
  tx_pool_timeout_limit : int64;
  tx_pool_addr_limit : int64;
  tx_pool_tx_per_addr_limit : int64;
  keep_alive : bool;
  rollup_node_endpoint : Uri.t;
  verbose : Internal_event.level;
  experimental_features : experimental_features;
  fee_history : fee_history;
}

(** [encoding data_dir] is the encoding of {!t} based on data dir [data_dir]. *)
val encoding : string -> t Data_encoding.t

(** [default_data_dir] is the default value for [data_dir]. *)
val default_data_dir : string

(** [config_filename data_dir] returns
    the configuration filename from the [data_dir] *)
val config_filename : data_dir:string -> string

(** [save ~force ~data_dir configuration] writes the [configuration]
    file in [data_dir]. If [force] is [true], existing configurations
    are overwritten. *)
val save : force:bool -> data_dir:string -> t -> unit tzresult Lwt.t

val load_file : data_dir:string -> string -> t tzresult Lwt.t

(** [load ~data_dir] loads a proxy configuration stored in [data_dir]. *)
val load : data_dir:string -> t tzresult Lwt.t

(** [sequencer_config_exn config] returns the sequencer config of
    [config] or fails *)
val sequencer_config_exn : t -> sequencer tzresult

(** [threshold_encryption_sequencer_config_exn config] returns the threshold
    encryption sequencer config of [config] or fails. *)
val threshold_encryption_sequencer_config_exn :
  t -> threshold_encryption_sequencer tzresult

(** [observer_config_exn config] returns the observer config of
    [config] or fails *)
val observer_config_exn : t -> observer tzresult

(** [sequencer_config_dft ()] returns the default sequencer config
    populated with given value. *)
val sequencer_config_dft :
  ?time_between_blocks:time_between_blocks ->
  ?max_number_of_chunks:int ->
  sequencer:Client_keys.sk_uri ->
  ?max_blueprints_lag:int ->
  ?max_blueprints_ahead:int ->
  ?max_blueprints_catchup:int ->
  ?catchup_cooldown:int ->
  ?dal_slots:int list ->
  unit ->
  sequencer

(** [threshold_encryption_sequencer_config_dft ()] returns the default
    threshold encryption sequencer config populated with given value. *)
val threshold_encryption_sequencer_config_dft :
  ?time_between_blocks:time_between_blocks ->
  ?max_number_of_chunks:int ->
  sequencer:Client_keys.sk_uri ->
  ?sidecar_endpoint:Uri.t ->
  ?max_blueprints_lag:int ->
  ?max_blueprints_ahead:int ->
  ?max_blueprints_catchup:int ->
  ?catchup_cooldown:int ->
  ?dal_slots:int list ->
  unit ->
  threshold_encryption_sequencer

(** [observer_config_dft ()] returns the default observer config
    populated with given value. *)
val observer_config_dft :
  evm_node_endpoint:Uri.t ->
  ?threshold_encryption_bundler_endpoint:Uri.t ->
  ?rollup_node_tracking:bool ->
  unit ->
  observer

val make_pattern_restricted_rpcs : string -> restricted_rpcs

module Cli : sig
  val create :
    data_dir:string ->
    ?rpc_addr:string ->
    ?rpc_port:int ->
    ?rpc_batch_limit:limit ->
    ?cors_origins:string list ->
    ?cors_headers:string list ->
    ?tx_pool_timeout_limit:int64 ->
    ?tx_pool_addr_limit:int64 ->
    ?tx_pool_tx_per_addr_limit:int64 ->
    keep_alive:bool ->
    ?rollup_node_endpoint:Uri.t ->
    ?dont_track_rollup_node:bool ->
    verbose:bool ->
    ?preimages:string ->
    ?preimages_endpoint:Uri.t ->
    ?time_between_blocks:time_between_blocks ->
    ?max_number_of_chunks:int ->
    ?private_rpc_port:int ->
    ?sequencer_key:Client_keys.sk_uri ->
    ?evm_node_endpoint:Uri.t ->
    ?threshold_encryption_bundler_endpoint:Uri.t ->
    ?log_filter_max_nb_blocks:int ->
    ?log_filter_max_nb_logs:int ->
    ?log_filter_chunk_size:int ->
    ?max_blueprints_lag:int ->
    ?max_blueprints_ahead:int ->
    ?max_blueprints_catchup:int ->
    ?catchup_cooldown:int ->
    ?sequencer_sidecar_endpoint:Uri.t ->
    ?restricted_rpcs:restricted_rpcs ->
    ?proxy_finalized_view:bool ->
    ?proxy_ignore_block_param:bool ->
    ?dal_slots:int list ->
    unit ->
    t

  val patch_configuration_from_args :
    ?rpc_addr:string ->
    ?rpc_port:int ->
    ?rpc_batch_limit:limit ->
    ?cors_origins:string trace ->
    ?cors_headers:string trace ->
    ?tx_pool_timeout_limit:int64 ->
    ?tx_pool_addr_limit:int64 ->
    ?tx_pool_tx_per_addr_limit:int64 ->
    keep_alive:bool ->
    ?rollup_node_endpoint:Uri.t ->
    ?dont_track_rollup_node:bool ->
    verbose:bool ->
    ?preimages:string ->
    ?preimages_endpoint:Uri.t ->
    ?time_between_blocks:time_between_blocks ->
    ?max_number_of_chunks:int ->
    ?private_rpc_port:int ->
    ?sequencer_key:Client_keys.sk_uri ->
    ?evm_node_endpoint:Uri.t ->
    ?threshold_encryption_bundler_endpoint:Uri.t ->
    ?log_filter_max_nb_blocks:int ->
    ?log_filter_max_nb_logs:int ->
    ?log_filter_chunk_size:int ->
    ?max_blueprints_lag:int ->
    ?max_blueprints_ahead:int ->
    ?max_blueprints_catchup:int ->
    ?catchup_cooldown:int ->
    ?sequencer_sidecar_endpoint:Uri.t ->
    ?restricted_rpcs:restricted_rpcs ->
    ?proxy_finalized_view:bool ->
    ?proxy_ignore_block_param:bool ->
    dal_slots:int trace option ->
    t ->
    t

  val create_or_read_config :
    data_dir:string ->
    ?rpc_addr:string ->
    ?rpc_port:int ->
    ?rpc_batch_limit:limit ->
    ?cors_origins:string list ->
    ?cors_headers:string list ->
    ?tx_pool_timeout_limit:int64 ->
    ?tx_pool_addr_limit:int64 ->
    ?tx_pool_tx_per_addr_limit:int64 ->
    keep_alive:bool ->
    ?rollup_node_endpoint:Uri.t ->
    ?dont_track_rollup_node:bool ->
    verbose:bool ->
    ?preimages:string ->
    ?preimages_endpoint:Uri.t ->
    ?time_between_blocks:time_between_blocks ->
    ?max_number_of_chunks:int ->
    ?private_rpc_port:int ->
    ?sequencer_key:Client_keys.sk_uri ->
    ?evm_node_endpoint:Uri.t ->
    ?threshold_encryption_bundler_endpoint:Uri.t ->
    ?max_blueprints_lag:int ->
    ?max_blueprints_ahead:int ->
    ?max_blueprints_catchup:int ->
    ?catchup_cooldown:int ->
    ?log_filter_max_nb_blocks:int ->
    ?log_filter_max_nb_logs:int ->
    ?log_filter_chunk_size:int ->
    ?sequencer_sidecar_endpoint:Uri.t ->
    ?restricted_rpcs:restricted_rpcs ->
    ?proxy_finalized_view:bool ->
    ?proxy_ignore_block_param:bool ->
    ?dal_slots:int list ->
    unit ->
    t tzresult Lwt.t
end

val time_between_blocks_encoding : time_between_blocks Data_encoding.t

val pp_time_between_blocks : Format.formatter -> time_between_blocks -> unit

(** [describe ()] prints the JSON schema of the configuration file to the
    standard output. *)
val describe : unit -> unit

val pp_print_json : data_dir:string -> Format.formatter -> t -> unit
