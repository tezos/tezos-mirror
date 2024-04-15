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
}

type sequencer = {
  preimages : string;  (** Path to the preimages directory. *)
  preimages_endpoint : Uri.t option;
      (** Endpoint where pre-images can be fetched individually when missing. *)
  time_between_blocks : time_between_blocks;  (** See {!time_between_blocks}. *)
  max_number_of_chunks : int;
      (** The maximum number of chunks per blueprints. *)
  private_rpc_port : int option;  (** Port for internal RPC services *)
  sequencer : Client_keys.sk_uri;  (** The key used to sign the blueprints. *)
  blueprints_publisher_config : blueprints_publisher_config;
}

type observer = {
  evm_node_endpoint : Uri.t;
  preimages : string;
  preimages_endpoint : Uri.t option;
}

type t = {
  rpc_addr : string;
  rpc_port : int;
  devmode : bool;
  cors_origins : string list;
  cors_headers : string list;
  log_filter : log_filter_config;
  sequencer : sequencer option;
  observer : observer option;
  max_active_connections :
    Tezos_rpc_http_server.RPC_server.Max_active_rpc_connections.t;
  tx_pool_timeout_limit : int64;
  tx_pool_addr_limit : int64;
  tx_pool_tx_per_addr_limit : int64;
  keep_alive : bool;
  rollup_node_endpoint : Uri.t;
  verbose : Internal_event.level;
}

(** [default_data_dir] is the default value for [data_dir]. *)
val default_data_dir : string

(** [config_filename data_dir] returns
    the configuration filename from the [data_dir] *)
val config_filename : data_dir:string -> string

(** [save ~force ~data_dir configuration] writes the [configuration]
    file in [data_dir]. If [force] is [true], existing configurations
    are overwritten. *)
val save : force:bool -> data_dir:string -> t -> unit tzresult Lwt.t

(** [load ~data_dir] loads a proxy configuration stored in [data_dir]. *)
val load : data_dir:string -> t tzresult Lwt.t

(** [sequencer_config_exn config] returns the sequencer config of
    [config] or fails *)
val sequencer_config_exn : t -> sequencer tzresult

(** [observer_config_exn config] returns the observer config of
    [config] or fails *)
val observer_config_exn : t -> observer tzresult

(** [sequencer_config_dft ()] returns the default sequencer config
    populated with given value. *)
val sequencer_config_dft :
  ?preimages:string ->
  ?preimages_endpoint:Uri.t ->
  ?time_between_blocks:time_between_blocks ->
  ?max_number_of_chunks:int ->
  ?private_rpc_port:int ->
  sequencer:Client_keys.sk_uri ->
  ?max_blueprints_lag:int ->
  ?max_blueprints_ahead:int ->
  ?max_blueprints_catchup:int ->
  ?catchup_cooldown:int ->
  unit ->
  sequencer

(** [observer_config_dft ()] returns the default observer config
    populated with given value. *)
val observer_config_dft :
  ?preimages:string ->
  ?preimages_endpoint:Uri.t ->
  evm_node_endpoint:Uri.t ->
  unit ->
  observer

module Cli : sig
  val create :
    devmode:bool ->
    ?rpc_addr:string ->
    ?rpc_port:int ->
    ?cors_origins:string list ->
    ?cors_headers:string list ->
    ?tx_pool_timeout_limit:int64 ->
    ?tx_pool_addr_limit:int64 ->
    ?tx_pool_tx_per_addr_limit:int64 ->
    keep_alive:bool ->
    rollup_node_endpoint:Uri.t ->
    verbose:bool ->
    ?preimages:string ->
    ?preimages_endpoint:Uri.t ->
    ?time_between_blocks:time_between_blocks ->
    ?max_number_of_chunks:int ->
    ?private_rpc_port:int ->
    ?sequencer_key:Client_keys.sk_uri ->
    ?evm_node_endpoint:Uri.t ->
    ?log_filter_max_nb_blocks:int ->
    ?log_filter_max_nb_logs:int ->
    ?log_filter_chunk_size:int ->
    ?max_blueprints_lag:int ->
    ?max_blueprints_ahead:int ->
    ?max_blueprints_catchup:int ->
    ?catchup_cooldown:int ->
    unit ->
    t

  val create_or_read_config :
    data_dir:string ->
    devmode:bool ->
    ?rpc_addr:string ->
    ?rpc_port:int ->
    ?cors_origins:string list ->
    ?cors_headers:string list ->
    ?tx_pool_timeout_limit:int64 ->
    ?tx_pool_addr_limit:int64 ->
    ?tx_pool_tx_per_addr_limit:int64 ->
    keep_alive:bool ->
    ?rollup_node_endpoint:Uri.t ->
    verbose:bool ->
    ?preimages:string ->
    ?preimages_endpoint:Uri.t ->
    ?time_between_blocks:time_between_blocks ->
    ?max_number_of_chunks:int ->
    ?private_rpc_port:int ->
    ?sequencer_key:Client_keys.sk_uri ->
    ?evm_node_endpoint:Uri.t ->
    ?max_blueprints_lag:int ->
    ?max_blueprints_ahead:int ->
    ?max_blueprints_catchup:int ->
    ?catchup_cooldown:int ->
    ?log_filter_max_nb_blocks:int ->
    ?log_filter_max_nb_logs:int ->
    ?log_filter_chunk_size:int ->
    unit ->
    t tzresult Lwt.t
end
