(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type proxy = {rollup_node_endpoint : Uri.t}

type time_between_blocks =
  | Nothing  (** Does not produce any block if not forced by the private RPC *)
  | Time_between_blocks of float
      (** Maximum time interval between blocks. If transactions are present
          in the tx pool, blocks will be created as soon as possible. However,
          if there are no transactions to include, a block is produced after
           [time_between_blocks]. *)

type sequencer = {
  rollup_node_endpoint : Uri.t;
      (** Rollup node endpoint used to make blueprints available and
          monitor the delayed inbox. *)
  preimages : string;  (** Path to the preimages directory. *)
  time_between_blocks : time_between_blocks;  (** See {!time_between_blocks}. *)
  private_rpc_port : int;  (** Port for internal RPC services *)
  sequencer : Signature.secret_key;
      (** The secret key used to sign the blueprints. *)
}

type observer = {evm_node_endpoint : Uri.t; preimages : string}

type 'a t = {
  rpc_addr : string;
  rpc_port : int;
  devmode : bool;
  cors_origins : string list;
  cors_headers : string list;
  verbose : bool;
  log_filter : log_filter_config;
  mode : 'a;
}

(** [default_data_dir] is the default value for [data_dir]. *)
val default_data_dir : string

(** [config_filename data_dir] returns
    the configuration filename from the [data_dir] *)
val config_filename : data_dir:string -> string

(** [save_proxy ~force ~data_dir configuration] writes the proxy [configuration] file in
    [data_dir]. If [force] is [true], existing configurations are
    overwritten. *)
val save_proxy : force:bool -> data_dir:string -> proxy t -> unit tzresult Lwt.t

(** Same as {!save_proxy} but for the sequencer configuration. *)
val save_sequencer :
  force:bool -> data_dir:string -> sequencer t -> unit tzresult Lwt.t

(** Same as {!save_proxy} but for the observer configuration. *)
val save_observer :
  force:bool -> data_dir:string -> observer t -> unit tzresult Lwt.t

(** [load_proxy ~data_dir] loads a proxy configuration stored in [data_dir]. *)
val load_proxy : data_dir:string -> proxy t tzresult Lwt.t

(** Same as {!load_proxy} but for the sequencer configuration. *)
val load_sequencer : data_dir:string -> sequencer t tzresult Lwt.t

(** Same as {!load_proxy} but for the observer configuration. *)
val load_observer : data_dir:string -> observer t tzresult Lwt.t

module Cli : sig
  val create_proxy :
    devmode:bool ->
    ?rpc_addr:string ->
    ?rpc_port:int ->
    ?cors_origins:string trace ->
    ?cors_headers:string trace ->
    ?log_filter:log_filter_config ->
    verbose:bool ->
    rollup_node_endpoint:Uri.t ->
    unit ->
    proxy t

  val create_sequencer :
    ?private_rpc_port:int ->
    devmode:bool ->
    ?rpc_addr:string ->
    ?rpc_port:int ->
    ?cors_origins:string trace ->
    ?cors_headers:string trace ->
    ?log_filter:log_filter_config ->
    verbose:bool ->
    ?rollup_node_endpoint:Uri.t ->
    ?preimages:string ->
    ?time_between_blocks:time_between_blocks ->
    sequencer:Signature.secret_key ->
    unit ->
    sequencer t

  val create_observer :
    devmode:bool ->
    ?rpc_addr:string ->
    ?rpc_port:int ->
    ?cors_origins:string trace ->
    ?cors_headers:string trace ->
    ?log_filter:log_filter_config ->
    verbose:bool ->
    ?evm_node_endpoint:Uri.t ->
    ?preimages:string ->
    unit ->
    observer t

  val create_or_read_proxy_config :
    data_dir:string ->
    devmode:bool ->
    ?rpc_addr:string ->
    ?rpc_port:int ->
    ?cors_origins:string trace ->
    ?cors_headers:string trace ->
    ?log_filter:log_filter_config ->
    verbose:bool ->
    rollup_node_endpoint:Uri.t ->
    unit ->
    proxy t tzresult Lwt.t

  val create_or_read_sequencer_config :
    data_dir:string ->
    devmode:bool ->
    ?rpc_addr:string ->
    ?rpc_port:int ->
    ?private_rpc_port:int ->
    ?cors_origins:string trace ->
    ?cors_headers:string trace ->
    ?log_filter:log_filter_config ->
    verbose:bool ->
    ?rollup_node_endpoint:Uri.t ->
    ?preimages:string ->
    ?time_between_blocks:time_between_blocks ->
    sequencer:Signature.secret_key ->
    unit ->
    sequencer t tzresult Lwt.t

  val create_or_read_observer_config :
    data_dir:string ->
    devmode:bool ->
    ?rpc_addr:string ->
    ?rpc_port:int ->
    ?cors_origins:string trace ->
    ?cors_headers:string trace ->
    ?log_filter:log_filter_config ->
    verbose:bool ->
    ?evm_node_endpoint:Uri.t ->
    ?preimages:string ->
    unit ->
    observer t tzresult Lwt.t
end
