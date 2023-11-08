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

type t = {
  rpc_addr : string;
  rpc_port : int;
  debug : bool;
  rollup_node_endpoint : Uri.t;
  devmode : bool;
  cors_origins : string list;
  cors_headers : string list;
  verbose : bool;
  log_filter : log_filter_config;
}

(** [default_config] is the default value for the configuration. *)
val default : t

(** [default_data_dir] is the default value for [data_dir]. *)
val default_data_dir : string

(** [config_filename data_dir] returns
    the configuration filename from the [data_dir] *)
val config_filename : data_dir:string -> string

(** [save ~force ~data_dir configuration] writes the [configuration] file in
    [data_dir]. If [force] is [true], existing configurations are
    overwritten. *)
val save : force:bool -> data_dir:string -> t -> unit tzresult Lwt.t

(** [load ~data_dir] loads a configuration stored in [data_dir]. *)
val load : data_dir:string -> t tzresult Lwt.t

module Cli : sig
  val create :
    devmode:bool ->
    ?rpc_addr:string ->
    ?rpc_port:int ->
    ?debug:bool ->
    ?cors_origins:string trace ->
    ?cors_headers:string trace ->
    ?log_filter:log_filter_config ->
    rollup_node_endpoint:Uri.t ->
    verbose:bool ->
    unit ->
    t

  val create_or_read_config :
    data_dir:string ->
    devmode:bool ->
    ?rpc_addr:string ->
    ?rpc_port:int ->
    ?debug:bool ->
    ?cors_origins:string trace ->
    ?cors_headers:string trace ->
    ?log_filter:log_filter_config ->
    rollup_node_endpoint:Uri.t ->
    verbose:bool ->
    unit ->
    t tzresult Lwt.t
end
