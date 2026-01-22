(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type neighbor = {addr : string; port : int}

(** The history mode decides for how long shards are kept in the store. *)
type history_mode =
  | Rolling of {blocks : [`Auto | `Some of int]}
      (** [Rolling {block = `Some n}] keeps the shards for about [n]
          blocks. [Rolling {block = `Auto}] infers the number of
          blocks depending on the L1 parametric constants and the
          profile. *)
  | Full  (** [Full] keeps the shards forever *)

(** The configuration of the validation of shards in batch mode.*)
type batching_configuration =
  | Disabled
      (** [Disabled] enforces the validation of shards one by one at reception time. *)
  | Enabled of {time_interval : int}
      (** [Enabled {time_interval}] accumulates messages received during
          [time_interval] milliseconds and verifies all shards related to the
          same commitment in the same batch in one pass. *)

(** Configuration settings for experimental features, with no backward
    compatibility guarantees. *)
type experimental_features = unit

type publish_slots_regularly = {
  frequency : int;
  slot_index : int;
  secret_key : Signature.Secret_key.t;
}

type t = {
  data_dir : string;  (** The path to the DAL node data directory. *)
  rpc_addr : P2p_point.Id.t;
      (** The TCP address the DAL node's RPC server listens to. *)
  listen_addr : P2p_point.Id.t;  (** The TCP address bound by the DAL node. *)
  public_addr : P2p_point.Id.t;
      (** The TCP address at which this instance can be reached. *)
  peers : string list;
      (** The list of P2P peers to connect to at startup, in addition to the
          list given by L1 node's configuration parameter
          dal_config.bootstrap_peers. *)
  expected_pow : float;
      (** The expected PoW difficulty level for the peers' identity. *)
  endpoint : Uri.t;  (** The endpoint of a Tezos L1 node. *)
  slots_backup_uris : Uri.t list;
      (** Backup URIs to fetch slot data if missing and unrecoverable from shards. *)
  trust_slots_backup_uris : bool;
      (** Whether to trust the data downlaoded from the provided HTTP backup URIs. *)
  metrics_addr : P2p_point.Id.t option;
      (** The TCP address of the node's server used to export metrics. *)
  profile : Profile_manager.unresolved_profile;
      (** The profiles determining the topics of interest. *)
  history_mode : history_mode;
  version : int;  (** The version of the configuration. *)
  service_name : string;  (** Name of the service provided by this node. *)
  service_namespace : string;  (** Namespace for the service. *)
  telemetry_env : string option; (* Running environment name for telemetry *)
  experimental_features : experimental_features;  (** Experimental features.  *)
  fetch_trusted_setup : bool;
      (** Should the trusted setup be downloaded if not found or has invalid hash. *)
  verbose : bool;
      (** Whether to emit detailed events for frequently received control
          messages from remote peers. *)
  ignore_l1_config_peers : bool;
      (** Ignore the boot(strap) peers provided by L1. *)
  disable_amplification : bool;  (** Disable amplification. *)
  batching_configuration : batching_configuration;
      (** The configuration of the batching of the shards.
          The default is [Enabled{time_interval=100}]. *)
  publish_slots_regularly : publish_slots_regularly option;
}

(** [default] is the default configuration. *)
val default : t

(** [default_metrics_port] is the default network port opened for metrics *)
val default_metrics_port : int

(** [store_path config] returns a path for the store *)
val store_path : t -> string

(** [default_config_file data_dir] constructs a default configuration file path
    from a data dir. *)
val default_config_file : string -> string

(** [save config] writes config file in [config.data_dir] *)
val save : config_file:string -> t -> unit tzresult Lwt.t

(** [load ~on_file_not_found ~config_file ()] load config file from
    [config.data_dir] if it exists. If not it calls the provided on_file_not_found function.
    If no such function is provided, an error is returned.
*)
val load :
  ?on_file_not_found:(unit -> t tzresult Lwt.t) ->
  config_file:string ->
  unit ->
  t tzresult Lwt.t

(** [identity_file config] returns the absolute path to the
    "identity.json" file of the DAL node, based on the configuration
    [config]. *)
val identity_file : t -> string

(** [peers_file config] returns the absolute path to the "peers.json"
    file of the DAL node, based on the configuration [config]. *)
val peers_file : t -> string

val legacy_network_name : string
