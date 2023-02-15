(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2020 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

type chain_name = Distributed_db_version.Name.t

type blockchain_network = {
  alias : string option;
      (** as given to [--network], only for built-in networks *)
  genesis : Genesis.t;
  genesis_parameters : Genesis.Parameters.t option;
  chain_name : chain_name;
  old_chain_name : chain_name option;
  incompatible_chain_name : chain_name option;
  sandboxed_chain_name : chain_name;
  user_activated_upgrades : User_activated.upgrades;
  user_activated_protocol_overrides : User_activated.protocol_overrides;
  default_bootstrap_peers : string list;
}

(** List of built-in networks with their alias. *)
val builtin_blockchain_networks : (string * blockchain_network) list

(** Data encoding for custom blockchain configuration. *)
val blockchain_network_encoding : blockchain_network Data_encoding.t

type t = {
  data_dir : string;
  disable_config_validation : bool;
  p2p : p2p;
  rpc : rpc;
  log : Lwt_log_sink_unix.cfg;
  internal_events : Tezos_base.Internal_event_config.t;
  shell : Shell_limits.limits;
  blockchain_network : blockchain_network;
  metrics_addr : string list;
  dal : Tezos_crypto_dal.Cryptobox.Config.t;
}

and p2p = {
  expected_pow : float;
  bootstrap_peers : string list option;
  listen_addr : string option;
  advertised_net_port : int option;
  discovery_addr : string option;
  private_mode : bool;
  limits : Tezos_p2p_services.P2p_limits.t;
  disable_mempool : bool;
  enable_testchain : bool;
  reconnection_config : Tezos_p2p_services.Point_reconnection_config.t;
  disable_peer_discovery : bool;
}

and rpc = {
  listen_addrs : string list;
  cors_origins : string list;
  cors_headers : string list;
  tls : tls option;
  acl : RPC_server.Acl.policy;
  media_type : Media_type.Command_line.t;
}

and tls = {cert : string; key : string}

val data_dir_env_name : string

val default_data_dir : string

val default_p2p_port : int

val default_rpc_port : int

val default_p2p : p2p

val default_config : t

(** Update the given configuration. This is mostly used to apply the CLI
    arguments to the default config when starting or configuring the node.
    Arguments to this function roughly reflect the options in
    [Shared_arg.t]. *)
val update :
  ?disable_config_validation:bool ->
  ?data_dir:string ->
  ?min_connections:int ->
  ?expected_connections:int ->
  ?max_connections:int ->
  ?max_download_speed:int ->
  ?max_upload_speed:int ->
  ?binary_chunks_size:int ->
  ?peer_table_size:int ->
  ?expected_pow:float ->
  ?bootstrap_peers:string list option ->
  ?listen_addr:string ->
  ?advertised_net_port:int ->
  ?discovery_addr:string ->
  ?rpc_listen_addrs:string list ->
  ?allow_all_rpc:P2p_point.Id.addr_port_id list ->
  ?media_type:Media_type.Command_line.t ->
  ?metrics_addr:string list ->
  ?operation_metadata_size_limit:Shell_limits.operation_metadata_size_limit ->
  ?private_mode:bool ->
  ?disable_p2p_maintenance:bool ->
  ?disable_p2p_swap:bool ->
  ?disable_mempool:bool ->
  ?disable_mempool_precheck:bool ->
  ?enable_testchain:bool ->
  ?cors_origins:string list ->
  ?cors_headers:string list ->
  ?rpc_tls:tls ->
  ?log_output:Lwt_log_sink_unix.Output.t ->
  ?synchronisation_threshold:int ->
  ?history_mode:History_mode.t ->
  ?network:blockchain_network ->
  ?latency:int ->
  t ->
  t tzresult Lwt.t

val to_string : t -> string

val read : string -> t tzresult Lwt.t

(** Write configuration file.

    Also check whether the directory of the given filename
    is a data directory. *)
val write : string -> t -> unit tzresult Lwt.t

type error += Failed_to_parse_address of (string * string)

(** [resolve_listening_addrs listening_addr] parses [listening_addr]
   and returns a list of [points].  The default port is
   [default_p2p_port]. Fails if the address could not be parsed. *)
val resolve_listening_addrs : string -> P2p_point.Id.t list tzresult Lwt.t

(** [resolve_discovery_addrs disco_addrs] parses [disco_addr] and
   returns a list of [points]. [disco_addrs] must resolves to Ipv4
   addresses otherwise it fails. The default port is
   [default_discovery_port]. Fails if the address could not be
   parsed. *)
val resolve_discovery_addrs : string -> (Ipaddr.V4.t * int) list tzresult Lwt.t

(** [resolve_rpc_listening_addrs rpc_addrs] parses [rpc_addr] and
   returns a list of [points]. The default port is
   [default_rpc_port]. Fails if the address could not be parsed. *)
val resolve_rpc_listening_addrs : string -> P2p_point.Id.t list tzresult Lwt.t

(** [resolve_metrics_addrs ?default_metrics_port metrics_addr] parses [metrics_addr] and
    returns a list of [points]).
    The default host is "localhost" and the default port is [default_metrics_port].
    Fails if the address could not be parsed.*)
val resolve_metrics_addrs :
  ?default_metrics_port:int -> string -> P2p_point.Id.t list tzresult Lwt.t

(** [resolve_boostrap_addrs bs_addrs] parses [bs_addrs] and returns
   for each [addr] a list of [points]. The default port is
   [default_p2p_port]. Fails if the address could not be parsed. *)
val resolve_bootstrap_addrs :
  string list -> (P2p_point.Id.t * P2p_peer.Id.t option) list tzresult Lwt.t

val encoding : t Data_encoding.t

(** Return [p2p.bootstrap_peers] if not [None],
    [network.default_bootstrap_peers] otherwise. *)
val bootstrap_peers : t -> string list
