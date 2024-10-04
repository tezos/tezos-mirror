(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>    *)
(* Copyright (c) 2018-2024 Nomadic Labs, <contact@nomadic-labs.com>          *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

type t

val get_version : t -> Tezos_version.Octez_node_version.t

type config = {
  genesis : Genesis.t;
  chain_name : Distributed_db_version.Name.t;
  sandboxed_chain_name : Distributed_db_version.Name.t;
  user_activated_upgrades : User_activated.upgrades;
  user_activated_protocol_overrides : User_activated.protocol_overrides;
  operation_metadata_size_limit : Shell_limits.operation_metadata_size_limit;
  data_dir : string;
  internal_events : Tezos_base.Internal_event_config.t;
  store_root : string;
  context_root_dir : string;
  protocol_root : string;
  patch_context :
    (Tezos_protocol_environment.Context.t ->
    Tezos_protocol_environment.Context.t tzresult Lwt.t)
    option;
  p2p : (P2p.config * P2p_limits.t) option;
  target : (Block_hash.t * int32) option;
  disable_mempool : bool;
      (** If [true], all non-empty mempools will be ignored. *)
  enable_testchain : bool;
      (** If [false], testchain related messages will be ignored. *)
  dal_config : Tezos_crypto_dal.Cryptobox.Config.t;
}

val create :
  ?sandboxed:bool ->
  ?sandbox_parameters:Data_encoding.json ->
  ?disable_context_pruning:bool ->
  ?history_mode:History_mode.t ->
  ?maintenance_delay:Storage_maintenance.delay ->
  singleprocess:bool ->
  version:string ->
  commit_info:Octez_node_version.commit_info ->
  config ->
  Shell_limits.peer_validator_limits ->
  Shell_limits.block_validator_limits ->
  Shell_limits.prevalidator_limits ->
  Shell_limits.chain_validator_limits ->
  (t, tztrace) result Lwt.t

val shutdown : t -> unit Lwt.t

(** [build_rpc_directory ~node_version node] builds a Tezos RPC
    directory for the node by gathering all the
    subdirectories. [node_version], and [node] contain all
    informations required to build such a directory. *)
val build_rpc_directory :
  node_version:Tezos_version.Octez_node_version.t ->
  t ->
  unit Tezos_rpc.Directory.t

(** [http_cache_header_tools] builds the tools required to expose the node
    internals in an abstract way to the http cache header middleware. *)
val http_cache_header_tools : t -> Http_cache_headers.tools
