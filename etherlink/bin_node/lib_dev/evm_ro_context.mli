(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t = {
  preimages : string;
  preimages_endpoint : Uri.t option;
  native_execution_policy : Configuration.native_execution_policy;
  data_dir : string;
  store : Evm_store.t;
  smart_rollup_address : Tezos_crypto.Hashed.Smart_rollup_address.t;
  index : Pvm.Context.ro_index;
  finalized_view : bool;
  execution_pool : Lwt_domain.pool;
  trace_host_funs : bool;
}

(** [load configuration] creates a new read-only handler on the
    node’s context. You can have as many read-only handlers as you want split
    over as many processes.

    If [smart_rollup_address] is omitted, the argument is fetched from the
    store.

    If [network] is set, the function performs additional sanity checks to
    ensure its local context is consistent with the expected network. *)
val load :
  pool:Lwt_domain.pool ->
  ?network:Configuration.supported_network ->
  ?smart_rollup_address:Address.t ->
  Configuration.t ->
  t tzresult Lwt.t

(** [read_chain_family chain_id] returns the chain_family associated to the
    chain_id passed on parameter. *)
val read_chain_family :
  t -> L2_types.chain_id -> L2_types.ex_chain_family tzresult Lwt.t

(** [read_enable_multichain_flag] reads the value of the `enable_multichain`
    feature_flag that enables multichain and tezos compatibility on the l2_node. *)
val read_enable_multichain_flag : t -> bool tzresult Lwt.t

(** [preload_known_kernels ctxt] uses [ctxt] to preload every kernel known to
    the node in the Fast Execution kernel cache. *)
val preload_known_kernels : t -> unit tzresult Lwt.t

(** [preload_kernel_from_level ctxt level] uses [ctxt] to preload the kernel
    used to apply the block at level [level]. *)
val preload_kernel_from_level :
  t -> Ethereum_types.quantity -> unit tzresult Lwt.t

val next_blueprint_number : t -> Ethereum_types.quantity tzresult Lwt.t

val ro_backend :
  ?evm_node_endpoint:Uri.t ->
  t ->
  Configuration.t ->
  (module Services_backend_sig.S)

type replay_result =
  | Replay_success of {
      block : Ethereum_types.legacy_transaction_object L2_types.block;
      evm_state : Evm_state.t;
      diverged : bool;
      process_time : Ptime.span;
      execution_gas : Ethereum_types.quantity;
    }
  | Replay_failure

val replay :
  t ->
  ?log_file:string ->
  ?profile:Configuration.profile_mode ->
  ?alter_evm_state:(Pvm.State.t -> (Pvm.State.t, tztrace) result Lwt.t) ->
  Ethereum_types.quantity ->
  replay_result tzresult Lwt.t

val evm_services_methods :
  t -> Configuration.time_between_blocks -> Rpc_server.evm_services_methods

val blueprints_range : t -> Blueprints_publisher.blueprints_range
