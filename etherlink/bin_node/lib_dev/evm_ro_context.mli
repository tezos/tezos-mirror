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

(** [chain_id ctxt] returns the chain id defined by the rollup. *)
val chain_id : t -> L2_types.chain_id tzresult Lwt.t

(** [michelson_runtime_chain_id ctxt] returns the chain id of the
    Michelson runtime defined by the rollup. *)
val michelson_runtime_chain_id : t -> L2_types.chain_id tzresult Lwt.t

(** [current_block_number_durable ctxt ~root] returns the current block
    number of the L2 chain prefixed by [root] from durable storage. *)
val current_block_number_durable :
  t -> root:string -> Ethereum_types.quantity tzresult Lwt.t

(** [storage_version ctxt] returns the latest storage version known to the
    current kernel. *)
val storage_version : t -> int tzresult Lwt.t

(** [kernel_version ctxt] returns the internal kernel version. *)
val kernel_version : t -> string tzresult Lwt.t

(** [kernel_root_hash ctxt] returns the internal kernel root hash. *)
val kernel_root_hash : t -> string option tzresult Lwt.t

val is_multichain_enabled : t -> bool tzresult Lwt.t

(** [list_runtimes ctxt] returns the list of runtimes activated in the kernel. *)
val list_runtimes : t -> Tezosx.runtime list tzresult Lwt.t

(** [smart_rollup_address_str ctxt] returns the smart rollup address as a
    string. *)
val smart_rollup_address_str : t -> string

val list_l1_l2_levels :
  t ->
  from_l1_level:int32 ->
  (int32 * Evm_store.L1_l2_finalized_levels.t) list tzresult Lwt.t

val l2_levels_of_l1_level :
  t -> int32 -> Evm_store.L1_l2_finalized_levels.t option tzresult Lwt.t

(** [block_param_to_block_number ctxt ~chain_family block_param] returns
    the block number of the block identified by [block_param]. Uses the
    store for hash lookups. *)
val block_param_to_block_number :
  t ->
  chain_family:_ L2_types.chain_family ->
  ?hash_column:[`Evm | `Michelson] ->
  Ethereum_types.Block_parameter.extended ->
  Ethereum_types.quantity tzresult Lwt.t

(** [single_chain_id_and_family ctxt ~config ~enable_multichain] should only
    be called if the node is expected to follow a single chain. *)
val single_chain_id_and_family :
  t ->
  config:Configuration.t ->
  enable_multichain:bool ->
  (L2_types.chain_id option * L2_types.ex_chain_family) tzresult Lwt.t

(** {2 Block storage operations (store-backed)} *)

(** [current_block_number ctxt] returns the most recent stored block number. *)
val current_block_number : t -> Ethereum_types.quantity tzresult Lwt.t

val nth_block :
  t ->
  full_transaction_object:bool ->
  Z.t ->
  Transaction_object.t Ethereum_types.block tzresult Lwt.t

val block_by_hash :
  t ->
  full_transaction_object:bool ->
  Ethereum_types.block_hash ->
  Transaction_object.t Ethereum_types.block tzresult Lwt.t

val block_receipts : t -> Z.t -> Transaction_receipt.t list tzresult Lwt.t

val block_range_receipts :
  t ->
  ?mask:Ethbloom.t ->
  Z.t ->
  int ->
  Transaction_receipt.t list tzresult Lwt.t

val transaction_receipt :
  t -> Ethereum_types.hash -> Transaction_receipt.t option tzresult Lwt.t

val transaction_object :
  t -> Ethereum_types.hash -> Transaction_object.t option tzresult Lwt.t

val next_blueprint_number : t -> Ethereum_types.quantity tzresult Lwt.t

val ro_backend :
  ?evm_node_endpoint:Uri.t ->
  t ->
  Configuration.t ->
  (module Services_backend_sig.S)

type replay_strategy = Blueprint | Assemble

type replay_result =
  | Replay_success of {
      block : Ethereum_types.legacy_transaction_object L2_types.block;
      evm_state : Evm_state.t;
      diverged : bool;
      process_time : Ptime.span;
      execution_gas : Ethereum_types.quantity;
      tezos_block : L2_types.Tezos_block.t option;
    }
  | Replay_failure

val replay :
  t ->
  ?log_file:string ->
  ?profile:Configuration.profile_mode ->
  ?evm_state:Pvm.State.t ->
  ?alter_evm_state:(Pvm.State.t -> (Pvm.State.t, tztrace) result Lwt.t) ->
  replay_strategy ->
  Ethereum_types.quantity ->
  replay_result tzresult Lwt.t

val evm_services_methods :
  t -> Configuration.time_between_blocks -> Rpc_server.evm_services_methods

val blueprints_range : t -> Blueprints_publisher.blueprints_range
