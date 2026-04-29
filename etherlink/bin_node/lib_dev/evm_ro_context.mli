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

(** [michelson_activation_level ctxt] returns the EVM block level at which the
    Michelson runtime was activated, or [None] if it has not been activated. *)
val michelson_activation_level : t -> int64 option tzresult Lwt.t

(** [current_block_number_durable ctxt ~chain_family] returns the current block
    number of the L2 chain for the given [chain_family] from durable storage. *)
val current_block_number_durable :
  t ->
  chain_family:_ L2_types.chain_family ->
  Ethereum_types.quantity tzresult Lwt.t

(** [storage_version ctxt] returns the latest storage version known to the
    current kernel. *)
val storage_version : t -> int tzresult Lwt.t

(** [kernel_version ctxt] returns the internal kernel version. *)
val kernel_version : t -> string tzresult Lwt.t

(** [kernel_root_hash ctxt] returns the internal kernel root hash. *)
val kernel_root_hash : t -> Ethereum_types.hex option tzresult Lwt.t

(** [list_runtimes ctxt] returns the list of runtimes activated in the kernel. *)
val list_runtimes : t -> Tezosx.runtime list tzresult Lwt.t

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

(** Same as {!block_by_hash} but returns [None] for the not-found case
    and propagates any other error from the store unchanged, instead of
    collapsing "hash unknown" into a generic error message. Callers that
    need to distinguish "unknown hash" from any other failure — e.g. to
    map the former to a structured RPC error while letting storage or
    migration errors surface with their original trace — should use this
    variant. *)
val block_by_hash_opt :
  t ->
  full_transaction_object:bool ->
  Ethereum_types.block_hash ->
  Transaction_object.t Ethereum_types.block option tzresult Lwt.t

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

(** [get_state ctxt ?block ()] returns the EVM state at the given block
    parameter. Defaults to [Latest]. *)
val get_state :
  t ->
  ?block:Ethereum_types.Block_parameter.extended ->
  unit ->
  Evm_state.t tzresult Lwt.t

(** [read_state state path] reads a value from the durable storage at [path]
    in the given [state]. Uses {!Durable_storageV2.read_opt} internally. *)
val read_state :
  Evm_state.t -> Durable_storage_path.path -> bytes option tzresult Lwt.t

(** [subkeys state path] returns the list of subkeys under [path] in the
    durable storage of [state]. *)
val subkeys :
  Evm_state.t -> Durable_storage_path.path -> string list tzresult Lwt.t

(** [execute_entrypoint ctxt state ~input_path ~input ~output_path ~entrypoint]
    writes [input] to [input_path] in durable storage, calls the kernel
    [entrypoint], and reads the result bytes from [output_path]. *)
val execute_entrypoint :
  t ->
  Evm_state.t ->
  input_path:string ->
  input:bytes ->
  output_path:string ->
  entrypoint:string ->
  bytes tzresult Lwt.t

(** Like [execute_entrypoint] but reads multiple insight paths from durable
    storage after calling the kernel entrypoint. *)
val execute_entrypoint_with_insights :
  t ->
  Evm_state.t ->
  input_path:string ->
  input:bytes ->
  insight_requests:Simulation.Encodings.insight_request list ->
  entrypoint:string ->
  bytes option list tzresult Lwt.t

(** {2 Etherlink backend operations} *)

module Etherlink : sig
  val balance :
    t ->
    Ethereum_types.address ->
    Ethereum_types.Block_parameter.extended ->
    Ethereum_types.quantity tzresult Lwt.t

  val nonce :
    t ->
    Ethereum_types.address ->
    Ethereum_types.Block_parameter.extended ->
    Ethereum_types.quantity option tzresult Lwt.t

  val code :
    t ->
    Ethereum_types.address ->
    Ethereum_types.Block_parameter.extended ->
    Ethereum_types.hex tzresult Lwt.t

  val storage_at :
    t ->
    Ethereum_types.address ->
    Ethereum_types.quantity ->
    Ethereum_types.Block_parameter.extended ->
    Ethereum_types.hex tzresult Lwt.t

  val base_fee_per_gas : t -> Ethereum_types.quantity tzresult Lwt.t

  val backlog : t -> Z.t tzresult Lwt.t

  val minimum_base_fee_per_gas : t -> Z.t tzresult Lwt.t

  val coinbase : t -> Ethereum_types.address tzresult Lwt.t

  val replay :
    t ->
    Ethereum_types.quantity ->
    Ethereum_types.legacy_transaction_object Ethereum_types.block tzresult Lwt.t
end

(** {2 HTTP trace operations (replay-based)} *)

module Http_tracer : sig
  (** [trace_transaction ctxt tx_hash] re-executes the block containing the
      transaction [tx_hash] with HTTP trace capture enabled, and returns the
      list of HTTP exchanges performed by that transaction. A transaction
      that performed no cross-runtime HTTP call returns the empty list
      rather than an error.

      Fails with:
      - [Tracer_types.Transaction_not_found] if the hash is unknown to the
        node;
      - [Tracer_types.Trace_not_found] if the replay of the containing block
        itself failed. *)
  val trace_transaction :
    t -> Ethereum_types.hash -> Simulation.http_trace list tzresult Lwt.t

  (** [trace_block ctxt block_number] re-executes the block at [block_number]
      with HTTP trace capture enabled, and returns one [(tx_hash, traces)]
      entry per transaction of the block, in block order. Transactions that
      performed no HTTP call come back with the empty list. Blocks with no
      transactions return the empty list without running a replay.

      Fails with [Tracer_types.Trace_not_found] if the replay itself failed,
      and propagates the underlying error if [block_number] is unknown. *)
  val trace_block :
    t ->
    Ethereum_types.quantity ->
    (Ethereum_types.hash * Simulation.http_trace list) list tzresult Lwt.t

  (** Same as {!trace_block}, but resolves the block by hash.

      Fails with:
      - [Tracer_types.Block_hash_not_found] if [block_hash] is unknown to
        the node (the same error surface as {!trace_block} for unknown
        block numbers);
      - [Tracer_types.Trace_not_found] if the replay itself failed. *)
  val trace_block_by_hash :
    t ->
    Ethereum_types.block_hash ->
    (Ethereum_types.hash * Simulation.http_trace list) list tzresult Lwt.t
end

(** {2 Tracer operations} *)

module Tracer_etherlink : sig
  val trace_transaction :
    t ->
    Ethereum_types.hash ->
    Tracer_types.config ->
    Tracer_types.output tzresult Lwt.t

  val trace_call :
    t ->
    Ethereum_types.call ->
    Ethereum_types.Block_parameter.extended ->
    Tracer_types.config ->
    Tracer_types.output tzresult Lwt.t

  val trace_block :
    t ->
    Ethereum_types.quantity ->
    Tracer_types.config ->
    Tracer_types.block_output tzresult Lwt.t
end

(** {2 Tezlink block storage} *)

val tezlink_nth_block : t -> Z.t -> L2_types.Tezos_block.t tzresult Lwt.t

val tezlink_nth_block_hash :
  t -> Z.t -> Ethereum_types.block_hash option tzresult Lwt.t

val tezosx_nth_block : t -> Z.t -> L2_types.Tezos_block.t tzresult Lwt.t

val tezosx_nth_block_hash :
  t -> Z.t -> Ethereum_types.block_hash option tzresult Lwt.t

(** [meta_block_hashes_of_number ctxt level] returns the meta-block hashes
    for the given level. *)
val meta_block_hashes_of_number :
  t -> Z.t -> Meta_block.hashes option tzresult Lwt.t

(** [meta_block_number_of_hash ctxt hash] returns the block level
    corresponding to the given block hash identifier (EVM or Michelson). *)
val meta_block_number_of_hash :
  t ->
  Meta_block.block_hash_identifier ->
  Ethereum_types.quantity option tzresult Lwt.t

val next_blueprint_number : t -> Ethereum_types.quantity tzresult Lwt.t

type evm_services_methods = {
  next_blueprint_number : unit -> Ethereum_types.quantity Lwt.t;
  find_blueprint :
    Ethereum_types.quantity -> Blueprint_types.with_events option tzresult Lwt.t;
  find_blueprint_legacy :
    Ethereum_types.quantity ->
    Blueprint_types.Legacy.with_events option tzresult Lwt.t;
  smart_rollup_address : Address.t;
  time_between_blocks : Evm_node_config.Configuration.time_between_blocks;
}

val evm_services_methods :
  t -> Configuration.time_between_blocks -> evm_services_methods

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

val blueprints_range : t -> Blueprints_publisher.blueprints_range
