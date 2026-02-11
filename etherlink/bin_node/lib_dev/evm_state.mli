(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t = Pvm.State.t

(** Directory where the kernel logs are stored. The function {!execute} below
    expect the directory to exist.*)
val kernel_logs_directory : data_dir:string -> string

(** [execute ?execution_timestamp ?simulation ~data_dir ?log_file
    ~wasm_entrypoint ~config evm_state messages] executes the
    [wasm_entrypoint] function (default to [kernel_run]) with
    [messages] within the inbox of [evm_state].

    Kernel logs are stored under the {!kernel_logs_directory} in [log_file].
    [simulation] adds a prefix to the event to differenciate the logs.

    When [execution_timestamp] is provided it's used as the l1
    timestamp of in the Info_per_level message of the inbox, default
    value is `epoch`.

    The execution is performed within a domain managed by [pool]. *)
val execute :
  pool:Lwt_domain.pool ->
  ?execution_timestamp:Time.Protocol.t ->
  ?wasm_pvm_fallback:bool ->
  ?profile:Configuration.profile_mode ->
  ?kind:Events.kernel_log_kind ->
  data_dir:string ->
  ?log_file:string ->
  ?wasm_entrypoint:string ->
  config:Pvm_types.config ->
  native_execution:bool ->
  t ->
  Wasm_runtime.kernel_input ->
  t tzresult Lwt.t

(** [init ~kernel] initializes the local [evm_state] with [kernel]. *)
val init : kernel:Pvm_types.kernel -> t tzresult Lwt.t

(** [modify ~key ~value evm_state] sets [value] at [key] in the local EVM
    state. *)
val modify : ?edit_readonly:bool -> key:string -> value:string -> t -> t Lwt.t

(** [delete ~kind evm_state key] delete the value/directory at [key] *)
val delete : kind:Tezos_scoru_wasm.Durable.kind -> t -> string -> t Lwt.t

(** [exists evm_state key] returns [true] if a value or a tree/subtree
    exists under [key] in [evm_state], [false] otherwise. *)
val exists : t -> string -> bool Lwt.t

(** [inspect evm_state key] returns the value stored under [key] in
    [evm_state], if any. *)
val inspect : t -> string -> bytes option Lwt.t

(** [subkeys evm_state key] returns the list of keys stored under [key] in
    [evm_state]. *)
val subkeys : t -> string -> string trace Lwt.t

(** [read evm_state key] returns the bytes stored under [key] in
    [evm_state]. *)
val read : t -> string -> bytes option tzresult Lwt.t

(** [execute_and_inspect ~pool ~data_dir ?wasm_entrypoint ~config ~input
    evm_state] executes the [wasm_entrypoint] function (default to
    [kernel_run]) with [input] within the inbox of [evm_state], and
    returns [input.insights_requests].

    The execution is performed within a domain managed by [pool]. *)
val execute_and_inspect :
  pool:Lwt_domain.pool ->
  ?wasm_pvm_fallback:bool ->
  data_dir:string ->
  ?wasm_entrypoint:string ->
  config:Pvm_types.config ->
  native_execution_policy:Configuration.native_execution_policy ->
  input:Simulation.Encodings.simulate_input ->
  t ->
  bytes option list tzresult Lwt.t

(** [current_block_height ~root evm_state] returns the height of the latest
    block produced by the kernel at [root]. *)
val current_block_height :
  root:Durable_storage_path.path -> t -> Ethereum_types.quantity Lwt.t

(** Same as {!current_block_height} for the block hash. *)
val current_block_hash :
  chain_family:_ L2_types.chain_family ->
  t ->
  Ethereum_types.block_hash tzresult Lwt.t

(** [retrieve_block_at_root ~chain_family ~root evm_state] reads the current
    block from durable storage at the given [root] path, decoding it using
    [chain_family]. *)
val retrieve_block_at_root :
  chain_family:_ L2_types.chain_family ->
  root:Durable_storage_path.path ->
  t ->
  Ethereum_types.legacy_transaction_object L2_types.block option tzresult Lwt.t

type apply_result =
  | Apply_success of {
      evm_state : t;
      block : Ethereum_types.legacy_transaction_object L2_types.block;
    }
  | Apply_failure

(** [apply_unsigned_chunks ~data-dir ~config state chunks] applies the
    blueprint [chunks] on top of [evm_state]. If the operation results in the
    production of a block, the new updated EVM state is returned along with the
    new block’s height. {e It is the responsibility of the caller to ensure
    the correctness of the chunks.}

    The [data-dir] is used to store the kernel logs in the
    {!kernel_logs_directory}. *)
val apply_unsigned_chunks :
  pool:Lwt_domain.pool ->
  ?wasm_pvm_fallback:bool ->
  ?log_file:string ->
  ?profile:Configuration.profile_mode ->
  data_dir:string ->
  chain_family:_ L2_types.chain_family ->
  config:Pvm_types.config ->
  native_execution_policy:Configuration.native_execution_policy ->
  t ->
  Sequencer_blueprint.unsigned_chunked_blueprint ->
  apply_result tzresult Lwt.t

type block_in_progress = {
  timestamp : Time.Protocol.t;
  number : Ethereum_types.quantity;
  transactions_count : int32;
}

(** [execute_single_transaction ~data_dir ~pool ~native_execution ~config
    evm_state block_in_progress hash txn] calls the kernel entrypoint allowing
    to execute [txn] on top of [evm_state], where [txn] is the
    [block_in_progress.transactions_count]th transaction of the next block. *)
val execute_single_transaction :
  data_dir:string ->
  pool:Lwt_domain.pool ->
  native_execution:bool ->
  config:Pvm_types.config ->
  t ->
  block_in_progress ->
  Ethereum_types.hash ->
  Broadcast.transaction ->
  (Transaction_receipt.t * t) tzresult Lwt.t

(** [assemble_block ~pool ~data_dir ~chain_family ~config ~timestamp ~number
    ~native_execution t]
    builds an L2 block at height [number] and [timestamp] from the transactions
    previously accumulated in durable storage by the kernel instant-confirmation
    execution. No blueprint application is (re)performed; the function only
    assembles the block from already recorded effects and returns the result
    of this operation. *)
val assemble_block :
  pool:Lwt_domain.pool ->
  data_dir:string ->
  chain_family:'a L2_types.chain_family ->
  config:Pvm_types.config ->
  timestamp:Time.Protocol.t ->
  number:Ethereum_types.quantity ->
  native_execution:bool ->
  t ->
  apply_result tzresult Lwt.t

(** [flag_local_exec evm_state] adds a flag telling the kernel it is executed
    by an EVM node, not a rollup node. *)
val flag_local_exec : t -> t Lwt.t

(** [clear_delayed_inbox evm_state] removes the delayed inbox from the current
    EVM state. *)
val clear_delayed_inbox : t -> t Lwt.t

val wasm_pvm_version : t -> Tezos_scoru_wasm.Wasm_pvm_state.version Lwt.t

(** [irmin_store_path ~data_dir] returns the path wherein the Irmin store is
    expected to be located, relatively to the data directory. *)
val irmin_store_path : data_dir:string -> string

(** [preload_kernel ~pool evm_state] ensures the kernel of [evm_state] is added
    to the kernel cache of the execution runtime in use. This will speed-up the
    execution time for the first call of this kernel (typically in the context
    of a RPC call).

    The execution of this command is performed within one of the domain managed
    by [pool]. *)
val preload_kernel : pool:Lwt_domain.pool -> t -> unit Lwt.t

(** [get_delayed_inbox_item state hash] returns the delayed inbox content behind
    the hash [hash]. It fails if the hash does not exist or if the value
    cannot be decoded. *)
val get_delayed_inbox_item :
  t -> Ethereum_types.hash -> Evm_events.Delayed_transaction.t tzresult Lwt.t

(** [clear_block_storage chain_family block state] removes the parent of [block],
    and all durable storage information stored for [block], if this function is
    called they need to be store elsewhere, mainly it consists in transactions. *)
val clear_block_storage :
  _ L2_types.chain_family -> 'transaction_object L2_types.block -> t -> t Lwt.t

(** [clear_events state] will remove events generated by the kernel from the
    durable storage of [state]. *)
val clear_events : t -> t Lwt.t

(** [storage_version tree] returns the current storage version set by the
    kernel. This storage version is used by the EVM node to determine whether a
    given feature is implemented by the kernel or not. *)
val storage_version : t -> int tzresult Lwt.t

(** [delayed_inbox_hashes tree] returns a list of hashes—each hash identifying
    an item of the delayed inbox.

    This function will raise an exception if the delayed inbox is nowhere to be
    found. *)
val delayed_inbox_hashes : t -> Ethereum_types.hash list Lwt.t
