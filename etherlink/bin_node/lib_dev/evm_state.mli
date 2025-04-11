(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t = Irmin_context.PVMState.value

(** Directory where the kernel logs are stored. The function {!execute} below
    expect the directory to exist.*)
val kernel_logs_directory : data_dir:string -> string

(** [execute ?simulation ~data_dir ?log_file ~wasm_entrypoint ~config
    evm_state messages] executes the [wasm_entrypoint] function
    (default to [kernel_run]) with [messages] within the inbox of
    [evm_state].

    Kernel logs are stored under the {!kernel_logs_directory} in [log_file].
    [simulation] adds a prefix to the event to differenciate the logs.
*)
val execute :
  ?wasm_pvm_fallback:bool ->
  ?profile:Configuration.profile_mode ->
  ?kind:Events.kernel_log_kind ->
  data_dir:string ->
  ?log_file:string ->
  ?wasm_entrypoint:string ->
  config:Wasm_debugger.config ->
  native_execution:bool ->
  t ->
  [< `Input of string] list ->
  t tzresult Lwt.t

(** [init ~kernel] initializes the local [evm_state] with [kernel]. *)
val init : kernel:Wasm_debugger.kernel -> t tzresult Lwt.t

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

(** [execute_and_inspect ~data_dir ?wasm_entrypoint ~config ~input
    evm_state] executes the [wasm_entrypoint] function (default to
    [kernel_run]) with [input] within the inbox of [evm_state], and
    returns [input.insights_requests]. *)
val execute_and_inspect :
  ?wasm_pvm_fallback:bool ->
  data_dir:string ->
  ?wasm_entrypoint:string ->
  config:Wasm_debugger.config ->
  native_execution_policy:Configuration.native_execution_policy ->
  input:Simulation.Encodings.simulate_input ->
  t ->
  bytes option list tzresult Lwt.t

(** [current_block_height evm_state] returns the height of the latest block
    produced by the kernel. *)
val current_block_height : t -> Ethereum_types.quantity Lwt.t

(** Same as {!current_block_height} for the block hash. *)
val current_block_hash :
  chain_family:L2_types.chain_family ->
  t ->
  Ethereum_types.block_hash tzresult Lwt.t

type apply_result =
  | Apply_success of {
      evm_state : t;
      block : Ethereum_types.legacy_transaction_object L2_types.block;
    }
  | Apply_failure

(** [apply_blueprint ~data-dir ~config state payload] applies the
    blueprint [payload] on top of [evm_state]. If the payload produces
    a block, the new updated EVM state is returned along with the new
    block’s height.

    The [data-dir] is used to store the kernel logs in the
    {!kernel_logs_directory}.
*)
val apply_blueprint :
  ?wasm_pvm_fallback:bool ->
  ?log_file:string ->
  ?profile:Configuration.profile_mode ->
  data_dir:string ->
  chain_family:L2_types.chain_family ->
  config:Wasm_debugger.config ->
  native_execution_policy:Configuration.native_execution_policy ->
  t ->
  Blueprint_types.payload ->
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

(** [preload_kernel evm_state] ensures the kernel of [evm_state] is added to
    the kernel cache of the execution runtime in use. This will speed-up the
    execution time for the first call of this kernel (typically in the context
    of a RPC call). *)
val preload_kernel : t -> unit Lwt.t

(** [get_delayed_inbox_item state hash] returns the delayed inbox content behind
    the hash [hash]. It fails if the hash does not exist or if the value
    cannot be decoded. *)
val get_delayed_inbox_item :
  t -> Ethereum_types.hash -> Evm_events.Delayed_transaction.t tzresult Lwt.t

(**[clear_block_storage block state] removes the parent of [block], and all
   durable storage information stored for [block], if this function is called
   they need to be store elsewhere, mainly it consists in transactions. *)
val clear_block_storage : 'transaction_object L2_types.block -> t -> t Lwt.t

(** [storage_version tree] returns the current storage version set by the
    kernel. This storage version is used by the EVM node to determine whether a
    given feature is implemented by the kernel or not. *)
val storage_version : t -> int tzresult Lwt.t
