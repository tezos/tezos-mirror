(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type init_status = Loaded | Created

type head = {
  current_block_hash : Ethereum_types.block_hash;
  finalized_number : Ethereum_types.quantity;
  next_blueprint_number : Ethereum_types.quantity;
  evm_state : Evm_state.t;
  pending_upgrade : Evm_events.Upgrade.t option;
}

type error += Cannot_apply_blueprint of {local_state_level : Z.t}

(** [start] creates a new worker to manage a local EVM context where it
    initializes the {!type-index}, and use a checkpoint mechanism to load the
    latest {!type-store} if any. Returns a value telling if the context was
    loaded from disk ([Loaded]) or was initialized from scratch ([Created]).
    Returns also the smart rollup address.

    [kernel_path] can be provided to cover the case where the context does not
    exist yet, and is ignored otherwise.

    [data_dir] is the path to the data-dir of the node, notably containing the
    SQLite store and the Irmin context.

    [smart_rollup_address] can be provided either when starting from a
    non-existing data-dir, or when starting a sandbox.

    [store_perm] decides whether or not the worker can modify the Irmin context
    (it is most certainly an artifact of the past, made outdated by the
    [Evm_ro_context] module. Clearly, [~store_perm:`Read_only] menas you want
    to use [Evm_ro_context] instead.

    [snapshot_url] can be provided to automatically fetch and import the
    snapshot if the [data_dir] was not initialized before. *)
val start :
  configuration:Configuration.t ->
  ?kernel_path:Wasm_debugger.kernel ->
  data_dir:string ->
  ?smart_rollup_address:string ->
  store_perm:[`Read_only | `Read_write] ->
  ?sequencer_wallet:Client_keys.sk_uri * Client_context.wallet ->
  ?snapshot_url:string ->
  tx_container:(module Services_backend_sig.Tx_container) ->
  unit ->
  (init_status * Address.t) tzresult Lwt.t

(** [init_from_rollup_node ~omit_delayed_tx_events ~data_dir
    ~rollup_node_data_dir ()]
    initialises the irmin context and metadata of the evm using the
    latest known evm state of the given rollup node. if
    [omit_delayed_tx_events] dont populate the delayed tx event from
    the state into the db. *)
val init_from_rollup_node :
  configuration:Configuration.t ->
  omit_delayed_tx_events:bool ->
  data_dir:string ->
  rollup_node_data_dir:string ->
  tx_container:(module Services_backend_sig.Tx_container) ->
  unit ->
  unit tzresult Lwt.t

(** [reset ~data_dir ~l2_level] reset the sequencer storage to
    [l2_level]. {b Warning: b} Data will be lost ! *)
val reset :
  data_dir:string -> l2_level:Ethereum_types.quantity -> unit tzresult Lwt.t

(** [apply_evm_events ~finalized_level events] applies all the
    events [events] on the local context. The events are performed in a
    transactional context.

    Stores [finalized_level] with {!new_last_known_l1_level} if provided.
*)
val apply_evm_events :
  ?finalized_level:int32 -> Evm_events.t list -> unit tzresult Lwt.t

(** [apply_blueprint ?events timestamp payload delayed_transactions]
    applies [payload] in the freshest EVM state stored under [ctxt] at
    timestamp [timestamp], forwards the
    {!Blueprint_types.with_events}, and returns the transaction hashes
    of the created block. It commits the result if the blueprint
    produces the expected block. *)
val apply_blueprint :
  ?events:Evm_events.t list ->
  Time.Protocol.t ->
  Blueprint_types.payload ->
  Evm_events.Delayed_transaction.t list ->
  Ethereum_types.hash Seq.t tzresult Lwt.t

val head_info : unit -> head Lwt.t

val next_blueprint_number : unit -> Ethereum_types.quantity Lwt.t

val last_known_l1_level : unit -> int32 option tzresult Lwt.t

val shutdown : unit -> unit tzresult Lwt.t

(** [delayed_inbox_hashes ctxt] returns the hashes in the delayed inbox. *)
val delayed_inbox_hashes : unit -> Ethereum_types.hash list tzresult Lwt.t

(** [patch_kernel ?block_number kernel] modifies the state of the
    [block_number] (defaults to current head) of the EVM node to replace its
    kernel with the provided [kernel]. *)
val patch_kernel :
  ?block_number:Ethereum_types.quantity ->
  Wasm_debugger.kernel ->
  unit tzresult Lwt.t

(** [provision_balance address value] modifies the state of the current head of
    the EVM node to increase the balance of [address] by [value].

    [block_number] can be provided to modify another block. *)
val provision_balance :
  ?block_number:Ethereum_types.quantity ->
  Ethereum_types.address ->
  Ethereum_types.quantity ->
  unit tzresult Lwt.t

(** [patch_sequencer_key public_key] modifies the in memory state of the
    EVM node to replace the sequencer key with [public_key]. It does not
    modify the current head.  *)
val patch_sequencer_key :
  ?block_number:Ethereum_types.quantity ->
  Signature.public_key ->
  unit tzresult Lwt.t

(** [patch_state ~key ~value ()] writes [value] at [key]. *)
val patch_state :
  ?block_number:Ethereum_types.quantity ->
  key:string ->
  value:string ->
  unit ->
  unit tzresult Lwt.t

(** [potential_observer_reorg evm_node_endpoint blueprint_with_events] checks
    with the [evm_node_endpoint] if a reorganization happened, and return the
    reorganization level if it exists.

    A reorganization can happen typically if the kernel flushed its delayed inbox
    in a blueprint. *)
val potential_observer_reorg :
  Uri.t ->
  Blueprint_types.with_events ->
  Ethereum_types.quantity option tzresult Lwt.t

(** Watcher that gets notified each time a new block is produced. *)
val head_watcher :
  Transaction_object.t Ethereum_types.Subscription.output Lwt_watcher.input

(** Watcher that gets notified each time a new receipt is produced. *)
val receipt_watcher : Transaction_receipt.t Lwt_watcher.input

(** Watcher that gets notified of new L1 levels its associated L2 levels. *)
val l1_l2_levels_watcher :
  Ethereum_types.Subscription.l1_l2_levels_output Lwt_watcher.input

(** [check_history_mode ?switch ~store_history_mode ~history_mode ()] checks
    that the history mode are compatible, and returns the history mode the node
    should run in, depending on its stored mode [store_history_mode], the one
    requested by the configuration [history_mode] and if it is allowed to
    [switch]. *)
val check_history_mode :
  ?switch:bool ->
  store_history_mode:Configuration.history_mode option ->
  history_mode:Configuration.history_mode option ->
  unit ->
  Configuration.history_mode tzresult Lwt.t
