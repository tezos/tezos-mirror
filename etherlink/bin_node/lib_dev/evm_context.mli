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

(** [lock_data_dir ~data_dir] takes an exclusive lock on [data_dir] for the
    duration of the process. It fails if there is already another evm node with
    a lock. *)
val lock_data_dir : data_dir:string -> unit tzresult Lwt.t

type store_info = {
  rollup_address : Address.t;
  current_number : Ethereum_types.quantity;
}

(** [export_store ~data_dir ~output_db_file] exports the store database with
    data from the [data_dir] into the [output_db_file] and returns the rollup
    address and the current level. *)
val export_store :
  data_dir:string -> output_db_file:string -> store_info tzresult Lwt.t

(** [start] creates a new worker to manage a local EVM context where it
    initializes the {!type-index}, and use a checkpoint mechanism to load the
    latest {!type-store} if any. Returns a value telling if the context was
    loaded from disk ([Loaded]) or was initialized from scratch ([Created]).
    Returns also the smart rollup address.

    [kernel_path] can be provided to cover the case where the context does not
    exist yet, and is ignored otherwise.

    [data_dir] is the path to the data-dir of the node, notably containing the
    SQLite store and the Irmin context.

    [preimages] is the path to the preimages directory, while a
    [preimages_endponit] URI can be optionally provided to download missing
    preimages when they are requested by the current kernel (during upgrades).

    [native_execution_policy] decides if native execution is used for supported
    kernel. In the context of the [Evm_context] worker, the native execution
    will only be used in [native_execution_policy = Always].

    [smart_rollup_address] can be provided either when starting from a
    non-existing data-dir, or when starting a sandbox.

    [fail_on_missing_blueprint] will decide the behavior of the worker when a
    blueprint is confirmed by a rollup node before it is applied by the EVM
    node. A sequencer will typically set it to true, while observers set it to
    [false] for now.

    [store_perm] decides whether or not the worker can modify the Irmin context
    (it is most certainly an artifact of the past, made outdated by the
    [Evm_ro_context] module. Clearly, [~store_perm:`Read_only] menas you want
    to use [Evm_ro_context] instead.

    [block_storage_sqlite3] decides whether or not the blocks are stored in the
    SQLite store or not.

    [garbage_collector] can be optionally provided to enable the garbage
    collector of the node with a given configuration. *)
val start :
  ?kernel_path:string ->
  data_dir:string ->
  preimages:string ->
  preimages_endpoint:Uri.t option ->
  native_execution_policy:Configuration.native_execution_policy ->
  ?smart_rollup_address:string ->
  fail_on_missing_blueprint:bool ->
  store_perm:[`Read_only | `Read_write] ->
  block_storage_sqlite3:bool ->
  ?garbage_collector:Configuration.garbage_collector ->
  ?sequencer_wallet:Client_keys.sk_uri * Client_context.wallet ->
  unit ->
  (init_status * Address.t) tzresult Lwt.t

(** [init_from_rollup_node ~omit_delayed_tx_events ~data_dir
    ~rollup_node_data_dir ()]
    initialises the irmin context and metadata of the evm using the
    latest known evm state of the given rollup node. if
    [omit_delayed_tx_events] dont populate the delayed tx event from
    the state into the db. *)
val init_from_rollup_node :
  omit_delayed_tx_events:bool ->
  data_dir:string ->
  rollup_node_data_dir:string ->
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
    {!Blueprint_types.with_events}.  It commits the result if the
    blueprint produces the expected block. *)
val apply_blueprint :
  ?events:Evm_events.t list ->
  Time.Protocol.t ->
  Blueprint_types.payload ->
  Evm_events.Delayed_transaction.t list ->
  unit tzresult Lwt.t

val head_info : unit -> head Lwt.t

val next_blueprint_number : unit -> Ethereum_types.quantity Lwt.t

val blueprint :
  Ethereum_types.quantity -> Blueprint_types.with_events option tzresult Lwt.t

val blueprints_range :
  Ethereum_types.quantity ->
  Ethereum_types.quantity ->
  (Ethereum_types.quantity * Blueprint_types.payload) list tzresult Lwt.t

val last_known_l1_level : unit -> int32 option tzresult Lwt.t

val shutdown : unit -> unit tzresult Lwt.t

(** [delayed_inbox_hashes ctxt] returns the hashes in the delayed inbox. *)
val delayed_inbox_hashes : unit -> Ethereum_types.hash list tzresult Lwt.t

(** [patch_kernel path] modifies the state of the current head of the EVM node
    to replace its kernel with the kernel file [path]. *)
val patch_kernel :
  ?block_number:Ethereum_types.quantity -> string -> unit tzresult Lwt.t

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

module State : sig
  (** Path of EVM state store. *)
  val store_path : data_dir:string -> string
end

(** Watcher that gets notified each time a new block is produced. *)
val head_watcher : Ethereum_types.Subscription.output Lwt_watcher.input
