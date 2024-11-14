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

(** [start ~data_dir ~preimages ~preimages_endpoint
    ~smart_rollup_address ()] creates a new worker to
    manage a local EVM context where it initializes the {!type-index},
    and use a checkpoint mechanism to load the latest {!type-store} if
    any.

    Returns a value telling if the context was loaded from disk
    ([Loaded]) or was initialized from scratch ([Created]). Returns
    also the smart rollup address. *)
val start :
  ?kernel_path:string ->
  data_dir:string ->
  preimages:string ->
  preimages_endpoint:Uri.t option ->
  ?smart_rollup_address:string ->
  fail_on_missing_blueprint:bool ->
  store_perm:[`Read_only | `Read_write] ->
  block_storage_sqlite3:bool ->
  ?garbage_collector:Configuration.garbage_collector ->
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

(** [reconstruct ~data_dir ~rollup_node_data_dir ~boot_sector] replays all
    L1 blocks of [rollup_node_data_dir] since the genesis. Populates the
    new [data_dir] with a full history. *)
val reconstruct :
  data_dir:string ->
  rollup_node_data_dir:string ->
  boot_sector:string ->
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

(** [last_produced_blueprint ctxt] returns the blueprint used to
    create the current head of the chain. *)
val last_produced_blueprint : unit -> Blueprint_types.t tzresult Lwt.t

(** [apply_blueprint timestamp payload delayed_transactions] applies
    [payload] in the freshest EVM state stored under [ctxt] at
    timestamp [timestamp], forwards the {!Blueprint_types.with_events}.
    It commits the result if the blueprint produces the expected block. *)
val apply_blueprint :
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

val new_last_known_l1_level : int32 -> unit tzresult Lwt.t

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

val block_param_to_block_number :
  Ethereum_types.Block_parameter.extended ->
  Ethereum_types.quantity tzresult Lwt.t

module State : sig
  (** Path of EVM state store. *)
  val store_path : data_dir:string -> string
end
