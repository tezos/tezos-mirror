(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** The EVM node’s store is built around and SQLite database. *)
include module type of Sqlite

(** [init ~data_dir ()] returns a handler to the EVM node store located under
    [data_dir]. If no store is located in [data_dir], an empty store is
    created. Also returns if the store was created ([true]) or was already
    existing ([false]).

    If [perm] is [`Read_only], then SQL requests requiring write access will
    fail. With [`Read_write], they will succeed as expected. *)
val init :
  data_dir:string -> perm:[`Read_only | `Read_write] -> unit -> t tzresult Lwt.t

(** name of the sqlite file *)
val sqlite_file_name : string

module Schemas : sig
  (** [get_all conn] returns the list of SQL statements allowing to recreate
      the tables in the current store. *)
  val get_all : conn -> string list tzresult Lwt.t
end

module Blueprints : sig
  val store : conn -> Blueprint_types.t -> unit tzresult Lwt.t

  val find :
    conn -> Ethereum_types.quantity -> Blueprint_types.t option tzresult Lwt.t

  val find_with_events :
    conn ->
    Ethereum_types.quantity ->
    Blueprint_types.with_events option tzresult Lwt.t

  val get_with_events :
    conn ->
    Ethereum_types.quantity ->
    Blueprint_types.with_events tzresult Lwt.t

  val find_range :
    conn ->
    from:Ethereum_types.quantity ->
    to_:Ethereum_types.quantity ->
    (Ethereum_types.quantity * Blueprint_types.payload) list tzresult Lwt.t

  val clear_after : conn -> Ethereum_types.quantity -> unit tzresult Lwt.t
end

module Context_hashes : sig
  val store :
    conn -> Ethereum_types.quantity -> Irmin_context.hash -> unit tzresult Lwt.t

  val find :
    conn -> Ethereum_types.quantity -> Irmin_context.hash option tzresult Lwt.t

  val find_latest :
    conn -> (Ethereum_types.quantity * Irmin_context.hash) option tzresult Lwt.t

  val get_latest :
    conn -> (Ethereum_types.quantity * Irmin_context.hash) tzresult Lwt.t

  val find_earliest :
    conn -> (Ethereum_types.quantity * Irmin_context.hash) option tzresult Lwt.t

  val get_earliest :
    conn -> (Ethereum_types.quantity * Irmin_context.hash) tzresult Lwt.t

  val find_finalized :
    conn -> (Ethereum_types.quantity * Irmin_context.hash) option tzresult Lwt.t

  val clear_after : conn -> Ethereum_types.quantity -> unit tzresult Lwt.t
end

type pending_kernel_upgrade = {
  kernel_upgrade : Evm_events.Upgrade.t;
  injected_before : Ethereum_types.quantity;
}

module Kernel_upgrades : sig
  val store :
    conn ->
    Ethereum_types.quantity ->
    Evm_events.Upgrade.t ->
    unit tzresult Lwt.t

  val activation_levels : conn -> Ethereum_types.quantity list tzresult Lwt.t

  val find_latest_pending : conn -> pending_kernel_upgrade option tzresult Lwt.t

  val record_apply : conn -> Ethereum_types.quantity -> unit tzresult Lwt.t

  val clear_after : conn -> Ethereum_types.quantity -> unit tzresult Lwt.t

  (** [find_latest_injecter_after n] returns the latest upgrade injected after
   the blueprint [n] was applied. *)
  val find_latest_injected_after :
    conn ->
    Ethereum_types.quantity ->
    Evm_events.Upgrade.t option tzresult Lwt.t
end

module Delayed_transactions : sig
  val store :
    conn ->
    Ethereum_types.quantity ->
    Evm_events.Delayed_transaction.t ->
    unit tzresult Lwt.t

  val at_level :
    conn ->
    Ethereum_types.quantity ->
    Evm_events.Delayed_transaction.t list tzresult Lwt.t

  val at_hash :
    conn ->
    Ethereum_types.hash ->
    Evm_events.Delayed_transaction.t option tzresult Lwt.t

  val clear_after : conn -> Ethereum_types.quantity -> unit tzresult Lwt.t
end

module Blocks : sig
  val store :
    conn ->
    Ethereum_types.legacy_transaction_object Ethereum_types.block ->
    unit tzresult Lwt.t

  (** [find_with_level ~full_transaction_object conn level] returns the block
      if it's present in the storage. If [full_transaction_object] is set to true,
      it will also retrieve the transactions objects part of the block. *)
  val find_with_level :
    full_transaction_object:bool ->
    conn ->
    Ethereum_types.quantity ->
    Transaction_object.t Ethereum_types.block option tzresult Lwt.t

  (** Same as {!find_with_level} but finds with the block hash instead of block
      number. *)
  val find_with_hash :
    full_transaction_object:bool ->
    conn ->
    Ethereum_types.block_hash ->
    Transaction_object.t Ethereum_types.block option tzresult Lwt.t

  val find_hash_of_number :
    conn ->
    Ethereum_types.quantity ->
    Ethereum_types.block_hash option tzresult Lwt.t

  val find_number_of_hash :
    conn ->
    Ethereum_types.block_hash ->
    Ethereum_types.quantity option tzresult Lwt.t

  val clear_after : conn -> Ethereum_types.quantity -> unit tzresult Lwt.t
end

module Block_storage_mode : sig
  (** [legacy conn] returns [true] if the EVM node storage uses the legacy
      block storage (i.e., Irmin). *)
  val legacy : conn -> bool tzresult Lwt.t

  (** [force_legacy conn] forces the node to use the legacy block storage. It
      is a requirement for the [init from rollup node] command, and should not
      be used in other context. *)
  val force_legacy : conn -> unit tzresult Lwt.t
end

val context_hash_of_block_hash :
  conn -> Ethereum_types.block_hash -> Irmin_context.hash option tzresult Lwt.t

module Transactions : sig
  val store : conn -> Transaction_info.t -> unit tzresult Lwt.t

  val find_receipt :
    conn -> Ethereum_types.hash -> Transaction_receipt.t option tzresult Lwt.t

  val find_object :
    conn -> Ethereum_types.hash -> Transaction_object.t option tzresult Lwt.t

  (** [receipts_of_block_number conn block_number] returns all the receipts found
      from level [block_number]. The function does not check if the block exists. *)
  val receipts_of_block_number :
    conn -> Ethereum_types.quantity -> Transaction_receipt.t list tzresult Lwt.t
end

module Irmin_chunks : sig
  val insert :
    conn ->
    Ethereum_types.quantity ->
    Time.Protocol.t ->
    unit Error_monad.tzresult Lwt.t

  val nth :
    conn ->
    int ->
    (Ethereum_types.quantity * Time.Protocol.t) option tzresult Lwt.t

  val latest :
    conn -> (Ethereum_types.quantity * Time.Protocol.t) option tzresult Lwt.t

  val clear : conn -> unit tzresult Lwt.t
end

module Pending_confirmations : sig
  val insert :
    conn ->
    Ethereum_types.quantity ->
    Ethereum_types.block_hash ->
    unit tzresult Lwt.t

  val find_with_level :
    conn ->
    Ethereum_types.quantity ->
    Ethereum_types.block_hash option tzresult Lwt.t

  val delete_with_level : conn -> Ethereum_types.quantity -> unit tzresult Lwt.t

  val is_empty : conn -> bool tzresult Lwt.t
end

module L1_l2_levels_relationships : sig
  type t = {
    l1_level : int32;
    current_number : Ethereum_types.quantity;
    finalized : Ethereum_types.quantity;
  }

  val store :
    conn ->
    l1_level:int32 ->
    latest_l2_level:Ethereum_types.quantity ->
    finalized_l2_level:Ethereum_types.quantity ->
    unit tzresult Lwt.t

  val find : conn -> t option tzresult Lwt.t

  val clear_after : conn -> Ethereum_types.quantity -> unit tzresult Lwt.t
end

type metadata = {
  smart_rollup_address : Address.t;
  history_mode : Configuration.history_mode;
}

module Metadata : sig
  val store : conn -> metadata -> unit tzresult Lwt.t

  val get : conn -> metadata tzresult Lwt.t

  val find : conn -> metadata option tzresult Lwt.t

  val store_history_mode :
    conn -> Configuration.history_mode -> unit tzresult Lwt.t

  val find_history_mode :
    conn -> Configuration.history_mode option tzresult Lwt.t

  val get_history_mode : conn -> Configuration.history_mode tzresult Lwt.t
end

(** [reset_after conn ~l2_level] clear the table that has information related to l2
    level after [l2_level] *)
val reset_after :
  conn -> l2_level:Ethereum_types.quantity -> unit tzresult Lwt.t

(** [reset_before conn ~l2_level] clear the table that has information related to l2
    level before [l2_level], if [history_mode] is set to [Full] keep block-related information and clear the rest *)
val reset_before :
  conn ->
  l2_level:Ethereum_types.quantity ->
  history_mode:Configuration.history_mode ->
  unit tzresult Lwt.t
