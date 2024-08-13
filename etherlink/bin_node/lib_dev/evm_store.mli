(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** A handler to the node’s store. *)
type t

(** A direct connection to the node’s store, allowing to interact with it. *)
type conn

(** [init ~data_dir ()] returns a handler to the EVM node store located under
    [data_dir]. If no store is located in [data_dir], an empty store is
    created. Also returns if the store was created ([true]) or was already
    existing ([false]).

    If [sqlite_journal_mode] is [`Force mode], then the journal mode of the
    SQLite database is updated if necessary to match the requested
    configuration. With [`Identity], the journal mode is left untouched.

    If [perm] is [`Read_only], then SQL requests requiring write access will
    fail. With [`Read_write], they will succeed as expected. *)
val init :
  data_dir:string -> perm:[`Read_only | `Read_write] -> unit -> t tzresult Lwt.t

(** [use store k] executes [k] with a fresh connection to [store]. *)
val use : t -> (conn -> 'a tzresult Lwt.t) -> 'a tzresult Lwt.t

(** Run VACUUM sqlite request *)
val vacuum : conn:conn -> output_db_file:string -> unit tzresult Lwt.t

(** [with_transaction conn k] wraps the accesses to the store from [conn] made
    in the continuation [k] within
    {{:https://www.sqlite.org/lang_transaction.html}a SQL transaction}. If [k]
    fails, the transaction is rollbacked. Otherwise, the transaction is
    committed. *)
val with_transaction : conn -> (conn -> 'a tzresult Lwt.t) -> 'a tzresult Lwt.t

(** [assert_in_transaction conn] raises an exception if a transaction has not
    been started with [conn].

    @raise Assert_failure *)
val assert_in_transaction : conn -> unit

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

  val find_earliest :
    conn -> (Ethereum_types.quantity * Irmin_context.hash) option tzresult Lwt.t

  val find_finalized :
    conn -> (Ethereum_types.quantity * Irmin_context.hash) option tzresult Lwt.t

  val clear_after : conn -> Ethereum_types.quantity -> unit tzresult Lwt.t
end

module Kernel_upgrades : sig
  val store :
    conn ->
    Ethereum_types.quantity ->
    Evm_events.Upgrade.t ->
    unit tzresult Lwt.t

  val activation_levels : conn -> Ethereum_types.quantity list tzresult Lwt.t

  val find_latest_pending : conn -> Evm_events.Upgrade.t option tzresult Lwt.t

  val find_applied_before :
    conn ->
    Ethereum_types.quantity ->
    Evm_events.Upgrade.t option tzresult Lwt.t

  val record_apply : conn -> Ethereum_types.quantity -> unit tzresult Lwt.t

  val clear_after : conn -> Ethereum_types.quantity -> unit tzresult Lwt.t
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

module Metadata : sig
  val store :
    conn -> Tezos_crypto.Hashed.Smart_rollup_address.t -> unit tzresult Lwt.t

  val get : conn -> Tezos_crypto.Hashed.Smart_rollup_address.t tzresult Lwt.t

  val find :
    conn -> Tezos_crypto.Hashed.Smart_rollup_address.t option tzresult Lwt.t
end

(** [reset conn ~l2_level] clear the table that has information related to l2
    level that after [l2_level] *)
val reset : conn -> l2_level:Ethereum_types.quantity -> unit tzresult Lwt.t
