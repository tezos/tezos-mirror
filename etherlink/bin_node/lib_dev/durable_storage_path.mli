(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2023-2025 Functori <contact@functori.com>                   *)
(* Copyright (c) 2023 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

type path = string

val etherlink_root : path

val etherlink_safe_root : path

(** [/tez/tez_accounts/contracts/index] — root of the Michelson contracts
    indexable storage. Standalone Tezlink and TezosX-mode KT1
    originations both write here. *)
val michelson_contracts_index : path

(** [/tez/tez_accounts/tezosx] — root of the TezosX projected accounts
    and cross-runtime alias subtree. *)
val michelson_ledger_root : path

(** Root of the Michelson world-state keyspace ([/tez/world_state]). *)
val tez_world_state_root : path

(** Shadow root of the Michelson world-state keyspace
    ([/tmp/tez/world_state]). *)
val tez_world_state_safe_root : path

(** TezosX: root for Tezos blocks stored under the Michelson world state
    ([/tez/world_state/tez_blocks]). *)
val tezosx_tezos_blocks_root : path

val root_of_chain_family : _ L2_types.chain_family -> path

val reboot_counter : string

module BASE : sig
  val make : string -> path
end

val evm_node_flag : storage_version:int -> path

val chain_id : path

val michelson_runtime_chain_id : path

val minimum_base_fee_per_gas : path

val backlog : path

val da_fee_per_byte : path

val michelson_to_evm_gas_multiplier : path

val kernel_version : storage_version:int -> path

val kernel_verbosity : storage_version:int -> path

val storage_version_base : path

val storage_version_legacy : path

val kernel_root_hash : storage_version:int -> path

val kernel_upgrade : storage_version:int -> path

val sequencer_upgrade : storage_version:int -> path

val delayed_inbox : storage_version:int -> path

val sequencer_pool_address : path

val sequencer_key_legacy : path

val sequencer_key_world_state : path

val sequencer_key : storage_version:int -> path

val maximum_gas_per_transaction : path

val michelson_runtime_sunrise_level : storage_version:int -> path

val michelson_runtime_target_sunrise_level : storage_version:int -> path

val maximum_allowed_ticks : storage_version:int -> path

(** Kernel communication canal for individual transaction execution (instant confirmations) *)
module Single_tx : sig
  val input_tx : storage_version:int -> path
end

(** Kernel communication canal for Tezos X operation simulation *)
module Tezosx_simulation : sig
  val input : path

  val result : path
end

(** Kernel communication channel for Tezos X Michelson entrypoints query *)
module Tezosx_entrypoints : sig
  val input : path

  val result : path
end

val delayed_input : storage_version:int -> path

(** Kernel communication canal for block assembling (instant confirmations) *)
module Assemble_block : sig
  val input : storage_version:int -> path
end

(** Paths related to accounts. *)
module Accounts : sig
  (** Path to the account's info. Should be used in place of `balance`, `nonce` and `code` *)
  val info : address -> path

  (** Path to the account's balance. DEPRECATED use `info` *)
  val balance : address -> path

  (** Path to the account's nonce. DEPRECATED use `info` *)
  val nonce : address -> path

  (** Path to the account's code. *)
  val code : address -> path

  (** Path to the account's code hash. DEPRECATED use `info` *)
  val code_hash : address -> path

  type error += Invalid_address of string | Invalid_key of string

  (** Validated EVM address (exactly 40 hex characters). The only way to
      construct a value is through {!fixed_address}, which surfaces
      [Invalid_address] for malformed inputs. Coerces to a {!path} so
      callers can observe the underlying string without re-validating. *)
  type fixed_address = private path

  (** Validated EVM storage-slot index (exactly 64 hex characters). The only
      way to construct a value is through {!fixed_index}, which surfaces
      [Invalid_key] for malformed inputs. Coerces to a {!path} so callers
      can observe the underlying string without re-validating. *)
  type fixed_index = private path

  (** [fixed_address a] returns [a] tagged as validated, or fails with
      [Invalid_address] if the underlying hex string is not 40 characters. *)
  val fixed_address : address -> fixed_address tzresult

  (** [fixed_index s] returns [s] tagged as validated, or fails with
      [Invalid_key] if [s] is not 64 hex characters. *)
  val fixed_index : path -> fixed_index tzresult

  (** Path to a storage slot for a validated address/index pair. *)
  val storage : fixed_address -> fixed_index -> path

  (** Directory holding all storage slots of a validated address. *)
  val storage_dir : fixed_address -> path
end

module Code : sig
  val code : hash -> path
end

module Blueprint : sig
  val current_generation : storage_version:int -> path

  val chunk :
    storage_version:int -> blueprint_number:Z.t -> chunk_index:int -> path

  val nb_chunks : storage_version:int -> blueprint_number:Z.t -> path

  val generation : storage_version:int -> blueprint_number:Z.t -> path
end

(** Paths related to blocks. *)
module Block : sig
  (** Block number is either the current head or a specific height. *)
  type number = Current | Nth of Z.t

  (** Path to the given block. *)
  val by_hash : root:path -> block_hash -> path

  (** Path to the current block data. *)
  val current_block : root:path -> path

  (** Path to the current block number. *)
  val current_number : root:path -> path

  (** Path to the current block hash. *)
  val current_hash : root:path -> path

  (** Path to the current block receipts. *)
  val current_receipts : root:path -> path

  (** Path to the current block transactions objects. *)
  val current_transactions_objects : root:path -> path
end

(** Paths related to block headers. *)
module BlockHeader : sig
  (** Path to the current block header. *)
  val current : storage_version:int -> path
end

(** Can't be used if storage version >= 41 *)
module Indexes : sig
  (** Make the path to the indexed block hash. *)
  val block_by_number : root:path -> Block.number -> path
end

(** Can't be used if storage version >= 41 *)
module Transaction_receipt : sig
  val receipts : path

  (** Path to the given transaction receipt. *)
  val receipt : hash -> path
end

(** Can't be used if storage version >= 41 *)
module Transaction_object : sig
  val objects : path

  (** Path to the given transaction object. *)
  val object_ : hash -> path
end

module Delayed_transaction : sig
  (** Path to the list of hashes of the delayed inbox. *)
  val hashes : storage_version:int -> path

  (** Path to the delayed transaction. *)
  val transaction : storage_version:int -> hash -> path
end

module Evm_events : sig
  (** Path to the list of events of the kernel. *)
  val events : storage_version:int -> path

  (** Path to the length. *)
  val length : storage_version:int -> path

  (** Path to the nth event of the kernel. *)
  val nth_event : storage_version:int -> int -> path
end

module Http_trace : sig
  (** Durable storage flag that enables per-transaction HTTP trace capture
      during a block replay. Written by the EVM node through
      [alter_evm_state] before invoking [Exe.replay] for the
      [http_traceTransaction] / [http_traceBlockByNumber] /
      [http_traceBlockByHash] RPCs, and read by the kernel on every applied
      transaction. *)
  val enabled_flag : path

  (** Path at which the kernel persists the RLP-encoded list of HTTP traces
      for the transaction [transaction_hash] (a hex string, without [0x]
      prefix). A missing key means the transaction performed no cross-runtime
      HTTP call. *)
  val for_tx : transaction_hash:path -> path
end

module Trace : sig
  (** Path under which all traces are stored when the
      given [hash] parameter is provided, otherwise it
      will just provide the path to the root. *)
  val root_indexed_by_hash : transaction_hash:path option -> path

  (** Path where is stored the input of the tracer. *)
  val input : path

  (** Path where is stored the gas consumed by the call. *)
  val output_gas : transaction_hash:path option -> path

  (** Path where the result of the traced transaction is stored. *)
  val output_failed : transaction_hash:path option -> path

  (** Path where is stored the value returned by the transaction's execution. *)
  val output_return_value : transaction_hash:path option -> path

  val logs_length : transaction_hash:path option -> path

  val opcode : transaction_hash:path option -> int -> path

  (** Path where is stored the number of call trace items *)
  val call_trace_length : transaction_hash:path option -> path

  (** Path where is stored the [i]eth trace *)
  val call_trace : transaction_hash:path option -> int -> path
end

module Chain_configuration : sig
  val minimum_base_fee_per_gas :
    storage_version:int -> L2_types.chain_id -> path

  val da_fee_per_byte : storage_version:int -> L2_types.chain_id -> path

  val maximum_gas_per_transaction :
    storage_version:int -> L2_types.chain_id -> path

  val chain_family : storage_version:int -> L2_types.chain_id -> path

  val world_state : storage_version:int -> L2_types.chain_id -> path
end

module Feature_flags : sig
  val multichain : path

  val tezos_runtime : path
end
