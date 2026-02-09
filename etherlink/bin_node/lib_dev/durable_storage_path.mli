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

val tezlink_root : path

val etherlink_root : path

val etherlink_safe_root : path

(** TezosX: root for Tezos blocks stored under the EVM world state
    ([/evm/world_state/tezlink]). *)
val tezosx_tezos_blocks_root : path

val root_of_chain_family : _ L2_types.chain_family -> path

val reboot_counter : string

val evm_node_flag : path

val chain_id : path

val minimum_base_fee_per_gas : path

val backlog : path

val da_fee_per_byte : path

val kernel_version : path

val kernel_verbosity : path

val storage_version : path

val kernel_root_hash : path

val kernel_upgrade : path

val sequencer_upgrade : path

val delayed_inbox : path

val sequencer_pool_address : path

val sequencer_key : path

val maximum_gas_per_transaction : path

(** Kernel communication canal for individual transaction execution (instant confirmations) *)
module Single_tx : sig
  val input_tx : path
end

(** Kernel communication canal for block assembling (instant confirmations) *)
module Assemble_block : sig
  val input : path
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

  (** Path to the account's storage at a given index. *)
  val storage : address -> path -> path

  type error += Invalid_address of string | Invalid_key of string

  (** Path to the account's storage at a given index. Error if address or
      storage key is invalid. *)
  val storage_e : address -> path -> path tzresult

  val storage_dir_e : address -> path tzresult
end

module Code : sig
  val code : hash -> path
end

module Blueprint : sig
  val current_generation : path

  val chunk : blueprint_number:Z.t -> chunk_index:int -> path

  val nb_chunks : blueprint_number:Z.t -> path

  val generation : blueprint_number:Z.t -> path
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
  val current : path
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
  val hashes : path

  (** Path to the delayed transaction. *)
  val transaction : hash -> path
end

module Evm_events : sig
  (** Path to the list of events of the kernel. *)
  val events : path

  (** Path to the length. *)
  val length : path

  (** Path to the nth event of the kernel. *)
  val nth_event : int -> path
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
  val minimum_base_fee_per_gas : L2_types.chain_id -> path

  val da_fee_per_byte : L2_types.chain_id -> path

  val maximum_gas_per_transaction : L2_types.chain_id -> path

  val chain_family : L2_types.chain_id -> path

  val world_state : L2_types.chain_id -> path
end

module Feature_flags : sig
  val multichain : path
end
