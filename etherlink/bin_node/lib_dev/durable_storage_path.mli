(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2023-2024 Functori <contact@functori.com>                   *)
(* Copyright (c) 2023 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

type path = string

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

(** Paths related to accounts. *)
module Accounts : sig
  (** Path to the account's balance. *)
  val balance : address -> path

  (** Path to the account's nonce. *)
  val nonce : address -> path

  (** Path to the account's code. *)
  val code : address -> path

  (** Path to the account's code hash. *)
  val code_hash : address -> path

  (** Path to the account's storage at a given index. *)
  val storage : address -> path -> path

  type error += Invalid_address of string | Invalid_key of string

  (** Path to the account's balance. Error if address is invalid. *)
  val balance_e : address -> path tzresult

  (** Path to the account's nonce. Error if address is invalid. *)
  val nonce_e : address -> path tzresult

  (** Path to the account's code. Error if address is invalid. *)
  val code_e : address -> path tzresult

  (** Path to the account's storage at a given index. Error if address or
      storage key is invalid. *)
  val storage_e : address -> path -> path tzresult

  val storage_dir_e : address -> path tzresult
end

module Code : sig
  val code : hash -> path
end

(** Paths related to blocks. *)
module Block : sig
  (** Block number is either the current head or a specific height. *)
  type number = Current | Nth of Z.t

  (** Path to the given block. *)
  val by_hash : block_hash -> path

  (** Path to the current block number. *)
  val current_number : path

  (** Path to the current block hash. *)
  val current_hash : path
end

module Indexes : sig
  (** Make the path to the indexed block hash. *)
  val block_by_number : Block.number -> path
end

module Transaction_receipt : sig
  val receipts : path

  (** Path to the given transaction receipt. *)
  val receipt : hash -> path
end

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
