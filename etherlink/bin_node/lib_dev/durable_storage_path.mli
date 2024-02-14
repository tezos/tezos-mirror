(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(* Copyright (c) 2023 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

type path = string

val chain_id : path

val base_fee_per_gas : path

val kernel_version : path

val kernel_upgrade : path

(** Paths related to accounts. *)
module Accounts : sig
  (** Path to the account's balance. *)
  val balance : address -> path

  (** Path to the account's nonce. *)
  val nonce : address -> path

  (** Path to the account's code. *)
  val code : address -> path

  (** Path to the account's storage at a given index. *)
  val storage : address -> path -> path
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
  (** Path to the given transaction receipt. *)
  val receipt : hash -> path
end

module Transaction_object : sig
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
