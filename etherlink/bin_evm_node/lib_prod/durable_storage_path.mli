open Ethereum_types

type path = string

val chain_id : path

val base_fee_per_gas : path

val kernel_version : path

val upgrade_nonce : path

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
