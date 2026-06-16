(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module type S = sig
  (** [current_block_number ()] returns the most recent processed and
      stored block number. *)
  val current_block_number : unit -> Ethereum_types.quantity tzresult Lwt.t

  (** [nth_block ~full_transaction_object n] returns the [n]th
      processed and stored block.

      If [full_transaction_object] is [true], returns the transaction objects,
      the transactions hashes otherwise.
    *)
  val nth_block :
    full_transaction_object:bool ->
    Z.t ->
    Transaction_object.t Ethereum_types.block tzresult Lwt.t

  (** [block_by_hash ~full_transaction_object hash] returns the block with the
      given [hash].

      If [full_transaction_object] is [true], returns the transaction objects,
      the transactions hashes otherwise.
    *)
  val block_by_hash :
    full_transaction_object:bool ->
    Ethereum_types.block_hash ->
    Transaction_object.t Ethereum_types.block tzresult Lwt.t

  (** [block_receipts n] returns the receipts of the [n]th
      processed and stored block.
    *)
  val block_receipts : Z.t -> Transaction_receipt.t list tzresult Lwt.t

  (** [block_range_receipts n len] returns the receipts of [len] blocks,
      starting from level [n]. *)
  val block_range_receipts :
    ?mask:Ethbloom.t -> Z.t -> int -> Transaction_receipt.t list tzresult Lwt.t

  (** [transaction_receipt tx_hash] returns the receipt of [tx_hash]. *)
  val transaction_receipt :
    Ethereum_types.hash -> Transaction_receipt.t option tzresult Lwt.t

  (** [transaction_object tx_hash] returns the informations of [tx_hash]. *)
  val transaction_object :
    Ethereum_types.hash -> Transaction_object.t option tzresult Lwt.t
end
